;;; im-org-grabbability.el --- Sort headers by how grabbable they are  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: org, productivity

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sort headers by how grabbable they are.  I use the
;; `im-org-grabbability-compare' function as the :SORT function to
;; org-ql.
;;
;; LLM generated.  Not really sure if it's useful, might delete later.

;;; Code:

(require 'org)
(require 'org-clock)

;;;; Customization

(defgroup im-org-grabbability nil
  "Sort org headings by how actionable they are."
  :group 'org)

(defcustom im-org-grabbability-weights
  '((priority       . 30)
    (effort-set     . 15)
    (effort-low     . 25)
    (todo-keyword   . 20)
    (scheduled      . 20)
    (checkbox       . 20)
    (clocksum       . 15)
    (recency        . 10)
    (age-penalty    . -15)
    (content        .  5)
    (tags           .  3)
    (id             .  2))
  "Alist of scoring factors and their max weight contribution."
  :type '(alist :key-type symbol :value-type number)
  :group 'im-org-grabbability)

(defcustom im-org-grabbability-recency-days 14
  "Tasks created within this many days get the full recency bonus."
  :type 'integer
  :group 'im-org-grabbability)

(defcustom im-org-grabbability-age-penalty-days 180
  "Tasks older than this many days get the full age penalty."
  :type 'integer
  :group 'im-org-grabbability)

(defcustom im-org-grabbability-effort-cap-minutes 240
  "Effort values at or above this (in minutes) get zero effort-low score."
  :type 'integer
  :group 'im-org-grabbability)

(defcustom im-org-grabbability-next-keywords '("NEXT")
  "TODO keywords that indicate a triaged/ready task."
  :type '(repeat string)
  :group 'im-org-grabbability)

(defcustom im-org-grabbability-tag-scores '(("bugfix" . 15)
                                            ("urgent" . 15)
                                            ("quick"  . 10)
                                            ("blocked" . -20))
  "Alist of tags and their score contribution.
Positive values boost grabbability, negative values reduce it.
Scores from multiple matching tags are summed."
  :type '(alist :key-type string :value-type number)
  :group 'im-org-grabbability)

;;;; Main

(defun im-org-grabbability--weight (factor)
  "Return the weight for FACTOR from `im-org-grabbability-weights'."
  (or (alist-get factor im-org-grabbability-weights) 0))

(defun im-org-grabbability--parse-ts (ts-string)
  "Parse an org timestamp string TS-STRING and return a float time, or nil."
  (when (and ts-string (string-match org-ts-regexp0 ts-string))
    (let ((date-str (match-string 0 ts-string)))
      (condition-case nil
          (float-time (org-time-string-to-time date-str))
        (error nil)))))

(defun im-org-grabbability--effort-minutes ()
  "Return the Effort property of the current entry in minutes, or nil."
  (let ((effort (org-entry-get nil "Effort")))
    (when effort
      (org-duration-to-minutes effort))))

(defun im-org-grabbability--clocksum-minutes ()
  "Return the clocked time in minutes for the current entry, or 0."
  (or (get-text-property (point) :org-clock-minutes) 0))

(defun im-org-grabbability--checkbox-progress ()
  "Return (checked . total) for checkbox stats, or nil if none."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t t)))
          (checked 0) (total 0))
      (when (re-search-forward "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" end t)
        (setq checked (string-to-number (match-string 1))
              total (string-to-number (match-string 2)))
        (when (> total 0)
          (cons checked total))))))

(defun im-org-grabbability--content-length ()
  "Return the approximate content length (chars) of the entry body."
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t t))))
    (max 0 (- end beg))))

(defun im-org-grabbability--score ()
  "Compute a grabbability score for the org entry at point.
Higher score means more actionable/attractive to pick up."
  (let ((score 0.0)
        (now (float-time)))

    (let ((pri (org-entry-get nil "PRIORITY")))
      (when pri
        (let ((val (cond ((string= pri "A") 1.0)
                         ((string= pri "B") 0.5)
                         ((string= pri "C") 0.2)
                         (t 0.0))))
          (setq score (+ score (* val (im-org-grabbability--weight 'priority)))))))

    (let ((effort-min (im-org-grabbability--effort-minutes)))
      (when effort-min
        (setq score (+ score (im-org-grabbability--weight 'effort-set)))
        (let* ((capped (min effort-min im-org-grabbability-effort-cap-minutes))
               (ratio (/ (float capped) im-org-grabbability-effort-cap-minutes))
               (val (- 1.0 ratio)))
          (setq score (+ score (* val (im-org-grabbability--weight 'effort-low)))))))

    (let ((kw (org-get-todo-state)))
      (when kw
        (let ((val (if (member kw im-org-grabbability-next-keywords) 1.0 0.3)))
          (setq score (+ score (* val (im-org-grabbability--weight 'todo-keyword)))))))

    (let ((sched (org-entry-get nil "SCHEDULED")))
      (when sched
        (let ((sched-time (im-org-grabbability--parse-ts sched)))
          (when (and sched-time (<= sched-time now))
            (let* ((days-overdue (/ (- now sched-time) 86400.0))
                   (val (min 1.0 (/ days-overdue 7.0))))
              (setq score (+ score (* val (im-org-grabbability--weight 'scheduled)))))))))

    (let ((progress (im-org-grabbability--checkbox-progress)))
      (when progress
        (let* ((checked (car progress))
               (total (cdr progress))
               (ratio (/ (float checked) total)))
          (setq score (+ score (* ratio (im-org-grabbability--weight 'checkbox)))))))

    (let ((clocked (im-org-grabbability--clocksum-minutes)))
      (when (> clocked 0)
        (let ((val (min 1.0 (/ clocked 60.0))))
          (setq score (+ score (* val (im-org-grabbability--weight 'clocksum)))))))

    (let ((created (org-entry-get nil "CREATED_AT")))
      (when created
        (let ((created-time (im-org-grabbability--parse-ts created)))
          (when created-time
            (let ((age-days (/ (- now created-time) 86400.0)))
              (let* ((recency-ratio (max 0.0 (- 1.0 (/ age-days (float im-org-grabbability-recency-days)))))
                     (val recency-ratio))
                (setq score (+ score (* val (im-org-grabbability--weight 'recency)))))
              (when (> age-days im-org-grabbability-age-penalty-days)
                (let* ((excess (- age-days im-org-grabbability-age-penalty-days))
                       (penalty-ratio (min 1.0 (/ excess (float im-org-grabbability-age-penalty-days)))))
                  (setq score (+ score (* penalty-ratio (im-org-grabbability--weight 'age-penalty)))))))))))

    (let ((content-len (im-org-grabbability--content-length)))
      (when (> content-len 0)
        (let ((val (min 1.0 (/ (float content-len) 500.0))))
          (setq score (+ score (* val (im-org-grabbability--weight 'content)))))))

    (let ((tags (org-get-tags)))
      (when tags
        (setq score (+ score (im-org-grabbability--weight 'tags)))
        (dolist (tag tags)
          (let ((tag-score (alist-get tag im-org-grabbability-tag-scores nil nil #'string=)))
            (when tag-score
              (setq score (+ score tag-score)))))))

    (let ((id (org-entry-get nil "ID")))
      (when id
        (setq score (+ score (im-org-grabbability--weight 'id)))))

    score))

(defun im-org-grabbability-sort-key ()
  "Return a numeric sorting key for `org-sort-entries'.
Called with point at the beginning of an entry."
  (im-org-grabbability--score))

(defun im-org-grabbability--score-at (marker)
  "Compute grabbability score at MARKER position."
  (org-with-point-at marker
    (im-org-grabbability--score)))

(defun im-org-grabbability-compare (a b)
  "Compare two org headings A and B by grabbability.
A and B are markers pointing to org headings.
Return t if A is more grabbable than B."
  (> (im-org-grabbability--score-at a)
     (im-org-grabbability--score-at b)))

;;;###autoload
(defun im-org-grabbability-sort ()
  "Sort org entries at the current level by grabbability score (most grabbable first)."
  (interactive)
  (org-clock-sum)
  (org-sort-entries nil ?F #'im-org-grabbability-sort-key #'>=))

;;;; Footer

(provide 'im-org-grabbability)
;;; im-org-grabbability.el ends here
