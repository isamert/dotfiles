;;; im-svgcal.el --- SVG Calendar -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-timegrid)
(require 'dash)
(require 'org)
(require 's)
(require 'hide-mode-line)

;; TODO Generate from {agenda,current buffer,current restriction} etc.
;; TODO Make weekly view
;; TODO Add links to headers if possible.
;; TODO Strip links in titles
;; TODO Add alt texts (like :alt or :details)
;; TODO Make it shareable (day view or week view, as svg file to someone else)

(defconst im-svgcal--buffer-name "*svgcal*"
  "Buffer name to display svgcal inside.")

(defvar im-svgcal--last-entries nil)

(defvar im-svgcal--timer nil
  "Timer that updates the *svgcal* buffer.")

(defvar im-svgcal--source-buffer nil
  "Buffer that is used to generate svgcal.")

;;;###autoload
(defun im-svgcal-enable ()
  "Enable svgcal."
  (interactive)
  (im-svgcal-disable)
  (setq im-svgcal--source-buffer (current-buffer))
  (add-hook 'after-save-hook #'im-svgcal-run nil 'local)
  (im-svgcal-run)
  (select-window (get-buffer-window im-svgcal--source-buffer))
  (setq
   im-svgcal--timer
   (run-at-time
    1 300
    (lambda ()
      (if (get-buffer im-svgcal--source-buffer)
          (im-svgcal-render-buffer)
        (im-svgcal-disable))))))

;;;###autoload
(defun im-svgcal-disable ()
  "Disable svgcal."
  (interactive)
  (when im-svgcal--timer
    (cancel-timer im-svgcal--timer)
    (setq im-svgcal--timer nil))
  (when (get-buffer im-svgcal--buffer-name)
    (kill-buffer im-svgcal--buffer-name))
  (when (and im-svgcal--source-buffer (buffer-live-p im-svgcal--source-buffer))
    (with-current-buffer im-svgcal--source-buffer
      (remove-hook 'after-save-hook #'im-svgcal-run 'local)
      (setq im-svgcal--last-entries nil))))

;;;###autoload
(defun im-svgcal-run ()
  (interactive)
  (when (and (buffer-narrowed-p)
             (equal "notes" (im-tab-bar-current-tab-name)))
    (im-svgcal--set-entries)
    (unless (im-buffer-visible-p im-svgcal--buffer-name)
      (let ((orig-win (selected-window)))
        (im-display-buffer-in-side-window (get-buffer-create im-svgcal--buffer-name) 35)
        (set-window-dedicated-p (get-buffer-window im-svgcal--buffer-name) t)
        (select-window orig-win)))
    (im-svgcal-render-buffer)))

(defun im-svgcal--set-entries ()
  (setq im-svgcal--last-entries
        (->>
         (org-map-entries
          (lambda ()
            (-map
             (-lambda ((range start end))
               (list
                :title (org-entry-get nil "ITEM")
                :done? (equal (org-entry-get nil "TODO") "DONE")
                :range range
                :start start
                :end end))
             (im-svgcal--org-entry-timestamps))))
         (-flatten-n 1)
         (--filter (plist-get it :end))))
  (setq im-svgcal--last-entries
        (append im-svgcal--last-entries (im-svgcal-diary-todays-entries))))

(defun im-svgcal-render-buffer ()
  "Refresh the read-only calendar using the last collected entries."
  (with-current-buffer (get-buffer-create im-svgcal--buffer-name)
    (let ((image (im-svgcal-render im-svgcal--last-entries))
          (inhibit-read-only t))
      (erase-buffer)
      (setq-local cursor-type nil)
      (when (boundp 'evil-normal-state-cursor)
        (setq-local evil-normal-state-cursor '(nil)))
      (insert-image image)
      (goto-char (point-max))
      (hide-mode-line-mode 1)
      image)))

(defun im-svgcal--time-minute (time)
  "Convert HH:MM string TIME to minutes on the calendar's overnight axis.
Times before 07:00 belong to the following morning."
  (let* ((parts (mapcar #'string-to-number (split-string time ":")))
         (minute (+ (* 60 (car parts)) (cadr parts))))
    (if (< minute (* 7 60)) (+ minute 1440) minute)))

(defun im-svgcal--task-block (task id)
  "Convert TASK to a read-only org-timegrid block with unique ID."
  (let* ((start (im-svgcal--time-minute (plist-get task :start)))
         (end (im-svgcal--time-minute (plist-get task :end))))
    (org-timegrid-block-create
     :id id :day 0 :time-kind 'timed
     :start start :end (if (< end start) (+ end 1440) end)
     :title (im-org-header-line-to-title (plist-get task :title))
     :done (plist-get task :done?))))

(defun im-svgcal-render (tasks)
  "Return an org-timegrid day image for TASKS.
Keep the original 07:00 through 03:00 overnight range.  org-timegrid
owns the grid, theme colours, overlap layout, titles and clock line."
  (let* ((window (get-buffer-window im-svgcal--buffer-name t))
         (width (if window (window-body-width window t) 310))
         (blocks (cl-loop for task in tasks
                          for id from 0
                          collect (im-svgcal--task-block task id))))
    (org-timegrid-day-image
     blocks (* 7 60) (* 27 60) width
     (im-svgcal--time-minute (format-time-string "%H:%M")))))

(defun im-svgcal-diary-todays-entries ()
  "Return diary entries for today in svgcal format.
Filters out entries ending with '!' (a way to hide some entries
from svgcal)."
  (save-window-excursion
    (let ((diary-display-function #'diary-simple-display))
      (->>
       (diary-list-entries (calendar-current-date) 1)
       (--map (-let* (((range title) (s-split " " (s-trim (nth 1 it))))
                      ((start end) (s-split "-" range)))
                (when (and end (not (s-suffix? "!" title)))
                  (list :title title :range range :start start :end end))))
       (-filter #'identity)))))

(defun im-svgcal--org-entry-timestamps ()
  "Get all timestamps of current entry.

>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\"))
>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55><2023-07-24 Sun 00:20-00:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\") (\"00:20-00:55\" \"00:20\" \"00:55\"))
>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55><2023-07-24 Sun 00:20-00:55>\\n** Another\\n<2023-07-23 Sun 13:20-13:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\") (\"00:20-00:55\" \"00:20\" \"00:55\"))"
  (->>
   (save-restriction
     (save-mark-and-excursion
       (widen)
       (org-narrow-to-subtree)
       ;; I was using (org-element-at-point) and it was returning
       ;; :contents-{begin,end} properties but after I upgraded
       ;; emacs/org it started not to return them for some reason even
       ;; though the function documentation says that :{begin,end}
       ;; should be there.
       (buffer-substring-no-properties (point-min) (point-max))))
   (s-lines)
   (-drop 1) ;; drop header
   (--take-while (not (s-matches? "^\\*" it))) ;; take until next header (unnecessary)
   (s-join "\n")
   (s-match-strings-all "\\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)")))

(provide 'im-svgcal)
;;; im-svgcal.el ends here
