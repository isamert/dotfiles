;;; im-wpm.el --- Track typing speed in insert mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))

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

;; This package provides a minor mode that tracks your typing speed
;; (words per minute) while in Evil insert mode.  It uses a sliding
;; window approach, looking at keystrokes within the last N seconds
;; (configurable via `im-wpm-window-seconds') to calculate a live WPM
;; value.
;;
;; The current WPM is displayed in the mode line while you are typing
;; in insert mode.  When you leave insert mode, a summary message is
;; shown in the echo area.
;;
;; Usage:
;;   (im-wpm-mode 1)
;;
;; WPM is calculated by dividing the number of characters typed by
;; `im-wpm-chars-per-word' (default 5, the standard "word" length) and then
;; scaling to a per-minute rate based on the elapsed time within the sliding window.


;;; Code:

;;;; Customization

(defgroup im-wpm nil
  "Settings for `im-wpm'.")

(defcustom im-wpm-window-seconds 15
  "Number of seconds to look back for WPM calculation."
  :type 'integer
  :group 'im-wpm)

(defcustom im-wpm-chars-per-word 5
  "Number of characters considered as one word for WPM calculation."
  :type 'integer
  :group 'im-wpm)

;;;; Variables

(defvar im-wpm--keystrokes nil
  "List of timestamps for each keystroke in the current insert session.")

(defvar im-wpm--timer nil
  "Timer for updating WPM display.")

(defvar im-wpm--current-wpm 0
  "Current words per minute.")

;;;; Main

(defvar evil-state nil)

(defun im-wpm--prune-keystrokes ()
  "Remove keystrokes older than `im-wpm-window-seconds'."
  (let ((cutoff (time-subtract (current-time) im-wpm-window-seconds)))
    (setq im-wpm--keystrokes
          (seq-filter (lambda (ts) (time-less-p cutoff ts))
                      im-wpm--keystrokes))))

(defun im-wpm--on-char ()
  "Called after each self-insert command to record timestamp."
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
    (push (current-time) im-wpm--keystrokes)))

(defun im-wpm--update-wpm ()
  "Recalculate WPM based on the sliding window."
  (if (and im-wpm--keystrokes
           (bound-and-true-p evil-mode)
           (eq evil-state 'insert))
      (progn
        (im-wpm--prune-keystrokes)
        (let* ((count (length im-wpm--keystrokes))
               (words (/ (float count) (float im-wpm-chars-per-word)))
               ;; Time span is either the window size or time since first
               ;; keystroke in window, whichever is smaller
               (oldest (car (last im-wpm--keystrokes)))
               (elapsed (float-time (time-subtract (current-time) oldest)))
               (minutes (/ (min elapsed (float im-wpm-window-seconds)) 60.0)))
          (if (> minutes 0.0)
              (setq im-wpm--current-wpm (round (/ words minutes)))
            (setq im-wpm--current-wpm 0))))
    (setq im-wpm--current-wpm 0)))

(defun im-wpm--enter-insert ()
  "Reset state when entering insert mode."
  (setq im-wpm--keystrokes nil
        im-wpm--current-wpm 0))

(defun im-wpm--exit-insert ()
  "Display final WPM when leaving insert mode."
  (im-wpm--update-wpm)
  (let ((count (length im-wpm--keystrokes)))
    (when (> count 0)
      (message "Typing speed: %d WPM (%d chars in window)"
               im-wpm--current-wpm count)))
  (setq im-wpm--keystrokes nil
        im-wpm--current-wpm 0))

(defun im-wpm--modeline ()
  "Return a string for the mode line showing current WPM."
  (if (and (bound-and-true-p evil-mode)
           (eq evil-state 'insert)
           im-wpm--keystrokes)
      (format " WPM:%d" im-wpm--current-wpm)
    ""))

;;;###autoload
(define-minor-mode im-wpm-mode
  "Minor mode to track typing speed (WPM) in Evil insert mode.
Uses a sliding window of `im-wpm-window-seconds' seconds."
  :lighter (:eval (im-wpm--modeline))
  :global t
  (if im-wpm-mode
      (progn
        (add-hook 'post-self-insert-hook #'im-wpm--on-char)
        (add-hook 'evil-insert-state-entry-hook #'im-wpm--enter-insert)
        (add-hook 'evil-insert-state-exit-hook #'im-wpm--exit-insert)
        (setq im-wpm--timer
              (run-with-timer 1 1 #'im-wpm--update-wpm))
        (message "WPM tracker enabled (window: %ds, chars/word: %d)" im-wpm-window-seconds im-wpm-chars-per-word))
    (remove-hook 'post-self-insert-hook #'im-wpm--on-char)
    (remove-hook 'evil-insert-state-entry-hook #'im-wpm--enter-insert)
    (remove-hook 'evil-insert-state-exit-hook #'im-wpm--exit-insert)
    (when im-wpm--timer
      (cancel-timer im-wpm--timer)
      (setq im-wpm--timer nil))
    (setq im-wpm--keystrokes nil)
    (message "WPM tracker disabled")))

;;;; Footer

(provide 'im-wpm)

;;; im-wpm.el ends here
