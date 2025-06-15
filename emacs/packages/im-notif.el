;;; im-notif.el --- Notifications within posframe  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-notif.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: utility notificatins

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

;; TODOs:
;; - Offer alternative positions for alert to show up, like top-left,
;;   top-right, top-center, center, bottom-left, bottom...

;;; Code:

(require 'im)
(require 'im-async-await)
(require 'posframe)
(require 'alert)
(require 'transient)

;;;; Customization

(defgroup im-notif nil
  "Settings for `im-notif'.")

(defconst im-notif--severity-log-mapping
  '((urgent   . fatal)
    (high     . error)
    (moderate . warn)
    (normal   . info)
    (low      . debug)
    (trivial  . trace))
  "Mapping from notification severities to log severities.")

(defcustom im-notif-log-level 'trivial
  "Log level for im-notif.
By default it logs every notification (\\='trivial)."
  :type '(choice (const :tag "Urgent" urgent)
                 (const :tag "High" high)
                 (const :tag "Moderate" moderate)
                 (const :tag "Normal" normal)
                 (const :tag "Low" low)
                 (const :tag "Trivial" trivial))
  :set (lambda (symbol value)
         (set-default symbol value)
         (im-notif--log-set-level (alist-get value im-notif--severity-log-mapping))))

(defcustom im-notif-post-notify-hooks nil
  "Functions to run after showing the notification.
  Functions should take one argument where the argument is a plist of
  notification data."
  :type 'list)

;;;; Main

;;;;; im-notif

(defvar im-notif-blacklist-regexp nil)
(defvar im-notif-dnd nil)
(defvar im-notif--active '())
(defvar-local im-notif--notification-data nil)

(async-cl-defun im-notif (&rest data &key id title message duration margin (severity 'normal) &allow-other-keys)
  (setq title (propertize title 'face '(:weight bold)))
  (let ((bname (format "*notif-%s*"
                       (or id (format "%s-%s" (if title (im-string-url-case title) "") (random))))))

    (when (and (not im-notif-dnd)
               (not (and im-notif-blacklist-regexp
                         (s-matches?
                          (if (stringp im-notif-blacklist-regexp)
                              im-notif-blacklist-regexp
                            (eval im-notif-blacklist-regexp))
                          (concat (or title "") "\n" message)))))

      (let ((message (if (await (im-screen-sharing-now?))
                         "[REDACTED due to screensharing]"
                       message)))
        (posframe-show
         (with-current-buffer (get-buffer-create bname)
           (setq im-notif--notification-data
                 (thread-first
                   data
                   (map-insert :time (float-time))
                   (map-insert :id (substring-no-properties bname))))
           (current-buffer))
         :string
         (if margin
             (format "\n  %s  \n  %s  \n\n" title (s-trim (s-join "  \n" (--map (concat "  " it) (s-lines message)))))
           (format "%s\n%s" title message))
         :poshandler
         (lambda (info)
           (let ((posy (* (line-pixel-height)
                          (--reduce-from (+ acc
                                            (with-current-buffer it
                                              (count-lines (point-min) (point-max))))
                                         0
                                         (remove bname im-notif--active)))))
             (cons (- (plist-get info :parent-frame-width)
                      (plist-get info :posframe-width)
                      20)
                   (if (> posy 0)
                       (+ posy (+ 3 3 15))
                     30))))
         :border-width 3
         :max-height 10
         :min-width 30
         :max-width 80
         :border-color (pcase severity
                         ((or 'high 'urgent) "red3")
                         ('normal "yellow3")
                         (_ nil)))
        (push bname im-notif--active)

        ;; Clear the notification after a certain time, if requested
        (when duration
          (run-with-timer
           duration nil
           (lambda ()
             (posframe-hide bname)
             (push bname im-notif--active)
             (setq im-notif--active (delete bname im-notif--active)))))

        ;; Also use native notifications if Emacs is not focused
        (unless (frame-focus-state)
          (let ((alert-default-style
                 (im-when-on
                  :linux 'libnotify
                  :darwin 'osx-notifier)))
            (ignore-errors
              (alert message :title title :severity severity)))))
      ;; Send it to my phone but only if Emacs is idle for a certain
      ;; time
      (dolist (fn im-notif-post-notify-hooks)
        (funcall fn data)))))

;;;;; Interactive functions

;;;###autoload
(defun im-notif-clear-all (&optional delete?)
  (interactive "P")
  (--each
      (--filter (s-prefix? "*notif" (buffer-name it)) (buffer-list))
    (if delete?
        (posframe-delete it)
      (posframe-hide it)))
  (setq im-notif--active '()))

;;;###autoload
(defun im-dummy-notification ()
  (interactive)
  (im-notif :title (format "%s" (random)) :message (with-temp-buffer (spook) (buffer-string)) :margin t))

;;;###autoload
(defun im-notif-notifications ()
  "Display actions for the selected notification; open, delete or snooze."
  (interactive)
  (let ((notification (im-notif--select)))
    (empv--select-action "Act on notification"
      "Open" → (switch-to-buffer-other-window (plist-get notification :buffer-name))
      "Delete" → (progn (posframe-delete (plist-get notification :buffer-name)) (message ">> Deleted."))
      "Snooze" → (im-notif-snooze notification (im-notif--read-duration)))))

;;;###autoload
(defun im-notif-enable-dnd (seconds)
  "Enable DND mode for SECONDS."
  (interactive (list (im-notif--read-duration)))
  (setq im-notif-dnd t)
  (message ">> Disabling notifications for %s seconds." seconds)
  (run-with-timer seconds nil #'im-notif-disable-dnd))

;;;###autoload
(defun im-notif-disable-dnd ()
  "Disable DND mode."
  (interactive)
  (setq im-notif-dnd nil))

;;;###autoload
(defun im-notif-blacklist ()
  "Prompt the user to set or clear `im-notif-blacklist-regexp'.

Interactively reads a new blacklist expression from the minibuffer,
defaulting to the current value of `im-notif-blacklist-regexp'.  If the
input is blank, clears the blacklist by setting
`im-notif-blacklist-regexp' to nil.

If the input starts with \"(rx\", it is read as a Lisp regexp form;
otherwise, it is taken as a plain string regexp."
  (interactive)
  (let* ((pp-use-max-width t)
         (result (read-string
                  "Blacklist expr: "
                  (if (stringp im-notif-blacklist-regexp)
                      im-notif-blacklist-regexp
                    (s-trim (pp-to-string im-notif-blacklist-regexp))))))
    (setq im-notif-blacklist-regexp
          (if (s-blank? result)
              nil
            (if (s-prefix? "(rx" result)
                (car (read-from-string result))
              result)))))

;; TODO Add ack, like tmr? Maybe add ack option to im-notif
;; itself and call with ack within this function
(defun im-notif-snooze (notification seconds)
  "Schedule NOTIFICATION to reappear after SECONDS seconds."
  (interactive (list
      (im-notif--select)
      (im-notif--read-duration)))
  (run-with-timer
   seconds nil
   (lambda () (apply #'im-notif notification)))
  (message ">> You will be reminded about '%s' in %s seconds." (plist-get notification :title) seconds))

(defun im-notif-snooze-last ()
  "Interactively snooze the last notification for a duration read from the user."
  (interactive)
  (im-notif-snooze (car (im-notif-notifications-list)) (im-notif--read-duration)))

;;;;; Logging

;; See `im-notif-log-level' variable.

(log4e:deflogger "im-notif" "%t [%l] %m" "%H:%M:%S")
(im-notif--log-enable-logging)
(im-notif--log-set-level (alist-get im-notif-log-level im-notif--severity-log-mapping))

(defun im-notif--log-notif (data)
  (im-notif--log
   (im-tap (alist-get (plist-get data :severity) im-notif--severity-log-mapping 'info))
   (prin1-to-string data)))

(add-hook 'im-notif-post-notify-hooks #'im-notif--log-notif)

(defun im-notif-logs ()
  (interactive)
  (switch-to-buffer " *log4e-im-notif*"))

;;;;; Transient menu

(transient-define-prefix im-notif-menu ()
  "Manage notifications."
  [["Basic"
    ("l" "List" im-notif-notifications)
    ("c" "Clear all" im-notif-clear-all)
    ("L" "Logs" im-notif-logs)
    ("d" "Dummy notification" im-dummy-notification)]
   ["DND/Blacklist"
    ("e" "Enable DND" im-notif-enable-dnd)
    ("E" "Disable DND" im-notif-disable-dnd)
    ("z" "Edit blacklist" im-notif-blacklist)]
   ["Snooze"
    ("s" "Snooze last" im-notif-snooze-last)]])

;;;;; Utility

(defun im-notif-notifications-list ()
  "Return notification datas in sorted order.
  First one is the latest one."
  (thread-last
    (buffer-list)
    (--filter (string-prefix-p "*notif" (buffer-name it)))
    (--map (with-current-buffer it (when-let* ((x im-notif--notification-data)) (map-insert x :buffer-name (buffer-name)))))
    (--filter it)
    (--sort (> (plist-get it :time) (plist-get other :time)))))

(defun im-notif--format-notification (it)
  (format "%s | %s - %s"
          (format-time-string "%Y-%m-%d %H:%M" (plist-get it :time))
          (plist-get it :title)
          (plist-get it :message)))

(defun im-notif--select ()
  (im-completing-read
   "Select notification: "
   (im-notif-notifications-list)
   :formatter #'im-notif--format-notification
   :category 'im-notification
   :sort? nil))

(defun im-notif--read-duration ()
  "Ask the user to type a duration in a human-readable way.
  Return parsed seconds from users answer."
  (tmr--parse-duration (current-time) (tmr--read-duration)))

;;;; Footer

(provide 'im-notif)

;;; im-notif.el ends here
