;;; im-notif.el --- Notifications within posframe  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-notif.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (posframe "1.4.4") (transient "0.8.7"))
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
(require 'log4e)
(require 'ring)

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
         (when (functionp 'im-notif--log-set-level)
           (im-notif--log-set-level (alist-get value im-notif--severity-log-mapping)))))

(defcustom im-notif-post-notify-hooks nil
  "Functions to run after showing the notification.
Functions should take one argument where the argument is a plist of
notification data."
  :group 'im-notif
  :type 'hooks)

(defcustom im-notif-dnd-enabled-hooks '(im-notif--gnome-dnd-enable im-notif--macos-dnd-enable)
  "Functions to run after enabling the DND mode."
  :group 'im-notif
  :type 'hooks)

(defcustom im-notif-dnd-disabled-hooks '(im-notif--gnome-dnd-disable im-notif--macos-dnd-disable)
  "Functions to run after disabling the DND mode."
  :group 'im-notif
  :type 'hooks)

(defcustom im-notif-dnd-whitelist-regexp nil
  "Show matching notifications even if in DND."
  :group 'im-notif
  :type 'string)

(defcustom im-notif-dnd-whitelist-labels nil
  "Show notifications with matching labels even if in DND."
  :group 'im-notif
  :type '(repeat string))

(defcustom im-notif-dnd-labels nil
  "List of labels with DND enabled.
This can be controlled interactively by
`im-notif-enable-dnd-for-labels'."
  :group 'im-notif
  :type '(repeat string))

(defcustom im-notif-blacklist-regexp nil
  "Disable showing notifications that matches this regexp.
This can be controlled intreactively by `im-notif-blacklist'."
  :group 'im-notif
  :type 'string)

(defcustom im-notif-label-default-durations '()
  "An alist of (label . duration) pairs to assign default duration to labels.
Each element should be a cons cell where the car is a label (string) and
the cdr is the duration in seconds (number).

Notifications can have more than one label, first label that matches
wins.  If :duration is explicitly given while calling `im-notif', that
overrides everything."
  :type '(alist :key-type (choice symbol string)
                :value-type number)
  :group 'im-notif)

(defcustom im-notif-default-duration 5
  "Default notification duration in seconds.
Also see `im-notif-label-default-durations'."
  :type 'number
  :group 'im-notif)

;;;; Main

;;;;; im-notif

(defvar im-notif-dnd nil)
(defvar im-notif--active '())
(defvar im-notif--dnd-timer nil)
(defvar im-notif--last-notifications (make-ring 300))
(defvar-local im-notif--notification-data nil)

;;;###autoload
(async-cl-defun im-notif (&rest data &key id title message duration (margin t) (severity 'normal) labels source &allow-other-keys)
  "Displays a notification message.

DATA -- a property list of keyword arguments:

ID -- (optional) unique identifier for the notification.

MESSAGE -- content of the notification.

TITLE -- (optional) notification title.

DURATION -- (optional) display time in seconds.  If nil,
notification persists.

LABELS -- (optional) labels/tags for the notification.  Used for
filtering.

MARGIN -- (optional) margin for notification popup.

SEVERITY -- (optional, default: \\='normal) notification severity.

SOURCE -- (optional) origin or source of the notification.  This can
be a function or a buffer object."
  (setq title (propertize title 'face '(:weight bold)))
  (let* ((id (format "*notif-%s*"
                     (or id (format "%s-%s" (if title (im-string-url-case title) "") (random)))))
         (bname id)
         (source-buffer (current-buffer))
         (duration (or duration
                       (cl-loop for label in labels
                                for duration = (cdr (assoc label im-notif-label-default-durations))
                                when duration
                                return duration)
                       im-notif-default-duration))
         (blacklisted? (and im-notif-blacklist-regexp
                            (s-matches?
                             (if (stringp im-notif-blacklist-regexp)
                                 im-notif-blacklist-regexp
                               (eval im-notif-blacklist-regexp))
                             (concat (or title "") "\n" message))))
         (in-dnd? (or im-notif-dnd
                      ;; Check if any label has DND enabled
                      (and labels
                           (--any? (member it im-notif-dnd-labels) labels))))
         (dnd-whitelisted? (and
                            in-dnd? ; check only if we are in dnd right now
                            (or
                             (and
                              im-notif-dnd-whitelist-regexp
                              (or (s-matches? im-notif-dnd-whitelist-regexp message)
                                  (s-matches? im-notif-dnd-whitelist-regexp title)))
                             (-intersection im-notif-dnd-whitelist-labels labels))))
         (should-show? (or (not in-dnd?) dnd-whitelisted?))
         (notif-data (thread-first
                       data
                       (map-insert :id id)
                       (map-insert :time (float-time))
                       (map-insert :duration duration)
                       (map-insert :source-buffer source-buffer))))
    (ring-insert im-notif--last-notifications notif-data)
    (when (not blacklisted?)
      (when should-show?
        (when (frame-focus-state) ; Only show posframe if frame is focused
          (posframe-show
           (with-current-buffer (get-buffer-create bname)
             (setq im-notif--notification-data notif-data)
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
               ;; Only remove if Emacs is not idle
               (unless (and (current-idle-time)
                            (>= (time-to-seconds (current-idle-time)) duration))
                 (posframe-delete bname)
                 (setq im-notif--active (delete bname im-notif--active)))))))

        ;; Use native notifications if Emacs is not focused
        (unless (frame-focus-state)
          (let ((alert-default-style
                 (im-when-on
                  :linux 'libnotify
                  :darwin 'osx-notifier)))
            (ignore-errors
              (alert message :title title :severity severity)))))
      (dolist (fn im-notif-post-notify-hooks)
        (funcall fn data)))))

;;;;; Interactive functions

;;;###autoload
(defun im-notif-clear-all ()
  (interactive "P")
  (--each (--filter (s-prefix? "*notif" (buffer-name it)) (buffer-list))
    (posframe-delete it))
  (setq im-notif--active '()))

;;;###autoload
(defun im-dummy-notification ()
  (interactive)
  (im-notif :title (format "%s" (random))
            :message (with-temp-buffer (spook) (buffer-string))))

;;;###autoload
(defun im-notif-notifications ()
  "Display actions for the selected notification; open, delete or snooze."
  (interactive)
  (let ((notification (im-notif--select)))
    (empv--select-action "Act on notification"
      "Open" → (switch-to-buffer-other-window (plist-get notification :buffer-name))
      "Buffer (origin)" → (switch-to-buffer (plist-get notification :source-buffer))
      "Go to Source" → (im-notif-go-to-source notification)
      "Delete" → (progn
                   (ignore-errors
                     (posframe-delete (plist-get notification :id))) (message ">> Deleted."))
      "Snooze" → (im-notif-snooze notification (im-notif--read-duration)))))

;;;###autoload
(defun im-notif-enable-dnd (seconds)
  "Enable DND mode for SECONDS."
  (interactive (list (im-notif--read-duration)))
  (setq im-notif-dnd t)
  (setq im-notif--dnd-timer
        (run-with-timer seconds nil #'im-notif-disable-dnd))
  (message ">> Disabling notifications for %s seconds." seconds)
  (dolist (fn im-notif-dnd-enabled-hooks)
    (funcall fn)))

(defun im-notif--set-gnome-dnd (x)
  (when (and (eq system-type 'gnu/linux) (executable-find "gsettings"))
    (call-process "gsettings" nil nil nil
                  "set" "org.gnome.desktop.notifications" "show-banners" (if (eq x 'enabled)
                                                                             "false" "true"))))

(defun im-notif--gnome-dnd-enable ()
  (im-notif--set-gnome-dnd 'enabled))

(defun im-notif--gnome-dnd-disable ()
  (im-notif--set-gnome-dnd 'disabled))

(defun im-notif--set-macos-dnd (x)
  "Enable/disable MacOS Focus/DnD.
To make this work, you need to have the Shortcuts app and the following
shortcut installed:
https://www.icloud.com/shortcuts/65840b635c7d4073b4319c1ddabcdce5

Source: https://mskelton.dev/bytes/20230927123410"
  (when (eq system-type 'darwin)
    (with-temp-buffer
      (insert (if (eq x 'enabled)
                  "on" "off"))
      (call-process-region (point-min) (point-max)
                           "shortcuts" nil 0 nil
                           "run" "Focus"))))

(defun im-notif--macos-dnd-enable ()
  (im-notif--set-macos-dnd 'enabled))

(defun im-notif--macos-dnd-disable ()
  (im-notif--set-macos-dnd 'disabled))

;;;###autoload
(defun im-notif-disable-dnd ()
  "Disable DND mode."
  (interactive)
  (setq im-notif-dnd nil)
  ;; Cancel any pending timer if manually disabled
  (when (timerp im-notif--dnd-timer)
    (cancel-timer im-notif--dnd-timer)
    (setq im-notif--dnd-timer nil))
  (dolist (fn im-notif-dnd-disabled-hooks)
    (funcall fn))
  (message ">> DND disabled!"))

;;;###autoload
(defun im-notif-enable-dnd-for-labels ()
  "Enable DND mode for selected labels for a given duration."
  (interactive)
  (let* ((labels (im-notif--get-all-labels))
         (selected-labels (if labels
                              (completing-read-multiple "Select labels for DND: " labels)
                            (user-error "No labels found in current notifications")))
         (seconds (im-notif--read-duration)))
    (dolist (label selected-labels)
      (cl-pushnew label im-notif-dnd-labels :test #'equal))
    (message ">> DND enabled for labels: %s for %s seconds."
             (s-join ", " selected-labels)
             seconds)
    (run-with-timer seconds nil
                    (lambda (labels-to-remove)
                      (dolist (label labels-to-remove)
                        (setq im-notif-dnd-labels
                              (delete label im-notif-dnd-labels))))
                    selected-labels)))

;;;###autoload
(defun im-notif-disable-dnd-for-label (label)
  "Disable DND mode for a specific LABEL."
  (interactive
   (list (completing-read "Disable DND for label: "
                          im-notif-dnd-labels
                          nil t)))
  (setq im-notif-dnd-labels
        (delete label im-notif-dnd-labels))
  (message ">> DND disabled for label: %s" label))

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

(defun im-notif-go-to-source (notification)
  (let ((source (plist-get notification :source)))
    (cond
     ((functionp source) (funcall source))
     ((bufferp source) (switch-to-buffer (plist-get notification :source)))
     (t (message "Can't open: %s" source)))))

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

(defun im-notif-go-to-source-last ()
  "Go to source of the last notification."
  (interactive)
  (im-notif-go-to-source (car (im-notif-notifications-list))))

;;;;; Logging

;; See `im-notif-log-level' variable.

(unless (functionp 'im-notif--log)
  (log4e:deflogger "im-notif" "%t [%l] %m" "%H:%M:%S"))
(im-notif--log-enable-logging)
(im-notif--log-set-level (alist-get im-notif-log-level im-notif--severity-log-mapping))

(defun im-notif--log-notif (data)
  (im-notif--log
   (alist-get (plist-get data :severity) im-notif--severity-log-mapping 'info)
   (let ((print-level 2)
         (print-length 25))
     (prin1-to-string data))))

(add-hook 'im-notif-post-notify-hooks #'im-notif--log-notif)

(defun im-notif-logs ()
  (interactive)
  (switch-to-buffer " *log4e-im-notif*"))

;;;;; Transient menu

(transient-define-prefix im-notif-menu ()
  "Manage notifications."
  [:description
   (lambda ()
     (format
      "%s: %s"
      (propertize "Do Not Disturb" 'face '(:weight bold))
      (propertize (if im-notif-dnd "Enabled" "Disabled") 'face `(:foreground ,(if im-notif-dnd "red" "green")))))
   "───"]
  [["Basic"
    ("l" "List" im-notif-notifications)
    ("c" "Clear all" im-notif-clear-all)
    ("L" "Logs" im-notif-logs)
    ("d" "Dummy notification" im-dummy-notification)]
   ["DND/Blacklist"
    ("e" "Enable DND" im-notif-enable-dnd)
    ("E" "Disable DND" im-notif-disable-dnd)
    ("D" "Enable DND for labels" im-notif-enable-dnd-for-labels)
    ("x" "Disable DND for label" im-notif-disable-dnd-for-label)
    ("z" "Edit blacklist" im-notif-blacklist)]
   ["Other"
    ("s" "Snooze last" im-notif-snooze-last)
    ("g" "Go to source last" im-notif-go-to-source-last)]])

;;;;; Utility

(defun im-notif--get-all-labels ()
  "Return a list of all unique labels from current notifications."
  (thread-last
    (im-notif-notifications-list)
    (--map (plist-get it :labels))
    (--filter it)
    (-flatten)
    (-uniq)
    (--sort (string< it other))))

(defun im-notif-notifications-list ()
  "Return notification datas in sorted order.
First one is the latest one."
  (ring-elements im-notif--last-notifications))

(defun im-notif--format-notification (it)
  (format "%s │ ✍️ %s 📰 %s%s"
          (format-time-string "%Y-%m-%d %H:%M" (plist-get it :time))
          (plist-get it :title)
          (plist-get it :message)
          (if-let* ((labels (plist-get it :labels)))
              (concat " #️⃣ " (s-join " " (--map (format "#%s" it) labels)))
            "")))

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
