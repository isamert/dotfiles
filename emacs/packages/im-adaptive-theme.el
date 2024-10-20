;;; im-adaptive-theme.el --- Change theme by day and night automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-adaptive-theme.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: utils

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

;; This package lets you define day and night themes and changes them
;; at sunrise and sunset with configurable offset.
;;
;; Usage:
;;
;;   (add-hook \\='after-init-hook #\\='im-adaptive-theme-enable)

;;; Code:

(require 'solar)
(require 'dash)
(require 'cl-macs)

;;;; Customization

(defgroup im-adaptive-theme nil
  "Settings for `im-adaptive-theme'."
  :group 'themes)

(defcustom im-adaptive-theme-sunrise-offset 1
  "How may hours after the sunrise should day theme kick in, can be negative."
  :type 'number
  :group 'im-adaptive-theme)

(defcustom im-adaptive-theme-sunset-offset -1
  "How may hours after the sunset should day theme kick in, can be negative."
  :type 'number
  :group 'im-adaptive-theme)

(defcustom im-adaptive-theme-day-themes
  '(tango)
  "List of themes that are suitable for day."
  :type 'number
  :group 'im-adaptive-theme)

(defcustom im-adaptive-theme-night-themes
  '(wombat)
  "List of themes that are suitable for night."
  :type 'number
  :group 'im-adaptive-theme)

(defcustom im-adaptive-theme-detect-geolocation-automatically
  nil
  "Use the geolocation information retrieved from ipinfo.io.
This changes the value of the following variables according to response:
  - `calendar-latitude'
  - `calendar-longitude'"
  :type 'boolean
  :group 'im-adaptive-theme)

(defcustom im-adaptive-theme-refresh-timezone t
  "Setting this to non-nil prevents time-zone change problems.
You may change time-zones without restarting Emacs, which may cause
issues with theme change hours.  This is a little hack to prevent it."
  :type 'boolean
  :group 'im-adaptive-theme)

;;;; Variables

(defvar im-adaptive-theme--next-timer nil)
(defvar im-adaptive-theme--day-timer nil)
(defvar im-adaptive-theme--night-timer nil)

;;;; Main

;;;###autoload
(defun im-adaptive-theme-enable (&optional only-enable)
  "Enable theme switching at day and night.
If ONLY-ENABLE is non-nil, then only set the timers and don't change the
theme right now."
  (interactive)
  (when im-adaptive-theme--next-timer
    (cancel-timer im-adaptive-theme--next-timer))
  (when im-adaptive-theme--day-timer
    (cancel-timer im-adaptive-theme--day-timer))
  (when im-adaptive-theme--day-timer
    (cancel-timer im-adaptive-theme--night-timer))
  (if im-adaptive-theme-detect-geolocation-automatically
      (im-adaptive-theme-detect-geolocation
       (lambda (_success?)
         (im-adaptive-theme--enable only-enable)))
    (im-adaptive-theme--enable only-enable)))

;;;###autoload
(defun im-adaptive-theme-reload ()
  "To get rid of some artifacts."
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (load-theme theme :no-confirm))))

(defun im-adaptive-theme-detect-geolocation (&optional on-finished)
  (im-request "ipinfo.io"
    :-on-success
    (lambda (data)
      (pcase-let ((`(,lat ,long) (string-split (alist-get 'loc data) ",")))
        (setq calendar-latitude (string-to-number lat))
        (setq calendar-longitude (string-to-number long)))
      (funcall on-finished t))
    :-on-error
    (lambda (_data)
      (message ">> im-adaptive-theme-detect-geolocation failed")
      (funcall on-finished nil))))

(defun im-adaptive-theme--enable (only-enable)
  (when im-adaptive-theme-refresh-timezone
    (set-time-zone-rule nil))
  (setq im-adaptive-theme--next-timer
        (run-at-time
         "24:10" nil
         (lambda () (im-adaptive-theme-enable :only-enable))))
  (cl-flet ((pick-and-load-theme-from
             (lst)
             (let ((theme (if (listp lst)
                              (nth (random (length lst)) lst)
                            lst)))
               (message ">> Loading %s theme..." theme)
               (mapc #'disable-theme custom-enabled-themes)
               (load-theme theme :no-confirm)
               (message ">> Loading %s theme...Done" theme))))
    (-let* ((((sunrise) (sunset) _daylight) (solar-sunrise-sunset (im-adaptive-theme--current-date)))
            (switch-to-day-hour (+ im-adaptive-theme-sunrise-offset sunrise))
            (switch-to-night-hour (+ im-adaptive-theme-sunset-offset sunset))
            (current-hour (string-to-number (format-time-string "%H"))))
      (unless only-enable
        (if (and (> current-hour switch-to-day-hour)
                 (< current-hour switch-to-night-hour))
            (pick-and-load-theme-from im-adaptive-theme-day-themes)
          (pick-and-load-theme-from im-adaptive-theme-night-themes)))
      (when (< current-hour switch-to-day-hour)
        (setq
         im-adaptive-theme--day-timer
         (run-at-time
          (im-adaptive-theme--hour-number-to-hour-string switch-to-day-hour)
          nil
          (lambda () (pick-and-load-theme-from im-adaptive-theme-day-themes)))))
      (when (< current-hour switch-to-day-hour)
        (setq
         im-adaptive-theme--night-timer
         (run-at-time
          (im-adaptive-theme--hour-number-to-hour-string switch-to-night-hour)
          nil
          (lambda () (pick-and-load-theme-from im-adaptive-theme-night-themes))))))))

;;;; Utils

(defun im-adaptive-theme--hour-number-to-hour-string (hour)
  "Convert given HOUR number to hour string, like  20.67 to \"20:40\"."
  (let* ((whole (truncate hour))
         (fraction (* 60 (- hour whole))))
    (format "%02d:%02d" whole (round fraction))))

(defun im-adaptive-theme--current-date ()
  "Return date in the format of \\='(MONTH DAY YEAR)."
  (mapcar #'string-to-number (string-split (format-time-string "%m,%d,%Y") ",")))

;;;; Footer

(provide 'im-adaptive-theme)

;;; im-adaptive-theme.el ends here
