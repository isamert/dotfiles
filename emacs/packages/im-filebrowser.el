;;; im-filebrowser.el --- Filebrowser integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: files

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

;; Filebrowser integration.

;; This essentially let's me select a file through `find-file' and
;; then get a public link for it to share using Filebrowser[^1].
;;
;; [^1]: https://github.com/filebrowser/filebrowser

;;; Code:

(require 'cl-lib)
(require 's)

;;;; Customization

;; TODO: Use auth sources.

(defcustom im-filebrowser-username nil
  "Filebrowser username."
  :group 'im-filebrowser
  :type 'string)

(defcustom im-filebrowser-password nil
  "Filebrowser password."
  :group 'im-filebrowser
  :type 'string)

(defcustom im-filebrowser-url "https://isamert.duckdns.org/filebrowser"
  "Filebrowser url."
  :group 'im-filebrowser
  :type 'url)

(defcustom im-filebrowser-base-path nil
  "Filebrowser base file selection path.
This will be stripped from the beginning of the filepath while sending a
request to Filebrowser."
  :group 'im-filebrowser
  :type 'path)

;;;; Variables

(defvar im-filebrowser--token nil
  "The token.
See `im-filebrowser-ensure-login'.")

;;;; Main

;;;###autoload
(cl-defun im-filebrowser-share (file &key password expires unit)
  "Share FILE.
PASSWORD can be empty string, which means no password.  UNIT is either
seconds, minutes, hours or days.  EXPIRES is a string containing a
number for the UNIT."
  (interactive
   (let ((fname (read-file-name "File: " im-filebrowser-base-path))
         (duration (im-filebrowser--read-duration)))
     (list
      (url-encode-url (s-chop-prefix im-filebrowser-base-path fname))
      :expires (car duration)
      :unit (nth 1 duration)
      :password (read-passwd "File password: "))))
  (unless im-filebrowser--token
    (im-filebrowser-ensure-login))
  (im-request
    (concat im-filebrowser-url "/api/share/" file)
    :-headers `(:X-Auth ,im-filebrowser--token)
    :-type "POST"
    :expires expires :unit unit
    :-data `(:password ,(or password "") :expires ,expires :unit ,unit)
    :-on-error
    (lambda (status)
      (if (equal status 401)
          (progn
            (im-filebrowser-ensure-login)
            (im-filebrowser-share
             file
             :password password
             :expires expires
             :unit unit))
        (error "Request failed with %s" status)))
    :-on-success
    (lambda (data)
      (let ((direct-link (format
                          "%s/api/public/dl/%s%s"
                          im-filebrowser-url
                          (alist-get 'hash data)
                          (alist-get 'path data)))
            (share-link (format
                         "%s/share/%s"
                         im-filebrowser-url
                         (alist-get 'hash data))))
        (if (equal ?d (car (read-multiple-choice "Select link type: " '((?d "direct") (?s "share")))))
            (im-kill direct-link)
          (im-kill share-link))))))

(defun im-filebrowser-ensure-login ()
  "Login with username and password to Filebrowser."
  (setq
   im-filebrowser--token
   (im-request
     (format "%s/api/login" im-filebrowser-url)
     :-raw t
     :-type "POST"
     :-data `(:username ,im-filebrowser-username
              :password ,im-filebrowser-password
              :recaptcha ""))))

;;;;; Utils

(defun im-filebrowser--read-duration ()
  (let* ((input (read-string "Enter duration (e.g., 1m, 2h): "))
         (number (substring input 0 -1))
         (unit (substring input -1)))
    (list number (pcase unit
                   ("m" "minutes")
                   ("h" "hours")
                   ("s" "seconds")
                   (_ (error "filebrowser :: Invalid duration"))))))

;;;; Footer

(provide 'im-filebrowser)

;;; im-filebrowser.el ends here
