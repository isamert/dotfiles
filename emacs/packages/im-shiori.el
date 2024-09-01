;;; im-shiori.el --- Shiori integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
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

;; Shiori integration for Emacs.
;;
;; Offers `im-shiori-add-bookmark' function to add bookmark to shiori
;; and `im-shiori-enable-elfeed-support' for adding some entries from
;; elfeed to Shiori automatically.

;;; Code:

(require 'dash)
(require 's)
(require 'async-await)
(require 'im-async-await)

;;;; Customization

(defgroup im-shiori nil
  "Settings for `im-shiori'."
  :group 'bookmarks)

(defcustom im-shiori-url nil
  "Shiori URL, like my-shiori-server.com."
  :type 'string)

(defcustom im-shiori-username nil
  "Shiori username."
  :type 'string)

(defcustom im-shiori-password nil
  "Shiori password."
  :type 'string)

(defcustom im-shiori-elfeed-tags '()
  "List of tags that triggers addition of Elfeed entry to Shiori."
  :type 'list)

;;;; Variables

(defvar im-shiori-token nil)
(defvar im-shiori-token-expire-date 0)

;;;; Main

(async-defun im-shiori-login-ensure ()
  (if (> im-shiori-token-expire-date (float-time))
      (await t) ;; Need to do this, otherwise promise is not resolved for some reason
    (let-alist (await (im-request
                        (format "%s/api/v1/auth/login" im-shiori-url)
                        :-type "POST"
                        :-async? t
                        :-headers '(("Content-Type" . "application/json"))
                        :-data `(:username ,im-shiori-username
                                 :password ,im-shiori-password
                                 :remember_me t)))
      (setq im-shiori-token .message.token)
      (setq im-shiori-token-expire-date .message.expires))))

;;;###autoload
(async-cl-defun im-shiori-add-bookmark
    (url &key
         (title "")
         (excerpt "")
         (tags '())
         create-archive?
         create-ebook?
         public?)
  (interactive (list
      (read-string "URL: ")
      :title (read-string "Title (leave empty to fill auto): ")
      :tags (mapcar #'s-trim (s-split "," (read-string "Tags (comma separated): ")))
      :create-archive? (y-or-n-p "Create archive? ")))
  (await (im-shiori-login-ensure))
  (await (im-request
           (format "%s/api/bookmarks" im-shiori-url)
           :-type "POST"
           :-async? t
           :-raw t
           :-headers `(:Content-Type "application/json"
                       :Authorization ,(format "Bearer %s" im-shiori-token))
           :-data `(:create_archive ,create-archive?
                    :create_ebook ,create-ebook?
                    :public ,(if public? 1 0)
                    :excerpt ,excerpt
                    :tags ,(--map (list (cons "name" it)) tags)
                    :title ,title
                    :url ,url)))
  (message ">> shiori :: Added %s" url))

;;;###autoload
(defun im-shiori-enable-elfeed-support ()
  "When one of `im-shiori-elfeed-tags' added to an elfeed entry, add it to Shiori."
  (add-hook 'elfeed-tag-hooks #'im-shiori--on-elfeed-entries))

(declare-function elfeed-entry-title "elfeed-db")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-tags "elfeed-db")
(async-defun im-shiori--on-elfeed-entries (entries tags)
  ;; Doing in reverse to append in date order
  (dolist (entry (seq-reverse entries))
    (when (--any? (-contains? tags it) im-shiori-elfeed-tags)
      (message ">> Adding %s to Shiori..." (elfeed-entry-title entry))
      (await (im-shiori-add-bookmark
              (elfeed-entry-link entry)
              :title (elfeed-entry-title entry)
              :tags (-uniq (mapcar
                            #'symbol-name
                            (append (elfeed-entry-tags entry)
                                    tags
                                    '(unread elfeed))))
              :create-archive? t)))))

;;;; Footer

(provide 'im-shiori)

;;; im-shiori.el ends here
