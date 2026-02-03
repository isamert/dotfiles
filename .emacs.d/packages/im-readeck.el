;;; im-readeck.el --- Readeck integration -*- lexical-binding: t; -*-

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

;; Readeck integration for Emacs.
;;
;; Offers `im-readeck-add-bookmark' function to add bookmark to readeck
;; and `im-readeck-enable-elfeed-support' for adding some entries from
;; elfeed to Readeck automatically.

;;; Code:

(require 'dash)
(require 's)
(require 'async-await)
(require 'im-async-await)

;;;; Customization

(defgroup im-readeck nil
  "Settings for `im-readeck'."
  :group 'bookmarks)

(defcustom im-readeck-url nil
  "Readeck URL, like my-readeck-server.com."
  :type 'string)

(defcustom im-readeck-token nil
  "Readeck token."
  :type 'string)

(defcustom im-readeck-elfeed-tags '()
  "List of tags that triggers addition of Elfeed entry to Readeck."
  :type 'list)

;;;; Variables

(defvar im-readeck-token nil)

;;;; Main

;;;###autoload
(async-cl-defun im-readeck-add-bookmark
    (url &key
         (title "")
         (labels '()))
  (interactive (list
      (read-string "URL: ")
      :title (read-string "Title (leave empty to fill auto): ")
      :labels (mapcar #'s-trim (s-split "," (read-string "Tags (comma separated): ")))))
  (await (im-request
           (format "%s/api/bookmarks" im-readeck-url)
           :-type "POST"
           :-async? t
           :-raw t
           :-headers `(:Content-Type "application/json"
                       :Authorization ,(format "Bearer %s" im-readeck-token))
           :-data `(:labels ,labels
                    :title ,title
                    :url ,url)))
  (message ">> readeck :: Added %s" url))

;;;###autoload
(defun im-readeck-enable-elfeed-support ()
  "When one of `im-readeck-elfeed-tags' added to an elfeed entry, add it to Readeck."
  (add-hook 'elfeed-tag-hooks #'im-readeck--on-elfeed-entries))

(declare-function elfeed-entry-title "elfeed-db")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-tags "elfeed-db")
(async-defun im-readeck--on-elfeed-entries (entries tags)
  ;; Doing in reverse to append in date order
  (dolist (entry (seq-reverse entries))
    (when (--any? (-contains? tags it) im-readeck-elfeed-tags)
      (message ">> Adding %s to Readeck..." (elfeed-entry-title entry))
      (await (im-readeck-add-bookmark
              (elfeed-entry-link entry)
              :title (elfeed-entry-title entry)
              :labels (-uniq (mapcar
                              #'symbol-name
                              (append (elfeed-entry-tags entry)
                                      tags
                                      '(unread elfeed)))))))))

(defun im-readeck-bookmark-highlights (id)
  (im-request
    (format "%s/api/bookmarks/%s/annotations" im-readeck-url id)
    :-type "GET"
    ;; :-async? t
    ;; :-raw t
    :-headers `(:Content-Type "application/json"
                :Authorization ,(format "Bearer %s" im-readeck-token))))

;;;; Footer

(provide 'im-readeck)

;;; im-readeck.el ends here
