;;; im-nextcloud.el --- Basic NextCloud Talk client  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: utility cloud

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

;; This package provides an Emacs interface for interacting with
;; various Nextcloud services, including Talk, Files, Contacts, and
;; Maps.  It allows users to:
;;
;; - List and view Nextcloud Talk chat rooms (read-only)
;; - Download/view files (from chats)
;; - Synchronize Nextcloud contacts through org-mode
;; - Synchronize Nextcloud Maps favorites through org-mode
;;
;; TODO:
;;
;; - Make chats interactive (send/receive basic messages)

;;; Code:

(require 'im)
(require 'request)
(require 's)
(require 'dash)
(require 'vtable)
(require 'cus-edit)
(require 'async-await)

;;;; Customization

(defgroup im-nextcloud nil
  "Settings for `im-nextcloud'.")

(defcustom im-nextcloud-url nil
  "NextCloud instance URL, like https://nextcloud.somewhere.com/."
  :type 'url)

(defcustom im-nextcloud-user nil
  "NextCloud user name."
  :type 'url)

(defcustom im-nextcloud-auth nil
  "Authorization header value.
If you are using app passwords, then you need to do the following:

  (setq im-nextcloud-auth (base64-encode-string (concat im-nextcloud-user \":\" YOUR-APP-PASSWORD)))"
  :type 'string)

;;;; Main

;; TODO: Some of the functions use `im-nextcloud-request' and some
;; don't. Make them all use this one.

(cl-defun im-nextcloud-request (endpoint &key json? type success data)
  (request
    (format "%s%s" im-nextcloud-url endpoint)
    :headers `(("Authorization" . ,(concat "Basic " im-nextcloud-auth))
               ,@(when json?
                   '(("Accept" . "application/json")))
               ,@(when (and json? data)
                   '(("Content-Type" . "application/json")))
               ,@(when (s-contains? "/ocs/" endpoint)
                   '(("OCS-APIRequest" . "true"))))
    :type (or type (if data "POST" "GET"))
    :data (json-serialize data)
    :parser (when json?
              (apply-partially #'json-parse-buffer :object-type 'alist :array-type 'list))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall success (let-alist data .ocs.data))))
    :error (cl-function
            (lambda (&key data symbol-status error-thrown &allow-other-keys)
              (error "NextCloud request failed: error-thrown=%s, status=%s, data=%s"
                     error-thrown symbol-status data)))))

;;;; Talk

;;;###autoload
(defun im-nextcloud-talk-list-rooms ()
  "List all Nextcloud Talk rooms using vtable."
  (interactive)
  (im-nextcloud-request
   "/ocs/v2.php/apps/spreed/api/v4/room"
   :json? t
   :success
   (lambda (rooms)
     (let ((buf (get-buffer-create "*nextcloud-talk: rooms*")))
       (with-current-buffer buf
         (read-only-mode +1)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (make-vtable
            :row-colors (im-vtable--pretty-colors)
            :column-colors (im-vtable--pretty-colors)
            :columns '("Name" "Last Message")
            :objects (--sort
                      (> (let-alist it .lastMessage.timestamp)
                         (let-alist other .lastMessage.timestamp))
                      rooms)
            :getter (lambda (object column vtable)
                      (let-alist object
                        (pcase (vtable-column vtable column)
                          ("Name" .displayName)
                          ("Last Message" (s-truncate 100 (s-replace "\n" "…" .lastMessage.message))))))
            :actions `("RET" im-nextcloud-talk-open-chat)))
         (switch-to-buffer buf))))))

(defun im-nextcloud-talk-open-chat (room)
  (im-nextcloud-request
   (format "/ocs/v2.php/apps/spreed/api/v1/chat/%s?lookIntoFuture=0" (alist-get 'token room))
   :json? t
   :success
   (lambda (messages)
     (let ((buf (get-buffer-create (format "*nextcloud-talk: %s*" (alist-get 'displayName room)))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (--each (reverse messages)
             (let-alist it
               (insert "[" .actorId "]: " .message "\n")
               (when .messageParameters.file
                 (insert (format "[%s](%s) (%s)  "
                                 .messageParameters.file.name
                                 .messageParameters.file.link
                                 .messageParameters.file.mimetype))
                 (insert-button
                  "Download..."
                  'action
                  (lambda (_button)
                    (im-nextcloud-download-file
                     .messageParameters.file.id
                     :name .messageParameters.file.name))
                  'face custom-button
                  'follow-link t))
               (insert "\n\n")))
           (markdown-view-mode)
           (page-break-lines-mode)
           ;; (font-lock-add-keywords nil '(("^\\[[a-zA-Z_\\.-]+\\]: " . font-lock-function-name-face)) t)
           (switch-to-buffer buf)
           (goto-char (point-max))))))))

;;;; Files

(cl-defun im-nextcloud-download-file (file-id &key name)
  "Download FILE-ID to temp directory and open.
If NAME is given, then use it as suffix to the temp file."
  (im-nextcloud-request
   "/ocs/v2.php/apps/dav/api/v1/direct"
   :json? t
   :data `(:fileId ,file-id
           :expirationTime 100)
   :success
   (lambda (file)
     (let ((url (alist-get 'url file))
           (file (make-temp-file "image" nil (or name ".dat"))))
       (url-copy-file url file t)
       (find-file file)))))

;;;; Contacts

(cl-defun im-nextcloud-put-contact (vcard &key on-success on-error)
  "Add given VCARD definition to my Nextcloud contacts."
  (let ((uuid (->>
               vcard
               (s-split "\n")
               (--find (s-prefix? "UID" it))
               (s-split ":")
               -last-item)))
    (request (format
              "%s/remote.php/dav/addressbooks/users/%s/contacts/%s.vcf"
              im-nextcloud-url im-nextcloud-user uuid)
      :headers `(("Content-Type" . "text/vcard; charset=utf-8;")
                 ("Authorization" . ,(concat "Basic " im-nextcloud-auth)))
      :type "PUT"
      :data vcard
      :success (lambda (&rest _) (when on-success (funcall on-success)))
      :error (lambda (&rest _) (when on-error (funcall on-error _))))))

;;;;; Org mode integration

;; TODO: This does not delete the contact if header is removed
;; completely. I probably need something like
;; `im-org-header-deleted-hook', akin to `im-org-header-changed-hook'.
(defun im-contacts--update-contact-on-change (info)
  "Whenever a contact changes in people.org, push changes to Nextcloud.
This is done by adding this function to `im-org-header-changed-hook'.
See down below."
  (when (s-ends-with? "people.org" (buffer-file-name))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (plist-get info :begin))
        (when-let* ((contact (im-contacts-build-vcard-for-heading)))
          ;; Save buffer in case of
          ;; `im-contacts-build-vcard-for-heading' adds an ID to the
          ;; heading
          (let ((before-save-hook nil)
                (after-save-hook nil))
            (save-buffer))
          (im-nextcloud-put-contact
           contact
           :on-success (lambda (&rest _) (message ">> Contact updated: %s" (plist-get info :header)))
           :on-error (lambda (&rest _) (message "!! Failed to update contact: %s. Reason: %s" (plist-get info :header) _))))))))

(add-hook 'im-org-header-changed-hook #'im-contacts--update-contact-on-change)

;;;; Maps

(cl-defun im-nextcloud-maps-put-favorite (&key id name lat lng category comment)
  "Create or update a favorite named NAME.
NAME, LAT, LNG, CATEGORY, COMMENT are required.

If ID is non-nil, then the favorite with ID is updated, otherwise
a new favorite is created.

This function returns a promise."
  (im-request-json-async
   (format
    "%s/index.php/apps/maps/api/1.0/favorites%s"
    im-nextcloud-url
    (if id (format "/%s" id) ""))
   :headers `(("Authorization" . ,(concat "Basic " im-nextcloud-auth)))
   :type (if id "PUT" "POST")
   :data (json-encode
          `((name . ,name)
            (lat . ,lat)
            (lng . ,lng)
            (category . ,category)
            (comment . ,comment)))))

;;;###autoload
(async-defun im-nextcloud-maps-remove-all-favorites ()
  "Delete ALL favorites from Nextcloud Maps."
  (interactive)
  (dolist (id (--map
               (alist-get 'id it)
               (await (im-request-json-async
                       (format "%s/index.php/apps/maps/api/1.0/favorites" im-nextcloud-url)
                       :headers `(("Authorization" . ,(concat "Basic " im-nextcloud-auth)))))))
    (message ">> Removing %s..." id)
    (await
     (im-request-json-async
      (format "%s/index.php/apps/maps/api/1.0/favorites/%s" im-nextcloud-url id)
      :type "DELETE"
      :headers `(("Authorization" . ,(concat "Basic " im-nextcloud-auth)))))
    (message ">> Removing %s...Done" id))
  ;; Remove all NC_IDs too
  (save-restriction
    (save-excursion
      (widen)
      (goto-char (point-min))
      (delete-matching-lines ":NC_ID:"))))

;;;###autoload
(defun im-nextcloud-maps-add-all-in-buffer ()
  "Add all GEO entries to Nextcloud Maps favorites."
  (interactive)
  (org-map-entries
   (lambda () (im-nextcloud-maps-put-favorite-from-org))))

;;;;; Org mode integration

;;;###autoload
(async-defun im-nextcloud-maps-put-favorite-from-org ()
  "Create or update map favorite under cursor.
A favorite is defined as an entry with GEO property, containing
an org geo: link.

If NC_ID property is non-nil, then favorite with NC_ID id is
updated, otherwise it a new favorite is created and then the
NC_ID property is set to the entry."
  (interactive)
  (-when-let* ((prop (org-entry-get nil "GEO"))
               ((_ lat lng _z address) (s-match "\\[\\[geo:\\([0-9\\.]+\\),\\([0-9\\.]+\\);z=\\([0-9]+\\)\\]\\[\\(.*\\)\\]\\]" prop)))
    (let* ((nc-id (-some->> (org-entry-get nil "NC_ID")
                    (string-to-number)))
           (name (org-entry-get nil "ITEM"))
           (id (org-id-get-create))
           (result (await
                    (im-nextcloud-maps-put-favorite
                     :id nc-id
                     :name name
                     :lat lat
                     :lng lng
                     ;; TODO: Multiple tags/categories are not supported in upstream.
                     ;; See https://github.com/nextcloud/maps/issues/1154
                     :category (car (org-get-tags))
                     :comment
                     (format
                      "- Created at :: %s\n- Tags :: %s\n\n%s"
                      (org-entry-get nil "CREATED_AT")
                      (org-entry-get nil "TAGS")
                      (org-agenda-get-some-entry-text (point-marker) most-positive-fixnum))))))
      (if nc-id
          (message ">> Updated map info.")
        (let-alist result
          (save-window-excursion
            (save-excursion
              (org-id-goto id)
              (org-set-property "NC_ID" (number-to-string .id))))
          (message ">> Updated %s with NC_ID=%s." name .id))))))

(defun im-nextcloud-maps--on-org-entry-changed (info)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (plist-get info :begin))
      (im-nextcloud-maps-put-favorite-from-org))))

(add-hook 'im-org-header-changed-hook #'im-nextcloud-maps--on-org-entry-changed)
;;;; Footer

(provide 'im-nextcloud)

;;; im-nextcloud.el ends here
