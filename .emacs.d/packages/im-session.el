;;; im-session.el --- Simple session manager -*- lexical-binding: t; -*-

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

;; Simple way to save and restore the current session.  Two functions
;; and a global mode:
;;
;; - `im-session-save' → Save the current session as "last".  With
;;   prefix arg, give it another name.
;; - `im-session-restore' → Restore the selected session.
;; - `im-session-auto-save-mode' → Periodically save current session
;;   as "last".
;;
;; This only supports tab-bar based sessions.  Saves every "visible"
;; buffer in each tab and restores them only.  Dired buffers and also
;; non-file text buffers are also handled specially.


;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'subr-x)

;;;; Customization

(defgroup im-session nil
  "Save and restore tab-bar sessions."
  :group 'convenience)

(defcustom im-session-auto-save-interval 60
  "Seconds between automatic saves of the `last' session."
  :type 'number
  :group 'im-session)

(defcustom im-session-file
  (locate-user-emacs-file "session.el")
  "File used by `im-session-save' and `im-session-restore'."
  :type 'path
  :group 'im-session)

;;;; Core

(defvar im-session--auto-save-timer nil
  "Timer used by `im-session-auto-save-mode'.")

(defun im-session--read-sessions ()
  "Return the saved session alist."
  (if (not (file-exists-p im-session-file))
      nil
    (with-temp-buffer
      (insert-file-contents im-session-file)
      ;; This file is written by `im-session--write-sessions'.
      (let ((contents (read (current-buffer))))
        (unless (plist-member contents :sessions)
          (user-error "Invalid session file: %s" im-session-file))
        (plist-get contents :sessions)))))

(defun im-session--write-sessions (sessions)
  "Write SESSIONS to `im-session-file'."
  (make-directory (file-name-directory im-session-file) t)
  (with-temp-file im-session-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 (list :version 1 :sessions sessions) (current-buffer))
      (insert "\n"))))

(defun im-session--session-names ()
  "Return saved session names, suitable as completion candidates."
  (sort (mapcar #'car (im-session--read-sessions)) #'string-lessp))

(defun im-session--read-session-name (prompt)
  "Read a session name with PROMPT."
  (let ((name (completing-read prompt (im-session--session-names))))
    (if (string-empty-p name)
        (user-error "Session name cannot be empty")
      name)))

(defun im-session--buffer-record (buffer)
  "Return a record sufficient to restore BUFFER."
  (with-current-buffer buffer
    (let ((file buffer-file-name)
          (dired-directory (when (derived-mode-p 'dired-mode)
                             default-directory)))
      (list (buffer-name buffer)
            file
            dired-directory
            (unless (or file dired-directory)
              (save-restriction
                (widen)
                (buffer-substring-no-properties (point-min) (point-max))))))))

(defun im-session--visible-buffer-records ()
  "Return descriptions of buffers visible in the selected tab."
  (delete-dups
   (mapcar #'im-session--buffer-record
           (mapcar #'window-buffer (window-list nil 'no-minibuf)))))

(defun im-session--window-state-buffer-records (state)
  "Return records for the live buffers displayed in window STATE."
  (let (records)
    (cl-labels
        ((walk (tree)
               (cond
                ((and (consp tree) (eq (car tree) 'buffer))
                 (when-let* ((buffer (get-buffer (cadr tree))))
                   (push (im-session--buffer-record buffer) records)))
                ((proper-list-p tree)
                 (mapc #'walk tree)))))
      (walk state))
    (delete-dups (nreverse records))))

(defun im-session--restore-buffer-contents (name contents)
  "Create NAME and restore its CONTENTS."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert contents)
      (set-buffer-modified-p nil))))

(defun im-session--restore-buffer-records (records)
  "Ensure the buffers in RECORDS exist before restoring a window state."
  (dolist (record records)
    (pcase-let ((`(,name ,file ,dired-directory ,contents) record))
      (cond
       (dired-directory
        (unless (get-buffer name)
          (condition-case nil
              (dired-noselect dired-directory)
            (error (get-buffer-create name)))))
       (file
        (unless (get-buffer name)
          ;; Usually recreates the buffer with NAME as well.
          (if (file-readable-p file)
              (condition-case nil
                  (find-file-noselect file)
                (error (get-buffer-create name)))
            (get-buffer-create name))))
       ;; Restore the contents even when a standard buffer such as
       ;; *scratch* already exists in the new Emacs session.
       (t
        (im-session--restore-buffer-contents name contents))))))

(defun im-session--tab-state ()
  "Return this frame's tab-bar state and its visible buffers.

This reads non-current tabs from their saved window states instead of
selecting them, so saving does not redraw tabs or run tab-selection hooks."
  (let* ((tabs (tab-bar-tabs))
         (selected
          (1+ (cl-position-if
               (lambda (tab) (eq (car tab) 'current-tab))
               tabs)))
         saved-tabs)
    (dolist (tab tabs)
      (let* ((current (eq (car tab) 'current-tab))
             ;; The current tab has no stored `ws' because it is already
             ;; displayed.  Every non-current tab stores its window state
             ;; in that field.
             (state (if current
                        (window-state-get (frame-root-window) t)
                      (copy-tree (alist-get 'ws tab))))
             (buffers (if current
                          (im-session--visible-buffer-records)
                        (im-session--window-state-buffer-records state))))
        (push (list :name (alist-get 'name tab)
                    :buffers buffers
                    :state state)
              saved-tabs)))
    (list :selected selected :tabs (nreverse saved-tabs))))

(defun im-session-save (prefix)
  "Save this frame's tab-bar layout and its visible buffers.
Without PREFIX, save the session as `last`.  With PREFIX, prompt for a
session name.  Existing session names are offered as completion candidates;
selecting one replaces that saved session."
  (interactive "P")
  (let* ((name (if prefix
                   (im-session--read-session-name "Save session: ")
                 "last"))
         (sessions (im-session--read-sessions))
         (state (im-session--tab-state)))
    (setq sessions (cons (cons name state)
                         (assoc-delete-all name sessions)))
    (im-session--write-sessions sessions)
    (message "Saved %d tab(s) as session %S"
             (length (plist-get state :tabs)) name)))

(defun im-session--auto-save ()
  "Save the `last' session, reporting errors without stopping the timer."
  (condition-case err
      ;; Do not replace the user's echo-area message for a routine save.
      (let ((inhibit-message t))
        (im-session-save nil))
    (error
     (message "Could not automatically save session: %s"
              (error-message-string err)))))

(define-minor-mode im-session-auto-save-mode
  "Globally save the `last' session after regular idle intervals.

The interval is controlled by `im-session-auto-save-interval'.  Using an
idle timer keeps automatic saves out of the way while you are working."
  :global t
  :group 'im-session
  (when (timerp im-session--auto-save-timer)
    (cancel-timer im-session--auto-save-timer))
  (setq im-session--auto-save-timer
        (when im-session-auto-save-mode
          (run-with-idle-timer im-session-auto-save-interval t
                               #'im-session--auto-save)))
  (when (called-interactively-p 'interactive)
    (message "Session auto-save %s"
             (if im-session-auto-save-mode "enabled" "disabled"))))

(defalias 'im-session-load #'im-session-restore)
(defun im-session-restore (name)
  "Restore the saved tab-bar session NAME.

Interactively, prompt for NAME with completion over saved session names."
  (interactive (list (im-session--read-session-name "Restore session: ")))
  (let* ((sessions (im-session--read-sessions))
         (saved (cdr (assoc-string name sessions)))
         (tabs (plist-get saved :tabs))
         (selected (plist-get saved :selected)))
    (unless saved
      (user-error "No saved session named %S" name))
    (unless tabs
      (user-error "The saved tab configuration contains no tabs"))
    (tab-bar-mode 1)
    (tab-bar-close-other-tabs)
    (cl-loop for tab in tabs
             for index from 1
             do
             (if (= index 1)
                 (tab-bar-rename-tab (plist-get tab :name))
               (tab-bar-new-tab)
               (tab-bar-rename-tab (plist-get tab :name)))
             (im-session--restore-buffer-records (plist-get tab :buffers))
             (window-state-put (plist-get tab :state)
                               (frame-root-window)))

    (tab-bar-select-tab selected)
    (message "Restored %d tab(s)" (length tabs))))

;;;; Footer

(provide 'im-session)

;;; im-session.el ends here
