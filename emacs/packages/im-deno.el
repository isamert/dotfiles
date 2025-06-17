;;; im-deno.el --- Deno extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: deno

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

;; My Deno extensions.
;;
;; - Support deno:/ links with Eglot.
;; - Functions for enabling Deno LSP support (for projects without
;;   deno.json). See `im-deno-enable-lsp-for-project'.
;; - Function for caching dependencies.

;;; Code:

(defvar im-deno-options '(:enable t
                          :lint t
                          :unstable t))

(with-eval-after-load 'eglot
  (add-to-list 'auto-mode-alist '("deno:/" . im-deno-uri-handler))
  (add-to-list
   'eglot-server-programs
   `((typescript-ts-mode :language-id "typescript")
     . ("deno" "lsp"
        :initializationOptions ,im-deno-options))))

;;;###autoload
(defun im-deno-uri-handler ()
  "Handle deno:/ files properly.

Also see: https://docs.deno.com/runtime/manual/advanced/language_server/overview/#requests"
  (when-let* ((buffer (marker-buffer
                       (caar (xref-global-history))))
              (uri (concat "deno:/" (nth 1 (s-split "deno:/" (buffer-file-name)))))
              (contents (with-current-buffer buffer
                          (eglot--request
                           (eglot-current-server)
                           :deno/virtualTextDocument
                           `(:textDocument
                             (:uri ,uri))))))
    (insert contents)
    (set-visited-file-name nil)
    (set-buffer-modified-p nil)
    (typescript-ts-mode)
    (rename-buffer (format "*%s*" uri))
    (read-only-mode)))

;; See https://github.com/denoland/deno/issues/21650#issuecomment-2329688884

(defun im-deno-enable-lsp-for-project ()
  "Enable Deno LSP for current project."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (im-deno-enable-lsp-for-current-folder)))

(defun im-deno-enable-lsp-for-current-folder ()
  "Enable Deno LSP for current folder."
  (interactive)
  (add-dir-local-variable 'typescript-ts-mode 'eglot-workspace-configuration
                          `(:deno ,im-deno-options))
  (message
   (substitute-command-keys
    ">> Call \\[eglot-signal-didChangeConfiguration] after save after saving and returning to buffer.")))

(defun im-deno-cache-buffer-dependencies (invalidate?)
  "Install and cache dependencies stated in current Deno file.
If INVALIDATE? is non-nil, then force reload dependencies instead
of just pulling non-cached ones.

LSP also offers a similar command but this is easier."
  (interactive "P")
  ;; TODO reload lsp on file on save?
  (let ((buf (current-buffer))
        (proc-buffer "*im-deno-cache-deps*"))
    (save-buffer)
    (im-shell-command
     :command (format "deno cache --allow-import %s %s"
                      (if invalidate? "-r" "")
                      (f-relative (buffer-file-name)))
     :buffer-name proc-buffer
     :on-start
     (lambda (&rest _)
       (message ">> Downloading deps..."))
     :on-fail
     (lambda (&rest _)
       (switch-to-buffer proc-buffer)
       (user-error ">> Downloading deps...Failed!"))
     :on-finish
     (lambda (&rest _)
       (message ">> Downloading deps...Done.")
       (switch-to-buffer buf)))))

;;;; Footer

(provide 'im-deno)

;;; im-deno.el ends here
