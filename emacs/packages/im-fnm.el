;;; im-fnm.el --- FNM (Fast Node Manager) support  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: npm nodejs utility

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

;; Switch Node versions with FNM in Emacs.  Simply do M-x
;; `im-fnm-use'.

;;; Code:

(defun im-fnm--ensure ()
  "Set necessary FNM env vars."
  (unless (getenv "FNM_DIR")
    (let ((fnm-env (json-parse-string
                    (shell-command-to-string "fnm env --json")
                    :object-type 'alist)))
      (message "Initializing FNM...")
      (dolist (it fnm-env)
        (let ((key (car it))
              (value (cdr it)))
          (message ">> Setting %s → %s" key value)
          (setenv (symbol-name key) value)
          (when (eq key 'FNM_MULTISHELL_PATH)
            (let ((bin (format "%s/bin" value)))
              (message ">> Adding %s to PATH..." bin)
              (setenv "PATH" bin t)
              (setq exec-path (cons bin exec-path)))))))))

(defun im-fnm-use ()
  "Call `fnm-use'.
If current project contains a .node-version file, use that directly.
Otherwise ask for a node version."
  (im-fnm--ensure)
  (let ((auto? (when-let* ((root (im-current-project-root)))
                 (file-exists-p (format "%s/.node-version" root)))))
    (im-shell-command
     :command "fnm"
     :args (list "use"
                 (format
                  "%s"
                  (if auto?
                      nil
                    (read-string "Node version: "))))
     :eat t
     :switch t)))

;;;; Footer

(provide 'im-fnm)

;;; im-fnm.el ends here
