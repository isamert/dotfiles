;;; im-occur.el --- Dynamically resize context around matches in occur-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Original Author: 2014-2025 Charles L.G. Comstock
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

;; Code is taken from: https://github.com/dgtized/occur-context-resize.el
;; I just added a evil layer on top, removed default bindings.
;;
;; Adds bindings to resize the context displayed around occur matches
;; in `occur-mode'.
;;
;; `im-occur-default' will revert to whatever size context is
;; specified in `list-matching-lines-default-context-lines'.

;;; Usage:

;; Enable the package in `occur-mode' with the following:
;;
;;   (add-hook 'occur-mode-hook 'im-occur-mode)

;;; Code:

(require 'general)

(defun im-occur-larger ()
  "Show more context around occur matches."
  (interactive)
  (setf (cadr occur-revert-arguments)
        (1+ (or (cadr occur-revert-arguments) 0)))
  (revert-buffer))

(defun im-occur-smaller ()
  "Show less context around occur matches."
  (interactive)
  (setf (cadr occur-revert-arguments)
        (1- (or (cadr occur-revert-arguments) 0)))
  (revert-buffer))

(defun im-occur-default ()
  "Revert to show default context around occur-matches."
  (interactive)
  (setf (cadr occur-revert-arguments) nil)
  (revert-buffer))

;;;###autoload
(define-minor-mode im-occur-mode
  "Dynamically resize context around matches in `occur-mode'.

\\{im-occur-mode-map}"
  :keymap (make-sparse-keymap)
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<im-occur-mode-map>Occur \\[im-occur-larger] context++ | \\[im-occur-smaller] context-- | \\[im-occur-default] default")))

(general-def :keymaps 'im-occur-mode-map :states 'normal
  "[" #'im-occur-smaller
  "]" #'im-occur-larger
  "=" #'im-occur-default)

(provide 'im-occur)
;;; im-occur.el ends here
