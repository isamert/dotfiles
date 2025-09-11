;;; im-macos.el --- My MacOS related extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: macos

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

;; MacOS related extensions.  Unfortunately, I use it for work.
;; Hammerspoon is cool though.

;;; Code:

(require 'im)

;;;; Keybindings

;; I use an external keyboard, this makes AltGr and Meta (Alt) work as
;; expected I have also inverted Meta and Control keys system-wide or
;; something, so this setting is done according to that.
(setq ns-option-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;;;; Hammerspoon IPC integration

(defun im-eval-hammerspoon (start end)
  "Run region (or current line) with hammerspoon."
  (interactive
   (if (use-region-p)
       (list
        (region-beginning)
        (region-end))
     (list
      (line-beginning-position)
      (line-end-position))))
  (shell-command-on-region start end "/opt/homebrew/bin/hs -c --" "*hammerspoon eval*" nil)
  (im-pulse-highlight-region start end 'im-eval-defun-pulse-highlight-face 0.3))

;;;; Hammerspoon-Menubar integration

;; I have a menubar that shows my current tab and currently clocked in
;; task. I disable the tab-bar visually and check which tab I
;; currently am in through this Hammerspoon menubar.

(defvar im-hammerspoon-server "http://localhost:4562")
(defvar im-hammerspoon-handle-clock-p t
  "Whether to handle org clock stuff in hammerspoon or not.")

(defun im-toggle-hammerspoon-handle-clock ()
  "Toggle the value of `im-hammerspoon-handle-clock-p'.
This is useful when using laptop screen only as the menubar disappears
because of the notch if the text is too long and I can't see which tab I
am on because of this."
  (interactive)
  (setq im-hammerspoon-handle-clock-p (not im-hammerspoon-handle-clock-p))
  ;; Reset text if disabled
  (unless im-hammerspoon-handle-clock-p
    (request (concat im-hammerspoon-server "/task")
      :type "POST"
      :data ""))
  (message ">> `im-hammerspoon-handle-clock-p' is now %s" im-hammerspoon-handle-clock-p))

(add-hook 'org-clock-in-hook #'im-hammerspoon-handle-clock-in)
(add-hook 'org-clock-out-hook #'im-hammerspoon-handle-clock-in)

(defun im-hammerspoon-handle-clock-in ()
  (when im-hammerspoon-handle-clock-p
    (request (concat im-hammerspoon-server "/task")
      :type "POST"
      :data (if (org-clock-is-active)
                (substring-no-properties (org-clock-get-clock-string))
              ""))))

(define-advice tab-bar-select-tab (:after (&rest _) report-tab-change-to-hammerspoon)
  (request (concat im-hammerspoon-server "/workspace")
    :type "POST"
    :data (or (alist-get 'name (tab-bar--current-tab))
              (tab-bar-tab-name-current))))

(define-advice org-clock-update-mode-line (:after (&rest _) report-to-hammerspoon)
  (im-hammerspoon-handle-clock-in))

(run-with-timer 30 30 #'im-hammerspoon-handle-clock-in)

;;;; Footer

(provide 'im-macos)
;;; im-macos.el ends here
