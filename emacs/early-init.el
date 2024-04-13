;;; early-init.el --- isamert's early init -*- lexical-binding: t; -*-

;;; Commentary:

;; isamert's early init

;;; Code:

;; Some stuff are taken from:
;; https://github.com/emacs-lsp/lsp-mode#performance

(setq package-enable-at-startup nil)
(setq-default lexical-binding 'lv)
(setq native-comp-async-report-warnings-errors nil)

(setq gc-cons-threshold 3000000)
;; ^ Set gc threshold to ~30MB
(setq read-process-output-max (* 1024 1024))
;; ^ Better than default
(setq confirm-kill-processes nil)
;; ^ When exitting, kill processes withouh asking
(setq ring-bell-function 'ignore)
;; ^ This completely disables alarms
(setq column-number-mode t)
;; ^ Show column number
(setq create-lockfiles nil)
;; ^ These just clutter the filesystem
(setq dabbrev-case-fold-search nil)
;; ^ Expansions are done respecting the case (Ctrl-n and Ctrl-p was
;; not behaving the way I wanted before this in evil mode)
(setq vc-follow-symlinks t)
;; ^ Don't ask about following symlinks
;; Ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Visuals
(menu-bar-mode 0)                ;; Disable menubar
(tool-bar-mode 0)                ;; Disable toolbar
(blink-cursor-mode 0)            ;; Disable blinking cursor
(scroll-bar-mode -1)             ;; Disable scrollbars
(setq inhibit-startup-message t) ;; Close startup screen
(setq frame-resize-pixelwise t)  ;; Fix gap issues with tiling WMs
(defconst im-init-file (expand-file-name "~/.emacs.d/scratch.el"))
(setq initial-buffer-choice im-init-file)
(add-to-list 'default-frame-alist '(undecorated . t))

(provide 'early-init)
;;; early-init.el ends here
