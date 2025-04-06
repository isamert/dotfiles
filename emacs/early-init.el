;;; early-init.el --- isamert's early init -*- lexical-binding: t; -*-

;;; Commentary:

;; isamert's early init

;;; Code:

;;;; Sane defaults

(setq package-enable-at-startup nil)
(setq-default lexical-binding 'lv)
(setq native-comp-async-report-warnings-errors nil)

;;;; Performance related stuff


;; If you don't use RTL ever, this could improve perf
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; Some stuff are taken from:
;; https://github.com/emacs-lsp/lsp-mode#performance

(setq gc-cons-threshold (* 40 1024 1024)) ;; 40 mb
(setq read-process-output-max (* 2 1024 1024)) ;; 2 mb
(setq process-adaptive-read-buffering nil) ;; Not sure if there is any gains from this.
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)

(setq confirm-kill-processes nil)
;; ^ When exitting, kill processes without asking
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

;;;; Visuals

(unless (daemonp)
  ;; Disable menubar
  (menu-bar-mode 0)
  ;; Disable toolbar
  (tool-bar-mode 0)
  ;; Disable scrollbars
  (ignore-errors
    (scroll-bar-mode -1)))

;; Disable blinking cursor
(blink-cursor-mode 0)

(setq inhibit-startup-message t) ;; Close startup screen
(setq frame-resize-pixelwise t)  ;; Fix gap issues with tiling WMs
(defconst im-init-file (expand-file-name "~/.emacs.d/scratch.el"))
(setq initial-buffer-choice im-init-file)
(add-to-list 'default-frame-alist '(undecorated . t))

(provide 'early-init)
;;; early-init.el ends here
