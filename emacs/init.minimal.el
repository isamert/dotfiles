;;; index.el --- isamert's simple & modern configuration for everyone -*- lexical-binding: t; -*-

;;; Commentary:

;; isamert's simple & modern configuration for everyone

;;;; Keybindings

;;;;; Legend

;; S → Shift
;; M → Option
;; C → Ctrl

;;;;; General

;; M-x :: All commands
;; C-x C-f :: Open file/folder
;;   Here, pressing M-p or M-n lets you directly jump to the path of the previous/next selected file
;;   This applies to most windows opened below; you can access previous inputs with M-p
;; C-x b :: List/switch open files/buffers
;; C-x k :: Kill/close open files/buffers

;;;;; File movement

;; C-up :: Previous paragraph
;; C-down :: Next paragraph
;; M-< :: Beginning of file
;; M-> :: End of file

;;;;; Window management

;; C-x 2 :: Split the screen into two (one above the other)
;; C-x 3 :: Split the screen into two (side by side)
;; C-x o :: Switch to the next screen
;; S-{up,down,right,left} :: Move to the {up, down, right, left} screen

;;;;; Other

;; M-x eshell :: Console
;; C-u M-x eshell :: New console in the current directory
;; M-x consult-theme :: Change theme

;;; Code:

;;;; straight.el and use-package

;; straight.el is a package manager. Think of npm/yarn, but for Emacs.
;; The following code does the installation. It is used along with
;; `use-package' which you will see down below.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
;; ^ Emacs also has a default package manager but we disable it in
;; favor of package.el

;;;; Some sensible default settings

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq native-comp-async-report-warnings-errors nil)
;; Disable some unnecessary logs
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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; ^ Scroll one line at a time
(setq mouse-wheel-progressive-speed nil)
;; ^ Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)
;; ^ Scroll window under mouse
(setq scroll-conservatively 100)
;; ^ When cursor reaches end, just scroll line-by-line
;; (setq scroll-margin 10)
;; ^ Start scolling earlier

;; Some bindings for scaling text with mouse and Ctrl +/-
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)

;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;;;; Visuals

;; Disable menubar
(menu-bar-mode 0)
;; Disable toolbar
(tool-bar-mode 0)
;; Disable blinking cursor
(blink-cursor-mode 0)
;; Disable scrollbars
(scroll-bar-mode -1)
;; Close startup screen
(setq inhibit-startup-message t)
;; Fix gap issues with tiling WMs
(setq frame-resize-pixelwise t)

;; See how we use `use-package' to install and configure a new
;; package.  Here we install some theme packages.  You can switch
;; between themes by calling M-x `my-change-theme' (which we define
;; somewhere below):

(use-package doom-themes
  ;; :straight t means that use straight.el to install the package,
  ;; the package manager I talked about at the beginning.
  :straight t)

(use-package ef-themes
  :straight t
  :config
  (load-theme 'ef-summer t))

;; Nicer mode line. Mode line is the thing you see below the buffer,
;; the place where file name, line number etc. displayed.
(use-package mood-line
  :straight t
  :config
  (mood-line-mode))

;; Nice icons for files etc.
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-completion
  :straight t
  :hook (after-init . all-the-icons-completion-mode))

;;;; general.el

;; This package lets us define new keybindings more easily.

(use-package general
  :demand t
  ;; ^ :demand means I want this package to be loaded immediately.
  ;; You don't need to load every package immediately, they generally
  ;; get loaded when needed.
  :straight t
  :config
  (general-override-mode))

;;;; Configure some built-in packages

;; We can also use `use-package' to configure built-in packages.

;; recentf is a package for managing last opened files etc.
(use-package recentf
  :straight (:type built-in)
  ;; ^ We tell straight that this is a built-in package, so that it
  ;; does not try to install it.
  :defer 5
  :config
  (setq recentf-max-saved-items 500)
  (add-to-list 'recentf-exclude (format ".*\\.elc" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/tmp/.*")
  (add-to-list 'recentf-exclude "/var/folders/.*")
  (recentf-mode t))

;; savehist is a package for saving history for some commands.
(use-package savehist
  :straight (:type built-in)
  :config
  ;; Clipboard selections are copied into the kill-ring
  (setq save-interprogram-paste-before-kill t)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode 1))

(defconst my-backup-dir  (concat user-emacs-directory "backups"))

(setq backup-directory-alist `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

(setq backup-by-copying t)
;; ^ Don't delink hardlinks
(setq version-control t)
;; ^ Use version numbers on backups
(setq delete-old-versions t)
;; ^ Automatically delete excess backups
(setq kept-new-versions 20)
;; ^ How many of the newest versions to keep
(setq kept-old-versions 5)
;; ^ How many of the old versions to keep

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight matching parenthesis
(use-package show-paren
  :straight (:type built-in)
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis))

;; Colorful parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :straight t)

;;;; consult, embark, vertico, orderless

;; Improves the selection interfaces that appears when you do M-x, C-x
;; C-f etc.
(use-package vertico
  :straight t
  :config (vertico-mode))

;; Some configuration to make vertico work better
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

;; This package has some utility functions for your usage. Do M-x and
;; search for `consult-' to see them.
(use-package consult
  :straight t
  :config
  (advice-add #'register-preview :override #'consult-register-window)

  (setq consult-preview-key "M-,")
  ;; ^ When you do M-, on a candidate, it previews it

  ;; Hide some buffers from consult-buffer window. If you want to jump
  ;; on one of these buffers, start with a space after opening
  ;; `consult-buffer'.
  (add-to-list
   'consult-buffer-filter
   "\\`\\*\\(Help\\|Backtrace\\|Messages\\|Buffer List\\|Flycheck.*\\|scratch.*\\)\\'")

  ;; This also supports previews. Use the `consult-preview-key'.
  (defalias 'my-change-theme #'consult-theme))

;; Context dependent menus. Like a right-click menu, displays some
;; context related actions that you can do. Hit `M-a' to see them.
(use-package embark
  :straight t
  :commands (embark embark-act-all)
  :config
  (bind-key (kbd "M-a") #'embark-act)
  (bind-key (kbd "M-A") #'embark-act-all)
  (setq embark-prompter #'embark-completing-read-prompter)
  ;; ^ This directly shows the actions in a completing read window.
  ;; By default, it is set to `embark-keymap-prompter' and you need to
  ;; hit `C-h' to bring this menu up.
  (setq embark-indicators '(embark-highlight-indicator embark-isearch-highlight-indicator))
  ;; ^ I removed embark-mixed-indicator from the list because I'm
  ;; using embark-completing-read-prompter by default which already
  ;; provides same functionality

  ;; Hitting C-h after any prefix command will open a completing-read
  ;; interface to select a command that prefix has. Much better than
  ;; which-key.
  (setq prefix-help-command 'embark-prefix-help-command)

  (setq embark-quit-after-action '((kill-buffer . nil)
                                   (t . t)))

  ;; Replace describe-symbol with helpful-symbol
  (define-key embark-symbol-map "h" #'helpful-symbol))

;; Consult-Embark integration.
(use-package embark-consult
  :straight t)

;; This shows little help texts on some interfaces.
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; orderless sorts the candidates that appear in vertico in a much
;; better way.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;;; corfu -- code completion interface

;; Corfu is the interface for code completion.

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :config
  (define-key corfu-map (kbd "M-j") #'corfu-next)
  (define-key corfu-map (kbd "M-k") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-complete)
  (define-key corfu-map (kbd "<tab>") nil)

  (set-face-background 'corfu-current "dim gray")
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (global-corfu-mode))

;; Nice icons for corfu.
(use-package kind-icon
  :after corfu
  :straight t
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Display documentation right next to corfu completion.
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :config
  (setq corfu-popupinfo-delay 0)
  (corfu-popupinfo-mode)
  ;; Toggle doc on/off while in corfu
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up))

;;;; Code formatting etc

;; Automatically format files on save.
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :straight t
  :init
  (defalias 'my-toggle-auto-code-formatter #'apheleia-mode))

;; Manually format files by doing M-x format-all-buffer.
(use-package format-all
  :straight t)

;;;; Better Emacs interface

;; helpful improves Emacs's help pages.
(use-package helpful
  :straight t
  :config
  ;; Replace default Emacs help interfaces with helpful
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h p") #'helpful-at-point))

;; which-key shows you a list of keys you can hit next when you hit a
;; key.
(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; Automatically use treesitter. Tresitter makes syntax highlighting
;; faster and it's better.
(use-package  treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; org-mode
;;;;; org-modern

(use-package org-modern
  :after org
  :custom
  (org-modern-timestamp nil)
  (org-use-sub-superscripts nil)
  (org-modern-block-fringe nil)
  (org-modern-table nil)
  (org-modern-hide-stars " ")
  (org-modern-list
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  :config
  (add-hook 'org-agenda-finalize #'org-modern-agenda)
  (add-hook 'org-mode-hook #'org-modern-mode 99))

;;;; avy -- jump to anywhere on buffer

(use-package avy
  :straight t
  :commands (avy-goto-subword-1 avy-goto-word-1)
  :bind (("C-." . avy-goto-word-1))
  :config
  (setq avy-keys '(?q ?w ?e ?r ?t ?a ?s ?d ?f ?j ?k ?l ?u ?i ?o ?p)
        avy-case-fold-search nil
        avy-all-windows t))

;;;; outli -- outlier for code files

;; Basic outlining for code files.
(use-package outli
  :straight (:host github :repo "jdtsmith/outli")
  :hook (prog-mode . outli-mode)
  :general
  ;; You can jump between /pages/ by using ~C-x [~ and ~C-x ]~. See
  ;; [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Pages.html][this]]
  ;; for more information. Using the same convention for outli:
  (:keymaps 'outli-mode-map
   "C-x [" #'outline-previous-heading
   "C-x ]" #'outline-next-heading)
  :config
  ;; Add h as narrow prefix for headings in consult-imenu
  (with-eval-after-load 'consult-imenu
    (push '(?h "Headings") (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))))

;;;; eshell -- emacs shell

;; eshell is emacs shell.  Like zsh/bash but better integrated to
;; Emacs.

;; eat improves eshell.
;; You may need to call `eat-compile-terminfo' after installing eat.
(use-package eat
  :straight (:type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (eat-eshell-mode))

;; Add syntax highlighting to eshell
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;;; dired & dirvish -- file manager

;; Dired is the default file manager of Emacs.
(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-dwmy-target t)
  (setq dired-listing-switches "-al")
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwmy-target t)
  (put 'dired-find-alternate-file 'disabled nil))

;; Better file manager for Emacs. Dirvish is based on Dired and it
;; inherits it's settings.
(use-package dirvish
  :straight t
  :after dired
  :init
  (dirvish-override-dired-mode)
  :general
  (:keymaps 'dirvish-mode-map
   "\\"    #'dired-find-file-other-window
   "h"     #'dired-up-directory
   "l"     #'dired-find-alternate-file
   "T"     #'dirvish-layout-toggle
   "e"     #'dirvish-dispatch
   "q"     #'dirvish-quit
   "<tab>" #'dirvish-toggle-subtree
   "f"     #'dirvish-file-info-menu
   "s"     #'dirvish-setup-menu
   "H"     #'dirvish-history-go-backward
   "L"     #'dirvish-history-go-forward)
  :config
  (setq dirvish-subtree-always-show-state t)
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  (setq dirvish-side-width 55)
  (dirvish-side-follow-mode))

;;;; eglot

(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     ;; :colorProvider
     :foldingRangeProvider)))

;;;; MacOS configuration/helpers

;; Make right option modifier work properly.
(setq mac-right-option-modifier 'none)

;; Add a function that opens current directory in finder. Just do M-x
;; `reveal-in-finder'.
(use-package reveal-in-osx-finder
  :straight t
  :if (eq system-type 'darwin))

;; Import shell variables to Emacs

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; init.el ends here
