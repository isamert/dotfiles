;;; index.el --- isamert's configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; isamert's configuration

;; This is my Emacs configuration.  My main focus is sanity.  I'm a
;; person who get frustrated pretty easily.  So instead of optimizing
;; the time spent on doing things, I try to find ways of doing things
;; that does not make me frustrated.  Most of the time you get speed
;; boost as a byproduct.

;;;; Emacs installation

;; I use my distributions package manager to install Emacs.  However,
;; in MacOS (my work computer) I use /homebrew/ and /emacs-plus/
;; formula.  Here is how you install it:
;;
;; #+begin_src sh
;;   brew tap d12frosted/emacs-plus
;;   brew install emacs-plus@30 \
;;        --with-dragon-icon \
;;        --with-dbus \
;;        --with-imagemagick \
;;        --with-poll \
;;        --with-debug
;; #+end_src
;;
;; There are also other nice Emacs distributions, like:
;;
;; - https://github.com/jimeh/emacs-builds
;; - https://emacsformacosx.com/
;;
;; But I need the aforementioned switches and patches.  Otherwise these
;; are very good.
;;
;; * --with-poll
;;
;; You can read about the options [[https://github.com/d12frosted/homebrew-emacs-plus#options][here]], but the most important one is
;; ~--with-poll~.  Otherwise you'll get lot's of /no file descriptors
;; left, too many files are open/ errors, especially if you are using
;; LSP etc..
;;
;; * --with-debug
;;
;; It is nice for debugging crashes or freezes.  Helps you get a better
;; output from the debugger.  When Emacs freezes and ~pkill -USR2 Emacs~
;; is not helping, what you can do to debug the issue is:
;;
;; #+begin_src sh
;;   $ cd ~/Library/Caches/Homebrew/emacs-plus@30--git/src
;;   $ lldb --local-lldbinit /opt/homebrew/Cellar/emacs-plus@30/<VERSION>/Emacs.app
;;   (lldb) r # → Do this to start Emacs
;;   # wait for the freeze
;;   # C-c to quit the Emacs after freeze
;;   (lldb) xbacktrace # or bt
;; #+end_src
;;
;; Same story with crashes.  Just run Emacs through =lldb= all the time
;; and be prepared I guess...

;;;; Usage notes

;;;;; General notes and conventions

;; - This configuration is meant to be used with /emacs daemon/, so I don't really care about the startup time etc.
;; - I try to split package configurations into multiple org src blocks and unify them using ~noweb~ references under a single =use-package= directive.
;; - I try to put things in way that easily copyable from the configuration.  An example would be using multiple =(setq ...)= lines instead of having one =(setq ... ...)= call and setting multiple variables in one go.
;; - I make use of =use-package= features quite minimally.  See [[id:3d974e67-11fc-4f07-8cd4-ec6fd63152c4][here]] for more information that.  This is generally related with the item above and future-proofing.
;; - I use =verbatim text= and ~code text~ completely randomly.
;; - I try to prefer built-in packages or packages that enhances built-in ones where possible.  I'm also trying to simplify my configuration, so another goal is to reduce the package number.  Although I intend to keep packages that enhances the overall experience with no special configuration (just install and forget type of packages).

;;;;; Keybinding conventions

;; - After leader
;;   - =e= :: is reserved for independent programs, that is not related to editing/programming.  For example, "ec" opens calendar, "ee" opens elfeed, "er..." controls the radio.
;;   - =t= :: is reserved for toggling stuff.  Toggle the terminal, toggle a frequently accessed buffer etc.
;;   - =h= :: is reserved for any menu with fuzzy selection that does not fit anywhere else.
;;   - =g= :: is for git related functionality.
;;   - =p= :: is for project related functionality.
;;   - =/= :: is for search/translate related functionality.  (Generally external programs)
;;   - =b= :: is for buffers.
;;   - =w= :: is for windows.  I also use =C-w= for this, which is default prefix for window-related functions in vim.
;;   - =o= :: is for org-mode/outline mode.

;;;;; Updating packages

;; Just do ~M-x straight-pull-all~.  I do this quite infrequently.  If
;; everything is working fine as it is, I tend to not update anything.

;;; Code:

;;;; Preparation

;;;;; straight.el and use-package

;; Useful to see which packages take long time to load
;; (require 'use-package)
;; (setq use-package-compute-statistics t)
(setq use-package-enable-imenu-support t) ;; With that, IMenu will show packages section

(defvar bootstrap-version)
(defvar straight-base-dir)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" (or (ignore-errors straight-base-dir) user-emacs-directory)))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

;; I tend to not use the =use-package= goodies while configuring my
;; packages, meaning that I don't use =:hook=, =:bind= etc. as they
;; have relatively simpler alternatives in Emacs and using
;; =use-package= alternatives of these makes copy/pasting harder. Here
;; are the keywords that I use the most:

;; - =:init= :: This gets called before the package gets initialized.
;; - =:config= :: This gets called after the package is initialized.
;; - =:after= :: This makes the current definition to wait the loading of listed packages, like =:after (evil org)= makes it wait for the =evil= and =org= packages to be loaded.
;; - =:if= :: Loads the package conditionally, like =:if (eq system-type 'darwin)=.

;;;;;; Hiding mode indicators from modeline

;; ~diminish.el~ provides a way to hide mode indicators from mode
;; line. Either pass ~:diminish t~ to use-package while installing or
;; just call ~(diminish 'x-mode)~.

;; (use-package diminish)

;; Currently I use [[mini-modeline]] as my modeline and it already
;; hides minor mode indicators from the modeline. So this package is
;; not needed but better have it as I might change my modeline in the
;; future.

;;;;; Load path

(defconst im-load-path (expand-file-name "~/.emacs.d/load/")
  "The path that I keep my extra stuff.
Probably stuff that not committed to git.")

(defconst im-packages-path (expand-file-name "~/.emacs.d/packages/")
  "The path that I keep my experimental packages in.
These packages are not published as a separate project but lives
in my dotfiles repository.")

(add-to-list 'load-path im-load-path)
(add-to-list 'load-path im-packages-path)

;; Also load ~im-secrets~ from =load-path=. I'll be utilizing some
;; variables defined here throughout my configuration. It contains
;; some api-keys, some tokens or some passwords etc. that I don't want
;; to leak into public. Instead of doing mutations on an external
;; hidden script, I define variables in this external hidden script
;; and reference them in the configuration. This way the logic stays
;; in the public configuration file so that everyone can take a look,
;; but only the variable itself will be hidden from the public.

(require 'im-secrets)

;; Plain (require 'im) is not enough, because straight needs to load
;; dependencies of im.el file. But I experienced some problems with
;; Emacs daemon using older byte-compiled version of this file time to
;; time. Removing ~/.emacs.d/straight/build/im was sufficent to
;; mitigate the issue.
(straight-use-package `(im :local-repo ,im-packages-path :files ("im.el")))

(require 'im)

;;;;; Essential packages

;; Following provides defmemoize macro. Use (memoize-restore
;; 'fn-name) to restore the original function.
(use-package memoize)

;; Web server stuff.  `elnode-make-webserver' is very useful for
;; starting a webserver in given directory.  Use `elnode-server-list'
;; to list active webservers.
(use-package elnode
  :defer t
  :custom
  (elnode-error-log-to-messages nil))

;; JS-like async/await. Simply return a promise from a function with
;;
;; #+begin_src elisp
;; (promise-new
;;  (lambda (resolve reject)
;;    (funcall resolve arg)))
;; #+end_src
;;
;; and then
;;
;; #+begin_src elisp
;; (async-defun some-function ()
;;   (message "Result is %s" (await promise-returned-function)))
;; #+end_src
;;
;; This is my wrapper with some extensions, real package is
;; async-await.

(use-package im-async-await
  :straight `(:local-repo ,im-packages-path :files ("im-async-await.el"))
  :demand t)

(use-package request
  :custom
  (request-log-level 'warn)
  (request-message-level -1))

(use-package llama
  :straight (:host github :repo "tarsius/llama"))

;;;;;; emacs-async

;; To be able execute elisp asynchronously. Of course this has lot's
;; of limitations, the lambda passed to ~async-start~ will be executed
;; in a freshly started Emacs instance and it will not have the any
;; context of the currently running Emacs instance. There are ways to
;; pass variables into the context of newly created Emacs instance
;; which helps a lot.

(use-package async)

;;;;;; midnight-mode

;; I run some functions periodically using midnight-mode. Runs once a
;; day at the specified time. Simply add your function via ~add-hook~
;; to ~midnight-hook~.

(use-package midnight
  :straight (:type built-in)
  :demand t
  :config
  ;; From: https://old.reddit.com/r/emacs/comments/y4nc0e/help_needed_with_midnightmode/
  ;; To make it *not* work at each emacs startup, use numbers instead of "10:30am"
  ;;
  ;; Set it to 10:30am because some jobs require VPN to be open.
  (midnight-delay-set 'midnight-delay (truncate (* 10.5 60 60)))
  ;; By default it contains `clean-buffer-list' but I don't use it.
  (setopt midnight-hook '())
  (midnight-mode +1))

;;;;; Variables and functions

;; Some basic variable and function definitions that will be used in
;; configuration.

(defun im-apply-patch-from-src-block (block-point-or-name target-folder)
  "Apply the patch at src block BLOCK-POINT-OR-NAME to TARGET-FOLDER.
I keep some of my changes to packages as in patch format and this
simplifies applying those patches.  I try to utilize advices most
of the time but this is for those times where advices either
complicates things or not sufficient."
  (interactive
   (list (if-let* ((x (org-babel-where-is-src-block-head)))
             x
           (save-excursion
             (org-babel-goto-named-src-block (read-string "Block name: "))
             (point)))))
  (save-excursion
    (if (stringp block-point-or-name)
        (org-babel-goto-named-result block-point-or-name)
      (goto-char block-point-or-name))
    (let* ((block-content (org-element-property :value (org-element-at-point)))
           (filename (make-temp-file "temp" nil ".patch" block-content))
           (default-directory target-folder)
           (patch-command (format "patch -p1 < %s" filename))
           (patch-command-result (shell-command-to-string patch-command)))
      (if (string-match-p "^patching file" patch-command-result)
          (message "Patch applied successfully:\n%s" patch-command-result)
        (error "Error applying patch:\n%s" patch-command-result)))))

(cl-defun im-tangle-file (&key doc path contents)
  "Tangle CONTENTS into PATH.
DOC is simply for documentation, have no specific use.  If CONTENTS has
a shebang at the beginning, then the executable bit is set to file."
  (make-directory (file-name-directory path) t)
  (write-region (concat contents "\n") nil path)
  ;; Make it executable if needed
  (when (s-matches? "^[ \t]*#!" contents)
    (set-file-modes path (file-modes-symbolic-to-number "+x" (file-modes path)))))

;;;; Basics

;;;;; Overriding some defaults

(setq save-silently t)
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")
(setenv "BROWSER" "emacs-browser")
(im-tangle-file
 :doc "Simple `browse-url' wrapper. This enables external processes to call emacs with a url."
 :path "~/.bin/emacs-browser"
 :contents "#!/bin/bash
emacsclient --eval \"(browse-url \\\"$1\\\")\"")

;;;;;; M-Backspace should delete, instead of killing

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

;;;;; Recent files

;; Save recent files. Also exclude package files that appears after
;; installing a package or after an update from recent list.

(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 500)
  (recentf-exclude (list ".*\\.elc"
                         "/tmp/.*"
                         "/var/folders/.*")))

;;;;; reveal-mode

;; Enable the reveal-mode. This is quite needed as some commands that
;; does jumping does not reveal the text if it's hidden and reveal
;; mode does that.

;; (global-reveal-mode)
;; (setq reveal-auto-hide t)

;;;;; Save minibuffer, kill-ring, search-ring history

(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :config
  ;; Clipboard selections are copied into the kill-ring
  (setq save-interprogram-paste-before-kill t)
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (setq savehist-file "~/.emacs.d/savehist"))


;;;;; Better scrolling

;;;;;; Better settings for mouse scroll

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse

;;;;;; Mouse shortcuts for zooming

;; Ctrl-Scroll to zoom in and out

(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)

;;;;;; Conservative scrolling

;; If the cursor is at the end of the file, when you scroll emacs does a strange jump. This fixes it.

(setq scroll-conservatively 101) ;; When cursor reaches end, just scroll line-by-line
;; (setq scroll-margin 10) ;; Start scolling earlier

;;;;; Backups

;; Instead of having a file that ends with ~ or '# files in same
;; directory, save all backup files in =~/.emacs.d/backups=.

(defconst im-backup-dir (im-mkdir-if-not "~/.emacs.d/backups/"))

(setq backup-directory-alist `((".*" . ,im-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,im-backup-dir t)))

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

;;;;; Remove trailing space before save

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;; Make script files executable automatically

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(with-eval-after-load 'ob-tangle
  (add-hook 'org-babel-post-tangle-hook #'executable-make-buffer-file-executable-if-script-p))

;;;;; Automatically run some commands after saving specific files

;; This is like =autocmd BufWritePost= of vim. When a particular file
;; is edited, I want to make sure a command runs after the save.

(defvar im-run-after-save-alist
  '(("~/.\\(Xresources\\|Xdefaults\\)" . "xrdb %; notify-send 'xrdb updated'")
    ("~/.Xresources.d/.*"              . "xrdb ~/.Xresources; notify-send 'xrdb updated'")
    ("~/.config/sxhkd/sxhkdrc"         . "pkill -USR1 -x sxhkd; notify-send 'sxhkd updated'")
    ("~/.config/skhd/skhdrc"           . "skhd --reload; osascript -e 'display notification \"skhd updated\"'")
    ("~/.config/kmonad-linux.kbd"      . "systemctl --user restart kmonad"))
  "File association list with their respective command.")

(add-hook 'after-save-hook #'im-post-save-run-command)
(with-eval-after-load 'ob-tangle
  (add-hook 'org-babel-post-tangle-hook #'im-post-save-run-command))

(defun im-post-save-run-command ()
  "Execute the specified command after saving specified file."
  (when-let* ((fname (buffer-file-name))
              (match (im-assoc-regexp fname im-run-after-save-alist #'expand-file-name)))
    (mapcar
     (-lambda ((_ . command))
       (cond
        ((stringp command)
         (shell-command (s-replace "%" fname command)))
        ((functionp command)
         (funcall command fname))
        (t
         (message ">> Wrong specification in `im-run-after-save-alist' for %s" fname))))
     match)))

;;;;; repeat-mode

;; Enables you to have repeatable keybindings.

(use-package repeat
  :straight (:type built-in)
  :hook (after-init . repeat-mode))

(defmacro im-make-repeatable (name &rest pairs)
  "Put given PAIRS in a keymap named NAME and mark them as repeatable."
  (declare (indent 1))
  (let ((pairs (cl-loop for (a b) on pairs by #'cddr collect (list a b)))
        (map-name (intern (format "im-repeat-map-for-%s" name))))
    `(progn
       (defvar ,map-name
         (let ((map (make-sparse-keymap)))
           ,@(mapcar (lambda (it) `(define-key map (kbd ,(car it)) ',(cadr it))) pairs)
           map))
       (-each ',(mapcar #'cadr pairs)
         (lambda (it) (put it 'repeat-map ',map-name))))))

;;;; Visuals

;;;;; General

;; Disable menubar
(menu-bar-mode 0)

;; Wrap long lines.
;; (global-visual-line-mode t)

;; This sometimes causes performance problems. So
;; instead of calling `global-visual-line-mode', I enable it per-mode
;; basis.  Here are some basic modes that it's enabled for:

(add-hook 'TeX-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; Highlight current line
(global-hl-line-mode t)

(defun im-disable-hl-line-mode-for-buffer ()
  "Disable `global-hl-line-mode' for current buffer only.
I explicitly disable this mode for some modes in my configuration
using this function."
  (setq-local global-hl-line-mode nil))

;;;;; Themes

(use-package modus-themes
  :defer t)

(use-package ef-themes
  :defer t)

(use-package kaolin-themes
  :defer t)

(use-package doom-themes
  :defer t
  :config
  ;; Fix for doomemacs/themes#720
  (custom-set-faces
   `(tab-bar
     ((t (:background ,(or (doom-color 'bg-alt) 'unspecified) :foreground ,(or (doom-color 'fg-alt) 'unspecified)))))))

(use-package im-adaptive-theme
  :straight nil
  :hook (after-init . im-adaptive-theme-enable)
  :custom
  (im-adaptive-theme-detect-geolocation-automatically t)
  (im-adaptive-theme-day-themes
   '(;; Pinkish white theme, really nice to look at.
     ef-summer))
  (im-adaptive-theme-night-themes
   '(;; Grayish theme with great colors.
     doom-one
     ;; A regular black theme with nice amount of contrast.
     modus-vivendi
     ;; Like solarized but much nicer colors.
     ef-melissa-dark
     ;; like ef-summer but dark. Black and purple.
     ef-cherie
     ;; Nice dark theme with good contrast.
     doom-Iosvkem)))

;;;;; Fonts

(defconst im-fonts
  '("Cascadia Code NF"
    "FiraCode Nerd Font"
    "Iosevka Nerd Font"
    "IBM Plex Mono"
    "Iosevka Comfy Motion"
    "Iosevka Comfy"
    "Liberation Mono"
    "Noto Sans Mono")
  "Fonts that I use.")

(defvar im-current-font nil
  "Holds the currently used font name.
One of `im-fonts'.")

(defconst im-font-height (im-when-on :linux 108 :darwin 150))

(defun im-set-font (&optional next)
  "Set the first available font from the `im-fonts' list.
If NEXT is non-nil, then use the next font."
  (interactive "P")
  (and-let* ((fonts (-filter #'im-font-exists-p im-fonts))
             (font (if next
                       (or (cadr (member im-current-font fonts)) (car fonts))
                     (car fonts))))
    (set-face-attribute 'default nil
                        :font font
                        :weight 'normal
                        :width 'normal
                        :height im-font-height)
    (setq im-current-font font)))

(add-hook 'after-init-hook #'im-set-font 99)

;;;;; prettify-symbols-mode

;; I make use of this mode quite frequently throughout the configuration.

(setq prettify-symbols-unprettify-at-point t)

(defmacro im-prettify-mode (mode pairs)
  "Prettify given PAIRS in given MODE.
Just a simple wrapper around `prettify-symbols-mode`"
  (declare (indent 1))
  (let ((hook (intern (concat (symbol-name mode) "-prettify-symbols-hook"))))
    `(progn
       (defun ,hook ()
         (setq prettify-symbols-alist (append prettify-symbols-alist ,pairs))
         (prettify-symbols-mode 1))
       (add-hook ',mode #',hook))))

;;;;; Frame title

;; Make window title contain buffer name so it's easier to identify
;; windows. I use ~rofi~ to switch between windows in my DE, so it
;; helps to have buffer name in window title.

(setq frame-title-format "%b - emacs")

;;;;; Parentheses

;;;;;; Matching

;; Just enable parenthesis matching.

(use-package show-paren
  :straight (:type built-in)
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis))

;;;;;; Rainbow

;; Colors parentheses depending on their dept.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;; Highlight trailing spaces

;; Following highlights trailing spaces. Also see: [[Remove trailing
;; space before save]]

(use-package whitespace
  :hook (after-init . global-whitespace-mode)
  :config
  (setq whitespace-style '(face empty tabs trailing))
  (setq whitespace-global-modes '(not org-mode markdown-mode vterm-mode magit-log-mode nov-mode eshell-mode dired-mode dirvish-mode w3m-mode eat-mode)))

(defun im-whitespace-mode-toggle ()
  "Toggle between more and less agressive whitespace modes.
Toggles between showing every whitespace (tabs, spaces, newlines
etc.) and only showing trailing spaces and tabs.  By default I use
the latter but sometimes I want to see everything and the
function helps me go between these modes easily."
  (interactive)
  (if (member 'spaces whitespace-style)
      (setq whitespace-style '(face empty tabs trailing))
    (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
  (whitespace-mode 0)
  (whitespace-mode 1))

;;;;; Spaces instead of tabs

;; TODO: This breaks org-mode.
;; (setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;;;; Shackle windows

;; Make some temproary windows appear at bottom. This makes buffer
;; management so much easier. Buffers that will match given regex will
;; appear at bottom while covering the given amount of screen.

;; SOURCE: https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
(defun im-shackle-window (name size)
  "Make the buffer NAME appear at bottom of the window, filling SIZE percent of the window."
  (add-to-list 'display-buffer-alist
               `(,name
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . ,size))))

(defun im-clear-side-windows ()
  "Clear all side windows.
This is sometimes required to get around the error: `Cannot make
side window the only window'"
  (interactive)
  (when (window-with-parameter 'window-side)
    (window-toggle-side-windows)))

;;;;; Miscellaneous packages

;; Some small packages that enriches editing experience visually. I
;; don't enable all of them by default, I enable most of them whenever
;; I need the functionality. I utilize an appearance [[Hydra]] to
;; quickly toggle the functionality I need.

;; Show column guidelines
(use-package fill-column-indicator
  :defer t)

;; By default Emacs scales fonts with text-scale-{increase,decrease}
;; per buffer. This scales fonts with
;; default-text-scale-{increase,decrease} globally.
(use-package default-text-scale
  :commands (default-text-scale-decrease default-text-scale-decrease))

;; This shows some indent guides and it's highly configurable.
(use-package highlight-indent-guides
  :defer t
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

;;;;; Spacious padding

;; Add some padding to Emacs frame to freshen things up.

;; Disabled for now.
(use-package spacious-padding
  :commands (spacious-padding-mode))

;;;;; page-break-lines

;; I used to separate files using  but then I discovered [[outli]]
;; and it did the same job but better. I still use page-break-lines
;; mode in some cases where I need to draw a horizontal line in the
;; buffer to separate some stuff.

(use-package page-break-lines
  :autoload (page-break-lines-mode))

;; C-x [,] are bound to {backward,forward}-page, let's make them
;; repeatable:
(im-make-repeatable page-movement
  "[" backward-page
  "]" forward-page)

;;;; evil-mode

;;;;; Basic configuration

(use-package evil
  :demand t
  :init
  ;; Following two is required by evil-collection. It's probably wiser
  ;; to set evil-want-keybinding to t if you will not use
  ;; evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; This generally confuses me.
  (setq evil-jumps-cross-buffers nil)
  ;; Disable evil bindings in insert mode. This needs to be called
  ;; before loading evil mode...
  (setq evil-disable-insert-state-bindings t)
  ;; This seems to be the most reliable undo system.  Also see `vundo'
  ;; package down below for undo-tree like functionality
  (setq evil-undo-system 'undo-fu)
  ;; C-i interferes with TAB key, so disable it
  (setq evil-want-C-i-jump nil)
  :config
  ;; ...but I want some default evil bindings in insert mode, so just
  ;; remap them

  (evil-define-key 'insert 'global
    (kbd "C-d") #'evil-shift-left-line
    (kbd "C-t") #'evil-shift-right-line
    (kbd "C-n") #'evil-complete-next
    (kbd "C-p") #'evil-complete-previous
    (kbd "C-o") #'evil-execute-in-normal-state)

  ;; C-a is `completion-at-point' but I like to be standard Emacs
  ;; beginning-of-line. I already bind `M-o p' to
  ;; `completion-at-point'
  (define-key evil-command-line-map (kbd "C-a") #'beginning-of-line)

  ;; C-i is bound to TAB, so I use C-l for `evil-jump-forward'
  (evil-define-key 'normal 'global (kbd "C-l") #'evil-jump-forward)
  (define-advice evil-jump-backward (:after (&rest _) reveal) (reveal-post-command))
  (define-advice evil-jump-forward (:after (&rest _) reveal) (reveal-post-command))

  ;; When I paste something in visual mode, I don't want it to take
  ;; over the kill ring I also use evil-exchange, which eliminates the
  ;; need for this totally
  (setq evil-kill-on-visual-paste nil)

  ;; Over the time I found myself utilizing emacs C-u more and more,
  ;; so disable this
  (setq evil-want-C-u-scroll nil)

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; Open ex-mode with `<`> text instead of '<'> by default while
  ;; visual mode is active. This way commands will work on selected
  ;; char range instead of selected line range.
  (setq evil-ex-visual-char-range t)

  ;; This is needed for being able to use *-eval-last-sexp kind of
  ;; functions in normal mode. Elisp-related ones works out of the box
  ;; but other ones (like for Racket, Clojure etc.) are not patched by
  ;; default.
  (setq evil-move-beyond-eol t)

  ;; Makes # and * search for symbols instead of words.
  (setq evil-symbol-word-search t)

  ;; Move between visual lines instead of real lines
  (evil-define-key 'normal 'global
    (kbd "j") #'evil-next-visual-line
    (kbd "k") #'evil-previous-visual-line)
  (evil-define-key 'motion 'global
    (kbd "j") #'evil-next-visual-line
    (kbd "k") #'evil-previous-visual-line)

  ;; Use default yank-pop because it integrates itself with consult
  ;; The binding may seem a bit weird but it's how it's done.
  (define-key evil-normal-state-map [remap yank-pop] 'yank-pop)

  ;; Change cursor colors based on current mode.
  (setq evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box)
        evil-emacs-state-cursor '("purple" box)
        evil-insert-state-cursor '("pink" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))

  (evil-mode 1))

;;;;; general.el

(use-package general
  ;; Not required to be installed after evil but loading it before
  ;; causes some problems like keys not getting bound sometimes etc.
  :demand t
  :config
  (general-override-mode))

(general-create-definer im-leader
  :prefix "<SPC>"
  ;; This is important because if you don't set it some modes will
  ;; fail to pick up leader bindings.  See:
  ;; https://github.com/noctuid/general.el/issues/190
  :keymaps 'override
  :states 'normal)

(general-create-definer im-leader-v
  :prefix "<SPC>"
  :keymaps 'override
  :states '(normal visual))

(general-create-definer im-leader2
  :prefix "<f16>"
  :keymaps 'override
  :states 'normal)

(general-create-definer im-leader2-v
  :prefix "<f16>"
  :keymaps 'override
  :states '(normal visual))

;;;;; evil-collection

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-want-unimpaired-p t)
  (evil-collection-unimpaired-want-repeat-mode-integration t))

(use-package grep
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-grep-setup))

(use-package man
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-man-setup))

(use-package replace
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-replace-setup))

(use-package compile
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-compile-setup))

(use-package xref
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-xref-setup))

(use-package help
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-help-setup))

(use-package imenu
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-imenu-setup)
  (evil-collection-imenu-list-setup))

(use-package custom
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-custom-setup))

;;;;;; evil-unimpaired

;; Apparently [[evil-collection]] has a vim-unimpaired implementation
;; already. It contains bindings like:
;;   - ~[<SPC>~ ~]<SPC>~ Insert newline above/below.
;;   - ~[b~ ~]b~ Go to prev/next buffer.
;;   - ~[p~, ~]p~ Paste up/down.
;;   - ~[e~, ~]e~ Move line up/down.
;;   - ~[d~, ~]d~ Delete line above/below.
;;   - ~[q~ ~]q~ Go to prev/next error.
;;   - ~[Q~ ~]Q~ Go to first/last error.
;;   - ~[n~ ~]n~ Go to prev/next conflict marker.
;;   - ~[t~ ~]t~ Go to prev/next TODO. (This is defined in [[Dummy IDE mode]])

;; These also support repeat-mode. You can do ~]b~ and spam ~b~ to switch buffers.

;; Following are my extensions:

(with-eval-after-load 'evil-collection
  (evil-collection-init 'unimpaired)

  (define-advice evil-collection-unimpaired-previous-error (:after (&rest _) recenter) (recenter))
  (define-advice evil-collection-unimpaired-next-error (:after (&rest _) recenter) (recenter))

  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    "[d" #'im-delete-line-above
    "]d" #'im-delete-line-below)

  (defun im-delete-line-above ()
    "Delete the line above."
    (interactive)
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (kill-line)
      (when (s-blank? (s-trim (thing-at-point 'line t)))
        (kill-line))))

  (defun im-delete-line-below ()
    "Delete the line below."
    (interactive)
    (save-excursion
      (forward-line 1)
      (beginning-of-line)
      (kill-line)
      (when (s-blank? (s-trim (thing-at-point 'line t)))
        (kill-line)))))

;;;;; evil-mc (multiple cursors)

;; Multiple cursors for evil.

;; - Basics
;;   - =C-n= / =C-p= are used for creating cursors
;;   - =M-n= / =M-p= are used for moving between cursors
;;   - =A= and =I= creates cursors in visual selection mode as you may expect.
;;   - =gkk= to clear all cursors.
;;
;; - To be able to create cursors at arbitrary positions:
;;   - =gkp= to pause all cursors. (Your main cursors moves freely while mc cursors stays still)
;;   - =gkr= to resume paused cursors.
;;   - =gkh= create a cursor at the point of main cursor. (Use after =gkp=).

(use-package evil-mc
  :after evil
  :general
  (:states '(normal visual) :keymaps 'evil-mc-key-map
   ;; Clear default evil-mc bindings
   "gr" nil
   ;; Add my bindings using "gc"
   "gcc" #'evil-mc-undo-all-cursors
   "gcp" #'evil-mc-pause-cursors
   "gcr" #'evil-mc-resume-cursors
   "gch" #'evil-mc-make-cursor-here)
  (:states 'visual
   "A" #'evil-mc-make-cursor-in-visual-selection-end
   "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  :hook
  (after-init . global-evil-mc-mode))

;;;;; evil-surround

;; - Change surroundings. Do =cs"'= to turn ="Hello world!"= into ='Hello world!'=.
;;   - ='Hello world!'= ~cs'<q>~ =<q>Hello world!</q>=
;;   - =Hel|lo= ~ysiw"~ ="Hello"= (| is the cursor position.)
;;   - =Hello= ~ysw{~ ={ Hello }=  (~{[(~ adds spaces)
;;   - =Hello= ~ysw}~ ={Hello}=    (~}])~ does not add spaces)
;;
;; - Wrap selection with ~<visual-state> S~.
;; - Wrap selection on new lines with ~<visual-state> gS~

(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode 1)

  ;; Invert some default pairs
  (require 'map)
  (map-put! evil-surround-pairs-alist ?\( '("(" . ")"))
  (map-put! evil-surround-pairs-alist ?\) '("( " . " )"))
  (map-put! evil-surround-pairs-alist ?\[ '("[" . "]"))
  (map-put! evil-surround-pairs-alist ?\] '("[ " . " ]"))
  (map-put! evil-surround-pairs-alist ?\{ '("{" . "}"))
  (map-put! evil-surround-pairs-alist ?\} '("{ " . " }"))

  ;; Convert `` to `' in emacs-lisp mode
  (add-hook 'emacs-lisp-mode-hook (lambda () (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

;;;;; key-chord

;; Return back to normal mode using ~jk~ from anywhere. It does not
;; play well with multiple cursors, so use ~ESC~ to when using evil-mc
;; related stuff.

;; I used to use evil-escape but lately it started causing random
;; freezes (as I understand from the backtraces), so I replaced it
;; with key-chord. So far so good. It also feels less clunkier
;; somehow.

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" #'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" #'evil-normal-state))

;;;;; evil-matchit

;; Jump between matching tags using ~%~, like =<div>...</div>=,
;; ={...}= etc. =ci%=, =da%= etc. works as expected.

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;;;;; evil-goggles

;; ~evil-goggles~ gives nice visual feedbacks while editing with
;; evil-mode. When you do =dd=, =yw=, =ciw= or something similar, it
;; will give a visual feedback for the selection. Feels kinda natural
;; to have this.

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-duration 0.20
        evil-goggles-pulse nil
        evil-goggles-enable-change t
        evil-goggles-enable-delete t
        evil-goggles-enable-indent t
        evil-goggles-enable-yank t
        evil-goggles-enable-join t
        evil-goggles-enable-fill-and-move t
        evil-goggles-enable-paste t
        evil-goggles-enable-shift t
        evil-goggles-enable-surround t
        evil-goggles-enable-commentary t
        evil-goggles-enable-nerd-commenter t
        evil-goggles-enable-replace-with-register t
        evil-goggles-enable-set-marker t
        evil-goggles-enable-undo t
        evil-goggles-enable-redo t)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;;;;; evil-snipe

;; - Overall better =f/F/t/T= and . Nice visual feedbacks.

(use-package evil-snipe
  :after evil
  :hook (magit-mode . turn-off-evil-snipe-override-mode)
  :demand t
  :config
  ;; (evil-snipe-mode 1) ;; This enables s/S bindings. I use those keys with avy
  (evil-snipe-override-mode 1) ;; This overrides default f/F, t/T bindings
  (setq evil-snipe-scope 'visible)
  ;; See https://github.com/hlissner/evil-snipe/issues/72
  (setq evil-snipe-skip-leading-whitespace nil))

;;;;; evil-exchange

;; Change two parts of the text.
;; - Mark some text in visual mode and do =gx=.
;; - Mark some other text in visual mode and do =gx= again to exchange two parts.
;; - You can use ~gx<motion>~ instead of visual mode too.

(use-package evil-exchange
  :config
  (evil-exchange-install))

;;;;; evil-visualstar

;; With this package, you can do a visual selection and ~*~, ~#~ keys will work on them.

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode 1))

;;;;; evil-numbers

;; Pretty useful for macros. Increment or decrement number at point
;; with ~+,-~ in normal mode.

(use-package evil-numbers
  :custom
  (evil-numbers-use-cursor-at-end-of-number t)
  (evil-numbers-pad-default t)
  :general
  (:states '(normal visual)
   "+" #'evil-numbers/inc-at-pt
   "-" #'evil-numbers/dec-at-pt))

;;;;; goto-chg

;; =g;= goes to the last change. (repeatable)

;; There is also =gv= which selects the last selection. Not related to
;; this package, it's a default functionality but I wanted to mention.

(use-package goto-chg
  :after evil
  :commands (evil-goto-last-change)
  :config
  (define-advice evil-goto-last-change (:after (&rest _) recenter)
    (reveal-post-command)
    (recenter)))

(im-make-repeatable evil-goto-chg
  ";" evil-goto-last-change)

;;;;; Custom text-objects

;;;;;; org blocks

;; There is `org-babel-mark-block' but it only works for source blocks
;; but this one works for everything between #begin_<> ... #end_<>.
;; There is also "e" object defined by evil-org, which works for quite
;; most things but it does not work, for example, org-ai blocks etc.

(evil-define-text-object im-evil-inner-org-block (count &optional _beg _end _type)
  "Select inner side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *#\\+begin" "^ *#\\+end"))

(evil-define-text-object im-evil-outer-org-block (count &optional _beg _end _type)
  "Select outer side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *#\\+begin" "^ *#\\+end" t))

(evil-define-text-object im-evil-inner-org-block (count &optional _beg _end _type)
  "Select inner side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *#\\+begin" "^ *#\\+end"))

(evil-define-text-object im-evil-outer-org-block (count &optional _beg _end _type)
  "Select outer side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *#\\+begin" "^ *#\\+end" t))

(evil-define-text-object im-evil-inner-markdown-block (count &optional _beg _end _type)
  "Select inner side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *```" "^ *```"))

(evil-define-text-object im-evil-outer-markdown-block (count &optional _beg _end _type)
  "Select outer side of org source blocks."
  :extend-selection nil
  (im-find-block-range "^ *```" "^ *```" t))

(define-key evil-inner-text-objects-map "#" 'im-evil-inner-org-block)
(define-key evil-outer-text-objects-map "#" 'im-evil-outer-org-block)

;; Doing vi` already works for `this`, and now ~ works for blocks.
(define-key evil-inner-text-objects-map "~" 'im-evil-inner-markdown-block)
(define-key evil-outer-text-objects-map "~" 'im-evil-outer-markdown-block)

(defun im-find-block-range (beginning end &optional include-hash-lines)
  "Find the range around the cursor between BEGINNING and END."
  (let ((up-pos nil)
        (down-pos nil))
    (save-excursion
      (beginning-of-line)
      (while (and (not up-pos) (not (bobp)))
        (forward-line -1)
        (beginning-of-line)
        (when (looking-at beginning)
          (unless include-hash-lines
            (forward-line 1))
          (setq up-pos (point)))))
    (when up-pos
      (save-excursion
        (beginning-of-line)
        (while (and (not down-pos) (not (eobp)))
          (forward-line 1)
          (beginning-of-line)
          (when (looking-at end)
            (when include-hash-lines
              (forward-line 1))
            (setq down-pos (point))))))
    (when (and up-pos down-pos)
      (list up-pos down-pos))))

;;;;; Extra repeatable keys for evil

(im-make-repeatable evil-section
  "[" evil-backward-section-begin
  "]" evil-forward-section-begin)

;;;; org-mode

;;;;; Tips etc.

;; Some lesser known functions:
;; - org-copy-visible :: Useful for only sending the outline. Also works with selection.
;; - orgtbl-create-or-convert-from-region :: Useful for data that is separated by \t or spaces.
;; - After converting to table, I can use ~org-table-sort-lines~. Alternative is using ~sort-fields~ without converting the data into table but it's not that nice to use.
;; - I generally use it in combination with ~embark-collect~. Any completing-read → embark-collect → orgtbl-create-or-convert-from-region.

;;;;; Constants

(defconst inbox-org "~/Documents/notes/inbox.org")
(defconst watchlist-org "~/Documents/notes/watchlist.org")
(defconst readinglist-org "~/Documents/notes/readinglist.org")
(defconst courses-org "~/Documents/notes/courses.org")
(defconst bullet-org "~/Documents/notes/bullet.org")
(defconst directory-notes-org "~/Documents/notes/directory-notes.org")
(defconst life-org "~/Documents/notes/life.org")
(defconst reality-org "~/Documents/notes/reality.org")
(defconst projects-org "~/Documents/notes/projects.org")
(defconst gpt-org "~/Documents/notes/extra/gpt.org")
(defconst people-org "~/Documents/notes/people.org")
(defconst diary-org "~/Documents/notes/diary.org")
(defconst snippets-org "~/Documents/notes/snippets.org")
(defconst bookmarks-org "~/Documents/notes/bookmarks.org")
(defconst work-org "~/Documents/notes/trendyol.org")
(defconst temp-org "~/Documents/notes/temp.org")
(defconst passwords-org "~/Documents/notes/passwords.org")
(defconst engineering-org "~/Documents/notes/engineering.org")
(defconst netherlands-org "~/Documents/notes/netherlands.org")

;;;;; org-plus-contrib

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-return-follows-link t)
  ;; ^ Open links with RET
  (org-src-fontify-natively t)
  ;; ^ Enable code highlighting in ~SRC~ blocks.
  (org-src-window-setup 'current-window)
  ;; ^ Open indirect edits in current window and don't mess with window config at all
  (org-hierarchical-todo-statistics t)
  ;; ^ Show all children in todo statistics [1/5]
  (org-imenu-depth 7)
  ;; ^ include up to 7-depth headers in imenu search
  (org-image-actual-width nil)
  ;; ^ Disable showing inline images in full width. Now you can add `#+ATTR_*: :width 300` to resize inline images
  ;; (setq org-ellipsis "⤵")
  ;; (setq org-ellipsis "…")
  ;; ^ Replace ... with … in collapsed sections
  (org-hide-emphasis-markers t)
  ;; Hide *...* /.../ etc.
  (org-pretty-entities t)
  (org-log-into-drawer t)
  ;; ^ Log into LOGBOOK drawer instead of directly loging under the heading
  (org-extend-today-until 3)
  ;; ^ Consider the current day to end at 3AM
  (org-use-effective-time t)
  ;; ^ Make timestamp processing functions aware of the previous config
  (org-element-use-cache nil)
  ;; ^ Cache causes bunch of random errors although disabling cache
  ;; reduces the agenda performance significantly
  (org-tags-column 0)
  ;; Tags starts right after the heading.
  (org-reverse-note-order t)
  ;; ^ I keep new notes at the beginning. This helps with that.
  (org-confirm-babel-evaluate nil)
  ;; ^ Don't ask permissions for evaluating code blocks
  (org-clock-clocked-in-display 'mode-line)
  ;; ^ Shows in the tab-bar, if tab-bar is enabled.
  (org-habit-show-habits nil)
  ;; ^ Speeds up agenda a bit
  (org-edit-src-content-indentation 0)
  ;; ^ No indentation for src blocks
  (org-cycle-include-plain-lists 'integrate)
  ;; ^ Also toggle visibility of plain list with TAB etc. like they are subheadings
  (org-fold-core-style 'overlays)
  ;; ^ Documentation says text-properties are more buggy but my
  ;; experience is the opposite. At least for now. overlay folding
  ;; messes things up with indirect buffers which I use quite a
  ;; lot. Haven't reported upstream tho.
  ;; UPDATE(2025-02-01): overlays seems to work better now.

  ;; Put archive files under an archive/ directory
  ;; I don't want them to pollute my directory
  (org-archive-location "archive/%s_archive::")
  (org-directory "~/Documents/notes")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; ^ org-store-link creates an ID for header only if called interactively and if there is no custom id

  :config
  ;; Automatically invoke `org-indent-mode' which gives nice little
  ;; indentation under subsections. It makes reading easier. This does
  ;; not add any spaces/tabs to the text file, the indentation is only
  ;; visually apparent in Emacs.
  ;; (add-hook 'org-mode-hook #'org-indent-mode t)

  (defvar im-org-calendar-directory (format "%s/calendars" org-directory))
  (setq im-calendar-files
        (when (file-exists-p im-org-calendar-directory)
          (directory-files im-org-calendar-directory 'full (rx ".org" eos))))
  (setq org-agenda-files `(,inbox-org ,bullet-org ,projects-org ,work-org ,people-org ,readinglist-org ,watchlist-org ,life-org ,netherlands-org ,@im-calendar-files))

  ;; When I unfold an header if the contents are longer than remaining
  ;; window width then cursor recenters to the top of the window. I
  ;; disable with the following line.
  (with-eval-after-load 'org-cycle
    (setq org-cycle-hook (delq 'org-cycle-optimize-window-after-visibility-change org-cycle-hook)))

  (add-to-list 'org-link-abbrev-alist '("imdb" . "https://www.imdb.com/title/%s"))
  (add-to-list 'org-link-abbrev-alist '("yt" . "https://youtu.be/%s"))
  ;; ^ More info: https://orgmode.org/manual/Link-Abbreviations.html

  ;; With the following, I can call functions defined inside this file in other org files
  (org-babel-lob-ingest (concat org-directory "/utils.org")))

(use-package org-contrib :after org)

;; http://www.foldl.me/2012/disabling-electric-indent-mode/
(defun im-disable-electric-indent ()
  (set (make-local-variable 'electric-indent-functions)
       (list (lambda (arg) 'no-indent))))

(add-hook 'org-mode-hook #'im-disable-electric-indent)

;;;;; Keybindings

(use-package evil-org
  :after (org org-agenda)
  :general
  (:keymaps 'org-mode-map :states 'normal
   "<RET>" #'org-return
   "S-<return>" #'im-org-link-to-indirect-buffer)
  (:keymaps 'org-mode-map :states '(insert normal)
   "M-Q" #'im-org-unfill-paragraph)
  (im-leader
    "oyy" #'im-org-store-link-dwim
    "oa"  #'org-agenda
    "ow"  #'widen
    ;; org-capture
    "og" #'org-capture
    "oG" #'org-capture-goto-last-stored
    ;; org-clock
    "occ" #'org-clock-in
    "ocC" #'org-clock-cancel
    "ocr" #'org-clock-in-last
    "ocl" #'org-clock-in-last
    "oco" #'org-clock-out
    "ocg" #'org-clock-goto)
  (im-leader :keymaps 'org-mode-map
    ;; general
    "op"  #'org-set-property
    "oi"  #'org-toggle-inline-images
    "oI"  #'org-redisplay-inline-images
    "or"  #'org-refile
    "oR"  #'org-mode-restart
    "oh"  #'outline-show-only-headings
    "os"  #'org-schedule
    "od"  #'org-deadline
    "ov"  #'org-babel-expand-src-block
    "oq"  #'im-org-tree-to-indirect-buffer
    "oQ"  #'im-org-link-to-indirect-buffer
    "o1"  #'im-show-outline-only
    ;; link stuff
    "oyi" #'org-copy-id
    ;; src blocks etc
    "d" #'org-babel-remove-result
    "D" #'im-org-babel-remove-all-results
    "oo" #'im-org-babel-result-to-buffer
    "oe" #'org-edit-special
    "ot" #'im-org-babel-tangle-current-block
    "o-" #'org-babel-demarcate-block)
  (im-leader :keymaps 'org-src-mode-map
    "ot" #'im-org-babel-tangle-current-block)
  (im-leader-v :keymaps 'org-mode-map
    "o#" #'org-insert-structure-template)
  (im-leader-v :keymaps 'org-src-mode-map
    "oe" #'org-edit-src-exit
    "oE" #'org-edit-src-abort)
  :hook
  (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (add-hook 'org-src-mode-hook #'evil-normalize-keymaps))

(defun im-org-tree-to-indirect-buffer ()
  "Like `org-tree-to-indirect-buffer' but let's you open multiple indirect buffers.
This also restricts the amount of indirect buffers per heading to 1 so
that you can have multiple indirect buffers for different headers but
only one indirect buffer per header to prevent unnecessary accumulation
of indirect buffers.."
  (interactive)
  (if-let* ((target-buffer-name (concat (buffer-name) "::"  (org-get-heading 'no-tags)))
            (existing-buffer (get-buffer target-buffer-name)))
      (switch-to-buffer-other-window existing-buffer)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'org-tree-to-indirect-buffer))))

(defun im-bullet-focus-inbox-indirect ()
  "Like `im-bullet-focus-inbox' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-inbox)))

(defmacro im-org-focused-tree-to-indirect-buffer (focus-buffer &rest forms)
  "Open a tree in an indirect buffer.
First, switch to target buffer by executing FOCUS-BUFFER.  Then execute
FORMS to focus into the header that you want to open in an
indirect-buffer.  Then create an indirect buffer of that header.  This
function does not change any restriction or position of any buffer, so
it saves everything done in FOCUS-BUFFER and FORMS.  With this, you can
work simultaneously with different headers of single org buffer."
  `(let ((current-prefix-arg '(4))
         (source-buffer (current-buffer))
         focused-buffer
         existing-buffer
         target-buffer-name)
     ;; Get the buffer that we want to focus. This is a separate step
     ;; because I want to save the restriction and excursion inside
     ;; that buffer.
     (setq focused-buffer
           (progn
             (save-window-excursion
               (save-restriction
                 ,focus-buffer
                 (point)
                 (current-buffer)))))
     (save-window-excursion
       (with-current-buffer focused-buffer
         (save-excursion
           (save-restriction
             (widen)
             ,@forms
             (setq target-buffer-name (concat (buffer-name) "::"  (org-get-heading 'no-tags)))
             (setq existing-buffer (get-buffer target-buffer-name))
             (unless existing-buffer
               (call-interactively #'org-tree-to-indirect-buffer))))))
     ;; If the link points to another buffer, current window will start
     ;; showing that buffer. We don't want that, so we are restoring
     ;; the current buffer here:
     (set-window-buffer nil source-buffer)
     (switch-to-buffer target-buffer-name)
     (im-show-outline-only)))

(defun im-org-link-to-indirect-buffer ()
  "Open link at point on a new indirect buffer."
  (interactive)
  ;; Force org to open the link in current window
  (with-default-browser
   (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
     (pcase (org-element-property :type (org-element-context))
       ((or "http" "https") (org-open-at-point))
       (_
        (switch-to-buffer-other-window
         (save-window-excursion
           (im-org-focused-tree-to-indirect-buffer
            ;; If we are in an indirect buffer, then find the real
            ;; buffer and widen it so that org-open-at-point
            ;; works. This does not restore narrowing but whatever.
            (progn
              (when-let* ((base (buffer-base-buffer)))
                (with-current-buffer base
                  (switch-to-buffer (current-buffer)))))
            ;; org-open-at-point opens in other-window, prevent that
            (switch-to-buffer
             (save-window-excursion
               (org-open-at-point)
               (current-buffer))))
           (current-buffer))))))))

(defun im-org-clocked-header-to-indirect-buffer ()
  "Open currently clocked header in an indirect buffer.
Normally you can go to clocked header with `org-clock-goto', this simply
opens it in an indirect buffer narrowed to the header so that the
original buffer is not affected and you can work on them simultaneously."
  (im-org-focused-tree-to-indirect-buffer
   (switch-to-buffer (marker-buffer org-clock-marker))
   (goto-char (marker-position org-clock-marker))))

(defun im-show-outline-only ()
  "Show all headers but hide all bodies."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)
    (outline-show-branches)
    (outline-hide-body)))

;;;;; Babel

;;;;;; General configuration

;; Allow these languages to run in code blocks
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (scheme . t)
     (python . t)
     (jupyter . t)
     (julia . t)
     (verb . t)
     (http . t)
     (R . t)
     (haskell . t)
     (js . t)
     (sql . t)
     (dot . t)
     (gnuplot . t)
     (plantuml . t))))

;; TODO: im-load-ob-lang-lazy
;; (defmacro im-load-ob-lang-lazy (&rest langs)
;;   "Like `org-babel-do-load-languages' but load them lazily using `use-package'."
;;   `(progn
;;      ,@(mapcar (lambda (lang)
;;                  `(use-package ,(intern (concat "ob-" (symbol-name lang)))
;;                     :defer t
;;                     :straight (:type built-in)
;;                     ;; TODO add view
;;                     :commands (,(intern (concat "org-babel-execute:" (symbol-name lang))))))
;;                langs)))
;; (im-load-ob-lang-lazy
;;  emacs-lisp shell scheme python verb http R haskell js sql dot gnuplot plantuml)

;;;;;; Helper functions

;; Some codeblocks produce image files as it's result (like dot
;; language). Re-executing these blocks removes the image
;; overlay. With this hook images are automatically updated after
;; code-block execution and not removed from screen.
(add-hook 'org-babel-after-execute-hook #'im-org-redisplay-images-if-enabled)

(defalias 'im-org-babel-split-or-wrap-src-code-block #'org-babel-demarcate-block)

(defun im-org-babel-tangle-current-block ()
  "Tangle the current source block and all other related blocks.
All blocks that tangles to same file are tangled.

This function also works inside `org-edit-special' buffers and
does not disrupt the current window configuration."
  (interactive nil org-mode)
  (let ((src-edit? (org-src-edit-buffer-p))
        buffer pos
        (current-prefix-arg '(16)))
    ;;     ^ '(4) only tangles current file, '(16) tangles all code
    ;;     blocks related to current tangle file target
    (save-window-excursion
      (when src-edit? (org-edit-src-exit))
      (setq buffer (current-buffer))
      (setq pos (point))
      (call-interactively 'org-babel-tangle))
    (when src-edit?
      (let ((org-src-window-setup 'current-window))
        (with-current-buffer buffer
          (save-excursion
            (goto-char pos)
            (org-edit-special)))))))

;; https://emacs.stackexchange.com/questions/23870/org-babel-result-to-a-separate-buffer/27190#27190
(defun im-org-babel-result-to-buffer ()
  "A function to efficiently feed babel code block result to a separate buffer."
  (interactive nil org-mode)
  (org-open-at-point)
  (org-babel-remove-result))

(defun im-org-redisplay-images-if-enabled ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;;;;; ob-http & verb-mode

;; Http request in org-mode babel.  You can get the generated curl
;; command after executing the code block, from *curl command history*
;; buffer or using `:print-curl' parameter to the block.
(use-package ob-http
  :straight (:host github :repo "ag91/ob-http")
  :after org)

;; An alternative to ob-http, might be better
(use-package verb
  :straight (:host github :repo "federicotdn/verb")
  :after org
  ;; :hook (org-mode . verb-mode)
  :config
  (im-leader "ur" #'verb-send-request-on-point))

(defun im-curl-to-org-http (curl-str)
  "Convert CURL-STR into an ob-http block.
More concretely this function converts given curl command (that
is copied from Chrome/Firefox dev tools, using the `Copy as curl'
option) into an ob-http block to be able to use all the goodies
that is provided by ob-http."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Curl string: "))))
  (let ((tokens (im-cmdargs-tokenize curl-str))
        (headers '())
        (request "GET")
        url
        (query-params '())
        data)
    (--each-indexed tokens
      (pcase it
        ((or "-H" "--header")
         (push (nth (1+ it-index) tokens) headers))
        ((or "-d" "--data-binary" "--data-raw")
         (setq data (nth (1+ it-index) tokens)))
        ((or "-X" "--request" "--data-raw")
         (setq request (nth (1+ it-index) tokens)))
        ;; Only supports --data-urlencode in the GET context. It will
        ;; append --data-urlencode arguments to url as query string.
        ("--data-urlencode"
         (push (nth (1+ it-index) tokens) query-params))
        ((pred (lambda (x) (s-prefix? "http" x)))
         (setq url it))))
    (when-let* ((query-params)
                (qs (url-build-query-string (--map (s-split-up-to "=" it 2) query-params))))
      (if (s-contains? "?" url)
          (setq url (concat url "&" qs))
        (setq url (concat url "?" qs))))
    (insert
     (s-trim (im-s-interpolated "#{request} #{url}\n#{(s-join \"\\n\" headers)}\n\n#{(or data \"\")}")))))

(defun im-cmdargs-tokenize (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (let ((tokens '()))
      (while-let ((chr (get-byte))
                  ((not (eobp))))
        (pcase chr
          ((or ?\s ?\t ?\n) (forward-char))
          (?\"
           (forward-char)
           (push
            (replace-regexp-in-string
             (regexp-quote "\\\"") "\""
             (buffer-substring
              (point)
              (1- (re-search-forward "[^\\]\"" nil t))))
            tokens))
          (?\'
           (forward-char)
           (push (buffer-substring
                  (point)
                  (1- (re-search-forward "'" nil t)))
                 tokens))
          (_ (push (buffer-substring
                    (point)
                    ;; TODO Handle eol
                    (1- (re-search-forward "[ \t]" nil t)))
                   tokens))))
      (nreverse tokens))))

;;;;; Exporting

;;;;;; HTML

(use-package htmlize :after org)

;;;;;; iCalendar settings

(with-eval-after-load 'org
  (setq org-icalendar-store-UID t)
  (setq org-icalendar-alarm-time 15)
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo))
  (setq org-icalendar-use-deadline '(todo-due event-if-todo)))

;;;;; Agenda

;; Some general settings.

(use-package org-agenda
  :straight nil
  :after org
  :general
  (:states 'normal :keymaps 'org-agenda-mode-map
   (kbd "<RET>") #'org-agenda-switch-to
   (kbd "\t") #'org-agenda-goto
   "s" #'org-agenda-schedule
   "w" #'org-agenda-week-view
   "d" #'org-agenda-day-view
   "t" #'org-agenda-todo
   "L" #'org-agenda-log-mode
   "q" #'org-agenda-quit
   "R" #'org-agenda-clockreport-mode
   "r" #'org-agenda-redo)
  :custom
  (org-agenda-remove-tags t)
  (org-agenda-include-diary t)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid
   '((daily today require-timed remove-match)
     (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  ;; Make it open faster
  ;; https://orgmode.org/manual/Speeding-Up-Your-Agendas.html
  ;; https://orgmode.org/worg/agenda-optimization.html
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-drawer-properties '(effort appt category))
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (add-hook 'org-agenda-finalize-hook #'evil-force-normal-state))

;;;;; org-caldav (calendar sync)

;; Sync calendars with: `org-caldav-sync'.

(use-package org-caldav
  :after org
  :defer t
  :custom
  (org-caldav-url (format "%s/remote.php/dav/calendars/%s" im-secrets-nextcloud-url im-secrets-nextcloud-user))
  (org-caldav-calendars (--map
                         (list
                          :calendar-id it
                          :inbox (format "%s/%s.org" im-org-calendar-directory it))
                         im-nextcloud-calendars))
  ;; TODO: Fix the following, automatically get
  (org-icalendar-timezone "Europe/Amsterdam")
  ;; Set it under the notes directory so that they will be synced
  ;; between devices
  (org-caldav-save-directory "~/Documents/notes/data/")
  ;; Set the following to nil, I already have set `org-caldav-calendars' above
  (org-caldav-inbox nil)
  (org-caldav-calendar-id nil)
  (org-caldav-files nil))

;;;;; ToDo keywords & faces

(with-eval-after-load 'org
  ;; Add this to org files if you need:
  ;; #+TODO: TODO PROG WAITING DONE
  ;; OR
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "DONE(d)")))
  ;; Now you can do C-c C-t {t,p,w,d} to set the state directly

  (setq org-todo-keyword-faces
        '(("WAIT" . (:foreground "yellow" :weight bold))
          ("PROG" . (:foreground "magenta" :weight bold :underline t))))

  (custom-set-faces
   '(org-headline-done ((t (:strike-through t))))
   '(org-agenda-done ((t (:strike-through t))))
   '(org-column ((t (:background unspecified))))
   '(org-scheduled-today ((t (:foreground "light green"))))

   ;; Make some stuff small
   '(org-drawer ((t (:height 0.8))))
   '(org-special-keyword ((t (:height 0.8))))
   '(org-block-end-line ((t (:height 0.8))))))

;;;;; org-capture

;; See [[https://orgmode.org/manual/Template-elements.html#Template-elements][this page]] for more detail on template elements.

;; Some functions that I utilize for capture templates:

(defun im-org-capture--find-daily-last-entry ()
  (im-bullet-focus-today)
  (goto-char (point-max)))

(defun im-org-capture--find-daily-summary ()
  (im-bullet-focus-today)
  (re-search-forward "^** Summary" nil t))

(defun im-org-capture--find-first-top-level-todo-entry ()
  "Find first top-level TODO entry in current buffer.
This way you can insert new entry right after other non-TODO
entries."
  (goto-char (point-min))
  (re-search-forward "^\\* TODO" nil t)
  (beginning-of-line))

(defun im-org-capture--find-first-N-level-todo-entry (n)
  "Find first N-level TODO entry in current buffer.
This way you can insert new entry right after other non-TODO
  entries."
  (goto-char (point-min))
  (re-search-forward (format "^\\*\\{%s\\} TODO" n) nil t)
  (beginning-of-line))

(defun im-org-capture--find-first-top-level-todo-entry-within-header (header)
  "Find first top-level TODO entry in under HEADER in current buffer."
  (lambda ()
    (goto-char (point-min))
    (let (n)
      (if (re-search-forward (format "^\\(\\*+\\) %s" header) nil t)
          (setq n (1+ (length (match-string 1))))
        (user-error "Cannot find header %s" header))
      (save-restriction
        (org-narrow-to-subtree)
        (im-org-capture--find-first-N-level-todo-entry n)))))

(defun im-org-capture--find-snippet ()
  (let* ((mode-name (with-current-buffer (org-capture-get :original-buffer)
                      (symbol-name major-mode)))
         (result (org-find-exact-headline-in-buffer mode-name)))
    (if result
        (goto-char result)
      (goto-char (point-min))
      (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
      (forward-line -1)
      (insert (format "\n* %s" mode-name)))))

(defun im-org-capture--find-snippet-one-liner ()
  (im-org-capture--find-snippet)
  (unless (re-search-forward "\\*\\* One-liners" nil t)
    (end-of-line)
    (insert "\n** One-liners")))

(with-eval-after-load 'org
  (setq
   org-capture-templates
   `(("i" "Inbox" item
      (file+headline bullet-org "Inbox")
      "- %U %?"
      :prepend t)

     ("I" "Work Inbox" item
      (file+headline work-org "Inbox")
      "- %U %?"
      :prepend t)

     ("n" "Meeting/clock note" item
      (clock))

     ("t" "Study/investigate later" item
      (file+olp bullet-org "Life backlog" "Investigate")
      "- [ ] %U %(im-org-make-link-string (read-string \"URL: \"))"
      :prepend t)

     ("l" "Life todo" plain
      (file+function bullet-org ,(im-org-capture--find-first-top-level-todo-entry-within-header "Life backlog"))
      "** TODO [#B] %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n")

     ("w" "Work todo" plain
      (file+function bullet-org ,(im-org-capture--find-first-top-level-todo-entry-within-header "Work backlog"))
      "** TODO [#B] %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n" :prepend t)

     ("c" "Computer todo" plain
      (file+function bullet-org ,(im-org-capture--find-first-top-level-todo-entry-within-header "Computer backlog"))
      "** TODO [#B] %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n" :prepend t)

     ("f" "Free time backlog" item
      (file+headline bullet-org "Free time backlog")
      "- %U %?" :prepend t)

     ("T" "Daily TODO" plain
      (file+function bullet-org im-org-capture--find-daily-last-entry)
      "** TODO [#B] %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n")

     ("S" "Daily summary" item
      (file+function bullet-org im-org-capture--find-daily-summary)
      "- %U %?")

     ("a" "Shopping" plain
      (file+function bullet-org ,(im-org-capture--find-first-top-level-todo-entry-within-header "Alisveris"))
      "*** TODO [#B] %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n"
      :prepend t)

     ("m" "Watchlist item" plain
      (file+function watchlist-org im-org-capture--find-first-top-level-todo-entry)
      "* TODO %?\n:PROPERTIES:\n:CREATED_AT: %U\n:WHY: FILLME!\n:END:\n\n")

     ("D" "Diary" entry
      (file diary-org)
      "* %u\n"
      :prepend t)

     ("s" "Snippets")

     ("ss" "Snippet" entry
      (file+function snippets-org im-org-capture--find-snippet)
      "** ")

     ("so" "One liner snippet" item
      (file+function snippets-org im-org-capture--find-snippet-one-liner)
      "- %? :: "))))

;;;;; org-refile

(with-eval-after-load 'org
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
  ;; Show the full path of the header
  (setq org-refile-use-outline-path 'file)
  ;; Make the refile completion work with vertico
  (setq org-outline-path-complete-in-steps nil)
  ;; Add new headers by appending "/new header name" and refile creates it for you
  (setq org-refile-allow-creating-parent-nodes 'confirm))

;;;;; org-modern

(use-package org-modern
  :after org
  :custom
  (org-modern-timestamp nil)
  (org-use-sub-superscripts nil)
  (org-modern-table nil)
  (org-modern-hide-stars " ")
  (org-modern-list
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  (org-modern-todo-faces
   '(("PROG" :background "purple" :foreground "plum1")
     ("WAIT" :background "goldenrod3" :foreground "yellow2")))
  (org-modern-priority-faces
   '((?A :background "forest green" :foreground "beige")
     (?B :background "goldenrod" :foreground "beige")
     (?C :background "coral" :foreground "beige")
     (?D :background "red3" :foreground "beige")))
  :config
  (add-hook 'org-agenda-finalize #'org-modern-agenda)
  ;; 99 is important or otherwise org-jupyter-client fails to
  ;; highlight ansi escape codes for some reason.
  (add-hook 'org-mode-hook #'org-modern-mode 99))

;;;;; org-rainbow-tags

(use-package org-rainbow-tags
  :custom
  (org-rainbow-tags-hash-start-index 6)
  (org-rainbow-tags-extra-face-attributes
   '(:inverse-video t :box nil :weight 'bold))
  :hook
  (org-mode . org-rainbow-tags-mode))

;;;;; org-ql

(use-package org-ql
  :defer t
  :after org
  ;; Load org-ql-search prematurely to be able to use org-ql blocks in
  ;; org-mode
  :init
  (with-eval-after-load 'org
    (require 'org-ql-search)))

;; Here are some utility functions that I use in org-ql dynamic blocks:

(defun sort-by-num-prop (prop x y)
  (< (string-to-number (or (org-element-property prop y) "0"))
     (string-to-number (or (org-element-property prop x) "0"))))

(defun sort-by-prop (prop x y)
  (string< (or (org-element-property prop y) "")
           (or (org-element-property prop x) "")))

;; You have to use ~:sort (lambda ...)~ syntax in org-ql dynamic
;; blocks if you want to supply a function for the ~:sort~
;; parameter. You can't use a function that returns a lambda, hence
;; the functions defined above should be used like this:

;; #+begin: org-ql :query ... :sort (lambda (x y) (sort-by-num-prop :RATING x y))
;; #+end

;; Here are some predefined searches:

(defun im-org-ql-current-week-tasks ()
  (interactive)
  (let ((week-end (im-date "next monday - 1 day")))
    (org-ql-search org-agenda-files
      `(and (or (scheduled :to ,week-end)
                (deadline :to ,week-end))
            (not (done)))
      :title (format "W%s" (format-time-string "%U"))
      :super-groups '((:auto-category))
      :sort '(date))))

;;;;; Linking improvements

;; Org does not provide an easy way to copy link at point. Here is a fix for that:

;; Source: https://emacs.stackexchange.com/a/60555
(defun im-org-link-copy ()
  "Extract URL from `org-mode' link and add it to kill ring."
  (interactive)
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (when (and type url (not (s-blank? url))) (concat type ":" url))))
    (when (called-interactively-p 'any)
      (message (concat "Copied URL: " (im-kill url))))
    url))

(defun im-org-store-link-dwim ()
  "Like `org-store-link' but if point is on an org-link, just copy it to clipboard.
Otherwise call `org-store-link'."
  (interactive)
  (if (org-in-regexp org-link-any-re 1)
      (call-interactively #'im-org-link-copy)
    (org-store-link nil t)))

;;;;;; Show expanded links at point

;; This following trick (got it from
;; [[https://www.reddit.com/r/emacs/comments/o68i0v/weekly_tips_tricks_c_thread/h2rizey?utm_source=share&utm_medium=web2x&context=3][this]]
;; comment) simply calls =C-h .= (=display-local-help=) when idle,
;; which shows the destination of links in the echo area (and maybe
;; displays other helpful stuff).

(defun im-help-at-point-mode ()
  "Show tooltips in the echo area automatically for current buffer."
  (interactive)
  (setq-local help-at-pt-display-when-idle t)
  (setq-local help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'im-help-at-point-mode))

;;;;;; Insert links/images more intelligently

;; - if region is selected and there is a url in the clipboard, convert it to a link directly.
;; - if nothing is selected and there is a link in clipboard, just insert it as a link with the link's own title.
;; - if clipboard has in image in it, save that into a file that you interactively select and then insert it into the buffer.
;; - otherwise call ~org-insert-link~

(im-leader-v "oP" #'im-org-insert-dwim)
(general-def :keymaps 'minibuffer-mode-map "C-o" #'im-org-insert-dwim)

(defun im-org-insert-dwim ()
  "Like `org-insert-link' but improved with dwim features.
Based on: https://xenodium.com/emacs-dwim-do-what-i-mean/"
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond
     ((im-clipboard-contains-image-p)
      (call-interactively 'im-org-attach-image-from-clipboard))
     ((and region-content clipboard-url (not point-in-link))
      (delete-region (region-beginning) (region-end))
      (insert (org-make-link-string clipboard-url region-content)))
     ((and clipboard-url (not point-in-link))
      (insert (im-org-make-link-string clipboard-url)))
     (t
      (call-interactively 'org-insert-link)))))

(defun im-org-attach-image-from-clipboard (&optional file-path)
  "Attach the image in the clipboard into your org-buffer.
This function saves the image file into the FILE-PATH or if it's
not provided then it saves the image into ~/.cache."
  (interactive
   (list
    (read-file-name
     "Save file to (leave empty to create a temp file): "
     nil nil nil (im-org-suggest-filename nil ".png"))))
  (let ((file (if (and file-path (not (string-empty-p file-path)))
                  (file-relative-name file-path)
                (expand-file-name (make-temp-file "~/.cache/org_temp_image_" nil ".png")))))
    (if (im-save-clipboard-image-to-file file)
        (insert (format "#+ATTR_ORG: :width 400\n[[file:%s]]" file))
      (user-error "Saving file failed!"))))

(defun im-org-make-link-string (url)
  "Like `org-make-link-string' but fetch URL and extract the title automatically.
Also adds author and duration info to YouTube links."
  (cond
   ((im-youtube-link-extract-id url) (im-org-format-youtube-link url))
   (t (org-link-make-string url (im-url-get-title url)))))

;; YouTube related

(defun im-youtube-link-extract-id (link)
  (nth 4 (s-match "https?://\\(www\\.\\)?\\(youtu\\.be\\|youtube.com\\)/\\(watch\\?v=\\)?\\([a-zA-Z0-9_-]+\\)\\(\\?\\|&\\)?/?" link)))

(defun im-org-format-youtube-link (link)
  "Format given YouTube link as `org-mode' link.
Length, author, title etc. are appended to the link."
  (let ((id (im-youtube-link-extract-id link)))
    (let-alist (im-request (format "%s/videos/%s" empv-invidious-instance id))
      (org-link-make-string
       link
       (format "%0.2f mins, by %s || %s" (/ .lengthSeconds 60.0) .author .title)))))

;;;;;; Find backlinks dynamically

(defun im-org-link-find-backlinks (&optional id)
  "Find backlinks for ID (or current heading)."
  (interactive)
  (let ((default-directory org-directory)
        (id (or id (org-id-get))))
    (unless id
      (user-error "No ID found for current heading"))
    (im-shell-command
     :command "rg"
     :args `(,(format "\\[\\[id:%s\\]" id) "--color=never" "--no-heading")
     :on-finish
     (lambda (out)
       (let* ((infos (--map (-take 2 (s-split-up-to ":" it 2)) (s-split "\n" out t)))
              (backlinks (->>
                          (--map
                           (with-current-buffer (find-file-noselect (f-join default-directory (car it)))
                             (save-excursion
                               (save-restriction
                                 (widen)
                                 (goto-char (point-min))
                                 (forward-line (1- (string-to-number (nth 1 it))))
                                 (format
                                  "- [[file:%s][%s]] :: %s / %s"
                                  (car it)
                                  (car it)
                                  (s-join " → " (--map (format "*%s*" it) (org-get-outline-path)))
                                  (substring-no-properties (org-store-link nil))
                                  (im-org-header-line-to-title (org-entry-get nil "ITEM"))))))
                           infos)
                          (-uniq))))
         (im-peek
          (lambda ()
            (with-current-buffer (get-buffer-create "*im-org-link-backlinks*")
              (erase-buffer)
              (insert (s-join "\n" backlinks))
              (unless (derived-mode-p 'org-mode)
                (org-mode))
              (current-buffer)))))))))

;;;;;; Jump to a header or link a header

;; Keybindings

(im-leader-v
  "ol" #'im-org-link-header
  ;; "oj" is bound to consult-org-agenda
  "oJ" #'im-org-jump-to-header)

;; Utility

(defun im-org-header-line-to-title (line)
  "Remove TODO/*/unnecessary whitespace from given LINE.
Then return the title of given `org-mode` header.  Just
like (org-entry-get nil \"ITEM\") but works on given string."
  (->> line
       (s-replace-regexp "\\(\\*\\|TODO\\|PROG\\|DONE\\|WAIT\\)" "") ;; Remove TODO states
       (s-replace-regexp "\\(\\[#.\\{1\\}\\]\\\)" "") ;; Remove priorities
       (s-replace-regexp ":\\(\\w+:\\)+$"  "") ;; Remove tags
       (replace-regexp-in-string "\\[\\[.*\\]\\[\\(.*\\)\\]\\]"  "\\1") ;; Fix links
       (replace-regexp-in-string "\\[\\[\\(.*\\)\\]\\]"  "\\1") ;; Fix links
       (s-trim)))

(defun im-org-file-get-header-id (file-path header-line)
  "Return the id of given header at HEADER-LINE in FILE-PATH."
  (interactive)
  (save-excursion
    (with-current-buffer (let ((enable-local-variables nil)) (find-file-noselect file-path))
      (save-restriction
        (save-excursion
          (widen)
          (goto-char 0)
          (forward-line header-line)
          (org-id-get nil 'create)
          (save-buffer)
          (org-id-get nil 'create))))))

(defun im-org-all-headers ()
  "Return all headers in `org-directory'."
  (->> (concat "cd " org-directory "; "
               "rg"
               " --no-heading" " --with-filename"
               " --line-number" " -t org"
               " " "\"^\\*+ \" ")
       (shell-command-to-string)
       (s-split "\n")
       (--filter (not (s-blank? it)))
       (--map
        (-let* (((fname line . content) (split-string it ":"))
                (header (im-org-header-line-to-title (string-join content ":"))))
          `(,(format
              "%s:%s %s %s"
              (propertize fname 'face '(:foreground "plum"  :slant italic))
              (propertize line 'face '(:slant italic :weight thin))
              (propertize "»" 'face '(:foreground "green"))
              (propertize header 'face '(:foreground "sky blue" :weight bold)))
            .
            (:fname ,(concat org-directory "/" fname) :line ,(1- (string-to-number line)) :header ,header))))))

(defun im-org-link-header ()
  "Interactively select a header and insert it as a link into the buffer.
Headers are gathered from all the org files found in `org-directory'."
  (interactive)
  (save-some-buffers)
  (let* ((selected (im-alist-completing-read
                    "Select header: "
                    (im-org-all-headers)
                    (when (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))))
         (header-id (im-org-file-get-header-id (plist-get selected :fname) (plist-get selected :line))))
    (org-insert-link
     nil
     (concat "id:" header-id)
     (unless (use-region-p)
       (read-string "Enter link text: " (plist-get selected :header))))))

(defun im-org-jump-to-header ()
  "Jump to selected header."
  (interactive)
  (let* ((selected (im-alist-completing-read "Select header:" (im-org-all-headers))))
    (with-current-buffer (find-file (plist-get selected :fname))
      (widen)
      (goto-char 0)
      (forward-line (plist-get selected :line))
      (reveal-post-command))))

;;;;; Insert image with width

;; This function is especially useful when used in combination with =embark-act-all=. The workflow is as follows:
;; - =M-x= =im-org-insert-image-file-with-width=
;; - Filter the files that you want to insert to your buffer.
;; - Hit enter if there is only one item.
;; - If there are multiple items that you want to insert, do =M-a= (embark-act) and then =S= (=embark-collect-snapshot=).
;; - Then you can do =embark-act-all= or just hit enter on the items that you want to insert to your buffer.

(defun im-org-insert-image-file-with-width ()
  "Insert interactively selected image file with fixed width information."
  (interactive)
  (let ((fname (file-relative-name (read-file-name "Select file: "))))
    (insert (format "#+ATTR_ORG: :width 400\n[[file:%s]]\n\n" fname))))

;;;;; Renaming files under cursor

(defun im-org-rename-file-at-point ()
  "Interactively rename the file under cursor and update the link."
  (interactive)
  (let* ((link (org-element-context))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (begin (org-element-property :begin link))
         (end (org-element-property :end link))
         (cbegin (org-element-property :contents-begin link))
         (cend (org-element-property :contents-end link))
         content)
    (unless (equal type "file")
      (user-error "Link is not a file"))
    (when (and cbegin cend)
      (setq content (format "[%s]" (buffer-substring-no-properties cbegin cend))))
    (let ((use-relative? (not (file-name-absolute-p path)))
          (fname (read-file-name "New name: "
                                 (expand-file-name
                                  (file-name-directory path))
                                 nil nil
                                 (im-org-suggest-filename path))))
      (when use-relative?
        (setq fname (concat "./" (file-relative-name fname))))
      (save-excursion
        (rename-file path fname)
        (delete-region begin end)
        (insert (format "[[file:%s]%s]"
                        fname
                        (or content "")))))))

;;;;; Project management

;; I'm doing all of my project management in org-mode. Here you can
;; find some supplementary functionality that makes project management
;; within org-mode easy.

;;;;;; Do a regexp search in a project inside a org dynamic block

;; Here I create a dynamic block for org-mode, named
;; ~project-grep~. You can create a block like the following:

;; #+begin: project-grep :root "~/Workspace/projects/dotfiles" :regexp "TODO"
;; #+end

;; When you invoke =C-c C-c= on that block, it will automatically run
;; given REGEXP in given ROOT and create a nicely formatted table
;; containing all the results. Results are formatted into org-links
;; you can easily jump into.

(defun org-dblock-write:project-grep (params)
  "Do a regular expression search in given project.
PARAMS may contain `:root' or `:regexp'.

`:root' - Where to run the search.  If it's skipped, it's
`default-directory'.

`:regexp' - Regexp to grep in given folder.  If it's skipped it
searches for TODO/FIXME items in given folder."
  (let* ((root (expand-file-name (or (plist-get params :root) default-directory)))
         (regexp (or (plist-get params :regexp) "(//|#|--|;)+ ?(TODO|FIXME)"))
         (default-directory root))
    (--map (insert (format "%s | " it)) '("" "ID" "File" "Content"))
    (insert "\n")
    (insert "|-|\n")
    (--each-indexed
        (s-split
         "\n"
         (shell-command-to-string (format "rg --line-number '%s'" regexp))
         'omit-nulls)
      (let* ((data (s-split-up-to ":" it 2))
             (file (s-join ":" (-take 2 data)))
             (file-link (concat "[[file:" (f-join default-directory (s-replace ":" "::" file)) "][" file "]]"))
             (content (s-replace "|" " \\vert " (-last-item data))))
        (insert "| ")
        (insert (format "%s" it-index))
        (insert " | ")
        (insert file-link)
        (insert " | ")
        (insert content)
        (insert " |\n"))))
  (delete-char 1)
  (org-table-align))

;;;;; Archiving URLS

(use-package im-archive
  :straight nil)

(defvar im-org-archive-url-path "~/Documents/notes/data/archvive/")

(defun im-org-archive-url ()
  "Archive URL and generate an new org entry for it."
  (interactive)
  (require 'im-archive)
  (let (url update?)
    ;; If we are on a heading and calling this function, we probably
    ;; just want to update/initialize the archive for current
    ;; heading. Otherwise we are creating a new archive.
    (if-let* ((_ (org-at-heading-p))
              (old-url (or (org-entry-get nil "URL")
                           ;; There might not be an URL property but
                           ;; the header itself might be a link, try to
                           ;; extract it
                           (or (plist-get (cadr (org-element-context)) :raw-link)))))
        (progn
          (setq url old-url)
          (setq update? t))
      (progn
        (setq url (read-string "URL: "))
        (org-insert-heading)))

    ;; Precautionary call
    (org-id-get-create)

    (let* ((url-title (if update?
                          (im-org-header-line-to-title (org-entry-get nil "ITEM"))
                        (read-string "Title: " (im-url-get-title url))))
           (archive-path (format
                          "%s/%s_%s_%s%s"
                          im-org-archive-url-path
                          (org-id-get-create)
                          (format-time-string "%Y%m%dT%H%M%S")
                          (im-string-url-case url-title)
                          (plist-get (cdar (im-assoc-regexp url im-archive-handlers)) :extension))))
      (org-set-property
       "ARCHIVED_AT"
       (format "%s[[file:./%s][%s]]"
               (if-let* ((older-archives (org-entry-get nil "ARCHIVED_AT")))
                   (format "%s, " older-archives)
                 "")
               (f-relative archive-path)
               (format-time-string "%Y-%m-%dT%H:%M")))
      (unless update?
        (insert (org-make-link-string url url-title)))

      (org-set-property "URL" url)
      (unless (org-entry-get nil "CREATED")
        (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))

      ;; Create the archive
      (f-mkdir im-org-archive-url-path)
      (im-archive-url url archive-path))))

(with-eval-after-load 'org-capture
  (add-to-list
   'org-capture-templates
   `("b" "Bookmark" entry
     (file+headline ,bookmarks-org "Unsorted")
     "** (call-interactively #'im-org-archive-url)\n- Don't forget to TAG!")))

;;;;; Convert items to TODO entries

(defun im-org-convert-item-to-todo-and-refile (&optional yank-only)
  "Convert org item to todo header and refile.
I take notes in the following format during the day via
`org-capture' template:

    - [2023-04-09 Sun 23:57] Some note

At the end of the day, I convert these notes into tasks if
applicable.  This means rewriting them into a TODO header with
CREATED_AT property and refiling into the appropriate
header.  This function automatizes that.

If YANK-ONLY is non-nil (with prefix arg while calling
interactively), only yank the result, do not refile.

>> (with-temp-buffer
    (insert \"- [2023-06-28 Wed 10:13] Test\\n\")
    (beginning-of-buffer)
    (im-org-convert-item-to-todo-and-refile t))
=> \"** TODO [#B] Test
:PROPERTIES:
:CREATED_AT: [2023-06-28 Wed 10:13]
:END:
\"

>> (with-temp-buffer
    (insert \"- [2023-06-28 Wed 10:13] Test :: The body.\\n\")
    (beginning-of-buffer)
    (im-org-convert-item-to-todo-and-refile t))
=> \"** TODO [#B] Test
:PROPERTIES:
:CREATED_AT: [2023-06-28 Wed 10:13]
:END:
The body.
\"

>> (with-temp-buffer
    (insert \"- [2023-06-28 Wed 10:13] Test and a link [[here]] :: The body.\\n\")
    (beginning-of-buffer)
    (im-org-convert-item-to-todo-and-refile t))
=> \"** TODO [#B] Test and a link [[here]]
:PROPERTIES:
:CREATED_AT: [2023-06-28 Wed 10:13]
:END:
The body.
\"

Version: 2023-07-31
- Fixed not being able to parse headers with links.
Version: 2023-06-28
- Initial version."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let ((line (substring-no-properties (thing-at-point 'line)))
          (timestamp nil)
          (text nil))
      (when-let* ((match (s-match "^- \\(\\[[ X-]] \\)?\\(\\[.*?]\\) \\(.*\\)" line)))
        (setq timestamp (nth 2 match))
        (setq text (nth 3 match))
        (let* ((buffer (current-buffer))
               (data (s-split " :: " text))
               (header (nth 0 data))
               (body (nth 1 data))
               result)
          (with-temp-buffer
            (insert
             (concat "** TODO [#B] " header "\n"
                     ":PROPERTIES:" "\n"
                     ":CREATED_AT: " timestamp "\n"
                     ":END:" "\n"
                     (if body (concat body "\n") "")))
            (setq result (buffer-string))
            ;; To be able to refile
            (org-mode)
            (if yank-only
                (im-kill result)
              (let ((org-refile-targets nil))
                (org-refile nil buffer))
              ;; Remove the line after everything to prevent loss of data
              (with-current-buffer buffer
                (delete-region (line-beginning-position) (1+ (line-end-position))))
              result)))))))

;;;;; Sort entries intelligently

(defun im-org-sort-entries-best ()
  "Best sorting mechanism for your backlog.
Entries are sorted by priority, with most effort entries being
on top in their priority range.  Also entries are sorted by their
TODO state."
  (interactive)
  (progn
    ;; Sort by creation time
    (org-sort-entries nil ?R nil nil "CREATED_AT")
    ;; Sort by Effort, the one with least effort is at top
    (org-sort-entries nil ?R nil nil "Effort")
    ;; Sort by priority
    (org-sort-entries nil ?p)
    ;; Sort by TODO state
    (org-sort-entries nil ?o)
    ;; Move non-TODO headers to top
    (org-sort-entries nil ?F (lambda () (if (or (org-entry-is-done-p) (org-entry-is-todo-p)) -1 (- 1000 (length (org-entry-get nil "ITEM"))))))))

;;;;; Remembering to clock in

;; I forget to clock in (and out) most of the time, so I want Emacs to
;; remind clocking to me. This function regularly checks if I'm
;; clocked in or not and tries to remind me to take the appropriate
;; action.

(defun im-bullet-clock-in-a-task ()
  "Display today's tasks and require user to select one to clock in."
  (interactive)
  (with-current-buffer (find-file-noselect bullet-org)
    (save-restriction
      (im-bullet-focus-today)
      (let ((task (cdr
                   (im-completing-read
                    "Select task: "
                    (->>
                     (org-map-entries
                      (lambda ()
                        (let ((props (org-entry-properties)))
                          (when-let* ((todo (alist-get "TODO" props nil nil #'equal))
                                      (_ (not (equal "DONE" todo))))
                            (let ((bounds (bounds-of-thing-at-point 'line)))
                              (cons (buffer-substring (car bounds) (cdr bounds)) (point)))))))
                     (-filter #'identity))
                    :formatter #'car))))
        (when task
          (widen)
          (goto-char task)
          (let ((elapsed-minutes (read-number "How may minutes you've already spent?" 1)))
            (org-clock-in nil (time-subtract (current-time) (seconds-to-time (* elapsed-minutes 60))))))))))

(im-leader "ocb" #'im-bullet-clock-in-a-task)

(defun im-org-check-clock ()
  "Check if are we currently clocked in or not.
If not, prompt user to clock in."
  (cond
   ;; If we are clocked in and we have been clocking longer than the
   ;; effort or allocated time, ask if we want to clock out.
   ((and (featurep 'org-clock) (org-clocking-p))
    (require 'im-svgcal)
    (with-current-buffer (find-file-noselect "~/Documents/notes/bullet.org")
      (save-window-excursion
        (save-restriction
          (save-excursion
            (org-clock-goto)
            (org-narrow-to-subtree)
            (let* ((effort (when-let* ((e (org-entry-get nil "EFFORT")))
                             (im-svgcal--time-diff "0:00" e)))
                   (allocated-time
                    (-sum (-map (-lambda ((range start end)) (im-svgcal--time-diff start end)) (im-svgcal--org-entry-timestamps)))))
              (when (ignore-errors
                      (> (org-clock-get-clocked-time)
                         (if (> allocated-time 0)
                             allocated-time
                           effort)))
                (message ">> You are still clocked in to '%s'! Want to clock out now?" (org-entry-get nil "ITEM")))))))))
   (t
    (unless (> (* 60 5) (time-to-seconds (current-idle-time)))
      (message ">> You are not clocked in!")))))

(run-with-timer 60 300 #'im-org-check-clock)

;;;;; im-org-unfill-paragraph

(defun im-org-unfill-paragraph (&optional justify region)
  "Unfill the paragraph at point.
This function unfills the paragraph at point, removing line
breaks and joining the lines together.  This function relies on
  `org-fill-paragraph' to perform the actual unfilling."
  (interactive (progn
       (barf-if-buffer-read-only)
       (list (when current-prefix-arg 'full) t)))
  (let ((fill-column 9999))
    (org-fill-paragraph justify region)))

;;;;; im-org-new-todo

;; NOTE: With C-o in minibuffer, I can insert links to minibuffer.

(defun im-org-new-todo (&optional priority)
  "Create new org TODO item with the fields I want them to be there."
  (interactive)
  (let ((content (concat
                  (format "TODO%s %s "
                          (if-let* ((priority
                                     (im-non-blank-or-nil
                                      (or priority
                                          (completing-read
                                           "Select priority:"
                                           (mapcar #'char-to-string (number-sequence
                                                                     org-priority-highest
                                                                     (1+ org-priority-lowest))))))))
                              (format " [#%s]" priority)
                            "")
                          (s-trim (read-string "Header: ")))
                  (when-let* ((date (im-bullet-current-date)))
                    (format "\nSCHEDULED: <%s>" date))
                  (format
                   "\n:PROPERTIES:\n:CREATED_AT: [%s]\n:END:"
                   (format-time-string "%Y-%m-%d %a %H:%M")))))
    (org-insert-heading '(4))
    (insert content)
    (call-interactively #'org-set-tags-command)
    (org-set-effort)))

(defun im-org-new-heading ()
  "Create a new heading with some automatically set properties."
  (interactive)
  (let ((content (concat
                  (format
                   "%s%s"
                   (s-trim (read-string "Header: "))
                   (format
                    "\n:PROPERTIES:\n:CREATED_AT: [%s]\n:END:"
                    (format-time-string "%Y-%m-%d %a %H:%M"))))))
    (org-insert-heading '(4))
    (insert content)
    (call-interactively #'org-set-tags-command)))

(im-leader :keymaps 'org-mode-map
  "ok" #'im-org-new-todo
  "oK" #'im-org-new-heading)

;;;;; org-transclusion

;; My main use case for
;; [[https://github.com/nobiot/org-transclusion][org-transclusion]] is
;; that I sometimes create curated lists of other headings and instead
;; of simply inserting standard links, I convert them into
;; org-transclusion directives. This way I can directly view linked
;; content on the spot. Especially useful for meeting notes.


(use-package org-transclusion
  :general
  (im-leader :keymaps 'org-mode-map
    "of" #'org-transclusion-add
    "oF" #'org-transclusion-remove))

;;;;; Capture changed headings on save

;; Sometimes, I want to capture which headings are changed on save and
;; do some actions on those headings. Following code accomplishes
;; that, but only for top and second level headings. To do so add a
;; function to ~im-org-header-changed-hook~.

(defvar im-org-header-changed-hook-blacklist-files '("bullet.org")
  "Do not run change hook on these files.
If file is too big (> 1 MB?, or too much headers), this hook
makes saving quite slow.  I could simply put a file limit on the
hooks but that makes thing even more complicated.")

(defvar im-org-header-changed-hook '()
  "Functions run after an entry is changed.

Functions are called with one argument which is the following plist:
    (:begin ... :end ... :level ... :properties ... :header... :body ...)

Functions are called separately for each changed entry/header.")

(defvar-local im--org-heading-prev-state nil
  "Holds the previous state of the top-level headers.")

(defun im-org-store-heading-state ()
  "Store the state of the top-level headers for later comparison."
  (when (derived-mode-p 'org-mode)
    (let ((file (buffer-file-name)))
      (when (and (file-exists-p file)
                 (not (-contains? im-org-header-changed-hook-blacklist-files (f-filename file))))
        (setq im--org-heading-prev-state
              (with-temp-buffer
                (insert-file-contents file)
                (delay-mode-hooks (org-mode))
                ;; Save only :body & :header because one line
                ;; added/removed will invalidate all headers by
                ;; changing :begin and :end
                (--map (cons (plist-get it :header) (plist-get it :body)) (im-org-get-some-headers))))))))

(defun im-org-compare-heading-state ()
  "Compare current top-level headers with the stored state to detect changes."
  (when (and (derived-mode-p 'org-mode)
             (not (-contains? im-org-header-changed-hook-blacklist-files (f-filename (buffer-file-name)))))
    (-each (--remove
            (member (cons (plist-get it :header) (plist-get it :body)) im--org-heading-prev-state)
            (im-org-get-some-headers))
      (lambda (header-plist)
        (--each im-org-header-changed-hook
          (funcall it header-plist))))))

(defun im-org-get-some-headers ()
  "Extract top-level headers and their content from the current Org buffer."
  (let ((headers '()))
    (save-restriction
      (widen)
      (org-map-entries
       (lambda ()
         (let* ((header (org-get-heading t t t t))
                (elem (org-element-at-point)))
           (push
            (list
             :begin (org-element-property :begin elem)
             :end (org-element-property :end elem)
             :level (org-element-property :level elem)
             :properties (org-entry-properties)
             :header header
             :body
             (when-let* ((start (org-element-property :contents-begin elem)))
               (buffer-substring-no-properties
                start
                (org-element-property :contents-end elem))))
            headers)))
       "LEVEL<3"))
    headers))

(add-hook 'before-save-hook 'im-org-store-heading-state)
(add-hook 'after-save-hook 'im-org-compare-heading-state)

;;;;; Get effort of linked header

(defun im-org-get-link-effort-at-point ()
  "Return the effort of linked header.
Only works with id links.  Useful while planning.  When called
interactively, insert the effort at the end of the line.

I generally bind this to a key while using by
`im-embark-bind-leader-command'."
  (interactive)
  (let (effort)
    (-let (((file . pos) (org-id-find (org-element-property :path (org-element-lineage (org-element-context) '(link) t)))))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char pos)
            (setq effort (or (org-entry-get nil "Effort")
                             (progn
                               (org-set-effort)
                               (org-entry-get nil "Effort"))))))))
    (when (called-interactively-p 'interactive)
      (end-of-line)
      (insert (format " → %s" effort)))))

;;;;; corg.el --- Org Mode block header completion

;; Completion at point for dynamic block headers, source block headers
;; etc.

(use-package corg
  :straight (:host github :repo "isamert/corg.el")
  :hook (org-mode . corg-setup))

;;;;; im-svgcal & org-timeblock

;; Experimental SVG visual calendar for the current day.

;; At the time of writing this, I wasn't aware of (or maybe it didn't
;; even exist?) org-timeblock[1].  im-svgcal is quite similar to that
;; but only works for given day and it's independent from the agenda,
;; it simply draws a calendar for current org file (or current
;; restricted section of the org file).

;; [1]: https://github.com/ichernyshovvv/org-timeblock

(use-package im-svgcal
  :commands (im-svgcal-enable im-svgcal-run)
  :straight nil)

(use-package org-timeblock
  :straight (:host github :repo "ichernyshovvv/org-timeblock")
  :general
  (:keymaps 'org-timeblock-mode-map :states 'normal
   "h" #'org-timeblock-backward-column
   "j" #'org-timeblock-forward-block
   "k" #'org-timeblock-backward-block
   "l" #'org-timeblock-forward-column
   "t" #'org-timeblock-todo
   "c" #'org-timeblock-clock-in
   "n" #'org-timeblock-new-task
   "r" #'org-timeblock-redraw-buffers
   "T" #'org-timeblock-list
   "q" #'org-timeblock-quit
   "RET" #'org-timeblock-goto)
  :custom
  (org-timeblock-inbox-file inbox-org)
  (org-timeblock-span 5)
  (org-timeblock-scale-options nil))

;;;; Extra functionality

;;;;; im-open-thing-at-point

(defvar im-open-thing-at-point-alist '()
  "List of (THING-DETECTOR . THING-OPENER) pairs.
THING-DETECTOR should be a no-arg function that returns the
thing, if found.  Otherwise it should return nil.

THING-OPENER should be a function with 1 arg, which should take
the thing (which is the thing return by the THING-DETECTOR) and
open.")

(im-leader-v
  "eo" #'im-open-thing-at-point
  "eO" #'im-open-thing)

(defun im-open-thing-at-point ()
  "Detect and open the thing at point."
  (interactive)
  (--any
   (when-let* ((result (funcall (car it))))
     (funcall (cdr it) result))
   im-open-thing-at-point-alist))

(defun im-open-thing (thing)
  "Detect and open the thing at point."
  (interactive "sThing: ")
  (with-temp-buffer
    (insert thing)
    (--any
     (when-let* ((result (funcall (car it))))
       (funcall (cdr it) result))
     im-open-thing-at-point-alist)))

(add-to-list 'im-open-thing-at-point-alist `(,(apply-partially #'thing-at-point 'url) . browse-url))

;;;; Other packages

;;;;; which-key

(use-package which-key
  :config
  (setq which-key-max-display-columns 4)
  (which-key-setup-minibuffer)
  (which-key-mode))

;;;;; doc-view-mode

(use-package doc-view
  :defer t
  :straight (:type built-in)
  :config
  (evil-collection-doc-view-setup))

;;;;; world-clock

;; - Simply do ~M-x world-clock~.

(setq world-clock-list
      '(("Europe/Amsterdam" "Amsterdam")
        ("Europe/Istanbul" "Istanbul")
        ("Asia/Tokyo" "Tokyo")
        ("America/Los_Angeles" "Seattle")))

(setq world-clock-time-format "%a %d %b %R %Z")

;;;;; eldoc

(use-package eldoc
  :straight (:type built-in)
  :custom
  ;; I have a `im-peek-doc' function that displays the documentation
  ;; inline, which eleminates the need for multiline doc in minibuffer
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

;;;;; display-time-mode

;; I started using Emacs in full screen (Emacs maximalism goes on), so
;; this is helpful.

(use-package display-time
  :straight (:type built-in)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-format "%b %d, %H:%M %a")
  (setq display-time-default-load-average nil))

;;;;; outline-mode

;; This is a built-in mode for providing basic outlining
;; features. Here are a few configurations to make the navigation a
;; bit more like what I use in org-mode:

(use-package outline-mode
  :straight (:type built-in)
  :general
  (:states 'normal :keymaps 'outline-mode-map
   "[[" #'outline-previous-heading
   "]]" #'outline-next-heading)
  :bind
  (:repeat-map outline-mode-repeat-map
   ("[" . outline-previous-heading)
   ("]" . outline-next-heading)))

;;;;; debugger-mode

(evil-set-initial-state 'debugger-mode 'normal)

;;;;; hideshow -- hs-minor mode for code folding

(use-package hideshow
  :straight (:type built-in)
  :general
  (:states 'normal :keymaps 'hs-minor-mode-map
   ;; Hide all stuff under current level.
   ;; Useful in Java classes etc.
   "zf" #'hs-hide-level)
  :custom
  ;; Comment folding results in weird behavior combined with
  ;; outli-mode.  And comments are not that long generally, so this is
  ;; good.
  (hs-hide-comments-when-hiding-all nil))

;;;;; eat & eshell

(use-package eshell
  :straight (:type built-in)
  :defer t
  :hook
  (eshell-mode . im-disable-hl-line-mode-for-buffer)
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode)))
  :general
  (:states 'insert :keymaps '(eshell-prompt-mode-map override)
   "C-l" (λ-interactive (eshell/clear t)))
  :config
  (evil-collection-eshell-setup)

  ;; This is nil on purpose. Because `im-eshell-append-history' does
  ;; duplication check before writing it to history and if this is
  ;; non-nil then it fails to do the duplication check because
  ;; duplicated items will not be inserted to history-ring at all.
  (setq eshell-hist-ignoredups nil)
  (setq eshell-history-size 10000)
  ;; I manage history with `im-eshell-append-history''
  (setq eshell-save-history-on-exit nil)
  ;; You can jump to a directory by doing a regex match with:
  ;;
  ;;    cd =some-regexp
  ;;
  ;; and you'll jump to the latest matching directory in in the last
  ;; dir ring.
  ;;
  ;; Also use
  ;;
  ;;     cd -
  ;;
  ;; to jump to the latest directory before the current one.
  (setq eshell-last-dir-ring-size 1000)

  (defun im-eshell-prompt ()
    (concat (abbreviate-file-name (eshell/pwd))
            (if (= (user-uid) 0) " # " " \n$ ")))

  (setq eshell-prompt-regexp "^$ ")
  (setq eshell-prompt-function #'im-eshell-prompt))

(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

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
  :autoload (eat-make)
  :config
  (setq eat-enable-shell-prompt-annotation nil)
  (setq eat-shell (executable-find "fish"))
  (setq eat-tramp-shells '(("docker" . "/bin/sh")
                           ("ssh" . "/usr/bin/zsh")))
  (add-hook 'eat-mode-hook #'im-disable-hl-line-mode-for-buffer)
  ;; While writing, eat buffers sometimes jump around for some unknown
  ;; reason. I realized that truncating lines helps with that.
  (add-hook 'eat-mode-hook #'toggle-truncate-lines)
  (eat-eshell-mode))

;;;;;; Send a notification for long running commands

;; See `im-eshell-notify-duration' to control what a long running
;; command means.  This only sends if user is focused to another
;; buffer or Emacs is out of focus.

;; I already have this feature in zsh, provided by starship
;; prompt. This is just an enhanced eshell implementation.

(with-eval-after-load 'eshell
  (defvar-local im-eshell-notify-enabled t)
  (defvar im-eshell-notify-duration 1
    "Minimum time in seconds that a command takes for a notification to fire.")

  (defvar-local im-eshell-notify--last-execution-time nil)

  (defun im-eshell-notify-toggle-for-buffer ()
    (interactive nil eshell-mode)
    (setq im-eshell-notify-enabled (not im-eshell-notify-enabled))
    (message ">> eshell-notify :: %s" im-eshell-notify-enabled))

  (defun im-eshell-notify--pre-command-hook ()
    (setq im-eshell-notify--last-execution-time (float-time)))

  (defun im-eshell-notify--post-command-hook ()
    (when im-eshell-notify-enabled
      (when im-eshell-notify--last-execution-time
        (let ((execution-time (- (float-time) im-eshell-notify--last-execution-time)))
          (when (> execution-time im-eshell-notify-duration)
            ;; Only send a notification if user focused to another buffer or
            ;; Emacs is out of focus.
            (when (or
                   (not (frame-focus-state))
                   (not (eq (current-buffer) (window-buffer (selected-window)))))
              (alert
               (format "Execution took %s seconds." execution-time)
               :title (format "Command finished: %s!" eshell-last-command-name))))))))

  (add-to-list 'eshell-pre-command-hook 'im-eshell-notify--pre-command-hook)
  (add-to-list 'eshell-post-command-hook 'im-eshell-notify--post-command-hook))

;;;;;; Proper command completion

;; This package provides command, subcommand and argument completion
;; using fish shell. You need to have fish shell installed. There is
;; also helm version that provides argument documentation but I
;; haven't tried it yet and I need to make it work with corfu instead
;; of helm.
;;
;; - In `eshell', fish completion is only used when `pcomplete' fails.
;; - Don't forget that pcomplete works with TAB, not M-TAB.
;;
;; Also do `fish_update_completions' time to time in a fish shell to
;; parse and generate completions for fish shell.

(use-package fish-completion
  :after eshell
  :hook (eshell-mode . fish-completion-mode))

;;;;;; Reading bash/zsh aliases into Eshell

(defun im-eshell-load-my-aliases ()
  "Load fish/zsh/bash aliases/abbreviations into eshell.
'$*' is appended after each alias so that they can take
positional parameters in eshell.

Aliases needs to be defined in a particular way:

  alias alias-name='alias value'

- There should be no spaces around the equals sign.
- Single quote should be used instead of double quoutes.

For fish shel abbreviations, only the following form is accepted:

  abbr -a -- alias-name 'alias value'

There is also a special syntax for defining eshell-specific aliases
that is read verbatim (meaning that no '$*' is appended):

  #eshell test='ls'"
  (interactive)
  (setq
   eshell-command-aliases-list
   (-filter
    #'identity
    (--map
     (-when-let ((_ eshell? name _2 imp)
                 (s-match "^\\(alias\\|#eshell\\|abbr -a --\\) \\([a-zA-Z0-9_-]+\\)\\(=\\| \\)'\\(.*\\)'\\( *#.*\\)*$" it))
       (list name (if (s-prefix? "#eshell" eshell?)
                      imp
                    (concat imp " $*"))))
     (--mapcat
      (s-split "\n" (with-temp-buffer (insert-file-contents it) (buffer-string)))
      (append
       (when-let* ((path "~/.config/fish/conf.d/aliases.fish")
                   (_ (file-exists-p path)))
         (list path))
       (when (file-exists-p "~/.config/aliases/")
         (directory-files "~/.config/aliases/" t directory-files-no-dot-files-regexp))))))))

(add-hook 'eshell-alias-load-hook #'im-eshell-load-my-aliases)

;;;;;; Eshell-specific aliases

;; Look at [[Aliases]] section in my config to see all bash/zsh
;; aliases. These are eshell specific:

(im-tangle-file
 :path "~/.config/aliases/eshell"
 :contents "#eshell clear='clear 1'")

;; Also here are some aliases/functions for eshell defined in elisp:

(defun eshell/mkcd (&rest args)
  (eshell/mkdir args)
  (eshell/cd args))

(defun eshell/project-root (&rest args)
  (eshell/cd (im-current-project-root)))

;;;;;; Automatically print notes for given directory

;; I have the file ~im-autodir-file~ which contains notes for
;; directories, like in the following format:

;; * ~/a/directory
;; Here are my notes for the directory...
;;
;; * ~/another/directory: This one contains some source code
;; Use the following to compile this:
;; ...

;; When I enter any of the listed directories, eshell automatically
;; prints out my notes.

(defvar im-autodir-file directory-notes-org)
(defvar im-autodir-last-doc nil)

(defun im-eshell-handle-dir-change ()
  (let ((dir (file-name-as-directory (expand-file-name default-directory))))
    (setq im-autodir-last-doc
          (with-current-buffer (find-file-noselect im-autodir-file)
            (save-restriction
              (widen)
              (catch 'found
                (org-map-entries
                 (lambda ()
                   (when (->>
                          (org-entry-get nil "ITEM")
                          (s-split ":")
                          (car)
                          (expand-file-name)
                          (file-name-as-directory)
                          (equal dir))
                     (throw 'found (buffer-substring-no-properties
                                    (org-element-property :contents-begin (org-element-at-point))
                                    (org-element-property :contents-end (org-element-at-point))))))
                 "LEVEL=1")
                nil))))
    (when im-autodir-last-doc
      (insert "im-autodir-show-doc")
      (eshell-send-input))))

(defun im-autodir-show-doc ()
  (->>
   im-autodir-last-doc
   s-lines
   (-take 20)
   (s-join "\n")))

(defun im-autodir-show-doc-force ()
  (interactive nil eshell-mode)
  (im-eshell-handle-dir-change))

(add-hook 'eshell-directory-change-hook #'im-eshell-handle-dir-change)
;; eshell-directory-change-hook is not get triggered when eshell is opened, hence:
(add-hook 'eshell-mode-hook #'im-eshell-handle-dir-change)

(defun im-org-jump-to-project-documentation ()
  "Jump to *my* documentation for current project."
  (interactive)
  (let* ((projdir (abbreviate-file-name (or (im-current-project-root) default-directory)))
         (buf (find-file-noselect directory-notes-org))
         (pos (with-current-buffer buf
                (widen)
                (org-find-exact-headline-in-buffer projdir))))
    (unless pos
      (if (y-or-n-p "Note not found for this project.  Want to create?")
          (with-current-buffer buf
            (widen)
            (goto-char (point-max))
            (org-insert-heading nil nil t)
            (org-edit-headline projdir)
            (org-set-property "CREATED_AT" (concat "[" (im-today) "]"))
            (setq pos (point)))
        (user-error "Doc not found directory: %s" projdir)))
    (switch-to-buffer buf)
    (goto-char pos)
    (org-fold-show-entry)
    (org-narrow-to-subtree)))

;;;;;; Consistent history

;; Normally, if you have more than one eshell instance open and quit
;; them consecutively, the last closed one will overwrite the
;; ~eshell-history-file~. The following /appends/ issued command to
;; history after each command.

(defvar im-eshell-history-blacklist "^\\(ls\\|pwd\\|cd\\|clear\\|exit\\|rm\\|mkdir\\|mkcd\\|im-autodir\\|lls\\|ll\\)")

;; Adapted from: https://emacs.stackexchange.com/questions/18564/merge-history-from-multiple-eshells
;; Changes:
;; - Duplication check
;; - Blacklisted commands
(defvar-local im-eshell-append-history nil)
(defun im-eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to t."
  (if (not im-eshell-append-history)
      ;; Remove the eshell save hook on first run.
      ;; Also don't append on first run to prevent duplication.
      (progn
        (remove-hook 'eshell-exit-hook #'eshell--save-history t)
        (setq im-eshell-append-history t))
    (when (and eshell-history-ring
               (ring-elements eshell-history-ring))
      (let ((last-elem (s-trim (car (ring-elements eshell-history-ring))))
            (prev-elem (or (ignore-errors (s-trim (nth 1 (ring-elements eshell-history-ring)))) "")))
        (when (and (not (string= last-elem prev-elem))
                   (not (s-matches? im-eshell-history-blacklist last-elem)))
          (with-temp-buffer
            (insert last-elem "\n")
            (eshell-with-private-file-modes
             (write-region (point-min) (point-max)
                           (file-truename eshell-history-file-name)
                           'append 'no-message))))))))

(add-hook 'eshell-post-command-hook #'im-eshell-append-history)

;;;;;; Reset Eshell

(defun im-eshell-reset ()
  "Reset current eshell."
  (interactive nil eshell-mode)
  (let ((name (buffer-name))
        (dir default-directory))
    (let ((kill-buffer-query-functions '()))
      (kill-buffer))
    (let ((default-directory dir))
      (im-eshell name))))

;;;;; bookmark.el

;; You can use ~list-bookmarks~ command to view/edit/delete them but
;; using ~consult-bookmark~ and calling ~embark-act~ on them to
;; view/delete/edit given bookmark might be easier.

(use-package bookmark
  :straight (:type built-in)
  :general
  (im-leader
    "bs" #'bookmark-set
    "bm" #'consult-bookmark)
  :custom
  ;; Save bookmarks automatically
  (bookmark-save-flag 1)
  ;; If it's a symlink, point to the real one so that bookmark.el can
  ;; detect changes
  :config
  (evil-collection-bookmark-setup)
  (setq bookmark-default-file (file-truename bookmark-default-file)))

;;;;; process-menu-mode

(general-def :keymaps 'process-menu-mode-map :states 'normal
  "r" #'revert-buffer
  "d" #'process-menu-delete-process
  "x" #'process-menu-delete-process)

;;;;; timer-list-mode

(general-def :keymaps 'timer-list-mode-map :states 'normal
  "r" #'revert-buffer
  "d" #'timer-list-cancel
  "x" #'timer-list-cancel)

;;;;; tabulated-list-mode

;; - It's a built-in mode that shows some kind of tabulated data.
;; - It is used by many major modes, like [[docker]], [[prodigy]], etc. I just add these common keybindings to have a consistent way of navigating in them.
;; - I also try to bind following keys in their respective mode maps:
;; - =a= key to a function that lists all the actions that can be taken on current column.
;; - =Enter= to the default action (generally opening something etc.)


(use-package tabulated-list
  :straight (:type built-in)
  :defer t
  :config
  (evil-define-key 'normal tabulated-list-mode-map
    (kbd "{") #'tabulated-list-narrow-current-column
    (kbd "}") #'tabulated-list-widen-current-column
    (kbd "H") #'tabulated-list-previous-column
    (kbd "L") #'tabulated-list-next-column
    (kbd "s") #'tabulated-list-sort
    (kbd "r") #'tabulated-list-revert))

;;;;; vtable-mode

;; Here are mostly used keys in vtable-map:
;; - S :: Sort by column.
;; - r :: Refresh the table
;; - {,} :: narrow/widen column

;; Unbind g first
(general-def :keymaps 'vtable-map "g" nil)

;; Add evil bindings for vtable mode
(general-def
  :keymaps 'vtable-map
  :states 'normal
  "r" #'vtable-revert-command
  "H" #'vtable-previous-column
  "L" #'vtable-next-column)

;;;;;; Helper functions

(defun im-vtable--pretty-colors ()
  "Return alternating colors for vtable rows.
Return a (color color) list that can be used with :column-colors and
:row-colors.  Makes the tables more readable."
  ;; Mini-frame dependency is not cool bot ok.  Rest of the config is still look like hell.
  `(,(alist-get 'background-color (frame-parameters)) ,(mini-frame-get-background-color)))

;;;;; Alert & im-notif

;; Several packages are using this package to show system-level
;; notifications. Here I set some defaults/fallback values.

(use-package alert
  :config
  (setq alert-log-messages nil) ; im-notif already logs them
  (alert-define-style
   'im-notif
   :title "im-notif"
   :notifier
   (lambda (info)
     (im-notif
      :title (plist-get info :title)
      :message (plist-get info :message)
      :margin t
      :duration (unless (plist-get info :persistent) alert-fade-time)
      :severity (plist-get info :severity))))
  (setq alert-default-style 'im-notif)
  (setq alert-fade-time 15)
  (when (eq system-type 'darwin)
    ;; Re-define osx-notifier to handle strings better.
    (defun alert-osx-notifier-notify (info)
      (do-applescript (format "display notification %S with title %S"
                              (substring-no-properties (plist-get info :message))
                              (substring-no-properties (plist-get info :title))))
      (alert-message-notify info)))
  ;; org-clock sends notification if the current tasks estimated effort is met.
  (setq
   org-show-notification-handler
   (lambda (notification)
     (alert notification :title "*org-mode*"))))

(use-package im-notif
  :straight nil
  :autoload (im-notif)
  :general
  (im-leader
    "y" #'im-notif-menu)
  :custom
  (defun im-notif--send-to-ntfy ()
    "Send notifications to ntfy after some idle time."
    (when (or (and (current-idle-time)
                   (>= (time-to-seconds (current-idle-time)) 30))
              ;; If we are sharing a screen, that means the
              ;; notification will only show REDACTED. Then send it
              ;; to my phone so that I can see it privately through
              ;; my watch.
              (await (im-screen-sharing-now?)))
      (im-ntfy
       (plist-get data :message)
       :title (or (plist-get data :title) "Emacs"))))
  (add-hook 'im-notif-post-notify-hooks #'im-notif--send-to-ntfy))

;;;;; im-ntfy -- ntfy wrapper

(cl-defun im-ntfy
    (message
     &rest options
     &key
     title
     priority
     (icon (concat im-server "/assets/emacs.png"))
     (channel "emacs")
     file
     &allow-other-keys)
  "Send a notification to my phone."
  (interactive
   (list
    (im-read-string "Message: ")
    :title (im-read-string "Title: ")
    :file (when (y-or-n-p "Attach local file? ")
            (expand-file-name (read-file-name "Attachment: ")))
    :channel (completing-read "Channel: " '("phone" "emacs"))))
  (set-process-sentinel
   (apply
    #'start-process
    "ntfy" "*ntfy-out*"
    `("ntfy"
      "publish"
      ,@(map-apply
         (lambda (opt val) (format "--%s=%s" (s-chop-prefix ":" (symbol-name opt)) val))
         (map-into ;; ← To remove the duplicates
          (map-filter (lambda (opt val) (and val (not (-contains? '(:channel) opt)))) `(:icon ,icon ,@options))
          'hash-table))
      ,channel
      ,message))
   (lambda (proc text)
     (unless (eq (process-exit-status proc) 0)
       (error ">> [%s] Failed to send message through `ntfy'" (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))))

;;;;; Hydra

;; Hydra creates a menu for quickly calling/toggling functions/modes
;; in a visually easy way. My main use case for it right now is
;; grouping bunch of appearance related functions/modes that I use
;; infrequently. I believe for hydra's are not very useful for
;; commands that you use frequently, it makes things slower than a
;; plain keybinding but it's quite useful for the stuff that you
;; forget or use infrequently.

(use-package hydra)

(use-package pretty-hydra
  :after hydra
  :defer t
  :general
  (im-leader "a" #'im-appearance/body)
  :config
  (pretty-hydra-define im-appearance
    (:foreign-keys warn :title "Appearance" :quit-key "q" :color amaranth)
    ("Writeroom"
     (("W" writeroom-mode "Writeroom mode" :toggle t)
      (">" writeroom-increase-width "Width +")
      ("<" writeroom-decrease-width "Width -")
      ("m" writeroom-toggle-mode-line "Toggle modeline" :toggle t)
      ("p" spacious-padding-mode "Spacious padding" :toggle t))
     "Zoom"
     (("0" text-scale-increase "Zoom In (Buffer)")
      ("9" text-scale-decrease "Zoom Out (Buffer)")
      ("+" default-text-scale-increase "Zoom In (All)")
      ("-" default-text-scale-decrease "Zoom Out (All)")
      ("=" default-text-scale-reset "Zoom Reset (All)"))
     "Tabs"
     (("tl" tab-line-mode "Tab Line mode (buffer)" :toggle t)
      ("tL" global-tab-line-mode "Tab Line mode (global)" :toggle t)
      ("tb" tab-bar-mode "Tab Bar mode (global)" :toggle t))
     "Highlighting"
     (("hg" diff-hl "Highlight git diff" :toggle t)
      ("hd" rainbow-delimiters-mode "Rainbow parens" :toggle t)
      ("hl" global-hl-line-mode "Highlight current line" :toggle t)
      ("hb" beacon-mode "Cursor trailer (baecon)" :toggle t)
      ("hw" im-whitespace-mode-toggle "Whitespaces" :toggle t)
      ("ht" highlight-thing-mode "Highlight current symbol" :toggle t))
     "Miscellaneous"
     (("n" display-line-numbers-mode "Line numbers" :toggle t)
      ("l" visual-line-mode "Wrap lines" :toggle t)
      ("T" toggle-truncate-lines "Truncate lines")
      ("v" visual-fill-column-mode "Wrap lines at 72th col" :toggle t)
      ("i" highlight-indent-guides-mode "Indent Guides" :toggle t)
      ("f" fci-mode "Fill column" :toggle t)
      ("<SPC>" nil "Quit" :color blue)))))

;;;;; wgrep

;; With this package, you can make =grep= buffers editable and your
;; edits can be applied to the files itself. Also =embark= has a
;; feature where you can export the current completing-read results
;; into a grep buffer, the action is called =embark-export= and it
;; works on =consult-ripgrep= etc.

;; - Do ~C-c C-p~ (or =i=, enabled by evil-collection) on a =grep= buffer to make it editable.
;; - Do ~C-j~ or ~C-k~ (enabled by evil-collection, by default you need to use =n=) to peek at next/prev instance.

(use-package wgrep
  :defer t)

;;;;; dired/dirvish

;; There is also ~wdired-mode~ which you can use to do bulk rename intuitively.

(use-package dired
  :straight (:type built-in)
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-l -A -h -v --group-directories-first")
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  :config
  (evil-collection-dired-setup)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :straight (:host github :repo "hlissner/dirvish")
  :after dired
  :general
  (im-leader
    "ed" #'dirvish-dwim
    "eD" #'im-dirvish)
  ;; Add a way to open dirvish in selected directory using Embark
  (:keymaps 'embark-file-map "J" #'im-dirvish)
  (:keymaps 'dirvish-mode-map :states 'normal
   "\\"    #'im-dired-find-file-ace-window
   "h"     #'dired-up-directory
   "l"     #'dired-find-alternate-file
   "T"     #'dirvish-layout-toggle
   "e"     #'dirvish-dispatch
   "q"     #'dirvish-quit
   "<tab>" #'dirvish-toggle-subtree
   "f"     #'dirvish-file-info-menu
   "s"     #'dirvish-setup-menu
   "H"     #'dirvish-history-go-backward
   "L"     #'dirvish-history-go-forward
   "Q"     #'im-quit)
  :init
  (with-eval-after-load 'dired
    (dirvish-override-dired-mode))
  :custom
  (dirvish-subtree-always-show-state t)
  (dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  :config
  ;; Disable wrapping lines in some modes so that full-screen dirvish looks good
  (add-hook 'dirvish-directory-view-mode-hook #'im-disable-line-wrapping)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (add-hook 'dired-mode-hook #'im-disable-line-wrapping)
  (add-hook 'dired-mode-hook #'im-disable-tab-line))

(defun im-dirvish (dir)
  "Start dirvish in selected DIR."
  (interactive "DOpen Dirvish in: ")
  (dirvish dir))

(defun im-disable-line-wrapping ()
  (let ((inhibit-message t))
    (visual-line-mode -1)
    (toggle-truncate-lines +1)))

(defun im-disable-tab-line ()
  (tab-line-mode -1))

(defun im-dired-find-file-ace-window ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (dired--find-file (if (one-window-p) #'find-file-other-window #'im-find-file-ace-window) (dired-get-file-for-visit)))

;;;;;; im-dired-rsync

;; Dired copies files sync. This function uses rsync to copy stuff
;; async, especially useful while copying stuff over tramp/ssh.


(defun im-dired-rsync (dest)
  "Copy selected files with rsync to DEST.
This works over TRAMP (but only ssh and remote to local is
supported).

NOTE: Use \"rsync --version\" > 3 or something like that."
  (interactive
   (let ((target (dired-dwim-target-directory)))
     (list
      (expand-file-name
       (read-file-name
        "Copy selected files to: "
        (if (s-contains? "ssh:" target) "~/" target))))))
  (setq dest (expand-file-name dest))
  (let* ((files (dired-get-marked-files nil current-prefix-arg))
         (ssh-regexp "/ssh:\\([a-zA-Z0-9_-\\.@]+\\):")
         (resolve-dir (lambda (it) (if (f-dir? it) it (f-dirname it))))
         (fix-remote-files
          (lambda (it)
            (if-let* ((host (nth 1 (s-match ssh-regexp it))))
                (concat host ":" (string-trim-left it ssh-regexp))
              (shell-quote-argument it))))
         ;; If `default-directory' points to ssh'ed directory, that
         ;; may cause some issues. This is just to keep things simple
         ;; and error-free.
         (default-directory
           (funcall resolve-dir (if (s-matches? ssh-regexp dest)
                                    (car files)
                                  dest))))
    (im-shell-command
     :command "rsync"
     :eat t
     :switch t
     :args
     `("-s" "--archive" "--recursive" "--verbose" "--human-readable" "--partial" "--xattrs" "--info=progress1"
       ,@(when (--any? (s-matches? ssh-regexp it) files)
           '("-e" "ssh"))
       ,@(mapcar fix-remote-files files)
       ,@(when (s-matches? ssh-regexp dest)
           '("-e" "ssh"))
       ,(funcall fix-remote-files dest))
     :on-finish
     (lambda (&rest _)
       (message ">> Files copied successfully."))
     :on-fail
     (lambda (&rest _)
       (message "!! Error while copying files.")))))

;;;;; image-mode

(evil-set-initial-state 'image-mode 'normal)
(evil-define-key 'normal image-mode-map
  ;; "q" #'evil-delete-buffer
  "+" #'image-increase-size
  "-" #'image-decrease-size
  "H" #'image-scroll-right
  "L" #'image-scroll-left
  "J" #'image-scroll-up
  "K" #'image-scroll-down
  "gg" #'image-scroll-down
  "G" #'image-scroll-up
  "h" (λ-interactive (image-scroll-right 5))
  "l" (λ-interactive (image-scroll-left 5))
  "j" (λ-interactive (image-scroll-up 5))
  "k" (λ-interactive (image-scroll-down 5))
  "r" #'image-rotate
  "_" #'image-flip-horizontally
  "\\" #'image-flip-vertically)

;;;;; calendar & diary & appt

;; Some tips:

;; - ~.~ go to today
;; - ~0,$~ beginning/end of the week
;; - ~(,)~ beginning/end of the month
;; - ~{,}~ prev/next month
;; - ~[,]~ prev/next year
;; - ~gs~ show sunrise/sunset time of date under cursor
;; - ~gd~ go to a date with a wizard.
;; - ~a~ to jump org-agenda for that day.
;; - ~d~ to show diary entries only. (agenda already shows these)
;; - ~ii~ to insert entry for that date.
;; - ~iw/im/iy/ia/ib~ to insert weekly/monthly/anneversary/block (block being between the marked day and selected day) entry.
;; - Set a mark by hitting ~v~, then go to another date and hit ~M-=~ which will show day count between those two dates.


(use-package im-holidays
  :straight nil)

(use-package calendar
  :straight (:type built-in)
  ;; Enable including other diary entries using the #include "..." syntax
  ;; I use this to separate my work and normal diary
  :hook ((diary-list-entries . diary-include-other-diary-files)
         (diary-list-entries . diary-sort-entries)
         ;; Show week numbers on calendar
         (after-init . im-calendar-week-number-mode)
         ;; Show sunrise/sunset when calendar opens
         ;; (Also use `gs' to manually show on current date)
         (calendar-today-visible . calendar-sunrise-sunset)
         (calendar-today-visible . calendar-mark-today))
  :general
  (im-leader "ec" #'calendar)
  :commands calendar
  :init
  (evil-define-key 'normal diary-fancy-display-mode-map "q" #'evil-delete-buffer)
  (evil-define-key 'normal calendar-mode-map
    (kbd "a") #'im-calendar-jump-org-agenda
    (kbd "H") #'calendar-backward-month
    (kbd "L") #'calendar-forward-month)
  :config
  (evil-collection-calendar-setup)

  (calendar-set-date-style 'european)

  ;; Discard other holiday definitions and use these only
  (setq calendar-holidays (append im-holidays-dutch-holidays im-holidays-turkish-holidays))

  ;; Mark holidays by default
  ;; u → unmark them
  ;; x → mark them again
  ;; r → show holiday for current date
  (setq calendar-mark-holidays-flag t)

  ;; Start the week from Monday
  (setq calendar-week-start-day 1)

  (setq calendar-mark-holidays-flag t)

  ;; lng and lat for my location, to get sunrise/sunset times on my
  ;; calendar (press `gs'). `calendar-sunrise-sunset' shows the
  ;; sunrise/sunset times for time under the cursor which means you
  ;; can look up sunrise/sunset times of past/future times.

  ;; See down below for im-adaptive-theme, it automatically sets
  ;; these variables. I keep this as fallback here:

  (setq calendar-latitude im-amsterdam-lat)
  (setq calendar-longitude im-amsterdam-long)

  ;; Use 24-hour format to display times
  (setq calendar-time-display-form
        '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

  (setq diary-display-function #'diary-fancy-display)
  (setq diary-number-of-entries 7)

  (general-def :keymaps 'calendar-mode-map :states '(normal motion)
    "t"  #'calendar-goto-today
    "ii" #'diary-insert-entry
    "id" #'diary-insert-entry
    "iw" #'diary-insert-weekly-entry
    "im" #'diary-insert-monthly-entry
    "iy" #'diary-insert-yearly-entry
    "ia" #'diary-insert-anniversary-entry
    "ib" #'diary-insert-block-entry
    ;; Show agenda for selected date
    "RET" #'(lambda ()
              (interactive)
              (org-agenda-list
               nil
               (calendar-absolute-from-gregorian (calendar-cursor-to-date)) 1)))

  ;; Show suntimes automatically when cursor moves
  (define-advice calendar-forward-day (:after (&rest _) show-suntimes)
    (let ((message-log-max nil))
      (calendar-sunrise-sunset)))

  ;; Show calendar at the bottom
  (im-shackle-window "Calendar" 0.3))

(defun im-calendar-jump-org-agenda ()
  "Open the currently selected calendar date on `org-agenda'."
  (interactive nil calendar-mode)
  (let ((org-agenda-window-setup 'other-window)
        (org-agenda-span 1))
    (org-calendar-goto-agenda)))

;; https://www.emacswiki.org/emacs/CalendarWeekNumbers
(defun im-calendar-week-number-mode ()
  "Show week numbers in M-x calendar."
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setq
   calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'calendar-iso-week-face)))

;;;;;; Automatically syncing with remote calendars

;; I'm experimenting with using default Emacs calendar as my work
;; calendar. To do so, I import the remote calendar into my diary
;; using the following function. Set ~im-calendar-remote-ics-file~ to
;; the url of ICS file beforehand.

;; You also need to import work diary file in your main diary file,
;; like this:

;; #include "~/.emacs.d/.cache/work-diary"


(defconst im-work-diary-path (expand-file-name "~/.emacs.d/.cache/work-diary"))

(defun im-update-calendar ()
  "Sync Emacs calendar/diary with my remote calendar."
  (interactive)
  (message ">> Updating the calender...")
  (url-retrieve
   im-calendar-remote-ics-file
   (lambda (status)
     (delete-region (point-min) url-http-end-of-headers)
     ;; Following is required because some diary entries may contain
     ;; Turkish characters and url-retrieve does not set
     ;; buffer-multibyte to t automatically
     (set-buffer-multibyte t)
     ;; Clean the work-diary first, so that items do not get
     ;; duplicated
     (with-current-buffer (find-file-noselect im-work-diary-path)
       (delete-region (point-min) (point-max)))
     (icalendar-import-buffer im-work-diary-path t)
     (message ">> Updating the calendar... DONE"))))


;; To sync it automatically:

;; Disabled it for now as our public calendar links are disabled.
;; (run-with-timer 60 (* 30 60) #'im-update-calendar)

;;;;;; appt.el: notifications for org-agenda and diary items

;; ~appt.el~ shows notifications for upcoming events in your diary and
;; in your org-agenda.

;; - By default appt shows only the entries found in diary and the included files in ~diary-file~
;; - If you want to add an appt but do not want to edit ~diary-file~, you can use ~appt-add~ which adds it to the ~appt-time-msg-list~.
;; - You can use ~appt-delete~ to remove appts from the list.
;; - ~appt-time-msg-list~ updated when
;; - this package is initialized, aka ~(require 'appt)~
;; - at 00:01
;; - manually by ~appt-check~
;; - manually by ~org-agenda-to-appt~
;; - whenever ~diary-file~ (and the files it includes) is edited
;; - whenever one of ~org-agenda-files~ is edited. (see below)

;; ~appt-message-warning-time~ is the minutes before appt.el starts showing warnings and it shows warnings every ~appt-display-interval~ minutes until the event starts.

(use-package appt
  :defer 20
  :straight (:type built-in)
  ;; Automatically update `appt-time-msg-list' after editing an
  ;; org-agenda file
  :hook (org-mode . im-org-agenda-to-appt-on-save)
  :config
  ;; Use my notification function for appt notifications
  (setq appt-disp-window-function #'im-appt-notify)
  (setq appt-message-warning-time 10)
  (setq appt-display-interval 4)

  (save-window-excursion
    (appt-activate 1)))

(defun im-org-agenda-to-appt-on-save ()
  (add-hook
   'after-save-hook
   (lambda ()
     (when (-contains? (org-agenda-files 'unrestricted) (buffer-file-name))
       ;; When REFRESH is non-nil, it removes all appts and
       ;; re-parses. This means appts that are manually added using
       ;; `appt-add' are removed. I used to use `appt-add' but now I
       ;; use `tmr' which supports both HH:MM and relative timers.

       ;; Do this async as it causes some lag on save on some of my
       ;; org files that are > 2MB.
       (async-start
        `(lambda ()
           ,(async-inject-variables "\\(org-agenda-files\\|load-path\\|org-todo-keywords\\)")
           (require 'org)
           (require 'org-agenda)
           (org-agenda-to-appt 'refresh)
           appt-time-msg-list)
        (lambda (result)
          (require 'appt)
          ;; Clear the properties that comes with it. I'm not quite
          ;; sure why is this needed but otherwise appt-check fails.
          (setq appt-time-msg-list (--map (list (car it) (car (nth 1 it)) (nth 2 it)) result))
          (message ">> appt updated")
          (appt-check)))))
   nil t))

(defun im-appt-notify (min-to-appt new-time appt-msg)
  (if (listp min-to-appt)
      (--each (-zip-pair min-to-appt appt-msg)
        (im-appt-notify--helper (car it) new-time (cdr it)))
    (im-appt-notify--helper min-to-appt new-time appt-msg)))

(defun im-appt-notify--helper (min-to-appt new-time appt-msg)
  (setq min-to-appt (string-to-number min-to-appt))
  (message ">> appt :: %s, remaining %s mins" appt-msg min-to-appt)
  (let ((important? (cond
                     ((s-contains? "meeting" (s-downcase appt-msg)) t)
                     (t nil))))
    (alert
     (im-org-header-line-to-title appt-msg)
     :title (format "Reminder, %s mins left" min-to-appt new-time)
     :severity (if important? 'high 'normal)
     :persistent important?
     :category 'appt
     :id appt-msg)))


;;;;;; More org-mode and diary integration and utilities


(defun im-diary-kill-entry-as-bullet-task (&optional include-description)
  "Copy current diary entry as a bullet.org task."
  (interactive "P")
  (-as->
   (thing-at-point 'line) it
   (s-match "\\([0-9]+:[0-9]+-[0-9]+:[0-9]+\\) \\(.*\\)" it)
   (format
    "** TODO [#A] %s :work:\nSCHEDULED: <%s %s>\n%s"
    (if-let ((link (ignore-errors (im-diary-entry-meeting-link))))
        (format "[[%s][%s]]" link (nth 2 it))
      (nth 2 it))
    (im-today)
    (nth 1 it)
    (if include-description (im-diary-entry-description) ""))
   (im-kill it)))

(defun im-diary-entry-meeting-link ()
  "Get meeting/zoom link of current diary entry."
  (let ((entry-details (im-diary-entry-description)))
    (with-temp-buffer
      (insert entry-details)
      (goto-char (point-min))
      (re-search-forward "https://.*zoom")
      (thing-at-point 'url))))

(defun im-diary-entry-description ()
  "Get details/description of current diary entry."
  (interactive)
  (save-window-excursion
    (push-button)
    (let ((beg (save-excursion
                 (re-search-backward "^[^ \n].+")
                 (forward-line)
                 (point)))
          (end (save-excursion
                 (re-search-forward "^[^ \n].+")
                 (beginning-of-line)
                 (point))))
      (im-kill (buffer-substring beg end)))))

;;;;; tramp

(setq tramp-default-method "ssh")
(setq tramp-verbose 2)
;; ^ Only show errors and warnings
(setq vc-handled-backends '(Git))
;; ^ Only try to handle git, this speeds up things a little bit

;;;;; w3m

;;;;;; Installation/Keybindings

(use-package w3m
  :commands (w3m im-w3m-open-url-dwim)
  :hook (w3m-mode . (lambda () (setq-local scroll-margin 0)))
  :general
  (:keymaps 'w3m-mode-map :states 'normal
   "&" (λ-interactive (funcall browse-url-secondary-browser-function w3m-current-url))
   "^" (λ-interactive (browse-url w3m-current-url))
   "K" #'w3m-next-buffer
   "J" #'w3m-previous-buffer
   "o" #'im-w3m-open-url-dwim
   "O" #'w3m-edit-url
   "t" (λ-interactive (im-w3m-open-url-dwim (read-string "URL: ") :new-session))
   "T" (λ-interactive (im-w3m-open-url-dwim (read-string "URL: " w3m-current-url) :new-session))
   "M-j" #'w3m-tab-move-left
   "M-k" #'w3m-tab-move-right
   "x" #'w3m-delete-buffer
   "Y" #'im-w3m-yank-url
   "f" #'im-w3m-avy-link
   "F" #'im-w3m-avy-link-new-session)
  :custom
  (w3m-fill-column 80)
  (w3m-use-title-buffer-name t)
  (w3m-default-display-inline-images t)
  (w3m-use-tab-line nil)
  ;; (setq w3m-display-mode 'plain)
  (w3m-display-mode 'plain)
  :config
  (evil-collection-w3m-setup))

(defun im-w3m-open-url-dwim (url &optional new-session)
  (interactive "sURL: ")
  (if (im-url? url)
      (if new-session (w3m-goto-url-new-session url) (w3m-goto-url url))
    (if new-session (w3m-search w3m-search-default-engine url) (w3m-search-new-session w3m-search-default-engine url))))

;;;;;; Extras

(defun im-w3m-all-anchor-points (&optional start end)
  "Return all anchor points for current w3m buffer.
If START and/or END is given, only return anchor points between
START and END positions of the buffer, otherwise return all
anchor points in the buffer."
  (let ((points (list))
        (start (or start (point-min)))
        (end (or end (point-max)))
        (prev 0))
    (save-excursion
      (goto-char start)
      (while (and (w3m-next-anchor)
                  (> (point) prev)
                  (<= (point) end))
        (setq prev (point))
        (when-let* ((url (w3m-url-valid (w3m-anchor)))
                    (_ (string-match "\\`https?:" url)))
          (push (point) points))))
    points))

(defun im-w3m-avy-link ()
  "Jump to a link and open it automatically."
  (interactive)
  (avy-process (im-w3m-all-anchor-points (window-start) (window-end)))
  (w3m-goto-url (w3m-anchor)))

(defun im-w3m-avy-link-new-session ()
  "Jump to a link and open it automatically."
  (interactive)
  (avy-process (im-w3m-all-anchor-points (window-start) (window-end)))
  (w3m-goto-url-new-session (w3m-anchor)))

(defun im-w3m-yank-url ()
  "Copy current URL to clipboard."
  (interactive nil 'w3m-mode)
  (im-kill w3m-current-url))

;; Rename w3m buffer names in a way that works with my
;; `im-tab-line-buffers' implementation
(define-advice w3m-buffer-name-add-title (:after (&rest _) rename-w3m-buffer-name)
  (when w3m-use-title-buffer-name
    (rename-buffer
     (format
      "*w3m: %s *w3m*<%d>" (w3m-current-title)
      ;; Lot's of functions on w3m depend on this number. Hence I left
      ;; it here. Ugly but not worth dealing with
      (w3m-buffer-number (current-buffer))))))

;;;;; eww -- web browser

(defconst im-reddit-comment-url-regexp ".*reddit.com/r/[a-zA-Z0-9_-]+/comments/[a-zA-Z0-9_-]+/\\([a-zA-Z0-9_-]+/?\\)\\(/[a-zA-Z0-9_-]+/?\\)?\\(&.*\\)?$")

(defun im-purified-url-handler (fn)
  "Clear the redundant stuff from urls.
Some links are prefixed with google redirection url, this
removes (and other similar stuff) so that the url handler works
properly."
  (lambda (url &rest _)
    (funcall
     fn
     (s-chop-prefix "https://www.google.com/url?q=" url))))

(setq browse-url-secondary-browser-function #'browse-url-firefox)
(setq browse-url-handlers
      `((".*jtracker.trendyol.*/browse/.*" . ,(im-purified-url-handler #'im-jira-view-ticket))
        (".*slack.com/archives/.*" . ,(im-purified-url-handler #'im-slack-open-link))
        (,im-reddit-comment-url-regexp . ,(im-purified-url-handler #'reddigg-view-comments))
        (".*news.ycombinator.com/item\\?id=.*" . ,(im-purified-url-handler #'hnreader-comment))
        (".*\\(stackoverflow.com\\|stackexchange.com\\).*" . ,(im-purified-url-handler #'im-open-stackexchange-link))
        (".*\\(youtube.com/watch.*\\|youtu.be/.*\\)" . ,(im-purified-url-handler #'empv-play-or-enqueue))
        (".*\\.mp3" . ,(im-purified-url-handler #'empv--play-or-enqueue))
        (".*github.com/.*issues/.*" . ,(im-purified-url-handler #'lab-github-issue-view))
        ("https://github.com/\\([^/]+\\)/\\([^/]+\\)/blob/\\([^/]+\\)/\\(.+\\)" . ,(im-purified-url-handler #'lab-github-view-repo-file))
        (".*github.com/[A-Za-z0-9\\. _-]+/[A-Za-z0-9\\. _-]+\\(\\?tab=readme-ov-file.*\\)?$" . ,(im-purified-url-handler #'lab-github-view-repo-readme))
        (".*zoom.us/j/.*" . ,(im-purified-url-handler #'im-open-zoom-meeting-dwim))
        (".*\\(trendyol\\|gitlab\\|slack\\|docs.google\\).*" . browse-url-firefox)
        ("." . (lambda (link &rest _) (im-eww link)))))

(use-package eww
  :commands eww
  :hook (eww-mode . tab-line-mode)
  :general
  (im-leader-v
    "ew" #'im-eww-plain
    "eW" #'im-eww)
  (:keymaps 'embark-file-map "/" #'eww-open-file)
  (:keymaps 'shr-map
   "z" nil ;; So that regular evil bindings work
   "v" nil ;; So that regular evil bindings work
   "O" nil ;; So that my bindings work
   "+" #'shr-zoom-image)
  (:keymaps 'eww-mode-map :states 'normal
   "Y" #'eww-copy-page-url
   ;; There is also u binding in shr-map but this is easier to remember
   "c" #'shr-maybe-probe-and-copy-url
   "d" #'im-eww-save-image
   "O" (λ-interactive (eww (read-string "URL: " (eww-current-url))))
   "t" #'im-eww
   "T" (λ-interactive (funcall-interactively #'im-eww (read-string "URL: " (eww-current-url))))
   "f" #'im-eww-avy-follow
   "F" (λ-interactive (im-eww-avy-follow :new-session))
   "r" #'eww-reload
   "R" #'eww-readable
   "<f2>" (λ-interactive (browse-url (eww-current-url)))
   "<f3>" (λ-interactive (browse-url-default-browser (eww-current-url)))
   "&" (λ-interactive (funcall browse-url-secondary-browser-function (eww-current-url))))
  :custom
  ;; Gifs make my computer suffer, so I just disable them
  ;; (setq shr-image-animate nil)
  (shr-max-image-proportion 0.6)
  (shr-discard-aria-hidden t)
  (shr-use-xwidgets-for-media t)

  ;; Use browse-url for each link opening. This way my
  ;; `browse-url-handlers' take precedence over eww.
  (eww-use-browse-url ".*")
  (eww-search-prefix "https://www.ecosia.org/search?q=")
  (eww-auto-rename-buffer
   (lambda () (format "*eww: %s*" (or (plist-get eww-data :title) "..."))))
  :config
  (evil-collection-eww-setup))

(defun im-eww (url)
  "`eww' wrapper.
Like `eww' but open URL in a new eww buffer instead of reusing
the same one if called interactively.  If inside an eww buffer,
then utilize `eww-browse-url' instead.  See `eww-use-browse-url'
for why."
  (interactive (list (im-web-autosuggest :history 'eww-prompt-history :initial (im-region-or ""))))
  (im-eww-plain url))

(defun im-eww-plain (url)
  (interactive (list (read-string "Search: " nil 'eww-prompt-history)))
  (cond
   ;; If called interactively, just use a new buffer
   ((called-interactively-p 'interactive) (eww url t))
   ;; If we are inside an eww buffer and this function is called, simply navigate to the URL.
   ((equal major-mode 'eww-mode) (eww-browse-url url))
   ;; Default to interactive behavior
   (t (eww url t))))

(defun im-eww-avy-follow (&optional follow-type)
  (interactive)
  (let ((wend (window-end))
        (urls '()))
    (save-excursion
      (goto-char (window-start))
      (while-let ((match (text-property-search-forward 'shr-tab-stop nil nil t))
                  ((< (point) wend)))
        (goto-char (prop-match-beginning match))
        ;; (push (get-text-property (point) 'shr-url) urls)
        (push (point) urls)))
    (avy-process (nreverse urls))
    (pcase follow-type
      (:new-session
       (eww (get-text-property (point) 'shr-url) :new-session))
      (:external (eww-browse-with-external-browser  t))
      (_ (eww-follow-link)))))

(defun im-eww-save-image ()
  "Save the image at point."
  (interactive)
  (let ((image (get-text-property (point) 'display))
        ;; Disable all save hooks as they might damage the file
        (before-save-hook '())
        (after-save-hook '()))
    (unless (and image
                 (eq (car image) 'image))
      (user-error "No images at point!"))
    (with-temp-buffer
      (setq buffer-file-name
            (read-file-name "Save to: "  nil default-directory nil))
      (insert (plist-get (cdr image) :data))
      (save-buffer))))

;;;;;; im-web-autosuggest -- autosuggestions for web searches

(defvar im-web-autosuggest-history nil)
(cl-defun im-web-autosuggest
    (&key
     (history 'im-web-autosuggest-history)
     (initial ""))
  "Autosuggest as you type for web searches.
This uses StartPage's autosuggestion endpoint.  It is not very good but
better than nothing.

HISTORY is by default `im-web-autosuggest-history', it should be a
symbol.  Access history as usual, C-r or \\[consult-history].

INITIAL is the initial string shown in the prompt.  By default it's
empty string."
  (interactive)
  (->>
   (consult--read
    (empv--consult-async-generator
     (lambda (action on-result)
       (when (not (string-empty-p (string-trim action)))
         (im-request
           "https://ac.ecosia.org/autocomplete"
           :type "list"
           :q action
           :-on-success on-result
           :-on-error (lambda (x) (list action)))))
     #'-flatten)
    :prompt "Search for: "
    :category 'url
    :lookup (lambda (selected candidates cand &rest _) selected)
    :sort nil
    :history (or history 'im-web-autosuggest-history)
    :async-wrap #'empv--consult-async-wrapper
    :require-match nil)))

;;;;;; Fixing page layouts

;; Some pages I frequently visit needs some fixing to display the
;; content better.  The following piece does exactly that.  For each
;; given url, it runs the specified function in the eww buffer.  This
;; way I get a better view.

(defvar im-eww-page-fixers
  '(("^https://\\(www.\\)?startpage.com/sp/search" . im-eww--fix-startpage)
    ("^https://\\(www.\\)?ecosia.org/search" . im-eww--fix-ecosia)
    ("^https://\\(www.\\)?eksisozluk.com" . im-eww--fix-eksisozluk)))

(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'im-eww-after-render)
  ;; The changes disappear after doing eww-readable, this fixes that.
  (define-advice eww-readable (:after (&rest _) run-after-render-hook)
    (im-eww-after-render)))

(defun im-eww-after-render ()
  (--each (im-assoc-regexp (eww-current-url) im-eww-page-fixers)
    (let ((fn (cdr it)))
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (condition-case err
            (funcall fn)
          (error
           (message "im-eww-after-render :: Error while `%s': %s" fn err))))
      (goto-char (point-min)))))

(defun im-eww--fix-startpage ()
  (while (re-search-forward "Visit in Anonymous View" nil t)
    (delete-region (line-beginning-position) (line-end-position))
    (insert ""))
  (goto-char (point-min))
  (let ((pos (point)))
    (while (and (not (eobp)) (not (looking-at "^Web results")))
      (forward-char))
    (delete-region pos (point)))
  (re-search-forward "^Startpage isn’t")
  (forward-line -1)
  (delete-region (point) (point-max))
  (page-break-lines-mode))

(defun im-eww--fix-ecosia ()
  (when (re-search-forward "^Search" nil t)
    (delete-region (point-min) (line-end-position)))
  (while (re-search-forward "^Submit" nil t)
    (delete-region (line-beginning-position) (line-end-position)))
  (page-break-lines-mode))

(defun im-eww--fix-eksisozluk ()
  (save-excursion
    (goto-char (point-min))
    (while-let ((match (text-property-search-forward
                        'display nil (lambda (_ value) (imagep value)))))
      (delete-region (1- (prop-match-beginning match)) (prop-match-end match))
      (insert "\n")))
  (page-break-lines-mode))

;;;;;; Language detection and code highlighting in eww buffers

(use-package language-detection
  :defer t)

(defun eww-tag-pre (dom)
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))

;;;;; shell-mode

(use-package shell
  :straight (:type built-in)
  :defer t
  :init
  (evil-define-key 'insert 'shell-mode-map (kbd "C-l") #'comint-clear-buffer))

;;;;; vc --- version control

;; I used to use but because of it's slowness (mostly fault of macOs
;; and my work mandatory security apps), I ditched it. vc is pretty
;; nice most of the time.

;; I've also implemented a status/commit workflow, similar to magit's
;; but it's very fast and much more streamlined for my usage. See
;; `im-git-status' and `im-git-commit'.

(use-package vc
  :straight (:type built-in)
  :general
  (:keymaps 'vc-dir-mode-map :states 'normal
   "r" #'vc-dir-refresh)
  (:keymaps 'vc-git-log-view-mode-map :states 'normal
   "<backtab>" #'im-vc-toggle-all-log-view-entries)
  :config
  (setq vc-log-short-style '(directory file))
  (evil-collection-vc-git-setup)
  (evil-collection-log-view-setup)
  (evil-collection-vc-dir-setup)
  (evil-collection-vc-annotate-setup))

(defun im-vc-diff-open-file-at-revision-dwim ()
  "Open the file at revision.
Simply works like you hit enter on a magit diff window.  Useful
when displaying old diffs and you want to jump to the full file
of that revision."
  (interactive nil diff-mode)
  (pcase-let* ((`(,old ,new) diff-vc-revisions)
               (file-path (s-trim
                           (s-chop-prefixes
                            '("--- a/" "+++ a/")
                            (save-excursion
                              (diff-beginning-of-file)
                              (thing-at-point 'line)))))
               (old? (equal "-" (char-to-string (char-after (point-at-bol)))))
               (rev (if old? old new))
               (line-info (save-excursion
                            (diff-beginning-of-hunk)
                            (s-split " " (s-trim (nth 1 (s-split "@@" (thing-at-point 'line)))))))
               (line (string-to-number
                      (car (s-split
                            ","
                            (s-chop-prefixes
                             '("-" "+")
                             (if old? (car line-info) (nth 1 line-info))))))))
    (with-current-buffer (get-buffer-create (format "%s.%s" file-path rev))
      (insert
       (shell-command-to-string
        (format
         "git show %s:%s" rev file-path)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (delay-mode-hooks
        (funcall (assoc-default file-path auto-mode-alist 'string-match))
        (reveal-mode))
      (forward-line line))))

(defun im-update-git-state (&rest _)
  "Update all diff-hl overlays and vc state for current project."
  (interactive)
  (when-let* ((project (project-current nil))
              (buffers (project-buffers project)))
    (--each buffers
      (with-current-buffer it
        (when (and buffer-file-name (derived-mode-p 'prog-mode))
          (diff-hl-update)
          (vc-refresh-state))))))

(defun im-vc-toggle-all-log-view-entries ()
  "Toggle the visibility of all log view entries in the current buffer."
  (interactive nil vc-git-log-view-mode)
  (message ">> Toggling all entries. This may take a while...")
  (save-excursion
    (goto-char (point-max))
    (while (not (equal (point) (point-min)))
      (ignore-errors (log-view-toggle-entry-display))
      (forward-line -1))
    (ignore-errors (log-view-toggle-entry-display)))
  (message ">> Toggling all entries. This may take a while...Done"))

;;;;;; git-timemachine

;; - Toggle with ~git-timemachine~ (SPC gt).
;; - When in timemachine mode,
;; - use =gt<SOMETHING>= to do timemachine specific operations. Some useful ones are:
;; - t -> =git-timemachine-show-revision-fuzzy=, selects a revision through completing-read interface.
;; - y/Y -> Yank (abbreviated) revision hash.
;; - ~C-j/k~ to go to prev/next revision of the file.

(use-package git-timemachine
  :commands (git-timemachine-toggle)
  :config
  (evil-collection-git-timemachine-setup))

;;;;; diff-mode & ediff

(use-package diff
  :straight (:type built-in)
  :config
  ;; Normally diff-mode uses reliable syntax highlighting by detecting
  ;; the full file with the support of vc backend but if there is no
  ;; context, then it does not apply the font lock.  This option makes
  ;; it use reliable highlighting if available, if not then it uses
  ;; hunk based dumb highlighting without the context.
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-font-lock-prettify t)

  (general-def
    :keymaps 'diff-mode-map
    :states '(normal motion)
    ;; This is for preventing strange jumps happening when hunks are
    ;; folded. evil-{next,previous}-visual-line causes it.
    "j" #'evil-next-line
    "k" #'evil-previous-line

    "[[" #'diff-file-prev
    "]]" #'diff-file-next
    "gj" #'diff-hunk-next
    "gk" #'diff-hunk-prev

    "A" 'diff-add-change-log-entries-other-window
    "-" #'diff-split-hunk
    ;; "u" #'evil-collection-diff-toggle-context-unified
    "o" #'im-vc-diff-open-file-at-revision-dwim
    "RET" #'diff-goto-source
    "D" #'diff-file-kill
    "d" #'diff-hunk-kill

    "TAB" #'outline-cycle
    "S-TAB" #'outline-cycle-buffer

    "1" (λ-interactive (outline-hide-sublevels 1))
    "2" (λ-interactive
         (outline-show-all)
         (outline-hide-body))
    "3" #'outline-show-all)

  (add-hook 'diff-mode-hook #'outline-minor-mode)

  (defun im-diff-mode-setup ()
    (set-face-attribute
     'diff-file-header nil
     :weight 'bold
     :height 1.5
     ;; :background "unspecified"
     ;; :box nil
     :underline nil)

    (set-face-attribute
     'diff-header nil
     ;; :background "unspecified"
     :underline nil)

    (set-face-attribute
     'diff-hunk-header nil
     :height 1.1
     :underline t))

  (add-hook 'diff-mode-hook #'im-diff-mode-setup)

  (define-advice diff-file-next (:after (&rest _) go-to-next-beginning)
    (forward-line 2)
    (outline-show-entry)))

(use-package ediff
  :straight (:type built-in)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (evil-collection-ediff-setup))

;;;;; diff-hl -- git gutter alternative

;; Highlights changed lines in git. You can also stage hunks directly
;; in the buffer.

;; I used to use `git-gutter' but it adds lots of advices and clashes
;; with my configuration.  diff-hl is a better maintained alternative
;; and seemingly faster.

;; Doom Emacs customizations for diff-hl:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/ui/vc-gutter/config.el

;; Some observations on Doom Emacs configurations:

;; `evil-insert-state-exit-hook' causes performance issues, macros
;; gets slower as the get in/out to insert state quickly.

(use-package diff-hl
  :straight (:host github :repo "dgutov/diff-hl")
  :hook
  ((prog-mode . diff-hl-mode)
   (diff-hl-mode . diff-hl-flydiff-mode))
  :custom
  (diff-hl-disable-on-remote t)
  ;; Performance optiomization for diffs.
  (vc-git-diff-switches '("--histogram"))
  ;; Async update breaks my emacs for some reason. It's not on by
  ;; default but wanted to keep it here to note this. This happens
  ;; probably due to make-thread calls. Possibly only happens on OSX.
  ;; UPDATE: After updating Emacs to 29.4, the crashing problem
  ;; disappeared but setting it to t makes diff-hl even more slow.
  (diff-hl-update-async nil)
  (diff-hl-flydiff-delay 0.5)
  (diff-hl-show-staged-changes nil)
  (diff-hl-draw-borders nil)
  :general
  (:states 'normal
   (kbd "[g") #'diff-hl-previous-hunk
   (kbd "]g") #'diff-hl-next-hunk)
  :config
  (define-advice diff-hl-previous-hunk (:after (&rest _) reveal) (reveal-post-command) (recenter))
  (define-advice diff-hl-next-hunk (:after (&rest _) reveal) (reveal-post-command) (recenter))
  (define-advice diff-hl-show-hunk-previous (:after (&rest _) reveal) (reveal-post-command) (recenter))
  (define-advice diff-hl-show-hunk-next (:after (&rest _) reveal) (reveal-post-command) (recenter))
  (define-advice diff-hl-stage-dwim (:around (orig-fn &rest args) apply-even-if-buffer-is-narrowed)
    "Original `diff-hl-stage-dwim' does not work if buffer is narrowed, this fixes it."
    (save-restriction
      (widen)
      (apply orig-fn args))))

;;;;; blamer -- git blame

(use-package blamer
  :commands (blamer-show-posframe-commit-info))

;;;;; avy

;; avy is very similar to ~vim-easymotion~. It simply jumps to a
;; visible text using a given char.

;; - =s= for jumping to beginning of a word
;; - =S= for jumping any part of the text

(use-package avy
  :commands (avy-goto-subword-1 avy-goto-word-1)
  :custom
  (avy-keys '(?q ?w ?e ?r ?t ?a ?s ?d ?f ?j ?k ?l ?u ?i ?o ?p))
  (avy-case-fold-search nil)
  (avy-all-windows t)
  :general
  (:states 'normal
   (kbd "S") #'avy-goto-subword-1
   (kbd "s") #'avy-goto-word-1))

;;;;; vertico & marginalia & orderless & mini-frame

;; A nice, fast minibuffer narrowing framework. It works well with quite a lot of package.
;; - =marginalia.el= brings annotations to completing-read, ie. it adds current keybinding of a command, summary of command to M-x.
;; - =miniframe.el= shows all completing-read prompts in a nice mini popup frame.
;; - Also see [[embark]].

;; Keybindings:
;; - =SPC 0= to repeat/open last vertico window you closed.

;; Some shortcuts you can use on any vertico window:
;; - =M-{n,p}= goes {back,forward} in minibuffer history.
;; - =M-{[,]}= goes {previous,next} group.
;; - =M-m= cycles the marginalia detail level.
;; - =M-a= brings up embark-act menu. See [[embark]].
;; - =M-A= embark act all.
;; - =M-w= copy the current candidate intelligently (im-vertico-save).
;; - =M-q= avy-like select and act.
;; - =M-q= avy-like select and act.
;; - =M-r= separaedit
;; - =M-d= insert directory (consult-dir)
;; - =M-g= vertico multigrid
;; - =M-c= vertico-select (and act with M-A on selected)
;; - =TAB= inserts the current candidate (into minibuffer).
;; - Do &<search_term> to search inside annotations (orderless feature). This is a bit slow.

(use-package vertico
  :demand t
  :hook ((after-init . vertico-mode))
  :config
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Show more candidates
  (setq vertico-count 15)
  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Bindings
  (define-key vertico-map (kbd "M-w") #'im-vertico-save)
  (define-key vertico-map (kbd "M-[") #'vertico-previous-group)
  (define-key vertico-map (kbd "M-]") #'vertico-next-group)
  (define-key vertico-map (kbd "M-j") #'next-line)
  (define-key vertico-map (kbd "M-k") #'previous-line)
  (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-b") #'vertico-scroll-down)
  (define-key minibuffer-mode-map (kbd "C-w") #'evil-window-map)

  ;; Use `consult-completion-in-region' which works with vertico
  (setq completion-in-region-function #'consult-completion-in-region))

(defun im-vertico-save ()
  "Same as `vertico-save' but removes bogus char and closes minibuffer.
Also see: https://isamert.net/2021/03/27/killing-copying-currently-selected-candidates-content-text-in-selectrum.html"
  (interactive)
  (let ((candidate (vertico--candidate))
        (prompt (minibuffer-prompt)))
    (kill-new
     (cond
      ((s-contains? "grep" prompt) (s-join ":" (-drop 2 (s-split ":" candidate))))
      ;; ^ Strip `filename:line-number:` from the text
      ((s-matches? ".*\\(Go to line\\|Switch to\\).*" prompt) (s-chop-right 1 candidate))
      ;; ^ `consult-line' and `consult-buffer' has an unrecognizable char at
      ;; the end of every candidate, so I just strip them here
      (t candidate))))
  (keyboard-escape-quit))

;;;;;; vertico extensions

;; Load vertico extensions
(add-to-list 'load-path (expand-file-name (format "%sstraight/build/vertico/extensions" straight-base-dir)))

(defun im-vertico-preview-and-suspend ()
  "Preview the candidate at point if possible and then suspend the session."
  (interactive)
  (when (and (minibufferp) consult--preview-function)
    (execute-kbd-macro (kbd consult-preview-key)))
  (vertico-suspend))

(use-package vertico-suspend
  :straight nil
  :after vertico
  :general
  (:states '(insert normal)
   "C-s" #'vertico-suspend)
  (:keymaps 'vertico-map
   "C-s" #'im-vertico-preview-and-suspend
   "C-S-s" #'vertico-suspend)
  :config
  (define-key vertico-map "\M-q" #'vertico-quick-exit))

;; Enable avy style candidate selection
(use-package vertico-quick
  :straight nil
  :after vertico
  :config
  (setq vertico-quick1 "asdfgqwe")
  (setq vertico-quick2 "hjklui")
  (define-key vertico-map "\M-q" #'vertico-quick-exit))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (im-leader "0" #'vertico-repeat))

;; Enable showing vertico in a buffer instead of minibuffer
(use-package vertico-buffer
  :straight nil
  :after vertico)

;; Switch between different vertico display forms for different commands
(use-package vertico-multiform
  :straight nil
  :after vertico
  :config
  ;; Toggle grid mode by hitting M-g while in vertico
  ;; Toggling grid mode does not work well with mini-frame.
  (define-key vertico-map "\M-g" #'vertico-multiform-grid)

  ;; '(command display-type... (buffer-local-variable . value))
  ;; vertico README explains this very well
  (setq vertico-multiform-commands
        '((consult-org-heading
           buffer
           (vertico-buffer-display-action . (display-buffer-in-side-window
                                             (side . right)
                                             (window-width . 0.4))))))
  (vertico-multiform-mode))

;; Enable displaying candidates in a grid.
(use-package vertico-grid
  :straight nil
  :after vertico)

;; Other complementary packages:

(use-package orderless
  :config
  ;; See the following: https://github.com/minad/vertico?tab=readme-ov-file#tramp-hostname-and-username-completion
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)

  ;; This is the default `orderless-affix-dispatch-alist', only
  ;; changed the orderless-annotation binding to @ from &. Copied
  ;; everything here for documentation purposes.
  (setq orderless-affix-dispatch-alist
        `(
          ;; Match anything that resembles given string,
          ;; like %3a → "[3³₃③３𝟑𝟛𝟥𝟯𝟹🯳]\\(?:a[̀-̄̆-̨̣̥̊̌̏̑]\\|[aªà-åāăąǎǟǡǻȁȃȧᵃḁạảấầẩẫậắằẳẵặₐⓐａ𝐚𝑎𝒂𝒶𝓪𝔞𝕒𝖆𝖺𝗮𝘢𝙖𝚊]\\)"
          (?% . ,#'char-fold-to-regexp)
          (?! . ,#'orderless-not)
          ;; Search inside marginalia annotations
          (?@ . ,#'orderless-annotation)
          ;; Given character should appear at word beginnings
          (?, . ,#'orderless-initialism)
          (?= . ,#'orderless-literal)
          ;; Line begins with
          (?^ . ,#'orderless-literal-prefix)
          ;; Match chars in given order
          (?~ . ,#'orderless-flex)))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :config
  (define-key minibuffer-local-map (kbd "M-m") #'marginalia-cycle)
  (setq marginalia-censor-variables '("pass\\|auth-source-netrc-cache\\|auth-source-.*-nonce\\|token\\|key\\|password\\|cookie\\|phone-number\\|app-id\\|feed-link"))
  (marginalia-mode))

(use-package mini-frame
  ;; Disabled for now.
  ;; :hook (after-init . mini-frame-mode)
  :config
  (setq mini-frame-show-parameters
        '((top . 0.15)
          (width . 0.70)
          (left . 0.5)))
  (setq mini-frame-color-shift-step 15)

  ;; Disable mini-frame while using some functions either that does
  ;; not work well with mini-frame or it doesn't make sense to use it
  ;; with mini-frame
  (setq mini-frame-ignore-commands
        '(completion-at-point
          evil-ex
          ctrlf-forward-default
          tab-jump-out
          consult-line
          consult-ripgrep
          consult-bookmark
          jq-interactively
          im-consult-ripgrep
          consult-org-heading
          consult-imenu)))

;;;;; ~project.el~ and project management

;; ~(project-remember-projects-under im-projects-root t)~ does not
;; work as I expect, so I add all of my projects using a custom
;; function that returns all of my projects' paths.

(use-package project
  :straight (:type built-in)
  :autoload (project--find-in-directory))

(defconst im-projects-root "~/Workspace/projects")

(defvar im-project-name-transformers '()
  "List of functions to do transformations on the function name.
Like shortening it in some form etc.")

(defun im-current-project-name ()
  "Return current projects name."
  (seq-reduce
   (lambda (acc it) (funcall it acc))
   im-project-name-transformers
   (if-let* ((curr-proj (im-current-project-root))
             (projects-root (expand-file-name im-projects-root)))
       (if (string-prefix-p projects-root curr-proj)
           (string-trim (string-remove-prefix projects-root curr-proj) "/" "/")
         (file-name-nondirectory (directory-file-name curr-proj)))
     (file-name-nondirectory (directory-file-name default-directory)))))

(defun im-all-project-roots ()
  "Find every project dir under `im-projects-root'.
It simply checks for folders with `.git' under them."
  (->>
   (expand-file-name im-projects-root)
   (format "fd . '%s' --type directory --maxdepth 8 --absolute-path")
   (shell-command-to-string)
   (s-trim)
   (s-split "\n")
   (--filter (im-is-git-dir it))
   (--map (abbreviate-file-name (f-full it)))))

(defalias 'im-import-projects #'im-load-projects-list)
(defun im-load-projects-list ()
  (interactive)
  (make-thread
   (lambda ()
     (message "Loading projects....")
     ;; Ensure all projects are known by project.el
     ;; The following does not recurse as I like it to be
     ;; (project-remember-projects-under im-projects-root t)
     (setq project--list '())
     (--each (--map (project--find-in-directory it) (im-all-project-roots))
       (project-remember-project it t))
     (message "Loading projects...Done."))))

(add-hook 'after-init-hook #'im-load-projects-list)

(im-leader
  "p" #'im-project-menu
  "P" #'im-project-menu-for-dir)
(setq project-switch-commands #'im-project-menu)

(defvar im-project-menu--current nil)
(defun im-project-menu ()
  (interactive)
  (let ((default-directory (or project-current-directory-override
                               (project-root (project-current t)))))
    (setq im-project-menu--current default-directory)
    (im-project-transient)))

(defun im-project-menu-for-dir ()
  (interactive)
  (setq im-project-menu--current default-directory)
  (im-project-transient))

(defun im-project-transient--wrapper (children)
  (cl-loop
   for (type . data) in children
   as command = (plist-get data :command)
   as wrapped-name = (intern (concat (symbol-name command) "--within-project"))
   do (eval `(defun ,wrapped-name ()
               (interactive)
               (let ((default-directory im-project-menu--current))
                 (call-interactively ',command))))
   collect `(,type ,@(plist-put data :command wrapped-name))))

(with-eval-after-load 'project
  (require 'transient)
  (transient-define-prefix im-project-transient ()
    "Project Commands"
    [:description (lambda () (format
                         "%s: %s"
                         (propertize "Project" 'face '(:weight bold))
                         (propertize (f-abbrev im-project-menu--current) 'face '(:foreground "green"))))
     ("s" "Switch" project-switch-project)]
    [["Core"
      :setup-children im-project-transient--wrapper
      ("RET" "Dired" dirvish-dwim)
      ("e" "Eshell" im-shell-for)
      ("t" "terminal" im-term)
      ("!" "Run shell command" im-shell-command)
      ("k" "Kill buffers" project-kill-buffers)
      ("n" "Project notes" im-org-jump-to-project-documentation)]
     ["Grep & Find"
      :setup-children im-project-transient--wrapper
      ("f" "Files" im-find-file-in)
      ("F" "Files (all projects)" im-select-any-project-file)
      ("g" "Grep" consult-ripgrep)
      ("d" "Deadgrep" deadgrep)
      ("G" "Grep (all projects)" im-consult-ripgrep-all-projects)
      ("r" "Replace regexp in project" project-query-replace-regexp)]
     ["Git (VC)"
      :setup-children im-project-transient--wrapper
      ("vd" "Status" vc-dir)
      ("vs" "Git status" im-git-status)
      ("vl" "Show stash" im-git-list-stash)
      ("vc" "Clone URL" lab-git-clone)
      ("wl" "Worktrees (list)" im-git-worktree-switch)
      ("wc" "Worktree (create)" im-git-worktree-add)
      ("wd" "Worktree (delete)" im-git-worktree-delete)]
     ["GitLab"
      :setup-children im-project-transient--wrapper
      ("M" "Merge requests" lab-list-project-merge-requests)
      ("P" "Pipelines" lab-list-project-pipelines)
      ("K" "Last failed pipeline job" lab-act-on-last-failed-pipeline-job)
      ("ms" "Search projects" lab-search-project)
      ("mc" "Create MR" lab-create-merge-request)]
     ["URLs"
      :setup-children im-project-transient--wrapper
      ("uh" "Homepage link" git-link-homepage)
      ("uc" "Copy homepage link" im-git-link-homepage)
      ("ul" "Link branch (region)" im-git-link-on-branch)]]))

;; All-projects commands

(defun im-select-any-project-file ()
  (interactive)
  (im-find-file-in im-projects-root))

(defun im-consult-ripgrep-all-projects ()
  (interactive)
  (im-consult-ripgrep im-projects-root))

;; Others

(defvar im-project-shell-last-height 20)
(defvar im-project-shell-last-window nil)

(defun im-shell-for (&optional type placement shell-fn shell-name)
  (interactive)
  (unless shell-fn
    (setq shell-fn
          (lambda (name)
            (require 'eshell)
            (save-window-excursion
              (let ((eshell-buffer-name name))
                (eshell t))))))
  (setq type (or type 'project))
  (let* ((proj-dir (or (ignore-errors
                         (project-root (project-current)))
                       default-directory))
         (default-directory (cl-case type
                              (project proj-dir)
                              (dir default-directory)
                              (t (expand-file-name type))))
         (proj-name (f-base default-directory))
         (name (format "*$%s: %s*" (or shell-name "shell") proj-name)))
    (if-let* (placement
              (window (--find (s-prefix? (format "\*\$%s: " (or shell-name "shell")) (buffer-name (window-buffer it))) (window-list)))
              (focused? (equal window (selected-window))))
        (progn
          (when (-contains? '(top bottom) placement)
            (setq im-project-shell-last-height (window-height window)))
          (delete-window window)
          (when im-project-shell-last-window
            (select-window im-project-shell-last-window)))
      (let* ((term (or (get-buffer name)
                       (funcall shell-fn name))))
        (with-current-buffer term
          (tab-line-mode -1))
        (setq im-project-shell-last-window (get-buffer-window))
        (if (not placement)
            (switch-to-buffer term)
          (progn
            (display-buffer-in-side-window
             term
             (append
              `((window-height . ,im-project-shell-last-height)
                (side . ,placement)
                (slot . 1))))
            (select-window (get-buffer-window term))))))))

(defun im-term ()
  "Select a term and open."
  (interactive)
  (empv--select-action "Term: "
    "eshell project" → (im-shell-for 'project nil nil "eshell")
    "eshell dir" → (im-shell-for 'dir nil nil "eshell")
    "eshell new" → (call-interactively #'im-eshell)
    "vterm project" → (im-shell-for 'project nil #'im--new-vterm "vterm")
    "vterm dir" → (im-shell-for 'dir nil #'im--new-vterm "vterm")
    "vterm new" → (call-interactively #'im-vterm)
    "eat project" → (im-shell-for 'project nil #'im--new-eat "eat")
    "eat dir" → (im-shell-for 'dir nil #'im--new-eat "eat")
    "eat new" → (call-interactively #'im-eat)))

(defun im--suggest-shell-name (type)
  (format "$%s: %s/%s" type (im-current-project-name)
          (if (buffer-file-name (current-buffer))
              (buffer-name)
            (symbol-name major-mode))))

(defun im-eat (name)
  "Like `im-eshell'."
  (interactive (list (read-string "Buffer name: " (im--suggest-shell-name "eat"))))
  (require 'eat)
  (let* ((eat-buffer-name name))
    (eat nil t)))

(defun im-vterm (name)
  "Like `im-eshell'."
  (interactive (list (read-string "Buffer name: " (format "$vterm: %s/%s" (im-current-project-name) (buffer-name)))))
  (require 'vterm)
  (let* ((vterm-buffer-name name))
    (vterm t)))

(defun im-eshell (name)
  (interactive (list (read-string "Buffer name: " (generate-new-buffer-name (format "*$eshell: %s*" (im-current-project-name))))))
  (with-current-buffer (eshell t)
    (rename-buffer name t)
    (current-buffer)))

(defun im--new-eat (name)
  (require 'eat)
  (save-window-excursion
    (let ((eat-buffer-name name))
      (eat nil t))))

(defun im--new-vterm (name)
  (require 'vterm)
  (save-window-excursion
    (let ((vterm-buffer-name name))
      (vterm t))))

(defun im-terminal-vertically ()
  (interactive)
  (select-window (split-window-vertically))
  (call-interactively #'im-eshell))

(defun im-terminal-horizontally ()
  (interactive)
  (select-window (split-window-horizontally))
  (call-interactively #'im-eshell))

(general-def :states 'normal
  "M-_" #'im-terminal-vertically
  "M-|" #'im-terminal-horizontally)

(im-leader "2" #'im-term)
(bind-key "<f2>" (λ-interactive (im-shell-for "~" 'top #'im--new-eat)))
(bind-key "S-<f2>" (λ-interactive (im-shell-for "~" 'top nil "eshell")))
(bind-key "<f3>" (λ-interactive (im-shell-for 'project 'bottom #'im--new-eat)))
(bind-key "S-<f3>" (λ-interactive (im-shell-for 'project 'bottom nil "eshell")))

;;;;; xref

(use-package xref
  :straight (:type built-in)
  :config
  (define-advice xref-find-definitions (:after (&rest _) reveal) (reveal-post-command)))

;;;;; consult

;; Some key points:

;; - =SPC RET= brings up =consult-buffer=.
;; - Typing =SPC {p,f,b,m}= narrows the list into {project files, files, buffers, bookmarks}.
;; - Also see: [[id:90769b1b-7baf-4285-80f9-153ae07d73ab][frequently used files]]
;; - =M-y= brings up =consult-yank=, where you can select from clipboard history and paste.
;; - =C-f= does fuzzy search on current file lines.
;; - Do =M-,= on a candidate to preview it.
;; - Also don't forget to utilize =M-a= (=embark-act=) in consult windows.
;; - Use =M-n= (future-history) to insert current symbol after running a consult command. Normally you would use =M-{p,n}= to cycle between history items but when you open minibuffer, typing =M-n= directly tries to guess what the user input would be.

(use-package consult
  :general
  (im-leader
    "oj" #'consult-org-agenda
    "cr" #'consult-history
    "RET" #'consult-buffer)
  (:keymaps 'minibuffer-mode-map
   "C-r" #'consult-history)
  (:states 'normal :keymaps 'org-mode-map
   "M-i" #'consult-org-heading
   "M-I" #'consult-org-agenda)
  (:states 'insert :keymaps 'eshell-mode-map
   "C-r" #'consult-history)
  (:states 'normal
   "M-i" (λ-interactive (condition-case nil
                            (consult-imenu)
                          (error (consult-outline)))))
  (:keymaps 'consult-narrow-map
   ;; Shows all narrowing possibilities when you hit "?"
   ;; If no narrowing is available, simply inserts "?"
   "?" #'consult-narrow-help)
  :config
  (evil-collection-consult-setup)

  (global-set-key (kbd "<f13>") #'consult-buffer)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq consult-preview-key "M-,")
  ;; ^ When you do M-, on a candidate, it previews it

  ;; Also follow symlinks while grepping
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --follow"))

  ;; xref consult integration
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; Hide some buffers from consult-buffer window. If you want to jump
  ;; on one of these buffers, start with a space after opening
  ;; `consult-buffer'.
  (add-to-list
   'consult-buffer-filter
   "\\`\\*\\(Help\\|Backtrace\\|Messages\\|Buffer List\\|Flycheck.*\\|scratch.*\\)\\'")

  ;; This also supports previews. Use the `consult-preview-key'.
  (defalias 'im-switch-theme #'consult-theme))

;;;;;; Project & file management

;; Some functionality for project management. I do some fine-tuning for =find= and =ripgrep= commands that consult uses.

(with-eval-after-load 'consult
  (setq consult-find-command "fd  --hidden --full-path ARG OPTS")
  (setq consult-ripgrep-command "rg  --hidden --null --line-buffered --color=always --max-columns=500 --no-heading --smart-case --line-number . -e ARG OPTS")
  (setq consult-project-root-function #'im-current-project-root))

(transient-define-prefix im-find-and-grep-transient ()
  "Finding and grepping."
  [["Find"
    ("f" "Find file in current directory" im-find-file-in)]
   ["Grep"
    ("d" "Deadgrep" deadgrep)
    ("b" "Grep selected branch" im-consult-git-grep-branch)
    ("g" "Ripgrep in current directory" im-consult-ripgrep-current-directory)
    ("G" "Ripgrep in given directory" im-consult-ripgrep-in-given-directory)]])

(im-leader
  "c" #'im-find-and-grep-transient)

(defun im-consult-ripgrep (&optional path)
  "`consult-ripgrep' in PATH (or in current project, if path is nil).
`consult-ripgrep' with `consult-project-root-function' shows full
path of the file in the results.  I don't want that."
  (interactive)
  (consult-ripgrep (or path (im-current-project-root))))

(defun im-consult-ripgrep-current-directory ()
  "Do ripgrep in `default-directory'."
  (interactive)
  (consult-ripgrep default-directory))

(defun im-consult-ripgrep-in-given-directory (dir)
  (interactive "DSelect directory: ")
  (consult-ripgrep dir))

(defun im-find-file-in (&optional dir)
  "Find file in DIR.
`fd' is already fast enough, no need for `consult-find's async
approach."
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (im-output-select
     :cmd "fd --exclude '.git' --exclude 'node_modules' --hidden ."
     :prompt "Open file: "
     :category 'file
     :map (s-chop-prefix "./" it)
     :do (find-file it))))

(defun im-consult-git-grep-branch (branch)
  "Run interactive grep inside given BRANCH and display the result at that revision."
  (interactive
   (list (im-output-select
          :cmd "git branch -a --format='%(refname:lstrip=2)'"
          :prompt (format "Select branch (current=%s): " (lab-git-current-branch))
          :keep-order t)))
  (pcase-let* ((default-directory (im-current-project-root))
               (result (save-window-excursion
                         (prog1 (consult-git-grep (list branch))
                           (kill-buffer (current-buffer)))))
               (`(,fname ,line-no) (s-split-up-to ":" (s-chop-prefix (concat branch ":") result) 2)))
    (with-current-buffer (get-buffer-create (format "*%s at %s*" fname branch))
      (erase-buffer)
      (insert
       (shell-command-to-string
        (format "git show %s:%s" branch fname)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-no)))
      (delay-mode-hooks
        (funcall (assoc-default fname auto-mode-alist 'string-match)))
      (font-lock-fontify-buffer)
      (switch-to-buffer (current-buffer))
      (recenter))))

;;;;;; consult-buffer and some extensions

;; I use =(consult-buffer)= function for switching between
;; buffers/files/marks etc. Here I add a source for my frequently used
;; files. This is handy in a way that =(consult-buffer)= becomes my
;; go-to place for switching to anything.

(defvar im-consult-source-files
  (list
   :name     "My files"
   :narrow   ?f
   :category 'file
   :face     'consult-file
   :history  'file-name-history
   :state    #'consult--file-state
   :default  t
   :hidden   nil
   :items    #'im-my-files)
  "My frequently accessed files source for `consult-buffer'.")

;; https://github.com/minad/consult/issues/206#issuecomment-886380330

(defvar im-consult-source-dired-buffers
  `(:name     "Dired"
    :narrow   ?d
    :hidden   t
    :category buffer
    :face     dired-header
    :state    ,#'consult--buffer-state
    :items
    ,(lambda ()
       (consult--buffer-query :mode 'dired-mode
                              :sort 'visibility
                              :as #'buffer-name)))
  "Dired buffer candidate source for `consult-buffer'.")

(with-eval-after-load 'consult
  (im-append! consult-buffer-sources 'im-consult-source-dired-buffers))

(defun im-my-files ()
  "Return list of all files I frequently use."
  `(;; Org may be loaded lazily
    ,@(when (bound-and-true-p org-directory)
        (directory-files org-directory t "^\\w+.*.org$"))
    ,@(directory-files im-load-path t "\\.el$")
    ,@(directory-files im-packages-path t "\\.el$")))

(with-eval-after-load 'consult
  (im-append! consult-buffer-sources 'im-consult-source-files)

  ;; Move bookmarks to the top of buffer sources
  (delq 'consult--source-bookmark consult-buffer-sources)
  (add-to-list 'consult-buffer-sources 'consult--source-bookmark))

;; I also add a source for listing all of my projects:

(defvar im-consult-source-projects
  (list
   :name     "Projects"
   :narrow   ?a
   :hidden   nil
   :category 'file
   :action  (lambda (it &rest _) (project-switch-project it))
   :items   (lambda () (mapcar #'car project--list)))
  "Projects source for `consult-buffer'.")

(with-eval-after-load 'consult
  (im-append! consult-buffer-sources 'im-consult-source-projects))

;;;;;; consult-dir: Jump to different places easily in minibuffer

;; Here is a nice description of the package from it's README:

;; > Avoid "navigating" long distances when picking a file or
;; > directory in any Emacs command that requires one. Think of it
;; > like the shell tools autojump, fasd or z but for Emacs.


(defvar consult-dir--my-dirs
  (list
   :name "My favorites"
   :narrow ?m
   :category 'file
   :face 'consult-file
   :items
   (lambda ()
     `("~/Downloads/"
       "~/Documents/"
       "~/Pictures/f3/Camera/"
       "~/Music/"
       "~/Music/mix/"
       "~/Videos/Shows/"
       "~/Videos/Movies/"
       "~/Documents/notes/"
       "~/Documents/notes/img/"
       "~/Workspace/projects/"
       "~/Workspace/temp/"
       "~/Workspace/temp/git/"
       "~/.emacs.d/"
       "~/.local/share/"
       "~/.local/bin/"
       "~/.config/"
       ,(format "/run/media/%s/" (user-login-name))
       ,(format "/run/media/%s/BINGUS/" (user-login-name))))))

(use-package consult-dir
  :defer
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("M-d" . consult-dir))
  :custom
  (consult-dir-sources '(consult-dir--my-dirs
                         consult-dir--source-bookmark
                         consult-dir--source-recentf
                         consult-dir--source-tramp-local
                         consult-dir--source-default
                         consult-dir--source-project)))


;;;;; embark

;; =embark.el= provides contextual command maps.

;; - =M-a= activates command mode. Next key should be command. Do =C-h= to list all commands with their keybindings.
;; - Commands are context specific, ie. the commands is based on if currently selected item is a file, folder, buffer etc.
;; - It's mostly used within the minibuffer, some example functions:
;; - =M-a w= (~embark-save~) saves the current candidate's text into kill-ring.
;; - =M-a i= (~embark-insert~) like the one above but instead of saving to the kill-ring, it directly inserts it to the buffer.
;; - =M-a S= (~embark-collect-snapshot~) creates a buffer containing all the candidates.

;; - To apply an action to multiple items, filter the items first and then do =embark-collect-snapshot=. A buffer will open and you can apply the main action or other actions on the items.

(use-package embark-consult)

(use-package embark
  :commands (embark embark-act-all)
  :config
  (bind-key (kbd "M-a") #'embark-act)
  (bind-key (kbd "M-A") #'embark-act-all)
  (bind-key (kbd "M-c") #'embark-select)
  (setq embark-prompter #'embark-completing-read-prompter)
  ;; ^ This directly shows the actions in a completing read window.
  ;; By default, it is set to `embark-keymap-prompter' and you need to
  ;; hit `C-h' to bring this menu up.
  (setq embark-indicators '(embark-highlight-indicator embark-isearch-highlight-indicator))
  ;; ^ I removed embark-mixed-indicator from the list because I'm
  ;; using embark-completing-read-prompter by default which already
  ;; provides same functionality

  ;; Hitting C-h after any prefix command will open a completing-read
  ;; interface to select a command that prefix has. Complements
  ;; which-key.
  (setq prefix-help-command 'embark-prefix-help-command)

  (setq embark-quit-after-action '((kill-buffer . nil)
                                   (t . t)))

  ;; Replace describe-symbol with helpful-symbol
  (define-key embark-symbol-map "h" #'helpful-symbol))

;;;;;; sudo-file-edit action

;; Nice little embark action that let's you open files with sudo.

;; Source: https://karthinks.com/software/fifteen-ways-to-use-embark/

(defun im-sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

(define-key embark-file-map (kbd "#") 'im-sudo-find-file)

;;;;;; search actions

(define-key embark-general-map (kbd "G") #'im-google-this)
(define-key embark-general-map (kbd "C") #'im-gpt)

;;;;;; URI-encode/hexify action

(defun im-embark-hexify/uri-encode (start end)
  "Properly URI-encode the region between START and END in current buffer."
  (interactive "r")
  (let ((encoded (url-hexify-string (buffer-substring-no-properties start end))))
    (delete-region start end)
    (insert encoded)))

(define-key embark-general-map (kbd "U") #'im-embark-hexify/uri-encode)

;;;;;; Bind command to a leader key action

(define-key embark-command-map  (kbd "m") #'im-embark-bind-leader-command)

(defun im-embark-bind-leader-command (command key)
  "Bind KEY to COMMAND in `im-leader' map.
KEY should not contain the leader key."
  (interactive
   (list
    (read-command "Set key to command: ")
    (read-string "Bind key (SPC<key>) (maybe use 3 4 5 6 7 8 9 etc.): ")))
  (im-leader :states '(normal)
    key command))

;;;;;; Fix =embark-org-copy-as-markdown=

(define-advice embark-org-copy-as-markdown (:before (&rest _) load-required-libs)
  "Load required libs."
  (require 'ox-md nil t))

;;;;;; More org-mode actions

(keymap-set embark-region-map "V" #'org-copy-visible)

;;;;; flymake

(use-package flymake
  :straight (:type built-in)
  ;; :hook ((prog-mode . flymake-mode)
  ;;        (after-init . (lambda () (setq ))))
  )

(use-package flymake-popon
  :straight (:host codeberg  :repo "akib/emacs-flymake-popon")
  :hook
  (flymake-mode . flymake-popon-mode)
  ;; Disable showing diagnostics in eldoc (in echo area) as we already
  ;; show diagnostics in a posframe.
  (flymake-popon-mode  . (lambda () (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)))
  :custom
  (flymake-popon-posframe-extra-arguments
   '(:poshandler posframe-poshandler-window-bottom-center)))

(defun im-flymake-yank-diagnostics-at-point ()
  "Copy the flymake diagnostics at point."
  (interactive)
  (im-kill
   (mapconcat #'flymake-diagnostic-text (flymake-diagnostics (point)) "\n")))

(defun im-show-diagnostic-list (arg)
  "Show all lsp errors or flycheck/flymake errors, depending on which is available.
When ARG is non-nil, query the whole workspace/project."
  (interactive "P")
  (cond
   ((bound-and-true-p lsp-mode)
    (consult-lsp-diagnostics arg))
   ((bound-and-true-p flymake-mode)
    (consult-flymake arg))
   ((bound-and-true-p flycheck-mode)
    (consult-flycheck))
   (t (user-error "No diagnostic provider found"))))

(general-def
  :states 'normal
  ;; Write ~n SPC~, ~w SPC~, ~e SPC~ to show notes, warnings, errors
  ;; only.
  "ge" #'im-show-diagnostic-list)

;;;;; corfu & corfu-doc & kind-icon

;; - When corfu popup is open
;; - ~M-SPC~ to insert a space to be able to filter with orderless.
;; - ~M-q~ to show an avy like quick selection keys.
;; - ~M-m~ to move completion items to mini-buffer (completing-read).

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  ;; Enabled differently in eshell
  (global-corfu-modes '((not eshell) t))
  :config
  (define-key corfu-map (kbd "M-j") #'corfu-next)
  (define-key corfu-map (kbd "M-k") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-complete)
  (define-key corfu-map (kbd "<tab>") nil)

  (set-face-background 'corfu-current "dim gray")
  (global-corfu-mode))

;; This is useful because sometimes I just want to collect list of
;; completions. With this I can use embark's collect functionality.
(defun corfu-move-to-minibuffer ()
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))
(keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
(add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

(use-package corfu-quick
  :straight nil
  :after corfu
  :config
  (setq corfu-quick1 "asdfgqwe")
  (setq corfu-quick2 "hjklui")
  (define-key corfu-map "\M-q" #'corfu-quick-insert))

(use-package corfu-history
  :straight nil
  :after corfu
  :config
  (corfu-history-mode))

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :config
  (setq corfu-popupinfo-delay '(1.0 . 0.3))
  (corfu-popupinfo-mode)
  ;; Toggle doc on/off while in corfu
  (define-key corfu-map (kbd "M-i") #'corfu-popupinfo-documentation)
  (define-key corfu-map (kbd "M-l") #'corfu-popupinfo-location)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up))

;;;;; cape

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-o p" . completion-at-point) ;; capf
         ("M-o t" . complete-tag)        ;; etags
         ("M-o d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-o h" . cape-history)
         ("M-o f" . cape-file)
         ("M-o k" . cape-elisp-keyword)
         ("M-o s" . cape-elisp-symbol)
         ("M-o a" . cape-abbrev)
         ("M-o i" . cape-ispell)
         ("M-o l" . cape-line)
         ("M-o e" . cape-emoji)
         ("M-o w" . cape-dict)
         ("M-o \\" . cape-tex)
         ("M-o _" . cape-tex)
         ("M-o ^" . cape-tex)
         ("M-o &" . cape-sgml)
         ("M-o r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(cl-defmacro im-cape
    (&key name completion extractor category
          bound key annotate doc (exclusive 'no) (sort t) kind)
  "Create a cape completion function with given parameters.

COMPLETION is a either a list that holds the completions or a
function that returns the completions.  This can be an arbitrary
object as EXTRACTOR is used for extracting the real completions
from this object.  EXTRACTOR is a function that is called with
the result of COMPLETION and real completion list is gathered
this way.

BOUND is boundaries of the thing that gets completed.  May be
symbol, word, file etc.

KEY is key to bind this cape to.

ANNOTATE is a function that returns annotation for given
completion.  It should return a string.  The function is called
with two arguments, first being the object returned by COMPLETION
and second being current completion item.

DOC is a function that returns documentation for given
completion.  Should return a string.  Signature is same with
ANNOTATE.

KIND is a function that returns the kind for current completion
item.  It should return a symbol like `file', `folder', `text',
`snippet', `keyword', `function', `variable', `module', `color'
etc.  In turn, there is a icon displayed that is associated with
the kind symbol.  Signature is same with ANNOTATE.

SORT should be nil to disable sorting."
  (let ((cape-fn (intern (format "im-cape-%s" (symbol-name name)))))
    `(progn
       (defun ,cape-fn (&optional interactive)
         (interactive (list t))
         (if interactive
             (cape-interactive #',cape-fn)
           (pcase-let ((`(,beg . ,end) (cape--bounds ',bound))
                       (xs (if (functionp ,completion)
                               (funcall ,completion)
                             ,completion)))
             (append
              (list beg end
                    (funcall ,extractor xs)
                    :exclusive ',exclusive)
              (when ,annotate
                (list :annotation-function (apply-partially ,annotate xs)))
              (when ,kind
                (list :company-kind (apply-partially ,kind xs)))
              (when ,doc
                (list :company-doc-buffer (lambda (x)
                                            (with-current-buffer (get-buffer-create " *im-cape-doc*")
                                              (erase-buffer)
                                              (insert (funcall ,doc xs x))
                                              (current-buffer)))))
              (when ',category
                (list :category ',category))))))
       (when ,key
         (bind-key ,key #',cape-fn)))))

;;;;; eglot

(use-package eglot
  :defer t
  :general
  (:states '(normal) :keymaps '(eglot-mode-map)
   "ga" #'eglot-code-actions
   "gd" #'xref-find-definitions
   "gr" #'xref-find-references)
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

;; This is needed so that go-to definition works for library files.
(use-package eglot-java
  :after eglot
  :straight (:host github :repo "yveszoundi/eglot-java")
  :hook (java-ts-mode . eglot-java-mode))

;;;;; lsp-mode

(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :commands lsp
  :general
  (:keymaps'(lsp-mode-map) :states '(normal)
   "gr" #'lsp-ui-peek-find-references
   "gd" #'lsp-ui-peek-find-definitions
   "gi" #'lsp-ui-peek-find-implementation
   "ga" #'lsp-execute-code-action
   "K"  #'im-peek-doc)
  :init
  (setq lsp-use-plists t)
  (setq lsp-keymap-prefix "M-l")
  :config
  (setq lsp-enable-xref t)
  (setq lsp-enable-links t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-before-save-edits nil)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-completion-provider :none) ;; for corfu

  (defalias 'im-lsp-list-workspaces #'lsp-describe-session)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.devbox\\'"))

(use-package lsp-ui
  :straight (:host github :repo "emacs-lsp/lsp-ui")
  :after lsp
  :custom
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-diagnostics nil))

(use-package consult-lsp
  :straight (:host github :repo "gagbo/consult-lsp")
  :after (lsp consult)
  :general
  (:keymaps '(lsp-mode-map) :states '(normal)
   "M-i" #'consult-lsp-file-symbols
   "M-I" #'consult-lsp-symbols))

(use-package consult-eglot
  :straight (:host github :repo "mohkale/consult-eglot")
  :after (eglot consult)
  :general
  (:keymaps '(eglot-mode-map) :states '(normal)
   ;; eglot already integrates with imenu
   ;; "M-i" #'consult-eglot-file-symbols
   "M-I" #'consult-eglot-symbols))

;;;;;; Language specific lsp-mode packages

(defun im-lsp-java-find-lombok-jar ()
  (car (sort (file-expand-wildcards "~/.m2/repository/org/projectlombok/lombok/*/lombok-*.jar") #'string>)))

(defun im-lsp-java-enable-lombok-support ()
  (interactive)
  (if-let (lombok (im-lsp-java-find-lombok-jar))
      (progn (add-to-list 'lsp-java-vmargs (concat "-javaagent:" (expand-file-name lombok)))
             (when (bound-and-true-p lsp-mode)
               (message ">> Restarting lsp workspace...")
               (lsp-workspace-restart)))
    (message ">> Lombok jar not found.")))

(use-package lsp-java
  :after lsp
  :config
  (im-lsp-java-enable-lombok-support))

;; Install metals with coursier:
;;
;;     cd ~/.local/bin/
;;     coursier bootstrap org.scalameta:metals_2.13:1.5.1 -o metals -f

(use-package lsp-metals
  :straight (:host github :repo "emacs-lsp/lsp-metals")
  :after lsp
  :custom
  (lsp-metals-enable-semantic-highlighting t)
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"
                            "-J-Dmetals.icons=unicode"))
  :hook (scala-mode . lsp))

(defun im-switch-java-version ()
  "Switch Java version using coursier."
  (interactive)
  (im-output-select
   :cmd "cs java --available | grep -Eo 'adoptium:\\d+\\.\\d+' | cut -d':' -f2 | sort -n | uniq"
   :prompt "Select Java version: "
   :do (im-shell-command
        :command "cs"
        :args (list "java" "--jvm" it "--env")
        :switch t
        :on-finish
        (lambda (output &rest _)
          (dolist (var (s-match-strings-all
                        "export \\([a-zA-Z_-]+\\)=\\(.*\\)"
                        output))
            (message ">> Setting %s → %s" (nth 1 var) (nth 2 var))
            (setenv (nth 1 var) (nth 2 var) t))))))

;;;;; vterm

;; Also check out =~/.zshrc= and =~/.config/zsh/emacs.sh=. These files
;; contains some helpful commands that enriches ~vterm~ usage.

;; Use =C-z= to go in/out (you can also use =jk= to go back into
;; normal mode from emacs mode) emacs state so that you can make use
;; of use vi-mode in zsh.

(defun evil-collection-vterm-escape-stay ()
  "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but it is not
appropriate in some cases like terminals."
  (setq-local evil-move-cursor-back nil))

(im-make-repeatable im-vterm-prompt
  "[" vterm-previous-prompt
  "]" vterm-next-prompt)

(use-package vterm
  :hook ((vterm-mode . evil-collection-vterm-escape-stay)
         (vterm-mode . im-disable-hl-line-mode-for-buffer)
         (vterm-mode . evil-emacs-state))
  :general
  (im-leader-v
    "tj" #'im-jump-to-visible-term
    "tl" #'im-run-last-command-on-visible-term
    "ty" #'im-send-selected-text-to-visible-term
    "tr" #'im-run-command-on-visible-term-with-history)
  (:keymaps 'vterm-mode-map :states 'insert
   "C-r" #'vterm--self-insert
   "M-\\" #'vterm--self-insert
   "M--" #'vterm--self-insert
   "C-c" #'vterm--self-insert
   "C-x" #'vterm--self-insert)
  :init
  :config
  (evil-collection-vterm-setup)

  ;; Setting screen as the terminal fixes some of vterm's problems, as
  ;; described here[1] but it creates some other problems like when
  ;; the screen size changes, some of the scrollback gets deleted etc.
  ;; [1]: https://github.com/akermu/emacs-libvterm/issues/179#issuecomment-1045331359

  (setq vterm-shell (executable-find "fish"))
  (setq vterm-kill-buffer-on-exit t))

;;;;;; Utility functions

(defvar im-terminal-buffer-name-regexp "\\*?\\$?\\(e?shell\\|v?term\\|eat\\).*")

(defun im-run-last-command-on-visible-term ()
  (interactive)
  (save-buffer)
  (im-with-visible-buffer im-terminal-buffer-name-regexp
    (pcase major-mode
      ('eshell-mode  (eshell-previous-matching-input "" 0)
                     (eshell-send-input))
      ('eat-mode (eat-self-input 1 (aref (kbd "<up>") 0))
                 (eat-self-input 1 (aref (kbd "RET") 0)))
      ('vterm-mode (vterm-send-up)
                   (vterm-send-return)
                   t))))

(defun im-run-command-on-visible-term (cmd)
  (im-with-visible-buffer im-terminal-buffer-name-regexp
    (pcase major-mode
      ('eshell-mode  (insert cmd)
                     (eshell-send-input))
      ('eat-mode
       (eat--send-string (eat-term-parameter eat-terminal 'eat--process) cmd)
       (eat-self-input 1 (aref (kbd "RET") 0)))
      ('vterm-mode (vterm-send-string cmd)
                   (vterm-send-return))))
  cmd)

(defun im-send-selected-text-to-visible-term (start end)
  (interactive "r")
  (if (use-region-p)
      (im-run-command-on-visible-term (buffer-substring-no-properties start end))
    (im-run-command-on-visible-term (s-trim (thing-at-point 'line t)))))

(defvar im-term-run-history '())
(defvar im-jump-to-term-last-window nil)

(defun im-jump-to-visible-term ()
  "Jump to the visible term window.
When invoked in a term window, return back to last window that
this command is invoked from."
  (interactive)
  (cond
   ((string-match im-terminal-buffer-name-regexp (buffer-name (window-buffer (selected-window))))
    (select-window im-jump-to-term-last-window))
   (t
    (setq im-jump-to-term-last-window (selected-window))
    (im-select-window-with-buffer im-terminal-buffer-name-regexp))))

(defun im-run-command-on-visible-term-with-history ()
  (interactive)
  (let ((cmd (im-run-command-on-visible-term
              (completing-read "Run new command: " im-term-run-history))))
    (when cmd
      (setq im-term-run-history (cons cmd (delete cmd im-term-run-history))))))

;;;;; simple-modeline

;; Lightweight and nice modeline.

(use-package simple-modeline
  :straight (:host github :repo "gexplorer/simple-modeline")
  :hook (after-init . simple-modeline-mode)
  :custom (simple-modeline-segments
           '((simple-modeline-segment-modified
              simple-modeline-segment-buffer-name
              simple-modeline-segment-position)
             (simple-modeline-segment-input-method
              simple-modeline-segment-eol
              simple-modeline-segment-encoding
              simple-modeline-segment-vc
              simple-modeline-segment-misc-info
              simple-modeline-segment-process
              simple-modeline-segment-major-mode))))

;;;;; howdoyou

;; When you search for something, it opens the results in an org-mode
;; buffer. Results are fetched from SX (stack-exchange, stackoverflow
;; etc) sites.

;; - =SPC hs= or =howdoyou-query= :: search function
;; - =C-M-Left= :: prev answer
;; - =C-M-Right= :: next answer

(use-package howdoyou
  :commands (howdoyou-query howdoyou--get-buffer)
  :general
  (:keymaps 'howdoyou-mode-keymap :states '(normal motion)
   "gn" #'howdoyou-next-link
   "gp" #'howdoyou-previous-link)
  (im-leader
    "is" #'howdoyou-query)
  :config
  (setq howdoyou-switch-to-answer-buffer t)
  (setq howdoyou-number-of-answers 5))

(defun im-open-stackexchange-link (link)
  "Open stackexchange LINK in a nicely formatted org buffer."
  (interactive "sLink: ")
  (switch-to-buffer (howdoyou--get-buffer))
  (insert "Loading...")
  (let ((buffer (current-buffer)))
    (promise-chain (howdoyou-read-so-link link)
      (then #'(lambda (_)
                (im-url-get-title-async
                 link
                 (lambda (title)
                   (with-current-buffer buffer (rename-buffer (format "*se: %s*" title) :unique)))))))))

;;;;; tldr

;; tldr client for Emacs.

(use-package tldr
  :commands tldr
  :general
  (im-leader
    "it" #'tldr))

;;;;; yasnippet & yankpad: template manager

(use-package yasnippet
  :hook
  ((minibuffer-setup . yas-minor-mode)
   ;; ^ Enable expanding in mini-buffer.
   (after-init . yas-global-mode)))

(use-package yankpad
  :straight (:host github :repo "isamert/yankpad")
  :after (org yasnippet)
  :autoload (yankpad--categories yankpad--snippets)
  :general
  (im-leader
    "sr" #'yankpad-reload
    "sc" #'yankpad-set-category
    "se" #'yankpad-edit
    "ss" #'yankpad-map
    "sm" #'yankpad-map)
  (:states 'normal
   (kbd "M-s") #'yankpad-insert)
  (:states 'insert
   (kbd "M-s") #'yankpad-insert
   (kbd "M-e") #'hippie-expand)
  (:keymaps 'minibuffer-local-map
   (kbd "M-s") #'yankpad-insert
   (kbd "M-e") #'hippie-expand)
  :custom
  (yankpad-file snippets-org)
  ;; Categories returned by the following functions will be used to
  ;; expand snippets
  (yankpad-auto-category-functions '(yankpad-major-mode-category im-current-project-name))
  :init
  (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand))

;; I also like to use these snippets outside of Emacs. For this, I
;; defined =im-select-any-snippet=. It let's you select any snippet
;; from the yankpad file (no mode restriction, shows all snippets
;; prefixed with mode name they are defined for) through
;; =completing-read=. Combined with ~im-globally~, you can use this
;; outside of Emacs.

(defvar im--all-snippets-cache nil)

(defun im-load-snippets-list ()
  (interactive)
  (make-thread
   (lambda ()
     (message ">> Loading snippets...")
     (setq
      im--all-snippets-cache
      (-mapcat
       (lambda (category)
         (--map (cons (format "%s :: %s" category (car it)) it)
                ;; Clear some sections of the data that causes
                ;; issues with emacs-async. It prints out some data
                ;; like #3 #5 etc. which can't be `read'.
                (--map
                 (if (alist-get 'src-block (nth 2 it))
                     `(,@(-take 2 it) nil ,@(-take-last 2 it))
                   it)
                 (ignore-errors
                   (yankpad--snippets category)))))
       (yankpad--categories)))
     (message ">> Loading snippets... Done"))))

;; Load snippets for the first time, after the startup
;; This may take a bit of time
(with-eval-after-load 'org
  (run-with-timer 5 nil #'im-load-snippets-list))

(defun im-select-any-snippet ()
  "List all templates for all modes and return the applied template as string."
  (interactive)
  (-let* ((selected (completing-read "Select snippet: " im--all-snippets-cache))
          (snippet (alist-get selected im--all-snippets-cache nil nil #'equal))
          ((_ category _) (s-match "\\(.*\\) :: \\(.*\\)" selected)))
    (with-temp-buffer
      (setq yankpad-category category)
      ;; Activate yas snippet on the temp buffer
      (let ((yas-dont-activate-functions '()))
        (yas-minor-mode-on))
      (yankpad--run-snippet snippet)
      (im-kill (buffer-string)))))

;;;;; git-link

(use-package git-link
  :demand t
  :config
  (setq git-link-open-in-browser t))

(defun im-git-link-homepage ()
  "Like `git-link-homepage' itself but it does not open in browser, simply returns the address as string."
  (interactive)
  (let ((git-link-open-in-browser nil))
    (call-interactively 'git-link-homepage)
    (car kill-ring)))

(defun im-git-link-on-branch (branch)
  "Like `git-link' but let's you select the branch first when called interactively."
  (interactive
   (list
    (completing-read
     "Select a branch: "
     (vc-git-branches)
     nil nil (lab-git-current-branch))))
  (let ((git-link-default-branch branch))
    (call-interactively 'git-link)))

(defun im-git-link-commit ()
  "Like `git-link' but use commit hash in url."
  (interactive)
  (let ((git-link-use-commit t))
    (call-interactively 'git-link)))

(defun im-git-link-merge-requests ()
  "Open MR page."
  (interactive)
  (browse-url
   (let ((homepage (im-git-link-homepage)))
     (cond
      ((s-contains? "gitlab" homepage) (concat homepage "/-/merge_requests"))
      ((s-contains? "github" homepage) (concat homepage "/pulls"))
      (t (user-error "Forge undefined"))))))

;;;;; tab-out

;; When you press tab, jump out from the current enclosing
;; parens/quotes etc. When there is no enclosing stuff, TAB key
;; automatically fallbacks to it's default behavior.

(use-package tab-jump-out
  :diminish
  :straight (:host github :repo "zhangkaiyulw/tab-jump-out")
  :config
  ;; This is not defined as a global minor mode, so define one and enable it
  (define-globalized-minor-mode global-tab-jump-out-mode tab-jump-out-mode
    (lambda ()
      (tab-jump-out-mode)
      (push "/" tab-jump-out-delimiters)
      (push "=" tab-jump-out-delimiters)))
  (global-tab-jump-out-mode 1))

;;;;; helpful and elisp-demos

;; - helpful :: Better help dialogs with syntax highlighting, references, source etc.
;; - elisp-demos :: Adds code examples into function help buffers.
;; - Code examples are maintained [[https://github.com/xuchunyang/elisp-demos/blob/master/elisp-demos.org][here]], don't forget to contribute!
;; - Call ~elisp-demos-add-demo~ to add a demo locally.

(use-package helpful
  ;; Override default help bindings
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h p" . helpful-at-point))
  :config
  (evil-define-key 'normal helpful-mode-map
    "q" 'evil-delete-buffer))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Here, I also bind some keys for convenience. Taken from
;; [[https://github.com/radian-software/radian/blob/23e80b3d865bfb60b166309249ac4db2b176b9fc/emacs/radian.el#LL3970C1-L3982C1][here]].

(bind-key "C-h C-f" #'find-function)
(bind-key "C-h C-v" #'find-variable)
(bind-key "C-h C-l" #'find-library)

;;;;; expand-region

;; Also see combobulate package. I use it instead
;; (`combobulate-mark-node-dwim') whenever possible instead of
;; expand-region.

(use-package expand-region
  :straight (:host github :repo "karthink/expand-region.el")
  :general
  (:states '(insert normal)
   "M-w" #'er/expand-region))

;;;;; aggressive-indent

;; It keeps your indentation working all the time. Seems like a good
;; idea but I have some concerns about it, so I just use it with elisp
;; for the time being.

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-protected-commands 'evil-undo)
  (add-to-list 'aggressive-indent-protected-commands 'format-all-buffer))

;;;;; xmodmap-mode

;; Simple mode for editing =~/.Xmodmap= file.
;; Source:  https://www.emacswiki.org/emacs/XModMapMode

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

;;;;; slack

(use-package slack
  :straight (:host github :repo "emacs-slack/emacs-slack")
  :defer t
  :general
  (im-leader-v
    "ess" #'im-slack-select-room
    "esd" #'im-slack-dms
    "esS" #'slack-select-unread-rooms
    "esm" #'im-slack-send-message
    "est" #'slack-all-threads
    "esr" #'im-slack-last-messages-alternative
    "esR" #'im-slack-last-messages
    "esl" #'im-slack-open-last-message
    "esy" #'im-slack-yank-last-message)
  :config
  (setq slack-log-level 'error)
  ;; ^ info level shows unnecessary stuff that distracts me
  (setq slack-block-highlight-source t)
  (setq slack-buffer-emojify nil)
  (setq slack-render-image-p t)
  (setq slack-image-max-height 150)
  (setq slack-prefer-current-team t)
  ;; ^ Set current team with `slack-change-current-team'
  (setq slack-buffer-function #'switch-to-buffer)

  (setq slack-typing-visibility nil)
  ;; ^ Typing indicators causes some issues on my end.  Gonna disable
  ;; it for now.

  ;; Disable filling as it fucks with copying stuff
  (setq lui-fill-type nil)
  (setq lui-fill-column 0)
  (setq lui-time-stamp-format "[%a %b %d %H:%M]")
  (setq slack-message-custom-notifier #'im-slack-notify)
  (setq slack-message-custom-delete-notifier #'im-slack-notify)

  (setq slack-visible-thread-sign "╚═> ")

  (add-hook 'slack-message-buffer-mode-hook #'tab-line-mode)
  (add-hook 'slack-thread-message-buffer-mode-hook #'tab-line-mode)

  (evil-set-initial-state 'slack-mode-map 'normal)

  (evil-define-key 'normal slack-message-edit-buffer-mode-map
    "@"  #'slack-message-embed-mention
    "mc" #'slack-message-embed-channel)
  (evil-define-key 'normal slack-message-compose-buffer-mode-map
    "@"  #'slack-message-embed-mention
    "mc" #'slack-message-embed-channel)

  ;; Fix for/from yuya373/emacs-slack#547-1542119271
  (advice-add 'lui-buttonize-urls :before-until (lambda () (derived-mode-p 'slack-mode)))

  (dolist (mode '(slack-buffer-mode-hook
                  slack-edit-message-mode-hook
                  slack-message-buffer-mode-hook
                  slack-all-threads-buffer-mode-hook
                  slack-message-edit-buffer-mode-hook
                  slack-search-result-buffer-mode-hook
                  slack-thread-message-buffer-mode-hook
                  slack-message-compose-buffer-mode-hook))
    (add-hook mode #'im-slack--enable-completion-at-point)
    (add-hook mode #'visual-line-mode)))

(defun im-slack-initialize ()
  (interactive)
  (ignore-errors (slack-ws-close))
  (ignore-errors (slack-team-delete))
  (setq im-slack--last-messages nil)
  (im-slack-kill-buffers)
  (slack-register-team
   :name ty-slack-name
   :token ty-slack-token
   :enterprise-token ty-slack-enterprise-token
   :cookie ty-slack-cookie
   :subscribed-channels ty-slack-channels
   :visible-threads nil
   :mark-as-read-immediately t)
  (slack-start)
  (slack-change-current-team)
  (run-at-time nil 3600 #'im-slack-check))

(defun im-slack--enable-completion-at-point ()
  (setq-local
   completion-at-point-functions
   `(,(cape-company-to-capf #'company-slack-backend)
     cape-emoji)))

(defun im-slack-kill-buffers ()
  (interactive)
  (--each (--filter
           (s-prefix? "*slack: " (buffer-name it)) (buffer-list))
    (unless (ignore-errors (kill-buffer it))
      (with-current-buffer it
        (let ((kill-buffer-query-functions nil)
              (kill-buffer-hook nil))
          (kill-buffer))))))

(defun im-slack-toggle-timestamps ()
  "Toggle visibility of timestamps in current buffer."
  (interactive nil slack-message-buffer-mode slack-thread-message-buffer-mode-map)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'lui-time-stamp)
          (if (get-text-property (point) 'invisible)
              (remove-text-properties (point) (1+ (point)) '(invisible nil))
            (put-text-property (point) (1+ (point)) 'invisible t)))
        (goto-char (next-property-change (point) nil (point-max)))))))

(defun im-slack--add-reaction-to-message (reaction)
  (defalias (intern (concat "react-" reaction))
    `(lambda ()
       (interactive)
       (slack-buffer-add-reaction-to-message
        slack-current-buffer
        ,reaction
        (slack-get-ts)))))

(defvar im-slack-dnd nil)
(defvar im-slack--last-messages '())

(defun im-slack-toggle-dnd ()
  (interactive)
  (message "slack :: DND is %s." (setq im-slack-dnd (not im-slack-dnd))))

(defun im-slack-check ()
  (interactive)
  (let* ((one-hour-ago (- (string-to-number (format-time-string "%s")) 3600))
         (msg-count (->>
                     (im-slack-last-messages-per-room)
                     (--filter (> (im-slack--message-ts it) one-hour-ago))
                     (length))))
    (when (> msg-count 0)
      (message ">> Check slack. %s messages in last hour."
               (propertize (number-to-string msg-count) 'face '(:weight bold))))))

(async-defun im-slack-notify (message room team)
  (unless (slack-room-muted-p room team)
    (let* ((sender-name (slack-message-sender-name message team))
           (room-name (slack-room-name room team))
           (title (format "%s - %s" room-name sender-name))
           (msg-str (im-slack--stringify-message
                     (list :message message :team team))))
      (push
       (list :room room
             :team team
             :message message
             :sender-name sender-name
             :room-name room-name
             :title title
             :message-string msg-str)
       im-slack--last-messages)
      (unless (or (slack-message-minep message team)
                  (s-contains? "message deleted" msg-str)
                  (s-contains? "has joined the" msg-str)
                  (s-contains? "has left the" msg-str)
                  ;; Dont show notifications for visible slack windows if emacs is not idle
                  (and
                   (< (time-to-seconds (or (current-idle-time) 0)) 15)
                   (--some
                    (s-contains? room-name it)
                    (--map (buffer-name (window-buffer it)) (window-list)))))
        ;; Only send desktop notifications for the things I'm interested
        ;; mpim || group || in subscribed channels
        (when (slack-message-notify-p message room team)
          ;; msg-str sometimes causes errors with `alert'. Thats why I
          ;; used `ignore-errors'.
          (ignore-errors
            (unless im-slack-dnd
              (alert
               msg-str
               :title title
               :category "slack"))))
        (unless im-slack-dnd
          (message
           ">> Slack: %s // %s"
           title
           (if (await (im-screen-sharing-now?))
               "[REDACTED due to screensharing]"
             (s-truncate 80 (s-replace "\n" "" msg-str)))))))))

(defun im-slack-yank-last-message ()
  "Yank the contents of the last received message as text."
  (interactive)
  (im-kill
   (im-slack--stringify-message
    (im-slack--last-message))))

(defun im-slack-open-last-message ()
  "Open last room that got new message."
  (interactive)
  (im-slack--open-message-or-thread (im-slack--last-message) :focus? nil))

(cl-defun im-slack--open-message-or-thread (msg &key (focus? t))
  (let-plist msg
    (if (ignore-errors (slack-thread-message-p .message))
        (slack-thread-show-messages .message .room .team)
      (slack-room-display
       .room
       .team))
    ;; Focus the message on buffer
    (when focus?
      (run-with-timer
       1.3 nil
       (lambda () (slack-buffer-goto (slack-ts .message)))))))

(defalias 'im-slack-recent-messages #'im-slack-last-messages)

(defun im-slack-last-messages-per-room ()
  (->>
   im-slack--last-messages
   (--map (list :room-name (plist-get it :room-name)
                :room (plist-get it :room)
                :team (plist-get it :team)))
   (-uniq)
   (-map (lambda (room)
           (--find (equal (plist-get room :room-name)
                          (plist-get it :room-name))
                   im-slack--last-messages)))))

(defun im-slack--message-ts (message)
  (->>
   (plist-get message :message)
   slack-ts
   (s-split "\\.")
   car
   string-to-number))

(defun im-slack-last-messages ()
  "List and open rooms that had new messages in them recently."
  (interactive)
  (im-slack--open-message-or-thread
   (im-completing-read
    "Select message: "
    im-slack--last-messages
    :sort? nil
    :formatter #'im-slack--format-message)
   :focus? nil))

(defun im-slack--format-message (it)
  (let-plist it
    (format "%s (%s) - %s"
            (propertize .room-name 'face '(:weight bold :foreground "systemPinkColor"))
            (when .message
              (propertize
               (lab--time-ago
                (im-slack--message-ts it))
               'face '(:slant italic :foreground "systemGrayColor")))
            (s-truncate 50 .message-string))))

(defun im-slack-last-messages-alternative ()
  "Like `im-slack-last-messages' but only show the last message per room."
  (interactive)
  (let ((selected
         (im-completing-read
          "Select room: "
          (im-slack-last-messages-per-room)
          :formatter #'im-slack--format-message
          :sort? nil)))
    (let-plist selected
      (slack-room-display
       .room
       .team))))

(defun im-slack-send-message (msg)
  "Send given MSG or region as message to interactively selected user."
  (interactive
   (list
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (if (y-or-n-p "Wrap with backticks? ")
              (format "```\n%s\n```" text)
            text))
      (read-string "Enter message: "))))
  (-let* (((room team) (im-slack--select-room)))
    (slack-message-send-internal
     msg room team)))

(defun im-slack-clipboard-image-upload ()
  "Uploads png image from clipboard.

The default `slack-clipboard-image-upload' was not working
properly in MacOS."
  (interactive)
  (unless (im-clipboard-contains-image-p)
    (user-error "No image in clipboard"))
  (let* ((file (make-temp-file "clip" nil ".png")))
    (im-save-clipboard-image-to-file file)
    (slack-file-upload file "png" "image.png")))

;; TODO multiple message quote
(defun im-slack-quote-message ()
  (interactive)
  (let ((quote-text (->>
                     (im-slack-current-message-content)
                     (substring-no-properties)
                     (s-trim)
                     (s-split "\n")
                     (-drop 1)
                     (--map (concat "> " it))
                     (s-join "\n")
                     (s-append "\n"))))
    (slack-message-write-another-buffer)
    (insert quote-text)))

(defun im-slack-current-message-content ()
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (room (slack-buffer-room buf))
                  (message (slack-room-find-message room (slack-get-ts))))
      (slack-message-to-string message team)))

(defun im-slack-open-link (link)
  (interactive
   (list
    (read-string "Link: " (thing-at-point 'url))))
  (let* ((m (s-match
             "https://\\(\\w+\\)\\(.enterprise\\)?.slack.com/archives/\\(\\w+\\)/p\\(\\w+\\).*\\(\\?thread_ts=\\(\\w+\\)\\)?"
             link))
         (team (--find (string= (oref it domain) (nth 1 m))
                       (hash-table-values slack-teams-by-token)))
         (room (slack-room-find (nth 3 m) team))
         (message-ts (number-to-string (/ (string-to-number (nth 4 m)) 1000000.0)))
         (message (slack-room-find-message room message-ts))
         (thread (nth 5 m)))
    (im-slack--open-message-or-thread (list :message message :room room :team team))
    thread))

(defun im-slack-dms (&optional data)
  "List and open dms.
Get DMs and MPIMs from client.dms endpoint, sort them by time and
present to user.  This kind of guarantees the order that you see
in the DM section of the official Slack client.

Fetches missing channels/users first."
  (interactive)
  (let ((team (or (slack-team-select))))
    (if data
        (let* ((selected
                (im-completing-read
                 "Select: "
                 (--sort
                  (>
                   (string-to-number (plist-get (plist-get it :message) :ts))
                   (string-to-number (plist-get (plist-get other :message) :ts)))
                  (append
                   (plist-get data :mpims)
                   (plist-get data :ims)))
                 :sort? nil
                 :formatter
                 (lambda (it)
                   (let ((room-name (slack-room-name (slack-room-find (plist-get it :id) team) team))
                         (text (plist-get (plist-get it :message) :text)))
                     (format "%s (%s) - %s"
                             (propertize room-name 'face '(:weight bold :foreground "systemPinkColor"))
                             (propertize
                              (lab--time-ago
                               (->>
                                (plist-get (plist-get it :message) :ts)
                                (s-split "\\.")
                                car
                                string-to-number))
                              'face '(:slant italic :foreground "systemGrayColor"))
                             (s-truncate 50 text)))))))
          (slack-room-display
           (slack-room-find (plist-get selected :id) team)
           team))
      (slack-request
       (slack-request-create
        "https://slack.com/api/client.dms?count=250"
        team
        :success
        (cl-function
         (lambda (&key data &allow-other-keys)
           (ignore-error (quit minibuffer-quit)
             (let* ((user-ids
                     (append
                      (-non-nil (--map (plist-get (plist-get it :message) :user) (plist-get data :ims)))
                      (-non-nil (--map (plist-get (plist-get it :message) :user) (plist-get data :mpims)))))
                    (missing-user-ids (slack-team-missing-user-ids (slack-team-select) user-ids))
                    (channels (append
                               (-non-nil (--map (plist-get it :id) (plist-get data :ims)))
                               (-non-nil (--map (plist-get it :id) (plist-get data :mpims)))))
                    (missing-channel-ids (-non-nil (--map (if (slack-room-find it (slack-team-select)) nil it) channels)))
                    (missing-users? (> (length missing-user-ids) 0))
                    (missing-channels? (> (length missing-channel-ids) 0)))
               (cond
                (missing-users?
                 (message ">> Some people are missing: %s, loading..." missing-user-ids)
                 (slack-user-info-request
                  missing-user-ids
                  team :after-success
                  (lambda ()
                    (message ">> Some people are missing: %s, loading...Done" missing-user-ids)
                    (im-slack-dms data))))
                (missing-channels?
                 (message ">> Some channels are missing: %s, loading..." missing-channel-ids)
                 (let* ((finished-count 0)
                        (cb (lambda ()
                              (setq finished-count (1+ finished-count))
                              (when (length= missing-channel-ids finished-count)
                                (message ">> Some channels are missing: %s, loading...Done" missing-channel-ids)
                                (im-slack-dms data)))))
                   (--each missing-channel-ids
                     (slack-conversations-info
                      it
                      team
                      (lambda () (funcall cb))))))
                (t (im-slack-dms data))))))))))))

(defun im-slack-select-room ()
  "Like `slack-select-rooms' but disable sorting."
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-room-names
                 (cl-loop for team in (list team)
                          append (append (slack-team-ims team)
                                         (slack-team-groups team)
                                         (slack-team-channels team)))
                 team
                 #'(lambda (rs)
                     (cl-remove-if
                      (lambda (it)
                        (slack-room-hidden-p it))
                      rs)))))
    (slack-room-display
     (cdr (im-completing-read "Select: " alist :formatter #'car :sort? nil))
     team)))

;;
;; Utils/internals
;;

(defun im-slack--last-message ()
  (--find (not (s-matches? ".*\\(alert\\|practice\\).*" (plist-get it :room-name))) im-slack--last-messages))

(defun im-slack--stringify-message (msg)
  (let ((message (plist-get msg :message))
        (team (plist-get msg :team)))
    (slack-message-to-alert message team)))

(defun im-slack--select-room ()
  "Select interactively and return (room team) pair."
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (append (slack-team-ims team)
                                        (slack-team-groups team)
                                        (slack-team-channels team)))
                team)))
    (list room team)))

(general-def :keymaps '(slack-message-buffer-mode-map slack-thread-message-buffer-mode-map slack-all-threads-buffer-mode-map) :states '(normal motion)
  "q" #'im-quit

  "@" 'slack-message-embed-mention
  "mc" 'slack-message-embed-channel
  "mm" 'slack-message-write-another-buffer
  "md" 'slack-message-delete
  "my" 'slack-message-copy-link
  "ml" 'slack-message-copy-link
  "me" 'slack-message-edit
  "mt" 'slack-thread-show-or-create
  "mq" 'im-slack-quote-message
  "mi" 'slack-display-user-profile-info
  "mj" 'slack-thread-message-buffer-jump-to-channel-buffer

  "mrr" 'slack-message-add-reaction
  "mR" 'slack-message-remove-reaction
  "mrs" (im-slack--add-reaction-to-message "seen")
  "mr1" (im-slack--add-reaction-to-message "+1")
  "mr2" (im-slack--add-reaction-to-message "ok_hand")
  "mr3" (im-slack--add-reaction-to-message "eyes")
  "mr4" (im-slack--add-reaction-to-message "ultrafastparrot")
  "mr5" (im-slack--add-reaction-to-message "pepedance")
  "mrp" (im-slack--add-reaction-to-message "prayge")
  "mrh" (im-slack--add-reaction-to-message "handshake")
  "mrl" (im-slack--add-reaction-to-message "pepe-love")

  "[[" 'slack-buffer-goto-prev-message
  "]]" 'slack-buffer-goto-next-message)

(im-make-repeatable slack
  "[" slack-buffer-goto-prev-message
  "]" slack-buffer-goto-next-message)

;;;;; prodigy

(use-package prodigy
  :straight (:host github :repo "rejeep/prodigy.el")
  :hook (after-init . im-prodigy-autostart)
  :demand t
  :general
  (im-leader
    "et" #'prodigy)
  :config
  (evil-define-key 'normal prodigy-mode-map
    "q" #'im-quit
    "m" #'prodigy-mark
    "u" #'prodigy-unmark
    "x" #'prodigy-stop
    "S" #'prodigy-start
    "r" #'prodigy-restart
    "R" #'prodigy-refresh
    "f" #'prodigy-jump-file-manager
    "M" #'prodigy-jump-magit
    "t" #'prodigy-add-tag-filter
    "T" #'prodigy-clear-filters
    (kbd "RET") #'prodigy-display-process))

(prodigy-define-service
  :name "kmonad - keyboard remapper"
  :command (expand-file-name "~/workspace/apps/kmonad/result/bin/kmonad")
  :args (lambda (x y)
          (->>
           (format
            "~/.config/kmonad-%s"
            (completing-read
             "Which configuration?"
             (--map (s-chop-left 7 it) (directory-files "~/.config/" nil "kmonad-.*"))))
           (expand-file-name)
           (list)))
  :sudo t)

(defun im-prodigy-autostart ()
  "Start all services with the `:auto-start' set to non-nil if they are not already started."
  (interactive)
  (prodigy-with-refresh
   (--each
       prodigy-services
     (when (and (plist-get it :auto-start)
                (not (prodigy-service-started-p it)))
       (prodigy-start-service it)))))

;;;;; separedit -- edit comment blocks/multiline strings etc. indirectly

;; Pop current comment section OR a multiline string into a markdown
;; buffer and edit it there. Pretty useful for writing long
;; comments/strings.

;; It also supports editing code blocks inside comment blocks in their
;; language, like
;; ```c
;; int main () {
;;    int x = 3; // Do M-x `separedit' here.
;; }
;; ```

;; If the code block does not have a language marker, then it's
;; assumed to be in the same language of the buffer.
;; ```
;; (progn
;;   (+ 1 2))
;; ```

(use-package separedit
  ;; C-u separedit → Let's you select the editing mode first.
  :general
  (:states 'normal
   "gm" #'separedit)
  (:keymaps 'minibuffer-mode-map
   "M-r" #'separedit)
  :config
  (setq separedit-default-mode 'markdown-mode)
  (setq separedit-continue-fill-column t)
  (advice-add
   'separedit--select-mode
   :override
   (lambda ()
     (completing-read "Select major mode: " obarray
                      (lambda (x)
                        (and (fboundp x)
                             (commandp x)
                             (string-match "-mode$" (symbol-name x))))
                      t
                      nil 'separedit--mode-history (or (car separedit--mode-history)
                                                       (format "%s" major-mode))))))

;;;;; all-the-icons

;; You should run =all-the-icons-install-fonts= command after
;; this. Also run =fc-cache -f -v= afterwards
;; (=all-the-icons-install-fonts= already does that but it may fail).
;; =all-the-icons-completion= gives you icons in completion UI, like
;; in =completing-read= etc.

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (all-the-icons vertico)
  :config
  (all-the-icons-completion-mode +1))

;;;;; reddigg and hnreader -- org-mode interface for reddit and hacernews

;; These packages lets me display comments of given reddit/hn thread
;; in an org buffer.
;;
;; Also see `browse-url-handlers' configuration to see how I utilize
;; these packages.

(use-package reddigg
  :defer t
  :straight (:host github :repo "isamert/emacs-reddigg")
  :config
  (setq reddigg-convert-md-to-org t))

(use-package hnreader
  :defer t)

;;;;; imenu-list

;; You can also do ~consult-imenu~ and ~embark-collect~ but it does not have a refresh feature.

(use-package imenu-list
  :defer t
  :init
  (im-leader "il" #'imenu-list))

;;;;; im-ai & org-ai & gptel -- interactions with LLMs

;; There are bunch of Emacs-ChatGPT integrations and org-ai seems to
;; be best as it fits my workflow quite well. Besides being useful in
;; org-mode, it also has org-mode independent features.

(use-package org-ai
  :straight (:host github :repo "rksm/org-ai")
  :hook (org-mode . org-ai-mode)
  :custom
  (org-ai-default-chat-model "gpt-4o")
  (org-ai-default-max-tokens 2000)
  (org-ai-default-chat-system-prompt "You're an helpful assistant designed to provide helpful, accurate, and concise responses. Your primary focus should be on delivering quick and clear solutions, especially for programming-related queries. Focus on providing quick, clear solutions. When appropriate, offer additional insights, alternative approaches (such as using standard functions over ad-hoc implementations), or different perspectives to enhance user understanding or outcomes along with the original solution. Keep replies relevant, to the point, and free from unnecessary explanations or obvious/elementary information.")
  :config
  (add-to-list 'org-ai-chat-models "gpt-4o-mini"))

(use-package im-ai
  :straight nil
  :custom
  (im-ai-file "~/Documents/notes/extra/gpt.org")
  :general
  (im-leader-v
    "eg" #'im-ai
    "sl" #'im-ai-lookup
    "sa" #'im-ai-snippet
    "sg" #'im-ai-gptel-dwim
    "sG" (λ-interactive (im-ai-gptel-dwim 'new))
    "ta" #'im-ai-gptel-toggle-side-buffer
    "tA" (λ-interactive (im-ai-gptel-toggle-side-buffer 'new)))
  :config
  (add-hook 'gptel-mode-hook #'tab-line-mode))

;; gptel seems more capable than org-ai. Probably going to replace
;; org-ai with this in the long run or they might just simply
;; co-exists where I use org-ai for simple cases and for more complex
;; cases (like tool usage etc.) gptel.
;;
;; - Supports images etc.
;; - Automatically converts markdown responses to org syntax on the fly,
;; - Very easy to add/delete context.
;;
;; Use <file:...> syntax to include media files in the prompt. Should
;; be in a single line.
;;
;; * Commands
;;
;; - ~gptel~                     → Start a dedicated chat buffer.
;; - ~gptel-add~                 → Add to context DWIM
;; - ~gptel-send~                → Send current region as prompt and insert the result.
;; - ~C-u gptel-send~            → Edit stuff before sending prompt.
;;
;; Inside a gptel buffer:
;;
;; - ~C-u C-x RET~ or ~gptel-menu~ → Menu
;;
;; By default, it uses authinfo to get api keys. See my netrc
;; file. The ones with username "apikey" are for gtpel.

(use-package gptel
  :config
  (setq gptel-default-mode #'org-mode)
  (setq gptel-track-media t)
  (setq gptel-prompt-prefix-alist '((org-mode . "-----\n[ME]: ")))
  (setq gptel-response-prefix-alist '((org-mode . "-----\n[AI]: ")))
  ;; As usual, my key is in netrc file.
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key gptel-api-key))

(general-def :keymaps '(gptel-mode-map org-ai-mode-map) :states '(normal motion)
  "C-x [" #'im-ai-previous-block
  "C-x ]" #'im-ai-next-block)

(im-make-repeatable im-ai
  "[" im-ai-previous-block
  "]" im-ai-next-block)

;;;;; tmr.el -- timers, reminders etc.

;; Pretty timers. I forget everything, so it's quite important for me
;; to have a quick way to define timers. I was using ~appt-add~ for
;; this before but it does not let you view/manipulate upcoming timers
;; easily and tmr does this very well with ~tmr-tabulated-view~.

;; - tmr :: To quickly define a timer, without any description.

;; - tmr-with-details :: Define a timer with description, also asks if
;;   you need acknowledgment. Acknowledgment is useful in a sense that
;;   you can re-schedule the timer if you need it. It will ask you to
;;   write ~ack~ when the time is up, if you are not ready, you can
;;   re-schedule the timer with the same notation you use while
;;   creating it which is super convenient.

(use-package tmr
  :defer t
  :autoload (tmr--parse-duration)
  :init
  (im-leader "T" #'tmr-with-details)
  :config
  ;; Replace the notification function so that it works on my Mac
  (remove-hook 'tmr-timer-finished-functions #'tmr-notification-notify)

  ;; Acknowledge using the GUI dialog as it requires mouse which makes
  ;; me more conscious (and also works when emacs is unfocued)
  (remove-hook 'tmr-timer-finished-functions #'tmr-acknowledge-minibuffer)
  (add-hook 'tmr-timer-finished-functions #'im-tmr-ack 90)

  (add-hook 'tmr-timer-finished-functions #'im-tmr-notify)
  (when (eq system-type 'darwin)
    (setq tmr-sound-file "/System/Library/Sounds/Glass.aiff"))

  ;; evilify
  (define-key tmr-tabulated-mode-map "j" #'next-line)
  (define-key tmr-tabulated-mode-map "k" #'previous-line)
  (define-key tmr-tabulated-mode-map "x" #'tmr-remove)
  (define-key tmr-tabulated-mode-map "d" #'tmr-remove)
  (evil-set-initial-state 'tmr-tabulated-mode 'emacs)

  ;; Show upcoming timers in global-mode-string
  (setq global-mode-string (append global-mode-string '(im-tmr-upcoming-string)))
  (run-with-timer 1 15 #'im-tmr-format-upcoming))

(defun im-tmr-ack (timer)
  (when (tmr--timer-acknowledgep timer)
    (im-force-focus-emacs)
    ;; Doing jk escapes the acknowledge dialog, so I disable it here
    (let ((evil-escape-inhibit t))
      (tmr-acknowledge-minibuffer timer))))

(defun im-tmr-notify (timer)
  (alert
   (tmr--long-description-for-finished-timer timer)
   :title "TMR"
   ;; TMR events are high priority for me
   :severity 'high))

(defvar im-tmr-upcoming-string nil)
(put 'im-tmr-upcoming-string 'risky-local-variable t) ;; This is required to make propertize work

(defun im-tmr-format-upcoming ()
  (setq
   im-tmr-upcoming-string
   (-some->>
       tmr--timers
     (--filter (and (not (tmr--timer-finishedp it))
                    (let ((remaining (- (float-time (tmr--timer-end-date it))
                                        (float-time))))
                      (<= remaining 300))))
     (--map (format "%s (%s)"
                    (propertize (tmr--timer-description it) 'face 'underline)
                    (propertize (tmr--format-remaining it) 'face 'italic)))
     (s-join ", ")
     (s-prepend " «")
     (s-append "» ")))
  (force-mode-line-update t))

;;;;; lab.el -- gitlab integration

(use-package lab
  :straight (:host github :repo "isamert/lab.el")
  :autoload (lab--git lab--time-ago lab-git-clone lab-current-branch lab-git-origin-switch-to-ssh)
  :init
  ;; Add advices for automatically starting to watch pipelines
  (with-eval-after-load 'magit
    (define-advice magit-push-current-to-pushremote (:after (&rest _) start-watching-pipeline)
      (lab-watch-pipeline-for-last-commit)))
  (with-eval-after-load 'vc
    (define-advice vc-push (:after (&rest _) start-watching-pipeline)
      (lab-watch-pipeline-for-last-commit)))
  :config
  ;; Automatically refresh projects list after cloning a project
  (add-hook 'lab-after-git-clone-functions #'im-load-projects-list)
  ;; And jump to that project
  (add-hook 'lab-after-git-clone-functions (lambda () (dired default-directory)))

  (setq lab-projects-directory im-projects-root)
  (setq lab-pipeline-watcher-initial-delay 30)
  (setq lab-pipeline-watcher-debounce-time 15)
  (setq lab-host ty-gitlab-url)
  (setq lab-token ty-gitlab-token)
  (setq lab-group ty-gitlab-group)
  (setq lab-should-open-pipeline-on-manual-action? t)
  (add-hook 'midnight-hook #'im-update-all-projects))

(defun im-update-all-projects ()
  (lab-pull-bulk (lab-all-project-roots im-projects-root))
  ;; Can't get all group projects async right now
  ;; (lab-get-all-group-projects-async :on-success ))
  )

;; I also have an experimental GitHub version, tailored for more
;; open-source work needs instead of regular stuff.

(use-package im-github
  :straight nil
  :custom
  (lab-github-user "isamert")
  (lab-github-token im-github-token)
  (lab-github-open-issues-projects-blacklist '("isamert.github.io" "addalias" "gedi" "gracer" "scli"))
  :init
  (add-to-list 'im-open-thing-at-point-alist '(lab-github-issue-at-point . lab-github-open-issue)))

;;;;; swagg.el -- Swagger UI

(use-package swagg
  :straight (:host github :repo "isamert/swagg.el")
  :general
  (im-leader
    "us" #'swagg-request-with-rest-block
    "uS" #'swagg-request)
  :config
  (setq swagg-rest-block-org-header-tags "verb")
  (setq swagg-fetch-lang "deno :allow net"))

;;;;; whisper -- Recording and transcribing audio locally

(use-package whisper
  :straight (:host github :repo "natrys/whisper.el")
  :bind ("C-M-r" . whisper-run)
  :config
  ;; Override `whisper-command' as instructed, to be able to use my
  ;; whisper server installation on my home lab.
  (setq whisper-install-whispercpp nil) ; Opt-out from local whisper installation
  (defun whisper-command (input-file)
    ;; No support for `whisper-translate',
    ;; `whisper-show-progress-in-mode-line' and `whisper-language'.
    `("curl"
      ,(concat im-server "/whisper/inference")
      "-H" ,(concat "isamert-token: " im-token)
      "-H" "Content-Type: multipart/form-data"
      "-F" ,(concat "file=@" (expand-file-name input-file))
      "-F" "response_format=text")))

(define-advice whisper-run (:before (&rest _) set-microphone)
  (unless whisper--ffmpeg-input-device
    (im-whisper-select-microphone)))

(defun im-whisper-select-microphone ()
  (interactive)
  (setq
   whisper--ffmpeg-input-device
   (->>
    (im-output-select
     :prompt "Select mic: "
     :cmd "/opt/homebrew/bin/ffmpeg -f avfoundation -list_devices true -i ''"
     :keep-order t
     :filter (s-prefix? "[AVFound" it))
    (s-match "\\[\\([0-9]+\\)\\]")
    (nth 1)
    (s-prepend ":"))))

;;;;; osm -- OpenStreetMaps in Emacs

;; Very cool and the nice thing is it integrates itself with the built-in bookmarking system. So you can bookmark places (or store them as org links) and jump to them whenever needed.

(use-package osm
  :general
  ;; TODO: Maybe add this to evil-collection
  (:keymaps 'osm-mode-map :states 'normal
   ;; Navigation
   "h" #'osm-left
   "l" #'osm-right
   "j" #'osm-down
   "k" #'osm-up
   "H" #'osm-left-left
   "L" #'osm-right-right
   "J" #'osm-down-down
   "K" #'osm-up-up
   ;; Positioning
   "+" #'osm-zoom-in
   "-" #'osm-zoom-out
   "c" #'osm-center
   "g"  #'osm-home
   "r" #'revert-buffer
   ;; Url
   "u" #'osm-save-url
   "y" #'org-store-link
   ;; Other
   "s" #'osm-search
   "X" #'osm-gpx-hide
   "q" #'quit-window)
  :custom
  (osm-server 'default)
  (osm-copyright nil)
  :init
  (add-hook 'osm-mode-hook #'im-disable-line-wrapping))

;;;;; reveal-in-finder

;; Well, sometimes you need to open that pesky program.

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin))

;;;;; vundo -- unto-tree like branching undo

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package vundo
  :general
  (:states 'normal
   "U" #'vundo)
  (:states 'normal :keymaps 'vundo-mode-map
   "q" #'vundo-quit
   "RET" #'vundo-confirm
   "d" #'vundo-diff
   "h" #'vundo-backward
   "l" #'vundo-forward
   "k" #'vundo-previous
   "j" #'vundo-next)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 20)
  (vundo-glyph-alist vundo-unicode-symbols)
  :config
  (evil-set-initial-state 'vundo-mode 'normal))

(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :after undo-fu
  :demand t
  :custom
  (undo-fu-session-compression 'zst)
  :config
  (global-undo-fu-session-mode 1))

;;;;; deadgrep

;; ~consult-ripgrep~ is nice but having a dedicated search buffer
;; where I can tinker with options is sometimes better

(use-package deadgrep
  :custom
  (deadgrep-display-buffer-function #'switch-to-buffer)
  (deadgrep-project-root-function #'im-current-project-root)
  ;; Normally =--multiline= has memory and performance penalty and not
  ;; recommended for every search but I normally use =consult-ripgrep=
  ;; and if I'm using deadgrep than this means shit got serious and I
  ;; need the advanced stuff.
  (deadgrep-extra-arguments '("--no-config" "--multiline"))
  :general
  (:keymaps 'deadgrep-mode-map :states 'normal
   "RET" #'deadgrep-visit-result
   "o" #'deadgrep-visit-result-other-window
   "TAB" #'deadgrep-toggle-file-results
   "S" #'deadgrep-search-term
   "T" #'deadgrep-cycle-search-type
   "C" #'deadgrep-cycle-search-case
   "F" #'deadgrep-cycle-files
   "D" #'deadgrep-directory
   "^" #'deadgrep-parent-directory
   "r" #'deadgrep-restart
   "I" #'deadgrep-incremental
   "i" #'deadgrep-edit-mode))

;;;;; consult-gh (GitHub interface for consult)

(use-package consult-gh
  :straight (:host github :repo "armindarvish/consult-gh")
  :after consult
  :defer t
  :custom
  ;; Previews are triggered with `consult-preview-key'
  (consult-gh-show-preview t)
  (consult-gh-issues-state-to-show "all")
  (consult-gh-issue-maxnum 150)
  (consult-gh-repo-maxnum 150)
  (consult-gh-pr-maxnum 150)
  (consult-gh-code-maxnum 50)
  (consult-gh-default-orgs-list '("isamert"))
  (consult-gh-default-clone-directory "~/Workspace/temp")
  (consult-gh-file-action #'consult-gh--files-view-action)
  (consult-gh-issue-action
   (lambda (it)
     (browse-url (format "https://github.com/%s/issues/%s" (plist-get (cdr it) :repo) (plist-get (cdr it) :issue)))))
  :config
  (setq
   consult-gh-repo-action
   (lambda (it)
     (let ((repo (car (s-split " " (s-trim (substring-no-properties it))))))
       (empv--select-action "Action: "
         "View README" => (browse-url (format "https://github.com/%s.git" repo))
         "Files" => (consult-gh--repo-browse-files-action it)
         "Issues" => (consult-gh-issue-list repo)
         "Clone" => (lab-git-clone
                     (format "https://github.com/%s.git" repo)
                     (read-directory-name "Directory to clone in: " lab-projects-directory))))))
  ;; Some aliases so that all GitHub functionality is under same prefix
  (defalias 'lab-github-search-repo #'consult-gh-search-repos)
  (defalias 'lab-github-search-code #'consult-gh-search-code))

;;;;; copilot

;; - Do ~copilot-login~ to activate.
;; - Turn on ~copilot-mode~ to use it.

;; [2024-04-12 Fri 18:28] I don't use it anymore. Tried it for one
;; month and decided that it's counter-productive. Utilizing org-ai is
;; much better when needed.

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :general
;;   (:keymaps 'copilot-completion-map
;;    "<right>" #'copilot-accept-completion
;;    "<left>" #'copilot-cancel-completion
;;    "<up>" #'copilot-previous-completion
;;    "<down>" #'copilot-next-completion)
;;   (:modes 'insert
;;    "M-o z" #'copilot-complete))

;;;;; artist-mode

;; Here I simply add some bindings form artist-mode and a cheatsheet in the header line.

(add-hook
 'artist-mode-hook
 #'im-artist-mode-hook)

(defun im-artist-mode-hook ()
  (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
  (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
  (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
  (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
  (setq-local header-line-format "f2 → pen, f3 → line, f4 → square, f5 → ellipse"))

;;;;; biome -- weather information

(use-package biome
  :straight (:host github :repo "SqrtMinusOne/biome")
  :defer t
  ;; These commands are generated in the :config section.
  :commands (im-biome-weather-daily-ankara
             im-biome-weather-daily-amsterdam
             im-biome-weather-daily-istanbul
             im-biome-weather-hourly-ankara
             im-biome-weather-hourly-amsterdam
             im-biome-weather-hourly-istanbul)
  :custom
  ;; See README, this fixes request--callback: peculiar error issue.
  (biome-api-try-parse-error-as-response t)
  (biome-query-coords
   `(("Amsterdam, Netherlands" ,im-amsterdam-lat ,im-amsterdam-long)
     ("Ankara, Turkey" ,im-ankara-lat ,im-ankara-long)
     ("Istanbul, Turkey" ,im-istanbul-lat ,im-istanbul-long)))
  (biome-grid-highlight-current t)
  (biome-query-override-column-names
   '(("apparent_temperature_max" . "Max feel")
     ("apparent_temperature_min" . "Min feel")
     ("temperature_2m_min" . "Min temp")
     ("temperature_2m_max" . "Max temp")
     ("precipitation_probability_max" . "🌧️ %%")
     ("precipitation_probability" . "🌧️ %%")
     ("precipitation_hours" . "🌧️ hours")
     ("precipitation_sum" . "🌧️ sum")
     ("precipitation" . "🌧️ ")
     ("relativehumidity_2m" . "Humid")
     ("weathercode" . " Code")
     ("is_day" . "Zone     ")
     ("shortwave_radiation_sum" . "Radiation")
     ("time" . "📅 ")))
  :config
  ;; Generate a preset for each city in biome-query-coords
  (--each (map-keys biome-query-coords)
    (eval
     `(biome-def-preset ,(intern (concat "im-biome-weather-daily-" (downcase (car (s-split ", " it)))))
        ((:name . "Weather Forecast")
         (:group . "daily")
         (:params
          ("daily" "shortwave_radiation_sum" "uv_index_clear_sky_max" "uv_index_max" "precipitation_probability_max"
           "precipitation_hours" "precipitation_sum" "sunset" "sunrise" "apparent_temperature_min" "apparent_temperature_max"
           "temperature_2m_min" "temperature_2m_max" "weathercode")
          ("latitude" . ,(car (map-elt biome-query-coords it)))
          ("longitude" . ,(cadr (map-elt biome-query-coords it))))))))

  (--each (map-keys biome-query-coords)
    (eval
     `(biome-def-preset ,(intern (concat "im-biome-weather-hourly-" (downcase (car (s-split ", " it)))))
        ((:name . "Weather Forecast")
         (:group . "hourly")
         (:params
          ("hourly" "uv_index_clear_sky" "uv_index" "relativehumidity_2m" "precipitation"
           "precipitation_probability" "apparent_temperature" "temperature_2m" "is_day"  "weathercode")
          ("latitude" . ,(car (map-elt biome-query-coords it)))
          ("longitude" . ,(cadr (map-elt biome-query-coords it)))))))))

;;;;; outli -- outlier for code files

;; I migrated my .org config into a single elisp file, thanks to
;; outli. This way it is easier to maintain and I get the use some of
;; the org features in my configuration.

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

  ;; Here are some org-mode like bindings for outli:
  (:keymaps 'outli-mode-map :states 'normal
   "gh" #'outline-up-heading
   "gk" #'outline-backward-same-level
   "gj" #'outline-forward-same-level)
  :config
  ;; Add h as narrow prefix for headings in consult-imenu
  (with-eval-after-load 'consult-imenu
    (push '(?h "Headings") (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))))

;;;;; epkg

;; Browse elisp packages inside Emacs. This is useful as I am not
;; using package.el and don't want to load it. Use
;; `epkg-list-packages'.

(use-package epkg
  :defer t)

;;;;; upver -- update dependencies interactively

(use-package upver
  :commands (upver)
  :straight (:host github :repo "isamert/upver.el"))

;;;;; notmuch & message & sendmail -- email stuff

(use-package ol-notmuch
  :after notmuch)

;; Easy to use, fast email client.  Tagging feature is something I am
;; familiar from elfeed, and notmuch executes this concept very
;; well. A pleasure to use.

;; Summary of what is going on:
;;
;; - `mbsync' (or `isync') is used for fetching mail from remote mail
;;   server to local.
;;   - The configuration is kept under =~/.mbsyncrc=
;;   - This needs to be run periodically to sync mail.  See my
;;     `im-sync-mail' function.
;; - `notmuch' is the mail indexer which essentially lets you search
;;   your mail.
;;   - The configuration is kept under =~/.notmuch-config=
;;   - It is an external program, that works on your local maildir
;;     (which is created by the `mbsync')
;;   - It needs to be run after each update to index your mail.  See
;;     my `im-sync-mail' function.
;;   - It is also an Emacs package which let's you view your email and
;;     interact with them.
;; - `msmtp' is the SMTP client that let's you send your mails.
;;   - The configuration is kept under =~/.msmtprc=.
;;   - See `sendmail' and `message' configuration down below for Emacs
;;     integration.
;;
;; See [[../index.org]] for relevant configuration files.
;;
;; Great video, explaining notmuch and the workflow by prot:
;; https://youtube.com/watch?v=g7iF11qamh8
;;
;; For gmail configuration, following is helpful:
;; https://frostyx.cz/posts/synchronize-your-2fa-gmail-with-mbsync#gmail-with-2fa
;;
;; Another helpful and easy to follow guide:
;; https://jonathanchu.is/posts/emacs-notmuch-isync-msmtp-setup/
;;
;; Most of the customizations down below are inspired by prot's configuration:
;; https://protesilaos.com/emacs/dotemacs

;; Good time to set these values:
(setq user-full-name "Isa Mert Gurbuz")
(setq user-mail-address "isamertgurbuz@gmail.com")

(use-package notmuch
  :defer t
  :custom
  (notmuch-show-logo nil)
  (notmuch-hello-auto-refresh t)
  (notmuch-show-all-tags-list t)
  (notmuch-show-empty-saved-searches t)
  (notmuch-show-relative-dates t)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-saved-searches
   '((:name "📥 inbox" :query "tag:inbox and not tag:deleted" :key "i")
     (:name "💬 unread (inbox)" :query "tag:unread and tag:inbox" :key "u")
     (:name "🏳️ flagged" :query "tag:flagged" :key "f")
     (:name "✍🏻 sent" :query "tag:sent" :key "t")
     (:name "📰 drafts" :query "tag:draft" :key "d")
     (:name "💭 all mail" :query "*" :key "a")))
  (notmuch-mua-hidden-headers nil)
  (notmuch-always-prompt-for-sender t)
  ;; Automatically move sent mails to Sent folder of corresponding
  ;; account and add/remove relevant tags
  (notmuch-fcc-dirs `(("isamertgurbuz@gmail.com" . "gmail/Sent +sent -new -inbox -unread")))
  :hook
  ;; Check if attachments are really attached before sending
  ;; Also see `notmuch-mua-attachment-regexp'
  (notmuch-mua-send . notmuch-mua-attachment-check)
  :general
  (:keymaps 'notmuch-search-mode-map :states 'normal
   ;; gr → Refresh view
   ;; C → compose mail
   "u" #'evil-collection-notmuch-search-toggle-unread
   "gs" #'notmuch-search
   "gS" #'notmuch-tree
   "o" #'notmuch-search-show-thread)
  (:keymaps 'notmuch-show-mode-map :states 'normal
   "o" #'notmuch-show-view-part
   "O" #'notmuch-show-save-part)
  (im-leader
    "en" #'im-notmuch-inbox
    "eN" #'notmuch-hello)
  :config
  (setq-default notmuch-search-oldest-first nil)
  (evil-collection-notmuch-setup)
  ;; I use the s key for another thing
  (evil-collection-define-key 'normal 'notmuch-common-keymap
    "s" nil
    "S" nil))

(use-package message
  :straight (:type built-in)
  :defer t
  :hook
  (message-setup . message-sort-headers)
  :custom
  (mail-user-agent 'message-user-agent)
  (message-mail-user-agent t)
  (message-elide-ellipsis "\n> [... %l lines elided]\n")
  (compose-mail-user-agent-warnings nil)
  (mail-signature "Isa Mert Gurbuz\nhttps://isamert.net\n")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-confirm-send nil)
  (message-kill-buffer-on-exit t))

(use-package sendmail
  :straight (:type built-in)
  :after message
  :config
  (setq send-mail-function 'sendmail-send-it)
  (setq sendmail-program (executable-find "msmtp"))
  (setq message-sendmail-envelope-from 'header))

(defun im-notmuch-inbox ()
  "Open Inbox directly."
  (interactive)
  (notmuch-search (plist-get (car notmuch-saved-searches) :query)))

;;;;;; Sync mail

(defvar im-unread-mail-count 0)
(async-defun im-sync-mail (&optional interactive?)
  "Run mbsync, update notmuch index and check new message count.

This could be a cron job but keeping it in Emacs gives me the
opportunity to notify the new email count right after fetching all
mails."
  (interactive (list t))
  (when (im-check-internet-connection)
    (condition-case reason
        (let (count)
          (when interactive?
            (message ">> Checking mail..."))
          (await (im-shell-command :command "mbsync --all --verbose" :async t))
          (await (im-shell-command :command "notmuch new" :async t))
          (await (im-shell-command :command "notmuch tag +work -- not tag:work and folder:work/INBOX" :async t))
          (setq
           count
           (string-to-number
            (await (im-shell-command :command "notmuch count tag:inbox and tag:unread" :async t))))
          (when interactive?
            (message "You have %s new mail." count))
          (when (and (> count 0) (not (eq im-unread-mail-count count)))
            (setq im-unread-mail-count count)
            (alert (format "You have %s new mail!" count)
                   :title "New Mail!")))
      (error (alert (format "Exit code: %s. See buffers *notmuch* and *mbsync*." reason)
                    :title "Checking for mail failed!")))))

(run-with-timer
 60
 (* 15 60)
 #'im-sync-mail)

;;;;;; Mail auto completion for my contacts

;; With the following, "M-o m" completes email addresses of my
;; contacts.

(im-cape
 :name people-emails
 :completion
 (lambda ()
   (->>
    (im-deserialize-from-file "~/.emacs.d/sync/people")
    (-map
     (lambda (entry)
       (let ((mails (--filter (s-prefix? "EMAIL" (car it)) entry)))
         (--map
          (let ((type (s-titleize (s-chop-prefix "_" (s-chop-prefix "EMAIL" (car it))))))
            (list
             (cdr it)
             ;; annotation
             (concat (if (s-blank? type) "" (concat type " of " )) (map-elt entry "ITEM"))
             ;; doc
             entry))
          mails))))
    (-flatten-n 1)
    (-non-nil)))
 :extractor (lambda (xs) (mapcar #'car xs))
 :bound symbol
 :category symbol
 :key "M-o m"
 :doc (lambda (xs x) (pp-to-string (nth 1 (map-elt xs x))))
 :annotate (lambda (xs x) (concat " -> " (nth 0 (map-elt xs x)))))

;;;;; easysession.el -- session manager

;; Simple package to save/retrieve Emacs sessions. It works well with
;; tab-bar. Currently my workflow is:

;; `easysession-save-as' :: Save a named session.
;; `easysession-switch-to' :: Load a session. Usually called after starting Emacs.

;; By default, all file visiting buffers, dired buffers, and indirect
;; buffers are persisted and restored.

(use-package easysession
  :straight (:host github :repo "jamescherti/easysession.el"))

;;;;; im-git -- my git workflow, magit alternative

(use-package im-git
  :straight nil
  :init
  (add-hook 'im-git-commit-finished-hook #'im-update-git-state))

(transient-define-prefix im-git-link-transient ()
  )

;; To invoke: (im-git-link-transient)


(transient-define-prefix im-git-transient ()
  [["Status"
    ("s" "Show status" im-git-status)
    ("B" "Blame annotate" vc-annotate)
    ("i" "Show blame at point" blamer-show-posframe-commit-info)
    ("t" "Open timemachine" git-timemachine-toggle)
    ("R" "Refresh state" im-update-git-state)]
   ["Commit/Branch"
    ("c" "Commit changes" im-git-commit)
    ("a" "Amend last commit" im-git-commit-amend)
    ("bc" "New branch" vc-create-branch)
    ("bs" "Switch branch" vc-switch-branch)
    ("P" "Push" vc-push)
    ("p" "Pull" vc-pull)]
   ["Log"
    ("Lr" "Repo log" vc-print-root-log)
    ("Lf" "File log" vc-print-log)]
   ["Stash"
    ("E" "Stash changes" vc-git-stash)
    ("e" "Show stashes" im-git-list-stash)]
   ["Worktrees"
    ("wa" "Add worktree" im-git-worktree-add)
    ("ws" "Switch worktree" im-git-worktree-switch)
    ("wd" "Remove worktree" im-git-worktree-delete)]
   ["Diff"
    ("[" "Prev hunk" diff-hl-previous-hunk)
    ("]" "Next hunk" diff-hl-next-hunk)
    ("{" "Show prev hunk" diff-hl-show-hunk-previous)
    ("}" "Show next hunk" diff-hl-show-hunk-next)
    ("S" "Stage hunk" diff-hl-stage-dwim)
    ("r" "Revert hunk" diff-hl-revert-hunk)]
   ["Merge Requests (GitLab)"
    ("mb" "Branches" lab-list-branch-merge-requests)
    ("mm" "Assigned to me" lab-list-my-merge-requests)
    ("ma" "In group" lab-list-group-merge-requests)
    ("mp" "In project" lab-list-project-merge-requests)
    ("mc" "Create" lab-create-merge-request)]
   ["Git Link"
    ("lm" "Merge Requests" im-git-link-merge-requests)
    ("ll" "On Branch" im-git-link-on-branch)
    ("lL" "Commit hash" im-git-link-commit)
    ("lc" "Commit (hash at point)" git-link-commit)
    ("lh" "Homepage" git-link-homepage)]])

(im-leader-v "g" #'im-git-transient)

;;;;; im-shiori -- Shiori bookmark manager integration

(use-package im-shiori
  :straight nil
  :custom
  (im-shiori-url im-homeserver-shiori-url)
  (im-shiori-username im-homeserver-username)
  (im-shiori-password im-homeserver-password)
  (im-shiori-elfeed-tags '(later bookmark shiori))
  :init
  (with-eval-after-load 'elfeed
    (im-shiori-enable-elfeed-support)))

;;;;; im-tab --- my tab related extensions

(use-package im-tab
  :straight nil
  :general
  (im-leader
    ;; Save current tab window configuration to a key
    "ww2" #'im-tab-configuration-save-current
    "ww3" #'im-tab-configuration-save-current
    "ww4" #'im-tab-configuration-save-current
    "ww5" #'im-tab-configuration-save-current
    ;; Restore a saved configuration with a key
    "w2" #'im-tab-configuration-restore-current
    "w3" #'im-tab-configuration-restore-current
    "w4" #'im-tab-configuration-restore-current
    "w5" #'im-tab-configuration-restore-current))

;;;;; im-filebrowser -- Filebrowser integration

(use-package im-filebrowser
  :straight nil
  :commands (im-filebrowser-share)
  :custom
  (im-filebrowser-url im-homeserver-filebrowser-url)
  (im-filebrowser-username im-homeserver-username)
  (im-filebrowser-password im-homeserver-password)
  (im-filebrowser-base-path "/ssh:files:"))

;;;;; im-kube --- my kubernetes interface

(use-package im-kube
  :straight nil)

;;;;; im-nextcloud --- my nextcloud integration

(use-package im-nextcloud
  :straight nil
  :demand t
  :custom
  (im-nextcloud-url im-secrets-nextcloud-url)
  (im-nextcloud-user im-secrets-nextcloud-user)
  (im-nextcloud-auth im-secrets-nextcloud-auth))

;;;;; ereader --- ebook reader with org integration

(use-package ereader
  :straight (:host github :repo "bddean/emacs-ereader")
  :after (org)
  :config
  ;; For org links integration
  (require 'org-ebook)
  (defun im-ereader-setup ()
    (interactive)
    ;; Otherwise it gets weird due to `variable-pitch-mode'
    (setq-local page-break-lines-max-width 80)
    (page-break-lines-mode)
    (variable-pitch-mode))
  (add-hook 'ereader-mode-hook 'im-ereader-setup))

;;;;; pdf-tools --- advanced pdf viewer

(use-package pdf-tools
  :general
  ;; P → Fit to page
  ;; W → Fit to window
  (:states '(normal visual) :keymaps 'pdf-view-mode-map
   "J" #'pdf-view-next-page-command
   "K" #'pdf-view-previous-page-command
   "s" 'pdf-occur)
  ;; :init
  ;; (pdf-tools-install)
  :config
  (defun im-pdf-view-setup ()
    (hl-line-mode -1))
  (evil-collection-pdf-setup)
  (add-hook 'pdf-view-mode #'im-pdf-view-setup))

;;;; Editing

;;;;; Breaking long texts/comments into multiple lines

;; I use =M-q= (=fill-paragraph=) to break long texts into multiple
;; lines. It also works well within comment sections. 80 col length is
;; quite readable. See how this item is formatted, it's done
;; automatically by the usage of =M-q=.

(setq fill-column 80)

;;;;; synosaurus & wordnut

;; Both synosaurus and wordnut uses ~wordnet~ to work. Use
;; ~synosaurus-choose-and-replace~ to replace the current word with
;; one of it's synonyms.

(use-package synosaurus
  :commands synosaurus-choose-and-replace
  :general
  (im-leader "mr" #'synosaurus-choose-and-replace)
  :custom
  (setq synosaurus-choose-method nil))

(use-package wordnut
  :defer t)

;;;;; sozluk.el -- Turkish dictionary

(use-package sozluk
  :straight (:host github :repo "isamert/sozluk.el")
  :defer t
  :custom
  ;; (sozluk-include-etymology-on-sozluk t)
  (sozluk-deasciify-if-not-found t))

;; This is an optional dependency for sozluk.
(use-package turkish
  :general
  (im-leader-v "tc" #'turkish-correct-region))

;;;;; jinx -- spellchecker, flyspell alternative

;; I was using ~flyspell~ and ~flyspell-correct~ before but Jinx works
;; much better & faster out-of-the-box. It automatically works nicely
;; in code buffers too!
;;
;; You need to install ~enchant~ to make Jinx work (and compile).
;;
;; [2024-08-24 Sat 03:30] Recently I have been having random crashes,
;; and it turns out (after a very long session of debugging) the reason
;; is enchant. enchant SEGFAULTs when using Apple Spell, so what I did
;; is:
;;
;; #+begin_src
;;   rm /opt/homebrew/Cellar/enchant/2.8.2/share/enchant-2/AppleSpell.config
;;   rm /opt/homebrew/Cellar/enchant/2.8.2/lib/enchant-2/enchant_applespell.so
;;   rm /opt/homebrew/Cellar/enchant/2.8.2/lib/enchant-2/enchant_applespell.a
;; #+end_src
;;
;; Seems like other people also had this issue and resolved
;; differently, like building enchant from source etc:
;;
;; - https://github.com/rrthomas/enchant/issues/391
;; - https://github.com/pyenchant/pyenchant/issues/321
;; - https://old.reddit.com/r/emacs/comments/1eg9hdg/multilingual_spellchecking_omg_what_a_rabbit_hole/

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom (jinx-languages "en_US tr_TR")
  :general
  (:keymaps 'evil-normal-state-map
   "z=" #'jinx-correct-word
   ;; Override flyspell bindings from evil-commands with jinx ones
   "[s" #'jinx-previous
   "]s" #'jinx-next)
  (:keymaps 'evil-visual-state-map
   "z=" #'jinx-correct-word)
  :config
  (add-to-list 'global-jinx-modes 'slack-message-compose-buffer-mode))

;;;;; puni & combobulate & electric-pair-mode (structrual editing stuff)

;; Using `electric-pair-mode' to automatically pair parenthesis.

(add-hook 'after-init-hook #'electric-pair-mode)

;; `puni' provides structral editing for a lot of languages using some
;; Emacs built-ins.  It's pretty good most of the time and I really
;; use a very small subset of it's feature set.

(use-package puni
  :straight (:host github :repo "AmaiKinono/puni")
  :general
  (:keymaps 'prog-mode-map :states '(insert)
   "M-[" #'puni-barf-forward
   "M-]" #'puni-slurp-forward
   "M-d" #'puni-splice
   "M-t" #'puni-transpose
   "M-\\" #'puni-split
   "M-(" #'puni-wrap-round
   "M-{" #'puni-wrap-curly)
  (:keymaps 'prog-mode-map :states '(normal)
   "(" #'puni-backward-sexp-or-up-list
   ")" #'puni-forward-sexp-or-up-list))

;; `combobulate' provides structural editing for some languages using
;; treesit.el, here are the bindings that I find useful (which are
;; enabled by `combobulate-mode'):

;; - M-P :: combobulate-drag-up
;; - M-N :: combobulate-drag-down

;; I also replace `expand-region' with combobulate equivalent and also
;; re-bind some of the bindings from `puni' with their combobulate
;; equivalent:

;; - M-w :: combobulate-mark-node-dwim
;; - M-t :: combobulate-transpose-sexps

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode))
  :general
  (:keymaps 'combobulate-key-map :states '(normal insert)
   "M-w" #'combobulate-mark-node-dwim)
  (:keymaps 'combobulate-key-map :states '(insert)
   "M-t" #'combobulate-transpose-sexps)
  :custom
  ;; Numeric selection is confusing
  (combobulate-proffer-allow-numeric-selection nil)
  :config
  (define-advice combobulate-mark-node-dwim (:before (&rest _) visual-mode)
    "Activate visual mode before marking."
    (evil-visual-char)))

;; The reason I use a very little subset of these packages is that I
;; already use evil-mode and it's editing capabilities cover most of
;; the feature set provided by puni and combobulate.

;;;;; writeroom-mode

;; Gives you a nice, uncluttered editing experience by removing all
;; unneeded visual clutter and by justifying the text in the middle.

(use-package writeroom-mode
  :commands writeroom-mode
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-global-effects nil)
  (setq writeroom-mode-line-toggle-position 'header-line-format)
  (setq writeroom-width 81))


;;;; Dummy IDE mode

;; I try to use ~lsp-mode~ and other language-specific packages for
;; the languages I use (see [[Language specific]]), but sometimes
;; either they are too slow or the computer I'm currently working on
;; requires some extra setup or I just don't want to use them for some
;; reason. For those cases, I use a collection of packages that gives
;; you the power of IDEs but in some dummy/restricted way.

;; - <<highlight-thing>> :: Automatically highlights the all instances of the symbol under the cursor in the buffer. Simply use evils ~*~ and ~#~ to jump between them.
;; - <<dumb-jump>> :: Jumps to definition by using predefined-regexps, generally works fine. Use =gd=.
;; - To debug why it's not working: M-x ~set-variable dumb-jump-debug t~, then go to *Messages* buffer.
;; - <<treesit>> :: This is a generic parser for bunch of languages. You can also inspect the syntax tree on the fly and do whatever you want to do with it. Best feature so far is just better (like, miles ahead better) syntax highlighting for some languages. Especially for JS/TS and Rust.
;; - <<hl-todo>> :: Highlight TODO/FIXME etc.
;; - Use ~]t~ and ~[t~ to go next/prev TODO/FIXME item.

(use-package dumb-jump
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)
  :config
  ;; ag is supported by nearly every rule but rg is not.
  ;; also see: https://github.com/jacktasia/dumb-jump/issues/376
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-ignore-context nil)
  (setq dumb-jump-fallback-search t)

  (remove-hook 'xref-backend-functions #'etags--xref-backend))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :bind ( :map evil-normal-state-map
          ("[t" . hl-todo-previous)
          ("]t" . hl-todo-next))
  :config
  (setq hl-todo-keyword-faces '(("TODO"   . "#FF0000")
                                ("FIXME"  . "#FF0000")
                                ("DEBUG"  . "#A020F0")
                                ("GOTCHA" . "#FF4500")
                                ("STUB"   . "#1E90FF")))
  (define-advice hl-todo-next (:after (&rest _) reveal) (reveal-post-command))
  (define-advice hl-todo-previous (:after (&rest _) reveal) (reveal-post-command)))

;;;;; Highlight thing at point manually

(setq
 hi-lock-face-defaults
 '("hi-salmon" "hi-aquamarine" "hi-blue" "hi-yellow" "hi-pink" "hi-green" "hi-black-b" "hi-blue-b" "hi-red-b" "hi-green-b" "hi-black-hb"))

(defun im-highlight-thing-at-point-dwim ()
  "Hightlight or unhighlight current symbol or selection."
  (interactive)
  (cond
   ((--any?
     (-contains? hi-lock-face-defaults
                 (symbol-name it))
     (-flatten (list (plist-get (text-properties-at (point)) 'face))))
    (unhighlight-regexp
     (if (use-region-p)
         (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))
       (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp)))))
   ((use-region-p)
    (highlight-regexp
     (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark))
   ((thing-at-point 'symbol)
    (hi-lock-face-symbol-at-point))
   (t (unhighlight-regexp t))))

(evil-define-key 'normal 'global
  (kbd "M-h") #'im-highlight-thing-at-point-dwim
  (kbd "M-H") (λ-interactive (unhighlight-regexp t)))


;;;; Media/feed/IRC

;; I try to maximize my Emacs usage which brings it's own benefits and
;; downsides which I will not go over here. Here are some packages and
;; configurations that are not related to programming/editing.

;;;;; elfeed (RSS feeds)

;; Feed reader.

;; - Filter examples (after hitting ~s~)
;; - +tag OR -tag (unread is also a tag)
;; - #number-of-entries-limit (like #20)
;; - !inverse-regex (!x?emacs will filter out titles containing x?emacs regex)
;; - =regex (entries that contains the regex will be shown)
;; - +unread +youtube =emacs #10 @5-months-ago

(use-package elfeed
  :commands (elfeed im-elfeed-reload-and-open)
  :general
  (im-leader
    "ee" #'im-elfeed-reload-and-open)
  (:keymaps 'elfeed-search-mode-map :states 'normal
   "o" #'elfeed-search-browse-url
   "O" #'im-elfeed-search-browse-url-in-default-browser)
  :config
  (evil-collection-elfeed-setup)

  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "s" nil
    "S" nil
    "gs" 'elfeed-search-live-filter
    "gS" 'elfeed-search-set-filter)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "s" nil
    "S" nil)

  ;; When adding tags, don't add any hierarchical tags like (blog
  ;; blog-software), or (metal metal-black) Just use something like:
  ;; (blog software) and (metal black)
  (require 'im-feeds)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-curl-extra-arguments '("--netrc"))
  (setq elfeed-feeds (mapcar #'im-elfeed--expand im-feeds))

  ;; To apply hooks to all existing entries, use: elfeed-apply-hooks-now
  (im-elfeed-auto-tag-url '(("youtube\\.com" youtube)))
  (im-elfeed-auto-tag-title '(("youtube\\.com" youtube)
                              ("c\\+\\+"  (programming c++))
                              ("python"   (programming python))
                              ("haskell"  (programming haskell)))))

;; TODO: experiment with custom faces
;; (defface elfeed-comic
;;   '((t :foreground "#BFF"))
;;   "Marks comics in Elfeed."
;;   :group 'elfeed)
;;
;; (push '(comic elfeed-comic)
;;       elfeed-search-face-alist)

(defun im-elfeed-search-browse-url-in-default-browser ()
  "Open URL in the default browser."
  (interactive)
  (with-default-browser
   (elfeed-search-browse-url)))

(defun im-elfeed-auto-tag-url (pairs)
  "Takes a list of url-regex and tag-list pairs and adds a new entry hook for each of them."
  (--map
   (add-hook 'elfeed-new-entry-hook
             (elfeed-make-tagger :feed-url (car it)
                                 :add (cdr it)))
   pairs))

(defun im-elfeed-auto-tag-title (pairs)
  "Takes a list of title-regex and tag-list pairs and adds a new entry hook for each of them."
  (--map
   (add-hook 'elfeed-new-entry-hook
             (elfeed-make-tagger :entry-title (car it)
                                 :add (cdr it)))
   pairs))

;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
(defvar youtube-feed-format
  '(("^UC" . "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
    ("^PL" . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    (""    . "https://www.youtube.com/feeds/videos.xml?user=%s")))

(defun im-elfeed--expand (listing)
  "Expand feed URLs depending on their tags."
  (cl-destructuring-bind (url . tags) listing
    (cond
     ((member 'youtube tags)
      (let* ((case-fold-search nil)
             (test (lambda (s r) (string-match-p r s)))
             (format (cl-assoc url youtube-feed-format :test test)))
        (cons (format (cdr format) url) tags)))
     ((member 'reddit tags)
      (cons (format "https://www.reddit.com/r/%s/.rss" url) tags))
     ((member 'gh-release tags)
      (cons (format "https://github.com/%s/releases.atom" url) tags))
     (listing))))

(defun im-elfeed-reload-and-open ()
  "Reload and open elfeed.
Useful if .elfeed directory is freshly syncned."
  (interactive)
  (require 'elfeed)
  (let* ((buffer (get-buffer "*elfeed-search*"))
         (jump? (and buffer (y-or-n-p "Elfeed is already open, do you want to jump without reloading?"))))
    (unless jump?
      (elfeed-db-load))
    (elfeed)
    (unless jump?
      (elfeed-search-update--force))))

;;;;; empv (music/media/radio/youtube management)

;; Manage media and streams through =completing-read=.

(use-package empv
  :straight (:host github :repo "isamert/empv.el")
  :defer t
  :autoload (empv--select-action)
  :general
  (im-leader
    "r" empv-map
    "R" #'empv-hydra/body
    "r4" #'empv-subsonic-search)
  :config
  (require 'im-radio-channels)

  (setq empv-radio-channels im-radio-channels)
  (setq empv-radio-log-file "~/Documents/notes/songs.org")
  (setq empv-base-directory "~/Music/")
  (setq empv-video-dir `("~/Videos" "~/Movies" ,(format "/run/media/%s/BINGUS/Videos" (user-login-name)) ,(format "/run/media/%s/FLOPPA/Videos" (user-login-name))))
  (setq empv-audio-dir `("~/Music" ,(format "/run/media/%s/BINGUS/Music" (user-login-name)) ,(format "/run/media/%s/FLOPPA/Music" (user-login-name))))
  (setq empv-allow-insecure-connections t)
  (setq empv-invidious-instance im-empv-invidious-instance)
  ;; ^ see https://api.invidious.io/
  (setq empv-youtube-use-tabulated-results t)
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo+bestaudio/best[ext=mp4]/best")
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (setq empv-reset-playback-speed-on-quit t)
  (add-hook 'empv-init-hook #'empv-override-quit-key)
  (add-hook 'empv-youtube-results-mode-hook #'im-disable-line-wrapping)

  (setq empv-subsonic-url im-navidrome-server)
  (setq empv-subsonic-username im-navidrome-username)
  (setq empv-subsonic-password im-navidrome-password)

  (evil-make-overriding-map empv-youtube-results-mode-map 'normal)
  (with-eval-after-load 'embark (empv-embark-initialize-extra-actions))
  (with-eval-after-load 'org
    (add-to-list 'org-file-apps '("\\.\\(mp3\\|ogg\\)\\'" . (lambda (path _str) (empv-play-file path))))))

(defun im-export-radio-channels-as-m3u (file)
  "Export radio list into an M3U FILE."
  (interactive
   (list
    (read-file-name
     "Where to save the .m3u file?"
     "~/Documents/sync/"
     "radiolist.m3u")))
  (with-temp-file file
    (->>
     im-radio-channels
     (--map
      (format
       "#EXTINF:0, %s\n%s"
       (car it)
       ;; Replace http:// with icyx://, because VLC on Android can't
       ;; retrieve song name if the stream is on http://
       (if (s-contains? "radcap.ru" (car it))
           (s-replace "http://" "icyx://" (cdr it))
         (cdr it))))
     (--reduce (format "%s\n%s" acc it))
     (s-prepend "#EXTM3U\n")
     (insert))))

;;;;; orgmdb (movies & shows)

;; I have a file called ~watchlist.org~ where I keep list of movies and shows that I watched and going to watch. Here are some packages and functions to deal with them.

(use-package orgmdb
  :straight (:host github :repo "isamert/orgmdb.el")
  :bind (:map evil-normal-state-map ("go" . orgmdb-act))
  :config
  (setq orgmdb-omdb-apikey im-orgmdb-omdb-apikey)
  (setq orgmdb-poster-folder "~/Documents/notes/data/posters")
  (setq orgmdb-fill-property-list '(genre runtime director country imdb-id imdb-link imdb-rating metascore actors poster plot)))

;;;;; rcirc -- simple, built-in irc client

(use-package rcirc
  :straight (:type built-in)
  :custom
  (rcirc-server-alist `(("irc.libera.chat"
                         :port 6697
                         :nick "isamert"
                         :password ,im-libera-chat-password
                         :encryption tls
                         :channels ("#rcirc" "#emacs"))))
  (rcirc-default-nick "isamert")
  (rcirc-default-full-name "Isa Mert Gurbuz")
  (rcirc-prompt (concat
                 (let ((all-the-icons-default-adjust 0))
                   (all-the-icons-faicon "angle-double-right")) "_ "))
  (rcirc-reconnect-delay 15)
  (rcirc-reconnect-attempts 5)
  :config
  (evil-set-initial-state 'rcirc-mode 'normal)
  (add-hook 'rcirc-mode-hook #'im-rcirc-setup-buffer))

(defun im-rcirc-setup-buffer ()
  (setq-local scroll-conservatively 8192)
  (whitespace-mode -1)
  (rcirc-omit-mode +1)
  (rcirc-track-minor-mode +1)
  (turn-on-visual-line-mode))

;;;; Keybindings

;; Keybindings are generally set in-place, following have no context,
;; so they are here.

;;;;; Some general keybindings

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x <escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c <escape>") 'keyboard-escape-quit)

(evil-define-key 'normal prog-mode-map
  "gd" 'xref-find-definitions
  "gr" 'xref-find-references)

(evil-define-key 'normal prog-mode-map (kbd "M-;") 'comment-line)
(evil-define-key 'visual prog-mode-map (kbd "M-;") 'comment-dwim)

(evil-define-key 'normal 'global (kbd "M-d") #'im-kill-this-buffer)

(im-leader "1" (λ-interactive (call-interactively (local-key-binding (kbd "C-c C-c")))))

;;;; Programming languages

;;;;; General/language-agnostic functionality

;;;;;; Jump to beginning/end of a statement

;; Using [w and ]w, I can jump between statements. For

;; TODO: should be able to descend down the tree
(defun im-treesit-end-of-statement ()
  (interactive)
  (treesit-end-of-thing
   (rx (or "local_variable" "return" "if" "for" "with" "try" "method" "lexical" "call")
       (or "_statement" "_declaration" "_definition" "_expression"))))

(defun im-treesit-beginning-of-statement ()
  (interactive)
  (treesit-beginning-of-thing
   (rx (or "local_variable" "return" "if" "for" "with" "try" "method" "lexical" "call")
       (or "_statement" "_declaration" "_definition" "_expression"))))

;; Only tried in java-ts-mode so far
(evil-define-key 'normal java-ts-mode-map "[w" 'im-treesit-beginning-of-statement)
(evil-define-key 'normal java-ts-mode-map "]w" 'im-treesit-end-of-statement)
(evil-define-key 'normal tsx-ts-mode-map "[w" 'im-treesit-beginning-of-statement)
(evil-define-key 'normal tsx-ts-mode-map "]w" 'im-treesit-end-of-statement)
(evil-define-key 'normal typescript-ts-mode-map "[w" 'im-treesit-beginning-of-statement)
(evil-define-key 'normal typescript-ts-mode-map "]w" 'im-treesit-end-of-statement)

(im-make-repeatable im-treesit-statement
  "[" im-treesit-beginning-of-statement
  "]" im-treesit-end-of-statement)

;;;;;; format-all, apheleia -- Format buffers, automatically

;; Use =format-all-buffer= function to format current buffer. Works for any language.

(use-package format-all
  :commands (format-all-buffer format-all-region))

;; /Apheleia/ is like ~format-all~ but works async and automatically
;; formats on buffer save.  I keep both of them because sometimes I
;; need ~format-all-buffer~ and it works with more formatters.

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :init
  (defalias 'im-toggle-auto-code-formatter #'apheleia-mode)

  ;; Disable it inside `emacs-lisp-mode' as I already use
  ;; `aggressive-indent-mode' for that.
  (add-hook 'emacs-lisp-mode-hook (lambda () (apheleia-mode -1))))

;;;;;; Display/get currently focused function name in modeline

(use-package which-function
  :straight (:type built-in)
  :hook (after-init . which-function-mode))

(defun im-disable-which-func ()
  (which-function-mode -1))

;; Generally unnecessary and sometimes causes problems
(add-hook 'org-mode-hook #'im-disable-which-func)

;;;;;; outline-indent-mode -- Code folding based on indendation

;; I generally use the built-in hs-minor-mode but it does not work
;; with all languages. outline-indent is a more generic solution that works
;; fairly well.

;; I used to use origami-mode for the use cases I mentioned but
;; outline-indent is much better and makes more sense while
;; folding. origami was a bit confusing (as the /thing/ itself).

(use-package outline-indent
  :straight (:host github :repo "jamescherti/outline-indent.el")
  :hook
  (python . outline-indent-minor-mode)
  (python-ts . outline-indent-minor-mode)
  (yaml . outline-indent-minor-mode)
  (yaml-ts . outline-indent-minor-mode)
  (docker-compose-mode . outline-indent-minor-mode)
  (sql-mode . outline-indent-minor-mode)
  :custom
  (outline-indent-ellipsis " ▼ "))

;;;;;; REPLs

(use-package im-repl
  :straight nil
  :general
  (im-leader-v
    ";" (general-predicate-dispatch (im-eval-dwim #'eros-eval-last-sexp #'eval-region #'eros-eval-defun)
          (eq major-mode 'java-ts-mode) #'im-repl-eval
          (eq major-mode 'js-ts-mode) #'im-repl-eval
          (eq major-mode 'typescript-ts-mode) #'im-repl-eval
          (eq major-mode 'clojure-mode) (im-eval-dwim #'cider-eval-last-sexp #'cider-eval-region #'cider-eval-defun-at-point)
          (eq major-mode 'lisp-mode) (im-eval-dwim #'slime-eval-last-expression #'slime-eval-region #'slime-eval-region)
          (eq major-mode 'racket-mode) (im-eval-dwim #'racket-eval-last-sexp #'racket-send-region #'racket-send-definition)
          (eq major-mode 'scheme-mode) (im-eval-dwim #'geiser-eval-last-sexp #'geiser-eval-region #'geiser-eval-definition)
          (eq major-mode 'kotlin-mode) (im-eval-dwim #'kotlin-send-line #'kotlin-send-region #'kotlin-send-line))
    "'" (general-predicate-dispatch #'eros-inspect-last-result
          (eq major-mode 'java-ts-mode) #'im-repl-inspect-last-result
          (eq major-mode 'js-ts-mode) #'im-repl-inspect-last-result
          (eq major-mode 'typescript-ts-mode) #'im-repl-inspect-last-result
          (eq major-mode 'clojure-mode) #'cider-inspect-last-result)))

(defun im-eval-dwim (lastf regionf defunf)
  "Generate an interactive function that you can bind to a key.
It calls LASTF, REGIONF or DEFUNF depending on the context.  Only
works for Lispy languages."
  (lambda ()
    (interactive)
    (cond
     ((use-region-p)
      (call-interactively regionf))
     ((or (-contains? '(?\) ?\") (char-before))
          (-contains? '(?\ ?\)) (char-after)))
      (call-interactively lastf))
     (t
      (call-interactively defunf)))))

;;;;; tree-sitter

(use-package treesit
  :straight (:type built-in)
  :hook (after-init . im-install-and-enable-treesit-grammers))

(defun im-install-and-enable-treesit-grammers ()
  "Install and enable grammers defined in `treesit-language-source-alist'.
Only for built-in modes.  Others are registered through `use-package's :mode keyword."
  (require 'treesit)
  (setq treesit-language-source-alist
        '((awk "https://github.com/Beaglefoot/tree-sitter-awk" nil nil nil nil)
          (bash "https://github.com/tree-sitter/tree-sitter-bash" nil nil nil nil)
          (c "https://github.com/tree-sitter/tree-sitter-c" nil nil nil nil)
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" nil nil nil nil)
          (clojure "https://github.com/sogaiu/tree-sitter-clojure" nil nil nil nil)
          (cmake "https://github.com/uyha/tree-sitter-cmake" nil nil nil nil)
          (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp" nil nil nil nil)
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp" nil nil nil nil)
          (css "https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil)
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" nil nil nil nil)
          (go "https://github.com/tree-sitter/tree-sitter-go" nil nil nil nil)
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod" nil nil nil nil)
          (html "https://github.com/tree-sitter/tree-sitter-html" nil nil nil nil)
          (java "https://github.com/tree-sitter/tree-sitter-java" nil nil nil nil)
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src" nil nil)
          (json "https://github.com/tree-sitter/tree-sitter-json" nil nil nil nil)
          (kotlin "https://github.com/fwcd/tree-sitter-kotlin" nil nil nil nil)
          ;; (latex "https://github.com/latex-lsp/tree-sitter-latex" nil nil nil nil)
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua" nil nil nil nil)
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make" nil nil nil nil)
          ;; (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil nil nil nil)
          (nix "https://github.com/nix-community/tree-sitter-nix" nil nil nil nil)
          (nu "https://github.com/nushell/tree-sitter-nu" nil nil nil nil)
          (python "https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil)
          (r "https://github.com/r-lib/tree-sitter-r" nil nil nil nil)
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby" nil nil nil nil)
          (rust "https://github.com/tree-sitter/tree-sitter-rust" nil nil nil nil)
          (scala "https://github.com/tree-sitter/tree-sitter-scala" nil nil nil nil)
          (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages" nil nil nil)
          (toml "https://github.com/tree-sitter/tree-sitter-toml" nil nil nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" nil nil nil nil)))
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source)))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
    (setq
     major-mode-remap-alist
     '((yaml . yaml-ts-mode)
       (toml . toml-ts-mode)
       (rust . rust-ts-mode)
       (ruby . ruby-ts-mode)
       (python-mode . python-ts-mode)
       (json-mode . json-ts-mode)
       (javascript-mode . js-ts-mode)
       (java-mode . java-ts-mode)
       (go-mode . go-ts-mode)
       (dockerfile-mode . dockerfile-ts-mode)
       (css-mode . css-ts-mode)
       (cpp-mode . cpp-ts-mode)
       (cmake-mode . cmake-ts-mode)
       (c++-mode . c++-ts-mode)
       (c-mode . c-ts-mode)
       (bash-mode . bash-ts-mode)))))

;;;;; markdown

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode)
   ("\\.txt\\'" . gfm-mode)
   ("qutebrowser-editor-" . gfm-mode))
  :general
  (:keymaps 'markdown-mode-map :states 'normal
   "<RET>" #'markdown-follow-thing-at-point
   "TAB" #'markdown-cycle
   "]]" #'markdown-outline-next
   "[[" #'markdown-outline-previous)
  (im-leader :keymaps 'markdown-mode-map
    "oi" #'markdown-toggle-inline-images)
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-display-remote-images t))

(use-package edit-indirect
  :after markdown-mode)

;;;;; haskell

(use-package haskell-mode
  :mode "\\.hs\\'")

;; (use-package lsp-haskell
;;   :after (lsp haskell)
;;   :config
;;   (setq lsp-haskell-process-path-hie "ghcide"
;;         lsp-haskell-process-args-hie '()))

;;;;; rust

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-clippy-preference 'on))

;;;;; web-mode

;; This helps me edit html files with css and javascipt inside them.

(use-package web-mode
  :mode "\\.html\\'")

;;;;; java

;; See lsp-java configuration above.

(add-hook 'java-ts-mode-hook #'hs-minor-mode)

;;;;; javascript

;;;;;; jsdoc.el

;; This is a package I wrote for inserting JSDoc comments easily. Check out the [[https://github.com/im-jsdoc.el][README]].

(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el")
  :defer t)

;;;;;; Add node_modules/.bin to PATH automatically

(defun im-add-node-modules-to-path ()
  "Add node_modules/.bin to variable `exec-path'."
  (interactive)
  (-some--> (locate-dominating-file "." "node_modules")
    (expand-file-name it)
    (f-join it "node_modules/.bin")
    (setq-local exec-path `(,it ,@exec-path))))

(add-hook 'js-ts-mode-hook #'im-add-node-modules-to-path)
(add-hook 'eshell-mode-hook #'im-add-node-modules-to-path)

;;;;;; Helper functions

;; TODO make this idempotent, it breaks absolute imports
;; TODO make it work on a line instead of between quotes
(defun im-js-relative-import-to-abs ()
  "Convert a relative import to an absolute import.
For example, if you are on the line:
  import Test from '../../test'
This function transforms the line into:
  import Test from 'src/a/b/test'"
  (interactive)
  (let ((fname (substring-no-properties (thing-at-point 'filename)))
        (bounds (bounds-of-thing-at-point 'filename)))
    (delete-region (car bounds) (cdr bounds))
    (insert (s-chop-prefix (im-current-project-root) (expand-file-name fname)))))

(defun im-convert-js-object-to-json ()
  "Convert selected JS object into JSON object.
Example:
  {a: 3, b: 5}
is converted into
  {
    \"a\": 3,
    \"b\": 5
  }"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       (region-beginning)
       (region-end)
       "node -e 'console.log(JSON.stringify(eval(\"(\" + require(\"fs\").readFileSync(0, \"utf-8\") + \")\"), null, 2))'"
       (current-buffer)
       t)
    (user-error "Select something first")))

(defun im-convert-json-to-js-object ()
  "Convert selected JS object into JSON object.
Example:
  {
    \"a\": 3,
    \"b\": 5
  }
is converted into
  {
    a: 3,
    b: 5,
  }"
  (interactive)
  (if (use-region-p)
      (shell-command-on-region
       (region-beginning)
       (region-end)
       "node -e 'console.log(require(\"util\").inspect(JSON.parse(require(\"fs\").readFileSync(0, \"utf-8\")), false, null))'"
       (current-buffer)
       t)
    (user-error "Select something first")))

;;;;;; Debug helpers

(defun im-refactor-debug-log-text ()
  "Return a string in the following format: 'BufferName:FunctionName:LineNumber'.
This is used in my snippets."
  (format "%s:%s:%s" (buffer-name) (which-function) (line-number-at-pos)))

(defun im-js-insert-debug-log-for-current-variable ()
  "Insert a `console.log' line for currently focused variable."
  (interactive)
  (let* ((curr-indent)
         (node (treesit-node-child-by-field-name
                (car (jsdoc--tsc-find-descendants-with-type
                      (treesit-node-parent (treesit-node-at (point)))
                      "variable_declarator"))
                "name"))
         (text (treesit-node-text node)))
    (goto-char (treesit-node-end (treesit-node-parent node)))
    (end-of-line)
    (setq curr-indent (current-indentation))
    (insert "\n")
    (insert (make-string curr-indent ? ))
    (insert
     (format
      "console.log(\"%s\", %s) // FIXME remove log"
      (im-refactor-debug-log-text)
      (->>
       text
       (s-split "\n")
       (-map #'s-trim)
       (s-join " "))))))

;;;;; typescript & deno

(setq-default typescript-ts-mode-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode-hook #'im-add-node-modules-to-path)
(add-hook 'tsx-ts-mode-hook #'im-add-node-modules-to-path)
(add-hook 'typescript-ts-mode-hook #'hs-minor-mode)
(add-hook 'tsx-ts-mode-hook #'hs-minor-mode)

(use-package im-deno
  :straight nil
  :demand t)

(use-package im-fnm
  :straight nil
  :demand t)

;;;;;; REPL interaction

(defun im-treesit-find-parent-with-type (node wanted-type)
  "Get first parent of NODE where parents type is WANTED-TYPE."
  (let ((curr-node node)
        (curr-type nil))
    (while (and curr-node (not (equal curr-type wanted-type)))
      (setq curr-node (treesit-node-parent curr-node))
      (setq curr-type (when curr-node (treesit-node-type curr-node))))
    curr-node))

(defun im-ts-current-expression ()
  "Get smallest meaningful expression (as something that can be sent to REPL)."
  (let* ((current-node (treesit-node-at (point)))
         (getter (lambda (it)
                   (when-let* ((node (im-treesit-find-parent-with-type current-node (car it))))
                     (if (numberp (cdr it))
                         (treesit-node-child node (cdr it))
                       node)))))
    (->>
     '(("lexical_declaration")
       ("expression_statement" . 0)
       ("function_declaration")
       ("interface_declaration")
       ("enum_declaration")
       ("import_statement")
       ("class_declaration"))
     (-find getter)
     (funcall getter))))

;;;;;; ob-deno

;; To allow net access (--allow-net), use ":allow net" in the src
;; block header.

;; Use "return ..." to get the object.

(use-package ob-deno
  :after org
  :straight (:host github :repo "isamert/ob-deno"))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("deno" . typescript-ts)))

;;;;; json

;; ~hs-minor-mode~ works great with JSON.

(add-hook 'json-mode-hook #'hs-minor-mode)
(add-hook 'json-ts-mode-hook #'hs-minor-mode)

;;;;;; Running JQ or Javascript on given JSON

;; I work with JSON a lot. Here I have some functions that let's you
;; run given expression on selected JSON.

(defvar im-nodejs-runner-preface
  (concat "const R = require(\"ramda\");\n"
          (with-temp-buffer
            (insert-file-contents (f-join im-packages-path "functions.js"))
            (buffer-string))))

(defvar im-deno-runner-preface
  (concat "const R = await import(\"https://esm.sh/v122/ramda@0.29.0\");\n"
          (with-temp-buffer
            (insert-file-contents (f-join im-packages-path "functions.js"))
            (buffer-string))))

(defun im-run-jq-on-json (json expression &optional replace)
  "Run given EXPRESSION with jq on JSON and return the result.
If REPLACE is non-nil, then clear the buffer or current
selection and insert the result.

When called with prefix argument, REPLACE becomes non-nil."
  (interactive (im--read-json-and-expression))
  (im--run-x-on-json
   (with-temp-buffer
     (insert json)
     (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" expression) nil 't)
     (buffer-string))
   (called-interactively-p 'interactive)
   replace))

(defun im-run-nodejs-on-json (json expression &optional replace)
  "Run given EXPRESSION with nodejs on JSON and return the result.
If REPLACE is non-nil, then clear the buffer or current
selection and insert the result.

When called with prefix argument, REPLACE becomes non-nil."
  (interactive (im--read-json-and-expression))
  (im--run-x-on-json
   (with-temp-buffer
     (insert im-nodejs-runner-preface)
     (insert "\n")
     (insert (format "const it = %s;" json))
     (insert (format "JSON.stringify(%s, null, 2)" expression))
     (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
     (buffer-string))
   (called-interactively-p 'interactive)
   replace))

(defun im-run-deno-on-json (json expression &optional replace)
  "Run given EXPRESSION with deno on JSON and return the result.
If REPLACE is non-nil, then clear the buffer or current
selection and insert the result.

When called with prefix argument, REPLACE becomes non-nil."
  (interactive (im--read-json-and-expression))
  (im--run-x-on-json
   (with-temp-buffer
     (insert im-deno-runner-preface)
     (insert "\n")
     (insert (format "const it = %s;" json))
     (insert (format "console.log(JSON.stringify(%s, null, 2))" expression))
     (shell-command-on-region (point-min) (point-max) "deno run --allow-all -" nil 't)
     (buffer-string))
   (called-interactively-p 'interactive)
   replace))

(defun im--run-x-on-json (result interactive? &optional replace)
  "Run given EXPRESSION with nodejs on JSON and return the result.
If REPLACE is non-nil, then clear the buffer or current
selection and insert the result.

When called with prefix argument, REPLACE becomes non-nil."
  (when (and interactive?
             (not replace))
    (if (>= (max-mini-window-lines) (length (s-lines result)))
        (message ">> %s" (s-trim result))
      (with-current-buffer (get-buffer-create "*nodejs-result*")
        (switch-to-buffer-other-window (current-buffer))
        (insert result))))
  (cond
   ((and replace (use-region-p))
    (delete-region (region-beginning) (region-end))
    (insert result))
   (replace
    (erase-buffer)
    (insert result)))
  result)

(defun im--read-json-and-expression ()
  (list
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (let ((json (read-string "JSON (leave empty to use whole buffer): ")))
       (if (s-blank? json)
           (buffer-substring-no-properties (point-min) (point-max)))))
   (read-string "Expression: ")
   current-prefix-arg))

(im-leader-v
  "qj" #'im-run-deno-on-json)

;;;;;; Extra functionality

(defun im-jsons-print-path-python ()
  (interactive)
  (let ((jsons-path-printer 'jsons-print-path-python))
    (jsons-print-path)))

(defun im-jsons-print-path-javascript-js-jq ()
  (interactive)
  (let ((jsons-path-printer 'jsons-print-path-jq))
    (jsons-print-path)))

;;  Provides jsons-print-path function, it simply kills the path to the key under point
(use-package json-snatcher
  :config
  ;; Copies paths like:.definition.summary.pastGroup.trackingResults[0].trackingItemReferenceId
  ;; I've created two functions above for the variations
  (setq jsons-path-printer 'jsons-print-path-jq))

;; Add execution ability to json blocks inside org-mode. Either add
;; `:jq some-jq-query' or `:node it.accessor' to code block's header
;; to filter the json
(defun org-babel-execute:json (body params)
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq (im-run-jq-on-json body jq))
     (node (im-run-nodejs-on-json body node)))))

;;;;; scala

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode)
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;;;;;; Supplementary functions

(defun im-jshell ()
  "Open JShell with runtime dependencies of the current project loaded.
Please note that tab-completion for runtime dependencies *do not*
work.  You need to enter full path while importing by yourself."
  (interactive)
  (let* ((default-directory (im-current-project-root))
         (vterm-shell "mvn -q com.github.johnpoth:jshell-maven-plugin:1.3:run -DtestClasspath")
         ;; ^ This plugin makes it possible to import project's maven dependencies
         ;; another option is to use jshell directly.
         (vterm-buffer-name "*vterm-jshell*"))
    (vterm)))

;;;;; clojure

;; Here is the current workflow I use:

;; - =lein new app project-name=
;; - =cider-jack-in=

(use-package cider
  :defer t
  :general
  (general-def :keymaps 'cider-inspector-mode-map :states 'normal
    "RET" #'cider-inspector-operate-on-point
    "DEL" #'cider-inspector-pop)
  :config
  (setq cider-inspector-page-size 50)
  (setq cider-show-error-buffer nil)

  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))

  ;; `compojure' indent
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

;;;;; common-lisp

(use-package slime
  :hook (lisp-mode . slime-mode)
  :general
  (:keymaps 'lisp-mode-map-map :states 'normal
   "K" #'slime-documentation)
  :custom
  (inferior-lisp-program "sbcl"))

;;;;; emmet-mode

;; Hit <C-j> after these and get:

;; - =a= ~<a href="|">|</a>~
;; - =.x= ~<div class="x"></div>~
;; - =br/= ~<br />~
;; - =p.x.y.z= ~<p className="x y z"></p>~ (Works well with JSX)
;; - ~input[type=text]~ ~<input type="text" name="" value=""/>~
;; - =a>b>c= ~<a href=""><b><c></c></b></a>~
;; - =b*3= ~<b></b><b></b><b></b>~


(use-package emmet-mode
  :hook (js-mode css-mode sgml-mode web-mode tsx-mode)
  :custom
  (emmet-expand-jsx-className? t)
  (emmet-self-closing-tag-style " /"))

;;;;; R-lang

(use-package ess
  :mode "\\.r\\'")

(setenv "R_LIBS" (expand-file-name "~/.rlibs"))
(setenv "R_LIBS_USER" (expand-file-name "~/.rlibs"))

;;;;; kotlin

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :custom
  ;; ki is a better REPL for Kotlin. You can save and reload your session.
  (kotlin-command "ki"))

;;;;; gradle/groovy

(use-package groovy-mode
  :mode "\\.gradle\\'")

;;;;; yaml

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'highlight-indent-guides-mode))

;;;;; elisp

;;;;;; Inspector/debugging/pretty-printing

;; There is a built-in function (~data-debug-eval-expression~) to
;; inspect objects (not good as CIDERs inspector but it works). I made
;; a helper function to evaluate last expression and open data-debug
;; window of it.

(defun im-eval-and-inspect-last-sexp ()
  (interactive)
  (require 'data-debug)
  (data-debug-show-stuff (eval-last-sexp nil) "last sexp"))

(use-package data-debug-mode
  :straight (:type built-in)
  :general
  (:states 'normal :keymaps 'data-debug-mode-map
   "RET" #'data-debug-expand-or-contract
   "TAB" #'data-debug-expand-or-contract))

;; There is also ~pp-eval-last-sexp~ which evaluates and pretty-prints
;; the result of last expression in a separate buffer, which can be
;; better for inspection sometimes.

;;;;;; eros

;; Like CIDER, it shows the results of ~eval-last-sexp~ etc. in an
;; overlay, right next to the expression itself. There is also
;; ~eros-inspect-last-result~ which essentially shows the result of
;; last evaluation in a pretty printed format in a different buffer.

(defface im-eval-defun-pulse-highlight-face
  '((((class color) (min-colors 88) (background dark))
     :background "#00CED1" :extend t :foreground "black") ; bright turquoise
    (((class color) (min-colors 88) (background light))
     :background "#32CD32" :extend t :foreground "black") ; vibrant lime green
    (t :inherit primary-selection))
  "Face for highlighting evaluated regions.")

(use-package eros
  :straight (:host github :repo "isamert/eros")
  :hook (after-init . eros-mode)
  :config
  (add-hook 'eros-inspect-hooks (lambda () (flycheck-mode -1)))

  (defun im-set-eros-last-result (orig-val)
    (setq eros--last-result orig-val)
    orig-val)
  (advice-add 'eval-expression :filter-return #'im-set-eros-last-result)

  (define-advice eros-eval-defun (:after (&rest _) highlight-expr)
    "Highlight expression."
    (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'defun)))
      (when (and start end)
        (im-pulse-highlight-region start end 'im-eval-defun-pulse-highlight-face 0.3))))

  (define-advice eros-eval-last-sexp (:after (&rest _) highlight-expr)
    "Highlight expression."
    (pcase-let ((`(,start . ,end) (bounds-of-thing-at-point 'sexp)))
      (when (and start end)
        (im-pulse-highlight-region start end 'im-eval-defun-pulse-highlight-face 0.3)))))

;;;;;; Pretty stuff

(defvar
  im-elisp-pretty-symbols
  '((">="     . ?≥)
    ("<="     . ?≤)
    ("defun"  . ?ƒ)
    ("interactive" . ?⎇)
    ("lambda" . ?λ)
    ("thread-last" . ?↠)
    ("thread-first" . ?→)
    ("->>" . ?↠)
    ("->" . ?→)))

(im-prettify-mode emacs-lisp-mode-hook im-elisp-pretty-symbols)
(im-prettify-mode lisp-interaction-mode-hook im-elisp-pretty-symbols)

;; Highlights quotes. Surprisingly useful
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;;;;;; Linting & package development

(use-package package-lint
  :commands package-lint-current-buffer)

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

;;;;;; Folding

(add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
(add-hook 'lisp-interaction-mode-hook #'hs-minor-mode)

;;;;;; Indentation fix

;; Taken from: https://github.com/Fuco1/.emacs.d/blob/2c302dcbedf2722c5c412b6a6d3e3258f6ac1ccf/site-lisp/my-redef.el#LL18C1-L100C62
;; Redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

;;;;;; doctest.el

;; See this:
;; [[https://ag91.github.io/blog/2023/03/20/doctestel-or-testing-your-pure-elisp-functions-in-your-docstring/][Doctest.el
;; or testing your pure Elisp functions in your docstring - Where
;; parallels cross]]

(use-package doctest
  :straight (:host github :repo "ag91/doctest")
  :commands (doctest doctest-here doctest-defun))

;;;;;; suggest.el -- Function suggestions

;; Do ~M-x suggest~, enter the inputs and the expected output and
;; it'll suggest you some functions. Quite nice and useful, I used it
;; plenty of time.

(use-package suggest
  :commands suggest)

;;;;;; emr & redshank -- refactoring tools

(use-package redshank
  ;; All interactive redshank commands
  :commands (redshank-maybe-splice-progn
             redshank-point-at-enclosing-let-form
             redshank-ignore-event
             redshank-asdf-insert-module-components
             redshank-highlight-binder
             redshank-unhighlight-binder
             redshank-slime-uninstall
             redshank-letify-form
             redshank-backward-down-list
             redshank-letify-form-up
             redshank-extract-to-defun
             redshank-enclose-form-with-lambda
             redshank-condify-form
             redshank-eval-whenify-form
             redshank-rewrite-negated-predicate
             redshank-elisp-generate-form
             redshank-lisp-generate-form
             redshank-generate-thing-at-point
             redshank-defclass-skeleton
             redshank-define-condition-skeleton)
  :defer t)

(use-package emr
  :straight (:host github :repo "isamert/emacs-refactor")
  :defer t)

;;;;; Racket

;; - Open a racket buffer.
;; - Do C-c C-c (racket-run)
;; - It'll drop you on a REPL within the scope of the file.

(use-package racket-mode
  :mode "\\.rkt\\'")

;;;;; dhall

(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  ;; I use dhall-lsp-server, so I don't need this
  (setq dhall-use-header-line nil))

;;;;; nix

(use-package nix-mode
  :mode "\\.nix\\'")

(defun im-import-env-from-nix-shell ()
  (interactive)
  (let ((default-directory (im-current-project-root)))
    (when (not (and (file-exists-p "shell.nix") (executable-find "nix-shell")))
      (error "Failed to find shell.nix or nix-shell"))
    (--> (shell-command-to-string "nix-shell --quiet --run 'env'")
         (split-string it "\n")
         (--map (-let (((name val) (s-split-up-to "=" it 1)))
                  (setenv name val)
                  (when (string-equal name "PATH")
                    (setq exec-path (split-string val path-separator)))
                  `(,name ,val))
                it))
    (message "Done.")))

;;;;; swift

(use-package swift-mode :defer t)

;;;;; scheme

(use-package scheme
  :straight (:type built-in)
  :config
  (with-eval-after-load 'outli
    (add-to-list 'outli-heading-config '(scheme-mode ";;" ?\; t))))

(use-package geiser
  :defer t
  :commands geiser
  :custom
  (geiser-debug-jump-to-debug-p nil)
  (geiser-default-implementation 'guile)
  :init
  (with-eval-after-load 'geiser
    (evil-collection-geiser-setup)))

(use-package geiser-guile
  :after geiser
  :defer t)

;;;;; Docker stuff

;; Some major modes for editing files.

(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package docker-compose-mode
  :mode "docker-compose\\'"
  :hook
  (docker-compose-mode . highlight-indent-guides-mode))

;; A package for  managing docker.

(use-package docker
  :defer t
  :config
  (evil-define-key 'normal docker-container-mode-map (kbd "a") #'docker-container-help)
  (evil-define-key 'normal docker-image-mode-map     (kbd "a") #'docker-image-help)
  (evil-define-key 'normal docker-machine-mode-map   (kbd "a") #'docker-machine-help)
  (evil-define-key 'normal docker-network-mode-map   (kbd "a") #'docker-network-help)
  (evil-define-key 'normal docker-volume-mode-map    (kbd "a") #'docker-volume-help))

;;;;; vimrc

;; Mostly for editing tridactyl and sometimes real vimrc.

(use-package vimrc-mode :defer t)

;;;;; Graphviz/dot

(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gv\\'")
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))


;;;;; PlantUML

(use-package plantuml-mode
  :mode "\\.\\(plantuml\\|pum\\|plu\\)\\'"
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "/nix/store/q0v5nv70zc23fx4hjgghnqf7lvydr6fq-plantuml-1.2021.3/lib/plantuml.jar")
  (setq org-plantuml-jar-path plantuml-jar-path))

;;;;; Couchbase

(use-package im-couchbase
  :straight nil)

;;;;; BigQuery

(define-derived-mode bqsql-mode sql-mode "bqsql-mode")
(add-to-list 'auto-mode-alist (cons (rx ".bqsql" string-end) #'bqsql-mode))

(defun org-babel-expand-body:bqsql (body params)
  (s-format
   body
   (lambda (key alist) (assoc-default (intern key) alist))
   (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params)))

(defun org-babel-execute:bqsql (query params)
  "Execute QUERY with given PARAMS.
`:var' syntax is ${var_name} and replaced as-is.

`:format' can be either `org-table', `table' or `json'.  Former outputs
an org table, other one outputs the result as json.  By default, it's
`org-table'.  `org-table' means the output is a regular org mode table.
`table' means the output is `table.el' formatted table (it is actually
what is returned from bq command with the `pretty' formatting option).

If `:buffer' is non-nil, then output results to a buffer, instead
of the results drawer.

If `:cmd' is non-nil, then instead of executing query, print out
the resulting bq command."
  (let* ((job-id (im-uuid))
         (dry-run? (alist-get :dry-run params))
         (format (or (alist-get :format params) "org-table"))
         (api (alist-get :api params))
         (project-id (alist-get :project-id params))
         (buffer? (alist-get :buffer params))
         (cmd? (alist-get :cmd params))
         (json-out? (s-matches? "json" format))
         (buf (get-buffer-create "*im-big-querysql*"))
         (org-buffer (current-buffer))
         (start-time (float-time))
         cmd cmd-str
         process
         (vars (org-babel--get-vars params)))
    (setq query (org-babel-expand-body:bqsql query params))
    (setq cmd `("query" ,buf "bq" "query"
                ,@(when api `("--api" ,api))
                ,@(when dry-run? `("--dry_run"))
                ,@(when project-id `("--project_id" ,project-id))
                "--quiet" "--max_rows" "100000" "--nouse_legacy_sql"
                "--format" ,(pcase format
                              ((or "org-table" "table") "pretty")
                              (x x))
                "--job_id" ,job-id ,query))
    (setq cmd-str (s-join
                   " "
                   (--map (if (not (s-prefix? "--" it))
                              (format "\"%s\"" (s-replace "\"" "\\\"" it))
                            it)
                          (-drop 2 cmd))))
    (when cmd?
      (org-babel-insert-result cmd-str)
      (user-error "Done"))
    (with-current-buffer buf
      (buffer-disable-undo)
      (erase-buffer))
    (im-ensure-binary "bq" :package "google-cloud-sdk" :installer "nix-env -iA")
    (setq process (apply #'start-process cmd))
    (set-process-sentinel
     process
     (lambda (p m)
       (let* ((end-time (float-time))
              (result (with-current-buffer buf
                        ;; Convert pretty table into an org mode table
                        (when (equal "org-table" format)
                          (goto-char (point-min))
                          (skip-chars-forward "\n\t ")
                          (when (looking-at "^\\+")
                            (kill-line 1)
                            (forward-line 1)
                            (delete-char 1)
                            (insert "|")
                            (end-of-line)
                            (delete-char -1)
                            (insert "|")
                            (goto-char (point-max))
                            (skip-chars-backward "\n\t ")
                            (beginning-of-line)
                            (when (looking-at "^\\+")
                              (kill-line 1))))
                        (buffer-substring-no-properties (point-min) (point-max))))
              (msg (format "=> Query finished, time elapsed: %s"
                           (format-seconds "%Y %D %H %M %z%S" (- end-time start-time))))
              (bname (format "*bqsql:%s" (if (eq buffer? t) job-id buffer?)))
              found?)
         (with-current-buffer org-buffer
           (save-excursion
             (goto-char (point-max))
             (setq found? (re-search-backward job-id nil t))
             (when (or buffer? (not found?))
               (with-current-buffer (get-buffer-create bname)
                 (erase-buffer)
                 (insert result)
                 (if json-out?
                     (json-ts-mode)
                   (progn
                     (org-mode)
                     (im-disable-line-wrapping)))
                 (setq header-line-format msg))
               (unless found?
                 (user-error "Org-block is gone.  Result inserted to the buffer %s" bname)))
             (forward-line -4)
             (org-babel-insert-result
              (if buffer? msg result)
              (list "replace" (cond
                               ((s-prefix? "Error" result) "drawer")
                               (buffer? "drawer")
                               (json-out? "lang")
                               (t "raw")))
              nil
              nil
              (when json-out? "json"))
             (when buffer?
               (switch-to-buffer-other-window bname)))))))
    job-id))

(defun im-big-query--fix-table-name (table-name)
  "Fix table name by replacing first `.' with `:'.
This is the format that `bq' tool expects."
  (s-replace-regexp "^\\([A-Za-z0-9_-]+\\)\\." "\\1:" table-name))

(defun im-big-query-job-status (job-id)
  "Get status for given job id."
  (interactive
   (list (read-string "Job ID: " (im-region-or 'symbol))))
  (let ((buf (get-buffer-create (format "*im-bigquery: %s*" job-id))))
    (switch-to-buffer buf)
    (insert (shell-command-to-string (format "bq show %s -j '%s'" (if current-prefix-arg "--format=prettyjson" "") job-id)))))

(defun im-big-query-cancel-job (job-id)
  "Get status for given job id."
  (interactive
   (list (read-string "Job ID: " (im-region-or 'symbol))))
  (im-shell-command
   :command "bq"
   :args `("cancel" ,job-id)
   :switch t
   :buffer-name (format "*bq-cancel-job: %s" job-id)))

(defun im-big-query-table-info (table-name)
  "Get summary information for TABLE-NAME.
This information includes schema summary, last modified date,
total {rows,bytes} etc. and first 10 rows of the table."
  (interactive
   (list (read-string
          "Table: "
          (ignore-errors
            (im-region-or
             (lambda () (im-inner-back-quote-at-point)))))))
  (setq table-name (im-big-query--fix-table-name table-name))
  (let ((buffer-name (format "*bq table info: %s*" table-name)))
    (im-shell-command
     :buffer-name buffer-name
     :switch t
     :command (format "bq show '%s'" table-name)
     :on-start
     (lambda (&rest _)
       (toggle-truncate-lines +1)
       (im-shell-command
        :buffer-name buffer-name
        :command (format "bq head -n 200 '%s'" table-name))))))

(defun im-big-query-get-all-datasets (project-id)
  (json-parse-string
   (shell-command-to-string
    (format "bq ls --format=json --project_id=%s" project-id))
   :object-type 'alist
   :array-type 'list))

(defun im-big-query-get-all-tables (dataset-id)
  (json-parse-string
   (shell-command-to-string
    (format "bq ls --format=json --max_results 5000 '%s'" dataset-id))
   :object-type 'alist
   :array-type 'list))

(defmemoizefile im-big-query-all-tables () "~/.emacs.d/big-query-table-cache"
  (->>
   (im-big-query-get-all-datasets im-big-query-project-id)
   (--map (alist-get 'id it))
   (--mapcat (ignore-errors (im-big-query-get-all-tables it)))))

(defun im-big-query-all-table-names ()
  (--map (alist-get 'id it) (im-big-query-all-tables)))

;; TODO: Act on it
(defun im-big-query-select-table ()
  "Select a BigQuery table and insert it."
  (interactive)
  (insert
   (im-completing-read
    "Table: "
    (--map (s-replace ":" "." it) (im-big-query-all-table-names))
    :category 'im-big-query-table-name)))

(with-eval-after-load 'embark
  (defvar-keymap im-big-query-table-name
    :doc "Actions for BQ tables"
    :parent embark-general-map
    ;; Define the first binding as the default action
    "i" #'im-big-query-table-info)

  (add-to-list 'embark-keymap-alist '(im-big-query-table-name . im-big-query-table-name)))

(bind-key "M-o B" #'im-big-query-select-table)

(im-cape
 :name big-query-tables
 :completion #'im-big-query-all-table-names
 :extractor
 (lambda (xs)
   (--map (s-replace ":" "." it) xs))
 :bound filename
 :kind (lambda (xs x) "" 'module)
 :category symbol
 :key "M-o b")

;;;;; kbd-mode

;; For working with KMonad kbd files. Do ~C-c C-c~ (~kbd-start-demo~)
;; to apply your config and try it in a buffer.

(use-package kbd-mode
  :straight (:host github :repo "kmonad/kbd-mode")
  :mode "\\.kbd\\'"
  :config
  (setq kbd-mode-kill-kmonad "pkill -9 kmonad")
  (setq kbd-mode-start-kmonad "kmonad ~/.config/kmonad.kbd"))

;;;;; lua-mode

(use-package lua-mode
  :mode "\\.lua\\'")

;;;;; jsonnet-mode

(use-package jsonnet-mode
  :mode ("\\.jsonnet\\'" "\\.libsonnet\\'")
  :config
  ;; Apheleia config, so that it automatically formats on save
  (setf (alist-get 'jsonnet apheleia-formatters) '("jsonnetfmt" "-"))
  (setf (alist-get 'jsonnet-mode apheleia-mode-alist) 'jsonnet))

;;;;; sql

;; I generally do not edit ~.sql~ files but instead I do SQL through
;; code blocks in org-mode. /lsp-mode/ offers ~lsp-org~ command to
;; help using LSP features inside org-mode code blocks. It synergzes
;; well with lsp-sqls. One important thing to remember is that you
;; need to have a ~:tangle <file>~ in the code blocks so that LSP mode
;; can actually have the tangled file to send to the corresponding
;; LSP.

;; - ~lsp-org~ :: Start LSP mode. Must be executed with cursor being source block.
;; - ~lsp-virtual-buffer-disconnect~ :: Turn off lsp-mode.

;; ~sql-connect~ also works fairly well, except for code
;; completion. But you can always list the tables and columns with SQL
;; and afterwards dabbrev helps you like you have code completion.

;; I initialize ~sql-connection-alist~ in another file that is not
;; checked out to this repository and afterwards I call the
;; ~im-sql-setup-lsp~ function.

(defun im-sql-setup-lsp ()
  "Set `lsp-sqls-connections' using `sql-connection-alist'.
Only works for PostgreSQL connections right now."
  (setq
   lsp-sqls-connections
   (--map
    `((driver . "postgresql")
      (dataSourceName . ,(format "host=%s port=%s user=%s password=%s dbname=%s sslmode=disable"
                                 (car (alist-get 'sql-server it))
                                 (car (alist-get 'sql-port it))
                                 (car (alist-get 'sql-user it))
                                 (car (alist-get 'sql-password it))
                                 (car (alist-get 'sql-database it)))))
    sql-connection-alist)))

(defun im-sql-run-query ()
  (interactive)
  "Open a new scratch buffer with code completion to run a query, using org-mode."
  (let ((buff (format-time-string "sql_%Y-%m-%d.org")))
    (unless (get-buffer buff)
      (with-current-buffer (get-buffer-create buff)
        (insert (im-s-interpolated
                 "#+TITLE: #{(format-time-string \"%Y-%m-%d\")}-sql
#+PROPERTY: header-args:sql :noweb yes :tangle .#{(format-time-string \"%Y%m%d\")}.sql
#+PROPERTY: header-args:bqsql :noweb yes\n
You can always use ~sql-connect~ to get a simple REPL. It is
faster for longer outputs.

Use ~lsp-sql-switch-connection~ to switch between different
connections, to make code completion work for your current
context.

Don't forget about snippets too."))
        (org-mode)
        (im-disable-line-wrapping)
        (write-file (format "~/Documents/notes/temp/%s" buff))))
    (with-current-buffer buff
      (goto-char (point-max))
      (insert
       (im-s-interpolated
        "\n\n* #{(format-time-string \"%Y-%m-%d %a %H:%M\")}
#+begin_src sql :engine postgresql :dbconnection #{(sql-read-connection \"Select DB: \")}
SELECT * FROM _ LIMIT 1;
#+end_src\n\n"))
      (forward-line -3)
      (unless lsp-mode
        (lsp-org))
      ;; I wasn't able to find a way to retrieve current
      ;; connection. If I was able to, then I would compare it with
      ;; the selected dbconnection from above and show the following
      ;; selection only if needed
      (lsp-sql-switch-connection))
    (switch-to-buffer buff)))

;;;;; go

(use-package go-mode
  :mode "\\.go\\'"
  :defer t)

;;;;; nginx

(use-package nginx-mode
  :mode (("nginx\\.conf\\'"  . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode))
  :defer t)

;;;;; Conf

(use-package conf-mode
  :straight (:type built-in)
  :defer t
  :config
  (add-hook 'conf-mode-hook #'outli-mode))

;;;;; fish

(use-package fish-mode
  :mode ("\\.fish\\'"))

;;;;; python

;; * Jupyter & org-mode
;;
;; - =jupyter= package enables you to use jupyter notebooks in Emacs.
;; - It provides two ways, one is through org-mode and it's the one I
;;   prefer.
;; - You need to convert =ipynb= files into org-mode files by running:
;;   =M-x im-convert-ipynb-to-org-buffer=
;;
;; - =jupyter-org-interaction-mode= is enabled in all org-mode buffers.
;; - Yan simply use ~jupyter-python~ source blocks to write
;;   your code and run them exactly as you do with normal org code
;;   blocks.
;; - There are also these functions which you can utilize:
;;   - jupyter-eval-defun
;;   - jupyter-inspect-at-point      → K
;;   - jupyter-eval-line-or-region   → C-x C-e
;;   - jupyter-repl-interrupt-kernel → C-c C-i
;;   - jupyter-repl-restart-kernel   → C-c C-r
;;
;; See ~org-babel-default-header-args:jupyter-python~ down below for
;; more details on how these blocks are getting executed.
;;
;; **WARNING**: org-modern breaks ANSI code escaping of
;; jupyter-org-client somehow if it starts before jupyter-org-client.
;; Hence I increased it's depth. See org-modern configuration for more
;; info.
;;
;; * Virtual Envs and Jupyter
;;
;; If you are using virtualenvironments, then simply do
;; =pyvenv-activate= and select the virtenv folder.
;;
;; - =python3 -m venv .venv= to create a virtenv
;; - ~pip install -r requirements.txt~ to install requirements
;; - ~source .venv/bin/activate.{,.fish,.zsh}~ to active the venv in terminal
;; - or =pyvenv-active= to active inside Emacs.
;;
;; After activating the venv, you may need to do
;; ~jupyter-repl-restart-kernel~.

(use-package pyvenv
  :defer t)

(use-package jupyter
  :straight (:host github :repo "isamert/jupyter")
  :custom
  (jupyter-org-want-keybinding . nil)
  (jupyter-org-want-integration . nil))

(with-eval-after-load 'jupyter-org-client
  (setq org-babel-default-header-args:jupyter-python
        '((:async . "yes")      ;; Make outputs async
          (:session . "py")     ;; A named session
          (:kernel . "python3") ;; Don't know but works
          (:pandoc . t)         ;; Convert rich outputs to org-mode format
          ;; Prefer plain-text outputs from jupyter instead of
          ;; direct html outputs
          (:display "text/plain text/html"))))

(defun im-convert-ipynb-to-org-buffer ()
  "Convert the current IPython Notebook file to Org format and open it in a new buffer."
  (interactive)
  (let* ((fname (file-name-sans-extension (buffer-name)))
         (org-buffer-name (concat fname ".org"))
         (pandoc-output (shell-command-to-string
                         (format "pandoc --from ipynb --to org '%s'" (buffer-file-name)))))
    (with-current-buffer (get-buffer-create org-buffer-name)
      (erase-buffer)
      (insert pandoc-output)
      (switch-to-buffer (current-buffer))
      ;; Add :session to code blocks so that each file has a separate session
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward " jupyter-python" nil t)
          (replace-match (concat " jupyter-python :session " fname) nil nil)))
      (goto-char (point-min))
      (org-mode)
      (jupyter-org-interaction-mode))))

;;;; Window and buffer management

;;;;; tab-bar-mode

;; It's a great workspace manager that comes bundled with Emacs. I was
;; using an abomination where ~persp.el~ and ~eyebrowse~ was glued
;; together but I guess this is a simpler and more sane alternative to
;; them.

(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice im-init-file)
  ;; Experimenting with not showing tab-bar at all.
  ;; TODO: need a way to display on which tab am I, possibly inside
  ;; modeline?
  (tab-bar-show t)
  (tab-bar-history-limit 20)
  ;; Show numbers before tab names
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-auto-width-max t)
  (tab-bar-tab-name-format-function #'tab-bar-tab-name-format-default)
  (tab-bar-tab-name-function #'tab-bar-tab-name-current)
  ;; FIXME: For some reason, my function gets slower over time. Dunno
  ;; why, will investigate later.
  ;; (setq tab-bar-tab-name-function #'im-current-project-name)

  (tab-bar-format
   '(tab-bar-format-tabs
     tab-bar-separator
     tab-bar-format-add-tab
     tab-bar-format-align-right
     tab-bar-format-global))
  ;; Don't show global-mode-string in mode-line because we already show
  ;; it on right side of the tab-bar
  (mode-line-misc-info (assq-delete-all 'global-mode-string mode-line-misc-info))
  :hook (after-init . tab-bar-mode)
  :config
  ;; Enable history so that I can use `tab-bar-history-back'
  ;; `tab-bar-history-forward'.  `winner-mode' also have the same
  ;; functionality but `tab-bar-history-mode' works per tab so that it
  ;; does not mangle things up between different tabs which is just
  ;; great.
  (tab-bar-history-mode)

  ;; Evil has a default binding for switching between tabs with ~gt~ and
  ;; ~gT~, switching forward and backward respectively. I just make them
  ;; repeatable so that after first ~gt~ (~gT~) I can hammer down ~t~
  ;; (or ~T~) to switch next/prev tab quickly instead of doing ~gt~ (or
  ;; ~gT~) again and again.
  (im-make-repeatable tab-bar-switch
    "t" evil-tab-next
    "t" tab-bar-switch-to-next-tab
    "T" tab-bar-switch-to-prev-tab))

;; I want to show ~consult-buffer~ when I open a new tab to be able to
;; quickly jump a file. I also want to show it in a buffer, not in
;; mini-buffer.
(defun im-tab-bar-new-tab ()
  "Open a new tab and display `consult-buffer'."
  (interactive)
  (tab-bar-new-tab)
  (let ((vertico-multiform-commands '((consult-buffer buffer (vertico-buffer-display-action . (display-buffer-same-window)))))
        (mini-frame-ignore-commands `(,@mini-frame-ignore-commands consult-buffer))
        (this-command 'consult-buffer))
    (consult-buffer)))

(defun im-tab-bar-new-tab-with-current-buffer ()
  "Open a new tab and display the current buffer."
  (interactive)
  (let ((tab-bar-new-tab-choice t))
    (tab-bar-new-tab)))

;;;;; tab-line-mode

(defvar im-tab-line-hidden-buffer-name-regexp
  (concat
   "\\*\\(Calc.*\\|Calendar\\|elfeed-log\\|helpful.*\\|Compile-Log\\|Help\\|lsp-log\\)\\*"
   "\\|magit.*"
   "\\|slack-curl-downloader"
   "\\|slack-log"
   "\\|slack-event-log"
   "\\|dir-data"
   "\\|.*-ls\\*"
   "\\|.*stderr\\*"
   "")
  "Regexp to filter out buffer names on tab line.")

(use-package tab-line
  :straight (:type built-in)
  ;; :hook (after-init . global-tab-line-mode)
  :custom
  (tab-line-close-button-show 'selected)
  (tab-line-tabs-function #'im-tab-line-buffers)
  (tab-line-tab-name-truncated-max 32)
  (tab-line-tab-name-function #'im-tab-line-buffer-name))

(defun im-tab-line-buffer-name (buffer &optional _buffers)
  (let ((name (tab-line-tab-name-truncated-buffer buffer)))
    ;; im-tab-line-buffers groups special with the same prefix
    ;; together. I mostly prefix my buffers like "*XXX: ..." and here
    ;; I delete that prefix to gain a little bit more space while
    ;; displaying.
    (if-let* ((str (s-match " ?\\*[a-zA-Z0-9_$-]+: \\(.*\\)" name)))
        (cadr str)
      name)))

(defun im-tab-line-buffers ()
  "Return a list of buffers grouped by common characteristics for tab-line display.

A \"group\" consists of buffers that:

- Share the same project root directory (as determined by
`im-current-project-root').

- Have buffer names starting with the same prefix (typically for buffers
such as '*eshell:', '*gptel:', and other duplicable special modes).

This function is intended for use with `tab-line-mode' to conveniently
group related buffers (e.g., multiple shells or AI chat sessions in the
same project) together on the tab line, improving navigation in projects
where these special buffers may be duplicated."
  (let* ((buffer-filter (apply-partially #'im-tab-line--buffer-same-group? (current-buffer))))
    (seq-sort-by
     #'buffer-name #'string<
     (seq-filter (lambda (b)
                   (with-current-buffer b
                     (and (im-tab-line--buffer-valid?)
                          (funcall buffer-filter))))
                 (funcall tab-line-tabs-buffer-list-function)))))

(defun im-tab-line--buffer-valid? ()
  (not (s-matches? im-tab-line-hidden-buffer-name-regexp (buffer-name))))

(defun im-tab-line--buffer-same-group? (orig-buffer proj-dir)
  (and (equal
        (or (im-current-project-root)
            default-directory)
        (with-current-buffer orig-buffer
          (or (im-current-project-root)
              default-directory)))
       (s-prefix? (substring (buffer-name orig-buffer) 0 4) (buffer-name))))

;;;;; tabgo.el

(use-package tabgo
  :straight (:host github :repo "isamert/tabgo.el")
  :demand t
  :general
  (im-leader
    "<SPC>" #'tabgo))

;;;;; ace-window

(use-package ace-window
  :defer t
  :autoload (aw-select)
  :custom
  (aw-background t)
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 4.5 :weight 'bold :foreground "deep sky blue")
  (ace-window-posframe-mode))

(defun im-find-file-ace-window (filename &optional wildcards)
  "Edit file FILENAME, in selected window.

Like \\[find-file] (which see), but uses the selected window by `ace-select-window'."
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((aw-ignore-current nil))
    (select-window (ace-select-window)))
  (switch-to-buffer (find-file-noselect filename nil nil wildcards)))

;;;;; bufler

;; Good for buffer management, especially for groupped killings.

(use-package bufler
  :commands (bufler)
  :config
  (evil-define-key 'normal bufler-list-mode-map
    (kbd "q") #'quit-window
    (kbd "x") #'bufler-list-buffer-kill
    (kbd "<return>") #'bufler-list-buffer-switch
    (kbd "<tab>") #'magit-section-cycle
    (kbd "M-,") #'bufler-list-buffer-peek
    (kbd "p") #'bufler-list-buffer-peek))

;;;;; Functions

(defun im-quit ()
  "Quit current window or buffer.
Inspired by `meow-quit' but I changed it in a way to make it work with side windows properly."
  (interactive)
  (if (or
       (window-parameter (selected-window) 'window-side)
       (> (seq-length (seq-filter (lambda (it) (not (window-parameter it 'window-side))) (window-list (selected-frame)))) 1))
      (delete-window)
    (previous-buffer)))

(defun im-split-window-below ()
  "Split window below and focus."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun im-split-window-right ()
  "Split window right and focus."
  (interactive)
  (split-window-right)
  (other-window 1))

;;;;; Keybindings

(evil-define-key 'normal 'global
  "Q" #'im-quit
  (kbd "M-\\") #'im-split-window-right
  (kbd "M--")  #'im-split-window-below
  (kbd "[2") #'tab-line-switch-to-prev-tab
  (kbd "]2") #'tab-line-switch-to-next-tab
  (kbd "[1") #'tab-bar-switch-to-prev-tab
  (kbd "]1") #'tab-bar-switch-to-next-tab)

(im-leader
  "l" #'evil-window-right
  "h" #'evil-window-left
  "j" #'evil-window-down
  "k" #'evil-window-up)

(im-leader
  ;; misc window operations
  "wo" #'other-window
  "w1" #'delete-other-windows
  "w=" #'balance-windows
  ;; window-move
  "wL" #'evil-window-move-far-right
  "wH" #'evil-window-move-far-left
  "wJ" #'evil-window-move-very-bottom
  "wK" #'evil-window-move-very-top
  ;; window-size
  "w+" #'enlarge-window
  "w-" #'shrink-window
  "w>" #'enlarge-window-horizontally
  "w<" #'shrink-window-horizontally
  ;; workspace (tab-bar-mode)
  "W" #'tab-bar-switch-to-tab
  "wu" #'tab-bar-history-back ;; undo
  "wr" #'tab-bar-history-forward ;; redo
  "wt" #'im-tab-bar-new-tab ;; tab
  "wT" #'im-tab-bar-new-tab-with-current-buffer ;; tab
  "wn" #'tab-bar-rename-tab ;; name
  "wm" #'tab-move ;; move
  "ws" #'tab-bar-switch-to-tab ;; switch
  "wl" #'tab-bar-switch-to-recent-tab ;; last
  "wk" #'tab-close ;; kill
  ;; buffer
  "wb" #'bufler
  ;; ace-window
  "wa" #'ace-window
  "ws" #'ace-swap-window)

;; Buffer related bindings
(im-leader-v
  "b0" #'erase-buffer
  "br" #'rename-buffer
  "bo" #'im-open-region-in-temp-buffer
  "bO" #'im-temp-buffer)

(evil-define-key '(normal insert motion) 'global
  (kbd "M-1") (λ-interactive (tab-bar-select-tab 1))
  (kbd "M-2") (λ-interactive (tab-bar-select-tab 2))
  (kbd "M-3") (λ-interactive (tab-bar-select-tab 3))
  (kbd "M-4") (λ-interactive (tab-bar-select-tab 4))
  (kbd "M-5") (λ-interactive (tab-bar-select-tab 5))
  (kbd "M-6") (λ-interactive (tab-bar-select-tab 6))
  (kbd "M-7") (λ-interactive (tab-bar-select-tab 7)))

(im-make-repeatable winner
  "u" tab-bar-history-back
  "r" tab-bar-history-forward)

(im-make-repeatable window-resize
  "+" enlarge-window
  "-" shrink-window
  ">" enlarge-window-horizontally
  "<" shrink-window-horizontally)

;;;; Misc functions

;;;;; marks integration

;; [[https://github.com/im-marks][marks]] is a grep-like tool for
;; searching org-mode and markdown files. The following snippet
;; provides a ~consult~ support for marks.


(im-leader "cm" #'im-marks-my-docs)

(defun im-consult--marks-builder (paths)
  (lambda (input)
    (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
      (cons
       (list "marks" "--no-color" "--null" "--limit=25" (s-trim arg) (expand-file-name default-directory))
       (apply-partially #'consult--highlight-regexps (list (regexp-quote arg)) t)))))

(defun im-marks (&optional directory)
  "Run `marks'."
  (interactive)
  (let ((default-directory (or directory default-directory)))
    (consult--grep "marks" #'im-consult--marks-builder default-directory "")))

(defun im-marks-my-docs ()
  "Run marks inside `~/Documents/notes'."
  (interactive)
  (im-marks (expand-file-name "~/Documents/notes")))

;;;;; Jira

(im-leader
  "eji" #'im-jira-list-issues
  "ejc" #'im-jira-create-ticket)

;;
;; Install required JIRA client
;;

(use-package jiralib2
  :autoload (jiralib2-assign-issue
             jiralib2-create-issue
             jiralib2-get-issue
             jiralib2-get-issuetypes
             jiralib2-get-users
             jiralib2-session-call
             jiralib2-update-summary-description
             jiralib2-create-issue
             jiralib2-jql-search
             jiralib2-board-issues)
  :custom
  (jiralib2-url ty-jira-url)
  (jiralib2-auth 'basic)
  (jiralib2-user-login-name ty-jira-login)
  (jiralib2-token nil))

;;
;; My completing-read based JIRA utilities
;;

(defvar im-git-main-branch "master"
  "Main branch name.")

(defvar im-jira-projects '("AI" "SAT" "DISP" "LISTI")
  "List of projects that I enrolled in JIRA.")

(defvar im-jira-base-branch "origin/master"
  "Brach to create feature branches from.
Consider using origin/something to create the branch from latest
something.")

(defvar im-jira-story-points-field-name 'customfield_10002
  "Which field describes the story points in JIRA response.")

(defvar im-jira-feature-branch-prefix "feature/"
  "Prefix to prepend feature branch names.")

(defvar im-jira-my-issues-query "Pair-programmer = currentUser() OR assignee = currentUser() OR creator = currentUser() ORDER BY createdDate DESC"
  "Query to find out issues that are assigned to me.")

(defvar im-jira-kanban-board-query
  "project = AI AND (fixVersion in unreleasedVersions() OR fixVersion is EMPTY) AND createdDate >= -2w ORDER BY Rank ASC"
  "Query to get kanban board issues.")

(defvar im-jira-board-ids
  '(("STA" . "2307")
    ("PRA" . "2306"))
  "Interested board ids.
See `im-jira-list-issues'.")

(defun im-jira-open-issue (issue-number)
  "Open given Jira ISSUE-NUMBER."
  (interactive "sIssue: ")
  ;; TODO: Remove the following if I move this functionality to a package/file
  (require 'jiralib2)
  (let ((url (format "%s/browse/%s" jiralib2-url (car (s-split " " issue-number)))))
    (kill-new url)
    (browse-url url)))

(defun im-jira-issue-at-point ()
  (let ((sym (or (thing-at-point 'symbol) "")))
    (when (s-matches? "[a-zA-Z]+-[0-9]+" sym)
      sym)))

(add-to-list 'im-open-thing-at-point-alist '(im-jira-issue-at-point . im-jira-open-issue))

(defmemoize im-jira-get-my-issues ()
  (jiralib2-jql-search im-jira-my-issues-query))

(defun im-jira-get-kanban-issues ()
  (jiralib2-jql-search im-jira-kanban-board-query))

(defun im-jira-get-board-issues ()
  (let ((board-id (if (= 1 (length im-jira-board-ids))
                      (cdar im-jira-board-ids)
                    (cdr (assoc (completing-read "Select board: " im-jira-board-ids) im-jira-board-ids)))))
    (jiralib2-board-issues board-id nil)))

(defun im-jira-jql (jql)
  (interactive (list (read-string "Enter JQL: " "text ~ \"...\" AND statusCategory = \"To Do|In Progress|Done\"")) "sEnter JQL: ")
  (jiralib2-jql-search jql))

(defun im-jira-get-current-sprint-issues (&optional projects)
  "Get current sprint issues for all PROJECTS.
If PROJECTS is nil, then `im-jira-projects' is used."
  (let ((issues '()))
    (mapc
     (lambda (project)
       (setq
        issues
        (thread-last
          (format "project = \"%s\" AND Sprint in openSprints()"
                  project)
          (jiralib2-jql-search)
          (append issues))))
     (or projects im-jira-projects))
    issues))

(defun im-jira-get-new-issues ()
  (let ((issues '()))
    (mapcar
     (lambda (project)
       (setq
        issues
        (thread-last
          (format "project = \"%s\" AND created > -10d"
                  project)
          (jiralib2-jql-search)
          (append issues))))
     im-jira-projects)
    issues))

(defun im-jira-list-issues (&optional arg)
  (interactive "P")
  (im-jira-issue-actions
   (im-completing-read
    "Select ticket: "
    (pcase (completing-read "Issue list: " '("My issues" "Current Sprint" "New issues" "Kanban" "Board" "JQL"))
      ("My issues" (im-jira-get-my-issues))
      ("New issues" (im-jira-get-new-issues))
      ("Current Sprint" (im-jira-get-current-sprint-issues))
      ("Board" (im-jira-get-board-issues))
      ("Kanban" (im-jira-get-kanban-issues))
      ("JQL" (call-interactively #'im-jira-jql)))
    :formatter #'im-jira--format-ticket-name
    :sort? nil)))

(defun im-jira-ticket-to-branch (key summary)
  "Create a new branch from given ISSUE-NAME and switch to it."
  (interactive "sIssue name: ")
  (let ((branch-name (im-jira--create-branch-name-from-ticket (concat key summary))))
    (message "Updating...")
    (unless (= 0 (shell-command "git fetch --all"))
      (user-error "Cannot git fetch --all"))
    (message "Creating branch...")
    (magit-branch-and-checkout branch-name im-jira-base-branch)
    (vc-refresh-state)
    (im-jira-change-issue-status-to-status key "In Progress")
    (message "Currently on %s." (lab-git-current-branch))))

(defun im-jira-create-ticket ()
  (interactive)
  (let ((project (im-jira--select-project))
        (issue-type (im-jira--select-issue-type))
        (summary (read-string "Issue summary: ")))
    (im-get-input
     :init
     (format (concat
              "* %s\n"
              ":PROPERTIES:\n"
              ":PROJECT-ID: %s\n"
              ":ISSUE-TYPE: %s\n"
              ":SPRINT: active|future\n"
              ":END:\n\n"
              (im-jira--get-issue-template issue-type))
             summary project issue-type)
     :pre-process
     (lambda ()
       (goto-char (point-min))
       (list
        :summary (org-entry-get nil "ITEM")
        :project-id (org-entry-get nil "PROJECT-ID")
        :type (org-entry-get nil "ISSUE-TYPE")
        :rest `((,(im-jira-get-issue-field-id-for "Sprint") .
                 ,(alist-get 'id (im-jira-find-sprint (org-entry-get nil "SPRINT")))))))
     :on-accept
     (lambda (description props)
       (setq description
             (->>
              (org-export-string-as description 'confluence t)
              (s-split "\n")
              (-drop 1)
              (s-join "\n")))
       (message ">> (im-jira-create-ticket %s :description %s)" props description)
       (thread-last
         (apply #'jiralib2-create-issue
                `(,(plist-get props :project-id)
                  ,(plist-get props :type)
                  ,(plist-get props :summary)
                  ,description
                  ,@(plist-get props :rest)))
         (im-jira-issue-actions))))))

(defun im-jira-get-issue-fields ()
  (jiralib2-session-call "/rest/api/2/field"))

;; TODO support pagination
(defun im-jira-get-sprints ()
  (alist-get 'values (jiralib2-session-call (format "/rest/agile/1.0/board/%s/sprint" im-jira-board-id))))

(defun im-jira-find-sprint (sprint)
  "Find a sprint.
SPRINT can be a full sprint name or one \"active\"|\"future\"."
  (let ((sprints (im-jira-get-sprints)))
    (or
     (--find (string-equal sprint (alist-get 'name it)) sprints)
     (--find (string-equal sprint (alist-get 'state it)) sprints))))

(defun im-jira-get-issue-field-id-for (field-name)
  (alist-get
   'id
   (--find
    (string-equal field-name (alist-get 'name it))
    (im-jira-get-issue-fields))))

(defun im-jira-get-issue-transitions (issue)
  (alist-get
   'transitions
   (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions?expand=transition.fields" issue))))

(defun im-jira-change-issue-status-to (issue status-id)
  (jiralib2-session-call
   (format "/rest/api/2/issue/%s/transitions?expand=transition.fields" issue)
   :type "POST"
   :data (json-encode
          `((transition (id . ,status-id))))))

(defun im-jira-change-issue-status (key)
  (interactive "sIssue number: ")
  (->>
   (im-completing-read
    "Select status: "
    (im-jira-get-issue-transitions key)
    :formatter (lambda (it) (let-alist it (format "%s [%s]" .name .to.name))))
   (alist-get 'id)
   (im-jira-change-issue-status-to key)))

(defun im-jira-change-issue-status-to-status (issue-id status)
  "Same as `im-jira-change-issue-status-to' but uses the status name as shown in Jira UI instead of status id."
  (im-jira-change-issue-status-to
   issue-id
   (alist-get
    'id
    (--find
     (string-equal (alist-get 'name it) status)
     (im-jira-get-issue-transitions issue-id)))))

;;
;; Utility
;;

(defmemoizefile im-jira-get-users () "~/.emacs.d/jira-user-cache"
  (mapcar
   (lambda (project) (cons project (jiralib2-get-users project)))
   im-jira-projects))

(defun im-jira--select-user ()
  (thread-last
    (im-jira-get-users)
    (assoc-string (im-jira--select-project))
    (cdr)
    (--map (cons (alist-get 'name it) it))
    (im-alist-completing-read "Select a user: ")))

(defun im-jira--select-project ()
  "Interactively select one of enrolled projects."
  (if (eq (length im-jira-projects) 1)
      (car im-jira-projects)
    (completing-read "Select project: " im-jira-projects)))

(defun im-jira--select-issue-type ()
  (completing-read
   "Issue type: "
   (--map
    (let-alist it (cons .name .id))
    (jiralib2-get-issuetypes))))

(defun im-jira--get-issue-template (issue-type)
  (pcase issue-type
    ("Story" "** Motivation\n\n\n** Description\n\n\n** Acceptance Criteria\n\n\n** Projects\n\n\n** Has Automation Test?\n\n\n** Links (UI/UX, Analysis etc.)\n\n")
    ("Sprint Development Bug" "** Description\n\n\n**Case\n\n\n** Projects\n\n")
    ("Production Bug" "** Description\n\n\n** Steps\n\n\n** Projects\n\n\n** Incident Excel\n\n\n** Links - SS - Video\n\n")))

(defun im-jira--create-branch-name-from-ticket (issue-name)
  "Create a branch name from given Jira ISSUE-NAME."
  (thread-last
    issue-name
    (im-string-url-case)
    (s-downcase)
    (im-s-upcase-until "-")
    (s-prepend im-jira-feature-branch-prefix)
    (read-string "Branch name: ")))

;; TODO Color code status etc.
(defun im-jira--format-ticket-name (it)
  "Format ticket name for displaying in `completing-read' window."
  (let-alist it
    (format
     "%-7s\t[%-11s]\t%-15s => %-15s\t%s"
     (propertize .key
                 'face 'bold)
     (propertize (s-truncate 11 .fields.status.name)
                 'face 'italic)
     (propertize (s-truncate 15 (or .fields.reporter.name "N/A"))
                 'face 'italic)
     (propertize (s-truncate 15 (or .fields.assignee.name "N/A"))
                 'face 'italic)
     .fields.summary)))

(defun im-jira-issue-actions (issue)
  (interactive
   (list (jiralib2-get-issue (read-string "Jira issue: " (or (im-jira-issue-at-point) "")))))
  (cl-loop
   (let-alist issue
     (let* ((action
             (im-completing-read
              (format "Act on %s: " (s-truncate 20 .fields.summary))
              '("View" "Open" "Update" "To branch" "To worktree" "Assign to..." "Insert as task" "Change status" "Inspect" "[Cancel]")
              :sort? nil)))
       (pcase action
         ("View"
          (im-jira-view-ticket .key)
          (cl-return))
         ("Open"
          (with-default-browser
           (im-jira-open-issue .key))
          (cl-return))
         ("Update"
          (im-jira-update-ticket .key .fields.summary .fields.description)
          (cl-return))
         ("To branch"
          (im-jira-ticket-to-branch .key .fields.summary)
          (cl-return))
         ("To worktree"
          (im-jira-ticket-to-worktree .key .fields.summary)
          (cl-return))
         ("Assign to..."
          (jiralib2-assign-issue
           .key
           (alist-get 'name (im-jira--select-user))))
         ("Change status"
          (im-jira-change-issue-status .key))
         ("Insert as task"
          (insert (format "** TODO [#A] %s %s :work:" .key .fields.summary)))
         ("Inspect"
          (im-json-encode-and-show issue)
          (cl-return))
         ("[Cancel]"
          (cl-return)))))))

(defun im-convert-jira-markup-to-org-mode (jira-markup)
  "Convert given JIRA-MARKUP string to `org-mode' format."
  (with-temp-buffer
    (insert jira-markup)
    ;; This creates loose lists where newlines appear between
    ;; list items and ox-confluence does not handle this well and
    ;; breaks lists.
    (shell-command-on-region
     (point-min) (point-max)
     "pandoc -f jira -t org --wrap=none"
     nil t)
    ;; So I try to remove those unnecassary new lines here.
    (->>
     (buffer-string)
     ;; Same replacement applied twice.
     (replace-regexp-in-string "^\\([ \t]*\\)-\\(.*\\)\n\n\\([ \t]*\\)-" "\\1-\\2\n\\3-")
     (replace-regexp-in-string "^\\([ \t]*\\)-\\(.*\\)\n\n\\([ \t]*\\)-" "\\1-\\2\n\\3-")
     (replace-regexp-in-string "\\\\\\\\$" "")
     ;; Fix deeply nested bullet points
     (replace-regexp-in-string "---- " "- ")
     ;; Fix - [ ] markers
     (replace-regexp-in-string "- \\*=( )=\\* " "- [ ] ")
     (replace-regexp-in-string "- \\*=(-)=\\* " "- [-] ")
     (replace-regexp-in-string "- \\*=(X)=\\* " "- [X] "))))

(defun im-jira-update-ticket (key summary description)
  (im-get-input
   :init
   (concat
    "* "
    summary
    "\n"
    (im-convert-jira-markup-to-org-mode description))
   :pre-process
   (lambda ()
     (goto-char (point-min))
     (org-entry-get nil "ITEM"))
   :on-accept
   (lambda (description summary)
     (setq description
           (->>
            (org-export-string-as description 'confluence t)
            (s-split "\n")
            (-drop 1)
            (s-join "\n")))
     (message ">> (im-jira-update-ticket \"%s\" \"%s\" \"%s\")" key summary description)
     (jiralib2-update-summary-description key summary description))))

;;
;; jira-view-mode
;;

(defvar-local jira-view-mode-ticket nil
  "Currently viewed ticket object.")
(define-derived-mode jira-view-mode org-mode "JiraView"
  "Mode for viewing JIRA tickets.")

(defun jira-view-mode-open-externally ()
  (interactive nil jira-view-mode)
  (with-default-browser
   (let-alist jira-view-mode-ticket
     (im-jira-open-issue .key))))

(defun jira-view-mode-edit ()
  (interactive nil jira-view-mode)
  (let-alist jira-view-mode-ticket
    (im-jira-update-ticket .key .fields.summary .fields.description)))

(defun jira-view-mode-reload ()
  (interactive nil jira-view-mode)
  (let-alist jira-view-mode-ticket
    (im-jira-view-ticket .key)))

(defun jira-view-mode-act ()
  (interactive nil jira-view-mode)
  (im-jira-issue-actions jira-view-mode-ticket))

(defun jira-view-mode-inspect ()
  (interactive nil jira-view-mode)
  (im-inspect jira-view-mode-ticket))

(evil-define-key 'normal jira-view-mode-map
  (kbd "&") #'jira-view-mode-open-externally
  (kbd "ge") #'jira-view-mode-edit
  (kbd "gr") #'jira-view-mode-reload
  (kbd "gi") #'jira-view-mode-inspect
  ;; TODO All actions might be single keypress.
  (kbd "ga") #'jira-view-mode-act)

(defun im-jira-view-ticket (key)
  (interactive "sIssue key: ")
  (when-let* ((match (s-match "browse/\\([a-zA-Z0-9-]+\\)/?" key)))
    (setq key (nth 1 match)))
  (let ((ticket (jiralib2-get-issue key)))
    (let-alist ticket
      (let ((buffer (get-buffer-create (format "*jira:%s:%s*" key .fields.summary))))
        (unless (eq (current-buffer) buffer)
          (switch-to-buffer-other-window buffer)))
      (erase-buffer)
      (insert
       (concat
        "* "
        key
        " - "
        .fields.summary
        "\n"
        (im-convert-jira-markup-to-org-mode .fields.description)))
      (jira-view-mode)
      (setq header-line-format "Hit `&' to open in browser, `ge' to edit, `gr' to reload, `ga' to see actions.")
      (setq-local jira-view-mode-ticket ticket)
      (goto-char (point-min))
      (org-set-property "STATUS" (or .fields.status.name "N/A"))
      (org-set-property "REPORTER" (or .fields.reporter.name "N/A"))
      (org-set-property "ASSIGNEE" (or .fields.assignee.name "N/A"))
      (org-set-property "STORY_POINTS" (format "%s" (or (alist-get im-jira-story-points-field-name .fields) "N/A")))
      (org-fold-show-all))))

(defun im-jira-list-current-sprint-assignee-swimlane ()
  "Draw a table for the current sprint that resembles assignee swimlanes of JIRA.
It also shows how much story point each assignee has and how much
story points they have released.  See the following figure:

  | Assignee  | Total | Done | Sub-total | Status      | Issue         |
  |-----------+-------+------+-----------+-------------+---------------|
  | someone-1 |   8.0 |  3.0 |           |             |               |
  |           |       |      |       3.0 | Open        | AI-483 - ...  |
  |           |       |      |       3.0 | Done        | AI-423 -  ... |
  |           |       |      |       2.0 | Code Review | AI-488 - ...  |
  |-----------+-------+------+-----------+-------------+---------------|
  | someone-2 |   7.0 |  2.0 |           |             |               |
  |           |       |      |       2.0 | Released    | AI-485 - ...  |
  |           |       |      |       5.0 | In Progress | AI-313 - ...  |"
  (interactive)
  (with-current-buffer (get-buffer-create "*jira: current-sprint-by-points*")
    (erase-buffer)
    (org-mode)
    (org-dblock-write:jira (list :projects im-jira-projects))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun org-dblock-write:jira (params)
  "Dynamic block version of `im-jira-list-current-sprint-assignee-swimlane'."
  (let* ((projects (plist-get params :projects))
         (issues (plist-get params :issues))
         (progress? (plist-get params :progress?))
         (jql (plist-get params :jql))
         (group-by-assignee? (plist-get params :group-by-assignee))
         results)
    (when issues
      (setq jql (format "key IN (%s)" (s-join ", " (mapcar #'symbol-name issues)))))
    (setq results (cond
                   (jql (jiralib2-jql-search jql))
                   (t (im-jira-get-current-sprint-issues projects))))
    (when progress?
      (let ((done (length (--filter (-contains? '("Released" "Done") (let-alist it .fields.status.name)) results)))
            (total (length results)))
        (insert
         (format "- Progress :: %s/%s (%s%%)\n\n" done total (/ (* 100 done) total)))))
    (if group-by-assignee?
        (insert "| Assignee | Total | Done |  Sub-total | Status | Creator  | Issue |\n|-\n")
      (insert "| Assignee | Creator | Point | Status | Sprint | Issue |\n|-\n"))
    (cond
     (group-by-assignee?
      (->>
       results
       (--group-by (let-alist it .fields.assignee.name))
       (map-apply
        (lambda (key vals)
          (list
           :assignee key
           :total (-sum (--map (let-alist it (or (alist-get im-jira-story-points-field-name .fields) 0)) vals))
           :done (->>
                  vals
                  (--filter (-contains? '("Done" "Released") (let-alist it .fields.status.name)))
                  (--map (let-alist it (or (alist-get im-jira-story-points-field-name .fields) 0)))
                  (-sum))
           :tasks
           (->>
            (--map (let-alist it
                     (list
                      :points (alist-get im-jira-story-points-field-name .fields)
                      :summary (format "%s - %s" .key .fields.summary)
                      :status .fields.status.name
                      :creator .fields.creator.name))
                   vals)
            (--sort (string> (plist-get it :status) (plist-get other :status)))))))
       (--sort (> (plist-get it :total) (plist-get other :total)))
       (--map (format "| %s | %s | %s | | | |\n%s"
                      (plist-get it :assignee)
                      (plist-get it :total)
                      (plist-get it :done)
                      (s-join
                       "\n"
                       (--map (format "| | | | %s | %s | %s | %s |"
                                      (plist-get it :points)
                                      (plist-get it :creator)
                                      (plist-get it :status)
                                      (s-truncate 120 (plist-get it :summary)))
                              (plist-get it :tasks)))))
       (s-join "\n|-\n")
       (insert)))
     (t
      (->>
       results
       (--map (let-alist it
                (format "| %s | %s | %s | %s | %s | %s |"
                        .fields.assignee.name
                        .fields.creator.name
                        (alist-get im-jira-story-points-field-name .fields)
                        .fields.status.name
                        (when-let* ((match (s-match "name=\\([^,]+\\)," (or (car .fields.customfield_10004) ""))))
                          (nth 1 match))
                        (s-truncate 120 (format "%s - %s" .key .fields.summary)))))
       (s-join "\n")
       (insert))))
    (org-table-align)))

(defun im-jira-create-quick-issue (arg)
  "Quickly create an issue and act on it.
If ARG is non-nil, insert the issue number to current buffer
instead of acting on issue."
  (interactive "P")
  (let ((issue (jiralib2-create-issue
                (im-jira--select-project)
                (im-jira--select-issue-type)
                (read-string "Issue summary: ")
                "This is an automatically generated small issue."
                (cons
                 (im-jira-get-issue-field-id-for "Sprint")
                 (alist-get 'id (im-jira-find-sprint "future"))))))
    (let-alist issue
      (save-window-excursion
        (im-jira-view-ticket .key)
        (message ">> Opened issue in a buffer."))
      (im-kill .key)
      (if arg
          (insert .key " - ")
        (im-jira-issue-actions issue)))))


;;;;; My Android phone and Emacs

;; I have an Android phone that is running
;; [[https://termux.com/][Termux]] all the time. If you install Termux
;; through [[https://www.f-droid.org/][F-Droid]] you can also install
;; [[https://f-droid.org/en/packages/com.termux.api/][Termux:API]]
;; package which brings bunch of commands like =termux-clipboard-set=,
;; =termux-sms-list= etc. Much of the commands requires to be called
;; in foreground, so they are not very useful over SSH but you can
;; work around that by starting a =tmux= session on the phone and
;; executing commands on that tmux session through SSH. This way I can
;; send arbitrary text to my phones clipboard using the commands
;; below.


(im-leader
  "ept" #'im-send-text-to-my-phone
  "epc" #'im-send-clipboard-to-my-phone)

(defvar im-phone-hostname
  "f3"
  "Hostname or local address to connect to my phone by SSH.")

(defun im-send-termux-command-async (cmd fn &rest args)
  "Send CMD to my phone.
When CMD finishes, FN is called with the process output."
  (interactive "sText: ")
  (let (output
        (txt
         (concat cmd " " (s-join "" (--map (concat "\"" (s-replace "\"" "\\\"" it) "\"" " ") args))))
        (proc (start-process
               "*im-termux-cmd*"
               nil
               "ssh"
               "-T"
               im-phone-hostname)))
    (set-process-filter proc (lambda (proc out) (setq output (concat output out))))
    (set-process-sentinel proc (lambda (proc event) (funcall fn output)))
    (process-send-string proc txt)
    (process-send-eof proc)
    (process-send-eof proc)
    proc))

(defun im-send-termux-command (cmd &rest args)
  "Send CMD to my phone."
  (interactive "sText: ")
  (with-temp-buffer
    (insert cmd " ")
    (--each args
      (insert "\"" (s-replace "\"" "\\\"" it) "\"" " "))
    (shell-command-on-region
     (point-min)
     (point-max)
     (format "ssh -T %s" im-phone-hostname)
     t t)
    (buffer-string)))

(defun im-send-text-to-my-phone (text)
  "Send TEXT to my phones clipboard.
This only works if the phone is already open."
  (interactive "sText: ")
  (im-send-termux-command-async "termux-clipboard-set" (lambda (_) (message ">> Text sent: %s." text)) text))

(defun im-send-clipboard-to-my-phone ()
  "Send current clipboard content to my phones clipboard.
This only works if the phone is already open."
  (interactive)
  (im-send-text-to-my-phone (current-kill 0)))

(defun im-list-phone-text-messages ()
  "List messages from my phone."
  (interactive)
  (let ((result (completing-read
                 "Messages: "
                 (seq-map
                  (lambda (msg) (let-alist msg (format "%s :: %s" .number .body)))
                  (json-read-from-string
                   (im-send-termux-command "termux-sms-list"))))))
    (switch-to-buffer-other-window (get-buffer-create "*im-message*"))
    (insert result)))

;;;;; people.org - Contact management

;; Please see
;; [[https://isamert.net/2021/04/21/managing-your-contacts-in-org-mode-and-syncing-them-to-your-phone-android-ios-whatever-.html][this
;; post]] for further information.


(defun im-contacts--build-contact-item (template-string contact-property)
  (cond
   ((s-contains? "$1" contact-property)
    (->>
     (org-entry-properties)
     (--filter (s-matches? (s-replace "$1" ".*" contact-property) (car it)))
     (--map (let ((subprop (nth 1 (s-match (s-replace "$1" "\\([a-zA-Z0-9_-]+\\)" contact-property) (car it)))))
              (concat (format (s-replace "$1" subprop template-string) (cdr it)) "\n")))))
   (t (if-let* ((stuff (org-entry-get nil contact-property)))
          (concat (format template-string stuff) "\n")
        ""))))

(defun im-contacts--contact-entry? ()
  (thread-last
    (org-entry-properties)
    (-map #'car)
    (--any (s-prefix? "PHONE" it))))

(defun im-contacts-build-vcard-for-heading ()
  "Build a VCARD for the current heading and return it as string."
  (when (im-contacts--contact-entry?)
    (string-join
     `("BEGIN:VCARD\nVERSION:4.0\nPRODID:ez-vcard 0.11.3\n"
       ,(format "UID:urn:uuid:%s\n" (org-id-get nil t))
       ,(im-contacts--build-contact-item "FN:%s" "ITEM")
       ,(if (org-entry-get nil "DISPLAY_NAME")
            (im-contacts--build-contact-item "N:;%s;;;" "DISPLAY_NAME")
          (im-contacts--build-contact-item "N:;%s;;;" "ITEM"))
       ,@(im-contacts--build-contact-item "TEL;TYPE=$1:%s" "PHONE_$1")
       ,@(im-contacts--build-contact-item "EMAIL;TYPE=$1:%s" "EMAIL_$1")
       ,@(im-contacts--build-contact-item "ADR;TYPE=$1:;;%s;;;;" "ADDRESS_$1")
       ,(im-contacts--build-contact-item "ORG:%s" "GROUP")
       ,(im-contacts--build-contact-item "CATEGORIES:%s" "GROUP")
       ,(thread-last
          (im-contacts--build-contact-item "BDAY:%s" "SCHEDULED")
          (s-replace-regexp "BDAY:<[0-9]\\{4\\}\\(.*\\) \\(\\w+\\)>" "BDAY:1900\\1")
          (s-replace "-" ""))
       ;; vcard supports multiple anniversaries but there is no
       ;; way to name them as far as I understand. So I just
       ;; drop anniversary names and add all of them.
       ,@(thread-last
           (im-contacts--build-contact-item "ANNIVERSARY:%s" "ANNIVERSARY_$1")
           (--map (->>
                   it
                   (s-replace-all '((" " . "") ("-" . "") ("[" . "") ("]" . "") ("<" . "") (">" . "")))
                   (s-trim)
                   (s-replace-regexp "[a-zA-Z]+$" "\n"))))
       ;;,(format "REV:%s\n" (format-time-string "%Y-%m-%dT%T"))
       ;; TODO: add notes
       ;; Be careful while exporting and sending contact to other people
       ,(when-let* ((image-base64
                     (-some->>
                         (org-agenda-get-some-entry-text (point-marker) most-positive-fixnum)
                       (substring-no-properties)
                       (s-match "^\\[\\[file:\\(.+\\(\\.jpg\\|jpeg\\|png\\)\\)]]")
                       (nth 1)
                       (im-encode-image-base64))))
          (format "PHOTO:%s\n" image-base64))
       "TITLE:\nEND:VCARD")
     "")))

(defun im-contacts-export-as-vcard (&optional file-name)
  "Create a .vcf FILE-NAME containing all contact information.
I use \"Simple Contacts Pro\" to import contacts to my phone."
  (interactive
   (list
    (read-file-name
     "Where to save the .vcf file?"
     "~/Documents/sync/"
     "contacts.vcf")))
  (write-region
   (thread-last
     (org-map-entries #'im-contacts-build-vcard-for-heading "LEVEL=1")
     (-non-nil)
     (s-join "\n"))
   nil
   file-name))

;; Also see im-nextcloud.el/Contacts

;;;;; people.org - Quick info selection

(defun im-people ()
  "Select and kill people info.
people.org should contain the following snippet on it's `after-save-hook':
  (im-serialize-into-file \"~/.emacs.d/sync/people\"
    (org-map-entries #'org-entry-properties \"LEVEL=1\"))"
  (interactive)
  (let* ((selected
          (->>
           (im-completing-read
            "Select info"
            (-mapcat
             (lambda (info)
               (--map
                (list
                 :format
                 (format
                  "%s >> %s → %s"
                  (propertize (alist-get "ITEM" info nil nil #'equal) 'face '(:weight bold))
                  (propertize (car it) 'face '(:foreground "yellow"))
                  (propertize (cdr it) 'face '(:weight bold :foreground "green")))
                 :item it :info info)
                (--filter
                 (and
                  (not (-contains? '("PRIORITY" "CATEGORY") (car it)))
                  (not (s-blank? (cdr it))))
                 info)))
             (im-deserialize-from-file "~/.emacs.d/sync/people"))
            :formatter (lambda (it) (plist-get it :format))
            :initial (im-region-or nil))))
         (prop (car (plist-get selected :item)))
         (val (cdr (plist-get selected :item)))
         (info (cdr (plist-get selected :info))))
    (im-kill val)
    (cond
     ;; Insert link to person if we are in org-mode
     ((and (frame-focus-state)
           (derived-mode-p 'org-mode)
           (equal prop "ID"))
      (insert (format "[[id:%s][%s]]"
                      val
                      (if (use-region-p)
                          (let* ((beg (region-beginning))
                                 (end (region-end))
                                 (str (buffer-substring-no-properties beg end)))
                            (delete-region beg end)
                            str)
                        (s-trim (read-string "Name: " (alist-get "ITEM" info nil nil #'equal)))))))
     ;; Strip out links from addresses
     ((s-prefix? "ADDRESS" prop)
      (if-let* ((x (s-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" val)))
          (progn
            ;; Put them both in clipboard
            (im-kill (car x))
            (im-kill (nth 2 x)))
        (im-kill val)))
     (t (im-kill val)))))

(im-leader-v
  "ou" #'im-people)

;;;;; Google search

(defun im-google-this (input)
  "Search INPUT in Google.
Interactively, INPUT is either region or current word.  Let's you edit
the query beforehand."
  (interactive
   (list
    (read-string
     "Googling: "
     (im-region-or 'word))))
  (browse-url
   (concat
    eww-search-prefix
    input)))

(im-leader-v "mg" #'im-google-this)

;;;;; bullet.org

;; I have a ~bullet.org~ file that I keep my daily journal and here
;; are some utility functions that use with it. I may document this
;; later (or even publish it as a package?)

;; Keybindings

(im-leader
  "ob" 'im-bullet-transient)

(transient-define-prefix im-bullet-transient ()
  "IM Bullet Focus Commands"
  [["Focus"
    ("n" "Non-day header" im-bullet-focus-non-day-header)
    ("y" "Yesterday" im-bullet-focus-yesterday)
    ("Y" "Yesterday indirect" im-bullet-focus-yesterday-indirect)
    ("t" "Today" im-bullet-focus-today)
    ("T" "Tomorrow" im-bullet-focus-tomorrow)
    ("f" "Given day" im-bullet-focus-given-day)
    ("r" "Recurring" im-bullet-focus-recurring)
    ("R" "Recurring indirect" im-bullet-focus-recurring-indirect)
    ("w" "Work" im-bullet-focus-work)
    ("W" "Work indirect" im-bullet-focus-work-indirect)
    ("l" "Life" im-bullet-focus-life)
    ("L" "Life indirect" im-bullet-focus-life-indirect)
    ("c" "Computer" im-bullet-focus-computer)
    ("C" "Computer indirect" im-bullet-focus-computer-indirect)
    ("i" "Inbox" im-bullet-focus-inbox)
    ("I" "Inbox indirect" im-bullet-focus-inbox-indirect)]
   ["Linking"
    ("b" "Backlinks" im-org-link-find-backlinks)]])

;;
;; Date utils
;;

(defun im-today ()
  "Get todays date in format YYYY-MM-DD Day."
  (format-time-string "%Y-%m-%d %a"))

(defun im-u ()
  "Get day of week as number."
  (string-to-number (format-time-string "%u")))

(defun im-d ()
  "Get day of month as number."
  (string-to-number (format-time-string "%d")))

(defun im-V ()
  "Get week number."
  (string-to-number (format-time-string "%V")))

(defun im-date (date &optional str)
  "Display the time described by DATE.
DATE can be 'now', 'yesterday', 'two days ago' etc."
  (s-trim-right
   (shell-command-to-string
    (concat (locate-file "date" exec-path) " --date='" date "' +'" (or str "%Y-%m-%d %a") "'"))))

;;
;; Create utils
;;

(defun im-bullet-create-a-day (date)
  "Create given DATE heading in bullet.org in the appropriate place..
DATE should be in the form of YYYY-MM-DD."
  (when-let* ((point (im-bullet-find-a-day date)))
    (goto-char point)
    (user-error "The day already exists"))
  (widen)
  (goto-char (point-max))
  (cl-loop
   (when (not (re-search-backward "^* \\[\\([0-9]+-[0-9]+-[0-9]+\\)\\( \\w+\\)?\\].*" nil t))
     (cl-return))
   (when (time-less-p
          (date-to-time (concat (match-string 1) "T000"))
          (date-to-time (concat date "T000")))
     (org-insert-heading-after-current)
     (insert (format "[%s%s] [%%]"
                     date
                     (if (s-matches? "[a-zA-Z]+$" date)
                         ""
                       (format " %s" (format-time-string "%a" (date-to-time (concat date "T000")))))))
     (org-narrow-to-subtree)
     (cl-return))))

;;
;; Focus a day
;;

(defun im-bullet-find-a-day (day)
  (save-excursion
    (widen)
    (goto-char (point-max))
    (when (re-search-backward (concat "^* \\[" day "\\( \\w+\\)?\\]") nil t)
      (point-marker))))

(defun im-bullet-focus-a-day (day)
  "Focus to given DAY."
  (im-bullet-org-ensure)
  (widen)
  (when-let* ((day-entry (im-bullet-find-a-day day)))
    (goto-char day-entry)
    (beginning-of-line)
    (org-narrow-to-subtree)
    (im-show-outline-only)
    t))

(defun im-bullet-focus-given-day (date)
  "Focus given DATE's header.
If it does not exists, create it."
  (interactive
   (list (org-read-date)))
  (when (not (im-bullet-focus-a-day date))
    (im-bullet-create-a-day date)))

(defun im-bullet-focus-today ()
  "Focus todays header.
If it does not exists, create it."
  (interactive)
  (let ((today (if (<= (string-to-number (format-time-string "%H")) 3)
                   (im-date "yesterday")
                 (format-time-string "%Y-%m-%d"))))
    (when (not (im-bullet-focus-a-day today))
      (im-bullet-create-a-day today))))

(defun im-bullet-focus-tomorrow ()
  "Focus yesterdays header."
  (interactive)
  (let ((tomorrow (im-date "tomorrow")))
    (when (not (im-bullet-focus-a-day tomorrow))
      (im-bullet-create-a-day tomorrow))))

(defun im-bullet-focus-tomorrow-indirect ()
  "Like `im-bullet-focus-tomorrow' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-tomorrow)))

(defun im-bullet-focus-yesterday ()
  "Focus yesterdays header."
  (interactive)
  (im-bullet-focus-a-day (im-date "yesterday")))

(defun im-bullet-focus-yesterday-indirect ()
  "Like `im-bullet-focus-yesterday' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-yesterday)))

(defun im-bullet-focus-heading (heading)
  (interactive)
  (im-bullet-org-ensure)
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (format "^*+ %s" heading))
    (beginning-of-line)
    (org-narrow-to-subtree)
    (im-show-outline-only)
    t))

(defun im-bullet-focus-recurring ()
  (interactive)
  (im-bullet-focus-heading "Recurring"))

(defun im-bullet-focus-recurring-indirect ()
  "Like `im-bullet-focus-recurring' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-recurring)))

(defun im-bullet-focus-work ()
  (interactive)
  (im-bullet-focus-heading "Work backlog"))

(defun im-bullet-focus-work-indirect ()
  "Like `im-bullet-focus-work' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-work)))

(defun im-bullet-focus-life ()
  (interactive)
  (im-bullet-focus-heading "Life backlog"))

(defun im-bullet-focus-life-indirect ()
  "Like `im-bullet-focus-life' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-life)))

(defun im-bullet-focus-computer ()
  (interactive)
  (im-bullet-focus-heading "Computer backlog"))

(defun im-bullet-focus-computer-indirect ()
  "Like `im-bullet-focus-computer' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-computer)))

(defun im-bullet-focus-inbox ()
  (interactive)
  (im-bullet-focus-heading "Inbox"))

(defun im-bullet-focus-inbox-indirect ()
  "Like `im-bullet-focus-inbox' but in an indirect buffer."
  (interactive)
  (im-org-focused-tree-to-indirect-buffer
   (im-bullet-org-ensure)
   (im-bullet-focus-inbox)))

;;
;; Focus non-day
;;

(defun im-bullet-focus-non-day-header ()
  "Interactively select and focus a non-day header."
  (interactive)
  (save-match-data
    (widen)
    (goto-char 0)
    (re-search-forward "^\\*+ \\[[0-9]+-" nil t)
    (narrow-to-region (point-min) (point-at-bol))
    (consult-org-heading)
    (org-narrow-to-subtree)))

;;
;; Template utils
;;

(defun im-bullet-current-date ()
  "Return current date."
  (save-restriction
    (save-excursion
      (widen)
      (when (re-search-backward "^* \\[202" nil t)
        (-some->> (org-get-heading t t t t)
          (s-match "\\[.*?\\]")
          (car)
          (substring-no-properties)
          (s-chop-prefix "[")
          (s-chop-suffix "]"))))))

(defmacro im-when-weekday (template &rest template-args)
  `(when (<= (im-u) 5)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-weekday-n (n template &rest template-args)
  `(when (= (im-u) ,n)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-monthday-n (n template &rest template-args)
  `(when (= (im-d) ,n)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-weekend (template &rest template-args)
  `(when (> (im-u) 5)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-saturday (template &rest template-args)
  `(when (= (im-u) 6)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-sunday (template &rest template-args)
  `(when (= (im-u) 7)
     (s-trim (im-s-interpolated ,template ,@template-args))))

(defmacro im-when-monday (template &rest template-args)
  `(when (= (im-u) 1)
     (s-trim (im-s-interpolated ,template ,@template-args))))

;;
;; Daily summary
;;

;; Implementation is a bit cumbersome but it's easy to adopt for my ad-hoc requests
(defun org-dblock-write:daily-summary (params)
  "Create a daily summary for my bullet.org."
  (let* ((items (org-map-entries
                 (lambda nil
                   (list
                    :name
                    (org-entry-get nil "ITEM")
                    :clock
                    (org-clock-sum-current-item)
                    :tags
                    (or (org-get-tags) '(empty))
                    :level
                    (org-current-level)
                    :parent
                    (save-excursion
                      (when (org-up-heading-safe)
                        (org-entry-get nil "ITEM")))))
                 "LEVEL>1"))
         (total-time (->>
                      items
                      (--filter (= (plist-get it :level) 2))
                      (--map (plist-get it :clock))
                      (-sum)
                      (org-minutes-to-clocksum-string)))
         (routines (--filter (s-equals? (plist-get it :parent) "Routines") items))
         (work (--filter (-contains? (plist-get it :tags) "work") items))
         (others (--filter (not (or
                                 (-contains? (plist-get it :tags) "work")
                                 (s-equals? (plist-get it :parent) "Routines")
                                 (s-equals? (plist-get it :name) "Routines"))) items)))

    (insert "| [Event] | [Duration] ||\n")
    (insert "|-|\n")

    (->>
     items
     (--find (s-equals? "Routines" (plist-get it :name)))
     (funcall (-flip #'plist-get) :clock)
     (org-minutes-to-clocksum-string)
     (format "|Routines|%s||\n")
     (insert))

    (->>
     routines
     (--filter (-contains? '("Breakfast" "Dinner") (plist-get it :name)))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Eating||%s|\n")
     (insert))

    (->>
     routines
     (--filter (not (-contains? '("Breakfast" "Dinner") (plist-get it :name))))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Other||%s|\n")
     (insert))

    (insert "|-|\n")

    (->>
     work
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|Work|%s||\n")
     (insert))

    (->>
     work
     (--filter (-contains? (plist-get it :tags) "meeting"))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Meetings||%s|\n")
     (insert))

    (->>
     work
     (--filter (not (-contains? (plist-get it :tags) "meeting")))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Other||%s|\n")
     (insert))

    (insert "|-|\n")

    (->>
     others
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|Other|%s||\n")
     (insert))

    (->>
     others
     (--filter (-contains? (plist-get it :tags) "side"))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Side projects||%s|\n")
     (insert))

    (->>
     others
     (--filter (not (-contains? (plist-get it :tags) "side")))
     (--map (plist-get it :clock))
     (-sum)
     (org-minutes-to-clocksum-string)
     (format "|\\-- Other||%s|\n")
     (insert))

    (insert "|-|\n")
    (insert (format "|Total|%s||\n" total-time))
    (delete-char 1)
    (org-table-align)))

;;
;; Misc utils
;;

(defun im-bullet-schedule-all-today ()
  "Schedule all level-2 org mode tasks today's date.
This does not change tasks time.  If the task is not scheduled,
schedules them to today's date."
  (interactive)
  (unless (buffer-narrowed-p)
    (user-error ">> Buffer needs to be narrowed for this!"))
  (let* ((today (save-excursion
                  (goto-char (point-min))
                  (let ((line (thing-at-point 'line)))
                    (if (string-match "\\[\\([^]]+\\)\\]" line)
                        (match-string 1 line)
                      (user-error ">> Not on a bullet heading!")))))
         (today-time (date-to-time today)))
    (org-map-entries
     (lambda ()
       (let ((entry-date (org-entry-get nil "SCHEDULED")))
         (when (and
                (org-entry-get nil "TODO")
                (if entry-date
                    (time-less-p
                     (date-to-time entry-date)
                     today-time)
                  t))
           (org-schedule nil
                         (if-let* ((entry-date)
                                   (hour-min (nth 2 (s-split " " entry-date))))
                             (concat today " " hour-min)
                           today)))))
     "LEVEL=2")))

(defun im-bullet-open-weekly-plan ()
  "Open weekly planning entry in an indirect buffer."
  (interactive)
  (with-current-buffer (find-buffer-visiting bullet-org)
    (save-restriction
      (save-excursion
        (when (or
               (ignore-errors
                 (org-link-open
                  `(link
                    (:type "fuzzy"
                     :path ,(format "W%s" (im-V))
                     :application nil :search-option nil :begin 0 :end 0))))
               ;; Try one week earlier
               (ignore-errors
                 (org-link-open
                  `(link
                    (:type "fuzzy"
                     :path ,(format "W%s" (1- (im-V)))
                     :application nil :search-option nil :begin 0 :end 0)))))
          (im-org-tree-to-indirect-buffer))))))

(defun im-bullet-open-monthly-plan ()
  "Open monthly planning entry in an indirect buffer."
  (interactive)
  (with-current-buffer (find-buffer-visiting bullet-org)
    (save-restriction
      (save-excursion
        (when (or
               (ignore-errors
                 (org-link-open
                  `(link
                    (:type "fuzzy"
                     :path ,(format "%s" (format-time-string "%B"))
                     :application nil :search-option nil :begin 0 :end 0))))
               ;; Try one month earlier
               (ignore-errors
                 (org-link-open
                  `(link
                    (:type "fuzzy"
                     :path ,(format "%s" (format-time-string "%B" (time-subtract (current-time) (days-to-time 30))))
                     :application nil :search-option nil :begin 0 :end 0)))))
          (im-org-tree-to-indirect-buffer))))))

(defun im-listen-voice-recordings ()
  (interactive)
  (mapcar #'empv-enqueue (file-expand-wildcards "~/Audio/watch-recordings/*/*.m4a")))

(defun im-bullet-org-ensure ()
  (if-let* ((buffer (find-buffer-visiting bullet-org)))
      (switch-to-buffer buffer)
    (find-file bullet-org)))

(defun im-org-suggest-filename (&optional path ext)
  "Intelligently suggest a filename for the current context."
  (let ((fname (if path (f-filename path) "")))
    (if (and (equal
              (expand-file-name (or (buffer-file-name (buffer-base-buffer (current-buffer))) "NOT_A_FILE"))
              (expand-file-name bullet-org))
             (not (s-prefix? "bullet_" fname)))
        (f-join
         (format "bullet_%s_%s%s"
                 (ignore-errors
                   (car (s-split
                         " "
                         (im-bullet-current-date))))
                 (if ext
                     (s-chop-suffix (f-ext fname) fname)
                   fname)
                 (if ext
                     ext
                   "")))
      fname)))

;;;;; Scratch/temporary buffers in side windows

;; Here I define two important functions and their helpers.  The
;; following functions are pretty useful for quick note taking or
;; evaluating elisp. Having these buffers in a side window makes them
;; immune to some window commands which is what I want.

;; - im-display-side-scratch-buffer :: This one opens (or closes if it's open) the *scratch* buffer in a side window, on the right. This is nice for quickly evaluating elisp, taking elisp related notes.
;; - im-display-side-temp-org-buffer :: This one opens (or closes if it's open) the ~temp.org~ file in a in a side window, on the right. This is nice for taking some quick notes, writing some temporary todos etc.

(defun im-buffer-visible-p (buffer)
  "Check if given BUFFER is visible or not.  BUFFER is a string representing the buffer name."
  (or (eq buffer (window-buffer (selected-window))) (get-buffer-window buffer)))

(defun im-display-buffer-other-frame ()
  "Like `display-buffer-other-frame' but with some sensible defaults."
  (interactive)
  (let ((default-frame-alist '((tab-bar-lines . 0) (vertical-scroll-bars))))
    (tab-line-mode -1)
    (display-buffer-other-frame (current-buffer))))

(defun im-display-buffer-in-side-window (buffer &optional width)
  "Just like `display-buffer-in-side-window` but only takes a BUFFER and rest of the parameters are for my taste."
  (set-window-dedicated-p
   ;; ^ Setting this to nil so that `pop-to-buffer-same-window' calls works in this window
   ;;   otherwise it'll set `window-dedicated-p' to `side' and this will cause `pop-to-buffer-same-window'
   ;;   to open stuff in another window.
   (select-window
    (display-buffer-in-side-window
     buffer
     `((side . right)
       (slot . 0)
       (window-width . ,(or width 84))
       (window-parameters
        (no-delete-other-windows . t)
        (no-other-window . nil)))))
   nil))

(defun im-remove-window-with-buffer (the-buffer-name)
  "Remove window containing given THE-BUFFER-NAME."
  (mapc (lambda (window)
          (when (string-equal (buffer-name (window-buffer window)) the-buffer-name)
            (delete-window window)))
        (window-list (selected-frame))))

(defun im-toggle-side-buffer-with-file (file-path)
  "Toggle FILE-PATH in a side buffer.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (let ((fname (file-name-nondirectory file-path)))
    (if (im-buffer-visible-p fname)
        (im-remove-window-with-buffer fname)
      (im-display-buffer-in-side-window
       (save-window-excursion
         (find-file file-path)
         (current-buffer))))))

(defun im-toggle-side-buffer-with-name (buffer-name)
  "Hide/show given BUFFER-NAME in a side window."
  (interactive)
  (if (im-buffer-visible-p buffer-name)
      (im-remove-window-with-buffer buffer-name)
    (im-display-buffer-in-side-window (get-buffer buffer-name))))

(defun im-toggle-side-scratch-buffer ()
  "Toggle the scratch buffer in side window.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (im-toggle-side-buffer-with-file "~/.emacs.d/scratch.el"))

(defun im-toggle-side-temp-org-buffer ()
  "Toggle `temp.org` in a side buffer for quick note taking.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (im-toggle-side-buffer-with-file temp-org))

(defun im-toggle-side-bullet-org-buffer ()
  "Toggle `bullet.org` in a side buffer for quick note taking.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (im-toggle-side-buffer-with-file bullet-org))

(defun im-toggle-side-projects-buffer ()
  "Toggle `projects.org` in a side buffer for quick note taking.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (im-toggle-side-buffer-with-file projects-org))

(defun im-toggle-side-messages-buffer ()
  "Toggle `projects.org` in a side buffer for quick note taking.
The buffer is opened in side window so it can't be accidentaly removed."
  (interactive)
  (im-toggle-side-buffer-with-name "*Messages*"))

(defun im-toggle-side-tmr-buffer ()
  (interactive)
  (im-toggle-side-buffer-with-name (get-buffer-create "*tmr-tabulated-view*"))
  (tmr-tabulated-mode))

(defun im-toggle-side-gpt-buffer ()
  (interactive)
  (im-toggle-side-buffer-with-file gpt-org))

(defun im-toggle-side-clocked-header ()
  "Toggle currently clocked header in a narrowed side buffer.
The created buffer is an indirect buffer.  So the narrowing of the
original buffer is not changed.  This let's you simultaneously work on
more than one header of a single org buffer."
  (interactive)
  (if (org-clocking-p)
      (progn
        (im-toggle-side-buffer-with-name
         (save-window-excursion
           (im-org-clocked-header-to-indirect-buffer)
           (buffer-name)))
        (org-fold-show-all))
    (user-error "No active clock")))

;; Toggle temproary buffers
(im-leader
  "ts" 'im-toggle-side-scratch-buffer
  "to" 'im-toggle-side-temp-org-buffer
  "th" 'im-toggle-side-bullet-org-buffer
  "tg" 'im-toggle-side-gpt-buffer
  "tH" 'im-toggle-side-clocked-header
  "tp" 'im-toggle-side-projects-buffer
  "tt" 'im-toggle-side-tmr-buffer
  "tm" 'im-toggle-side-messages-buffer)

;;;;; Scratch project management

(defvar im-scratch-project-path "~/workspace/projects/personal/scratch")

(defun im-new-scratch-file ()
  "Create a new scratch file in `im-scratch-project-path' with selected major mode."
  (interactive)
  (let* ((mode (completing-read
                "Select major mode: " obarray
                (lambda (x)
                  (and (fboundp x)
                       (commandp x)
                       (string-match "-mode$" (symbol-name x))))
                t
                nil nil
                (format "%s" major-mode)))
         (extensions '((python-ts-mode . ".py")
                       (js-ts-mode . ".js")
                       (typescript-ts-mode . ".ts")
                       (java-ts-mode . ".java")
                       (emacs-lisp-mode . ".el")
                       (org-mode . ".org")
                       (markdown-mode . ".md")
                       (go-ts-mode . ".go")
                       (bash-ts-mode . ".sh")))
         (mode-symbol (intern mode))
         (date (format-time-string "%Y-%m-%d")))
    (find-file (make-temp-file
                (concat im-scratch-project-path "/adhoc/" date "_")
                nil
                (alist-get mode-symbol extensions ".txt")))
    (funcall mode-symbol)))

(defun im-toggle-side-scratch-buffer-typescript ()
  "Toggle the scratch buffer in side window."
  (interactive)
  (im-toggle-side-buffer-with-file (concat im-scratch-project-path "/" "scratch.ts")))

;;;;; org-babel extension functions

(defun im-org-babel-remove-all-results nil
  (interactive)
  (goto-char 1)
  (let ((total-removed 0))
    (while (org-babel-next-src-block)
      (when (org-babel-remove-result)
        (setq total-removed (+ total-removed 1))))
    (message (format "%d result blocks are removed." total-removed))))

;;;;; Functions for easy indentation switching

;; - http://blog.binchen.org/posts/easy-indentation-setup-in-emacs-for-web-development.html

(defun im-setup-indent-local (n)
  "Set indent for current buffer."
  (interactive "nHow many spaces do you want? ")
  (setq-local tab-width n)
  (setq-local c-basic-offset n)
  (setq-local sh-basic-offset n)
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local java-ts-mode-indent-offset n)
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  (setq-local typescript-indent-level n) ; typescript-mode
  (setq-local typescript-ts-mode-indent-offset n)
  (setq-local java-ts-mode-indent-offset n)
  (setq-local go-ts-mode-indent-offset n)
  (message "OK!"))

(defun im-setup-indent-global (n)
  "Set indent globally."
  (interactive "nHow many spaces do you want? ")
  (setq tab-width n)
  (setq c-basic-offset n)
  (setq sh-basic-offset n)
  (setq coffee-tab-width n) ; coffeescript
  (setq java-ts-mode-indent-offset n)
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  (setq typescript-indent-level n) ; typescript-mode
  (setq typescript-ts-mode-indent-offset n)
  (setq java-ts-mode-indent-offset n)
  (setq go-ts-mode-indent-offset n)
  (message "OK!"))

;;;;; Current file functionality

(im-leader-v
  "f" #'im-file-transient)

(transient-define-prefix im-file-transient ()
  [["File Operations"
    ("s" "Save buffer" save-buffer)
    ("r" "Rename file & buffer" im-rename-current-file-name-and-buffer)
    ("D" "Delete file" im-delete-current-file)
    ("i" "Show file info" im-print-buffer-file-info)]
   ["Copy Path"
    ("c" "Copy path (pretty)" im-copy-current-filename)
    ("C" "Copy path (raw)" (lambda () (interactive) (im-copy-current-filename :pretty nil)))]
   ["Navigate"
    ("o" "Open file" find-file)
    ("d" "Open directory" consult-dir)
    ("h" "Dirvish sidebar" dirvish-side)
    ("b" "Switch buffer" switch-to-buffer)]
   ["Search/Index"
    ("l" "Search lines in file" consult-line)
    ("L" "Search lines in project" consult-line-multi)
    ("m" "Jump to symbol in file" consult-imenu)
    ("M" "Jump to symbol in project" consult-imenu-multi)]])

;; Sometimes I just want to delete/rename/move etc. the current file
;; without resorting to dired or any other file manager. Here are some
;; interactive functions to do that.

;; Slightly modified from:
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defalias 'im-rename-this-file-name-and-buffer #'im-rename-current-file-name-and-buffer)
(defun im-rename-current-file-name-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (cond
   ;; Rename file at point if we have one
   ((and (derived-mode-p 'org-mode)
         (equal "file" (org-element-property :type (org-element-context))))
    (im-org-rename-file-at-point))
   ;; Otherwise rename current buffer
   (t (let* ((new-name (read-file-name "New name: " nil nil nil (f-filename (buffer-file-name))))
             (name (buffer-name))
             (filename (buffer-file-name)))
        (unless filename
          (user-error "Buffer '%s' is not visiting a file!" name))
        (when (get-buffer new-name)
          (user-error "A buffer named '%s' already exists!" new-name))
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defalias 'im-delete/remove-this-file #'im-delete-current-file)
;; Slightly modified version of: http://www.ergoemacs.org/emacs/elisp_delete-current-file.html
(defun im-delete-current-file ()
  "Delete the current file and copy it's content to `kill-ring'."
  (interactive)
  (when (y-or-n-p (format "Do you really want to remove this: \"%s\"?" (buffer-file-name)))
    (kill-new (buffer-string))
    (message "Buffer content copied to kill-ring.")
    (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (delete-file (buffer-file-name))
      (message "Deleted file: 「%s」." (buffer-file-name)))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defalias 'im-copy-current-file-path 'im-copy-current-filename)
(cl-defun im-copy-current-filename (&key uri pretty)
  "Copy the current buffer file name to the clipboard.
If the URI is non-nil, then add file:// in front of the file-path."
  (interactive
   (list :pretty t))
  (let* ((fname (if (equal major-mode 'dired-mode)
                    default-directory
                  (buffer-file-name)))
         (filename (cond
                    ((and fname uri)
                     (concat "file://" fname))
                    ((and fname pretty)
                     (string-replace (expand-file-name "~") "~" fname))
                    (fname fname)
                    (t (user-error ">> No filename associated with buffer!")))))
    (message ">> Copied: '%s'" (im-kill (or filename (buffer-name))))))

(defun im-print-buffer-file-info (&optional kill-file-path)
  (interactive "P")
  (cl-flet ((prop (x) (propertize x 'face '(:foreground "systemPurpleColor" :slant italic)))
            (plum (x) (propertize x 'face '(:foreground "systemPurpleColor")))
            (bold (x) (propertize x 'face 'bold))
            (yellow (x) (propertize (if (numberp x) (number-to-string x) x) 'face '(:foreground "systemOrangeColor")))
            (italic (x) (propertize x 'face '(:slant italic))))
    (let* ((region? (use-region-p))
           (point-min (if region? (region-beginning) (point-min)))
           (point-max (if region? (region-end) (point-max)))
           (proj-name (im-current-project-name))
           (proj-path (or (im-current-project-root) (expand-file-name "~/")))
           (fpath (or (buffer-file-name) (buffer-name)))
           (fpath-pretty (f-abbrev fpath))
           (fpath-true buffer-file-truename)
           (in-git? (file-exists-p (expand-file-name ".git" (im-current-project-root))))
           (text
            (format
             "[%s] %s\n%s\n%s%s\n%s\n%s%s\n%s\n%s%s%s%s"
             (bold proj-name)
             (plum (string-remove-prefix proj-path fpath))
             (if region? "*region*" "")
             (format "%s%s" (if kill-file-path (bold "Copied: ") "") (italic fpath-pretty))
             (if (equal fpath-pretty fpath-true)
                 ""
               (concat " → " fpath-true))
             (format "%s: %s" (prop "- Size") (yellow (im-human-readable-size (- point-max point-min))))
             (format
              "%s: %s, %s: %s, %s: %s\n"
              (prop "- Lines")
              (yellow (count-lines point-min point-max))
              (prop "Words")
              (yellow (length (split-string (buffer-substring-no-properties point-min point-max) "\\W+" 'omit-nulls)))
              (prop "Chars")
              (yellow (- point-max point-min)))
             (format "%s: %s" (prop "- Major Mode") major-mode)
             (if in-git? (format "%s: %s" (prop "- Current branch") (bold (lab-git-current-branch))) "")
             (concat "\n" (im-read-time) "\n")
             (if in-git?
                 (concat (bold "\nGit Status: \n") (ansi-color-apply (shell-command-to-string (format "git -c color.ui=always status %s" fpath))))
               (italic "\n Not in a git repository."))
             "\n"
             (if in-git?
                 (let* ((diff (ansi-color-apply (shell-command-to-string (format "git diff --color=always %s" fpath))))
                        (len (or (-some->> (s-split "\n" diff) (car) (length)) 0)))
                   (concat
                    "\n" (s-repeat len "─") "\n\n"
                    diff))
               ""))))
      (im-peek
       (lambda ()
         (with-current-buffer (get-buffer-create "*im-buffer-info*")
           (page-break-lines-mode)
           (erase-buffer)
           (insert (s-trim text))
           (current-buffer))))
      (when kill-file-path
        (im-kill fpath-pretty)))))

;;;;; xah-open-file-at-cursor

;; This is better than =find-file-at-point= because it takes line
;; numbers etc. into account.

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.  If
the path starts with “http://”, open the URL in browser.  Input
path can be {relative, full path, URL}.

Path may have a trailing “:‹n›” that indicates line number, or
“:‹n›:‹m›” with line and column number.  If so, jump to that line
number.  If path does not have a file extension, automatically
try with “.el” for elisp files.  This command is similar to
`find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of
                      ;; file path or url, e.g. whitespace, comma. The
                      ;; colon is a problem. cuz it's in url, but not
                      ;; in file name. Don't want to use just space as
                      ;; delimiter because path or url are often in
                      ;; brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char 1)
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "File no exist: 「%s」.  Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "File no exist: 「%s」.  Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "File no exist: 「%s」.  Create?" $path))
                  (find-file $path ))))))))))

(define-key evil-normal-state-map (kbd "gf") 'xah-open-file-at-cursor)

;;;;; xah-{escape,unescape}-quotes

(defun xah-escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
  See also: `xah-escape-quotes'

  URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
  Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(general-def :states '(visual)
  "ze" #'xah-escape-quotes
  "zE" #'xah-unescape-quotes)

;;;;; eksisozluk gundem

(defun im-eksi-gundem-sirali ()
  "Eksi gundemini entry sayisina gore sirala ve `completing-read' yap."
  (interactive)
  (let ((results (->>
                  (with-temp-buffer
                    (insert
                     (shell-command-to-string "curl --silent --header \"User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:137.0) Gecko/20100101 Firefox/137.0\" https://eksisozluk.com/basliklar/gundem | grep '?a=popular' | sed -E 's/[ ]*href=\"(.*)\">(.*) <small>(.*)<\\/small>(.*)/(\\3) \\2|||\\1/' | sort -V -r | uniq"))
                    (goto-char (point-min))
                    (xml-parse-string))
                  (s-trim)
                  (s-split "\n")
                  (--map (s-split "|||" it))
                  (--map `(,(car it) . ,(cadr it))))))
    (->>
     (im-completing-read
      "Baslik: " results
      :sort? nil
      :formatter #'car)
     (cdr)
     (format "https://eksisozluk.com/%s")
     (browse-url))))

;;;;; insert uuid

(defun im-uuid ()
  "Generate a UUID."
  (s-trim (shell-command-to-string "uuidgen")))

(defun im-insert-uuid ()
  "Insert UUID."
  (interactive)
  (insert (im-uuid)))

;;;;; im-shell-command

(defvar-local im-shell-command-mode-command nil
  "Current shell command that belongs to the buffer.")

(define-minor-mode im-shell-command-mode
  "Shell command mode."
  :lighter "iscm"
  :keymap (make-sparse-keymap))

(evil-define-key 'normal im-shell-command-mode-map
  (kbd "gr") #'im-shell-command-mode-rerun-command)

(defun im-shell-command-mode-rerun-command ()
  "Re-run the shell command."
  (interactive)
  (apply #'im-shell-command im-shell-command-mode-command))


;; In normal mode, hitting ! will display im-shell-command but in
;; visual mode hitting ! will open evil's default evil-shell-command
;; on selected region.
(evil-define-key 'normal 'global (kbd "!") #'im-shell-command)

(defun im-shell-smart-history ()
  (->>
   (with-temp-buffer
     (insert-file-contents eshell-history-file-name)
     (buffer-string))
   s-trim
   (s-split "\n")
   (--map (if (s-prefix? ":" it)
              (s-replace-regexp "^: [0-9:]+;" "" it)
            it))
   (--filter (and (not (s-blank? it))
                  (> (length it) 5)
                  (not (s-matches? "^\\(ls\\|pwd\\|exit\\|cd\\|echo\\)" it))
                  (not (s-suffix? "\\" it))))
   (reverse)))

(cl-defun im-shell-command
    (&key
     command
     args
     eat
     env
     async
     switch
     (buffer-name (concat "*" command "*"))
     (on-start (lambda (&rest _)))
     (on-finish (lambda (&rest _)))
     (on-fail (lambda (&rest _))))
  "Run given shell COMMAND and redirect output to given BUFFER-NAME.
This is a wrapper around `start-process-shell-command' that adds
support for ANSI term colors and some syntactic convenience.

If ARGS is non-nil list, then use `start-process' with command as
COMMAND and args as ARGS.

ENV is a list of environment variables and values in the
following form:

    '(\"DISPLAY=\" \"X=5\" \"Y=6\")

If EAT is non nil, use eat shell to display output which handles
terminal outputs way better.  ARGS must be a list.

When called interactively, asks for a command to run (with eshell
completion).

In BUFFER-NAME, `im-shell-command-mode' is activated and you can
use `im-shell-command-mode-rerun-command' or \"gr\" to replay the
command.

If EAT is non-nil and BUFFER-NAME does not contain \"*...*\", the
asterisks will be added automatically by eat.

Returns process buffer."
  (interactive
   (let ((command (im-completing-read "Command: " (im-shell-smart-history) :sort? nil)))
     (list
      :command command
      :switch t
      :on-finish
      (lambda (&rest _)
        (let ((msg (format ">> \"%s\" finished successfully." command)))
          (message msg)
          (when (not (frame-focus-state))
            (alert msg))))
      :on-fail
      (lambda (&rest _)
        (let ((msg (format ">> \"%s\" FAILED." command)))
          (message msg)
          (when (not (frame-focus-state))
            (alert msg)))))))
  (let* ((process-environment (append env process-environment))
         (proc
          (if eat
              (progn
                (let ((buffer (apply #'eat-make (->>
                                                 buffer-name
                                                 (s-chop-prefix "*")
                                                 (s-chop-suffix "*"))
                                     command nil args)))
                  (setq buffer-name (buffer-name buffer))
                  (get-buffer-process buffer)))
            (if args
                (apply #'start-process command buffer-name command args)
              (start-process-shell-command command buffer-name command))))
         (proc-out ""))
    (let ((fn (lambda (resolve reject)
                (set-process-sentinel
                 proc
                 (lambda (p e)
                   (with-current-buffer (get-buffer-create buffer-name)
                     (read-only-mode -1))
                   (let ((exit-code (process-exit-status p)))
                     (if (= 0 exit-code)
                         (funcall resolve proc-out)
                       (funcall reject exit-code))))))))
      (prog1 (if async
                 (promise-new fn)
               (funcall fn on-finish on-fail))
        (unless eat
          (set-process-filter
           proc
           (lambda (proc str)
             (with-current-buffer buffer-name
               (setq proc-out (concat proc-out str))
               (let ((inhibit-read-only t))
                 (save-excursion
                   (goto-char (point-max))
                   (insert (ansi-color-apply (s-replace "" "\n" str)))))))))
        (with-current-buffer buffer-name
          (unless eat
            (prog-mode))
          (im-shell-command-mode 1)
          (evil-normal-state)
          (setq-local
           im-shell-command-mode-command
           (list
            :command command
            :args args
            :buffer-name buffer-name
            :on-start on-start
            :on-finish on-finish
            :on-fail on-fail))
          (funcall on-start))
        (when switch
          (switch-to-buffer buffer-name))
        (get-buffer buffer-name)))))

;;;;; Password manager

(defun im-password-all ()
  "Get list of all passwords and their properties from `passwords.org'."
  (with-current-buffer "passwords.org"
    (->>
     (lambda ()
       (let ((link "")
             (match (org-entry-get nil "MATCH"))
             (header-link (save-excursion (forward-char 5) (org-element-context)))
             (title (org-entry-get nil "ITEM"))
             (props (im-alist-to-plist (org-entry-properties))))
         (when (plist-get props :username)
           ;; ^ A password entry should contain at least the :username: prop
           (when (eq (org-element-type header-link) 'link)
             (setq link (org-element-property :raw-link header-link))
             (save-excursion
               (setq title
                     (buffer-substring-no-properties
                      (org-element-property :contents-begin header-link)
                      (org-element-property :contents-end header-link)))))
           `(:title ,title :link ,link ,@props))))
     (org-map-entries)
     (-filter #'identity))))

;; TODO Better matching algorithm Check if full string matches any,
;; then check if host matches. Return full string match only if it
;; exists. Also check if toplevel domain matches, if it does not match
;; fully
(defun im-password-find-for (url)
  "Return matching accounts for given URL.
If there are multiple accounts registered for one entry, then
list them as seperate entries."
  (let ((urlobj (url-generic-parse-url url)))
    (setq url (url-host urlobj))
    (when-let* ((port (url-port-if-non-default urlobj)))
      (setq url (format "%s:%s" url port))))
  (let* ((candidates (--filter
                      (when (or (ignore-errors (s-match (plist-get it :match) url))
                                (s-contains? url (plist-get it :link)))
                        it)
                      (im-password-all))))
    (-flatten-n
     1
     (-map (lambda (info)
             (let ((unames (s-split " " (plist-get info :username)))
                   (pwds (s-split " " (plist-get info :password))))
               (-zip-pair  (--map (format "%s - %s" (plist-get info :title) it) unames)
                           (--map (list :info info :acc it)
                                  (-zip-pair unames pwds)))))
           candidates))))

(defun im-password-qutebrowser (url fifo)
  "Find credentials for currently open link in Qutebrowser and fill."
  (let* ((candidates (im-password-find-for url))
         (result (plist-get
                  (alist-get (completing-read "Select account: " candidates) candidates nil nil #'equal) :acc))
         (username (car result))
         (password (-some->> (cdr result)
                     (s-replace "\"" "\\\"")
                     (s-replace "'" "\\'"))))
    (when (and username password)
      (pcase (completing-read "Method: " '("Fill all with TAB" "Fill username" "Fill password"))
        ("Fill all with TAB"
         (write-region "mode-enter insert\n" nil fifo 'append)
         (write-region (format "fake-key %s\n" username) nil fifo 'append)
         (write-region "fake-key <Tab>\n" nil fifo 'append)
         (write-region (format "fake-key %s\n" password) nil fifo 'append))
        ("Fill username"
         (write-region "mode-enter insert\n" nil fifo 'append)
         (write-region (format "fake-key %s\n" username) nil fifo 'append))
        ("Fill password"
         (write-region "mode-enter insert\n" nil fifo 'append)
         (write-region (format "fake-key %s\n" password) nil fifo 'append))))))

;; TODO Add other actions:
;; - Fill with tab
;; - Copy username/password etc.
(defun im-password-act ()
  (interactive)
  (and-let* ((passwords (im-password-all))
             (candidates (--map (cons (plist-get it :title) it) passwords))
             (selected (im-alist-completing-read "Select: " candidates)))
    (let-plist selected
      (pcase (completing-read
              "Action: "
              (list "Copy as username:password"
                    "Copy as PostgreSQL connection string"
                    "Copy as SQL src block header args"
                    "Copy as Couchbase (CBC, N1QL) src block header args"))
        ("Copy as username:password"
         (kill-new
          (format "%s:%s" .username .password)))
        ("Copy as PostgreSQL connection string"
         (kill-new
          (format "postgresql://%s:%s@%s:%s/%s"
                  .username .password .host .port .db)))
        ("Copy as SQL src block header args"
         (kill-new
          (format ":engine postgresql :dbhost %s :dbuser %s :dbpassword %s :database %s :dbport %s"
                  .host .username .password .db .port)))
        ("Copy as Couchbase (CBC, N1QL) src block header args"
         (kill-new
          (format ":host %s :username %s :password %s"
                  .host .username .password)))))))

;;;;; Run functions globally

;; Simply use ~im-dmenu~ function instead of the default
;; ~completing-read~ function when the code is called inside this
;; macro. This way, you can use your ~completing-read~ based functions
;; within your system, without needing to focus Emacs first.

(defmacro im-globally (&rest body)
  `(let ((completing-read-function #'im-dmenu))
     ,@body))

;;;;; narrow-indirect & im-narrow-dwim

(use-package narrow-indirect)

(im-leader-v
  "n" #'im-narrow-dwim
  "N" (im-eval-dwim
       #'ni-narrow-to-page-indirect-other-window
       #'ni-narrow-to-region-indirect-other-window
       #'ni-narrow-to-defun-indirect-other-window))

(defun im-narrow-dwim ()
  "Smart narrowing."
  (interactive)
  (cond
   ((use-region-p) (narrow-to-region (region-beginning) (region-end)))
   ((eq major-mode 'org-mode) (org-narrow-to-subtree))
   ((eq major-mode 'markdown-mode) (markdown-narrow-to-subtree))
   ((derived-mode-p 'diff-mode) (diff-restrict-view))
   ((eq major-mode 'emacs-lisp-mode)
    (cond
     ((which-function) (narrow-to-defun))
     (t (outli-toggle-narrow-to-subtree))))
   ((s-contains? "-ts-" (symbol-name major-mode))
    (when-let* ((def (treesit-defun-at-point)))
      (narrow-to-region (treesit-node-start def)
                        (treesit-node-end def))))
   (outli-mode (outli-toggle-narrow-to-subtree))))



;;;;; im-meme-downloader

(defun im-meme-downloader (url &optional file-title)
  (interactive
   (list
    (read-string "URL: ")
    (read-string "Title: ")))
  (let* ((title
          (or file-title
              (s-trim
               (thread-last
                 url
                 im-url-get-title
                 im-string-url-case
                 s-downcase))))
         (dir (expand-file-name "~/Documents/memes"))
         (default-directory dir)
         (cmd (format "yt-dlp --output '%s.%%(ext)s' '%s'" title url)))
    (message "im-meme-downloader :: %s" cmd)
    (im-shell-command
     :command cmd
     :switch nil
     :on-finish
     (lambda (output &rest _)
       (if-let* ((matches (or
                           (s-match "\\[download\\] \\(.*\\) has already been downloaded" output)
                           (s-match "^\\[Merger\\] Merging formats into \"\\(.*\\)\"" output)
                           (s-match "^\\[download\\] Destination: \\(.*\\)" output)))
                 (fname (abbreviate-file-name (nth 1 matches))))
           (progn
             (alert (format "Downloaded: %s!" fname))
             (kill-new (format "%s/%s" dir fname)))
         (alert "Can't parse yt-dlp output!")))
     :on-fail
     (lambda (&rest _)
       (alert (format "Error while downloading: %s!" url))))))

;;;;; zoom

(defun im-open-zoom-meeting-dwim (&optional link)
  (interactive
   (list
    (or (thing-at-point 'url) (read-string "Link: "))))
  (when-let* ((zoom (s-match
                     "https://.*zoom\\.us/j/\\(\\w+\\)\\?pwd=\\(\\w+\\)"
                     link))
              (cmd (format
                    "open 'zoommtg://zoom.us/join?confno=%s&pwd=%s'"
                    (nth 1 zoom)
                    (nth 2 zoom))))
    (message ">> Running %s" cmd)
    (shell-command cmd)))

;;;;; macOS calendar functions

;; My work computer is a Mac. I synchronize my work calendar into my
;; local by using macOS' Calendar app and I utilize
;; [[https://hasseg.org/icalBuddy/][icalBuddy]] to interact with that
;; calendar from Emacs.

(defun im-calendar-now ()
  "Show the current calendar item details in a buffer and open the zoom link if it has one."
  (interactive)
  (im-shell-command
   :command "icalBuddy -f -ea eventsNow"
   :switch t
   :buffer-name "*calendar-now*"
   :on-start (lambda (&rest _) (erase-buffer))
   :on-finish (lambda (it)
                (when-let* ((zoom (nth 1 (s-match "\\(https://.*zoom.us/j/.*\\)\\(\b\\|\n\\)" it))))
                  (im-open-zoom-meeting-dwim zoom)))))

(define-derived-mode im-calendar-mode outline-mode "Calendar"
  "Calendar...")

(general-def :keymaps 'im-calendar-mode-map :states 'motion
  "TAB" #'outline-cycle)

(defun im-calendar-today ()
  "Show today's calendar in a buffer.
The resulting buffer has `outline-mode' enabled, so you can use
`outline-mode' specific bindings to jump, hide/show stuff."
  (interactive)
  (im-shell-command
   :command "icalBuddy -f eventsToday"
   :switch t
   :on-start (lambda (&rest _) (erase-buffer))
   :on-finish (lambda (&rest _)
                (im-calendar-mode)
                (setq-local outline-regexp "• ")
                (setq-local outline-level #'outline-level)
                (goto-char (point-min))
                (outline-cycle-buffer))
   :buffer-name "*calendar-today*"))

(defun im-calendar-for-date (date)
  "Show given DATEs calendar in a buffer."
  (interactive (list (format-time-string "%Y-%m-%d" (org-read-date nil 'to-time nil "Date:  "))))
  (im-shell-command
   :command (format "icalBuddy -f eventsFrom:%s to:%s" date date)
   :switch t
   :on-start (lambda (&rest _) (erase-buffer))
   :on-finish (lambda (&rest _)
                (im-calendar-mode)
                (setq-local outline-regexp "• ")
                (setq-local outline-level #'outline-level)
                (goto-char (point-min))
                (outline-cycle-buffer))
   :buffer-name (format "*calendar-%s*" date)))

;;;;; im-clipboard-image-to-text

(defalias 'im-clipboard-tesseract-ocr-text #'im-clipboard-image-to-text)
(defun im-clipboard-image-to-text ()
  (interactive)
  (let ((temp-image (make-temp-file "tesseract-image" nil ".png"))
        (temp-text (make-temp-file "tesseract-text"))
        (buffer (get-buffer-create "*ocr-result*")))
    (im-save-clipboard-image-to-file temp-image)
    (set-process-sentinel
     (start-process "*tesseract-ocr*" nil "tesseract" temp-image temp-text)
     (lambda (proc out)
       (if (and
            (eq 'exit (process-status proc))
            (eq 0 (process-exit-status proc)))
           (progn
             (with-current-buffer buffer
               (erase-buffer)
               (insert (format "[[%s]]\n\n\n" temp-image))
               (insert-file-contents (concat temp-text ".txt"))
               (iimage-mode))
             (switch-to-buffer buffer))
         (message "*tesseract-ocr*: %s" (string-trim out)))))))

;;;;; im-gnuplot

(cl-defmacro im-gnuplot (settings &rest forms)
  "A horrible hack for using `gnuplot' in emacs-lisp.

Example usage:

    (im-gnuplot
        (:persist t)
      (plot sin(x))
      (plot sin(x)/x)
      (plot cos(x))
      (plot cos(x)/x)
      (plot sin(x) title \"Sin\", tan(x) title \"Tangent\"))

First argument is a plist which consists gnuplot configs or variable
definitions.  Configs are`:persist', `:default', `:slow'.  See `man
gnuplot' for more information on these settings.  They can be either t
or nil.  Default is nil for each one.

Variable definitions are like the `let' form, simply define variables
and use them in the gnuplot code below.  Variable namings should follow
`:this-style:' and the values can be arbitrary elisp.

    (im-gnuplot
        (:data: (make-temp-file \"im-gnuplot\" nil \".dat\" \"a 1\nb 2\nc 3\")
         :out-file: \"~/output.png\")
      (set terminal pngcairo size 900,300 enhanced font \"Verdana,8\")
      (set output :out-file:)
      (set style data histograms)
      (set boxwidth 0.9)
      (set style fill solid)
      (set xlabel \"Time\")
      (set ylabel \"Log count\")
      (set title \"Log count over time\")
      (plot :data: using 2:xtic(1) title \"\"))"
  (declare (indent 1))
  (let* ((script (thread-last
                   forms
                   (mapcar
                    (lambda (it)
                      (thread-first
                        it
                        (prin1-to-string)
                        (string-trim-left "(")
                        (string-trim-right ")"))))
                   (s-join "\n")
                   (s-replace-regexp " (\\\\, \\(.*?\\))" ",\\1"))))
    `(progn
       (let* ((items (list ,@(-flatten-n 1 (map-apply (lambda (key val) (list key val)) settings))))
              (script-last (thread-last
                             ,script
                             (s-prepend "reset\n")
                             (s-replace-regexp
                              "\\(:[a-zA-Z-]+:\\)"
                              (lambda (match) (format " %s " (prin1-to-string (plist-get items (intern (s-trim match))))))))))
         (with-temp-buffer
           (insert script-last)
           (shell-command-on-region
            (point-min) (point-max)
            (format "gnuplot%s%s%s"
                    (if (plist-get items :persist) " --persist" "")
                    (if (plist-get items :default) " --default" "")
                    (if (plist-get items :slow) " --slow" ""))
            nil t)
           (buffer-string))))))

;;;;; Image and file system tags

(defun im-image-edit-tags (tags image &optional clear-all)
  "Add TAGS to IMAGE.
If CLEAR-ALL is non-nil, clear all tags before setting TAGS as image
tags.  Otherwise TAGS are appended.  If cursor is on an org link, use
that as the IMAGE (when called interactively)."
  (interactive
   (let* ((org-link? (org-in-regexp org-link-any-re 1))
          (file
           (if org-link?
               (progn
                 (goto-char (car org-link?))
                 (plist-get (cadr (org-element-link-parser)) :path))
             (read-file-name "Image file: ")))
          (file-tags (s-join ", " (im-image-tags file)))
          (tags (completing-read-multiple
                 "Tags: "
                 (im-directory-image-tags (f-dirname file))
                 nil nil
                 (if (s-blank? file-tags) nil (concat file-tags ",")))))
     (list
      tags
      file
      t)))
  (setq image (expand-file-name image))
  (when clear-all
    (shell-command (format "exiftool -overwrite_original -keywords='' '%s'" image)))
  (shell-command
   (format "exiftool -overwrite_original %s '%s'"
           (s-join " " (--map (format "-keywords+='%s'" (s-trim it)) tags))
           image))
  ;; Also add these tags as file extended attribute which helps KDE to
  ;; easily index these files.  I'm using this in conjunction with exif
  ;; attributes because it's quite easy to loose extended attributes
  ;; and exif attributes serves as a backup.
  (if clear-all
      (im-set-file-attribute image "user.xdg.tags" (s-join "," tags))
    (im-set-file-attribute
     image "user.xdg.tags"
     (s-join "," (cons (im-get-file-attribute image "user.xdg.tags") tags)))))

(defun im-image-tags (image)
  "Return tags of IMAGE."
  (let ((result
         (plist-get
          (seq-first
           (json-parse-string
            (shell-command-to-string (format "exiftool -quiet -json -keywords '%s'" (expand-file-name image)))
            :array-type 'list
            :object-type 'plist))
          :Keywords)))
    (if (listp result) result (list result))))

(defun im-directory-image-tags (&optional directory)
  "Return all unique image tags in DIRECTORY.
If DIRECTORY is null, `default-directory' is used."
  (-uniq
   (--mapcat
    (plist-get it :Keywords)
    (json-parse-string
     (shell-command-to-string
      (im-ensure-binary
       (format "exiftool -m -quiet -json -keywords '%s'" (expand-file-name (or directory default-directory)))))
     :object-type 'plist :array-type 'list))))

(defun im-get-file-attribute (file name)
  "Get extended attribute NAME for FILE."
  (shell-command-to-string (format "getfattr --absolute-names --only-values --name='%s' '%s'" name (expand-file-name file))))

(defun im-set-file-attribute (file name value)
  "Set extended attribute NAME as VALUE for FILE.
When called interactively, set given NAME to VALUE as extended
attribute for current buffers file or selected file."
  (interactive
   (let ((attr (completing-read "Attribute: " '("user.xdg.tags"))))
     (list
      (or (buffer-file-name) (read-file-name "File: "))
      attr
      (read-string (format "Value for '%s': " attr)))))
  (shell-command-to-string (format "setfattr --name='%s' --value='%s' '%s'" name value (expand-file-name file))))

;;;;; Converting minibuffer candidates to a table automatically

(defun im-minibuffer-to-table ()
  (interactive)
  (let ((buffer (get-buffer-create "*im-minibuffer-table*"))
        (items (completion-all-completions
                (minibuffer-contents)
                minibuffer-completion-table
                minibuffer-completion-predicate
                (max 0 (- (point) (minibuffer-prompt-end))))))
    (when-let* ((last (last items)))
      (setcdr last nil))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (thread-last
        (cdr items)
        (s-join "\n")
        (insert))
      (goto-char (point-min))
      (push-mark (point-max) t t)
      (orgtbl-create-or-convert-from-region nil)
      (deactivate-mark)
      (org-mode))
    (run-with-timer 0.1 nil (lambda () (switch-to-buffer-other-window buffer)))
    (abort-recursive-edit)))

(define-key minibuffer-local-map (kbd "M-|") #'im-minibuffer-to-table)

;;;;; im-pair-prog-mode -- Minor mode for pair programming

(define-minor-mode im-pair-prog-mode
  "Pair programming mode so that everyone can enjoy the beauty of Emacs."
  :lighter " PairProg"
  :global t
  (unless im-pair-prog-mode
    (message "Restoring normal environment...")
    (global-display-line-numbers-mode -1)
    (default-text-scale-reset)
    (message "Restoring normal environment...Done."))
  (when im-pair-prog-mode
    (message "Preparing pair programming environment...")
    (global-display-line-numbers-mode t)
    (default-text-scale-increment (* 2 default-text-scale-amount))
    (message "Preparing pair programming environment...Done.")))

;;;;; im-read-time

(defun im-read-time ()
  "Show approximate read time of the selected area or whole buffer."
  (interactive)
  (funcall
   (if (called-interactively-p 'interactive) #'message #'format)
   "%s min read"
   (ceiling
    (/ (apply #'count-words
              (if (region-active-p)
                  (list (region-beginning) (region-end))
                (list (point-min) (point-max))))
       ;; I am using a value lower than average wpm to account for other
       ;; things (distractions etc.)
       230.0))))

;;;;; im-peek -- Inline/popup documentation/translate/dictionary etc.

;;;;;; quick-peek

;; I based im-peek on quick-peek but will drop the dependency
;; eventually as I am not able to properly configure some aspects of
;; the "peek window".

(use-package quick-peek
  :straight (:host github :repo "cpitclaudel/quick-peek"))

;;;;;; Bindings

(im-leader-v
  "mt" #'im-peek-translate
  "ms" #'im-peek-sozluk
  "mS" #'im-peek-dictionary
  "me" #'im-peek-etimoloji
  "mE" #'im-peek-etymology
  "md" #'im-peek-doc
  "mc" #'im-peek-source)

(general-def
  :states 'normal
  :keymaps '(prog-mode-map)
  "K" #'im-peek-doc)

(with-eval-after-load 'evil
  (setq evil-lookup-func #'im-peek-doc))

(general-def
  :states 'normal
  :keymaps 'im-peek-mode-map
  (kbd "K") #'im-peek-remove)

;;;;;; im-peek implementation

;;;;;;; Variables

(defvar im-peek--buffer nil)
(defvar im-peek--string nil)
(defvar im-peek--pos 0)
(defconst im-peek--line-count 30)

;;;;;;; Mode

(define-minor-mode im-peek-mode
  "Elisp popup documentation mode."
  :lighter " ElPopDoc"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-j") #'im-peek-mode-scroll-down)
            (define-key map (kbd "M-k") #'im-peek-mode-scroll-up)
            (define-key map (kbd "C-g") #'im-peek-remove)
            (define-key map (kbd "M-RET") #'im-peek-jump)
            map)
  (setf (alist-get #'im-peek-mode minor-mode-overriding-map-alist)
        im-peek-mode-map)
  (evil-normalize-keymaps))

(defun im-peek-jump ()
  (interactive nil im-peek-mode)
  (im-peek-remove)
  (switch-to-buffer-other-window im-peek--buffer))

(defun im-peek-mode-scroll-down (&optional lines)
  (interactive nil im-peek-mode)
  (quick-peek-hide)
  (quick-peek-show
   (->>
    (s-lines im-peek--string)
    (-drop (setq im-peek--pos (+ (or lines 1) im-peek--pos)))
    (s-join "\n"))
   nil nil im-peek--line-count))

(defun im-peek-mode-scroll-up (&optional lines)
  (interactive nil im-peek-mode)
  (quick-peek-hide)
  (quick-peek-show
   (->>
    (s-lines im-peek--string)
    (-drop (setq im-peek--pos (max 0 (+ (- (or lines 1)) im-peek--pos))))
    (s-join "\n"))
   nil nil im-peek--line-count))

(defun im-peek-remove ()
  (interactive nil im-peek-mode)
  (setq im-peek--pos 0)
  (im-peek-mode -1)
  (quick-peek-hide)
  (setq minor-mode-overriding-map-alist
        (assq-delete-all #'im-peek-mode
                         minor-mode-overriding-map-alist)))

(defun im-unfold-if-folded (&optional pt)
  (setq pt (or pt (point)))
  (when (listp buffer-invisibility-spec)
    (when (and
           ;; Check if org-fold is enabled or not first
           (alist-get 'org-fold-outline buffer-invisibility-spec)
           (org-fold-folded-p pt))
      (save-excursion
        (goto-char pt)
        (org-fold-show-entry)))
    (when (and
           (alist-get 'outline buffer-invisibility-spec)
           (outline-invisible-p pt))
      (save-excursion
        (goto-char pt)
        (outline-show-entry)))
    (when (and
           (alist-get 'hs buffer-invisibility-spec)
           (save-excursion
             (goto-char pt)
             (hs-already-hidden-p)))
      (save-excursion
        (goto-char pt)
        (hs-toggle-hiding)))))

;; TODO support fn returning a string instead of a buffer. Create a
;; temp buffer when jump is called
(defun im-peek (fn)
  "Display a nice little small window below current line.
FN should take no args and return a buffer with the intended
contents."
  (let ((buffer (save-window-excursion (funcall fn))))
    (setq im-peek--buffer buffer)
    ;; `run-at-time' function is useful for forcing the following code
    ;; to run in main event loop. This helps when `im-peek' is called
    ;; in an async context.
    (run-at-time
     0 nil
     (lambda ()
       (let ((str (setq im-peek--string
                        (with-current-buffer buffer
                          (font-lock-ensure)
                          (buffer-string)))))
         ;; Center the current point if the remaining line count is low
         (when (and
                (< (im-line-count-below-cursor) (length (s-lines str)))
                (< (im-line-count-below-cursor) (- (window-height) 10)))
           (recenter 10))
         (quick-peek-show str nil nil im-peek--line-count)
         (im-unfold-if-folded (save-excursion (end-of-line) (1+ (point)))))
       (im-peek-mode +1)))))

(defun im-peek-open? ()
  "Return t if >=1 peek window is open."
  (car quick-peek--overlays))

;;;;;; Special peek functions

(use-package google-translate
  :defines (google-translate-pop-up-buffer-set-focus)
  :custom
  (google-translate-listen-program "mpv")
  ;; Try 'curl or 'wget if getting any errors
  (google-translate-backend-method 'emacs)
  :init
  (setq google-translate-translation-directions-alist
        ;; Ordered based on my usage:
        '(("auto" . "en")
          ("en" . "tr")
          ("nl" . "en")
          ("tr" . "en")
          ("en" . "nl"))))

(defun im-peek-translate ()
  "Simple `google-translate-smooth-translate' wrapper.
Use C-n C-p to switch between translation directions."
  (interactive)
  (im-peek
   (lambda ()
     (let ((google-translate-pop-up-buffer-set-focus t))
       (google-translate-smooth-translate)
       (current-buffer)))))

(defun im-peek-sozluk ()
  "Show definition of the word at point (tr)."
  (interactive)
  (im-peek
   (lambda ()
     (call-interactively #'sozluk)
     (current-buffer))))

(defun im-peek-dictionary ()
  "Show definition of the word at point (en)."
  (interactive)
  (im-peek
   (lambda ()
     (call-interactively #'wordnut-lookup-current-word)
     (current-buffer))))

(defun im-peek-etimoloji ()
  "Show etymology of the word at point (tr)."
  (interactive)
  (im-peek
   (lambda ()
     (call-interactively #'sozluk-etymology)
     (current-buffer))))

(defun im-peek-etymology ()
  "Show etymology of the word at point (en)."
  (interactive)
  (im-peek
   (lambda ()
     (call-interactively #'im-etymology-en)
     (current-buffer))))

;; TODO: If org mode, show link's text as peek
(defun im-peek-doc ()
  "Show documentation at point."
  (interactive)
  (im-peek
   (cond
    ((ignore-errors (symbol-value 'lsp-mode)) #'im-peek-doc--lsp)
    ((-contains? '(emacs-lisp-mode lisp-interaction-mode) major-mode) #'im-peek-doc--elisp)
    ((equal major-mode 'clojure-mode) #'im-peek-doc--clojure)
    ((equal major-mode 'org-mode)
     (cond
      (jupyter-org-interaction-mode #'im-peek-doc--jupyter)
      (t (lambda ()
           (call-interactively #'wordnut-lookup-current-word)
           (current-buffer)))))
    (eldoc-mode #'eldoc-doc-buffer))))

;; TODO: Add lsp mode etc.
(defun im-peek-source ()
  "Show source at point."
  (interactive)
  (let (pos)
    (im-peek
     (lambda ()
       (call-interactively #'xref-find-definitions)
       (setq pos (line-number-at-pos))
       (current-buffer)))
    (im-peek-mode-scroll-down (- pos 2))))

(defun im-peek-doc--elisp ()
  (let ((help-xref-following t))
    (helpful-symbol (symbol-at-point))
    (current-buffer)))

(defun im-peek-doc--lsp ()
  (let ((result
         ;; Taken from lsp-describe-thing-at-point
         (-some->> (lsp--text-document-position-params)
           (lsp--make-request "textDocument/hover")
           (lsp--send-request)
           (lsp:hover-contents)
           (funcall (-flip #'lsp--render-on-hover-content) t)
           (string-trim-right))))
    (unless result
      (user-error "No doc at point"))
    (with-current-buffer (get-buffer-create "*im-lsp-md-doc*")
      (erase-buffer)
      (insert result)
      (current-buffer))))

(defun im-peek-doc--clojure ()
  (save-window-excursion
    (cider-doc)
    (current-buffer)))

(defun im-peek-doc--jupyter ()
  (save-window-excursion
    (jupyter-inspect-at-point)
    (get-buffer "*Help*")))

;;;;; im-extract

(cl-defun im-extract (files)
  "Extract each FILES into a folder.
This is a simple wrapper around `aunpack' binary.  It simply
finds the best way to extract and does not overwrite anything."
  (interactive (list (dired-get-marked-files)))
  (let ((buff (current-buffer)))
    (im-shell-command
     :command (im-ensure-binary "aunpack" :package "atool")
     :args (append '("--each") files)
     :on-finish
     (lambda (out)
       (let ((extracted (--keep
                         (s-match "\\(.*\\): extracted to `\\(.*\\)'\\( (.*)\\)?" it)
                         (s-split "\n" out t))))
         (with-current-buffer buff
           ;; Dirvish does not kick in after doing the following, I
           ;; don't know why. Doing anything on the buffer fixes the
           ;; buffer tho.
           (when (derived-mode-p 'dired-mode)
             (revert-buffer)
             (dired-goto-file (expand-file-name (nth 2 (car extracted))))))
         (message (s-join "\n" (-map #'car extracted)))))
     :on-fail
     (lambda (&rest _)
       (error ">> Extraction failed! See *im-extract* buffer"))
     :buffer-name "*im-extract*")))

;;;;; Etymology lookup inside Emacs

(defun im-etymology-en (input)
  "Find and retrieve the given word etymology from etymonline.
Adapted from: https://babbagefiles.xyz/emacs_etymologies/"
  (interactive
   (list (read-string "Word: " (im-region-or 'word))))
  (let* ((buffer (generate-new-buffer (format "*etymology: %s*" input))))
    (switch-to-buffer buffer)
    (insert (shell-command-to-string (concat "links -dump http://www.etymonline.com/word/" input)))
    (goto-char (point-min))
    (if (re-search-forward "Error 404 (Not Found)" nil t)
        (progn
          (kill-buffer)
          (user-error "Word not found: %s" input))
      (progn
        (delete-region (point-min) (progn
                                     (goto-char (point-min))
                                     (search-forward "[\s\s]")))
        (ignore-errors
          (delete-region
           (progn
             (goto-char (point-max))
             (search-backward-regexp "See all related words"))
           (point-max)))))
    (goto-char (point-min))))

;;;;; My git configuration

(defun im-git-use/apply-my/personal-config ()
  "Configure current repo to use my personal (non-work) git info."
  (interactive)
  (ignore-errors
    (lab-git-origin-switch-to-ssh))
  (shell-command-to-string "git config user.name \"Isa Mert Gurbuz\"")
  (shell-command-to-string "git config user.email \"isamertgurbuz@gmail.com\"")
  (message "Done."))

;;;;; YAML <-> JSON conversation

(defun im-yaml-to-json (yaml &optional replace?)
  "Convert YAML to JSON."
  (interactive (list (im-region-or 'string)))
  (let ((json (im-kill (json-encode (yaml-parse-string yaml)))))
    (when (and replace? (use-region-p))
      (replace-region-contents
       (region-beginning)
       (region-end)
       (lambda () json)))))

(defun im-json-to-yaml (json &optional replace?)
  "Convert JSON to YAML."
  (interactive (list (im-region-or 'string)))
  (let ((yaml (im-kill (yaml-encode (json-parse-string yaml)))))
    (when (and replace? (use-region-p))
      (replace-region-contents
       (region-beginning)
       (region-end)
       (lambda () yaml)))))

;;;;; im-test -- test related utility functions

(defun im-test-run-code-coverage-report ()
  "Run all tests and then open coverage."
  (interactive)
  (im-open-test-code-coverage-report t))

(defun im-test-run-current-test-only ()
  "Run currently focused test only."
  (interactive)
  (im-test-run-current-test-file t))

(defun im-test-open-code-coverage-report (run-test-before)
  "Open code coverage report for current project.
With RUN-TEST-BEFORE is non-nil, then run tests before and then
open coverage.  You can simply call \"gf\" or
`xah-open-file-at-cursor' to navigate to a file shown on the
output."
  (interactive "P")
  (let ((default-directory (im-current-project-root))
        (cov-file (format "file://%s/coverage/lcov-report/index.html" (im-current-project-root))))
    (if run-test-before
        (im-shell-command
         :command (pcase (im-test-get-nodejs-test-program)
                    ('jest "yarn jest --coverage --colors")
                    ('vitest "FORCE_COLOR=1 yarn vitest --coverage --run"))
         :buffer-name (format "%s-test-cov" (im-current-project-name))
         :switch t
         :on-finish (lambda (&rest _) (browse-url-default-browser cov-file))
         :on-fail (lambda (&rest _) (user-error ">> Tests are failed!")))
      (browse-url-default-browser cov-file))))

(defun im-test-run-current-test-file (run-only-focused-test)
  "Run the current test file."
  (interactive "P")
  (cond
   ((locate-dominating-file "." "yarn.lock") (im-test-run-current-file--yarn run-only-focused-test))
   ((locate-dominating-file "." "pom.xml") (im-test-run-current-file--mvn run-only-focused-test))
   (t (user-error "This project type is not recognized"))))

(defun im-test-get-nodejs-test-program ()
  "Return either `vitest' or `yarn'.
Throw error otherwise."
  (let-alist (json-read-file (f-join (im-current-project-root) "package.json"))
    (or (and .devDependencies.vitest 'vitest)
        (and .devDependencies.jest 'jest)
        (user-error "Not a jest or vitest file project!"))))

(defun im-test-run-current-file--yarn (run-only-focused-test)
  (let* ((test-file (file-relative-name buffer-file-name (im-current-project-root)))
         (default-directory (im-current-project-root))
         (program (im-test-get-nodejs-test-program))
         (current-test-name
          (ignore-errors
            (save-excursion
              (end-of-line)
              (re-search-backward "it(['\"]" nil t)
              (nth 1 (s-match "it(['\"]\\(.*\\)['\"]," (substring-no-properties (thing-at-point 'line))))))))
    (pcase program
      ('vitest
       (switch-to-buffer-other-window
        (apply
         #'eat-make
         (format
          "yarn test: %s%s" test-file
          (if run-only-focused-test (concat "::" current-test-name) ""))
         "yarn"
         nil
         "vitest"
         test-file
         (when (and run-only-focused-test current-test-name)
           (list "--testNamePattern" current-test-name)))))
      ('jest
       (switch-to-buffer-other-window
        (apply
         #'eat-make
         (format
          "yarn test: %s%s" test-file
          (if run-only-focused-test (concat "::" current-test-name) ""))
         "yarn"
         nil
         "jest"
         "--colors"
         "--runTestsByPath"
         test-file
         (when (and run-only-focused-test current-test-name)
           (list current-test-name))))))))

(defun im-test-run-current-file--mvn (run-only-focused-test)
  (let* ((test-file buffer-file-name)
         (default-directory (im-current-project-root))
         (current-test-class
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp "^\\(public\\|internal\\)? ?class \\([A-Za-z0-9_]+\\)\\( \\|$\\)" nil t)
            (beginning-of-line)
            (nth 1 (s-match "class \\([A-Za-z0-9_]+\\)\\( \\|$\\)" (substring-no-properties (thing-at-point 'line))))))
         (current-test-name
          (ignore-errors
            (save-excursion
              (end-of-line)
              (re-search-backward "@Test" nil t)
              (forward-line)
              (nth 1 (s-match " \\([A-Za-z0-9_]+\\)(" (substring-no-properties (thing-at-point 'line))))))))
    (im-shell-command
     :command (format "./mvnw test --offline -Dstyle.color=always -Djansi.force=true -Dtest='%s%s'"
                      current-test-class
                      (if (and run-only-focused-test current-test-name)
                          (format "#%s" current-test-name)
                        ""))
     :switch t)))

;;;;; Shopping Mode

;; I have a simple shopping list that I sync with my phone. This is a
;; simple mode for easier editing on that file.

(define-derived-mode alisveris-mode markdown-mode "AlisverisMode"
  "Mode for my shopping list."
  (setq-local header-line-format (substitute-command-keys "Bindings :: \\[im-alisveris-add-to-market] → Market")))

(general-def :keymaps 'alisveris-mode-map :states 'normal
  "\\a" #'im-alisveris-add-to-market)

(defun im-alisveris-add-to-market ()
  "Add current item to market alisverisi."
  (interactive nil im-alisveris-mode)
  (let ((thing (s-trim (thing-at-point 'line t))))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^## Market")
      (insert (format "\n%s" thing))
      (message "Done."))))

;;;;; im-tuir  -- Emacs tuir (reddit terminal viewer) wrapper

;; Tuir can be installed like:
;;
;; #+begin_src sh
;; python3 -m venv .
;; bin/activate
;; python3 -m pip install tuir-continued
;; #+end_src
;;
;; I had to change the ~open_browser~ function of tuir so that it works
;; properly on MacOS.
;;
;; The file can be found under:
;; =tuir/lib/python3.12/site-packages/tuir/terminal.py=
;;
;; #+begin_src python
;; def open_browser(self, url):
;;     with self.suspend():
;;         if self.config['force_new_browser_window']:
;;             webbrowser.open_new(url)
;;         else:
;;             webbrowser.open_new_tab(url)
;; #+end_src

(defun im-tuir (&optional link)
  "A wrapper for terminal `tuir' command.
If LINK is non-nil, then open the link instead of homepage.  Calling
this function without a link should open the already existing tuir
buffer, otherwise it will create one.  Calling it with link kills the
existing buffer and opens the link in tuir."
  (interactive
   (list
    (cond
     ((s-starts-with? "*reddigg-" (buffer-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "https://\\(www\\.\\)?reddit.com")
          (thing-at-point 'url))))
     (t (thing-at-point 'url)))))
  (require 'eat)
  (if (and (not link) (get-buffer "*tuir*"))
      (switch-to-buffer "*tuir*")
    (let ((eat-buffer-name "*tuir*")
          (eat-kill-buffer-on-exit t)
          (evil-default-state 'insert))
      (when (and link (get-buffer "*tuir*"))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer "*tuir*")))
      (eat
       (let ((path "~/workspace/apps/tuir/bin/tuir"))
         (if link
             (format "%s '%s'" path link)
           path)))
      (with-current-buffer "*tuir*"
        (setq-local evil-escape-inhibit t)))))

;;;;; Global Mode Line formatting

;; Automatically format global mode-line just like I wanted.  There is
;; no easy way of editing the order of items inside
;; `global-mode-string' and no really way to inject adjacent items to
;; already existing items.  So I simply format it just like what I
;; want periodically.

(when tab-bar-show
  (run-at-time 10 45 #'im-update-global-mode-line))

(defun im-update-global-mode-line (&optional force)
  (interactive)
  (let ((all-the-icons-default-adjust 0))
    (setq
     global-mode-string
     `(""
       ,@(when im-is-mic-muted?
           (list
            (all-the-icons-faicon "microphone-slash")
            " | "))
       ,@(when (bound-and-true-p appt-mode-string)
           (list
            (all-the-icons-faicon "calendar-check-o")
            " "
            appt-mode-string))
       ,@(if (and (functionp #'org-clocking-p) (org-clocking-p))
             (list
              (all-the-icons-fileicon "org")
              " "
              org-mode-line-string)
           (list (all-the-icons-fileicon "xmos") " "))
       ,@(when display-time-string
           (list
            (all-the-icons-wicon
             (format "time-%s"
                     ;; Convert to number first so that leading 0s are stripped away
                     (string-to-number (format-time-string "%I"))))
            " "
            display-time-string)))))
  (when force
    (force-mode-line-update)))

(with-eval-after-load 'org
  (add-hook 'org-clock-in-hook #'im-update-global-mode-line)
  (add-hook 'org-clock-out-hook #'im-update-global-mode-line))

;;;;; Set environment variable at point

(defun im-set-env-at-point ()
  "Set the environment variable from an 'export VAR=value' statement on the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "export \\([^= ]+\\)=[\"']?\\([^'\"]*\\)[\"']?")
      (let ((var (match-string 1))
            (value (match-string 2)))
        (setenv var value t)
        (when (equal var "PATH")
          (setq exec-path
                ;; This may alter the ordering but let's hope not.
                (-union (s-split ":" (getenv "PATH")) exec-path)))
        (message "Set environment variable: %s=%s" var value)))))

;;;; Operating system related

;;;;; Sound/audio output chooser

(defun im-osx-select-audio-output ()
  (im-output-select
   :cmd (im-ensure-binary "SwitchAudioSource -t output -a" :package "switchaudio-osx")
   :prompt (let ((current (im-shell-command-to-string "SwitchAudioSource -t output -c")))
             (format "Select audio output (%s): " current))
   :do (progn
         (shell-command-to-string (format "SwitchAudioSource -t output -s '%s'" it))
         (when-let* ((out (-find (lambda (out) (equal it out))
                                 (s-split "\n" (shell-command-to-string "SwitchAudioSource -t input -a")))))
           (when (y-or-n-p (format "Found an output with the same name (%s), switch to it?" out))
             (shell-command-to-string (format "SwitchAudioSource -t input -s '%s'" out)))))))

(defun im-osx-select-audio-input ()
  (im-output-select
   :cmd (im-ensure-binary "SwitchAudioSource -t input -a" :package "switchaudio-osx")
   :prompt (let ((current (im-shell-command-to-string "SwitchAudioSource -t input -c")))
             (format "Select audio input (%s): " current))
   :do (shell-command-to-string (format "SwitchAudioSource -t input -s '%s'" it))))

(defun im-linux-select-audio-output ()
  (let ((sink
         (->>
          (shell-command-to-string
           "pactl list sinks | grep -E 'Name|device.description' | cut -d: -f2 | cut -d= -f2 | tr -d '\"'")
          (s-trim)
          (s-split "\n")
          (mapcar #'s-trim)
          (-partition 2)
          (mapcar #'nreverse)
          (im-alist-completing-read "Select sink: ")
          car)))
    ;; Set default sink
    (shell-command-to-string (format "pactl set-default-sink %s" sink))
    ;; Move inputs to the new sink
    (->>
     (shell-command-to-string "pactl list short sink-inputs | cut -d'\t' -f1")
     (s-trim)
     (s-split "\n")
     (--map (shell-command-to-string (format "pactl move-sink-input %s %s" it sink))))))

(defun im-select-audio-output ()
  (interactive)
  (im-when-on
   :linux (im-linux-select-audio-output)
   :darwin (im-osx-select-audio-output)))

(defun im-select-audio-input ()
  (interactive)
  (im-when-on
   :linux (im-linux-select-audio-input)
   :darwin (im-osx-select-audio-input)))

(defvar im-is-mic-muted? nil)

(defun im-set-mic-status (status)
  "Set mic status to STATUS.
STATUS is either mute, unmute or toggle.

Asks for STATUS if called interactively."
  (interactive
   (list
    (s-chop-suffix
     "d" (cadr (read-multiple-choice "Your mic should be " '((?m "muted") (?u "unmuted") (?t "toggled")))))))
  (im-when-on
   :linux
   (user-error "Not implemented for gnu/linux")
   :darwin
   (set-process-filter
    (start-process "*SwitchAudioSourceMic*" "*SwitchAudioSourceMic*"
                   "SwitchAudioSource" "-t" "input" "-m" status)
    (lambda (proc out)
      (let* ((status (car (s-match "\\(\\(un\\)?muted\\)" (s-trim out))))
             (muted? (equal status "muted"))
             (icon (if muted? "🔇" "🔊")))
        (unless (frame-focus-state)
          (let ((alert-fade-time 5)
                (alert-default-style 'osx-notifier))
            (alert icon
                   :title (format "Mic is %s" status)
                   :persistent nil
                   :never-persist t)))
        (setq im-is-mic-muted? muted?)
        (im-update-global-mode-line :force)
        (message
         "Your mic is %s %s"
         (propertize status 'face 'bold)
         icon))))))

(defun im-toggle-mic ()
  "Toggle the mute status of your microphone."
  (interactive)
  (im-set-mic-status "toggle"))

(im-leader
  "em" #'im-toggle-mic
  "eM" #'im-set-mic-status
  "ea" #'im-select-audio-output
  "eA" #'im-select-audio-input)

;;;;; Bluetooth device connector

(defun im-osx-connect-paired-bluetooth-device ()
  (interactive)
  (im-output-select
   :prompt "Select Bluetooth device: "
   :cmd (im-ensure-binary "blueutil --paired")
   :formatter (->> it
                   (s-split ", ")
                   (--map (s-split ":" it))
                   (--find (equal (car it) "name"))
                   cadr
                   (s-replace "\"" "")
                   s-trim)
   :filter (s-contains? "not connected" it)
   :keep-order nil
   :do (let ((id (->> (s-split "," it) car (s-split ":") cadr s-trim)))
         (message ">> Connecting to %s..." id)
         (shell-command-to-string (format "blueutil --connect %s" id))
         (message ">> Connecting to %s...Connected." id))))

(defun im-linux-connect-paired-bluetooth-device ()
  (interactive)
  (let* ((paired-devices (->>
                          (im-ensure-binary "bluetoothctl devices Paired" :package "bluez-utils")
                          (shell-command-to-string)
                          (s-trim)
                          (s-split "\n")))
         (connected-devices (->>
                             (shell-command-to-string "bluetoothctl devices Connected")
                             (s-trim)
                             (s-split "\n")))
         (device
          (->>
           (-difference paired-devices connected-devices)
           (--map (->>
                   (s-split-up-to " " it 2)
                   (-drop 1)
                   (nreverse)))
           (im-alist-completing-read "Select bluetooth device to connect: ")
           (car))))
    (set-process-filter
     (start-process "im-connect-bluetooth" nil "bluetoothctl" "connect" device)
     (lambda (proc out)
       (cond
        ((s-matches? "Failed to connect" out) (message "%s" out))
        ((s-matches? "Connection successful" out) (message "%s" out)))))))

(defun im-connect-paired-bluetooth-device ()
  (interactive)
  (im-when-on
   :darwin (im-osx-connect-paired-bluetooth-device)
   :linux (im-linux-connect-paired-bluetooth-device)))

(im-leader
  "eb" #'im-connect-paired-bluetooth-device)

;;;;; Switch next monitor input

(defun im-ddcutil-toggle/switch-monitor-input (monitor)
  "Switch to other monitor, USBC, DisplayPort or HDMI."
  (interactive (list (intern (completing-read "Monitor: " '(usbc hdmi dp)))))
  (let* ((target (alist-get monitor '((hdmi . "17")
                                      (dp . "15")
                                      (usbc . "27"))))
         (cmd (im-when-on
               :linux
               (format
                "%s setvcp 0x60 %s"
                (im-ensure-binary "ddcutil" :package "ddcutil" :installer "See index.org")
                target)
               :darwin
               (format
                "%s set input %s"
                (im-ensure-binary "m1ddc" :package "m1ddc" :installer "brew install")
                target))))
    (shell-command-to-string cmd)))

;;;;; Check if screen is shared by Zoom/Chrome etc.

(defun im-screen-sharing-now? ()
  "Check if screen is being shared by Zoom or Google Chrome.
Return a `promise', meant to be used in a async- function.
Only works for OSX right now."
  (im-when-on
   :linux
   (progn
     ;; (warn ">> im-screen-sharing-now? is not implemented for gnu/linux system-type")
     nil)
   :darwin
   (let ((script "
tell application \"System Events\"
    set listOfProcesses to every process whose visible is true
    set output to \"\"
    repeat with aProcess in listOfProcesses
        tell aProcess
            set windowList to every window where its name is not \"\"
            repeat with aWindow in windowList
                set windowName to name of aWindow
                if windowName contains \"zoom share\" or windowName contains \"is sharing your screen\" then
                    return true
                end if
            end repeat
        end tell
    end repeat
    return false
end tell"))
     (promise-new
      (lambda (resolve _reject)
        (make-process :name "check-screen-sharing"
                      :command `("osascript" "-e" ,script)
                      :noquery t
                      :filter
                      (lambda (_proc string)
                        (funcall resolve (equal "true" (s-trim string))))))))))

;;;;; MacOS

;;;;;; Keybindings

(when (eq system-type 'darwin)
  ;; I use an external keyboard, this makes AltGr and Meta (Alt) work as expected
  ;; I have also inverted Meta and Control keys system-wide or something, so
  ;; this setting is done according to that.
  (setq ns-option-modifier 'meta)
  (setq ns-right-alternate-modifier 'none))

;;;;;; Hammerspoon integration

;; I have a menubar that shows my current tab and currently clocked in
;; task. I disable the tab-bar visually and check which tab I
;; currently am in through this Hammerspoon menubar.

(when (eq system-type 'darwin)
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

  (run-with-timer 30 30 #'im-hammerspoon-handle-clock-in))

;;;; Postamble

;; Load the remaining external files that I want to be loaded
(--each
    (directory-files im-load-path t (rx ".el" eos))
  (load it))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(when (and (not (daemonp)) (not (server-running-p)))
  ;; This is good for the cases where emacsclient may be called inside
  ;; emacs (from vterm etc.). Otherwise Emacs acts weird about the
  ;; window placement.
  (setq server-window #'pop-to-buffer)
  (server-start))

(message ">>> Started in %s" (emacs-init-time))

(provide 'init)

;; Here are some variables for this file specifically. Some of them
;; are self-explanatory but I'll go over some of them for clarity:
;;
;; - separedit-default-mode :: All the comments in this file are
;;   formatted with Org mode markup. When I do \\[separedit] on a
;;   comment, an indirect buffer will pop up and normally edit that
;;   section in markdown mode but through this variable I set it to
;;   org.
;; - I don't care about some byte compiler warnings for this file,
;;   which are disabled through =byte-compile-warnings= variable.
;; - In the same vein of the one above, I don't care about some
;;   checkdoc warnings which are disabled through checkdoc-* variables
;;   below.

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; checkdoc-force-docstrings-flag: nil
;; checkdoc--argument-missing-flag: nil
;; separedit-default-mode: org-mode
;; eval: (setq elisp-flymake-byte-compile-load-path load-path)
;; End:
;;; init.el ends here
