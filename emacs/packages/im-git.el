;;; im-git.el --- My git workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/isamert/im-git.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1") (s "1.13.0"))

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

;; This is meant to be used as a replacement for Magit workflow.  In my
;; work computer magit is quite slow due to some management apps
;; intertwining with external process calls.
;;
;; I use either `diff-hl' or `im-git-status' to stage/revert hunks in
;; the file and then I use `im-git-commit' to commit.
;;
;; When using `diff-hl', I simply stage hunks in-file.  I can also use
;; `im-git-stage-region' to stage all hunks in a region.
;;
;; With `im-git-status', I can also stage hunks or full files.
;; Similar to `magit-status' but a very stripped down version.  You
;; still get the best parts.
;;
;; `im-git-commit' has a convenient way to configure a commit through
;; in buffer buttons.  When you do `im-git-commit', you'll get a
;; markdown buffer that you write your commit message into, like
;; below:
;;
;; <message goes here>
;;
;; ⚙ Amend: no
;; ⚙ No Verify: no
;; ⚙ Author: Your Name <your@mail>
;; ⚙ Tag: no
;; ⚙ Fixup: no
;;
;; Clicking/hitting RET on no will toggle no to yes and it will bring
;; old commits message to buffer.  Changing the author line will
;; change the author for the commit.  Tag automatically tags your
;; commit with given string.  Fixup lets you select an older commit
;; and incorporate current changes into that etc...  These all hit 95%
;; of what I use git for, for the rest I simply use the CLI.


;;; Code:

(require 's)
(require 'diff)
(require 'diff-mode)
(require 'outline)
(require 'dash)
(require 'ring)
(require 'markdown-mode)
(require 'page-break-lines)
(require 'async-await)
(require 'general)

;;;; diff-mode improvements

(with-eval-after-load 'diff-mode
  (add-hook 'diff-mode-hook #'outline-minor-mode)

  (evil-define-key 'normal diff-mode-map
    (kbd "RET") #'diff-goto-source
    (kbd "<tab>") #'outline-cycle
    (kbd "<backtab>") #'outline-cycle-buffer))

;;;; im-git-status

;; TODO: Arguments should persist (except amend?). When I toggle No
;; Verify, it should stay toggled for the next im-git-commit call.

(defvar-local im-git-dif--context nil
  "It can be either \\='im-git-status or \\='im-git-commit depending on where the diff is shown.")

(defvar-keymap im-git-diff-mode-map
  "s" #'im-git-stage-hunk-or-file
  "x" #'im-git-reverse-hunk
  "c" #'im-git-commit
  "r" #'im-git-status-reload
  "q" #'im-git-status-cancel
  "C-c C-k" #'im-git-status-cancel
  "1" (λ-interactive (outline-hide-sublevels 1))
  "2" (λ-interactive
       (outline-show-all)
       (outline-hide-body))
  "3" #'outline-show-all)

(general-def
  :keymaps 'im-git-diff-mode-map
  :states 'normal
  "s" #'im-git-stage-hunk-or-file
  "x" #'im-git-reverse-hunk
  "c" #'im-git-status-commit
  "r" #'im-git-status-reload
  "q" #'im-git-status-cancel
  "-" #'diff-split-hunk
  "1" (λ-interactive (outline-hide-sublevels 1))
  "2" (λ-interactive
       (outline-show-all)
       (outline-hide-body))
  "3" #'outline-show-all)

(define-derived-mode im-git-diff-mode diff-mode "DS"
  "Mode to show unstaged git diff."
  (setq buffer-read-only nil)
  (setq buffer-read-only t)
  (setq-local diff-vc-backend 'Git)
  (setq-local im-git-dif--context 'im-git-status)
  (goto-char (point-min)))

(defvar im-git-status--old-window-conf nil)

(defun im-git-status-reload ()
  "Reload current git status window."
  (interactive nil im-git-diff-mode)
  (im-git-status :window-conf im-git-status--old-window-conf)
  (message ">> Reloaded."))

;;;###autoload
(cl-defun im-git-status (&key window-conf)
  (interactive)
  (let* ((default-directory (im-current-project-root))
         (diff (shell-command-to-string "git diff"))
         (dbuff (im-get-reset-buffer "*im-git-diff*")))
    (setq im-git-status--old-window-conf (or window-conf (current-window-configuration)))
    (when (s-blank? diff)
      (if (s-blank? (shell-command-to-string "git diff --staged"))
          (message ">> Nothing changed")
        (when (y-or-n-p ">> All changes are staged.  Commit?")
          (im-git-commit :window-conf im-git-status--old-window-conf)))
      (cl-return-from im-git-status))
    (with-current-buffer dbuff
      (erase-buffer)
      (insert diff)
      (im-git-diff-mode)
      (switch-to-buffer dbuff)
      (delete-other-windows))))

(defun im-git-status-commit ()
  "Like `im-git-commit' but restore the right window cfg when commit finishes."
  (interactive nil im-git-diff-mode)
  (if (eq im-git-dif--context 'im-git-status)
      (im-git-commit :window-conf im-git-status--old-window-conf)
    (im-git-commit :window-conf im-git-commit--old-window-conf)))

(defun im-git-status-cancel ()
  "Cancel."
  (interactive nil im-git-diff-mode)
  (kill-buffer (current-buffer))
  (when (equal im-git-dif--context 'im-git-status)
    (set-window-configuration im-git-status--old-window-conf)))

(defun im-git-diff-at-file? ()
  "Return if cursor is on somewhere around the start of file diff."
  (-let* (((fstart _fend) (diff-bounds-of-file))
          (cursor (point)))
    (save-excursion
      (goto-char fstart)
      (diff-hunk-next)
      (and (< cursor (point))
           (> cursor fstart)))))

(cl-defun im-git-diff-apply (diff &key reverse cached callback)
  "Stage the DIFF.
Call CALLBACK when successful."
  (-let* ((file (make-temp-file "diff_" nil  ".patch" diff)))
    (set-process-sentinel
     (apply
      #'start-process
      "*im-stage-diff*" "*im-stage-diff*"
      "git" "apply" file "--verbose"
      (-non-nil
       (list
        (when reverse "--reverse")
        (when cached "--cached"))))
     (lambda (proc _event)
       (if (eq (process-exit-status proc) 0)
           (when callback (funcall callback))
         (user-error ">> Failed to apply the diff! exitCode=%s"
                     (process-exit-status proc)))))))

(cl-defun im-git-stage-hunk-or-file (&optional reverse callback)
  "Stage the currently selected hunk or file.
If REVERSE is non-nil, than reverse it instead of staging it.  CALLBACK
is called after the hunk is applied with no arguments."
  (interactive nil im-git-diff-mode)
  (-when-let* ((_ (im-git-diff-at-file?))
               ((file) (diff-find-source-location))
               ((fstart fend) (diff-bounds-of-file)))
    (when (y-or-n-p (format "%sStage whole file: %s?" (if reverse "Un" "") file))
      (im-git-diff-apply
       (buffer-substring-no-properties fstart fend)
       :cached t
       :reverse reverse
       :callback
       (lambda ()
         (diff-file-kill)
         (message ">> File %sstaged successfully!" (if reverse "un" ""))
         (when callback (funcall callback)))))
    (cl-return-from im-git-stage-hunk-or-file))
  (-let* (((hstart hend) (diff-bounds-of-hunk))
          (hunk (buffer-substring-no-properties hstart hend))
          (header (save-excursion
                    (buffer-substring-no-properties
                     (diff-beginning-of-file)
                     (progn
                       (diff-hunk-next)
                       (point)))))
          (pt (point)))
    (im-git-diff-apply
     (concat header hunk)
     :cached t
     :reverse reverse
     :callback
     (lambda ()
       (save-excursion
         (goto-char pt)
         (diff-hunk-kill))
       (message ">> Hunk applied successfully!")
       (when callback (funcall callback))))))

(defun im-git-unstage-hunk-or-file ()
  "Unstage the currently selected hunk or file."
  (interactive nil im-git-staged-diff-mode)
  (im-git-stage-hunk-or-file :reverse))

;; FIXME(1OKAkW): It does not work in im-git-staged-diff-mode because
;; we need to unstage it first and then reverse (or reverse and stage
;; the reversed diff)
(cl-defun im-git-reverse-hunk ()
  "Reverse hunk at point."
  (interactive nil im-git-diff-mode)
  (-when-let* ((_ (im-git-diff-at-file?))
               ((file) (diff-find-source-location))
               ((fstart fend) (diff-bounds-of-file)))
    (when (y-or-n-p (format "Revert whole file: %s?" file))
      (im-git-diff-apply
       (buffer-substring-no-properties fstart fend)
       :reverse t
       :callback
       (lambda ()
         (diff-file-kill)
         (message ">> Reverted: %s" file))))
    (cl-return-from im-git-reverse-hunk))
  (pcase-let ((`(,buf ,_line-offset ,_pos ,_src ,_dst ,_switched)
               (diff-find-source-location nil nil)))
    (when (y-or-n-p "Really want to revert?")
      (save-window-excursion
        (save-excursion
          ;; Widen the buffer if there is a restriction so that
          ;; `diff-apply-hunk' can work.  We can't easily restore
          ;; the restriction because the restriction bounds may
          ;; have been changed by `diff-apply-hunk'.  Sure, we can
          ;; calculate but as of now, I don't need it.
          (ignore-errors
            (with-current-buffer (find-file-noselect (diff-find-file-name))
              (when (buffer-narrowed-p)
                (widen))))
          (diff-apply-hunk :reverse)
          (with-current-buffer buf
            (save-buffer))))
      (diff-hunk-kill))))

;;;; im-git-commit

(defvar im-git-commit-finished-hook '()
  "Functions to run after successfully committing.
Each function is called with COMMIT-MSG, inside project root.")

(defconst im-git-commit-message-buffer "*im-git-commit-message*")
(defconst im-git-commit-diff-buffer "*im-git-diff-staged*")
(defconst im-git-commit-config-prefix "⚙")
(defconst im-git--status-filename-prefix "〉")
(defvar im-git-commit--old-window-conf nil)
(defvar im-git-commit-message-history (make-ring 100))
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'im-git-commit-message-history))
(defvar-local im-git-commit--current-message-ref nil)
(defvar-local im-git-commit--template nil)

;;;###autoload
(cl-defun im-git-commit (&key window-conf)
  "Commit staged changes.
If commit is called from another command, and when commit
finishes or discard you want to restore an older window
configuration, pass it as WINDOW-CONF."
  (interactive)
  (let* ((default-directory (im-current-project-root))
         (diff (shell-command-to-string "git diff --staged"))
         (commit-buffer (im-get-reset-buffer im-git-commit-message-buffer)))
    (when (and (s-blank? diff) (not (y-or-n-p "> Nothing staged. Still want to commit?")))
      (user-error ">> Commit aborted"))
    (setq im-git-commit--old-window-conf (or window-conf (current-window-configuration)))
    (switch-to-buffer commit-buffer)
    (im-git-commit-mode)
    (delete-other-windows)
    (select-window (split-window-right))
    (switch-to-buffer (im-git-commit--reload-diff-buffer diff))
    (other-window 1)))

(defun im-git-commit--reload-diff-buffer (diff)
  (with-current-buffer (im-get-reset-buffer im-git-commit-diff-buffer)
    (insert diff)
    (setq buffer-read-only t)
    (im-git-staged-diff-mode)
    (goto-char (point-min))
    (setq-local diff-vc-backend 'Git)
    (current-buffer)))

(defun im-git-commit-reload ()
  "Reload the diff."
  (interactive nil im-git-commit-mode)
  (im-git-commit :window-conf im-git-commit--old-window-conf)
  (message ">> Reloaded."))

(defun im-git-commit-finalize ()
  "Finalize the commit in progress."
  (interactive)
  (let* ((lines (s-split
                 "\n"
                 (with-current-buffer im-git-commit-message-buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (msg (->>
               (--take-while (not (equal "" it)) lines)
               (s-join "\n")
               (s-trim)))
         (props (->>
                 lines
                 (--filter (s-starts-with? im-git-commit-config-prefix it))
                 (--map (-let [(key val) (s-split-up-to ":" (s-chop-left 1 it) 1)]
                          (setq key (concat "--" (s-dashed-words (s-trim key))))
                          (setq val (s-trim val))
                          (pcase val
                            ("yes" (list key))
                            ("no" '())
                            (_ (list key val)))))))
         (non-args '("--tag"))
         (args (-flatten (--filter (not (-contains? non-args (car it))) props)))
         (fixup (cadr (--find (equal (car it) "--fixup") props)))
         (tag (cadr (--find (equal (car it) "--tag") props))))
    (unless fixup
      (setq args (append (list "--message" msg) args)))
    (ring-insert
     im-git-commit-message-history
     ;; This command is supposed to run at project root, so the
     ;; `default-directory' is assumed to be the project root.
     (list :project default-directory :msg msg))
    (set-process-sentinel
     (apply #'start-process "*im-git-commit*" "*im-git-commit*" "git" "commit" args)
     (lambda (proc _event)
       (if (eq (process-exit-status proc) 0)
           (progn
             (message "im-git-commit :: Committed")
             (--each im-git-commit-finished-hook (funcall it msg))
             (when tag
               (set-process-sentinel
                (start-process "*im-git-tag*" " *im-git-tag*" "git" "tag" tag)
                (lambda (proc _event)
                  (if (eq (process-exit-status proc) 0)
                      (message ">> im-git-commit :: Tag created")
                    (message "!! Failed to tag. See *im-git-tag* buffer for further details.")))))
             (when fixup
               (let ((process-environment `("GIT_SEQUENCE_EDITOR=true" ,@process-environment)))
                 (set-process-sentinel
                  (start-process "*im-git-fixup*" " *im-git-fixup*"
                                 "git" "rebase" "--interactive" "--autosquash" (concat fixup "^"))
                  (lambda (proc _event)
                    (if (eq (process-exit-status proc) 0)
                        (message ">> im-git-fixup :: Commit %s fixed." fixup)
                      (message "!! Failed to fixup. See *im-git-fixup* buffer for further details.")))))) )
         (message "im-git-commit :: Failed. See buffer *im-git-commit*"))))
    (im-git-commit-cancel)))

(defun im-git-commit-cancel ()
  "Cancel the commit in progress."
  (interactive)
  (kill-buffer im-git-commit-message-buffer)
  (kill-buffer im-git-commit-diff-buffer)
  (set-window-configuration im-git-commit--old-window-conf))

(defun im-git-commit-prev-message ()
  (interactive nil im-git-commit-mode)
  (let ((curr (1+ (or im-git-commit--current-message-ref -1))))
    (when (>= curr (ring-length im-git-commit-message-history))
      (user-error "End of history"))
    (when-let ((old (im-git-commit--reset-message (plist-get (ring-ref im-git-commit-message-history curr) :msg))))
      (when (= curr 0)
        (setq im-git-commit--template old)))
    (setq im-git-commit--current-message-ref curr)))

(defun im-git-commit-next-message ()
  (interactive nil im-git-commit-mode)
  (let ((curr (1- (or im-git-commit--current-message-ref 0))))
    (im-git-commit--reset-message
     (cl-case curr
       (-1 im-git-commit--template)
       (-2 (user-error "Beginning of history"))
       (otherwise (plist-get (ring-ref im-git-commit-message-history curr) :msg))))
    (setq im-git-commit--current-message-ref curr)))

(defvar-keymap im-git-commit-mode-map
  "C-c C-c" #'im-git-commit-finalize
  "C-c C-k" #'im-git-commit-cancel
  "C-c C-r" #'im-git-commit-reload
  "C-c C-p" #'im-git-commit-prev-message
  "C-c C-n" #'im-git-commit-next-message)

(general-def :keymaps 'im-git-commit-mode-map :states 'normal
  "gr" #'im-git-commit-reload
  "gr" #'im-git-commit-reload
  "gj" #'im-git-commit-next-message
  "gk" #'im-git-commit-prev-message)

(define-derived-mode im-git-commit-mode markdown-mode "CM"
  "Commit message editing mode."
  (require 'whitespace)
  (setq-local header-line-format "`C-c C-c' to commit, `C-c C-k' to discard.")
  (setq-local whitespace-line-column 72)
  (setq-local whitespace-style '(face empty tabs lines-tail trailing))
  ;; FIXME: First line should not exceed 50 chars, how to indicate that?
  (whitespace-mode +1)
  (display-fill-column-indicator-mode +1)
  (page-break-lines-mode)
  (insert "\n\n")
  (insert "\n\n\n")
  (insert "# Status\n\n")
  (insert "# Settings\n\n")
  (insert im-git-commit-config-prefix " No Verify: ")
  (im-insert-toggle-button "no" "yes" :help "RET: Toggle no-verify")
  (insert "\n")
  (insert im-git-commit-config-prefix " Amend: ")
  (im-insert-toggle-button
   "no" "yes"
   :help "RET: Toggle amend"
   :on-toggle
   (lambda (state)
     (when (and (equal state "yes") (y-or-n-p "Use old commit message?"))
       (im-git-commit--reset-message (s-trim (shell-command-to-string "git log -1 --pretty=%B"))))))
  (insert "\n")
  (insert im-git-commit-config-prefix " Author: AUTHOR_NAME <AUTHOR_MAIL>\n")
  (insert im-git-commit-config-prefix " Tag: ")
  (im-insert-toggle-button "no" (lambda () (read-string "Tag: ")) :help "RET: Toggle tagging")
  (insert "\n")
  (insert im-git-commit-config-prefix " Fixup: ")
  (insert-text-button
   "no"
   'action (lambda (button)
             (let ((start (button-start button))
                   (end (button-end button))
                   (action (button-get button 'action)))
               (im-git-select-commit
                (lambda (tag)
                  (delete-region start end)
                  (insert-text-button tag 'action action 'follow-link t)
                  (im-git-commit--reset-message "<!-- No need to update the message -->")))))
   'kbd-help "RET: Select commit to fixup"
   'follow-link t)
  (insert "\n")
  (goto-char (point-min))
  (im-help-at-point-mode)
  (im-git-commit--setup (current-buffer)))

(async-defun im-git-commit--setup (buffer)
  "Fill the commit BUFFER without blocking."
  (let ((name  (await (lab--git "config" "--get" "user.name")))
        (email (await (lab--git "config" "--get" "user.email"))))
    (with-current-buffer buffer
      (replace-regexp "AUTHOR_NAME" name  nil (point-min) (point-max))
      (replace-regexp "AUTHOR_MAIL" email nil (point-min) (point-max))
      (await (im-git-commit--update-unstaged))
      (goto-char (point-min)))))

(defmacro im-git-commit--change-header-contents (header &rest forms)
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-min))
     (re-search-forward (format "\n# *%s" ,header))
     (end-of-line)
     (delete-region
      (point)
      (or
       (progn (when (search-forward "\n# " nil t) (- (point) 3)))
       (point-max)))
     (backward-char 2)
     (insert "\n")
     ,@forms
     (insert "\n")))

;; TODO: Predictable sort order
(async-defun im-git-commit--update-unstaged ()
  (im-git-commit--change-header-contents "Status"
    (--each-indexed (s-lines (ansi-color-apply
                              (await (lab--git
                                      "-c" "color.status=always"
                                      "status" "--branch" "--short"))))
      (if (= it-index 0)
          (insert (s-chop-prefix "## " it) "\n")
        (let* ((start (point))
               (overlay (progn
                          (insert (s-prepend (concat im-git--status-filename-prefix " ") it) "\n")
                          (make-overlay start (1- (point))))))
          (overlay-put overlay 'keymap im-git-commit-status-map))))))

(defvar-keymap im-git-commit-status-map
  "u" #'im-git-commit-unstage-at-point
  "s" #'im-git-commit-stage-at-point
  "TAB" #'im-git-commit-diff-at-point
  "RET" #'im-git-commit-diff-at-point)

(defun im-git-commit--file-at-point (&optional line)
  (let ((line (or line (thing-at-point 'line t))))
    (-as->
     line %
     (s-chop-prefix im-git--status-filename-prefix %)
     (s-trim %)
     (s-split " " % t)
     (nth 1 %))))

(async-defun im-git-commit--run-command-on-file-at-point (&rest git-args)
  (let ((line (line-number-at-pos))
        (files (if (use-region-p)
                   (->>
                    (buffer-substring-no-properties (region-beginning) (region-end))
                    (s-trim)
                    (s-lines)
                    (-map #'im-git-commit--file-at-point))
                 (list (im-git-commit--file-at-point)))))
    (await (apply #'lab--git (append git-args files)))
    (await (im-git-commit--update-unstaged))
    (deactivate-mark)
    (goto-line line)
    (let ((diff (await (lab--git "diff" "--no-color" "--staged"))))
      (switch-to-buffer-other-window (im-git-commit--reload-diff-buffer diff))
      (other-window 1))))

(async-defun im-git-commit-diff-at-point ()
  (interactive)
  (if (im-peek-open?)
      (im-peek-remove)
    (let* ((default-directory (im-current-project-root))
           (file (im-git-commit--file-at-point))
           (diff (s-trim (await (lab--git "diff" file))))
           (result (if (s-blank? diff)
                       ;; TODO highlight file
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                     (ansi-color-apply diff))))
      (im-peek
       (lambda ()
         (with-current-buffer (im-get-reset-buffer "*im-commit-diff-at-point*")
           (erase-buffer)
           (insert result)
           (im-git-diff-mode)
           (setq im-git-dif--context 'im-git-commit)
           (current-buffer)))))))

(async-defun im-git-commit-stage-at-point ()
  (interactive)
  (im-peek-remove)
  (im-git-commit--run-command-on-file-at-point "add"))

(async-defun im-git-commit-unstage-at-point ()
  (interactive)
  (im-peek-remove)
  (im-git-commit--run-command-on-file-at-point "restore" "--staged"))

(defun im-git-commit--reset-message (str)
  "Clear the current message in the buffer and set it to STR.
Return old message."
  (goto-char (point-min))
  (insert str)
  (let* ((s (point))
         (e (- (search-forward "\n") 3))
         (old (buffer-substring s e)))
    (delete-region s e)
    (goto-char (point-min))
    old))

(defvar-keymap im-git-staged-diff-mode-map
  "x" #'im-git-reverse-hunk
  "u" #'im-git-unstage-hunk-or-file
  "C-c C-c" #'im-git-commit-finalize
  "C-c C-k" #'im-git-commit-cancel
  "-" #'diff-split-hunk
  "1" (λ-interactive (outline-hide-sublevels 1))
  "2" (λ-interactive
       (outline-show-all)
       (outline-hide-body))
  "3" #'outline-show-all)

(define-derived-mode im-git-staged-diff-mode diff-mode "SD"
  "Mode for showing staged changes.")

(general-def
  :keymaps 'im-git-staged-diff-mode-map
  :states 'normal
  "u" #'im-git-unstage-hunk-or-file
  ;; FIXME: see 1OKAkW
  ;; "x" #'im-git-reverse-hunk
  "1" (λ-interactive (outline-hide-sublevels 1))
  "2" (λ-interactive
       (outline-show-all)
       (outline-hide-body))
  "3" #'outline-show-all)

;;;; im-git-select-commit

;; An interface for selecting a previous commit.

(defvar-local im-git-select-commit-finalize-callback nil)

(defun im-git-select-commit (callback)
  "Select a commit.
CALLBACK will be called with the selected commit ref."
  (interactive)
  (let ((buffer (save-window-excursion
                  (vc-print-root-log)
                  (current-buffer))))
    (switch-to-buffer buffer)
    (setq im-git-select-commit-finalize-callback callback)
    (im-git-select-commit-mode)))

(defun im-git-select-commit-finalize ()
  (interactive)
  (im-git-select-commit-mode -1)
  (let ((tag (log-view-current-tag (point)))
        (fn im-git-select-commit-finalize-callback))
    (kill-buffer)
    (funcall fn tag)))

(define-minor-mode im-git-select-commit-mode
  "Select a commit."
  :lighter " SelectCommit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'im-git-select-commit-finalize)
            map))

;;;; im-git-commit-amend

;;;###autoload
(defun im-git-commit-amend ()
  "Asks you a message and does `git commit --amend -m ...'."
  (interactive)
  (let ((buffer-name "*im-git-amend*")
        (message (read-string
                  "Message: "
                  (s-trim (shell-command-to-string "git log -1 --pretty=%B")))))
    (im-shell-command
     :command "git"
     :args `("commit" "--amend" "-m" ,message)
     :switch nil
     :buffer-name buffer-name
     :on-finish (lambda (&rest _) (message ">> Amended"))
     :on-fail (lambda (&rest _)
                (message ">> Amend failed!")
                (switch-to-buffer buffer-name)))))

;;;; im-git-stash

(defconst im-git--stash-list-buffer-name "*im-git-stash-list*")

;;;###autoload
(defun im-git-list-stash ()
  (interactive)
  (when-let ((buffer (get-buffer im-git--stash-list-buffer-name)))
    (kill-buffer buffer))
  (let ((buffer-name im-git--stash-list-buffer-name))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "stash" "list")
     :switch t
     :buffer-name buffer-name
     :on-finish
     (lambda (output &rest _)
       (read-only-mode)
       (text-mode)
       (im-git-stash-list-mode))
     :on-fail
     (lambda (&rest _)
       (message ">> `git stash show -p' failed!")
       (switch-to-buffer buffer-name)))))

;;;###autoload
(defun im-git-show-stash-diff (&optional stash-entry)
  "Show stashed diff."
  (interactive (list (im-git--parse-stash-entry-at-point)))
  (let ((buffer-name "*im-git-stash-diff*"))
    (im-shell-command
     :command "git"
     :args (-non-nil `("--no-pager" "stash" "show" "-p" ,stash-entry))
     :switch nil
     :buffer-name buffer-name
     :on-finish
     (lambda (output &rest _)
       (with-current-buffer buffer-name
         ;; TODO: gd → drop stash & show next stash
         ;; TODO: x → drop hunk from the stash? gerekli mi cok
         ;;       bilemedim ama ise yarar sanki baya.
         ;; (setq header-line-format "`gd' → drop stash & refresh")
         (diff-mode)
         (im-git-diff-mode)
         (switch-to-buffer (current-buffer))))
     :on-fail
     (lambda (&rest _)
       (message ">> `git stash show -p' failed!")
       (switch-to-buffer buffer-name)))))

(defun im-git-drop-stash ()
  (interactive nil im-git-stash-list-mode)
  (when-let* ((stash-entry (im-git--parse-stash-entry-at-point))
              (begin (line-beginning-position))
              (end (line-end-position)))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "stash" "drop" ,stash-entry)
     :switch nil
     :buffer-name " *im-git-stash-drop*"
     :on-finish
     (lambda (output &rest _)
       (with-current-buffer im-git--stash-list-buffer-name
         (let ((inhibit-read-only t))
           (add-text-properties begin end '(face (:strike-through t))))))
     :on-fail
     (lambda (output &rest _)
       (message ">> `git stash drop' failed with: " output)
       (switch-to-buffer buffer-name)))))

(define-minor-mode im-git-stash-list-mode
  "A minor mode for interacting with git stash list."
  :lighter " StashList"
  :keymap (make-sparse-keymap))

(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'im-git-stash-list-mode
    (kbd "x") #'im-git-drop-stash
    (kbd "RET") #'im-git-show-stash-diff))

(defun im-git--parse-stash-entry-at-point ()
  "Return the stash entry name at point.
Works only if the stash entry is at the beginning of the line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "stash@{[0-9]+}")
      (match-string 0))))

(provide 'im-git)
;;; im-git.el ends here
