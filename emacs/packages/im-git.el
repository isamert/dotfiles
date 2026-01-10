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
(require 'f)
(require 'diff)
(require 'diff-mode)
(require 'outline)
(require 'dash)
(require 'ring)
(require 'markdown-mode)
(require 'page-break-lines)
(require 'async-await)
(require 'general)
(require 'im)
(require 'lab)

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
  "C-c C-k" #'im-git-status-cancel)

(general-def
  :keymaps 'im-git-diff-mode-map
  :states 'normal
  "a" #'diff-apply-hunk
  "s" #'im-git-stage-hunk-or-file
  "x" #'im-git-reverse-hunk
  "c" #'im-git-status-commit
  "r" #'im-git-status-reload
  "q" #'im-git-status-cancel)

(define-derived-mode im-git-diff-mode diff-mode "DS"
  "Mode to show unstaged git diff."
  (setq buffer-read-only nil)
  (setq buffer-read-only t)
  (setq-local diff-vc-backend 'Git)
  (setq-local im-git-dif--context 'im-git-status)
  (goto-char (point-min)))

(defvar im-git-status--old-window-conf nil)
(defconst im-git-status-buffer "*im-git-diff*")

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
         (dbuff (im-get-reset-buffer im-git-status-buffer)))
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
  (when (equal im-git-dif--context 'im-git-status)
    (set-window-configuration im-git-status--old-window-conf))
  (kill-buffer im-git-status-buffer))

(defun im-git-diff-at-file? ()
  "Return if cursor is on somewhere around the start of file diff."
  (-let* (((fstart _fend) (diff-bounds-of-file))
          (cursor (point)))
    (save-excursion
      (goto-char fstart)
      (diff-hunk-next)
      (and (<= cursor (point))
           (>= cursor fstart)))))

(cl-defun im-git-diff-apply (diff &key reverse cached callback)
  "Stage the DIFF.
Call CALLBACK when successful."
  (-let* ((file (make-temp-file "diff_" nil  ".patch" diff)))
    (set-process-sentinel
     (apply
      #'start-process
      "*im-stage-diff*" (im-get-reset-buffer " *im-stage-diff*")
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
       (when callback (funcall callback))
       ;; Update the git commit message buffer, if open.  It is
       ;; possible to display this diff in another frame and have git
       ;; commit buffer open in another.
       (when-let* ((commit-msg-buffer (get-buffer im-git-commit-message-buffer))
                   (window (car (get-buffer-window-list commit-msg-buffer nil t)))
                   ;; If we are already in the staged diff, no need to do this
                   (_ (not (equal (buffer-name) im-git-commit-diff-buffer))))
         (with-selected-frame (window-frame window)
           (save-window-excursion
             (with-current-buffer commit-msg-buffer
               (im-git-commit-reload)))))))))

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

(defvar im-git-commit-pre-hook '()
  "Functions to run after opening the commit window.
Each function is called with DIFF, inside project root.")

(defconst im-git-commit-message-buffer "*im-git-commit-message*")
(defconst im-git-commit-diff-buffer "*im-git-diff-staged*")
(defconst im-git-commit-config-prefix "⚙")
(defconst im-git--status-filename-prefix "▶")
(defvar im-git-commit--old-window-conf nil)
(defvar im-git-commit-message-history (make-ring 100))
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'im-git-commit-message-history))
(defvar-local im-git-commit--current-message-ref nil)
(defvar-local im-git-commit--template nil)
(defvar-local im-git-commit--diff nil)

;;;###autoload
(cl-defun im-git-commit (&key window-conf initial-message)
  "Commit staged changes.
If commit is called from another command, and when commit
finishes or discard you want to restore an older window
configuration, pass it as WINDOW-CONF."
  (interactive)
  (let* ((default-directory (im-current-project-root))
         (diff (shell-command-to-string "git diff --staged"))
         (commit-buffer (im-get-reset-buffer im-git-commit-message-buffer)))
    (when (and (s-blank? diff) (not (y-or-n-p "> Nothing staged.  Still want to commit?")))
      (user-error ">> Commit aborted"))
    (setq im-git-commit--old-window-conf (or window-conf (current-window-configuration)))
    (setq im-git-commit--diff diff)
    (switch-to-buffer commit-buffer)
    (im-git-commit-mode)
    (im-git-commit--setup (current-buffer) initial-message)
    (delete-other-windows)
    (select-window (split-window-right))
    (switch-to-buffer (im-git-commit--reload-diff-buffer diff))
    (select-window (get-buffer-window im-git-commit-message-buffer))))

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
  (im-git-commit :window-conf im-git-commit--old-window-conf
                 :initial-message (im-git-commit--parse-commit-message))
  (message ">> Reloaded."))

(defun im-git-commit--parse-commit-message ()
  (with-current-buffer im-git-commit-message-buffer
    (s-trim
     (replace-regexp-in-string
      "<!--\\(.\\|\n\\)*?-->" ""
      (buffer-substring-no-properties
       (point-min)
       (save-excursion
         (goto-char (point-min))
         (if (search-forward "" nil t)
             (match-beginning 0)
           (point-max))))))))

(defun im-git-commit-finalize ()
  "Finalize the commit in progress."
  (interactive)
  (let* ((lines (s-split
                 "\n"
                 (with-current-buffer im-git-commit-message-buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
         (msg (im-git-commit--parse-commit-message))
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
    (let ((start-time (float-time))
          (proj (im-current-project-name)))
      (message "im-git-commit :: Started...")
      (set-process-sentinel
       (apply #'start-process "*im-git-commit*" (im-get-reset-buffer " *im-git-commit*") "git" "commit" args)
       (lambda (proc _event)
         (let ((notify? (> (- (float-time) start-time) 2)))
           (if (eq (process-exit-status proc) 0)
               (progn
                 (message "im-git-commit :: Started...Done")
                 (when notify?
                   (im-notif :title "*git-commit*" :message (format "Commit finished for %s" proj) :duration 2))
                 (--each im-git-commit-finished-hook (funcall it msg))
                 (when tag
                   (set-process-sentinel
                    (start-process "*im-git-tag*" (im-get-reset-buffer " *im-git-tag*") "git" "tag" tag)
                    (lambda (proc _event)
                      (if (eq (process-exit-status proc) 0)
                          (message ">> im-git-commit :: Tag created")
                        (message "!! Failed to tag. See *im-git-tag* buffer for further details.")))))
                 (when fixup
                   (let ((process-environment `("GIT_SEQUENCE_EDITOR=true" ,@process-environment)))
                     (set-process-sentinel
                      (start-process "*im-git-fixup*" (im-get-reset-buffer " *im-git-fixup*")
                                     "git" "rebase" "--interactive" "--autosquash" (concat fixup "^"))
                      (lambda (proc _event)
                        (if (eq (process-exit-status proc) 0)
                            (message ">> im-git-fixup :: Commit %s fixed." fixup)
                          (message "!! Failed to fixup. See *im-git-fixup* buffer for further details.")))))) )
             (message "im-git-commit :: Failed. See buffer *im-git-commit*")
             (when notify?
               (im-notif :title "*git-commit*" :message (format "Commit FAILED for %s" proj) :duration 2)))))))
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
    (when-let* ((old (im-git-commit--reset-message (plist-get (ring-ref im-git-commit-message-history curr) :msg))))
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
  (setq-local header-line-format (substitute-command-keys "Commit :: \\[im-git-commit-finalize] → commit, \\[im-git-commit-cancel] → discard, \\[im-git-commit-reload] → reload buffer, \\[im-git-commit-prev-message] → prev, \\[im-git-commit-next-message] → next"))
  (setq-local whitespace-line-column 72)
  (setq-local whitespace-style '(face empty tabs lines-tail trailing))
  ;; FIXME: First line should not exceed 50 chars, how to indicate that?
  (whitespace-mode +1)
  (display-fill-column-indicator-mode +1)
  (page-break-lines-mode)
  (insert "\n\n")
  (insert "\n\n\n")
  (insert "# Status\n")
  (insert "# Settings\n")
  (insert "# Last commits\n")
  (goto-char (point-min))
  (im-help-at-point-mode))

(async-defun im-git-commit--setup (buffer &optional initial-message)
  "Fill the commit BUFFER without blocking."
  (let* ((start-time (float-time))
         (namep  (lab--git "config" "--get" "user.name"))
         (emailp (lab--git "config" "--get" "user.email"))
         (hookspathp (lab--git "config" "--default" ".git/hooks" "--get" "core.hooksPath"))
         (commitsp (lab--git "log"
                             "-10" "--color=always"
                             "--graph" "--decorate" "--date=short"
                             "--pretty=tformat:'%C(cyan)%d%C(reset)%C(yellow)%h%C(reset)..: %C(green)%an %C(blue)%ad%C(reset) %s'"
                             "--abbrev-commit"))
         (statusoutp (await (lab--git "-c" "color.status=always" "status" "--branch" "--short")))
         (name (await namep))
         (email (await emailp))
         (hookspath (await hookspathp))
         (commits (ansi-color-apply (await commitsp)))
         (statusout (await statusoutp))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (goto-char (point-min))
      (im-git-commit--change-header-contents "Last commits"
        (let ((start (point)))
          (insert "\n" commits)
          (let ((overlay (make-overlay start (point))))
            (overlay-put overlay 'keymap im-git-commit-log-map))))
      (im-git-commit--change-header-contents "Settings"
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
                        (let ((inhibit-read-only t))
                          (delete-region start end)
                          (insert-text-button tag 'action action 'follow-link t)
                          (im-git-commit--reset-message "<!-- You can't update the message -->"))))))
         'kbd-help "RET: Select commit to fixup"
         'follow-link t)
        ;; TODO: maybe also add this version so that I can also change the commit message if I want
        ;; git commit --squash=<commit-hash> -m "New message you want"
        ;; git rebase --autosquash --no-edit <commit-hash>^
        (insert "\n"))
      (when (re-search-forward "AUTHOR_NAME" nil t)
        (replace-match name t t))
      (when (re-search-forward "AUTHOR_MAIL" nil t)
        (replace-match email t t))
      (im-git-commit--update-unstaged)
      (dolist (hook im-git-commit-pre-hook)
        (await (funcall hook im-git-commit--diff)))
      (goto-char (point-min))
      ;; Insert prepared commit message by the git hooks
      (when-let* ((prepare-commit-msg-hook (f-expand
                                            (f-join
                                             (or hookspath ".git/hooks")
                                             "prepare-commit-msg")))
                  (tmp-commit-msg-file "/tmp/im-git-commit-msg")
                  (_ (f-exists? prepare-commit-msg-hook))
                  (_ (not initial-message)))
        (when (f-exists? tmp-commit-msg-file)
          (delete-file tmp-commit-msg-file))
        (call-process
         prepare-commit-msg-hook nil nil nil tmp-commit-msg-file)
        (when (f-exists? tmp-commit-msg-file)
          (insert (with-temp-buffer
                    (insert-file-contents tmp-commit-msg-file)
                    (goto-char (point-min))
                    (while (re-search-forward "^#\\(.*\\)$" nil t)
                      (replace-match "<!--\\1 -->" t))
                    (buffer-substring-no-properties (point-min) (point-max))))))
      (when initial-message
        (insert initial-message))
      (goto-char (point-min)))
    (message "Ready in %.2f seconds" (- (float-time) start-time))))

(defmacro im-git-commit--change-header-contents (header &rest forms)
  (declare (indent 1))
  `(save-excursion
     (let ((inhibit-read-only t)
           start end)
       (goto-char (point-min))
       (re-search-forward (format "\n# %s" ,header))
       (beginning-of-line)
       (setq start (point))
       (end-of-line)
       (delete-region
        (point)
        (if (search-forward "\n# " nil t)
            (prog1 (- (point) 3)
              (backward-char 2))
          (point-max)))
       ,@forms
       (insert "\n")
       (setq end (point))
       (add-text-properties start end '(read-only t)))))

(defvar-keymap im-git-commit-status-map
  "u" #'im-git-commit-unstage-at-point
  "s" #'im-git-commit-stage-at-point
  "x" #'im-git-commit-delete-at-point
  "TAB" #'im-git-commit-diff-at-point
  "RET" #'im-git-commit-diff-at-point-popup)

(defvar-keymap im-git-commit-log-map
  "RET" #'im-git-commit-log-diff-at-point)

;; TODO: Predictable sort order
(async-defun im-git-commit--update-unstaged (&optional output)
  (im-git-commit--change-header-contents "Status"
    (--each-indexed (s-lines (ansi-color-apply
                              (s-trim (or output (await (lab--git
                                                         "-c" "color.status=always"
                                                         "status" "--branch" "--short"))))))
      (if (= it-index 0)
          (insert (s-chop-prefix "## " it) "\n")
        (let* ((start (point))
               (overlay (progn
                          (insert (s-prepend (concat im-git--status-filename-prefix " ") it) "\n")
                          (make-overlay start (1- (point))))))
          (overlay-put overlay 'keymap im-git-commit-status-map)
          (overlay-put overlay 'help-echo
                       (lambda (_window _obj _pos)
                         (substitute-command-keys "\\[im-git-commit-stage-at-point] → Stage file, \\[im-git-commit-unstage-at-point] → Unstage file, \\[im-git-commit-delete-at-point] → Delete file, \\[im-git-commit-diff-at-point] → Diff | You can select multiple files by selecting region."))))))))

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
    (when git-args
      (await (apply #'lab--git (append git-args files))))
    (await (im-git-commit--update-unstaged))
    (deactivate-mark)
    (goto-line line)
    (let ((diff (await (lab--git "diff" "--no-color" "--staged"))))
      (switch-to-buffer-other-window (im-git-commit--reload-diff-buffer diff))
      (other-window 1))))

(async-defun im-git-commit-diff-at-point (&optional popup?)
  (interactive nil im-git-commit-mode)
  (if (im-peek-open?)
      (im-peek-remove)
    (let* ((default-directory (im-current-project-root))
           (file (im-git-commit--file-at-point))
           (diff (concat (await (lab--git "diff" file)) "\n"))
           (result (if (s-blank? diff)
                       ;; TODO highlight file
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                     (ansi-color-apply diff)))
           (diff-buffer (with-current-buffer (im-get-reset-buffer " *im-commit-diff-at-point*")
                          (erase-buffer)
                          (insert result)
                          (im-git-diff-mode)
                          (setq im-git-dif--context 'im-git-commit)
                          (font-lock-ensure)
                          (current-buffer))))
      (if popup?
          (im-display-buffer-other-frame diff-buffer)
        (im-peek (lambda () diff-buffer))))))

(defun im-git-commit-diff-at-point-popup ()
  (interactive nil im-git-commit-mode)
  (im-git-commit-diff-at-point 'popup))

(async-defun im-git-commit-delete-at-point ()
  (interactive)
  (im-peek-remove)
  (when-let* ((file (im-git-commit--file-at-point)))
    (when (and (file-exists-p file)
               (y-or-n-p (format "Do you really want to delete this file: %s?" file)))
      (delete-file file)))
  (im-git-commit--run-command-on-file-at-point))

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

(defun im-git-commit-log-diff-at-point ()
  (interactive nil im-git-commit-mode)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\b\\([0-9a-fA-F]+\\)\\." (line-end-position) t)
      (let* ((sha (match-string 1))
             (line (thing-at-point 'line)))
        (let ((buf (im-get-reset-buffer " *im-commit-diff*")))
          (with-current-buffer buf
            (erase-buffer)
            (diff-mode)
            (setq header-line-format (format " Commit :: %s" (s-trim line)))
            (call-process "git" nil buf t "diff" (concat sha "^") sha)
            (goto-char (point-min))
            (font-lock-ensure))
          (pop-to-buffer buf))))))

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
            map)
  (setq header-line-format
        (substitute-command-keys "Move to a commit and do \\[im-git-select-commit-finalize] to select it.")))

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

;;;; im-git-commit-fixup

;;;###autoload
(defun im-git-commit-fixup ()
  "Interactively select a commit and fixup."
  (interactive)
  (im-git-select-commit
   (lambda (hash)
     (set-process-sentinel
      (funcall #'start-process "*im-git-commit*" (im-get-reset-buffer "*im-git-commit*") "git" "commit" "--fixup" hash)
      (lambda (proc _event)
        (if (eq (process-exit-status proc) 0)
            (progn
              (message "im-git-commit :: Committed")
              (--each im-git-commit-finished-hook (funcall it nil))
              (let ((process-environment `("GIT_SEQUENCE_EDITOR=true" ,@process-environment)))
                (set-process-sentinel
                 (start-process "*im-git-fixup*" (im-get-reset-buffer " *im-git-fixup*")
                                "git" "rebase" "--interactive" "--autosquash" (concat hash "^"))
                 (lambda (proc _event)
                   (if (eq (process-exit-status proc) 0)
                       (message ">> im-git-fixup :: Commit %s fixed." hash)
                     (message "!! Failed to fixup. See *im-git-fixup* buffer for further details."))))) )
          (message "im-git-commit :: Failed. See buffer *im-git-commit*")))))))

;;;; im-git-stash

(defconst im-git--stash-list-buffer-name "*im-git-stash-list*")

;;;###autoload
(defun im-git-list-stash ()
  (interactive)
  (when-let* ((buffer (get-buffer im-git--stash-list-buffer-name)))
    (kill-buffer buffer))
  (let ((default-directory (im-current-project-root))
        (buffer-name im-git--stash-list-buffer-name))
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
  "Show stashed diff for STASH-ENTRY."
  (interactive (list (im-git--parse-stash-entry-at-point)))
  (let ((default-directory (im-current-project-root))
        (buffer-name (format "*im-git-stash-diff: %s {%s}*"
                             (im-current-project-name)
                             stash-entry)))
    (when-let* ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))
    (im-shell-command
     :command "git"
     :args (-non-nil `("--no-pager" "stash" "show" "-p" ,stash-entry))
     :switch nil
     :buffer-name buffer-name
     :on-finish
     (lambda (_output &rest _)
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
    (let ((inhibit-read-only t))
      (add-text-properties begin end '(face (:strike-through t))))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "stash" "drop" ,stash-entry)
     :switch nil
     :buffer-name " *im-git-stash-drop*"
     :on-finish
     (lambda (output &rest _)
       (with-current-buffer im-git--stash-list-buffer-name
         (im-git-list-stash)))
     :on-fail
     (lambda (output &rest _)
       (message ">> `git stash drop' failed with: " output)
       (switch-to-buffer buffer-name)))))

(defun im-git-pop-stash ()
  (interactive nil im-git-stash-list-mode)
  (when-let* ((stash-entry (im-git--parse-stash-entry-at-point))
              (begin (line-beginning-position))
              (end (line-end-position)))
    (let ((inhibit-read-only t))
      (add-text-properties begin end '(face (:strike-through t))))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "stash" "pop" ,stash-entry)
     :switch nil
     :buffer-name " *im-git-stash-pop*"
     :on-finish
     (lambda (output &rest _)
       (with-current-buffer im-git--stash-list-buffer-name
         (im-git-list-stash)
         (message ">> Successfully popped!")))
     :on-fail
     (lambda (output &rest _)
       (message ">> `git stash pop' failed with: " output)
       (switch-to-buffer buffer-name)))))

(define-minor-mode im-git-stash-list-mode
  "A minor mode for interacting with git stash list."
  :lighter " StashList"
  :keymap (make-sparse-keymap))

(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'im-git-stash-list-mode
    (kbd "gr") #'im-git-list-stash
    (kbd "x") #'im-git-drop-stash
    (kbd "p") #'im-git-pop-stash
    (kbd "RET") #'im-git-show-stash-diff))

(defun im-git--parse-stash-entry-at-point ()
  "Return the stash entry name at point.
Works only if the stash entry is at the beginning of the line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "stash@{[0-9]+}")
      (match-string 0))))

;;;; im-git-tags

(defconst im-git--tag-list-buffer-name "*im-git-tag-list*")

;;;###autoload
(defun im-git-list-tags ()
  (interactive)
  (when-let* ((buffer (get-buffer im-git--tag-list-buffer-name)))
    (kill-buffer buffer))
  (let ((default-directory (im-current-project-root))
        (buffer-name im-git--tag-list-buffer-name))
    (im-shell-command
     :command "git"
     :args `("for-each-ref" "--sort=-creatordate" "--format" "%(refname:short) • %(objectname:short) • %(creatordate:short) • %(subject)" "refs/tags")
     :switch t
     :buffer-name buffer-name
     :on-finish
     (lambda (_output &rest _)
       (im-shell-command
        :command "git"
        :args `("ls-remote" "--tags" "origin")
        :on-finish
        (lambda (output &rest _)
          (with-current-buffer buffer-name
            (let ((inhibit-read-only t))
              (let ((lines (s-lines (s-trim (buffer-string))))
                    (remote-tags (->> (s-lines (s-trim output))
                                    (--map (-let (((hash tag) (s-split "\t" it)))
                                             (list (s-chop-prefix "refs/tags/" tag)
                                                   (substring hash 0 7))))
                                    (delete-dups)))
                    (local-tags '()))
                (goto-char (point-min))
                (while (< (point) (point-max))
                  (let* ((tag (im-git--parse-tag-at-point))
                         (remote? (-contains? remote-tags tag)))
                    (push tag local-tags)
                    (add-text-properties
                     (line-beginning-position)
                     (line-end-position)
                     `(face ,(if remote? 'success 'warning)
                            tag-info (:name ,(car tag)
                                      :hash ,(cdr tag)
                                      :remote? ,remote?
                                      :local? t))))
                  (forward-line 1))
                (when-let* ((remote-only (cl-set-difference remote-tags local-tags :test #'equal)))
                  (insert "========== (remote only) ==========\n")
                  (--each remote-only
                    (let ((start (point)))
                      (insert (format "%s • %s\n" (car it) (cadr it)))
                      (add-text-properties
                       start (1- (point))
                       `(tag-info (:name ,(car it)
                                   :hash ,(cdr it)
                                   :remote? t
                                   :local? nil))))))
                (goto-char (point-min)))))))
       (align-regexp (point-min) (point-max) "\\(\\s-*\\)•" nil 1 t)
       (read-only-mode)
       (text-mode)
       (im-git-tag-list-mode))
     :on-fail
     (lambda (&rest _)
       (message ">> Listing tags failed!")
       (switch-to-buffer buffer-name)))))

(defun im-git-remove-tag ()
  (interactive nil im-git-stash-list-mode)
  (when-let* ((tag-info (im-git--tag-info-at-point))
              (begin (line-beginning-position))
              (end (line-end-position))
              (_ (y-or-n-p (format ">> Remove tag %s? " (plist-get tag-info :name)))))
    (let ((inhibit-read-only t))
      (add-text-properties begin end '(face (:strike-through t))))
    (cl-flet ((remove-remote-tag
               (tag-info)
               (im-shell-command
                :command "git"
                :args `("push" "origin" "--delete" ,(plist-get tag-info :name))
                :switch nil
                :buffer-name " *im-git-tag-remove-remote*"
                :on-finish
                (lambda (_output &rest _)
                  (message ">> Also removed from remote.")
                  (with-current-buffer im-git--tag-list-buffer-name
                    (im-git-list-tags)))
                :on-fail
                (lambda (output &rest _)
                  (message ">> Failed to remove from remote: %s" output)
                  (switch-to-buffer " *im-git-tag-remove-remote*")))))
      (if (and (plist-get tag-info :remote?)
               (not (plist-get tag-info :local?)))
          (remove-remote-tag tag-info)
        (im-shell-command
         :command "git"
         :args `("tag" "-d" ,(plist-get tag-info :name))
         :switch nil
         :buffer-name " *im-git-tag-remove*"
         :on-finish
         (lambda (_output &rest _)
           (if (and
                (plist-get tag-info :remote?)
                (y-or-n-p ">> Also remove from remote? "))
               (remove-remote-tag tag-info)
             (with-current-buffer im-git--tag-list-buffer-name
               (im-git-list-tags))))
         :on-fail
         (lambda (output &rest _)
           (message ">> `git tag -d' failed with: " output)
           (switch-to-buffer " *im-git-tag-remove*")))))))

(defun im-git-open-file-at-tag ()
  (interactive nil im-git-tag-list-mode)
  (when-let* ((default-directory (im-current-project-root))
              (tag-info (im-git--tag-info-at-point))
              (tag (plist-get tag-info :name))
              (file (im-output-select
                     :cmd (concat "git ls-tree --name-only -r " tag)
                     :prompt (format "File at %s: " tag)
                     :require-match? t))
              (buffer-name (format "*im-git-file-at: %s:%s*" tag file)))
    (when-let* ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "show" ,(format "%s:%s" tag file))
     :switch nil
     :buffer-name buffer-name
     :on-finish
     (lambda (_output &rest _)
       (with-current-buffer buffer-name
         (funcall (assoc-default file auto-mode-alist 'string-match #'prog-mode))
         (setq header-line-format (format "File %s at tag %s" file tag))
         (switch-to-buffer (current-buffer))))
     :on-fail
     (lambda (&rest _)
       (message ">> `git show %s:%s' failed!" tag file)
       (switch-to-buffer buffer-name)))))

(define-minor-mode im-git-tag-list-mode
  "A minor mode for interacting with git tag list."
  :lighter " TagList"
  :keymap (make-sparse-keymap)
  (setq header-line-format
        (substitute-command-keys "Tags :: \\[im-git-list-tags] ⟶ Refresh, \\[im-git-open-file-at-tag] ⟶ Open file at tag, \\[im-git-remove-tag] ⟶ Remove tag")))

(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'im-git-tag-list-mode
    (kbd "gr") #'im-git-list-tags
    (kbd "o") #'im-git-open-file-at-tag
    (kbd "x") #'im-git-remove-tag))

(defun im-git--parse-tag-at-point ()
  "Return (TAG HASH) at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^ ]+\\) *• *\\([^ ]+\\) ")
      (list (match-string 1) (match-string 2)))))

(defun im-git--tag-info-at-point ()
  (get-text-property (point) 'tag-info))

;;;; im-git-search-history

(defun im-git-search-diff-history (regexp &optional all-branches?)
  "Search through git diffs for REGEXP.
If ALL-BRANCHES? is non-nil, also search for all other branches for
REGEXP."
  (interactive
   (list (read-string (if current-prefix-arg
                          "Search for regexp (in all branches): "
                        "Search for regexp: "))
         current-prefix-arg))
  (let ((default-directory (or (im-current-project-root) default-directory))
        (buffer-name "*git-history-search*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (im-shell-command
     :command "git"
     :args `("--no-pager" "log" "-G" ,regexp ,@(when all-branches? '("--branches" "--all")) "-p")
     :switch t
     :eat t
     :buffer-name buffer-name)))

;;;; im-git-list-dirty

(async-defun im-git-list-dirty-projects ()
  "List all dirty projects or projects that are not on main branch.
I keep projects clean in my local and do developments inside worktrees
but sometimes projects gets dirty and this fixes that."
  (interactive)
  (let ((buff (im-get-reset-buffer "*im-dirty-projects*")))
    (message "im-git-list-dirty-projects :: started.")
    (switch-to-buffer buff)
    (dolist (project (im-all-project-roots))
      (let* ((default-directory (expand-file-name project))
             (branches (split-string (await (lab--git "branch" "--list")) "\n" t "[ \\*\t]+"))
             (current-branch (await (lab--git "branch" "--show-current")))
             (main-branch (seq-find
                           (lambda (branch)
                             (string-match (regexp-opt (lab--listify lab-main-branch-name) t) (or branch "NULL")))
                           branches))
             (main? (string= current-branch main-branch))
             (dirty? (im-git-dirty?)))
        (with-current-buffer buff
          (save-excursion
            (goto-char (point-max))
            (when dirty?
              (insert (format "- %s is dirty!\n" project))
              (insert "  ")
              (insert-button
               "Stash"
               'follow-link t
               'face custom-button
               'action (lambda (_button)
                         (let ((default-directory (expand-file-name project)))
                           (call-interactively #'vc-git-stash))))
              (insert "\n"))
            (when (and main-branch (not main?))
              (insert (format "- %s not on master!\n" project))
              (insert "  ")
              (insert-button
               "Switch to MAIN"
               'follow-link t
               'face custom-button
               'action (lambda (_button)
                         (let ((default-directory (expand-file-name project)))
                           (if (eq 0 (call-process "git" nil nil nil "checkout" main-branch))
                               (message ">> Switched! isDirty=%s" (im-git-dirty?))
                             (message "!! Failed to switch!")))))
              (insert "\n"))
            (when (or dirty? (not main?))
              (redisplay t)))))))
  (message "im-git-list-dirty-projects :: finished."))

;;;; git worktrees

;; - `im-git-worktree-add': Creates a new git worktree from an existing or
;; new branch under a common directory, then opens it in dired.
;; - `im-git-worktree-switch': Prompts to select an existing git worktree
;; and then opens it in dired.
;; - `im-git-worktree-delete-current': Deletes the current git worktree
;; (with option to force if dirty) and kills related buffers.
;;
;; Typical workflow:
;; 1. Use im-git-worktree-switch to navigate and open an existing worktree.
;; 2. Use im-git-worktree-add to create a new worktree from a branch and start working there.
;; 3. Use im-git-worktree-delete-current to clean up and remove a worktree when done.
;;
;; These commands takes care of edge cases, like deleting a dirty
;; worktree, pruning before switching, copying necessary untracked
;; files while creating worktrees etc.

(defvar im-git-worktrees-root "~/Workspace/worktrees"
  "Directory to create worktrees in.")

(defun im-git-worktree-switch ()
  (interactive)
  (cl-flet ((get-worktree
             (it)
             (nth 1 (s-split-up-to " " (nth 0 (s-split "\n" it t)) 1 t))))
    (im-output-select
     :cmd "git worktree list --porcelain"
     :prompt "Select a worktree: "
     :formatter
     (format "%s"
             (s-replace-all
              `((,(expand-file-name im-projects-root) . ,(propertize "$PROJECTS" 'face '(:weight bold)))
                (,(expand-file-name im-git-worktrees-root) . ,(propertize "$WORKTREES" 'face '(:weight bold))))
              (expand-file-name (get-worktree it))))
     :split "\n\n"
     :require-match? nil
     :do (dired (get-worktree it)))))

(defun im-git-worktree-add (new-branch?)
  "Create a new worktree from a branch (or with a new branch).
This creates every worktree under a common directory (see
`im-git-worktrees-root') with a common
pattern (project-name--branch-name).

NEW-BRANCH? it create a new branch or not.

Also copies some predefined list of untracked files/folders with
COW (copy on write).

If worktree already exists, simply switches to it."
  (unless (= 0 (shell-command "git fetch --all"
                              " *im-git-worktree: git fetch stdout*"
                              " *im-git-worktree: git fetch stderr*"))
    (user-error "Cannot git fetch --all"))
  (unless (= 0 (shell-command "git worktree prune"
                              " *im-git-worktree: git worktree prune stdout*"
                              " *im-git-worktree: git worktree prune stderr*"))
    (user-error "Cannot git worktree prune"))
  (let* ((old-proj (im-current-project-root))
         (source (im-output-select
                  :cmd "git branch -a --format='%(refname:short)'"
                  :prompt "From (or enter new): "
                  :require-match? nil))
         (new-branch-name (when new-branch?
                            (read-string "New branch name: ")))
         (worktree-name
          (format "%s--%s"
                  (im-string-url-case (im-current-project-name))
                  (im-string-url-case (if new-branch? new-branch-name source))))
         (worktree (expand-file-name (f-join im-git-worktrees-root worktree-name)))
         (exists? (f-exists? worktree)))
    (unless exists?
      (unless (= 0 (shell-command (format "git worktree add %s '%s' '%s'"
                                          (if new-branch?
                                              (format "-b '%s'" new-branch-name)
                                            "")
                                          worktree
                                          source)
                                  " *im-git-worktree: git worktree add stdout*"
                                  " *im-git-worktree: git worktree add stderr*"))
        (user-error "Can't create worktree, see buffer `*im-git-worktrees: git worktree add stderr*'"))
      (message ">> Created the worktree...")

      ;; Copy node_modules with COW, if user wants it
      ;; If I need anything else, I'll simply edit here.

      (let ((src (f-join old-proj "node_modules"))
            (dst (f-join worktree "node_modules")))
        (when (and (file-directory-p src)
                   (y-or-n-p "Copy node_modules from old-proj to current folder with COW? "))
          (unless (= 0 (shell-command
                        (concat (im-when-on
                                 :linux "cp -R --reflink=always"
                                 :darwin "cp -Rc")
                                (expand-file-name src)
                                (expand-file-name dst))
                        " *im-git-worktree: cp stdout*"
                        " *im-git-worktree: cp stderr*"))
            (user-error "Can't copy files, see buffer `*im-git-worktrees: cp stderr*'")))))
    (dired worktree)
    (vc-refresh-state)))

(defun im-git-worktree-add-new ()
  "Create a new branch from an existing one and switch to in a worktree."
  (interactive)
  (im-git-worktree-add :new-branch))

(defun im-git-worktree-add-existing ()
  "Switch to an existing branch in a new worktree."
  (interactive)
  (im-git-worktree-add nil))

(defun im-git-worktree-delete ()
  "Remove current worktree and close all buffers.
If worktree is dirty, asks user if they want to force delete it."
  (interactive)
  (when-let* ((proj (im-current-project-root))
              (y/n (y-or-n-p (format "Want to delete: %s?" proj))))
    (let ((default-directory proj)
          (args (if (im-git-dirty?)
                    (if (y-or-n-p "Worktree is dirty, force remove?")
                        "--force"
                      (user-error "Aborted by user"))
                  "")))
      (project-kill-buffers)
      (if (= 0 (shell-command (format "git worktree remove %s '%s'" args proj)
                              " *im-git-worktree: git worktree remove stdout*"
                              " *im-git-worktree: git worktree remove stderr*"))
          (message ">> Removed")
        (user-error "Can't remove worktree, see  *im-git-worktree: git worktree remove stderr*")))))

;;;; git utils

(defun im-git-dirty? ()
  "Return t if there are uncommitted changes in the working tree."
  (eq 1 (call-process "git" nil nil nil "diff-index" "--quiet" "HEAD" "--")))

(provide 'im-git)
;;; im-git.el ends here
