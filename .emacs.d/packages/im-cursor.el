;;; im-cursor.el --- cursor-agent helpers  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (eat "0.9"))
;; Keywords: tools, terminals

;;; Commentary:

;; Run the `cursor-agent` CLI inside an `eat` terminal buffer, with a small UX layer:
;;
;; - `im-cursor-agent': open a per-project *cursor-agent* buffer, enable helper modes.
;; - `im-cursor-gen-watch-mode': poll the terminal title to detect agent status and
;;   notify when generation finishes or user confirmation is needed (suppressed when
;;   Emacs + the buffer are currently visible).
;; - `im-cursor-agent-insert-prompt': edit a Markdown prompt in a temp buffer (with `gptel')
;;   and send it to the terminal, remembering the last prompt.
;;
;; Main entry point:
;; - `im-cursor-agent': start (or switch to) a per-project
;;   `*cursor-agent: …*` buffer, enable convenience minor modes, and
;;   (when available) drop into `evil-insert-state'.
;;
;; Convenience features:
;; - `im-cursor-agent-mode': keybindings for the agent buffer (notably
;;   `C-i' to compose a prompt in a temporary Markdown buffer and send
;;   it to the running agent).
;; - `im-cursor-gen-watch-mode': periodically checks the terminal
;;   title to detect agent status (working/waiting/ready) and sends a
;;   one-shot notification when the agent finishes or needs user
;;   interaction.  To avoid noisy notifications, it suppresses them
;;   when both Emacs and the agent buffer are currently
;;   visible.  /status-indicators needs to be enabled on cursor CLI.

;; DISCLAIMER: Of course, mostly AI generated.  I know about
;; agent-shell but it's not fully working with cursor-cli (maybe it's
;; the ACP bridge's problem but I digress), hence I generated this.

;;; Code:

(require 'im)
(require 'im-notif)
(require 'cl-lib)
(require 'eat)

;;;; Customization

(defgroup im-cursor nil
  "Cursor-agent helpers."
  :group 'tools)

(defcustom im-cursor-gen-watch-interval 3
  "Seconds between generating-state checks."
  :type 'number
  :group 'im-cursor)

(defcustom im-cursor-gen-watch-finished-notify-function
  (lambda (user-interaction?)
    (im-notif
     :title (format "*cursor-agent* finished: %s" (buffer-name))
     :message  (if user-interaction?
                   "USER INTERACTION REQUIRED!"
                 "FINISHED!")
     :source (current-buffer)
     :labels '("agent" "cursor")))
  "Function called when agent transitions from working to not working.

Called with one argument: non-nil if user interaction is required."
  :type 'function
  :group 'im-cursor)

;;;; Internal

(defconst im-cursor-agent-empty-prompt-regexp "→ \\(?:Plan, search\\|Add a follow-up\\)")
(defvar-local im-cursor-agent--last-prompt nil)

(defvar-local im-cursor-gen-watch--kill-hook-installed nil)
(defvar-local im-cursor-gen-watch--timer nil)
(defvar-local im-cursor-gen-watch--was-generating nil)

(defun im-cursor-gen-watch--parse-title-status ()
  "Parse the terminal title and return the agent status as a symbol.
Returns `working', `waiting', `ready', or nil if unparseable."
  (when (and (boundp 'eat-terminal) eat-terminal)
    (let ((title (eat-term-title eat-terminal)))
      (when (and title (string-match " - \\(.+\\)" title))
        (let ((status-part (match-string 1 title)))
          (cond
           ((string-match "⏳" status-part) 'working)
           ((string-match "🔐" status-part) 'waiting)
           ((string-match "✅" status-part) 'ready)
           (t nil)))))))

;;;; im-cursor-gen-watch-mode

(defun im-cursor-gen-watch--buffer-visible-now-p (buf)
  "Non-nil if BUF is visible in any live frame right now."
  (and (buffer-live-p buf)
       (get-buffer-window buf t)))

(defun im-cursor-gen-watch--emacs-visible-now-p ()
  "Non-nil if at least one live frame is visible."
  (seq-some (lambda (fr) (frame-visible-p fr)) (frame-list)))

(defun im-cursor-gen-watch--tick (buf)
  "Update generation state for BUF and notify when generation finishes."
  (when (buffer-live-p buf)
    (condition-case nil
        (with-current-buffer buf
          (let* ((status (im-cursor-gen-watch--parse-title-status))
                 (now (eq status 'working))
                 (user-interaction? (eq status 'waiting)))
            (when (and im-cursor-gen-watch--was-generating (not now)
                       (not (and (im-cursor-gen-watch--emacs-visible-now-p)
                                 (im-cursor-gen-watch--buffer-visible-now-p buf))))
              (funcall im-cursor-gen-watch-finished-notify-function user-interaction?))
            (setq im-cursor-gen-watch--was-generating now)))
      (error nil))))

(defun im-cursor-gen-watch--cleanup ()
  (when (timerp im-cursor-gen-watch--timer)
    (cancel-timer im-cursor-gen-watch--timer))
  (setq im-cursor-gen-watch--timer nil
        im-cursor-gen-watch--was-generating nil))

(define-minor-mode im-cursor-gen-watch-mode
  "Watch current buffer and notify when generation finishes (once per run)."
  :lighter (:eval (if im-cursor-gen-watch--was-generating " GenWatch…" " GenWatch"))
  (if im-cursor-gen-watch-mode
      (progn
        (let ((status (im-cursor-gen-watch--parse-title-status)))
          (setq im-cursor-gen-watch--was-generating (eq status 'working)))
        (setq im-cursor-gen-watch--timer
              (run-with-timer 0 im-cursor-gen-watch-interval
                              #'im-cursor-gen-watch--tick
                              (current-buffer)))
        (unless im-cursor-gen-watch--kill-hook-installed
          (add-hook 'kill-buffer-hook #'im-cursor-gen-watch--cleanup nil t)
          (setq im-cursor-gen-watch--kill-hook-installed t)))
    (im-cursor-gen-watch--cleanup)))

;;;; Cursor agent buffer + keybindings

(defvar im-cursor-agent-mode-map (make-sparse-keymap)
  "Keymap for `im-cursor-agent-mode'.")

(define-minor-mode im-cursor-agent-mode
  "A minor mode for interacting with cursor-agent."
  :lighter " Cursor"
  :keymap im-cursor-agent-mode-map)

(with-eval-after-load 'general
  (with-eval-after-load 'evil
    (general-def :keymaps 'im-cursor-agent-mode-map :states '(insert normal)
      "C-i" #'im-cursor-agent-insert-prompt
      "RET" (lambda () (interactive) (eat-self-input 1 (aref (kbd "RET") 0)))
      ;; "C-<escape>" (lambda () (interactive)
      ;;                ;; send a literal ESC to the terminal (eat)
      ;;                (eat-self-input 1 (aref (kbd "ESC") 0)))
      )))

;;;###autoload
(defun im-cursor-agent ()
  "Open cursor-agent for current project."
  (interactive)
  (require 'eat)
  (let* ((buff (format "*cursor-agent: %s*" (im-current-project-name)))
         (eat-buffer-name buff))
    (eat "cursor-agent")
    (with-current-buffer buff
      (im-cursor-agent-mode 1)
      (im-cursor-gen-watch-mode 1)
      (when (featurep 'evil)
        (evil-insert-state)
        (evil-normalize-keymaps)
        (setq-local evil-insert-state-cursor '(nil))))))

(defun im-cursor-agent-insert-prompt ()
  "Ask for a prompt in a temp buffer; on C-c C-c insert it into current buffer."
  (interactive)
  (let ((target-buf (current-buffer)))
    (im-get-input
     :mode #'markdown-mode
     :restore t
     :init (if (save-excursion
                 (re-search-backward im-cursor-agent-empty-prompt-regexp nil t))
               ""
             (or im-cursor-agent--last-prompt ""))
     :switcher #'switch-to-buffer-other-window
     :on-init
     (lambda ()
       (gptel-mode))
     :on-accept
     (lambda (prompt &rest _)
       (when (buffer-live-p target-buf)
         (with-current-buffer target-buf
           (while (not (save-excursion
                         (goto-char (point-max))
                         (search-backward-regexp im-cursor-agent-empty-prompt-regexp nil t)))
             (eat-self-input 1 (aref (kbd "M-<backspace>") 0)))
           (setq im-cursor-agent--last-prompt prompt)
           (eat-term-send-string eat-terminal prompt))))
     :on-reject #'ignore)))

(provide 'im-cursor)
;;; im-cursor.el ends here
