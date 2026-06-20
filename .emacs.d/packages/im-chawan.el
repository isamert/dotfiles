;;; im-chawan.el --- Run Chawan in Ghostel -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1"))
;; Keywords: terminals, browser

;;; Commentary:

;; Start Chawan with `cha -V' in a Ghostel terminal.  Handles evil
;; integration.
;;
;; Mostly LLM generated.

;;; Code:

(require 'evil)
(require 'ghostel)
(require 'subr-x)
(require 'thingatpt)
(require 'url-parse)
(require 'cl-lib)

(defgroup im-chawan nil
  "Run Chawan in Ghostel."
  :group 'applications)

(defcustom im-chawan-program "cha"
  "Program used to start Chawan."
  :type 'string)

(defcustom im-chawan-arguments '("-V")
  "Arguments passed to `im-chawan-program'."
  :type '(repeat string))

(defcustom im-chawan-buffer-name "*chawan*"
  "Default Chawan buffer name."
  :type 'string)

(defcustom im-chawan-reuse-buffers t
  "Whether `im-chawan' should reuse existing Chawan buffers.

When non-nil, `chawan' reuses a matching live buffer when possible.
With prefix argument, `im-chawan' always creates a fresh buffer."
  :type 'boolean)

(defcustom im-chawan-browse-url-new-session t
  "Whether `im-chawan-browse-url' should open URLs in a fresh Chawan session."
  :type 'boolean)

(defcustom im-chawan-text-prompt-regexp "^[[:space:]]*\\(TEXT:\\|URL:\\|/\\)"
  "Regexp used to detect when Chawan is accepting text input."
  :type 'regexp)

(defcustom im-chawan-text-prompt-check-lines 3
  "How many lines from the end of the Chawan buffer to check for text input."
  :type 'integer)

(defvar-local im-chawan--url nil
  "URL associated with the current Chawan buffer.")

(defvar-local im-chawan--last-arguments nil
  "Arguments used to start the current Chawan process.")

(define-derived-mode im-chawan-mode ghostel-mode "Chawan"
  "Major mode for Chawan running inside Ghostel."
  (setq-local ghostel-buffer-name-function #'im-chawan--buffer-name-by-title)
  (setq-local mode-line-format nil)
  (tab-line-mode +1))

(defun im-chawan--normalize-url (url)
  "Normalize URL.

Return nil for empty strings."
  (when url
    (setq url (string-trim url))
    (unless (string-empty-p url)
      url)))

(defun im-chawan--url-host (url)
  "Return a display name for URL."
  (condition-case nil
      (let* ((parsed (url-generic-parse-url url))
             (host (url-host parsed)))
        (or host
            (car (split-string url "[/?#]" t))
            url))
    (error
     (car (split-string url "[/?#]" t)))))

(defun im-chawan--buffer-name-by-title (title)
  "Return \"*ghostel: TITLE*\", or nil when TITLE is nil or empty.
A `ghostel-buffer-name-function' that names the buffer after the title."
  (and title (not (string= "" title))
       (format "*chawan: %s*" title)))

(defun im-chawan--buffer-name-for-url (url)
  "Return a Chawan buffer name for URL."
  (if-let* ((url (im-chawan--normalize-url url)))
      (format "*chawan: %s*" (im-chawan--url-host url))
    im-chawan-buffer-name))

(defun im-chawan--program-arguments (url)
  "Return Chawan command arguments for URL."
  (append im-chawan-arguments
          (when-let* ((url (im-chawan--normalize-url url)))
            (list url))))

(defun im-chawan--running-p (buffer)
  "Return non-nil if BUFFER has a live Ghostel process."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'ghostel--process)
              (process-live-p ghostel--process)))))

(defun im-chawan--buffers ()
  "Return all Chawan buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'im-chawan-mode)))
   (buffer-list)))

(defun im-chawan--find-buffer-for-url (url)
  "Find an existing Chawan buffer for URL."
  (setq url (im-chawan--normalize-url url))
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'im-chawan-mode)
            (equal im-chawan--url url))))
   (buffer-list)))

(defun im-chawan--get-buffer (url fresh)
  "Return a Chawan buffer for URL.

If FRESH is non-nil, always create a new buffer."
  (let* ((url (im-chawan--normalize-url url))
         (name (im-chawan--buffer-name-for-url url)))
    (cond
     (fresh
      (generate-new-buffer name))
     ((not im-chawan-reuse-buffers)
      (generate-new-buffer name))
     (url
      (or (im-chawan--find-buffer-for-url url)
          ;; Avoid accidentally reusing `*chawan: host*' for a different URL.
          (if (get-buffer name)
              (generate-new-buffer name)
            (get-buffer-create name))))
     (t
      (get-buffer-create name)))))

(defun im-chawan--stop-process (&optional buffer)
  "Stop Chawan/Ghostel process in BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'ghostel--process)
                 (process-live-p ghostel--process))
        (delete-process ghostel--process))
      ;; Ghostel internals, but useful to allow immediate restart.
      (when (boundp 'ghostel--process)
        (setq ghostel--process nil))
      (when (boundp 'ghostel--pid)
        (setq ghostel--pid nil)))))

(defun im-chawan--evil-active-p ()
  "Return non-nil if Evil is available and enabled."
  (and (fboundp 'evil-emacs-state)
       (or (bound-and-true-p evil-local-mode)
           (bound-and-true-p evil-mode))))

(defun im-chawan-evil-emacs-state ()
  "Enter Evil Emacs state instead of Evil Insert state.

This suppresses `evil-ghostel--insert-state-entry', which may fail
before Ghostel has reported an initial cursor position."
  (interactive)
  (when (fboundp 'evil-emacs-state)
    (let ((evil-emacs-state-entry-hook
           (remove #'evil-ghostel--insert-state-entry
                   evil-emacs-state-entry-hook)))
      (evil-emacs-state 1))))

(defun im-chawan--maybe-evil-emacs-state ()
  "Use Evil Emacs state in `im-chawan-mode' buffers."
  (when (and (derived-mode-p 'im-chawan-mode)
             (im-chawan--evil-active-p))
    (im-chawan-evil-emacs-state)))

(defun im-chawan--defer-evil-emacs-state (buffer)
  "Switch BUFFER to Evil Emacs state soon."
  (run-at-time
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (im-chawan--maybe-evil-emacs-state))))
   buffer))

(defun im-chawan--start-in-buffer (buffer url)
  "Start Chawan for URL in BUFFER."
  (unless (executable-find im-chawan-program)
    (user-error "Cannot find Chawan executable: %s" im-chawan-program))
  (let ((args (im-chawan--program-arguments url)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'im-chawan-mode)
        (im-chawan-mode))
      (setq im-chawan--url (im-chawan--normalize-url url))
      (setq im-chawan--last-arguments args))
    (ghostel-exec buffer im-chawan-program args)
    (im-chawan--defer-evil-emacs-state buffer)
    buffer))

;;;###autoload
(defun im-chawan (&optional url fresh)
  "Start Chawan visual mode, `cha -V'.

Interactively, prompt for URL.  Empty input opens a default Chawan
session.

With prefix argument FRESH, create a fresh buffer.

From Lisp:

  (chawan)
  (chawan \"https://example.com\")
  (chawan \"https://example.com\" t)"
  (interactive
   (list
    (im-chawan--normalize-url
     (read-string "URL, search, or path for Chawan: "))
    current-prefix-arg))
  (let ((buffer (im-chawan--get-buffer url fresh)))
    (pop-to-buffer buffer)
    (unless (im-chawan--running-p buffer)
      (im-chawan--start-in-buffer buffer url))
    (im-chawan--defer-evil-emacs-state buffer)
    buffer))

;;;###autoload
(defun im-chawan-url (url &optional fresh)
  "Prompt for URL and open it in Chawan.

With prefix argument FRESH, create a fresh buffer."
  (interactive
   (list
    (read-string "Open URL in Chawan: " (thing-at-point-url-at-point))
    current-prefix-arg))
  (im-chawan url fresh))

;;;###autoload
(defun im-chawan-new (&optional url)
  "Always create a fresh Chawan session.

With URL, open URL.  Interactively, prompt for URL."
  (interactive
   (list
    (im-chawan--normalize-url
     (read-string "URL, search, or path for new Chawan: "
                  (thing-at-point-url-at-point)))))
  (im-chawan url t))

;;;###autoload
(defun im-chawan-at-point (&optional fresh)
  "Open URL at point in Chawan.

With prefix argument FRESH, create a fresh buffer."
  (interactive "P")
  (let ((url (thing-at-point-url-at-point)))
    (unless url
      (user-error "No URL at point"))
    (im-chawan url fresh)))

;;;###autoload
(defun im-chawan-browse-url (url &optional _new-window)
  "Open URL in Chawan.

This function is suitable for `browse-url-browser-function'."
  (interactive "sBrowse URL in Chawan: ")
  (im-chawan url im-chawan-browse-url-new-session))

;;;###autoload
(defun im-chawan-switch ()
  "Switch to an existing Chawan buffer."
  (interactive)
  (let* ((buffers (im-chawan--buffers))
         (names (mapcar #'buffer-name buffers)))
    (unless buffers
      (user-error "No Chawan buffers"))
    (pop-to-buffer
     (get-buffer
      (completing-read "Switch to Chawan buffer: " names nil t)))))

;;;###autoload
(defun im-chawan-kill (&optional buffer)
  "Kill Chawan BUFFER.

Interactively, kill the current Chawan buffer."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (unless (buffer-live-p buffer)
    (user-error "No such buffer"))
  (with-current-buffer buffer
    (unless (derived-mode-p 'im-chawan-mode)
      (user-error "Current buffer is not a Chawan buffer")))
  (im-chawan--stop-process buffer)
  (kill-buffer buffer))

;;;###autoload
(defun im-chawan-restart (&optional url)
  "Restart the current Chawan buffer.

With prefix argument, prompt for a new URL."
  (interactive
   (list
    (when current-prefix-arg
      (im-chawan--normalize-url
       (read-string "Restart Chawan with URL: "
                    (or im-chawan--url
                        (thing-at-point-url-at-point)))))))
  (unless (derived-mode-p 'im-chawan-mode)
    (user-error "Current buffer is not a Chawan buffer"))
  (let ((url (or (im-chawan--normalize-url url) im-chawan--url))
        (buffer (current-buffer)))
    (im-chawan--stop-process buffer)
    (im-chawan--start-in-buffer buffer url)
    (pop-to-buffer buffer)))

;;;###autoload
(defun im-chawan-open-url (url)
  "Open URL in the current Chawan buffer by restarting it.

If the current buffer is not a Chawan buffer, open URL using
`chawan'."
  (interactive
   (list
    (read-string "Open URL in Chawan: " (thing-at-point-url-at-point))))
  (if (derived-mode-p 'im-chawan-mode)
      (im-chawan-restart url)
    (im-chawan url)))

;; Emacs-side Chawan controls.
(define-key im-chawan-mode-map (kbd "C-c C-k") #'im-chawan-kill)
(define-key im-chawan-mode-map (kbd "C-c C-r") #'im-chawan-restart)
(define-key im-chawan-mode-map (kbd "C-c C-u") #'im-chawan-open-url)
(define-key im-chawan-mode-map (kbd "C-c C-n") #'im-chawan-new)
(define-key im-chawan-mode-map (kbd "C-c C-b") #'im-chawan-switch)

(defun im-chawan--evil-insert-state-advice (orig &rest args)
  "In `im-chawan-mode', redirect Evil Insert state to Emacs state."
  (if (derived-mode-p 'im-chawan-mode)
      (im-chawan-evil-emacs-state)
    (apply orig args)))

(defun im-chawan--evil-setup ()
  "Configure Evil integration for `im-chawan-mode'."
  (evil-set-initial-state 'im-chawan-mode 'emacs)

  ;; Safety net: no Evil Insert state in Chawan buffers.
  (unless (advice-member-p #'im-chawan--evil-insert-state-advice
                           #'evil-insert-state)
    (advice-add 'evil-insert-state
                :around #'im-chawan--evil-insert-state-advice))

  ;; Useful Evil/Emacs passthrough keys.
  ;; These are more useful for controlling Emacs than for controlling Chawan.
  (define-key im-chawan-mode-map (kbd "SPC") #'im-chawan-space)
  (define-key im-chawan-mode-map (kbd "C-w") evil-window-map)
  (define-key im-chawan-mode-map (kbd "C-^") #'evil-switch-to-windows-last-buffer)
  (define-key im-chawan-mode-map (kbd "C-6") #'evil-switch-to-windows-last-buffer)
  (define-key im-chawan-mode-map (kbd "C-o") #'evil-execute-in-normal-state))

(with-eval-after-load 'evil
  (im-chawan--evil-setup))

;;;; SPC handling

(defun im-chawan--text-prompt-visible-p ()
  "Return non-nil if Chawan appears to be accepting text input."
  (and im-chawan-text-prompt-regexp
       (save-excursion
         (save-restriction
           (widen)
           (let ((end (point-max))
                 (start (save-excursion
                          (goto-char (point-max))
                          (forward-line (- im-chawan-text-prompt-check-lines))
                          (point))))
             (goto-char start)
             (re-search-forward im-chawan-text-prompt-regexp end t))))))

(defun im-chawan-space ()
  "Send SPC to Chawan in text input, otherwise run normal Emacs SPC binding."
  (interactive)
  (if (im-chawan--text-prompt-visible-p)
      (ghostel-send-string " ")
    (setq unread-command-events
          (append (listify-key-sequence (kbd "C-o SPC"))
                  unread-command-events))))

(provide 'chawan)
;;; im-chawan.el ends here
