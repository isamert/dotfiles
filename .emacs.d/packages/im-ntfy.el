;;; im-ntfy.el --- Simple ntfy client for Emacs -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, notifications
;; URL: https://github.com/yourname/im-ntfy

;;; Commentary:

;; A simple ntfy (https://ntfy.sh) client for Emacs.
;;
;; Features:
;; - Subscribe to topics and receive messages via callbacks
;; - Interactive topic browser with message history
;; - Send messages to topics
;; - Basic authentication support
;;
;; Usage:
;;   ;; Configure your server and credentials
;;   (setq im-ntfy-server "https://ntfy.sh")
;;   (setq im-ntfy-username "myuser")  ; optional
;;   (setq im-ntfy-password "mypass")  ; optional
;;
;;   ;; Subscribe to a topic with a callback
;;   (im-ntfy-subscribe "mytopic"
;;     (lambda (msg _cb) (message "Got: %s" (alist-get 'message msg))))
;;
;;   ;; Open interactive topic browser
;;   (im-ntfy-topics)
;;
;; Mostly AI-generated.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;;; Customization

(defgroup im-ntfy nil
  "Simple ntfy client for Emacs."
  :group 'communication
  :prefix "im-ntfy-")

(defcustom im-ntfy-server nil
  "The ntfy server URL."
  :type 'string
  :group 'im-ntfy)

(defcustom im-ntfy-username nil
  "Username for basic authentication.
Set to nil for anonymous access."
  :type '(choice (const nil) string)
  :group 'im-ntfy)

(defcustom im-ntfy-password nil
  "Password for basic authentication.
Set to nil for anonymous access."
  :type '(choice (const nil) string)
  :group 'im-ntfy)

(defcustom im-ntfy-token nil
  "Access token for authentication.
If set, this takes precedence over username/password."
  :type '(choice (const nil) string)
  :group 'im-ntfy)

(defcustom im-ntfy-topics nil
  "List of topics to track.
Each element can be a string (topic name) or a plist with
:name, :server, :username, :password, :token keys."
  :type '(repeat (choice string plist))
  :group 'im-ntfy)

(defcustom im-ntfy-default-priority 3
  "Default message priority (1-5, where 3 is default)."
  :type 'integer
  :group 'im-ntfy)

;;; Constants

(defconst im-ntfy--prompt "> ")

;;; Internal variables

(defvar im-ntfy--subscriptions (make-hash-table :test 'equal)
  "Hash table mapping topic names to subscription data.
Each entry contains: (:process PROC :callbacks (FN1 FN2 ...) :buffer BUF)")

(defvar im-ntfy--message-cache (make-hash-table :test 'equal)
  "Hash table mapping topic names to list of cached messages.")

(defvar-local im-ntfy--current-topic nil
  "The topic associated with the current buffer.")

;;; Authentication helpers

(defun im-ntfy--auth-header (&optional username password token)
  "Generate authorization header value.
Uses TOKEN if provided, otherwise USERNAME and PASSWORD."
  (let ((tok (or token im-ntfy-token))
        (user (or username im-ntfy-username))
        (pass (or password im-ntfy-password)))
    (cond
     (tok (concat "Bearer " tok))
     ((and user pass)
      (concat "Basic " (base64-encode-string (concat user ":" pass) t)))
     (t nil))))

(defun im-ntfy--make-url (topic &optional server endpoint)
  "Construct URL for TOPIC on SERVER with optional ENDPOINT."
  (let ((srv (or server im-ntfy-server)))
    (concat (string-trim-right srv "/")
            "/" topic
            (when endpoint (concat "/" endpoint)))))

;;; Core subscription functionality

(defun im-ntfy--parse-json-line (line)
  "Parse a JSON LINE into an alist."
  (condition-case nil
      (json-parse-string line :object-type 'alist :array-type 'list :null-object nil :false-object nil)
    (error nil)))

(defun im-ntfy--handle-message (topic message)
  "Handle incoming MESSAGE for TOPIC.
Calls all registered callbacks and caches the message."
  (when (and message (not (equal (alist-get 'event message) "keepalive")))
    ;; Cache the message
    (let ((cache (gethash topic im-ntfy--message-cache)))
      (puthash topic (append cache (list message)) im-ntfy--message-cache))
    ;; Call callbacks for non-open events
    (when (equal (alist-get 'event message) "message")
      (let* ((sub (gethash topic im-ntfy--subscriptions))
             (callbacks (plist-get sub :callbacks)))
        (dolist (cb callbacks)
          (condition-case err
              (funcall cb message cb)
            (error (message "im-ntfy: callback error for %s: %s" topic err))))))))

(defun im-ntfy--process-filter (proc string)
  "Process filter for subscription PROC receiving STRING."
  (let* ((topic (process-get proc 'im-ntfy-topic))
         (buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        ;; Process complete lines
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let* ((line-end (point))
                 (line-start (line-beginning-position 0))
                 (line (buffer-substring-no-properties line-start (1- line-end))))
            (delete-region line-start line-end)
            (goto-char (point-min))
            (unless (string-empty-p (string-trim line))
              (when-let ((msg (im-ntfy--parse-json-line line)))
                (im-ntfy--handle-message topic msg)))))))))

(defun im-ntfy--process-sentinel (proc event)
  "Process sentinel for subscription PROC with EVENT."
  (let ((topic (process-get proc 'im-ntfy-topic)))
    (message "im-ntfy: subscription to '%s' ended: %s" topic (string-trim event))
    ;; Clean up
    (when-let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Remove from subscriptions if process died unexpectedly
    (when (memq (process-status proc) '(exit signal))
      (remhash topic im-ntfy--subscriptions))))

(defun im-ntfy-subscribe (topic callback &optional server username password token)
  "Subscribe to TOPIC and call CALLBACK for each message.
CALLBACK receives a single argument: the message as an alist.
Optional SERVER, USERNAME, PASSWORD, or TOKEN override defaults.
Returns the subscription process."
  (let* ((url (im-ntfy--make-url topic server "json"))
         (auth (im-ntfy--auth-header username password token))
         (buf-name (format " *im-ntfy-%s*" topic))
         (existing (gethash topic im-ntfy--subscriptions))
         proc)
    ;; If already subscribed, just add the callback
    (if (and existing (process-live-p (plist-get existing :process)))
        (progn
          (plist-put existing :callbacks
                     (cons callback (plist-get existing :callbacks)))
          (plist-get existing :process))
      ;; Create new subscription
      (let ((buf (generate-new-buffer buf-name))
            (args (list "curl" "-L" "-sN")))
        ;; Add auth header if needed
        (when auth
          (setq args (append args (list "-H" (concat "Authorization: " auth)))))
        (setq args (append args (list url)))
        ;; Start process
        (setq proc (apply #'start-process
                          (format "im-ntfy-%s" topic)
                          buf
                          args))
        (process-put proc 'im-ntfy-topic topic)
        (set-process-filter proc #'im-ntfy--process-filter)
        (set-process-sentinel proc #'im-ntfy--process-sentinel)
        (set-process-query-on-exit-flag proc nil)
        ;; Store subscription
        (puthash topic
                 (list :process proc :callbacks (list callback) :buffer buf)
                 im-ntfy--subscriptions)
        proc))))

(defun im-ntfy-unsubscribe (topic &optional callback)
  "Unsubscribe from TOPIC.
If CALLBACK is provided, only remove that callback.
If no callbacks remain, close the connection."
  (when-let ((sub (gethash topic im-ntfy--subscriptions)))
    (if callback
        ;; Remove specific callback
        (let ((callbacks (delete callback (plist-get sub :callbacks))))
          (if callbacks
              (plist-put sub :callbacks callbacks)
            ;; No callbacks left, kill the connection
            (when-let ((proc (plist-get sub :process)))
              (when (process-live-p proc)
                (delete-process proc)))
            (remhash topic im-ntfy--subscriptions)))
      ;; Remove all callbacks and close
      (when-let ((proc (plist-get sub :process)))
        (when (process-live-p proc)
          (delete-process proc)))
      (remhash topic im-ntfy--subscriptions))))

;;; Publishing messages

(cl-defun im-ntfy-publish (topic message &key title priority tags server username password token file on-success on-error)
  "Publish MESSAGE to TOPIC.
Optional TITLE, PRIORITY (1-5), TAGS (list of strings).
Optional FILE path to attach a file (MESSAGE becomes the notification text).
Optional SERVER, USERNAME, PASSWORD, or TOKEN override defaults.
Optional ON-SUCCESS and ON-ERROR callbacks for async notification."
  (let* ((url (im-ntfy--make-url topic server))
         (auth (im-ntfy--auth-header username password token))
         (args (append
                (when auth (list "-H" (format "Authorization: %s" auth)))
                (when title (list "-H" (format "Title: %s" title)))
                (when priority (list "-H" (format "Priority: %d" priority)))
                (when tags (list "-H" (format "Tags: %s" (mapconcat #'identity tags ","))))
                (when file (list "-H" (format "Filename: %s" (file-name-nondirectory file))))
                (when file (list "-T" file))
                (when file (list "-H" (format "Message: %s" message)))
                (unless file (list "-d" message))
                (list url)))
         (buf (generate-new-buffer " *im-ntfy-curl*")))
    (make-process
     :name "im-ntfy-publish"
     :buffer buf
     :command (cons "curl" args)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (zerop (process-exit-status proc))
             (when on-success (funcall on-success))
           (let ((error-msg (with-current-buffer buf (buffer-string))))
             (when on-error (funcall on-error error-msg))))
         (kill-buffer buf))))))

;;; Fetch cached messages

(defun im-ntfy-fetch-messages (topic &optional since server username password token)
  "Fetch cached messages from TOPIC.
SINCE can be a duration (e.g. \"10m\"), timestamp, message ID, or \"all\".
Returns a list of message alists."
  (let* ((url (concat (im-ntfy--make-url topic server "json")
                      "?poll=1"
                      (when since (concat "&since=" since))))
         (auth (im-ntfy--auth-header username password token))
         (args (list "curl" "-s")))
    (when auth
      (setq args (append args (list "-H" (concat "Authorization: " auth)))))
    (setq args (append args (list url)))
    (with-temp-buffer
      (apply #'call-process (car args) nil t nil (cdr args))
      (goto-char (point-min))
      (let (messages)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (when-let ((msg (im-ntfy--parse-json-line line)))
                (when (equal (alist-get 'event msg) "message")
                  (push msg messages)))))
          (forward-line 1))
        (nreverse messages)))))

;;; Interactive topic browser

(defvar im-ntfy-topic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'im-ntfy-send-input)
    (define-key map (kbd "C-c C-r") #'im-ntfy-refresh)
    map)
  "Keymap for `im-ntfy-topic-mode'.")

(define-derived-mode im-ntfy-topic-mode markdown-mode "ntfy"
  "Major mode for viewing ntfy topic messages."
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines t)
  (page-break-lines-mode))

(defun im-ntfy--format-message (msg)
  "Format MSG for display in the topic buffer."
  (let* ((time (alist-get 'time msg))
         (title (alist-get 'title msg))
         (message (alist-get 'message msg))
         (priority (alist-get 'priority msg))
         (tags (alist-get 'tags msg))
         (time-str (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time time)))
         (priority-str (pcase priority
                         (5 "🔴")
                         (4 "🟠")
                         (3 "")
                         (2 "🟢")
                         (1 "⚪")
                         (_ "")))
         (tags-str (if tags
                       (concat "[" (mapconcat #'identity (append tags nil) ", ") "] ")
                     "")))
    (concat
     "# " (concat time-str " ") priority-str
     (when (not (string-empty-p priority-str)) " ")
     tags-str
     (when title (concat "→ " title ": "))
     "\n"
     message)))

(defun im-ntfy--insert-messages (messages)
  "Insert MESSAGES into current buffer."
  (dolist (msg messages)
    (when (equal (alist-get 'event msg) "message")
      (insert "\n")
      (insert (im-ntfy--format-message msg))
      (insert "\n"))))

(defun im-ntfy--setup-topic-buffer (topic)
  "Set up a buffer for TOPIC."
  (let* ((buf-name (format "*ntfy: %s*" topic))
         (buffer (get-buffer buf-name)))
    (if buffer
        buffer
      (setq buffer (get-buffer-create buf-name))
      (with-current-buffer buffer
        (im-ntfy-topic-mode)
        (setq im-ntfy--current-topic topic)
        (erase-buffer)
        ;; Insert header
        (insert (format "Topic: %s\n" topic))
        (insert (format "Server: %s\n" im-ntfy-server))
        (insert "")
        (insert "\n")
        ;; Insert cached messages
        (let ((cached (gethash topic im-ntfy--message-cache)))
          (when cached
            (im-ntfy--insert-messages cached)))
        ;; Fetch recent messages if cache is empty
        (unless (gethash topic im-ntfy--message-cache)
          (let ((messages (im-ntfy-fetch-messages topic "48h")))
            (puthash topic messages im-ntfy--message-cache)
            (im-ntfy--insert-messages messages)))
        ;; Add input area
        (goto-char (point-max))
        (insert "\n")
        (insert im-ntfy--prompt)
        ;; Subscribe for new messages
        (let ((callback (lambda (msg cb)
                          (if (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (save-excursion
                                  ;; Find the separator before input
                                  (goto-char (point-max))
                                  (when (search-backward "" nil t)
                                    (end-of-line)
                                    (im-ntfy--insert-messages (list msg)))))
                            ;; If buffer is gone, unsubscribe
                            (im-ntfy-unsubscribe topic cb)))))
          (im-ntfy-subscribe topic callback)
          ;; Unsubscribe if buffer is killed
          (add-hook
           'kill-buffer-hook
           (lambda () (im-ntfy-unsubscribe topic callback))
           nil t))
        (current-buffer)))))

(defun im-ntfy-send-input ()
  "Send the input message to the current topic."
  (interactive nil im-ntfy-topic-mode)
  (unless im-ntfy--current-topic
    (user-error "No topic associated with this buffer"))
  (save-excursion
    (goto-char (point-max))
    (when (search-backward-regexp (concat "^" im-ntfy--prompt) nil t)
      (goto-char (match-end 0))
      (let ((message (s-trim (buffer-substring-no-properties (point) (point-max)))))
        (unless (string-empty-p (string-trim message))
          (let ((buffer (current-buffer)))
            (im-ntfy-publish
             im-ntfy--current-topic message
             :on-success (lambda ()
                           (with-current-buffer buffer
                             (when-let* ((pt (search-backward-regexp (concat "^" im-ntfy--prompt) nil t)))
                               (goto-char (match-end 0))
                               (delete-region (point) (point-max)))
                             (message ">> im-ntfy :: Message sent!")) )
             :on-error (lambda (err)
                         (message ">> im-ntfy :: Failed to send message: %s" err)))))))))

(defun im-ntfy-refresh ()
  "Refresh the current topic buffer."
  (interactive nil im-ntfy-topic-mode)
  (when im-ntfy--current-topic
    (let ((topic im-ntfy--current-topic))
      ;; Clear cache and refetch
      (remhash topic im-ntfy--message-cache)
      (let ((messages (im-ntfy-fetch-messages topic "all")))
        (puthash topic messages im-ntfy--message-cache))
      ;; Rebuild buffer
      (im-ntfy--setup-topic-buffer topic)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun im-ntfy-open-topic (topic)
  "Open an interactive buffer for TOPIC."
  (interactive
   (list (completing-read "Topic: "
                          (mapcar (lambda (t)
                                    (if (stringp t) t (plist-get t :name)))
                                  im-ntfy-topics)
                          nil nil)))
  (let ((buf (im-ntfy--setup-topic-buffer topic)))
    (pop-to-buffer buf)
    (goto-char (point-max))
    (search-backward-regexp (concat "^" im-ntfy--prompt) nil t)
    (goto-char (match-end 0))))

;;;###autoload
(defun im-ntfy-topics ()
  "Select and open a topic from `im-ntfy-topics'."
  (interactive)
  (if im-ntfy-topics
      (let* ((topic-names (mapcar (lambda (t)
                                    (if (stringp t) t (plist-get t :name)))
                                  im-ntfy-topics))
             (topic (completing-read "Select topic: " topic-names nil t)))
        (im-ntfy-open-topic topic))
    (call-interactively #'im-ntfy-open-topic)))

;;;###autoload
(defun im-ntfy-send (topic message &optional title file)
  "Interactively send MESSAGE to TOPIC with optional TITLE."
  (interactive
   (let* ((topic (completing-read "Topic: "
                                  (mapcar (lambda (t)
                                            (if (stringp t) t (plist-get t :name)))
                                          im-ntfy-topics)))
          (title (read-string "Title (optional): "))
          (message (read-string "Message: "))
          (file (when (y-or-n-p "Attach local file? ")
                  (expand-file-name (read-file-name "Attachment: ")))) )
     (list topic message
           (unless (string-empty-p title) title)
           file)))
  (im-ntfy-publish
   topic message
   :title title
   :file file
   :on-success (lambda () (message ">> ntfy :: Sent to %s: %s" topic message))
   :on-error (lambda () (message ">> ntfy :: Failed to send message to %s" topic))))

;;; List active subscriptions

(defun im-ntfy-list-subscriptions ()
  "List all active subscriptions."
  (interactive)
  (let ((subs '()))
    (maphash (lambda (topic data)
               (when (process-live-p (plist-get data :process))
                 (push (format "• %s (%d callbacks)"
                               topic
                               (length (plist-get data :callbacks)))
                       subs)))
             im-ntfy--subscriptions)
    (if subs
        (message "Active subscriptions:\n%s" (string-join (nreverse subs) "\n"))
      (message "No active subscriptions"))))

(defun im-ntfy-listen (topic)
  "Recive notifications from TOPIC directly in Emacs."
  (im-ntfy-subscribe
   topic
   (lambda (msg _cb)
     (let-alist msg
       (im-notif
        :title (or .title (format "*ntfy-%s*" topic))
        :message .message
        :duration 7
        :source (lambda ()
                  (im-ntfy-open-topic topic))
        :severity (pcase .priority
                    (5 'urgent)
                    (4 'high)
                    (3 'moderate)
                    (2 'normal)
                    (1 'low)
                    (_ 'trivial))
        :labels `("ntfy" ,topic ,@.tags))))))

;;; Cleanup

(defun im-ntfy-unsubscribe-all ()
  "Unsubscribe from all topics."
  (interactive)
  (let ((count (hash-table-count im-ntfy--subscriptions)))
    (maphash (lambda (topic _)
               (im-ntfy-unsubscribe topic))
             im-ntfy--subscriptions)
    (clrhash im-ntfy--subscriptions)
    (message "im-ntfy :: Unsubscribed from %d topics" count)))

(provide 'im-ntfy)
;;; im-ntfy.el ends here
