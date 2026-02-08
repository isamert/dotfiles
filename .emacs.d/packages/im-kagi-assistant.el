;;; im-kagi-assistant.el --- Interface to Kagi Assistant -*- lexical-binding: t; -*-

;; Author: Isa Mert Gurbuz
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (plz "0.7"))
;; Keywords: ai, tools

;;; Commentary:

;; Simple interface to Kagi Assistant from Emacs.
;;
;; Do M-x `im-kagi-assistant' to start a new conversation.
;;
;; Limitations:
;; - Can't change earlier prompts right now
;; - Web search is always on.
;;
;; DISCLAIMER: Mostly AI generated.

;;; Code:

(require 'plz)
(require 'json)

;;;; Customization

(defgroup im-kagi-assistant nil
  "Kagi Assistant interface."
  :group 'tools)

(defcustom im-kagi-assistant-cookie nil
  "Cookie for Kagi authentication.
Set this to your Kagi session cookie."
  :type 'string
  :group 'im-kagi-assistant)

(defcustom im-kagi-assistant-model "glm-4-7-thinking"
  "Model to use for Kagi Assistant."
  :type 'string
  :group 'im-kagi-assistant)

(defcustom im-kagi-assistant-models '(("Kimi Quick" . "ki_quick")
                                      ("GLM-4-7" . "glm-4-7")
                                      ("GLM-4-7 Thinking" . "glm-4-7-thinking")
                                      ("Qwen 3 Coder" . "qwen-3-coder")
                                      ("Groq 4 Fast" . "grok-4-fast")
                                      ("Groq 4.1 Fast Thinking" . "grok-4-fast-thinking")
                                      ("Qwen 235b Thinking" . "qwen-3-235b-a22b-thinking")
                                      ("Kimi K2-5" . "kimi-k2-5")
                                      ("Kimi K2-5 Thinking" . "kimi-k2-5-thinking"))
  "List of models."
  :type '(list string)
  :group 'im-kagi-assistant)

(defcustom im-kagi-assistant-show-sources t
  "Whether to show search sources in responses."
  :type 'boolean
  :group 'im-kagi-assistant)

;;;; Internal

(defvar-local im-kagi-assistant--thread-id nil
  "Current thread ID for the conversation.")

(defvar-local im-kagi-assistant--branch-id "00000000-0000-4000-0000-000000000000"
  "Current branch ID for the conversation.")

(defvar-local im-kagi-assistant--last-message-id nil
  "Last message ID in the conversation.")

(defconst im-kagi-assistant--url "https://kagi.com/assistant/prompt"
  "Kagi Assistant API endpoint.")

(defconst im-kagi-assistant--user-marker "\n\n[USER]: "
  "Marker for user messages.")

(defconst im-kagi-assistant--assistant-marker "\n\n[ASSISTANT]: "
  "Marker for assistant messages.")

(defconst im-kagi-assistant--sources-marker "\n\n[SOURCES]:\n"
  "Marker for search sources.")

(defconst im-kagi-assistant--thinking-marker-begin "\n\n<thinking>\n"
  "Marker for reasoning/thinking blocks.")

(defconst im-kagi-assistant--thinking-marker-end "\n</thinking>\n\n"
  "Marker for reasoning/thinking blocks.")

;;;; Utility

(defun im-kagi-assistant--build-headers ()
  "Build HTTP headers for Kagi API request."
  `(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:147.0) Gecko/20100101 Firefox/147.0")
    ("Accept" . "application/vnd.kagi.stream")
    ("Accept-Language" . "en-US,en;q=0.9")
    ("Content-Type" . "application/json")
    ("Origin" . "https://kagi.com")
    ("Cookie" . ,im-kagi-assistant-cookie)))

(defun im-kagi-assistant--build-payload (prompt)
  "Build JSON payload for PROMPT."
  (let* ((focus (if im-kagi-assistant--thread-id
                    `((thread_id . ,im-kagi-assistant--thread-id)
                      (branch_id . ,im-kagi-assistant--branch-id)
                      (prompt . ,prompt)
                      (message_id . ,im-kagi-assistant--last-message-id))
                  `((thread_id . ,json-null)
                    (branch_id . ,im-kagi-assistant--branch-id)
                    (prompt . ,prompt))))
         (profile `((id . ,json-null)
                    (personalizations . t)
                    (internet_access . t)
                    (model . ,im-kagi-assistant-model)
                    (lens_id . ,json-null)))
         (payload `((focus . ,focus)
                    (profile . ,profile))))
    ;; Add threads array only for new conversations
    (unless im-kagi-assistant--thread-id
      (setq payload (append payload
                            `((threads . [((tag_ids . [])
                                           (saved . ,json-false)
                                           (shared . ,json-false))])))))
    (json-encode payload)))

(defun im-kagi-assistant--parse-stream-line (line)
  "Parse a LINE from the Kagi stream response."
  (when (and line (not (string-empty-p line)))
    (let ((colon-pos (string-match ":" line)))
      (when colon-pos
        (let ((type (substring line 0 colon-pos))
              (data (substring line (1+ colon-pos))))
          (cons type data))))))

(defun im-kagi-assistant--extract-sources-from-html (html)
  "Extract search sources from HTML response."
  (let ((sources '())
        (pos 0))
    ;; Extract URLs and titles from the HTML
    (while (string-match "<a href=\"\\([^\"]+\\)\"[^>]*>\\([^<]*\\)</a>" html pos)
      (let ((url (match-string 1 html))
            (text (match-string 2 html)))
        (setq pos (match-end 0))
        ;; Skip internal links and empty URLs
        (when (and url
                   (string-prefix-p "http" url)
                   (not (string-prefix-p "https://kagi.com" url)))
          (push (cons url text) sources))))
    ;; Also extract titles marked with <strong>
    (setq pos 0)
    (let ((titles '()))
      (while (string-match "<strong>\\([^<]+\\)</strong>" html pos)
        (push (match-string 1 html) titles)
        (setq pos (match-end 0)))
      (setq titles (nreverse titles))
      ;; Combine titles with URLs where possible
      (let ((result '())
            (url-list (nreverse sources)))
        (dolist (url-pair url-list)
          (let* ((url (car url-pair))
                 ;; Find a title that might match this URL
                 (title (or (car titles) (cdr url-pair))))
            (when titles (setq titles (cdr titles)))
            (push (list :url url :title (string-trim title)) result)))
        (nreverse result)))))

(defun im-kagi-assistant--parse-search-details (html)
  "Parse the search details section from HTML and return sources list."
  ;; Look for content within <details><summary>Searched with Kagi...
  (when (string-match "<details><summary>Searched with Kagi" html)
    (let* ((start (match-beginning 0))
           (end (or (string-match "</details>" html start)
                    (length html)))
           (details-content (substring html start (min (+ end 10) (length html)))))
      (im-kagi-assistant--extract-sources-from-html details-content))))

(defun im-kagi-assistant--format-sources (sources)
  "Format SOURCES list for display."
  (when sources
    (let ((formatted (list im-kagi-assistant--sources-marker))
          (seen-urls (make-hash-table :test 'equal))
          (count 0))
      (dolist (source sources)
        (let ((url (plist-get source :url))
              (title (plist-get source :title)))
          ;; Deduplicate URLs
          (unless (gethash url seen-urls)
            (puthash url t seen-urls)
            (setq count (1+ count))
            (push (format "%d. [%s](%s)\n"
                          count
                          (if (and title (not (string-empty-p title)))
                              (decode-coding-string title 'utf-8)
                            url)
                          url)
                  formatted))))
      (when (> count 0)
        (apply #'concat (nreverse formatted))))))

(defun im-kagi-assistant--extract-thinking (html)
  "Extract thinking/reasoning content from HTML."
  (when (string-match "<details><summary>Thinking</summary>\\(\\(?:.\\|\n\\)*?\\)</details>" html)
    (let ((content (match-string 1 html)))
      ;; Clean up HTML tags
      (setq content (replace-regexp-in-string "<[^>]+>" "" content))
      (setq content (replace-regexp-in-string "&quot;" "\"" content))
      (setq content (replace-regexp-in-string "&lt;" "<" content))
      (setq content (replace-regexp-in-string "&gt;" ">" content))
      (setq content (replace-regexp-in-string "&amp;" "&" content))
      (string-trim content))))

(defun im-kagi-assistant--extract-md-and-sources (json-str)
  "Extract markdown content, sources, and thinking from JSON-STR."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (parsed (json-read-from-string json-str))
             (md (alist-get 'md parsed))
             (reply (alist-get 'reply parsed))
             (text (alist-get 'text parsed))
             (sources nil)
             (thinking nil))
        ;; Extract thinking from reply or text
        (when reply
          (setq thinking (im-kagi-assistant--extract-thinking reply))
          (setq sources (im-kagi-assistant--parse-search-details reply)))
        (when (and (not thinking) text)
          (setq thinking (im-kagi-assistant--extract-thinking text)))
        (when (and (not sources) text)
          (setq sources (im-kagi-assistant--parse-search-details text)))
        ;; Clean thinking from md if present
        (when (and md (string-match "^<details><summary>Thinking</summary>" md))
          (setq md (replace-regexp-in-string
                    "<details><summary>Thinking</summary>\\(?:.\\|\n\\)*?</details>\\s-*"
                    "" md)))
        (list :md (or md text)
              :sources sources
              :thinking thinking))
    (error nil)))

(defun im-kagi-assistant--update-thread-info (json-str type)
  "Update thread info from JSON-STR of TYPE."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (parsed (json-read-from-string json-str)))
        (pcase type
          ("thread.json"
           (when-let ((id (alist-get 'id parsed)))
             (setq im-kagi-assistant--thread-id id))
           nil)
          ("new_message.json"
           (when-let ((id (alist-get 'id parsed)))
             (setq im-kagi-assistant--last-message-id id))
           (when (equal (alist-get 'state parsed) "done")
             (let* ((md (alist-get 'md parsed))
                    (reply (alist-get 'reply parsed))
                    (sources (when reply (im-kagi-assistant--parse-search-details reply)))
                    (thinking (when reply (im-kagi-assistant--extract-thinking reply))))
               (list :md md :sources sources :thinking thinking))))
          ("messages.json"
           (when (and (listp parsed) (> (length parsed) 0))
             (let ((last-msg (car (last parsed))))
               (when-let ((id (alist-get 'id last-msg)))
                 (setq im-kagi-assistant--last-message-id id))))
           nil)))
    (error nil)))

(defun im-kagi-assistant--process-response (buffer response)
  "Process RESPONSE and insert into BUFFER."
  (with-current-buffer buffer
    (let ((lines (split-string response "\n"))
          (final-result nil)
          (current-content nil)
          (current-sources nil)
          (current-thinking nil))
      (dolist (line lines)
        (when-let ((parsed (im-kagi-assistant--parse-stream-line line)))
          (let ((type (car parsed))
                (data (cdr parsed)))
            (pcase type
              ("thread.json"
               (im-kagi-assistant--update-thread-info data type))
              ("messages.json"
               (im-kagi-assistant--update-thread-info data type))
              ("new_message.json"
               (when-let ((result (im-kagi-assistant--update-thread-info data type)))
                 (setq final-result result)))
              ("tokens.json"
               (let ((extracted (im-kagi-assistant--extract-md-and-sources data)))
                 (when (plist-get extracted :md)
                   (setq current-content (plist-get extracted :md)))
                 (when (plist-get extracted :sources)
                   (setq current-sources (plist-get extracted :sources)))
                 (when (plist-get extracted :thinking)
                   (setq current-thinking (plist-get extracted :thinking)))))))))
      ;; Use final result if available
      (let ((md (or (plist-get final-result :md) current-content))
            (sources (or (plist-get final-result :sources) current-sources))
            (thinking (or (plist-get final-result :thinking) current-thinking)))
        (goto-char (point-max))
        ;; Insert thinking if available
        (when thinking
          (insert im-kagi-assistant--thinking-marker-begin)
          (insert thinking)
          (insert im-kagi-assistant--thinking-marker-end))
        ;; Insert the markdown response
        (when md
          ;; Clean any remaining details tags from md
          (setq md (replace-regexp-in-string
                    "<details><summary>[^<]*</summary>\\(?:.\\|\n\\)*?</details>\\s-*"
                    "" md))
          (setq md (string-trim md))
          (unless (string-empty-p md)
            (insert "\n" md)))
        ;; Insert sources if available
        (when (and im-kagi-assistant-show-sources sources)
          (insert (im-kagi-assistant--format-sources sources)))))))

(defun im-kagi-assistant--get-user-input ()
  "Get the user input from the current buffer."
  (save-excursion
    (goto-char (point-max))
    (when (search-backward im-kagi-assistant--user-marker nil t)
      (let ((start (+ (point) (length im-kagi-assistant--user-marker))))
        (goto-char (point-max))
        (string-trim (buffer-substring-no-properties start (point)))))))

;;;; kagi-assistant-mode

(defun im-kagi-assistant-send ()
  "Send the current user input to Kagi Assistant."
  (interactive nil im-kagi-assistant-mode)
  (unless im-kagi-assistant-cookie
    (user-error "Please set `im-kagi-assistant-cookie' first"))
  (let* ((prompt (im-kagi-assistant--get-user-input))
         (buffer (current-buffer)))
    (when (or (not prompt) (string-empty-p prompt))
      (user-error "No input to send"))
    ;; Add assistant marker
    (goto-char (point-max))
    (insert im-kagi-assistant--assistant-marker)
    ;; Make request
    (plz 'post im-kagi-assistant--url
      :headers (im-kagi-assistant--build-headers)
      :body (im-kagi-assistant--build-payload prompt)
      :as 'string
      :then (lambda (response)
              (im-kagi-assistant--process-response buffer response)
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert im-kagi-assistant--user-marker)))
      :else (lambda (err)
              (with-current-buffer buffer
                (goto-char (point-max))
                (insert (format "\n[ERROR]: %s" err)))))))

(defun im-kagi-assistant-new-input ()
  "Start a new user input block."
  (interactive nil im-kagi-assistant-mode)
  (goto-char (point-max))
  (unless (looking-back (regexp-quote im-kagi-assistant--user-marker) nil)
    (insert im-kagi-assistant--user-marker)))

(defun im-kagi-assistant-clear ()
  "Clear the buffer and start a new conversation."
  (interactive nil im-kagi-assistant-mode)
  (setq im-kagi-assistant--thread-id nil)
  (setq im-kagi-assistant--last-message-id nil)
  (erase-buffer)
  (insert "# Kagi Assistant")
  (insert im-kagi-assistant--user-marker))

(defvar im-kagi-assistant-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'im-kagi-assistant-send)
    (define-key map (kbd "C-c C-n") #'im-kagi-assistant-new-input)
    (define-key map (kbd "C-c C-k") #'im-kagi-assistant-clear)
    map)
  "Keymap for `im-kagi-assistant-mode'.")

;;;###autoload
(define-minor-mode im-kagi-assistant-mode
  "Minor mode for interacting with Kagi Assistant."
  :lighter " Kagi"
  :keymap im-kagi-assistant-mode-map
  (when im-kagi-assistant-mode
    (setq-local im-kagi-assistant--thread-id nil)
    (setq-local im-kagi-assistant--last-message-id nil)))

(defun im-kagi-assistant-change-model ()
  "Change the Kagi Assistant model via completing-read."
  (interactive)
  (let* ((models im-kagi-assistant-models)
         (choice (completing-read "Select model: " models nil t)))
    (setq im-kagi-assistant-model (cdr (assoc choice models)))
    (message "Kagi Assistant model set to: %s" choice)))

;;;###autoload
(defun im-kagi-assistant ()
  "Open a new Kagi Assistant buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*Kagi Assistant*")))
    (switch-to-buffer buffer)
    (markdown-mode)
    (im-kagi-assistant-mode 1)
    (insert "# Kagi Assistant")
    (insert im-kagi-assistant--user-marker)
    (message "Kagi Assistant ready. Type your message and press C-c RET to send.")))

(provide 'im-kagi-assistant)
;;; im-kagi-assistant.el ends here
