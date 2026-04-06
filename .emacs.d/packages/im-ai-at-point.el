;;; im-ai-at-point.el --- AI extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: utilities ai llm gpt

;; This file is not part of GNU Emacs.

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

;;; Code:

(require 'im)
(require 'im-ai)
(require 'cl-macs)
(require 's)
(require 'dash)
(require 'gptel)

;;;; Customization

;; TODO: Make directives dynamically append to sys prompt instead of showing all in the request format.
(defcustom im-ai-at-point-sys-prompt
  "You are a code generation assistant. Your output will be inserted directly into a live editor buffer WITHOUT ANY POST-PROCESSING.

# REQUEST FORMAT

<language>Programming Language</language>
<file_name>The file name you are currently working on. Your result will be in this file.</file_name>
<user_query>The specific instruction or requirement from the user.</user_query>
<target>Your output replaces this region verbatim. Emit only the replacement content — no preamble, no surrounding context, no commentary.</target>
<full_file_contents>Full file context. Optionally provided.</full_file_contents>
<lines_before>Surrounding code BEFORE the target, for reference only. Optionally provided.</lines_before>
<lines_after>Surrounding code AFTER the target, for reference only. Optionally provided.</lines_after>
<workspace_contents>All workspace items, symbols etc. Optionally provided.</workspace_contents>

# RULES

- Return ONLY raw code—no markdown fences, no explanations, no comments unless requested.
- Prefer standard library/built-ins over custom implementations.
- Use idiomatic patterns for the target language.
- Match the style/conventions visible in provided context (indentation, naming, etc.).
- Generate minimal code that fulfills the request—nothing extraneous.
- Assume code will execute immediately in context; avoid wrappers unless required.
- When a <target> block is present, your entire response must be solely the replacement for <target>. No explanations, no code fences, no preamble, no commentary."
  "System prompt used in `im-ai-at-point'."
  :type 'string
  :group 'im-ai-at-point)

(defcustom im-ai-at-point-dumb-prompt
  "You are an helpful assistant. You will get a requirement from user and handle that as simple as possible.

**Request Format:**

```
<user_query>
The thing that user wants you to do/provide.
</user_query>

<target>
The context that you need to work on. Optionally provided.
</target>

<full_file_contents>
Full file context. Optionally provided.
</full_file_contents>

<workspace_contents>
All workspace items, symbols etc. Optionally provided.
</workspace_contents>
```

ONLY output your answer to the query, with no explanations."
  "System prompt used in `im-ai-at-point'."
  :type 'string
  :group 'im-ai-at-point)

(defface im-ai-at-point-before-face
  '((((class color) (min-colors 88) (background dark))
     :background "#8b1a1a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai-at-point)

(defface im-ai-at-point-after-face
  '((((class color) (min-colors 88) (background dark))
     :background "#29422d" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (t :inherit primary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai-at-point)

;;;; Local vars

(defvar-local im-ai-at-point--last-processed-point nil)
(defvar-local im-ai-at-point--history nil)
(defvar-local im-ai-at-point--agentic? nil)
(defvar-local im-ai-reenable-aggressive-indent nil
  "Aggressive indent may fuck things up while the AI is streaming.")
(defvar-local im-ai-disable-help-at-pt-mode nil)
(add-hook 'gptel-post-stream-hook #'im-ai-at-point--cleanup-stream)
(add-hook 'gptel-post-response-functions #'im-ai-at-point--cleanup-after)

(declare-function aggressive-indent-mode "aggressive-indent")

(cl-defun im-ai-at-point--cleanup-stream ()
  (when (or gptel-mode im-ai-at-point--agentic?)
    (cl-return-from im-ai-at-point--cleanup-stream))
  (save-excursion
    (when (> (- (line-number-at-pos (point)) (line-number-at-pos im-ai-at-point--last-processed-point)) 2)
      (let ((contents
             (replace-regexp-in-string
              "^[\n ]*```.*[\n ]*$" "" (buffer-substring-no-properties im-ai-at-point--last-processed-point (point)))))
        (delete-region im-ai-at-point--last-processed-point (point))
        (goto-char im-ai-at-point--last-processed-point)
        (insert contents)
        ;; (indent-region im-ai-at-point--last-processed-point (point))
        (setq im-ai-at-point--last-processed-point (point))))))

(cl-defun im-ai-at-point--cleanup-after (beg end)
  (when im-spinner-mode
    (im-spinner-mode -1))
  (when (or gptel-mode im-ai-at-point--agentic?)
    (cl-return-from im-ai-at-point--cleanup-after))
  (when (and beg end)
    (save-excursion
      (let ((contents
             (replace-regexp-in-string
              "^[\n ]*```.*[\n ]*$" ""
              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (goto-char beg)
        (insert (s-trim contents) "\n"))
      (setq im-ai-at-point--history (append im-ai-at-point--history (list (buffer-substring-no-properties beg (point)))))
      ;; Indent the code to match the buffer indentation if it's messed up.
      (indent-region beg (point))
      (pulse-momentary-highlight-region beg (point))
      (im-ai-at-point--draw-snippet-overlay beg (1+ (line-end-position)) 'im-ai-at-point-after-face)
      (when im-ai-reenable-aggressive-indent
        (aggressive-indent-mode +1)
        (setq im-ai-reenable-aggressive-indent nil))
      (unless (bound-and-true-p im-help-at-point-mode)
        (setq im-ai-disable-help-at-pt-mode t)
        (im-help-at-point-mode +1)))))

;;;; Directive System

(defvar im-ai-at-point--directives nil
  "Alist of (name . plist) for registered directives.")

(defun im-ai-at-point--register (name &rest props)
  "Register a directive with NAME and PROPS.
PROPS may include:
  :desc        Description string
  :implies     List of directive names to run first
  :flags       Plist of gptel dynamic vars to set
  :fn          Function receiving info plist, returning partial info
               plist (can return (:abort REASON) to abort)

Any other key (e.g. :sys-prompt, :tools, :agentic?) is merged into info
directly."
  (declare (indent 1))
  (setf (alist-get name im-ai-at-point--directives nil nil #'equal)
        props))

(defun im-ai-at-point--directive-static-info (props)
  "Extract static info keys from directive PROPS (excluding meta-keys)."
  (let ((meta-keys '(:desc :implies :flags :fn)))
    (cl-loop for (k v) on props by #'cddr
             unless (memq k meta-keys)
             append (list k v))))

(defun im-ai-at-point--directive-base-name (name)
  "Return the registry key for NAME.
For parametric directives like \"tool=search_buffer\", return \"tool=\"."
  (if (string-match "\\`\\([^=]+=\\)" name)
      (match-string 1 name)
    name))

(defun im-ai-at-point--directive-param (name)
  "Return the parameter value for a parametric directive NAME.
For \"tool=search_buffer\" return \"search_buffer\", for plain
directives return nil."
  (when (string-match "\\`[^=]+=\\(.+\\)" name)
    (match-string 1 name)))

(defun im-ai-at-point--apply-directive (name info)
  "Apply directive NAME to INFO, returning updated info or (:abort REASON)."
  (let* ((base (im-ai-at-point--directive-base-name name))
         (param (im-ai-at-point--directive-param name)))
    (if-let* ((props (alist-get base im-ai-at-point--directives nil nil #'equal)))
        (let* ((info (if param
                         (im-ai-at-point--merge-info info (list :directive-param param))
                       info))
               ;; 1. run implied directives first
               (info (cl-reduce
                      (lambda (acc implied)
                        (if (eq (car acc) :abort)
                            acc
                          (im-ai-at-point--apply-directive implied acc)))
                      (plist-get props :implies)
                      :initial-value info))
               (_ (when (eq (car info) :abort) (cl-return-from im-ai-at-point--apply-directive info)))
               ;; 2. merge static info keys
               (info (im-ai-at-point--merge-info info (im-ai-at-point--directive-static-info props)))
               ;; 3. call :fn if present, fn wins over static keys
               (info (if-let* ((fn (plist-get props :fn)))
                         (let ((result (funcall fn info)))
                           (if (eq (car result) :abort)
                               result
                             (im-ai-at-point--merge-info info result)))
                       info)))
          info)
      (warn "im-ai: unknown directive %s" name)
      info)))

(defvar im-ai-at-point--append-keys '(:gptel-tools)
  "Info keys whose values are appended rather than overridden during merge.")

(defvar im-ai-at-point-inspect nil
  "When non-nil, inspect request parameters instead of sending them.")

(defun im-ai-at-point-toggle-inspect ()
  "Toggle `im-ai-at-point-inspect' and report the new state."
  (interactive)
  (setq im-ai-at-point-inspect (not im-ai-at-point-inspect))
  (message "im-ai-at-point inspect mode: %s" (if im-ai-at-point-inspect "ON" "OFF")))

(defun im-ai-at-point--merge-info (base override)
  "Merge OVERRIDE plist into BASE plist.
Keys listed in `im-ai-at-point--append-keys' are combined via
append; all other keys are overridden."
  (let ((result (copy-sequence base)))
    (cl-loop for (k v) on override by #'cddr
             do (if (memq k im-ai-at-point--append-keys)
                    (plist-put result k (append (plist-get result k) v))
                  (plist-put result k v)))
    result))

(defun im-ai-at-point--directive-regexp (name &optional parametric?)
  (if parametric?
      (rx (or (any " \t\n") line-start)
          (literal (concat "@" name))
          (group (one-or-more (not (any " \t\n,"))))
          (or (any " \t\n,") line-end))
    (rx (or (any " \t\n") line-start)
        (literal (concat "@" name))
        (or (any " \t\n,") line-end))))

(defun im-ai-at-point--directive-re-no-trailing (name)
  "Like `im-ai-at-point--directive-regexp' but without consuming trailing delimiter.
Used for multi-occurrence scanning so the trailing space is available
as the leading delimiter for the next match."
  (rx (or (any " \t\n") line-start)
      (literal (concat "@" name))
      (group (one-or-more (not (any " \t\n,"))))))

(defun im-ai-at-point--parse-directives (prompt)
  "Return (STRIPPED-PROMPT . DIRECTIVE-NAMES) from PROMPT.
Handles both @name and @name=value forms.  Parametric directives
can appear multiple times (e.g. @tool=a @tool=b)."
  (let ((directives nil)
        (stripped prompt))
    (cl-loop for (name . _) in im-ai-at-point--directives
             do (cond
                 ;; parametric @name=... — collect ALL occurrences
                 ((string-suffix-p "=" name)
                  (let ((scan-re (im-ai-at-point--directive-re-no-trailing name))
                        (strip-re (im-ai-at-point--directive-regexp name t))
                        (search-start 0))
                    (while (string-match scan-re stripped search-start)
                      (push (concat name (match-string 1 stripped)) directives)
                      (setq search-start (match-end 0)))
                    (setq stripped (s-replace-regexp strip-re " " stripped))))
                 ;; plain @name
                 (t
                  (when (s-matches? (im-ai-at-point--directive-regexp name) prompt)
                    (push name directives)
                    (setq stripped (s-replace-regexp (im-ai-at-point--directive-regexp name) " " stripped))))))
    (cons (s-trim stripped)
          (nreverse directives))))

(defun im-ai-at-point--build-initial-info (prompt stripped-prompt)
  "Build initial info plist from current buffer state."
  (let* ((region (when (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
         (rbegin (if (use-region-p) (region-beginning) (point)))
         (rend   (if (use-region-p) (region-end) (point))))
    (list
     :prompt          stripped-prompt
     :raw-prompt      prompt
     :sys-prompt      im-ai-at-point-sys-prompt
     :tools           nil
     :agentic?        nil
     :region          region
     :rbegin          rbegin
     :rend            rend
     :edit-region?    nil
     :buffer          (current-buffer)
     :language        (im-ai--get-current-language)
     :context-fragments nil
     :gptel-vars      nil)))

(defun im-ai-at-point--build-gptel-vars (info)
  "Build the :gptel-vars alist from all :gptel-* keys in INFO."
  (cl-loop for (k v) on info by #'cddr
           when (and (keywordp k)
                     (not (eq k :gptel-vars))
                     (string-prefix-p ":gptel" (symbol-name k)))
           collect (cons k v)))

(defun im-ai-at-point--run-directives (info directive-names)
  "Apply all DIRECTIVE-NAMES to INFO in order.
Return final info or (:abort REASON)."
  (let ((result (cl-reduce
                 (lambda (acc name)
                   (if (eq (car acc) :abort)
                       acc
                     (im-ai-at-point--apply-directive name acc)))
                 (cl-remove-duplicates directive-names :test #'equal :from-end t)
                 :initial-value info)))
    (if (eq (car result) :abort)
        result
      (plist-put result :gptel-vars (im-ai-at-point--build-gptel-vars result)))))

(defun im-ai-at-point--build-formatted-prompt (info)
  "Build the final XML-wrapped prompt string from INFO."
  (let* ((language     (plist-get info :language))
         (buffer       (plist-get info :buffer))
         (rbegin       (plist-get info :rbegin))
         (rend         (plist-get info :rend))
         (edit-region? (plist-get info :edit-region?))
         (region       (plist-get info :region))
         (fragments    (plist-get info :context-fragments))
         (prompt       (plist-get info :prompt)))
    (s-join
     "\n"
     (append
      (list
       "<language>" language "</language>"
       "<buffer_name>" (buffer-name buffer) "</buffer_name>")
      (when-let* ((file (buffer-file-name buffer)))
        (list "<file_name>" (file-name-nondirectory file) "</file_name>"))
      (if edit-region?
          (list "<current_range>"
                (format "%d-%d"
                        (line-number-at-pos rbegin)
                        (line-number-at-pos rend))
                "</current_range>")
        (list "<current_line>"
              (number-to-string (line-number-at-pos))
              "</current_line>"))
      (when edit-region?
        (list
         "<target>" (s-trim region) "</target>"))
      ;; directive-contributed fragments
      (cl-loop for (tag content) in fragments
               append (list (format "<%s>" tag) content (format "</%s>" tag)))
      (list "<user_query>" prompt "</user_query>")))))

;;;; Directives

(im-ai-at-point--register "dumb"
  :desc "Use a simpler sys prompt, without much context."
  :sys-prompt im-ai-at-point-dumb-prompt)

(im-ai-at-point--register "noexp"
  :desc "Add instruction to remove any explanations."
  :fn (lambda (info)
        (list :prompt (concat (plist-get info :prompt)
                              "\nDo not include any explanations, only output the solution."))))

(im-ai-at-point--register "region"
  :desc "Refer to region but don't change it."
  :fn (lambda (info)
        (if-let* ((region (plist-get info :region)))
            (list :edit-region? nil
                  :context-fragments
                  (append (plist-get info :context-fragments)
                          (list (list "region" (s-trim region)))))
          (list :abort "No active region for @region"))))

(im-ai-at-point--register "fullfile"
  :desc "Add full file as context."
  :fn (lambda (info)
        (with-current-buffer (plist-get info :buffer)
          (list :context-fragments
            (append (plist-get info :context-fragments)
                    (list
                     (list "full_file_contents"
                           (save-restriction
                             (widen)
                             (buffer-substring-no-properties (point-min) (point-max))))))))))

(im-ai-at-point--register "context"
  :desc "Add lines around point as context."
  :fn (lambda (info)
        (with-current-buffer (plist-get info :buffer)
          (let* ((rbegin (plist-get info :rbegin))
                 (rend   (plist-get info :rend))
                 (before-start (save-excursion (goto-char rbegin) (forward-line -50) (line-beginning-position)))
                 (after-end    (save-excursion (goto-char rend)   (forward-line  50) (line-end-position))))
            (list :context-fragments
              (append (plist-get info :context-fragments)
                      (list (list "lines_before"
                                  (buffer-substring-no-properties before-start rbegin))
                            (list "lines_after"
                                  (buffer-substring-no-properties rend after-end)))))))))

(im-ai-at-point--register "file"
  :desc "Add file context (imenu items)."
  :fn (lambda (info)
        (with-current-buffer (plist-get info :buffer)
          (list :context-fragments
            (append (plist-get info :context-fragments)
                    (list (list "file_context"
                                (im-ai-file-context
                                 (im-current-project-root)
                                 (buffer-file-name)))))))))

(im-ai-at-point--register "workspace"
  :desc "Add workspace context (imenu items)."
  :fn (lambda (info)
        (list :context-fragments
          (append (plist-get info :context-fragments)
                  (list (list "workspace_contents"
                              (im-ai-workspace-context)))))))

(im-ai-at-point--register "agent"
  :desc "Can operate on full file with tools."
  :gptel-use-tools t
  :gptel-include-tool-results t
  :gptel-tools (list
                (alist-get
                 "read_buffer_lines"
                 (alist-get "buffers" gptel--known-tools nil nil #'equal) nil nil #'equal)
                (alist-get
                 "search_buffer"
                 (alist-get "buffers" gptel--known-tools nil nil #'equal) nil nil #'equal)
                (alist-get
                 "edit_buffer"
                 (alist-get "buffers" gptel--known-tools nil nil #'equal) nil nil #'equal) )
  :fn (lambda (info)
        (list :agentic? t
              :prompt (concat (plist-get info :prompt)
                              "\nUse the tools provided to make edits in the current buffer. Do not output anything else."))))

(im-ai-at-point--register "model="
  :desc "Switch model for this request."
  :fn (lambda (info)
        (let ((model-str (plist-get info :directive-param)))
          (if-let* ((entry (assoc model-str (im-ai--gptel-all-models))))
              (list :gptel-backend (cadr entry)
                    :gptel-model   (caddr entry))
            (list :abort (format "Unknown model: %s" model-str))))))

(im-ai-at-point--register "tool="
  :desc "Add a tool by name."
  :gptel-use-tools t
  :gptel-include-tool-results nil
  :fn (lambda (info)
        (let ((tool-name (plist-get info :directive-param)))
          (if-let* ((tool (gptel-get-tool tool-name)))
              (list :gptel-tools (list tool))
            (list :abort (format "Unknown tool: %s" tool-name))))))

;;;; Initialize minibuffer completion candidates

(with-eval-after-load 'gptel
  (let ((ai-commands `(,@(--map (cons (concat "@" (car it)) (plist-get (cdr it) :desc)) im-ai-at-point--directives)
                       ,@(--map (cons (format "@model=%s" (car it)) "Switch model") (im-ai--gptel-all-models))
                       ,@(cl-loop for (_cat . tools) in gptel--known-tools
                                  append (cl-loop for (name . _) in tools
                                                  collect (cons (format "@tool=%s" name) "Add tool"))))))
    (im-cape
     :name ai-commands
     :completion ai-commands
     :annotate (lambda (obj item)
                 (concat " " (alist-get item ai-commands nil nil #'equal)))
     :extractor (lambda (it) (mapcar #'car it))
     :bound filename
     :kind (lambda (_xs _x) "" 'module)
     :category symbol)))

;;;; Interactive

(defun im-ai-at-point--read-prompt (context)
  (list
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local completion-at-point-functions
                      (list (cape-capf-prefix-length #'im-cape-ai-commands 1))))
      (read-string
       (format "%s (current model: %s): " context (im-ai--format-model-name))))))

(defun im-ai-at-point--overlay-at-point (&optional type)
  "Return the im-ai overlay at or before point, if any.
TYPE can be `before' or `after' to find a specific overlay.
If TYPE is `after', find the overlay with face `im-ai-after-face'.
If TYPE is `before', find the overlay with face `im-ai-before-face'.
If TYPE is nil, return any im-ai overlay."
  (let ((predicate (lambda (o)
                     (and (overlay-get o 'im-ai-at-point)
                          (pcase type
                            ('after (eq (overlay-get o 'face) 'im-ai-at-point-after-face))
                            ('before (eq (overlay-get o 'face) 'im-ai-at-point-before-face))
                            (_ t))))))
    (or (seq-find predicate (overlays-at (point)))
        (let ((pos (point)) result)
          (while (and (not result) (> pos (point-min)))
            (setq pos (previous-overlay-change pos))
            (setq result (seq-find predicate (overlays-at pos))))
          result))))

(defun im-ai-at-point--clear-overlays ()
  (remove-overlays (point-min) (point-max) 'im-ai-at-point t)
  (when im-ai-disable-help-at-pt-mode
    (im-help-at-point-mode -1)))

(defun im-ai-at-point-accept ()
  (interactive)
  (when-let* ((ov (im-ai-at-point--overlay-at-point 'before)))
    (delete-region (overlay-start ov) (overlay-end ov)))
  (im-ai-at-point--clear-overlays))

(defun im-ai-at-point-reject ()
  (interactive)
  (when-let* ((ov (im-ai-at-point--overlay-at-point 'after)))
    (delete-region (overlay-start ov) (overlay-end ov)))
  (im-ai-at-point--clear-overlays))

(defun im-ai-at-point-iterate (prompt)
  (interactive (im-ai-at-point--read-prompt "Iterate"))
  (when-let* ((ov (im-ai-at-point--overlay-at-point 'after)))
    (delete-region (overlay-start ov) (overlay-end ov)))
  (im-ai-at-point--clear-overlays)
  (im-ai-at-point prompt im-ai-at-point--history))

(defvar-keymap im-ai-at-point-rewrite-map
  :doc "Keymap for ai rewrite actions at point."
  "C-c C-c" #'im-ai-at-point-accept
  "C-c C-k" #'im-ai-at-point-reject
  "C-c C-i" #'im-ai-at-point-iterate)

(defun im-ai-at-point--draw-snippet-overlay (beg end face)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'keymap im-ai-at-point-rewrite-map)
    (overlay-put ov 'im-ai-at-point t)
    (overlay-put ov 'help-echo (format "accept: \\[im-ai-at-point-accept], reject: \\[im-ai-at-point-reject], iterate: \\[im-ai-at-point-iterate]"))
    ov))

;;;; Main

(defun im-ai-at-point (prompt &optional history)
  (interactive (im-ai-at-point--read-prompt "Query"))
  (pcase-let* ((`(,stripped . ,directive-names) (im-ai-at-point--parse-directives prompt))
               (info (im-ai-at-point--build-initial-info prompt stripped))
               ;; FIXME: Better handle this?
               ;; handle region: if region active and no @region, it's edit-region
               (info (if (and (use-region-p) (not (member "region" directive-names)))
                         (im-ai-at-point--merge-info info '(:edit-region? t))
                       info))
               (info (im-ai-at-point--run-directives info directive-names)))
    (pcase info
      (`(:abort ,reason)
       (user-error "im-ai: aborted: %s" reason))
      (_
       (let* ((agentic?         (plist-get info :agentic?))
              (edit-region?     (plist-get info :edit-region?))
              (sys-prompt       (plist-get info :sys-prompt))
              (gptel-vars       (plist-get info :gptel-vars))
              (gptel-include-reasoning nil)
              (formatted     (im-ai-at-point--build-formatted-prompt info))
              (response-buffer
               (if agentic?
                   (get-buffer-create (format "*im-ai-response: %s*" (buffer-name)))
                 (current-buffer))))
         ;; cursor setup
         (if (and edit-region? (not agentic?))
             (progn
               (im-ai-at-point--draw-snippet-overlay
                (region-beginning) (region-end) 'im-ai-at-point-before-face)
               (goto-char (region-end))
               (deactivate-mark)
               (insert "\n")
               (backward-char))
           (when (use-region-p)
             (goto-char (region-end))
             (deactivate-mark)))
         (setq im-ai-at-point--last-processed-point (point))
         (when (and (not agentic?) (bound-and-true-p aggressive-indent-mode))
           (setq im-ai-at-point--reenable-aggressive-indent t)
           (aggressive-indent-mode -1))
         (let ((history (setq im-ai-at-point--history
                              (if history
                                  (append history (list prompt))
                                (list formatted)))))
           (unless im-ai-at-point-inspect
             (im-spinner-mode))
           (when agentic?
             (with-current-buffer response-buffer
               (goto-char (point-max))
               (insert "\n\n---------------------------------------------\n\n")))
           (with-current-buffer response-buffer
             (setq im-ai-at-point--agentic? agentic?)
             (cl-progv
                 (--map (intern (substring (symbol-name (car it)) 1)) gptel-vars)
                 (mapcar #'cdr gptel-vars)
               (if im-ai-at-point-inspect
                   (im-inspect
                    (list
                     :info info
                     :directive-names directive-names
                     :agentic? agentic?
                     :edit-region? edit-region?
                     :gptel-vars gptel-vars
                     :gptel-backend gptel-backend
                     :gptel-model gptel-model
                     :gptel-use-tools gptel-use-tools
                     :gptel-tools gptel-tools
                     :gptel-include-tool-results gptel-include-tool-results
                     :sys-prompt sys-prompt
                     :formatted formatted))
                 (gptel-request history
                   :stream t
                   :system sys-prompt
                   :fsm (gptel-make-fsm :handlers gptel-send--handlers)
                   :buffer response-buffer))))))))))

;;;; Footer

(provide 'im-ai-at-point)
;;; im-ai-at-point.el ends here
