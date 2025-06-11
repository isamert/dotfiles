;;; im-ai.el --- AI extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

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

;; My AI extensions.  Mostly uses `org-ai'/`gptel' to do the
;; heavy-lifting.

;; - `im-ai' is a function that gives you pre-defined prompts (see
;;   `im-ai-prompts') that you can use (by standalone, or on a region
;;   etc.) and get results in a predefined org buffer (uses `org-ai').
;; - `im-ai-snippet' simply generates a snippet you requested in the
;;   language of the current buffer.
;; - `im-ai-lookup' is like `im-ai' but opens a fresh buffer (uses
;;   `gptel')
;; - `im-ai-switch-model' switches default model for all functions.
;; - `im-ai-{next,previous}-block' goes to next/prev prompt/ai answer.

;;; Code:

(require 's)
(require 'org-ai)
(require 'gptel)

;;;; Customization

(defgroup im-ai nil
  "Settings for `im-ai'."
  :group 'utils)

(defcustom im-ai-model "gpt-4.1-mini"
  "AI model name."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-service "openai"
  "AI service."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-models '("openai:gpt-4.1"
                          "openai:gpt-4.1-mini"
                          "openai:gpt-4.1-nano"
                          "deepseek:deepseek-chat"
                          "deepseek:deepseek-reasoner")
  "AI service:model list."
  :type 'list
  :group 'im-ai)

(defcustom im-ai-snippet-sys-prompt
  "You are a code generation assistant focused on producing accurate, efficient solutions using best-practice and idiomatic code. Adhere strictly to the following rules:

1. Always prefer standard libraries and built-in functions over custom code, unless a standard solution is impractical.
2. Output concise solutions—include only essential code. Omit comments, type hints, or boilerplate unless they are explicitly required or critical for understanding.
3. If the query mentions edge cases or constraints, ensure your solution properly handles or documents them.
4. When context (such as file content or workspace symbols) is provided, tailor your solution to integrate cleanly, minimizing unnecessary changes.
5. If multiple idiomatic solutions exist, pick the shortest and simplest idiomatic approach for the requested language. Do not explain alternatives unless the query requests them.
6. If user input, output, or error handling is required by the query, provide only the minimal viable code.
7. When in doubt about unspecified details, assume standard programming conventions for the given language.

**Request Format:**

```
Language: <Programming language>
Query: <The user query.>
Context: <Optional context.>
Full file contents: <Optional, if relevant.>
Workspace contents: <Optional, if relevant.>
```

**Response Format:**

```
<Reasoning lines as a comment, if relevant.>
<Code solution>
```

**Example Request:**

```
Language: Python
Query: Merge two lists alternately
```

**Example Response:**

```python
# Use zip and chain for alternate merging of lists.
from itertools import chain
list(chain.from_iterable(zip(list1, list2)))
```

Handle further requests in this precise format. Do not provide explanations outside the required reasoning comment."
  "System prompt used in `im-ai-snippet'."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-prompts
  '((:display "[EMPTY]"
     :empty t)
    (:prompt "Summarize the following section: #{region}" :region t)
    (:prompt "Add documentation to the following code: #{region}" :region t)
    (:prompt "Add documentation to the following Elisp code: #{region}"
     :system "You are an Emacs lisp and documentation expert. You add inline documentation to given code by complying with Emacs lisp documentation conventions (refer parameters in UPPERCASE style without quoting them, use `this' style quoting for other Elisp objects/references). Refer to \"Tips for Documentation Strings\" in Emacs manual. ONLY return the documentation string in quotes, without repeating the code."
     :region t)
    (:display "Add documentation testing to the following Elisp code."
     :prompt "You are an Emacs lisp and documentation expert. You create tests for given function and add them into the documentation string of the function in the following format:

>> <TEST-CODE>
=> <TEST-RESULT>

Where <TEST-CODE> is the test itself, like (+ 1 1), and <TEST-RESULT> is the result of calling <TEST-CODE>, like 2.

Here is the function:
#{code}"
     :region t)
    (:prompt "Explain following code: #{code}" :region t)
    (:display "Grammar, spelling fix/improve"
     :prompt "The sentence is:  #{region}"
     :system "I want you to act as an English translator, spelling corrector and improver. I will speak to you in any language and you will detect the language, translate it and answer in the corrected and improved version of my text, in English. I want you to replace my simplified words and sentences with more upper level English words and sentences. Keep the meaning same. I want you to only reply the correction, the improvements and nothing else, do not write explanations. Keep the text to the point, don't add too many mumbo-jumbo. Also do corrections based on the context, like using proper abbreviations etc.")
    (:display "Rambling"
     :prompt
     (lambda ()
       (format "I will give you my ramblings about a topic. I want you to turn it into a coherent piece of text. Remove unnecessary parts (like if I changed my mind during the text, only include the latest idea). Fix grammatical errors and misspellings. Make it easy to read and understand. Summarize the points without loosing important details. Turn them into bullet points. Here is your first input: \n\n %s" (read-string "Ramble: " (im-region-or ""))))))
  "A list of prompts for the `im-ai' function.

Each element is a property list with the following keys:
- PROMPT: The user prompt.
- DISPLAY (Optional) The text to show to the user while selecting prompts.
- SYSTEM: (Optional) The system prompt for the AI assistant.
- REGION: (Optional) A boolean indicating whether to include the
  selected region when calling `im-ai' with the prompt.
- MODEL: (Optional) Model to use with this prompt.  (Default=`org-ai-default-chat-model')"
  :type 'list
  :group 'im-ai)

(defcustom im-ai-file "~/ai.org"
  "If non-nil, use this file to output interactive `im-ai' requests."
  :type 'file
  :group 'im-ai)

(defface im-ai-before-face
  '((((class color) (min-colors 88) (background dark))
     :background "#8b1a1a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai)

(defface im-ai-after-face
  '((((class color) (min-colors 88) (background dark))
     :background "#29422d" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (t :inherit primary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai)

;;;; Variables

(defconst im-ai--buffer "*im-ai*")

(defconst im-ai--block-start-regexp "^\\[\\(ME\\|AI\\|AI_REASON\\)\\]:")

;;;; im-ai

(cl-defun im-ai (prompt
                 &key
                 (system)
                 (model im-ai-model)
                 (service im-ai-service)
                 (callback)
                 (empty))
  "Use the ChatGPT to generate responses to user prompts.

- PROMPT is the user input to generate a response for.
- SYSTEM is the system prompt for the AI assistant.
- CALLBACK is a function that takes the prompt and the generated
  response as an argument and performs custom actions with it. It
  defaults to a function that displays the response in a separate
  buffer using `markdown-mode' and switches to that buffer.

When called interactively, this function prompts for PROMPT and
lets you customize SYSTEM and CALLBACK. See `im-ai-prompts' for
predefined prompts."
  (interactive
   (let* ((selection (im-completing-read
                      "Prompt: "
                      (--filter (or (and (use-region-p) (plist-get it :region)) t)
                                im-ai-prompts)
                      :formatter (lambda (it)
                                   (or (plist-get it :display)
                                       (plist-get it :prompt)))))
          (prompt (or (plist-get selection :prompt) selection))
          (system (plist-get selection :system))
          (model (plist-get selection :model))
          (service (plist-get selection :service))
          (empty (plist-get selection :empty)))
     (-let (((service model) (cond
                              (current-prefix-arg (im-ai--select-model))
                              ((and service model) (list service model))
                              (t (list im-ai-service im-ai-model)))))
       (list
        prompt
        :model model
        :service service
        :system system
        :empty empty))))
  (when (functionp prompt)
    (setq prompt (funcall prompt))
    (deactivate-mark))
  (when empty
    (setq prompt (im-region-or "")))
  (when-let* ((region (im-region-or nil)))
    (cond
     ((s-matches? "#{\\(code\\|region\\)" prompt)
      (setq prompt (s-replace "#{region}" region prompt))
      (setq prompt (s-replace "#{code}" (format "\n```\n%s\n```\n" region) prompt)))
     ((use-region-p)
      (setq prompt (concat prompt "\n\n" region)))))
  (let ((buffer
         (if im-ai-file
             (find-file-noselect im-ai-file)
           (get-buffer-create im-ai--buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "* " (format-time-string "%Y-%m-%d %a %H:%M"))
      (unless empty
        (insert " -- " (s-truncate 50 prompt)))
      (insert "\n")
      (insert (format "#+begin_ai markdown :service \"%s\" :model \"%s\"\n" service model))
      (when system
        (insert (format "[SYS]: %s\n\n" system)))
      (insert (format "[ME]: %s\n" prompt))
      (insert "#+end_ai\n")
      (when empty
        (forward-line -2)
        (end-of-line))
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (unless org-ai-mode
        (org-ai-mode))
      (unless empty
        (org-ai-complete-block)))
    (unless (im-buffer-visible-p buffer)
      (switch-to-buffer buffer))
    buffer))

;;;; im-ai-lookup

(defvar im-ai-lookup--history nil)

(defun im-ai-lookup (prompt)
  (interactive (list (read-string "Ask AI: " nil im-ai-lookup--history)))
  (when (string= prompt "")
    (user-error "A prompt is required."))
  (let ((context (if (use-region-p)
                     (concat "\n" (s-trim (buffer-substring-no-properties (region-beginning) (region-end))) "\n")
                   "")))
    (with-current-buffer (get-buffer-create "*im-ai-lookup*")
      (erase-buffer)
      (org-mode)
      (display-buffer (current-buffer)
                      `((display-buffer-in-side-window)
                        (side . bottom)
                        (window-height . 15)))
      (insert
       (format
        "#+begin_ai markdown :service \"%s\" :model \"%s\"\n[ME]:%s%s\n#+end_ai"
        im-ai-service
        im-ai-model
        prompt
        context))
      (org-ctrl-c-ctrl-c))))

;;;; im-ai-context

(defvar im-ai--typescript-treesit-typescript-toplevel-query
  (treesit-query-compile
   'typescript
   '((program (function_declaration) @func)
     (program (export_statement (function_declaration) @func))

     (program (lexical_declaration
               (variable_declarator
                name: (_)
                value: (arrow_function))) @lexical_func)
     (program (export_statement (lexical_declaration
                                 (variable_declarator
                                  name: (_)
                                  value: (arrow_function))) @lexical_func))

     (program (class_declaration (class_body (method_definition))) @class)
     (program (export_statement (class_declaration (class_body (method_definition))) @class))

     (program (type_alias_declaration) @type)
     (program (export_statement (type_alias_declaration) @type))

     (program (interface_declaration) @interface)
     (program (export_statement (interface_declaration) @interface)))))

(defvar im-ai--treesit-typescript-class-methods-query
  (treesit-query-compile
   'typescript
   '((class_body (method_definition) @method))))

(defun im-ai--treesit-query (language query file)
  (treesit-query-capture
   (treesit-parse-string
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))
    language)
   query))

(defun im-ai--compact-text (str)
  (s-replace-regexp "[ \t\n]+" " " str))

(defun im-ai--treesit-node-header (node)
  (->>
   node
   (treesit-node-children)
   (-butlast)
   (-map #'treesit-node-text)
   (s-join " ")
   (im-ai--compact-text)))

(defun im-ai--treesit-export-if-exported (node text)
  (if-let* ((parent (treesit-node-parent node)))
      (if (string= (treesit-node-type parent) "export_statement")
          (concat "export " text)
        text)
    text))

(defun im-ai-file-context-typescript (root file)
  (let* ((language 'typescript)
         (nodes (im-ai--treesit-query
                 language
                 im-ai--typescript-treesit-typescript-toplevel-query
                 (f-join root file))))
    (list
     :file file
     :types
     (--map
      (im-ai--treesit-export-if-exported (cdr it) (im-ai--compact-text (treesit-node-text (cdr it))))
      (--filter (eq 'type (car it)) nodes))
     :interfaces
     (--map
      (im-ai--treesit-export-if-exported (cdr it) (im-ai--compact-text (treesit-node-text (cdr it))))
      (--filter (eq 'interface (car it)) nodes))
     :classes
     (--map
      (list
       :class (im-ai--treesit-export-if-exported (cdr it) (im-ai--treesit-node-header (cdr it)))
       :methods (--map (im-ai--treesit-node-header (cdr it))
                       (treesit-query-capture
                        (cdr it)
                        im-ai--treesit-typescript-class-methods-query)))
      (--filter (eq 'class (car it)) nodes))
     :functions
     (append
      (--map
       (im-ai--treesit-export-if-exported (cdr it) (im-ai--treesit-node-header (cdr it)))
       (--filter (eq 'func (car it)) nodes))
      (--map
       (let* ((node (cdr it))
              (variable-declarator (treesit-node-child node 0 t)))
         (concat
          (im-ai--treesit-export-if-exported node (treesit-node-text (treesit-node-child node 0)))
          " "
          (treesit-node-text (treesit-node-child-by-field-name variable-declarator "name"))
          (when-let* ((type (treesit-node-text (treesit-node-child-by-field-name variable-declarator "type"))))
            (concat ": " type))
          " = "
          (im-ai--treesit-node-header (treesit-node-child-by-field-name variable-declarator "value"))
          " ..."))
       (--filter (eq 'lexical_func (car it)) nodes))))))

(defun im-ai-file-context-elisp (root file)
  (with-temp-buffer
    (let* ((fname (im-tap (f-join root file))) ; required to make `set-auto-mode' work
           (buffer-file-name fname))
      (insert-file-contents fname)
      (delay-mode-hooks (set-auto-mode))
      (imenu--cleanup)
      (->>
       (imenu--make-index-alist)
       (--filter (listp (cdr it)))
       (--map (list
               (intern (concat ":" (downcase (car it))))
               (-map #'car (cdr it))))
       (append `(:file ,file))
       (-flatten-n 1)))))

(defun im-ai-file-context (root file)
  (let* ((mode-name (symbol-name (assoc-default file auto-mode-alist 'string-match)))
         (language (intern (s-chop-suffixes '("-ts-mode" "-mode") mode-name))))
    (pcase language
      ('typescript (im-ai-file-context-typescript root file))
      ;; The rest is handled by imenu elements
      (_ (im-ai-file-context-elisp root file)))))

(defconst im-ai--code-file-extensions
  '("*.ts" "*.tsx" "*.js" "*.jsx" "*.py" "*.rb" "*.java" "*.c" "*.cpp" "*.go"
    "*.rs" "*.html" "*.css" "*.lua" "*.php" "*.json" "*.swift" "*.kotlin" "*.kt"
    "*.el"))

(defun im-ai-workspace-context ()
  "Generate context for current workspace using treesit."
  (with-temp-buffer
    (let* ((repo-root (im-current-project-root))
           (result))
      (dolist (file (apply #'process-lines "git" "-C" repo-root "ls-files" "--" im-ai--code-file-extensions) result)
        (let ((context (im-ai-file-context repo-root file)))
          (insert "./" (plist-get context :file) "\n")
          (dolist (x (plist-get context :functions))
            (insert "    - " x "\n"))
          (dolist (x (plist-get context :types))
            (insert "    - " x "\n"))
          (dolist (x (plist-get context :interfaces))
            (insert "    - " x "\n"))
          (dolist (x (plist-get context :variables))
            (insert "    - " x "\n"))
          (dolist (class (plist-get context :classes))
            (insert "    - " (plist-get class :class) "\n")
            (dolist (method (plist-get class :methods))
              (insert "        - " method "\n"))))))
    (buffer-string)))

;;;; im-ai-snippet*

(defvar im-ai--last-processed-point nil)
(add-hook 'gptel-post-stream-hook #'im-ai--cleanup-stream)
(add-hook 'gptel-post-response-functions #'im-ai--cleanup-after)

(defun im-ai-snippet (prompt)
  "Ask for a snippet with PROMPT and get it directly inside your buffer.

If you select some region and prompt, then this will change the
region but if you refer the region as @region, instead of
changing the region, it will add the answer below the region.

Use @file to include full file contents to the prompt and use
@workspace to include all workspace symbols to the prompt."
  (interactive "sQuestion (Use @region, @file, @workspace): ")
  (let* ((gptel-org-convert-response nil)
         (edit-region? (and (use-region-p)
                            (not (s-matches? (rx (or bos space)
                                                 "@region"
                                                 (or space "," eos))
                                             prompt))))
         (region (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   ""))
         (gptel-backend (im-ai--get-gptel-backend im-ai-service))
         (gptel-model im-ai-model))
    (if edit-region?
        (progn
          (setq
           im-ai--before-overlay
           (im-ai--draw-snippet-overlay (region-beginning) (region-end) 'im-ai-before-face))
          (goto-char (region-end))
          (deactivate-mark)
          (insert "\n")
          (backward-char))
      (when (use-region-p)
        (let ((end (region-end)))
          (deactivate-mark)
          (goto-char end))))
    (setq im-ai--last-processed-point (point))
    ;; TODO: Move this expansion features to other functions too
    (gptel-request
        (s-trim
         (format
          "Language: %s
Query: %s
%s
%s
%s"
          (im-ai--get-current-language)
          (s-replace-all `(("@file" . "")
                           ("@workspace" . "")
                           ("@region" . ,(concat "\n```\n" region "\n```\n")))
                         prompt)
          (if edit-region? (concat "Context: \n```\n" region "\n```") "")
          (if (s-matches? (rx (or bos space) "@file" (or space eos)) prompt)
              (concat "Full file contents: \n```\n"
                      (save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max)))
                      "\n```")
            "")
          (if (s-matches? (rx (or bos space) "@workspace" (or space eos)) prompt)
              (concat "Workspace contents: \n```\n"  (im-ai-workspace-context) "\n```")
            "")))
      :stream t
      :system im-ai-snippet-sys-prompt
      :fsm (gptel-make-fsm :handlers gptel-send--handlers))))

(cl-defun im-ai--cleanup-stream ()
  (when gptel-mode
    (cl-return-from im-ai--cleanup-stream))
  (save-excursion
    (when (> (- (line-number-at-pos (point)) (line-number-at-pos im-ai--last-processed-point)) 2)
      (let ((contents
             (replace-regexp-in-string
              "^[\n ]*```.*[\n ]*$" "" (buffer-substring-no-properties im-ai--last-processed-point (point)))))
        (delete-region im-ai--last-processed-point (point))
        (goto-char im-ai--last-processed-point)
        (insert contents)
        ;; (indent-region im-ai--last-processed-point (point))
        (setq im-ai--last-processed-point (point))))))

(cl-defun im-ai--cleanup-after (beg end)
  (when gptel-mode
    (cl-return-from im-ai--cleanup-after))
  (when (and beg end)
    (save-excursion
      (let ((contents
             (replace-regexp-in-string
              "^[\n ]*```.*[\n ]*$" ""
              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (goto-char beg)
        (insert contents))
      ;; Indent the code to match the buffer indentation if it's messed up.
      (indent-region beg end)
      (pulse-momentary-highlight-region beg (point))
      (setq im-ai--after-overlay
            (im-ai--draw-snippet-overlay beg (1+ (line-end-position)) 'im-ai-after-face)))))

(defun im-ai--accept ()
  (interactive)
  (ignore-errors
    (delete-region
     (overlay-start im-ai--before-overlay)
     (overlay-end im-ai--before-overlay)))
  (remove-overlays (point-min) (point-max) 'im-ai t))

(defun im-ai--reject ()
  (interactive)
  (delete-region
   (overlay-start im-ai--after-overlay)
   (overlay-end im-ai--after-overlay))
  (remove-overlays (point-min) (point-max) 'im-ai t))

(defvar-keymap im-ai-snippet-rewrite-map
  :doc "Keymap for ai rewrite actions at point."
  "C-c C-c" #'im-ai--accept
  "C-c C-k" #'im-ai--reject)

(defun im-ai--draw-snippet-overlay (beg end face)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'keymap im-ai-snippet-rewrite-map)
    (overlay-put ov 'im-ai t)
    (overlay-put ov 'help-echo (format "accept: \\[im-ai--accept], reject: \\[im-ai--reject]"))
    ov))

;;;; org-ai extensions

(defun im-ai-remove-answers (&optional beginning)
  (interactive)
  (save-excursion
    (let ((start (progn
                   (goto-char (or beginning (point-min)))
                   (re-search-forward "^\\[AI_?\\w*\\]: ")
                   (beginning-of-line)
                   (point)))
          (end (progn
                 (or
                  (when (re-search-forward "^#\\+end_ai" nil t)
                    (beginning-of-line)
                    (point))
                  (point-max)))))
      (when (and start end)
        (delete-region start end)))))

(defun im-ai-toggle-gpt-model-on-ai-block ()
  "Toggle GPT model of current org-ai block.
Also removes the answers, if user wants it."
  (interactive)
  (save-excursion
    (when (re-search-backward
           "#\\+begin_ai markdown :service \"\\([a-zA-Z0-9_-]+\\)\" :model \"\\([a-zA-Z0-9_\\.-]+\\)\""
           nil t)
      (-let* ((match-start (match-beginning 0))
              (match-end (match-end 0))
              (current-model (match-string 1))
              (current-service (match-string 2))
              ((service model) (im-ai--select-model)))
        (replace-match
         (format "#+begin_ai markdown :service \"%s\" :model \"%s\"" service model)
         nil nil)
        (when (y-or-n-p "Want to remove AI answers?")
          (im-ai-remove-answers match-start))))))

;;;; Interactive utils

(defun im-ai-switch-model ()
  "Switch to another model, changes default model for `org-ai' and `gptel' too."
  (interactive)
  (-let (((service model) (im-ai--select-model)))
    (setq im-ai-service service)
    (setq im-ai-model model)
    ;; GPTEL
    (setq gptel-backend (im-ai--get-gptel-backend service))
    (setq gptel-model (intern model))
    ;; ORG-AI
    (setq org-ai-default-chat-model model)
    (message ">> Model is set to %s" model)))

(defun im-ai-next-block ()
  "Move cursor to the next prompt/response."
  (interactive)
  (unless (re-search-forward im-ai--block-start-regexp nil t)
    (message ">> No next block.")))

(defun im-ai-previous-block ()
  "Move cursor to the next prompt/response."
  (interactive)
  (unless (re-search-backward im-ai--block-start-regexp nil t)
    (message ">> No previous block.")))

;;;; Utils

(defun im-ai--get-current-language ()
  "Get the current programming language of the buffer.
This is context aware in `org-mode' buffers, takes src blocks into
consideration."
  (->>
   (im-major-mode-at-point)
   (s-chop-suffix "-mode")
   (s-chop-suffix "-ts")
   (s-replace-all '(("interaction" . "")))))

(defun im-ai--get-gptel-backend (backend-name)
  (alist-get
   (s-replace-all '(("openai" . "chatgpt"))
                  (s-downcase backend-name))
   gptel--known-backends nil nil
   (lambda (x y) (equal (s-downcase x) (s-downcase y)))))

(defun im-ai--select-model ()
  (thread-last
    (completing-read (format "Select a model (%s:%s): " im-ai-service im-ai-model) im-ai-models)
    (s-split ":")))

;;;; GPTEL

;;;;; DWIM

(defun im-ai-gptel-dwim (&optional new?)
  "Open and switch to a GPTEL buffer for the current project.
- If a buffer already exists, switch to it.
- With a prefix argument, create and switch to a new GPTEL buffer.
- If multiple buffers exist for the current project, prompt the user to select one.
- If some text is selected, put that into the buffer that we are going to switch into."
  (interactive "P")
  (if (and (bound-and-true-p gptel-mode) (not new?))
      (gptel-menu)
    (let* ((proj (expand-file-name (im-current-project-root)))
           (buff (format "*gptel: %s*" (im-current-project-name)))
           (existing (--filter (s-prefix? buff (buffer-name it)) (buffer-list)))
           (final-buffer (if (and (length> existing 1) (not new?))
                             (read-buffer "Switch to: " nil nil
                                          (lambda (b)
                                            (and-let* ((buf (get-buffer (or (car-safe b) b))))
                                              (s-prefix? buff (buffer-name buf)))))
                           (when (and (length= existing 1) (not new?))
                             (setq buff (car existing)))
                           (when new?
                             (setq buff (generate-new-buffer-name buff)))
                           (or (get-buffer buff)
                               (gptel buff))
                           buff)))
      (when-let* ((region (im-region-or nil)))
        (let ((lang (im-ai--get-current-language)))
          (with-current-buffer final-buffer
            (goto-char (point-max))
            (insert "\n#+begin_src " lang "\n" region "\n#+end_src"))))
      (switch-to-buffer final-buffer))))

(defun im-ai-gptel-toggle-side-buffer (&optional new?)
  "Same as `im-ai-gptel-dwim' but toggle buffer in side buffer."
  (interactive)
  (im-toggle-side-buffer-with-name
   (save-window-excursion
     (im-ai-gptel-dwim new?)
     (buffer-name))))


;;;;; Higlighting prompts

(define-advice gptel-mode (:after (&rest _) highlight-prompts)
  "Highlight prompt prefixes."
  (if gptel-mode
      (font-lock-add-keywords nil '(("^\\[ME\\]: " . font-lock-warning-face)
                                    ("^\\[AI\\]: " . font-lock-function-name-face)) t)
    (font-lock-remove-keywords nil '(("^\\[ME\\]: " . font-lock-warning-face)
                                     ("^\\[AI\\]: " . font-lock-function-name-face)))))

;;;;; Recomputing bounds before sending the request

(define-advice gptel-send (:before (&rest args) purge-bounds)
  "Re-compute bounds before sending the query.
This gets rid of bunch of problems, at the expense of some speed.
Tracking of answers through text properties does not align with my
mental model of the whole interaction where I frequently edit both mine
and AI's answers.  Thus, I simply recalculate the all bounds in the
buffer."
  (when (and gptel-mode (null (car args)))
    (im-ai--gptel-purge-bounds)))

;; Source: https://github.com/karthink/gptel/discussions/321#discussioncomment-12878768

(defun im-ai--gptel-recompute-bounds ()
  (beginning-of-buffer)
  (let ((ai-f
         (lambda () (ignore-errors
                 (list
                  (progn
                    (search-forward (gptel-response-prefix-string))
                    (point))
                  (-
                   (or
                    (ignore-errors
                      (progn
                        (search-forward (gptel-prompt-prefix-string))
                        (goto-char (- (match-beginning 0) 1))))
                    (point-max))
                   1)))))
        (tally nil)
        (ai-bound nil))
    (while (setq ai-bound (funcall ai-f))
      (when ai-bound
        (push ai-bound tally)))

    (when tally
      (concat
       "((response "
       (string-join
        (-map (apply-partially #'format "%s")
              (reverse tally))
        " ")
       "))"))))

(defun im-ai--gptel-update-bounds (&rest _)
  (save-excursion
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (org-at-heading-p)
       (org-open-line 1)))

    (when-let ((bounds (im-ai--gptel-recompute-bounds)))
      (beginning-of-buffer)
      (org-set-property "GPTEL_BOUNDS" bounds)

      ;; Changing the bounds might change all positions in the
      ;; buffer. Redo them if they differ after the change
      (unless (string= bounds (im-ai--gptel-recompute-bounds))
        (im-ai--gptel-update-bounds)))))

(defun im-ai--gptel-purge-bounds ()
  (gptel-mode -1)
  (im-ai--gptel-update-bounds)
  (gptel-mode 1))


;;;;; Tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "write_file"
   :function (lambda (contents fname)
               (message "gptel :: write_file(%s)" fname)
               (if (or (s-blank? contents) (s-blank? fname))
                   "Operation failed: contents and/or file_path cant be empty."
                 (let ((default-directory (im-current-project-root)))
                   (write-region contents nil fname)
                   (when-let* ((buff (find-buffer-visiting fname)))

                     (with-current-buffer buff
                       (revert-buffer)))
                   "File written successfully.")))
   :description "Write given text to file."
   :args '((:name "contents"
            :type string
            :description "Text contents to write into the file")
           (:name "file_path"
            :type string
            :description "Path to the file. Path is relative to the current project's root."))
   :category "files")

  (gptel-make-tool
   :name "read_file"
   :function (lambda (filepath)
               (message "gptel :: read_file(%s)" filepath)
               (if (s-blank? filepath)
                   "Operation failed: file_path can't be empty."
                 (let ((default-directory (im-current-project-root)))
                   (with-temp-buffer
                     (insert-file-contents filepath)
                     (buffer-string)))))
   :description "Return the contents of the file."
   :args '((:name "file_path"
            :type string
            :description "Path to the file. Path is relative to the current project's root."))
   :category "files")

  (gptel-make-tool
   :name "list_project_files"
   :function (lambda ()
               (message "gptel :: list_project_files()")
               (->>
                (im-directory-files-recursively (im-current-project-root))
                (s-join "\n")))
   :description "List all file names in the current project, relative to the project dir."
   :category "files")

  (gptel-make-tool
   :name "grep_project"
   :function (lambda (callback regexp)
               (message "gptel :: grep_project(%s)" regexp)
               (if (s-blank? regexp)
                   "Operation failed: regexp can't be blank."
                 (let ((default-directory (im-current-project-root))
                       (buf (generate-new-buffer "*gptel-grep*")))
                   (make-process
                    :name "*gptel-grep*"
                    :buffer buf
                    :command `("rg" "--vimgrep" ,regexp)
                    :sentinel (lambda (proc _)
                                (when (eq (process-status proc) 'exit)
                                  (with-current-buffer buf
                                    (funcall callback (buffer-string)))
                                  (kill-buffer buf)))))))
   :description "Run grep inside the project."
   :async t
   :args '((:name "regexp"
            :type string
            :description "Regexp to search inside the project."))
   :category "files")

  (gptel-make-tool
   :name "get_webpage_contents"
   :function (lambda (callback url)
               (message "gptel :: get_webpage_contents(%s)" filepath)
               (require 'async)
               ;; Instead of using async fetch, I use async-start to
               ;; offload the parsing of the webpage because it may
               ;; also take some noticeable time and block Emacs.
               (async-start
                (lambda ()
                  (require 'shr)
                  (require 'url-handlers)
                  (with-temp-buffer
                    (url-insert-file-contents url)
                    (shr-insert-document
                     (prog1
                         (libxml-parse-html-region (point-min) (point-max))
                       (erase-buffer)))
                    (buffer-substring-no-properties (point-min) (point-max))))
                (lambda (result) (funcall callback result))))
   :async t
   :description "Return the contents of a webpage."
   :args '((:name "url"
            :type string
            :description "URL of the webpage to fetch contents from."))
   :category "web"))


;;;; Footer

(provide 'im-ai)

;;; im-ai.el ends here
