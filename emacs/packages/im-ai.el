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

(defcustom im-ai-model "gpt-4o"
  "AI model name."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-service "openai"
  "AI service."
  :type 'string
  :group 'im-ai)
(defcustom im-ai-models '("openai:gpt-4.1"
                          "openai:gpt-4o"
                          "openai:gpt-4o-mini"
                          "deepseek:deepseek-chat"
                          "deepseek:deepseek-reasoner")
  "AI service:model list."
  :type 'list
  :group 'im-ai)

(defcustom im-ai-snippet-sys-prompt
  "You are a code generation assistant focused on producing accurate, efficient solutions using language idiomatic patterns. Follow these rules:

1. Prioritize standard libraries/functions over custom implementations
2. Include minimal essential code (no comments/boilerplate unless critical)
3. Validate edge cases mentioned in the query
4. For context-dependent queries, assume common conventions
5. If multiple approaches exist, choose the most idiomatic one for the language

The input format is:

```
Language: <Programming language>
Query: <The user query, user may refer to the Context.>
Context: <Optional context, if applicable.>
Full file contents: <Optional file contents for the currently worked on file, if applicable.>
Workspace contents: <Optional list of all workspace symbols, if applicable.>
```

Format responses as:

```
<Reasoning as comment line>
<solution code>
```

Example request:

```
Language: Python
Query: Merge two lists alternately
```

Example response:

```python
# Using zip for pairwise iteration, chain for flattening.
from itertools import chain
list(chain.from_iterable(zip(list1, list2)))
```

Now handle new requests following the example format exactly.
"
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

;; From gptel-rewrite-highlight-face
(defface im-ai-before-face
  '((((class color) (min-colors 88) (background dark))
     :background "#FF4500" :extend t) ; vivid red-orange
    (((class color) (min-colors 88) (background light))
     :background "#FFD700" :extend t) ; bright gold
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai)

(defface im-ai-after-face
  '((((class color) (min-colors 88) (background dark))
     :background "#00CED1" :extend t) ; bright turquoise
    (((class color) (min-colors 88) (background light))
     :background "#32CD32" :extend t) ; vibrant lime green
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

(defun im-ai-file-context (root file)
  (let* ((mode-name (symbol-name (assoc-default file auto-mode-alist 'string-match)))
         (ts-mode? (s-suffix? "-ts-mode" mode-name))
         (language (intern (s-chop-suffixes '("-ts-mode" "-mode") mode-name))))
    (if (not ts-mode?)
        (list :file file)
      (let ((nodes (im-ai--treesit-query
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
           (--filter (eq 'lexical_func (car it)) nodes))))))))

(defconst im-ai--code-file-extensions
  '("*.ts" "*.tsx" "*.js" "*.jsx" "*.py" "*.rb" "*.java" "*.c" "*.cpp" "*.go"
    "*.rs" "*.html" "*.css" "*.lua" "*.php" "*.json" "*.swift" "*.kotlin" "*.kt"
    "*.el"))

;; TODO: Only supports typescript file for now.
(defun im-ai-workspace-context ()
  "Generate context for current workspace using treesit."
  (with-temp-buffer
    (let* ((repo-root (im-current-project-root))
           (result))
      (dolist (file (apply #'process-lines "git" "-C" repo-root "ls-files" "--" im-ai--code-file-extensions) result)
        (let ((context (im-ai-file-context repo-root file)))
          (insert "./" (plist-get context :file) "\n")
          (dolist (function (plist-get context :functions))
            (insert "    - " function "\n"))
          (dolist (type (plist-get context :types))
            (insert "    - " type "\n"))
          (dolist (interface (plist-get context :interfaces))
            (insert "    - " interface "\n"))
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
                            (not (s-matches? (rx (or bos space) "@region" (or space eos)) prompt))))
         (region (buffer-substring-no-properties (region-beginning) (region-end)))
         (gptel-backend (im-ai--get-gptel-backend im-ai-service))
         (gptel-model im-ai-model))
    (if edit-region?
        (progn
          (message "OVE YEA")
          (setq
           im-ai--before-overlay
           (im-ai--draw-snippet-overlay (region-beginning) (region-end) 'im-ai-before-face))
          (goto-char (region-end))
          (deactivate-mark)
          (insert "\n")
          (backward-char))
      (let ((end (region-end)))
        (deactivate-mark)
        (goto-char end)))
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
              (concat "Full file contents: \n```\n" (region (buffer-substring-no-properties (point-min) (point-max)))  "\n```")
            "")
          (if (s-matches? (rx (or bos space) "@workspace" (or space eos)) prompt)
              (concat "Workspace contents: \n```\n"  (im-ai-workspace-context) "\n```")
            "")))
      :stream t
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

(defun im-ai-toggle-gpt-model-on-ai-block ()
  "Toggle GPT model of current org-ai block.
Also removes the answers, if user wants it."
  (interactive)
  (save-excursion
    (when (re-search-backward
           "#\\+begin_ai markdown :service \"\\([a-zA-Z0-9_-]+\\)\" :model \"\\([a-zA-Z0-9_-]+\\)\""
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
          (let ((start (progn
                         (goto-char match-start)
                         (re-search-forward "^\\[AI_?\\w*\\]: ")
                         (beginning-of-line)
                         (point)))
                (end (progn
                       (re-search-forward "^#\\+end_ai")
                       (beginning-of-line)
                       (point))))
            (when (and start end)
              (delete-region start end))))))))

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
   (s-replace-all '(("-" . " ")
                    ("interaction" . "")))
   (s-titleize)))

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

;;;; Footer

(provide 'im-ai)

;;; im-ai.el ends here
