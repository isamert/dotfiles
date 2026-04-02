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

;; My AI extensions.  Mostly uses `gptel' to do the heavy-lifting.

;;   `im-ai-prompts') that you can use (by standalone, or on a region
;;   etc.) and get results in a predefined org buffer (uses `org-ai').
;; - `im-ai-snippet' simply generates a snippet you requested in the
;;   language of the current buffer.
;; - `im-ai-switch-model' switches default model for all functions.
;; - `im-ai-{next,previous}-block' goes to next/prev prompt/ai answer.

;;; Code:

(require 's)
(require 'gptel)

;;;; Customization

(defgroup im-ai nil
  "Settings for `im-ai'."
  :group 'utils)

(defcustom im-ai-snippet-sys-prompt
  "You are a code generation assistant. Your output will be inserted directly into a live editor buffer WITHOUT ANY POST-PROCESSING.

RULES:
1. Return ONLY raw code—no markdown fences, no explanations, no comments unless requested.
2. Prefer standard library/built-ins over custom implementations.
3. Use idiomatic patterns for the target language.
4. Match the style/conventions visible in provided context (indentation, naming, etc.).
5. Generate minimal code that fulfills the request—nothing extraneous.
6. When <surrounding_context> is provided, generate code that replaces <GENERATE_HERE> seamlessly.
7. Assume code will execute immediately in context; avoid wrappers unless required.

REQUEST FORMAT:
<language>Programming Language</language>
<file_name>The file name you are currently working on. Your result will be in this file.</file_name>
<user_query>The specific instruction or requirement from the user.</user_query>
<context>The general context that you need to work on. Optionally provided.</context>
<full_file_contents>Full file context. Optionally provided.</full_file_contents>
<surrounding_context>The code immediately surrounding the target location. You must output ONLY the code that perfectly replaces the <GENERATE_HERE> placeholder. Optionally provided.</surrounding_context>
<workspace_contents>All workspace items, symbols etc. Optionally provided.</workspace_contents>"
  "System prompt used in `im-ai-snippet'."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-snippet-dumb-prompt
  "You are an helpful assistant. You will get a requirement from user and handle that as simple as possible.

**Request Format:**

```
<user_query>
The thing that user wants you to do/provide.
</user_query>

<context>
The context that you need to work on. Optionally provided.
</context>

<full_file_contents>
Full file context. Optionally provided.
</full_file_contents>

<workspace_contents>
All workspace items, symbols etc. Optionally provided.
</workspace_contents>
```

ONLY output your answer to the query, with no explanations."
  "System prompt used in `im-ai-snippet'."
  :type 'string
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

(defvar im-ai-programming-agent-prompt "You are a an AI coding assistant. You operate in Emacs. Use the instructions below and the tools available to you to assist the user.

# Tone and style
You should be concise, direct, and to the point, while providing complete information and matching the level of detail you provide in your response with the level of complexity of the user's query or the work you have completed.
IMPORTANT: You should minimize output tokens as much as possible while maintaining helpfulness, quality, and accuracy. Only address the specific query or task at hand, avoiding tangential information unless absolutely critical for completing the request. If you can answer in 1-3 sentences or a short paragraph, please do.
IMPORTANT: You should NOT answer with unnecessary preamble or postamble (such as explaining your code or summarizing your action), unless the user asks you to.
Do not add additional code explanation summary unless requested by the user. After working on a file, just stop, rather than providing an explanation of what you did.
Answer the user's question directly, without elaboration, explanation, or details. One word answers are best. Avoid introductions, conclusions, and explanations. You MUST avoid text before/after your response, such as \"The answer is <answer>.\", \"Here is the content of the file...\" or \"Based on the information provided, the answer is...\" or \"Here is what I will do next...\". Here are some examples to demonstrate appropriate verbosity:

<example>
user: 2 + 2
assistant: 4
</example>

<example>
user: what is 2+2?
assistant: 4
</example>

<example>
user: is 11 a prime number?
assistant: Yes
</example>

<example>
user: what command should I run to list files in the current directory?
assistant: ls
</example>

<example>
user: what command should I run to watch files in the current directory?
assistant: [use the available tools to list the files in the current directory, then read docs/commands in the relevant file to find out how to watch files]
npm run dev
</example>

<example>
user: what files are in the directory src/?
assistant: [runs ls and sees foo.c, bar.c, baz.c]
user: which file contains the implementation of foo?
assistant: src/foo.c
</example>

Remember that your output will be displayed on a command line interface. Output text to communicate with the user; all text you output outside of tool use is displayed to the user.
Only use tools to complete tasks. Never use tools like Bash or code comments as means to communicate with the user during the session.
IMPORTANT: Keep your responses short, since they will be displayed on a command line interface.

# Proactiveness
You are allowed to be proactive, but only when the user asks you to do something. You should strive to strike a balance between:
- Doing the right thing when asked, including taking actions and follow-up actions
- Not surprising the user with actions you take without asking
For example, if the user asks you how to approach something, you should do your best to answer their question first, and not immediately jump into taking actions.

# Following conventions
When making changes to files, first understand the file's code conventions. Mimic code style, use existing libraries and utilities, and follow existing patterns.
- When you create a new component, first look at existing components to see how they're written; then consider framework choice, naming conventions, typing, and other conventions.
- When you edit a piece of code, first look at the code's surrounding context, imports etc. to understand the code's choice of frameworks and libraries. Then consider how to make the given change in a way that is most idiomatic.
- IMPORTANT: DO NOT ADD ***ANY*** COMMENTS unless asked

# Tool calling
You have tools at your disposal to solve the coding task. Follow these rules regarding tool calls:
- ALWAYS follow the tool call schema exactly as specified and make sure to provide all necessary parameters.
- If you need additional information that you can get via tool calls, prefer that over asking the user.
- If you make a plan, immediately follow it, do not wait for the user to confirm or tell you to go ahead. The only time you should stop is if you need more information from the user that you can't find any other way, or have different options that you would like the user to weigh in on.
- If you are not sure about file content or codebase structure pertaining to the user's request, use your tools to read files and gather the relevant information: do NOT guess or make up an answer.
- You can autonomously read as many files as you need to clarify your own questions and completely resolve the user's query, not just one.

# Searcing and reading
If you are unsure about the answer to the USER's request or how to satiate their request, you should gather more information. This can be done with additional tool calls, asking clarifying questions, etc...
For example, if you've performed a semantic search, and the results may not fully answer the USER's request, or merit gathering more information, feel free to call more tools.
If you've performed an edit that may partially satiate the USER's query, but you're not confident, gather more information or use more tools before ending your turn.
Bias towards not asking the user for help if you can find the answer yourself.

# Task Management
You have access to the todo_write tool to help you manage and plan tasks. Use these tool VERY frequently to ensure that you are tracking your tasks and giving the user visibility into your progress. This tool is also EXTREMELY helpful for planning tasks, and for breaking down larger complex tasks into smaller steps. If you do not use this tool when planning, you may forget to do important tasks - and that is unacceptable.
It is critical that you mark todos as completed as soon as you are done with a task. Do not batch up multiple tasks before marking them as completed.

Examples:

<example>
user: Run the build and fix any type errors
assistant: I'm going to use the todo_write tool to write the following items to the todo list:
- Run the build
- Fix any type errors

I'm now going to run the build using Bash.

Looks like I found 10 type errors. I'm going to use the TodoWrite tool to write 10 items to the todo list.

marking the first todo as in_progress

Let me start working on the first item...

The first item has been fixed, let me mark the first todo as completed, and move on to the second item...
..
..
</example>

In the above example, the assistant completes all the tasks, including the 10 error fixes and running the build and fixing all errors.

<example>
user: Help me write a new feature that allows users to track their usage metrics and export them to various formats

assistant: I'll help you implement a usage metrics tracking and export feature. Let me first use the TodoWrite tool to plan this task.
Adding the following todos to the todo list:
1. Research existing metrics tracking in the codebase
2. Design the metrics collection system
3. Implement core metrics tracking functionality
4. Create export functionality for different formats

Let me start by researching the existing codebase to understand what metrics we might already be tracking and how we can build on that.

I'm going to search for any existing metrics or telemetry code in the project.

I've found some existing telemetry code. Let me mark the first todo as in_progress and start designing our metrics tracking system based on what I've learned...

[Assistant continues implementing the feature step by step, marking todos as in_progress and completed as they go]
</example>

IMPORTANT: Always use the TodoWrite tool to plan and track tasks throughout the conversation.")

(defvar im-ai-research-prompt
  "You are a researcher tasked with providing accurate, trustworthy, and easily auditable answers to queries. Follow these principles and workflow:

*1. Official Documentation First*
- Always check the official documentation before any other sources.
- Cite, quote, and reference from the official docs wherever possible.

*2. Reputable Supplementary Sites*
- If the official docs are unclear or incomplete, consult authoritative community sites like Stack Overflow and official author or project blogs—excluding Medium unless there’s no alternative.
- Never rely on non-authoritative, user-generated content as your primary source.

*3. Direct Searches for Straightforward Questions*
- For simple, well-known questions, perform targeted searches to retrieve reliable, direct answers from primary or highly reputable secondary sources.

*4. Project Site/Source Navigation for Complex Queries*
- If steps 1–3 are insufficient, emulate a skilled developer:
    - Search for the project’s official website, GitHub/GitLab (or equivalent forge) repository, and locate the documentation, source files, or project references.
    - Detail how to find relevant information, including repository structure and navigation steps.

*5. Raw Source Links for Code Forges*
- When referencing files or source code from forges like GitHub or GitLab:
    - *ALWAYS prefer linking to the raw file version over the UI version* (e.g., use `raw.githubusercontent.com` for GitHub instead of the standard UI link). Even for the READMEs. Especially if you get access restrictions etc. try this.
    - If the raw link is not readily visible, construct it by transforming the UI link (e.g., for GitHub, replace `github.com/user/repo/blob/branch/file` with `raw.githubusercontent.com/user/repo/branch/file`).
    - This ensures minimal noise and a direct view of the content.
    - Explain or show how you constructed the raw link if it was not immediately available.

*General Principles:*
- Prioritize primary and clean sources of truth.
- Supplement only with community contributions that follow official recommendations.
- Make the navigation process auditable and transparent, describing steps taken.")

;;;; Variables

(defconst im-ai--block-start-regexp "^\\[\\(ME\\|AI\\|AI_REASON\\)\\]:")

;;;; gptel utils

(defun im-ai--format-model-name (&optional backend model)
  (format "%s:%s" (gptel-backend-name (or backend gptel-backend)) (or model gptel-model)))

(defun im-ai--gptel-all-models ()
  "Return all models as string in Backend:Model-Name format."
  (cl-loop
   for (name . backend) in gptel--known-backends
   nconc (cl-loop for model in (gptel-backend-models backend)
                  collect (list (concat name ":" (gptel--model-name model))
                                backend model))
   into models-alist finally return models-alist))

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
    (let* ((fname (f-join root file)) ; required to make `set-auto-mode' work
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

(defvar-local im-ai--last-processed-point nil)
(defvar-local im-ai--history nil)
(defvar-local im-ai--agentic? nil)
(defvar-local im-ai-reenable-aggressive-indent nil
  "Aggressive indent may fuck things up while the AI is streaming.")
(defvar-local im-ai-disable-help-at-pt-mode nil)
(add-hook 'gptel-post-stream-hook #'im-ai--cleanup-stream)
(add-hook 'gptel-post-response-functions #'im-ai--cleanup-after)

;; im-cape-ai-commands
(with-eval-after-load 'gptel
  (let ((ai-commands `(("@dumb" . "Use a simpler sys prompt, without much context.")
                       ("@region" . "Refer to region, but don't change it")
                       ("@file" . "Add file context (i.e. imenu items)")
                       ("@noexp" . "Add instruction to remove any explanations")
                       ("@fullfile" . "Add full file as context. Implies @context")
                       ("@agent" . "Can operate on full file, implies @fullfile.")
                       ("@workspace" . "Add workspace context (i.e. imenu items)")
                       ("@context" . "Add lines around point as context")
                       ,@(--map (cons (format "@model=%s" (car it)) "Switch model") (im-ai--gptel-all-models)))))
    (im-cape
     :name ai-commands
     :completion ai-commands
     :annotate (lambda (obj item)
                 (concat " " (alist-get item ai-commands nil nil #'equal)))
     :extractor (lambda (it) (mapcar #'car it))
     :bound filename
     :kind (lambda (xs x) "" 'module)
     :category symbol)))

(defun im-ai-snippet (prompt &optional history)
  "Ask for a snippet with PROMPT and get it directly inside your buffer.

If you select some region and prompt, then this will change the
region but if you refer the region as @region, instead of
changing the region, it will add the answer below the region.

Use @file to include full file contents to the prompt and use
@workspace to include all workspace symbols to the prompt."
  (interactive (list
      (minibuffer-with-setup-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-prefix-length #'im-cape-ai-commands 1))))
        (read-string
         (format "Question (current model: %s): " (im-ai--format-model-name))))))
  (-let* (;; capabilities
          (dumb? (s-matches? (rx (or bos space) "@dumb" (or space eos)) prompt))
          (agentic? (s-matches? (rx (or bos space) "@agent" (or space eos)) prompt))
          ;; region
          (region (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    ""))
          (rbegin (if (use-region-p)
                      (region-beginning)
                    (point)))
          (rend (if (use-region-p)
                    (region-end)
                  (point)))
          (edit-region? (and (use-region-p)
                             (not (s-matches? (rx (or bos space) "@region" (or space "," eos))
                                              prompt))))
          ;; prompt overrides
          ((backend model) (-when-let ((_ backend model) (s-match "@model=\\([^: ]+\\):\\([^ ]+\\)" prompt))
                             (setq prompt (s-replace-regexp "@model=\\([^ ]+\\)" "" prompt))
                             (cdr (assoc (format "%s:%s" backend model) (im-ai--gptel-all-models)))))
          ;; gptel stuff
          (gptel-org-convert-response nil)
          (gptel-use-context nil)
          (gptel-temperature 0.0)
          (gptel-use-tools (if agentic? t nil))
          (gptel-tools (list
                        (alist-get
                         "read_buffer_lines"
                         (alist-get "buffers" gptel--known-tools nil nil #'equal) nil nil #'equal)
                        (alist-get
                         "edit_buffer"
                         (alist-get "buffers" gptel--known-tools nil nil #'equal) nil nil #'equal) ))
          (gptel-backend (or backend gptel-backend))
          (gptel-model (or model gptel-model))
          ;; prompt
          (formatted-prompt
           (s-join
            "\n"
            `(,@(when (not dumb?)
                  (seq-concatenate
                   'list
                   (list
                    "<language>"
                    (im-ai--get-current-language)
                    "</language>")
                   (when-let ((file-name (buffer-file-name)))
                     (list
                      "<file_name>"
                      (file-name-nondirectory file-name)
                      "</file_name>"))
                   (list
                    "<buffer_name>"
                    (buffer-name)
                    "</buffer_name>")
                   (if edit-region?
                       (list
                        "<current_range>"
                        (format "%d-%d"
                                (line-number-at-pos (region-beginning))
                                (line-number-at-pos (region-end)))
                        "</current_range>")
                     (list
                      "<current_line>"
                      (number-to-string (line-number-at-pos))
                      "</current_line>"))))
              ,@(when edit-region?
                  (list
                   "<context>" (s-trim region) "</context>"))
              ,@(when (s-matches? (rx (or bos space) "@fullfile" (or space eos)) prompt)
                  (list
                   "<full_file_contents>"
                   (save-restriction
                     (widen)
                     (buffer-substring-no-properties (point-min) (point-max))
                     "</full_file_contents>")))
              ;; When @agent is given, insert full file with line numbers:
              ,@(when (s-matches? (rx (or bos space) (or "@agent") (or space eos)) prompt)
                  (list
                   "<full_file_contents_with_line_numbers>"
                   (save-restriction
                     (widen)
                     (let ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
                           (line-num 1))
                       (mapconcat (lambda (line)
                                    (prog1 (format "%d: %s" line-num line)
                                      (setq line-num (1+ line-num))))
                                  lines
                                  "\n")))
                   "</full_file_contents_with_line_numbers>"))
              ,@(when (s-matches? (rx (or bos space) "@file" (or space eos)) prompt)
                  (list
                   "<file_context>"
                   (im-ai-file-context (im-current-project-root) (buffer-file-name))
                   "</file_context>"))
              ,@(when (s-matches? (rx (or bos space) "@workspace" (or space eos)) prompt)
                  (list "<workspace_contents>" (im-ai-workspace-context) "</workspace_contents>"))
              ,@(when (s-matches? (rx (or bos space) (or "@context" "@fullfile") (or space eos)) prompt)
                  ;; NOTE: Need to give context when @fullfile is present
                  (list
                   "<surrounding_context>"
                   "..."
                   (let* ((start
                           (progn
                             (insert "<GENERATE_HERE>")
                             (save-excursion (goto-char rbegin) (forward-line -10) (line-beginning-position))))
                          (end
                           (save-excursion (goto-char rend) (forward-line 10) (line-end-position))))
                     (prog1 (buffer-substring-no-properties start end)
                       (delete-region (point) (- (point) (length "<GENERATE_HERE>")))))
                   "..."
                   "</surrounding_context>"))
              "<user_query>"
              ,(s-replace-all `(("@file" . "")
                                ("@fullfile" . "")
                                ("@context" . "")
                                ("@agent" . "\nFull file context with line numbers is also included, you can depend on it for your changes. Do not output anything else.\n")
                                ("@dumb" . "")
                                ("@workspace" . "")
                                ("@noexp" . "\nDo not include any explanations, only output the solution.\n")
                                ("@region" . ,(concat "<region>" (s-trim region) "</region>")))
                              prompt)
              "</user_query>"))))
    (if (and edit-region? (not agentic?))
        (progn
          (im-ai--draw-snippet-overlay (region-beginning) (region-end) 'im-ai-before-face)
          (goto-char (region-end))
          (deactivate-mark)
          (insert "\n")
          (backward-char))
      (when (use-region-p)
        (let ((end (region-end)))
          (deactivate-mark)
          (goto-char end))))
    (setq im-ai--last-processed-point (point))
    (when (bound-and-true-p aggressive-indent-mode)
      (setq im-ai-reenable-aggressive-indent t)
      (aggressive-indent-mode -1))
    (let ((history (setq im-ai--history (if history
                                            (append history (list prompt))
                                          (list formatted-prompt))))
          (response-buffer
           (if agentic?
               (get-buffer-create (format "*im-ai-response: %s*" (buffer-name)))
             (current-buffer))))
      (im-spinner-mode)
      (when agentic?
        (with-current-buffer response-buffer
          (goto-char (point-max))
          (insert "\n\n---------------------------------------------\n\n")))
      (with-current-buffer response-buffer
        (setq im-ai--agentic? agentic?)
        (gptel-request history
          :stream t
          :system (if dumb? im-ai-snippet-dumb-prompt im-ai-snippet-sys-prompt)
          :fsm (gptel-make-fsm :handlers gptel-send--handlers)
          :buffer response-buffer)))))

(cl-defun im-ai--cleanup-stream ()
  (when (or gptel-mode im-ai--agentic?)
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
  (when im-spinner-mode
    (im-spinner-mode -1))
  (when (or gptel-mode im-ai--agentic?)
    (cl-return-from im-ai--cleanup-after))
  (when (and beg end)
    (save-excursion
      (let ((contents
             (replace-regexp-in-string
              "^[\n ]*```.*[\n ]*$" ""
              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (goto-char beg)
        (insert (s-trim contents) "\n"))
      (setq im-ai--history (append im-ai--history (list (buffer-substring-no-properties beg (point)))))
      ;; Indent the code to match the buffer indentation if it's messed up.
      (indent-region beg (point))
      (pulse-momentary-highlight-region beg (point))
      (im-ai--draw-snippet-overlay beg (1+ (line-end-position)) 'im-ai-after-face)
      (when im-ai-reenable-aggressive-indent
        (aggressive-indent-mode +1)
        (setq im-ai-reenable-aggressive-indent nil))
      (unless (bound-and-true-p im-help-at-point-mode)
        (setq im-ai-disable-help-at-pt-mode t)
        (im-help-at-point-mode +1)))))

(defun im-ai--clear-overlays ()
  "Remove all im-ai overlays and disable help-at-point mode if needed."
  (remove-overlays (point-min) (point-max) 'im-ai t)
  (when im-ai-disable-help-at-pt-mode
    (im-help-at-point-mode -1)))

(defun im-ai--accept ()
  (interactive)
  (when-let ((ov (im-ai--overlay-at-point 'before)))
    (ignore-errors (delete-region (overlay-start ov) (overlay-end ov))))
  (im-ai--clear-overlays))

(defun im-ai--reject ()
  (interactive)
  (let ((ov (im-ai--overlay-at-point 'after)))
    (delete-region (overlay-start ov) (overlay-end ov)))
  (im-ai--clear-overlays) )

(defun im-ai--overlay-at-point (&optional type)
  "Return the im-ai overlay at or before point, if any.
TYPE can be `before' or `after' to find a specific overlay.
If TYPE is `after', find the overlay with face `im-ai-after-face'.
If TYPE is `before', find the overlay with face `im-ai-before-face'.
If TYPE is nil, return any im-ai overlay."
  (let ((predicate (lambda (o)
                     (and (overlay-get o 'im-ai)
                          (pcase type
                            ('after (eq (overlay-get o 'face) 'im-ai-after-face))
                            ('before (eq (overlay-get o 'face) 'im-ai-before-face))
                            (_ t))))))
    (or (seq-find predicate (overlays-at (point)))
        (let ((pos (point)) result)
          (while (and (not result) (> pos (point-min)))
            (setq pos (previous-overlay-change pos))
            (setq result (seq-find predicate (overlays-at pos))))
          result))))

(defun im-ai--iterate ()
  (interactive)
  (let ((prompt (read-string "Iterate: "))
        (ov (im-ai--overlay-at-point 'after)))
    (delete-region
     (overlay-start ov)
     (overlay-end ov))
    (im-ai--clear-overlays)
    (im-ai-snippet prompt im-ai--history)))

(defvar-keymap im-ai-snippet-rewrite-map
  :doc "Keymap for ai rewrite actions at point."
  "C-c C-c" #'im-ai--accept
  "C-c C-k" #'im-ai--reject
  "C-c C-i" #'im-ai--iterate)

(defun im-ai--draw-snippet-overlay (beg end face)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'keymap im-ai-snippet-rewrite-map)
    (overlay-put ov 'im-ai t)
    (overlay-put ov 'help-echo (format "accept: \\[im-ai--accept], reject: \\[im-ai--reject], iterate: \\[im-ai--iterate]"))
    ov))

;;;; Interactive utils

(defun im-ai-switch-model ()
  "Switch to another model, changes default model for `org-ai' and `gptel' too."
  (interactive)
  (-let* ((models-alist (im-ai--gptel-all-models))
          (completion-extra-properties
           `(:annotation-function
             ,(lambda (comp)
                (let* ((model (nth 2 (assoc comp models-alist)))
                       (desc (get model :description))
                       (caps (get model :capabilities))
                       (context (get model :context-window))
                       (input-cost (get model :input-cost))
                       (output-cost (get model :output-cost))
                       (cutoff (get model :cutoff-date)))
                  (when (or desc caps context input-cost output-cost cutoff)
                    (concat
                     (propertize " " 'display `(space :align-to 40))
                     (when desc (truncate-string-to-width desc 70 nil ? t t))
                     " " (propertize " " 'display `(space :align-to 112))
                     (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
                     " " (propertize " " 'display `(space :align-to 134))
                     (when context (format "%5dk" context))
                     " " (propertize " " 'display `(space :align-to 142))
                     (when input-cost (format "$%5.2f in" input-cost))
                     (if (and input-cost output-cost) "," " ")
                     " " (propertize " " 'display `(space :align-to 153))
                     (when output-cost (format "$%6.2f out" output-cost))
                     " " (propertize " " 'display `(space :align-to 166))
                     cutoff))))))
          ((backend model) (cdr (assoc (completing-read
                                        (format "Select model (current=%s): "
                                                (im-ai--format-model-name))
                                        models-alist nil t nil nil)
                                       models-alist))))
    (setq-default gptel-backend backend)
    (setq-default gptel-model model)
    (setq gptel-backend backend)
    (setq gptel-model model)
    (message ">> Model is set to %s" (im-ai--format-model-name backend model))))

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
    (let* ((default-directory (expand-file-name (or (im-current-project-root) default-directory)))
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
      (if (im-buffer-visible-p final-buffer)
          (select-window (get-buffer-window final-buffer))
        (switch-to-buffer final-buffer))
      (goto-char (point-max))
      (recenter))))

(defun im-ai-gptel-toggle-side-buffer (&optional new?)
  "Same as `im-ai-gptel-dwim' but toggle buffer in side buffer."
  (interactive)
  (let ((buffer (save-window-excursion
                  (im-ai-gptel-dwim new?)
                  (current-buffer))))
    (if (and (im-buffer-visible-p buffer)
             (use-region-p))
        (progn
          (im-display-buffer-in-side-window buffer)
          (goto-char (point-max))
          (recenter))
      (im-toggle-side-buffer-with-name (buffer-name buffer)))))

;;;;; Higlighting prompts

(define-advice gptel-mode (:after (&rest _) highlight-prompts)
  "Highlight prompt prefixes."
  (if gptel-mode
      (font-lock-add-keywords nil '(("^\\[ME\\]:" . font-lock-warning-face)
                                    ("^\\[AI\\]:" . font-lock-function-name-face)) t)
    (font-lock-remove-keywords nil '(("^\\[ME\\]:" . font-lock-warning-face)
                                     ("^\\[AI\\]:" . font-lock-function-name-face)))))

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
;;;;;; file/folder tools

;; taken from llm-tool-collection
(defun im-gptel--edit-tool (buffer-or-file old-string new-string)
  "Replace exactly one occurrence of OLD-STRING with NEW-STRING.
BUFFER-OR-FILE is either a buffer object or a file path string."
  (if (string= old-string "")
      "ERROR: `old_string' cannot be empty"
    (let* ((is-file? (not (bufferp buffer-or-file)))
           (name (if is-file?
                     (concat "file " buffer-or-file)
                   (concat "buffer " (buffer-name buffer-or-file))))
           (file-path (when is-file? (expand-file-name buffer-or-file)))
           (existing-buffer (when file-path (find-buffer-visiting file-path))))
      (cl-labels ((do-edit ()
                           (let ((case-fold-search nil))
                             (save-excursion
                               (goto-char (point-min))
                               (let ((count 0)
                                     (first-match-pos nil))
                                 (while (search-forward old-string nil 'noerror)
                                   (setq count (1+ count))
                                   (unless first-match-pos
                                     (setq first-match-pos (match-beginning 0))))
                                 (cond
                                  ((= count 0)
                                   (error "Could not find text '%s' to replace in %s"
                                          old-string name))
                                  ((> count 1)
                                   (error "Found %d matches for '%s' in %s, need exactly one"
                                          count old-string name))
                                  (t
                                   (goto-char first-match-pos)
                                   (search-forward old-string nil 'noerror)
                                   (replace-match new-string 'fixedcase 'literal)
                                   (format "Successfully edited %s" name))))))))
        (if (bufferp buffer-or-file)
            (with-current-buffer buffer-or-file
              (do-edit))
          (if existing-buffer
              ;; There's an existing buffer; edit in temp buffer, write file, revert existing buffer
              (let ((temp-buf (generate-new-buffer " *temp*")))
                (with-current-buffer temp-buf
                  (insert-file-contents file-path)
                  (do-edit)
                  (write-file file-path))
                (with-current-buffer existing-buffer
                  (revert-buffer t t))
                (kill-buffer temp-buf)
                (format "Successfully edited %s" name))
            ;; No existing buffer, edit in temp buffer and write file
            (let ((temp-buf (generate-new-buffer " *temp*")))
              (with-current-buffer temp-buf
                (insert-file-contents file-path)
                (do-edit)
                (write-file file-path))
              (kill-buffer temp-buf)
              (format "Successfully edited %s" name))))))))

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
   :description "Write given text to file. Replaces whole file with given text if file already exists."
   :args '((:name "contents"
            :type string
            :description "Text contents to write into the file")
           (:name "file_path"
            :type string
            :description "Path to the file. Path is relative to the current project's root."))
   :category "files_mutative")

  (gptel-make-tool
   :name "mkdir"
   :function (lambda (dir)
               (message "gptel :: mkdir(%s)" dir)
               (if (s-blank? dir)
                   "Operation failed: directory path cannot be empty."
                 (let ((default-directory (im-current-project-root))
                       (dir-path (expand-file-name dir)))
                   (if (file-directory-p dir-path)
                       "Directory already exists."
                     (make-directory dir-path t)
                     "Directory created successfully."))))
   :description "Create a directory relative to the current project's root."
   :args '((:name "directory"
            :type string
            :description "Relative path of the directory to create."))
   :category "files_mutative")

  (gptel-make-tool
   :name "read_file_lines"
   :function (lambda (filepath start-line end-line)
               (message "gptel :: read_file_lines(%s, %d, %d)" filepath start-line end-line)
               (if (or (s-blank? filepath)
                       (not (and (numberp start-line) (numberp end-line)))
                       (< start-line 1)
                       (< end-line start-line))
                   "Operation failed: invalid input."
                 (let ((default-directory (im-current-project-root)))
                   (with-temp-buffer
                     (insert-file-contents filepath)
                     (let ((start-pos (progn (goto-char (point-min)) (forward-line (1- start-line)) (point)))
                           (end-pos (progn (goto-char (point-min)) (forward-line (1- end-line)) (point))))
                       (concat
                        (format "<file_lines start_line=%s end_line=%s>\n" start-line end-line)
                        (buffer-substring-no-properties start-pos end-pos)
                        "\n</file_lines>"))))))
   :description "Return the contents of the file from start_line to end_line (inclusive)."
   :args '((:name "file_path"
            :type string
            :description "Path to the file. Path is relative to the current project's root.")
           (:name "start_line"
            :type integer
            :description "Starting line number.")
           (:name "end_line"
            :type integer
            :description "Ending line number."))
   :category "files")

  (gptel-make-tool
   :name "edit_file"
   :function
   (lambda (file old_string new_string)
     (message "gptel :: edit_file(%s)" file)
     (im-gptel--edit-tool (expand-file-name file) old_string new_string))
   :description "Edit a file by replacing exactly one occurrence of OLD_STRING with NEW_STRING in FILE. Errors if OLD_STRING is empty, not found, or found more than once."
   :args (list '(:name "file"
                 :type string
                 :description "Path to the file to edit (absolute or relative).")
               '(:name "old_string"
                 :type string
                 :description "Exact text to replace (must match exactly once), can span multiple lines.")
               '(:name "new_string"
                 :type string
                 :category "files_mutative"
                 :description "Replacement text."))))

;;;;;; buffer tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "list_buffers"
   :function (lambda ()
               (message "gptel :: list_buffers()")
               (concat
                "<buffers>\n"
                (mapconcat
                 (lambda (n) n)
                 (cl-loop for b in (buffer-list)
                          for n = (buffer-name b)
                          when (and n (not (string-prefix-p " " n)))
                          collect n)
                 "\n")
                "\n</buffers>"))
   :description "List names of open buffers. Act directly on buffers if you know the name already, without listing."
   :args '()
   :category "buffers")

  (gptel-make-tool
   :name "read_buffer_lines"
   :function (lambda (buffer-name &optional start-line end-line)
               (message "gptel :: read_buffer_lines(%s, %s, %s)" buffer-name start-line end-line)
               (if (or (s-blank? buffer-name) (not (get-buffer buffer-name)))
                   "Operation failed: invalid input."
                 (with-current-buffer buffer-name
                   (let* ((start-pos (if start-line
                                         (save-excursion
                                           (goto-char (point-min))
                                           (forward-line (1- start-line))
                                           (point))
                                       (point-min)))
                          (end-pos (if end-line
                                       (save-excursion
                                         (goto-char (point-min))
                                         (forward-line end-line)
                                         (point))
                                     (point-max)))
                          (content (buffer-substring-no-properties start-pos end-pos))
                          (lines (split-string content "\n"))
                          (limited-lines (seq-take lines 500))
                          (truncated (> (length lines) 500)))
                     (concat
                      (format "<buffer name=%S%s%s>\n"
                              buffer-name
                              (if start-line (format " start-line=%d" start-line) "")
                              (if end-line (format " end-line=%d" end-line) ""))
                      (string-join limited-lines "\n")
                      (if truncated "\n[... truncated, showing first 500 lines ...]" "")
                      "\n</buffer>")))))
   :description "Return the contents of the buffer with the given name (max 500 lines). Optionally specify a line range."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to read.")
           (:name "start_line"
            :type integer
            :optional t
            :description "Starting line number (1-indexed). Optional.")
           (:name "end_line"
            :type integer
            :optional t
            :description "Ending line number (1-indexed). Optional."))
   :category "buffers")

  (gptel-make-tool
   :name "search_buffer"
   :function (lambda (buffer-name pattern &optional regexp case-sensitive)
               (message "gptel :: search_buffer(%s, %s, %s, %s)" buffer-name pattern regexp case-sensitive)
               (if (or (s-blank? buffer-name) (not (get-buffer buffer-name)))
                   "Operation failed: invalid buffer name."
                 (if (s-blank? pattern)
                     "Operation failed: search pattern is empty."
                   (with-current-buffer buffer-name
                     (let ((case-fold-search (not case-sensitive))
                           (search-fn (if regexp #'re-search-forward #'search-forward))
                           (matches '())
                           (max-matches 50))
                       (save-excursion
                         (goto-char (point-min))
                         (while (and (< (length matches) max-matches)
                                     (funcall search-fn pattern nil t))
                           (let* ((line-num (line-number-at-pos (match-beginning 0)))
                                  (line-content (buffer-substring-no-properties
                                                 (line-beginning-position)
                                                 (line-end-position))))
                             (push (format "%d: %s" line-num line-content) matches))))
                       (if matches
                           (concat
                            (format "<search_results buffer=%S pattern=%S matches=%d%s>\n"
                                    buffer-name pattern (length matches)
                                    (if (= (length matches) max-matches) " truncated=true" ""))
                            (string-join (nreverse matches) "\n")
                            "\n</search_results>")
                         (format "No matches found for %S in buffer %S." pattern buffer-name)))))))
   :description "Search for a pattern in a buffer and return matching lines with line numbers (max 50 matches)."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to search in.")
           (:name "pattern"
            :type string
            :description "The search pattern to look for.")
           (:name "regexp"
            :type boolean
            :optional t
            :description "If true, treat pattern as a regular expression. Default is false.")
           (:name "case_insensitive"
            :type boolean
            :optional t
            :description "If true, search is case-insensitive. By default does a case insensitive search.."))
   :category "buffers")

  (gptel-make-tool
   :name "edit_buffer"
   :function
   (lambda (buffer old_string new_string)
     (message "gptel :: edit_buffer(%s)" buffer)
     (if-let ((buf (get-buffer buffer)))
         (progn (im-gptel--edit-tool buf old_string new_string)
                (with-current-buffer buf (save-buffer)))
       "Buffer not found"))
   :description "Edit a buffer by replacing exactly one occurrence of OLD_STRING with NEW_STRING in BUFFER. Errors if OLD_STRING is empty, not found, or found more than once."
   :args (list '(:name "buffer"
                 :type string
                 :description "Name of the buffer to edit.")
               '(:name "old_string"
                 :type string
                 :description "Exact text to replace (must match exactly once).")
               '(:name "new_string"
                 :type string
                 :description "Replacement text."))
   :confirm nil
   :category "buffers"))

;;;;;; project tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "list_project_files"
   :function (lambda ()
               (message "gptel :: list_project_files()")
               (->>
                (im-directory-files-recursively (im-current-project-root))
                (s-join "\n")))
   :description "List all file names in the current project, relative to the project dir."
   :category "project")

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
                    :command `("rg" "--vimgrep" "--no-column" ,regexp)
                    :sentinel (lambda (proc _)
                                (when (eq (process-status proc) 'exit)
                                  (with-current-buffer buf
                                    (funcall callback (buffer-string)))
                                  (kill-buffer buf)))))))
   :description "Run grep inside the project. Every match includes file name and file line number."
   :async t
   :args '((:name "regexp"
            :type string
            :description "Regexp to search inside the project."))
   :category "project")

  ;; (gptel-make-tool
  ;;  :name "ast_grep_project"
  ;;  :function (lambda (callback pattern lang)
  ;;              (message "gptel :: ast_grep_project(%s, %s)" pattern lang)
  ;;              (if (s-blank? pattern)
  ;;                  "Operation failed: pattern can't be blank."
  ;;                (let ((default-directory (im-current-project-root))
  ;;                      (buf (generate-new-buffer "*gptel-ast-grep*")))
  ;;                  (make-process
  ;;                   :name "*gptel-grep*"
  ;;                   :buffer buf
  ;;                   :command `("ast-grep" "--lang" ,lang "--pattern" ,pattern)
  ;;                   :sentinel (lambda (proc _)
  ;;                               (when (eq (process-status proc) 'exit)
  ;;                                 (with-current-buffer buf
  ;;                                   (funcall callback (buffer-string)))
  ;;                                 (kill-buffer buf)))))))
  ;;  :description "Run ast-grep inside the project. whenever a search requires syntax-aware or structural matching, use this tool."
  ;;  :async t
  ;;  :args '((:name "pattern"
  ;;           :type string
  ;;           :description "ast-grep pattern, like $A && $A($B) for finding expressions like Array.isArray && Array.isArray(something)")
  ;;          (:name "lang"
  ;;           :type string
  ;;           :description "Language like rust, typescript etc.") )
  ;;  :category "files")
  )

;;;;;; web tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "get_webpage_contents"
   :function (lambda (callback url)
               (message "gptel :: get_webpage_contents(%s)" url)
               ;; TODO: Maybe do this asyncly cause rendering can take some time.
               (request url
                 :headers `(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.10 Safari/605.1.1"))
                 :parser (lambda ()
                           (let ((shr-use-fonts nil)
                                 (shr-fill-text nil)
                                 (shr-use-colors nil)
                                 (shr-inhibit-images t))
                             (shr-render-region (point-min) (point-max))
                             (goto-char (point-min))
                             ;; Links are encoded to text
                             ;; properties. Here we extract them and
                             ;; encode those links in markdown format
                             ;; to buffer so that agent can follow
                             ;; these links.
                             (while-let ((match (text-property-search-forward 'shr-url nil nil t))
                                         (begin (prop-match-beginning match))
                                         (end (prop-match-end match))
                                         (str (format "[%s](%s)"
                                                      (buffer-substring-no-properties begin end)
                                                      (get-text-property begin 'shr-url))))
                               (replace-region-contents begin end (lambda () str))))
                           (buffer-substring-no-properties (point-min) (point-max)))
                 :success
                 (cl-function
                  (lambda (&key data &allow-other-keys)
                    (funcall callback data)))))
   :async t
   :description "Return the contents of a webpage."
   :args '((:name "url"
            :type string
            :description "URL of the webpage to fetch contents from."))
   :category "web")

  (gptel-make-tool
   :name "web_search"
   :function (lambda (callback query)
               (im-kagi-search
                query
                :success
                (lambda (results)
                  (funcall
                   callback
                   (mapconcat
                    (lambda (res)
                      (let-alist res
                        (concat
                         (when .title (format "Title: %s\n" .title))
                         (when .url (format "URL: %s\n" .url))
                         (when .description (format "Desc: %s\n" .description))
                         "---\n")))
                    results "")))
                :error (lambda (it)
                         (funcall
                          callback
                          (format "Error while searching: %s" it)))))
   :description
   "Perform a web search and receive concise results and links to sources."
   :async t
   :args
   '((:name "query"
      :type string
      :description "The search query."))
   :category "web"))

;;;;;; elisp tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "get_elisp_symbol_info"
   :function (lambda (symbol-name symbol-type)
               (message "gptel :: get_elisp_symbol_info(%s, %s)" symbol-name symbol-type)
               (save-window-excursion
                 (let ((help-xref-following t))
                   (cond
                    ((string= symbol-type "function")
                     (helpful-function (intern symbol-name)))
                    ((string= symbol-type "variable")
                     (helpful-variable (intern symbol-name)))
                    (t
                     (helpful-symbol (intern symbol-name))))
                   (buffer-substring-no-properties (point-min) (point-max)))))
   :description "Get detailed information (docs, implementation, current value etc.) about given elisp symbol/function etc. If you are unsure about specifics of function/variable, use this tool. This makes your edits less error prone."
   :args '((:name "symbol_name"
            :type string
            :description "Name of the Elisp symbol.")
           (:name "symbol_type"
            :type string
            :description "Type of symbol: 'function', 'variable', or 'any'."))
   :category "elisp")

  (gptel-make-tool
   :name "search_elisp_functions"
   :function (lambda (pattern search-in)
               (message "gptel :: search_elisp_functions(%s, %s)" pattern search-in)
               (let ((matches '())
                     (search-docs (equal search-in "docs"))
                     (max-results 50))
                 (mapatoms
                  (lambda (sym)
                    (when (and (fboundp sym)
                               (< (length matches) max-results)
                               (if search-docs
                                   (let ((doc (documentation sym t)))
                                     (and doc (string-match-p pattern doc)))
                                 (string-match-p pattern (symbol-name sym))))
                      (push (cons (symbol-name sym)
                                  (let ((doc (documentation sym t)))
                                    (when doc
                                      (car (split-string doc "\n")))))
                            matches))))
                 (if matches
                     (format "Found %d functions matching \"%s\" (in %s):\n%s"
                             (length matches)
                             pattern
                             (if search-docs "docstrings" "names")
                             (mapconcat
                              (lambda (m)
                                (if (cdr m)
                                    (format "• %s: %s" (car m) (cdr m))
                                  (format "• %s" (car m))))
                              (sort matches (lambda (a b) (string< (car a) (car b))))
                              "\n"))
                   (format "No functions found matching \"%s\"" pattern))))
   :description "Search for Emacs functions by name or docstring. Returns function names with first line of their documentation. Returns max 50 results. We have a lot of ready to use libraries/functions available in this environment."
   :args '((:name "pattern"
            :type string
            :description "Regex pattern to search for.")
           (:name "search_in"
            :type string
            :description "Where to search: 'name' (default) or 'docs' for docstrings."))
   :category "elisp")

  (gptel-make-tool
   :name "run_elisp"
   :function (lambda (code)
               (message "gptel :: run_elisp(%s)" code)
               (condition-case err
                   (let* ((output (with-output-to-string
                                    (setq result (eval (read code)))))
                          (result-str (format "%S" result)))
                     (if (string-empty-p output)
                         result-str
                       (format "Output:\n%s\nResult: %s" output result-str)))
                 (error (format "Error: %S" err))))
   :confirm t
   :description "Evaluate Elisp code and return the result. Also captures any printed output. Provide only one form, no comments. You can always wrap multiple forms with let/progn/prog1 etc."
   :args '((:name "code"
            :type string
            :description "Elisp code to evaluate. It'll be evaluated in the current Emacs environment."))
   :category "elisp_mutative"))

;;;;;; jira tools

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "jira_create_issue"
   :function (lambda (project issue-type summary description sprint labels)
               (message "gptel :: jira_create_issue(%s, %s, %s, ...)" project issue-type summary)
               (condition-case err
                   (let* ((sprint-field (cons (im-jira-get-issue-field-id-for "Sprint")
                                              (alist-get 'id (im-jira-find-sprint project sprint))))
                          (extra-fields (list sprint-field))
                          (result (apply #'jiralib2-create-issue
                                         project
                                         issue-type
                                         summary
                                         description
                                         (if (and labels (not (equal labels [])))
                                             (cons (cons 'labels (append labels nil)) extra-fields)
                                           extra-fields))))
                     (format "Issue created successfully: %s" (alist-get 'key result)))
                 (error (format "Failed to create issue: %s" (error-message-string err)))))
   :description "Create a Jira issue in the specified project with summary, description, sprint, and optional labels."
   :confirm t
   :args '((:name "project"
            :type string
            :description "Project key (e.g., 'MYPROJ').")
           (:name "issue_type"
            :type string
            :description "Issue type (e.g., 'Story', 'Bug', 'Task').")
           (:name "summary"
            :type string
            :description "Issue summary/title.")
           (:name "description"
            :type string
            :description "Issue description body.")
           (:name "sprint"
            :type string
            :description "Sprint identifier: 'active', 'future', or full sprint name.")
           (:name "labels"
            :type array
            :items (:type string)
            :description "Optional list of labels to add to the issue."
            :optional t))
   :category "jira")

  (gptel-make-tool
   :name "jira_get_issue"
   :function (lambda (issue-key)
               (message "gptel :: jira_get_issue(%s)" issue-key)
               (condition-case err
                   (let ((issue (jiralib2-get-issue issue-key)))
                     (let-alist issue
                       (let-alist .fields
                         (format "Issue: %s
Summary: %s
Type: %s
Status: %s
Priority: %s
Resolution: %s
Assignee: %s
Reporter: %s
Created: %s
Updated: %s
Labels: %s
Epic: %s
Sprint: %s
Story Points: %s
Project: %s

Description:
%s

Comments (%d):
%s"
                                 (alist-get 'key issue)
                                 .summary
                                 .issuetype.name
                                 .status.name
                                 .priority.name
                                 (or .resolution.name "Unresolved")
                                 (or .assignee.displayName "Unassigned")
                                 .reporter.displayName
                                 .created
                                 .updated
                                 (if .labels (string-join .labels ", ") "None")
                                 (or .customfield_10005 "None")  ;; Epic link
                                 (if .customfield_10004
                                     (car .customfield_10004)
                                   "None")
                                 (or .customfield_10002 "None")  ;; Story points
                                 .project.name
                                 (or .description "No description")
                                 (length (alist-get 'comments .comment))
                                 (mapconcat
                                  (lambda (c)
                                    (let-alist c
                                      (format "- [%s] %s: %s"
                                              .created
                                              .author.displayName
                                              (truncate-string-to-width .body 200 nil nil "..."))))
                                  (alist-get 'comments .comment)
                                  "\n")))))
                 (error (format "Failed to get issue: %s" (error-message-string err)))))
   :description "Get a Jira issue by its key and return a formatted summary with key fields."
   :args '((:name "issue_key"
            :type string
            :description "The Jira issue key (e.g., 'PRA-333', 'PROJ-123')."))
   :category "jira"))

;;;;;; todo_write

(with-eval-after-load 'gptel
  (defvar-local todo-write-list '())
  (gptel-make-tool
   :name "todo_write"
   :function (lambda (id content status)
               (message "gptel :: todo_write(%s, %s, %s)" id content status)
               (cl-block nil
                 (let* ((id-str (format "%s" id))
                        (valid-statuses '("todo" "in_progress" "done"))
                        (todo-list todo-write-list))
                   (unless (member status valid-statuses)
                     (cl-return (format "Invalid status. Must be one of: %s" valid-statuses)))
                   (unless (boundp 'todo-write-list)
                     (make-local-variable 'todo-write-list)
                     (setq todo-write-list nil))
                   (let ((existing (assoc id-str todo-write-list)))
                     (if existing
                         (progn
                           (when content
                             (setf (nth 1 existing) content))
                           (setf (nth 2 existing) status))
                       (unless content
                         (cl-return "Content required for new todo item"))
                       (push (list id-str content status) todo-write-list)))
                   (setq todo-write-list
                         (sort todo-write-list
                               (lambda (a b)
                                 (string< (car a) (car b)))))
                   (when (and todo-write-list
                              (cl-every (lambda (item)
                                          (string= (nth 2 item) "done"))
                                        todo-write-list))
                     (setq todo-write-list nil))
                   (if todo-write-list
                       (mapconcat (lambda (item)
                                    (format "%s:%s:%s"
                                            (car item)
                                            (nth 2 item)
                                            (nth 1 item)))
                                  todo-write-list
                                  "\n")
                     "All todos completed! List cleared."))))

   :description "Create or update todo items. Takes an ID (string or number), content (string, optional when updating), and status (todo/in_progress/done). Returns current todo list. If all items are marked 'done', the list is automatically cleared."
   :args '((:name "id"
            :type string
            :description "Unique identifier for the todo item.")
           (:name "content"
            :type string
            :description "Todo item text (optional when updating existing item).")
           (:name "status"
            :type string
            :description "Current status: 'todo', 'in_progress', or 'done'."))
   :category "meta"))

;;;;; Presets

(with-eval-after-load 'gptel
  (gptel-make-preset 'default
    :system "You are a large language model living in Emacs and a helpful assistant. Respond concisely."
    :backend "DeepSeek"
    :model 'deepseek-chat
    :confirm-tool-calls t
    :tools nil
    :use-tools nil)

  (gptel-make-preset 'research-agent
    :system im-ai-research-prompt
    :backend "DeepSeek"
    :model 'deepseek-chat
    :confirm-tool-calls nil
    :tools '("web")
    :use-tools t)

  (gptel-make-preset 'coding-helper-agent
    :system im-ai-programming-agent-prompt
    :backend "DeepSeek"
    :model 'deepseek-reasoner
    :confirm-tool-calls nil
    :tools '("web")
    :use-tools t)

  (gptel-make-preset 'elisp-coding-helper-agent
    :system im-ai-programming-agent-prompt
    :backend "DeepSeek"
    :model 'deepseek-reasoner
    :confirm-tool-calls nil
    :tools '("web" "elisp")
    :use-tools t)

  (gptel-make-preset 'coding-agent
    :system im-ai-programming-agent-prompt
    :backend "DeepSeek"
    :model 'deepseek-reasoner
    :confirm-tool-calls nil
    :tools '("web" "files" "files_mutative" "project")
    :use-tools t)

  (gptel-make-preset 'elisp-coding-agent
    :system (concat im-ai-programming-agent-prompt
                    "\nIf you are unsure about a variable or a functions usage, look it up before using.\n*IMPORTANT*: Prefer existing tool calls to running arbitrary elisp.")
    :backend "DeepSeek"
    :model 'deepseek-reasoner
    :confirm-tool-calls nil
    :tools '("web" "files" "files_mutative" "project" "elisp" "project")
    :use-tools t))

;;;; Footer

(provide 'im-ai)

;;; im-ai.el ends here
