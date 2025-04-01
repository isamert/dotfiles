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

;; My AI extensions.  Mostly uses `org-ai' to do the heavy-lifting.

;; `im-ai-snippet' simply generates a snippet you requested in the
;; language of the current buffer.  `im-ai-snippet-superior' is the
;; same thing but gives better results at expense of speed.

;; `im-ai' is a function that gives you pre-defined prompts (see
;; `im-ai-prompts') that you can use (by standalone, or on a region
;; etc.) and get results in a predefined org buffer.

;;; Code:

(require 's)
(require 'org-ai)
(require 'gptel)

;;;; Customization

(defgroup im-ai nil
  "Settings for `im-ai'."
  :group 'utils)

(defcustom im-ai-powerful-model "gpt-4o"
  "Powerful model, possibly more expensive one."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-default-model "gpt-4o-mini"
  "Default model, possibly a cheaper one."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-snippet-sys-prompt
  "For given programming language and 'query', you generate a snippet/code. The request format is as follows:

```
Language: <programming language>
Query: <the user query>
Context: <optional context, if any>
```

You should answer in the following format:


```
<your short reasoning for the solution as comment>
<your solution>
```

Your solution should be succinct and to the point without any explanations."
  "System prompt used in `im-ai-snippet'."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-snippet-superior-sys-prompt
  "You are a code snippet assistant focused on providing directly usable code examples that prioritize built-in solutions and standard library functions over ad-hoc implementations. When responding, ensure that the snippets are concise, well-formatted, and easy to modify. Avoid introducing unnecessary functions and provide only the code relevant to the user's request.

Your goal is to deliver clear and efficient solutions that users can immediately integrate into their projects, with minimal additional context. Stay focused on the user's specific query and output only the essential code needed for implementation."
  "System prompt used in `im-ai-snippet-superior'."
  :type 'string
  :group 'im-ai)

(defcustom im-ai-snippet-superior-formatter-prompt
  "For the following question and answer, convert it to a snippet:

- Strip out explanations
- Keep the code only, convert it to something directly usable.
- Incorporate necessary commentaries as code comments.
- Do not use any markdown formatting, only include the code snippet in the answer.
- Get rid of unnecessary functions, variables etc. Only keep the relevant snippet.

The question: %s

The answer:

%s"
  "Formatter prompt for `im-ai-snippet-superior'.
First %s will be replaced by the question and the second will be
replaced by the answer."
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
     :background "#800000" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai)

(defface im-ai-after-face
  '((((class color) (min-colors 88) (background dark))
     :background "#063D3A" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow" :extend t)
    (t :inherit secondary-selection))
  "Face for highlighting regions with pending rewrites."
  :group 'im-ai)

;;;; Variables

(defconst im-ai--buffer "*im-ai*")

;;;; Main

(cl-defun im-ai-prompt (prompt &key follow model sys-prompt output-buffer callback)
  "Like `org-ai-prompt' but do not stream by default.
CALLBACK is called with the result, if OUTPUT-BUFFER is nil.  If
OUTPUT-BUFFER is not nil, then the result is streamed to the
OUTPUT-BUFFER."
  (org-ai-interrupt-current-request)
  (let ((buff (or output-buffer (get-buffer-create " *im-ai*")))
        (org-ai-default-chat-model model))
    (with-current-buffer buff
      (unless output-buffer
        (erase-buffer))
      (org-ai-prompt
       prompt
       :output-buffer buff
       :sys-prompt sys-prompt
       :follow follow
       :callback
       (lambda ()
         (im-run-deferred
          (unless output-buffer
            (funcall callback (with-current-buffer buff (buffer-string))))))))))

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
        "#+begin_ai markdown :service \"openai\" :model \"%s\"\n[ME]:%s%s\n#+end_ai"
        im-ai-powerful-model
        prompt
        context))
      (org-ctrl-c-ctrl-c))))

;;;; im-ai-snippet*

(defvar im-ai--last-processed-point nil)
(add-hook 'gptel-post-stream-hook #'im-ai--cleanup-stream)
(add-hook 'gptel-post-response-functions #'im-ai--cleanup-after)

(defun im-ai-snippet (prompt)
  "Ask for a snippet with PROMPT and get it directly inside your buffer."
  (interactive "sQuestion: ")
  (let ((region (when (use-region-p)
                  (prog1
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (setq
                     im-ai--before-overlay
                     (im-ai--draw-snippet-overlay (region-beginning) (region-end) 'im-ai-before-face))
                    (goto-char (region-end))
                    (deactivate-mark)
                    (insert "\n")
                    (backward-char))))
        (gptel-model im-ai-powerful-model))
    (setq im-ai--last-processed-point (point))
    (gptel-request
        (s-trim
         (format
          "Language: %s
Query: %s
%s"
          (im-ai--get-current-language)
          prompt
          (if region (concat "Context: \n" region) "")))
      :system im-ai-snippet-sys-prompt
      :stream t
      :fsm (gptel-make-fsm :handlers gptel-send--handlers))))

(cl-defun im-ai--cleanup-stream ()
  (when gptel-mode
    (cl-return-from im-ai--cleanup-stream))
  (save-excursion
    (when (> (- (line-number-at-pos (point)) (line-number-at-pos im-ai--last-processed-point)) 2)
      (let ((contents
             (replace-regexp-in-string
              "\n*``.*\n*" "" (buffer-substring-no-properties im-ai--last-processed-point (point)))))
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
              "\n*``.*\n*" ""
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

;;;; im-ai

(cl-defun im-ai (prompt
                 &key
                 (system)
                 (model org-ai-default-chat-model)
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
          (empty (plist-get selection :empty)))
     (list
      prompt
      :model (cond
              (current-prefix-arg
               (completing-read (format "Model [current=%s]: " org-ai-default-chat-model) org-ai-chat-models))
              (model model)
              (t org-ai-default-chat-model))
      :system system
      :empty empty)))
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
      (insert (format "#+begin_ai markdown :service \"openai\" :model \"%s\"\n" model))
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
              ((service model)
               (s-split ":" (completing-read "Select a model: "
                                             '("openai:gpt-4o"
                                               "openai:gpt-4o-mini"
                                               "deepseek:deepseek-chat"
                                               "deepseek:deepseek-reasoner")))))
        (replace-match
         (format "#+begin_ai markdown :service \"%s\" :model \"%s\"" service model)
         nil nil)
        (when (y-or-n-p "Want to remove AI answers?")
          (let ((start (progn
                         (goto-char match-start)
                         (re-search-forward "^\\[AI_?\\w?\\]: ")
                         (beginning-of-line)
                         (point)))
                (end (progn
                       (re-search-forward "^#\\+end_ai")
                       (beginning-of-line)
                       (point))))
            (when (and start end)
              (delete-region start end))))))))

;;;; Utils

(defun im-ai--get-current-language ()
  "Get the current programming language of the buffer.
This is context aware in `org-mode' buffers, takes src blocks into
consideration."
  (let ((mode-name
         (if (and (derived-mode-p 'org-mode) (org-in-src-block-p))
             (org-element-property :language (org-element-at-point))
           (symbol-name major-mode))))
    (->>
     mode-name
     (s-chop-suffix "-mode")
     (s-chop-suffix "-ts")
     (s-replace-all '(("-" . " ")
                      ("interaction" . "")))
     (s-titleize))))

;;;; Footer

(provide 'im-ai)

;;; im-ai.el ends here
