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
  "You are a code snippet assistant focused on providing directly usable code examples that prioritize built-in solutions and standard library functions over ad-hoc implementations. When responding, ensure that the snippets are concise, well-formatted, and easy to modify. Avoid introducing unnecessary functions and provide only the code relevant to the user's request.

Your goal is to deliver clear and efficient solutions that users can immediately integrate into their projects, with minimal additional context. Stay focused on the user's specific query and output only the essential code needed for implementation.

Respond ONLY with the code snippet. Present the code in plain text; avoid markdown or any extra formatting. Do not unnecessarily wrap snippets into functions or introduce unnecessary variables."
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
        "#+begin_ai markdown :model \"%s\"\n[ME]:%s%s\n#+end_ai"
        im-ai-powerful-model
        prompt
        context))
      (org-ctrl-c-ctrl-c))))

;;;; im-ai-snippet*

(defun im-ai-snippet (prompt)
  "Ask for a snippet with PROMPT and get it directly inside your buffer."
  (interactive "sQuestion: ")
  (org-ai-interrupt-current-request)
  (let* ((region (when (use-region-p)
                   (prog1
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (delete-region (region-beginning) (region-end)))))
         (org-ai-default-chat-model im-ai-powerful-model))
    (org-ai-prompt
     (s-trim
      (format
       "Language: %s
Snippet for: %s
%s"
       (im-ai--get-current-language)
       prompt
       (if region (concat "Relevant context: \n" region) "")))
     :output-buffer (current-buffer)
     :sys-prompt im-ai-snippet-sys-prompt
     :follow nil
     :callback (lambda () (message ">> AI request finished.")))))

(defun im-ai-snippet-superior (input)
  "Like `im-ai-snippet' but superior and slower.

Requesting from AI to conform a specific output type reduces it's answer
quality.  This one, compared to `im-ai-snippet' it simply asks the
question without any formatting restrictions and then formats the given
answer with another ai call, but this time with a cheaper model to just
format the result.

I use this when `im-ai-snippet' fails to give me a meaningful answer."
  (interactive "sQuestion: ")
  (let ((prompt (format
                 "%s in %s"
                 input
                 (im-ai--get-current-language))))
    (im-ai-prompt
     prompt
     :sys-prompt im-ai-snippet-superior-sys-prompt
     :model im-ai-powerful-model
     :callback
     (lambda (answer)
       (im-ai-prompt
        (format im-ai-snippet-superior-formatter-prompt prompt answer)
        :model im-ai-default-model
        :output-buffer (current-buffer))))))

;;;; im-ai

(cl-defun im-ai (prompt
                 &key
                 (system)
                 (include-system)
                 (model org-ai-default-chat-model)
                 (callback)
                 (empty))
  "Use the ChatGPT to generate responses to user prompts.

- PROMPT is the user input to generate a response for.
- SYSTEM is the system prompt for the AI assistant.
- INCLUDE-SYSTEM, if non-nil, includes SYSTEM prompt to PROMPT.
  I found that it's more reliable to include system prompt to
  every message.
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
          (empty (plist-get selection :empty))
          (include-system (plist-get selection :include-system)))
     (list
      prompt
      :model (cond
              (current-prefix-arg
               (completing-read (format "Model [current=%s]: " org-ai-default-chat-model) org-ai-chat-models))
              (model model)
              (t org-ai-default-chat-model))
      :system system
      :include-system (or include-system system)
      :empty empty)))
  (when (functionp prompt)
    (setq prompt (funcall prompt))
    (deactivate-mark))
  (when empty
    (setq prompt (im-region-or "")))
  (when-let ((region (im-region-or nil)))
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
      (insert (format "#+begin_ai markdown :model \"%s\" :sys-everywhere %s\n" model include-system))
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
           (format "#\\+begin_ai markdown :model \"\\(%s\\|%s\\)\""
                   im-ai-default-model
                   im-ai-powerful-model)
           nil t)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (current-model (match-string 1)))
        (cond ((string= current-model im-ai-default-model)
               (replace-match (format "#+begin_ai markdown :model \"%s\"" im-ai-powerful-model) nil nil))
              ((string= current-model im-ai-powerful-model)
               (replace-match (format "#+begin_ai markdown :model \"%s\"" im-ai-default-model) nil nil)))
        (when (y-or-n-p "Want to remove AI answers?")
          (let ((start (progn
                         (goto-char match-start)
                         (re-search-forward "^\\[AI]: ")
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
