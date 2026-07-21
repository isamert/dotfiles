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

;; TODO: ...

;;; Code:

(require 'im)
(require 's)
(require 'dash)
(require 'treesit)
(require 'f)
(require 'org)

;;;; Customization

(defgroup im-ai nil
  "Settings for `im-ai'."
  :group 'utils)

;;;; gptel extensions

;;;;; Higlighting prompts

(with-eval-after-load 'gptel
  (define-advice gptel-mode (:after (&rest _) highlight-prompts)
    "Highlight prompt prefixes."
    (if gptel-mode
        (font-lock-add-keywords nil '(("^\\[ME\\]:" . font-lock-warning-face)
                                      ("^\\[AI\\]:" . font-lock-function-name-face)) t)
      (font-lock-remove-keywords nil '(("^\\[ME\\]:" . font-lock-warning-face)
                                       ("^\\[AI\\]:" . font-lock-function-name-face))))))

;;;;; Recomputing bounds before sending the request

(with-eval-after-load 'gptel
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

      (when-let* ((bounds (im-ai--gptel-recompute-bounds)))
        (beginning-of-buffer)
        (org-set-property "GPTEL_BOUNDS" bounds)

        ;; Changing the bounds might change all positions in the
        ;; buffer. Redo them if they differ after the change
        (unless (string= bounds (im-ai--gptel-recompute-bounds))
          (im-ai--gptel-update-bounds)))))

  (defun im-ai--gptel-purge-bounds ()
    (gptel-mode -1)
    (im-ai--gptel-update-bounds)
    (gptel-mode 1)))

;;;; ellm extensions

;;;;; DWIM

(defun im-ai-ellm-dwim (&optional new?)
  (interactive "P")
  (let* ((root (im-current-project-root))
         (default-directory root)
         (same-ellm-buffer? (lambda () (and (eq major-mode 'ellm-mode) (f-same? root default-directory))))
         (existing (--filter (with-current-buffer it (funcall same-ellm-buffer?)) (buffer-list)))
         (result (if (and (length> existing 1) (not new?))
                     (read-buffer "Switch to: " nil nil
                                  (lambda (b)
                                    (and-let* ((buf (get-buffer (or (car-safe b) b))))
                                      (with-current-buffer buf (funcall same-ellm-buffer?)))))
                   (if (and (length= existing 1) (not new?))
                       (car existing)
                     (save-window-excursion (ellm-new-buffer))))))
    (when-let* ((region (im-region-or nil)))
      (let ((lang (im-ai--get-current-language))
            (lang-alt (and-let* ((fname (buffer-file-name))
                                 ((y-or-n-p "Include file path? ")))
                        (format
                         "%s:%s:%s"
                         (line-number-at-pos (region-beginning) 'absolute)
                         (line-number-at-pos (region-end) 'absolute)
                         (f-relative fname root)))))
        (with-current-buffer result
          (goto-char (point-max))
          (insert "\n```" (or lang-alt lang) "\n" (string-trim region "\n" "\n") "\n```"))))
    (if (im-buffer-visible-p result)
        (select-window (get-buffer-window result))
      (switch-to-buffer result))
    (goto-char (point-max))
    (recenter)))

(defun im-ai-ellm-toggle-side-buffer (&optional new?)
  "Same as `im-ai-ellm-dwim' but toggle buffer in side buffer."
  (interactive)
  (let ((buffer (save-window-excursion
                  (im-ai-ellm-dwim new?)
                  (current-buffer))))
    (if (and (im-buffer-visible-p buffer)
             (use-region-p))
        (progn
          (im-display-buffer-in-side-window buffer)
          (goto-char (point-max))
          (recenter))
      (im-toggle-side-buffer-with-name (buffer-name buffer)))))

;;;;; Tools

;;;;;; web tools

(with-eval-after-load 'ellm-tools
  (ellm-deftool web/get-page (:async t)
    ((url :string "URL of the webpage to fetch contents from."))
    "Return the contents of a webpage."
    (message "ellm :: web/get-page(%s)" url)
    (request url
      :headers `(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.10 Safari/605.1.1"))
      :parser (lambda ()
                (let ((shr-use-fonts nil)
                      (shr-fill-text nil)
                      (shr-use-colors nil)
                      (shr-inhibit-images t))
                  (shr-render-region (point-min) (point-max))
                  (goto-char (point-min))
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

  (ellm-deftool web/search (:async t)
    ((query :string "The search query."))
    "Perform a web search and receive concise results and links to sources."
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
               (format "Error while searching: %s" it))))))

;;;;;; elisp tools

(defun im-ai-tool--get-elisp-symbol-info (symbol-name symbol-type)
   "Get detailed information about an Elisp symbol.
SYMBOL-NAME is the name of the symbol.
SYMBOL-TYPE is 'function', 'variable', or 'any'."
  (message "ellm :: get_elisp_symbol_info(%s, %s)" symbol-name symbol-type)
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

(defun im-ai-tool--search-elisp-functions (pattern search-in)
   "Search for Emacs functions by name or docstring.
PATTERN is a regex pattern to search for.
SEARCH-IN is 'name' or 'docs' for docstrings."
  (message "ellm :: search_elisp_functions(%s, %s)" pattern search-in)
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

(defun im-ai-tool--run-elisp (code)
   "Evaluate Elisp code and return the result.
CODE is the Elisp code to evaluate."
  (message "ellm :: run_elisp(%s)" code)
  (condition-case err
      (let* ((result nil)
             (output (with-output-to-string
                       (setq result (eval (read code) t))))
             (result-str (format "%S" result)))
        (if (string-empty-p output)
            result-str
          (format "Output:\n%s\nResult: %s" output result-str)))
    (error (format "Error: %S" err))))

(with-eval-after-load 'ellm-tools
  (ellm-deftool elisp/get-elisp-symbol-info ()
    ((symbol-name :string "Name of the Elisp symbol.")
     (symbol-type :string "Type of symbol: 'function', 'variable', or 'any'."))
    "Get detailed information (docs, implementation, current value etc.) about given elisp symbol/function etc. If you are unsure about specifics of function/variable, use this tool. This makes your edits less error prone."
    (im-ai-tool--get-elisp-symbol-info symbol-name symbol-type))

  (ellm-deftool elisp/search-elisp-functions ()
    ((pattern :string "Regex pattern to search for.")
     (search-in :string "Where to search: 'name' (default) or 'docs' for docstrings."))
    "Search for Emacs functions by name or docstring. Returns function names with first line of their documentation. Returns max 50 results. We have a lot of ready to use libraries/functions available in this environment."
    (im-ai-tool--search-elisp-functions pattern search-in))

  (ellm-deftool elisp/run-elisp ()
    ((code :string "Elisp code to evaluate. It'll be evaluated in the current Emacs environment."))
    "Evaluate Elisp code and return the result. Also captures any printed output. Provide only one form, no comments. You can always wrap multiple forms with let/progn/prog1 etc."
    (im-ai-tool--run-elisp code)))

;;;;;; jira tools

(defun im-ai-tool--jira-create-issue (project issue-type summary description sprint labels)
  "Create a Jira issue in the specified project with summary, description, sprint, and optional labels."
  (message "ellm :: jira_create_issue(%s, %s, %s, ...)" project issue-type summary)
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

(defun im-ai-tool--jira-get-issue (issue-key)
  "Get a Jira issue by its key and return a formatted summary with key fields."
  (message "ellm :: jira_get_issue(%s)" issue-key)
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

(with-eval-after-load 'ellm
  (ellm-deftool jira/get-issue ()
    ((issue-key :string "The Jira issue key (e.g., 'PRA-333', 'PROJ-123')."))
    "Get a Jira issue by its key and return a formatted summary with key fields."
    (im-ai-tool--jira-get-issue issue-key callback))

  (ellm-deftool jira/create-issue ()
    ((project    :string "Project key (e.g., 'MYPROJ').")
     (issue-type :string "Issue type (e.g., 'Story', 'Bug', 'Task').")
     (summary    :string "Issue summary/title.")
     (description :string "Issue description body.")
     (sprint     :string "Sprint identifier: 'active', 'future', or full sprint name.")
     (labels     :array "Optional list of labels to add to the issue." &optional))
    "Create a Jira issue in the specified project with summary, description, sprint, and optional labels."
    (im-ai-tool--jira-create-issue project issue-type summary description sprint labels callback)))

;;;; Footer

(provide 'im-ai)

;;; im-ai.el ends here
