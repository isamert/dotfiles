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

;;;;; Tools

;;;;;; web tools

(with-eval-after-load 'ellm-tools
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
