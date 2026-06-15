;;; im-jira.el --- Jira extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: jira

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

;; This is my simple Jira interface for Emacs.  Uses jiralib2.  It
;; does not have complex interfaces and does not expose all Jira
;; functionality but it lets you:
;;
;; - view/create/edit Jira issues in Org mode
;; - act on issues, like changing it's status/assignee etc.
;; - convert issues into Git branches/worktrees etc.
;; - search issues interactively (like Jira search page)
;; - search issues via an Org mode dynamic block
;; - some other stuff like this


;;; Code:

(require 'im)
(require 's)
(require 'memoize)
(require 'jiralib2)
(require 'vtable)

;;;; Customization

(defgroup im-jira nil
  "Settings for `im-jira'."
  :group 'utility)

;;;; Variables

(defcustom im-git-main-branch "main"
  "Main branch name."
  :type 'string
  :group 'im-jira)

(defcustom im-jira-projects nil
  "List of projects that I enrolled in JIRA."
  :type '(repeat string)
  :group 'im-jira)

(defcustom im-jira-base-branch "origin/main"
  "Branch to create feature branches from.
Consider using origin/something to create the branch from latest
something."
  :type 'string
  :group 'im-jira)

(defcustom im-jira-story-points-field-name 'customfield_10002
  "Which field describes the story points in JIRA response."
  :type 'symbol
  :group 'im-jira)

(defcustom im-jira-feature-branch-prefix "feature/"
  "Prefix to prepend feature branch names."
  :type 'string
  :group 'im-jira)

(defcustom im-jira-my-issues-query
  "assignee = currentUser() OR creator = currentUser() ORDER BY createdDate DESC"
  "Query to find out issues that are assigned to me."
  :type 'string
  :group 'im-jira)

(defcustom im-jira-kanban-board-query
  "(fixVersion in unreleasedVersions() OR fixVersion is EMPTY) AND createdDate >= -2w ORDER BY Rank ASC"
  "Query to get kanban board issues."
  :type 'string
  :group 'im-jira)

(defcustom im-jira-board-ids nil
  "Interested board ids.
See `im-jira-list-issues'."
  :type '(alist :key-type string :value-type string)
  :group 'im-jira)

(defconst im-jira-jql-example-query
  (format "text ~ \"...\" labels IN (\"tech-debt\") AND statusCategory = \"To Do|In Progress|Done\" and project IN (%s) and created > startOfYear()"
          (s-join ", " im-jira-projects)))

;;;; Main

;;;;; Open jira issue at point

(defun im-jira-issue-at-point ()
  (let ((sym (or (thing-at-point 'symbol) "")))
    (when (s-matches? "[a-zA-Z]+-[0-9]+" sym)
      sym)))

;;;;; Interactive stuff

(defun im-jira-open-issue (issue-number)
   "Open given Jira ISSUE-NUMBER."
  (interactive "sIssue: ")
  ;; TODO: Remove the following if I move this functionality to a package/file
  (require 'jiralib2)
  (let ((url (format "%s/browse/%s" jiralib2-url (car (s-split " " issue-number)))))
    (kill-new url)
    (browse-url url)))

(defun im-jira-jql (jql)
  (interactive (list (read-string "Enter JQL: " im-jira-jql-example-query)))
  (jiralib2-jql-search jql))

(defun im-jira-jql-interactive ()
  (interactive)
  (let ((search-jql (lambda (values)
                      (let* ((inhibit-read-only t)
                             (inhibit-modification-hooks t)
                             (jql (nth 0 values))
                             (issues (jiralib2-jql-search jql)))
                        (delete-region (point) (point-max))
                        (make-vtable
                         :row-colors (im-vtable-pretty-colors)
                         :column-colors (im-vtable-pretty-colors)
                         :columns '((:name "Key" :width 10)
                                    (:name "Status" :width 15)
                                    (:name "Reporter" :width 18)
                                    (:name "Assignee" :width 18)
                                    "Summary")
                         :use-header-line nil
                         :objects issues
                         :actions '("RET" im-jira-issue-actions
                                    "i" tbp--inspect)
                         :getter (lambda (object column vtable)
                                   (let-alist object
                                     (pcase (vtable-column vtable column)
                                       ("Key" .key)
                                       ("Status" .fields.status.name)
                                       ("Reporter" (or .fields.reporter.name "N/A"))
                                       ("Assignee" (or .fields.assignee.name "N/A"))
                                       ("Summary" .fields.summary)))))))))
    (im-query-buffer
     :name "*jira-jql-search*"
     :inputs `((:name "JQL Query"
                :value ,im-jira-jql-example-query
                :on ,(lambda (_input values)
                       (funcall search-jql values))))
     :on search-jql)))

(defun im-jira-list-issues ()
  (interactive)
  (im-jira-issue-actions
   (im-completing-read
    "Select ticket: "
    (pcase (completing-read "Issue list: " '("My issues" "Current Sprint" "New issues" "Kanban" "Board" "JQL"))
      ("My issues" (im-jira-get-my-issues))
      ("New issues" (im-jira-get-new-issues))
      ("Current Sprint" (im-jira-get-current-sprint-issues))
      ("Board" (im-jira-get-board-issues))
      ("Kanban" (im-jira-get-kanban-issues))
      ("JQL" (call-interactively #'im-jira-jql)))
    :formatter #'im-jira--format-ticket-name
    :sort? nil)))

(defun im-jira-ticket-to-branch (key summary)
  "Create a new branch from given ISSUE-NAME and switch to it."
  (interactive "sIssue name: ")
  (let ((branch-name (im-jira--create-branch-name-from-ticket (concat key summary))))
    (message "Updating...")
    (unless (= 0 (shell-command "git fetch --all"))
      (user-error "Cannot git fetch --all"))
    (message "Creating branch...")
    ;; FIXME: magit-branch-and-checkout
    (magit-branch-and-checkout branch-name im-jira-base-branch)
    (vc-refresh-state)
    (im-jira-change-issue-status-to-status key "In Progress")
    (message "Currently on %s." (lab-git-current-branch))))

(defun im-jira-create-ticket ()
  (interactive)
  (let ((project (im-jira--select-project))
        (issue-type (im-jira--select-issue-type))
        (summary (read-string "Issue summary: ")))
    (im-get-input
     :init
     (format (concat
              "* %s\n"
              ":PROPERTIES:\n"
              ":PROJECT-ID: %s\n"
              ":ISSUE-TYPE: %s\n"
              ":SPRINT: active|future\n"
              ":END:\n\n"
              (im-jira--get-issue-template issue-type))
             summary project issue-type)
     :pre-process
     (lambda ()
       (goto-char (point-min))
       (list
        :summary (org-entry-get nil "ITEM")
        :project-id (org-entry-get nil "PROJECT-ID")
        :type (org-entry-get nil "ISSUE-TYPE")
        :rest `((,(im-jira-get-issue-field-id-for "Sprint") .
                 ,(alist-get 'id (im-jira-find-sprint (org-entry-get nil "PROJECT-ID") (org-entry-get nil "SPRINT")))))))
     :on-accept
     (lambda (description props)
       (setq description
             (->>
              (org-export-string-as description 'confluence t)
              (s-split "\n")
              (-drop 1)
              (s-join "\n")))
       (message ">> (im-jira-create-ticket %s :description %s)" props description)
       (thread-last
         (apply #'jiralib2-create-issue
                `(,(plist-get props :project-id)
                  ,(plist-get props :type)
                  ,(plist-get props :summary)
                  ,description
                  ,@(plist-get props :rest)))
         (im-jira-issue-actions))))))

(defun im-jira-change-issue-status (key)
  (interactive "sIssue number: ")
  (->>
   (im-completing-read
    "Select status: "
    (im-jira-get-issue-transitions key)
    :formatter (lambda (it) (let-alist it (format "%s [%s]" .name .to.name))))
   (alist-get 'id)
   (im-jira-change-issue-status-to key)))

(defun im-jira-issue-actions (issue)
  (interactive
   (list (jiralib2-get-issue (read-string "Jira issue: " (or (im-jira-issue-at-point) "")))))
  (cl-loop
   (let-alist issue
     (let* ((action
             (im-completing-read
              (format "Act on %s: " (s-truncate 20 .fields.summary))
              '("View" "Open" "Update" "To branch" "To worktree" "Assign to..." "Insert as task" "Change status" "Inspect" "[Cancel]")
              :sort? nil)))
       (pcase action
         ("View"
          (im-jira-view-ticket .key)
          (cl-return))
         ("Open"
          (with-default-browser
           (im-jira-open-issue .key))
          (cl-return))
         ("Update"
          (im-jira-update-ticket .key .fields.summary .fields.description)
          (cl-return))
         ("To branch"
          (im-jira-ticket-to-branch .key .fields.summary)
          (cl-return))
         ("To worktree"
          (im-jira-ticket-to-worktree .key .fields.summary)
          (cl-return))
         ("Assign to..."
          (jiralib2-assign-issue
           .key
           (alist-get 'name (im-jira--select-user))))
         ("Change status"
          (im-jira-change-issue-status .key))
         ("Insert as task"
          (insert (format "** TODO [#A] %s %s :work:" .key .fields.summary)))
         ("Inspect"
          (im-json-encode-and-show issue)
          (cl-return))
         ("[Cancel]"
          (cl-return)))))))

(defun im-jira-create-quick-issue (arg)
   "Quickly create an issue and act on it.
If ARG is non-nil, insert the issue number to current buffer
instead of acting on issue."
  (interactive "P")
  (let* ((project (im-jira--select-project))
         (issue (jiralib2-create-issue
                 project
                 (im-jira--select-issue-type)
                 (read-string "Issue summary: ")
                 (concat
                  "THIS IS AN AUTOMATICALLY GENERATED ISSUE. TO BE FILLED LATER. \n\n"
                  (im-jira--get-issue-template "Story"))
                 (cons
                  (im-jira-get-issue-field-id-for "Sprint")
                  (alist-get 'id (im-jira-find-sprint project (completing-read "Sprint: " '("active" "future"))))))))
    (let-alist issue
      (save-window-excursion
        (im-jira-view-ticket .key)
        (message ">> Opened issue in a buffer."))
      (im-kill .key)
      (if arg
          (insert .key " - ")
        (im-jira-issue-actions issue)))))

;;;;; Internal

(defmemoize im-jira-get-my-issues ()
   (jiralib2-jql-search im-jira-my-issues-query))

(defun im-jira-get-kanban-issues ()
  (jiralib2-jql-search (format "project = " (im-jira--select-project) " AND" im-jira-kanban-board-query)))

(defun im-jira-get-board-issues ()
  (let ((board-id (if (= 1 (length im-jira-board-ids))
                      (cdar im-jira-board-ids)
                    (cdr (assoc (completing-read "Select board: " im-jira-board-ids) im-jira-board-ids)))))
    (jiralib2-board-issues board-id nil)))

(defun im-jira-get-current-sprint-issues (&optional projects)
  "Get current sprint issues for all PROJECTS.
If PROJECTS is nil, then `im-jira-projects' is used."
  (let ((issues '()))
    (mapc
     (lambda (project)
       (setq
        issues
        (thread-last
          (format "project = \"%s\" AND Sprint in openSprints()"
                  project)
          (jiralib2-jql-search)
          (append issues))))
     (or projects im-jira-projects))
    issues))

(defun im-jira-get-new-issues ()
  (cl-mapcan
   (lambda (project)
     (jiralib2-jql-search
      (format "project = \"%s\" AND created > -10d" project)))
   im-jira-projects))

(defun im-jira-get-issue-fields ()
  (jiralib2-session-call "/rest/api/2/field"))

;; TODO support pagination
(defun im-jira-get-sprints (project)
  (alist-get 'values (jiralib2-session-call (format "/rest/agile/1.0/board/%s/sprint" (alist-get project im-jira-board-ids nil nil #'equal)))))

(defun im-jira-find-sprint (project sprint)
  "Find a sprint.
SPRINT can be a full sprint name or one \"active\"|\"future\"."
  (let ((sprints (im-jira-get-sprints project)))
    (or
     (--find (string-equal sprint (alist-get 'name it)) sprints)
     (--find (string-equal sprint (alist-get 'state it)) sprints))))

(defun im-jira-get-issue-field-id-for (field-name)
  (alist-get
   'id
   (--find
    (string-equal field-name (alist-get 'name it))
    (im-jira-get-issue-fields))))

(defun im-jira-get-issue-transitions (issue)
  (alist-get
   'transitions
   (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions?expand=transition.fields" issue))))

(defun im-jira-change-issue-status-to (issue status-id)
  (jiralib2-session-call
   (format "/rest/api/2/issue/%s/transitions?expand=transition.fields" issue)
   :type "POST"
   :data (json-encode
          `((transition (id . ,status-id))))))

(defun im-jira-change-issue-status-to-status (issue-id status)
  "Change status of ISSUE-ID by STATUS name.
Same as `im-jira-change-issue-status-to' but uses the status name as
shown in Jira UI instead of status id."
  (im-jira-change-issue-status-to
   issue-id
   (alist-get
    'id
    (--find
     (string-equal (alist-get 'name it) status)
     (im-jira-get-issue-transitions issue-id)))))

(defmemoizefile im-jira-get-users () "~/.emacs.d/jira-user-cache"
  (mapcar
   (lambda (project) (cons project (jiralib2-get-users project)))
   im-jira-projects))

(defun im-jira--select-user ()
  (thread-last
    (im-jira-get-users)
    (assoc-string (im-jira--select-project))
    (cdr)
    (--map (cons (alist-get 'name it) it))
    (im-alist-completing-read "Select a user: ")))

(defun im-jira--select-project ()
  "Interactively select one of enrolled projects."
  (if (eq (length im-jira-projects) 1)
      (car im-jira-projects)
    (completing-read "Select project: " im-jira-projects)))

(defun im-jira--select-issue-type ()
  (completing-read
   "Issue type: "
   (--map
    (let-alist it (cons .name .id))
    (jiralib2-get-issuetypes))))

(defun im-jira--get-issue-template (issue-type)
  (pcase issue-type
    ("Story" "** Motivation & Description\n\n\n** Acceptance Criteria\n\n\n** Projects\n\n\n** Has Automation Test?\n\n\n** Links (UI/UX, Analysis etc.)\n\n")
    ("Sprint Development Bug" "** Description\n\n\n**Case\n\n\n** Projects\n\n")
    ("Production Bug" "** Description\n\n\n** Steps\n\n\n** Projects\n\n\n** Incident Excel\n\n\n** Links - SS - Video\n\n")))

(defun im-jira--create-branch-name-from-ticket (issue-name)
  "Create a branch name from given Jira ISSUE-NAME."
  (thread-last
    issue-name
    (im-string-url-case)
    (s-downcase)
    (im-s-upcase-until "-")
    (s-prepend im-jira-feature-branch-prefix)
    (read-string "Branch name: ")))

;; TODO Color code status etc.
(defun im-jira--format-ticket-name (it)
  "Format ticket name for displaying in `completing-read' window."
  (let-alist it
    (format
     "%-7s\t[%-11s]\t%-15s => %-15s\t%s"
     (propertize .key
                 'face 'bold)
     (propertize (s-truncate 11 .fields.status.name)
                 'face 'italic)
     (propertize (s-truncate 15 (or .fields.reporter.name "N/A"))
                 'face 'italic)
     (propertize (s-truncate 15 (or .fields.assignee.name "N/A"))
                 'face 'italic)
     .fields.summary)))

(defun im-convert-jira-markup-to-org-mode (jira-markup)
  "Convert given JIRA-MARKUP string to `org-mode' format."
  (with-temp-buffer
    (insert jira-markup)
    ;; This creates loose lists where newlines appear between
    ;; list items and ox-confluence does not handle this well and
    ;; breaks lists.
    (shell-command-on-region
     (point-min) (point-max)
     "pandoc -f jira -t org --wrap=none"
     nil t)
    ;; So I try to remove those unnecassary new lines here.
    (->>
     (buffer-string)
     ;; Same replacement applied twice.
     (replace-regexp-in-string "^\\([ \t]*\\)-\\(.*\\)\n\n\\([ \t]*\\)-" "\\1-\\2\n\\3-")
     (replace-regexp-in-string "^\\([ \t]*\\)-\\(.*\\)\n\n\\([ \t]*\\)-" "\\1-\\2\n\\3-")
     (replace-regexp-in-string "\\\\\\\\$" "")
     ;; Fix deeply nested bullet points
     (replace-regexp-in-string "---- " "- ")
     ;; Fix - [ ] markers
     (replace-regexp-in-string "- \\*=( )=\\* " "- [ ] ")
     (replace-regexp-in-string "- \\*=(-)=\\* " "- [-] ")
     (replace-regexp-in-string "- \\*=(X)=\\* " "- [X] "))))

(defun im-jira-update-ticket (key summary description)
   (im-get-input
    :init
    (concat
     "* "
     summary
     "\n"
     (im-convert-jira-markup-to-org-mode description))
    :pre-process
    (lambda ()
      (goto-char (point-min))
      (org-entry-get nil "ITEM"))
    :on-accept
    (lambda (description summary)
      (setq description
            (->>
             (org-export-string-as description 'confluence t)
             (s-split "\n")
             (-drop 1)
             (s-join "\n")))
      (message ">> (im-jira-update-ticket \"%s\" \"%s\" \"%s\")" key summary description)
      (jiralib2-update-summary-description key summary description))))

;;;;; jira-view-mode

(defvar-local jira-view-mode-ticket nil
   "Currently viewed ticket object.")
(define-derived-mode jira-view-mode org-mode "JiraView"
  "Mode for viewing JIRA tickets.")

(defun jira-view-mode-open-externally ()
  (interactive nil jira-view-mode)
  (with-default-browser
   (let-alist jira-view-mode-ticket
     (im-jira-open-issue .key))))

(defun jira-view-mode-edit ()
  (interactive nil jira-view-mode)
  (let-alist jira-view-mode-ticket
    (im-jira-update-ticket .key .fields.summary .fields.description)))

(defun jira-view-mode-reload ()
  (interactive nil jira-view-mode)
  (let-alist jira-view-mode-ticket
    (im-jira-view-ticket .key)))

(defun jira-view-mode-act ()
  (interactive nil jira-view-mode)
  (im-jira-issue-actions jira-view-mode-ticket))

(defun jira-view-mode-inspect ()
  (interactive nil jira-view-mode)
  (im-inspect jira-view-mode-ticket))

(evil-define-key 'normal jira-view-mode-map
  (kbd "&") #'jira-view-mode-open-externally
  (kbd "ge") #'jira-view-mode-edit
  (kbd "gr") #'jira-view-mode-reload
  (kbd "gi") #'jira-view-mode-inspect
  ;; TODO All actions might be single keypress.
  (kbd "ga") #'jira-view-mode-act)

(defun im-jira-view-ticket (key)
  (interactive "sIssue key: ")
  (when-let* ((match (s-match "browse/\\([a-zA-Z0-9-]+\\)/?" key)))
    (setq key (nth 1 match)))
  (let ((ticket (jiralib2-get-issue key)))
    (let-alist ticket
      (let ((buffer (get-buffer-create (format "*jira:%s:%s*" key .fields.summary))))
        (unless (eq (current-buffer) buffer)
          (switch-to-buffer-other-window buffer)))
      (erase-buffer)
      (insert
       (concat
        "* "
        key
        " - "
        .fields.summary
        "\n"
        (im-convert-jira-markup-to-org-mode .fields.description)))
      (insert "\n\n-----\n\n* Comments\n")
      (jira-view-mode)
      (dolist (comment .fields.comment.comments)
        (let-alist comment
          (insert (format "** %s (%s)\n%s\n\n" .author.displayName .created .body))))
      (setq header-line-format "Hit `&' to open in browser, `ge' to edit, `gr' to reload, `ga' to see actions.")
      (setq-local jira-view-mode-ticket ticket)
      (goto-char (point-min))
      (org-set-property "STATUS" (or .fields.status.name "N/A"))
      (org-set-property "REPORTER" (or .fields.reporter.name "N/A"))
      (org-set-property "ASSIGNEE" (or .fields.assignee.name "N/A"))
      (org-set-property "STORY_POINTS" (format "%s" (or (alist-get im-jira-story-points-field-name .fields) "N/A")))
      (org-fold-show-all))))

;;;;; org-mode integration

(defun im-jira-list-current-sprint-assignee-swimlane ()
  "Draw a table for the current sprint that resembles assignee swimlanes of JIRA.
It also shows how much story point each assignee has and how much
story points they have released.  See the following figure:

  | Assignee  | Total | Done | Sub-total | Status      | Issue         |
  |-----------+-------+------+-----------+-------------+---------------|
  | someone-1 |   8.0 |  3.0 |           |             |               |
  |           |       |      |       3.0 | Open        | AI-483 - ...  |
  |           |       |      |       3.0 | Done        | AI-423 -  ... |
  |           |       |      |       2.0 | Code Review | AI-488 - ...  |
  |-----------+-------+------+-----------+-------------+---------------|
  | someone-2 |   7.0 |  2.0 |           |             |               |
  |           |       |      |       2.0 | Released    | AI-485 - ...  |
  |           |       |      |       5.0 | In Progress | AI-313 - ...  |"
  (interactive)
  (with-current-buffer (get-buffer-create "*jira: current-sprint-by-points*")
    (erase-buffer)
    (org-mode)
    (org-dblock-write:jira (list :projects im-jira-projects))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(defun org-dblock-write:jira (params)
  "Dynamic block version of `im-jira-list-current-sprint-assignee-swimlane'.
Not mentioned in this function but using :target XXX will make these
blocks trackable by `im-jira-check-tracked-issue-groups`."
  (let* ((projects (plist-get params :projects))
         (issues (plist-get params :issues))
         (progress? (plist-get params :progress?))
         (jql (plist-get params :jql))
         (group-by-assignee? (plist-get params :group-by-assignee))
         results)
    (when issues
      (setq jql (format "key IN (%s)" (s-join ", " (mapcar #'symbol-name issues)))))
    (setq results (cond
                   (jql (jiralib2-jql-search jql))
                   (t (im-jira-get-current-sprint-issues projects))))
    (when progress?
      (let ((done (length (--filter (-contains? '("Released" "Done") (let-alist it .fields.status.name)) results)))
            (total (length results)))
        (insert
         (format "- Progress :: %s/%s (%s%%)\n\n" done total (/ (* 100 done) total)))))
    (if group-by-assignee?
        (insert "| Assignee | Total | Done |  Sub-total | Creator | Status  | Issue |\n|-\n")
      (insert "| Assignee | Creator | Point | Status | Sprint | Issue |\n|-\n"))
    (cond
     (group-by-assignee?
      (->>
       results
       (--group-by (let-alist it .fields.assignee.name))
       (map-apply
        (lambda (key vals)
          (list
           :assignee key
           :total (-sum (--map (let-alist it (or (alist-get im-jira-story-points-field-name .fields) 0)) vals))
           :done (->>
                  vals
                  (--filter (-contains? '("Done" "Released") (let-alist it .fields.status.name)))
                  (--map (let-alist it (or (alist-get im-jira-story-points-field-name .fields) 0)))
                  (-sum))
           :tasks
           (->>
            (--map (let-alist it
                     (list
                      :points (alist-get im-jira-story-points-field-name .fields)
                      :summary (format "%s - %s" .key .fields.summary)
                      :status .fields.status.name
                      :creator .fields.creator.name))
                   vals)
            (--sort (string> (plist-get it :status) (plist-get other :status)))))))
       (--sort (> (plist-get it :total) (plist-get other :total)))
       (--map (format "| %s | %s | %s | | | |\n%s"
                      (plist-get it :assignee)
                      (plist-get it :total)
                      (plist-get it :done)
                      (s-join
                       "\n"
                       (--map (format "| | | | %s | %s | %s | %s |"
                                      (plist-get it :points)
                                      (plist-get it :creator)
                                      (plist-get it :status)
                                      (s-truncate 120 (plist-get it :summary)))
                              (plist-get it :tasks)))))
       (s-join "\n|-\n")
       (insert)))
     (t
      (->>
       results
       (--map (let-alist it
                (format "| %s | %s | %s | %s | %s | %s |"
                        .fields.assignee.name
                        .fields.creator.name
                        (alist-get im-jira-story-points-field-name .fields)
                        .fields.status.name
                        (when-let* ((match (s-match "name=\\([^,]+\\)," (or (car .fields.customfield_10004) ""))))
                          (nth 1 match))
                        (s-truncate 120 (format "%s - %s" .key .fields.summary)))))
       (s-join "\n")
       (insert))))
    (org-table-align)))

;;;###autoload
(defun im-jira-check-tracked-issue-groups ()
  "Check the status of tracked issue groups and add them to INBOX if needed.
I track some Jira tickets (or one) using jira dblock, which is like:

    #+begin: jira :issues (XXX-111 YYY-222) :progress? t :target 100
    - Progress :: 1/2 (50%)

    | Assignee | Creator | Point | Status | Sprint | Issue |
    |----------+---------+-------+--------+--------+-------|
    | ...      | ...     |   3.0 | ...    | ...    | ...   |
    | ...      | ...     |   3.0 | DONE   | ...    | ...   |
    #+end:

This function goes through all Jira blocks marked with :target, updates
them, and checks if they meet their target.  If they do, it adds the
block's heading (its parent) to \\='inbox-org."
  (interactive)
  (when (not (im-check-internet-connection work-vpn-check-host))
    (user-error "Not connected to the VPN"))
  (let ((start (float-time)))
    (with-current-buffer (find-file-noselect trendyol-org)
      ;; Collect positions of all relevant dblocks first
      (let ((block-positions '()))
        (org-map-dblocks
         (lambda ()
           (when (save-excursion
                   (beginning-of-line)
                   (re-search-forward "jira.*:target[ \t]+\\([0-9]+\\)"
                                      (line-end-position) t))
             (push (point-marker) block-positions))))
        ;; Process in reverse order (bottom to top) so updates don't
        ;; shift positions of not-yet-processed blocks above
        (dolist (marker block-positions)
          (goto-char marker)
          (when-let* ((target (save-excursion
                                (beginning-of-line)
                                (when (re-search-forward
                                       "jira.*:target[ \t]+\\([0-9]+\\)"
                                       (line-end-position) t)
                                  (string-to-number (match-string 1)))))
                      (not-done? (not (equal "DONE" (org-get-todo-state))))
                      (source-id (org-id-get-create))
                      (source-header (org-get-heading t t t t)))
            (message "jira :: Processing %s..." source-header)
            (org-dblock-update)
            (let ((percent "")
                  (end (save-excursion
                         (re-search-forward org-dblock-end-re nil t))))
              (save-excursion
                (when (re-search-forward
                       "- Progress :: .*?([ \t]*\\([0-9]+\\)%[ \t]*)"
                       end t)
                  (setq percent (string-to-number (match-string 1)))))
              (when (and percent target (>= percent target))
                (with-current-buffer (find-file-noselect inbox-org)
                  (unless (-any
                           #'identity
                           (org-map-entries
                            (lambda ()
                              (s-contains? (format "[[id:%s]" source-id)
                                           (or (org-get-heading t t t t) "")))
                            "LEVEL=1"))
                    (let ((link (format "[[id:%s][%s]]" source-id source-header)))
                      (goto-char (point-min))
                      (re-search-forward org-heading-regexp nil t)
                      (org-insert-heading-respect-content)
                      (insert (format "TODO [#A] Target achieved: %s \nDEADLINE: " link))
                      (org-insert-timestamp (current-time))
                      (save-buffer)))))))
          (set-marker marker nil))))
    (save-buffer)
    (message ">> Took %.2f seconds..." (- (float-time) start))))

;;;; Footer

(provide 'im-jira)

;;; im-jira.el ends here
