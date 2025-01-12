;;; im-github.el --- GitHub backend for lab.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 3.1.0
;; Homepage: https://github.com/isamert/im-github.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1") (s "1.13.0"))

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

;; lab.el GitHub extension.

;; Important functions:
;; - `lab-list-all-owned-github-projects' → List all my projects.
;; - `lab-github-list-all-open-issues' → List all open issues of my projects.

;; This code also integrates itself with
;; `im-open-thing-at-point-alist' so that when you do
;; \\[im-open-thing-at-point] on a "user/repo#3" or "repo#3" it opens
;; it automatically (on the second one, user is assumed to be
;; `lab-github-user').

;; This code also provides a way to display project READMEs and ISSUES
;; inside Emacs in a nicely formatted markdown buffer.  See functions:
;; `lab-github-view-repo-readme' and `lab-github-issue-view'.  I
;; already integrated them in my config.

;;; Code:

(require 'lab)

;;; Customization

(defvar lab-github-user "isamert")

(defvar lab-github-token im-github-token)

(defvar lab-github-open-issues-projects-blacklist '("isamert.github.io" "addalias" "gedi" "gracer" "scli")
  "Projects that you want to filter out when you use `lab-github-list-all-open-issues'.")

;;; Core

(cl-defun lab-github-request (endpoint &key type success error data)
  (let (result)
    (request
      (s-replace-all
       `(("#{user}" . ,lab-github-user))
       (if (s-starts-with? "https" endpoint)
           endpoint
         (concat "https://api.github.com/" endpoint)))
      :sync (not success)
      :type type
      :parser #'(lambda ()
                  (json-parse-buffer
                   :object-type 'alist
                   :array-type 'list
                   :null-object nil))
      :headers `(("User-Agent" . "emacs")
                 ,@(when lab-github-token
                     `(("Authorization" . ,(format "Bearer %s" lab-github-token))
                       ("Accept" . "application/vnd.github+json")
                       ("X-GitHub-Api-Version" . "2022-11-28"))))
      :data data
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data)
                  (when success
                    (funcall success data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (if error
                    (funcall error error-thrown)
                  (message "Error: %S" error-thrown)))))
    result))

;;; Utils

(defun lab-github-repo-name-from-url (url)
  (string-trim-left (car (url-path-and-query (url-generic-parse-url (s-chop-suffix ".git" url)))) "/\\(repos/\\)?"))

(defun lab-github-interactive-url (prompt)
  (read-string prompt (or (eww-current-url) "")))

;;;###autoload
(defun lab-github-url-to-raw (url)
  "Convert a GitHub URL to its raw version."
  (when (string-match "https://github.com/\\([^/]+\\)/\\([^/]+\\)/blob/\\([^/]+\\)/\\(.+\\)" url)
    (let ((user (match-string 1 url))
          (repo (match-string 2 url))
          (branch (match-string 3 url))
          (path (match-string 4 url)))
      ;; Check if branch looks like a commit hash or should be a branch reference
      (setq branch
            (if (string-match-p "^[0-9a-f]\\{40\\}$" branch)
                branch
              (concat "refs/heads/" branch)))
      (format "https://raw.githubusercontent.com/%s/%s/%s/%s" user repo branch path))))

;;; Projects

(lab--define-actions-for github-project
  :formatter (lambda (it) (format "%s" (alist-get 'full_name it)))
  :keymap
  ((?o "Open"
       (lab--open-web-url .html_url))
   (?c "Clone"
       (lab-git-clone
        (alist-get 'ssh_url it)
        (read-directory-name "Directory to clone in: " lab-projects-directory)))
   (?p "Pull requests and issues"
       (lab-github-list-project-pull-requests .full_name))
   (?i "Inspect"
       (lab--inspect-obj it))))

;; TODO: instead of defining this function, can I make
;; `lab-github-project-act-on' interactive using the macro

;;;###autoload
(defun lab-github-act-on-project (repo-or-link)
  (interactive (list (lab-github-interactive-url "URL or repo name: ")))
  (lab-github-project-act-on (lab-github-request (format "repos/%s" (lab-github-repo-name-from-url repo-or-link)))))

;;;###autoload
(defun lab-github-list-all-owned-projects ()
  (interactive)
  (lab-github-project-select-and-act-on
   (lab-github-request "users/#{user}/repos?per_page=100")))

(defalias 'lab-github-list-my-projects #'lab-github-list-all-owned-projects)

;;;###autoload
(defun lab-github-list-project-pull-requests (full-repo-name)
  (interactive "sFull repo name: ")
  (lab-github-pull-request-select-and-act-on
   ;; TODO: Get all, not only open issues
   ;; &state=open
   (lab-github-request (format "repos/%s/issues?per_page=100&state=all" full-repo-name))))

;;; Pull requests & Issues & README & Files

(lab--define-actions-for github-pull-request
  :sort? nil
  :formatter
  (lambda (it)
    (let-alist it
      (format "%s%-5s %-7s [%-10s] - %s, by %s"
              (format "%-20s → " (format "%s#%s" (or .repository
                                                     .full_name
                                                     (nth 1 (s-split "/repos/" (or .repository_url "")))
                                                     "") .number))
              (format "%-5s"
                      (if (s-matches? "/issues/" .html_url)
                          (propertize "ISSUE" 'face '(:foreground "systemOrangeColor"))
                        (propertize "PR" 'face '(:foreground "systemBlueColor"))))
              (propertize (upcase .state) 'face `(:foreground ,(pcase .state
                                                                 ("open" "systemGreenColor")
                                                                 ("closed" "systemBrownColor"))))
              (substring .created_at 0 10)
              (propertize .title 'face 'bold)
              ;; graphql response → .author
              ;; rest response → .user
              (propertize
               (if (and .author (not (equal .author :null)))
                   (alist-get 'login .author)
                 .user.login)
               'face 'italic))))
  :keymap
  ((?v "View"
       (let-alist it
         (let ((repo (lab-github-repo-name-from-url .repository_url))
               (issue-no .number))
           (with-current-buffer (get-buffer-create (format "*lab-github: %s#%s %s*" repo issue-no .title))
             (erase-buffer)
             (insert (format "- Status :: %s " (s-titleize .state)))
             (insert-button
              (if (equal "closed" .state) "Re-open" "Close")
              'action
              (lambda (_button)
                (lab-github-update-issue
                 .url
                 :state (if (equal "closed" .state) "opened" "closed")
                 :reason (when (equal "opened" .state) "completed")
                 :success (lambda (_) (browse-url .html_url))))
              'face custom-button
              'follow-link t)
             (insert "\n")
             (insert (format "- Created at :: %s\n" .created_at))
             (insert (format "- [Reload](%s)\n" .html_url))
             (insert "- ")
             (insert-text-button
              "View on Github"
              'action (lambda (_button)
                        (funcall browse-url-secondary-browser-function .html_url))
              'follow-link t)
             (insert "\n")
             (insert "- ")
             (insert-text-button
              "Inspect"
              'action (lambda (_button) (lab--inspect-obj it))
              'follow-link t)
             (insert "\n\n")
             (insert (format "# *%s#%s* - %s by _%s_\n" repo issue-no .title .user.login))
             (insert (s-replace "\r\n" "\n" (or .body "")))
             (insert "\n\n")
             (--each (lab-github-request .comments_url)
               (let-alist it
                 (insert (format "## Comment by _%s_" .user.login))
                 (insert (format " [%s#%s-%s] " repo issue-no .id))
                 (insert-text-button
                  "*i*"
                  'action (lambda (_button)
                            (lab--inspect-obj it))
                  'follow-link t)
                 (insert "\n")
                 (insert (s-replace "\r\n" "\n" (or .body "")))
                 (when (equal .user.login lab-github-user)
                   (insert "\n\n")
                   (insert-button
                    "Edit"
                    'action
                    (lambda (_button)
                      (when (y-or-n-p "Want to send the update?")
                        (save-excursion
                          ;; TODO Get the updated comment and post it
                          (let ((comment (lab-github--get-user-comment "^## Comment by ")))
                            (lab-github-request
                             .url
                             :data (json-encode `((body . ,comment)))
                             ;; Refresh
                             :success (lambda (data) (browse-url .html_url))
                             :error (lambda (data) (user-error "Can't comment: %s" data)))))))
                    'face custom-button
                    'follow-link t))
                 (insert "\n\n")))
             (insert "# New Comment\n\n\n")
             (insert "⇒ ")
             (insert-button
              "Add"
              'action
              (lambda (_button)
                (lab-github-comment-on-issue
                 .comments_url (lab-github--get-user-comment "^# New Comment")
                 :success (lambda (data) (browse-url .html_url))))
              'face custom-button
              'follow-link t)
             (insert " ")
             (if (equal .state "closed")
                 (insert-button
                  "Add (OPEN)"
                  'action
                  (lambda (_button)
                    (lab-github-comment-on-issue
                     .comments_url (lab-github--get-user-comment "^# New Comment")
                     :success
                     (lambda (data)
                       (lab-github-update-issue .url :state "opened")
                       (browse-url .html_url))))
                  'face custom-button
                  'follow-link t)
               (insert-button
                "Add (DONE)"
                'action
                (lambda (_button)
                  (lab-github-comment-on-issue
                   .comments_url (lab-github--get-user-comment "^# New Comment")
                   :success
                   (lambda (data)
                     (lab-github-update-issue .url :state "closed" :reason "completed")
                     (browse-url .html_url))))
                'face custom-button
                'follow-link t)
               (insert " ")
               (insert-button
                "Add (WONTFIX)"
                'action
                (lambda (_button)
                  (lab-github-comment-on-issue
                   .comments_url (lab-github--get-user-comment "^# New Comment")
                   :success
                   (lambda (data)
                     (lab-github-update-issue .url :state "closed" :reason "not_planned")
                     (browse-url .html_url))))
                'face custom-button
                'follow-link t))
             (markdown-mode)
             (progn ;; Show first entry and it's body, hide all responses
               (outline-next-heading)
               (outline-hide-subtree)
               (outline-show-children)
               (outline-show-entry))
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))))))
   (?o "Open"
       (lab--open-web-url .html_url))
   (?c "Copy url"
       (kill-new .html_url))
   ;; TODO: This may not make sense to put here because we don't know
   ;; the current state of the issue. I can add these options if I add
   ;; the current state to `it'.
   ;; (?d "Close (COMPLETED)"
   ;;     (lab-github-request
   ;;      .url
   ;;      :data
   ;;      (json-encode
   ;;       `((state . "closed")
   ;;         (state_reason . "not_planned")))))
   ;; (?w "Close (WONTFIX)"
   ;;     (lab-github-request
   ;;      (progn (im-inspect it)
   ;;             (im-tap .url))
   ;;      :data
   ;;      (json-encode
   ;;       `((state . "closed")
   ;;         (state_reason . "completed")))))
   (?i "Inspect"
       (lab--inspect-obj it))))

(cl-defun lab-github-update-issue (url &key state reason success)
  "Update given issue.
STATE is either closed or opened.  REASON is one of completed,
not_planned, reopened or null.  Both are strings."
  (lab-github-request
   ;; "https://api.github.com/repos/isamert/playground/issues/1"
   url
   :data
   (json-encode
    `((state . ,state)
      (state_reason . ,reason)))
   :success success))

(defun lab-github--get-user-comment (header-regexp)
  "Used in issue view.
This assumes that this function is called on the button itself."
  (let ((pt (point)))
    (->>
     (buffer-substring-no-properties
      (save-excursion
        (re-search-backward header-regexp)
        (end-of-line)
        (point))
      pt)
     (s-trim)
     (s-lines)
     (-drop-last 1)
     (s-join "\n")
     (s-trim))))

(cl-defun lab-github-comment-on-issue (url comment &key success error)
  "COMMENT on URL."
  (lab-github-request
   url
   :data (json-encode `((body . ,comment)))
   :success success
   :error (or error (lambda (data) (user-error ">> Can't comment: %s" data)))))

;;;###autoload
(defun lab-github-issue-view (url-or-path)
  (interactive (list (lab-github-interactive-url "URL or repo name: ")))
  (lab--github-pull-request-view (lab-github-request (concat "repos/" (lab-github-repo-name-from-url url-or-path)))))

;;;###autoload
(defun lab-github-view-repo-readme (url-or-path)
  (interactive (list (lab-github-interactive-url "URL or repo name: ")))
  (let* ((repo (lab-github-repo-name-from-url url-or-path))
         (bufname (format "*lab-github: %s*" repo))
         (buf (progn
                (ignore-errors (kill-buffer bufname))
                (get-buffer-create bufname)))
         (readme (lab-github-request (format "repos/%s/readme" repo)))
         (repo-info (lab-github-request (format "repos/%s" repo)))
         (last-commits (lab-github-request (format "repos/%s/commits?per_page=3" repo)))
         (last-tags (lab-github-request (format "repos/%s/tags?per_page=3" repo))))
    (switch-to-buffer buf)
    (erase-buffer)
    (let-alist readme
      (insert-text-button
       "Reload"
       'action (lambda (_button)
                 (lab-github-view-repo-readme url-or-path))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Issues"
       'action (lambda (_button)
                 ;; TODO: use my implementation in the future
                 (consult-gh-issue-list repo))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Pull Requests"
       'action (lambda (_button)
                 ;; TODO: use my implementation in the future
                 (consult-gh-pr-list repo))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Browse Files"
       'action (lambda (_button)
                 ;; TODO: use my implementation in the future
                 (consult-gh-find-file repo))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Open externally"
       'action (lambda (_button)
                 (funcall browse-url-secondary-browser-function url-or-path))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Clone"
       'action (lambda (_button)
                 (lab-git-clone
                  (format "https://github.com/%s.git" repo)
                  (read-directory-name "Directory to clone in: " lab-projects-directory)))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Inspect (Readme)"
       'action (lambda (_button) (im-inspect readme))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Inspect (Repo)"
       'action (lambda (_button) (im-inspect repo-info))
       'follow-link t)
      (insert " | ")
      (insert-text-button
       "Inspect (Last Commits)"
       'action (lambda (_button) (im-inspect last-commits))
       'follow-link t)
      (insert "\n\n")
      (insert "**" repo "**" "\n**Other**\n\n")
      (ignore-errors
        (insert
         (format
          "```elisp\n(use-package %s :straight (:host github :repo \"%s\"))\n```"
          (car (s-split "\\." (nth 1 (s-split "/" repo))))
          repo)))
      (insert "\n\n")
      (insert "\n")
      (insert (base64-decode-string .content))
      (goto-char (point-min))
      (pcase (f-ext .name)
        ("org" (org-mode))
        ((or "md" "markdown")
         (markdown-view-mode)
         (let ((markdown-max-image-size (cons 900 500))
               (markdown-translate-filename-function
                (lambda (x)
                  (concat (s-chop-suffix .path .download_url) x))))
           (markdown-display-inline-images)))
        (_ (text-mode)))
      (page-break-lines-mode))
    ;; Needs to be called after the mode is initialized so that icons
    ;; are not mangled
    (let ((all-the-icons-default-adjust 0)
          (inhibit-read-only t))
      (goto-char (point-min))
      (forward-line 3)
      (insert
       (let-alist repo-info
         (concat
          "\n"
          (all-the-icons-faicon "star-o") " "
          (number-to-string .stargazers_count) " | "
          (all-the-icons-octicon "repo-forked") " "
          (number-to-string .forks_count) " | "
          (all-the-icons-octicon "issue-reopened") " "
          (number-to-string .open_issues) " | "
          (all-the-icons-octicon "law") " "
          .license.key " | "
          (all-the-icons-octicon "eye") " "
          (number-to-string .subscribers_count) " | "
          (all-the-icons-octicon "code") " "
          .language
          (unless (equal .archived :false)
            (concat " | " (all-the-icons-octicon "package") " archived"))
          "\n\n"
          (all-the-icons-octicon "rocket") " Created at :: "
          .created_at "\n"
          (all-the-icons-octicon "pulse") " Updated at :: "
          .updated_at "\n"
          (all-the-icons-octicon "flame") " Pushed at :: "
          .pushed_at "\n"
          "\n")))
      (insert
       "**Commits**\n\n")
      (dolist (it last-commits)
        (let-alist it
          (insert
           (all-the-icons-faicon "calendar") " "
           "*" .commit.author.date "* "
           (all-the-icons-faicon "user") " "
           "**" .commit.author.name "** "
           (all-the-icons-octicon "git-commit") " ")
          (insert-text-button
           (car (s-lines .commit.message))
           'action (lambda (_button)
                     ;; TODO: use my implementation in the future
                     (lab-github-show-diff-for-commit .html_url))
           'follow-link t)
          (insert "\n")))
      (insert "\n...\n\n")
      (insert
       "**Tags**\n\n"
       (s-join "\n" (--map (let-alist it (concat .name)) last-tags))
       "\n...\n\n"))))

(defun lab-github-show-diff-for-commit (html-url &optional description)
  (let ((patch (im-request (concat html-url ".patch") :-raw t))
        (buffer (get-buffer-create (format "*lab-github-commit-patch: %s *" description))))
    (with-current-buffer buffer
      (insert patch)
      (goto-char (point-min))
      (diff-mode)
      (read-only-mode)
      (switch-to-buffer (current-buffer)))))

;; TODO: Filter by date
;; https://docs.github.com/en/graphql/overview/explorer
;;;###autoload
(defun lab-github-list-all-open-issues ()
  "List all open GitHub issues belonging to my repositories."
  (interactive)
  (lab-github-request
   "graphql"
   :data
   (im-s-interpolated
    "{\"query\": \"query { user(login: \\\"#{lab-github-user}\\\") { repositories(first: 100) { nodes { name, issues(states: OPEN, first: 100) { nodes { title, url, number, body, state, createdAt, author { login }  } }, pullRequests(states: OPEN, first: 100) { nodes { title, url, number, body, state, createdAt, author { login }  } } } } } }\"}")
   :success
   (lambda (data)
     (lab-github-pull-request-select-and-act-on
      (--sort
       (string> (alist-get 'created_at it) (alist-get 'created_at other))
       (let-alist data
         (-mapcat
          (lambda (node)
            (let-alist node
              (seq-concatenate
               'list
               ;; Issues
               (--filter
                (not (-contains?
                      lab-github-open-issues-projects-blacklist
                      (alist-get 'repository it)))
                `(,@(--map `(,@it
                             (html_url . ,(im-s-interpolated "https://github.com/#{lab-github-user}/#{(alist-get 'name node)}/issues/#{(alist-get 'number it)}"))
                             (comments_url . ,(im-s-interpolated "https://api.github.com/repos/#{lab-github-user}/#{(alist-get 'name node)}/issues/#{(alist-get 'number it)}/comments"))
                             (created_at . ,(alist-get 'createdAt it))
                             (repository_url . ,(im-s-interpolated "https://api.github.com/repos/#{lab-github-user}/#{(alist-get 'name node)}"))
                             (repository . ,(alist-get 'name node)))
                           .issues.nodes)))
               ;; Pull Requests
               (--filter
                (not (-contains?
                      lab-github-open-issues-projects-blacklist
                      (alist-get 'repository it)))
                `(,@(--map `(,@it
                             (html_url . ,(im-s-interpolated "https://github.com/#{lab-github-user}/#{(alist-get 'name node)}/pulls/#{(alist-get 'number it)}"))
                             (comments_url . ,(im-s-interpolated "https://api.github.com/repos/#{lab-github-user}/#{(alist-get 'name node)}/issues/#{(alist-get 'number it)}/comments"))
                             (created_at . ,(alist-get 'createdAt it))
                             (repository_url . "https://api.github.com/repos/#{lab-github-user}/#{(alist-get 'name node)}")
                             (repository . ,(alist-get 'name node)))
                           .pullRequests.nodes))))))
          .data.user.repositories.nodes)))))
   :error (lambda (data) (message "NO! %s" data))))

(defun lab-github--get-author-login (author)
  (when (and author (not (equal author :null)))
    (alist-get 'login author)))

;;;###autoload
(defun org-dblock-write:github-issues (params)
  "Print GitHub issues in an org table.

PARAMS is a plist where:

:account' is the GitHub account name.  Can be nil, in that case
`lab-github-user' is used.

`:project' is the project name, like \"empv.el\".  Or it can be
nil or `all' (:project all, no quoting) which lists all open
issues for given `:account'."
  (let* (;; account is used below in im-s-interpolated
         (account (or (plist-get params :account) lab-github-user))
         (project (or (plist-get params :project) 'all))
         (all? (eq project 'all))
         (issues (lab-github-request
                  "graphql"
                  :data
                  (if all?
                      ;; Get all issues for the user
                      (im-s-interpolated
                       "{\"query\": \"query { user(login: \\\"#{account}\\\") { repositories(first: 100) { nodes { name, pullRequests(states: OPEN, first: 100) { nodes { title, url, number, state, createdAt, author { login }  } }, issues(states: OPEN, first: 100) { nodes { title, url, number, state, createdAt, author { login }  } } } } } }\"}")
                    ;; Get issues for the project
                    (im-s-interpolated
                     "{\"query\": \"query { user(login: \\\"#{account}\\\") { repository(name: \\\"#{project}\\\") { name, pullRequests(states: OPEN, first: 100) { nodes { title, url, number, state, createdAt, author { login }  } }, issues(states: OPEN, first: 100) { nodes { title, url, number, state, createdAt, author { login }  } } } } }\"}")))))
    (insert "| No. | Type | Title | By |\n")
    (insert "|-\n")
    (if all?
        (-each
            (--filter
             (not (-contains?
                   lab-github-open-issues-projects-blacklist
                   (alist-get 'name it)))
             (let-alist issues .data.user.repositories.nodes))
          (lambda (repo)
            (--each (let-alist repo .issues.nodes)
              (let-alist it
                (insert (format "| [[%s][%s#%s]] | ISSUE | %s | [[https://github.com/%s][%s]] |\n" .url (alist-get 'name repo)  .number .title (lab-github--get-author-login .author) (lab-github--get-author-login .author)))))
            (--each (let-alist repo .pullRequests.nodes)
              (let-alist it
                (insert (format "| [[%s][%s#%s]] | PR | %s | [[https://github.com/%s][%s]] |\n" .url (alist-get 'name repo)  .number .title (lab-github--get-author-login .author) (lab-github--get-author-login .author)))))))
      ;; else
      (--each
          (let-alist issues .data.user.repository.issues.nodes)
        (let-alist it
          (insert (format "| [[%s][#%s]] | ISSUE | %s | [[https://github.com/%s][%s]] |\n" .url .number .title (lab-github--get-author-login .author) (lab-github--get-author-login .author)))))
      (--each
          (let-alist issues .data.user.repository.pullRequests.nodes)
        (let-alist it
          (insert (format "| [[%s][#%s]] | PR | %s | [[https://github.com/%s][%s]] |\n" .url .number .title (lab-github--get-author-login .author) (lab-github--get-author-login .author)))))))
  (delete-char 1)
  (org-table-align))

(defun lab-github-view-repo-file (url)
  "Display GitHub file at URL in Emacs.
Supposed to be used within `browse-url-handlers'."
  (let* ((raw-url (lab-github-url-to-raw url))
         (temp-file (make-temp-file "github-raw-")))
    (url-copy-file raw-url temp-file t)
    (find-file temp-file)))

;;; Detect and open issues/comments etc.

;;;###autoload
(defun lab-github-issue-at-point (&optional str)
  "Return the issue comment at point or nil, if not found.

Use STR instead of text at point if not nil.

Returned value will be in the form of (OWNER PROJECT ISSUE
COMMENT) where OWNER and COMMENT are optional."
  (-let [(_ owner project issue comment)
         (s-match
          "\\([a-zA-Z0-9_\\.-]+/\\)?\\([a-zA-Z0-9_\\.-]+\\)#\\([0-9]+\\)+\\(-[0-9]+\\)?" (or str (im-region-or 'filename) ""))]
    (when (and project issue)
      (list (when owner (s-chop-suffix "/" owner)) project issue (when comment (s-chop-prefix "-" comment))))))

;;;###autoload
(defun lab-github-open-issue (issue)
  "Open the ISSUE, formatted like (OWNER PROJECT NO COMMENT-NO).
OWNER and COMMENT-NO is optional.  If OWNER is nil, then it's
assumed to be the `lab-github-user'."
  (interactive (list (lab-github-issue-at-point (read-string "Issue (repo#N): "))))
  (-let [(owner project issue comment) issue]
    (browse-url
     (format
      "https://github.com/%s/%s/issues/%s%s"
      (or owner lab-github-user)
      project
      issue
      (if comment (format "#issuecomment-%s" comment) "")))))

(provide 'im-github)
;;; im-github.el ends here
