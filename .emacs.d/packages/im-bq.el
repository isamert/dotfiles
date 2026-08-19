;;; im-bq.el --- My BigQuery extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))

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

;; This package contains my BigQuery extensions.

;; TODO: Document PARAMS (for `im-bq-run-query`, from `im-bq--command' + pass im-bq-project as default project id and let the caller override it)
;; TODO: Get rid of all dash.el and s.el usages
;; TODO: Get rid of
;; TODO: Add a readme, explaining:
;;   - SQL integration
;;   - Org babel integration
;;   - Major mode
;;   - Interactive commands (jobs status, table info, set project etc.)
;;   - im-bq-run-query
;; TODO: Add ability to run query at point (selected region or whole buffer, in major mode?)
;; TODO: Explore what else can be added, how can we let user discover datasets and their tables? Can we provide capf for major mode and interactive
;; TODO: Add sync option to babel runner, might be useful for exporting situations
;; TODO: Document prior art, what I've used from them:
;;   - [2026-08-18 Tue 16:16] https://github.com/christophstockhusen/bigquery-mode/blob/master/bigquery-mode.el
;;   - [2026-08-18 Tue 16:16] https://github.com/MartinNowak/sql-bigquery/blob/main/sql-bigquery.el

;;; Code:

(require 'sql)

;;;; Customization

(defgroup im-bq nil
  "BigQuery integration for running queries."
  :group 'SQL
  :prefix "im-bq-")

(defcustom im-bq-project nil
  "The project you want to use to execute queries."
  :type 'string
  :group 'im-bq)

(defcustom im-bq-program "bq"
  "Path to bq executable."
  :type 'file
  :group 'im-bq)

;;;;; SQL integration customs

(defcustom im-bq-sql-login-params '(database)
  "Parameters needed to connect to BigQuery."
  :type 'sql-login-params
  :group 'im-bq)

(defcustom im-bq-shell-options '("--format=pretty" "shell")
  "List of options for running bq in shell mode."
  :type '(repeat string)
  :group 'im-bq)

;;;;; Query customs

(defcustom im-bq-query-max-rows 1000
  "Max rows returned by queries.
Set this to nil to use whatever bq's own default is.  This setting does
not effect the bq shell, used by interactive sql buffer."
  :type 'number
  :group 'im-bq)

;;;; SQL integration

(defun im-bq-comint (product options &optional buffer-name)
  "Connect to BigQuery in a comint buffer."
  (let ((params (append (when (and im-bq-project (not (string= "" im-bq-project)))
                          `("--project_id", im-bq-project))
                        options)))
    (sql-comint product params buffer-name)))

;;;###autoload
(defun sql-bigquery (&optional buffer)
  "Run BigQuery as an inferior process.
The buffer with name BUFFER will be used or created."
  (interactive "P")
  (sql-product-interactive 'bigquery buffer))

(sql-add-product
 'bigquery "BigQuery"
 :prompt-regexp "^[^>]+> "
 :prompt-cont-regexp "^[ ]+-> "
 :sqli-comint-func #'im-bq-comint
 :font-lock 'sql-mode-ansi-font-lock-keywords
 :sqli-login nil
 :sqli-program 'im-bq-program
 :sqli-options 'im-bq-shell-options
 :input-filter '(sql-escape-newlines-filter)
 '(:free-software t))

;; (sql-del-product 'bigquery)
;; (sql-product-interactive 'bigquery)

;;;; Interactive helpers

(defun im-bq-switch-project (project)
  "Switch to PROJECT.
After project is set, this project will be used for all BigQuery
commands, including the SQL integration."
  (interactive
   (list
    (read-string
     (format "GCloud project (current: %s): "
             (if-let* ((it im-bq-project)) it "default")))))
  (setq im-bq-project (if (string-blank-p project) nil project)))

;;;; Major mode

(define-derived-mode bqsql-mode sql-mode "bqsql-mode"
  "A major mode for editing and running BigQuery SQLs.")
(add-to-list 'auto-mode-alist (cons (rx ".bqsql" string-end) #'bqsql-mode))

;;;; Query runner

(defun im-bq--command (query params output-buffer job-id)
  "Return a `start-process' command for QUERY."
  (let ((dry-run? (alist-get :dry-run params))
        (format (or (alist-get :format params) "org-table"))
        (api (alist-get :api params))
        (project-id (alist-get :project-id params)))
    `("query" ,output-buffer "bq" "query"
      ,@(when api `("--api" ,api))
      ,@(when dry-run? '("--dry_run"))
      ,@(when project-id `("--project_id" ,project-id))
      ,@(when im-bq-query-max-rows `("--max_rows" ,im-bq-query-max-rows))
      "--quiet" "--nouse_legacy_sql"
      "--format" ,(pcase format
                    ((or "org-table" "table") "pretty")
                    (x x))
      "--job_id" ,job-id
      ,query)))

(defun im-bq--command-string (command)
  "Render COMMAND, as returned by `im-bq--command', for display."
  (string-join
   (--map (if (not (s-prefix? "--" it))
              (format "\"%s\"" (s-replace "\"" "\\\"" it))
            it)
          (-drop 2 command))
   " "))

(defun im-bq-run-query (query params callback)
  "Run BQ QUERY with PARAMS.
CALLBACK is called asynchronously as:

  (CALLBACK RESULT META)

RESULT is the command output as a string.  META is a plist containing
`:job-id', `:elapsed', and `:exit-status'.

Return the BigQuery job id immediately."
  (let* ((job-id (im-uuid))
         (output-buffer (generate-new-buffer
                         (format " *im-big-querysql:%s*" job-id)))
         (start-time (float-time))
         (command (im-bq--command query params output-buffer job-id)))
    im-bq-program
    (let ((process (apply #'start-process command)))
      (set-process-sentinel
       process
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let* ((end-time (float-time))
                  (result (when (buffer-live-p output-buffer)
                            (with-current-buffer output-buffer
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
                  (meta (list :job-id job-id
                              :elapsed (- end-time start-time)
                              :exit-status (process-exit-status process))))
             (unwind-protect
                 (funcall callback result meta)
               (when (buffer-live-p output-buffer)
                 (kill-buffer output-buffer)))))))
      job-id)))

;;;; Babel integration

(defun org-babel-expand-body:bqsql (body params)
  (s-format
   body
   (lambda (key alist) (assoc-default (intern key) alist))
   (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params)))

(defun org-babel-execute:bqsql (query params)
  "Execute QUERY with given PARAMS.
`:var' syntax is ${var_name} and replaced as-is.

`:format' can be either `org-table', `table' or `json'.  Former outputs
an org table, other one outputs the result as json.  By default, it's
`org-table'.  `org-table' means the output is a regular org mode table.
`table' means the output is `table.el' formatted table (it is actually
what is returned from bq command with the `pretty' formatting option).

If `:buffer' is non-nil, then output results to a buffer, instead
of the results drawer.

If `:cmd' is non-nil, then instead of executing query, print out
the resulting bq command."
  (let* ((format (or (alist-get :format params) "org-table"))
         (buffer? (alist-get :buffer params))
         (cmd? (alist-get :cmd params))
         (json-out? (string= format "json"))
         (org-buffer (current-buffer)))
    (setq query (org-babel-expand-body:bqsql query params))

    (when cmd?
      (let* ((job-id (im-uuid))
             (command (im-bq--command query params nil job-id)))
        (org-babel-insert-result (im-bq--command-string command))
        (user-error "Done")))
    (im-bq-run-query
     query params
     (lambda (result meta)
       (let* ((job-id (plist-get meta :job-id))
              (elapsed (plist-get meta :elapsed))
              (result (if (string= format "org-table")
                          (im-bq--pretty-table-to-org result)
                        result))
              (msg (format "=> Query finished, time elapsed: %s"
                           (format-seconds "%Y %D %H %M %z%S" elapsed)))
              (bname (format "*bqsql:%s"
                             (if (eq buffer? t) job-id buffer?))))
         (if (not (buffer-live-p org-buffer))
             (with-current-buffer (get-buffer-create bname)
               (erase-buffer)
               (insert result)
               (message "Org buffer is gone; result inserted into %s" bname))

           (with-current-buffer org-buffer
             (save-excursion
               (goto-char (point-max))
               (let ((found? (re-search-backward job-id nil t)))
                 (when (or buffer? (not found?))
                   (with-current-buffer (get-buffer-create bname)
                     (erase-buffer)
                     (insert result)
                     (if json-out?
                         (json-ts-mode)
                       (org-mode)
                       (im-disable-line-wrapping))
                     (setq header-line-format msg))
                   (unless found?
                     (message "Org block is gone; result inserted into %s"
                              bname)))

                 (when found?
                   (forward-line -4)
                   (org-babel-insert-result
                    (if buffer? msg result)
                    (list "replace"
                          (cond
                           ((s-prefix? "Error" result) "drawer")
                           (buffer? "drawer")
                           (json-out? "lang")
                           (t "raw")))
                    nil nil
                    (when json-out? "json")))

                 (when buffer?
                   (switch-to-buffer-other-window bname)))))))))))

(defun im-bq--pretty-table-to-org (result)
  "Convert BQ's pretty table RESULT into an Org table."
  (with-temp-buffer
    (insert result)
    (goto-char (point-min))
    (skip-chars-forward "\n\t ")
    (when (looking-at "^\\+")
      (kill-line 1)
      (forward-line 1)
      (delete-char 1)
      (insert "|")
      (end-of-line)
      (delete-char -1)
      (insert "|")
      (goto-char (point-max))
      (skip-chars-backward "\n\t ")
      (beginning-of-line)
      (when (looking-at "^\\+")
        (kill-line 1)))
    (buffer-string)))

;;;; Footer

(provide 'im-bq)

;;; im-bq.el ends here
