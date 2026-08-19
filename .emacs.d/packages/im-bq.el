;;; im-bq.el --- BigQuery integration -*- lexical-binding: t; -*-

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

;; im-bq provides several complementary ways to use Google BigQuery
;; from Emacs.  It requires the `bq' executable from the Google Cloud
;; CLI, already installed and authenticated outside Emacs.  It's async
;; by default, so your Emacs is not frozen while running queries.
;;
;; * SQL integration
;;
;; `sql-bigquery' registers the BigQuery shell as an `sql.el' product.
;; It opens an SQLi/comint buffer and uses `im-bq-project' as its
;; project.  This is useful for an exploratory, persistent SQL
;; session.
;;
;; * Org Babel integration
;;
;; Source blocks named `bqsql' are executed by
;; `org-babel-execute:bqsql'.  `${name}' occurrences are expanded from
;; `:var name=value' headers.  Babel execution is asynchronous by
;; default; use `:sync yes' (or `:async no') for blocking execution,
;; in particular while exporting.  Other useful headers are
;; `:project-id', `:format', `:buffer', `:dry-run', and `:cmd'.
;;
;; * Major mode
;;
;; `bqsql-mode' derives from `sql-mode'.  It primarily gives Org a
;; mode for editing bqsql source blocks, but it is also useful for
;; standalone .bqsql files (It's a made-up format, you can also simply
;; do `M-x bqsql-mode' in any .sql file).  In that mode `C-c C-c'
;; executes the region, or the whole buffer when no region is active.
;;
;; * Interactive commands
;;
;; - `im-bq-switch-project' selects the default project for current
;;   Emacs session.
;; - `im-bq-job-status' and `im-bq-cancel-job' inspect and cancel jobs.
;; - `im-bq-table-info' shows table metadata and sample rows.
;;
;; * Programmatic queries
;;
;; `im-bq-run-query' is the asynchronous API.  It accepts SQL, a
;; parameter alist, and a callback.  `im-bq-run-query-sync' is its
;; blocking counterpart.  Both expose metadata including the BigQuery
;; job id and exit status (through the callback for asynchronous
;; queries).
;;
;; Prior art:
;; - https://github.com/christophstockhusen/bigquery-mode/blob/master/bigquery-mode.el
;; - https://github.com/MartinNowak/sql-bigquery/blob/main/sql-bigquery.el
;;
;; They mostly seem to be not maintained right now.  I incorporated
;; some ideas from them into this package.

;;; Code:

(require 'seq)
(require 'sql)
(require 'org-macs)
(eval-when-compile (require 'subr-x))

;;;; Customization

(defgroup im-bq nil
  "BigQuery integration for running queries."
  :group 'SQL
  :prefix "im-bq-")

(defcustom im-bq-project nil
  "Default project id for BigQuery commands.
When nil, let the bq executable use its configured default project."
  :type '(choice (const :tag "bq default" nil) string)
  :group 'im-bq)

(defcustom im-bq-program "bq"
  "Path to the bq executable."
  :type 'file
  :group 'im-bq)

;;;;; SQL integration customs

(defcustom im-bq-shell-options '("--format=pretty" "shell")
  "List of options for running bq in shell mode."
  :type '(repeat string)
  :group 'im-bq)

;;;;; Query customs

(defcustom im-bq-query-max-rows 1000
  "Maximum number of rows returned by queries.
Set this to nil to use bq's own default.  This does not affect the bq
shell used by the interactive SQL buffer."
  :type '(choice (const :tag "bq default" nil) integer)
  :group 'im-bq)

(defcustom im-bq-babel-async t
  "Whether bqsql Babel blocks execute asynchronously by default.
A block can override this with `:sync yes' or `:async no'."
  :type 'boolean
  :group 'im-bq)

;;;; SQL integration

(defun im-bq-comint (product options &optional buffer-name)
  "Connect PRODUCT to BigQuery with OPTIONS in BUFFER-NAME."
  (let ((params (append (when (and im-bq-project
                                   (not (string-empty-p im-bq-project)))
                          (list "--project_id" im-bq-project))
                        options)))
    (sql-comint product params buffer-name)))

;;;###autoload
(defun sql-bigquery (&optional buffer)
  "Run BigQuery as an inferior process in BUFFER."
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
 :input-filter '(sql-escape-newlines-filter))

;;;; Query runner

(defun im-bq--param (key params default)
  "Return KEY from PARAMS, or DEFAULT when KEY is absent.
Unlike `alist-get', an explicitly supplied nil overrides DEFAULT."
  (let ((cell (assq key params)))
    (if cell (cdr cell) default)))

(defun im-bq--true-p (value)
  "Return non-nil when VALUE represents an affirmative value."
  (or (eq value t)
      (and (numberp value) (not (zerop value)))
      (and (stringp value)
           (member (downcase value) '("yes" "true" "t" "on" "1")))))

(defun im-bq--command (query params output-buffer job-id)
  "Return a `start-process' command for QUERY.
PARAMS has the same keys documented by `im-bq-run-query'.
OUTPUT-BUFFER receives process output and JOB-ID identifies the BQ job."
  (let* ((dry-run (im-bq--true-p (alist-get :dry-run params)))
         (format (format "%s" (or (alist-get :format params) "org-table")))
         (api (alist-get :api params))
         (project-id (im-bq--param :project-id params im-bq-project)))
    (append
     (list "im-bq-query" output-buffer im-bq-program "query")
     (when api (list "--api" (format "%s" api)))
     (when dry-run (list "--dry_run"))
     (when (and project-id (not (string-empty-p (format "%s" project-id))))
       (list "--project_id" (format "%s" project-id)))
     (when im-bq-query-max-rows
       (list "--max_rows" (number-to-string im-bq-query-max-rows)))
     (list "--quiet" "--nouse_legacy_sql"
           "--format" (pcase format
                        ((or "org-table" "table") "pretty")
                        (other other))
           "--job_id" job-id query))))

(defun im-bq--command-string (command)
  "Render COMMAND, as returned by `im-bq--command', for display."
  (mapconcat #'shell-quote-argument (nthcdr 2 command) " "))

;;;###autoload
(defun im-bq-run-query (query params callback)
  "Run BigQuery QUERY asynchronously with PARAMS.
PARAMS is an alist which recognizes these keys:

  `:api'         Value for bq's --api option.
  `:dry-run'     Non-nil (or \"yes\") to add --dry_run.
  `:format'      Output format: \"org-table\", \"table\", \"json\", or a
                 format accepted by bq.  It defaults to \"org-table\".
  `:project-id'  Project used for the job.  When this key is absent,
                 `im-bq-project' is passed as the default project id.
                 A caller may override it, including with explicit nil.

CALLBACK is called as (CALLBACK RESULT META) after the process exits.
RESULT is combined command output as a string.  META is a plist with
`:job-id', `:elapsed', and `:exit-status'.

Return the BigQuery job id immediately."
  (let* ((job-id (org-id-uuid))
         (output-buffer (generate-new-buffer
                         (format " *im-bq-query:%s*" job-id)))
         (start-time (float-time))
         (command (im-bq--command query params output-buffer job-id))
         (process (apply #'start-process command)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let* ((result (when (buffer-live-p output-buffer)
                          (with-current-buffer output-buffer
                            (buffer-substring-no-properties
                             (point-min) (point-max)))))
                (meta (list :job-id job-id
                            :elapsed (- (float-time) start-time)
                            :exit-status (process-exit-status process))))
           (unwind-protect
               (funcall callback (or result "") meta)
             (when (buffer-live-p output-buffer)
               (kill-buffer output-buffer)))))))
    job-id))

;;;###autoload
(defun im-bq-run-query-sync (query params)
  "Run BigQuery QUERY synchronously with PARAMS.
PARAMS recognizes the same keys as `im-bq-run-query'.  Return a cons
cell (RESULT . META), where META contains `:job-id', `:elapsed', and
`:exit-status'."
  (let* ((job-id (org-id-uuid))
         (start-time (float-time))
         (command (im-bq--command query params nil job-id))
         (program (nth 2 command))
         (args (nthcdr 3 command)))
    (with-temp-buffer
      (let ((exit-status (apply #'process-file program nil (list t t) nil args)))
        (cons (buffer-substring-no-properties (point-min) (point-max))
              (list :job-id job-id
                    :elapsed (- (float-time) start-time)
                    :exit-status exit-status))))))

;;;; Babel integration

(declare-function org-babel--get-vars "ob-core" (params))
(declare-function org-babel-insert-result "ob-core"
                  (result &optional result-params info hash lang exec-time))
(declare-function org-in-src-block-p "org" (&optional inside element))

(defvar org-babel-default-header-args:bqsql
  '((:results . "replace raw"))
  "Default header arguments for bqsql Babel blocks.")

;;;###autoload
(defun org-babel-expand-body:bqsql (body params)
  "Expand `${name}' variables in BODY according to Babel PARAMS."
  (let ((vars (org-babel--get-vars params)))
    (replace-regexp-in-string
     "\\${[^}]+}"
     (lambda (match)
       (let* ((name (substring match 2 -1))
              (binding
               (seq-find (lambda (pair)
                           (string= name (format "%s" (car pair))))
                         vars)))
         (if binding (format "%s" (cdr binding)) "")))
     body t t)))

(defun im-bq--babel-buffer-option (params)
  "Normalize the `:buffer' value in Babel PARAMS."
  (let ((value (alist-get :buffer params)))
    (cond
     ((or (null value)
          (and (stringp value)
               (member (downcase value) '("no" "false" "nil" "off" "0"))))
      nil)
     ((im-bq--true-p value) t)
     (t value))))

(defun im-bq--babel-async-p (params)
  "Return whether Babel PARAMS request asynchronous execution."
  (cond
   ((im-bq--true-p (alist-get :sync params)) nil)
   ((assq :async params) (im-bq--true-p (alist-get :async params)))
   (t im-bq-babel-async)))

(defun im-bq--format-result (result format)
  "Convert RESULT according to FORMAT."
  (if (equal format "org-table")
      (im-bq--pretty-table-to-org result)
    result))

(defun im-bq--prepare-result-buffer (name result format header)
  "Put RESULT in buffer NAME using FORMAT and HEADER, then return it."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (json? (equal format "json")))
        (erase-buffer)
        (insert result)
        (goto-char (point-min))
        (cond
         ((equal format "org-table") (org-mode))
         ((and json? (fboundp 'json-ts-mode))
          (json-ts-mode))
         ((and json? (fboundp 'json-mode))
          (json-mode))
         ((and json? (fboundp 'js-mode))
          (js-mode))
         (t (special-mode))))
      (setq-local truncate-lines t)
      (setq header-line-format header))
    buffer))

(defun im-bq--elapsed-message (meta)
  "Return a completion message for query META."
  (format "Query %s finished in %s (exit %s)"
          (plist-get meta :job-id)
          (format-seconds "%Y %D %H %M %z%S" (plist-get meta :elapsed))
          (plist-get meta :exit-status)))

;;;###autoload
(defun org-babel-execute:bqsql (query params)
  "Execute BigQuery QUERY according to Babel PARAMS.

Variables use `${var_name}' syntax and are replaced as-is from `:var'
headers.  `:format' may be `org-table' (the default), `table', `json',
or another bq output format.  `:project-id' overrides `im-bq-project'.

Execution is asynchronous by default.  Use `:sync yes' or `:async no'
to block and return the result normally to Babel; this is suitable for
export.  `:buffer yes' writes output to a job-named buffer, while any
other non-false `:buffer' value names that buffer.  `:cmd yes' returns
the command without running it.  `:dry-run yes' asks bq for a dry run."
  (let* ((format (format "%s" (or (alist-get :format params) "org-table")))
         (buffer-option (im-bq--babel-buffer-option params))
         (command-only (im-bq--true-p (alist-get :cmd params)))
         (async (im-bq--babel-async-p params))
         (org-buffer (current-buffer))
         (source-marker (copy-marker (point)))
         (expanded-query (org-babel-expand-body:bqsql query params)))
    (if command-only
        (im-bq--command-string
         (im-bq--command expanded-query params nil (org-id-uuid)))
      (if (not async)
          (let* ((response (im-bq-run-query-sync expanded-query params))
                 (result (im-bq--format-result (car response) format))
                 (meta (cdr response))
                 (message (im-bq--elapsed-message meta)))
            (unless (zerop (plist-get meta :exit-status))
              (user-error "BigQuery failed: %s" (string-trim result)))
            (if buffer-option
                (let* ((name (format "*bqsql:%s*"
                                     (if (eq buffer-option t)
                                         (plist-get meta :job-id)
                                       buffer-option)))
                       (buffer (im-bq--prepare-result-buffer
                                name result format message)))
                  (display-buffer buffer)
                  message)
              result))
        (im-bq-run-query
         expanded-query params
         (lambda (raw-result meta)
           (let* ((result (im-bq--format-result raw-result format))
                  (message (im-bq--elapsed-message meta))
                  (job-id (plist-get meta :job-id))
                  (buffer-name (format "*bqsql:%s*"
                                       (if (eq buffer-option t)
                                           job-id
                                         (or buffer-option job-id))))
                  result-buffer)
             (when (or buffer-option
                       (not (and (buffer-live-p org-buffer)
                                 (marker-position source-marker))))
               (setq result-buffer
                     (im-bq--prepare-result-buffer
                      buffer-name result format message)))
             (if (and (buffer-live-p org-buffer)
                      (marker-position source-marker))
                 (with-current-buffer org-buffer
                   (save-excursion
                     (goto-char source-marker)
                     (if (ignore-errors (org-in-src-block-p))
                         (org-babel-insert-result
                          (if buffer-option message result)
                          (list "replace"
                                (cond
                                 ((not (zerop (plist-get meta :exit-status)))
                                  "drawer")
                                 (buffer-option "drawer")
                                 ((string= format "json") "code")
                                 (t "raw")))
                          nil nil
                          (when (string= format "json") "json"))
                       (unless result-buffer
                         (setq result-buffer
                               (im-bq--prepare-result-buffer
                                buffer-name result format message)))
                       (message "Source block is gone; result is in %s"
                                buffer-name))))
               (message "Org buffer is gone; result is in %s" buffer-name))
             (when buffer-option
               (display-buffer (or result-buffer
                                   (get-buffer buffer-name))))
             (set-marker source-marker nil))))))))

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

;;;; Interactive commands

(defun im-bq--identifier-at-point ()
  "Return the BigQuery-like identifier at point, if any."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion
      (let ((end (progn (skip-chars-forward "-[:alnum:]_.$:") (point)))
            (start (progn (skip-chars-backward "-[:alnum:]_.$:") (point))))
        (unless (= start end)
          (buffer-substring-no-properties start end))))))

;;;###autoload
(defun im-bq-switch-project (project)
  "Switch the default BigQuery project to PROJECT.
An empty PROJECT makes bq use its own configured default.  This affects
query execution and SQLi integration."
  (interactive
   (list (read-string
          (format "BigQuery project (current: %s): "
                  (or im-bq-project "bq default"))
          im-bq-project)))
  (setq im-bq-project (unless (string-blank-p project) project))
  (message "BigQuery project: %s" (or im-bq-project "bq default")))

(defun im-bq--read-value (prompt)
  "Read a value with PROMPT, defaulting to the region or identifier."
  (read-string prompt (im-bq--identifier-at-point)))

(defun im-bq--display-process (buffer-name process-name args)
  "Run bq with ARGS, displaying PROCESS-NAME output in BUFFER-NAME."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)))
    (let ((process (apply #'start-process process-name buffer
                          im-bq-program args)))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (goto-char (point-min))
               (setq header-line-format
                     (format "%s exited with status %s"
                             process-name (process-exit-status proc)))
               (special-mode))))))
      (display-buffer buffer)
      buffer)))

;;;###autoload
(defun im-bq-job-status (job-id)
  "Display status information for BigQuery JOB-ID.
With a prefix argument, request pretty JSON output."
  (interactive (list (im-bq--read-value "Job id: ")))
  (im-bq--display-process
   (format "*bq job status: %s*" job-id) "im-bq-job-status"
   (append (list "show")
           (when current-prefix-arg (list "--format=prettyjson"))
           (list "-j" job-id))))

;;;###autoload
(defun im-bq-cancel-job (job-id)
  "Cancel the BigQuery job JOB-ID and display bq's response."
  (interactive (list (im-bq--read-value "Job id: ")))
  (im-bq--display-process
   (format "*bq cancel job: %s*" job-id) "im-bq-cancel-job"
   (list "cancel" job-id)))

(defun im-bq--cli-table-name (table-name)
  "Convert SQL TABLE-NAME to the project:dataset.table CLI notation."
  (setq table-name (string-trim table-name "`+" "`+"))
  (if (string-match
       "\\`\\([^.]+\\)\\.\\([^.]+\\)\\.\\(.+\\)\\'" table-name)
      (concat (match-string 1 table-name) ":"
              (match-string 2 table-name) "."
              (match-string 3 table-name))
    table-name))

;;;###autoload
(defun im-bq-table-info (table-name)
  "Display metadata and the first 200 rows of TABLE-NAME."
  (interactive
   (list (read-string "Table: " (im-bq--identifier-at-point))))
  (setq table-name (im-bq--cli-table-name table-name))
  (let* ((buffer-name (format "*bq table info: %s*" table-name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Metadata for %s\n\n" table-name))))
    (let ((process (start-process "im-bq-table-info" buffer im-bq-program
                                  "show" table-name)))
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel
       process
       (lambda (show-process _event)
         (when (memq (process-status show-process) '(exit signal))
           (if (zerop (process-exit-status show-process))
               (progn
                 (with-current-buffer buffer
                   (goto-char (point-max))
                   (insert "\n\nFirst 200 rows\n\n"))
                 (let ((head-process
                        (start-process "im-bq-table-head" buffer im-bq-program
                                       "head" "-n" "200" table-name)))
                   (set-process-query-on-exit-flag head-process nil)
                   (set-process-sentinel
                    head-process
                    (lambda (proc _event)
                      (when (memq (process-status proc) '(exit signal))
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (goto-char (point-min))
                            (setq header-line-format
                                  (format "bq head exited with status %s"
                                          (process-exit-status proc)))
                            (special-mode))))))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (goto-char (point-min))
                 (setq header-line-format
                       (format "bq show exited with status %s"
                               (process-exit-status show-process)))
                 (special-mode)))))))
      (display-buffer buffer))))

;;;###autoload
(defun im-bq-run-query-at-point (begin end &optional synchronous)
  "Run the active region, or the whole buffer, as a BigQuery query.
BEGIN and END delimit the selected text.  With prefix argument
SYNCHRONOUS, block until bq exits."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (point-min) (point-max) current-prefix-arg)))
  (let* ((query (buffer-substring-no-properties begin end))
         (params '((:format . "table"))))
    (if synchronous
        (let* ((response (im-bq-run-query-sync query params))
               (meta (cdr response))
               (buffer (im-bq--prepare-result-buffer
                        (format "*bq query:%s*" (plist-get meta :job-id))
                        (car response) "table"
                        (im-bq--elapsed-message meta))))
          (display-buffer buffer))
      (im-bq-run-query
       query params
       (lambda (result meta)
         (display-buffer
          (im-bq--prepare-result-buffer
           (format "*bq query:%s*" (plist-get meta :job-id))
           result "table" (im-bq--elapsed-message meta))))))))

(defalias 'im-bq-query-at-point #'im-bq-run-query-at-point)

;;;; Major mode

(defvar bqsql-mode-map (make-sparse-keymap)
  "Keymap for `bqsql-mode'.")
(set-keymap-parent bqsql-mode-map sql-mode-map)
(define-key bqsql-mode-map (kbd "C-c C-c") #'im-bq-run-query-at-point)
(define-key bqsql-mode-map (kbd "C-c C-p") #'im-bq-switch-project)

;;;###autoload
(define-derived-mode bqsql-mode sql-mode "BQSQL"
  "Major mode for editing and running Google BigQuery SQL.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bqsql\\'" . bqsql-mode))

;; `bqsql' already should work, also support "bigquery"
(defvar org-src-lang-modes)
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("bigquery" . bqsql)))

;;;; Footer

(provide 'im-bq)

;;; im-bq.el ends here
