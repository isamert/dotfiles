;;; im-couchbase.el --- Couchbase integration with sql.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-couchbase.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: sql, database, utils

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

;; Integrate Couchbase N1QL with sql.el and add `org-babel' support
;; for n1ql.
;;
;; * sql.el support
;;
;; It's not very polished, currently it can't highlight the JSON
;; output but the interactive window.
;;
;; You simply need to add a definition like following to
;; `sql-connection-alist':
;;
;;     ("<connection-name>"
;;         (sql-product 'n1ql)
;;         (sql-server   "http://<server-ip>:8091")
;;         (sql-user     "<user>")
;;         (sql-password "<password>"))
;;
;; Now you can do `sql-connect' and connect to "<connection-name>".
;;
;; This requires cbq binary to be in the PATH.  On MacOS, if you have
;; installed couchbase-server-community with brew, then cbq binary is
;; somehwere around:
;;
;;     /Applications/Couchbase Server.app/Contents/Resources/couchbase-core/bin/cbq
;;
;; Add this to PATH and you are good to go.
;;
;;
;; * org-babel support
;;
;; Unfortunately `ob-sql' is not very extensible, hence a new mode
;; called n1ql is introduced.
;;
;; Following should work:
;;
;;     #+begin_src n1ql :dbconnection 'dbcon
;;     select * from `bucket` limit 1;
;;     #+end_src
;;
;; where 'dbcon is from `sql-connection-alist'.  Alternatively you can
;; use:
;;
;;     #+begin_src n1ql :server "..." :user "..." :password "..."
;;     select * from `bucket` limit 1;
;;     #+end_src
;;
;; or combination of both:
;;
;;
;;     #+begin_src n1ql :dbconnection 'someconn  :user "..."
;;     select * from `bucket` limit 1;
;;     #+end_src
;;
;; where :user parameter from the block shadows the one from the
;; :dbconnection.

;;; Code:

(require 'sql)

;;;; Customization

(defcustom sql-n1ql-login-params
  `(user password server)
  "List of login parameters needed to connect to Postgres."
  :type 'sql-login-params
  :group 'im-couchbase)

(defcustom sql-n1ql-cbq-binary-path
  "cbq"
  "Path to cbq binary.

This requires cbq binary to be in the PATH.  On MacOS, if you have
installed couchbase-server-community with brew, then cbq binary is
somehwere around:

    /Applications/Couchbase Server.app/Contents/Resources/couchbase-core/bin/cbq"
  :type 'string
  :group 'im-couchbase)

;;;; sql.el integration

(sql-add-product
 'n1ql
 "Couchbase N1QL"
 :free-software nil
 :sqli-program sql-n1ql-cbq-binary-path
 :sqli-options nil
 :sqli-login 'sql-n1ql-login-params
 :sqli-comint-func 'sql-comint-n1ql
 :list-all "select RAW k.`path` from system:keyspaces k;"
 :prompt-regexp "^cbq> "
 :prompt-cont-regexp "^   >")

;; (sql-del-product 'n1ql)

;;;###autoload
(defun sql-n1ql (&optional buffer)
  "Run n1ql by cbq as an inferior process."
  (interactive "P")
  (sql-product-interactive 'n1ql buffer))

(defun sql-comint-n1ql (product options &optional buf-name)
  "Create comint buffer and connect to Couchbase."
  (let ((params
         (append
          (if (not (string= "" sql-server))
              (list "-e" sql-server))
          (if (not (string= "" sql-password))
              (list "-p" sql-password))
          (if (not (string= "" sql-user))
              (list "-u" sql-user))
          options)))
    (sql-comint product params buf-name)))

;;;; n1ql mode and org-mode support

;; Create a mode for n1ql and make n1ql code blocks inside org-mode
;; runnable.

;; Create a dummy derived mode based on sql-mode for n1ql, so that we
;; get some syntax highlighting for free

(define-derived-mode n1ql-mode sql-mode "n1ql-mode")
(add-to-list 'auto-mode-alist (cons (rx ".n1ql" string-end) #'n1ql-mode))

;;;###autoload
(defun org-babel-execute:n1ql (body params)
  "Function to execute n1ql code blocks in `org-mode'.
It works just like how `sql-mode' code blocks is executed.  It also
supports :dbconnection parameter just like sql code blocks, using
`sql-connection-alist'.

Also supports :select, where you can run an arbitrary JQ command on the
output."
  (let* ((conn (alist-get :dbconnection params))
         (db (or (alist-get conn sql-connection-alist nil nil #'equal)
                 (alist-get (symbol-name conn) sql-connection-alist nil nil #'equal) )))
    (im-cbq
     body
     :server (or (alist-get :server params)
                 (car (alist-get 'sql-server db)))
     :user (or (alist-get :username params)
               (car (alist-get 'sql-user db)))
     :password (or (alist-get :password params)
                   (car (alist-get 'sql-password db)))
     :select (alist-get :select params))))

;;;###autoload
(cl-defun im-cbq (query &key server user password select)
  "Run a couchbase query and return the result."
  (with-temp-buffer
    (insert query)
    (shell-command-on-region
     (point-min)
     (point-max)
     (format "%s -quiet -engine '%s' -credentials '%s'"
             sql-n1ql-cbq-binary-path
             server
             (format "%s:%s" user password))
     nil t)
    (replace-regexp-in-region "^cbq> " "" (point-min) (point-max))
    (when select
      (shell-command-on-region (point-min) (point-max) (format "jq -r '%s'" select) nil t))
    (buffer-string)))

;;;; Footer

(provide 'im-couchbase)
;;; im-couchbase.el ends here
