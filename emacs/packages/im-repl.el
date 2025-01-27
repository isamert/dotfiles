;;; im-repl.el --- REPL integration for all languages  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-repl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: TODO something

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

;; TODO: ...commentary...

;;; Code:

(require 'cl-lib)

;;;; Customization

;; TODO: ...customization...

;;;; Data Structure Definition

(cl-defstruct im-repl
  name       ; REPL name (e.g., "jshell")
  args       ; Command-line arguments
  prefix     ; Result prefix in echo area
  expr-fn    ; Function to get expression at point
  parser-fn  ; Function to parse process output
  prompt-re  ; Regexp to detect REPL prompt
  region-fn  ; Function to process region

  ;; Private
  (last-result nil)
  (accumulated-output nil)
  (process nil)
  (buffer nil))

;;;; Registry and State

(defvar im-repl-display-result-fn
  (lambda (prefix result)
    (message "%s %s" prefix (s-trim result))))

(defvar im-repl-registry (make-hash-table :test 'equal)
  "Registry of available REPL types.")

(defvar im-repl-pool (make-hash-table :test 'equal)
  "Running REPL pool.")

(defvar-local im-active-repl nil
  "Currently active REPL instance (struct).")

;;;; Core Implementation

(defun im-repl-start (repl-name)
  "Start a REPL by name."
  (interactive (list (completing-read "REPL type: " (map-keys im-repl-registry) nil t)))
  (let* ((repl (copy-im-repl (gethash repl-name im-repl-registry)))
         (buffer-name (generate-new-buffer-name (format "*im-%s-repl*" repl-name))))
    (map-put! im-repl-pool buffer-name repl)
    (setq-local im-active-repl repl)
    (setf (im-repl-buffer repl) buffer-name
          (im-repl-process repl) (make-process
                                  :name buffer-name
                                  :buffer buffer-name
                                  :command `(,(downcase repl-name) ,@(im-repl-args repl))
                                  :filter #'im--repl-filter
                                  :sentinel #'im--repl-sentinel))
    (with-current-buffer buffer-name
      (setq-local im-active-repl repl))

    (message "Started %s REPL" repl-name)))

(defun im-repl-send (input)
  "Send input to active REPL."
  (interactive "sInput: ")
  (let* ((repl im-active-repl)
         (process (im-repl-process repl)))
    (when (process-live-p process)
      (process-send-string process (concat input "\n"))
      (with-current-buffer (im-repl-buffer repl)
        (goto-char (process-mark process))
        (insert input "\n")
        (set-marker (process-mark process) (point))))))

(defun im-repl-all-running-repls ()
  (map-do
   (lambda (key repl)
     (when (not (process-live-p (im-repl-process repl)))
       (message "im-repl :: Removing dead REPL %s" key)
       (remhash key im-repl-pool)))
   im-repl-pool)
  (map-keys im-repl-pool))

(defun im-repl-eval ()
  "Evaluate expression at point or region."
  (interactive)
  (when (or (not im-active-repl)
            (not (process-live-p (im-repl-process im-active-repl))))
    (if-let* ((running-repls (im-repl-all-running-repls)))
        (setq-local
         im-active-repl
         (map-elt im-repl-pool (completing-read "Select a REPL: " running-repls nil t)))
      (call-interactively #'im-repl-start)))
  (let ((input
         (funcall (im-repl-region-fn im-active-repl)
                  (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (funcall (im-repl-expr-fn im-active-repl))))))
    (im-repl-send input)))

;; TODO: Kill all REPLS
;; TODO: Kill current REPL
;; TODO: Rename REPL?
;; TODO: Restart current REPL
;; TODO: Dump REPL state (simply dump all commands and apply them again?)

;;;; Process Handling

(defun im--repl-filter (process output)
  "Handle process output with active REPL's parser."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t)
            (repl im-active-repl))
        (setf (im-repl-accumulated-output repl)
              (concat (or (im-repl-accumulated-output repl) "") output))
        (save-excursion
          (goto-char (process-mark process))
          (insert (ansi-color-apply output))
          (set-marker (process-mark process) (point)))

        (when-let* ((prompt (im-repl-prompt-re repl))
                    (combined (im-repl-accumulated-output repl))
                    (prompt-pos (string-match prompt combined)))

          (let* ((result (substring combined 0 prompt-pos))
                 (remaining (substring combined (match-end 0)))
                 (parsed (ansi-color-apply (funcall (im-repl-parser-fn repl) result))))

            (setf (im-repl-accumulated-output repl) remaining)

            (when parsed
              (funcall im-repl-display-result-fn (im-repl-prefix repl) parsed)
              (setf (im-repl-last-result repl) parsed)
              (when (im-buffer-visible-p im-repl-result-buffer)
                (im-repl-inspect-last-result)))))))))

(defun im--repl-sentinel (process event)
  "Handle process state changes."
  (unless (process-live-p process)
    (message "%s REPL terminated" (process-name process))))

;;;; Registration API

(defun im-register-repl (name &rest args)
  "Register a new REPL type.
Example:
  (im-register-repl \"deno\"
    :args '(\"--quiet\")
    :prefix \"Deno>\"
    :expr-fn #'im-ts-current-expression
    :parser-fn (lambda (out) (s-trim (s-chop-suffix \"deno>\" out)))
    :prompt-re \"deno> \"
    :region-fn #'buffer-substring)"
  (puthash name (apply #'make-im-repl :name name args) im-repl-registry))

;;;; Interactive utils

(defconst im-repl-result-buffer "*im-repl-result*")
(defun im-repl-inspect-last-result ()
  "Inspect last REPL result in a separate buffer."
  (interactive)
  (unless im-active-repl
    (user-error "There is no active REPL associated with this buffer"))
  (let ((result (im-repl-last-result im-active-repl)))
    (with-current-buffer (get-buffer-create im-repl-result-buffer)
      (erase-buffer)
      (insert result)
      (goto-char (point-min))
      (unless (im-buffer-visible-p (current-buffer))
        (switch-to-buffer-other-window (current-buffer))))))

(defun im-repl-eval-buffer-top-level-forms ()
  "Eval buffer but only some selected types of forms.
Only tested with JS/TS."
  (interactive)
  (let ((top-level-forms '("import_statement"
                           "type_alias_declaration"
                           "function_declaration"
                           ;; TODO: lexical_declaration → Run only
                           ;; if function declarations or a
                           ;; constant?
                           "lexical_declaration"
                           "export_statement"
                           "interface_declaration"
                           "class_declaration")))
    (->>
     (treesit-buffer-root-node)
     (treesit-node-children)
     (--filter (-contains? top-level-forms (treesit-node-type it)))
     (--map (substring-no-properties (treesit-node-text it)))
     (-map #'im-repl-send))))

;;;; REPLs

;;;;; Deno

(im-register-repl
 "Deno"
 :args '()
 :prefix "=> "
 :prompt-re "> "
 :expr-fn
 (lambda ()
   (let ((curr (im-ts-current-expression))
         (this-command 'evil-paste-after))
     (im-pulse-highlight-region
      (treesit-node-start curr)
      (treesit-node-end curr)
      "turquoise"
      0.3)
     (treesit-node-text curr)))
 :parser-fn (lambda (str) (s-chop-suffix "\n> " str))
 :region-fn #'im-repl--deno-clear)

(defun im-repl--deno-clear (str)
  "Remove comments and newlines from STR, except when inside backticks, single quotes, or double quotes."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((inside-backticks nil)
          (inside-single-quotes nil)
          (inside-double-quotes nil)
          (inside-comment nil))
      (while (not (eobp))
        (pcase (char-after)
          (?`
           (unless (or inside-single-quotes inside-double-quotes)
             (setq inside-backticks (not inside-backticks)))
           (forward-char))
          (?'
           (unless (or inside-backticks inside-double-quotes)
             (setq inside-single-quotes (not inside-single-quotes)))
           (forward-char))
          (?\"
           (unless (or inside-backticks inside-single-quotes)
             (setq inside-double-quotes (not inside-double-quotes)))
           (forward-char))
          (?/
           (if (and (not inside-backticks)
                    (not inside-single-quotes)
                    (not inside-double-quotes)
                    (not inside-comment))
               (pcase (char-after (1+ (point)))
                 (?/
                  ;; Handle // comments
                  (setq inside-comment 'line)
                  (delete-char 2)
                  (while (and (not (eobp)) (not (eq (char-after) ?\n)))
                    (delete-char 1)))
                 (?*
                  ;; Handle /* comments
                  (setq inside-comment 'block)
                  (delete-char 2))
                 (_
                  (forward-char)))
             (forward-char)))
          (?*
           (if (and (eq inside-comment 'block)
                    (eq (char-after (1+ (point))) ?/))
               (progn
                 (setq inside-comment nil)
                 (delete-char 2))
             (forward-char)))
          (?\n
           (if (and (not inside-backticks)
                    (not inside-single-quotes)
                    (not inside-double-quotes)
                    (eq inside-comment 'line))
               (progn
                 (setq inside-comment nil)
                 (delete-char 1))
             (if (or inside-backticks inside-single-quotes inside-double-quotes)
                 (forward-char)
               (delete-char 1))))
          (_
           (if (and (not inside-backticks)
                    (not inside-single-quotes)
                    (not inside-double-quotes)
                    inside-comment)
               (delete-char 1)
             (forward-char))))))
    (buffer-string)))

;;;;; JShell

;; TODO: JShell
;; (im-create-repl
;;  :name "JShell"
;;  :args nil
;;  :prefix ""
;;  :process-region 'identity
;;  :input-regexp "jshell>"
;;  :expr (im-ts-current-expression)
;;  :parser
;;  (lambda (out)
;;    (->>
;;     out
;;     (s-split "\n")
;;     (-drop-last 1)
;;     (s-join "\n"))))

;;;; Footer

(provide 'im-repl)

;;; im-repl.el ends here
