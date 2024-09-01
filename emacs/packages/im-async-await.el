;;; im-async-await.el --- My async-await extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: async

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

;; My async-await extensions.

;;; Code:

(require 'async-await)

;;;###autoload
(defmacro async-cl-defun (name arglist &rest body)
  "Same as `async-defun' but use `cl-defun' to define the function.
For NAME, ARGLIST and BODY, see `async-defun'."
  (declare (doc-string 3) (indent 2))
  (cl-assert lexical-binding)
  (let* ((parsed-body (macroexp-parse-body body))
         (declarations (car parsed-body))
         (exps (macroexpand-all
                `(cl-macrolet
                     ((await (value)
                             `(async-await--check-return-value (iter-yield ,value))))
                   ,@(cdr parsed-body))
                macroexpand-all-environment)))
    `(cl-defun ,name ,arglist
       ,@declarations
       (async-await--awaiter
        (funcall (iter2-lambda () ,exps))))))

(provide 'im-async-await)

;;; im-async-await.el ends here
