;;; im-gnuplot.el --- Gnuplot macro  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: utility, plotting

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

;; Provides an interoperable gnuplot macro to use in Elisp.
;;
;; Example usage (assuming you have `datas'):
;;
;;     (gnuplot
;;      (set 'terminal (raw "pngcairo size 1080,500")
;;           'output output
;;           'title "some-title"
;;           'xdata 'time
;;           'timefmt "%s"
;;           'bmargin 5)
;;      (set1 'format 'x "%Y-%m-%d %H:%M:%S")
;;      (set1 'xtics 'font ",8")
;;      (set1 'xtics 'rotate 'by 45)
;;      (set1 'xtics 'offset (raw "0, -3"))
;;      (set1 'grid)
;;      (apply #'plot (--map
;;                     (curve (plist-get it :file) 'using (raw "($1/1000):2") 'with 'lines 'title (plist-get it :label))
;;                     datas)))
;;
;; Available functions:
;;
;; - plot
;; - set
;; - set1
;; - raw

;;; Code:

(defun im-gnuplot--render-value (v)
  (cond
   ((and (consp v) (eq (car v) :gnuplot-raw)) (cadr v))
   ((stringp v) (prin1-to-string v))
   ((symbolp v) (symbol-name v))
   (t (format "%s" v))))

(defmacro gnuplot (&rest body)
  `(cl-flet
       ((raw (s) (list :gnuplot-raw s))
        (set (&rest options)
             (mapconcat
              (lambda (pair)
                (format "set %s %s"
                        (symbol-name (car pair))
                        (im-gnuplot--render-value (cdr pair))))
              (let ((lst options))
                (cl-loop for (k v) on lst by #'cddr collect (cons k v)))
              "\n"))
        (set1 (key &rest options)
              (format "set %s%s"
                      (symbol-name key)
                      (if options
                          (concat " " (mapconcat #'im-gnuplot--render-value options " "))
                        "")))
        (curve (&rest opts) opts)
        (plot (&rest curves)
              (concat
               "plot "
               (mapconcat
                (lambda (opts)
                  (let* ((opts (if (and (consp (car opts)) (null (cdr (car opts))))
                                   (car opts) opts)) ;; unpack (curve ...)
                         (args '())
                         (src nil))
                    ;; The first argument is always the source (datafile/string/expression)
                    (setq src (im-gnuplot--render-value (car opts)))
                    (setq opts (cdr opts))
                    ;; Each remaining argument is pairs of 'keyword value
                    (while opts
                      (let ((key (symbol-name (car opts)))
                            (val (im-gnuplot--render-value (cadr opts))))
                        (push (format "%s %s" key val) args)
                        (setq opts (cddr opts))))
                    (string-join (cons src (nreverse args)) " ")))
                curves
                ",\\\n     "))))
     (let ((script (mapconcat #'identity
                              (list ,@body)
                              "\n")))
       (with-temp-buffer
         (insert script)
         (call-process-region
          (point-min) (point-max)
          "gnuplot"
          t t nil)
         (buffer-string)))))

;;;; Footer

(provide 'im-gnuplot)

;;; im-gnuplot.el ends here
