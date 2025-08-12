;; -*- lexical-binding: t; -*-

(defun gnuplot--render-value (v)
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
                        (gnuplot--render-value (cdr pair))))
              (let ((lst options))
                (cl-loop for (k v) on lst by #'cddr collect (cons k v)))
              "\n"))
        (set1 (key &rest options)
              (format "set %s%s"
                      (symbol-name key)
                      (if options
                          (concat " " (mapconcat #'gnuplot--render-value options " "))
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
                    (setq src (gnuplot--render-value (car opts)))
                    (setq opts (cdr opts))
                    ;; Each remaining argument is pairs of 'keyword value
                    (while opts
                      (let ((key (symbol-name (car opts)))
                            (val (gnuplot--render-value (cadr opts))))
                        (push (format "%s %s" key val) args)
                        (setq opts (cddr opts))))
                    (string-join (cons src (nreverse args)) " ")))
                curves
                ",\n     "))))
     (let ((script (mapconcat #'identity
                              (list ,@body)
                              "\n")))
       (with-temp-buffer
         (insert script)
         (shell-command-on-region
          (point-min) (point-max)
          "gnuplot"
          nil t)
         (buffer-string)))))
