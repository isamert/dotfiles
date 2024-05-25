;; -*- lexical-binding: t; -*-

;; Experimental notifications for org-mode.

(defun im-gorg-enable ()
  ""
  (let ((start-time (float-time)))
    (async-start
     `(lambda ()
        (setf org-agenda-use-time-grid nil)
        (setf org-agenda-compact-blocks t)
        ,(async-inject-variables "\\(org-agenda-files\\|load-path\\|org-todo-keywords\\)")

        (require 'dash)

        (defun orga-day-event? (seconds)
          (let ((decoded (decode-time seconds)))
            (and
             (zerop (nth 1 decoded))
             (zerop (nth 2 decoded)))))

        (defun orga-in-minute-mark? (min remanining-seconds)
          (let ((secs (* min 60)))
            (and (> remanining-seconds (- 15 secs))
                 (< remanining-seconds (+ 15 secs)))))

        (org-agenda-list 2 (org-read-date nil nil "today"))
        (->>
         (org-split-string (buffer-string) "\n")
         (--map (plist-get (org-fix-agenda-info (text-properties-at 0 it)) 'org-marker))
         (-non-nil)
         (-map
          (lambda (marker)
            (let ((props (org-entry-properties marker))
                  (now (float-time)))
              (--each '("DEADLINE" "SCHEDULED" "TIMESTAMP")
                (when-let* ((val (map-elt props it))
                            (then (float-time (org-time-string-to-time val)))
                            (remaining (- then now)))
                  (setq
                   props
                   (->
                    props
                    (map-insert (format "%s_S" it) then)
                    (map-insert 'remaining (and (> then now) remaining))
                    (map-insert 'day-event? (orga-day-event? then))
                    (map-insert 'in-1-min-mark (orga-in-minute-mark? 1 remaining))
                    (map-insert 'in-3-min-mark (orga-in-minute-mark? 3 remaining))
                    (map-insert 'in-5-min-mark (orga-in-minute-mark? 5 remaining))
                    (map-insert 'in-10-min-mark (orga-in-minute-mark? 10 remaining))
                    ))))
              props)))))
     (lambda (results)
       (setq im-notif-org-agenda-results results)
       (--filter (< (alist-get 'remaining it) ) results)
       (message ">> Took %s seconds" (- (float-time) start-time))))))
