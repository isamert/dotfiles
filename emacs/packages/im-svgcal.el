;;; im-svgcal.el --- SVG Calendar -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'svg)
(require 'dash)
(require 'org)
(require 's)

;; TODO Generate from {agenda,current buffer,current restriction} etc.
;; TODO Make weekly view
;; TODO Add links to headers if possible.
;; TODO Strip links in titles
;; TODO Add alt texts (like :alt or :details)
;; TODO Make it shareable (day view or week view, as svg file to someone else)

(defconst im-svgcal--buffer-name "*svgcal*"
  "Buffer name to display svgcal inside.")

(defconst im-svgcal--title-size 14
  "Title font size in pixels, used for displaying task titles.")

(defconst im-svgcal--subtitle-size 11
  "Subtitle font size in pixels, used for displaying time range of a task.")

(defconst im-svgcal--hours
  '("07:00" "08:00" "09:00" "10:00"
    "11:00" "12:00" "13:00"
    "14:00" "15:00" "16:00"
    "17:00" "18:00" "19:00"
    "20:00" "21:00" "22:00"
    "23:00" "00:00" "01:00"
    "02:00"))

(defvar im-svgcal--last-entries nil)

(defvar im-svgcal--timer nil
  "Timer that updates the *svgcal* buffer.")

(defvar im-svgcal--source-buffer nil
  "Buffer that is used to generate svgcal.")

;;;###autoload
(defun im-svgcal-enable ()
  "Enable svgcal."
  (interactive)
  (im-svgcal-disable)
  (setq im-svgcal--source-buffer (current-buffer))
  (add-hook 'after-save-hook #'im-svgcal-run nil 'local)
  (im-svgcal-run)
  (select-window (get-buffer-window im-svgcal--source-buffer))
  (setq
   im-svgcal--timer
   (run-at-time
    1 300
    (lambda ()
      (if (get-buffer im-svgcal--source-buffer)
          (im-svgcal-render-buffer)
        (im-svgcal-disable))))))

;;;###autoload
(defun im-svgcal-disable ()
  "Disable svgcal."
  (interactive)
  (when im-svgcal--timer
    (cancel-timer im-svgcal--timer)
    (setq im-svgcal--timer nil))
  (when (get-buffer im-svgcal--buffer-name)
    (kill-buffer im-svgcal--buffer-name))
  (when (and im-svgcal--source-buffer (buffer-live-p im-svgcal--source-buffer))
    (with-current-buffer im-svgcal--source-buffer
      (remove-hook 'after-save-hook #'im-svgcal-run 'local)
      (setq im-svgcal--last-entries nil))))

;;;###autoload
(defun im-svgcal-run ()
  (interactive)
  (when (and (buffer-narrowed-p) (equal "notes" (im-tab-bar-current-tab-name)))
    (setq im-svgcal--last-entries
          (->>
           (org-map-entries
            (lambda ()
              (-map
               (-lambda ((range start end))
                 (list
                  :title (org-entry-get nil "ITEM")
                  :done? (equal (org-entry-get nil "TODO") "DONE")
                  :range range
                  :start start
                  :end end))
               (im-svgcal--org-entry-timestamps))))
           (-flatten-n 1)
           (--filter (plist-get it :end))))
    (setq im-svgcal--last-entries
          (append im-svgcal--last-entries (im-svgcal-diary-todays-entries)))
    (im-svgcal-render-buffer)
    (unless (im-buffer-visible-p im-svgcal--buffer-name)
      (im-display-buffer-in-side-window (get-buffer im-svgcal--buffer-name) 35)
      (set-window-dedicated-p (get-buffer-window im-svgcal--buffer-name) t))))

(defun im-svgcal-render-buffer ()
  (when im-svgcal--last-entries
    (with-current-buffer (get-buffer-create im-svgcal--buffer-name)
      (erase-buffer)
      (insert "** svgcal **\n")
      (svg-insert-image (im-svgcal-render im-svgcal--last-entries))
      (insert "\n")
      (goto-char (point-min)))))

(defun im-svgcal-render (tasks)
  "Return SVG for given TASKS."
  (let* ((left-gap 5)
         (top-gap 20)
         (hour-text-width 40)
         ;; Hours
         (hour-height 60)
         (hour-width 300)
         (minute-height (/ hour-height 60.0))
         ;; Canvas
         (canvas-height (* hour-height (length im-svgcal--hours)))
         (canvas-width (+ (* 2 left-gap) hour-width))
         (svg (svg-create canvas-width canvas-height))
         ;; Other
         (current-time (format-time-string "%H:%M")))
    (--each-indexed im-svgcal--hours
      (svg-text svg it :x left-gap :y (+ (* it-index hour-height) top-gap) :font-size 14)
      (svg-rectangle svg left-gap (* (1+ it-index) hour-height) hour-width 1 :fill "white")
      (svg-rectangle svg left-gap (* (1+ it-index) hour-height) hour-width 1 :fill "white"))
    (--each-indexed tasks
      (let* ((start (plist-get it :start))
             (end (plist-get it :end))
             (y-start (* minute-height (im-svgcal--time-diff (car im-svgcal--hours) start)))
             (box-height (* minute-height (im-svgcal--time-diff start end)))
             (box-width (- hour-width (+ left-gap 55)))
             (large-enough? (>= box-height (+ im-svgcal--title-size im-svgcal--subtitle-size))))
        ;; Draw item box
        (svg-rectangle
         svg
         (+ left-gap left-gap hour-text-width)
         y-start
         box-width
         box-height
         :fill "SlateBlue"
         :stroke-width 1
         :stroke "white"
         :rx 5)
        ;; Draw item text
        (svg-text
         svg (if large-enough?
                 (s-truncate 27 (im-org-header-line-to-title (plist-get it :title)) "…")
               (s-truncate 20 (im-org-header-line-to-title (plist-get it :title)) "…"))
         :x (+ (* 3 left-gap) hour-text-width)
         :y (+ y-start im-svgcal--title-size)
         :font-size im-svgcal--title-size
         :text-decoration (if (plist-get it :done?) "line-through" ""))
        (svg-text
         svg (format "%s" (plist-get it :range))
         :x (+ (* 3 left-gap) hour-text-width (if large-enough? 0 (- box-width 73)))
         :y (+ y-start im-svgcal--title-size (if large-enough? im-svgcal--title-size 0))
         :font-size im-svgcal--subtitle-size
         :text-decoration (if (plist-get it :done?) "line-through" ""))))
    ;; Draw a red circle + line for current time
    (svg-circle
     svg
     5
     (+ 2 (* minute-height (im-svgcal--time-diff (car im-svgcal--hours) current-time)))
     5
     :fill "red")
    (svg-rectangle
     svg
     0
     (* minute-height (im-svgcal--time-diff (car im-svgcal--hours) current-time))
     (+ hour-width (* 2 left-gap))
     4
     :fill "red")
    svg))

(defun im-svgcal-diary-todays-entries ()
  "Return diary entries for today in svgcal format.
Filters out entries ending with '!' (a way to hide some entries
from svgcal)."
  (save-window-excursion
    (let ((diary-display-function #'diary-simple-display))
      (->>
       (diary-list-entries (calendar-current-date) 1)
       (--map (-let* (((range title) (s-split " " (s-trim (nth 1 it))))
                      ((start end) (s-split "-" range)))
                (when (and end (not (s-suffix? "!" title)))
                  (list :title title :range range :start start :end end))))
       (-filter #'identity)))))

(defun im-svgcal--org-entry-timestamps ()
  "Get all timestamps of current entry.

>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\"))
>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55><2023-07-24 Sun 00:20-00:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\") (\"00:20-00:55\" \"00:20\" \"00:55\"))
>> (with-temp-buffer
    (org-mode)
    (insert \"* Test\\n<2023-07-23 Sun 13:20-13:55><2023-07-24 Sun 00:20-00:55>\\n** Another\\n<2023-07-23 Sun 13:20-13:55>\")
    (goto-char 0)
    (im-svgcal--org-entry-timestamps))
=> ((\"13:20-13:55\" \"13:20\" \"13:55\") (\"00:20-00:55\" \"00:20\" \"00:55\"))"
  (->>
   (save-restriction
     (save-mark-and-excursion
       (widen)
       (org-narrow-to-subtree)
       ;; I was using (org-element-at-point) and it was returning
       ;; :contents-{begin,end} properties but after I upgraded
       ;; emacs/org it started not to return them for some reason even
       ;; though the function documentation says that :{begin,end}
       ;; should be there.
       (buffer-substring-no-properties (point-min) (point-max))))
   (s-lines)
   (-drop 1) ;; drop header
   (--take-while (not (s-matches? "^\\*" it))) ;; take until next header (unnecessary)
   (s-join "\n")
   (s-match-strings-all "\\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)")))

;; FIXME: This does not work if header does not have timestamp at all
;; OR no body at all
(defun im-svgcal--time-diff (time1 time2)
  "Return diff between TIME2 and TIME1 in minutes.
It is treated as TIME2 > TIME1.

>> (im-svgcal--time-diff \"00:00\" \"00:40\")
=> 40
>> (im-svgcal--time-diff \"00:00\" \"01:40\")
=> 100
>> (im-svgcal--time-diff \"23:00\" \"01:40\")
=> 160
>> (im-svgcal--time-diff \"05:00\" \"07:40\")
=> 160
>> (im-svgcal--time-diff \"23:35\" \"00:40\")
=> 65"
  (unless (and (s-prefix? "00:" time1)
               (s-prefix? "00:" time2))
    (setq time2 (string-replace "00:" "24:" time2)))
  (let* ((time1-hh-mm (mapcar #'string-to-number (split-string time1 ":")))
         (time2-hh-mm (mapcar #'string-to-number (split-string time2 ":"))))
    (if (> (car time1-hh-mm) (car time2-hh-mm))
        (+ (im-svgcal--time-diff time1 "00:00")
           (im-svgcal--time-diff "00:00" time2))
      (abs (- (+ (* (car time1-hh-mm) 60) (cadr time1-hh-mm))
              (+ (* (car time2-hh-mm) 60) (cadr time2-hh-mm)))))))

(provide 'im-svgcal)
;;; im-svgcal.el ends here
