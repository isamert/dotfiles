;;; im.el --- my utils  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (s "1.13.1") (f "0.20.0") (parse-csv "0.3"))
;; Keywords: utils

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

;; TODO ....

;;; Code:

(require 's)
(require 'f)
(require 'dash)
(require 'parse-csv)
(require 'map)
(require 'seq)
(require 'json)
(eval-when-compile (require 'subr-x))

;;;; Variables

;; TODO ...

;;;; OS Utils

(defun im-ssh-host-list ()
  "Return all host names defined in ~/.ssh/config."
  (->>
   (f-read-text "~/.ssh/config")
   (s-lines)
   (--map (nth 1 (s-match "^Host \\(.*\\)" it)))
   (-filter #'identity)
   (-remove-item "*")))

(cl-defmacro im-when-on (&key linux darwin)
  (pcase system-type
    ('darwin darwin)
    ((or 'gnu/linux 'linux) linux)))

(defun im-port-in-use? (port)
  "Check if PORT is in use."
  (let ((process nil)
        (port-in-use nil))
    (condition-case nil
        (progn
          (setq process (make-network-process :name "test-port"
                                              :server t
                                              :host 'local
                                              :service port))
          (delete-process process))
      (file-error
       (setq port-in-use t)))
    port-in-use))

;;;; General utilities

(defun im-mkdir-if-not (dir)
  "Create the DIR if it does not exist return DIR."
  (unless (file-exists-p dir)
    (make-directory dir))
  dir)

(defun im-font-exists-p (font)
  "Check if FONT exists."
  (when (fboundp 'x-list-fonts)
    (x-list-fonts font)))

(defun im-assoc-regexp (key list &optional fn)
  "Like `assoc` but uses `string-match (car pair) KEY` for comparison.
Returns all the matching pairs.  FN is applied to the keys before
matching, if present."
  (seq-filter
   (lambda (pair)
     (when (string-match-p (if fn (funcall fn (car pair)) (car pair)) key)
       pair))
   list))

(defun im-region-or (what &optional alternative)
  "Return currently selected string or WHAT-at-point string.
WHAT can be \\='symbol \\='word or a function that returns string
etc.

If WHAT is \\='string, then when no region is found user is
prompted with `read-string'.

If WHAT is a literal string, then it's treated as an argument to
`skip-chars-forward' and `skip-chars-forward'.

If ALTERNATIVE is non-nil, then if the result of WHAT returns nil
then `im-region-or' is called with ALTERNATIVE.  A useful example
would be providing \\='string as ALTERNATIVE so that if a match
not found with WHAT, then it is requested from the user."
  (let ((result (cond
                 ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                 ((equal what 'string) (read-string "Enter value: "))
                 ((functionp what) (funcall what))
                 ((symbolp what) (thing-at-point what t))
                 ((stringp what)
                  (save-excursion
                    (let* ((start (progn (skip-chars-backward "0-9") (point)))
                           (end (progn (skip-chars-forward "0-9") (point)))
                           (x (buffer-substring-no-properties start end)))
                      (if (s-blank? x) nil x))))
                 (t what))))
    (if result
        result
      (if alternative
          (im-region-or alternative)
        result))))

(declare-function evil-inner-back-quote "evil-commands")

(defun im-inner-back-quote-at-point ()
  "Return text inside the back quotes at point."
  (let ((bounds (evil-inner-back-quote)))
    (buffer-substring-no-properties
     (nth 0 bounds)
     (nth 1 bounds))))

(defun im-shell-command-to-string (cmd)
  "Like `shell-command-to-string' but only stdout is returned."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (process-file
        shell-file-name nil '(t nil)  nil shell-command-switch
        cmd)))))

(defun im-serialize-into-file (file data)
  (unless (f-exists? (f-dirname file))
    (f-mkdir-full-path (f-dirname file)))
  (with-temp-file (expand-file-name file)
    (let ((print-length nil)
          (print-level nil))
      (prin1 data (current-buffer)))))

(defun im-deserialize-from-file (file)
  (let ((fpath (expand-file-name file)))
    (when (and (file-exists-p fpath))
      (with-temp-buffer
        (insert-file-contents fpath)
        (goto-char (point-min))
        (read (current-buffer))))))

;; TODO Add a way to invalidate the file after given date
(defmacro defmemoizefile (name arglist file &rest body)
  "Like a normal memoize function but persist the memoize cache to a file.
This way, when Emacs is opened freshly, it'll continue using the memoize
cache."
  (declare (indent 3) (doc-string 4))
  (let ((origfn (intern (concat (symbol-name name) "---defmemoizefile-origfn")))
        (memoizemap (intern (concat (symbol-name name) "---defmemoizefile-memoizemap"))))
    `(progn
       (setq ,memoizemap (make-hash-table :test 'equal))
       (when (file-exists-p (expand-file-name ,file))
         (setq ,memoizemap (im-deserialize-from-file ,file)))

       (defun ,origfn ,arglist
         ,@body)

       (defun ,name (&rest ___args)
         (if-let ((memoizedresult (gethash ___args ,memoizemap)))
             memoizedresult
           (let ((___result (apply #',origfn ___args)))
             (map-put! ,memoizemap ___args ___result)
             (im-serialize-into-file ,file ,memoizemap)
             ___result))))))

(declare-function aw-select "ace-window")

(defun im-select-window-with-buffer (buffer-name)
  "Select the visible window that matches given BUFFER-NAME.
If more than one buffer is matched, then let user interactively
select the right window using `aw-select'."
  (declare (indent 1))
  (let* ((curr (buffer-name (current-buffer)))
         (windows (--filter
                   (-as-> (window-buffer it) buffer
                          (buffer-name buffer)
                          (and (string-match buffer-name buffer) (not (equal curr buffer))))
                   (window-list))))
    (select-window
     (if (= 1 (length windows))
         (car windows)
       (cl-letf (((symbol-function 'aw-window-list)
                  (lambda () (sort `(,@windows ,(selected-window)) 'aw-window<))))
         (aw-select nil))))))

(defmacro im-with-visible-buffer (buffer-name &rest body)
  "Evaluate BODY within the BUFFER-NAME that is currently visible."
  (declare (indent 1))
  `(with-selected-window (selected-window)
     (when (im-select-window-with-buffer ,buffer-name)
       ,@body)))

(defun im-sync-async-command-to-string (command &rest args)
  "Run async command and wait until it's finished.
This may seem stupid but I had to use it."
  (with-temp-buffer
    (let ((process (apply 'start-process `("sync-async-proc" ,(current-buffer) ,command ,@args))))
      (while (process-live-p process)
        (sit-for 0.1))
      (buffer-string))))

(defun im-plist-to-alist (plist)
  "Convert PLIST to an alist.
Taken from transient.el."
  (let (alist)
    (while plist
      (push (cons (let* ((symbol (pop plist))
                         (name (symbol-name symbol)))
                    (if (eq (aref name 0) ?:)
                        (intern (substring name 1))
                      symbol))
                  (pop plist))
            alist))
    (nreverse alist)))

(defmacro let-plist (plist &rest form)
  "Like `let-alist' but for plists."
  (declare (indent 1))
  `(let-alist (im-plist-to-alist ,plist)
     ,@form))

(defun im-mimetype (path)
  "Return mimetype of given file at PATH."
  (string-trim (shell-command-to-string (format "file --brief --mime-type '%s'" path))))

(defun im-to-keyword (it)
  "Convert given string or symbol to a :keyword."
  (thread-last
    (cond
     ((stringp it) it)
     ((symbolp it) (symbol-name it))
     (t (error "Trying to convert %s to symbol" it)))
    (string-remove-prefix ":")
    (concat ":")
    (downcase)
    (intern)))

(defun im-alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(:a 1 :b 2 :c 3)

The original alist is not modified.

This function is taken from `mm-decode.el' and modified."
  (let (plist)
    (while alist
      (let ((el (car alist)))
        (setq plist (cons (cdr el) (cons (im-to-keyword (car el)) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(defmacro λ-interactive (&rest body)
  "Useful for defining keybindings."
  `(lambda () (interactive) ,@body))

(defun im-kill (x &optional replace)
  (kill-new x replace)
  x)

(defun im-force-focus-emacs ()
  "Focus Emacs frame if not focused already."
  (unless (frame-focus-state)
    (im-when-on
     :darwin
     (shell-command-to-string
      "osascript -e 'tell application \"System Events\" to click UI element \"Emacs\" of list 1 of application process \"Dock\"'")
     :linux
     (user-error "Implement this: im-force-focus-emacs"))))

(defun im-line-count-below-cursor ()
  "Return the number of lines displayed below the cursor in the current window."
  (let ((line (line-number-at-pos)))
    (save-excursion
      (move-to-window-line 0)
      (- (window-height) (- line (line-number-at-pos))))))

;;;; Elisp utils

(defmacro im-tap (form)
  "Evaluate FORM and return its result.
Additionally, print a message to the *Messages* buffer showing
the form and its result.

This macro is useful for debugging and inspecting the intermediate
results of Elisp code without changing your code structure.  Just wrap
the form with `im-tap' that you want to see it's output without
introducing an intermediate let-form."
  `(let ((result ,form)
         (print-length nil)
         (print-level nil))
     (message "[im-tap :: %s] → %s" ,(prin1-to-string form) result)
     result))

(defun im-inspect (thing)
  "Like `im-tap' but use `pp-display-expression' to display the THING."
  (let ((print-length nil)
        (print-level nil))
    (pp-display-expression thing "*im-inspect*"))
  thing)

(defmacro im-append! (lst item)
  "Append ITEM to end of the LST.
Modifies LST.  Only meant to be used in configuration."
  `(setq ,lst (append ,lst (list ,item))))

(defun im-elisp-find-file-prefix ()
  "Extract prefix from defgroup statement in current buffer.
I use this in my `defun' snippet via yasnippet."
  (or (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp ":prefix \"\\(.*\\)\"" nil t)
          (match-string 1)))
      "im-"))

(declare-function json-pretty-print-buffer "json")

(defun im-open-region-in-temp-buffer (content &optional majormode)
  "Open CONTENT in a temporary buffer with MAJORMODE.
When called interactively, CONTENT selected region or given string."
  (interactive (list
      (im-region-or 'string)
      (completing-read
       "Select major mode: " obarray
       (lambda (x)
         (and (fboundp x)
              (commandp x)
              (string-match "-mode$" (symbol-name x))))

       t
       nil nil
       (format "%s" major-mode))))
  (switch-to-buffer (generate-new-buffer "*temp-region*"))
  (insert content)
  (switch-to-buffer (current-buffer))
  (when majormode
    (funcall (intern majormode))
    (pcase majormode
      ((or "json-ts-mode" "json-mode") (json-pretty-print-buffer)))))

(defun im-kill-this-buffer ()
  "Kill current buffer.
Function `kill-this-buffer' does not work reliably.  See
documentation of it."
  (interactive)
  (kill-buffer (current-buffer)))

(defun im-add-to-path (path)
  "Add given PATH to PATH variable.
Useful for adding something to Emacs' PATH without restarting it."
  (interactive "sPath: ")
  (add-to-list 'exec-path (expand-file-name path))
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name path))))

(defun im-get-reset-buffer (buffer)
  "Create BUFFER and return it.
If it exists, it's killed first and return a new buffer."
  (ignore-errors (kill-buffer buffer))
  (get-buffer-create buffer))

;;;; Clipboard functions

(defun im-clipboard-contains-image-p ()
  "Check whether the clipboard has image or not."
  (cond
   ((locate-file "wl-paste" exec-path)
    (s-contains? "image/" (shell-command-to-string "wl-paste --list-types")))
   ((locate-file "xclip" exec-path)
    (s-contains? "image/" (im-sync-async-command-to-string "xclip" "-o" "-sel" "c" "-t" "TARGETS")))
   ((locate-file "pngpaste" exec-path)
    (eq (shell-command "pngpaste - &>/dev/null") 0))))

(defun im-save-clipboard-image-to-file (file)
  "Save the image in clipboard (if there is any) to given FILE.
Also see `im-clipboard-contains-image-p' to check if there is one."
  (interactive "FFile to save the image: ")
  (shell-command
   (format "%s > %s"
           (cond
            ((locate-file "wl-paste" exec-path) "wl-paste")
            ((locate-file "xclip" exec-path) "xclip -selection clipboard -target image/png -out")
            ((locate-file "pngpaste" exec-path) "pngpaste -"))
           file)))

;;;; User input

(cl-defun im-get-input (&key (mode #'org-mode)
                             (init "")
                             on-accept
                             on-reject
                             pre-process)
  "Display a buffer to user to enter some input."
  (let* ((buffer (get-buffer-create "*im-input*"))
         (success-handler (lambda ()
                            (interactive)
                            (let ((pre-proc-result (when pre-process
                                                     (with-current-buffer buffer
                                                       (funcall pre-process))))
                                  (result (substring-no-properties (buffer-string))))
                              (kill-buffer buffer)
                              (if pre-process
                                  (funcall on-accept result pre-proc-result)
                                (funcall on-accept result)))))
         (reject-handler (lambda ()
                           (interactive)
                           (kill-buffer buffer)
                           (when on-reject
                             (funcall on-reject)))))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (funcall mode)
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "C-c C-c") success-handler)
      (local-set-key (kbd "C-c C-k") reject-handler)
      (setq header-line-format "Hit `C-c C-c' to save `C-c C-k' to reject.")
      (insert init))))

(defun im-alist-completing-read (prompt alist &optional initial)
  "Like `completing-read' but returns value of the selected key in given ALIST."
  (alist-get
   (completing-read prompt alist nil nil initial)
   alist nil nil #'equal))

(cl-defun im-completing-read
    (prompt objects &key (formatter #'identity) category (sort? t) def multiple? initial require-match?)
  "Provide a completion interface for selecting an item from a list of objects.

- PROMPT: The prompt string to display to the user.
- OBJECTS: A list of objects to choose from.
- FORMATTER: (Optional) A function that formats each object
  before displaying it to the user.  The default is `'identity',
  which means no formatting.
- CATEGORY: (Optional) A category symbol associated with the
  completion.  This can be used to provide additional completion
  behavior.
- SORT?: (Optional) A boolean value indicating whether the
  completion list should be sorted.  The default is t.
- DEF: (Optional) The default value to return if no selection is
  made.  If multiple selections are allowed, this value will be
  returned as a list.
- MULTIPLE?: (Optional) A boolean value indicating whether
  multiple selections are allowed.  The default is `nil`.

If MULTIPLE? is nil, this function returns the selected object
from the completion list.  If MULTIPLE? is t, this function
returns a list of selected objects.  If no selection is made, the
DEF value is returned."
  (let* ((object-table
          (make-hash-table :test 'equal :size (length objects)))
         (object-strings
          (mapcar
           (lambda (object)
             (let ((formatted-object (funcall formatter object)))
               (puthash formatted-object object object-table)
               (propertize formatted-object 'empv-item object)))
           objects))
         (selected
          (funcall
           (if multiple? #'completing-read-multiple #'completing-read)
           (format "%s " prompt)
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(when category (cons 'category category))
                   ,@(unless sort?
                       '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
               (complete-with-action
                action object-strings string predicate)))
           nil require-match? initial)))
    (if multiple?
        (or (mapcar (lambda (it) (gethash it object-table)) selected) def)
      (gethash selected object-table (or def selected)))))

(defun im-dmenu (prompt items &rest _ignored)
  "Like `completing-read' but instead use dmenu.
Useful for system-wide scripts.  ITEMS should be list of
strings.  PROMPT and rest are _IGNORED."
  (with-temp-buffer
    (thread-first
      (cond
       ((functionp items)
        (funcall items "" nil t))
       ((listp (car items))
        (mapcar #'car items))
       (t
        items))
      (string-join "\n")
      string-trim
      insert)
    (shell-command-on-region
     (point-min)
     (point-max)
     (im-when-on
      :linux (format "rofi -dmenu -fuzzy -i -p '%s'" prompt)
      :darwin "choose")
     nil t "*im-dmenu error*" nil)
    (string-trim (buffer-string))))

(cl-defmacro im-output-select
    (&key cmd prompt keep-order
          (formatter 'it)
          initial
          (append '())
          (prepend '())
          (split "\n")
          (drop 0)
          (filter t)
          (map 'it) (do 'it) category)
  "Run given CMD and do a `completing-read' on it.
This macro is intended to quicken up the process of running a
shell command and doing a `completing-read' on it and then using
the result in another context, possibly on another shell
command."
  `((lambda (it) ,do)
    (im-completing-read
     ,prompt
     (append
      ,prepend
      (seq-filter
       (lambda (it) ,filter)
       (seq-map-indexed
        (lambda (it idx) (ignore idx) ,map)
        (seq-drop
         (s-split
          ,split
          (shell-command-to-string ,cmd)
          t)
         ,drop)))
      ,append)
     :formatter (lambda (it) ,formatter)
     :sort? ,(not keep-order)
     :initial ,initial
     :category ,category
     :require-match? t)))

(defun im-read-string (prompt &rest rest)
  "Like `read-string' but returns nil on empty input."
  (let ((result (string-trim (apply #'read-string prompt rest))))
    (if (string-equal result "")
        nil
      result)))

;;;; String utils

;; Source: https://gist.github.com/jordonbiondo/c4e22b4289be130bc59b
(defmacro im-s-interpolated (str)
  "Elisp string interpolation.
Uses #{elisp-code} syntax."
  (let ((exprs nil))
    (with-temp-buffer
      (insert str)
      (goto-char 1)
      (while (re-search-forward "#{" nil t 1)
        (let ((here (point))
              (emptyp (eql (char-after) ?})))
          (unless  emptyp (push (read (buffer-substring (point) (progn (forward-sexp 1) (point)))) exprs))
          (delete-region (- here 2) (progn (search-forward "}") (point)))
          (unless emptyp (insert "%s"))
          (ignore-errors (forward-char 1))))
      (append (list 'format (buffer-string)) (reverse exprs)))))

(defun im-s-upcase-until (until s)
  "Make prefix of a string S uppercase until given char UNTIL.
`(im-s-upcase-until \"-\" \"aha-hehe\")' -> \"AHA-hehe\""
  (let ((end (s-index-of until s)))
    (concat
     (s-upcase (substring s 0 end))
     (substring s end))))

(defun im-string-url-case (str)
  "Convert STR to something like `a-string-appropriate-for-urls'.
>> (im-string-url-case \" test: test! şeyler ÜPPERCaSE   vs.\")
=> \"test-test-seyler-uppercase-vs\""
  (->>
   str
   downcase
   (s-replace-all
    '(("'" . "")
      ("ö" . "o")
      ("ı" . "i")
      ("ğ" . "g")
      ("ü" . "u")
      ("ş" . "s")
      ("ö" . "o")
      ("ç" . "c")))
   (s-trim)
   (replace-regexp-in-string "[^a-zA-Z0-9]" "-")
   (replace-regexp-in-string "-+" "-")
   (s-chop-prefix "-")
   (s-chop-suffix "-")))

(defun im-human-readable-size (size-in-bytes)
  (let* ((units '("B" "KB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB"))
         (unit (car units))
         (bytes (float size-in-bytes))
         (exponent (floor (log bytes 1024))))
    (setq units (cdr units))
    (while (> exponent 0)
      (setq bytes (/ bytes 1024.0))
      (setq exponent (1- exponent))
      (setq unit (car units))
      (setq units (cdr units)))
    (format "%.2f %s" bytes unit)))

(defun im-non-blank-or-nil (x)
  (if (and x (s-blank-str? x))
      nil
    x))

;;;; List/hash-table/vector utils

(defun im-hash-table-to-alist (val)
  "Bad way to convert hash-tables with vectors into alists.
I use this only for debugging."
  (cond
   ((hash-table-p val) (im-hash-table-to-alist (map-into val 'alist)))
   ((vectorp val) (mapcar #'im-hash-table-to-alist (cl-coerce val 'list)))
   ((json-alist-p val) (map-apply (lambda (key it) (cons key (im-hash-table-to-alist it))) val))
   ((listp val) (mapcar (lambda (key it) (cons key (im-hash-table-to-alist it))) val))
   (t val)))

;;;; UI utilities

(cl-defun im-insert-toggle-button (state1 state2 &key help on-toggle)
  "Insert a button that change it's text from STATE1 to STATE2 when clicked.
HELP is displayed when cursor is on the button and
`im-help-at-point-mode' is enabled.

ON-TOGGLE is called when user toggles the button.  It will be
called with the current state of the button."
  (insert-text-button
   (if (functionp state1) (funcall state1) state1) 'action
   (lambda (button)
     (let ((start (button-start button))
           (end (button-end button))
           (cursor (point)))
       (delete-region start end)
       (im-insert-toggle-button state2 state1 :help help :on-toggle on-toggle)
       (goto-char (if (< cursor (+ start (length state2))) cursor start))
       (when on-toggle
         (funcall on-toggle state2))))
   'kbd-help help
   'follow-link t))

(defun im-pulse-highlight-region (start end color duration)
  "Highlight the region from START to END with COLOR for DURATION seconds."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face `(:background ,color))
    (run-at-time duration nil #'delete-overlay overlay)))

;;;; API call

;; This function is for doing easy REST calls and it uses plists for
;; everything because it's more readable and easier to type than
;; alists (but you can still use alists if you want or need to). I use
;; this to quickly prototype stuff in elisp.

(cl-defun im-request
    (endpoint
     &rest params
     &key (-type "GET") (-headers) (-data) (-params) (-async?) (-on-success) (-on-error) (-raw)
     &allow-other-keys)
  "Like `request' but plist and JSON oriented.

JSON responses are automatically parsed, query parameters are
constructed from top-level keywords, request body can be a plist (which
will be serialized into JSON).  Examples:

    (im-request \"some/endpoint\")

With url parameters:

    (im-request \"...\" :query \"test\" :page 3 :page_size 15)

If you want to pass an alist as url params:

    (im-request \"...\" :-params \\='((query . \"test\") (page . 3) (page_size . 15)))

POST with json body:

    (im-request \"...\" :-type \\='POST :-data \\='(:key1 1 :key2 2))

With some HTTP headers:

    (im-request \"...\" :-headers \\='(:Authorization \"Bearer e21ewqfasdwtkl\"))

For async requests, simply provide a success handler:

    (im-request \"...\"
      :-on-success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ...use the parsed json DATA...)))"
  (declare (indent defun))
  (interactive (list (read-string "URL: ") :-raw t))
  (let (json
        (json-object-type 'alist)
        (json-array-type #'list)
        (json-key-type 'symbol))
    ;; Remove request related items from params list
    (dolist (key '(:-type :-headers :-data :-params :-async? :-on-success :-raw :-on-error))
      (cl-remf params key))

    (let ((fn (lambda (resolve reject)
                (request
                  endpoint
                  :type -type
                  :headers (cond
                            ((and -headers (json-alist-p -headers)) -headers)
                            ((and -headers (json-plist-p -headers)) (im-plist-to-alist -headers))
                            (t nil))
                  :parser (if -raw #'buffer-string (apply-partially #'json-parse-buffer :object-type 'alist :array-type 'list))
                  :success (cl-function
                            (lambda (&key data &allow-other-keys)
                              (funcall resolve data)))
                  :error (cl-function
                          (lambda (&key data status error-thrown &allow-other-keys)
                            (unless -on-error
                              (message "im-request :: failed status=%s, error-thrown=%s, data=%s" status error-thrown data))
                            (funcall reject data)))
                  :sync (and (not -on-success) (not -async?))
                  :data (cond
                         ((and -data (json-alist-p -data)) (json-encode -data))
                         ((and -data (json-plist-p -data)) (json-encode (im-plist-to-alist -data)))
                         ((stringp -data) -data)
                         (t nil))
                  :params (cond
                           ((and -params (json-alist-p -params)) -params)
                           ((and -params (json-plist-p params)) (im-plist-to-alist -params))
                           (t (im-plist-to-alist params)))))))
      (cond
       (-async? (promise-new fn))
       ((or -on-success -on-error)
        (funcall
         fn
         (or -on-success (lambda (data) (message "im-request :: Unhandled success. Data: %s" data)))
         (or -on-error (lambda (data) (message "im-request :: Unhandled error. Data: %s" data)))))
       (t (funcall fn (lambda (data) (setq json data)) (lambda (data) (user-error "Request failed, see earlier logs")))
          (when (called-interactively-p 'interactive)
            (with-current-buffer (get-buffer-create "*im-request-response*")
              (erase-buffer)
              (insert json)
              (json-pretty-print-buffer)
              (json-ts-mode)
              (switch-to-buffer-other-window (current-buffer))
              (goto-char (point-min))))
          json)))))

;; TODO: Remove this, use the one above with :-async? t
(cl-defun im-request-json-async (url &key headers type data)
  "Async `request'."
  (promise-new
   (lambda (resolve reject)
     (request
       url
       :headers `(("Content-Type" . "application/json")
                  ,@headers)
       :type (or type "GET")
       :data data
       :parser (apply-partially #'json-parse-buffer :object-type 'alist :array-type 'list)
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (funcall resolve data)))
       :error (cl-function
               (lambda (&key data error-thrown &allow-other-keys)
                 (message "request failed :: %s | %s" data error-thrown)
                 (funcall reject data)))))))

;;;; URL/web utils

(declare-function dom-text "dom")
(declare-function dom-by-tag "dom")

(defun im-url-parse-title ()
  (dom-text (car (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'title))))

(defun im-url-get-title (url)
  "Get title of the URL."
  (with-current-buffer (url-retrieve-synchronously url :silent :inhibit-cookies)
    (im-url-parse-title)))

(defun im-url-get-title-async (url cb)
  "Get title of the URL, call CB with it."
  (url-retrieve
   url
   (lambda (_status)
     (funcall cb (im-url-parse-title))
     (kill-buffer))
   nil :silent :inhibit-cookies))

(defmacro with-default-browser (&rest body)
  `(let* ((browse-url-handlers nil)
          (browse-url-browser-function browse-url-secondary-browser-function))
     ,@body))

(defun im-json-encode-and-show (obj)
  "Show given elisp OBJ as pretty printed JSON."
  (switch-to-buffer-other-window (get-buffer-create "*raw-pretty*"))
  (insert (json-encode obj))
  (json-pretty-print-buffer)
  (json-ts-mode))

(defun im-url? (url)
  "Check if given URL is really an URL or not."
  (or (s-match "^\\(https?\\|file\\)://\\|www." url)
      (s-match "\\.\\(org\\|net\\|com\\)$" url)))

(defun im-check-internet-connection ()
  "Return t if there is an active internet connection.
May return false on slow connections.  Checks blocking, max 150 ms (for
Linux), 1 secs for Mac."
  (im-when-on
   :linux
   (= 0 (call-process "nc" nil nil nil "-w" "150ms" "-z" "www.google.com" "80"))
   :darwin
   (= 0 (call-process "nc" nil nil nil  "-G" "1" "-z" "www.google.com" "80"))))

;;;; File operations

(defun im-latest-file (&optional path)
  "Get latest file in PATH."
  (car (directory-files (or path default-directory) 'full "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" #'file-newer-than-file-p)))

(defun im-directory-files-recursively (dir regexp)
  "Faster alternative to `directory-files-recursively'."
  (->>
   (format
    "fd '%s' '%s' --type file --maxdepth 4 --absolute-path"
    regexp
    (expand-file-name dir))
   (shell-command-to-string)
   (s-trim)
   (s-split "\n")))

(defun im-encode-image-base64 (path)
  (let* ((type (nth 1 (s-match ".*\\.\\(jpg\\|jpeg\\|png\\)" path)))
         (base64 (with-temp-buffer
                   (let ((coding-system-for-read 'no-conversion))
                     (insert-file-contents path)
                     (base64-encode-region (point-min) (point-max) t)
                     (buffer-string)))))
    (format "data:image/%s;base64,%s" type base64)))

;;;; Git

(declare-function lab-git-clone "lab")
(declare-function alert "alert")

(cl-defun im-git-temp-clone (url &key
                                 on-success
                                 (on-fail
                                  (lambda ()
                                    (alert "Cloning failed!" :title "im-git-temp-clone"))))
  "Clone URL into a temp directory and run ON-SUCCESS inside it."
  (let ((dir (make-temp-file "im_temp_git_dir" t)))
    (lab-git-clone
     url
     dir
     :shallow t
     :callback
     (lambda (success?)
       (if success?
           (let ((default-directory (f-join dir (file-name-base url))))
             (funcall on-success))
         (funcall on-fail))))))

;;;; Interactive utilities

(defalias 'im-generate-random-string #'im-insert-random-string)
(defalias 'im-generate-random-id #'im-insert-random-string)
(defun im-insert-random-string ()
  "Generate a 7 character random string.
Useful for the cases where I need to reference stuff inside a
file/project etc."
  (interactive)
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (string ""))
    (dotimes (_ 7 string)
      (setq string (concat string (string (aref chars (random (length chars)))))))
    (insert string)))

;; TODO: Formalize the notation, something like "ref:<id>" and
;; "def:<id>" and have a function that finds this specific id in the
;; current project.

;;;; CSV

(defun im-parse-csv (csv)
  "Parse CSV into an elisp object.

CSV is a string that contains the csv data.  The first line should be
the header line."
  (let* ((parsed (parse-csv-string-rows csv ?\, ?\" "\n"))
         (headers (car (-take 1 parsed)))
         (data (-drop 1 parsed)))
    (--map (-zip-pair headers it) data)))

(defun im-parse-csv-buffer ()
  "Parse a CSV buffer into an elisp list and inspect it."
  (interactive)
  (im-inspect (im-parse-csv (buffer-substring-no-properties (point-min) (point-max)))))

(autoload 'parse-csv-string-rows "parse-csv")

;;;; Footer

(provide 'im)
;;; im.el ends here
