;;; im-slack.el --- Slack related extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: messaging

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

;; TODO: ....

;;; Code:

(require 'svg)
(require 's)
(require 'dash)
(require 'slack)
(require 'vtable)
(require 'async-await)
(require 'im-notif)
(require 'im)

;;;; Customization

(defgroup im-slack nil
  "Settings for `im-slack'."
  :group 'im)

(defcustom im-slack-dms-message-lines 5
  "Number of message lines to render in the Message column."
  :type 'integer
  :group 'im-slack)

;;;; Variables

(defconst im-slack-dms-buffer-name "*slack-ims*")

(defvar-local im-slack-dms-team nil
  "Team associated with the current DMs buffer.")

(defvar im-slack--last-messages '()
  "Last slack messages.

List of:

  (:room room
   :team team
   :message message
   :sender-name sender-name
   :room-name room-name
   :title title
   :message-string msg-str)

Example usage:

  (s-join \"\n\n---\n\n\"
          (--map
           (let-plist it
             (format \"Room: %s, Sender: %s, When: %s, Message: %s\"
                     .room-name
                     .sender-name
                     (when .message
                       (lab--time-ago (im-slack--message-ts it)))
                     .message-string))
           (-take 100 im-slack--last-messages))))")

;;;; im-slack-dms

(defun im-slack-dms--wrap-text (text width-px)
  "Wrap TEXT into lines that fit within WIDTH-PX pixels.
Return a list of at most `im-slack-dms-message-lines' strings, the
last one ellipsized if TEXT does not fit."
  (let ((words (s-split " " (or text "")))
        (lines nil)
        (current ""))
    (cl-flet ((pw (s) (string-pixel-width (propertize s 'face 'variable-pitch))))
      (dolist (word words)
        (let ((candidate (if (string-empty-p current) word (concat current " " word))))
          (if (<= (pw candidate) width-px)
              (setq current candidate)
            (when (not (string-empty-p current))
              (push current lines))
            ;; Hard-break very long single words.
            (while (> (pw word) width-px)
              (let ((i (length word)))
                (while (and (> i 1)
                            (> (pw (substring word 0 i)) width-px))
                  (setq i (1- i)))
                (push (substring word 0 i) lines)
                (setq word (substring word i))))
            (setq current word)))))
    (unless (string-empty-p current)
      (push current lines))
    (setq lines (nreverse lines))
    (if (> (length lines) im-slack-dms-message-lines)
        (let ((shown (seq-take lines im-slack-dms-message-lines)))
          (append (butlast shown)
                  (list (concat (car (last shown)) "…"))))
      lines)))

(defun im-slack-dms--message-image (text width-px)
  "Render TEXT as an SVG image WIDTH-PX wide with multiple lines."
  (let* ((line-height (default-font-height))
         (lines (im-slack-dms--wrap-text text width-px))
         (height (* line-height im-slack-dms-message-lines))
         (color (face-attribute 'default :foreground nil t))
         (font-size (round (* line-height 0.78)))
         (svg (svg-create width-px height)))
    (seq-do-indexed
     (lambda (line i)
       (svg-text svg line
                 :x 0
                 :y (+ (* i line-height) font-size)
                 :font-size font-size
                 :font-family (face-attribute 'variable-pitch :family nil t)
                 :fill color))
     lines)
    (propertize " " 'display (svg-image svg :scale 1 :ascent 'center))))

(defun im-slack-dms--message-displayer (value width table)
  "Displayer for the Message column rendering VALUE as an SVG image."
  (ignore table)
  (im-slack-dms--message-image value width))

(defun im-slack-dms--display (data team)
  "Render DATA (from client.dms) for TEAM in a vtable buffer."
  (let ((conversations
         (--sort
          (>
           (string-to-number (plist-get (plist-get it :message) :ts))
           (string-to-number (plist-get (plist-get other :message) :ts)))
          (append (plist-get data :mpims) (plist-get data :ims)))))
    (with-current-buffer (get-buffer-create im-slack-dms-buffer-name)
      (setq im-slack-dms-team team)
      (variable-pitch-mode 1)
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (let ((table
               (make-vtable
                :insert nil
                :columns '((:name "Room" :align left :max-width 55)
                           (:name "When" :align left)
                           (:name "Message" :align left :width "100ex"
                            :displayer im-slack-dms--message-displayer))
                :row-colors (im-vtable-pretty-colors)
                :objects conversations
                :keymap (define-keymap "R" #'im-slack-dms-refresh)
                :getter
                (lambda (it column vtable)
                  (pcase (vtable-column vtable column)
                    ("Room"
                     (propertize
                      (slack-room-name (slack-room-find (plist-get it :id) team) team)
                      'face '(:weight bold :foreground "systemPinkColor")))
                    ("When"
                     (propertize
                      (lab--time-ago
                       (->> (plist-get (plist-get it :message) :ts)
                          (s-split "\\.") car string-to-number))
                      'face '(:slant italic :foreground "systemGrayColor")))
                    ("Message"
                     (or (plist-get (plist-get it :message) :text) ""))))
                :actions
                `("RET"
                  ,(lambda (it)
                     (slack-room-display
                      (slack-room-find (plist-get it :id) team) team))))))
          (vtable-insert table))
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun im-slack-dms--fetch (team callback)
  "Fetch DMs/MPIMs for TEAM, ensure channels/users are loaded, then call CALLBACK with data."
  (slack-request
   (slack-request-create
    "https://slack.com/api/client.dms?count=250"
    team
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (let* ((user-ids
               (append
                (-non-nil (--map (plist-get (plist-get it :message) :user) (plist-get data :ims)))
                (-non-nil (--map (plist-get (plist-get it :message) :user) (plist-get data :mpims)))))
              (missing-user-ids (slack-team-missing-user-ids team user-ids))
              (channels (append
                         (-non-nil (--map (plist-get it :id) (plist-get data :ims)))
                         (-non-nil (--map (plist-get it :id) (plist-get data :mpims)))))
              (missing-channel-ids (-non-nil (--map (if (slack-room-find it team) nil it) channels)))
              (missing-users? (> (length missing-user-ids) 0))
              (missing-channels? (> (length missing-channel-ids) 0)))
         (cond
          (missing-users?
           (message ">> Some people are missing: %s, loading..." missing-user-ids)
           (slack-user-info-request
            missing-user-ids team
            :after-success
            (lambda ()
              (message ">> Some people are missing: %s, loading...Done" missing-user-ids)
              (im-slack-dms--fetch team callback))))
          (missing-channels?
           (message ">> Some channels are missing: %s, loading..." missing-channel-ids)
           (let* ((finished-count 0)
                  (cb (lambda ()
                        (setq finished-count (1+ finished-count))
                        (when (length= missing-channel-ids finished-count)
                          (message ">> Some channels are missing: %s, loading...Done" missing-channel-ids)
                          (im-slack-dms--fetch team callback)))))
             (--each missing-channel-ids
               (slack-conversations-info it team (lambda () (funcall cb))))))
          (t (funcall callback data team)))))))))

(defun im-slack-dms-refresh ()
  "Refresh the DMs buffer."
  (interactive)
  (let ((team (or im-slack-dms-team (slack-team-select))))
    (im-slack-dms--fetch team #'im-slack-dms--display)))

(defun im-slack-dms ()
  "List and open DMs.
Get DMs and MPIMs from the client.dms endpoint, sort them by time
and present them in a vtable.  This roughly mirrors the order you
see in the DM section of the official Slack client.

Fetches missing channels/users first."
  (interactive)
  (let ((team (slack-team-select)))
    (im-slack-dms--fetch
     team
     (lambda (data team)
       (im-slack-dms--display data team)
       (pop-to-buffer im-slack-dms-buffer-name)))))

(defun im-slack-toggle-timestamps ()
  "Toggle visibility of timestamps in current buffer."
  (interactive nil slack-message-buffer-mode slack-thread-message-buffer-mode-map)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'lui-time-stamp)
          (if (get-text-property (point) 'invisible)
              (remove-text-properties (point) (1+ (point)) '(invisible nil))
            (put-text-property (point) (1+ (point)) 'invisible t)))
        (goto-char (next-property-change (point) nil (point-max)))))))

;;;; im-slack-select-room

(defun im-slack-select-room ()
  "Like `slack-select-rooms' but disable sorting."
  (interactive)
  (let* ((team (slack-team-select))
         (alist (slack-room-names
                 (cl-loop for team in (list team)
                          append (append (slack-team-ims team)
                                         (slack-team-groups team)
                                         (slack-team-channels team)))
                 team
                 #'(lambda (rs)
                     (cl-remove-if
                      (lambda (it)
                        (slack-room-hidden-p it))
                      rs)))))
    (slack-room-display
     (cdr (im-completing-read "Select: " alist :formatter #'car :sort? nil))
     team)))

;;;; im-slack-quote-message

(defun im-slack-current-message-content ()
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (room (slack-buffer-room buf))
                  (message (slack-room-find-message room (slack-get-ts))))
      (slack-message-to-string message team)))

;; TODO multiple message quote
(defun im-slack-quote-message ()
  (interactive)
  (let ((quote-text (->>
                     (im-slack-current-message-content)
                     (substring-no-properties)
                     (s-trim)
                     (s-split "\n")
                     (-drop 1)
                     (--map (concat "> " it))
                     (s-join "\n")
                     (s-append "\n"))))
    (slack-message-write-another-buffer)
    (insert quote-text)))

;;;; im-slack-open-link

(defun im-slack-open-link (link)
  "Open slack link at point, useful for `browse-url-handlers'."
  (interactive
   (list
    (read-string "Link: " (thing-at-point 'url))))
  (let* ((m (s-match
             "https://\\(\\w+\\)\\(.enterprise\\)?.slack.com/archives/\\(\\w+\\)/p\\(\\w+\\).*\\(\\?thread_ts=\\(\\w+\\)\\)?"
             link))
         (team (--find (string= (oref it domain) (nth 1 m))
                       (hash-table-values slack-teams-by-token)))
         (room (slack-room-find (nth 3 m) team))
         (message-ts (number-to-string (/ (string-to-number (nth 4 m)) 1000000.0)))
         (message (slack-room-find-message room message-ts))
         (thread (nth 5 m)))
    (im-slack--open-message-or-thread (list :message message :room room :team team))
    thread))

;;;; im-slack-clipboard-image-upload

(defun im-slack-clipboard-image-upload ()
  "Uploads png image from clipboard.

The default `slack-clipboard-image-upload' was not working
properly in MacOS."
  (interactive)
  (unless (im-clipboard-contains-image-p)
    (user-error "No image in clipboard"))
  (let* ((file (make-temp-file "clip" nil ".png")))
    (im-save-clipboard-image-to-file file)
    (slack-file-upload file "png" "image.png")))

;;;; Notifications

(async-defun im-slack-notify (message room team)
  (unless (slack-room-muted-p room team)
    (let* ((sender-name (slack-message-sender-name message team))
           (room-name (slack-room-name room team))
           (title (format "%s - %s" room-name sender-name))
           (msg-str (im-slack--stringify-message
                     (list :message message :team team)))
           (msg-str-short (s-truncate 80 (s-replace "\n" "↩" msg-str)))
           (message-data (list :room room
                               :team team
                               :message message
                               :sender-name sender-name
                               :room-name room-name
                               :title title
                               :message-string msg-str)))
      (push message-data im-slack--last-messages)
      (unless (or (slack-message-minep message team)
                  (s-contains? "message deleted" msg-str)
                  (s-contains? "has joined the" msg-str)
                  (s-contains? "has left the" msg-str)
                  ;; Don't show notifications for visible slack windows if emacs is not idle
                  (and
                   (< (time-to-seconds (or (current-idle-time) 0)) 15)
                   (--some
                    (s-contains? room-name it)
                    (--map (buffer-name (window-buffer it)) (window-list)))))
        ;; Only send desktop notifications for the things I'm interested
        ;; mpim || group || in subscribed channels
        (when (slack-message-notify-p message room team)
          (ignore-errors
            (im-notif
             :message msg-str
             :title title
             :source (lambda ()
                       (im-slack--open-message-or-thread message-data))
             :labels '("slack")))
          (unless im-notif-dnd
            (message
             ">> Slack: %s // %s"
             title
             (if (await (im-screen-sharing-now?))
                 "[REDACTED due to screensharing]"
               msg-str-short))))))))

(defun im-slack-yank-last-message ()
  "Yank the contents of the last received message as text."
  (interactive)
  (im-kill
   (im-slack--stringify-message
    (im-slack--last-message))))

(defun im-slack-open-last-message ()
  "Open last room that got new message."
  (interactive)
  (im-slack--open-message-or-thread (im-slack--last-message) :focus? nil))

(cl-defun im-slack--open-message-or-thread (msg &key (focus? t))
  (let-plist msg
    (if (ignore-errors (slack-thread-message-p .message))
        (slack-thread-show-messages .message .room .team)
      (slack-room-display
       .room
       .team))
    ;; Focus the message on buffer
    (when focus?
      (run-with-timer
       1.3 nil
       (lambda () (slack-buffer-goto (slack-ts .message)))))))

(defalias 'im-slack-recent-messages #'im-slack-last-messages)

(defun im-slack-last-messages-per-room ()
  (->>
   im-slack--last-messages
   (--map (list :room-name (plist-get it :room-name)
                :room (plist-get it :room)
                :team (plist-get it :team)))
   (-uniq)
   (-map (lambda (room)
           (--find (equal (plist-get room :room-name)
                          (plist-get it :room-name))
                   im-slack--last-messages)))))

(defun im-slack--message-ts (message)
  (->>
   (plist-get message :message)
   slack-ts
   (s-split "\\.")
   car
   string-to-number))

(defun im-slack-last-messages ()
  "List and open rooms that had new messages in them recently."
  (interactive)
  (im-slack--open-message-or-thread
   (im-completing-read
    "Select message: "
    im-slack--last-messages
    :sort? nil
    :formatter #'im-slack--format-message)
   :focus? nil))

(defun im-slack--format-message (it)
  (let-plist it
    (format "%30s ➔ %s (%s) ➤ %s"
            (propertize .room-name 'face '(:weight bold :foreground "systemPinkColor"))
            (propertize .sender-name 'face '(:weight bold :foreground "VioletRed1"))
            (when .message
              (propertize
               (lab--time-ago
                (im-slack--message-ts it))
               'face '(:slant italic :foreground "systemGrayColor")))
            (s-replace "\n" "|" (s-truncate 100 .message-string)))))

(defun im-slack-last-messages-alternative ()
  "Like `im-slack-last-messages' but only show the last message per room."
  (interactive)
  (let ((selected
         (im-completing-read
          "Select room: "
          (im-slack-last-messages-per-room)
          :formatter #'im-slack--format-message
          :sort? nil)))
    (let-plist selected
      (slack-room-display
       .room
       .team))))

;;;; im-slack-send-message

(defun im-slack-send-message (msg)
  "Send given MSG or region as message to interactively selected user."
  (interactive
   (list
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (if (y-or-n-p "Wrap with backticks? ")
              (format "```\n%s\n```" text)
            text))
      (read-string "Enter message: "))))
  (-let* (((room team) (im-slack--select-room)))
    (slack-message-send-internal
     msg room team)))

;;;; Utils/internals

(defun im-slack--last-message ()
  (--find (not (s-matches? ".*\\(alert\\|practice\\).*" (plist-get it :room-name))) im-slack--last-messages))

(defun im-slack--stringify-message (msg)
  (let ((message (plist-get msg :message))
        (team (plist-get msg :team)))
    (slack-message-to-alert message team)))

(defun im-slack--select-room ()
  "Select interactively and return (room team) pair."
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (append (slack-team-ims team)
                                        (slack-team-groups team)
                                        (slack-team-channels team)))
                team)))
    (list room team)))

;;;; Footer

(provide 'im-slack)
;;; im-slack.el ends here
