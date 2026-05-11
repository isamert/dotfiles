;;; im-tab.el --- my tab-bar extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-tab.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tabs, utility

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

;; This package provides a simple way to create and save ad-hoc
;; tab-bar window configurations and return back to them easily.
;;
;; * Rationale
;;
;; When I work with a specific feature, I generally set up a window
;; configuration for that but of course that changes pretty quickly
;; when you look up for something or some buffer pops up and destroys
;; your configuration.  These changes can be moderated with
;; `tab-bar-history-back' and `tab-bar-history-forward' and you can
;; return to your specific setup.  But sometimes, the yak shaving goes
;; too deep and your specific window configuration is buried deep
;; inside your `tab-bar-history' or it's even completely lost or just
;; {back,forward} ergonomics are not good enough.  This package solves
;; this problem.  It let's you assign specific window configuration to
;; specific keys and restore them with those specific keys.
;;
;; * Usage
;;
;; `im-tab-configuration-save-current' function saves the window
;; configuration using `last-input-event'.  It is advised to bind this
;; function to a key chords like
;;
;;   C-c w w {1,2,3,4...}
;;
;; so that you can retrieve with same number keys later.  Assuming you
;; bind `im-tab-configuration-restore-current' to:
;;
;;   C-c w {1,2,3,4...}
;;
;; Now you will be able to save the window configuration for the
;; current tab with "C-c w w 1" and then retrieve it back with
;; "C-c w 1" in the same tab.  Configurations are saved per tab.
;;
;; Sometimes you want to return to a window configuration but keeping
;; the currently focused buffer in your "main area".  Assume you have
;; the following window layout:
;;
;; +--------+----------+
;; |        |          |
;; | cursor |          |
;; | here   |          |
;; |        +----------|
;; |        |          |
;; |        |          |
;; |        |          |
;; +--------+----------+
;;       (layout 1)
;;
;; And you hit `C-x w w 1' to save it.  You did some yak shaving and
;; ended up in completely different layout:
;;
;; +--------+----------+
;; |        |          |
;; |        +----------+
;; |        |  cursor  |
;; |        +----+-----|
;; |        |    |     |
;; +--------+    |     |
;; |        |    |     |
;; +--------+----+-----+
;;       (layout 2)
;;
;; Now you want to get back into the saved layout, layout 1, but you
;; want to keep the currently focused buffer (the one with the cursor
;; from layout 2) in your "main area", the window with the cursor from
;; the layout 1. To do so, bind
;; `im-tab-configuration-restore-current-sticky' to a key like:
;;
;;   C-c W {1,2,3,4...}
;;
;; and now you can hit `C-x W 1' to restore the layout 1 but you keep
;; currently focused buffer in your "main area".
;;
;; * Example bindings
;;
;; ** define-key
;;
;;   (dolist (n (number-sequence 1 5))
;;     ;; Save current tab window configuration to a key
;;     (define-key global-map (kbd (format "C-c w w %s" n)) #'im-tab-configuration-save-current)
;;     ;; Restore a saved configuration with a key
;;     (define-key global-map (kbd (format "C-c w %s" n))   #'im-tab-configuration-restore-current)
;;     ;; Restore sticky (keep current buffer in main area)
;;     (define-key global-map (kbd (format "C-c W %s" n)) #'im-tab-configuration-restore-current-sticky))
;;
;; ** general.el examples:
;;
;; If you are using evil and general.el, these might help:
;;
;;   (dolist (n (number-sequence 1 5))
;;     (<your-leader>
;;       ;; Save current tab window configuration to a key
;;       (format "ww%s" n) #'im-tab-configuration-save-current
;;       ;; Restore a saved configuration with a key
;;       (format "w%s" n) #'im-tab-configuration-restore-current
;;       ;; Restore a saved configuration but keep the current buffer
;;       (format "W%s" n) #'im-tab-configuration-restore-current-sticky))

;;; Code:

(require 'map)
(require 'tab-bar)

;;;; Customization

(defgroup im-tab nil
  "Save window configurations to a predefined keybindings ephemerally."
  :group 'utility)

(defcustom im-tab-warn-on-unnamed-tabs t
  "Warn if currently saved window configuration is on an unnamed tab.
If the current tab is not explicitly named, then the configurations are
saved depending on the tab's position which is fragile.  If you don't
want this warning to appear, set this to nil."
  :group 'im-tab)

;;;; Save window configurations to a predefined keybinding ephemerally

(defvar im-tab-configuration (make-hash-table :test 'equal :size 20))

;;;###autoload
(defun im-tab-configuration-save-current ()
  "Save current window configuration to last pressed key."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-tab (alist-get 'current-tab tabs))
         (name (alist-get 'name current-tab))
         (tab-pos (im-tab--seq-find-index (lambda (it) (equal name (alist-get 'name it))) tabs))
         (explicit-name? (alist-get 'explicit-name current-tab))
         (_ (unless explicit-name?
              (setq name (format "<tab:%s>-" tab-pos))))
         (cfg (concat "<tab:" name ">" "-" (char-to-string last-input-event))))
    (map-put!
     im-tab-configuration
     cfg
     (current-window-configuration))
    (message ">> Configuration saved as %s" cfg)))

(defun im-tab--seq-find-index (fn seq)
  "Return the first index in SEQ for which FN evaluate to non-nil."
  (seq-position seq 'dummy-item (lambda (it _) (funcall fn it))))

;;;###autoload
(defun im-tab-configuration-restore-current (&optional sticky?)
  "Save window configuration for current key.
If STICKY? is non-nil, move the currently focused buffer to restored
layout's main area."
  (interactive)
  (let* ((cb (current-buffer))
         (tabs (funcall tab-bar-tabs-function))
         (current-tab (alist-get 'current-tab tabs))
         (name (alist-get 'name current-tab))
         (tab-pos (im-tab--seq-find-index (lambda (it) (equal name (alist-get 'name it))) tabs))
         (explicit-name? (alist-get 'explicit-name current-tab))
         (_ (unless explicit-name?
              (setq name (format "<tab:%s>" tab-pos))))
         (concat "<tab:" name ">" "-" (char-to-string last-input-event)))
    (when (and (not explicit-name?) im-tab-warn-on-unnamed-tabs)
      (warn  "Use explicit names on tabs to avoid surprises."))
    (set-window-configuration
     (map-elt
      im-tab-configuration
      cfg))
    (when (and sticky? cb)
      (switch-to-buffer cb))
    (message ">> Restored configuration %s" cfg)))

;;;###autoload
(defun im-tab-configuration-restore-current-sticky ()
  "Save window configuration for current key."
  (interactive)
  (im-tab-configuration-restore-current t))

;;;; Footer

(provide 'im-tab)

;;; im-tab.el ends here
