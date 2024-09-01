;;; im-tab.el --- TODO: Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/im-tab.el
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

;; My tab-bar related extensions.

;;; Code:

(require 'map)
(require 'tab-bar)

;;;; Save window configurations to a predefined keybinding ephemerally

;; When I work with a specific feature, I generally set up a window
;; configuration for that but of course that changes pretty quickly
;; when you look up for something or some buffer pops up and destroys
;; your configuration.  These changes can be moderated with
;; `tab-bar-history-back' and `tab-bar-history-forward' and you can
;; return to your specific setup.  But sometimes, the yak shaving goes
;; too deep and your specific window configuration is buried deep
;; inside your `tab-bar-history' or it's even completely lost.  Also
;; it is not a very pleasant experience to go back and forth for a
;; specific configuration.  The following configuration solves this.
;; It let's you assign specific window configuration to specific keys
;; and restore them with those specific keys.

(defvar im-tab-configuration (make-hash-table :test 'equal :size 20))

;;;###autoload
(defun im-tab-configuration-save-current ()
  "Save current window configuration to a key.

  This function saves the window configuration using `last-input-event'.
It is advised to bind this function to a key chords like

  C-x w w {1,2,3,4...}

so that you can retrieve with same number keys later.  Assuming you
bind `im-tab-configuration-restore-current' to:

  C-x w {1,2,3,4...}

Now you will be able to save the window configuration for the current
tab with \"C-x w w 1\" and then retrieve it back with \"C-x w 1\" in the
same tab.  Configurations are saved per tab."
  (interactive)
  (let* ((current-tab (alist-get 'current-tab (funcall tab-bar-tabs-function)))
         (name (alist-get 'name current-tab))
         (explicit-name? (alist-get 'explicit-name current-tab))
         (cfg (concat name (char-to-string last-input-event))))
    (unless explicit-name?
      (user-error "This function does not work on unnamed tabs"))
    (map-put!
     im-tab-configuration
     cfg
     (current-window-configuration))
    (message ">> Configuration saved as %s" cfg)))

;;;###autoload
(defun im-tab-configuration-restore-current ()
  "Save window configuration for current key."
  (interactive)
  (let* ((current-tab (alist-get 'current-tab (funcall tab-bar-tabs-function)))
         (name (alist-get 'name current-tab))
         (explicit-name? (alist-get 'explicit-name current-tab))
         (cfg (concat name (char-to-string last-input-event))))
    (unless explicit-name?
      (user-error "This function does not work on unnamed tabs"))
    (set-window-configuration
     (map-elt
      im-tab-configuration
      cfg))
    (message ">> Restored configuration %s" cfg)))

;;;; Footer

(provide 'im-tab)

;;; im-tab.el ends here
