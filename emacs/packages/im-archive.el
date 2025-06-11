;;; im-archive.el --- Archive URLs with ease  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: utility, web

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

;; This package allows you to archive URLs in readable/searchable
;; formats.
;;
;; It uses special packages for different websites, like:
;;
;; | Website       | Package                               | Author     |
;; |---------------|---------------------------------------|------------|
;; | Reddit        | reddigg                               | thanhvg    |
;; | HackerNews    | hnreader                              | thanhvg    |
;; | StackOverflow | howdoyou                              | thanhvg    |
;; |---------------|---------------------------------------|------------|
;; | Rest          | eww or single-file (external program) | *in-place* |
;;
;; You can also add extra handlers for specific websites by extending
;; `im-archive-handlers'.

;; TODO: Implement eww/shr backend.

;;;; Usage:

;; Simply call the `im-archive' function and follow instructions.
;; It'll ask you for a URL to archive and will call the relevant
;; archiving function.
;;
;; If you are in a buffer/file that you want to archive, call
;; `im-archive-buffer' to archive the current buffer.  It'll skip the
;; specific handler part and will archive the buffer into your
;; archives directory directly, applying simple post processing if
;; needed (See `im-archive-post-archive-hook', which runs in the
;; archived contents buffer).

;;; Code:

(require 'im)
(require 'url)

;;;; Customization

(defvar im-archive-handlers
  `((".*reddit.com/r/[a-zA-Z0-9_-]+/comments/[a-zA-Z0-9_-]+/\\([a-zA-Z0-9_-]+/?\\)\\(/[a-zA-Z0-9_-]+/?\\)?\\(&.*\\)?$" . (:handler im-archive-url--reddit :extension ".org"))
    (".*news.ycombinator.com/item\\?id=.*" . (:handler im-archive-url--hackernews :extension ".org"))
    (".*stackoverflow.com/questions/.*" . (:handler im-archive-url--stackexchange :extension ".org"))
    (".*" . (:handler im-archive-url-singlefile :extension ".html")))
  "Alist mapping URL regexps to specialized archival handlers.")

(defvar im-archive-dir (expand-file-name "~/archives/")
  "Default directory for archived contents.")

(defvar im-archive-post-archive-hook nil
  "Hook run after archiving content, with the archived buffer current.")

(defvar im-archive-chromium-path
  (if (eq system-type 'darwin)
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "chromium-browser")
  "Chromium browser path for use with single-file.")

;;;; Main

;;;###autoload
(cl-defun im-archive-url (url where &key on-finish on-fail)
  "Archive the URL into WHERE.
WHERE can be a directory or a file.  If it's a directory, it should
already exists otherwise WHERE is interpreted as a file name.

ON-FINISH is called with the created HTML file path, and ON-FAIL
is called with a symbol/string indicating the failure."
  (interactive (im-archive--interactive-handler))
  (setq where (expand-file-name where))
  (let ((handler (cdar (im-assoc-regexp url im-archive-handlers))))
    (funcall
     (plist-get handler :handler) url where
     :handler handler
     :on-finish on-finish
     :on-fail on-fail)))

;;;;; Utility

(defun im-archive--interactive-handler ()
  (list (read-string
         "URL to archive: "
         (or (when (derived-mode-p 'org-mode)
               (im-org-link-copy))
             (thing-at-point 'url)))
        (read-directory-name "Save files into: " im-archive-dir)))

(defun im-archive--get-org-mode-header ()
  "Get the #+TITLE: of current org mode buffer."
  (car (org-element-map (org-element-parse-buffer) 'keyword
         (lambda (kw)
           (when (string= (org-element-property :key kw) "TITLE")
             (org-element-property :value kw)))
         t)))

(defun im-archive--generate-filename (path title ext)
  (cond
   ((f-dir? path) (f-join path (concat (format-time-string "%Y%m%dT%H%M-") (im-string-url-case (or title "")) ext)))
   (t path)))

;;;;; Handlers

;; All handlers should include "&rest _" in their parameters. The
;; absolute minimum for the signatures is:
;;
;;   (url where &rest _ &key handler on-finish on-fail <other options, if applicable>)

;; Handlers also can be interactive functions.

;; TODO: Maybe add a transient UI for this?

;;;###autoload
(cl-defun im-archive-url-singlefile (url where &rest _ &key crawl (tidy t)
                                         handler
                                         (on-finish (lambda (file) (message "Archived into: %s" file)))
                                         (on-fail (lambda (err) (message "Failed: %s" err))))
  "Archive URL with single-file.
WHERE is to path to save, it might be a directory or full file
name.  In case it's a directory, then the file name will be
decided by single-file.

Set CRAWL to t or a number to crawl referenced links in the page.
If passed t, then only 1-depth links will be crawled otherwise
the depth is set to given number.

HANDLER is the handler definition, which may contain some useful
properties for generating filename etc.

Setting TIDY to t runs tidy command on resulting HTML file.

ON-FINISH is called with the created HTML file path, and ON-FAIL
is called with a symbol/string indicating the failure."
  (interactive (im-archive--interactive-handler))
  (setq where (expand-file-name where))
  (let* ((command
          (format
           "%s --filename-replacement-character='-' %s %s \"%s\" \"%s\""
           (im-ensure-binary "deno run -A npm:single-file-cli" :installer "Install deno and thats it.")
           (format "--browser-executable-path=%s" (shell-quote-argument im-archive-chromium-path))
           ;; (if (eq backend 'firefox)
           ;;     "--back-end=webdriver-gecko"
           ;;   "")
           (if crawl
               ;; FIXME --crawl-replace-urls=true does not work
               (format "--crawl-links=true --crawl-external-links-max-depth=%s --crawl-max-depth=1 --crawl-replace-urls=true --crawl-inner-links-only=false --crawl-no-parent=true"
                       (if (numberp crawl) crawl 1))
             "")
           url
           (cond
            ((f-dir? where) (format "--output-directory=%s" (shell-quote-argument where)))
            (t where)))))
    (im-shell-command
     :command command
     :switch nil
     :buffer-name "*single-file output*"
     :on-finish
     (lambda (&rest _)
       (when (called-interactively-p 'any)
         (message "Archived `%s'!" url)
         (kill-new command))
       ;; single-file does not return the created file name, finding that manually
       (let ((created-file (cond
                            ((f-file? where) where)
                            ((f-dir? where) (im-latest-file where))
                            (t (error "Can't find file created backup file")))))
         (when tidy
           (shell-command
            (format "%s -q -w 120 -m '%s'"
                    (im-ensure-binary "tidy")
                    created-file)))
         (and on-finish (funcall on-finish created-file))))
     :on-fail
     (lambda (&rest _)
       (when (called-interactively-p 'any)
         (message "Archiving failed with single-file: '%s'. Command is copied to your kill ring." url)
         (kill-new command))
       (and on-fail (funcall on-fail 'abnormal-exit))))))

(cl-defun im-archive-url--reddit (link where &rest _ &key handler on-finish on-fail)
  "Archive Reddit comment page LINK to WHERE in `org-mode'.
See `im-archive-url' for WHERE's definition.

HANDLER is the handler definition, which may contain some useful
properties for generating filename etc.

ON-FINISH is called with the created ORG file path, and ON-FAIL
is called with a symbol/string indicating the failure."
  (unless (require 'reddigg nil t)
    (user-error "You need to install `reddigg' package to archive Reddit URLs"))
  (setq link (substring link (length "https://www.reddit.com/") nil))
  (promise-chain (reddigg--promise-comments link)
    (then #'reddigg--print-comments)
    (then (lambda (&rest _)
            (with-current-buffer (reddigg--get-cmt-buffer)
              (write-region
               (buffer-substring-no-properties (point-min) (point-max))
               nil
               (im-archive--generate-filename where (im-archive--get-org-mode-header) (plist-get handler :extension)))
              (funcall on-finish where))))
    (promise-catch (lambda (reason) (funcall on-fail reason)))))

(cl-defun im-archive-url--hackernews (link where &rest _ &key handler on-finish on-fail)
  "Archive HackerNews comment page LINK to WHERE in `org-mode'.
See `im-archive-url' for WHERE's definition.

HANDLER is the handler definition, which may contain some useful
properties for generating filename etc.

ON-FINISH is called with the created ORG file path, and ON-FAIL
is called with a symbol/string indicating the failure."
  (unless (require 'hnreader nil t)
    (user-error "You need to install `hnreader' package to archive HackerNews URLs"))
  (promise-chain (hnreader--promise-dom link)
    (then (lambda (dom) (hnreader--print-comments dom link)))
    (then (lambda (&rest _)
            (with-current-buffer (hnreader--get-hn-comment-buffer)
              (write-region
               (buffer-substring-no-properties (point-min) (point-max))
               nil
               (im-archive--generate-filename where (im-archive--get-org-mode-header) (plist-get handler :extension)))
              (funcall on-finish where))))
    (promise-catch (lambda (reason) (funcall on-fail reason)))))

(cl-defun im-archive-url--stackexchange (link where &rest _ &key handler on-finish on-fail)
  "Archive HackerNews comment page LINK to WHERE in `org-mode'.
See `im-archive-url' for WHERE's definition.

HANDLER is the handler definition, which may contain some useful
properties for generating filename etc.

ON-FINISH is called with the created ORG file path, and ON-FAIL
is called with a symbol/string indicating the failure."
  (unless (require 'howdoyou nil t)
    (user-error "You need to install `howdoyou' package to archive StackOverflow URLs"))
  (promise-chain (howdoyou-read-so-link link)
    (then (lambda (&rest _)
            (with-current-buffer (howdoyou--get-buffer)
              (write-region
               (buffer-substring-no-properties (point-min) (point-max))
               nil
               (im-archive--generate-filename where (im-archive--get-org-mode-header) (plist-get handler :extension))))
            (funcall on-finish where)))
    (promise-catch (lambda (reason) (funcall on-fail reason)))))

;;;; Footer

(provide 'im-archive)

;;; im-archive.el ends here
