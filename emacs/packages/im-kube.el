;;; im-kube.el --- my kubectl wrappers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: kubernetes

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

;; There are some alternatives:
;;
;; - https://github.com/kubernetes-el/kubernetes-el
;; - https://github.com/eshelyaron/kubed
;; - https://github.com/abrochard/kubel
;;
;; They all work fine and quite similar but this is mine, tailored to
;; my specific needs.
;;
;; Most important function is `im-kube-context-overview'.  It shows
;; all pods belonging to a context and a namespace (which can be also
;; --all-namespaces) in a nicely formatted vtable.  Context and
;; namespace can be changed with in-buffer configurations.

;;; Code:

(require 's)
(require 'dash)
(require 'vtable)
(require 'page-break-lines)
(require 'vterm)

(require 'im)

;;;; Customization

;; ...

;;;; Variables

(defvar-local im-kube--current-context nil)
(defvar-local im-kube--current-namespace nil)

;;;; Main

;;;###autoload
(defun im-kube-use-context ()
  (interactive)
  (im-output-select
   :cmd "kubectl config get-contexts --output=name"
   :keep-order t
   :prompt (format "Select context (current=%s): " (im-kube--current-context))
   :do (shell-command-to-string (format "kubectl config use-context '%s'" it))))

(defalias 'im-kube-get-context-server-ip 'im-kube-get-cluster-server-ip)

;;;###autoload
(defun im-kube-get-cluster-server-ip ()
  (interactive)
  (im-output-select
   :cmd "kubectl config get-clusters"
   :drop 1
   :keep-order t
   :prepend '("")
   :prompt (format "Select context (leave blank to use %s): " (im-kube--current-context))
   :do
   (->>
    (if (s-blank? it)
        (nth 1 (s-split "@" (im-kube--current-context)))
      it)
    (format "kubectl config view -o jsonpath=\"{.clusters[?(@.name == '%s')].cluster.server}\"")
    shell-command-to-string
    im-kill)))

;;;###autoload
(defun im-kube-context-overview (&optional context namespace)
  "List all pods of CONTEXT and NAMESPACE in a nicely formatted buffer."
  (interactive (progn
       (when current-prefix-arg
         (im-kube-use-context))
       (list
        (im-kube--current-context)
        (im-kube--select-namespace))))
  (let ((all-namespaces? (equal namespace "--all-namespaces")))
    (with-current-buffer (get-buffer-create (format "*im-kube-context: %s/%s*" context namespace))
      (erase-buffer)
      (setq-local im-kube--current-context context)
      (setq-local im-kube--current-namespace namespace)
      (insert "Context:   ")
      (insert-text-button
       (or context (im-kube--current-context))
       'action
       (lambda (_button)
         (im-kube-context-overview
          (progn (im-kube-use-context) (im-kube--current-context))
          (im-kube--select-namespace (im-kube--current-context) namespace)))
       'follow-link t)
      (insert "\n")
      (insert "Namespace: ")
      (insert-text-button
       (im-kube--extract-namespace-name (or namespace (im-kube--current-namespace)))
       'action
       (lambda (_button)
         (im-kube-context-overview context (im-kube--select-namespace)))
       'follow-link t)
      (insert "\n\n")
      (page-break-lines-mode)
      (make-vtable
       :row-colors (im-vtable--pretty-colors)
       :column-colors (im-vtable--pretty-colors)
       :columns
       `(,@(when all-namespaces? '("Namespace"))
         "Name"
         (:name "Ready" :min-width 6)
         (:name "Status"
          :min-width 11
          :formatter (lambda (value)
                       (cond
                        ((equal value "Running") (im-kube--color "green" value))
                        ((equal value "Running") (im-kube--color "green" value))
                        ((-contains? '("Error" "CrashLoopBackOff" "NotReady") value) (im-kube--color "red" value))
                        (t (im-kube--color "yellow" value)))))
         (:name "Restarts"
          :min-width 8
          :formatter (lambda (value)
                       (if (ignore-errors (> (string-to-number value) 0))
                           (im-kube--color "red" value)
                         value)))
         (:name "Age" :min-width 3))
       :use-header-line nil
       :objects-function (lambda ()
                           (->>
                            (format "kubectl get pods %s --no-headers --context=%s" namespace context)
                            (shell-command-to-string)
                            (s-trim)
                            (s-split "\n")
                            (--map (let ((data (s-split " " it t))
                                         (offset (if all-namespaces? 0 -1)))
                                     (list
                                      :namespace (if all-namespaces?
                                                     (format "--namespace='%s'" (nth offset data))
                                                   namespace)
                                      :name (nth (+ offset 1) data) :ready (nth (+ offset 2) data)
                                      :status (nth (+ offset 3) data)
                                      :restarts (nth (+ offset 4) data)
                                      :age (nth (+ offset 5) data)
                                      :context context)))))
       :actions `("RET" im-kube-pod--act
                  ;; "L" im-kube-pod--logs
                  "F" im-kube-pod--logs-follow
                  "x" im-kube-pod--remove
                  "i" im-kube-pod--info)
       :getter (lambda (object column vtable)
                 (let-plist object
                   (pcase (vtable-column vtable column)
                     ("Namespace" (im-kube--extract-namespace-name .namespace))
                     ("Name" .name)
                     ("Ready" .ready)
                     ("Status" .status)
                     ("Restarts" .restarts)
                     ("Age" .age)))))
      (switch-to-buffer (current-buffer)))))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "kubeoverview"
   :follow (lambda (link _) (im-kube-context-overview link))
   :store (lambda ()
            (when (s-prefix? "*im-kube: " (buffer-name))
              (org-link-store-props
               :type "kubeoverview"
               :description im-kube--current-context
               :link
               (format "kubeoverview:context=%s;namespace=%s"
  im-kube--current-context
  im-kube--current-namespace))))))

;;;;; Pod actions

(defun im-kube-pod--act (pod &optional container)
  (let ((namespace (plist-get pod :namespace))
        (name (plist-get pod :name))
        (context (plist-get pod :context)))
    (empv--select-action "Action for"
      "Exec into container" → (im-kube-pod--exec-into-container pod container)
      "Logs (follow)" → (im-kube-pod--logs pod container)
      "Logs (follow, term)" → (im-kube-pod--logs-follow pod container)
      "Logs (to a file)" → (im-kube-pod--logs-to-file pod container)
      "Logs (previous pod)" → (im-kube-pod--previous-logs pod container)
      "App logs" → (im-kube-pod--app-logs pod)
      "Top" → (im-kube-pod--top pod)
      "Remove" → (im-kube-pod--remove pod)
      "Events" → (im-kube-pod--events pod)
      "Info" → (im-kube-pod--info pod))))

(defun im-kube-pod--select-container (pod)
  (im-output-select
   :cmd (format
         "kubectl get pods '%s' %s --context='%s' -o jsonpath='{.spec.containers[*].name}'"
         (plist-get pod :name)
         (plist-get pod :namespace)
         (plist-get pod :context))
   :prompt (format "Container for %s: " (plist-get pod :name))
   :split " "))

(defun im-kube-pod--info (pod)
  "Pod info action."
  (with-current-buffer (get-buffer-create (format "*im-kube-pod-info-%s*" (plist-get pod :name)))
    (insert
     (shell-command-to-string (format "kubectl get pod '%s' %s --context='%s' --output=json"
                                      (plist-get pod :name)
                                      (plist-get pod :namespace)
                                      (plist-get pod :context))))
    (json-ts-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

(defun im-kube-pod--top (pod)
  (im-shell-command
   :switch t
   :eat t
   :buffer-name (format "*im-kube-top: %s*" (plist-get pod :name))
   :command "watch"
   :args (list "--differences"
               "--beep"
               "--errexit"
               "--interval" "1"
               "kubectl" "top" "pod"
               (plist-get pod :name)
               (plist-get pod :namespace)
               "--context" (plist-get pod :context))))

(defun im-kube-pod--remove (pod)
  (when (y-or-n-p (format "Do you really want to remove %s? " (plist-get pod :name)))
    (im-shell-command
     :switch t
     :command (im-kill (format "kubectl delete pod '%s' %s --context='%s'"
                               (plist-get pod :name)
                               (plist-get pod :namespace)
                               (plist-get pod :context))))))

(defun im-kube-pod--events (pod)
  (shell-command
   (im-kill
    (format
     "kubectl events --for 'pod/%s' %s --context='%s'"
     (plist-get pod :name)
     (plist-get pod :namespace)
     (plist-get pod :context)))
   (format "*im-kube-events:%s*" (plist-get pod :name))))

(defun im-kube-pod--app-logs (pod &optional container)
  ;; Logs from all pods combined for given pod's app
  (let ((container (or container (im-kube-pod--select-container pod))))
    (with-current-buffer (im-eshell (format "$pod: %s" (plist-get pod :name)))
      (insert (im-kill
               (format "kubectl logs -f --selector app=%s %s --container='%s' --context='%s'"
                       (im-kube-pod--get-app-name pod)
                       (plist-get pod :namespace)
                       container
                       (plist-get pod :context)))))))

(defun im-kube-pod--previous-logs (pod &optional container)
  (let ((container (or container (im-kube-pod--select-container pod))))
    (shell-command
     (im-kill
      (format
       "kubectl logs %s --since=0 %s --container='%s' --context='%s' -p"
       (plist-get pod :name)
       (plist-get pod :namespace)
       container
       (plist-get pod :context)))
     (format "*im-kube-logs:%s-%s-PREVIOUS*" (plist-get pod :name) container))))

(defun im-kube-pod--logs-to-file (pod &optional container)
  (let ((container (or container (im-kube-pod--select-container pod)))
        (fname (expand-file-name (read-file-name
                                  "File: "
                                  "~/Workspace/temp/"
                                  nil
                                  nil
                                  (format "%s-%s.logs" (format-time-string "%Y-%m-%d") (plist-get pod :name))))))
    (message ">> Downloading logs for %s. Done." (plist-get pod :name))
    (set-process-sentinel
     (start-process-shell-command
      "*im-kube-log-write*"
      "*im-kube-log-write*"
      (im-kill (format "kubectl logs %s --since=0 %s --container='%s' --context='%s' > '%s'"
                       (plist-get pod :name)
                       (plist-get pod :namespace)
                       container
                       (plist-get pod :context)
                       fname)))
     (lambda (proc event)
       (if (eq (process-exit-status proc) 0)
           (progn
             (message ">> Downloading logs for %s. Done." (plist-get pod :name))
             (find-file fname))
         (user-error "Failed to get logs"))))))

(defun im-kube-pod--logs-follow (pod &optional container)
  (let ((container (or container (im-kube-pod--select-container pod))))
    (with-current-buffer (vterm (format "$pod: %s" (plist-get pod :name)))
      (vterm-insert
       (im-kill (format "kubectl logs %s -f %s --container='%s' --context='%s'"
                        (plist-get pod :name)
                        (plist-get pod :namespace)
                        container
                        (plist-get pod :context)))))))

(defun im-kube-pod--logs (pod &optional container)
  (let ((container (or container (im-kube-pod--select-container pod))))
    (async-shell-command
     (im-kill
      (format
       "kubectl logs %s --since=0 %s --container='%s' --context='%s' --follow"
       (plist-get pod :name)
       (plist-get pod :namespace)
       container
       (plist-get pod :context)))
     (format "*im-kube-logs:%s-%s*" (plist-get pod :name) container))))

(defun im-kube-pod--exec-into-container (pod &optional container)
  (let ((container (or container (im-kube-pod--select-container pod))))
    (with-current-buffer (im-vterm (format "$pod: %s" (plist-get pod :name)))
      (vterm-insert
       (im-kill (format "kubectl exec %s --container='%s' -i -t '%s' --context='%s' -- bash"
                        (plist-get pod :namespace)
                        container
                        (plist-get pod :name)
                        (plist-get pod :context)))))))

(defun im-kube-pod--get-app-name (pod)
  "Return the application name of the POD belongs."
  (s-trim
   (shell-command-to-string
    (format
     "kubectl get pod '%s' %s --context='%s' -o custom-columns=:metadata.labels.app"
     (plist-get pod :name)
     (plist-get pod :namespace)
     (plist-get pod :context)))))

;;;;; Utils

(defun im-kube--select-namespace (&optional context initial)
  (im-output-select
   :cmd (format "kubectl get namespaces %s --no-headers" (if context (concat "--context=" context) ""))
   :initial initial
   :prompt "Select namespace:"
   :prepend '("--all-namespaces")
   :map (concat
         (propertize "--namespace='" 'face '(:foreground "darkgray"))
         (car (s-split " " it))
         (propertize "'" 'face '(:foreground "darkgray")))
   :do (substring-no-properties it)))

(defun im-kube--current-namespace ()
  (s-trim (shell-command-to-string "kubectl config view --minify --output 'jsonpath={..namespace}'")))

(defun im-kube--extract-namespace-name (namespace)
  (cond
   ((string-match "--namespace=\'\\([^\"]+\\)\'" namespace)
    (match-string 1 namespace))
   ((string-match "--all-namespaces" namespace)
    "All Namespaces")))

(defun im-kube--current-context ()
  (s-trim (shell-command-to-string "kubectl config current-context")))

(defun im-kube--color (color s)
  (propertize s 'face `(:foreground ,color)))

;;;; Footer

(provide 'im-kube)
;;; im-kube.el ends here
