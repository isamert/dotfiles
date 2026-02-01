;;; im-arr.el --- arr-stack functionality -*- lexical-binding: t; -*-

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.1.0
;; Homepage: https://github.com/isamert/dotfiles
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, convenience

;;; Commentary:

;; * im-radarr
;;
;; This package provides functionality to add movies to a Radarr instance.
;; Configure `im-radarr-url' and `im-radarr-api-key' before using.
;;
;; Usage:
;;   M-x im-radarr-add-movie
;;
;; You will be prompted for an IMDB ID (or it'll be detected
;; automatically from the current org header) and can interactively
;; select a quality profile and root folder.
;;
;; Other functions are:
;;   M-x im-radarr-{search-and-add-movie,list-movies}
;;
;; This is 95% AI generated but I reviewed it, kind of.

;;; Code:

(require 'cl-lib)
(require 'im)

;;;; Customize

(defgroup im-radarr nil
  "Radarr integration for Emacs."
  :group 'applications
  :prefix "im-radarr-")

(defcustom im-radarr-url nil
  "URL of your Radarr instance.
Example: \"http://localhost:7878\""
  :type 'string
  :group 'im-radarr)

(defcustom im-radarr-api-key nil
  "API key for your Radarr instance.
You can find this in Radarr under Settings -> General -> Security."
  :type 'string
  :group 'im-radarr)

(defcustom im-radarr-default-monitored t
  "Whether newly added movies should be monitored by default."
  :type 'boolean
  :group 'im-radarr)

(defcustom im-radarr-search-on-add t
  "Whether to search for the movie immediately after adding."
  :type 'boolean
  :group 'im-radarr)

(defcustom im-radarr-minimum-availability "released"
  "Default minimum availability for added movies.
Possible values: \"announced\", \"inCinemas\", \"released\", \"deleted\"."
  :type '(choice (const "announced")
                 (const "inCinemas")
                 (const "released")
                 (const "deleted"))
  :group 'im-radarr)

;;;; Internal variables

(defvar im-radarr--quality-profiles-cache nil
  "Cached quality profiles.")

(defvar im-radarr--root-folders-cache nil
  "Cached root folders.")

;;;; Helper functions

(defun im-radarr--ensure-config ()
  "Ensure Radarr URL and API key are configured."
  (unless im-radarr-url
    (user-error "Please set `im-radarr-url' (e.g., \"http://localhost:7878\")"))
  (unless im-radarr-api-key
    (user-error "Please set `im-radarr-api-key'")))

(defun im-radarr--api-url (endpoint)
  "Construct full API URL for ENDPOINT."
  (concat (string-trim-right im-radarr-url "/")
          "/api/v3/"
          (string-trim-left endpoint "/")))

(defun im-radarr--api-headers ()
  "Return headers for Radarr API requests."
  `(:X-Api-Key ,im-radarr-api-key
    :Content-Type "application/json"))

(defun im-radarr--get (endpoint)
  "Make a GET request to ENDPOINT."
  (im-radarr--ensure-config)
  (im-request (im-radarr--api-url endpoint)
    :-headers (im-radarr--api-headers)))

(defun im-radarr--post (endpoint data)
  "Make a POST request to ENDPOINT with DATA."
  (im-radarr--ensure-config)
  (im-request (im-radarr--api-url endpoint)
    :-type "POST"
    :-headers (im-radarr--api-headers)
    :-data data))

;;;; Quality Profiles

(defun im-radarr--fetch-quality-profiles ()
  "Fetch quality profiles from Radarr."
  (or im-radarr--quality-profiles-cache
      (setq im-radarr--quality-profiles-cache
            (im-radarr--get "qualityprofile"))))

(defun im-radarr--select-quality-profile ()
  "Interactively select a quality profile.
Returns the profile ID."
  (let* ((profiles (im-radarr--fetch-quality-profiles))
         (profile-names (mapcar (lambda (p)
                                  (cons (alist-get 'name p)
                                        (alist-get 'id p)))
                                profiles))
         (selected-name (completing-read "Quality Profile: "
                                         (mapcar #'car profile-names)
                                         nil t)))
    (cdr (assoc selected-name profile-names))))

;;;; Root Folders

(defun im-radarr--fetch-root-folders ()
  "Fetch root folders from Radarr."
  (or im-radarr--root-folders-cache
      (setq im-radarr--root-folders-cache
            (im-radarr--get "rootfolder"))))

(defun im-radarr--select-root-folder ()
  "Interactively select a root folder.
Returns the folder path."
  (let* ((folders (im-radarr--fetch-root-folders))
         (folder-paths (mapcar (lambda (f)
                                 (let ((path (alist-get 'path f))
                                       (free (alist-get 'freeSpace f)))
                                   (cons (format "%s (%.1f GB free)"
                                                 path
                                                 (/ (or free 0) 1073741824.0))
                                         path)))
                               folders))
         (selected (completing-read "Root Folder: "
                                    (mapcar #'car folder-paths)
                                    nil t)))
    (cdr (assoc selected folder-paths))))

;;;; Movie Lookup

(defun im-radarr-lookup-movie (imdb-id)
  "Look up a movie by IMDB-ID.
Returns movie info from TMDB via Radarr."
  (im-radarr--ensure-config)
  (let ((result (im-request (im-radarr--api-url "movie/lookup")
                  :-headers (im-radarr--api-headers)
                  :term (concat "imdb:" imdb-id))))
    (if (and result (listp result) (> (length result) 0))
        (car result)
      (user-error "Movie not found for IMDB ID: %s" imdb-id))))

;;;; Add Movie

(defun im-radarr--build-movie-payload (movie-info quality-profile-id root-folder-path)
  "Build the payload for adding a movie.
MOVIE-INFO is the result from lookup.
QUALITY-PROFILE-ID is the selected quality profile.
ROOT-FOLDER-PATH is the selected root folder."
  `((title . ,(alist-get 'title movie-info))
    (qualityProfileId . ,quality-profile-id)
    (tmdbId . ,(alist-get 'tmdbId movie-info))
    (titleSlug . ,(alist-get 'titleSlug movie-info))
    (images . ,(vconcat (alist-get 'images movie-info)))
    (year . ,(alist-get 'year movie-info))
    (rootFolderPath . ,root-folder-path)
    (monitored . ,im-radarr-default-monitored)
    (minimumAvailability . ,im-radarr-minimum-availability)
    (addOptions . ((searchForMovie . ,im-radarr-search-on-add)))))

(declare-function org-entry-get "org")

;;;###autoload
(defun im-radarr-add-movie (imdb-id)
  "Add a movie to Radarr by IMDB-ID.
Interactively prompts for quality profile and root folder."
  (interactive (list
                (or (when (derived-mode-p 'org-mode)
                      (when-let* ((val (or (org-entry-get nil "IMDB")
                                           (org-entry-get nil "IMDB-ID"))))
                        (when (string-match "\\(tt[0-9]+\\)" val)
                          (match-string 1 val))))
                    (read-string "IMDB ID (e.g., tt1234567): "))))
  (im-radarr--ensure-config)
  ;; Normalize IMDB ID
  (unless (string-prefix-p "tt" imdb-id)
    (setq imdb-id (concat "tt" imdb-id)))
  (message "Looking up movie...")
  (let* ((movie-info (im-radarr-lookup-movie imdb-id))
         (title (alist-get 'title movie-info))
         (year (alist-get 'year movie-info)))
    (when (y-or-n-p (format "Add \"%s (%s)\" to Radarr? " title year))
      (let* ((quality-profile-id (im-radarr--select-quality-profile))
             (root-folder-path (im-radarr--select-root-folder))
             (payload (im-radarr--build-movie-payload
                       movie-info quality-profile-id root-folder-path))
             (result (im-radarr--post "movie" payload)))
        (if (alist-get 'id result)
            (message "Successfully added \"%s\" to Radarr!" title)
          (if-let ((error-msg (alist-get 'message result)))
              (user-error "Failed to add movie: %s" error-msg)
            (user-error "Failed to add movie: %S" result)))))))

;;;###autoload
(defun im-radarr-search-and-add-movie ()
  "Search for a movie by title and add it to Radarr."
  (interactive)
  (im-radarr--ensure-config)
  (let* ((query (read-string "Search movie: "))
         (results (im-request (im-radarr--api-url "movie/lookup")
                    :-headers (im-radarr--api-headers)
                    :term query)))
    (if (and results (> (length results) 0))
        (let* ((candidates (mapcar (lambda (m)
                                     (cons (format "%s (%s) [%s]"
                                                   (alist-get 'title m)
                                                   (or (alist-get 'year m) "?")
                                                   (or (alist-get 'imdbId m) "no imdb"))
                                           m))
                                   results))
               (selected-name (completing-read "Select movie: "
                                               (mapcar #'car candidates)
                                               nil t))
               (movie-info (cdr (assoc selected-name candidates)))
               (title (alist-get 'title movie-info))
               (year (alist-get 'year movie-info)))
          (when (y-or-n-p (format "Add \"%s (%s)\" to Radarr? " title year))
            (let* ((quality-profile-id (im-radarr--select-quality-profile))
                   (root-folder-path (im-radarr--select-root-folder))
                   (payload (im-radarr--build-movie-payload
                             movie-info quality-profile-id root-folder-path))
                   (result (im-radarr--post "movie" payload)))
              (if (alist-get 'id result)
                  (message "Successfully added \"%s\" to Radarr!" title)
                (if-let ((error-msg (alist-get 'message result)))
                    (user-error "Failed to add movie: %s" error-msg)
                  (user-error "Failed to add movie: %S" result))))))
      (user-error "No movies found for query: %s" query))))

;;;; Other interactive functions

;;;###autoload
(defun im-radarr-clear-cache ()
  "Clear cached quality profiles and root folders."
  (interactive)
  (setq im-radarr--quality-profiles-cache nil)
  (setq im-radarr--root-folders-cache nil)
  (message "Radarr cache cleared."))

;;;###autoload
(defun im-radarr-list-movies ()
  "List all movies in Radarr library."
  (interactive)
  (im-radarr--ensure-config)
  (let* ((movies (im-radarr--get "movie"))
         (candidates (mapcar (lambda (m)
                               (format "%-50s %4s  %s"
                                       (truncate-string-to-width
                                        (alist-get 'title m) 50 nil nil "…")
                                       (or (alist-get 'year m) "?")
                                       (if (alist-get 'hasFile m) "✓" "✗")))
                             (seq-sort-by (lambda (m) (alist-get 'title m))
                                          #'string< movies))))
    (with-current-buffer (get-buffer-create "*Radarr Movies*")
      (erase-buffer)
      (insert (format "Radarr Library (%d movies)\n" (length movies)))
      (insert (make-string 60 ?─) "\n\n")
      (dolist (c candidates)
        (insert c "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'im-arr)
;;; im-arr.el ends here
