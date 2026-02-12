;;; im-arr.el --- arr-stack functionality -*- lexical-binding: t; -*-

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.1.0
;; Homepage: https://github.com/isamert/dotfiles
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, convenience

;;; Commentary:

;; * im-arr
;;
;; This package provides integrations for Radarr (movies) and Sonarr
;; (series). Configure:
;;
;; - `im-radarr-url' / `im-radarr-api-key'
;; - `im-sonarr-url' / `im-sonarr-api-key'
;;
;; Main commands:
;;
;; - `im-radarr-add-movie'
;; - `im-sonarr-add-series'
;;
;; Other commands:
;;
;; - `im-radarr-{search-and-add-movie,list-movies,clear-cache}'
;; - `im-sonarr-{search-and-add-series,list-series,clear-cache}'
;;
;; This is 95% AI generated but I reviewed it, kind of.

;;; Code:

(require 'cl-lib)
(require 'im)
(require 'subr-x)

;;;; Customize

(defgroup im-arr nil
  "ARR stack integrations for Emacs."
  :group 'applications
  :prefix "im-")

(defgroup im-radarr nil
  "Radarr integration for Emacs."
  :group 'im-arr
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

(defgroup im-sonarr nil
  "Sonarr integration for Emacs."
  :group 'im-arr
  :prefix "im-sonarr-")

(defcustom im-sonarr-url nil
  "URL of your Sonarr instance.
Example: \"http://localhost:8989\""
  :type 'string
  :group 'im-sonarr)

(defcustom im-sonarr-api-key nil
  "API key for your Sonarr instance.
You can find this in Sonarr under Settings -> General -> Security."
  :type 'string
  :group 'im-sonarr)

(defcustom im-sonarr-default-monitored t
  "Whether newly added series should be monitored by default."
  :type 'boolean
  :group 'im-sonarr)

(defcustom im-sonarr-search-on-add t
  "Whether to search for missing episodes immediately after adding."
  :type 'boolean
  :group 'im-sonarr)

(defcustom im-sonarr-season-folder t
  "Whether Sonarr should create season folders when adding series."
  :type 'boolean
  :group 'im-sonarr)

;;;; Internal variables

(defvar im-radarr--quality-profiles-cache nil
  "Cached quality profiles.")

(defvar im-radarr--root-folders-cache nil
  "Cached root folders.")

(defvar im-sonarr--quality-profiles-cache nil
  "Cached quality profiles.")

(defvar im-sonarr--root-folders-cache nil
  "Cached root folders.")

;;;; Helper functions

(defun im-arr--service-display-name (service)
  "Return human readable name of SERVICE."
  (capitalize (symbol-name service)))

(defun im-arr--service-var (service suffix)
  "Return variable symbol for SERVICE and SUFFIX."
  (intern (format "im-%s-%s" (symbol-name service) suffix)))

(defun im-arr--service-setting (service suffix)
  "Return SERVICE setting value for SUFFIX."
  (let ((var (im-arr--service-var service suffix)))
    (unless (boundp var)
      (error "Unknown setting variable: %s" var))
    (symbol-value var)))

(defun im-arr--ensure-config (service)
  "Ensure SERVICE URL and API key are configured."
  (let* ((url-var (im-arr--service-var service "url"))
         (api-key-var (im-arr--service-var service "api-key"))
         (url (im-arr--service-setting service "url"))
         (api-key (im-arr--service-setting service "api-key"))
         (service-name (im-arr--service-display-name service))
         (port (pcase service
                 ('radarr "7878")
                 ('sonarr "8989")
                 (_ "8080"))))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error "Please set `%s' (e.g., \"http://localhost:%s\")" url-var port))
    (unless (and (stringp api-key) (not (string-empty-p api-key)))
      (user-error "Please set `%s' for %s" api-key-var service-name))))

(defun im-arr--api-url (service endpoint)
  "Construct full API URL for SERVICE ENDPOINT."
  (concat (string-trim-right (im-arr--service-setting service "url") "/")
          "/api/v3/"
          (string-trim-left endpoint "/")))

(defun im-arr--api-headers (service)
  "Return headers for SERVICE API requests."
  `(:X-Api-Key ,(im-arr--service-setting service "api-key")
    :Content-Type "application/json"))

(cl-defun im-arr--request (service endpoint &rest params)
  "Make a request to SERVICE ENDPOINT with PARAMS."
  (im-arr--ensure-config service)
  (apply #'im-request
         (im-arr--api-url service endpoint)
         :-headers (im-arr--api-headers service)
         params))

(cl-defun im-arr--get (service endpoint &rest params)
  "Make a GET request to SERVICE ENDPOINT."
  (apply #'im-arr--request service endpoint params))

(defun im-arr--post (service endpoint data)
  "Make a POST request to SERVICE ENDPOINT with DATA."
  (im-arr--request service endpoint :-type "POST" :-data data))

(defun im-arr--cached-fetch (cache-var fetcher)
  "Return CACHE-VAR value or update it via FETCHER."
  (or (symbol-value cache-var)
      (set cache-var (funcall fetcher))))

(defun im-arr--fetch-quality-profiles (service cache-var)
  "Fetch quality profiles for SERVICE with CACHE-VAR."
  (im-arr--cached-fetch cache-var
                        (lambda ()
                          (im-arr--get service "qualityprofile"))))

(defun im-arr--fetch-root-folders (service cache-var)
  "Fetch root folders for SERVICE with CACHE-VAR."
  (im-arr--cached-fetch cache-var
                        (lambda ()
                          (im-arr--get service "rootfolder"))))

(defun im-arr--select-quality-profile (service profiles)
  "Interactively select one of PROFILES for SERVICE.
Return selected profile ID."
  (let* ((choices (mapcar (lambda (profile)
                            (cons (alist-get 'name profile)
                                  (alist-get 'id profile)))
                          profiles))
         (selected (completing-read
                    (format "%s Quality Profile: " (im-arr--service-display-name service))
                    (mapcar #'car choices)
                    nil t)))
    (cdr (assoc selected choices))))

(defun im-arr--format-root-folder-choice (folder)
  "Build display/value pair for FOLDER."
  (let ((path (alist-get 'path folder))
        (free (alist-get 'freeSpace folder)))
    (cons (if (numberp free)
              (format "%s (%.1f GB free)" path (/ free 1073741824.0))
            path)
          path)))

(defun im-arr--select-root-folder (service folders)
  "Interactively select root folder from FOLDERS for SERVICE.
Return selected root folder path."
  (let* ((choices (mapcar #'im-arr--format-root-folder-choice folders))
         (selected (completing-read
                    (format "%s Root Folder: " (im-arr--service-display-name service))
                    (mapcar #'car choices)
                    nil t)))
    (cdr (assoc selected choices))))

(defun im-arr--as-vector (value)
  "Return VALUE as a vector (empty vector for nil)."
  (vconcat (or value '())))

(defun im-arr--first-result-or-error (results thing query)
  "Return first item of RESULTS or signal user error for THING QUERY."
  (if (and (listp results) results)
      (car results)
    (user-error "%s not found for: %s" thing query)))

(defun im-arr--select-result (prompt results formatter)
  "Select one entry from RESULTS.
PROMPT is passed to `completing-read' and FORMATTER formats each row."
  (let* ((choices (cl-loop for result in results
                           for idx from 1
                           collect (cons (format "%d. %s"
                                                 idx
                                                 (funcall formatter result))
                                         result)))
         (selected (completing-read prompt (mapcar #'car choices) nil t)))
    (cdr (assoc selected choices))))

(defun im-arr--ensure-added-id (service item-type result)
  "Ensure RESULT has an ID after adding ITEM-TYPE in SERVICE."
  (unless (alist-get 'id result)
    (if-let ((error-msg (alist-get 'message result)))
        (user-error "%s failed to add %s: %s"
                    (im-arr--service-display-name service)
                    item-type
                    error-msg)
      (user-error "%s failed to add %s: %S"
                  (im-arr--service-display-name service)
                  item-type
                  result))))

(defun im-arr--display-library (buffer-name header lines)
  "Display BUFFER-NAME with HEADER and LINE entries."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert header "\n")
    (insert (make-string 60 ?─) "\n\n")
    (dolist (line lines)
      (insert line "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun im-arr--clear-cache (service vars)
  "Reset cache VARS and show message for SERVICE."
  (dolist (var vars)
    (set var nil))
  (message "%s cache cleared." (im-arr--service-display-name service)))

(declare-function org-entry-get "org")

(defun im-arr--extract-org-id (properties regexp)
  "Extract first REGEXP capture from org PROPERTY list."
  (when (derived-mode-p 'org-mode)
    (cl-loop for property in properties
             for value = (org-entry-get nil property)
             when (and value (string-match regexp value))
             return (match-string 1 value))))

;;;; Radarr wrappers

(defun im-radarr--ensure-config ()
  "Ensure Radarr URL and API key are configured."
  (im-arr--ensure-config 'radarr))

(defun im-radarr--api-url (endpoint)
  "Construct full Radarr API URL for ENDPOINT."
  (im-arr--api-url 'radarr endpoint))

(defun im-radarr--api-headers ()
  "Return headers for Radarr API requests."
  (im-arr--api-headers 'radarr))

(cl-defun im-radarr--get (endpoint &rest params)
  "Make a GET request to Radarr ENDPOINT."
  (apply #'im-arr--get 'radarr endpoint params))

(defun im-radarr--post (endpoint data)
  "Make a POST request to Radarr ENDPOINT with DATA."
  (im-arr--post 'radarr endpoint data))

;;;; Quality Profiles

(defun im-radarr--fetch-quality-profiles ()
  "Fetch quality profiles from Radarr."
  (im-arr--fetch-quality-profiles 'radarr 'im-radarr--quality-profiles-cache))

(defun im-radarr--select-quality-profile ()
  "Interactively select a quality profile.
Returns the profile ID."
  (im-arr--select-quality-profile 'radarr (im-radarr--fetch-quality-profiles)))

;;;; Root Folders

(defun im-radarr--fetch-root-folders ()
  "Fetch root folders from Radarr."
  (im-arr--fetch-root-folders 'radarr 'im-radarr--root-folders-cache))

(defun im-radarr--select-root-folder ()
  "Interactively select a root folder.
Returns the folder path."
  (im-arr--select-root-folder 'radarr (im-radarr--fetch-root-folders)))

;;;; Movie Lookup

(defun im-radarr--normalize-imdb-id (imdb-id)
  "Normalize IMDB-ID and return canonical tt-prefixed ID."
  (let ((id (string-trim imdb-id)))
    (cond
     ((string-match "\\(tt[0-9]+\\)" id)
      (match-string 1 id))
     ((string-match-p "^[0-9]+$" id)
      (concat "tt" id))
     (t
      (user-error "Invalid IMDB ID: %s" imdb-id)))))

(defun im-radarr-lookup-movie (imdb-id)
  "Look up a movie by IMDB-ID.
Returns movie info from TMDB via Radarr."
  (let* ((normalized-id (im-radarr--normalize-imdb-id imdb-id))
         (results (im-radarr--get "movie/lookup" :term (concat "imdb:" normalized-id))))
    (im-arr--first-result-or-error results "Movie" normalized-id)))

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
    (images . ,(im-arr--as-vector (alist-get 'images movie-info)))
    (year . ,(alist-get 'year movie-info))
    (rootFolderPath . ,root-folder-path)
    (monitored . ,im-radarr-default-monitored)
    (minimumAvailability . ,im-radarr-minimum-availability)
    (addOptions . ((searchForMovie . ,im-radarr-search-on-add)))))

(defun im-radarr--add-movie-from-info (movie-info)
  "Add MOVIE-INFO to Radarr with interactive quality/root selection."
  (let ((title (alist-get 'title movie-info))
        (year (alist-get 'year movie-info)))
    (when (y-or-n-p (format "Add \"%s (%s)\" to Radarr? " title year))
      (let* ((quality-profile-id (im-radarr--select-quality-profile))
             (root-folder-path (im-radarr--select-root-folder))
             (payload (im-radarr--build-movie-payload
                       movie-info quality-profile-id root-folder-path))
             (result (im-radarr--post "movie" payload)))
        (im-arr--ensure-added-id 'radarr "movie" result)
        (message "Successfully added \"%s\" to Radarr!" title)))))

;;;###autoload
(defun im-radarr-add-movie (imdb-id)
  "Add a movie to Radarr by IMDB-ID.
Interactively prompts for quality profile and root folder."
  (interactive (list
                (or (im-arr--extract-org-id '("IMDB" "IMDB-ID")
                                            "\\(tt[0-9]+\\)")
                    (read-string "IMDB ID (e.g., tt1234567): "))))
  (setq imdb-id (im-radarr--normalize-imdb-id imdb-id))
  (message "Looking up movie...")
  (im-radarr--add-movie-from-info (im-radarr-lookup-movie imdb-id)))

;;;###autoload
(defun im-radarr-search-and-add-movie ()
  "Search for a movie by title and add it to Radarr."
  (interactive)
  (im-radarr--ensure-config)
  (let* ((query (read-string "Search movie: "))
         (results (im-radarr--get "movie/lookup" :term query)))
    (if (and (listp results) results)
        (let ((movie-info
               (im-arr--select-result
                "Select movie: "
                results
                (lambda (movie)
                  (format "%s (%s) [%s]"
                          (alist-get 'title movie)
                          (or (alist-get 'year movie) "?")
                          (or (alist-get 'imdbId movie) "no imdb"))))))
          (im-radarr--add-movie-from-info movie-info))
      (user-error "No movies found for query: %s" query))))

;;;; Other interactive functions

;;;###autoload
(defun im-radarr-clear-cache ()
  "Clear cached quality profiles and root folders."
  (interactive)
  (im-arr--clear-cache 'radarr
                       '(im-radarr--quality-profiles-cache
                         im-radarr--root-folders-cache)))

;;;###autoload
(defun im-radarr-list-movies ()
  "List all movies in Radarr library."
  (interactive)
  (im-radarr--ensure-config)
  (let* ((movies (im-radarr--get "movie"))
         (lines (mapcar (lambda (movie)
                          (format "%-50s %4s  %s"
                                  (truncate-string-to-width
                                   (or (alist-get 'title movie) "<untitled>")
                                   50 nil nil "…")
                                  (or (alist-get 'year movie) "?")
                                  (if (alist-get 'hasFile movie) "✓" "✗")))
                        (seq-sort-by (lambda (movie)
                                       (or (alist-get 'title movie) ""))
                                     #'string<
                                     movies))))
    (im-arr--display-library
     "*Radarr Movies*"
     (format "Radarr Library (%d movies)" (length movies))
     lines)))

;;;; Sonarr wrappers

(defun im-sonarr--ensure-config ()
  "Ensure Sonarr URL and API key are configured."
  (im-arr--ensure-config 'sonarr))

(defun im-sonarr--api-url (endpoint)
  "Construct full Sonarr API URL for ENDPOINT."
  (im-arr--api-url 'sonarr endpoint))

(defun im-sonarr--api-headers ()
  "Return headers for Sonarr API requests."
  (im-arr--api-headers 'sonarr))

(cl-defun im-sonarr--get (endpoint &rest params)
  "Make a GET request to Sonarr ENDPOINT."
  (apply #'im-arr--get 'sonarr endpoint params))

(defun im-sonarr--post (endpoint data)
  "Make a POST request to Sonarr ENDPOINT with DATA."
  (im-arr--post 'sonarr endpoint data))

(defun im-sonarr--fetch-quality-profiles ()
  "Fetch quality profiles from Sonarr."
  (im-arr--fetch-quality-profiles 'sonarr 'im-sonarr--quality-profiles-cache))

(defun im-sonarr--select-quality-profile ()
  "Interactively select a Sonarr quality profile.
Returns the profile ID."
  (im-arr--select-quality-profile 'sonarr (im-sonarr--fetch-quality-profiles)))

(defun im-sonarr--fetch-root-folders ()
  "Fetch root folders from Sonarr."
  (im-arr--fetch-root-folders 'sonarr 'im-sonarr--root-folders-cache))

(defun im-sonarr--select-root-folder ()
  "Interactively select a Sonarr root folder.
Returns the folder path."
  (im-arr--select-root-folder 'sonarr (im-sonarr--fetch-root-folders)))

(defun im-sonarr--normalize-tvdb-id (tvdb-id)
  "Extract numeric TVDB ID from TVDB-ID."
  (let ((raw (string-trim tvdb-id)))
    (if (string-match "\\([0-9]+\\)" raw)
        (match-string 1 raw)
      (user-error "Invalid TVDB ID: %s" tvdb-id))))

(defun im-sonarr-lookup-series (term)
  "Look up a series by TERM via Sonarr."
  (let ((results (im-sonarr--get "series/lookup" :term term)))
    (im-arr--first-result-or-error results "Series" term)))

(defun im-sonarr--series-year (series-info)
  "Return display year extracted from SERIES-INFO."
  (or (alist-get 'year series-info)
      (when-let* ((first-aired (alist-get 'firstAired series-info))
                  (_ (>= (length first-aired) 4)))
        (substring first-aired 0 4))
      "?"))

(defun im-sonarr--build-series-payload (series-info quality-profile-id root-folder-path)
  "Build the payload for adding SERIES-INFO."
  `((title . ,(alist-get 'title series-info))
    (qualityProfileId . ,quality-profile-id)
    (tvdbId . ,(alist-get 'tvdbId series-info))
    (titleSlug . ,(alist-get 'titleSlug series-info))
    (images . ,(im-arr--as-vector (alist-get 'images series-info)))
    (seasons . ,(im-arr--as-vector (alist-get 'seasons series-info)))
    (rootFolderPath . ,root-folder-path)
    (seasonFolder . ,im-sonarr-season-folder)
    (monitored . ,im-sonarr-default-monitored)
    (addOptions . ((searchForMissingEpisodes . ,im-sonarr-search-on-add)))))

(defun im-sonarr--add-series-from-info (series-info)
  "Add SERIES-INFO to Sonarr with interactive quality/root selection."
  (let ((title (alist-get 'title series-info))
        (year (im-sonarr--series-year series-info)))
    (when (y-or-n-p (format "Add \"%s (%s)\" to Sonarr? " title year))
      (let* ((quality-profile-id (im-sonarr--select-quality-profile))
             (root-folder-path (im-sonarr--select-root-folder))
             (payload (im-sonarr--build-series-payload
                       series-info quality-profile-id root-folder-path))
             (result (im-sonarr--post "series" payload)))
        (im-arr--ensure-added-id 'sonarr "series" result)
        (message "Successfully added \"%s\" to Sonarr!" title)))))

;;;###autoload
(defun im-sonarr-add-series (tvdb-id)
  "Add a series to Sonarr by TVDB-ID.
Interactively prompts for quality profile and root folder."
  (interactive
   (list
    (or (im-arr--extract-org-id '("TVDB" "TVDB-ID") "\\([0-9]+\\)")
        (read-string "TVDB ID (e.g., 121361): "))))
  (let ((normalized-tvdb-id (im-sonarr--normalize-tvdb-id tvdb-id)))
    (message "Looking up series...")
    (im-sonarr--add-series-from-info
     (im-sonarr-lookup-series (format "tvdb:%s" normalized-tvdb-id)))))

;;;###autoload
(defun im-sonarr-search-and-add-series ()
  "Search for a series by title and add it to Sonarr."
  (interactive)
  (im-sonarr--ensure-config)
  (let* ((query (read-string "Search series: "))
         (results (im-sonarr--get "series/lookup" :term query)))
    (if (and (listp results) results)
        (let ((series-info
               (im-arr--select-result
                "Select series: "
                results
                (lambda (series)
                  (format "%s (%s) [tvdb:%s]"
                          (alist-get 'title series)
                          (im-sonarr--series-year series)
                          (or (alist-get 'tvdbId series) "?"))))))
          (im-sonarr--add-series-from-info series-info))
      (user-error "No series found for query: %s" query))))

;;;###autoload
(defun im-sonarr-clear-cache ()
  "Clear cached Sonarr quality profiles and root folders."
  (interactive)
  (im-arr--clear-cache 'sonarr
                       '(im-sonarr--quality-profiles-cache
                         im-sonarr--root-folders-cache)))

;;;###autoload
(defun im-sonarr-list-series ()
  "List all series in Sonarr library."
  (interactive)
  (im-sonarr--ensure-config)
  (let* ((series-list (im-sonarr--get "series"))
         (lines (mapcar
                 (lambda (series)
                   (let* ((stats (alist-get 'statistics series))
                          (percent (alist-get 'percentOfEpisodes stats))
                          (percent-str (if (numberp percent)
                                           (format "%.1f%%" percent)
                                         "n/a")))
                     (format "%-50s %8s  %s"
                             (truncate-string-to-width
                              (or (alist-get 'title series) "<untitled>")
                              50 nil nil "…")
                             percent-str
                             (if (alist-get 'monitored series) "✓" "✗"))))
                 (seq-sort-by (lambda (series)
                                (or (alist-get 'title series) ""))
                              #'string<
                              series-list))))
    (im-arr--display-library
     "*Sonarr Series*"
     (format "Sonarr Library (%d series)" (length series-list))
     lines)))

(provide 'im-arr)
;;; im-arr.el ends here
