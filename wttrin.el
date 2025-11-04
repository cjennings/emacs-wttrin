;;; wttrin.el --- Emacs Frontend for Service wttr.in -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Copyright (C) 2024 Craig Jennings
;; Maintainer: Craig Jennings <c@cjennings.net>
;;
;; Original Authors: Carl X. Su <bcbcarl@gmail.com>
;;                   ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Version: 0.2.3
;; Package-Requires: ((emacs "24.4") (xterm-color "1.0"))
;; Keywords: weather, wttrin, games
;; URL: https://github.com/cjennings/emacs-wttrin

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Displays the weather information from the wttr.in service for your submitted
;; location.

;;; Code:

(require 'face-remap)
(require 'url)
(require 'xterm-color) ;; https://github.com/atomontage/xterm-color

(defgroup wttrin nil
  "Emacs frontend for the weather web service wttr.in."
  :prefix "wttrin-"
  :group 'comm)

(defcustom wttrin-font-name "Liberation Mono"
  "Preferred monospaced font name for weather display."
  :group 'wttrin
  :type 'string)

(defcustom wttrin-font-height 130
  "Preferred font height for weather display."
  :group 'wttrin
  :type 'integer)

(defface wttrin-buffer-face
  `((t :height ,wttrin-font-height :family ,wttrin-font-name))
  "Default face for the weather display buffer."
  :group 'wttrin)

(defcustom wttrin-default-locations '("Honolulu, HI"
									  "Berkeley, CA"
									  "New Orleans, LA"
									  "New York, NY"
                                      "London, GB"
                                      "Paris, FR"
                                      "Berlin, DE"
                                      "Naples, IT"
                                      "Athens, GR"
									  "Kyiv, UA"
									  "Tokyo, JP"
                                      "Taipei, TW")
  "Specify default locations list for quick completion."
  :group 'wttrin
  :type '(repeat string))

(defcustom wttrin-default-languages
  '("Accept-Language" . "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4")
  "Specify default HTTP request Header for Accept-Language."
  :group 'wttrin
  :type '(cons (string :tag "Header") (string :tag "Language codes")))

(defcustom wttrin-unit-system nil
  "Specify units of measurement.
Use \='m\=' for \='metric\=', \='u\=' for \='USCS\=', or nil for location based
units (default)."
  :group 'wttrin
  :type 'string)


(defcustom wttrin-cache-ttl 900  ; 15 minutes
  "Time to live for cached weather data in seconds."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-cache-max-entries 50
  "Maximum number of entries to keep in cache."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-use-async t
  "If non-nil, fetch weather data asynchronously to avoid blocking Emacs."
  :group 'wttrin
  :type 'boolean)

(defvar wttrin--cache (make-hash-table :test 'equal)
  "Cache for weather data: cache-key -> (timestamp . data).")

(defvar wttrin--force-refresh nil
  "When non-nil, bypass cache on next fetch.")

(defun wttrin-additional-url-params ()
  "Concatenates extra information into the URL."
  (if wttrin-unit-system
      (concat "?" wttrin-unit-system)
    "?"))

(defun wttrin--build-url (query)
  "Build wttr.in URL for QUERY with configured parameters.
This is a pure function with no side effects, suitable for testing."
  (when (null query)
    (error "Query cannot be nil"))
  (concat "https://wttr.in/"
          (url-hexify-string query)
          (wttrin-additional-url-params)
          "A"))

(defun wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY.
Returns the weather data as a string, or signals an error on failure."
  (let* ((url (wttrin--build-url query))
         (url-request-extra-headers (list wttrin-default-languages))
         (url-user-agent "curl")
         (buf (url-retrieve-synchronously url t t)))
    (unless buf
      (error "wttrin: Network failure - could not retrieve %S" query))
    (unwind-protect
        (with-current-buffer buf
          ;; Skip HTTP headers
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil t)
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (kill-buffer buf))))

(defun wttrin-fetch-raw-string-async (query callback)
  "Asynchronously fetch weather information for QUERY.
CALLBACK is called with the weather data string when ready, or nil on error."
  (let* ((url (wttrin--build-url query))
         (url-request-extra-headers (list wttrin-default-languages))
         (url-user-agent "curl"))
    (url-retrieve
     url
     (lambda (status)
       (let ((data nil))
         (condition-case err
             (if (plist-get status :error)
                 (progn
                   (message "wttrin: Network error - %s" (cdr (plist-get status :error)))
                   (setq data nil))
               (unwind-protect
                   (progn
                     ;; Skip HTTP headers
                     (goto-char (point-min))
                     (re-search-forward "\r?\n\r?\n" nil t)
                     (setq data (decode-coding-string
                                 (buffer-substring-no-properties (point) (point-max))
                                 'utf-8)))
                 (kill-buffer (current-buffer))))
           (error
            (message "wttrin: Error processing response - %s" (error-message-string err))
            (setq data nil)))
         (funcall callback data))))))

(defun wttrin-exit ()
  "Exit the wttrin buffer."
  (interactive)
  (quit-window t))

(defun wttrin-requery ()
  "Kill buffer and requery wttrin."
  (interactive)
  (let ((new-location (completing-read
                       "Location Name: " wttrin-default-locations nil nil
                       (when (= (length wttrin-default-locations) 1)
                         (car wttrin-default-locations)))))
    (when (get-buffer "*wttr.in*")
      (kill-buffer "*wttr.in*"))
    (if wttrin-use-async
        (wttrin-query-async new-location)
      (wttrin-query new-location))))

(defun wttrin--display-weather (location-name raw-string)
  "Display weather data RAW-STRING for LOCATION-NAME in weather buffer."
  (if (or (null raw-string) (string-match "ERROR" raw-string))
      (message "Cannot retrieve weather data. Perhaps the location was misspelled?")
    (let ((buffer (get-buffer-create (format "*wttr.in*")))
          date-time-stamp location-info)
      (switch-to-buffer buffer)
      (setq-local wttrin--current-location location-name)
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; set the preferred font attributes for this buffer only
      (setq buffer-face-mode-face `(:family ,wttrin-font-name :height
                                             ,wttrin-font-height))

      ;; display buffer text and insert wttr.in data
      (buffer-face-mode t)
      (insert (xterm-color-filter raw-string))

      ;; rearrange header information
      (goto-char (point-min))
      (forward-line 4)
      (setq date-time-stamp (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))
      (goto-char (point-min))
      (forward-line 6)
      (setq location-info (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
      (goto-char (point-min))
      (forward-line 8)
      (delete-region (point-min) (line-beginning-position))

      (insert "\n" location-info "\n" date-time-stamp "\n\n\n")

      ;; provide user instructions
      (goto-char (point-max))
      (insert "\nPress: [g] to query another location [r] to refresh [q] to quit")

      ;; align buffer to top
      (goto-char (point-min))

      ;; create choice keymap and disallow modifying buffer
      (use-local-map (make-sparse-keymap))
      (local-set-key "q" 'wttrin-exit)
      (local-set-key "r" 'wttrin-requery-force)
      (local-set-key "g" 'wttrin-requery)
      (setq buffer-read-only t))))

(defun wttrin-query (location-name)
  "Query weather of LOCATION-NAME via wttrin, display the result in new buffer."
  (let ((raw-string (wttrin--get-cached-or-fetch location-name)))
    (wttrin--display-weather location-name raw-string)))

(defun wttrin-query-async (location-name)
  "Asynchronously query weather of LOCATION-NAME, display result when ready."
  (let ((buffer (get-buffer-create (format "*wttr.in*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Loading weather for " location-name "...")
    (setq buffer-read-only t)
    (wttrin--get-cached-or-fetch-async
     location-name
     (lambda (raw-string)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (wttrin--display-weather location-name raw-string)))))))

(defun wttrin--make-cache-key (location)
  "Create cache key from LOCATION and current settings."
  (concat location "|" (or wttrin-unit-system "default")))

(defun wttrin--get-cached-or-fetch (location)
  "Get cached weather for LOCATION or fetch if expired.
Returns the weather data string or nil on error."
  (let* ((cache-key (wttrin--make-cache-key location))
		 (cached (gethash cache-key wttrin--cache))
		 (timestamp (car cached))
		 (data (cdr cached))
		 (now (float-time)))
	(if (and cached
			 (< (- now timestamp) wttrin-cache-ttl)
			 (not wttrin--force-refresh))
		data
	  (condition-case err
		  (let ((fresh-data (wttrin-fetch-raw-string location)))
			(when fresh-data
			  (wttrin--cleanup-cache-if-needed)
			  (puthash cache-key (cons now fresh-data) wttrin--cache))
			fresh-data)
		(error
		 ;; On error, return stale cache if available
		 (if cached
			 (progn
			   (message "Failed to fetch new data, using cached version")
			   data)
		   (signal (car err) (cdr err))))))))

(defun wttrin--get-cached-or-fetch-async (location callback)
  "Asynchronously get cached weather for LOCATION or fetch if expired.
CALLBACK is called with the weather data string when ready, or nil on error."
  (let* ((cache-key (wttrin--make-cache-key location))
         (cached (gethash cache-key wttrin--cache))
         (timestamp (car cached))
         (data (cdr cached))
         (now (float-time)))
    (if (and cached
             (< (- now timestamp) wttrin-cache-ttl)
             (not wttrin--force-refresh))
        ;; Return cached data immediately
        (funcall callback data)
      ;; Fetch fresh data asynchronously
      (wttrin-fetch-raw-string-async
       location
       (lambda (fresh-data)
         (if fresh-data
             (progn
               (wttrin--cleanup-cache-if-needed)
               (puthash cache-key (cons now fresh-data) wttrin--cache)
               (funcall callback fresh-data))
           ;; On error, return stale cache if available
           (if cached
               (progn
                 (message "Failed to fetch new data, using cached version")
                 (funcall callback data))
             (funcall callback nil))))))))

(defun wttrin--cleanup-cache-if-needed ()
  "Remove old entries if cache exceeds max size."
  (when (> (hash-table-count wttrin--cache) wttrin-cache-max-entries)
	(let ((entries nil))
	  (maphash (lambda (k v)
				 (push (cons k (car v)) entries))
			   wttrin--cache)
	  (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
	  ;; Remove oldest 20% of entries
	  (dotimes (_ (/ (length entries) 5))
		(when entries
		  (remhash (caar entries) wttrin--cache)
		  (setq entries (cdr entries)))))))

(defun wttrin-clear-cache ()
  "Clear the weather cache."
  (interactive)
  (clrhash wttrin--cache)
  (message "Weather cache cleared"))

(defvar-local wttrin--current-location nil
  "Current location displayed in this weather buffer.")

(defun wttrin-requery-force ()
  "Force refresh weather data for current location, bypassing cache."
  (interactive)
  (if wttrin--current-location
	  (let ((wttrin--force-refresh t))
		(message "Refreshing weather data...")
		(if wttrin-use-async
			(wttrin-query-async wttrin--current-location)
		  (wttrin-query wttrin--current-location)
		  (message nil)))
	(message "No location to refresh")))

;;;###autoload
(defun wttrin (location)
  "Display weather information for LOCATION.
Uses asynchronous fetching if `wttrin-use-async' is non-nil."
  (interactive
   (list
    (completing-read "Location Name: " wttrin-default-locations nil nil
                     (when (= (length wttrin-default-locations) 1)
                       (car wttrin-default-locations)))))
  (if wttrin-use-async
      (wttrin-query-async location)
    (wttrin-query location)))

(provide 'wttrin)
;;; wttrin.el ends here
