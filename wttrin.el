;;; wttrin.el --- Emacs Frontend for Service wttr.in -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Copyright (C) 2024-2026 Craig Jennings
;; Maintainer: Craig Jennings <c@cjennings.net>
;;
;; Original Authors: Carl X. Su <bcbcarl@gmail.com>
;;                   ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Version: 0.3.2
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

;; Declare xterm-color functions (loaded on-demand)
(declare-function xterm-color-filter "xterm-color" (string))

;; Declare geolocation entry point (loaded on-demand by
;; `wttrin-set-location-from-geolocation')
(declare-function wttrin-geolocation-detect "wttrin-geolocation" (callback))

;; No-op stubs for debug functions (overridden when wttrin-debug.el is loaded)
(defun wttrin--debug-mode-line-info ()
  "No-op stub.  Replaced by `wttrin-debug' when debug mode is active."
  nil)

(defun wttrin--debug-log (_format-string &rest _args)
  "No-op stub.  Replaced by `wttrin-debug' when debug mode is active."
  nil)

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


(define-obsolete-variable-alias 'wttrin-cache-ttl 'wttrin-refresh-interval "0.3.0")

(defcustom wttrin-refresh-interval 3600  ; 1 hour
  "Interval in seconds between proactive weather data refreshes.
Controls how often the background timer refreshes cached weather data
for `wttrin-favorite-location'.  Data older than 2x this interval
is considered stale.  The wttr.in service updates roughly every 10
minutes, so values below 600 just waste their bandwidth."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-cache-max-entries 50
  "Maximum number of entries to keep in cache."
  :group 'wttrin
  :type 'integer)

(defconst wttrin--cache-cleanup-percentage 0.20
  "Percentage of cache entries to remove when max size is exceeded.
When cache reaches `wttrin-cache-max-entries', remove the oldest 20%
to avoid frequent cleanup cycles.  This value (0.20) means remove 1/5
of entries, providing a reasonable buffer before the next cleanup.")

(defcustom wttrin-favorite-location nil
  "Favorite location to display weather for.
When nil, favorite location features are disabled.
Set to a location string (e.g., \"New Orleans, LA\") to enable mode-line
weather display and other location-based features.
The weather icon and tooltip will update automatically in the background."
  :group 'wttrin
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Location")))

(defcustom wttrin-mode-line-refresh-interval 3600
  "Interval in seconds to refresh mode-line weather data.
Default is 3600 seconds (1 hour).  The wttr.in service updates its
data roughly every 10 minutes; polling more often than that just
wastes their bandwidth.  Be kind to the free service."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-mode-line-startup-delay 3
  "Seconds to delay initial mode-line weather fetch after Emacs starts.
This allows network stack and daemon initialization to complete before
fetching weather data.  Must be between 1 and 10 seconds."
  :group 'wttrin
  :type '(restricted-sexp :match-alternatives
                          ((lambda (val)
                             (and (integerp val)
                                  (>= val 1)
                                  (<= val 10))))))

(defcustom wttrin-mode-line-emoji-font "Noto Color Emoji"
  "Font family to use for mode-line weather emoji.
Common color emoji fonts include:
- \"Noto Color Emoji\" (Linux)
- \"Apple Color Emoji\" (macOS)
- \"Segoe UI Emoji\" (Windows)
- \"Twitter Color Emoji\"
Set to nil to use default font (may render as monochrome)."
  :group 'wttrin
  :type '(choice (const :tag "Use default font" nil)
                 (string :tag "Font family name")))

(defcustom wttrin-mode-line-auto-enable nil
  "If non-nil, automatically enable mode-line weather display when loading wttrin.
When enabled, weather for `wttrin-favorite-location' will appear
in the mode-line automatically.  You can also manually toggle the mode-line
display with `wttrin-mode-line-mode'."
  :group 'wttrin
  :type 'boolean)

(defcustom wttrin-debug nil
  "Enable debug functions for troubleshooting wttrin behavior.
When non-nil, loads wttrin-debug.el which provides:
- Automatic mode-line diagnostic logging when wttrin runs
- Raw weather data saved to timestamped files in variable
  `temporary-file-directory'
- Interactive debug commands for troubleshooting

Set this to t BEFORE loading wttrin, typically in your init file:
  (setq wttrin-debug t)
  (require \\='wttrin)"
  :group 'wttrin
  :type 'boolean)

;; When debug mode is active, load the real implementations of
;; wttrin--debug-log and wttrin--debug-mode-line-info, replacing the
;; no-op stubs defined above.  Must be set before loading wttrin.
(when wttrin-debug
  (require 'wttrin-debug
           (expand-file-name "wttrin-debug.el"
                             (file-name-directory (or load-file-name buffer-file-name)))
           t))

(defvar wttrin--cache (make-hash-table :test 'equal)
  "Cache for weather data: cache-key -> (timestamp . data).")

(defvar wttrin--force-refresh nil
  "When non-nil, bypass cache on next fetch.")

(defvar wttrin-mode-line-string nil
  "Mode-line string showing weather for favorite location.")
;; Emacs strips text properties from mode-line strings unless the
;; variable is marked risky.  Without this, face and help-echo are lost.
(put 'wttrin-mode-line-string 'risky-local-variable t)

(defvar wttrin--mode-line-timer nil
  "Timer object for mode-line weather refresh.")

(defvar wttrin--mode-line-cache nil
  "Cached mode-line weather data as (timestamp . data) cons cell.
When non-nil, car is the `float-time' when data was fetched,
and cdr is the weather string from the API.")

(defvar wttrin--mode-line-rendered-stale nil
  "Whether the mode-line emoji is currently rendered as stale (dimmed).")

(defvar wttrin--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'wttrin-mode-line-click)
    (define-key map [mode-line mouse-3] 'wttrin-mode-line-force-refresh)
    map)
  "Keymap for mode-line weather widget interactions.
Left-click: refresh weather and open buffer.
Right-click: force-refresh cache and update tooltip.")

(defun wttrin--format-age (seconds)
  "Format SECONDS as a human-readable age string.
Returns \"just now\" for <60s, \"X minutes ago\", \"X hours ago\", or \"X days ago\"."
  (cond
   ((< seconds 60) "just now")
   ((< seconds 3600)
    (let ((minutes (floor (/ seconds 60))))
      (format "%d %s ago" minutes (if (= minutes 1) "minute" "minutes"))))
   ((< seconds 86400)
    (let ((hours (floor (/ seconds 3600))))
      (format "%d %s ago" hours (if (= hours 1) "hour" "hours"))))
   (t
    (let ((days (floor (/ seconds 86400))))
      (format "%d %s ago" days (if (= days 1) "day" "days"))))))

(defun wttrin-additional-url-params ()
  "Concatenates extra information into the URL."
  (if wttrin-unit-system
      (concat "?" wttrin-unit-system)
    "?"))

(defun wttrin--build-url (query)
  "Build wttr.in URL for QUERY with configured parameters."
  (when (null query)
    (error "Query cannot be nil"))
  (concat "https://wttr.in/"
          (url-hexify-string query)
          (wttrin-additional-url-params)
          "A"))

(defun wttrin--extract-http-status ()
  "Return the HTTP status code from the current buffer, or nil.
Reads the status line without moving point."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun wttrin--extract-response-body ()
  "Extract and decode HTTP response body from current buffer.
Skips headers and returns UTF-8 decoded body.
Returns nil for non-2xx status codes or on error.  Kills buffer when done."
  (condition-case err
      (unwind-protect
          (let ((status (wttrin--extract-http-status)))
            (if (and status (>= status 300))
                (progn
                  (wttrin--debug-log "wttrin--extract-response-body: HTTP %d" status)
                  nil)
              (goto-char (point-min))
              ;; Skip past HTTP headers — blank line separates headers from body
              (re-search-forward "\r?\n\r?\n" nil t)
              (let ((body (decode-coding-string
                           (buffer-substring-no-properties (point) (point-max))
                           'utf-8)))
                (wttrin--debug-log "wttrin--extract-response-body: Successfully fetched %d bytes"
                                   (length body))
                body)))
        ;; unwind-protect handles buffer cleanup for all paths
        (ignore-errors (kill-buffer (current-buffer))))
    (error
     (wttrin--debug-log "wttrin--extract-response-body: Error - %s"
                        (error-message-string err))
     nil)))

(defun wttrin--handle-fetch-callback (status callback)
  "Handle `url-retrieve' callback STATUS and invoke CALLBACK with result.
Calls CALLBACK with (DATA &optional ERROR-MSG).  DATA is the response
body string on success, nil on failure.  ERROR-MSG is a human-readable
description of what went wrong, or nil on success."
  (wttrin--debug-log "wttrin--handle-fetch-callback: Invoked with status = %S" status)
  (let ((data nil)
        (error-msg nil))
    (cond
     ;; Network-level failure (DNS, connection refused, timeout)
     ((plist-get status :error)
      (wttrin--debug-log "wttrin--handle-fetch-callback: Network error - %s"
                         (cdr (plist-get status :error)))
      (setq error-msg "Network error — check your connection")
      (message "wttrin: %s" error-msg))
     ;; HTTP response received — extract body (returns nil for non-2xx)
     (t
      (let ((http-status (wttrin--extract-http-status)))
        (setq data (wttrin--extract-response-body))
        (when (and (not data) http-status)
          (setq error-msg
                (cond
                 ((and (>= http-status 400) (< http-status 500))
                  (format "Location not found (HTTP %d)" http-status))
                 ((>= http-status 500)
                  (format "Weather service error (HTTP %d)" http-status))
                 (t (format "Unexpected HTTP status %d" http-status))))
          (when error-msg
            (message "wttrin: %s" error-msg))))))
    (condition-case err
        (progn
          (wttrin--debug-log "wttrin--handle-fetch-callback: Calling user callback with %s"
                             (if data (format "%d bytes" (length data)) "nil"))
          (funcall callback data error-msg))
      (error
       (wttrin--debug-log "wttrin--handle-fetch-callback: Error in user callback - %s"
                          (error-message-string err))
       (message "wttrin: Error in callback - %s" (error-message-string err))))))

(defun wttrin--fetch-url (url callback)
  "Asynchronously fetch URL and call CALLBACK with decoded response.
CALLBACK is called with the weather data string when ready, or nil on error.
Handles header skipping, UTF-8 decoding, and error handling automatically."
  (wttrin--debug-log "wttrin--fetch-url: Starting fetch for URL: %s" url)
  ;; wttr.in returns plain text for curl but HTML for browsers
  (let ((url-request-extra-headers (list wttrin-default-languages))
        (url-user-agent "curl"))
    (url-retrieve url
                  (lambda (status)
                    (wttrin--handle-fetch-callback status callback)))))

(defun wttrin-fetch-raw-string (query callback)
  "Asynchronously fetch weather information for QUERY.
CALLBACK is called with the weather data string when ready, or nil on error."
  (wttrin--fetch-url (wttrin--build-url query) callback))

(defun wttrin--requery-location (new-location)
  "Kill current weather buffer and query NEW-LOCATION."
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*"))
  (wttrin-query new-location))

(defun wttrin-requery ()
  "Kill buffer and requery wttrin."
  (interactive)
  (let ((new-location (completing-read
                       "Location Name: " wttrin-default-locations nil nil
                       (when (= (length wttrin-default-locations) 1)
                         (car wttrin-default-locations)))))
    (wttrin--requery-location new-location)))

(defvar wttrin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'wttrin-requery)
    (define-key map (kbd "g") 'wttrin-requery-force)
    ;; Note: 'q' is bound to quit-window by special-mode
    map)
  "Keymap for wttrin-mode.")

(define-derived-mode wttrin-mode special-mode "Wttrin"
  "Major mode for displaying wttr.in weather information.

Weather data is displayed in a read-only buffer with the following keybindings:

\\{wttrin-mode-map}"
  (buffer-disable-undo)
  ;; ASCII art breaks if lines wrap at the window edge
  (setq truncate-lines t)
  ;; Use face-remap instead of buffer-face-mode to preserve xterm-color faces
  (face-remap-add-relative 'default
                           :family wttrin-font-name
                           :height wttrin-font-height))

(defun wttrin--save-debug-data (location-name raw-string)
  "Save RAW-STRING to a timestamped debug file for LOCATION-NAME.
Returns the path to the saved file."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "wttrin-debug-%s.txt" timestamp))
         (filepath (expand-file-name filename temporary-file-directory)))
    (with-temp-file filepath
      (insert (format "Location: %s\n" location-name))
      (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "wttrin-unit-system: %s\n" wttrin-unit-system))
      (insert "\n--- Raw Response ---\n\n")
      (insert (or raw-string "(nil — no data received)")))
    (wttrin--debug-log "Debug data saved to: %s" filepath)
    filepath))

(defun wttrin--validate-weather-data (raw-string)
  "Check if RAW-STRING has valid weather data.
Return t if valid, nil if missing or contains errors."
  (not (or (null raw-string) (string-match-p "ERROR" raw-string))))

(defun wttrin--process-weather-content (raw-string)
  "Process RAW-STRING: apply ANSI filtering and remove verbose lines.
Returns processed string ready for display."
  (require 'xterm-color)
  (let ((processed (xterm-color-filter raw-string)))
    ;; Remove verbose Location: coordinate line
    (with-temp-buffer
      (insert processed)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*Location:.*\\[.*\\].*$" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))
      (buffer-string))))

(defun wttrin--add-buffer-instructions ()
  "Add user instructions at bottom of current buffer."
  (goto-char (point-max))
  (insert "\n\nPress: [a] for another location [g] to refresh [q] to quit"))

(defun wttrin--format-staleness-header (location)
  "Return a staleness header string for LOCATION, or nil if no cache entry.
Looks up the cache timestamp for LOCATION and formats a line like
\"Last updated: 2:30 PM (5 minutes ago)\"."
  (let* ((cache-key (wttrin--make-cache-key location))
         (cached (gethash cache-key wttrin--cache)))
    (when cached
      (let* ((timestamp (car cached))
             (age (- (float-time) timestamp))
             (time-str (format-time-string "%l:%M %p" (seconds-to-time timestamp)))
             (age-str (wttrin--format-age age)))
        (format "Last updated: %s (%s)" (string-trim time-str) age-str)))))

(defun wttrin--display-weather (location-name raw-string &optional error-msg)
  "Display weather data RAW-STRING for LOCATION-NAME in weather buffer.
When ERROR-MSG is provided and data is invalid, show that instead of
the generic error message."
  (when wttrin-debug
    (wttrin--save-debug-data location-name raw-string))

  (if (not (wttrin--validate-weather-data raw-string))
      (message "wttrin: %s"
               (or error-msg
                   "Cannot retrieve weather data. Perhaps the location was misspelled?"))
    (let ((buffer (get-buffer-create (format "*wttr.in*"))))
      (switch-to-buffer buffer)

      ;; wttrin-mode calls kill-all-local-variables, so it must run
      ;; before setting any buffer-local state (xterm-color, location)
      (wttrin-mode)

      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; xterm-color--state must be set AFTER wttrin-mode for the same
        ;; reason — mode initialization would wipe it
        (require 'xterm-color)
        (setq-local xterm-color--state :char)
        (insert (wttrin--process-weather-content raw-string))
        ;; wttr.in returns location in lowercase — replace with user's casing
        (goto-char (point-min))
        (when (re-search-forward "^Weather report: .*$" nil t)
          (replace-match (concat "Weather report: " location-name)))
        (let ((staleness (wttrin--format-staleness-header location-name)))
          (when staleness
            (insert "\n" staleness)))
        (wttrin--add-buffer-instructions)
        (goto-char (point-min)))

      (setq-local wttrin--current-location location-name)
      (wttrin--debug-mode-line-info))))

(defun wttrin-query (location-name)
  "Asynchronously query weather of LOCATION-NAME, display result when ready."
  (let ((buffer (get-buffer-create (format "*wttr.in*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Loading weather for " location-name "...")
    (setq buffer-read-only t)
    (wttrin--get-cached-or-fetch
     location-name
     (lambda (raw-string &optional error-msg)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (wttrin--display-weather location-name raw-string error-msg)))))))

(defun wttrin--make-cache-key (location)
  "Create cache key from LOCATION and current settings."
  (concat location "|" (or wttrin-unit-system "default")))

(defun wttrin--get-cached-or-fetch (location callback)
  "Get cached weather for LOCATION or fetch if not cached.
If cache has data and not force-refreshing, serves it immediately
regardless of age.  The background refresh timer keeps data fresh.
CALLBACK is called with (DATA &optional ERROR-MSG)."
  (let* ((cache-key (wttrin--make-cache-key location))
         (cached (gethash cache-key wttrin--cache))
         (data (cdr cached)))
    (if (and cached (not wttrin--force-refresh))
        ;; Serve cached data regardless of age — background timers keep it fresh
        (funcall callback data)
      (wttrin-fetch-raw-string
       location
       (lambda (fresh-data &optional error-msg)
         (if fresh-data
             (progn
               (wttrin--cleanup-cache-if-needed)
               (puthash cache-key (cons (float-time) fresh-data) wttrin--cache)
               (funcall callback fresh-data))
           ;; On error, return stale cache if available
           (if cached
               (progn
                 (message "Failed to fetch new data, using cached version")
                 (funcall callback data))
             (funcall callback nil error-msg))))))))

(defun wttrin--get-cache-entries-by-age ()
  "Return list of (key . timestamp) pairs sorted oldest-first.
Extracts all cache entries and sorts them by timestamp in ascending order.
Returns a list where each element is a cons cell (key . timestamp)."
  (let ((entries nil))
    (maphash (lambda (key value)
               (push (cons key (car value)) entries))  ; car value = timestamp
             wttrin--cache)
    (sort entries (lambda (a b) (< (cdr a) (cdr b))))))

(defun wttrin--cleanup-cache-if-needed ()
  "Remove oldest entries if cache exceeds max size.
Removes oldest entries based on `wttrin--cache-cleanup-percentage'
when cache count exceeds `wttrin-cache-max-entries'.
This creates headroom to avoid frequent cleanups."
  (when (> (hash-table-count wttrin--cache) wttrin-cache-max-entries)
    (let* ((entries-by-age (wttrin--get-cache-entries-by-age))
           (num-to-remove (floor (* (length entries-by-age)
                                    wttrin--cache-cleanup-percentage))))
      (dotimes (i num-to-remove)
        (remhash (car (nth i entries-by-age)) wttrin--cache)))))

(defun wttrin-clear-cache ()
  "Clear the weather cache."
  (interactive)
  (clrhash wttrin--cache)
  (message "Weather cache cleared"))

;;;###autoload
(defun wttrin-set-location-from-geolocation ()
  "Detect your location via IP geolocation and set it as the favorite.
Uses the provider named by `wttrin-geolocation-provider' to fetch
\"City, Region\", asks for confirmation, and on yes assigns the
result to `wttrin-favorite-location' for this session.

To persist the setting across Emacs sessions, either run
\\[customize-save-variable] on `wttrin-favorite-location', or add
\(setq wttrin-favorite-location ...\) to your init file.

IP-based geolocation can be wrong behind a VPN or a mobile hotspot.
The confirmation prompt shows the detected location so you can
reject inaccurate results."
  (interactive)
  (require 'wttrin-geolocation)
  (message "Detecting location...")
  (wttrin-geolocation-detect
   (lambda (location)
     (cond
      ((null location)
       (message "Could not detect location (network or provider error)"))
      ((yes-or-no-p (format "Detected location: %s. Set as favorite? "
                            location))
       (setq wttrin-favorite-location location)
       (message "Set wttrin-favorite-location to: %s. Run M-x customize-save-variable to persist."
                location))
      (t
       (message "Location detection cancelled"))))))

(defvar-local wttrin--current-location nil
  "Current location displayed in this weather buffer.")

(defun wttrin-requery-force ()
  "Force refresh weather data for current location, bypassing cache."
  (interactive)
  (if wttrin--current-location
      (let ((wttrin--force-refresh t))
        (message "Refreshing weather data...")
        (wttrin-query wttrin--current-location))
    (message "No location to refresh")))

;;; Mode-line weather display

(defun wttrin--replace-response-location (response location)
  "Replace the API's location prefix in RESPONSE with LOCATION.
The wttr.in API returns locations in lowercase.  This substitutes the
user's original casing so tooltips display what the user expects."
  (if (string-match ":" response)
      (concat location (substring response (match-beginning 0)))
    response))

(defun wttrin--make-emoji-icon (emoji &optional foreground)
  "Create EMOJI string with optional font face and FOREGROUND color.
Uses `wttrin-mode-line-emoji-font' when configured.
Omits `:foreground' from the face plist when FOREGROUND is nil — a literal
`:foreground nil' entry triggers \"Invalid face attribute\" warnings on every
redisplay."
  (if wttrin-mode-line-emoji-font
      (propertize emoji
                  'face `(:family ,wttrin-mode-line-emoji-font
                          :height 1.0
                          ,@(when foreground (list :foreground foreground))))
    (if foreground
        (propertize emoji 'face (list :foreground foreground))
      emoji)))

(defun wttrin--set-mode-line-string (icon tooltip)
  "Set mode-line weather string to ICON with TOOLTIP and standard properties."
  (setq wttrin-mode-line-string
        (propertize (concat " " icon)
                    'help-echo tooltip
                    'mouse-face 'mode-line-highlight
                    'local-map wttrin--mode-line-map))
  (force-mode-line-update t))

(defun wttrin--mode-line-valid-response-p (weather-string)
  "Return non-nil if WEATHER-STRING looks like a valid mode-line response.
Expected format: \"Location: emoji temp conditions\",
e.g., \"Paris: ☀️ +61°F Clear\"."
  (and (stringp weather-string)
       (not (string-empty-p weather-string))
       (string-match-p ":" weather-string)))

(defun wttrin--mode-line-update-placeholder-error ()
  "Update placeholder to show fetch error state.
Keeps the hourglass icon but updates tooltip to explain the failure
and indicate when retry will occur."
  (let ((retry-minutes (ceiling (/ wttrin-mode-line-refresh-interval 60.0))))
    (wttrin--set-mode-line-string
     (wttrin--make-emoji-icon "⏳")
     (format "Weather fetch failed for %s — will retry in %d minutes"
             wttrin-favorite-location retry-minutes))))

(defun wttrin--mode-line-fetch-weather ()
  "Fetch weather for favorite location and update mode-line display.
Uses wttr.in custom format for concise weather with emoji.
On success, writes to `wttrin--mode-line-cache' and updates display.
On failure with existing cache, shows stale data.
On failure with no cache, shows error placeholder."
  (wttrin--debug-log "mode-line-fetch: Starting fetch for %s" wttrin-favorite-location)
  (if (not wttrin-favorite-location)
      (wttrin--debug-log "mode-line-fetch: No favorite location set, skipping")
    (let* ((location wttrin-favorite-location)
           ;; wttr.in format codes: %l=location %c=emoji %t=temp %C=conditions
           (format-params (if wttrin-unit-system
                              (concat "?" wttrin-unit-system "&format=%l:+%c+%t+%C")
                            "?format=%l:+%c+%t+%C"))
           (url (concat "https://wttr.in/"
                       (url-hexify-string location)
                       format-params)))
      (wttrin--debug-log "mode-line-fetch: URL = %s" url)
      (wttrin--fetch-url
       url
       (lambda (data &optional _error-msg)
         (if data
             (let ((trimmed-data (string-trim data)))
               (wttrin--debug-log "mode-line-fetch: Received data = %S" trimmed-data)
               (if (wttrin--mode-line-valid-response-p trimmed-data)
                   (progn
                     (setq wttrin--mode-line-cache
                          (cons (float-time)
                                (wttrin--replace-response-location trimmed-data location)))
                     (wttrin--mode-line-update-display))
                 (wttrin--debug-log "mode-line-fetch: Invalid response, keeping previous display")))
           ;; Network error / nil data
           (wttrin--debug-log "mode-line-fetch: No data received (network error)")
           (if wttrin--mode-line-cache
               ;; Have stale cache — update display to show staleness
               (wttrin--mode-line-update-display)
             ;; No cache at all — show error placeholder
             (wttrin--mode-line-update-placeholder-error))))))))

(defun wttrin--mode-line-tooltip (&optional _window _object _pos)
  "Compute tooltip text from `wttrin--mode-line-cache'.
Calculates age at call time so the tooltip is always current.
If staleness has changed since the last render, triggers a re-render
so the emoji dimming matches.
Optional arguments are ignored (required by `help-echo' function protocol)."
  (when wttrin--mode-line-cache
    (let* ((timestamp (car wttrin--mode-line-cache))
           (weather-string (cdr wttrin--mode-line-cache))
           (age (- (float-time) timestamp))
           (stale-p (> age (* 2 wttrin-mode-line-refresh-interval)))
           (age-str (wttrin--format-age age)))
      ;; Re-render emoji if staleness state has changed
      (unless (eq stale-p wttrin--mode-line-rendered-stale)
        (wttrin--mode-line-update-display))
      (if stale-p
          (format "%s\nStale: updated %s — fetch failed, will retry"
                  weather-string age-str)
        (format "%s\nUpdated %s" weather-string age-str)))))

(defun wttrin--mode-line-update-display ()
  "Update mode-line display from `wttrin--mode-line-cache'.
Reads cached weather data, computes age, and sets the mode-line string.
If data is stale (age > 2x refresh interval), dims the emoji and
shows staleness info in tooltip."
  (when wttrin--mode-line-cache
    (let* ((timestamp (car wttrin--mode-line-cache))
           (weather-string (cdr wttrin--mode-line-cache))
           (age (- (float-time) timestamp))
           (stale-p (> age (* 2 wttrin-mode-line-refresh-interval))))
      (wttrin--debug-log "mode-line-display: Updating from cache, stale=%s" stale-p)
      ;; Response format is "Location: ☀️ +72°F Clear" — grab first char after colon
      (let ((emoji (if (string-match ":\\s-*\\(.\\)" weather-string)
                       (match-string 1 weather-string)
                     "?")))
        (wttrin--debug-log "mode-line-display: Extracted emoji = %S, stale = %s"
                           emoji stale-p)
        (setq wttrin--mode-line-rendered-stale stale-p)
        (setq wttrin-mode-line-string
              (propertize (concat " " (wttrin--make-emoji-icon emoji (when stale-p "gray60")))
                          'help-echo #'wttrin--mode-line-tooltip
                          'mouse-face 'mode-line-highlight
                          'local-map wttrin--mode-line-map)))))
  (force-mode-line-update t))

(defun wttrin-mode-line-click ()
  "Handle left-click on mode-line weather widget.
Check cache, refresh if needed, then open weather buffer."
  (interactive)
  (when wttrin-favorite-location
    (wttrin wttrin-favorite-location)))

(defun wttrin-mode-line-force-refresh ()
  "Handle right-click on mode-line weather widget.
Force-refresh cache and update tooltip without opening buffer."
  (interactive)
  (when wttrin-favorite-location
    (let ((wttrin--force-refresh t))
      (wttrin--mode-line-fetch-weather))))

(defun wttrin--mode-line-set-placeholder ()
  "Set a placeholder icon in the mode-line while waiting for weather data."
  (wttrin--set-mode-line-string
   (wttrin--make-emoji-icon "⏳")
   (format "Fetching weather for %s..." wttrin-favorite-location)))

(defvar wttrin--buffer-refresh-timer nil
  "Timer object for proactive buffer cache refresh.")

(defun wttrin--buffer-cache-refresh ()
  "Proactively refresh the buffer cache for `wttrin-favorite-location'.
Fetches fresh weather data and updates the buffer cache entry without
displaying anything.  This keeps buffer data fresh for when the user
opens the weather buffer."
  (when wttrin-favorite-location
    (let* ((location wttrin-favorite-location)
           (cache-key (wttrin--make-cache-key location)))
      (wttrin-fetch-raw-string
       location
       (lambda (fresh-data &optional _error-msg)
         (when fresh-data
           (wttrin--cleanup-cache-if-needed)
           (puthash cache-key (cons (float-time) fresh-data) wttrin--cache)))))))

(defun wttrin--mode-line-start ()
  "Start mode-line weather display and refresh timer."
  (wttrin--debug-log "wttrin mode-line: Starting mode-line display (location=%s, interval=%s)"
                     wttrin-favorite-location
                     wttrin-mode-line-refresh-interval)
  (when wttrin-favorite-location
    (wttrin--mode-line-set-placeholder)
    ;; Delay first fetch — network/daemon may not be ready at startup
    (run-at-time wttrin-mode-line-startup-delay nil #'wttrin--mode-line-fetch-weather)
    ;; Cancel existing timers to prevent duplicates on re-enable
    (when wttrin--mode-line-timer
      (cancel-timer wttrin--mode-line-timer))
    (setq wttrin--mode-line-timer
          (run-at-time wttrin-mode-line-refresh-interval
                      wttrin-mode-line-refresh-interval
                      #'wttrin--mode-line-fetch-weather))
    (when wttrin--buffer-refresh-timer
      (cancel-timer wttrin--buffer-refresh-timer))
    (setq wttrin--buffer-refresh-timer
          (run-at-time wttrin-refresh-interval
                      wttrin-refresh-interval
                      #'wttrin--buffer-cache-refresh))
    (wttrin--debug-log "wttrin mode-line: Initial fetch scheduled in %s seconds, then every %s seconds"
                       wttrin-mode-line-startup-delay
                       wttrin-mode-line-refresh-interval)))

(defun wttrin--mode-line-stop ()
  "Stop mode-line weather display and cancel timers."
  (wttrin--debug-log "wttrin mode-line: Stopping mode-line display")
  (when wttrin--mode-line-timer
    (cancel-timer wttrin--mode-line-timer)
    (setq wttrin--mode-line-timer nil))
  (when wttrin--buffer-refresh-timer
    (cancel-timer wttrin--buffer-refresh-timer)
    (setq wttrin--buffer-refresh-timer nil))
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-rendered-stale nil)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode wttrin-mode-line-mode
  "Toggle weather display in mode-line.
When enabled, shows weather for `wttrin-favorite-location'."
  :global t
  :lighter (:eval wttrin-mode-line-string)
  (if wttrin-mode-line-mode
      (progn
        (wttrin--debug-log "wttrin mode-line: Mode enabled")
        ;; after-init-time is nil during startup — defer network until ready.
        ;; noninteractive check skips deferral in batch mode (tests).
        (if (and (not after-init-time) (not noninteractive))
            (progn
              (wttrin--debug-log "wttrin mode-line: Deferring start until after-init-hook")
              (add-hook 'after-init-hook #'wttrin--mode-line-start))
          (wttrin--mode-line-start))
        ;; :lighter handles the built-in mode-line, but custom modelines
        ;; (e.g., doom-modeline) read global-mode-string instead
        (if global-mode-string
            (add-to-list 'global-mode-string 'wttrin-mode-line-string 'append)
          (setq global-mode-string '("" wttrin-mode-line-string)))
        (wttrin--debug-log "wttrin mode-line: Added to global-mode-string = %S" global-mode-string))
    (wttrin--debug-log "wttrin mode-line: Mode disabled")
    (wttrin--mode-line-stop)
    (setq global-mode-string
          (delq 'wttrin-mode-line-string global-mode-string))))

;;;###autoload
(defun wttrin (location)
  "Display weather information for LOCATION.
Weather data is fetched asynchronously to avoid blocking Emacs."
  (interactive
   (list
    (completing-read "Location Name: " wttrin-default-locations nil nil
                     (when (= (length wttrin-default-locations) 1)
                       (car wttrin-default-locations)))))
  (wttrin-query location))

(when wttrin-mode-line-auto-enable
  (wttrin-mode-line-mode 1))

(provide 'wttrin)
;;; wttrin.el ends here
