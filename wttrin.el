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

(defface wttrin-mode-line-stale
  '((t :inherit shadow))
  "Face for the mode-line weather emoji when its data is stale.
Applied when a scheduled refresh has failed and the cached reading is
older than twice `wttrin-mode-line-refresh-interval'.  A color emoji
font may ignore the foreground, in which case the dimming is only
visible on monochrome glyphs."
  :group 'wttrin)

(defface wttrin-staleness-header
  '((t :inherit shadow))
  "Face for the \"Last updated: ...\" line in the weather buffer."
  :group 'wttrin)

(defface wttrin-instructions
  '((t :inherit shadow))
  "Face for the key-hint footer prose in the weather buffer."
  :group 'wttrin)

(defface wttrin-key
  '((t :inherit bold))
  "Face for the bracketed key chords in the weather buffer footer.
`help-key-binding' would be the natural parent, but it only exists in
Emacs 28+, and wttrin supports 24.4, so the default inherits `bold'."
  :group 'wttrin)

(defface wttrin-instructions-header
  '((t :inherit (bold shadow)))
  "Face for the two column headers in the weather buffer footer.
Styles the \"This view\" and \"Saved locations\" labels that head the
two key-hint columns."
  :group 'wttrin)

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

(defcustom wttrin-display-options nil
  "wttr.in display option flags concatenated as a string.
Each character is a wttr.in flag, as documented at https://wttr.in/:help.
Common options:

  0    only current weather (no forecast)
  1    current weather + today's forecast
  2    current weather + today's + tomorrow's forecast
  d    restrict output to standard console font glyphs
  F    do not show the \"Follow\" line
  n    narrow version (only day and night)
  q    quiet version (no \"Weather report\" text)
  Q    superquiet version (no \"Weather report\", no city name)

Example: \"0Fq\" gives current weather only with no Follow line and no
header.  Default nil means no extra options.

Avoid \"A\" and \"T\" — wttrin manages ANSI output internally so the
xterm-color rendering produces the colored glyphs."
  :group 'wttrin
  :type '(choice (const :tag "None" nil)
                 (string :tag "Options")))


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

(defcustom wttrin-geolocation-enabled t
  "Whether geolocation features are available.
When non-nil (the default), the \"Current location (detect)\" entry is
offered in the picker, the `wttrin-favorite-location' = t auto-detect
runs, and the geolocation command works.  Set to nil to opt out: no
geolocation surface is offered and no detection request is made.
Geolocation is on by default; you opt out, you never have to opt in."
  :group 'wttrin
  :type 'boolean)

(defcustom wttrin-favorite-location nil
  "Favorite location to display weather for.

Three modes:
- nil      Favorite-location features are disabled (default).
- a string Use the string as the location, e.g. \"Berkeley, CA\".
- t        Auto-detect via IP geolocation.  wttrin runs the lookup
           once on first use and caches the result for the session.
           To pick a specific provider, customize
           `wttrin-geolocation-provider'.

When set, the weather icon and tooltip update automatically in the
background.  IP-based auto-detection can be inaccurate behind a VPN
or a mobile hotspot — use a string if you need accuracy."
  :group 'wttrin
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Auto-detect via geolocation" t)
                 (string :tag "Location")))

(defcustom wttrin-saved-locations nil
  "Directory of named locations, an alist of (NAME . QUERY) string conses.
NAME is what shows in the picker, the buffer header, and the mode-line.  QUERY
is what wttr.in is fetched with: a city, a street address, or \"lat,lng\"
coordinates.  For example:

  ((\"Superdome\" . \"1500 Sugar Bowl Dr, New Orleans\")
   (\"Home\" . \"41.37,-71.83\"))

A bare string S used anywhere a location is expected is shorthand for
\(S . S) — name and query the same.  Persisted across sessions via
`savehist-mode'; add entries interactively with \\[wttrin-save-location] or the
`d' key in a weather buffer, or set this in your init."
  :group 'wttrin
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Query")))

(defvar wttrin--resolved-favorite-location nil
  "Cached geolocation result for `wttrin-favorite-location' = t.
Holds the resolved \"City, Region\" string so subsequent reads
do not re-fetch.  Reset implicitly when the Emacs session ends.")

(defvar wttrin--favorite-location-pending nil
  "Non-nil while a geolocation lookup for the favorite is in flight.
Prevents duplicate concurrent lookups when several consumers ask
during the resolution window.")

(defun wttrin--resolve-favorite-location ()
  "Return the favorite location's query string, or nil if unavailable.
Resolves `wttrin-favorite-location' across the three modes:
- nil      -> nil (disabled)
- a string -> its saved-locations query when the string is a saved name,
              otherwise the string as-is (the query for a plain location)
- t        -> the cached geolocation result.  When the cache is empty
              and no lookup is in flight, kicks off an async detect
              and returns nil for this call.  The next call after the
              lookup completes returns the resolved string."
  (cond
   ((null wttrin-favorite-location) nil)
   ((stringp wttrin-favorite-location)
    (wttrin--resolve-location-query wttrin-favorite-location))
   ((eq wttrin-favorite-location t)
    (or wttrin--resolved-favorite-location
        (progn
          (wttrin--start-favorite-location-detect)
          nil)))))

(defun wttrin--start-favorite-location-detect ()
  "Kick off an async geolocation lookup if one is not already pending.
On success the resolved string is stored in
`wttrin--resolved-favorite-location'.  Failures (network error, parse
error) leave the cache empty and clear the pending flag, so the next
call retries."
  (when (and wttrin-geolocation-enabled
             (not wttrin--favorite-location-pending))
    (setq wttrin--favorite-location-pending t)
    (require 'wttrin-geolocation)
    (wttrin-geolocation-detect
     (lambda (location &optional _address)
       (setq wttrin--favorite-location-pending nil)
       (when location
         (setq wttrin--resolved-favorite-location location)
         (wttrin--debug-log
          "Resolved favorite-location via geolocation: %s" location))))))

(defun wttrin--favorite-location-display-name ()
  "Return a human-readable name for the favorite location.
For a string favorite this is the string itself (a saved-location name shows as
its name, not its resolved query).  For t it is the resolved geolocation place,
or \"current location\" while a lookup is pending.  Nil when disabled."
  (cond
   ((stringp wttrin-favorite-location) wttrin-favorite-location)
   ((eq wttrin-favorite-location t)
    (or wttrin--resolved-favorite-location "current location"))
   (t nil)))

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

;;; Mode-line state and update flow
;;
;; The state lives in four variables (defined below): the cache as source
;; of truth, the rendered string for `global-mode-string', a stale-render
;; flag, and the refresh timer.
;;
;; Normal update path:
;;   `wttrin--mode-line-fetch-weather' updates `wttrin--mode-line-cache'
;;   and then calls `wttrin--mode-line-update-display', which reads the
;;   cache, decides staleness via `wttrin--mode-line-stale-p', and writes
;;   both `wttrin-mode-line-string' and `wttrin--mode-line-rendered-stale'.
;;
;; Tooltip-driven re-render:
;;   `wttrin--mode-line-tooltip' fires on every mouse hover.  It
;;   re-evaluates staleness against the current cache age and, if that
;;   flips relative to `wttrin--mode-line-rendered-stale', calls
;;   `wttrin--mode-line-update-display' to refresh dimming.  This keeps
;;   the tooltip age and emoji color in sync when a fetch has been
;;   failing for a while.

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

;;; Error Types

;; A small condition hierarchy so callers can branch on the *class* of a
;; failure instead of matching message text.  `wttrin-error' is the parent.
;; Synchronous code paths signal these directly; the async fetch path tags its
;; human-readable error string with the class via the `wttrin-error-type' text
;; property (see `wttrin--error-message'), so two-arg callbacks keep working
;; while callers that care can read the class.

(define-error 'wttrin-error "wttrin error")
(define-error 'wttrin-invalid-input "Invalid input" 'wttrin-error)
(define-error 'wttrin-network-error "Network error" 'wttrin-error)
(define-error 'wttrin-not-found-error "Location not found" 'wttrin-error)
(define-error 'wttrin-service-error "Weather service error" 'wttrin-error)
(define-error 'wttrin-parse-error "Could not parse weather response" 'wttrin-error)

(defun wttrin--error-message (type format-string &rest args)
  "Format an error message of class TYPE.
Return the string built from FORMAT-STRING and ARGS with TYPE stored in its
`wttrin-error-type' text property.  This lets the async fetch path hand a
plain string to callbacks while still carrying the error class; read it back
with `wttrin-error-message-type'."
  (propertize (apply #'format format-string args) 'wttrin-error-type type))

(defun wttrin-error-message-type (error-msg)
  "Return the error-class symbol carried by ERROR-MSG, or nil.
ERROR-MSG is a string produced by wttrin's async fetch path; its class is
stored in the `wttrin-error-type' text property.  A plain, empty, or nil
ERROR-MSG has no class."
  (and (stringp error-msg)
       (> (length error-msg) 0)
       (get-text-property 0 'wttrin-error-type error-msg)))

(defun wttrin--build-url (query)
  "Build wttr.in URL for QUERY with configured parameters."
  (when (null query)
    (signal 'wttrin-invalid-input '("Query cannot be nil")))
  (concat "https://wttr.in/"
          (url-hexify-string query)
          (wttrin-additional-url-params)
          "A"
          (or wttrin-display-options "")))

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
      (setq error-msg (wttrin--error-message
                       'wttrin-network-error
                       "Network error — check your connection"))
      (message "wttrin: %s" error-msg))
     ;; HTTP response received — extract body (returns nil for non-2xx)
     (t
      (let ((http-status (wttrin--extract-http-status)))
        (setq data (wttrin--extract-response-body))
        (when (not data)
          (setq error-msg
                (cond
                 ((null http-status)
                  (wttrin--error-message
                   'wttrin-parse-error "Could not read weather response"))
                 ((and (>= http-status 400) (< http-status 500))
                  (wttrin--error-message
                   'wttrin-not-found-error "Location not found (HTTP %d)" http-status))
                 ((>= http-status 500)
                  (wttrin--error-message
                   'wttrin-service-error "Weather service error (HTTP %d)" http-status))
                 ((< http-status 300)
                  (wttrin--error-message
                   'wttrin-parse-error "Could not parse weather response (HTTP %d)" http-status))
                 (t
                  (wttrin--error-message
                   'wttrin-error "Unexpected HTTP status %d" http-status))))
          (message "wttrin: %s" error-msg)))))
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

;;; Location Search History

(defcustom wttrin-location-history-max 20
  "Maximum number of entries to keep in location search history.
When the history exceeds this limit, the oldest entries are removed."
  :group 'wttrin
  :type 'integer)

(defvar wttrin--location-history nil
  "History of successfully searched locations, most recent first.
Persisted across sessions via `savehist-mode'.")

;; Declared so the byte-compiler doesn't warn; savehist defines it for real.
(defvar savehist-additional-variables)

(defun wttrin--savehist-register ()
  "Ensure wttrin's persisted variables are saved by savehist.
Registers `wttrin--location-history', `wttrin-favorite-location', and
`wttrin-saved-locations' so they survive across restarts without the Emacs
custom-variable mechanism.
Run both at load and on `savehist-save-hook', so the registration survives a
user `setq' of `savehist-additional-variables' (a common config pattern) that
would otherwise drop the entries before they could be saved."
  (add-to-list 'savehist-additional-variables 'wttrin--location-history)
  (add-to-list 'savehist-additional-variables 'wttrin-favorite-location)
  (add-to-list 'savehist-additional-variables 'wttrin-saved-locations))

(with-eval-after-load 'savehist
  (wttrin--savehist-register)
  (add-hook 'savehist-save-hook #'wttrin--savehist-register))

(defconst wttrin--geolocation-sentinel "Current location (detect)"
  "Picker candidate that triggers geolocation detection.
Selecting it routes through `wttrin--query-selection' to a
detect-then-query flow instead of being treated as a literal place
name.  It is never persisted to history or the cache as a location.")

(defun wttrin--add-to-location-history (location)
  "Record LOCATION as a recent successful search.
No-op when LOCATION is nil, empty, the geolocation sentinel, raw \"LAT,LNG\"
coordinates, a default location, or a saved-directory name (coordinates jitter
and saved names are already offered via the directory).  An existing entry is
promoted to most-recent, and the list is trimmed to
`wttrin-location-history-max'."
  (when (and location
             (not (string= location ""))
             (not (string= location wttrin--geolocation-sentinel))
             (not (wttrin--coordinates-p location))
             (not (member location wttrin-default-locations))
             (not (assoc location (wttrin--saved-locations))))
    (setq wttrin--location-history (delete location wttrin--location-history))
    (push location wttrin--location-history)
    (let ((max (max 0 wttrin-location-history-max)))
      (when (> (length wttrin--location-history) max)
        (setq wttrin--location-history
              (butlast wttrin--location-history
                       (- (length wttrin--location-history) max)))))))

(defun wttrin--drop-from-location-history (&rest locations)
  "Remove each non-nil string in LOCATIONS from `wttrin--location-history'.
Keeps the directory and the history disjoint: a place lives in one or the
other, never both."
  (dolist (location locations)
    (when (and location (stringp location))
      (setq wttrin--location-history
            (delete location wttrin--location-history)))))

(defun wttrin--saved-locations ()
  "Return `wttrin-saved-locations' as a clean list of (NAME . QUERY) pairs.
Skips malformed entries — non-cons, a non-string name or query, or an empty
name or query — and trims surrounding whitespace, so stale or hand-edited
config never errors.  A bare string S is read as (S . S)."
  (delq nil
        (mapcar
         (lambda (entry)
           (cond
            ((and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
             (let ((name (string-trim (car entry)))
                   (query (string-trim (cdr entry))))
               (and (> (length name) 0) (> (length query) 0) (cons name query))))
            ((stringp entry)
             (let ((s (string-trim entry)))
               (and (> (length s) 0) (cons s s))))
            (t nil)))
         wttrin-saved-locations)))

(defun wttrin--resolve-location-query (selection)
  "Return the query string for a picker SELECTION.
When SELECTION is a saved-location name, return its query; otherwise return
SELECTION unchanged, so typed, default, and history strings pass through."
  (or (cdr (assoc selection (wttrin--saved-locations)))
      selection))

(defun wttrin--coordinates-p (string)
  "Return non-nil when STRING looks like \"LAT,LNG\" coordinates.
Used to keep a raw geolocation fix out of history and to decide when the
`d' key should prompt for a name instead of promoting coordinates directly."
  (and (stringp string)
       (string-match-p "\\`[ ]*-?[0-9.]+[ ]*,[ ]*-?[0-9.]+[ ]*\\'" string)))

(defun wttrin--saved-locations-without (name)
  "Return `wttrin-saved-locations' with any entry named NAME removed."
  (delq nil
        (mapcar (lambda (entry)
                  (unless (and (consp entry) (equal (car entry) name)) entry))
                wttrin-saved-locations)))

(defun wttrin--put-saved-location (name query)
  "Add or update NAME -> QUERY in `wttrin-saved-locations'; return the saved name.
Trims NAME and QUERY.  Signals a `user-error' for an empty name or query, or a
name equal to the geolocation sentinel.  An existing name has its query updated."
  (let ((name (string-trim (or name "")))
        (query (string-trim (or query ""))))
    (when (string= name "") (user-error "Location name cannot be empty"))
    (when (string= query "") (user-error "Location query cannot be empty"))
    (when (string= name wttrin--geolocation-sentinel)
      (user-error "That name is reserved for the geolocation entry"))
    (setq wttrin-saved-locations
          (append (wttrin--saved-locations-without name)
                  (list (cons name query))))
    name))

(defun wttrin--remove-saved-location (name)
  "Remove the saved location named NAME from `wttrin-saved-locations'."
  (setq wttrin-saved-locations (wttrin--saved-locations-without name)))

(defvar-local wttrin--current-location nil
  "Query for the weather shown in this buffer (the fetch/cache identity).")

(defvar-local wttrin--current-display nil
  "Display name for the weather shown in this buffer (a saved-location name).
Falls back to the query when there is no distinct name.")

(defvar-local wttrin--current-address nil
  "Resolved address for this buffer, shown on the \"Location:\" line.
Set by the geolocation command path; nil otherwise.")

(defun wttrin--location-name-prefill ()
  "Best prefill for naming the current buffer's place when saving it.
An existing alias name (a display distinct from the query), else the detected
address, else the query.  Shared by the save command and the `d' key so both
offer the same starting text."
  (or (and wttrin--current-display
           (not (equal wttrin--current-display wttrin--current-location))
           wttrin--current-display)
      wttrin--current-address
      wttrin--current-location))

(defun wttrin--current-saved-name ()
  "Return this buffer's display name when it names a saved location, else nil.
Lets the rename and remove commands default to the place on screen."
  (and wttrin--current-display
       (assoc wttrin--current-display (wttrin--saved-locations))
       wttrin--current-display))

(defun wttrin--completion-candidates ()
  "Return picker candidates: saved names, the favorite, defaults, then history.
De-duplicated by display string with precedence saved > favorite > defaults >
history (the explicit alias wins over a same-named default or history string),
so each place appears exactly once.  The geolocation sentinel is prepended when
geolocation is enabled."
  (let* ((saved (mapcar #'car (wttrin--saved-locations)))
         (favorite (and (stringp wttrin-favorite-location)
                        (list wttrin-favorite-location)))
         (deduped (delete-dups
                   (append saved favorite
                           (copy-sequence wttrin-default-locations)
                           (copy-sequence wttrin--location-history)))))
    (if wttrin-geolocation-enabled
        (cons wttrin--geolocation-sentinel deduped)
      deduped)))

(defun wttrin--sort-completions (candidates)
  "Return CANDIDATES with the geolocation sentinel pinned first.
The remaining candidates keep the order `wttrin--completion-candidates'
produced (favorite, defaults, then history).  Used as the completion
metadata `display-sort-function' so sorting UIs (vertico, icomplete, the
default *Completions* buffer) keep the sentinel at the top instead of
re-sorting it into alphabetical position."
  (if (member wttrin--geolocation-sentinel candidates)
      (cons wttrin--geolocation-sentinel
            (remove wttrin--geolocation-sentinel candidates))
    candidates))

(defun wttrin--completion-table (candidates)
  "Return a completion table over CANDIDATES that pins the sentinel first.
The table answers the `metadata' action with a `display-sort-function'
of `wttrin--sort-completions', and otherwise completes over CANDIDATES.
Wrapping the list this way is what keeps the sentinel first across
completion frameworks that impose their own sort order."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'wttrin--sort-completions))
      (complete-with-action action candidates string predicate))))

(defun wttrin--detect-then-query ()
  "Detect the current location asynchronously, then query weather for it.
No-op with a message when `wttrin-geolocation-enabled' is nil.  On detection
failure, show an actionable message and leave the favorite untouched; the user
can fall back to typing a city in the picker."
  (if (not wttrin-geolocation-enabled)
      (message "Geolocation is disabled (set wttrin-geolocation-enabled to enable it)")
    (require 'wttrin-geolocation)
    (message "Detecting location...")
    (wttrin-geolocation-detect
     (lambda (location &optional address)
       (if location
           (wttrin-query location nil address)
         (message "Could not detect location (network or provider error)"))))))

(defun wttrin--query-selection (selection)
  "Route a picker SELECTION to the right query path.
The geolocation sentinel routes to `wttrin--detect-then-query'.  Any other
SELECTION is resolved through the saved-locations directory to its query and
fetched with the name shown as the display value, so an alias shows its name
while wttr.in is hit with the target.  This is the single guard that keeps the
sentinel from reaching `wttrin-query' as a place name."
  (if (string= selection wttrin--geolocation-sentinel)
      (wttrin--detect-then-query)
    (wttrin-query (wttrin--resolve-location-query selection) selection)))

(defun wttrin-remove-location-history (location)
  "Remove LOCATION from the search history.
Prompts with completion over the current history entries."
  (interactive
   (list (completing-read "Remove from history: "
                          wttrin--location-history nil t)))
  (setq wttrin--location-history (delete location wttrin--location-history))
  (message "Removed '%s' from location history" location))

(defun wttrin-clear-location-history ()
  "Clear all location search history."
  (interactive)
  (when (yes-or-no-p "Clear all location search history? ")
    (setq wttrin--location-history nil)
    (message "Location history cleared")))

;;; Saved-location directory management

;;;###autoload
(defun wttrin-save-location (name query)
  "Save QUERY under NAME in the saved-locations directory.
Interactively, default QUERY to the current weather buffer's location (or
prompt for one), and prefill the name with the buffer's display name, else its
address, else the query.  Saving an existing name updates its query."
  (interactive
   (let* ((query (or wttrin--current-location
                     (wttrin--resolve-location-query
                      (completing-read
                       "Save which location (query): "
                       (wttrin--completion-table (wttrin--completion-candidates))))))
          (name (read-string "Save location as: " (wttrin--location-name-prefill))))
     (list name query)))
  (if (string= (string-trim name) "")
      (message "Cancelled")
    (let ((existing (assoc (string-trim name) (wttrin--saved-locations)))
          (saved (wttrin--put-saved-location name query)))
      (wttrin--drop-from-location-history saved query)
      (message (if existing "Updated %s" "Saved %s") saved))))

(defun wttrin-rename-location (old new)
  "Rename the saved location OLD to NEW.
Refuses when NEW already names a different entry.  When OLD is the favorite,
the favorite is updated to NEW."
  (interactive
   (let* ((default (wttrin--current-saved-name))
          (old (completing-read "Rename saved location: "
                                (mapcar #'car (wttrin--saved-locations))
                                nil t nil nil default))
          (new (read-string "New name: " old)))
     (list old new)))
  (let ((new (string-trim new))
        (entry (assoc old (wttrin--saved-locations))))
    (cond
     ((not entry) (user-error "No saved location named %s" old))
     ((string= new "") (user-error "New name cannot be empty"))
     ((and (not (string= new old)) (assoc new (wttrin--saved-locations)))
      (user-error "A saved location named %s already exists" new))
     (t
      (let ((query (cdr entry)))
        (wttrin--remove-saved-location old)
        (wttrin--put-saved-location new query)
        (when (equal wttrin-favorite-location old)
          (wttrin--set-favorite-location new))
        (message "Renamed %s to %s" old new))))))

(defun wttrin-remove-location (name)
  "Remove the saved location NAME from the directory, after confirmation.
When NAME is the favorite, it is left as a literal query with a warning."
  (interactive
   (list (completing-read "Remove saved location: "
                          (mapcar #'car (wttrin--saved-locations))
                          nil t nil nil (wttrin--current-saved-name))))
  (cond
   ((not (assoc name (wttrin--saved-locations)))
    (user-error "No saved location named %s" name))
   ((yes-or-no-p (format "Remove saved location \"%s\"? " name))
    (let ((query (cdr (assoc name (wttrin--saved-locations)))))
      (wttrin--remove-saved-location name)
      (wttrin--drop-from-location-history name query))
    (if (equal wttrin-favorite-location name)
        (progn
          (when (bound-and-true-p wttrin-mode-line-mode)
            (wttrin--mode-line-refresh-now))
          (message "Removed %s; it was your favorite and is now a literal query until you set a new one"
                   name))
      (message "Removed %s" name)))
   (t (message "Cancelled"))))

(defun wttrin--requery-location (new-location)
  "Kill current weather buffer and query NEW-LOCATION."
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*"))
  (wttrin--query-selection new-location))

(defun wttrin-requery ()
  "Kill buffer and requery wttrin."
  (interactive)
  (let ((new-location (completing-read
                       "Location Name: "
                       (wttrin--completion-table (wttrin--completion-candidates))
                       nil nil
                       (when (= (length wttrin-default-locations) 1)
                         (car wttrin-default-locations)))))
    (wttrin--requery-location new-location)))

(defvar wttrin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'wttrin-requery)
    (define-key map (kbd "g") 'wttrin-requery-force)
    (define-key map (kbd "d") 'wttrin-make-default)
    (define-key map (kbd "s") 'wttrin-save-location)
    (define-key map (kbd "r") 'wttrin-rename-location)
    (define-key map (kbd "x") 'wttrin-remove-location)
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

(defconst wttrin--footer-left-width 23
  "Visible width of the left column in the weather-buffer footer.
The right column begins at this offset so the two columns align.")

(defun wttrin--footer-cell (key label)
  "Return a propertized \"[KEY] LABEL\" footer cell.
The bracketed KEY uses `wttrin-key'; LABEL uses `wttrin-instructions'."
  (concat (propertize (format "[%s]" key) 'face 'wttrin-key)
          (propertize (format " %s" label) 'face 'wttrin-instructions)))

(defun wttrin--footer-pad (cell width)
  "Pad CELL with trailing spaces to a visible WIDTH.
Returns CELL unchanged when it is already at least WIDTH characters wide."
  (let ((deficit (- width (length cell))))
    (if (> deficit 0)
        (concat cell (make-string deficit ?\s))
      cell)))

(defun wttrin--add-buffer-instructions ()
  "Add the two-column key-hint footer at the bottom of the current buffer.
The left column lists keys that act on the current view; the right column
lists keys that act on the saved-locations directory.  Bracketed key chords
use `wttrin-key', labels use `wttrin-instructions', and the column headers
use `wttrin-instructions-header'."
  (goto-char (point-max))
  (insert "\n\n")
  (let* ((header (concat (wttrin--footer-pad
                          (propertize "This view" 'face 'wttrin-instructions-header)
                          wttrin--footer-left-width)
                         (propertize "Saved locations"
                                     'face 'wttrin-instructions-header)))
         (rows (list (cons (wttrin--footer-cell "a" "another")
                           (wttrin--footer-cell "s" "save"))
                     (cons (wttrin--footer-cell "g" "refresh")
                           (wttrin--footer-cell "d" "make default"))
                     (cons (wttrin--footer-cell "q" "quit")
                           (wttrin--footer-cell "r" "rename"))
                     (cons nil
                           (wttrin--footer-cell "x" "remove"))))
         (lines (cons header
                      (mapcar (lambda (row)
                                (concat (wttrin--footer-pad (or (car row) "")
                                                            wttrin--footer-left-width)
                                        (or (cdr row) "")))
                              rows))))
    (insert (mapconcat #'identity lines "\n"))))

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
        (propertize (format "Last updated: %s (%s)" (string-trim time-str) age-str)
                    'face 'wttrin-staleness-header)))))

(defun wttrin--format-location-line (address)
  "Return a propertized \"Location: ADDRESS\" line, or nil when ADDRESS is empty.
Shown in the weather buffer when a geolocation command supplied a human-readable
place name alongside its coordinates, so the resolved location is recognizable
even though the weather was fetched by raw coordinates."
  (when (and (stringp address) (> (length address) 0))
    (propertize (concat "Location: " address) 'face 'wttrin-staleness-header)))

(defun wttrin--display-weather (query raw-string &optional error-msg display address)
  "Display weather RAW-STRING for QUERY in the weather buffer.
QUERY is the location wttr.in was fetched with — the cache key and the buffer's
refresh identity.  DISPLAY is what the header shows (a saved-location name);
when nil it falls back to QUERY.  ERROR-MSG, when provided and the data is
invalid, is shown instead of the generic error.  ADDRESS, when non-empty, shows
on a \"Location:\" line below the weather (the geolocation path fetches by
coordinates but can name the place)."
  (let ((display (or display query)))
    (when wttrin-debug
      (wttrin--save-debug-data query raw-string))

    (if (not (wttrin--validate-weather-data raw-string))
        (message "wttrin: %s"
                 (or error-msg
                     "Cannot retrieve weather data. Perhaps the location was misspelled?"))
      (wttrin--add-to-location-history display)
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
          ;; wttr.in returns location in lowercase — replace with the display name
          (goto-char (point-min))
          (when (re-search-forward "^Weather report: .*$" nil t)
            (replace-match (concat "Weather report: " display)))
          (let ((location-line (wttrin--format-location-line address)))
            (when location-line
              (insert "\n" location-line)))
          ;; The cache is keyed on QUERY, so the staleness header reads QUERY.
          (let ((staleness (wttrin--format-staleness-header query)))
            (when staleness
              (insert "\n" staleness)))
          (wttrin--add-buffer-instructions)
          (goto-char (point-min)))

        ;; Anchor the window to the top.  Point is at point-min, but when the
        ;; buffer is taller than the window a reused window can keep an old
        ;; mid-buffer window-start, hiding the weather above the fold.
        (let ((win (get-buffer-window buffer)))
          (when win (set-window-start win (point-min))))

        (setq-local wttrin--current-location query)
        (setq-local wttrin--current-display display)
        (setq-local wttrin--current-address address)
        (wttrin--debug-mode-line-info)))))

(defun wttrin-query (query &optional display address)
  "Asynchronously query weather for QUERY, display the result when ready.
QUERY is what weather is fetched by (and the cache key).  Optional DISPLAY is
the name shown in the header (a saved-location name); when nil it falls back to
QUERY.  Optional ADDRESS is shown on a \"Location:\" line, used when QUERY is raw
coordinates from a geolocation command."
  (let ((buffer (get-buffer-create (format "*wttr.in*"))))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Loading weather for " (or display query) "...")
    (setq buffer-read-only t)
    (wttrin--get-cached-or-fetch
     query
     (lambda (raw-string &optional error-msg)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (wttrin--display-weather query raw-string error-msg display address)))))))

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
result to `wttrin-favorite-location'.

With `savehist-mode' on, the favorite persists across sessions
automatically (wttrin registers it with savehist); no
`customize-save-variable' step is needed.

IP-based geolocation can be wrong behind a VPN or a mobile hotspot.
The confirmation prompt shows the detected location so you can
reject inaccurate results.

This command is obsolete.  Prefer the \"Current location (detect)\"
entry in \\[wttrin], then press `d' in the weather buffer to keep
the detected city as your default."
  (interactive)
  (if (not wttrin-geolocation-enabled)
      (message "Geolocation is disabled (set wttrin-geolocation-enabled to enable it)")
    (require 'wttrin-geolocation)
    (message "Detecting location...")
    (wttrin-geolocation-detect
     (lambda (location)
       (cond
        ((null location)
         (message "Could not detect location (network or provider error)"))
        ((yes-or-no-p (format "Detected location: %s. Set as favorite? "
                              location))
         (wttrin--set-favorite-location location)
         (message "Set wttrin-favorite-location to: %s%s"
                  location
                  (if (bound-and-true-p savehist-mode)
                      " (persisted via savehist)."
                    ". Enable savehist-mode to persist it across sessions.")))
        (t
         (message "Location detection cancelled")))))))

(make-obsolete
 'wttrin-set-location-from-geolocation
 "use the \"Current location (detect)\" entry in `wttrin', then press `d' to keep it as the default."
 "0.4.0")

;;;###autoload
(defun wttrin-use-current-location ()
  "Make your current location the persistent favorite (always auto-detect).
Sets `wttrin-favorite-location' to t after confirmation, so the mode-line
and buffer track wherever you are via geolocation rather than a fixed city.
This is the labeled way to choose auto-detect without typing the bare symbol
t into your init.

With `savehist-mode' on, the choice persists across sessions automatically.
Does nothing when `wttrin-geolocation-enabled' is nil."
  (interactive)
  (cond
   ((not wttrin-geolocation-enabled)
    (message "Geolocation is disabled (set wttrin-geolocation-enabled to enable it)"))
   ((yes-or-no-p "Always use your current location (auto-detect via geolocation)? ")
    (wttrin--set-favorite-location t)
    (message "Favorite location set to auto-detect%s"
             (if (bound-and-true-p savehist-mode)
                 " (persisted via savehist)."
               ". Enable savehist-mode to persist it across sessions.")))
   (t
    (message "Cancelled"))))

(defun wttrin-requery-force ()
  "Force refresh weather data for current location, bypassing cache."
  (interactive)
  (if wttrin--current-location
      (let ((wttrin--force-refresh t))
        (message "Refreshing weather data...")
        (wttrin-query wttrin--current-location
                      wttrin--current-display
                      wttrin--current-address))
    (message "No location to refresh")))

(defun wttrin--set-favorite-location (location)
  "Set `wttrin-favorite-location' to LOCATION and drop it from search history.
LOCATION becomes a permanent default, so it no longer needs a history entry,
mirroring how `wttrin-default-locations' entries are kept out of history.
Persistence is handled by `wttrin--savehist-register', which registers the
variable when savehist loads and again on `savehist-save-hook', so the value
survives restarts without the Emacs custom-variable mechanism, and setting it
here works whether or not savehist is loaded."
  (let ((changed (not (equal location wttrin-favorite-location))))
    (setq wttrin-favorite-location location)
    (setq wttrin--location-history (delete location wttrin--location-history))
    (when (and changed (bound-and-true-p wttrin-mode-line-mode))
      (wttrin--mode-line-refresh-now))))

(defun wttrin-make-default ()
  "Make the location shown in this buffer the favorite (persisted) default.
A named buffer (a saved alias or a typed location) is saved to the directory and
promoted, so the default also persists as a named entry rather than only the
favorite string.  A raw coordinate buffer (a fresh geolocation detection) first
prompts for a name, prefilled with the detected address; the entered name is
saved and promoted.  An empty name keeps the raw coordinates as the default
without saving a directory entry.  No-op with a message when the buffer has no
current location."
  (interactive)
  (cond
   ((null wttrin--current-location)
    (message "wttrin: no location to make default"))
   ((wttrin--coordinates-p wttrin--current-location)
    (let ((name (string-trim
                 (read-string "Save location as (empty keeps coordinates): "
                              (wttrin--location-name-prefill)))))
      (if (string= name "")
          (progn
            (wttrin--set-favorite-location wttrin--current-location)
            (message "wttrin: %s is now the default location" wttrin--current-location))
        (wttrin--put-saved-location name wttrin--current-location)
        (wttrin--set-favorite-location name)
        (wttrin--drop-from-location-history name wttrin--current-location)
        (message "wttrin: %s is now the default location" name))))
   (t
    (let ((favorite (or wttrin--current-display wttrin--current-location)))
      (wttrin--put-saved-location favorite wttrin--current-location)
      (wttrin--set-favorite-location favorite)
      (wttrin--drop-from-location-history favorite wttrin--current-location)
      (message "wttrin: %s is now the default location" favorite)))))

;;; Mode-line weather display

(defun wttrin--replace-response-location (response location)
  "Replace the API's location prefix in RESPONSE with LOCATION.
The wttr.in API returns locations in lowercase.  This substitutes the
user's original casing so tooltips display what the user expects."
  (if (string-match ":" response)
      (concat location (substring response (match-beginning 0)))
    response))

(defun wttrin--make-emoji-icon (emoji &optional face)
  "Create EMOJI string, optionally styled with FACE and the emoji font.
Uses `wttrin-mode-line-emoji-font' when configured.  FACE, when non-nil,
is applied via `:inherit'.  Omitting it avoids a literal `:inherit nil'
entry, which triggers \"Invalid face attribute\" warnings on every
redisplay."
  (if wttrin-mode-line-emoji-font
      (propertize emoji
                  'face `(:family ,wttrin-mode-line-emoji-font
                          :height 1.0
                          ,@(when face (list :inherit face))))
    (if face
        (propertize emoji 'face (list :inherit face))
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
  (let ((retry-minutes (ceiling (/ wttrin-mode-line-refresh-interval 60.0)))
        (label (or (wttrin--favorite-location-display-name) "favorite")))
    (wttrin--set-mode-line-string
     (wttrin--make-emoji-icon "⏳")
     (format "Weather fetch failed for %s — will retry in %d minutes"
             label retry-minutes))))

(defun wttrin--mode-line-fetch-weather ()
  "Fetch weather for favorite location and update mode-line display.
Uses wttr.in custom format for concise weather with emoji.
On success, writes to `wttrin--mode-line-cache' and updates display.
On failure with existing cache, shows stale data.
On failure with no cache, shows error placeholder.
When `wttrin-favorite-location' is t and geolocation has not yet
resolved, this call is a no-op; the next tick after resolution
proceeds normally."
  (wttrin--debug-log "mode-line-fetch: Starting fetch for %s" wttrin-favorite-location)
  (let ((location (wttrin--resolve-favorite-location)))
    (if (not location)
        (wttrin--debug-log "mode-line-fetch: No favorite location available, skipping")
      (let* (;; wttr.in format codes: %l=location %c=emoji %t=temp %C=conditions
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
                   (let ((display (or (wttrin--favorite-location-display-name)
                                      location)))
                     (setq wttrin--mode-line-cache
                          (cons (float-time)
                                (wttrin--replace-response-location trimmed-data display)))
                     (wttrin--mode-line-update-display))
                 (wttrin--debug-log "mode-line-fetch: Invalid response, keeping previous display")))
           ;; Network error / nil data
           (wttrin--debug-log "mode-line-fetch: No data received (network error)")
           (if wttrin--mode-line-cache
               ;; Have stale cache — update display to show staleness
               (wttrin--mode-line-update-display)
             ;; No cache at all — show error placeholder
             (wttrin--mode-line-update-placeholder-error)))))))))

(defun wttrin--mode-line-extract-emoji (weather-string)
  "Extract the emoji character from WEATHER-STRING.
The expected format is \"Location: emoji temp conditions\".  Returns
the first non-whitespace character after the colon, or \"?\" when
WEATHER-STRING contains no colon."
  (if (string-match ":\\s-*\\(.\\)" weather-string)
      (match-string 1 weather-string)
    "?"))

(defun wttrin--mode-line-stale-p (cache-entry)
  "Return non-nil if CACHE-ENTRY is stale.
Stale means age greater than 2 × `wttrin-mode-line-refresh-interval'.
CACHE-ENTRY is a (TIMESTAMP . WEATHER-STRING) cons or nil.  A nil
entry returns nil so callers can pass `wttrin--mode-line-cache' directly
without a separate guard."
  (when cache-entry
    (let ((age (- (float-time) (car cache-entry))))
      (> age (* 2 wttrin-mode-line-refresh-interval)))))

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
           (stale-p (wttrin--mode-line-stale-p wttrin--mode-line-cache))
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
    (let* ((weather-string (cdr wttrin--mode-line-cache))
           (stale-p (wttrin--mode-line-stale-p wttrin--mode-line-cache)))
      (wttrin--debug-log "mode-line-display: Updating from cache, stale=%s" stale-p)
      (let ((emoji (wttrin--mode-line-extract-emoji weather-string)))
        (wttrin--debug-log "mode-line-display: Extracted emoji = %S, stale = %s"
                           emoji stale-p)
        (setq wttrin--mode-line-rendered-stale stale-p)
        (setq wttrin-mode-line-string
              (propertize (concat " " (wttrin--make-emoji-icon emoji (when stale-p 'wttrin-mode-line-stale)))
                          'help-echo #'wttrin--mode-line-tooltip
                          'mouse-face 'mode-line-highlight
                          'local-map wttrin--mode-line-map)))))
  (force-mode-line-update t))

(defun wttrin-mode-line-click ()
  "Handle left-click on mode-line weather widget.
Check cache, refresh if needed, then open weather buffer."
  (interactive)
  (let ((location (wttrin--resolve-favorite-location)))
    (when location
      (wttrin location))))

(defun wttrin-mode-line-force-refresh ()
  "Handle right-click on mode-line weather widget.
Force-refresh cache and update tooltip without opening buffer."
  (interactive)
  (when (wttrin--resolve-favorite-location)
    (let ((wttrin--force-refresh t))
      (wttrin--mode-line-fetch-weather))))

(defun wttrin--mode-line-set-placeholder ()
  "Set a placeholder icon in the mode-line while waiting for weather data."
  (wttrin--set-mode-line-string
   (wttrin--make-emoji-icon "⏳")
   (format "Fetching weather for %s..."
           (or (wttrin--favorite-location-display-name) "favorite"))))

(defun wttrin--mode-line-refresh-now ()
  "Discard the cached mode-line weather and fetch fresh data immediately.
Called when `wttrin-favorite-location' changes so the mode-line stops
showing the previous location's weather instead of waiting for the next
scheduled refresh."
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-rendered-stale nil)
  (wttrin--mode-line-set-placeholder)
  (wttrin--mode-line-fetch-weather))

(defvar wttrin--buffer-refresh-timer nil
  "Timer object for proactive buffer cache refresh.")

(defun wttrin--buffer-cache-refresh ()
  "Proactively refresh the buffer cache for `wttrin-favorite-location'.
Fetches fresh weather data and updates the buffer cache entry without
displaying anything.  This keeps buffer data fresh for when the user
opens the weather buffer.  When the favorite is set to t and
geolocation has not yet resolved, this call is a no-op."
  (let ((location (wttrin--resolve-favorite-location)))
    (when location
      (let ((cache-key (wttrin--make-cache-key location)))
        (wttrin-fetch-raw-string
         location
         (lambda (fresh-data &optional _error-msg)
           (when fresh-data
             (wttrin--cleanup-cache-if-needed)
             (puthash cache-key (cons (float-time) fresh-data) wttrin--cache))))))))

(defun wttrin--mode-line-start ()
  "Start mode-line weather display and refresh timer."
  (wttrin--debug-log "wttrin mode-line: Starting mode-line display (location=%s, interval=%s)"
                     wttrin-favorite-location
                     wttrin-mode-line-refresh-interval)
  (when wttrin-favorite-location
    ;; Trigger geolocation resolution in the background if needed; the
    ;; placeholder + scheduled fetch will pick up the resolved string
    ;; on the next tick.
    (wttrin--resolve-favorite-location)
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
    (completing-read "Location Name: "
                     (wttrin--completion-table (wttrin--completion-candidates))
                     nil nil
                     (when (= (length wttrin-default-locations) 1)
                       (car wttrin-default-locations)))))
  (wttrin--query-selection location))

(when wttrin-mode-line-auto-enable
  (wttrin-mode-line-mode 1))

(provide 'wttrin)
;;; wttrin.el ends here
