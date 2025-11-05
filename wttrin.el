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

;; Declare functions from wttrin-debug.el (loaded conditionally)
(declare-function wttrin--debug-mode-line-info "wttrin-debug")

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


(defcustom wttrin-cache-ttl 900  ; 15 minutes
  "Time to live for cached weather data in seconds."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-cache-max-entries 50
  "Maximum number of entries to keep in cache."
  :group 'wttrin
  :type 'integer)

(defcustom wttrin-mode-line-favorite-location nil
  "Favorite location to display weather for in the mode-line.
When nil, mode-line weather display is disabled.
Set to a location string (e.g., \"New Orleans, LA\") to enable.
The weather icon and tooltip will update automatically in the background."
  :group 'wttrin
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Location")))

(defcustom wttrin-mode-line-refresh-interval 900
  "Interval in seconds to refresh mode-line weather data.
Default is 900 seconds (15 minutes)."
  :group 'wttrin
  :type 'integer)

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
When enabled, weather for `wttrin-mode-line-favorite-location' will appear
in the mode-line automatically.  You can also manually toggle the mode-line
display with `wttrin-mode-line-mode'."
  :group 'wttrin
  :type 'boolean)

(defcustom wttrin-debug nil
  "Enable debug functions for troubleshooting wttrin behavior.
When non-nil, loads wttrin-debug.el which provides:
- Automatic mode-line diagnostic logging when wttrin runs
- Raw weather data saved to timestamped files in `temporary-file-directory'
- Interactive debug commands for troubleshooting

Set this to t BEFORE loading wttrin, typically in your init file:
  (setq wttrin-debug t)
  (require \\='wttrin)"
  :group 'wttrin
  :type 'boolean)

;; Load debug functions if enabled
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
;;;###autoload(put 'wttrin-mode-line-string 'risky-local-variable t)
(put 'wttrin-mode-line-string 'risky-local-variable t)

(defvar wttrin--mode-line-timer nil
  "Timer object for mode-line weather refresh.")

(defvar wttrin--mode-line-tooltip-data nil
  "Cached full weather data for tooltip display.")

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

(defun wttrin-fetch-raw-string (query callback)
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
    (wttrin-query new-location)))

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
      (insert raw-string))
    (message "Debug data saved to: %s" filepath)
    filepath))

(defun wttrin--display-weather (location-name raw-string)
  "Display weather data RAW-STRING for LOCATION-NAME in weather buffer."
  ;; Save debug data if enabled
  (when wttrin-debug
    (wttrin--save-debug-data location-name raw-string))

  (if (or (null raw-string) (string-match "ERROR" raw-string))
      (message "Cannot retrieve weather data. Perhaps the location was misspelled?")
    (let ((buffer (get-buffer-create (format "*wttr.in*"))))
      (switch-to-buffer buffer)

      ;; Enable wttrin-mode first (calls kill-all-local-variables)
      ;; This must be done before setting any buffer-local variables
      (wttrin-mode)

      ;; Temporarily allow editing
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Initialize xterm-color state AFTER wttrin-mode to prevent it being wiped
        (setq-local xterm-color--state :char)
        (insert (xterm-color-filter raw-string))

        ;; Remove verbose Location: coordinate line
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*Location:.*\\[.*\\].*$" nil t)
          (delete-region (line-beginning-position) (1+ (line-end-position))))

        ;; Add user instructions at the bottom
        (goto-char (point-max))
        (insert "\n\nPress: [a] for another location [g] to refresh [q] to quit")

        ;; align buffer to top
        (goto-char (point-min)))

      ;; Set location after mode initialization (mode calls kill-all-local-variables)
      (setq-local wttrin--current-location location-name)

      ;; Auto-generate debug diagnostics if debug mode is enabled
      (when (featurep 'wttrin-debug)
        (wttrin--debug-mode-line-info)))))

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
     (lambda (raw-string)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (wttrin--display-weather location-name raw-string)))))))

(defun wttrin--make-cache-key (location)
  "Create cache key from LOCATION and current settings."
  (concat location "|" (or wttrin-unit-system "default")))

(defun wttrin--get-cached-or-fetch (location callback)
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
      (wttrin-fetch-raw-string
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
        (wttrin-query wttrin--current-location))
    (message "No location to refresh")))

;;; Mode-line weather display

(defun wttrin--mode-line-fetch-weather ()
  "Fetch weather for favorite location and update mode-line display.
Uses wttr.in custom format for concise weather with emoji."
  (when (featurep 'wttrin-debug)
    (message "wttrin mode-line: Fetching weather for %s" wttrin-mode-line-favorite-location))
  (when wttrin-mode-line-favorite-location
    (let* ((location wttrin-mode-line-favorite-location)
           ;; Custom format: location + emoji + temp + conditions
           ;; %l=location, %c=weather emoji, %t=temp, %C=conditions
           ;; Note: unit system must come BEFORE format in query string
           (format-params (if wttrin-unit-system
                              (concat "?" wttrin-unit-system "&format=%l:+%c+%t+%C")
                            "?format=%l:+%c+%t+%C"))
           (url (concat "https://wttr.in/"
                       (url-hexify-string location)
                       format-params))
           (url-request-extra-headers (list wttrin-default-languages))
           (url-user-agent "curl"))
      (when (featurep 'wttrin-debug)
        (message "wttrin mode-line: URL = %s" url))
      (url-retrieve
       url
       (lambda (status)
         (let ((data nil))
           (condition-case err
               (if (plist-get status :error)
                   (progn
                     (message "wttrin mode-line: Network error - %s"
                             (cdr (plist-get status :error)))
                     (setq data nil))
                 (unwind-protect
                     (progn
                       ;; Skip HTTP headers
                       (goto-char (point-min))
                       (re-search-forward "\r?\n\r?\n" nil t)
                       (setq data (string-trim
                                  (decode-coding-string
                                   (buffer-substring-no-properties (point) (point-max))
                                   'utf-8)))
                       (when (featurep 'wttrin-debug)
                         (message "wttrin mode-line: Received data = %S" data)))
                   (kill-buffer (current-buffer))))
             (error
              (message "wttrin mode-line: Error - %s" (error-message-string err))
              (setq data nil)))
           (when data
             (wttrin--mode-line-update-display data))))))))

(defun wttrin--mode-line-update-display (weather-string)
  "Update mode-line display with WEATHER-STRING.
Extracts emoji for mode-line, stores full info for tooltip.
WEATHER-STRING format: \"Location: emoji temp conditions\" (e.g., \"Paris: ☀️ +61°F Clear\")."
  (when (featurep 'wttrin-debug)
    (message "wttrin mode-line: Updating display with: %S" weather-string))
  ;; Store full weather info for tooltip
  (setq wttrin--mode-line-tooltip-data weather-string)
  ;; Extract just the emoji for mode-line display
  ;; Format is "Location: emoji +temp conditions"
  ;; We want just the emoji (first character after ": ")
  (let* ((emoji (if (string-match ":\\s-*\\(.\\)" weather-string)
                    (match-string 1 weather-string)
                  "?"))  ; Fallback if parsing fails
         ;; Force color emoji rendering by setting font family
         (emoji-with-font (if wttrin-mode-line-emoji-font
                              (propertize emoji
                                          'face (list :family wttrin-mode-line-emoji-font
                                                      :height 1.0))
                            emoji)))
    (setq wttrin-mode-line-string
          (propertize (concat " " emoji-with-font)
                      'help-echo (lambda (_window _object _pos)
                                   (or wttrin--mode-line-tooltip-data
                                       (format "Weather for %s\nClick to refresh"
                                               wttrin-mode-line-favorite-location)))
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1]
                                     'wttrin-mode-line-click)
                                   (define-key map [mode-line mouse-3]
                                     'wttrin-mode-line-force-refresh)
                                   map))))
  (force-mode-line-update t)
  (when (featurep 'wttrin-debug)
    (message "wttrin mode-line: Display updated, mode-line-string = %S, tooltip = %S"
             wttrin-mode-line-string wttrin--mode-line-tooltip-data)))

(defun wttrin-mode-line-click ()
  "Handle left-click on mode-line weather widget.
Check cache, refresh if needed, then open weather buffer."
  (interactive)
  (when wttrin-mode-line-favorite-location
    (wttrin wttrin-mode-line-favorite-location)))

(defun wttrin-mode-line-force-refresh ()
  "Handle right-click on mode-line weather widget.
Force-refresh cache and update tooltip without opening buffer."
  (interactive)
  (when wttrin-mode-line-favorite-location
    (let ((wttrin--force-refresh t))
      (wttrin--mode-line-fetch-weather))))

(defun wttrin--mode-line-start ()
  "Start mode-line weather display and refresh timer."
  (when (featurep 'wttrin-debug)
    (message "wttrin mode-line: Starting mode-line display (location=%s, interval=%s)"
             wttrin-mode-line-favorite-location
             wttrin-mode-line-refresh-interval))
  (when wttrin-mode-line-favorite-location
    ;; Delay initial fetch by 3 seconds to allow network to initialize during startup
    (run-at-time 3 nil #'wttrin--mode-line-fetch-weather)
    ;; Set up refresh timer (starts after the interval from now)
    (when wttrin--mode-line-timer
      (cancel-timer wttrin--mode-line-timer))
    (setq wttrin--mode-line-timer
          (run-at-time wttrin-mode-line-refresh-interval
                      wttrin-mode-line-refresh-interval
                      #'wttrin--mode-line-fetch-weather))
    (when (featurep 'wttrin-debug)
      (message "wttrin mode-line: Initial fetch scheduled in 3 seconds, then every %s seconds"
               wttrin-mode-line-refresh-interval))))

(defun wttrin--mode-line-stop ()
  "Stop mode-line weather display and cancel timer."
  (when (featurep 'wttrin-debug)
    (message "wttrin mode-line: Stopping mode-line display"))
  (when wttrin--mode-line-timer
    (cancel-timer wttrin--mode-line-timer)
    (setq wttrin--mode-line-timer nil))
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-tooltip-data nil)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode wttrin-mode-line-mode
  "Toggle weather display in mode-line.
When enabled, shows weather for `wttrin-mode-line-favorite-location'."
  :global t
  :lighter (:eval wttrin-mode-line-string)
  (if wttrin-mode-line-mode
      (progn
        (when (featurep 'wttrin-debug)
          (message "wttrin mode-line: Mode enabled"))
        (wttrin--mode-line-start)
        ;; Add modeline string to global-mode-string for custom modelines
        (if global-mode-string
            (add-to-list 'global-mode-string 'wttrin-mode-line-string 'append)
          (setq global-mode-string '("" wttrin-mode-line-string)))
        (when (featurep 'wttrin-debug)
          (message "wttrin mode-line: Added to global-mode-string = %S" global-mode-string)))
    (when (featurep 'wttrin-debug)
      (message "wttrin mode-line: Mode disabled"))
    (wttrin--mode-line-stop)
    ;; Remove from global-mode-string
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

;; Auto-enable mode-line display if requested
(when wttrin-mode-line-auto-enable
  (wttrin-mode-line-mode 1))

(provide 'wttrin)
;;; wttrin.el ends here
