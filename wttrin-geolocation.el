;;; wttrin-geolocation.el --- IP geolocation for wttrin -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Copyright (C) 2026 Craig Jennings
;; Maintainer: Craig Jennings <c@cjennings.net>
;; Version: 0.3.2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: weather, wttrin

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

;; IP-based geolocation support for wttrin.  Three built-in providers ship
;; with the package: ipapi.co (default), ipinfo.io, and ipwho.is.  All three
;; are HTTPS, keyless, and have generous free tiers.
;;
;; Users select a provider via `wttrin-geolocation-provider' or register
;; custom providers by pushing onto `wttrin-geolocation--providers'.
;;
;; Each provider is a plist with keys:
;;   :name    - Human-readable name (string)
;;   :url     - JSON endpoint (string)
;;   :parser  - Function symbol taking a JSON string and returning
;;              "City, Region" or nil on any failure.
;;
;; The public entry point is `wttrin-geolocation-detect', which fetches
;; asynchronously and invokes a callback with the parsed location string
;; or nil on failure.

;;; Code:

(require 'json)
(require 'url)
;; For the shared error hierarchy (`wttrin-invalid-input' et al.).  This is a
;; sub-module of wttrin and is only ever loaded through it, so the require is a
;; no-op in practice; it makes the dependency explicit and keeps the condition
;; symbols defined even if this file is loaded on its own.
(require 'wttrin)

(defgroup wttrin-geolocation nil
  "IP geolocation settings for wttrin."
  :prefix "wttrin-geolocation-"
  :group 'wttrin)

(defcustom wttrin-geolocation-provider 'ipapi
  "Provider used by `wttrin-geolocation-detect'.
The value is a key into `wttrin-geolocation--providers'.  Three
providers ship with the package: `ipapi' (ipapi.co, the default),
`ipinfo' (ipinfo.io), and `ipwhois' (ipwho.is).  Users who register
additional providers by pushing onto `wttrin-geolocation--providers'
can select them here."
  :group 'wttrin-geolocation
  :type '(choice (const :tag "ipapi.co" ipapi)
                 (const :tag "ipinfo.io" ipinfo)
                 (const :tag "ipwho.is" ipwhois)
                 (symbol :tag "Other (registered in wttrin-geolocation--providers)")))

(defcustom wttrin-geolocation-command nil
  "Optional shell command for higher-accuracy geolocation.
When non-nil, `wttrin-geolocation-detect' runs this command and reads its
standard output as JSON, expecting numeric `lat' and `lng' keys (any other
keys are ignored).  The detected location resolves to \"LAT,LNG\", which
wttr.in accepts and echoes a place name for.

This is the opt-in accuracy path.  The command may do whatever the system
supports (a WiFi scan, a GPS read) to beat IP geolocation, and it runs
asynchronously so a multi-second lookup does not block Emacs.  The package
ships no command and assumes nothing about the OS or network stack, so it is
inert until set.  Ready-to-adapt example commands ship under the package's
examples/geolocation/ directory.

On any failure (the command is unset, exits non-zero, or prints no parseable
lat/lng), wttrin falls back to the IP provider named by
`wttrin-geolocation-provider'."
  :group 'wttrin-geolocation
  :type '(choice (const :tag "None (use IP provider)" nil)
                 (string :tag "Shell command")))

;;; Response Parsers
;;
;; Each parser takes a raw JSON string and returns "City, Region" or nil.
;; They differ only in which quirks of the upstream response they have to
;; handle (error flags, success flags) before extracting city and region.

(defun wttrin-geolocation--decode-json (json-string)
  "Parse JSON-STRING into an alist.
Return nil for a nil input, an empty string, or malformed JSON."
  (when (and (stringp json-string) (> (length json-string) 0))
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string json-string))
      (error nil))))

(defun wttrin-geolocation--format-city-region (data)
  "Return \"City, Region\" built from DATA alist.
Return nil if either the city or the region field is missing or empty."
  (let ((city (cdr (assq 'city data)))
        (region (cdr (assq 'region data))))
    (when (and (stringp city) (> (length city) 0)
               (stringp region) (> (length region) 0))
      (format "%s, %s" city region))))

(defun wttrin-geolocation--parse-ipapi (json-string)
  "Parse an ipapi.co JSON response into a \"City, Region\" string.
Return nil on malformed JSON, missing city or region, or an ipapi
error response (which carries an \"error\": true field on rate-limit
or other failures)."
  (let ((data (wttrin-geolocation--decode-json json-string)))
    (when (and data (not (eq t (cdr (assq 'error data)))))
      (wttrin-geolocation--format-city-region data))))

(defun wttrin-geolocation--parse-ipinfo (json-string)
  "Parse an ipinfo.io JSON response into a \"City, Region\" string.
Return nil on malformed JSON or missing city or region."
  (let ((data (wttrin-geolocation--decode-json json-string)))
    (when data
      (wttrin-geolocation--format-city-region data))))

(defun wttrin-geolocation--parse-ipwhois (json-string)
  "Parse an ipwho.is JSON response into a \"City, Region\" string.
Return nil on malformed JSON, missing city or region, or a
`success: false' response (which ipwho.is uses to signal rate-limit
or lookup failure even when the HTTP response itself is 200)."
  (let ((data (wttrin-geolocation--decode-json json-string)))
    (when (and data (eq t (cdr (assq 'success data))))
      (wttrin-geolocation--format-city-region data))))

;;; Provider Registry

(defvar wttrin-geolocation--providers
  '((ipapi
     :name "ipapi.co"
     :url "https://ipapi.co/json/"
     :parser wttrin-geolocation--parse-ipapi)
    (ipinfo
     :name "ipinfo.io"
     :url "https://ipinfo.io/json"
     :parser wttrin-geolocation--parse-ipinfo)
    (ipwhois
     :name "ipwho.is"
     :url "https://ipwho.is/"
     :parser wttrin-geolocation--parse-ipwhois))
  "Alist of geolocation providers keyed by symbol.
Each entry is a plist with keys :name, :url, and :parser.  The
parser is a function symbol taking a JSON string and returning
\"City, Region\" or nil.  Users may register additional providers
by pushing onto this list; the keys become valid values for
`wttrin-geolocation-provider'.")

(defun wttrin-geolocation--lookup-provider (symbol)
  "Return the provider plist for SYMBOL.
Signal an error if SYMBOL is not registered."
  (or (cdr (assq symbol wttrin-geolocation--providers))
      (signal 'wttrin-invalid-input
              (list (format "Unknown wttrin-geolocation provider: %S" symbol)))))

;;; Fetch and Detect

(defun wttrin-geolocation--extract-body ()
  "Return the UTF-8 decoded HTTP body from the current buffer.
Return nil for non-2xx status codes or when no body separator is
found.  Intended for use inside a `url-retrieve' callback."
  (goto-char (point-min))
  (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (let ((status (string-to-number (match-string 1))))
      (when (and (>= status 200) (< status 300))
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))))))

(defun wttrin-geolocation--parse-coordinates (json-string)
  "Parse JSON-STRING for numeric `lat'/`lng'; return \"LAT,LNG\" or nil.
Any keys beyond `lat' and `lng' are ignored.  Returns nil on malformed
JSON or missing/non-numeric coordinates.  wttr.in accepts the \"LAT,LNG\"
form directly and echoes a place name for it."
  (let ((data (wttrin-geolocation--decode-json json-string)))
    (when data
      (let ((lat (cdr (assq 'lat data)))
            (lng (cdr (assq 'lng data))))
        (when (and (numberp lat) (numberp lng))
          (format "%s,%s" lat lng))))))

(defun wttrin-geolocation--parse-address (json-string)
  "Return the `address' (or `label') string from JSON-STRING, or nil.
A command may include a human-readable place name alongside its
coordinates; wttrin shows it in the weather buffer.  Returns nil on
malformed JSON or a missing or empty value."
  (let ((data (wttrin-geolocation--decode-json json-string)))
    (when data
      (let ((address (or (cdr (assq 'address data))
                         (cdr (assq 'label data)))))
        (when (and (stringp address) (> (length address) 0))
          address)))))

(defun wttrin-geolocation--detect-via-ip (callback)
  "Detect location via the IP provider; invoke CALLBACK with the result.
CALLBACK receives a \"City, Region\" string on success or nil on failure.
The provider is selected by `wttrin-geolocation-provider'."
  (let* ((provider (wttrin-geolocation--lookup-provider
                    wttrin-geolocation-provider))
         (url (plist-get provider :url))
         (parser (plist-get provider :parser)))
    (url-retrieve
     url
     (lambda (status)
       (let ((result nil))
         (unless (plist-get status :error)
           (condition-case nil
               (let ((body (wttrin-geolocation--extract-body)))
                 (setq result (and body (funcall parser body))))
             (error nil)))
         (ignore-errors (kill-buffer (current-buffer)))
         (funcall callback result))))))

(defun wttrin-geolocation--detect-via-command (callback)
  "Run `wttrin-geolocation-command' asynchronously; invoke CALLBACK with result.
CALLBACK is called with (COORDS ADDRESS): COORDS is a \"LAT,LNG\" string when
the command exits zero and prints JSON with numeric `lat'/`lng', and ADDRESS
is the optional human-readable place name from the JSON (nil when absent).
On any failure (spawn error, non-zero exit, or unparseable output) CALLBACK
is called with nil."
  (let ((output ""))
    (condition-case nil
        (make-process
         :name "wttrin-geolocation"
         :command (list shell-file-name shell-command-switch
                        wttrin-geolocation-command)
         :connection-type 'pipe
         :noquery t
         :filter (lambda (_proc chunk) (setq output (concat output chunk)))
         :sentinel (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (let* ((ok (eq (process-exit-status proc) 0))
                              (coords (and ok (wttrin-geolocation--parse-coordinates
                                               output)))
                              (address (and coords
                                            (wttrin-geolocation--parse-address
                                             output))))
                         (funcall callback coords address)))))
      (error (funcall callback nil)))))

(defun wttrin-geolocation-detect (callback)
  "Detect current location; invoke CALLBACK asynchronously with the result.
CALLBACK is called with (LOCATION &optional ADDRESS).  LOCATION is a location
string (\"City, Region\" from an IP provider, or \"LAT,LNG\" from a command) on
success, or nil on any failure.  ADDRESS is the optional human-readable place
name a command may supply alongside coordinates (always nil on the IP path).
Callers that only need the location may accept a single argument.

When `wttrin-geolocation-command' is non-nil, run it first; on success its
coordinates (and address, if any) are used, and on failure detection falls back
to the IP provider named by `wttrin-geolocation-provider'.  When no command is
set, the IP provider is used directly.  Signals an error synchronously if
`wttrin-geolocation-provider' is not registered and the IP path is reached."
  (if wttrin-geolocation-command
      (wttrin-geolocation--detect-via-command
       (lambda (coords &optional address)
         (if coords
             (funcall callback coords address)
           (wttrin-geolocation--detect-via-ip callback))))
    (wttrin-geolocation--detect-via-ip callback)))

(provide 'wttrin-geolocation)
;;; wttrin-geolocation.el ends here
