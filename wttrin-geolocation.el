;;; wttrin-geolocation.el --- IP geolocation for wttrin -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Copyright (C) 2026 Craig Jennings
;; Maintainer: Craig Jennings <c@cjennings.net>
;; Version: 0.3.1
;; Package-Requires: ((emacs "24.4"))
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
      (error "Unknown wttrin-geolocation provider: %S" symbol)))

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

(defun wttrin-geolocation-detect (callback)
  "Detect current location via the configured geolocation provider.
CALLBACK is invoked asynchronously with a single argument: a
\"City, Region\" string on success, or nil on any failure (network
error, HTTP 4xx or 5xx, malformed response, missing fields, or
provider-specific rate-limit signals).

The provider is selected by `wttrin-geolocation-provider'.  Signals
an error synchronously if that value is not registered in
`wttrin-geolocation--providers'."
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

(provide 'wttrin-geolocation)
;;; wttrin-geolocation.el ends here
