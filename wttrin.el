;;; wttrin.el --- Emacs Frontend for Service wttr.in -*- lexical-binding: t; -*-
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

(defun wttrin-additional-url-params ()
  "Concatenates extra information into the URL."
  (concat "?" wttrin-unit-system))

(defun wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
	(add-to-list 'url-request-extra-headers wttrin-default-languages)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query "?A")
         (lambda () (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

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

(defun wttrin-query (location-name)
  "Query weather of LOCATION-NAME via wttrin, display the result in new buffer."
  (let ((raw-string (wttrin-fetch-raw-string location-name)))
	(if (string-match "ERROR" raw-string)
		(message "Cannot retrieve weather data. Perhaps the location was
misspelled?")
	  (let ((buffer (get-buffer-create (format "*wttr.in*")))
			date-time-stamp location-info)
		(switch-to-buffer buffer)
		(setq buffer-read-only nil)
		(erase-buffer)

		;; Enable `truncate-lines` to avoid garbling the output in small windows
		(setq truncate-lines t)

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
		(insert "\nPress: [g] to query another location\n")
		(insert "       [q] to quit\n")

		;; align buffer to top
		(goto-char (point-min))

		;; create choice keymap and disallow modifying buffer
		(use-local-map (make-sparse-keymap))
		(local-set-key "q" 'wttrin-exit)
		(local-set-key "g" 'wttrin-requery)
		(setq buffer-read-only t)))))

;;;###autoload
(defun wttrin (location)
  "Display weather information for LOCATION."
  (interactive
   (list
    (completing-read "Location Name: " wttrin-default-locations nil nil
                     (when (= (length wttrin-default-locations) 1)
                       (car wttrin-default-locations)))))
  (wttrin-query location))

(provide 'wttrin)
;;; wttrin.el ends here
