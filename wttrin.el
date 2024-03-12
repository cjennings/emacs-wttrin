;;; wttrin.el --- Emacs Frontend for Weather WWeb Service wttr.in -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Craig Jennings
;; Maintainer: Craig Jennings <c@cjennings.net>
;;
;; Original Authors: Carl X. Su <bcbcarl@gmail.com>
;;                   ono hiroko (kuanyui) <azazabc123@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (xterm-color "1.0"))
;; Keywords: weather, wttrin
;; URL: https://github.com/cjennings/emacs-wttrin

;;; Commentary:
;; Provides the weather information from wttr.in based on your query condition.

;;; Code:

(require 'url)
(require 'xterm-color)

(defgroup wttrin nil
  "Emacs frontend for weather web service wttr.in."
  :prefix "wttrin-"
  :group 'comm)

(defcustom wttrin-default-cities '("Berkeley" "New Orleans" "New York" "London" "Taipei")
  "Specify default cities list for quick completion."
  :group 'wttrin
  :type 'list)

(defcustom wttrin-default-accept-language '("Accept-Language" . "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4")
  "Specify default HTTP request Header for Accept-Language."
  :group 'wttrin
  :type '(list))

(defcustom wttrin-unit-system nil
  "Specify units of measurement.
Use 'm' for 'metric', 'u' for 'USCS, or nil for location based units (default)."
  :group 'wttrin
  :type 'string)

(defun wttrin-additional-url-params ()
  "Concatenates extra information into the URL."
  (concat "?" wttrin-unit-system))

(defun wttrin-fetch-raw-string (query) "Get the weather information based on your QUERY."
	   (let ((url-user-agent "curl"))
	 (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
	 (with-current-buffer
		 (url-retrieve-synchronously
		  (concat "http://wttr.in/" query "?A")
		  (lambda (status) (switch-to-buffer (current-buffer))))
	   (decode-coding-string (buffer-string) 'utf-8))))

(defun wttrin-exit ()
  "Exit the wttrin buffer."
  (interactive)
  (quit-window t))

(defun wttrin-query (city-name)
  "Query weather of CITY-NAME via wttrin, and display the result in new buffer."
  (let ((raw-string (wttrin-fetch-raw-string city-name)))
    (if (string-match "ERROR" raw-string)
		(message "Cannot retrieve weather data. Perhaps the city name was misspelled?")
      (let ((buffer (get-buffer-create (format "*wttr.in - %s*" city-name))))
        (switch-to-buffer buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (xterm-color-filter raw-string))
        (goto-char (point-min))
        (re-search-forward "^$")
		(delete-region (point-min) (1+ (point)))
		(goto-char (point-max))
		(insert "\n")
		(insert "Press 'g' for another city's weather, 'q' to quit.")
        (use-local-map (make-sparse-keymap))
        (local-set-key "q" 'wttrin-exit)
		(local-set-key "g" 'wttrin)
		(setq buffer-read-only t)))))

;;;###autoload
(defun wttrin (city)
  "Display weather information for CITY."
  (interactive
   (list
	(completing-read "City Name: " wttrin-default-cities nil nil
                     (when (= (length wttrin-default-cities) 1)
                       (car wttrin-default-cities)))))
  (wttrin-query city))

(provide 'wttrin)
;;; wttrin.el ends here
