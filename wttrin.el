;;; wttrin.el --- Emacs Frontend for the Weather Web Service wttr.in -*- lexical-binding: t; -*-
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

;;; Commentary:
;; Displays the weather information from the wttr.in service for your submitted location.

;;; Code:

(require 'url)
(require 'xterm-color) ;; https://github.com/atomontage/xterm-color

(defgroup wttrin nil
  "Emacs frontend for the weather web service wttr.in."
  :prefix "wttrin-"
  :group 'comm)

(defcustom wttrin-font-name "Lucida Console"
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

(defcustom wttrin-default-cities '("Berkeley, California"
                                   "New Orleans, Louisiana"
                                   "New York, New York"
                                   "London, England"
                                   "Paris, France"
                                   "Berlin, Germany"
                                   "Naples, Italy"
                                   "Athens, Greece"
                                   "Kyiv, Ukraine"
                                   "Tainan, Taiwan"
                                   "Taipei, Taiwan")
  "Specify default cities list for quick completion."
  :group 'wttrin
  :type '(repeat string))

(defcustom wttrin-default-accept-language '("Accept-Language" . "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4")
  "Specify default HTTP request Header for Accept-Language."
  :group 'wttrin
  :type '(cons (string :tag "Header") (string :tag "Language codes")))

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

(defun wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query "?A")
		 (lambda () (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

(defun wttrin-exit ()
  "Exit the wttrin buffer."
  (interactive)
  (quit-window t))

(defun wttrin-restart ()
  "Kill buffer and restart wttrin."
  (interactive)
  (kill-buffer)
  (call-interactively 'wttrin))

(defun wttrin-query (city-name)
  "Query weather of CITY-NAME via wttrin, and display the result in new buffer."
  (let ((raw-string (wttrin-fetch-raw-string city-name)))
    (if (string-match "ERROR" raw-string)
        (message "Cannot retrieve weather data. Perhaps the location was misspelled?")
      (let ((buffer (get-buffer-create (format "*wttr.in - %s*" city-name))))
        (switch-to-buffer buffer)
        (setq buffer-read-only nil)
        (erase-buffer)

        ;; set the preferred font attributes for this buffer only
        (setq buffer-face-mode-face `(:family  ,wttrin-font-name :height  ,wttrin-font-height))
        (buffer-face-mode t)

        ;; insert weather data
        (insert (xterm-color-filter raw-string))

        ;; remove all info lines except date and location
        (goto-char (point-min))
        (kill-whole-line 4)
        (forward-line)
        (kill-whole-line)

        ;; provide user instructions
        (goto-char (point-max))
        (insert "\n")
        (insert "Press 'g' for another location's weather, 'q' to quit.")

        ;; align buffer to top
        (goto-char (point-min))

        ;; create choice keymap and disallow modifying buffer
        (use-local-map (make-sparse-keymap))
        (local-set-key "q" 'wttrin-exit)
        (local-set-key "g" 'wttrin-restart)
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
