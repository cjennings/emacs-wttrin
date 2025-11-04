;;; debug-wttrin.el --- Debug helper for wttrin -*- lexical-binding: t -*-
;;
;; This file provides utilities for debugging wttrin display issues.
;;
;;; Commentary:
;;
;; To enable debug mode:
;;   (setq wttrin-debug t)
;;
;; This will save raw weather responses to timestamped files in your
;; temp directory for bug reports.
;;
;; To view raw data with line numbers for development:
;;   M-x debug-wttrin-show-raw RET <location> RET
;;
;;; Code:

(require 'wttrin)

(defun debug-wttrin-show-raw (location)
  "Fetch and display raw wttr.in data for LOCATION with line numbers.
This is useful for debugging header parsing issues."
  (interactive "sLocation: ")
  (let ((raw-string (wttrin--get-cached-or-fetch location)))
    (with-current-buffer (get-buffer-create "*wttrin-debug*")
      (erase-buffer)
      (insert raw-string)
      (goto-char (point-min))
      (let ((line-num 1))
        (while (not (eobp))
          (beginning-of-line)
          (insert (format "%2d: " line-num))
          (setq line-num (1+ line-num))
          (forward-line 1)))
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defun debug-wttrin-enable ()
  "Enable wttrin debug mode.
Raw weather data will be saved to timestamped files for bug reports."
  (interactive)
  (setq wttrin-debug t)
  (message "Wttrin debug mode enabled. Raw data will be saved to: %s" temporary-file-directory))

(defun debug-wttrin-disable ()
  "Disable wttrin debug mode."
  (interactive)
  (setq wttrin-debug nil)
  (message "Wttrin debug mode disabled"))

(provide 'debug-wttrin)
;;; debug-wttrin.el ends here
