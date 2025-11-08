;;; wttrin-debug.el --- Debug functions for wttrin.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings
;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: debug weather wttrin

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file contains debug functions for troubleshooting wttrin.el behavior.
;; It is only loaded when `wttrin-debug' is non-nil.
;;
;; Enable with:
;;   (setq wttrin-debug t)
;;   (require 'wttrin)
;;
;; Available debug functions:
;; - `debug-wttrin-show-raw' - View raw weather data with line numbers
;; - `debug-wttrin-mode-line' - Diagnose mode-line lighter issues
;; - `wttrin--debug-mode-line-info' - Auto-called when wttrin runs (if debug enabled)
;;
;; Interactive commands:
;; - M-x debug-wttrin-enable  - Enable debug mode
;; - M-x debug-wttrin-disable - Disable debug mode
;;
;; When debug mode is enabled, raw weather data is automatically saved to
;; timestamped files in `temporary-file-directory' for bug reports.

;;; Code:

;; wttrin-debug.el is loaded by wttrin.el, so wttrin is already loaded
;; No need for (require 'wttrin) here

;; Declare variables and functions from wttrin.el
(defvar wttrin-debug)
(declare-function wttrin--get-cached-or-fetch "wttrin")

;;;###autoload
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

;;;###autoload
(defun debug-wttrin-enable ()
  "Enable wttrin debug mode.
Raw weather data will be saved to timestamped files for bug reports."
  (interactive)
  (setq wttrin-debug t)
  (message "Wttrin debug mode enabled. Raw data will be saved to: %s" temporary-file-directory))

;;;###autoload
(defun debug-wttrin-disable ()
  "Disable wttrin debug mode."
  (interactive)
  (setq wttrin-debug nil)
  (message "Wttrin debug mode disabled"))

;;;###autoload
(defun debug-wttrin-mode-line ()
  "Display detailed mode-line information for the wttrin buffer.
This is useful for diagnosing why the mode-line lighter isn't appearing."
  (interactive)
  (if-let ((buf (get-buffer "*wttr.in*")))
      (with-current-buffer buf
        (let* ((has-custom-modeline (boundp 'cj/modeline-major-mode))
               (formatted-mode (when has-custom-modeline
                                 (format-mode-line mode-name))))
          (with-output-to-temp-buffer "*wttrin-mode-debug*"
            (princ (format "=== Wttrin Mode-Line Debug Info ===\n\n"))
            (princ (format "Buffer: %s\n" (buffer-name)))
            (princ (format "Major mode: %s\n" major-mode))
            (princ (format "mode-name variable: %S\n" mode-name))
            (princ (format "mode-name type: %s\n" (type-of mode-name)))
            (princ (format "\nCustom modeline detected: %s\n" has-custom-modeline))
            (when has-custom-modeline
              (princ (format "format-mode-line result: %S\n" formatted-mode)))
            (princ (format "\nmode-line-format first 5 elements:\n"))
            (let ((i 0))
              (dolist (elem mode-line-format)
                (when (< i 5)
                  (princ (format "  [%d] %S\n" i elem))
                  (setq i (1+ i)))))
            (princ (format "\nSpecial-mode parent: %s\n"
                          (get 'wttrin-mode 'derived-mode-parent)))
            (princ (format "Is special-mode active: %s\n"
                          (derived-mode-p 'special-mode))))))
    (message "No *wttr.in* buffer exists. Run M-x wttrin first.")))

(defun wttrin--debug-mode-line-info ()
  "Auto-generate mode-line diagnostic information.
This function is called automatically when wttrin runs if debug mode is enabled.
It creates the *wttrin-mode-debug* buffer with diagnostic information."
  (debug-wttrin-mode-line))

(defvar wttrin--debug-log nil
  "List of debug log entries. Each entry is (timestamp . message).")

(defun wttrin--debug-log (format-string &rest args)
  "Log a debug message if wttrin-debug is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when wttrin-debug
    (let ((msg (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (push (cons timestamp msg) wttrin--debug-log)
      (message "[wttrin-debug %s] %s" timestamp msg))))

;;;###autoload
(defun wttrin-debug-clear-log ()
  "Clear the debug log."
  (interactive)
  (setq wttrin--debug-log nil)
  (message "Wttrin debug log cleared"))

;;;###autoload
(defun wttrin-debug-show-log ()
  "Display the wttrin debug log in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*wttrin-debug-log*")
    (erase-buffer)
    (insert "=== WTTRIN DEBUG LOG ===\n")
    (insert (format "Total entries: %d\n\n" (length wttrin--debug-log)))
    (if wttrin--debug-log
        (dolist (entry (reverse wttrin--debug-log))
          (insert (format "[%s] %s\n" (car entry) (cdr entry))))
      (insert "(No log entries yet)\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'wttrin-debug)
;;; wttrin-debug.el ends here
