;;; test-wttrin--mode-line-update-display.el --- Tests for mode-line display update -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--mode-line-update-display and
;; wttrin--mode-line-valid-response-p.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-update-display-setup ()
  "Setup for mode-line update display tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-tooltip-data nil))

(defun test-wttrin--mode-line-update-display-teardown ()
  "Teardown for mode-line update display tests."
  (testutil-wttrin-teardown)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-tooltip-data nil))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-valid-response-p
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-valid-response-p-normal-standard-response ()
  "Valid response with location, emoji, temp, and conditions."
  (should (wttrin--mode-line-valid-response-p "Paris: ☀️ +61°F Clear")))

(ert-deftest test-wttrin--mode-line-valid-response-p-normal-minimal-with-colon ()
  "Minimal valid response containing a colon."
  (should (wttrin--mode-line-valid-response-p "X: Y")))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-valid-response-p-boundary-colon-only ()
  "Response that is just a colon is technically valid (has expected delimiter)."
  (should (wttrin--mode-line-valid-response-p ":")))

;;; Error Cases

(ert-deftest test-wttrin--mode-line-valid-response-p-error-nil ()
  "Nil input is invalid."
  (should-not (wttrin--mode-line-valid-response-p nil)))

(ert-deftest test-wttrin--mode-line-valid-response-p-error-empty-string ()
  "Empty string is invalid."
  (should-not (wttrin--mode-line-valid-response-p "")))

(ert-deftest test-wttrin--mode-line-valid-response-p-error-no-colon ()
  "String without colon is invalid (doesn't match expected format)."
  (should-not (wttrin--mode-line-valid-response-p "no colon here")))

(ert-deftest test-wttrin--mode-line-valid-response-p-error-not-a-string ()
  "Non-string input is invalid."
  (should-not (wttrin--mode-line-valid-response-p 42)))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-update-display
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-update-display-normal-sets-mode-line-string ()
  "Display update sets wttrin-mode-line-string to non-nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (should wttrin-mode-line-string))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-stores-tooltip-data ()
  "Display update stores weather string as tooltip data."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (should (equal wttrin--mode-line-tooltip-data "Paris: ☀️ +61°F Clear")))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-extracts-emoji ()
  "Display update extracts emoji character into mode-line string."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-emoji-font nil))
        (wttrin--mode-line-update-display "Paris: X +61°F Clear")
        ;; Mode-line string should contain the extracted character
        (should (string-match-p "X" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-has-help-echo ()
  "Display update sets help-echo property for tooltip."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (should (get-text-property 0 'help-echo wttrin-mode-line-string)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-has-local-map ()
  "Display update sets local-map property for mouse interaction."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (should (eq (get-text-property 0 'local-map wttrin-mode-line-string)
                    wttrin--mode-line-map)))
    (test-wttrin--mode-line-update-display-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-update-display-boundary-no-emoji-match-uses-fallback ()
  "When emoji regex doesn't match, fallback character '?' is used."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-emoji-font nil))
        (wttrin--mode-line-update-display "no colon here")
        (should (string-match-p "\\?" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; Tooltip Lambda Tests

(ert-deftest test-wttrin--mode-line-update-display-normal-tooltip-returns-weather-data ()
  "Tooltip lambda returns weather data when available."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (let ((tooltip-fn (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (equal (funcall tooltip-fn nil nil nil) "Paris: ☀️ +61°F Clear"))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-boundary-tooltip-empty-string-uses-fallback ()
  "Tooltip lambda falls back when tooltip data is empty string."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        ;; Simulate empty tooltip data (as would happen with bad response)
        (setq wttrin--mode-line-tooltip-data "")
        (let ((tooltip-fn (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (string-match-p "Weather for Paris" (funcall tooltip-fn nil nil nil)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-boundary-tooltip-nil-uses-fallback ()
  "Tooltip lambda falls back when tooltip data is nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (setq wttrin--mode-line-tooltip-data nil)
        (let ((tooltip-fn (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (string-match-p "Weather for Paris" (funcall tooltip-fn nil nil nil)))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-fetch-weather (validation integration)
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--mode-line-fetch-weather-error-empty-response-keeps-previous ()
  "Empty API response does not overwrite previous valid display."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        ;; Set up a valid prior state
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (let ((previous-string wttrin-mode-line-string)
              (previous-tooltip wttrin--mode-line-tooltip-data))
          ;; Simulate fetch returning empty response
          (testutil-wttrin-mock-http-response ""
            (wttrin--mode-line-fetch-weather)
            ;; Previous state should be preserved
            (should (equal wttrin-mode-line-string previous-string))
            (should (equal wttrin--mode-line-tooltip-data previous-tooltip)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-error-no-colon-response-keeps-previous ()
  "Malformed API response without colon does not overwrite previous valid display."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-update-display "Paris: ☀️ +61°F Clear")
        (let ((previous-string wttrin-mode-line-string)
              (previous-tooltip wttrin--mode-line-tooltip-data))
          (testutil-wttrin-mock-http-response "Unknown location"
            (wttrin--mode-line-fetch-weather)
            (should (equal wttrin-mode-line-string previous-string))
            (should (equal wttrin--mode-line-tooltip-data previous-tooltip)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-error-nil-location-does-not-fetch ()
  "No fetch is attempted when wttrin-favorite-location is nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (fetch-called nil))
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (_url _callback) (setq fetch-called t))))
          (wttrin--mode-line-fetch-weather)
          (should-not fetch-called)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-normal-valid-response-updates-display ()
  "Valid API response updates the mode-line display."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (testutil-wttrin-mock-http-response "Paris: ☀️ +61°F Clear"
          (wttrin--mode-line-fetch-weather)
          (should wttrin-mode-line-string)
          (should (equal wttrin--mode-line-tooltip-data "Paris: ☀️ +61°F Clear"))))
    (test-wttrin--mode-line-update-display-teardown)))

(provide 'test-wttrin--mode-line-update-display)
;;; test-wttrin--mode-line-update-display.el ends here
