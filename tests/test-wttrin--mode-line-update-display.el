;;; test-wttrin--mode-line-update-display.el --- Tests for mode-line display update -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--mode-line-update-display,
;; wttrin--mode-line-valid-response-p, wttrin--mode-line-fetch-weather,
;; wttrin--mode-line-set-placeholder, and wttrin--mode-line-update-placeholder-error.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-update-display-setup ()
  "Setup for mode-line update display tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

(defun test-wttrin--mode-line-update-display-teardown ()
  "Teardown for mode-line update display tests."
  (testutil-wttrin-teardown)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

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
;;; wttrin--format-age
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--format-age-just-now ()
  "Seconds under 60 returns just now."
  (should (equal (wttrin--format-age 0) "just now"))
  (should (equal (wttrin--format-age 59) "just now")))

(ert-deftest test-wttrin--format-age-minutes ()
  "Seconds in the minutes range."
  (should (equal (wttrin--format-age 60) "1 minute ago"))
  (should (equal (wttrin--format-age 300) "5 minutes ago"))
  (should (equal (wttrin--format-age 3599) "59 minutes ago")))

(ert-deftest test-wttrin--format-age-hours ()
  "Seconds in the hours range."
  (should (equal (wttrin--format-age 3600) "1 hour ago"))
  (should (equal (wttrin--format-age 7200) "2 hours ago"))
  (should (equal (wttrin--format-age 86399) "23 hours ago")))

(ert-deftest test-wttrin--format-age-days ()
  "Seconds in the days range."
  (should (equal (wttrin--format-age 86400) "1 day ago"))
  (should (equal (wttrin--format-age 172800) "2 days ago")))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-update-display
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-update-display-normal-sets-mode-line-string ()
  "Display update from cache sets wttrin-mode-line-string to non-nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (should wttrin-mode-line-string))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-extracts-emoji ()
  "Display update extracts emoji character into mode-line string."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-emoji-font nil))
        (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: X +61°F Clear"))
          (wttrin--mode-line-update-display)
          (should (string-match-p "X" (substring-no-properties wttrin-mode-line-string)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-has-help-echo ()
  "Display update sets help-echo property for tooltip."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (should (get-text-property 0 'help-echo wttrin-mode-line-string)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-has-local-map ()
  "Display update sets local-map property for mouse interaction."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (should (eq (get-text-property 0 'local-map wttrin-mode-line-string)
                    wttrin--mode-line-map)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-normal-fresh-tooltip-shows-updated ()
  "Fresh data tooltip shows weather data and 'Updated' age."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1300.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (let ((tooltip (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (string-match-p "Paris" tooltip))
          (should (string-match-p "Updated 5 minutes ago" tooltip))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; Stale Cases

(ert-deftest test-wttrin--mode-line-update-display-stale-tooltip-shows-stale ()
  "Stale data tooltip indicates staleness and retry info."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-refresh-interval 900))
        (cl-letf (((symbol-function 'float-time) (lambda () 3000.0)))
          ;; Data is 2000 seconds old, threshold is 2*900=1800 -> stale
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
          (wttrin--mode-line-update-display)
          (let ((tooltip (get-text-property 0 'help-echo wttrin-mode-line-string)))
            (should (string-match-p "Stale" tooltip))
            (should (string-match-p "fetch failed" tooltip)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-stale-emoji-dimmed ()
  "Stale data dims the emoji with gray foreground."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-refresh-interval 900)
            (wttrin-mode-line-emoji-font nil))
        (cl-letf (((symbol-function 'float-time) (lambda () 3000.0)))
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: X +61°F Clear"))
          (wttrin--mode-line-update-display)
          ;; The emoji character should have a gray face
          (let* ((str wttrin-mode-line-string)
                 ;; Find the emoji position (after the space)
                 (emoji-pos 1)
                 (face (get-text-property emoji-pos 'face str)))
            (should face)
            (should (equal (plist-get face :foreground) "gray60")))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-update-display-boundary-no-emoji-match-uses-fallback ()
  "When emoji regex doesn't match, fallback character '?' is used."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-mode-line-emoji-font nil))
        (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
          (setq wttrin--mode-line-cache (cons 1000.0 "no colon here"))
          (wttrin--mode-line-update-display)
          (should (string-match-p "\\?" (substring-no-properties wttrin-mode-line-string)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-display-boundary-nil-cache-does-nothing ()
  "When cache is nil, update-display does not set mode-line-string."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (progn
        (setq wttrin--mode-line-cache nil)
        (wttrin--mode-line-update-display)
        (should-not wttrin-mode-line-string))
    (test-wttrin--mode-line-update-display-teardown)))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-fetch-weather
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--mode-line-fetch-weather-normal-valid-response-updates-cache ()
  "Valid API response populates cache and updates display."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (testutil-wttrin-mock-http-response "Paris: ☀️ +61°F Clear"
          (wttrin--mode-line-fetch-weather)
          (should wttrin--mode-line-cache)
          (should (equal (cdr wttrin--mode-line-cache) "Paris: ☀️ +61°F Clear"))
          (should wttrin-mode-line-string)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-error-empty-response-keeps-previous ()
  "Empty API response does not overwrite previous valid cache."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        ;; Set up a valid prior cache
        (setq wttrin--mode-line-cache (cons (float-time) "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (let ((previous-cache wttrin--mode-line-cache))
          ;; Simulate fetch returning empty response
          (testutil-wttrin-mock-http-response ""
            (wttrin--mode-line-fetch-weather)
            ;; Cache should be preserved
            (should (equal wttrin--mode-line-cache previous-cache)))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-error-no-colon-response-keeps-previous ()
  "Malformed API response without colon does not overwrite previous valid cache."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (setq wttrin--mode-line-cache (cons (float-time) "Paris: ☀️ +61°F Clear"))
        (let ((previous-cache wttrin--mode-line-cache))
          (testutil-wttrin-mock-http-response "Unknown location"
            (wttrin--mode-line-fetch-weather)
            (should (equal wttrin--mode-line-cache previous-cache)))))
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

(ert-deftest test-wttrin--mode-line-fetch-weather-error-network-fail-with-cache-shows-stale ()
  "Network failure with existing cache triggers stale display."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-refresh-interval 900))
        ;; Set up old cache data
        (setq wttrin--mode-line-cache (cons (- (float-time) 2000) "Paris: ☀️ +61°F Clear"))
        ;; Simulate network failure (nil data)
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (_url callback) (funcall callback nil))))
          (wttrin--mode-line-fetch-weather)
          ;; Cache should still exist
          (should wttrin--mode-line-cache)
          ;; Mode-line should be updated (stale display)
          (should wttrin-mode-line-string)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-fetch-weather-error-network-fail-no-cache-shows-error ()
  "Network failure with no cache shows error placeholder."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-emoji-font nil))
        ;; No cache
        (should-not wttrin--mode-line-cache)
        ;; Simulate network failure
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (_url callback) (funcall callback nil))))
          (wttrin--mode-line-fetch-weather)
          ;; Should show error placeholder with hourglass
          (should wttrin-mode-line-string)
          (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string)))
          ;; Tooltip should mention failure
          (let ((tooltip (get-text-property 0 'help-echo wttrin-mode-line-string)))
            (should (string-match-p "failed" tooltip))
            (should (string-match-p "Paris" tooltip)))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-set-placeholder
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-set-placeholder-normal-sets-mode-line-string ()
  "Placeholder sets wttrin-mode-line-string to non-nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-set-placeholder)
        (should wttrin-mode-line-string))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-set-placeholder-normal-contains-hourglass ()
  "Placeholder displays hourglass emoji."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-emoji-font nil))
        (wttrin--mode-line-set-placeholder)
        (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-set-placeholder-normal-has-fetching-tooltip ()
  "Placeholder tooltip mentions fetching weather."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-set-placeholder)
        (let ((tooltip (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (string-match-p "Fetching" tooltip))
          (should (string-match-p "Paris" tooltip))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-set-placeholder-normal-has-local-map ()
  "Placeholder has mouse interaction keymap."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-set-placeholder)
        (should (eq (get-text-property 0 'local-map wttrin-mode-line-string)
                    wttrin--mode-line-map)))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-set-placeholder-normal-replaced-by-real-data ()
  "Placeholder is replaced when real weather data arrives via cache."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-emoji-font nil))
        (wttrin--mode-line-set-placeholder)
        (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string)))
        (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
          (wttrin--mode-line-update-display))
        (should-not (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-update-display-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-set-placeholder-boundary-does-not-set-cache ()
  "Placeholder does not contaminate cache variable."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-set-placeholder)
        (should-not wttrin--mode-line-cache))
    (test-wttrin--mode-line-update-display-teardown)))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-update-placeholder-error
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--mode-line-update-placeholder-error-sets-mode-line-string ()
  "Error placeholder sets wttrin-mode-line-string to non-nil."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (wttrin--mode-line-update-placeholder-error)
        (should wttrin-mode-line-string))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-placeholder-error-contains-hourglass ()
  "Error placeholder displays hourglass emoji."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-emoji-font nil))
        (wttrin--mode-line-update-placeholder-error)
        (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-update-display-teardown)))

(ert-deftest test-wttrin--mode-line-update-placeholder-error-tooltip-mentions-failure ()
  "Error placeholder tooltip mentions failure and retry."
  (test-wttrin--mode-line-update-display-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-refresh-interval 900))
        (wttrin--mode-line-update-placeholder-error)
        (let ((tooltip (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (string-match-p "failed" tooltip))
          (should (string-match-p "Paris" tooltip))
          (should (string-match-p "retry" tooltip))
          (should (string-match-p "15 minutes" tooltip))))
    (test-wttrin--mode-line-update-display-teardown)))

(provide 'test-wttrin--mode-line-update-display)
;;; test-wttrin--mode-line-update-display.el ends here
