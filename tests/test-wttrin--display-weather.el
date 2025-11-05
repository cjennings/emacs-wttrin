;;; test-wttrin--display-weather.el --- Tests for wttrin--display-weather -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--display-weather function.
;; Tests the extracted display logic that formats and shows weather data.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Test Data Fixtures

(defconst test-wttrin--display-weather-sample-raw-data
  "
Weather report: Paris, France

                                                       ┌─────────────┐
                                                  ┌──────────────────────────┐
   ┌──────────────────────────────┬───────────────────────────────────────────┴───────────────────────────────────────┐
   │            Monday             └──────────────────────────────────────────────────────────────────────────────────────┤
   │                                         2025-11-04 08:00:00 CST
   │
   │        Coordinates: 48.8566, 2.3522
   │
     \\  /       Partly cloudy
   _ /\".-. 22 °C
     \\_(   ).  ↓ 15 km/h
     /(___(__)  10 km
                0.0 mm"
  "Sample raw weather data with realistic wttr.in structure for testing.")

;;; Test Setup and Teardown

(defun test-wttrin--display-weather-setup ()
  "Setup for display weather tests."
  (testutil-wttrin-setup)
  ;; Kill any existing weather buffer
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin--display-weather-teardown ()
  "Teardown for display weather tests."
  (testutil-wttrin-teardown)
  ;; Clean up weather buffer
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Normal Cases

(ert-deftest test-wttrin--display-weather-normal-valid-data-creates-buffer ()
  "Test that valid weather data creates and displays buffer correctly."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "Paris, France" test-wttrin--display-weather-sample-raw-data)

        ;; Buffer should exist
        (should (get-buffer "*wttr.in*"))

        ;; Buffer should be displayed
        (should (get-buffer-window "*wttr.in*"))

        ;; Buffer should have content
        (with-current-buffer "*wttr.in*"
          (should (> (buffer-size) 0))

          ;; Buffer should be read-only
          (should buffer-read-only)

          ;; Location should be set
          (should (equal wttrin--current-location "Paris, France"))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-normal-valid-data-sets-keybindings ()
  "Test that keybindings are properly set up in weather buffer."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "London" test-wttrin--display-weather-sample-raw-data)

        (with-current-buffer "*wttr.in*"
          ;; Check that keybindings are set (they should be in the local map)
          (should (keymapp (current-local-map)))
          (should (commandp (lookup-key (current-local-map) "q")))
          (should (commandp (lookup-key (current-local-map) "a")))
          (should (commandp (lookup-key (current-local-map) "g")))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-normal-valid-data-contains-instructions ()
  "Test that help instructions are displayed at bottom of buffer."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "Tokyo" test-wttrin--display-weather-sample-raw-data)

        (with-current-buffer "*wttr.in*"
          (goto-char (point-max))
          (forward-line -2)
          ;; Should contain help text
          (should (search-forward "Press:" nil t))
          (should (search-forward "[a] for another location" nil t))
          (should (search-forward "[g] to refresh" nil t))
          (should (search-forward "[q] to quit" nil t))))
    (test-wttrin--display-weather-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--display-weather-boundary-empty-location-name-creates-buffer ()
  "Test that empty location name still creates buffer."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "" test-wttrin--display-weather-sample-raw-data)

        ;; Buffer should still be created
        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; Location should be set to empty string
          (should (equal wttrin--current-location ""))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-boundary-location-with-special-chars-creates-buffer ()
  "Test that location with special characters creates buffer."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "São Paulo, BR 🌆" test-wttrin--display-weather-sample-raw-data)

        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; Location with Unicode should be preserved
          (should (equal wttrin--current-location "São Paulo, BR 🌆"))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-boundary-empty-string-creates-buffer ()
  "Test that empty weather string creates buffer without error.
Empty string does not match ERROR pattern, so it's processed as data."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "Paris" "")

        ;; Empty string is not treated as error, buffer is created
        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; Buffer exists but will have minimal/broken content
          ;; Just verify it was created and made read-only
          (should buffer-read-only)))
    (test-wttrin--display-weather-teardown)))

;;; Error Cases

(ert-deftest test-wttrin--display-weather-error-nil-raw-string-shows-message ()
  "Test that nil raw-string displays error message."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        ;; Capture message output
        (let ((message-log-max t)
              (message-displayed nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-displayed (apply #'format format-string args)))))
            (wttrin--display-weather "InvalidCity" nil)

            ;; Should display error message
            (should message-displayed)
            (should (string-match-p "Cannot retrieve" message-displayed)))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-error-string-with-error-shows-message ()
  "Test that weather string containing ERROR shows error message."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        (let ((message-log-max t)
              (message-displayed nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-displayed (apply #'format format-string args)))))
            (wttrin--display-weather "BadLocation" testutil-wttrin-sample-error-response)

            ;; Should display error message
            (should message-displayed)
            (should (string-match-p "Cannot retrieve" message-displayed)))))
    (test-wttrin--display-weather-teardown)))

(ert-deftest test-wttrin--display-weather-error-nil-raw-string-no-buffer-created ()
  "Test that nil raw-string does not create weather buffer."
  (test-wttrin--display-weather-setup)
  (unwind-protect
      (progn
        ;; Suppress message output
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin--display-weather "InvalidCity" nil)

          ;; Buffer should not be created for error case
          ;; (or if it exists from before, it shouldn't be switched to)
          ;; This is testing the error path doesn't create/switch to buffer
          (should-not (string-match-p "wttr.in"
                                      (buffer-name (current-buffer))))))
    (test-wttrin--display-weather-teardown)))

(provide 'test-wttrin--display-weather)
;;; test-wttrin--display-weather.el ends here
