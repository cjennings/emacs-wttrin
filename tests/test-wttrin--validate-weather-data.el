;;; test-wttrin--validate-weather-data.el --- Tests for wttrin--validate-weather-data -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--validate-weather-data function.
;; Tests validation of weather data strings for errors and nil values.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--validate-weather-data-setup ()
  "Setup for validate-weather-data tests."
  (testutil-wttrin-setup))

(defun test-wttrin--validate-weather-data-teardown ()
  "Teardown for validate-weather-data tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--validate-weather-data-normal-valid-weather-string-returns-true ()
  "Test that valid weather data string is accepted."
  (let ((valid-weather "Weather: 20°C\nCondition: Sunny\nWind: 10 km/h"))
    (should (wttrin--validate-weather-data valid-weather))))

(ert-deftest test-wttrin--validate-weather-data-normal-multiline-weather-returns-true ()
  "Test that multiline weather data is accepted."
  (let ((weather "┌─────────────┐\n│   Weather   │\n│    20°C     │\n└─────────────┘"))
    (should (wttrin--validate-weather-data weather))))

(ert-deftest test-wttrin--validate-weather-data-normal-weather-with-unicode-returns-true ()
  "Test that weather data with Unicode characters is accepted."
  (let ((weather "Temperature: 20°C ☀️\nWind: 15 km/h 💨"))
    (should (wttrin--validate-weather-data weather))))

;;; Boundary Cases

(ert-deftest test-wttrin--validate-weather-data-boundary-empty-string-returns-true ()
  "Test that empty string is considered valid (though unusual)."
  (should (wttrin--validate-weather-data "")))

(ert-deftest test-wttrin--validate-weather-data-boundary-whitespace-only-returns-true ()
  "Test that whitespace-only string is considered valid."
  (should (wttrin--validate-weather-data "   \n  \t  ")))

(ert-deftest test-wttrin--validate-weather-data-boundary-single-character-returns-true ()
  "Test that single character string is valid."
  (should (wttrin--validate-weather-data "x")))

(ert-deftest test-wttrin--validate-weather-data-boundary-error-lowercase-returns-nil ()
  "Test that lowercase 'error' is rejected (case-insensitive matching)."
  ;; string-match uses case-fold-search which defaults to t in Emacs
  (should-not (wttrin--validate-weather-data "error: connection failed")))

(ert-deftest test-wttrin--validate-weather-data-boundary-error-in-middle-returns-nil ()
  "Test that 'ERROR' anywhere in string causes rejection."
  (should-not (wttrin--validate-weather-data "Weather: ERROR occurred while fetching")))

;;; Error Cases

(ert-deftest test-wttrin--validate-weather-data-error-nil-string-returns-nil ()
  "Test that nil string is rejected."
  (should-not (wttrin--validate-weather-data nil)))

(ert-deftest test-wttrin--validate-weather-data-error-uppercase-error-returns-nil ()
  "Test that string containing 'ERROR' is rejected."
  (should-not (wttrin--validate-weather-data "ERROR: Unable to fetch weather")))

(ert-deftest test-wttrin--validate-weather-data-error-error-at-start-returns-nil ()
  "Test that 'ERROR' at start of string causes rejection."
  (should-not (wttrin--validate-weather-data "ERROR 404")))

(ert-deftest test-wttrin--validate-weather-data-error-error-at-end-returns-nil ()
  "Test that 'ERROR' at end of string causes rejection."
  (should-not (wttrin--validate-weather-data "Network ERROR")))

(provide 'test-wttrin--validate-weather-data)
;;; test-wttrin--validate-weather-data.el ends here
