;;; test-wttrin--process-weather-content.el --- Tests for wttrin--process-weather-content -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--process-weather-content function.
;; Tests ANSI filtering and removal of verbose Location lines.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'xterm-color)

;;; Normal Cases

(ert-deftest test-wttrin--process-weather-content-normal-plain-text ()
  "Test processing plain text without ANSI codes or Location lines."
  (let ((input "Weather: Sunny\nTemperature: 20°C"))
    (should (string= input (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-normal-removes-location-line ()
  "Test that Location line with coordinates is removed."
  (let ((input "Weather: Sunny\n   Location: Paris [48.8566, 2.3522]\nTemperature: 20°C")
        (expected "Weather: Sunny\nTemperature: 20°C"))
    (should (string= expected (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-normal-multiple-location-lines ()
  "Test that multiple Location lines are all removed."
  (let ((input "Location: Paris [48.8566, 2.3522]\nWeather: Sunny\n  Location: Test [0, 0]\nTemp: 20°C")
        (expected "Weather: Sunny\nTemp: 20°C"))
    (should (string= expected (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-normal-with-ansi-codes ()
  "Test that ANSI color codes are filtered by xterm-color-filter."
  ;; xterm-color-filter should process ANSI codes
  ;; We'll test that the function calls xterm-color-filter by checking
  ;; that basic ANSI codes are handled (this depends on xterm-color behavior)
  (let* ((input-with-ansi "\x1b[31mRed Text\x1b[0m\nNormal Text")
         (result (wttrin--process-weather-content input-with-ansi)))
    ;; xterm-color-filter adds text properties but returns the text content
    (should (string-match-p "Red Text" result))
    (should (string-match-p "Normal Text" result))))

;;; Boundary Cases

(ert-deftest test-wttrin--process-weather-content-boundary-empty-string ()
  "Test processing empty string."
  (should (string= "" (wttrin--process-weather-content ""))))

(ert-deftest test-wttrin--process-weather-content-boundary-only-location-line ()
  "Test processing string with only a Location line removes it."
  (let ((input "  Location: Test [0, 0]\n")
        (expected ""))
    ;; Input must have newline for delete-region to work correctly
    (should (string= expected (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-boundary-location-without-brackets ()
  "Test that Location line without brackets is not removed (doesn't match pattern)."
  (let ((input "Location: Paris\nWeather: Sunny"))
    ;; Pattern requires [coordinates], so this line should remain
    (should (string-match-p "Location: Paris" (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-boundary-location-case-insensitive ()
  "Test that 'location' (lowercase) IS removed due to case-insensitive regex."
  (let ((input "location: test [0, 0]\nWeather: Sunny"))
    ;; re-search-forward uses case-fold-search (defaults to t)
    ;; so lowercase 'location' matches 'Location' pattern
    (should-not (string-match-p "location:" (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-boundary-whitespace-variations ()
  "Test Location line with various whitespace patterns."
  (let ((input "     Location: Test [1, 2]  \nWeather: Sunny")
        (expected "Weather: Sunny"))
    (should (string= expected (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-boundary-preserves-non-location-brackets ()
  "Test that lines with brackets but not Location pattern are preserved."
  (let ((input "Weather: [Sunny] 20°C\nWind: [Strong]"))
    (should (string= input (wttrin--process-weather-content input)))))

;;; Error Cases

(ert-deftest test-wttrin--process-weather-content-error-handles-malformed-ansi ()
  "Test that function handles malformed ANSI codes gracefully."
  ;; xterm-color-filter should handle this, but we verify no errors occur
  (let ((input "\x1b[9999mInvalid ANSI\nNormal text"))
    (should (stringp (wttrin--process-weather-content input)))))

(ert-deftest test-wttrin--process-weather-content-error-very-long-line ()
  "Test processing very long line without errors."
  (let ((long-line (make-string 10000 ?x)))
    (should (string= long-line (wttrin--process-weather-content long-line)))))

(provide 'test-wttrin--process-weather-content)
;;; test-wttrin--process-weather-content.el ends here
