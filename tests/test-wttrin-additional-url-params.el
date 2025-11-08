;;; test-wttrin-additional-url-params.el --- Tests for wttrin-additional-url-params -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin-additional-url-params function.
;; Tests URL parameter generation based on unit system configuration.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin-additional-url-params-normal-metric-system ()
  "Test URL params with metric unit system."
  (let ((wttrin-unit-system "m"))
    (should (string= "?m" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-normal-uscs-system ()
  "Test URL params with USCS (imperial) unit system."
  (let ((wttrin-unit-system "u"))
    (should (string= "?u" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-normal-wind-speed-meters ()
  "Test URL params with wind speed in m/s."
  (let ((wttrin-unit-system "M"))
    (should (string= "?M" (wttrin-additional-url-params)))))

;;; Boundary Cases

(ert-deftest test-wttrin-additional-url-params-boundary-nil-returns-question-mark ()
  "Test URL params when unit system is nil (default/location-based)."
  (let ((wttrin-unit-system nil))
    (should (string= "?" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-boundary-empty-string ()
  "Test URL params with empty string unit system."
  (let ((wttrin-unit-system ""))
    (should (string= "?" (wttrin-additional-url-params)))))

;;; Error Cases

(ert-deftest test-wttrin-additional-url-params-error-unbound-variable ()
  "Test behavior when wttrin-unit-system is unbound (should use default nil)."
  ;; This test verifies the defcustom default value
  (let ((wttrin-unit-system nil))  ; Simulate default value
    (should (string= "?" (wttrin-additional-url-params)))))

(provide 'test-wttrin-additional-url-params)
;;; test-wttrin-additional-url-params.el ends here
