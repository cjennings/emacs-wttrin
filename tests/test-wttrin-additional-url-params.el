;;; test-wttrin-additional-url-params.el --- Tests for wttrin-additional-url-params -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-additional-url-params function.
;; Tests URL parameter generation with different unit system configurations.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Normal Cases

(ert-deftest test-wttrin-additional-url-params-normal-metric-returns-param ()
  "Test that metric unit system returns ?m parameter."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "?m" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-normal-uscs-returns-param ()
  "Test that USCS unit system returns ?u parameter."
  (testutil-wttrin-with-unit-system "u"
    (should (equal "?u" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-normal-wind-speed-returns-param ()
  "Test that wind speed unit returns ?M parameter."
  (testutil-wttrin-with-unit-system "M"
    (should (equal "?M" (wttrin-additional-url-params)))))

;;; Boundary Cases

(ert-deftest test-wttrin-additional-url-params-boundary-nil-returns-question-mark ()
  "Test that nil unit system returns just ? parameter."
  (testutil-wttrin-with-unit-system nil
    (should (equal "?" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-boundary-empty-string-returns-question-mark ()
  "Test that empty string unit system returns just ? parameter."
  (testutil-wttrin-with-unit-system ""
    (should (equal "?" (wttrin-additional-url-params)))))

(ert-deftest test-wttrin-additional-url-params-boundary-single-char-returns-param ()
  "Test that single character unit system returns ?<char> parameter."
  (testutil-wttrin-with-unit-system "x"
    (should (equal "?x" (wttrin-additional-url-params)))))

;;; Error Cases

(ert-deftest test-wttrin-additional-url-params-error-number-signals-error ()
  "Test that number unit system signals a type error."
  ;; concat requires string or sequence, number causes wrong-type-argument error
  (testutil-wttrin-with-unit-system 123
    (should-error (wttrin-additional-url-params)
                  :type 'wrong-type-argument)))

(provide 'test-wttrin-additional-url-params)
;;; test-wttrin-additional-url-params.el ends here
