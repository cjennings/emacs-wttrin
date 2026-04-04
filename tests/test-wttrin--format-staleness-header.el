;;; test-wttrin--format-staleness-header.el --- Tests for staleness header -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--format-staleness-header function.
;; Tests that buffer staleness information is formatted correctly.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--format-staleness-header-setup ()
  "Setup for staleness header tests."
  (testutil-wttrin-setup))

(defun test-wttrin--format-staleness-header-teardown ()
  "Teardown for staleness header tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--format-staleness-header-normal-returns-formatted-string ()
  "Staleness header includes time and age when cache exists."
  (test-wttrin--format-staleness-header-setup)
  (unwind-protect
      (let ((now 1000000.0))
        (cl-letf (((symbol-function 'float-time) (lambda () now)))
          ;; Add data cached 5 minutes ago
          (testutil-wttrin-add-to-cache "Paris" "weather data" 300)
          (let ((header (wttrin--format-staleness-header "Paris")))
            (should header)
            (should (string-match-p "Last updated:" header))
            (should (string-match-p "5 minutes ago" header)))))
    (test-wttrin--format-staleness-header-teardown)))

(ert-deftest test-wttrin--format-staleness-header-normal-just-now ()
  "Staleness header shows 'just now' for very recent data."
  (test-wttrin--format-staleness-header-setup)
  (unwind-protect
      (let ((now 1000000.0))
        (cl-letf (((symbol-function 'float-time) (lambda () now)))
          (testutil-wttrin-add-to-cache "Paris" "weather data" 10)
          (let ((header (wttrin--format-staleness-header "Paris")))
            (should (string-match-p "just now" header)))))
    (test-wttrin--format-staleness-header-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--format-staleness-header-boundary-no-cache-returns-nil ()
  "Returns nil when no cache entry exists for location."
  (test-wttrin--format-staleness-header-setup)
  (unwind-protect
      (should-not (wttrin--format-staleness-header "Unknown City"))
    (test-wttrin--format-staleness-header-teardown)))

(ert-deftest test-wttrin--format-staleness-header-boundary-different-unit-system ()
  "Cache lookup respects current unit system setting."
  (test-wttrin--format-staleness-header-setup)
  (unwind-protect
      (let ((now 1000000.0))
        (cl-letf (((symbol-function 'float-time) (lambda () now)))
          ;; Add data under metric unit system
          (let ((wttrin-unit-system "m"))
            (testutil-wttrin-add-to-cache "Paris" "metric data" 300))
          ;; Query under imperial unit system — should find nothing
          (let ((wttrin-unit-system "u"))
            (should-not (wttrin--format-staleness-header "Paris")))
          ;; Query under metric — should find it
          (let ((wttrin-unit-system "m"))
            (should (wttrin--format-staleness-header "Paris")))))
    (test-wttrin--format-staleness-header-teardown)))

(provide 'test-wttrin--format-staleness-header)
;;; test-wttrin--format-staleness-header.el ends here
