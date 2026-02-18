;;; test-wttrin-mode-line-startup-delay.el --- Tests for wttrin-mode-line-startup-delay -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-mode-line-startup-delay defcustom.
;; Tests that the startup delay variable exists and has reasonable defaults.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-mode-line-startup-delay-setup ()
  "Setup for startup delay tests."
  (testutil-wttrin-setup))

(defun test-wttrin-mode-line-startup-delay-teardown ()
  "Teardown for startup delay tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin-mode-line-startup-delay-normal-exists ()
  "Test that wttrin-mode-line-startup-delay defcustom exists."
  (test-wttrin-mode-line-startup-delay-setup)
  (unwind-protect
      (should (boundp 'wttrin-mode-line-startup-delay))
    (test-wttrin-mode-line-startup-delay-teardown)))

(ert-deftest test-wttrin-mode-line-startup-delay-normal-is-number ()
  "Test that startup delay is a number."
  (test-wttrin-mode-line-startup-delay-setup)
  (unwind-protect
      (should (numberp wttrin-mode-line-startup-delay))
    (test-wttrin-mode-line-startup-delay-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-mode-line-startup-delay-boundary-reasonable-range ()
  "Test that startup delay default value is in reasonable range (1-10 seconds)."
  (test-wttrin-mode-line-startup-delay-setup)
  (unwind-protect
      ;; Check the defcustom's standard value, not current runtime value
      ;; (other tests may set it to 0 for faster testing)
      (let ((default-value (eval (car (get 'wttrin-mode-line-startup-delay 'standard-value)))))
        (should (>= default-value 1))
        (should (<= default-value 10)))
    (test-wttrin-mode-line-startup-delay-teardown)))

(provide 'test-wttrin-mode-line-startup-delay)
;;; test-wttrin-mode-line-startup-delay.el ends here
