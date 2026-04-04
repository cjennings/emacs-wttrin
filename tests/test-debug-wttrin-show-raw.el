;;; test-debug-wttrin-show-raw.el --- Tests for debug-wttrin-show-raw -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for debug-wttrin-show-raw function.
;; Tests that the debug display shows raw weather data with line numbers.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;; Load wttrin-debug for the function under test
(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Setup and Teardown

(defun test-debug-wttrin-show-raw-setup ()
  "Setup for debug-wttrin-show-raw tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttrin-debug*")
    (kill-buffer "*wttrin-debug*")))

(defun test-debug-wttrin-show-raw-teardown ()
  "Teardown for debug-wttrin-show-raw tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttrin-debug*")
    (kill-buffer "*wttrin-debug*")))

;;; Normal Cases

(ert-deftest test-debug-wttrin-show-raw-normal-creates-debug-buffer ()
  "Calling show-raw should create the *wttrin-debug* buffer."
  (test-debug-wttrin-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Line one\nLine two\nLine three"
          (debug-wttrin-show-raw "Paris"))
        (should (get-buffer "*wttrin-debug*")))
    (test-debug-wttrin-show-raw-teardown)))

(ert-deftest test-debug-wttrin-show-raw-normal-adds-line-numbers ()
  "Each line of raw data should be prefixed with its line number."
  (test-debug-wttrin-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Alpha\nBravo\nCharlie"
          (debug-wttrin-show-raw "Paris"))
        (with-current-buffer "*wttrin-debug*"
          (let ((contents (buffer-string)))
            (should (string-match-p "^ *1: Alpha" contents))
            (should (string-match-p "^ *2: Bravo" contents))
            (should (string-match-p "^ *3: Charlie" contents)))))
    (test-debug-wttrin-show-raw-teardown)))

(ert-deftest test-debug-wttrin-show-raw-normal-fetches-correct-location ()
  "The function should fetch weather for the specified location."
  (test-debug-wttrin-show-raw-setup)
  (unwind-protect
      (let ((fetched-location nil))
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (location callback)
                     (setq fetched-location location)
                     (funcall callback "data"))))
          (debug-wttrin-show-raw "Berlin, DE")
          (should (equal fetched-location "Berlin, DE"))))
    (test-debug-wttrin-show-raw-teardown)))

;;; Boundary Cases

(ert-deftest test-debug-wttrin-show-raw-boundary-single-line ()
  "Single-line response should get line number 1."
  (test-debug-wttrin-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Just one line"
          (debug-wttrin-show-raw "Paris"))
        (with-current-buffer "*wttrin-debug*"
          (let ((contents (buffer-string)))
            (should (string-match-p "^ *1: Just one line" contents)))))
    (test-debug-wttrin-show-raw-teardown)))

;;; Error Cases

(ert-deftest test-debug-wttrin-show-raw-error-nil-response-should-not-crash ()
  "When the fetch returns nil, the function should handle it gracefully."
  (test-debug-wttrin-show-raw-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                 (lambda (_location callback)
                   (funcall callback nil))))
        ;; Should not crash
        (debug-wttrin-show-raw "BadLocation")
        ;; Buffer should exist even on error
        (should (get-buffer "*wttrin-debug*")))
    (test-debug-wttrin-show-raw-teardown)))

(provide 'test-debug-wttrin-show-raw)
;;; test-debug-wttrin-show-raw.el ends here
