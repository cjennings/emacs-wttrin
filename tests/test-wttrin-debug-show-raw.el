;;; test-wttrin-debug-show-raw.el --- Tests for wttrin-debug-show-raw -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-debug-show-raw function.
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

(defun test-wttrin-debug-show-raw-setup ()
  "Setup for wttrin-debug-show-raw tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttrin-debug*")
    (kill-buffer "*wttrin-debug*")))

(defun test-wttrin-debug-show-raw-teardown ()
  "Teardown for wttrin-debug-show-raw tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttrin-debug*")
    (kill-buffer "*wttrin-debug*")))

;;; Normal Cases

(ert-deftest test-wttrin-debug-show-raw-normal-creates-debug-buffer ()
  "Calling show-raw should create the *wttrin-debug* buffer."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Line one\nLine two\nLine three"
          (wttrin-debug-show-raw "Paris"))
        (should (get-buffer "*wttrin-debug*")))
    (test-wttrin-debug-show-raw-teardown)))

(ert-deftest test-wttrin-debug-show-raw-normal-adds-line-numbers ()
  "Each line of raw data should be prefixed with its line number."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Alpha\nBravo\nCharlie"
          (wttrin-debug-show-raw "Paris"))
        (with-current-buffer "*wttrin-debug*"
          (let ((contents (buffer-string)))
            (should (string-match-p "^ *1: Alpha" contents))
            (should (string-match-p "^ *2: Bravo" contents))
            (should (string-match-p "^ *3: Charlie" contents)))))
    (test-wttrin-debug-show-raw-teardown)))

(ert-deftest test-wttrin-debug-show-raw-normal-fetches-correct-location ()
  "The function should fetch weather for the specified location."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (let ((fetched-location nil))
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (location callback)
                     (setq fetched-location location)
                     (funcall callback "data"))))
          (wttrin-debug-show-raw "Berlin, DE")
          (should (equal fetched-location "Berlin, DE"))))
    (test-wttrin-debug-show-raw-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-debug-show-raw-boundary-single-line ()
  "Single-line response should get line number 1."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (progn
        (testutil-wttrin-mock-http-response "Just one line"
          (wttrin-debug-show-raw "Paris"))
        (with-current-buffer "*wttrin-debug*"
          (let ((contents (buffer-string)))
            (should (string-match-p "^ *1: Just one line" contents)))))
    (test-wttrin-debug-show-raw-teardown)))

;;; Error Cases

(ert-deftest test-wttrin-debug-show-raw-error-nil-response-should-not-crash ()
  "When the fetch returns nil, the function should handle it gracefully."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                 (lambda (_location callback)
                   (funcall callback nil))))
        ;; Should not crash
        (wttrin-debug-show-raw "BadLocation")
        ;; Buffer should exist even on error
        (should (get-buffer "*wttrin-debug*")))
    (test-wttrin-debug-show-raw-teardown)))

(ert-deftest test-wttrin-debug-show-raw-normal-always-fetches-fresh ()
  "A debug command should always fetch from the API, not serve cached data.
When debugging, the user needs to see what the API currently returns."
  (test-wttrin-debug-show-raw-setup)
  (unwind-protect
      (let ((force-refresh-was-set nil))
        ;; Seed cache so there IS data to serve
        (testutil-wttrin-add-to-cache "Paris" "old cached data" 300)
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (_location callback)
                     (setq force-refresh-was-set wttrin--force-refresh)
                     (funcall callback "fresh from API"))))
          (wttrin-debug-show-raw "Paris")
          ;; Force-refresh should have been active during the fetch
          (should force-refresh-was-set)))
    (test-wttrin-debug-show-raw-teardown)))

(provide 'test-wttrin-debug-show-raw)
;;; test-wttrin-debug-show-raw.el ends here
