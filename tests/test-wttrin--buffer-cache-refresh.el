;;; test-wttrin--buffer-cache-refresh.el --- Tests for wttrin--buffer-cache-refresh -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--buffer-cache-refresh function.
;; Tests the proactive background refresh that keeps buffer cache fresh.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--buffer-cache-refresh-setup ()
  "Setup for buffer-cache-refresh tests."
  (testutil-wttrin-setup))

(defun test-wttrin--buffer-cache-refresh-teardown ()
  "Teardown for buffer-cache-refresh tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--buffer-cache-refresh-normal-success-updates-cache ()
  "Successful fetch should store fresh data in the buffer cache."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        (testutil-wttrin-mock-http-response "Fresh weather data for Paris"
          (wttrin--buffer-cache-refresh)
          ;; Cache should now have an entry for Paris
          (let* ((cache-key (wttrin--make-cache-key "Paris"))
                 (cached (gethash cache-key wttrin--cache)))
            (should cached)
            (should (equal (cdr cached) "Fresh weather data for Paris")))))
    (test-wttrin--buffer-cache-refresh-teardown)))

(ert-deftest test-wttrin--buffer-cache-refresh-normal-uses-favorite-location ()
  "Refresh should fetch weather for the configured favorite location."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Tokyo, JP")
            (fetched-query nil))
        (cl-letf (((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (query callback)
                     (setq fetched-query query)
                     (funcall callback "some data"))))
          (wttrin--buffer-cache-refresh)
          (should (equal fetched-query "Tokyo, JP"))))
    (test-wttrin--buffer-cache-refresh-teardown)))

(ert-deftest test-wttrin--buffer-cache-refresh-normal-cache-key-respects-unit-system ()
  "Cache entry should use the correct key based on unit system settings."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-unit-system "m"))
        (testutil-wttrin-mock-http-response "metric data"
          (wttrin--buffer-cache-refresh)
          ;; Cache key should include unit system
          (let* ((expected-key (wttrin--make-cache-key "Paris"))
                 (cached (gethash expected-key wttrin--cache)))
            (should cached)
            (should (equal (cdr cached) "metric data")))))
    (test-wttrin--buffer-cache-refresh-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--buffer-cache-refresh-boundary-nil-location-is-noop ()
  "When favorite-location is nil, no fetch should be attempted."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (fetch-called nil))
        (cl-letf (((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_query _callback) (setq fetch-called t))))
          (wttrin--buffer-cache-refresh)
          (should-not fetch-called)
          (should (= 0 (testutil-wttrin-cache-size)))))
    (test-wttrin--buffer-cache-refresh-teardown)))

(ert-deftest test-wttrin--buffer-cache-refresh-boundary-overwrites-stale-entry ()
  "A refresh should replace any existing stale cache entry for the same location."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        ;; Seed cache with old data
        (testutil-wttrin-add-to-cache "Paris" "old stale data" 9999)
        (testutil-wttrin-mock-http-response "fresh new data"
          (wttrin--buffer-cache-refresh)
          (let* ((cache-key (wttrin--make-cache-key "Paris"))
                 (cached (gethash cache-key wttrin--cache)))
            (should (equal (cdr cached) "fresh new data")))))
    (test-wttrin--buffer-cache-refresh-teardown)))

;;; Error Cases

(ert-deftest test-wttrin--buffer-cache-refresh-error-fetch-failure-preserves-cache ()
  "Failed fetch should not overwrite existing cache entry."
  (test-wttrin--buffer-cache-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris"))
        ;; Seed cache with existing data
        (testutil-wttrin-add-to-cache "Paris" "existing good data" 300)
        (cl-letf (((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_query callback) (funcall callback nil))))
          (wttrin--buffer-cache-refresh)
          ;; Existing cache should be untouched
          (let* ((cache-key (wttrin--make-cache-key "Paris"))
                 (cached (gethash cache-key wttrin--cache)))
            (should cached)
            (should (equal (cdr cached) "existing good data")))))
    (test-wttrin--buffer-cache-refresh-teardown)))

(provide 'test-wttrin--buffer-cache-refresh)
;;; test-wttrin--buffer-cache-refresh.el ends here
