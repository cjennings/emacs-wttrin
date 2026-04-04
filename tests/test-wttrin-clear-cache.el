;;; test-wttrin-clear-cache.el --- Tests for wttrin-clear-cache -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-clear-cache function.
;; Tests the interactive command that clears all cached weather data.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-clear-cache-setup ()
  "Setup for clear-cache tests."
  (testutil-wttrin-setup))

(defun test-wttrin-clear-cache-teardown ()
  "Teardown for clear-cache tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin-clear-cache-normal-empties-all-entries ()
  "Clearing the cache should remove all stored weather entries."
  (test-wttrin-clear-cache-setup)
  (unwind-protect
      (progn
        ;; Populate cache with several entries
        (testutil-wttrin-add-to-cache "Paris" "data1")
        (testutil-wttrin-add-to-cache "London" "data2")
        (testutil-wttrin-add-to-cache "Tokyo" "data3")
        (should (= 3 (testutil-wttrin-cache-size)))
        (wttrin-clear-cache)
        (should (= 0 (testutil-wttrin-cache-size))))
    (test-wttrin-clear-cache-teardown)))

(ert-deftest test-wttrin-clear-cache-normal-shows-confirmation ()
  "User should be told the cache was cleared."
  (test-wttrin-clear-cache-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (wttrin-clear-cache)
          (should displayed-message)
          (should (string-match-p "cache cleared" displayed-message))))
    (test-wttrin-clear-cache-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-clear-cache-boundary-already-empty ()
  "Clearing an already empty cache should not error."
  (test-wttrin-clear-cache-setup)
  (unwind-protect
      (progn
        (should (= 0 (testutil-wttrin-cache-size)))
        ;; Should not error
        (wttrin-clear-cache)
        (should (= 0 (testutil-wttrin-cache-size))))
    (test-wttrin-clear-cache-teardown)))

(provide 'test-wttrin-clear-cache)
;;; test-wttrin-clear-cache.el ends here
