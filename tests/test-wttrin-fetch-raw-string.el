;;; test-wttrin-fetch-raw-string.el --- Tests for wttrin-fetch-raw-string -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-fetch-raw-string function.
;; Tests the public API for fetching weather data by location query.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-fetch-raw-string-setup ()
  "Setup for fetch-raw-string tests."
  (testutil-wttrin-setup))

(defun test-wttrin-fetch-raw-string-teardown ()
  "Teardown for fetch-raw-string tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin-fetch-raw-string-normal-builds-correct-url ()
  "The fetch should use a properly constructed wttr.in URL for the query."
  (test-wttrin-fetch-raw-string-setup)
  (unwind-protect
      (let ((fetched-url nil))
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (url _callback) (setq fetched-url url))))
          (wttrin-fetch-raw-string "Paris" #'ignore)
          ;; URL should contain wttr.in and the encoded location
          (should (string-match-p "wttr\\.in" fetched-url))
          (should (string-match-p "Paris" fetched-url))))
    (test-wttrin-fetch-raw-string-teardown)))

(ert-deftest test-wttrin-fetch-raw-string-normal-passes-callback-through ()
  "The user's callback should receive the fetched data."
  (test-wttrin-fetch-raw-string-setup)
  (unwind-protect
      (let ((received-data nil))
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (_url callback)
                     (funcall callback "weather response"))))
          (wttrin-fetch-raw-string "Paris"
                                   (lambda (data) (setq received-data data)))
          (should (equal received-data "weather response"))))
    (test-wttrin-fetch-raw-string-teardown)))

;;; Error Cases

(ert-deftest test-wttrin-fetch-raw-string-error-nil-query-signals-error ()
  "Passing nil as query should signal an error (invalid URL construction)."
  (test-wttrin-fetch-raw-string-setup)
  (unwind-protect
      (should-error (wttrin-fetch-raw-string nil #'ignore)
                    :type 'error)
    (test-wttrin-fetch-raw-string-teardown)))

(provide 'test-wttrin-fetch-raw-string)
;;; test-wttrin-fetch-raw-string.el ends here
