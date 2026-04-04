;;; test-wttrin-query.el --- Tests for wttrin-query -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-query function.
;; Tests the async weather query orchestration: buffer creation,
;; loading state, cache lookup, and display callback.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-query-setup ()
  "Setup for wttrin-query tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-query-teardown ()
  "Teardown for wttrin-query tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Normal Cases

(ert-deftest test-wttrin-query-normal-creates-buffer ()
  "Calling query should create the *wttr.in* buffer."
  (test-wttrin-query-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                 (lambda (_location _callback) nil)))
        (wttrin-query "Paris")
        (should (get-buffer "*wttr.in*")))
    (test-wttrin-query-teardown)))

(ert-deftest test-wttrin-query-normal-shows-loading-message ()
  "Buffer should show a loading message with the location name while fetching."
  (test-wttrin-query-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                 (lambda (_location _callback) nil)))
        (wttrin-query "New Orleans, LA")
        (with-current-buffer "*wttr.in*"
          (let ((contents (buffer-string)))
            (should (string-match-p "Loading" contents))
            (should (string-match-p "New Orleans, LA" contents)))))
    (test-wttrin-query-teardown)))

(ert-deftest test-wttrin-query-normal-buffer-is-read-only-during-loading ()
  "The loading buffer should be read-only to prevent user edits."
  (test-wttrin-query-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                 (lambda (_location _callback) nil)))
        (wttrin-query "Tokyo")
        (with-current-buffer "*wttr.in*"
          (should buffer-read-only)))
    (test-wttrin-query-teardown)))

(ert-deftest test-wttrin-query-normal-fetches-for-correct-location ()
  "Query should request weather for the specified location."
  (test-wttrin-query-setup)
  (unwind-protect
      (let ((fetched-location nil))
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (location _callback)
                     (setq fetched-location location))))
          (wttrin-query "Berlin, DE")
          (should (equal fetched-location "Berlin, DE"))))
    (test-wttrin-query-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-query-boundary-dead-buffer-callback-is-safe ()
  "If the buffer is killed before the async callback fires, it should not error."
  (test-wttrin-query-setup)
  (unwind-protect
      (let ((saved-callback nil))
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (_location callback)
                     (setq saved-callback callback))))
          (wttrin-query "Paris")
          ;; Kill the buffer before callback fires
          (kill-buffer "*wttr.in*")
          ;; Invoke callback — should not error
          (should-not (get-buffer "*wttr.in*"))
          (funcall saved-callback "weather data")
          ;; Buffer should NOT be recreated
          (should-not (get-buffer "*wttr.in*"))))
    (test-wttrin-query-teardown)))

;;; Error Cases

(ert-deftest test-wttrin-query-error-nil-response-shows-error-message ()
  "When fetch returns nil, the user should see an error message, not a crash."
  (test-wttrin-query-setup)
  (unwind-protect
      (let ((saved-callback nil)
            (displayed-message nil))
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (_location callback)
                     (setq saved-callback callback)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (wttrin-query "BadLocation")
          ;; Simulate fetch returning nil
          (funcall saved-callback nil)
          ;; Should have shown error message (from wttrin--display-weather validation)
          (should displayed-message)
          (should (string-match-p "Cannot retrieve" displayed-message))))
    (test-wttrin-query-teardown)))

(provide 'test-wttrin-query)
;;; test-wttrin-query.el ends here
