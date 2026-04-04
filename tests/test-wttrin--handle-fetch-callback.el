;;; test-wttrin--handle-fetch-callback.el --- Tests for wttrin--handle-fetch-callback -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--handle-fetch-callback function.
;; Tests callback coordination and error handling logic in isolation.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--handle-fetch-callback-setup ()
  "Setup for handle-fetch-callback tests."
  (testutil-wttrin-setup))

(defun test-wttrin--handle-fetch-callback-teardown ()
  "Teardown for handle-fetch-callback tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--handle-fetch-callback-normal-successful-response-calls-callback-with-data ()
  "Test handling successful response with callback invocation."
  (let ((callback-called nil)
        (callback-data nil))
    ;; Mock wttrin--extract-response-body to return test data
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "Weather: ☀️ Sunny")))
      (wttrin--handle-fetch-callback
       nil ;; status with no error
       (lambda (data &optional _error-msg)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "Weather: ☀️ Sunny" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-normal-empty-response-calls-callback-with-empty ()
  "Test handling empty but successful response."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "")))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data &optional _error-msg)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-normal-large-response-calls-callback-with-full-data ()
  "Test handling large response data."
  (let ((callback-called nil)
        (callback-data nil)
        (large-data (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () large-data)))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data &optional _error-msg)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (= 10000 (length callback-data)))
      (should (string= large-data callback-data)))))

;;; Boundary Cases

(ert-deftest test-wttrin--handle-fetch-callback-boundary-nil-status-calls-callback ()
  "Test handling nil status (successful response)."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       nil ;; nil status means success
       (lambda (data &optional _error-msg)
         (setq callback-called t)))

      (should callback-called))))

(ert-deftest test-wttrin--handle-fetch-callback-boundary-empty-status-plist-calls-callback ()
  "Test handling empty status plist."
  (let ((callback-called nil)
        (callback-data nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       '() ;; empty plist, no error key
       (lambda (data &optional _error-msg)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "data" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-boundary-status-with-other-keys-calls-callback ()
  "Test handling status with various keys but no error."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       '(:peer "example.com" :redirect nil) ;; status with other keys
       (lambda (data &optional _error-msg)
         (setq callback-called t)))

      (should callback-called))))

;;; Error Cases

(ert-deftest test-wttrin--handle-fetch-callback-error-network-error-calls-callback-with-nil ()
  "Test handling network error in status."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "Network unreachable"))
     (lambda (data &optional _error-msg)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-http-404-calls-callback-with-nil ()
  "Test handling HTTP error status."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "HTTP 404"))
     (lambda (data &optional _error-msg)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-timeout-calls-callback-with-nil ()
  "Test handling timeout error."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "Request timed out"))
     (lambda (data &optional _error-msg)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-callback-throws-does-not-propagate ()
  "Test handling errors thrown by user callback."
  (let ((callback-called nil)
        (error-caught nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      ;; Should not propagate error from callback
      (condition-case err
          (wttrin--handle-fetch-callback
           nil
           (lambda (data &optional _error-msg)
             (setq callback-called t)
             (error "User callback error")))
        (error
         (setq error-caught (error-message-string err))))

      (should callback-called)
      ;; Error should be caught and handled, not propagated
      (should-not error-caught))))

(ert-deftest test-wttrin--handle-fetch-callback-error-extract-body-throws-propagates-error ()
  "Test that errors during body extraction are not caught and propagate."
  (let ((callback-called nil)
        (error-caught nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () (error "Extraction error"))))
      (condition-case err
          (wttrin--handle-fetch-callback
           nil
           (lambda (data &optional _error-msg)
             (setq callback-called t)))
        (error
         (setq error-caught t)))

      ;; Extraction errors propagate, callback should not be called
      (should-not callback-called)
      (should error-caught))))

(ert-deftest test-wttrin--handle-fetch-callback-error-nil-data-from-extract-calls-callback-with-nil ()
  "Test handling nil data returned from extraction."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil)))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data &optional _error-msg)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (null callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-error-multiple-error-keys-calls-callback-with-nil ()
  "Test handling status with multiple error indicators."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "First error") :another-error "Second error")
     (lambda (data &optional _error-msg)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    ;; Should return nil when :error key is present
    (should (null callback-data))))

;;; User-facing error messages

(ert-deftest test-wttrin--handle-fetch-callback-error-network-shows-message ()
  "Network errors should show a specific message in the echo area,
not leave the user guessing."
  (let ((displayed-message nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq displayed-message (apply #'format fmt args)))))
      (wttrin--handle-fetch-callback
       '(:error (error "Network unreachable"))
       #'ignore)
      (should displayed-message)
      (should (string-match-p "network" (downcase displayed-message))))))

(ert-deftest test-wttrin--handle-fetch-callback-error-http-404-shows-message ()
  "HTTP 404 should tell the user the location wasn't found."
  (let ((displayed-message nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'wttrin--extract-http-status)
               (lambda () 404))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq displayed-message (apply #'format fmt args)))))
      ;; No :error in status — url-retrieve succeeded but server returned 404
      (wttrin--handle-fetch-callback nil #'ignore)
      (should displayed-message)
      (should (string-match-p "not found\\|404" (downcase displayed-message))))))

(ert-deftest test-wttrin--handle-fetch-callback-error-http-500-shows-message ()
  "HTTP 500 should tell the user the weather service had an error."
  (let ((displayed-message nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'wttrin--extract-http-status)
               (lambda () 500))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq displayed-message (apply #'format fmt args)))))
      (wttrin--handle-fetch-callback nil #'ignore)
      (should displayed-message)
      (should (string-match-p "service\\|server\\|500" (downcase displayed-message))))))

(provide 'test-wttrin--handle-fetch-callback)
;;; test-wttrin--handle-fetch-callback.el ends here
