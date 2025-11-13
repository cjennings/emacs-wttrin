;;; test-wttrin--handle-fetch-callback.el --- Tests for wttrin--handle-fetch-callback -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--handle-fetch-callback function.
;; Tests callback coordination and error handling logic in isolation.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--handle-fetch-callback-normal-successful-response ()
  "Test handling successful response with callback invocation."
  (let ((callback-called nil)
        (callback-data nil))
    ;; Mock wttrin--extract-response-body to return test data
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "Weather: ☀️ Sunny")))
      (wttrin--handle-fetch-callback
       nil ;; status with no error
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "Weather: ☀️ Sunny" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-normal-empty-response ()
  "Test handling empty but successful response."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "")))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-normal-large-response ()
  "Test handling large response data."
  (let ((callback-called nil)
        (callback-data nil)
        (large-data (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () large-data)))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (= 10000 (length callback-data)))
      (should (string= large-data callback-data)))))

;;; Boundary Cases

(ert-deftest test-wttrin--handle-fetch-callback-boundary-nil-status ()
  "Test handling nil status (successful response)."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       nil ;; nil status means success
       (lambda (data)
         (setq callback-called t)))

      (should callback-called))))

(ert-deftest test-wttrin--handle-fetch-callback-boundary-empty-status-plist ()
  "Test handling empty status plist."
  (let ((callback-called nil)
        (callback-data nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       '() ;; empty plist, no error key
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (string= "data" callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-boundary-status-with-other-keys ()
  "Test handling status with various keys but no error."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      (wttrin--handle-fetch-callback
       '(:peer "example.com" :redirect nil) ;; status with other keys
       (lambda (data)
         (setq callback-called t)))

      (should callback-called))))

;;; Error Cases

(ert-deftest test-wttrin--handle-fetch-callback-error-network-error ()
  "Test handling network error in status."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "Network unreachable"))
     (lambda (data)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-http-404 ()
  "Test handling HTTP error status."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "HTTP 404"))
     (lambda (data)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-timeout ()
  "Test handling timeout error."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "Request timed out"))
     (lambda (data)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    (should (null callback-data))))

(ert-deftest test-wttrin--handle-fetch-callback-error-callback-throws ()
  "Test handling errors thrown by user callback."
  (let ((callback-called nil)
        (error-caught nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "data")))
      ;; Should not propagate error from callback
      (condition-case err
          (wttrin--handle-fetch-callback
           nil
           (lambda (data)
             (setq callback-called t)
             (error "User callback error")))
        (error
         (setq error-caught (error-message-string err))))

      (should callback-called)
      ;; Error should be caught and handled, not propagated
      (should-not error-caught))))

(ert-deftest test-wttrin--handle-fetch-callback-error-extract-body-throws ()
  "Test that errors during body extraction are not caught and propagate."
  (let ((callback-called nil)
        (error-caught nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () (error "Extraction error"))))
      (condition-case err
          (wttrin--handle-fetch-callback
           nil
           (lambda (data)
             (setq callback-called t)))
        (error
         (setq error-caught t)))

      ;; Extraction errors propagate, callback should not be called
      (should-not callback-called)
      (should error-caught))))

(ert-deftest test-wttrin--handle-fetch-callback-error-nil-data-from-extract ()
  "Test handling nil data returned from extraction."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil)))
      (wttrin--handle-fetch-callback
       nil
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (null callback-data)))))

(ert-deftest test-wttrin--handle-fetch-callback-error-multiple-error-keys ()
  "Test handling status with multiple error indicators."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (wttrin--handle-fetch-callback
     '(:error (error "First error") :another-error "Second error")
     (lambda (data)
       (setq callback-called t)
       (setq callback-data data)))

    (should callback-called)
    ;; Should return nil when :error key is present
    (should (null callback-data))))

(provide 'test-wttrin--handle-fetch-callback)
;;; test-wttrin--handle-fetch-callback.el ends here
