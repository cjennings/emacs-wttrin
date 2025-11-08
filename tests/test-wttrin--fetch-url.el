;;; test-wttrin--fetch-url.el --- Tests for wttrin--fetch-url -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--fetch-url function.
;; Tests the common async URL fetching logic extracted during refactoring.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--fetch-url-normal-success-calls-callback-with-data ()
  "Test that successful fetch calls callback with decoded data."
  (let ((callback-called nil)
        (callback-data nil))
    ;; Mock url-retrieve to simulate successful response
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 ;; Simulate successful HTTP response
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n")
                   (insert "Content-Type: text/plain\r\n")
                   (insert "\r\n")
                   (insert "Weather data here")
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      ;; Verify callback was called with correct data
      (should callback-called)
      (should (string= "Weather data here" callback-data)))))

(ert-deftest test-wttrin--fetch-url-normal-utf8-decoding ()
  "Test that UTF-8 content is properly decoded."
  (let ((callback-data nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (insert "Weather: ☀️ Sunny 中文")
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-data data)))

      (should (string-match-p "☀️" callback-data))
      (should (string-match-p "中文" callback-data)))))

(ert-deftest test-wttrin--fetch-url-normal-headers-stripped ()
  "Test that HTTP headers are correctly stripped from response."
  (let ((callback-data nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n")
                   (insert "Content-Type: text/plain\r\n")
                   (insert "Content-Length: 17\r\n")
                   (insert "\r\n")
                   (insert "Weather data here")
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-data data)))

      ;; Headers should not be in the data
      (should-not (string-match-p "HTTP/1.1" callback-data))
      (should-not (string-match-p "Content-Type" callback-data))
      (should (string= "Weather data here" callback-data)))))

;;; Boundary Cases

(ert-deftest test-wttrin--fetch-url-boundary-empty-response ()
  "Test handling of empty response body."
  (let ((callback-data nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   ;; Empty body
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-data data)))

      (should (string= "" callback-data)))))

(ert-deftest test-wttrin--fetch-url-boundary-large-response ()
  "Test handling of large response body."
  (let ((callback-data nil)
        (large-body (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (insert large-body)
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-data data)))

      (should (= 10000 (length callback-data))))))

(ert-deftest test-wttrin--fetch-url-boundary-unix-line-endings ()
  "Test handling of Unix-style line endings (LF only)."
  (let ((callback-data nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n")
                   (insert "Content-Type: text/plain\n")
                   (insert "\n")
                   (insert "Weather data")
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-data data)))

      (should (string= "Weather data" callback-data)))))

;;; Error Cases

(ert-deftest test-wttrin--fetch-url-error-network-error-calls-callback-with-nil ()
  "Test that network errors result in callback being called with nil."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 ;; Simulate network error
                 (with-temp-buffer
                   (funcall callback '(:error (error "Network unreachable")))))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      (should callback-called)
      (should (null callback-data)))))

(ert-deftest test-wttrin--fetch-url-error-processing-error-calls-callback-with-nil ()
  "Test that processing errors result in callback being called with nil."
  (let ((callback-called nil)
        (callback-data 'not-nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   ;; Simulate a real error by having decode-coding-string fail
                   ;; Make buffer-substring-no-properties return invalid data
                   (cl-letf (((symbol-function 'decode-coding-string)
                              (lambda (string coding-system)
                                (error "Decoding error"))))
                     (insert "HTTP/1.1 200 OK\r\n\r\ndata")
                     (funcall callback nil))))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-called t)
         (setq callback-data data)))

      ;; Should still call callback even on error
      (should callback-called)
      ;; But data should be nil due to error
      (should (null callback-data)))))

(ert-deftest test-wttrin--fetch-url-error-buffer-killed-after-processing ()
  "Test that response buffer is properly killed after processing."
  (let ((buffers-before (buffer-list))
        (callback-called nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n\r\ndata")
                   (funcall callback nil)))))

      (wttrin--fetch-url
       "http://example.com/weather"
       (lambda (data)
         (setq callback-called t)))

      (should callback-called)
      ;; Buffer list should be the same (no leaked buffers)
      (should (= (length buffers-before) (length (buffer-list)))))))

(provide 'test-wttrin--fetch-url)
;;; test-wttrin--fetch-url.el ends here
