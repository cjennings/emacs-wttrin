;;; test-wttrin--extract-response-body.el --- Tests for wttrin--extract-response-body -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--extract-response-body function.
;; Tests HTTP response parsing and UTF-8 decoding in isolation.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--extract-response-body-normal-simple-response ()
  "Test extracting body from simple HTTP response."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Content-Type: text/plain\r\n")
    (insert "\r\n")
    (insert "Weather data")
    (let ((result (wttrin--extract-response-body)))
      (should (string= "Weather data" result)))))

(ert-deftest test-wttrin--extract-response-body-normal-utf8-content ()
  "Test extracting UTF-8 encoded body with emoji and international characters."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (insert "☀️ Sunny 中文 مرحبا")
    (let ((result (wttrin--extract-response-body)))
      (should (string-match-p "☀️" result))
      (should (string-match-p "中文" result))
      (should (string-match-p "مرحبا" result)))))

(ert-deftest test-wttrin--extract-response-body-normal-multiline-body ()
  "Test extracting multi-line response body."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (insert "Line 1\n")
    (insert "Line 2\n")
    (insert "Line 3")
    (let ((result (wttrin--extract-response-body)))
      (should (string-match-p "Line 1" result))
      (should (string-match-p "Line 2" result))
      (should (string-match-p "Line 3" result)))))

;;; Boundary Cases

(ert-deftest test-wttrin--extract-response-body-boundary-empty-body ()
  "Test extracting empty response body."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    ;; No body content
    (let ((result (wttrin--extract-response-body)))
      (should (string= "" result)))))

(ert-deftest test-wttrin--extract-response-body-boundary-large-body ()
  "Test extracting large response body."
  (let ((large-content (make-string 50000 ?x)))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\n\r\n")
      (insert large-content)
      (let ((result (wttrin--extract-response-body)))
        (should (= 50000 (length result)))
        (should (string= large-content result))))))

(ert-deftest test-wttrin--extract-response-body-boundary-unix-line-endings ()
  "Test extracting body with Unix-style LF line endings."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n")
    (insert "Content-Type: text/plain\n")
    (insert "\n")
    (insert "Body content")
    (let ((result (wttrin--extract-response-body)))
      (should (string= "Body content" result)))))

(ert-deftest test-wttrin--extract-response-body-boundary-windows-line-endings ()
  "Test extracting body with Windows-style CRLF line endings."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Content-Type: text/plain\r\n")
    (insert "\r\n")
    (insert "Body content")
    (let ((result (wttrin--extract-response-body)))
      (should (string= "Body content" result)))))

(ert-deftest test-wttrin--extract-response-body-boundary-mixed-line-endings ()
  "Test extracting body with mixed LF/CRLF line endings in headers."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Header1: value\n")
    (insert "Header2: value\r\n")
    (insert "\r\n")
    (insert "Body content")
    (let ((result (wttrin--extract-response-body)))
      (should (string= "Body content" result)))))

(ert-deftest test-wttrin--extract-response-body-boundary-many-headers ()
  "Test extracting body with many response headers."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n")
    (dotimes (i 20)
      (insert (format "Header-%d: value-%d\r\n" i i)))
    (insert "\r\n")
    (insert "Body content")
    (let ((result (wttrin--extract-response-body)))
      (should (string= "Body content" result))
      ;; Headers should not be in result
      (should-not (string-match-p "Header-" result)))))

(ert-deftest test-wttrin--extract-response-body-boundary-body-looks-like-headers ()
  "Test extracting body that contains text resembling HTTP headers."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (insert "HTTP/1.1 404 Not Found\r\n")
    (insert "This looks like headers but it's body content")
    (let ((result (wttrin--extract-response-body)))
      (should (string-match-p "HTTP/1.1 404" result))
      (should (string-match-p "This looks like headers" result)))))

;;; Error Cases

(ert-deftest test-wttrin--extract-response-body-error-no-header-separator ()
  "Test handling of response with no header/body separator."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Content-Type: text/plain\r\n")
    ;; Missing \r\n\r\n separator
    (insert "Body content")
    (let ((result (wttrin--extract-response-body)))
      ;; Should return whatever comes after attempting to find separator
      (should result))))

(ert-deftest test-wttrin--extract-response-body-error-empty-buffer ()
  "Test handling of completely empty buffer."
  (with-temp-buffer
    ;; Empty buffer
    (let ((result (wttrin--extract-response-body)))
      ;; Should return empty string or nil without crashing
      (should (or (null result) (string= "" result))))))

(ert-deftest test-wttrin--extract-response-body-error-buffer-kills-cleanly ()
  "Test that buffer is killed even when processing succeeds."
  (let ((buffers-before (buffer-list))
        result)
    (with-temp-buffer
      (let ((test-buffer (current-buffer)))
        (insert "HTTP/1.1 200 OK\r\n\r\ndata")
        (setq result (wttrin--extract-response-body))
        ;; Buffer should be killed after extraction
        (should-not (buffer-live-p test-buffer))))
    (should (string= "data" result))))

(provide 'test-wttrin--extract-response-body)
;;; test-wttrin--extract-response-body.el ends here
