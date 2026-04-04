;;; test-wttrin--extract-http-status.el --- Tests for wttrin--extract-http-status -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--extract-http-status function.
;; Parses the HTTP status code from url-retrieve's response buffer.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--extract-http-status-normal-200 ()
  "Standard 200 OK response should return 200."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nbody")
    (should (= (wttrin--extract-http-status) 200))))

(ert-deftest test-wttrin--extract-http-status-normal-404 ()
  "404 Not Found response should return 404."
  (with-temp-buffer
    (insert "HTTP/1.1 404 Not Found\r\n\r\nNot found")
    (should (= (wttrin--extract-http-status) 404))))

(ert-deftest test-wttrin--extract-http-status-normal-500 ()
  "500 Internal Server Error should return 500."
  (with-temp-buffer
    (insert "HTTP/1.1 500 Internal Server Error\r\n\r\nerror")
    (should (= (wttrin--extract-http-status) 500))))

;;; Boundary Cases

(ert-deftest test-wttrin--extract-http-status-boundary-no-status-line ()
  "Buffer with no HTTP status line should return nil."
  (with-temp-buffer
    (insert "just some text with no HTTP headers")
    (should-not (wttrin--extract-http-status))))

(ert-deftest test-wttrin--extract-http-status-boundary-http2 ()
  "HTTP/2 responses use a different format but still have a status code."
  (with-temp-buffer
    (insert "HTTP/2 301 Moved Permanently\r\n\r\n")
    (should (= (wttrin--extract-http-status) 301))))

(ert-deftest test-wttrin--extract-http-status-boundary-does-not-move-point ()
  "Parsing the status should not change point, so it doesn't interfere
with subsequent header/body parsing."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\nbody")
    (goto-char (point-min))
    (let ((pos-before (point)))
      (wttrin--extract-http-status)
      (should (= (point) pos-before)))))

(provide 'test-wttrin--extract-http-status)
;;; test-wttrin--extract-http-status.el ends here
