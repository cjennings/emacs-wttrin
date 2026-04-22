;;; test-wttrin-geolocation-detect.el --- Tests for wttrin-geolocation-detect -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for `wttrin-geolocation-detect'.  Mocks `url-retrieve' at
;; the boundary — exercises the real extract/parse logic via the selected
;; provider.  No network, no async timing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin-geolocation)

;;; Setup and Teardown

(defun test-wttrin-geolocation-detect-setup ()
  "Setup for detect tests — pin provider to ipapi for determinism."
  (setq wttrin-geolocation-provider 'ipapi))

(defun test-wttrin-geolocation-detect-teardown ()
  "Teardown for detect tests — restore default provider."
  (setq wttrin-geolocation-provider 'ipapi))

;;; Helpers

(defmacro test-wttrin-geolocation-detect--with-http (status body-string &rest body)
  "Run BODY with `url-retrieve' mocked to return HTTP STATUS and BODY-STRING.
The mock writes a full HTTP response into a temp buffer and invokes the
retrieval callback with a nil status plist (success)."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'url-retrieve)
              (lambda (_url callback)
                (with-temp-buffer
                  (insert (format "HTTP/1.1 %d OK\r\n" ,status))
                  (insert "Content-Type: application/json\r\n\r\n")
                  (insert ,body-string)
                  (funcall callback nil)))))
     ,@body))

(defmacro test-wttrin-geolocation-detect--with-network-error (&rest body)
  "Run BODY with `url-retrieve' mocked to report a network-level error."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'url-retrieve)
              (lambda (_url callback)
                (with-temp-buffer
                  (funcall callback '(:error (error "Network unreachable")))))))
     ,@body))

;;; Normal Cases

(ert-deftest test-wttrin-geolocation-detect-normal-ipapi-success-callback-receives-location ()
  "A successful ipapi response leads to the callback receiving \"City, Region\"."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 200
            "{\"city\":\"Berkeley\",\"region\":\"California\"}"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should (string= "Berkeley, California" result)))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-normal-selected-provider-used ()
  "Switching `wttrin-geolocation-provider' routes through that provider's URL and parser."
  (let ((wttrin-geolocation-provider 'ipinfo)
        (requested-url nil)
        (result nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback)
                 (setq requested-url url)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (insert "{\"city\":\"Mountain View\",\"region\":\"California\",\"loc\":\"37.4,-122.0\"}")
                   (funcall callback nil)))))
      (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
    (should (string= "https://ipinfo.io/json" requested-url))
    (should (string= "Mountain View, California" result))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation-detect-boundary-parser-returns-nil-callback-gets-nil ()
  "If the parser rejects the response (missing fields), the callback receives nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 200
            "{\"city\":\"Berkeley\"}"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-boundary-ipapi-rate-limit-error-flag-returns-nil ()
  "An ipapi rate-limit response (HTTP 200 with error flag) yields nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 200
            "{\"error\":true,\"reason\":\"RateLimited\"}"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

;;; Error Cases

(ert-deftest test-wttrin-geolocation-detect-error-network-failure-calls-callback-with-nil ()
  "A network-level error surfaces as nil to the callback."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((called nil)
            (result 'unset))
        (test-wttrin-geolocation-detect--with-network-error
          (wttrin-geolocation-detect (lambda (loc)
                                       (setq called t)
                                       (setq result loc))))
        (should called)
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-http-429-returns-nil ()
  "An HTTP 429 rate-limit response yields nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 429
            "{\"error\":\"rate limit\"}"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-http-500-returns-nil ()
  "An HTTP 500 server error yields nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 500
            "Internal Server Error"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-empty-body-returns-nil ()
  "An empty response body yields nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 200 ""
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-malformed-json-returns-nil ()
  "Malformed JSON in the response body yields nil."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((result 'unset))
        (test-wttrin-geolocation-detect--with-http 200 "{not valid json"
          (wttrin-geolocation-detect (lambda (loc) (setq result loc))))
        (should-not result))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-buffer-cleanup-after-success ()
  "The response buffer is killed after a successful fetch (no leaks)."
  (test-wttrin-geolocation-detect-setup)
  (unwind-protect
      (let ((buffers-before (length (buffer-list))))
        (test-wttrin-geolocation-detect--with-http 200
            "{\"city\":\"Paris\",\"region\":\"IDF\"}"
          (wttrin-geolocation-detect #'ignore))
        (should (= buffers-before (length (buffer-list)))))
    (test-wttrin-geolocation-detect-teardown)))

(ert-deftest test-wttrin-geolocation-detect-error-unknown-provider-signals-error ()
  "Selecting an unknown provider signals a clear error."
  (let ((wttrin-geolocation-provider 'nonexistent-provider))
    (should-error (wttrin-geolocation-detect #'ignore) :type 'error)))

(provide 'test-wttrin-geolocation-detect)
;;; test-wttrin-geolocation-detect.el ends here
