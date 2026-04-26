;;; test-wttrin-geolocation--internals.el --- Tests for geolocation internal helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for the four pure helpers used by the geolocation parsers:
;;   - wttrin-geolocation--decode-json
;;   - wttrin-geolocation--format-city-region
;;   - wttrin-geolocation--lookup-provider
;;   - wttrin-geolocation--extract-body
;;
;; These functions are exercised indirectly through the parser tests
;; (test-wttrin-geolocation--parse-ipapi.el and friends), but were not
;; covered directly.  This file fills the gap so each helper's edge
;; cases are locked in isolation.

;;; Code:

(require 'ert)
(require 'wttrin-geolocation)

;;; --------------------------------------------------------------------------
;;; wttrin-geolocation--decode-json
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--decode-json-normal-valid-object ()
  "Valid JSON object decodes to an alist with symbol keys."
  (let ((result (wttrin-geolocation--decode-json "{\"city\":\"Paris\",\"region\":\"IDF\"}")))
    (should (equal (cdr (assq 'city result)) "Paris"))
    (should (equal (cdr (assq 'region result)) "IDF"))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--decode-json-boundary-empty-object ()
  "Empty JSON object decodes to nil (an empty alist)."
  (should (equal (wttrin-geolocation--decode-json "{}") nil)))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--decode-json-error-nil-input ()
  "Nil input returns nil with no signal."
  (should-not (wttrin-geolocation--decode-json nil)))

(ert-deftest test-wttrin-geolocation--decode-json-error-empty-string ()
  "Empty string returns nil with no signal."
  (should-not (wttrin-geolocation--decode-json "")))

(ert-deftest test-wttrin-geolocation--decode-json-error-malformed-json ()
  "Malformed JSON returns nil with no signal."
  (should-not (wttrin-geolocation--decode-json "not valid json {")))

;;; --------------------------------------------------------------------------
;;; wttrin-geolocation--format-city-region
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--format-city-region-normal-both-present ()
  "City and region both present produces \"City, Region\"."
  (let ((data '((city . "Paris") (region . "Île-de-France"))))
    (should (equal (wttrin-geolocation--format-city-region data)
                   "Paris, Île-de-France"))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--format-city-region-boundary-empty-city ()
  "Empty city string returns nil even when region is present."
  (let ((data '((city . "") (region . "Île-de-France"))))
    (should-not (wttrin-geolocation--format-city-region data))))

(ert-deftest test-wttrin-geolocation--format-city-region-boundary-empty-region ()
  "Empty region string returns nil even when city is present."
  (let ((data '((city . "Paris") (region . ""))))
    (should-not (wttrin-geolocation--format-city-region data))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--format-city-region-error-missing-city-key ()
  "Missing city key returns nil."
  (let ((data '((region . "Île-de-France"))))
    (should-not (wttrin-geolocation--format-city-region data))))

(ert-deftest test-wttrin-geolocation--format-city-region-error-missing-region-key ()
  "Missing region key returns nil."
  (let ((data '((city . "Paris"))))
    (should-not (wttrin-geolocation--format-city-region data))))

(ert-deftest test-wttrin-geolocation--format-city-region-error-nil-data ()
  "Nil data returns nil with no signal."
  (should-not (wttrin-geolocation--format-city-region nil)))

;;; --------------------------------------------------------------------------
;;; wttrin-geolocation--lookup-provider
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--lookup-provider-normal-builtin-ipapi ()
  "Built-in ipapi provider returns its plist with :name, :url, :parser."
  (let ((result (wttrin-geolocation--lookup-provider 'ipapi)))
    (should (equal (plist-get result :name) "ipapi.co"))
    (should (stringp (plist-get result :url)))
    (should (eq (plist-get result :parser) 'wttrin-geolocation--parse-ipapi))))

(ert-deftest test-wttrin-geolocation--lookup-provider-normal-builtin-ipinfo ()
  "Built-in ipinfo provider returns its plist."
  (let ((result (wttrin-geolocation--lookup-provider 'ipinfo)))
    (should (equal (plist-get result :name) "ipinfo.io"))
    (should (eq (plist-get result :parser) 'wttrin-geolocation--parse-ipinfo))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--lookup-provider-boundary-custom-registered ()
  "User-registered provider is reachable via lookup."
  (let ((wttrin-geolocation--providers
         (cons '(test-provider
                 :name "Test"
                 :url "https://example.invalid/"
                 :parser ignore)
               wttrin-geolocation--providers)))
    (let ((result (wttrin-geolocation--lookup-provider 'test-provider)))
      (should (equal (plist-get result :name) "Test"))
      (should (eq (plist-get result :parser) 'ignore)))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--lookup-provider-error-unknown-symbol ()
  "Unknown provider symbol signals error."
  (should-error (wttrin-geolocation--lookup-provider 'definitely-not-registered)
                :type 'error))

;;; --------------------------------------------------------------------------
;;; wttrin-geolocation--extract-body
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--extract-body-normal-200-with-body ()
  "200 OK response with CRLF separator returns the body."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"city\":\"Paris\"}")
    (should (equal (wttrin-geolocation--extract-body)
                   "{\"city\":\"Paris\"}"))))

(ert-deftest test-wttrin-geolocation--extract-body-normal-utf8-body ()
  "UTF-8 body bytes are decoded correctly."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (insert (encode-coding-string "{\"city\":\"São Paulo\"}" 'utf-8))
    (set-buffer-multibyte t)
    (should (equal (wttrin-geolocation--extract-body)
                   "{\"city\":\"São Paulo\"}"))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--extract-body-boundary-empty-body ()
  "200 response with empty body returns the empty string."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\n")
    (should (equal (wttrin-geolocation--extract-body) ""))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--extract-body-error-404 ()
  "404 response returns nil."
  (with-temp-buffer
    (insert "HTTP/1.1 404 Not Found\r\n\r\nNot Found")
    (should-not (wttrin-geolocation--extract-body))))

(ert-deftest test-wttrin-geolocation--extract-body-error-500 ()
  "500 response returns nil."
  (with-temp-buffer
    (insert "HTTP/1.1 500 Internal Server Error\r\n\r\noops")
    (should-not (wttrin-geolocation--extract-body))))

(ert-deftest test-wttrin-geolocation--extract-body-error-missing-separator ()
  "Response without a blank-line separator returns nil."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nbody-without-separator")
    (should-not (wttrin-geolocation--extract-body))))

(provide 'test-wttrin-geolocation--internals)
;;; test-wttrin-geolocation--internals.el ends here
