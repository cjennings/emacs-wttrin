;;; test-wttrin-geolocation--parse-ipapi.el --- Tests for ipapi.co response parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for `wttrin-geolocation--parse-ipapi'.  Pure function — no
;; network, no async.  Covers normal, boundary, and error cases per the
;; Normal/Boundary/Error discipline.

;;; Code:

(require 'ert)
(require 'wttrin-geolocation)

;;; Setup and Teardown

(defun test-wttrin-geolocation--parse-ipapi-setup ()
  "Setup for ipapi parser tests."
  nil)

(defun test-wttrin-geolocation--parse-ipapi-teardown ()
  "Teardown for ipapi parser tests."
  nil)

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--parse-ipapi-normal-full-response-returns-formatted-string ()
  "A full ipapi.co response yields \"City, Region\"."
  (let ((json "{\"ip\":\"1.2.3.4\",\"city\":\"Berkeley\",\"region\":\"California\",\"country_name\":\"United States\",\"latitude\":37.87,\"longitude\":-122.27}"))
    (should (string= "Berkeley, California"
                     (wttrin-geolocation--parse-ipapi json)))))

(ert-deftest test-wttrin-geolocation--parse-ipapi-normal-unicode-preserved ()
  "Unicode city and region names round-trip through the parser."
  (let ((json "{\"city\":\"São Paulo\",\"region\":\"São Paulo\"}"))
    (should (string= "São Paulo, São Paulo"
                     (wttrin-geolocation--parse-ipapi json)))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--parse-ipapi-boundary-extra-fields-ignored ()
  "Unknown fields in the response do not affect the parsed result."
  (let ((json "{\"city\":\"Paris\",\"region\":\"Ile-de-France\",\"currency\":\"EUR\",\"asn\":\"AS1234\"}"))
    (should (string= "Paris, Ile-de-France"
                     (wttrin-geolocation--parse-ipapi json)))))

(ert-deftest test-wttrin-geolocation--parse-ipapi-boundary-multi-word-city-preserved ()
  "Multi-word city names are preserved verbatim."
  (let ((json "{\"city\":\"New Orleans\",\"region\":\"Louisiana\"}"))
    (should (string= "New Orleans, Louisiana"
                     (wttrin-geolocation--parse-ipapi json)))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-nil-input-returns-nil ()
  "A nil input string returns nil."
  (should-not (wttrin-geolocation--parse-ipapi nil)))

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-empty-string-returns-nil ()
  "An empty input string returns nil."
  (should-not (wttrin-geolocation--parse-ipapi "")))

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-malformed-json-returns-nil ()
  "Malformed JSON returns nil rather than signalling."
  (should-not (wttrin-geolocation--parse-ipapi "{not valid json")))

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-missing-city-returns-nil ()
  "A response without a city field returns nil."
  (let ((json "{\"region\":\"California\"}"))
    (should-not (wttrin-geolocation--parse-ipapi json))))

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-missing-region-returns-nil ()
  "A response without a region field returns nil."
  (let ((json "{\"city\":\"Berkeley\"}"))
    (should-not (wttrin-geolocation--parse-ipapi json))))

(ert-deftest test-wttrin-geolocation--parse-ipapi-error-api-error-flag-returns-nil ()
  "An ipapi rate-limit / error response returns nil even if city/region are absent."
  (let ((json "{\"error\":true,\"reason\":\"RateLimited\",\"message\":\"wait\"}"))
    (should-not (wttrin-geolocation--parse-ipapi json))))

(provide 'test-wttrin-geolocation--parse-ipapi)
;;; test-wttrin-geolocation--parse-ipapi.el ends here
