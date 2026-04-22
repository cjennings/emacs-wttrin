;;; test-wttrin-geolocation--parse-ipwhois.el --- Tests for ipwho.is response parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for `wttrin-geolocation--parse-ipwhois'.  Pure function — no
;; network, no async.  The ipwho.is response carries a `success' flag which
;; is false on rate-limit / lookup failure even when the response is
;; well-formed JSON.

;;; Code:

(require 'ert)
(require 'wttrin-geolocation)

;;; Setup and Teardown

(defun test-wttrin-geolocation--parse-ipwhois-setup ()
  "Setup for ipwho.is parser tests."
  nil)

(defun test-wttrin-geolocation--parse-ipwhois-teardown ()
  "Teardown for ipwho.is parser tests."
  nil)

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--parse-ipwhois-normal-success-response-returns-formatted-string ()
  "A successful ipwho.is response yields \"City, Region\"."
  (let ((json "{\"ip\":\"8.8.8.8\",\"success\":true,\"type\":\"IPv4\",\"country\":\"United States\",\"country_code\":\"US\",\"region\":\"California\",\"region_code\":\"CA\",\"city\":\"Mountain View\",\"latitude\":37.386,\"longitude\":-122.0838}"))
    (should (string= "Mountain View, California"
                     (wttrin-geolocation--parse-ipwhois json)))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--parse-ipwhois-boundary-unicode-preserved ()
  "Unicode city and region names round-trip through the parser."
  (let ((json "{\"success\":true,\"city\":\"München\",\"region\":\"Bayern\"}"))
    (should (string= "München, Bayern"
                     (wttrin-geolocation--parse-ipwhois json)))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--parse-ipwhois-error-nil-input-returns-nil ()
  "A nil input string returns nil."
  (should-not (wttrin-geolocation--parse-ipwhois nil)))

(ert-deftest test-wttrin-geolocation--parse-ipwhois-error-success-false-returns-nil ()
  "A response with `success: false' returns nil even if city/region are present."
  (let ((json "{\"success\":false,\"message\":\"rate limit reached\",\"city\":\"Anywhere\",\"region\":\"Anywhere\"}"))
    (should-not (wttrin-geolocation--parse-ipwhois json))))

(ert-deftest test-wttrin-geolocation--parse-ipwhois-error-missing-city-returns-nil ()
  "A success response without a city field returns nil."
  (let ((json "{\"success\":true,\"region\":\"California\"}"))
    (should-not (wttrin-geolocation--parse-ipwhois json))))

(ert-deftest test-wttrin-geolocation--parse-ipwhois-error-malformed-json-returns-nil ()
  "Malformed JSON returns nil rather than signalling."
  (should-not (wttrin-geolocation--parse-ipwhois "{\"success\":true,\"city\":")))

(provide 'test-wttrin-geolocation--parse-ipwhois)
;;; test-wttrin-geolocation--parse-ipwhois.el ends here
