;;; test-wttrin--replace-response-location.el --- Tests for wttrin--replace-response-location -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--replace-response-location function.
;; Tests that the API's lowercase location prefix gets replaced with the
;; user's original casing while preserving the weather data.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--replace-response-location-setup ()
  "Setup for replace-response-location tests."
  (testutil-wttrin-setup))

(defun test-wttrin--replace-response-location-teardown ()
  "Teardown for replace-response-location tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--replace-response-location-normal-replaces-lowercase ()
  "API's lowercase location should be replaced with user's original casing."
  (should (equal (wttrin--replace-response-location
                  "new orleans, la: ☀️ +72°F Clear"
                  "New Orleans, LA")
                 "New Orleans, LA: ☀️ +72°F Clear")))

(ert-deftest test-wttrin--replace-response-location-normal-preserves-weather-data ()
  "Everything after the colon should be identical to the original response."
  (let* ((response "berlin: 🌧 +48°F Light rain")
         (result (wttrin--replace-response-location response "Berlin")))
    ;; Weather data portion should be untouched
    (should (string-suffix-p ": 🌧 +48°F Light rain" result))))

;;; Boundary Cases

(ert-deftest test-wttrin--replace-response-location-boundary-already-correct ()
  "When API casing already matches user's, the string should be unchanged."
  (should (equal (wttrin--replace-response-location
                  "Paris: ☀️ +61°F Clear"
                  "Paris")
                 "Paris: ☀️ +61°F Clear")))

(ert-deftest test-wttrin--replace-response-location-boundary-unicode ()
  "Unicode location should be preserved correctly in the replacement."
  (should (equal (wttrin--replace-response-location
                  "são paulo: 🌤 +77°F Partly cloudy"
                  "São Paulo")
                 "São Paulo: 🌤 +77°F Partly cloudy")))

(ert-deftest test-wttrin--replace-response-location-boundary-no-colon ()
  "Response with no colon should be returned unchanged."
  (should (equal (wttrin--replace-response-location
                  "Unknown location"
                  "Paris")
                 "Unknown location")))

;;; Integration: fetch populates cache with corrected casing

(ert-deftest test-wttrin--mode-line-fetch-caches-user-location-casing ()
  "After a successful fetch, the cached string should use the user's
location casing, not the API's lowercase version."
  (test-wttrin--replace-response-location-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "New Orleans, LA")
            (wttrin--mode-line-cache nil))
        (testutil-wttrin-mock-http-response
            "new orleans, la: ☀️ +72°F Clear"
          (wttrin--mode-line-fetch-weather)
          (should wttrin--mode-line-cache)
          (let ((cached-string (cdr wttrin--mode-line-cache)))
            (should (string-prefix-p "New Orleans, LA:" cached-string)))))
    (test-wttrin--replace-response-location-teardown)))

(provide 'test-wttrin--replace-response-location)
;;; test-wttrin--replace-response-location.el ends here
