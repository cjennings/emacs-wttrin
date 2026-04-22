;;; test-wttrin-geolocation--parse-ipinfo.el --- Tests for ipinfo.io response parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for `wttrin-geolocation--parse-ipinfo'.  Pure function — no
;; network, no async.

;;; Code:

(require 'ert)
(require 'wttrin-geolocation)

;;; Setup and Teardown

(defun test-wttrin-geolocation--parse-ipinfo-setup ()
  "Setup for ipinfo parser tests."
  nil)

(defun test-wttrin-geolocation--parse-ipinfo-teardown ()
  "Teardown for ipinfo parser tests."
  nil)

;;; Normal Cases

(ert-deftest test-wttrin-geolocation--parse-ipinfo-normal-full-response-returns-formatted-string ()
  "A full ipinfo.io response yields \"City, Region\"."
  (let ((json "{\"ip\":\"8.8.8.8\",\"city\":\"Mountain View\",\"region\":\"California\",\"country\":\"US\",\"loc\":\"37.3860,-122.0838\",\"org\":\"AS15169 Google LLC\",\"postal\":\"94035\",\"timezone\":\"America/Los_Angeles\"}"))
    (should (string= "Mountain View, California"
                     (wttrin-geolocation--parse-ipinfo json)))))

;;; Boundary Cases

(ert-deftest test-wttrin-geolocation--parse-ipinfo-boundary-minimal-fields-works ()
  "A response with only city and region parses correctly."
  (let ((json "{\"city\":\"Berlin\",\"region\":\"Berlin\"}"))
    (should (string= "Berlin, Berlin"
                     (wttrin-geolocation--parse-ipinfo json)))))

;;; Error Cases

(ert-deftest test-wttrin-geolocation--parse-ipinfo-error-nil-input-returns-nil ()
  "A nil input string returns nil."
  (should-not (wttrin-geolocation--parse-ipinfo nil)))

(ert-deftest test-wttrin-geolocation--parse-ipinfo-error-malformed-json-returns-nil ()
  "Malformed JSON returns nil rather than signalling."
  (should-not (wttrin-geolocation--parse-ipinfo "not json at all")))

(ert-deftest test-wttrin-geolocation--parse-ipinfo-error-missing-city-returns-nil ()
  "A response without a city field returns nil."
  (let ((json "{\"region\":\"California\",\"country\":\"US\"}"))
    (should-not (wttrin-geolocation--parse-ipinfo json))))

(ert-deftest test-wttrin-geolocation--parse-ipinfo-error-missing-region-returns-nil ()
  "A response without a region field returns nil."
  (let ((json "{\"city\":\"Mountain View\",\"country\":\"US\"}"))
    (should-not (wttrin-geolocation--parse-ipinfo json))))

(provide 'test-wttrin-geolocation--parse-ipinfo)
;;; test-wttrin-geolocation--parse-ipinfo.el ends here
