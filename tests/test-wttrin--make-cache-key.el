;;; test-wttrin--make-cache-key.el --- Tests for wttrin--make-cache-key -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--make-cache-key function.
;; Tests cache key generation to ensure different configurations produce different keys.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--make-cache-key-normal-location-with-metric ()
  "Test cache key generation with metric unit system."
  (let ((wttrin-unit-system "m"))
    (should (string= "Paris|m" (wttrin--make-cache-key "Paris")))))

(ert-deftest test-wttrin--make-cache-key-normal-location-with-imperial ()
  "Test cache key generation with USCS/imperial unit system."
  (let ((wttrin-unit-system "u"))
    (should (string= "London|u" (wttrin--make-cache-key "London")))))

(ert-deftest test-wttrin--make-cache-key-normal-location-with-nil-system ()
  "Test cache key generation with nil unit system (default)."
  (let ((wttrin-unit-system nil))
    (should (string= "Tokyo|default" (wttrin--make-cache-key "Tokyo")))))

(ert-deftest test-wttrin--make-cache-key-normal-different-locations-different-keys ()
  "Test that different locations produce different cache keys."
  (let ((wttrin-unit-system "m"))
    (should-not (string= (wttrin--make-cache-key "Paris")
                        (wttrin--make-cache-key "London")))))

;;; Boundary Cases

(ert-deftest test-wttrin--make-cache-key-boundary-same-location-different-units ()
  "Test that same location with different unit systems produces different keys.
This is critical - cache misses would occur if keys were the same."
  (let* ((location "Paris")
         (key-metric (let ((wttrin-unit-system "m"))
                      (wttrin--make-cache-key location)))
         (key-imperial (let ((wttrin-unit-system "u"))
                        (wttrin--make-cache-key location)))
         (key-default (let ((wttrin-unit-system nil))
                       (wttrin--make-cache-key location))))
    (should-not (string= key-metric key-imperial))
    (should-not (string= key-metric key-default))
    (should-not (string= key-imperial key-default))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-special-chars ()
  "Test cache key generation with special characters in location."
  (let ((wttrin-unit-system "m"))
    (should (string= "São Paulo|m" (wttrin--make-cache-key "São Paulo")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-unicode ()
  "Test cache key generation with Unicode characters."
  (let ((wttrin-unit-system nil))
    (should (string= "北京|default" (wttrin--make-cache-key "北京")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-comma ()
  "Test cache key generation with comma in location (e.g., 'City, Country')."
  (let ((wttrin-unit-system "m"))
    (should (string= "London, GB|m" (wttrin--make-cache-key "London, GB")))))

(ert-deftest test-wttrin--make-cache-key-boundary-empty-string-location ()
  "Test cache key generation with empty string location."
  (let ((wttrin-unit-system "m"))
    (should (string= "|m" (wttrin--make-cache-key "")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-pipe-char ()
  "Test cache key with location containing pipe character (separator).
This could potentially cause cache key parsing issues."
  (let ((wttrin-unit-system "m"))
    (let ((key (wttrin--make-cache-key "Test|Location")))
      (should (string-match-p "|" key))
      ;; Should contain TWO pipe characters: one from location, one as separator
      (should (= 2 (cl-count ?| key))))))

;;; Error Cases

(ert-deftest test-wttrin--make-cache-key-error-consistent-keys ()
  "Test that calling with same inputs always produces same key (idempotent)."
  (let ((wttrin-unit-system "m"))
    (let ((key1 (wttrin--make-cache-key "Paris"))
          (key2 (wttrin--make-cache-key "Paris"))
          (key3 (wttrin--make-cache-key "Paris")))
      (should (string= key1 key2))
      (should (string= key2 key3)))))

(provide 'test-wttrin--make-cache-key)
;;; test-wttrin--make-cache-key.el ends here
