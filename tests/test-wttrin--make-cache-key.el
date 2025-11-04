;;; test-wttrin--make-cache-key.el --- Tests for wttrin--make-cache-key -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--make-cache-key function.
;; Tests cache key generation from location and unit system.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--make-cache-key-normal-location-with-metric-returns-key ()
  "Test that location with metric unit system creates correct cache key."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "Paris|m" (wttrin--make-cache-key "Paris")))))

(ert-deftest test-wttrin--make-cache-key-normal-location-with-uscs-returns-key ()
  "Test that location with USCS unit system creates correct cache key."
  (testutil-wttrin-with-unit-system "u"
    (should (equal "New York|u" (wttrin--make-cache-key "New York")))))

(ert-deftest test-wttrin--make-cache-key-normal-location-no-unit-returns-default ()
  "Test that location with no unit system uses default in cache key."
  (testutil-wttrin-with-unit-system nil
    (should (equal "London|default" (wttrin--make-cache-key "London")))))

;;; Boundary Cases

(ert-deftest test-wttrin--make-cache-key-boundary-empty-location-returns-key ()
  "Test that empty location string creates cache key."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "|m" (wttrin--make-cache-key "")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-spaces-returns-key ()
  "Test that location with spaces creates correct cache key."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "New York, NY|m" (wttrin--make-cache-key "New York, NY")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-commas-returns-key ()
  "Test that location with commas creates correct cache key."
  (testutil-wttrin-with-unit-system nil
    (should (equal "Berlin, DE|default" (wttrin--make-cache-key "Berlin, DE")))))

(ert-deftest test-wttrin--make-cache-key-boundary-unicode-location-returns-key ()
  "Test that Unicode location creates correct cache key."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "東京|m" (wttrin--make-cache-key "東京")))))

(ert-deftest test-wttrin--make-cache-key-boundary-location-with-special-chars-returns-key ()
  "Test that location with special characters creates cache key."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "~Eiffel+Tower|m" (wttrin--make-cache-key "~Eiffel+Tower")))))

;;; Error Cases

(ert-deftest test-wttrin--make-cache-key-error-nil-location-returns-key ()
  "Test that nil location creates cache key with empty string."
  ;; Note: concat converts nil to empty string, this documents current behavior
  (testutil-wttrin-with-unit-system "m"
    (should (equal "|m" (wttrin--make-cache-key nil)))))

(provide 'test-wttrin--make-cache-key)
;;; test-wttrin--make-cache-key.el ends here
