;;; test-wttrin--build-url.el --- Tests for wttrin--build-url -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--build-url function.
;; Tests URL construction with proper encoding and parameters.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--build-url-normal-simple-location-returns-url ()
  "Test that simple location builds correct URL."
  (testutil-wttrin-with-unit-system nil
    (should (equal "https://wttr.in/Paris?A"
                   (wttrin--build-url "Paris")))))

(ert-deftest test-wttrin--build-url-normal-location-with-metric-returns-url ()
  "Test that location with metric unit system builds correct URL."
  (testutil-wttrin-with-unit-system "m"
    (should (equal "https://wttr.in/London?mA"
                   (wttrin--build-url "London")))))

(ert-deftest test-wttrin--build-url-normal-location-with-uscs-returns-url ()
  "Test that location with USCS unit system builds correct URL."
  (testutil-wttrin-with-unit-system "u"
    (should (equal "https://wttr.in/Berlin?uA"
                   (wttrin--build-url "Berlin")))))

;;; Boundary Cases

(ert-deftest test-wttrin--build-url-boundary-location-with-spaces-encodes-url ()
  "Test that location with spaces is properly URL-encoded."
  (testutil-wttrin-with-unit-system nil
    (should (equal "https://wttr.in/New%20York?A"
                   (wttrin--build-url "New York")))))

(ert-deftest test-wttrin--build-url-boundary-location-with-comma-encodes-url ()
  "Test that location with comma is properly URL-encoded."
  (testutil-wttrin-with-unit-system nil
    (should (equal "https://wttr.in/New%20York%2C%20NY?A"
                   (wttrin--build-url "New York, NY")))))

(ert-deftest test-wttrin--build-url-boundary-location-with-special-chars-encodes-url ()
  "Test that location with special characters is properly URL-encoded."
  (testutil-wttrin-with-unit-system "m"
    ;; ~Eiffel+Tower format is used by wttr.in for landmarks
    ;; ~ is an unreserved character (RFC 3986) and is not encoded
    ;; + is encoded as %2B
    (should (equal "https://wttr.in/~Eiffel%2BTower?mA"
                   (wttrin--build-url "~Eiffel+Tower")))))

(ert-deftest test-wttrin--build-url-boundary-unicode-location-encodes-url ()
  "Test that Unicode location is properly URL-encoded."
  (testutil-wttrin-with-unit-system nil
    ;; Unicode should be properly encoded
    (let ((result (wttrin--build-url "東京")))
      (should (string-prefix-p "https://wttr.in/" result))
      (should (string-suffix-p "?A" result))
      ;; Should contain URL-encoded Unicode
      (should (string-match-p "%[0-9A-F][0-9A-F]" result)))))

(ert-deftest test-wttrin--build-url-boundary-empty-location-returns-url ()
  "Test that empty location builds URL with empty query."
  (testutil-wttrin-with-unit-system nil
    (should (equal "https://wttr.in/?A"
                   (wttrin--build-url "")))))

(ert-deftest test-wttrin--build-url-boundary-gps-coordinates-encodes-url ()
  "Test that GPS coordinates are properly URL-encoded."
  (testutil-wttrin-with-unit-system nil
    ;; Format: -78.46,106.79
    (should (equal "https://wttr.in/-78.46%2C106.79?A"
                   (wttrin--build-url "-78.46,106.79")))))

;;; Error Cases

(ert-deftest test-wttrin--build-url-error-nil-location-signals-error ()
  "Test that nil location signals an error."
  (testutil-wttrin-with-unit-system nil
    (should-error (wttrin--build-url nil)
                  :type 'error)))

(provide 'test-wttrin--build-url)
;;; test-wttrin--build-url.el ends here
