;;; test-wttrin--build-url.el --- Tests for wttrin--build-url -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--build-url function.
;; Tests URL construction from location queries with proper encoding and parameters.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--build-url-normal-simple-city ()
  "Test URL building with simple city name."
  (let ((wttrin-unit-system nil))
    (should (string= "https://wttr.in/Paris?A"
                     (wttrin--build-url "Paris")))))

(ert-deftest test-wttrin--build-url-normal-city-with-country ()
  "Test URL building with city and country code."
  (let ((wttrin-unit-system nil))
    (should (string= "https://wttr.in/London%2C%20GB?A"
                     (wttrin--build-url "London, GB")))))

(ert-deftest test-wttrin--build-url-normal-metric-system ()
  "Test URL building with metric unit system."
  (let ((wttrin-unit-system "m"))
    (should (string= "https://wttr.in/Tokyo?mA"
                     (wttrin--build-url "Tokyo")))))

(ert-deftest test-wttrin--build-url-normal-imperial-system ()
  "Test URL building with USCS/imperial unit system."
  (let ((wttrin-unit-system "u"))
    (should (string= "https://wttr.in/NewYork?uA"
                     (wttrin--build-url "NewYork")))))

;;; Boundary Cases

(ert-deftest test-wttrin--build-url-boundary-unicode-city-name ()
  "Test URL building with Unicode characters in city name."
  (let ((wttrin-unit-system nil))
    (let ((url (wttrin--build-url "北京")))  ; Beijing in Chinese
      (should (string-match-p "https://wttr.in/%[0-9A-F]+.*\\?A" url))
      (should (string-prefix-p "https://wttr.in/" url))
      (should (string-suffix-p "?A" url)))))

(ert-deftest test-wttrin--build-url-boundary-special-characters ()
  "Test URL building with special characters requiring encoding."
  (let ((wttrin-unit-system nil))
    (let ((url (wttrin--build-url "São Paulo")))
      (should (string-match-p "https://wttr.in/S%C3%A3o%20Paulo\\?A" url)))))

(ert-deftest test-wttrin--build-url-boundary-spaces-encoded ()
  "Test that spaces in query are properly URL-encoded."
  (let ((wttrin-unit-system nil))
    (let ((url (wttrin--build-url "New York")))
      (should (string= "https://wttr.in/New%20York?A" url))
      (should-not (string-match-p " " url)))))

(ert-deftest test-wttrin--build-url-boundary-airport-code ()
  "Test URL building with 3-letter airport code."
  (let ((wttrin-unit-system nil))
    (should (string= "https://wttr.in/SFO?A"
                     (wttrin--build-url "SFO")))))

(ert-deftest test-wttrin--build-url-boundary-gps-coordinates ()
  "Test URL building with GPS coordinates."
  (let ((wttrin-unit-system nil))
    (let ((url (wttrin--build-url "-78.46,106.79")))
      (should (string-match-p "https://wttr.in/-78\\.46.*106\\.79\\?A" url)))))

(ert-deftest test-wttrin--build-url-boundary-domain-name ()
  "Test URL building with domain name (wttr.in supports @domain).
The @ symbol should be URL-encoded as %40."
  (let ((wttrin-unit-system nil))
    (let ((url (wttrin--build-url "@github.com")))
      (should (string-prefix-p "https://wttr.in/" url))
      (should (string-match-p "%40github\\.com" url)))))

;;; Error Cases

(ert-deftest test-wttrin--build-url-error-nil-query-signals-error ()
  "Test that nil query signals an error."
  (let ((wttrin-unit-system nil))
    (should-error (wttrin--build-url nil)
                  :type 'error)))

(ert-deftest test-wttrin--build-url-error-nil-query-has-message ()
  "Test that nil query error has descriptive message."
  (let ((wttrin-unit-system nil))
    (condition-case err
        (wttrin--build-url nil)
      (error
       (should (string-match-p "cannot be nil" (error-message-string err)))))))

(provide 'test-wttrin--build-url)
;;; test-wttrin--build-url.el ends here
