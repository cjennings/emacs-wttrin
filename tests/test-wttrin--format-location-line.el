;;; test-wttrin--format-location-line.el --- Tests for the buffer Location line -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for `wttrin--format-location-line', the "Location: ADDRESS" line
;; shown in the weather buffer when a geolocation command supplies an address.

;;; Code:

(require 'ert)
(require 'wttrin)

(ert-deftest test-wttrin--format-location-line-normal-builds-line ()
  "Normal: a non-empty address becomes a \"Location: ...\" line."
  (let ((line (wttrin--format-location-line "Westerly, Rhode Island, USA")))
    (should (stringp line))
    (should (string-prefix-p "Location: Westerly, Rhode Island, USA" line))))

(ert-deftest test-wttrin--format-location-line-normal-uses-face ()
  "Normal: the line carries the staleness-header face."
  (let ((line (wttrin--format-location-line "Westerly, RI")))
    (should (eq 'wttrin-staleness-header
                (get-text-property 0 'face line)))))

(ert-deftest test-wttrin--format-location-line-boundary-nil-returns-nil ()
  "Boundary: a nil address yields no line."
  (should (null (wttrin--format-location-line nil))))

(ert-deftest test-wttrin--format-location-line-boundary-empty-returns-nil ()
  "Boundary: an empty address yields no line."
  (should (null (wttrin--format-location-line ""))))

(provide 'test-wttrin--format-location-line)
;;; test-wttrin--format-location-line.el ends here
