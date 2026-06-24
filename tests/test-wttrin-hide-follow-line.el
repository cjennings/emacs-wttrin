;;; test-wttrin-hide-follow-line.el --- Tests for hiding the follow line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--effective-display-options and its effect on
;; wttrin--build-url: the wttr.in F flag is added to the buffer request when
;; wttrin-hide-follow-line is non-nil, composing with wttrin-display-options.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; --------------------------------------------------------------------------
;;; wttrin--effective-display-options
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--effective-display-options-normal-hide-empty-adds-f ()
  "Normal: hiding with no other options yields just the F flag."
  (let ((wttrin-display-options nil)
        (wttrin-hide-follow-line t))
    (should (equal (wttrin--effective-display-options) "F"))))

(ert-deftest test-wttrin--effective-display-options-normal-hide-appends-f ()
  "Normal: hiding appends F to existing options."
  (let ((wttrin-display-options "0q")
        (wttrin-hide-follow-line t))
    (should (equal (wttrin--effective-display-options) "0qF"))))

(ert-deftest test-wttrin--effective-display-options-normal-no-hide-unchanged ()
  "Normal: not hiding leaves the options untouched."
  (let ((wttrin-display-options "0q")
        (wttrin-hide-follow-line nil))
    (should (equal (wttrin--effective-display-options) "0q"))))

;;; Boundary Cases

(ert-deftest test-wttrin--effective-display-options-boundary-f-already-present ()
  "Boundary: F already in the options is not duplicated."
  (let ((wttrin-display-options "0Fq")
        (wttrin-hide-follow-line t))
    (should (equal (wttrin--effective-display-options) "0Fq"))))

(ert-deftest test-wttrin--effective-display-options-boundary-no-hide-nil-options ()
  "Boundary: not hiding with nil options yields an empty string."
  (let ((wttrin-display-options nil)
        (wttrin-hide-follow-line nil))
    (should (equal (wttrin--effective-display-options) ""))))

;;; --------------------------------------------------------------------------
;;; wttrin--build-url integration
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--build-url-normal-hide-appends-f-to-url ()
  "Normal: hiding the follow line puts the F flag at the end of the URL."
  (let ((wttrin-display-options nil)
        (wttrin-hide-follow-line t)
        (wttrin-unit-system nil))
    (should (string-suffix-p "F" (wttrin--build-url "Tokyo")))))

(ert-deftest test-wttrin--build-url-normal-no-hide-no-trailing-f ()
  "Normal: with hiding off, the URL does not end in the F flag."
  (let ((wttrin-display-options nil)
        (wttrin-hide-follow-line nil)
        (wttrin-unit-system nil))
    (should-not (string-suffix-p "F" (wttrin--build-url "Tokyo")))))

(provide 'test-wttrin-hide-follow-line)
;;; test-wttrin-hide-follow-line.el ends here
