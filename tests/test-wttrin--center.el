;;; test-wttrin--center.el --- Tests for weather-buffer centering -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the block-centering helpers: wttrin--block-width,
;; wttrin--center-margin, and the wttrin--center-buffer applier.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; wttrin--block-width — Normal

(ert-deftest test-wttrin--block-width-normal-returns-widest-line ()
  "Normal: returns the display width of the widest line."
  (should (= 4 (wttrin--block-width "ab\nabcd\nabc"))))

(ert-deftest test-wttrin--block-width-normal-counts-wide-chars ()
  "Normal: wide (double-width) characters count as two columns."
  ;; "你好" is two double-width chars = 4 columns, wider than the "a" line.
  (should (= 4 (wttrin--block-width "a\n你好"))))

(ert-deftest test-wttrin--block-width-normal-ignores-text-properties ()
  "Normal: text properties do not affect the measured width."
  (should (= 4 (wttrin--block-width (propertize "abcd" 'face 'bold)))))

;;; wttrin--block-width — Boundary

(ert-deftest test-wttrin--block-width-boundary-single-line ()
  "Boundary: a single line returns its own width."
  (should (= 5 (wttrin--block-width "hello"))))

(ert-deftest test-wttrin--block-width-boundary-empty-string-is-zero ()
  "Boundary: the empty string has width 0."
  (should (= 0 (wttrin--block-width ""))))

(ert-deftest test-wttrin--block-width-boundary-trailing-newline ()
  "Boundary: a trailing newline's empty final line does not inflate the width."
  (should (= 3 (wttrin--block-width "abc\n"))))

;;; wttrin--block-width — Error

(ert-deftest test-wttrin--block-width-error-nil-is-zero ()
  "Error: nil input returns 0 rather than signaling."
  (should (= 0 (wttrin--block-width nil))))

;;; wttrin--center-margin — Normal

(ert-deftest test-wttrin--center-margin-normal-centers-block ()
  "Normal: a block narrower than the window gets half the slack as margin."
  (should (= 30 (wttrin--center-margin 20 80))))

;;; wttrin--center-margin — Boundary

(ert-deftest test-wttrin--center-margin-boundary-equal-widths-zero ()
  "Boundary: a block exactly as wide as the window gets no margin."
  (should (= 0 (wttrin--center-margin 40 40))))

(ert-deftest test-wttrin--center-margin-boundary-block-wider-than-window-zero ()
  "Boundary: a block wider than the window gets no margin (never negative)."
  (should (= 0 (wttrin--center-margin 50 40))))

(ert-deftest test-wttrin--center-margin-boundary-odd-slack-floors ()
  "Boundary: an odd amount of slack floors to a whole column."
  (should (= 30 (wttrin--center-margin 20 81))))

(ert-deftest test-wttrin--center-margin-boundary-one-column-slack-zero ()
  "Boundary: one column of slack floors to 0."
  (should (= 0 (wttrin--center-margin 20 21))))

;;; wttrin--center-margin — Error

(ert-deftest test-wttrin--center-margin-error-non-integer-is-zero ()
  "Error: non-integer inputs return 0 rather than signaling."
  (should (= 0 (wttrin--center-margin nil 80)))
  (should (= 0 (wttrin--center-margin 20 nil))))

;;; wttrin--center-buffer — applier

(ert-deftest test-wttrin--center-buffer-normal-is-idempotent ()
  "Normal: centering a displayed buffer is stable — a second call does not move
the margin (the margin-shrinks-the-body feedback is handled)."
  (let ((buf (get-buffer-create "*wttrin-center-test*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "hi\nthere")))
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (wttrin--center-buffer)
            (let* ((win (get-buffer-window buf))
                   (first (or (car (window-margins win)) 0)))
              (wttrin--center-buffer)
              (should (= first (or (car (window-margins win)) 0))))))
      (kill-buffer buf))))

(ert-deftest test-wttrin--center-buffer-boundary-no-window-is-noop ()
  "Boundary: with no window for the buffer, centering is a no-op, no error."
  (with-temp-buffer
    (insert "content")
    (should-not (wttrin--center-buffer))))

(provide 'test-wttrin--center)
;;; test-wttrin--center.el ends here
