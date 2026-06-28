;;; test-wttrin--fit-font-height.el --- Tests for font auto-fit math -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--fit-font-height, the pure helper that scales the
;; weather font so the fixed-width block fills the window, clamped to a floor
;; and cap.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Normal

(ert-deftest test-wttrin--fit-font-height-normal-exact-fit-keeps-height ()
  "Normal: a block that already fills the width keeps the current height.
50 cols x 12 px = 600 px = avail, ratio 1.0, so 130 stays 130."
  (should (= 130 (wttrin--fit-font-height 50 600 12 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-normal-wider-window-grows-height ()
  "Normal: a wider window grows the height proportionally.
avail 900 / block-px 600 = 1.5, so 130 -> 195, within [100,200]."
  (should (= 195 (wttrin--fit-font-height 50 900 12 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-normal-narrower-window-shrinks-height ()
  "Normal: a narrower window shrinks the height proportionally.
avail 480 / block-px 600 = 0.8, so 130 -> 104, within [100,200]."
  (should (= 104 (wttrin--fit-font-height 50 480 12 130 100 200))))

;;; Boundary

(ert-deftest test-wttrin--fit-font-height-boundary-clamps-to-cap ()
  "Boundary: a very wide window clamps the height to the cap."
  ;; avail 1800 / 600 = 3 -> 390 -> capped at 200.
  (should (= 200 (wttrin--fit-font-height 50 1800 12 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-boundary-clamps-to-floor ()
  "Boundary: a very narrow window clamps the height to the floor."
  ;; avail 120 / 600 = 0.2 -> 26 -> floored at 100.
  (should (= 100 (wttrin--fit-font-height 50 120 12 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-boundary-floor-equals-cap ()
  "Boundary: when floor equals cap, that value is forced."
  (should (= 150 (wttrin--fit-font-height 50 900 12 130 150 150))))

;;; Error

(ert-deftest test-wttrin--fit-font-height-error-zero-block-returns-clamped-current ()
  "Error: a zero block width returns the current height, clamped."
  (should (= 130 (wttrin--fit-font-height 0 900 12 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-error-zero-char-px-returns-clamped-current ()
  "Error: a zero per-character width returns the current height, clamped."
  (should (= 130 (wttrin--fit-font-height 50 900 0 130 100 200))))

(ert-deftest test-wttrin--fit-font-height-error-nil-avail-returns-clamped-current ()
  "Error: a nil available width returns the current height, clamped."
  (should (= 130 (wttrin--fit-font-height 50 nil 12 130 100 200))))

(provide 'test-wttrin--fit-font-height)
;;; test-wttrin--fit-font-height.el ends here
