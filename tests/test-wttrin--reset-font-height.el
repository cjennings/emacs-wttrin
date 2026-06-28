;;; test-wttrin--reset-font-height.el --- Tests for the base-font reset -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; wttrin--reset-font-height re-applies the base wttrin-font-height, used for the
;; loading placeholder so it does not keep the previous weather's auto-fitted
;; (possibly capped) size.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(ert-deftest test-wttrin--reset-font-height-normal-restores-base ()
  "Normal: resetting restores the applied height to the base wttrin-font-height."
  (with-temp-buffer
    (setq-local wttrin--face-remap-cookie
                (face-remap-add-relative 'default :height 200))
    (setq-local wttrin--current-font-height 200)
    (let ((wttrin-font-height 130))
      (wttrin--reset-font-height)
      (should (= 130 wttrin--current-font-height)))))

(ert-deftest test-wttrin--reset-font-height-boundary-no-cookie-is-noop ()
  "Boundary: with no remap cookie, reset is a no-op and does not signal."
  (with-temp-buffer
    (setq-local wttrin--face-remap-cookie nil)
    (setq-local wttrin--current-font-height 200)
    (wttrin--reset-font-height)
    (should (= 200 wttrin--current-font-height))))

(provide 'test-wttrin--reset-font-height)
;;; test-wttrin--reset-font-height.el ends here
