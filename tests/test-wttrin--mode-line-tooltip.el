;;; test-wttrin--mode-line-tooltip.el --- Tests for mode-line tooltip -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--mode-line-tooltip and dynamic help-echo behavior.
;; The tooltip should compute age at hover time, not at fetch time, so the
;; user sees accurate "Updated X ago" text regardless of when they hover.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-tooltip-setup ()
  "Setup for mode-line tooltip tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

(defun test-wttrin--mode-line-tooltip-teardown ()
  "Teardown for mode-line tooltip tests."
  (testutil-wttrin-teardown)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

;;; --------------------------------------------------------------------------
;;; wttrin--mode-line-tooltip
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-tooltip-normal-minutes-old ()
  "Cache that is 5 minutes old should report '5 minutes ago'."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1300.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (let ((tooltip (wttrin--mode-line-tooltip)))
          (should (string-match-p "5 minutes ago" tooltip))))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-normal-hours-old ()
  "Cache that is 2 hours old should report '2 hours ago'."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 8200.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (let ((tooltip (wttrin--mode-line-tooltip)))
          (should (string-match-p "2 hours ago" tooltip))))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-normal-includes-weather-string ()
  "Tooltip should include the full weather string so user sees conditions."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1060.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (let ((tooltip (wttrin--mode-line-tooltip)))
          (should (string-match-p "Paris" tooltip))
          (should (string-match-p "Clear" tooltip))))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-normal-stale-shows-stale-message ()
  "Data older than 2x refresh interval should show stale warning."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (let ((wttrin-mode-line-refresh-interval 900))
        (cl-letf (((symbol-function 'float-time) (lambda () 3000.0)))
          ;; Age is 2000s, threshold is 2*900=1800 → stale
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
          (let ((tooltip (wttrin--mode-line-tooltip)))
            (should (string-match-p "Stale" tooltip))
            (should (string-match-p "fetch failed" tooltip)))))
    (test-wttrin--mode-line-tooltip-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-tooltip-boundary-just-fetched ()
  "Cache less than 60 seconds old should say 'just now'."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1030.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (let ((tooltip (wttrin--mode-line-tooltip)))
          (should (string-match-p "just now" tooltip))))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-boundary-nil-cache ()
  "When cache is nil, tooltip function should return nil without crashing."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (progn
        (setq wttrin--mode-line-cache nil)
        (should-not (wttrin--mode-line-tooltip)))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-boundary-exactly-at-stale-threshold ()
  "Age exactly at 2x refresh interval should NOT be stale (threshold is >)."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (let ((wttrin-mode-line-refresh-interval 900))
        (cl-letf (((symbol-function 'float-time) (lambda () 2800.0)))
          ;; Age is exactly 1800s = 2*900 → NOT stale (> not >=)
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
          (let ((tooltip (wttrin--mode-line-tooltip)))
            (should (string-match-p "Updated" tooltip))
            (should-not (string-match-p "Stale" tooltip)))))
    (test-wttrin--mode-line-tooltip-teardown)))

;;; --------------------------------------------------------------------------
;;; Integration: dynamic help-echo
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--mode-line-tooltip-integration-help-echo-is-function ()
  "After update-display, help-echo should be a function, not a static string."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-update-display)
        (let ((help-echo (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (functionp help-echo))))
    (test-wttrin--mode-line-tooltip-teardown)))

(ert-deftest test-wttrin--mode-line-tooltip-integration-age-updates-over-time ()
  "Same cache, different times: tooltip should show different ages.
This is the bug reproduction — previously the tooltip was frozen at 'just now'."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (progn
        ;; Cache data at time 1000
        (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))

        ;; At time 1000: should say "just now"
        (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
          (wttrin--mode-line-update-display)
          (let* ((help-echo-fn (get-text-property 0 'help-echo wttrin-mode-line-string))
                 (tooltip (funcall help-echo-fn nil nil nil)))
            (should (string-match-p "just now" tooltip))))

        ;; At time 1720 (12 minutes later), WITHOUT re-fetching: should say "12 minutes ago"
        (cl-letf (((symbol-function 'float-time) (lambda () 1720.0)))
          (let* ((help-echo-fn (get-text-property 0 'help-echo wttrin-mode-line-string))
                 (tooltip (funcall help-echo-fn nil nil nil)))
            (should (string-match-p "12 minutes ago" tooltip)))))
    (test-wttrin--mode-line-tooltip-teardown)))

;;; Error Cases

(ert-deftest test-wttrin--mode-line-tooltip-error-cache-cleared-after-display ()
  "If cache is cleared after display was set, hovering should not crash."
  (test-wttrin--mode-line-tooltip-setup)
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'float-time) (lambda () 1000.0)))
          (setq wttrin--mode-line-cache (cons 1000.0 "Paris: ☀️ +61°F Clear"))
          (wttrin--mode-line-update-display))
        ;; Cache cleared (e.g., by wttrin--mode-line-stop)
        (setq wttrin--mode-line-cache nil)
        ;; Hovering over the now-stale mode-line string should not crash
        (let ((help-echo-fn (get-text-property 0 'help-echo wttrin-mode-line-string)))
          (should (functionp help-echo-fn))
          ;; Should return nil or a string, not crash
          (let ((result (funcall help-echo-fn nil nil nil)))
            (should (or (null result) (stringp result))))))
    (test-wttrin--mode-line-tooltip-teardown)))

(provide 'test-wttrin--mode-line-tooltip)
;;; test-wttrin--mode-line-tooltip.el ends here
