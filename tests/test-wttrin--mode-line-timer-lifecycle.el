;;; test-wttrin--mode-line-timer-lifecycle.el --- Mode-line timer lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Tests that disabling wttrin-mode-line-mode before the delayed startup fetch
;; fires cannot start the mode-line: the one-shot startup timer is cancelled and
;; the deferred after-init hook is removed, and the startup callback no-ops when
;; the mode is off.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

(ert-deftest test-wttrin--mode-line-fetch-weather-if-enabled-normal-runs-when-enabled ()
  "Normal: the delayed startup fetch runs when the mode is on."
  (let ((called nil))
    (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda (&rest _) (setq called t))))
      (let ((wttrin-mode-line-mode t))
        (wttrin--mode-line-fetch-weather-if-enabled)
        (should called)))))

(ert-deftest test-wttrin--mode-line-fetch-weather-if-enabled-error-skips-when-disabled ()
  "Error: the delayed startup fetch is skipped when the mode was turned off
before it fired, so a disabled mode never hits the network or mutates state."
  (let ((called nil))
    (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda (&rest _) (setq called t))))
      (let ((wttrin-mode-line-mode nil))
        (wttrin--mode-line-fetch-weather-if-enabled)
        (should-not called)))))

(ert-deftest test-wttrin--mode-line-stop-boundary-cancels-startup-timer-and-hook ()
  "Boundary: stop cancels a pending startup timer and removes the after-init
hook, so neither can start the mode-line after the user disabled it."
  (let ((wttrin--mode-line-timer nil)
        (wttrin--buffer-refresh-timer nil)
        (wttrin--mode-line-startup-timer (run-at-time 9999 nil #'ignore))
        (wttrin-mode-line-string nil)
        (wttrin--mode-line-cache nil)
        (wttrin--mode-line-rendered-stale nil))
    (add-hook 'after-init-hook #'wttrin--mode-line-start)
    (unwind-protect
        (progn
          (wttrin--mode-line-stop)
          (should (null wttrin--mode-line-startup-timer))
          (should-not (memq #'wttrin--mode-line-start after-init-hook)))
      (remove-hook 'after-init-hook #'wttrin--mode-line-start))))

(provide 'test-wttrin--mode-line-timer-lifecycle)
;;; test-wttrin--mode-line-timer-lifecycle.el ends here
