;;; test-wttrin-mode-initialization-order.el --- Test mode initialization order -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; This test verifies that wttrin--display-weather initializes wttrin-mode
;; BEFORE making `xterm-color--state' buffer-local.  This prevents
;; kill-all-local-variables (called by derived modes) from wiping out
;; important state.
;;
;; Bug context: On fresh Emacs launch, weather displayed with no colors because
;; xterm-color--state was set buffer-local BEFORE wttrin-mode was called, and
;; wttrin-mode's kill-all-local-variables wiped it out.
;;
;; Earlier versions of this file mocked the `set' primitive to observe the
;; ordering.  That was fragile because `setq-local' doesn't always go through
;; the `set' function -- the byte-code path differs across Emacs versions, and
;; on Emacs master the mock missed the assignment entirely.  This rewrite uses
;; `advice-add' on `wttrin-mode' and `make-local-variable', both of which are
;; ordinary advisable functions guaranteed to be invoked on every code path
;; that defines a buffer-local binding.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

(defvar test-wttrin--init-events nil
  "Ordered list of init events captured during the order test.
Each entry is `mode' or `xterm-color', recorded by advice on
`wttrin-mode' and `make-local-variable' respectively.  Reset before
each test run.")

(defun test-wttrin--record-mode-call (&rest _args)
  "Advice that records a `mode' event when `wttrin-mode' runs."
  (push 'mode test-wttrin--init-events))

(defun test-wttrin--record-mlv-call (symbol &rest _args)
  "Advice that records an `xterm-color' event when SYMBOL is the target.
Only triggers for `xterm-color--state' so unrelated buffer-local
declarations don't pollute the timeline."
  (when (eq symbol 'xterm-color--state)
    (push 'xterm-color test-wttrin--init-events)))

;;; Setup and Teardown

(defun test-wttrin-mode-init-setup ()
  "Setup for mode initialization tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-mode-init-teardown ()
  "Teardown for mode initialization tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Tests

(ert-deftest test-wttrin-mode-initialization-order-normal-mode-runs-before-xterm-color-state-binding ()
  "Verify wttrin-mode runs before xterm-color--state is made buffer-local.

If `make-local-variable' for `xterm-color--state' ran first, the subsequent
`wttrin-mode' call would invoke `kill-all-local-variables' and wipe the
state, leaving the rendered buffer without ANSI colors.  Observing the
ordering directly catches a regression that re-introduces the bug even if
the outcome test happens to pass on a particular Emacs version."
  (test-wttrin-mode-init-setup)
  (setq test-wttrin--init-events nil)
  (advice-add 'wttrin-mode :before #'test-wttrin--record-mode-call)
  (advice-add 'make-local-variable :before #'test-wttrin--record-mlv-call)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (wttrin--display-weather "Paris" "Test weather data")
        (let ((order (nreverse test-wttrin--init-events)))
          (should (memq 'mode order))
          (should (memq 'xterm-color order))
          (should (< (cl-position 'mode order)
                     (cl-position 'xterm-color order)))))
    (advice-remove 'wttrin-mode #'test-wttrin--record-mode-call)
    (advice-remove 'make-local-variable #'test-wttrin--record-mlv-call)
    (test-wttrin-mode-init-teardown)))

(ert-deftest test-wttrin-mode-initialization-order-normal-xterm-color-state-survives-mode-init ()
  "Test that xterm-color--state remains buffer-local after wttrin--display-weather.

This verifies that the state isn't wiped by kill-all-local-variables."
  (test-wttrin-mode-init-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (wttrin--display-weather "London" "Test data")

        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; xterm-color--state should be buffer-local
          (should (local-variable-p 'xterm-color--state))
          ;; It should have the correct value
          (should (eq xterm-color--state :char))))

    (test-wttrin-mode-init-teardown)))

(provide 'test-wttrin-mode-initialization-order)
;;; test-wttrin-mode-initialization-order.el ends here
