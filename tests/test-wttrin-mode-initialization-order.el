;;; test-wttrin-mode-initialization-order.el --- Test mode initialization order -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; This test verifies that wttrin--display-weather initializes wttrin-mode
;; BEFORE setting buffer-local variables. This prevents kill-all-local-variables
;; (called by derived modes) from wiping out important state like xterm-color--state.
;;
;; Bug context: On fresh Emacs launch, weather displayed with no colors because
;; xterm-color--state was set buffer-local BEFORE wttrin-mode was called, and
;; wttrin-mode's kill-all-local-variables wiped it out.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

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

(ert-deftest test-wttrin-mode-initialization-order-mode-before-buffer-local-vars ()
  "Test that wttrin-mode is activated before setting buffer-local variables.

This test verifies the fix for the color rendering bug where xterm-color--state
was wiped by kill-all-local-variables.

The test strategy:
1. Advise wttrin-mode to record when it's called
2. Advise setq-local to record when buffer-local vars are set
3. Call wttrin--display-weather
4. Verify wttrin-mode was called BEFORE any buffer-local vars were set"
  (test-wttrin-mode-init-setup)
  (unwind-protect
      (let ((mode-called-at nil)
            (first-setq-local-at nil)
            (call-counter 0))

        ;; Advise to track when wttrin-mode is called
        (cl-letf (((symbol-function 'wttrin-mode)
                   (let ((orig-fn (symbol-function 'wttrin-mode)))
                     (lambda ()
                       (setq mode-called-at (cl-incf call-counter))
                       (funcall orig-fn))))

                  ;; Advise to track first buffer-local variable set
                  ((symbol-function 'set)
                   (let ((orig-fn (symbol-function 'set)))
                     (lambda (symbol value)
                       ;; Track xterm-color--state specifically
                       (when (and (eq symbol 'xterm-color--state)
                                  (null first-setq-local-at))
                         (setq first-setq-local-at (cl-incf call-counter)))
                       (funcall orig-fn symbol value)))))

          (wttrin--display-weather "Paris" "Test weather data")

          ;; Verify mode was called
          (should mode-called-at)
          ;; Verify buffer-local var was set
          (should first-setq-local-at)
          ;; Critical: mode must be called BEFORE buffer-local var
          (should (< mode-called-at first-setq-local-at))))

    (test-wttrin-mode-init-teardown)))

(ert-deftest test-wttrin-mode-initialization-order-xterm-color-state-survives ()
  "Test that xterm-color--state remains buffer-local after wttrin--display-weather.

This verifies that the state isn't wiped by kill-all-local-variables."
  (test-wttrin-mode-init-setup)
  (unwind-protect
      (progn
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
