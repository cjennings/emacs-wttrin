;;; test-wttrin--mode-line-stop.el --- Tests for wttrin--mode-line-stop -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--mode-line-stop function.
;; Tests that stopping mode-line display cleans up all state properly.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-stop-setup ()
  "Setup for mode-line-stop tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-timer nil)
  (setq wttrin--buffer-refresh-timer nil))

(defun test-wttrin--mode-line-stop-teardown ()
  "Teardown for mode-line-stop tests."
  (testutil-wttrin-teardown)
  ;; Cancel any real timers that may have been created
  (when (timerp wttrin--mode-line-timer)
    (cancel-timer wttrin--mode-line-timer))
  (when (timerp wttrin--buffer-refresh-timer)
    (cancel-timer wttrin--buffer-refresh-timer))
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-timer nil)
  (setq wttrin--buffer-refresh-timer nil))

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-stop-normal-clears-mode-line-string ()
  "After stop, mode-line-string should be nil so nothing shows in the mode-line."
  (test-wttrin--mode-line-stop-setup)
  (unwind-protect
      (progn
        (setq wttrin-mode-line-string "some weather display")
        (wttrin--mode-line-stop)
        (should-not wttrin-mode-line-string))
    (test-wttrin--mode-line-stop-teardown)))

(ert-deftest test-wttrin--mode-line-stop-normal-clears-mode-line-cache ()
  "After stop, cached mode-line data should be discarded."
  (test-wttrin--mode-line-stop-setup)
  (unwind-protect
      (progn
        (setq wttrin--mode-line-cache (cons (float-time) "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-stop)
        (should-not wttrin--mode-line-cache))
    (test-wttrin--mode-line-stop-teardown)))

(ert-deftest test-wttrin--mode-line-stop-normal-cancels-mode-line-timer ()
  "After stop, the mode-line refresh timer should be cancelled and nil."
  (test-wttrin--mode-line-stop-setup)
  (unwind-protect
      (progn
        ;; Create a real timer so cancel-timer has something to work with
        (setq wttrin--mode-line-timer
              (run-at-time 99999 nil #'ignore))
        (wttrin--mode-line-stop)
        (should-not wttrin--mode-line-timer))
    (test-wttrin--mode-line-stop-teardown)))

(ert-deftest test-wttrin--mode-line-stop-normal-cancels-buffer-refresh-timer ()
  "After stop, the buffer-refresh timer should be cancelled and nil."
  (test-wttrin--mode-line-stop-setup)
  (unwind-protect
      (progn
        (setq wttrin--buffer-refresh-timer
              (run-at-time 99999 nil #'ignore))
        (wttrin--mode-line-stop)
        (should-not wttrin--buffer-refresh-timer))
    (test-wttrin--mode-line-stop-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-stop-boundary-safe-when-already-stopped ()
  "Calling stop when everything is already nil should not error."
  (test-wttrin--mode-line-stop-setup)
  (unwind-protect
      (progn
        ;; Everything is already nil from setup
        (should-not wttrin--mode-line-timer)
        (should-not wttrin--buffer-refresh-timer)
        (should-not wttrin-mode-line-string)
        (should-not wttrin--mode-line-cache)
        ;; This should not error
        (wttrin--mode-line-stop)
        ;; Still nil
        (should-not wttrin--mode-line-timer)
        (should-not wttrin-mode-line-string))
    (test-wttrin--mode-line-stop-teardown)))

(provide 'test-wttrin--mode-line-stop)
;;; test-wttrin--mode-line-stop.el ends here
