;;; test-wttrin-mode-line-force-refresh.el --- Tests for wttrin-mode-line-force-refresh -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-mode-line-force-refresh function.
;; Tests the right-click handler that force-refreshes mode-line weather.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-mode-line-force-refresh-setup ()
  "Setup for mode-line-force-refresh tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

(defun test-wttrin-mode-line-force-refresh-teardown ()
  "Teardown for mode-line-force-refresh tests."
  (testutil-wttrin-teardown)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

;;; Normal Cases

(ert-deftest test-wttrin-mode-line-force-refresh-normal-calls-fetch ()
  "Right-click should trigger a weather fetch."
  (test-wttrin-mode-line-force-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (fetch-called nil))
        (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
                   (lambda () (setq fetch-called t))))
          (wttrin-mode-line-force-refresh)
          (should fetch-called)))
    (test-wttrin-mode-line-force-refresh-teardown)))

(ert-deftest test-wttrin-mode-line-force-refresh-normal-sets-force-flag ()
  "The fetch should run with force-refresh bound to t to bypass cache."
  (test-wttrin-mode-line-force-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (force-flag-during-fetch nil))
        (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
                   (lambda () (setq force-flag-during-fetch wttrin--force-refresh))))
          (wttrin-mode-line-force-refresh)
          (should force-flag-during-fetch)))
    (test-wttrin-mode-line-force-refresh-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-mode-line-force-refresh-boundary-nil-location-is-noop ()
  "When no favorite location is set, right-click should do nothing."
  (test-wttrin-mode-line-force-refresh-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (fetch-called nil))
        (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
                   (lambda () (setq fetch-called t))))
          (wttrin-mode-line-force-refresh)
          (should-not fetch-called)))
    (test-wttrin-mode-line-force-refresh-teardown)))

(provide 'test-wttrin-mode-line-force-refresh)
;;; test-wttrin-mode-line-force-refresh.el ends here
