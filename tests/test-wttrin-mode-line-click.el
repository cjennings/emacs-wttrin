;;; test-wttrin-mode-line-click.el --- Tests for wttrin-mode-line-click -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-mode-line-click function.
;; Tests the left-click handler for the mode-line weather widget.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-mode-line-click-setup ()
  "Setup for mode-line-click tests."
  (testutil-wttrin-setup))

(defun test-wttrin-mode-line-click-teardown ()
  "Teardown for mode-line-click tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Normal Cases

(ert-deftest test-wttrin-mode-line-click-normal-opens-weather-for-favorite ()
  "Clicking the mode-line widget should open weather for the favorite location."
  (test-wttrin-mode-line-click-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "New Orleans, LA")
            (opened-location nil))
        (cl-letf (((symbol-function 'wttrin)
                   (lambda (location) (setq opened-location location))))
          (wttrin-mode-line-click)
          (should (equal opened-location "New Orleans, LA"))))
    (test-wttrin-mode-line-click-teardown)))

(ert-deftest test-wttrin-mode-line-click-normal-passes-exact-location-string ()
  "The exact favorite location string should be passed to wttrin, not modified."
  (test-wttrin-mode-line-click-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "São Paulo, BR")
            (opened-location nil))
        (cl-letf (((symbol-function 'wttrin)
                   (lambda (location) (setq opened-location location))))
          (wttrin-mode-line-click)
          (should (equal opened-location "São Paulo, BR"))))
    (test-wttrin-mode-line-click-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-mode-line-click-boundary-nil-location-is-noop ()
  "When no favorite location is configured, clicking should do nothing."
  (test-wttrin-mode-line-click-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (wttrin-called nil))
        (cl-letf (((symbol-function 'wttrin)
                   (lambda (_location) (setq wttrin-called t))))
          (wttrin-mode-line-click)
          (should-not wttrin-called)))
    (test-wttrin-mode-line-click-teardown)))

(provide 'test-wttrin-mode-line-click)
;;; test-wttrin-mode-line-click.el ends here
