;;; test-wttrin-requery-force.el --- Tests for wttrin-requery-force -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-requery-force function.
;; Tests the force-refresh behavior that bypasses cache for the current location.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-requery-force-setup ()
  "Setup for requery-force tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-requery-force-teardown ()
  "Teardown for requery-force tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Normal Cases

(ert-deftest test-wttrin-requery-force-normal-queries-current-location ()
  "Force refresh should query weather for the buffer's current location."
  (test-wttrin-requery-force-setup)
  (unwind-protect
      (let ((queried-location nil))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (location) (setq queried-location location))))
          ;; Set up a weather buffer with a known location
          (with-current-buffer (get-buffer-create "*wttr.in*")
            (setq-local wttrin--current-location "Berlin, DE")
            (wttrin-requery-force)
            (should (equal queried-location "Berlin, DE")))))
    (test-wttrin-requery-force-teardown)))

(ert-deftest test-wttrin-requery-force-normal-sets-force-refresh-flag ()
  "The force-refresh flag should be true when the query executes."
  (test-wttrin-requery-force-setup)
  (unwind-protect
      (let ((force-refresh-was-set nil))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (_location)
                     (setq force-refresh-was-set wttrin--force-refresh))))
          (with-current-buffer (get-buffer-create "*wttr.in*")
            (setq-local wttrin--current-location "Paris")
            (wttrin-requery-force)
            (should force-refresh-was-set))))
    (test-wttrin-requery-force-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-requery-force-boundary-no-location-shows-message ()
  "When no current location is set, user should be told there's nothing to refresh."
  (test-wttrin-requery-force-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (with-current-buffer (get-buffer-create "*wttr.in*")
            ;; wttrin--current-location is nil (buffer-local default)
            (wttrin-requery-force)
            (should displayed-message)
            (should (string-match-p "No location" displayed-message)))))
    (test-wttrin-requery-force-teardown)))

(ert-deftest test-wttrin-requery-force-boundary-force-flag-does-not-leak ()
  "The force-refresh flag should not persist after the requery completes."
  (test-wttrin-requery-force-setup)
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'wttrin-query) (lambda (_location) nil)))
          (with-current-buffer (get-buffer-create "*wttr.in*")
            (setq-local wttrin--current-location "Paris")
            (wttrin-requery-force)))
        ;; After the let-binding unwinds, force-refresh should be back to nil
        (should-not wttrin--force-refresh))
    (test-wttrin-requery-force-teardown)))

(provide 'test-wttrin-requery-force)
;;; test-wttrin-requery-force.el ends here
