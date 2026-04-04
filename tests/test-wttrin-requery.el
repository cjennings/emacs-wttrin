;;; test-wttrin-requery.el --- Tests for wttrin-requery -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--requery-location and wttrin-requery.
;; wttrin--requery-location holds the core logic (kill buffer, query new location).
;; wttrin-requery is the interactive wrapper that adds completing-read.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-requery-setup ()
  "Setup for requery tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-requery-teardown ()
  "Teardown for requery tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; --------------------------------------------------------------------------
;;; wttrin--requery-location (core logic)
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--requery-location-normal-kills-existing-buffer ()
  "Requerying should kill the existing *wttr.in* buffer before opening a new one."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((old-buffer (get-buffer-create "*wttr.in*")))
        (cl-letf (((symbol-function 'wttrin-query) (lambda (_loc) nil)))
          (wttrin--requery-location "Tokyo")
          ;; Old buffer should be dead
          (should-not (buffer-live-p old-buffer))))
    (test-wttrin-requery-teardown)))

(ert-deftest test-wttrin--requery-location-normal-queries-new-location ()
  "Requerying should fetch weather for the newly specified location."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((queried-location nil))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (loc) (setq queried-location loc))))
          (wttrin--requery-location "Berlin, DE")
          (should (equal queried-location "Berlin, DE"))))
    (test-wttrin-requery-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--requery-location-boundary-no-existing-buffer ()
  "Requerying when no weather buffer exists should still query the new location."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((queried-location nil))
        ;; Ensure no buffer exists
        (should-not (get-buffer "*wttr.in*"))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (loc) (setq queried-location loc))))
          (wttrin--requery-location "Paris")
          (should (equal queried-location "Paris"))))
    (test-wttrin-requery-teardown)))

(ert-deftest test-wttrin--requery-location-boundary-unicode-location ()
  "Requerying with unicode characters should pass them through unchanged."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((queried-location nil))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (loc) (setq queried-location loc))))
          (wttrin--requery-location "Zürich, CH")
          (should (equal queried-location "Zürich, CH"))))
    (test-wttrin-requery-teardown)))

;;; --------------------------------------------------------------------------
;;; wttrin-requery (interactive wrapper)
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-requery-normal-uses-selected-location ()
  "The interactive command should pass the user's completing-read selection
to the core requery function."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((requeried-location nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _collection &rest _args) "London, GB"))
                  ((symbol-function 'wttrin--requery-location)
                   (lambda (loc) (setq requeried-location loc))))
          (wttrin-requery)
          (should (equal requeried-location "London, GB"))))
    (test-wttrin-requery-teardown)))

(ert-deftest test-wttrin-requery-normal-offers-default-locations ()
  "Completing-read should be called with wttrin-default-locations as candidates."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((offered-collection nil)
            (wttrin-default-locations '("Paris" "London" "Tokyo")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _args)
                     (setq offered-collection collection)
                     "Paris"))
                  ((symbol-function 'wttrin--requery-location)
                   (lambda (_loc) nil)))
          (wttrin-requery)
          (should (equal offered-collection '("Paris" "London" "Tokyo")))))
    (test-wttrin-requery-teardown)))

(ert-deftest test-wttrin-requery-boundary-single-default-prefills ()
  "When only one default location exists, it should be pre-filled in the prompt."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((initial-input nil)
            (wttrin-default-locations '("Solo City")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _collection _predicate _require-match init &rest _args)
                     (setq initial-input init)
                     "Solo City"))
                  ((symbol-function 'wttrin--requery-location)
                   (lambda (_loc) nil)))
          (wttrin-requery)
          (should (equal initial-input "Solo City"))))
    (test-wttrin-requery-teardown)))

(ert-deftest test-wttrin-requery-boundary-multiple-defaults-no-prefill ()
  "When multiple default locations exist, nothing should be pre-filled."
  (test-wttrin-requery-setup)
  (unwind-protect
      (let ((initial-input 'not-called)
            (wttrin-default-locations '("Paris" "London")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _collection _predicate _require-match init &rest _args)
                     (setq initial-input init)
                     "Paris"))
                  ((symbol-function 'wttrin--requery-location)
                   (lambda (_loc) nil)))
          (wttrin-requery)
          (should-not initial-input)))
    (test-wttrin-requery-teardown)))

(provide 'test-wttrin-requery)
;;; test-wttrin-requery.el ends here
