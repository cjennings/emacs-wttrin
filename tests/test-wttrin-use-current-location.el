;;; test-wttrin-use-current-location.el --- Tests for wttrin-use-current-location -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the `wttrin-use-current-location' command, the labeled way to
;; set `wttrin-favorite-location' to t (always auto-detect) instead of typing
;; the bare symbol.  Mocks `yes-or-no-p' and `message'; touches no network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)

;;; Setup and Teardown

(defvar test-wttrin-use-current-location--saved nil
  "Snapshot of `wttrin-favorite-location' restored in teardown.")

(defun test-wttrin-use-current-location-setup ()
  "Snapshot `wttrin-favorite-location' and clear it."
  (setq test-wttrin-use-current-location--saved wttrin-favorite-location)
  (setq wttrin-favorite-location nil))

(defun test-wttrin-use-current-location-teardown ()
  "Restore `wttrin-favorite-location'."
  (setq wttrin-favorite-location test-wttrin-use-current-location--saved))

;;; Normal Cases

(ert-deftest test-wttrin-use-current-location-normal-confirm-sets-t ()
  "Normal: confirming sets the favorite to t (auto-detect)."
  (test-wttrin-use-current-location-setup)
  (unwind-protect
      (let ((wttrin-geolocation-enabled t))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-use-current-location))
        (should (eq t wttrin-favorite-location)))
    (test-wttrin-use-current-location-teardown)))

(ert-deftest test-wttrin-use-current-location-normal-decline-leaves-unchanged ()
  "Normal: declining leaves the favorite untouched."
  (test-wttrin-use-current-location-setup)
  (setq wttrin-favorite-location "Berkeley, CA")
  (unwind-protect
      (let ((wttrin-geolocation-enabled t))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-use-current-location))
        (should (equal "Berkeley, CA" wttrin-favorite-location)))
    (test-wttrin-use-current-location-teardown)))

;;; Boundary / Error Cases

(ert-deftest test-wttrin-use-current-location-boundary-disabled-no-prompt-no-set ()
  "Boundary: with geolocation disabled, no prompt and the favorite is unchanged."
  (test-wttrin-use-current-location-setup)
  (setq wttrin-favorite-location "Berkeley, CA")
  (unwind-protect
      (let ((prompted nil)
            (wttrin-geolocation-enabled nil))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (setq prompted t) t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-use-current-location))
        (should-not prompted)
        (should (equal "Berkeley, CA" wttrin-favorite-location)))
    (test-wttrin-use-current-location-teardown)))

(provide 'test-wttrin-use-current-location)
;;; test-wttrin-use-current-location.el ends here
