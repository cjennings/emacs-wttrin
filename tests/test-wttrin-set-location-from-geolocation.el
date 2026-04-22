;;; test-wttrin-set-location-from-geolocation.el --- Tests for wttrin-set-location-from-geolocation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for the interactive `wttrin-set-location-from-geolocation'
;; command.  Mocks `wttrin-geolocation-detect' (to invoke the callback
;; synchronously with a chosen value) and `yes-or-no-p' (to simulate
;; user consent).  Does not hit the network and does not prompt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'wttrin-geolocation)

;;; Setup and Teardown

(defvar test-wttrin-set-location-from-geolocation--saved-favorite nil
  "Snapshot of `wttrin-favorite-location' restored in teardown.")

(defun test-wttrin-set-location-from-geolocation-setup ()
  "Snapshot `wttrin-favorite-location' and clear it for the test."
  (setq test-wttrin-set-location-from-geolocation--saved-favorite
        wttrin-favorite-location)
  (setq wttrin-favorite-location nil))

(defun test-wttrin-set-location-from-geolocation-teardown ()
  "Restore `wttrin-favorite-location' to its pre-test value."
  (setq wttrin-favorite-location
        test-wttrin-set-location-from-geolocation--saved-favorite))

;;; Helpers

(defmacro test-wttrin-set-location--with-detected (location confirm &rest body)
  "Run BODY with `wttrin-geolocation-detect' returning LOCATION and `yes-or-no-p' returning CONFIRM."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'wttrin-geolocation-detect)
              (lambda (callback) (funcall callback ,location)))
             ((symbol-function 'yes-or-no-p)
              (lambda (&rest _) ,confirm)))
     ,@body))

;;; Normal Cases

(ert-deftest test-wttrin-set-location-from-geolocation-normal-confirm-sets-variable ()
  "Successful detection followed by user confirmation sets the favorite."
  (test-wttrin-set-location-from-geolocation-setup)
  (unwind-protect
      (progn
        (test-wttrin-set-location--with-detected "Berkeley, California" t
          (wttrin-set-location-from-geolocation))
        (should (string= "Berkeley, California" wttrin-favorite-location)))
    (test-wttrin-set-location-from-geolocation-teardown)))

(ert-deftest test-wttrin-set-location-from-geolocation-normal-decline-leaves-variable-unchanged ()
  "Successful detection followed by user declining leaves the favorite untouched."
  (test-wttrin-set-location-from-geolocation-setup)
  (setq wttrin-favorite-location "Pre-existing, Place")
  (unwind-protect
      (progn
        (test-wttrin-set-location--with-detected "Berkeley, California" nil
          (wttrin-set-location-from-geolocation))
        (should (string= "Pre-existing, Place" wttrin-favorite-location)))
    (test-wttrin-set-location-from-geolocation-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-set-location-from-geolocation-boundary-unicode-location ()
  "A Unicode location string round-trips into the favorite variable."
  (test-wttrin-set-location-from-geolocation-setup)
  (unwind-protect
      (progn
        (test-wttrin-set-location--with-detected "München, Bayern" t
          (wttrin-set-location-from-geolocation))
        (should (string= "München, Bayern" wttrin-favorite-location)))
    (test-wttrin-set-location-from-geolocation-teardown)))

;;; Error Cases

(ert-deftest test-wttrin-set-location-from-geolocation-error-nil-detection-leaves-variable-unchanged ()
  "When detection returns nil, the favorite variable is not modified."
  (test-wttrin-set-location-from-geolocation-setup)
  (setq wttrin-favorite-location "Pre-existing, Place")
  (unwind-protect
      (progn
        (test-wttrin-set-location--with-detected nil t
          (wttrin-set-location-from-geolocation))
        (should (string= "Pre-existing, Place" wttrin-favorite-location)))
    (test-wttrin-set-location-from-geolocation-teardown)))

(ert-deftest test-wttrin-set-location-from-geolocation-error-nil-detection-does-not-prompt ()
  "When detection returns nil, the user is not prompted for confirmation."
  (test-wttrin-set-location-from-geolocation-setup)
  (unwind-protect
      (let ((prompt-called nil))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (callback) (funcall callback nil)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (setq prompt-called t) t)))
          (wttrin-set-location-from-geolocation))
        (should-not prompt-called))
    (test-wttrin-set-location-from-geolocation-teardown)))

(ert-deftest test-wttrin-set-location-from-geolocation-error-detection-failure-shows-message ()
  "When detection returns nil, the user sees a diagnostic message."
  (test-wttrin-set-location-from-geolocation-setup)
  (unwind-protect
      (let ((messages nil))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (callback) (funcall callback nil)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (wttrin-set-location-from-geolocation))
        (should (cl-some (lambda (m) (string-match-p "[Cc]ould not detect" m))
                         messages)))
    (test-wttrin-set-location-from-geolocation-teardown)))

(provide 'test-wttrin-set-location-from-geolocation)
;;; test-wttrin-set-location-from-geolocation.el ends here
