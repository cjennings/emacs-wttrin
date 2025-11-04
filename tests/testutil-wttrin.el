;;; testutil-wttrin.el --- Test utilities for wttrin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Shared test utilities for wttrin test suite.
;; Provides helper functions, fixtures, and common setup/teardown functionality.

;;; Code:

(require 'ert)

;;; Test Data Fixtures

(defconst testutil-wttrin-sample-weather-response
  "Weather report: Paris, France

     \\  /       Partly cloudy
   _ /\"\".-. 22 °C
     \\_(   ).  ↓ 15 km/h
     /(___(__)  10 km
                0.0 mm"
  "Sample weather response for testing parsing logic.")

(defconst testutil-wttrin-sample-error-response
  "ERROR: Unknown location; please try ~curl wttr.in/:help"
  "Sample error response from wttr.in service.")

;;; Cache Testing Helpers

(defun testutil-wttrin-clear-cache ()
  "Clear the wttrin cache for test isolation."
  (clrhash wttrin--cache))

(defun testutil-wttrin-add-to-cache (location data &optional age-seconds)
  "Add DATA to cache for LOCATION, optionally aged by AGE-SECONDS."
  (let* ((cache-key (wttrin--make-cache-key location))
         (timestamp (if age-seconds
                        (- (float-time) age-seconds)
                      (float-time))))
    (puthash cache-key (cons timestamp data) wttrin--cache)))

(defun testutil-wttrin-cache-size ()
  "Return the current number of entries in the cache."
  (hash-table-count wttrin--cache))

;;; Custom Variable Management

(defmacro testutil-wttrin-with-unit-system (unit-system &rest body)
  "Execute BODY with wttrin-unit-system temporarily set to UNIT-SYSTEM."
  (declare (indent 1))
  `(let ((wttrin-unit-system ,unit-system))
     ,@body))

(defmacro testutil-wttrin-with-cache-ttl (ttl &rest body)
  "Execute BODY with wttrin-cache-ttl temporarily set to TTL."
  (declare (indent 1))
  `(let ((wttrin-cache-ttl ,ttl))
     ,@body))

(defmacro testutil-wttrin-with-cache-max (max-entries &rest body)
  "Execute BODY with wttrin-cache-max-entries temporarily set to MAX-ENTRIES."
  (declare (indent 1))
  `(let ((wttrin-cache-max-entries ,max-entries))
     ,@body))

;;; Test Setup and Teardown

(defun testutil-wttrin-setup ()
  "Common setup for wttrin tests.
Call this at the beginning of each test."
  (testutil-wttrin-clear-cache)
  (setq wttrin--force-refresh nil))

(defun testutil-wttrin-teardown ()
  "Common teardown for wttrin tests.
Call this at the end of each test."
  (testutil-wttrin-clear-cache)
  (setq wttrin--force-refresh nil))

(provide 'testutil-wttrin)
;;; testutil-wttrin.el ends here
