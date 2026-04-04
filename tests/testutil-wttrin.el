;;; testutil-wttrin.el --- Test utilities for wttrin -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings
;; Author: Craig Jennings <c@cjennings.net>

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

(defconst testutil-wttrin-sample-ansi-response
  "Weather report: Paris

\x1b[38;5;226m   \\  /\x1b[0m       Partly cloudy
\x1b[38;5;226m _ /\"\"\x1b[38;5;250m.-.\x1b[0m    \x1b[38;5;118m+13\x1b[0m(\x1b[38;5;082m12\x1b[0m) °C
\x1b[38;5;226m   \\_\x1b[38;5;250m(   ).  \x1b[0m ↑ \x1b[38;5;190m12\x1b[0m km/h
"
  "Sample weather data with ANSI color codes for testing rendering.")

(defconst testutil-wttrin-sample-full-weather
  "Weather for Berkeley, CA

     \\    /      Clear
      .-.       62 °F
   ― (   ) ―    ↑ 5 mph
      `-'       10 mi
     /    \\     0.0 in"
  "Sample full weather display data for integration tests.")

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

(defmacro testutil-wttrin-with-refresh-interval (interval &rest body)
  "Execute BODY with wttrin-refresh-interval temporarily set to INTERVAL."
  (declare (indent 1))
  `(let ((wttrin-refresh-interval ,interval))
     ,@body))

(defmacro testutil-wttrin-with-cache-max (max-entries &rest body)
  "Execute BODY with wttrin-cache-max-entries temporarily set to MAX-ENTRIES."
  (declare (indent 1))
  `(let ((wttrin-cache-max-entries ,max-entries))
     ,@body))

;;; Buffer Management

(defmacro testutil-wttrin-with-clean-weather-buffer (&rest body)
  "Execute BODY with clean *wttr.in* buffer setup/teardown."
  (declare (indent 0))
  `(progn
     (when (get-buffer "*wttr.in*")
       (kill-buffer "*wttr.in*"))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer "*wttr.in*")
         (kill-buffer "*wttr.in*")))))

;;; HTTP Mock Helpers

(defmacro testutil-wttrin-mock-http-response (response-body &rest body)
  "Mock url-retrieve to return HTTP 200 with RESPONSE-BODY, execute BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'url-retrieve)
              (lambda (url callback)
                (with-temp-buffer
                  (insert "HTTP/1.1 200 OK\n\n")
                  (insert ,response-body)
                  (funcall callback nil)))))
     ,@body))

;;; Mode-line Cache Helpers

(defun testutil-wttrin-set-mode-line-cache (data &optional age-seconds)
  "Set mode-line cache to DATA, optionally aged by AGE-SECONDS."
  (let ((timestamp (if age-seconds
                       (- (float-time) age-seconds)
                     (float-time))))
    (setq wttrin--mode-line-cache (cons timestamp data))))

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
