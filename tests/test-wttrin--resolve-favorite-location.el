;;; test-wttrin--resolve-favorite-location.el --- Tests for wttrin--resolve-favorite-location -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for `wttrin--resolve-favorite-location' and its helpers.
;; The resolver maps the three modes of `wttrin-favorite-location'
;; (nil / string / t) onto a returned location string, kicking off an
;; async geolocation lookup the first time t is seen.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

;; Load wttrin-geolocation up front so cl-letf mocks of
;; `wttrin-geolocation-detect' aren't undone by the resolver's lazy
;; (require 'wttrin-geolocation) — which would otherwise re-evaluate
;; the defun and overwrite the mocked symbol-function.
(require 'wttrin-geolocation
         (expand-file-name "wttrin-geolocation.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Helpers

(defmacro test-wttrin--resolve-with-geolocation-mock (returned-location &rest body)
  "Run BODY with `wttrin-geolocation-detect' mocked to call back with
RETURNED-LOCATION synchronously.  Resets resolver caches before BODY."
  (declare (indent 1))
  `(let ((wttrin--resolved-favorite-location nil)
         (wttrin--favorite-location-pending nil))
     (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                (lambda (callback) (funcall callback ,returned-location))))
       ,@body)))

;;; Normal Cases

(ert-deftest test-wttrin--resolve-favorite-location-normal-nil-returns-nil ()
  "Disabled mode (nil) resolves to nil."
  (let ((wttrin-favorite-location nil)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil))
    (should-not (wttrin--resolve-favorite-location))))

(ert-deftest test-wttrin--resolve-favorite-location-normal-string-returns-itself ()
  "Explicit string resolves to itself."
  (let ((wttrin-favorite-location "Berkeley, CA")
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil))
    (should (string= "Berkeley, CA" (wttrin--resolve-favorite-location)))))

(ert-deftest test-wttrin--resolve-favorite-location-normal-t-with-cache-returns-cached ()
  "When t and cache populated, resolver returns cached value without re-fetching."
  (let ((wttrin-favorite-location t)
        (wttrin--resolved-favorite-location "Cached, Place")
        (wttrin--favorite-location-pending nil)
        (detect-calls 0))
    (cl-letf (((symbol-function 'wttrin-geolocation-detect)
               (lambda (_callback) (cl-incf detect-calls))))
      (should (string= "Cached, Place" (wttrin--resolve-favorite-location)))
      (should (= 0 detect-calls)))))

(ert-deftest test-wttrin--resolve-favorite-location-normal-t-detection-success-caches-result ()
  "t-mode detect callback populates the resolver cache."
  (let ((wttrin-favorite-location t))
    (test-wttrin--resolve-with-geolocation-mock "Detected, City"
      (wttrin--resolve-favorite-location)
      (should (string= "Detected, City" wttrin--resolved-favorite-location))
      (should-not wttrin--favorite-location-pending))))

(ert-deftest test-wttrin--resolve-favorite-location-normal-second-call-returns-cached-from-first ()
  "After detection completes, the second call returns the cached string."
  (let ((wttrin-favorite-location t))
    (test-wttrin--resolve-with-geolocation-mock "Detected, City"
      (wttrin--resolve-favorite-location)
      (should (string= "Detected, City" (wttrin--resolve-favorite-location))))))

;;; Boundary Cases

(ert-deftest test-wttrin--resolve-favorite-location-boundary-t-without-cache-returns-nil ()
  "First call with t and empty cache returns nil while detection runs."
  (let ((wttrin-favorite-location t)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil))
    (cl-letf (((symbol-function 'wttrin-geolocation-detect)
               (lambda (_callback)
                 ;; Simulate an async lookup that has NOT called back yet.
                 (setq wttrin--favorite-location-pending t))))
      (should-not (wttrin--resolve-favorite-location)))))

(ert-deftest test-wttrin--resolve-favorite-location-boundary-pending-suppresses-duplicate-detect ()
  "When a lookup is already pending, the resolver does not start another."
  (let ((wttrin-favorite-location t)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending t)
        (detect-calls 0))
    (cl-letf (((symbol-function 'wttrin-geolocation-detect)
               (lambda (_callback) (cl-incf detect-calls))))
      (wttrin--resolve-favorite-location)
      (should (= 0 detect-calls)))))

;;; Error Cases

(ert-deftest test-wttrin--resolve-favorite-location-error-detection-failure-leaves-cache-empty ()
  "When the detect callback returns nil, the cache stays empty and pending clears."
  (let ((wttrin-favorite-location t))
    (test-wttrin--resolve-with-geolocation-mock nil
      (wttrin--resolve-favorite-location)
      (should-not wttrin--resolved-favorite-location)
      (should-not wttrin--favorite-location-pending))))

(ert-deftest test-wttrin--resolve-favorite-location-error-detection-failure-allows-retry ()
  "After a failed detection, a subsequent resolve call kicks off a new lookup."
  (let ((wttrin-favorite-location t)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil)
        (detect-calls 0))
    (cl-letf (((symbol-function 'wttrin-geolocation-detect)
               (lambda (callback)
                 (cl-incf detect-calls)
                 (funcall callback nil))))
      (wttrin--resolve-favorite-location)
      (wttrin--resolve-favorite-location)
      (should (= 2 detect-calls)))))

;;; Display Name Helper

(ert-deftest test-wttrin--favorite-location-display-name-resolved-string-returns-string ()
  "Display name returns the resolved string when available."
  (let ((wttrin-favorite-location "Tokyo, JP")
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil))
    (should (string= "Tokyo, JP" (wttrin--favorite-location-display-name)))))

(ert-deftest test-wttrin--favorite-location-display-name-t-pending-returns-current-location-label ()
  "Display name returns \"current location\" when t-mode lookup is pending."
  (let ((wttrin-favorite-location t)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending t))
    (should (string= "current location"
                     (wttrin--favorite-location-display-name)))))

(ert-deftest test-wttrin--favorite-location-display-name-nil-returns-nil ()
  "Display name returns nil when feature is disabled."
  (let ((wttrin-favorite-location nil)
        (wttrin--resolved-favorite-location nil)
        (wttrin--favorite-location-pending nil))
    (should-not (wttrin--favorite-location-display-name))))

(provide 'test-wttrin--resolve-favorite-location)
;;; test-wttrin--resolve-favorite-location.el ends here
