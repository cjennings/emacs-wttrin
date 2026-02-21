;;; test-wttrin--get-cached-or-fetch.el --- Tests for wttrin--get-cached-or-fetch -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--get-cached-or-fetch function.
;; Tests the core cache workflow: cache hits, misses, and error fallback.
;; TTL-based expiration has been removed — cached data is served regardless
;; of age, with proactive refresh keeping data fresh in the background.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Test Data Fixtures

(defconst test-wttrin--get-cached-or-fetch-sample-weather
  "Weather for Paris: Sunny 22°C"
  "Sample weather data for testing.")

(defconst test-wttrin--get-cached-or-fetch-new-weather
  "Weather for Paris: Cloudy 18°C"
  "Updated weather data for testing cache updates.")

;;; Test Setup and Teardown

(defun test-wttrin--get-cached-or-fetch-setup ()
  "Setup for get-cached-or-fetch tests."
  (testutil-wttrin-setup))

(defun test-wttrin--get-cached-or-fetch-teardown ()
  "Teardown for get-cached-or-fetch tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--get-cached-or-fetch-normal-cache-hit-returns-cached-data ()
  "Test that cached data is returned without fetching."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Paris")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (callback-result nil)
             (fetch-called nil))
        ;; Pre-populate cache with data
        (puthash cache-key (cons now test-wttrin--get-cached-or-fetch-sample-weather)
                 wttrin--cache)

        ;; Mock time to be 100 seconds later
        (cl-letf (((symbol-function 'float-time)
                   (lambda () (+ now 100.0)))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location _callback)
                     (setq fetch-called t))))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should return cached data immediately
          (should (equal callback-result test-wttrin--get-cached-or-fetch-sample-weather))
          ;; Should NOT call fetch
          (should-not fetch-called)))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-normal-cache-miss-fetches-new-data ()
  "Test that missing cache entry triggers fetch and stores result."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "London")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (callback-result nil)
             (fetch-called nil))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () now))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (setq fetch-called t)
                     (funcall callback test-wttrin--get-cached-or-fetch-new-weather)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should call fetch
          (should fetch-called)
          ;; Should return fresh data
          (should (equal callback-result test-wttrin--get-cached-or-fetch-new-weather))
          ;; Should store in cache
          (let ((cached (gethash cache-key wttrin--cache)))
            (should cached)
            (should (equal (car cached) now))
            (should (equal (cdr cached) test-wttrin--get-cached-or-fetch-new-weather)))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-normal-old-data-still-served ()
  "Test that old cached data is served without fetching (no TTL expiration).
Proactive refresh keeps data fresh; on-demand reads always use cache."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Tokyo")
             (cache-key (wttrin--make-cache-key location))
             (old-time 1000.0)
             (new-time (+ old-time 10000.0)) ; Very old data
             (callback-result nil)
             (fetch-called nil))

        ;; Pre-populate cache with old data
        (puthash cache-key (cons old-time test-wttrin--get-cached-or-fetch-sample-weather)
                 wttrin--cache)

        (cl-letf (((symbol-function 'float-time)
                   (lambda () new-time))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location _callback)
                     (setq fetch-called t))))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should serve old data without fetching
          (should-not fetch-called)
          (should (equal callback-result test-wttrin--get-cached-or-fetch-sample-weather))))
    (test-wttrin--get-cached-or-fetch-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--get-cached-or-fetch-boundary-force-refresh-bypasses-fresh-cache ()
  "Test that force refresh flag bypasses fresh cache."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Rome")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (callback-result nil)
             (fetch-called nil)
             (wttrin--force-refresh t)) ; Force refresh enabled

        ;; Pre-populate cache with fresh data
        (puthash cache-key (cons now test-wttrin--get-cached-or-fetch-sample-weather)
                 wttrin--cache)

        (cl-letf (((symbol-function 'float-time)
                   (lambda () (+ now 100.0)))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (setq fetch-called t)
                     (funcall callback test-wttrin--get-cached-or-fetch-new-weather)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should fetch despite fresh cache
          (should fetch-called)
          (should (equal callback-result test-wttrin--get-cached-or-fetch-new-weather))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-boundary-empty-cache-fetches ()
  "Test that completely empty cache triggers fetch."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Athens")
             (callback-result nil)
             (fetch-called nil))

        ;; Cache is already empty from setup
        (should (= (hash-table-count wttrin--cache) 0))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () 1000.0))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (setq fetch-called t)
                     (funcall callback test-wttrin--get-cached-or-fetch-new-weather)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should fetch
          (should fetch-called)
          (should (equal callback-result test-wttrin--get-cached-or-fetch-new-weather))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-boundary-cache-key-includes-unit-system ()
  "Test that cache keys differentiate by unit system."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Oslo")
             (now 1000.0)
             (metric-data "Weather: 20°C")
             (imperial-data "Weather: 68°F")
             (callback-result nil))

        ;; Cache metric version
        (let ((wttrin-unit-system "m"))
          (puthash (wttrin--make-cache-key location)
                   (cons now metric-data)
                   wttrin--cache))

        ;; Cache imperial version
        (let ((wttrin-unit-system "u"))
          (puthash (wttrin--make-cache-key location)
                   (cons now imperial-data)
                   wttrin--cache))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () (+ now 100.0))))

          ;; Fetch with metric - should get metric cache
          (let ((wttrin-unit-system "m"))
            (wttrin--get-cached-or-fetch
             location
             (lambda (data) (setq callback-result data)))
            (should (equal callback-result metric-data)))

          ;; Fetch with imperial - should get imperial cache
          (let ((wttrin-unit-system "u"))
            (wttrin--get-cached-or-fetch
             location
             (lambda (data) (setq callback-result data)))
            (should (equal callback-result imperial-data)))))
    (test-wttrin--get-cached-or-fetch-teardown)))

;;; Error Cases

(ert-deftest test-wttrin--get-cached-or-fetch-error-fetch-fails-with-stale-cache-returns-stale ()
  "Test that fetch failure with stale cache falls back to stale data."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Vienna")
             (cache-key (wttrin--make-cache-key location))
             (callback-result nil)
             (message-shown nil)
             (wttrin--force-refresh t)) ; Force refresh to trigger fetch

        ;; Pre-populate cache with data
        (puthash cache-key (cons 1000.0 test-wttrin--get-cached-or-fetch-sample-weather)
                 wttrin--cache)

        (cl-letf (((symbol-function 'float-time)
                   (lambda () 3000.0))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     ;; Simulate network failure
                     (funcall callback nil)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil))
                  ((symbol-function 'message)
                   (lambda (format-string &rest _args)
                     (setq message-shown format-string))))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should fall back to stale cache
          (should (equal callback-result test-wttrin--get-cached-or-fetch-sample-weather))
          ;; Should show message about using cached version
          (should message-shown)
          (should (string-match-p "cached" message-shown))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-error-fetch-fails-with-no-cache-returns-nil ()
  "Test that fetch failure with no cache returns nil."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location "Dublin")
             (callback-result 'unset))

        ;; Cache is empty
        (should (= (hash-table-count wttrin--cache) 0))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () 1000.0))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     ;; Simulate network failure
                     (funcall callback nil)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should return nil (no fallback available)
          (should (null callback-result))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(ert-deftest test-wttrin--get-cached-or-fetch-error-nil-location-creates-cache-key ()
  "Test that nil location is handled (creates cache key with nil)."
  (test-wttrin--get-cached-or-fetch-setup)
  (unwind-protect
      (let* ((location nil)
             (callback-result nil)
             (fetch-called nil))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () 1000.0))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (setq fetch-called t)
                     (funcall callback test-wttrin--get-cached-or-fetch-new-weather)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          (wttrin--get-cached-or-fetch
           location
           (lambda (data) (setq callback-result data)))

          ;; Should attempt to fetch (nil is a valid location input)
          (should fetch-called)
          (should (equal callback-result test-wttrin--get-cached-or-fetch-new-weather))))
    (test-wttrin--get-cached-or-fetch-teardown)))

(provide 'test-wttrin--get-cached-or-fetch)
;;; test-wttrin--get-cached-or-fetch.el ends here
