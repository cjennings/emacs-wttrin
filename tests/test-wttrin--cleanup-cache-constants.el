;;; test-wttrin--cleanup-cache-constants.el --- Tests for cache cleanup constants -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests verifying cache cleanup behavior with named constants.
;; These tests verify the behavior before and after refactoring magic numbers.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases - Cache Cleanup Behavior

(ert-deftest test-wttrin--cleanup-cache-removes-oldest-entries ()
  "Test that cleanup removes oldest entries when cache exceeds max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 5)
        (now (float-time)))
    ;; Add 10 entries with sequential timestamps
    (dotimes (i 10)
      (puthash (format "location-%d" i)
               (cons (+ now i) (format "data-%d" i))
               wttrin--cache))
    ;; Cache should have 10 entries
    (should (= 10 (hash-table-count wttrin--cache)))

    ;; Trigger cleanup
    (wttrin--cleanup-cache-if-needed)

    ;; After cleanup, cache should have fewer entries
    ;; With 10 entries and max of 5, should remove 20% = 2 entries
    ;; Leaving 8 entries
    (should (= 8 (hash-table-count wttrin--cache)))

    ;; Oldest entries (location-0, location-1) should be removed
    (should-not (gethash "location-0" wttrin--cache))
    (should-not (gethash "location-1" wttrin--cache))

    ;; Newer entries should remain
    (should (gethash "location-9" wttrin--cache))
    (should (gethash "location-8" wttrin--cache))))

(ert-deftest test-wttrin--cleanup-cache-removes-approximately-20-percent ()
  "Test that cleanup removes approximately 20% of entries."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 10)
        (now (float-time)))
    ;; Add 20 entries (twice the max)
    (dotimes (i 20)
      (puthash (format "loc-%d" i)
               (cons (+ now i) (format "data-%d" i))
               wttrin--cache))

    (should (= 20 (hash-table-count wttrin--cache)))

    ;; Trigger cleanup - should remove 20% of 20 = 4 entries
    (wttrin--cleanup-cache-if-needed)

    ;; Should have 16 entries remaining (20 - 4)
    (should (= 16 (hash-table-count wttrin--cache)))))

(ert-deftest test-wttrin--cleanup-cache-no-action-when-under-max ()
  "Test that cleanup does nothing when cache is under max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 10)
        (now (float-time)))
    ;; Add 5 entries (under max of 10)
    (dotimes (i 5)
      (puthash (format "location-%d" i)
               (cons (+ now i) (format "data-%d" i))
               wttrin--cache))

    (should (= 5 (hash-table-count wttrin--cache)))

    ;; Trigger cleanup
    (wttrin--cleanup-cache-if-needed)

    ;; Should still have 5 entries (no cleanup needed)
    (should (= 5 (hash-table-count wttrin--cache)))))

;;; Boundary Cases

(ert-deftest test-wttrin--cleanup-cache-exactly-at-max ()
  "Test that cleanup does nothing when cache is exactly at max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 5)
        (now (float-time)))
    ;; Add exactly 5 entries (at max)
    (dotimes (i 5)
      (puthash (format "location-%d" i)
               (cons (+ now i) (format "data-%d" i))
               wttrin--cache))

    (should (= 5 (hash-table-count wttrin--cache)))

    ;; Trigger cleanup - should do nothing since not > max
    (wttrin--cleanup-cache-if-needed)

    ;; Should still have 5 entries
    (should (= 5 (hash-table-count wttrin--cache)))))

(ert-deftest test-wttrin--cleanup-cache-one-over-max ()
  "Test cleanup when cache has just one entry over max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 5)
        (now (float-time)))
    ;; Add 6 entries (1 over max)
    (dotimes (i 6)
      (puthash (format "location-%d" i)
               (cons (+ now i) (format "data-%d" i))
               wttrin--cache))

    (should (= 6 (hash-table-count wttrin--cache)))

    ;; Trigger cleanup - should remove 20% of 6 = 1.2 = 1 entry (floor)
    (wttrin--cleanup-cache-if-needed)

    ;; Should have 5 entries remaining
    (should (= 5 (hash-table-count wttrin--cache)))

    ;; Oldest entry (location-0) should be removed
    (should-not (gethash "location-0" wttrin--cache))))

;;; Tests for Mode-Line Startup Delay (after refactoring)

(ert-deftest test-wttrin-mode-line-startup-delay-exists ()
  "Test that wttrin-mode-line-startup-delay defcustom exists after refactoring."
  ;; This test will fail initially, then pass after refactoring
  (should (boundp 'wttrin-mode-line-startup-delay)))

(ert-deftest test-wttrin-mode-line-startup-delay-is-number ()
  "Test that startup delay is a number."
  (should (numberp wttrin-mode-line-startup-delay)))

(ert-deftest test-wttrin-mode-line-startup-delay-reasonable-range ()
  "Test that startup delay is in reasonable range (1-10 seconds)."
  (should (>= wttrin-mode-line-startup-delay 1))
  (should (<= wttrin-mode-line-startup-delay 10)))

(provide 'test-wttrin--cleanup-cache-constants)
;;; test-wttrin--cleanup-cache-constants.el ends here
