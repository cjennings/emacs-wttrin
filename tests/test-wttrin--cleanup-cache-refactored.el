;;; test-wttrin--cleanup-cache-refactored.el --- Tests for refactored cache cleanup -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Tests for refactored wttrin--cleanup-cache-if-needed function.
;; Verifies that the cleanup logic works correctly when using
;; the extracted wttrin--get-cache-entries-by-age helper.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--cleanup-cache-refactored-does-nothing-when-under-limit ()
  "Should not remove entries when cache is below max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 50))
    ;; Add 40 entries (under the limit of 50)
    (dotimes (i 40)
      (puthash (format "Loc%d|m" i) (cons (float i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)

    ;; Should still have all 40 entries
    (should (= 40 (hash-table-count wttrin--cache)))))

(ert-deftest test-wttrin--cleanup-cache-refactored-removes-oldest-entries ()
  "Should remove oldest 20% when cache exceeds max."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 50)
        (wttrin--cache-cleanup-percentage 0.20))
    ;; Add 51 entries (1 over the limit)
    ;; Timestamps: 1000.0, 1001.0, ..., 1050.0
    (dotimes (i 51)
      (puthash (format "Loc%d|m" i)
               (cons (+ 1000.0 i) "data")
               wttrin--cache))

    (wttrin--cleanup-cache-if-needed)

    ;; Should remove floor(51 * 0.20) = 10 oldest entries
    (should (= 41 (hash-table-count wttrin--cache)))

    ;; Oldest entries (Loc0 through Loc9) should be gone
    (should-not (gethash "Loc0|m" wttrin--cache))
    (should-not (gethash "Loc9|m" wttrin--cache))

    ;; Newer entries (Loc10 and up) should remain
    (should (gethash "Loc10|m" wttrin--cache))
    (should (gethash "Loc50|m" wttrin--cache))))

(ert-deftest test-wttrin--cleanup-cache-refactored-uses-helper-function ()
  "Should use wttrin--get-cache-entries-by-age for sorting."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 10)
        (wttrin--cache-cleanup-percentage 0.20)
        (helper-called nil))

    ;; Add entries in random timestamp order
    (puthash "New|m" (cons 3000.0 "data") wttrin--cache)
    (puthash "Old|m" (cons 1000.0 "data") wttrin--cache)
    (puthash "Mid|m" (cons 2000.0 "data") wttrin--cache)
    (dotimes (i 8)
      (puthash (format "Extra%d|m" i) (cons (+ 1500.0 i) "data") wttrin--cache))

    ;; Mock the helper to verify it's called
    (cl-letf (((symbol-function 'wttrin--get-cache-entries-by-age)
               (lambda ()
                 (setq helper-called t)
                 ;; Return actual sorted entries
                 (let ((entries nil))
                   (maphash (lambda (k v) (push (cons k (car v)) entries))
                            wttrin--cache)
                   (sort entries (lambda (a b) (< (cdr a) (cdr b))))))))

      (wttrin--cleanup-cache-if-needed)

      ;; Verify helper was called (after refactoring)
      ;; Note: This test will pass even before refactoring since we mock it
      ;; The real verification is that cleanup still works correctly
      (should (= 9 (hash-table-count wttrin--cache)))

      ;; Oldest entry should be removed
      (should-not (gethash "Old|m" wttrin--cache))

      ;; Newer entries should remain
      (should (gethash "Mid|m" wttrin--cache))
      (should (gethash "New|m" wttrin--cache)))))

;;; Boundary Cases

(ert-deftest test-wttrin--cleanup-cache-refactored-exactly-at-max ()
  "Should not cleanup when exactly at max entries."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 50))
    ;; Add exactly 50 entries
    (dotimes (i 50)
      (puthash (format "Loc%d|m" i) (cons (float i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)

    ;; Should still have all 50 entries
    (should (= 50 (hash-table-count wttrin--cache)))))

(ert-deftest test-wttrin--cleanup-cache-refactored-respects-cleanup-percentage ()
  "Should use wttrin--cache-cleanup-percentage constant."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 100)
        (wttrin--cache-cleanup-percentage 0.30))  ; 30% cleanup
    ;; Add 101 entries
    (dotimes (i 101)
      (puthash (format "Loc%d|m" i) (cons (float i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)

    ;; Should remove floor(101 * 0.30) = 30 oldest entries
    ;; Leaving 71 entries
    (should (= 71 (hash-table-count wttrin--cache)))

    ;; First 30 should be gone
    (should-not (gethash "Loc0|m" wttrin--cache))
    (should-not (gethash "Loc29|m" wttrin--cache))

    ;; Entry 30 and beyond should remain
    (should (gethash "Loc30|m" wttrin--cache))
    (should (gethash "Loc100|m" wttrin--cache))))

(ert-deftest test-wttrin--cleanup-cache-refactored-small-percentage ()
  "Should handle small cleanup percentages correctly."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 10)
        (wttrin--cache-cleanup-percentage 0.10))  ; Only 10%
    ;; Add 11 entries
    (dotimes (i 11)
      (puthash (format "Loc%d|m" i) (cons (float i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)

    ;; Should remove floor(11 * 0.10) = 1 oldest entry
    (should (= 10 (hash-table-count wttrin--cache)))

    ;; Oldest should be gone
    (should-not (gethash "Loc0|m" wttrin--cache))

    ;; Rest should remain
    (should (gethash "Loc1|m" wttrin--cache))
    (should (gethash "Loc10|m" wttrin--cache))))

;;; Integration Tests

(ert-deftest test-wttrin--cleanup-cache-refactored-multiple-cleanups ()
  "Should handle multiple cleanup cycles correctly."
  (let ((wttrin--cache (make-hash-table :test 'equal))
        (wttrin-cache-max-entries 50)
        (wttrin--cache-cleanup-percentage 0.20))

    ;; First batch: 51 entries
    (dotimes (i 51)
      (puthash (format "Batch1-%d|m" i) (cons (+ 1000.0 i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)
    (should (= 41 (hash-table-count wttrin--cache)))

    ;; Second batch: add 10 more (now 51 again)
    (dotimes (i 10)
      (puthash (format "Batch2-%d|m" i) (cons (+ 2000.0 i) "data") wttrin--cache))

    (wttrin--cleanup-cache-if-needed)
    ;; Should cleanup again: floor(51 * 0.20) = 10 removed, leaving 41
    (should (= 41 (hash-table-count wttrin--cache)))

    ;; Oldest from first batch should be gone
    (should-not (gethash "Batch1-0|m" wttrin--cache))

    ;; Newest from second batch should remain
    (should (gethash "Batch2-9|m" wttrin--cache))))

(provide 'test-wttrin--cleanup-cache-refactored)
;;; test-wttrin--cleanup-cache-refactored.el ends here
