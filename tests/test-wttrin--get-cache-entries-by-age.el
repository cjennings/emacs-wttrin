;;; test-wttrin--get-cache-entries-by-age.el --- Tests for cache entry sorting -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Tests for wttrin--get-cache-entries-by-age helper function.
;; This function extracts cache entries sorted by age (oldest first).

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--get-cache-entries-by-age-empty-cache ()
  "Should return empty list when cache is empty."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    (should (equal '() (wttrin--get-cache-entries-by-age)))))

(ert-deftest test-wttrin--get-cache-entries-by-age-single-entry ()
  "Should return single entry in list."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    (puthash "Paris|m" (cons 1000.0 "weather data") wttrin--cache)
    (let ((result (wttrin--get-cache-entries-by-age)))
      (should (= 1 (length result)))
      (should (equal "Paris|m" (car (car result))))
      (should (= 1000.0 (cdr (car result)))))))

(ert-deftest test-wttrin--get-cache-entries-by-age-sorted-oldest-first ()
  "Should return entries sorted by timestamp, oldest first."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    ;; Add entries with different timestamps (not in sorted order)
    (puthash "New York|u" (cons 3000.0 "newest data") wttrin--cache)
    (puthash "Paris|m" (cons 1000.0 "oldest data") wttrin--cache)
    (puthash "Tokyo|m" (cons 2000.0 "middle data") wttrin--cache)

    (let ((result (wttrin--get-cache-entries-by-age)))
      (should (= 3 (length result)))
      ;; Check order: oldest to newest
      (should (equal "Paris|m" (car (nth 0 result))))
      (should (= 1000.0 (cdr (nth 0 result))))
      (should (equal "Tokyo|m" (car (nth 1 result))))
      (should (= 2000.0 (cdr (nth 1 result))))
      (should (equal "New York|u" (car (nth 2 result))))
      (should (= 3000.0 (cdr (nth 2 result)))))))

(ert-deftest test-wttrin--get-cache-entries-by-age-returns-key-timestamp-pairs ()
  "Should return (key . timestamp) pairs, not full cache values."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    ;; Cache value is (timestamp . data)
    (puthash "London|m" (cons 1500.0 "complete weather string with lots of data") wttrin--cache)

    (let ((result (wttrin--get-cache-entries-by-age)))
      (should (= 1 (length result)))
      (let ((entry (car result)))
        ;; Entry should be (key . timestamp), not (key . (timestamp . data))
        (should (equal "London|m" (car entry)))
        (should (= 1500.0 (cdr entry)))
        ;; Should NOT contain the weather data string
        (should-not (stringp (cdr entry)))))))

;;; Boundary Cases

(ert-deftest test-wttrin--get-cache-entries-by-age-same-timestamp ()
  "Should handle entries with identical timestamps."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    (puthash "Location1|m" (cons 1000.0 "data1") wttrin--cache)
    (puthash "Location2|m" (cons 1000.0 "data2") wttrin--cache)
    (puthash "Location3|m" (cons 1000.0 "data3") wttrin--cache)

    (let ((result (wttrin--get-cache-entries-by-age)))
      (should (= 3 (length result)))
      ;; All should have same timestamp
      (should (= 1000.0 (cdr (nth 0 result))))
      (should (= 1000.0 (cdr (nth 1 result))))
      (should (= 1000.0 (cdr (nth 2 result)))))))

(ert-deftest test-wttrin--get-cache-entries-by-age-float-timestamps ()
  "Should correctly sort float timestamps with fractional seconds."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    (puthash "A|m" (cons 1000.123 "data") wttrin--cache)
    (puthash "B|m" (cons 1000.789 "data") wttrin--cache)
    (puthash "C|m" (cons 1000.456 "data") wttrin--cache)

    (let ((result (wttrin--get-cache-entries-by-age)))
      ;; Should be sorted: 1000.123 < 1000.456 < 1000.789
      (should (equal "A|m" (car (nth 0 result))))
      (should (equal "C|m" (car (nth 1 result))))
      (should (equal "B|m" (car (nth 2 result)))))))

(ert-deftest test-wttrin--get-cache-entries-by-age-many-entries ()
  "Should handle cache at max capacity (50 entries)."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    ;; Add 50 entries with timestamps 1000, 1001, 1002, ..., 1049
    (dotimes (i 50)
      (puthash (format "Location%d|m" i)
               (cons (+ 1000.0 i) "data")
               wttrin--cache))

    (let ((result (wttrin--get-cache-entries-by-age)))
      (should (= 50 (length result)))
      ;; First should be oldest (1000.0)
      (should (= 1000.0 (cdr (car result))))
      ;; Last should be newest (1049.0)
      (should (= 1049.0 (cdr (car (last result))))))))

;;; Error Cases

(ert-deftest test-wttrin--get-cache-entries-by-age-preserves-original-cache ()
  "Should not modify the original cache hash table."
  (let ((wttrin--cache (make-hash-table :test 'equal)))
    (puthash "Paris|m" (cons 1000.0 "data") wttrin--cache)
    (puthash "Tokyo|m" (cons 2000.0 "data") wttrin--cache)

    (let ((original-count (hash-table-count wttrin--cache)))
      (wttrin--get-cache-entries-by-age)
      ;; Cache should still have same number of entries
      (should (= original-count (hash-table-count wttrin--cache)))
      ;; Original entries should still be present
      (should (gethash "Paris|m" wttrin--cache))
      (should (gethash "Tokyo|m" wttrin--cache)))))

(provide 'test-wttrin--get-cache-entries-by-age)
;;; test-wttrin--get-cache-entries-by-age.el ends here
