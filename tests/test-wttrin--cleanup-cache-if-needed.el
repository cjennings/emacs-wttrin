;;; test-wttrin--cleanup-cache-if-needed.el --- Tests for wttrin--cleanup-cache-if-needed -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--cleanup-cache-if-needed function.
;; Tests cache eviction when max size is exceeded.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--cleanup-cache-if-needed-setup ()
  "Setup for cleanup cache tests."
  (testutil-wttrin-setup))

(defun test-wttrin--cleanup-cache-if-needed-teardown ()
  "Teardown for cleanup cache tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--cleanup-cache-if-needed-normal-under-max-does-nothing ()
  "Test that cache under max size is not cleaned up."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 10
        ;; Add 5 entries (under max of 10)
        (dotimes (i 5)
          (testutil-wttrin-add-to-cache (format "loc%d" i) "data"))
        (wttrin--cleanup-cache-if-needed)
        (should (= 5 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-normal-exceeds-max-removes-oldest ()
  "Test that cache exceeding max size removes oldest 20% of entries."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 10
        ;; Add 11 entries (exceeds max of 10)
        ;; Entries added earlier should be older
        (dotimes (i 11)
          (testutil-wttrin-add-to-cache (format "loc%d" i) "data" (* i 10)))
        (wttrin--cleanup-cache-if-needed)
        ;; Should remove 20% = 2 entries (11/5 = 2.2, rounds to 2)
        (should (= 9 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-normal-removes-correct-entries ()
  "Test that cleanup removes the oldest entries based on timestamp."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 5
        ;; Add 6 entries with specific ages (older = higher age-seconds)
        (testutil-wttrin-add-to-cache "old1" "data1" 1000)  ; oldest
        (testutil-wttrin-add-to-cache "old2" "data2" 900)
        (testutil-wttrin-add-to-cache "mid1" "data3" 500)
        (testutil-wttrin-add-to-cache "mid2" "data4" 300)
        (testutil-wttrin-add-to-cache "new1" "data5" 100)
        (testutil-wttrin-add-to-cache "new2" "data6" 50)   ; newest

        (wttrin--cleanup-cache-if-needed)
        ;; Should remove 20% = 1 entry (6/5 = 1.2, rounds to 1)
        ;; Should keep 5 entries
        (should (= 5 (testutil-wttrin-cache-size)))
        ;; The oldest entry (old1) should be gone
        (should-not (gethash (wttrin--make-cache-key "old1") wttrin--cache))
        ;; The newest entries should remain
        (should (gethash (wttrin--make-cache-key "new1") wttrin--cache))
        (should (gethash (wttrin--make-cache-key "new2") wttrin--cache)))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-empty-cache-does-nothing ()
  "Test that cleanup with empty cache does nothing."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 10
        (wttrin--cleanup-cache-if-needed)
        (should (= 0 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-exactly-at-max-does-nothing ()
  "Test that cache exactly at max size is not cleaned up."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 10
        ;; Add exactly 10 entries (at max)
        (dotimes (i 10)
          (testutil-wttrin-add-to-cache (format "loc%d" i) "data"))
        (wttrin--cleanup-cache-if-needed)
        (should (= 10 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-one-entry-at-max-one-does-nothing ()
  "Test that one entry at max=1 does not trigger cleanup."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 1
        (testutil-wttrin-add-to-cache "loc1" "data")
        (wttrin--cleanup-cache-if-needed)
        (should (= 1 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-two-entries-at-max-one-removes-none ()
  "Test that two entries at max=1 removes no entries due to integer division."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 1
        (testutil-wttrin-add-to-cache "old" "data1" 100)
        (testutil-wttrin-add-to-cache "new" "data2" 50)
        (wttrin--cleanup-cache-if-needed)
        ;; 2 entries / 5 = 0 in integer division, so no entries removed
        (should (= 2 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-one-over-max-removes-oldest ()
  "Test cleanup when cache has just one entry over max."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 5
        ;; Add 6 entries (1 over max)
        (dotimes (i 6)
          (testutil-wttrin-add-to-cache (format "loc%d" i) (format "data-%d" i) (* (- 6 i) 100)))
        (should (= 6 (testutil-wttrin-cache-size)))
        (wttrin--cleanup-cache-if-needed)
        ;; Should remove 20% of 6 = 1.2 = 1 entry (floor)
        (should (= 5 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-normal-large-dataset-removes-20-percent ()
  "Test that cleanup removes approximately 20% of entries with larger dataset."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (testutil-wttrin-with-cache-max 10
        ;; Add 20 entries (twice the max)
        (dotimes (i 20)
          (testutil-wttrin-add-to-cache (format "loc%d" i) (format "data-%d" i) (* (- 20 i) 100)))
        (should (= 20 (testutil-wttrin-cache-size)))
        ;; Trigger cleanup - should remove 20% of 20 = 4 entries
        (wttrin--cleanup-cache-if-needed)
        ;; Should have 16 entries remaining (20 - 4)
        (should (= 16 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-custom-cleanup-percentage-removes-expected ()
  "Test that custom cleanup percentage is respected."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (let ((wttrin-cache-max-entries 100)
            (wttrin--cache-cleanup-percentage 0.30)) ; 30% cleanup
        ;; Add 101 entries
        (dotimes (i 101)
          (testutil-wttrin-add-to-cache (format "loc%d" i) "data" (* (- 101 i) 10)))
        (wttrin--cleanup-cache-if-needed)
        ;; Should remove floor(101 * 0.30) = 30 oldest entries, leaving 71
        (should (= 71 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-boundary-small-percentage-removes-minimum ()
  "Test that small cleanup percentage (10%) removes correct number."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (let ((wttrin-cache-max-entries 10)
            (wttrin--cache-cleanup-percentage 0.10)) ; Only 10%
        ;; Add 11 entries
        (dotimes (i 11)
          (testutil-wttrin-add-to-cache (format "loc%d" i) "data" (* (- 11 i) 100)))
        (wttrin--cleanup-cache-if-needed)
        ;; Should remove floor(11 * 0.10) = 1 oldest entry
        (should (= 10 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(ert-deftest test-wttrin--cleanup-cache-if-needed-normal-multiple-cleanups-work-correctly ()
  "Test that multiple cleanup cycles work correctly."
  (test-wttrin--cleanup-cache-if-needed-setup)
  (unwind-protect
      (let ((wttrin-cache-max-entries 50)
            (wttrin--cache-cleanup-percentage 0.20))
        ;; First batch: 51 entries
        (dotimes (i 51)
          (testutil-wttrin-add-to-cache (format "batch1-%d" i) "data" (* (- 51 i) 10)))
        (wttrin--cleanup-cache-if-needed)
        (should (= 41 (testutil-wttrin-cache-size)))

        ;; Second batch: add 10 more (now 51 again)
        (dotimes (i 10)
          (testutil-wttrin-add-to-cache (format "batch2-%d" i) "data"))
        (wttrin--cleanup-cache-if-needed)
        ;; Should cleanup again: floor(51 * 0.20) = 10 removed, leaving 41
        (should (= 41 (testutil-wttrin-cache-size))))
    (test-wttrin--cleanup-cache-if-needed-teardown)))

(provide 'test-wttrin--cleanup-cache-if-needed)
;;; test-wttrin--cleanup-cache-if-needed.el ends here
