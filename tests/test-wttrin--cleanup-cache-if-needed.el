;;; test-wttrin--cleanup-cache-if-needed.el --- Tests for wttrin--cleanup-cache-if-needed -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

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

(provide 'test-wttrin--cleanup-cache-if-needed)
;;; test-wttrin--cleanup-cache-if-needed.el ends here
