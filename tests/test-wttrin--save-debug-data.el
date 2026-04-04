;;; test-wttrin--save-debug-data.el --- Tests for wttrin--save-debug-data -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--save-debug-data function.
;; Tests that debug data files are created correctly with the expected contents.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--save-debug-data-setup ()
  "Setup for save-debug-data tests."
  (testutil-wttrin-setup))

(defun test-wttrin--save-debug-data-teardown ()
  "Teardown for save-debug-data tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--save-debug-data-normal-creates-file-that-exists ()
  "Returned filepath should point to a file that actually exists on disk."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "Paris" "some weather data")))
        (unwind-protect
            (should (file-exists-p filepath))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(ert-deftest test-wttrin--save-debug-data-normal-file-contains-location ()
  "File should contain a Location header with the queried location."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "New Orleans, LA" "weather")))
        (unwind-protect
            (let ((contents (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string))))
              (should (string-match-p "^Location: New Orleans, LA$" contents)))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(ert-deftest test-wttrin--save-debug-data-normal-file-contains-unit-system ()
  "File should record the wttrin-unit-system value at the time of capture."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let* ((wttrin-unit-system "m")
             (filepath (wttrin--save-debug-data "Paris" "weather data")))
        (unwind-protect
            (let ((contents (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string))))
              (should (string-match-p "wttrin-unit-system: m" contents)))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(ert-deftest test-wttrin--save-debug-data-normal-file-contains-raw-response ()
  "File should contain the raw weather response body after the separator."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "Berlin" "Clear skies 72°F")))
        (unwind-protect
            (let ((contents (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string))))
              (should (string-match-p "--- Raw Response ---" contents))
              (should (string-match-p "Clear skies 72°F" contents)))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(ert-deftest test-wttrin--save-debug-data-normal-file-contains-timestamp ()
  "File should contain a human-readable timestamp."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "Tokyo" "data")))
        (unwind-protect
            (let ((contents (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string))))
              ;; Timestamp should look like YYYY-MM-DD HH:MM:SS
              (should (string-match-p "^Timestamp: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}$"
                                      contents)))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--save-debug-data-boundary-unicode-location ()
  "File should preserve unicode characters in the location name."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "São Paulo, BR" "data")))
        (unwind-protect
            (let ((contents (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string))))
              (should (string-match-p "São Paulo" contents)))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(ert-deftest test-wttrin--save-debug-data-boundary-empty-raw-string ()
  "Empty raw-string should still produce a valid file (just no response body)."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      (let ((filepath (wttrin--save-debug-data "Paris" "")))
        (unwind-protect
            (progn
              (should (file-exists-p filepath))
              (let ((contents (with-temp-buffer
                                (insert-file-contents filepath)
                                (buffer-string))))
                ;; Should still have the headers and separator
                (should (string-match-p "Location: Paris" contents))
                (should (string-match-p "--- Raw Response ---" contents))))
          (when (file-exists-p filepath) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

;;; Error Cases

(ert-deftest test-wttrin--save-debug-data-error-nil-raw-string-should-not-crash ()
  "Nil raw-string should be handled gracefully, not cause an insert error.
This can happen when wttrin--display-weather is called with nil data
and debug mode is on — save-debug-data is called before validation."
  (test-wttrin--save-debug-data-setup)
  (unwind-protect
      ;; wttrin--save-debug-data calls (insert raw-string) which errors on nil.
      ;; This test documents the bug: a nil raw-string should produce a file
      ;; with an empty or placeholder response body, not crash.
      (let ((filepath (wttrin--save-debug-data "Paris" nil)))
        (unwind-protect
            (should (file-exists-p filepath))
          (when (and filepath (file-exists-p filepath)) (delete-file filepath))))
    (test-wttrin--save-debug-data-teardown)))

(provide 'test-wttrin--save-debug-data)
;;; test-wttrin--save-debug-data.el ends here
