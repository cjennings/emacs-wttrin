;;; test-wttrin-debug-show-log.el --- Tests for wttrin-debug-show-log -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-debug-show-log.  Verifies the *wttrin-debug-log*
;; buffer is created with the expected header, entry rendering, and the
;; reversed (oldest-first) ordering used for human reading.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Setup and Teardown

(defun test-wttrin-debug-show-log-setup ()
  "Setup for show-log tests."
  (when (get-buffer "*wttrin-debug-log*")
    (kill-buffer "*wttrin-debug-log*")))

(defun test-wttrin-debug-show-log-teardown ()
  "Teardown for show-log tests."
  (when (get-buffer "*wttrin-debug-log*")
    (kill-buffer "*wttrin-debug-log*")))

;;; Normal Cases

(ert-deftest test-wttrin-debug-show-log-normal-creates-display-buffer ()
  "Show-log creates the *wttrin-debug-log* buffer."
  (test-wttrin-debug-show-log-setup)
  (unwind-protect
      (let ((wttrin--debug-log nil))
        (wttrin-debug-show-log)
        (should (get-buffer "*wttrin-debug-log*")))
    (test-wttrin-debug-show-log-teardown)))

(ert-deftest test-wttrin-debug-show-log-normal-header-shows-entry-count ()
  "Header reflects the number of entries currently in the log."
  (test-wttrin-debug-show-log-setup)
  (unwind-protect
      (let ((wttrin--debug-log '(("00:00:03.000" . "c")
                                 ("00:00:02.000" . "b")
                                 ("00:00:01.000" . "a"))))
        (wttrin-debug-show-log)
        (with-current-buffer "*wttrin-debug-log*"
          (should (string-match-p "Total entries: 3" (buffer-string)))))
    (test-wttrin-debug-show-log-teardown)))

(ert-deftest test-wttrin-debug-show-log-normal-renders-entries-with-timestamps ()
  "Each entry appears as [timestamp] message in the display."
  (test-wttrin-debug-show-log-setup)
  (unwind-protect
      (let ((wttrin--debug-log '(("12:34:56.789" . "hello world"))))
        (wttrin-debug-show-log)
        (with-current-buffer "*wttrin-debug-log*"
          (should (string-match-p "\\[12:34:56\\.789\\] hello world"
                                  (buffer-string)))))
    (test-wttrin-debug-show-log-teardown)))

(ert-deftest test-wttrin-debug-show-log-normal-entries-rendered-oldest-first ()
  "Storage is newest-first (push); display reverses to oldest-first for reading."
  (test-wttrin-debug-show-log-setup)
  (unwind-protect
      (let ((wttrin--debug-log '(("00:00:03.000" . "third")
                                 ("00:00:02.000" . "second")
                                 ("00:00:01.000" . "first"))))
        (wttrin-debug-show-log)
        (with-current-buffer "*wttrin-debug-log*"
          (let* ((contents (buffer-string))
                 (pos-first (string-match "first" contents))
                 (pos-second (string-match "second" contents))
                 (pos-third (string-match "third" contents)))
            (should pos-first)
            (should pos-second)
            (should pos-third)
            (should (< pos-first pos-second))
            (should (< pos-second pos-third)))))
    (test-wttrin-debug-show-log-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin-debug-show-log-boundary-empty-log-shows-placeholder ()
  "Empty log renders the \"No log entries yet\" placeholder."
  (test-wttrin-debug-show-log-setup)
  (unwind-protect
      (let ((wttrin--debug-log nil))
        (wttrin-debug-show-log)
        (with-current-buffer "*wttrin-debug-log*"
          (should (string-match-p "(No log entries yet)" (buffer-string)))
          (should (string-match-p "Total entries: 0" (buffer-string)))))
    (test-wttrin-debug-show-log-teardown)))

(provide 'test-wttrin-debug-show-log)
;;; test-wttrin-debug-show-log.el ends here
