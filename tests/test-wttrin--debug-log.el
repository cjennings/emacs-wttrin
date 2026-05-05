;;; test-wttrin--debug-log.el --- Tests for wttrin--debug-log -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--debug-log.  Verifies that messages are appended
;; only when `wttrin-debug' is non-nil, that format args are interpolated,
;; and that entries are timestamped (timestamp . message) cons cells.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-wttrin--debug-log-normal-appends-entry-when-debug-enabled ()
  "Calling debug-log with debug enabled appends a single entry."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "hello")
    (should (= 1 (length wttrin--debug-log)))))

(ert-deftest test-wttrin--debug-log-normal-interpolates-format-args ()
  "Format args are passed through `format' before storage."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "value=%d name=%s" 42 "foo")
    (should (string= "value=42 name=foo" (cdar wttrin--debug-log)))))

(ert-deftest test-wttrin--debug-log-normal-prepends-most-recent-entry ()
  "Most recent entry is at the head of the log (push semantics)."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "first")
    (wttrin--debug-log "second")
    (should (string= "second" (cdar wttrin--debug-log)))
    (should (string= "first" (cdadr wttrin--debug-log)))))

(ert-deftest test-wttrin--debug-log-normal-timestamp-has-millisecond-format ()
  "Timestamp is a string in HH:MM:SS.mmm format."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "anything")
    (should (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}$"
                            (caar wttrin--debug-log)))))

;;; Boundary Cases

(ert-deftest test-wttrin--debug-log-boundary-empty-format-string-stores-empty-message ()
  "An empty format string produces an entry with an empty message."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "")
    (should (= 1 (length wttrin--debug-log)))
    (should (string= "" (cdar wttrin--debug-log)))))

(ert-deftest test-wttrin--debug-log-boundary-no-format-args-stores-literal-string ()
  "Calling with only a format string and no args stores it as-is."
  (let ((wttrin-debug t)
        (wttrin--debug-log nil))
    (wttrin--debug-log "literal text with no format directives")
    (should (string= "literal text with no format directives"
                     (cdar wttrin--debug-log)))))

;;; Error Cases

(ert-deftest test-wttrin--debug-log-error-no-entry-when-debug-disabled ()
  "When wttrin-debug is nil, debug-log adds nothing."
  (let ((wttrin-debug nil)
        (wttrin--debug-log nil))
    (wttrin--debug-log "should not appear")
    (should-not wttrin--debug-log)))

(provide 'test-wttrin--debug-log)
;;; test-wttrin--debug-log.el ends here
