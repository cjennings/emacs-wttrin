;;; test-wttrin-debug-clear-log.el --- Tests for wttrin-debug-clear-log -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-debug-clear-log.  Verifies the interactive
;; command empties the debug log alist regardless of prior state.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-wttrin-debug-clear-log-normal-empties-populated-log ()
  "Clearing a non-empty log leaves it nil."
  (let ((wttrin--debug-log '(("00:00:01.000" . "one")
                             ("00:00:02.000" . "two"))))
    (wttrin-debug-clear-log)
    (should-not wttrin--debug-log)))

(ert-deftest test-wttrin-debug-clear-log-normal-multiple-calls-stay-empty ()
  "Calling clear twice in a row leaves the log empty."
  (let ((wttrin--debug-log '(("00:00:01.000" . "one"))))
    (wttrin-debug-clear-log)
    (wttrin-debug-clear-log)
    (should-not wttrin--debug-log)))

;;; Boundary Cases

(ert-deftest test-wttrin-debug-clear-log-boundary-clearing-already-empty-log ()
  "Clearing an already-empty log is a no-op and leaves it nil."
  (let ((wttrin--debug-log nil))
    (wttrin-debug-clear-log)
    (should-not wttrin--debug-log)))

(provide 'test-wttrin-debug-clear-log)
;;; test-wttrin-debug-clear-log.el ends here
