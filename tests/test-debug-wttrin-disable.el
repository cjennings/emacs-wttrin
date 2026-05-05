;;; test-debug-wttrin-disable.el --- Tests for debug-wttrin-disable -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for debug-wttrin-disable.  Verifies the interactive command
;; clears `wttrin-debug' and is idempotent when already disabled.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-debug-wttrin-disable-normal-flips-from-t-to-nil ()
  "Calling disable when wttrin-debug is t sets it to nil."
  (let ((wttrin-debug t))
    (debug-wttrin-disable)
    (should-not wttrin-debug)))

(ert-deftest test-debug-wttrin-disable-normal-idempotent-when-already-disabled ()
  "Calling disable when wttrin-debug is already nil leaves it nil."
  (let ((wttrin-debug nil))
    (debug-wttrin-disable)
    (should-not wttrin-debug)))

;;; Boundary Cases

(ert-deftest test-debug-wttrin-disable-boundary-clears-non-boolean-truthy-value ()
  "Calling disable replaces any truthy value with nil."
  (let ((wttrin-debug 'verbose))
    (debug-wttrin-disable)
    (should-not wttrin-debug)))

(provide 'test-debug-wttrin-disable)
;;; test-debug-wttrin-disable.el ends here
