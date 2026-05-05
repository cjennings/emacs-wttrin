;;; test-wttrin-debug-disable.el --- Tests for wttrin-debug-disable -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-debug-disable.  Verifies the interactive command
;; clears `wttrin-debug' and is idempotent when already disabled.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-wttrin-debug-disable-normal-flips-from-t-to-nil ()
  "Calling disable when wttrin-debug is t sets it to nil."
  (let ((wttrin-debug t))
    (wttrin-debug-disable)
    (should-not wttrin-debug)))

(ert-deftest test-wttrin-debug-disable-normal-idempotent-when-already-disabled ()
  "Calling disable when wttrin-debug is already nil leaves it nil."
  (let ((wttrin-debug nil))
    (wttrin-debug-disable)
    (should-not wttrin-debug)))

;;; Boundary Cases

(ert-deftest test-wttrin-debug-disable-boundary-clears-non-boolean-truthy-value ()
  "Calling disable replaces any truthy value with nil."
  (let ((wttrin-debug 'verbose))
    (wttrin-debug-disable)
    (should-not wttrin-debug)))

(provide 'test-wttrin-debug-disable)
;;; test-wttrin-debug-disable.el ends here
