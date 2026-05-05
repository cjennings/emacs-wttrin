;;; test-debug-wttrin-enable.el --- Tests for debug-wttrin-enable -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for debug-wttrin-enable.  Verifies the interactive command
;; flips `wttrin-debug' to t and is idempotent when already enabled.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-debug-wttrin-enable-normal-flips-from-nil-to-t ()
  "Calling enable when wttrin-debug is nil sets it to t."
  (let ((wttrin-debug nil))
    (debug-wttrin-enable)
    (should (eq wttrin-debug t))))

(ert-deftest test-debug-wttrin-enable-normal-idempotent-when-already-enabled ()
  "Calling enable when wttrin-debug is already t leaves it t."
  (let ((wttrin-debug t))
    (debug-wttrin-enable)
    (should (eq wttrin-debug t))))

;;; Boundary Cases

(ert-deftest test-debug-wttrin-enable-boundary-overrides-non-boolean-truthy-value ()
  "Calling enable replaces a non-boolean truthy value with t."
  (let ((wttrin-debug 'verbose))
    (debug-wttrin-enable)
    (should (eq wttrin-debug t))))

(provide 'test-debug-wttrin-enable)
;;; test-debug-wttrin-enable.el ends here
