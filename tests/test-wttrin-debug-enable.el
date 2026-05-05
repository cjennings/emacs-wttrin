;;; test-wttrin-debug-enable.el --- Tests for wttrin-debug-enable -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin-debug-enable.  Verifies the interactive command
;; flips `wttrin-debug' to t and is idempotent when already enabled.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Normal Cases

(ert-deftest test-wttrin-debug-enable-normal-flips-from-nil-to-t ()
  "Calling enable when wttrin-debug is nil sets it to t."
  (let ((wttrin-debug nil))
    (wttrin-debug-enable)
    (should (eq wttrin-debug t))))

(ert-deftest test-wttrin-debug-enable-normal-idempotent-when-already-enabled ()
  "Calling enable when wttrin-debug is already t leaves it t."
  (let ((wttrin-debug t))
    (wttrin-debug-enable)
    (should (eq wttrin-debug t))))

;;; Boundary Cases

(ert-deftest test-wttrin-debug-enable-boundary-overrides-non-boolean-truthy-value ()
  "Calling enable replaces a non-boolean truthy value with t."
  (let ((wttrin-debug 'verbose))
    (wttrin-debug-enable)
    (should (eq wttrin-debug t))))

(provide 'test-wttrin-debug-enable)
;;; test-wttrin-debug-enable.el ends here
