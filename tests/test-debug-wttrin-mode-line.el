;;; test-debug-wttrin-mode-line.el --- Tests for debug-wttrin-mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for debug-wttrin-mode-line.  This function is a UI-heavy
;; diagnostic dump, so testing focuses on the two top-level branches:
;; - When *wttr.in* is missing, no diagnostic buffer is produced.
;; - When *wttr.in* exists, the diagnostic buffer is created and contains
;;   the expected section labels.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

;;; Setup and Teardown

(defun test-debug-wttrin-mode-line-setup ()
  "Setup for debug-wttrin-mode-line tests."
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*"))
  (when (get-buffer "*wttrin-mode-debug*")
    (kill-buffer "*wttrin-mode-debug*")))

(defun test-debug-wttrin-mode-line-teardown ()
  "Teardown for debug-wttrin-mode-line tests."
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*"))
  (when (get-buffer "*wttrin-mode-debug*")
    (kill-buffer "*wttrin-mode-debug*")))

;;; Normal Cases

(ert-deftest test-debug-wttrin-mode-line-normal-no-buffer-skips-diagnostic-buffer ()
  "When the *wttr.in* buffer is absent, no diagnostic buffer is created."
  (test-debug-wttrin-mode-line-setup)
  (unwind-protect
      (progn
        (debug-wttrin-mode-line)
        (should-not (get-buffer "*wttrin-mode-debug*")))
    (test-debug-wttrin-mode-line-teardown)))

(ert-deftest test-debug-wttrin-mode-line-normal-buffer-exists-creates-diagnostic-buffer ()
  "When the *wttr.in* buffer exists, the diagnostic buffer is created."
  (test-debug-wttrin-mode-line-setup)
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer-create "*wttr.in*")
          (wttrin-mode))
        (debug-wttrin-mode-line)
        (should (get-buffer "*wttrin-mode-debug*")))
    (test-debug-wttrin-mode-line-teardown)))

(ert-deftest test-debug-wttrin-mode-line-normal-diagnostic-buffer-contains-section-labels ()
  "Diagnostic buffer contains the expected section labels."
  (test-debug-wttrin-mode-line-setup)
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer-create "*wttr.in*")
          (wttrin-mode))
        (debug-wttrin-mode-line)
        (with-current-buffer "*wttrin-mode-debug*"
          (let ((contents (buffer-string)))
            (should (string-match-p "Wttrin Mode-Line Debug Info" contents))
            (should (string-match-p "Buffer:" contents))
            (should (string-match-p "Major mode:" contents))
            (should (string-match-p "mode-name variable:" contents))
            (should (string-match-p "mode-line-format first 5 elements:" contents)))))
    (test-debug-wttrin-mode-line-teardown)))

(provide 'test-debug-wttrin-mode-line)
;;; test-debug-wttrin-mode-line.el ends here
