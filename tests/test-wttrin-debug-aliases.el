;;; test-wttrin-debug-aliases.el --- Tests for obsolete debug aliases -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; The four `debug-wttrin-*' names were renamed to `wttrin-debug-*' in
;; 0.4.0 to follow the package-prefix convention.  The old names remain
;; as obsolete aliases so existing keybindings keep working.  These tests
;; verify each alias resolves to the new function and is marked obsolete
;; with the expected target and version.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(require 'wttrin-debug
         (expand-file-name "wttrin-debug.el"
                           (file-name-directory (locate-library "wttrin"))))

(defconst test-wttrin-debug-aliases-pairs
  '((debug-wttrin-show-raw   . wttrin-debug-show-raw)
    (debug-wttrin-enable     . wttrin-debug-enable)
    (debug-wttrin-disable    . wttrin-debug-disable)
    (debug-wttrin-mode-line  . wttrin-debug-mode-line))
  "Old-name to new-name pairs for the renamed debug functions.")

;;; Normal Cases

(ert-deftest test-wttrin-debug-aliases-normal-each-old-name-resolves-to-new ()
  "Every old name is fboundp and indirects to its new name."
  (dolist (pair test-wttrin-debug-aliases-pairs)
    (let ((old (car pair))
          (new (cdr pair)))
      (should (fboundp old))
      (should (eq (indirect-function old) (symbol-function new))))))

(ert-deftest test-wttrin-debug-aliases-normal-each-old-name-marked-obsolete ()
  "Every old name has byte-obsolete-info pointing at the new name and 0.4.0."
  (dolist (pair test-wttrin-debug-aliases-pairs)
    (let* ((old (car pair))
           (new (cdr pair))
           (info (get old 'byte-obsolete-info)))
      (should info)
      ;; byte-obsolete-info is (CURRENT-NAME HANDLER WHEN)
      (should (eq new (car info)))
      (should (string= "0.4.0" (nth 2 info))))))

(provide 'test-wttrin-debug-aliases)
;;; test-wttrin-debug-aliases.el ends here
