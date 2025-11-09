;;; test-wttrin-smoke.el --- Smoke tests for wttrin package loading -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Smoke tests to verify wttrin package loads correctly and core
;; infrastructure is available.  These are sanity checks that catch:
;; - Package installation issues
;; - Missing dependencies
;; - Public API breakage
;; - Autoload cookie problems
;; - Package metadata issues
;;
;; These tests do NOT test functionality (that's in unit tests).
;; They only verify that functions/variables exist and have correct types.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Package Loading Tests

(ert-deftest test-wttrin-smoke-package-loads ()
  "Test that wttrin package loads without errors.
If we got here, the package loaded successfully via (require 'wttrin)."
  (should t))

(ert-deftest test-wttrin-smoke-provide-statement ()
  "Test that wttrin feature is provided."
  (should (featurep 'wttrin)))

;;; Dependency Tests

(ert-deftest test-wttrin-smoke-xterm-color-available ()
  "Test that xterm-color dependency can be loaded when needed.
This is a REQUIRED dependency - wttrin cannot function without it."
  (should (require 'xterm-color nil t)))

(ert-deftest test-wttrin-smoke-url-available ()
  "Test that url library is available (built-in)."
  (should (require 'url nil t)))

;;; Function Availability Tests

(ert-deftest test-wttrin-smoke-main-command-defined ()
  "Test that main wttrin command is defined."
  (should (fboundp 'wttrin)))

(ert-deftest test-wttrin-smoke-main-command-is-interactive ()
  "Test that main wttrin command is interactive."
  (should (commandp 'wttrin)))

(ert-deftest test-wttrin-smoke-wttrin-mode-defined ()
  "Test that wttrin-mode is defined."
  (should (fboundp 'wttrin-mode)))

(ert-deftest test-wttrin-smoke-mode-line-mode-defined ()
  "Test that wttrin-mode-line-mode is defined."
  (should (fboundp 'wttrin-mode-line-mode)))

(ert-deftest test-wttrin-smoke-clear-cache-defined ()
  "Test that cache clear command is defined."
  (should (fboundp 'wttrin-clear-cache)))

;;; Configuration Variable Tests

(ert-deftest test-wttrin-smoke-defcustoms-exist ()
  "Test that key defcustom variables are defined."
  (should (boundp 'wttrin-default-locations))
  (should (boundp 'wttrin-unit-system))
  (should (boundp 'wttrin-cache-ttl))
  (should (boundp 'wttrin-cache-max-entries))
  (should (boundp 'wttrin-mode-line-favorite-location))
  (should (boundp 'wttrin-mode-line-refresh-interval))
  (should (boundp 'wttrin-mode-line-startup-delay)))

(ert-deftest test-wttrin-smoke-internal-vars-exist ()
  "Test that internal variables are defined."
  (should (boundp 'wttrin--cache))
  (should (boundp 'wttrin--mode-line-map))
  (should (boundp 'wttrin-mode-line-string)))

;;; Keymap Tests

(ert-deftest test-wttrin-smoke-wttrin-mode-map-exists ()
  "Test that wttrin-mode keymap exists and is a keymap."
  (should (boundp 'wttrin-mode-map))
  (should (keymapp wttrin-mode-map)))

(ert-deftest test-wttrin-smoke-mode-line-map-exists ()
  "Test that mode-line keymap exists and is a keymap."
  (should (boundp 'wttrin--mode-line-map))
  (should (keymapp wttrin--mode-line-map)))

;;; Package Metadata Tests

(ert-deftest test-wttrin-smoke-package-version ()
  "Test that package has version information in correct format."
  ;; Verify the Version header exists in the file
  (let ((version-line (with-temp-buffer
                        (insert-file-contents "wttrin.el" nil 0 500)
                        (goto-char (point-min))
                        (re-search-forward "^;; Version: \\([0-9.]+\\)" nil t)
                        (match-string 1))))
    (should version-line)
    (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" version-line))))

(ert-deftest test-wttrin-smoke-autoload-cookies ()
  "Test that key functions have autoload cookies in source."
  ;; Verify autoload cookies exist (they become effective after package install)
  (let ((source (with-temp-buffer
                  (insert-file-contents "wttrin.el")
                  (buffer-string))))
    ;; Main command should have autoload
    (should (string-match-p ";;;###autoload\n(defun wttrin" source))
    ;; Mode-line mode should have autoload
    (should (string-match-p ";;;###autoload\n(define-minor-mode wttrin-mode-line-mode" source))))

(provide 'test-wttrin-smoke)
;;; test-wttrin-smoke.el ends here
