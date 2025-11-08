;;; test-wttrin-integration.el --- Integration tests for wttrin package loading -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Integration tests to verify wttrin package loads correctly and
;; key functionality is available.

;;; Code:

(require 'ert)

;;; Package Loading Tests

(ert-deftest test-wttrin-integration-package-loads ()
  "Test that wttrin package loads without errors."
  ;; This test runs AFTER wttrin.el is loaded by the test runner
  ;; If we got here, the package loaded successfully
  (should t))

(ert-deftest test-wttrin-integration-provide-statement ()
  "Test that wttrin feature is provided."
  (should (featurep 'wttrin)))

;;; Dependency Tests

(ert-deftest test-wttrin-integration-xterm-color-available ()
  "Test that xterm-color dependency is available.
This is a REQUIRED dependency - wttrin cannot function without it."
  (should (featurep 'xterm-color)))

(ert-deftest test-wttrin-integration-url-available ()
  "Test that url library is available (built-in)."
  (should (require 'url nil t)))

;;; Function Availability Tests

(ert-deftest test-wttrin-integration-main-command-defined ()
  "Test that main wttrin command is defined."
  (should (fboundp 'wttrin)))

(ert-deftest test-wttrin-integration-main-command-is-interactive ()
  "Test that main wttrin command is interactive."
  (should (commandp 'wttrin)))

(ert-deftest test-wttrin-integration-wttrin-mode-defined ()
  "Test that wttrin-mode is defined."
  (should (fboundp 'wttrin-mode)))

(ert-deftest test-wttrin-integration-mode-line-mode-defined ()
  "Test that wttrin-mode-line-mode is defined."
  (should (fboundp 'wttrin-mode-line-mode)))

(ert-deftest test-wttrin-integration-clear-cache-defined ()
  "Test that cache clear command is defined."
  (should (fboundp 'wttrin-clear-cache)))

;;; Configuration Variable Tests

(ert-deftest test-wttrin-integration-defcustoms-exist ()
  "Test that key defcustom variables are defined."
  (should (boundp 'wttrin-default-locations))
  (should (boundp 'wttrin-unit-system))
  (should (boundp 'wttrin-cache-ttl))
  (should (boundp 'wttrin-cache-max-entries))
  (should (boundp 'wttrin-mode-line-favorite-location))
  (should (boundp 'wttrin-mode-line-refresh-interval))
  (should (boundp 'wttrin-mode-line-startup-delay)))

(ert-deftest test-wttrin-integration-internal-vars-exist ()
  "Test that internal variables are defined."
  (should (boundp 'wttrin--cache))
  (should (boundp 'wttrin--mode-line-map))
  (should (boundp 'wttrin-mode-line-string)))

;;; Keymap Tests

(ert-deftest test-wttrin-integration-wttrin-mode-map-exists ()
  "Test that wttrin-mode keymap exists."
  (should (boundp 'wttrin-mode-map))
  (should (keymapp wttrin-mode-map)))

(ert-deftest test-wttrin-integration-mode-line-map-exists ()
  "Test that mode-line keymap exists."
  (should (boundp 'wttrin--mode-line-map))
  (should (keymapp wttrin--mode-line-map)))

;;; Package Metadata Tests

(ert-deftest test-wttrin-integration-package-version ()
  "Test that package has version information."
  ;; Check if package-get-version would work (not calling it as it's not always available)
  ;; Just verify the Version header exists in the file
  (let ((version-line (with-temp-buffer
                        (insert-file-contents "wttrin.el" nil 0 500)
                        (goto-char (point-min))
                        (re-search-forward "^;; Version: \\([0-9.]+\\)" nil t)
                        (match-string 1))))
    (should version-line)
    (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" version-line))))

;;; Autoload Tests

(ert-deftest test-wttrin-integration-autoload-cookies ()
  "Test that key functions have autoload cookies."
  ;; Verify autoload cookies exist in source (they become effective after package install)
  (let ((source (with-temp-buffer
                  (insert-file-contents "wttrin.el")
                  (buffer-string))))
    ;; Main command should have autoload
    (should (string-match-p ";;;###autoload\n(defun wttrin" source))
    ;; Mode-line mode should have autoload
    (should (string-match-p ";;;###autoload\n(define-minor-mode wttrin-mode-line-mode" source))))

;;; Smoke Test - Basic Functionality

(ert-deftest test-wttrin-integration-url-building ()
  "Test that URL building works correctly."
  (let ((wttrin-unit-system "m"))
    (should (string= "https://wttr.in/Paris?mA"
                     (wttrin--build-url "Paris")))))

(ert-deftest test-wttrin-integration-cache-operations ()
  "Test that cache operations don't error."
  ;; Clear cache
  (wttrin-clear-cache)
  (should (= 0 (hash-table-count wttrin--cache)))

  ;; Add an entry
  (puthash "test-location" (cons (float-time) "test-data") wttrin--cache)
  (should (= 1 (hash-table-count wttrin--cache)))

  ;; Clear again
  (wttrin-clear-cache)
  (should (= 0 (hash-table-count wttrin--cache))))

(ert-deftest test-wttrin-integration-mode-line-display ()
  "Test that mode-line display function doesn't error."
  (let ((wttrin-mode-line-favorite-location "Test, CA")
        (wttrin--mode-line-tooltip-data "Test weather"))
    ;; Should not error (function returns nil but sets wttrin-mode-line-string)
    (wttrin--mode-line-update-display "☀️")
    ;; Should set the mode-line string
    (should wttrin-mode-line-string)
    (should (stringp wttrin-mode-line-string))))

(provide 'test-wttrin-integration)
;;; test-wttrin-integration.el ends here
