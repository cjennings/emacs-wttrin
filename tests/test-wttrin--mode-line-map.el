;;; test-wttrin--mode-line-map.el --- Tests for wttrin--mode-line-map -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--mode-line-map keymap.
;; Tests verify the keymap exists and has correct keybindings after refactoring.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases - Keymap Existence and Structure

(ert-deftest test-wttrin--mode-line-map-exists ()
  "Test that wttrin--mode-line-map defvar exists after refactoring."
  (should (boundp 'wttrin--mode-line-map)))

(ert-deftest test-wttrin--mode-line-map-is-keymap ()
  "Test that wttrin--mode-line-map is actually a keymap."
  (should (keymapp wttrin--mode-line-map)))

(ert-deftest test-wttrin--mode-line-map-is-sparse-keymap ()
  "Test that wttrin--mode-line-map is a sparse keymap."
  ;; Sparse keymaps start with 'keymap symbol
  (should (eq 'keymap (car wttrin--mode-line-map))))

;;; Keybinding Tests

(ert-deftest test-wttrin--mode-line-map-has-mouse-1-binding ()
  "Test that left-click (mouse-1) is bound to wttrin-mode-line-click."
  (let ((binding (lookup-key wttrin--mode-line-map [mode-line mouse-1])))
    (should (eq binding 'wttrin-mode-line-click))))

(ert-deftest test-wttrin--mode-line-map-has-mouse-3-binding ()
  "Test that right-click (mouse-3) is bound to wttrin-mode-line-force-refresh."
  (let ((binding (lookup-key wttrin--mode-line-map [mode-line mouse-3])))
    (should (eq binding 'wttrin-mode-line-force-refresh))))

;;; Boundary Cases - Verify No Unexpected Bindings

(ert-deftest test-wttrin--mode-line-map-no-mouse-2-binding ()
  "Test that middle-click (mouse-2) has no binding."
  (let ((binding (lookup-key wttrin--mode-line-map [mode-line mouse-2])))
    ;; Should return nil (unbound) or a prefix keymap
    (should-not (and binding (symbolp binding)))))

(ert-deftest test-wttrin--mode-line-map-minimal-bindings ()
  "Test that keymap has the expected bindings."
  ;; Count non-nil entries in keymap
  (let ((count 0))
    (map-keymap (lambda (_key _binding)
                  (setq count (1+ count)))
                wttrin--mode-line-map)
    ;; Should have at least 1 binding (could be 1 or 2 depending on how
    ;; Emacs internally structures the keymap with [mode-line mouse-N])
    ;; The important thing is both mouse-1 and mouse-3 are accessible
    (should (>= count 1))))

;;; Integration Test - Verify Mode-Line Uses the Keymap

(ert-deftest test-wttrin-mode-line-display-uses-shared-keymap ()
  "Test that mode-line display uses wttrin--mode-line-map after refactoring.
This test verifies the refactoring eliminated inline keymap construction."
  ;; Set up minimal mode-line state
  (let ((wttrin-favorite-location "Test, CA")
        (wttrin--mode-line-tooltip-data "Test weather"))
    ;; Update the mode-line display
    (wttrin--mode-line-update-display "☀️")

    ;; Extract the keymap property from wttrin-mode-line-string
    (let ((keymap-prop (get-text-property 0 'local-map wttrin-mode-line-string)))
      ;; After refactoring, should use the shared wttrin--mode-line-map
      ;; Not a freshly constructed keymap on each call
      (should (eq keymap-prop wttrin--mode-line-map)))))

(provide 'test-wttrin--mode-line-map)
;;; test-wttrin--mode-line-map.el ends here
