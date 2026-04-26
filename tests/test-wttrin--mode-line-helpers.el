;;; test-wttrin--mode-line-helpers.el --- Tests for mode-line helper functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--make-emoji-icon and wttrin--set-mode-line-string.
;; These helpers extract the common mode-line icon creation and string
;; assignment pattern used by placeholder, error, and display functions.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-helpers-setup ()
  "Setup for mode-line helper tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil))

(defun test-wttrin--mode-line-helpers-teardown ()
  "Teardown for mode-line helper tests."
  (testutil-wttrin-teardown)
  (setq wttrin-mode-line-string nil))

;;; --------------------------------------------------------------------------
;;; wttrin--make-emoji-icon
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--make-emoji-icon-normal-without-font ()
  "Without emoji font configured, should return the plain emoji string."
  (let ((wttrin-mode-line-emoji-font nil))
    (should (equal (wttrin--make-emoji-icon "☀") "☀"))))

(ert-deftest test-wttrin--make-emoji-icon-normal-with-font ()
  "With emoji font configured, should apply font family face property."
  (let ((wttrin-mode-line-emoji-font "Noto Color Emoji"))
    (let ((result (wttrin--make-emoji-icon "☀")))
      (should (stringp result))
      (let ((face (get-text-property 0 'face result)))
        (should (equal (plist-get face :family) "Noto Color Emoji"))
        (should (equal (plist-get face :height) 1.0))))))

(ert-deftest test-wttrin--make-emoji-icon-normal-with-foreground ()
  "Foreground color should be applied when specified."
  (let ((wttrin-mode-line-emoji-font nil))
    (let ((result (wttrin--make-emoji-icon "☀" "gray60")))
      (let ((face (get-text-property 0 'face result)))
        (should (equal (plist-get face :foreground) "gray60"))))))

(ert-deftest test-wttrin--make-emoji-icon-normal-with-font-and-foreground ()
  "Both font and foreground should be applied together."
  (let ((wttrin-mode-line-emoji-font "Noto Color Emoji"))
    (let ((result (wttrin--make-emoji-icon "⏳" "gray60")))
      (let ((face (get-text-property 0 'face result)))
        (should (equal (plist-get face :family) "Noto Color Emoji"))
        (should (equal (plist-get face :foreground) "gray60"))))))

;;; Boundary Cases

(ert-deftest test-wttrin--make-emoji-icon-boundary-nil-foreground-no-color ()
  "Nil foreground should not add any :foreground property when no font."
  (let ((wttrin-mode-line-emoji-font nil))
    (let ((result (wttrin--make-emoji-icon "☀" nil)))
      ;; Without font or foreground, should be plain string
      (should (equal result "☀")))))

(ert-deftest test-wttrin--make-emoji-icon-boundary-nil-foreground-with-font ()
  "With font set and nil foreground, the face plist must omit :foreground entirely.
A literal `:foreground nil' entry triggers \"Invalid face attribute\" warnings on
every redisplay.  `plist-member' (not `plist-get') is required: `plist-get' can't
distinguish a missing key from a present key bound to nil."
  (let ((wttrin-mode-line-emoji-font "Noto Color Emoji"))
    (let* ((result (wttrin--make-emoji-icon "☀" nil))
           (face (get-text-property 0 'face result)))
      (should (equal (plist-get face :family) "Noto Color Emoji"))
      (should-not (plist-member face :foreground)))))

;;; --------------------------------------------------------------------------
;;; wttrin--set-mode-line-string
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--set-mode-line-string-normal-sets-string ()
  "Should set wttrin-mode-line-string to a non-nil propertized value."
  (test-wttrin--mode-line-helpers-setup)
  (unwind-protect
      (progn
        (wttrin--set-mode-line-string "X" "tooltip text")
        (should wttrin-mode-line-string))
    (test-wttrin--mode-line-helpers-teardown)))

(ert-deftest test-wttrin--set-mode-line-string-normal-includes-icon ()
  "The icon text should appear in the mode-line string."
  (test-wttrin--mode-line-helpers-setup)
  (unwind-protect
      (progn
        (wttrin--set-mode-line-string "⏳" "tip")
        (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string))))
    (test-wttrin--mode-line-helpers-teardown)))

(ert-deftest test-wttrin--set-mode-line-string-normal-has-tooltip ()
  "The help-echo property should contain the tooltip text."
  (test-wttrin--mode-line-helpers-setup)
  (unwind-protect
      (progn
        (wttrin--set-mode-line-string "X" "Weather is sunny")
        (should (equal (get-text-property 0 'help-echo wttrin-mode-line-string)
                       "Weather is sunny")))
    (test-wttrin--mode-line-helpers-teardown)))

(ert-deftest test-wttrin--set-mode-line-string-normal-has-keymap ()
  "The local-map property should be the mode-line keymap."
  (test-wttrin--mode-line-helpers-setup)
  (unwind-protect
      (progn
        (wttrin--set-mode-line-string "X" "tip")
        (should (eq (get-text-property 0 'local-map wttrin-mode-line-string)
                    wttrin--mode-line-map)))
    (test-wttrin--mode-line-helpers-teardown)))

(ert-deftest test-wttrin--set-mode-line-string-normal-has-mouse-face ()
  "The mouse-face property should highlight on hover."
  (test-wttrin--mode-line-helpers-setup)
  (unwind-protect
      (progn
        (wttrin--set-mode-line-string "X" "tip")
        (should (equal (get-text-property 0 'mouse-face wttrin-mode-line-string)
                       'mode-line-highlight)))
    (test-wttrin--mode-line-helpers-teardown)))

(provide 'test-wttrin--mode-line-helpers)
;;; test-wttrin--mode-line-helpers.el ends here
