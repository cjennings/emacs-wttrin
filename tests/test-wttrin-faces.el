;;; test-wttrin-faces.el --- Tests for wttrin themeable faces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests verifying the package's customizable faces are defined so
;; themes and `customize-face' can target them.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin-faces-normal-all-defined ()
  "Normal: every package face is defined after loading wttrin."
  (dolist (face '(wttrin-mode-line-stale
                  wttrin-staleness-header
                  wttrin-instructions
                  wttrin-key))
    (should (facep face))))

(provide 'test-wttrin-faces)
;;; test-wttrin-faces.el ends here
