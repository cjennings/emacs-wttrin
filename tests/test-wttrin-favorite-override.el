;;; test-wttrin-favorite-override.el --- Tests for the runtime favorite override -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; Unit tests for the split between the configured favorite
;; (`wttrin-favorite-location', a defcustom set in init) and the runtime
;; favorite (`wttrin--favorite-override', set by `d'/the geolocation commands
;; and persisted by savehist).  The effective favorite resolves via
;; `wttrin--favorite-location': the override wins over the config, so a favorite
;; chosen at runtime is not clobbered by an init that also sets the option.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)

;;; wttrin--favorite-location (resolver)

(ert-deftest test-wttrin-favorite-override-normal-config-when-no-override ()
  "Normal: with no override, the effective favorite is the configured one."
  (let ((wttrin-favorite-location "New Orleans, LA")
        (wttrin--favorite-override nil))
    (should (equal "New Orleans, LA" (wttrin--favorite-location)))))

(ert-deftest test-wttrin-favorite-override-normal-override-wins ()
  "Normal: a runtime override shadows the configured favorite."
  (let ((wttrin-favorite-location "New Orleans, LA")
        (wttrin--favorite-override "Hyatt Place Warwick, RI"))
    (should (equal "Hyatt Place Warwick, RI" (wttrin--favorite-location)))))

(ert-deftest test-wttrin-favorite-override-boundary-both-nil ()
  "Boundary: no config and no override yields nil (favorite disabled)."
  (let ((wttrin-favorite-location nil)
        (wttrin--favorite-override nil))
    (should (null (wttrin--favorite-location)))))

(ert-deftest test-wttrin-favorite-override-boundary-auto-detect-override ()
  "Boundary: the override carries the tri-state t (auto-detect)."
  (let ((wttrin-favorite-location "New Orleans, LA")
        (wttrin--favorite-override t))
    (should (eq t (wttrin--favorite-location)))))

(ert-deftest test-wttrin-favorite-override-regression-init-does-not-clobber ()
  "Regression: setting the config after an override (as an init `setopt' would)
does not change the effective favorite.  This is the bug where a favorite set
with `d' reverted to the init value on restart."
  (let ((wttrin--favorite-override "Hyatt Place Warwick, RI")
        (wttrin-favorite-location nil))
    ;; Simulate the init running its (setopt wttrin-favorite-location ...).
    (setq wttrin-favorite-location "New Orleans, LA")
    (should (equal "Hyatt Place Warwick, RI" (wttrin--favorite-location)))))

;;; wttrin--set-favorite-location writes the override, not the config

(ert-deftest test-wttrin-favorite-override-normal-setter-writes-override ()
  "Normal: the setter writes the runtime override and leaves the config intact."
  (let ((wttrin-favorite-location "New Orleans, LA")
        (wttrin--favorite-override nil)
        (wttrin--location-history nil))
    (wttrin--set-favorite-location "Paris, FR")
    (should (equal "Paris, FR" wttrin--favorite-override))
    (should (equal "New Orleans, LA" wttrin-favorite-location))
    (should (equal "Paris, FR" (wttrin--favorite-location)))))

;;; savehist registration targets the runtime vars, not the config defcustoms

(ert-deftest test-wttrin-favorite-override-normal-savehist-registers-override ()
  "Normal: savehist persists the override, not the config defcustom."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring)))
    (wttrin--savehist-register)
    (should (memq 'wttrin--favorite-override savehist-additional-variables))
    (should (memq 'wttrin--saved-locations-runtime savehist-additional-variables))
    (should-not (memq 'wttrin-favorite-location savehist-additional-variables))
    (should-not (memq 'wttrin-saved-locations savehist-additional-variables))))

(provide 'test-wttrin-favorite-override)
;;; test-wttrin-favorite-override.el ends here
