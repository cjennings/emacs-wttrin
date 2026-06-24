;;; test-wttrin-make-default.el --- Tests for promote-to-default command -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--set-favorite-location and wttrin-make-default,
;; the weather-buffer command (bound to "d") that promotes the displayed
;; location to the persisted favorite.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)

;;; --------------------------------------------------------------------------
;;; wttrin--set-favorite-location
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin--set-favorite-location-normal-sets-variable ()
  "Normal: sets `wttrin-favorite-location' to the given location."
  (let ((wttrin-favorite-location nil)
        (savehist-additional-variables nil))
    (wttrin--set-favorite-location "Paris, FR")
    (should (equal wttrin-favorite-location "Paris, FR"))))

(ert-deftest test-wttrin--set-favorite-location-error-no-savehist-loaded ()
  "Error: setting the favorite works even when savehist is not loaded.
The setter must not touch `savehist-additional-variables' directly (it may be
unbound); persistence is left to `wttrin--savehist-register'."
  (let ((wttrin-favorite-location nil))
    ;; Simulate savehist absent: the variable is unbound.
    (cl-letf (((symbol-function 'wttrin--savehist-register)
               (lambda () (error "Should not be called from the setter"))))
      (wttrin--set-favorite-location "Oslo, NO")
      (should (equal wttrin-favorite-location "Oslo, NO")))))

(ert-deftest test-wttrin--set-favorite-location-normal-drops-from-history ()
  "Normal: promoting a location removes it from the search history."
  (let ((wttrin-favorite-location nil)
        (wttrin--location-history '("Reykjavik" "Oslo, NO")))
    (wttrin--set-favorite-location "Reykjavik")
    (should-not (member "Reykjavik" wttrin--location-history))
    (should (equal wttrin--location-history '("Oslo, NO")))))

(ert-deftest test-wttrin--set-favorite-location-boundary-not-in-history-is-noop ()
  "Boundary: promoting a location absent from history leaves history intact."
  (let ((wttrin-favorite-location nil)
        (wttrin--location-history '("Oslo, NO")))
    (wttrin--set-favorite-location "Berkeley, CA")
    (should (equal wttrin--location-history '("Oslo, NO")))))

(ert-deftest test-wttrin-favorite-savehist-register-includes-favorite ()
  "Normal: `wttrin--savehist-register' registers the favorite for persistence."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring)))
    (wttrin--savehist-register)
    (should (memq 'wttrin-favorite-location savehist-additional-variables))))

;;; --------------------------------------------------------------------------
;;; wttrin-make-default
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-make-default-normal-sets-favorite-from-current ()
  "Normal: promotes the buffer's current location to the favorite."
  (let ((wttrin-favorite-location nil)
        (savehist-additional-variables nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "Tokyo, JP")
      (wttrin-make-default)
      (should (equal wttrin-favorite-location "Tokyo, JP")))))

;;; Boundary Cases

(ert-deftest test-wttrin-make-default-boundary-nil-current-leaves-favorite ()
  "Boundary: no current location is a no-op that leaves the favorite intact."
  (let ((wttrin-favorite-location "Berkeley, CA")
        (savehist-additional-variables nil))
    (with-temp-buffer
      (setq-local wttrin--current-location nil)
      (wttrin-make-default)
      (should (equal wttrin-favorite-location "Berkeley, CA")))))

;;; --------------------------------------------------------------------------
;;; favorite in completion candidates
;;; --------------------------------------------------------------------------

;;; Normal Cases

(ert-deftest test-wttrin-make-default-normal-favorite-prepended-to-candidates ()
  "Normal: a typed-in favorite is offered in the picker, at the front."
  (let ((wttrin-default-locations '("Honolulu, HI" "Berkeley, CA"))
        (wttrin--location-history nil)
        (wttrin-favorite-location "Reykjavik"))
    (should (equal (wttrin--completion-candidates)
                   '("Reykjavik" "Honolulu, HI" "Berkeley, CA")))))

;;; Boundary Cases

(ert-deftest test-wttrin-make-default-boundary-favorite-default-not-duplicated ()
  "Boundary: a favorite that is already a default appears exactly once."
  (require 'cl-lib)
  (let ((wttrin-default-locations '("Honolulu, HI" "Berkeley, CA"))
        (wttrin--location-history nil)
        (wttrin-favorite-location "Berkeley, CA"))
    (should (= 1 (cl-count "Berkeley, CA" (wttrin--completion-candidates) :test #'equal)))))

(ert-deftest test-wttrin-make-default-boundary-nil-favorite-candidates-unchanged ()
  "Boundary: nil favorite leaves the candidate list as defaults plus history."
  (let ((wttrin-default-locations '("Honolulu, HI"))
        (wttrin--location-history '("Oslo, NO"))
        (wttrin-favorite-location nil))
    (should (equal (wttrin--completion-candidates)
                   '("Honolulu, HI" "Oslo, NO")))))

;;; --------------------------------------------------------------------------
;;; keymap binding
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-make-default-normal-d-bound-in-mode-map ()
  "Normal: the weather-buffer keymap binds \"d\" to the command."
  (should (eq (lookup-key wttrin-mode-map (kbd "d")) 'wttrin-make-default)))

(provide 'test-wttrin-make-default)
;;; test-wttrin-make-default.el ends here
