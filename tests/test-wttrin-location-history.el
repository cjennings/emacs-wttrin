;;; test-wttrin-location-history.el --- Tests for location search history -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the location search history feature: wttrin--add-to-location-history,
;; wttrin--completion-candidates, wttrin-remove-location-history,
;; wttrin-clear-location-history, and savehist integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-location-history-setup ()
  "Setup: isolate history and defaults from the user's real config."
  (testutil-wttrin-setup)
  (setq wttrin--location-history nil))

(defun test-wttrin-location-history-teardown ()
  "Teardown: clear history."
  (setq wttrin--location-history nil)
  (testutil-wttrin-teardown))

;;; wttrin--add-to-location-history

(ert-deftest test-wttrin-location-history-normal-adds-new-location ()
  "A new location is pushed onto the front of history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "Tokyo")
        (should (equal '("Tokyo") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-promotes-existing-to-front ()
  "Re-adding an existing location moves it to the front without duplicating."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history '("Paris" "Tokyo" "Berlin")))
        (wttrin--add-to-location-history "Tokyo")
        (should (equal '("Tokyo" "Paris" "Berlin") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-skips-default-location ()
  "A location already in defaults is not added to history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "Honolulu, HI")
        (should (null wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-boundary-trims-to-max ()
  "History is trimmed to `wttrin-location-history-max', keeping the most recent."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin-location-history-max 3)
            (wttrin--location-history '("c" "b" "a")))
        (wttrin--add-to-location-history "d")
        (should (equal '("d" "c" "b") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-boundary-empty-history ()
  "Adding to empty history yields a single-entry list."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "Reykjavik")
        (should (equal '("Reykjavik") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-boundary-max-zero-keeps-none ()
  "A max of 0 results in empty history after a trim."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin-location-history-max 0)
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "Nowhere")
        (should (null wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-error-nil-location-no-op ()
  "A nil location is a no-op."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history '("Paris")))
        (wttrin--add-to-location-history nil)
        (should (equal '("Paris") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-error-empty-string-no-op ()
  "An empty string is a no-op."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history '("Paris")))
        (wttrin--add-to-location-history "")
        (should (equal '("Paris") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

;;; wttrin--completion-candidates

(ert-deftest test-wttrin-location-history-normal-candidates-defaults-then-history ()
  "Candidates list the sentinel, then defaults, then history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (wttrin-default-locations '("Honolulu, HI" "Berkeley, CA"))
            (wttrin--location-history '("Tokyo" "Paris")))
        (should (equal (list wttrin--geolocation-sentinel
                             "Honolulu, HI" "Berkeley, CA" "Tokyo" "Paris")
                       (wttrin--completion-candidates))))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-candidates-only-defaults ()
  "With empty history, candidates are the sentinel then the defaults."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history nil))
        (should (equal (list wttrin--geolocation-sentinel "Honolulu, HI")
                       (wttrin--completion-candidates))))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-candidates-only-history ()
  "With empty defaults, candidates are the sentinel then the history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (wttrin-default-locations '())
            (wttrin--location-history '("Tokyo")))
        (should (equal (list wttrin--geolocation-sentinel "Tokyo")
                       (wttrin--completion-candidates))))
    (test-wttrin-location-history-teardown)))

;;; wttrin-remove-location-history

(ert-deftest test-wttrin-location-history-normal-remove-entry ()
  "Removing an entry drops it from history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin--location-history '("Tokyo" "Paris" "Berlin")))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-remove-location-history "Paris"))
        (should (equal '("Tokyo" "Berlin") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-remove-absent-no-op ()
  "Removing an entry not present leaves history unchanged."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin--location-history '("Tokyo")))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-remove-location-history "Mars"))
        (should (equal '("Tokyo") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-boundary-remove-last-leaves-empty ()
  "Removing the only entry leaves an empty list."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin--location-history '("Tokyo")))
        (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-remove-location-history "Tokyo"))
        (should (null wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

;;; wttrin-clear-location-history

(ert-deftest test-wttrin-location-history-normal-clear-confirmed ()
  "Confirming the prompt clears all history."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin--location-history '("Tokyo" "Paris")))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-clear-location-history))
        (should (null wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

(ert-deftest test-wttrin-location-history-normal-clear-declined-keeps-history ()
  "Declining the prompt leaves history intact."
  (test-wttrin-location-history-setup)
  (unwind-protect
      (let ((wttrin--location-history '("Tokyo" "Paris")))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin-clear-location-history))
        (should (equal '("Tokyo" "Paris") wttrin--location-history)))
    (test-wttrin-location-history-teardown)))

;;; savehist integration

(ert-deftest test-wttrin-location-history-integration-savehist-registers-variable ()
  "Loading savehist registers wttrin--location-history for persistence."
  (require 'savehist)
  (should (memq 'wttrin--location-history savehist-additional-variables)))

(ert-deftest test-wttrin-location-history-normal-savehist-register-adds-var ()
  "wttrin--savehist-register adds the history variable to the save list."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring)))
    (wttrin--savehist-register)
    (should (memq 'wttrin--location-history savehist-additional-variables))))

(ert-deftest test-wttrin-location-history-boundary-savehist-register-idempotent ()
  "Registering an already-present variable does not duplicate it."
  (require 'savehist)
  (require 'cl-lib)
  (let ((savehist-additional-variables '(wttrin--location-history wttrin-favorite-location)))
    (wttrin--savehist-register)
    (should (= 1 (cl-count 'wttrin--location-history savehist-additional-variables)))
    (should (= 1 (cl-count 'wttrin-favorite-location savehist-additional-variables)))))

(ert-deftest test-wttrin-location-history-integration-savehist-register-on-save-hook ()
  "The registration runs on `savehist-save-hook' so it survives a clobber."
  (require 'savehist)
  (should (memq 'wttrin--savehist-register savehist-save-hook)))

(ert-deftest test-wttrin-location-history-integration-savehist-survives-clobber ()
  "A user setq that drops the variable is repaired before the next save."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring search-ring)))
    ;; simulate the save path: savehist-save runs this hook first
    (run-hooks 'savehist-save-hook)
    (should (memq 'wttrin--location-history savehist-additional-variables))))

(provide 'test-wttrin-location-history)
;;; test-wttrin-location-history.el ends here
