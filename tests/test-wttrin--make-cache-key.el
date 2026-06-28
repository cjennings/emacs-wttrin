;;; test-wttrin--make-cache-key.el --- Tests for wttrin--make-cache-key -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--make-cache-key.  The key must change whenever a
;; setting that shapes the requested response changes (location, unit system,
;; display options, Accept-Language), so a settings change can't serve a
;; stale-format cached response.  Tests assert behavior (distinctness and
;; stability), not the exact key string, so the format can evolve.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(defun test-wttrin--make-cache-key-setup ()
  "Setup for make-cache-key tests."
  (testutil-wttrin-setup))

(defun test-wttrin--make-cache-key-teardown ()
  "Teardown for make-cache-key tests."
  (testutil-wttrin-teardown))

;;; Normal Cases

(ert-deftest test-wttrin--make-cache-key-normal-same-inputs-same-key ()
  "Normal: identical location and settings produce the same key."
  (let ((wttrin-unit-system "m"))
    (should (string= (wttrin--make-cache-key "Paris")
                     (wttrin--make-cache-key "Paris")))))

(ert-deftest test-wttrin--make-cache-key-normal-different-locations-differ ()
  "Normal: different locations produce different keys."
  (let ((wttrin-unit-system "m"))
    (should-not (string= (wttrin--make-cache-key "Paris")
                         (wttrin--make-cache-key "London")))))

(ert-deftest test-wttrin--make-cache-key-normal-key-contains-location ()
  "Normal: the key carries the location so distinct places never collide."
  (should (string-match-p "Paris" (wttrin--make-cache-key "Paris"))))

;;; Boundary Cases — every response-shaping setting changes the key

(ert-deftest test-wttrin--make-cache-key-boundary-unit-system-changes-key ()
  "Boundary: the same location under different unit systems differs."
  (let ((m (let ((wttrin-unit-system "m")) (wttrin--make-cache-key "Paris")))
        (u (let ((wttrin-unit-system "u")) (wttrin--make-cache-key "Paris")))
        (d (let ((wttrin-unit-system nil)) (wttrin--make-cache-key "Paris"))))
    (should-not (string= m u))
    (should-not (string= m d))
    (should-not (string= u d))))

(ert-deftest test-wttrin--make-cache-key-boundary-display-options-changes-key ()
  "Boundary: changing display options yields a distinct key.
Without this, changing the forecast format keeps serving the old cached output."
  (let ((a (let ((wttrin-display-options nil)) (wttrin--make-cache-key "Paris")))
        (b (let ((wttrin-display-options "F")) (wttrin--make-cache-key "Paris")))
        (c (let ((wttrin-display-options "0")) (wttrin--make-cache-key "Paris"))))
    (should-not (string= a b))
    (should-not (string= a c))
    (should-not (string= b c))))

(ert-deftest test-wttrin--make-cache-key-boundary-language-changes-key ()
  "Boundary: changing Accept-Language yields a distinct key."
  (let ((en (let ((wttrin-default-languages '("Accept-Language" . "en-US")))
              (wttrin--make-cache-key "Paris")))
        (fr (let ((wttrin-default-languages '("Accept-Language" . "fr-FR")))
              (wttrin--make-cache-key "Paris"))))
    (should-not (string= en fr))))

(ert-deftest test-wttrin--make-cache-key-boundary-special-chars-in-location ()
  "Boundary: special characters in the location do not break key generation."
  (dolist (loc '("São Paulo" "北京" "London, GB" "Test|Location" ""))
    (let ((key (wttrin--make-cache-key loc)))
      (should (stringp key))
      (should (> (length key) 0)))))

;;; Error Cases

(ert-deftest test-wttrin--make-cache-key-error-idempotent ()
  "Error: repeated calls with the same inputs are stable."
  (let ((wttrin-unit-system "m"))
    (let ((k1 (wttrin--make-cache-key "Paris"))
          (k2 (wttrin--make-cache-key "Paris")))
      (should (string= k1 k2)))))

(ert-deftest test-wttrin--make-cache-key-error-nil-settings-no-error ()
  "Error: nil unit system and nil display options produce a key, no signal."
  (let ((wttrin-unit-system nil)
        (wttrin-display-options nil))
    (should (stringp (wttrin--make-cache-key "Paris")))))

(provide 'test-wttrin--make-cache-key)
;;; test-wttrin--make-cache-key.el ends here
