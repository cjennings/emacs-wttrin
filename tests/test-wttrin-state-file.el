;;; test-wttrin-state-file.el --- Tests for state-file persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for the wttrin state file: `wttrin--state-save',
;; `wttrin--state-read', and `wttrin--state-load'.  The state file persists
;; the runtime favorite (`wttrin--favorite-override') and runtime directory
;; (`wttrin--saved-locations-runtime') in a file only wttrin writes, so a
;; foreign Emacs process saving savehist without wttrin loaded can no longer
;; scrub them.  Covers round-trips, authority over in-memory values, the
;; savehist-legacy adoption path, setter integration, and failure isolation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)

(defmacro test-wttrin-state-file--with-sandbox (&rest body)
  "Run BODY with `wttrin-state-file' bound to a fresh temp path.
The runtime vars start nil.  The file is removed afterward."
  (declare (indent 0))
  `(let ((wttrin-state-file (expand-file-name
                             (format "wttrin-test-state-%s.el" (random 1000000))
                             temporary-file-directory))
         (wttrin--favorite-override nil)
         (wttrin--saved-locations-runtime nil))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p wttrin-state-file)
         (delete-file wttrin-state-file)))))

;;; --------------------------------------------------------------------------
;;; Normal Cases
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-state-file-normal-save-load-round-trip ()
  "Normal: save writes both vars; load restores them after they are cleared."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override "Hyatt Place Warwick, RI")
    (setq wttrin--saved-locations-runtime
          '(("Hyatt Place Warwick, RI" . "41.7266678,-71.443097")))
    (wttrin--state-save)
    (setq wttrin--favorite-override nil)
    (setq wttrin--saved-locations-runtime nil)
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Hyatt Place Warwick, RI"))
    (should (equal wttrin--saved-locations-runtime
                   '(("Hyatt Place Warwick, RI" . "41.7266678,-71.443097"))))))

(ert-deftest test-wttrin-state-file-normal-file-wins-over-memory ()
  "Normal: an existing state file is authoritative over in-memory values.
This is the guard against a later savehist restore of stale legacy lines."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override "Newer, ME")
    (wttrin--state-save)
    (setq wttrin--favorite-override "Stale Legacy, LA")
    (setq wttrin--saved-locations-runtime '(("Stale" . "stale")))
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Newer, ME"))
    (should (null wttrin--saved-locations-runtime))))

(ert-deftest test-wttrin-state-file-normal-set-favorite-writes-file ()
  "Normal: `wttrin--set-favorite-location' persists to the state file."
  (test-wttrin-state-file--with-sandbox
    (let ((wttrin-favorite-location nil)
          (wttrin--location-history nil))
      (wttrin--set-favorite-location "Paris, FR")
      (should (file-exists-p wttrin-state-file))
      (should (equal (plist-get (wttrin--state-read) :favorite-override)
                     "Paris, FR")))))

(ert-deftest test-wttrin-state-file-normal-put-saved-location-writes-file ()
  "Normal: `wttrin--put-saved-location' persists to the state file."
  (test-wttrin-state-file--with-sandbox
    (wttrin--put-saved-location "Home" "New Orleans, LA")
    (should (equal (plist-get (wttrin--state-read) :saved-locations)
                   '(("Home" . "New Orleans, LA"))))))

(ert-deftest test-wttrin-state-file-normal-remove-saved-location-writes-file ()
  "Normal: `wttrin--remove-saved-location' persists the removal."
  (test-wttrin-state-file--with-sandbox
    (wttrin--put-saved-location "Home" "New Orleans, LA")
    (wttrin--remove-saved-location "Home")
    (should (null (plist-get (wttrin--state-read) :saved-locations)))))

(ert-deftest test-wttrin-state-file-normal-rename-location-writes-file ()
  "Normal: `wttrin-rename-location' persists the rename and favorite update."
  (test-wttrin-state-file--with-sandbox
    (let ((wttrin-saved-locations nil)
          (wttrin-favorite-location nil)
          (wttrin--location-history nil))
      (wttrin--put-saved-location "Hotel" "41.72,-71.44")
      (wttrin--set-favorite-location "Hotel")
      (wttrin-rename-location "Hotel" "Hyatt")
      (let ((data (wttrin--state-read)))
        (should (equal (plist-get data :saved-locations)
                       '(("Hyatt" . "41.72,-71.44"))))
        (should (equal (plist-get data :favorite-override) "Hyatt"))))))

(ert-deftest test-wttrin-state-file-normal-adopts-savehist-legacy-values ()
  "Normal: with no state file, non-nil vars (savehist legacy) are adopted."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override "Legacy, TX")
    (wttrin--state-load)
    (should (file-exists-p wttrin-state-file))
    (should (equal (plist-get (wttrin--state-read) :favorite-override)
                   "Legacy, TX"))))

(ert-deftest test-wttrin-state-file-normal-savehist-mode-hook-wired ()
  "Normal: `wttrin--state-load' is on `savehist-mode-hook' so a savehist
restore after wttrin loads cannot clobber state-file values."
  (should (memq #'wttrin--state-load savehist-mode-hook)))

;;; --------------------------------------------------------------------------
;;; Boundary Cases
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-state-file-boundary-tri-state-t-round-trips ()
  "Boundary: the favorite's auto-detect value t survives a round-trip."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override t)
    (wttrin--state-save)
    (setq wttrin--favorite-override nil)
    (wttrin--state-load)
    (should (eq wttrin--favorite-override t))))

(ert-deftest test-wttrin-state-file-boundary-nils-round-trip ()
  "Boundary: an explicitly saved all-nil state loads as nils."
  (test-wttrin-state-file--with-sandbox
    (wttrin--state-save)
    (setq wttrin--favorite-override "Ghost, AZ")
    (setq wttrin--saved-locations-runtime '(("Ghost" . "ghost")))
    (wttrin--state-load)
    (should (null wttrin--favorite-override))
    (should (null wttrin--saved-locations-runtime))))

(ert-deftest test-wttrin-state-file-boundary-long-directory-round-trips ()
  "Boundary: a long saved-locations alist survives intact.
Guards the `print-length' / `print-level' bindings in the writer."
  (test-wttrin-state-file--with-sandbox
    (let ((entries (cl-loop for i from 1 to 60
                            collect (cons (format "Place %02d" i)
                                          (format "%d.0,-%d.0" i i)))))
      (setq wttrin--saved-locations-runtime entries)
      (wttrin--state-save)
      (setq wttrin--saved-locations-runtime nil)
      (wttrin--state-load)
      (should (equal wttrin--saved-locations-runtime entries))
      (should (= (length wttrin--saved-locations-runtime) 60)))))

(ert-deftest test-wttrin-state-file-boundary-unicode-round-trips ()
  "Boundary: unicode names and queries survive a round-trip."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override "Zürich 🌦️")
    (setq wttrin--saved-locations-runtime '(("北京" . "Beijing, CN")))
    (wttrin--state-save)
    (setq wttrin--favorite-override nil)
    (setq wttrin--saved-locations-runtime nil)
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Zürich 🌦️"))
    (should (equal wttrin--saved-locations-runtime '(("北京" . "Beijing, CN"))))))

(ert-deftest test-wttrin-state-file-boundary-read-absent-file-returns-nil ()
  "Boundary: reading a nonexistent state file returns nil."
  (test-wttrin-state-file--with-sandbox
    (should-not (wttrin--state-read))))

(ert-deftest test-wttrin-state-file-boundary-absent-file-nil-vars-noop ()
  "Boundary: no file and nil vars is a silent no-op — no file created."
  (test-wttrin-state-file--with-sandbox
    (wttrin--state-load)
    (should-not (file-exists-p wttrin-state-file))
    (should (null wttrin--favorite-override))
    (should (null wttrin--saved-locations-runtime))))

;;; --------------------------------------------------------------------------
;;; Error Cases
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-state-file-error-corrupt-file-does-not-signal ()
  "Error: unreadable lisp in the state file is ignored, vars untouched."
  (test-wttrin-state-file--with-sandbox
    (with-temp-file wttrin-state-file (insert "(((( not lisp"))
    (setq wttrin--favorite-override "Kept, OK")
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Kept, OK"))))

(ert-deftest test-wttrin-state-file-error-empty-file-does-not-signal ()
  "Error: an empty state file is ignored, vars untouched."
  (test-wttrin-state-file--with-sandbox
    (with-temp-file wttrin-state-file)
    (setq wttrin--favorite-override "Kept, OK")
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Kept, OK"))))

(ert-deftest test-wttrin-state-file-error-wrong-shape-data-ignored ()
  "Error: readable lisp that is not a versioned plist is treated as corrupt."
  (test-wttrin-state-file--with-sandbox
    (with-temp-file wttrin-state-file (insert "[1 2 3]"))
    (setq wttrin--favorite-override "Kept, OK")
    (wttrin--state-load)
    (should (equal wttrin--favorite-override "Kept, OK"))))

(ert-deftest test-wttrin-state-file-error-failed-write-preserves-file ()
  "Error: a failed write leaves the previous state file intact.
The writer goes through a temp file + rename, so an error before the
rename cannot truncate or clobber the existing file."
  (test-wttrin-state-file--with-sandbox
    (setq wttrin--favorite-override "Good, OK")
    (wttrin--state-save)
    (setq wttrin--favorite-override "Never Written, NV")
    (cl-letf (((symbol-function 'write-region)
               (lambda (&rest _) (error "Disk full"))))
      (wttrin--state-save))
    (should (equal (plist-get (wttrin--state-read) :favorite-override)
                   "Good, OK"))))

(ert-deftest test-wttrin-state-file-error-corrupt-file-survives-adoption ()
  "Error: adoption never writes over an existing corrupt state file.
The file is left byte-for-byte in place for inspection; non-nil vars do
not trigger the missing-file adoption write because the file exists."
  (test-wttrin-state-file--with-sandbox
    (with-temp-file wttrin-state-file (insert "(((( not lisp"))
    (setq wttrin--favorite-override "Legacy, TX")
    (wttrin--state-load)
    (should (equal (with-temp-buffer
                     (insert-file-contents wttrin-state-file)
                     (buffer-string))
                   "(((( not lisp"))))

(ert-deftest test-wttrin-state-file-error-unwritable-path-does-not-signal ()
  "Error: a save to an unwritable path messages instead of signaling."
  (let ((wttrin-state-file "/nonexistent-root-dir/wttrin/state.el")
        (wttrin--favorite-override "Anywhere, US")
        (wttrin--saved-locations-runtime nil))
    (should-not
     (condition-case nil (progn (wttrin--state-save) nil) (error t)))))

;;; --------------------------------------------------------------------------
;;; savehist registration (post state-file)
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-state-file-normal-savehist-excludes-state-vars ()
  "Normal: `wttrin--savehist-register' no longer registers the state-file
vars; only the scrub-tolerant search history stays with savehist."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring)))
    (wttrin--savehist-register)
    (should (memq 'wttrin--location-history savehist-additional-variables))
    (should-not (memq 'wttrin--favorite-override savehist-additional-variables))
    (should-not (memq 'wttrin--saved-locations-runtime
                      savehist-additional-variables))))

(provide 'test-wttrin-state-file)
;;; test-wttrin-state-file.el ends here
