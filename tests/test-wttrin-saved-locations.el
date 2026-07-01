;;; test-wttrin-saved-locations.el --- Tests for the named-locations directory -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the named-locations directory: the normalizer/union
;; `wttrin--saved-locations', the resolver `wttrin--resolve-location-query',
;; candidate de-duplication/precedence, favorite-as-name resolution, alias cache
;; identity, history suppression of saved names, and savehist registration.
;;
;; The directory has two layers: the configured `wttrin-saved-locations'
;; defcustom (a seed the user sets in init) and the runtime
;; `wttrin--saved-locations-runtime' (savehist-persisted, mutated by `d',
;; `wttrin-save-location', rename, and remove).  `wttrin--saved-locations'
;; returns the union, runtime winning on a name collision.  Read-only tests set
;; up the directory via the config layer; mutation tests use the runtime layer,
;; since the runtime commands cannot delete a config-seeded entry.  The favorite
;; is likewise split: the `wttrin-favorite-location' defcustom plus the runtime
;; `wttrin--favorite-override', read back through `wttrin--favorite-location'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

;;; wttrin--saved-locations (normalizer/union)

(ert-deftest test-wttrin-saved-locations-normal-pairs-returned ()
  "Normal: well-formed pairs are returned as (NAME . QUERY)."
  (let ((wttrin-saved-locations '(("Home" . "1500 Sugar Bowl Dr, New Orleans")))
        (wttrin--saved-locations-runtime nil))
    (should (equal '(("Home" . "1500 Sugar Bowl Dr, New Orleans"))
                   (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-boundary-bare-string-shorthand ()
  "Boundary: a bare string S becomes (S . S)."
  (let ((wttrin-saved-locations '("Berkeley, CA"))
        (wttrin--saved-locations-runtime nil))
    (should (equal '(("Berkeley, CA" . "Berkeley, CA"))
                   (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-boundary-whitespace-trimmed ()
  "Boundary: surrounding whitespace on name and query is trimmed."
  (let ((wttrin-saved-locations '(("  Home  " . "  Paris, FR  ")))
        (wttrin--saved-locations-runtime nil))
    (should (equal '(("Home" . "Paris, FR")) (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-error-malformed-skipped ()
  "Error: non-cons, non-string, and empty entries are skipped, not fatal."
  (let ((wttrin-saved-locations
         (list '("Good" . "Tokyo") 42 '("" . "x") '("y" . "") "  " '(a . b)))
        (wttrin--saved-locations-runtime nil))
    (should (equal '(("Good" . "Tokyo")) (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-normal-runtime-overlays-config ()
  "Normal: a runtime entry wins over a config entry with the same name."
  (let ((wttrin-saved-locations '(("Home" . "Paris, FR")))
        (wttrin--saved-locations-runtime '(("Home" . "Tokyo, JP"))))
    (should (equal '(("Home" . "Tokyo, JP")) (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-normal-union-runtime-then-config ()
  "Normal: distinct config and runtime entries both appear, runtime first."
  (let ((wttrin-saved-locations '(("Work" . "Tokyo, JP")))
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR"))))
    (should (equal '(("Home" . "Paris, FR") ("Work" . "Tokyo, JP"))
                   (wttrin--saved-locations)))))

;;; wttrin--resolve-location-query

(ert-deftest test-wttrin-saved-locations-normal-resolve-name-to-query ()
  "Normal: a saved name resolves to its query."
  (let ((wttrin-saved-locations '(("Craig's House" . "1500 Sugar Bowl Dr, New Orleans")))
        (wttrin--saved-locations-runtime nil))
    (should (equal "1500 Sugar Bowl Dr, New Orleans"
                   (wttrin--resolve-location-query "Craig's House")))))

(ert-deftest test-wttrin-saved-locations-boundary-resolve-passthrough ()
  "Boundary: a non-saved selection passes through unchanged."
  (let ((wttrin-saved-locations '(("Home" . "Paris, FR")))
        (wttrin--saved-locations-runtime nil))
    (should (equal "Tokyo, JP" (wttrin--resolve-location-query "Tokyo, JP")))))

;;; Candidate de-duplication and precedence

(ert-deftest test-wttrin-saved-locations-normal-candidates-precedence ()
  "Normal: candidates are saved, favorite, defaults, then history, deduped."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-geolocation-enabled nil)
            (wttrin-saved-locations '(("Home" . "Paris, FR")))
            (wttrin--saved-locations-runtime nil)
            (wttrin-favorite-location "Reykjavik")
            (wttrin--favorite-override nil)
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history '("Tokyo")))
        (should (equal '("Home" "Reykjavik" "Honolulu, HI" "Tokyo")
                       (wttrin--completion-candidates))))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-saved-locations-boundary-candidates-dedup-saved-wins ()
  "Boundary: a name present in saved and defaults appears once (saved first)."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-geolocation-enabled nil)
            (wttrin-saved-locations '(("Honolulu, HI" . "Honolulu, HI")))
            (wttrin--saved-locations-runtime nil)
            (wttrin-favorite-location nil)
            (wttrin--favorite-override nil)
            (wttrin-default-locations '("Honolulu, HI" "Berkeley, CA"))
            (wttrin--location-history nil))
        (should (equal '("Honolulu, HI" "Berkeley, CA")
                       (wttrin--completion-candidates))))
    (testutil-wttrin-teardown)))

;;; Favorite-as-name resolution

(ert-deftest test-wttrin-saved-locations-normal-favorite-name-resolves-to-query ()
  "Normal: a favorite that is a saved name resolves to its query for fetching."
  (let ((wttrin-saved-locations '(("Home" . "1500 Sugar Bowl Dr, New Orleans")))
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location "Home")
        (wttrin--favorite-override nil))
    (should (equal "1500 Sugar Bowl Dr, New Orleans"
                   (wttrin--resolve-favorite-location)))))

(ert-deftest test-wttrin-saved-locations-normal-favorite-display-shows-name ()
  "Normal: the favorite display name is the saved name, not its query."
  (let ((wttrin-saved-locations '(("Home" . "1500 Sugar Bowl Dr, New Orleans")))
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location "Home")
        (wttrin--favorite-override nil))
    (should (equal "Home" (wttrin--favorite-location-display-name)))))

;;; Alias cache identity

(ert-deftest test-wttrin-saved-locations-normal-cache-keyed-on-query ()
  "Normal: cache identity follows the query, not the display name.
Two names with the same query share a key; the name never leaks into the key."
  (let ((wttrin-saved-locations '(("A" . "Paris, FR") ("B" . "Paris, FR")))
        (wttrin--saved-locations-runtime nil))
    (should (equal (wttrin--make-cache-key (wttrin--resolve-location-query "A"))
                   (wttrin--make-cache-key (wttrin--resolve-location-query "B"))))))

;;; History suppression of saved names

(ert-deftest test-wttrin-saved-locations-boundary-saved-name-not-logged ()
  "Boundary: a saved-directory name is not added to history."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-saved-locations '(("Home" . "Paris, FR")))
            (wttrin--saved-locations-runtime nil)
            (wttrin-default-locations '())
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "Home")
        (should (null wttrin--location-history)))
    (testutil-wttrin-teardown)))

;;; savehist

(ert-deftest test-wttrin-saved-locations-integration-savehist-registers ()
  "Integration: the runtime directory is registered for savehist persistence,
not the `wttrin-saved-locations' defcustom (which the user sets in init)."
  (require 'savehist)
  (let ((savehist-additional-variables '(kill-ring)))
    (wttrin--savehist-register)
    (should (memq 'wttrin--saved-locations-runtime savehist-additional-variables))
    (should-not (memq 'wttrin-saved-locations savehist-additional-variables))))

;;; wttrin--coordinates-p

(ert-deftest test-wttrin-saved-locations-normal-coordinates-p ()
  "Normal/Boundary: coordinate strings match; place names do not."
  (should (wttrin--coordinates-p "41.37,-71.83"))
  (should (wttrin--coordinates-p "1.5,2.5"))
  (should-not (wttrin--coordinates-p "New York, NY"))
  (should-not (wttrin--coordinates-p "Berkeley, CA"))
  (should-not (wttrin--coordinates-p "")))

;;; wttrin--put-saved-location

(ert-deftest test-wttrin-saved-locations-normal-put-adds-and-updates ()
  "Normal: put adds a new entry and updates an existing name without duplicating."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil))
    (wttrin--put-saved-location "Home" "Paris, FR")
    (should (equal "Paris, FR" (wttrin--resolve-location-query "Home")))
    (wttrin--put-saved-location "Home" "Tokyo, JP")
    (should (equal "Tokyo, JP" (wttrin--resolve-location-query "Home")))
    (should (= 1 (length (wttrin--saved-locations))))))

(ert-deftest test-wttrin-saved-locations-error-put-rejects-empty-and-sentinel ()
  "Error: put refuses an empty name, empty query, or the sentinel name."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil))
    (should-error (wttrin--put-saved-location "" "Paris") :type 'user-error)
    (should-error (wttrin--put-saved-location "Home" "") :type 'user-error)
    (should-error (wttrin--put-saved-location wttrin--geolocation-sentinel "x")
                  :type 'user-error)))

;;; wttrin-rename-location

(ert-deftest test-wttrin-saved-locations-normal-rename ()
  "Normal: rename moves the entry and updates the favorite reference."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR")))
        (wttrin-favorite-location nil)
        (wttrin--favorite-override "Home"))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-rename-location "Home" "Casa"))
    (should (equal "Paris, FR" (wttrin--resolve-location-query "Casa")))
    (should-not (assoc "Home" (wttrin--saved-locations)))
    (should (equal "Casa" (wttrin--favorite-location)))))

(ert-deftest test-wttrin-saved-locations-error-rename-collision-refused ()
  "Error: renaming onto an existing name is refused and changes nothing."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR") ("Work" . "Tokyo, JP"))))
    (should-error (wttrin-rename-location "Home" "Work") :type 'user-error)
    (should (equal "Paris, FR" (wttrin--resolve-location-query "Home")))))

(ert-deftest test-wttrin-saved-locations-normal-rename-favorite-refreshes-mode-line ()
  "Normal: renaming the favorite refreshes the mode-line so the icon and tooltip
follow the new name immediately instead of at the next scheduled fetch."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR")))
        (wttrin-favorite-location nil)
        (wttrin--favorite-override "Home")
        (wttrin--location-history nil)
        (wttrin-mode-line-mode t)
        (fetched nil))
    (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda () (setq fetched t)))
              ((symbol-function 'wttrin--mode-line-set-placeholder)
               (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-rename-location "Home" "Casa"))
    (should (equal "Casa" (wttrin--favorite-location)))
    (should fetched)))

(ert-deftest test-wttrin-saved-locations-boundary-rename-non-favorite-no-refresh ()
  "Boundary: renaming a location that is not the favorite does not refresh."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR") ("Work" . "Tokyo, JP")))
        (wttrin-favorite-location nil)
        (wttrin--favorite-override "Work")
        (wttrin--location-history nil)
        (wttrin-mode-line-mode t)
        (fetched nil))
    (cl-letf (((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda () (setq fetched t)))
              ((symbol-function 'wttrin--mode-line-set-placeholder)
               (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-rename-location "Home" "Casa"))
    (should-not fetched)))

;;; wttrin-remove-location

(ert-deftest test-wttrin-saved-locations-normal-remove-confirmed ()
  "Normal: confirming removes the entry."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR"))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-remove-location "Home"))
    (should (null (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-boundary-remove-declined-keeps ()
  "Boundary: declining the confirmation keeps the entry."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR"))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-remove-location "Home"))
    (should (assoc "Home" (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-boundary-remove-config-entry-remains ()
  "Boundary: removing a config-seeded entry (not in the runtime layer) leaves it
in the directory, since the runtime removal cannot delete an init-defined entry."
  (let ((wttrin-saved-locations '(("Home" . "Paris, FR")))
        (wttrin--saved-locations-runtime nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-remove-location "Home"))
    (should (assoc "Home" (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-normal-remove-favorite-refreshes-mode-line ()
  "Normal: removing the favorite refreshes the mode-line so it stops showing the
now-deleted alias's resolved weather and re-fetches against the bare query."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR")))
        (wttrin-favorite-location nil)
        (wttrin--favorite-override "Home")
        (wttrin-mode-line-mode t)
        (fetched nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda () (setq fetched t)))
              ((symbol-function 'wttrin--mode-line-set-placeholder)
               (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-remove-location "Home"))
    (should fetched)))

(ert-deftest test-wttrin-saved-locations-boundary-remove-non-favorite-no-refresh ()
  "Boundary: removing a location that is not the favorite does not refresh."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR") ("Work" . "Tokyo, JP")))
        (wttrin-favorite-location nil)
        (wttrin--favorite-override "Work")
        (wttrin-mode-line-mode t)
        (fetched nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'wttrin--mode-line-fetch-weather)
               (lambda () (setq fetched t)))
              ((symbol-function 'wttrin--mode-line-set-placeholder)
               (lambda () nil))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-remove-location "Home"))
    (should-not fetched)))

;;; wttrin-make-default — geolocation naming

(ert-deftest test-wttrin-saved-locations-normal-d-names-and-promotes ()
  "Normal: d on a coordinate buffer names it, saves it, and promotes the name."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location nil)
        (wttrin--favorite-override nil)
        (wttrin-mode-line-mode nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "41.37,-71.83")
      (setq-local wttrin--current-display "41.37,-71.83")
      (setq-local wttrin--current-address "Westerly, RI")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Home"))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (wttrin-make-default)))
    (should (equal "41.37,-71.83" (wttrin--resolve-location-query "Home")))
    (should (equal "Home" (wttrin--favorite-location)))))

(ert-deftest test-wttrin-saved-locations-boundary-d-empty-keeps-coordinates ()
  "Boundary: an empty name at the d prompt keeps the coordinates, saves no entry."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location nil)
        (wttrin--favorite-override nil)
        (wttrin-mode-line-mode nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "41.37,-71.83")
      (setq-local wttrin--current-address "Westerly, RI")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (wttrin-make-default)))
    (should (null (wttrin--saved-locations)))
    (should (equal "41.37,-71.83" (wttrin--favorite-location)))))

(ert-deftest test-wttrin-saved-locations-boundary-d-named-buffer-no-prompt ()
  "Boundary: d on a named buffer promotes the display name without prompting."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location nil)
        (wttrin--favorite-override nil)
        (wttrin-mode-line-mode nil)
        (prompted nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "1500 Sugar Bowl Dr")
      (setq-local wttrin--current-display "Craig's House")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompted t) "x"))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (wttrin-make-default)))
    (should-not prompted)
    (should (equal "Craig's House" (wttrin--favorite-location)))
    (should (equal "1500 Sugar Bowl Dr"
                   (wttrin--resolve-location-query "Craig's House")))))

(ert-deftest test-wttrin-saved-locations-normal-d-typed-saves-to-directory ()
  "Normal: making a typed location the default also saves it to the directory,
so it persists as a named entry rather than only as the favorite string."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil)
        (wttrin-favorite-location nil)
        (wttrin--favorite-override nil)
        (wttrin-mode-line-mode nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "Reykjavik")
      (setq-local wttrin--current-display "Reykjavik")
      (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
        (wttrin-make-default)))
    (should (equal "Reykjavik" (wttrin--favorite-location)))
    (should (assoc "Reykjavik" (wttrin--saved-locations)))
    (should (equal "Reykjavik" (wttrin--resolve-location-query "Reykjavik")))))

;;; interactive entry smoke tests (cover the prompt forms)

(ert-deftest test-wttrin-saved-locations-normal-save-location-interactive ()
  "Normal: the interactive save command reads the buffer query and a name."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "Paris, FR")
      (setq-local wttrin--current-display "Paris, FR")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Home"))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (call-interactively 'wttrin-save-location)))
    (should (equal "Paris, FR" (wttrin--resolve-location-query "Home")))))

(ert-deftest test-wttrin-saved-locations-normal-rename-interactive ()
  "Normal: the interactive rename command prompts for the entry and new name."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR"))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Home"))
              ((symbol-function 'read-string) (lambda (&rest _) "Casa"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (call-interactively 'wttrin-rename-location))
    (should (equal "Paris, FR" (wttrin--resolve-location-query "Casa")))))

(ert-deftest test-wttrin-saved-locations-normal-remove-interactive ()
  "Normal: the interactive remove command prompts and confirms."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime '(("Home" . "Paris, FR"))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Home"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (call-interactively 'wttrin-remove-location))
    (should (null (wttrin--saved-locations)))))

(ert-deftest test-wttrin-saved-locations-boundary-save-empty-name-cancels ()
  "Boundary: an empty name at the save prompt cancels without saving."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil))
    (with-temp-buffer
      (setq-local wttrin--current-location "Paris, FR")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  "))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (call-interactively 'wttrin-save-location)))
    (should (null (wttrin--saved-locations)))))

;;; coordinate suppression in history

(ert-deftest test-wttrin-saved-locations-boundary-coordinates-not-logged ()
  "Boundary: a raw coordinate string is never added to history."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin-saved-locations nil)
            (wttrin--saved-locations-runtime nil)
            (wttrin--location-history nil))
        (wttrin--add-to-location-history "41.37,-71.83")
        (should (null wttrin--location-history)))
    (testutil-wttrin-teardown)))

;;; history stays in sync with the directory

(ert-deftest test-wttrin-saved-locations-normal-save-drops-query-from-history ()
  "Normal: saving a location drops its query from history, so the place lives in
the directory only and does not also appear as a separate history candidate."
  (let ((wttrin-saved-locations nil)
        (wttrin--saved-locations-runtime nil)
        (wttrin--location-history '("New Orleans" "Paris")))
    (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin-save-location "Home" "New Orleans"))
    (should-not (member "New Orleans" wttrin--location-history))
    (should (member "Paris" wttrin--location-history))))

(provide 'test-wttrin-saved-locations)
;;; test-wttrin-saved-locations.el ends here
