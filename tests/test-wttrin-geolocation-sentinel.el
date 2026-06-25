;;; test-wttrin-geolocation-sentinel.el --- Tests for the picker geolocation sentinel -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the "Current location (detect)" picker sentinel: its presence
;; and position in `wttrin--completion-candidates', the selection routing in
;; `wttrin--query-selection' (literal vs detect-then-query), and the guard that
;; keeps the sentinel out of location history.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'wttrin-geolocation)
(require 'testutil-wttrin)

;;; wttrin--completion-candidates — sentinel presence and position

(ert-deftest test-wttrin-geolocation-sentinel-normal-first-candidate ()
  "Normal: the sentinel is the first completion candidate."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history '("Tokyo")))
        (should (equal wttrin--geolocation-sentinel
                       (car (wttrin--completion-candidates)))))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-normal-first-even-with-favorite ()
  "Normal: the sentinel precedes a string favorite in the candidate list."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "New Orleans, LA")
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history nil))
        (should (equal (list wttrin--geolocation-sentinel
                             "New Orleans, LA" "Honolulu, HI")
                       (wttrin--completion-candidates))))
    (testutil-wttrin-teardown)))

;;; wttrin--sort-completions — pin the sentinel first

(ert-deftest test-wttrin-geolocation-sentinel-normal-sort-pins-first ()
  "Normal: the sentinel is moved to the front, the rest keep their order.
Sorting completion UIs (vertico, icomplete) call the metadata
display-sort-function, so this is what keeps the sentinel pinned."
  (should (equal (list wttrin--geolocation-sentinel "Honolulu, HI" "Tokyo")
                 (wttrin--sort-completions
                  (list "Honolulu, HI" wttrin--geolocation-sentinel "Tokyo")))))

(ert-deftest test-wttrin-geolocation-sentinel-boundary-sort-no-sentinel-unchanged ()
  "Boundary: a list without the sentinel is returned in its original order."
  (should (equal '("Honolulu, HI" "Tokyo")
                 (wttrin--sort-completions '("Honolulu, HI" "Tokyo")))))

(ert-deftest test-wttrin-geolocation-sentinel-boundary-sort-empty ()
  "Boundary: an empty candidate list sorts to empty."
  (should (null (wttrin--sort-completions nil))))

;;; wttrin--completion-table — metadata + completion

(ert-deftest test-wttrin-geolocation-sentinel-normal-table-metadata-sort-fn ()
  "Normal: the table advertises the pin-first display-sort-function."
  (let* ((table (wttrin--completion-table
                 (list wttrin--geolocation-sentinel "Tokyo")))
         (meta (funcall table "" nil 'metadata)))
    (should (eq #'wttrin--sort-completions
                (cdr (assq 'display-sort-function (cdr meta)))))))

(ert-deftest test-wttrin-geolocation-sentinel-normal-table-completes-candidates ()
  "Normal: the table completes over the candidates it was given."
  (let ((table (wttrin--completion-table
                (list wttrin--geolocation-sentinel "Tokyo" "Paris"))))
    (should (equal (sort (list wttrin--geolocation-sentinel "Tokyo" "Paris")
                         #'string-lessp)
                   (sort (all-completions "" table) #'string-lessp)))))

;;; wttrin interactive entry — delegates to routing

(ert-deftest test-wttrin-geolocation-sentinel-normal-entry-delegates-to-query-selection ()
  "Normal: the interactive `wttrin' command routes its picker selection
through `wttrin--query-selection' (smoke test of the entry wrapper)."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((routed nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "London, GB"))
                  ((symbol-function 'wttrin--query-selection)
                   (lambda (loc) (setq routed loc))))
          (call-interactively 'wttrin))
        (should (equal "London, GB" routed)))
    (testutil-wttrin-teardown)))

;;; wttrin--query-selection — routing

(ert-deftest test-wttrin-geolocation-sentinel-normal-typed-location-queries-literally ()
  "Normal: a typed location is passed straight to `wttrin-query'."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((captured nil))
        (cl-letf (((symbol-function 'wttrin-query)
                   (lambda (loc) (setq captured loc))))
          (wttrin--query-selection "Paris"))
        (should (equal "Paris" captured)))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-normal-routes-to-detect-then-query ()
  "Normal: selecting the sentinel detects, then queries the resolved city."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((captured nil))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (callback) (funcall callback "Austin, TX")))
                  ((symbol-function 'wttrin-query)
                   (lambda (loc &optional _address) (setq captured loc)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin--query-selection wttrin--geolocation-sentinel))
        (should (equal "Austin, TX" captured)))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-error-detect-failure-no-query ()
  "Error: a failed detection does not query and does not mutate the favorite."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((queried nil)
            (wttrin-favorite-location "New Orleans, LA"))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (callback) (funcall callback nil)))
                  ((symbol-function 'wttrin-query)
                   (lambda (_loc) (setq queried t)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin--query-selection wttrin--geolocation-sentinel))
        (should-not queried)
        (should (equal "New Orleans, LA" wttrin-favorite-location)))
    (testutil-wttrin-teardown)))

;;; sentinel never enters history

(ert-deftest test-wttrin-geolocation-sentinel-boundary-never-added-to-history ()
  "Boundary: the sentinel string is never recorded in location history."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-default-locations '())
            (wttrin--location-history nil))
        (wttrin--add-to-location-history wttrin--geolocation-sentinel)
        (should (null wttrin--location-history)))
    (testutil-wttrin-teardown)))

;;; wttrin-geolocation-enabled — opt-out switch

(ert-deftest test-wttrin-geolocation-sentinel-normal-disabled-hides-sentinel ()
  "Normal: with geolocation disabled, the sentinel is not offered."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-geolocation-enabled nil)
            (wttrin-favorite-location nil)
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history '("Tokyo")))
        (should-not (member wttrin--geolocation-sentinel
                            (wttrin--completion-candidates)))
        (should (equal '("Honolulu, HI" "Tokyo")
                       (wttrin--completion-candidates))))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-boundary-enabled-shows-sentinel ()
  "Boundary: with geolocation enabled (default), the sentinel is offered first."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-geolocation-enabled t)
            (wttrin-favorite-location nil)
            (wttrin-default-locations '("Honolulu, HI"))
            (wttrin--location-history nil))
        (should (equal wttrin--geolocation-sentinel
                       (car (wttrin--completion-candidates)))))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-error-disabled-detect-then-query-no-detect ()
  "Error: with geolocation disabled, detect-then-query does not detect or query."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((detected nil)
            (queried nil)
            (wttrin-geolocation-enabled nil))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (_cb) (setq detected t)))
                  ((symbol-function 'wttrin-query)
                   (lambda (_loc) (setq queried t)))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (wttrin--detect-then-query))
        (should-not detected)
        (should-not queried))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin-geolocation-sentinel-boundary-disabled-favorite-no-autodetect ()
  "Boundary: with geolocation disabled, the t-favorite auto-detect does not fire."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((detected nil)
            (wttrin-geolocation-enabled nil)
            (wttrin--favorite-location-pending nil))
        (cl-letf (((symbol-function 'wttrin-geolocation-detect)
                   (lambda (_cb) (setq detected t))))
          (wttrin--start-favorite-location-detect))
        (should-not detected)
        (should-not wttrin--favorite-location-pending))
    (testutil-wttrin-teardown)))

(provide 'test-wttrin-geolocation-sentinel)
;;; test-wttrin-geolocation-sentinel.el ends here
