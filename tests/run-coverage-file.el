;;; run-coverage-file.el --- Undercover setup for per-file coverage runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by `make coverage' before each unit-test file, BEFORE the
;; wttrin source files are loaded.  Instrumenting must happen first so
;; the subsequent loads pick up the instrumented source.
;;
;; Coverage data is merged across per-file invocations into a single
;; simplecov JSON at .coverage/simplecov.json.

;;; Code:

(unless (require 'undercover nil t)
  (message "")
  (message "ERROR: undercover not installed.")
  (message "Add `undercover' to Eask as a development dep, then run `make deps'.")
  (message "")
  (kill-emacs 1))

;; Force coverage collection in non-CI environments.  Must be set after
;; loading undercover because the library's top-level form
;; `(setq undercover-force-coverage (getenv "UNDERCOVER_FORCE"))' would
;; otherwise overwrite the value.
(setq undercover-force-coverage t)

(undercover "wttrin.el"
            "wttrin-debug.el"
            "wttrin-geolocation.el"
            (:report-format 'simplecov)
            (:report-file ".coverage/simplecov.json")
            (:merge-report t)
            (:send-report nil))

;;; run-coverage-file.el ends here
