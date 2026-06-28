;;; test-wttrin-query-request-id.el --- Stale async response guard -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; wttrin-query reuses the single *wttr.in* buffer, so an async response for an
;; earlier query that completes after a later one must not overwrite the newer
;; buffer.  A per-buffer request id, captured in the callback closure, drops the
;; stale response.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

(ert-deftest test-wttrin-query-normal-current-callback-displays ()
  "Normal: a query's own callback (matching request id) displays its result."
  (let ((cb nil) (displayed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (_loc c) (setq cb c)))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf &rest _) (set-buffer buf)))
                  ((symbol-function 'wttrin--display-weather)
                   (lambda (query &rest _) (setq displayed query))))
          (wttrin-query "Paris")
          (funcall cb "paris-data")
          (should (equal displayed "Paris")))
      (when (get-buffer "*wttr.in*") (kill-buffer "*wttr.in*")))))

(ert-deftest test-wttrin-query-error-stale-callback-does-not-overwrite-newer ()
  "Error: a slow response for an earlier query does not overwrite a newer query.
Paris then Berlin share the *wttr.in* buffer; the Paris callback arriving after
Berlin must be ignored."
  (let ((callbacks nil) (displayed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'wttrin--get-cached-or-fetch)
                   (lambda (_loc cb) (push cb callbacks)))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buf &rest _) (set-buffer buf)))
                  ((symbol-function 'wttrin--display-weather)
                   (lambda (query &rest _) (setq displayed query))))
          (wttrin-query "Paris")
          (wttrin-query "Berlin")
          (let ((paris-cb (cadr callbacks))   ; first pushed
                (berlin-cb (car callbacks)))  ; last pushed
            (funcall berlin-cb "berlin-data")
            (should (equal displayed "Berlin"))
            (funcall paris-cb "paris-data")
            (should (equal displayed "Berlin"))))
      (when (get-buffer "*wttr.in*") (kill-buffer "*wttr.in*")))))

(provide 'test-wttrin-query-request-id)
;;; test-wttrin-query-request-id.el ends here
