;;; test-wttrin--update-layout.el --- Auto-fit gating -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Auto-fit must not size the transient "Loading..." placeholder to fill the
;; window; it runs only once real weather has rendered.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

(ert-deftest test-wttrin--update-layout-error-skips-fit-on-placeholder ()
  "Error: with auto-fit on, the loading placeholder (weather not rendered) is
not auto-fitted; the fit runs only after weather renders."
  (let ((buf (get-buffer-create "*wttrin-layout-test*"))
        (fit-calls 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'wttrin--apply-fit-font)
                     (lambda (&rest _) (setq fit-calls (1+ fit-calls))))
                    ((symbol-function 'wttrin--center-buffer) #'ignore))
            (with-current-buffer buf (insert "Loading..."))
            (set-window-buffer (selected-window) buf)
            (with-current-buffer buf
              (let ((wttrin-auto-fit-font t))
                (setq-local wttrin--weather-rendered nil)
                (wttrin--update-layout)
                (should (= 0 fit-calls))
                (setq-local wttrin--weather-rendered t)
                (wttrin--update-layout)
                (should (= 1 fit-calls))))))
      (kill-buffer buf))))

(provide 'test-wttrin--update-layout)
;;; test-wttrin--update-layout.el ends here
