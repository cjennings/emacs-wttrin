;;; test-wttrin--render-loading-placeholder.el --- Loading placeholder centering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:
;; The loading placeholder must be centered every time it is shown, including on
;; an `a' switch where the *wttr.in* buffer is already displayed.  In that case
;; `window-configuration-change-hook' does not fire, so `wttrin-query' relied on
;; a stale window margin from the previous weather and the placeholder rendered
;; off-center.  `wttrin--render-loading-placeholder' centers it explicitly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)

(ert-deftest test-wttrin--render-loading-placeholder-normal-shows-display-name ()
  "Normal: the placeholder text names the display string, not the raw query."
  (with-temp-buffer
    (cl-letf (((symbol-function 'wttrin--update-layout) #'ignore))
      (wttrin--render-loading-placeholder "1500 Sugar Bowl Dr" "Superdome")
      (should (string-match-p "Loading weather for Superdome\\.\\.\\."
                              (buffer-string)))
      (should-not wttrin--weather-rendered))))

(ert-deftest test-wttrin--render-loading-placeholder-boundary-nil-display-uses-query ()
  "Boundary: with no display name, the placeholder falls back to the query."
  (with-temp-buffer
    (cl-letf (((symbol-function 'wttrin--update-layout) #'ignore))
      (wttrin--render-loading-placeholder "Reykjavik" nil)
      (should (string-match-p "Loading weather for Reykjavik" (buffer-string))))))

(ert-deftest test-wttrin--render-loading-placeholder-normal-centers-via-layout ()
  "Normal: rendering the placeholder runs the layout pass, so it is centered by
the same path weather uses rather than left to the hook (which does not fire on
a reused, already-displayed buffer)."
  (with-temp-buffer
    (let ((layout-calls 0))
      (cl-letf (((symbol-function 'wttrin--update-layout)
                 (lambda (&rest _) (setq layout-calls (1+ layout-calls)))))
        (wttrin--render-loading-placeholder "Reykjavik" "Reykjavik")
        (should (= 1 layout-calls))))))

(ert-deftest test-wttrin--render-loading-placeholder-regression-replaces-stale-margin ()
  "Regression: a stale window margin left by the previous weather block is
recomputed for the placeholder, so it no longer renders at the old margin after
an `a' switch."
  (let ((buf (get-buffer-create "*wttrin-ph-test*")))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          ;; A wide margin as if left over from a previous (wide) weather block.
          (set-window-margins (selected-window) 40)
          (with-current-buffer buf
            (let ((wttrin-auto-fit-font nil))
              (wttrin--render-loading-placeholder "Reykjavik" "Reykjavik")
              ;; The placeholder path must have recomputed the margin, not left
              ;; the stale 40 in place.
              (should-not (equal 40 (car (window-margins (selected-window))))))))
      (kill-buffer buf))))

(provide 'test-wttrin--render-loading-placeholder)
;;; test-wttrin--render-loading-placeholder.el ends here
