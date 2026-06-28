;;; test-wttrin--display-weather-focus.el --- Async render doesn't steal focus -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; wttrin--display-weather runs from an async callback.  If the user moved to
;; another buffer while the fetch was in flight, rendering the result must not
;; select *wttr.in* and steal the window.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

(ert-deftest test-wttrin--display-weather-error-does-not-steal-focus ()
  "Error: rendering an async response does not select *wttr.in*.
If the user moved to another buffer while the fetch was in flight, displaying
the result must leave the selected window on that other buffer."
  (let ((other (get-buffer-create "*wttrin-other*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) other)
          (wttrin--display-weather "Paris" "Weather report: paris\n+10 C\n"
                                   nil nil nil)
          (should (eq (window-buffer (selected-window)) other)))
      (when (get-buffer "*wttr.in*") (kill-buffer "*wttr.in*"))
      (when (buffer-live-p other) (kill-buffer other)))))

(ert-deftest test-wttrin--display-weather-normal-populates-buffer ()
  "Normal: rendering still fills *wttr.in* with the weather content."
  (let ((other (get-buffer-create "*wttrin-other*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) other)
          (wttrin--display-weather "Paris" "Weather report: paris\n+10 C\n"
                                   nil nil nil)
          (with-current-buffer "*wttr.in*"
            (should (string-match-p "Weather report: Paris"
                                    (buffer-string)))))
      (when (get-buffer "*wttr.in*") (kill-buffer "*wttr.in*"))
      (when (buffer-live-p other) (kill-buffer other)))))

(provide 'test-wttrin--display-weather-focus)
;;; test-wttrin--display-weather-focus.el ends here
