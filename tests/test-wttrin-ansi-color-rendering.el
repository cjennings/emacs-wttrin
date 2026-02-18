;;; test-wttrin-ansi-color-rendering.el --- Test ANSI color rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;;; Commentary:

;; Test that ANSI color codes are properly rendered in the weather buffer.
;; This reproduces the bug where clicking the mode-line icon shows white text.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'xterm-color)
(require 'testutil-wttrin)

;;; Test Setup and Teardown

(defun test-wttrin-ansi-setup ()
  "Setup for ANSI color rendering tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-ansi-teardown ()
  "Teardown for ANSI color rendering tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; Helper Functions

(defun test-wttrin-ansi--count-colored-text ()
  "Count number of characters with color text properties in current buffer.
Returns cons (colored-chars . total-chars)."
  (save-excursion
    (goto-char (point-min))
    (let ((colored 0)
          (total 0))
      (while (not (eobp))
        (let ((props (text-properties-at (point))))
          (setq total (1+ total))
          ;; Check for xterm-color face properties
          (when (or (plist-get props 'face)
                    (plist-get props 'font-lock-face))
            (setq colored (1+ colored))))
        (forward-char 1))
      (cons colored total))))

(defun test-wttrin-ansi--has-ansi-escape-codes (str)
  "Check if STR contains ANSI escape codes."
  (string-match-p "\x1b\\[" str))

;;; Tests

(ert-deftest test-wttrin-ansi-normal-xterm-color-filter-removes-escape-codes ()
  "Test that xterm-color-filter properly processes ANSI codes."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let ((filtered (xterm-color-filter testutil-wttrin-sample-ansi-response)))
        ;; After filtering, ANSI escape codes should be removed
        (should-not (test-wttrin-ansi--has-ansi-escape-codes filtered))

        ;; The filtered text should be shorter (escape codes removed)
        (should (< (length filtered) (length testutil-wttrin-sample-ansi-response)))

        ;; Text should still contain the actual weather content
        (should (string-match-p "Paris" filtered))
        (should (string-match-p "Partly cloudy" filtered)))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-display-weather-renders-colored-text ()
  "Test that display-weather properly renders ANSI colors in buffer."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (wttrin--display-weather "Paris" testutil-wttrin-sample-ansi-response)

        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; Buffer should not contain raw ANSI escape codes
          (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
            (should-not (test-wttrin-ansi--has-ansi-escape-codes buffer-text)))

          ;; Buffer should contain the weather text
          (goto-char (point-min))
          (should (search-forward "Paris" nil t))
          (should (search-forward "Partly cloudy" nil t))

          ;; Check that some text has color properties
          (let* ((counts (test-wttrin-ansi--count-colored-text))
                 (colored (car counts))
                 (total (cdr counts)))
            ;; At least some characters should have color properties
            (should (> colored 0))
            ;; Colored text should be a reasonable portion (not just 2 chars)
            (should (> colored (/ total 10))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-mode-line-click-renders-colored-text ()
  "Test that clicking mode-line icon renders colors properly.
This reproduces the bug where mode-line click shows white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (let* ((location "Paris")
               (cache-key (wttrin--make-cache-key location))
               (now 1000.0)
               (wttrin-favorite-location location))

          ;; Mock the async fetch to return ANSI-coded data
          (cl-letf (((symbol-function 'float-time)
                     (lambda () now))
                    ((symbol-function 'wttrin-fetch-raw-string)
                     (lambda (_location callback)
                       (funcall callback testutil-wttrin-sample-ansi-response)))
                    ((symbol-function 'wttrin--cleanup-cache-if-needed)
                     (lambda () nil)))

            (wttrin location)

            (should (get-buffer "*wttr.in*"))

            (with-current-buffer "*wttr.in*"
              ;; Buffer should not contain raw ANSI codes
              (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
                (should-not (test-wttrin-ansi--has-ansi-escape-codes buffer-text)))

              ;; Check that text has color properties (not all white)
              (let* ((counts (test-wttrin-ansi--count-colored-text))
                     (colored (car counts)))
                (should (> colored 0)))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-cached-data-preserves-color-properties ()
  "Test that cached weather data preserves color information.
Verifies that cache doesn't strip ANSI codes or color properties."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (let* ((location "Berlin")
               (cache-key (wttrin--make-cache-key location))
               (now 1000.0))

          ;; Pre-populate cache with ANSI-coded data
          (puthash cache-key (cons now testutil-wttrin-sample-ansi-response)
                   wttrin--cache)

          (cl-letf (((symbol-function 'float-time)
                     (lambda () (+ now 100.0)))) ; Within TTL

            ;; Call wttrin - should use cached data
            (wttrin location)

            (should (get-buffer "*wttr.in*"))

            (with-current-buffer "*wttr.in*"
              ;; Even with cached data, colors should be rendered
              (let* ((counts (test-wttrin-ansi--count-colored-text))
                     (colored (car counts))
                     (total (cdr counts)))
                (should (> colored 0))
                ;; Should have reasonable color coverage
                (should (> colored (/ total 10))))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-boundary-buffer-face-mode-preserves-colors ()
  "Test that buffer-face-mode doesn't strip xterm-color face properties.
This reproduces the bug where weather buffer shows mostly white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (wttrin--display-weather "Paris" testutil-wttrin-sample-ansi-response)

        (should (get-buffer "*wttr.in*"))

        (with-current-buffer "*wttr.in*"
          ;; Check that face-remapping is active (for font customization)
          (should face-remapping-alist)

          ;; But colors should still be present despite face remapping
          (let* ((counts (test-wttrin-ansi--count-colored-text))
                 (colored (car counts))
                 (total (cdr counts))
                 (percentage (if (> total 0)
                                 (* 100.0 (/ (float colored) total))
                               0)))
            ;; Should have substantial colored text (>10%)
            (should (> percentage 10.0))
            (should (> colored 20)))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-boundary-mode-line-plain-text-cache-shows-content ()
  "Test that mode-line weather fetch doesn't pollute main cache with plain text.
This reproduces the bug where clicking mode-line shows white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (let* ((location "Berlin")
               (cache-key (wttrin--make-cache-key location))
               (now 1000.0)
               (plain-text-weather "Berlin: ☀️ +20°C Clear"))  ; No ANSI codes

          ;; Simulate mode-line storing plain-text in cache (the bug)
          (puthash cache-key (cons now plain-text-weather) wttrin--cache)

          (cl-letf (((symbol-function 'float-time)
                     (lambda () (+ now 100.0)))  ; Within TTL, will use cache
                    ((symbol-function 'wttrin-fetch-raw-string)
                     (lambda (_location callback)
                       (error "Should not fetch when cache is fresh"))))

            (wttrin location)

            (should (get-buffer "*wttr.in*"))

            (with-current-buffer "*wttr.in*"
              (should (> (buffer-size) 0))
              (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Berlin" buffer-text)))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-full-scenario-mode-line-then-click-has-colors ()
  "Test full scenario: mode-line fetch, then user clicks to open buffer.
This is the exact user workflow that exposes the bug."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (let* ((location "Tokyo")
               (now 1000.0)
               (wttrin-favorite-location location)
               (mode-line-fetch-count 0)
               (main-fetch-count 0))

          (cl-letf (((symbol-function 'float-time)
                     (lambda () now))
                    ((symbol-function 'url-retrieve)
                     (lambda (url callback)
                       (setq mode-line-fetch-count (1+ mode-line-fetch-count))
                       (with-temp-buffer
                         (insert "HTTP/1.1 200 OK\n\n")
                         (insert "Tokyo: ☀️ +25°C Clear")
                         (funcall callback nil))))
                    ((symbol-function 'wttrin-fetch-raw-string)
                     (lambda (_location callback)
                       (setq main-fetch-count (1+ main-fetch-count))
                       (funcall callback testutil-wttrin-sample-ansi-response)))
                    ((symbol-function 'wttrin--cleanup-cache-if-needed)
                     (lambda () nil)))

            ;; Step 1: Mode-line fetches weather (simulated)
            (wttrin--mode-line-fetch-weather)
            (should (= mode-line-fetch-count 1))

            ;; Step 2: User clicks mode-line icon (calls wttrin)
            (wttrin location)
            (should (= main-fetch-count 1))

            ;; Step 3: Verify buffer has colors
            (should (get-buffer "*wttr.in*"))

            (with-current-buffer "*wttr.in*"
              (let* ((counts (test-wttrin-ansi--count-colored-text))
                     (colored (car counts))
                     (total (cdr counts)))
                (should (> colored 0))
                (should (> colored (/ total 10))))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-cache-stores-raw-ansi-codes ()
  "Test that cache stores raw ANSI codes, not filtered text.
This verifies the cache workflow preserves ANSI codes for later filtering."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (testutil-wttrin-with-clean-weather-buffer
        (let* ((location "Vienna")
               (cache-key (wttrin--make-cache-key location))
               (now 1000.0)
               (fetch-count 0))

          (cl-letf (((symbol-function 'float-time)
                     (lambda () now))
                    ((symbol-function 'wttrin-fetch-raw-string)
                     (lambda (_location callback)
                       (setq fetch-count (1+ fetch-count))
                       (funcall callback testutil-wttrin-sample-ansi-response)))
                    ((symbol-function 'wttrin--cleanup-cache-if-needed)
                     (lambda () nil)))

            (wttrin location)

            (should (= fetch-count 1))

            (let* ((cached (gethash cache-key wttrin--cache))
                   (cached-data (cdr cached)))
              (should cached)
              (should cached-data)
              ;; CRITICAL: Cache should store RAW ANSI codes
              (should (stringp cached-data))
              (should (string-match-p "\x1b\\[" cached-data))
              (should (string-match-p "Partly cloudy" cached-data))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-fetch-returns-raw-ansi-codes ()
  "Test that wttrin-fetch-raw-string returns data with ANSI codes.
This verifies the fetch function returns unfiltered data from wttr.in."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let ((location "Prague")
            (callback-data nil)
            (url-retrieve-called nil)
            (wttrin-unit-system "u"))

        ;; Mock url-retrieve to simulate wttr.in response
        (testutil-wttrin-mock-http-response testutil-wttrin-sample-ansi-response
          (wttrin-fetch-raw-string
           location
           (lambda (data)
             (setq callback-data data)))

          ;; Verify callback received ANSI codes
          (should callback-data)
          (should (string-match-p "\x1b\\[" callback-data))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-normal-build-url-includes-ansi-flag ()
  "Test that wttrin--build-url includes ANSI color flag in URL.
The 'A' flag tells wttr.in to include ANSI color codes in the response."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (progn
        ;; Test with metric system
        (let ((wttrin-unit-system "m"))
          (let ((url (wttrin--build-url "Berlin")))
            (should (string-match-p "\\?mA" url))))

        ;; Test with USCS system
        (let ((wttrin-unit-system "u"))
          (let ((url (wttrin--build-url "London")))
            (should (string-match-p "\\?uA" url))))

        ;; Test with no unit system
        (let ((wttrin-unit-system nil))
          (let ((url (wttrin--build-url "Paris")))
            (should (string-match-p "\\?A" url)))))
    (test-wttrin-ansi-teardown)))

(provide 'test-wttrin-ansi-color-rendering)
;;; test-wttrin-ansi-color-rendering.el ends here
