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

;;; Test Data - Realistic wttr.in response with ANSI codes

(defconst test-wttrin-ansi-sample-with-colors
  "Weather report: Paris

\x1b[38;5;226m   \\  /\x1b[0m       Partly cloudy
\x1b[38;5;226m _ /\"\"\x1b[38;5;250m.-.\x1b[0m    \x1b[38;5;118m+13\x1b[0m(\x1b[38;5;082m12\x1b[0m) °C
\x1b[38;5;226m   \\_\x1b[38;5;250m(   ).  \x1b[0m ↑ \x1b[38;5;190m12\x1b[0m km/h
"
  "Sample weather data with ANSI color codes (escape sequences).")

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

(ert-deftest test-wttrin-ansi-xterm-color-filter-processes-codes ()
  "Test that xterm-color-filter properly processes ANSI codes."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let ((filtered (xterm-color-filter test-wttrin-ansi-sample-with-colors)))
        ;; After filtering, ANSI escape codes should be removed
        (should-not (test-wttrin-ansi--has-ansi-escape-codes filtered))

        ;; The filtered text should be shorter (escape codes removed)
        (should (< (length filtered) (length test-wttrin-ansi-sample-with-colors)))

        ;; Text should still contain the actual weather content
        (should (string-match-p "Paris" filtered))
        (should (string-match-p "Partly cloudy" filtered)))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-display-weather-renders-colors ()
  "Test that display-weather properly renders ANSI colors in buffer."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "Paris" test-wttrin-ansi-sample-with-colors)

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
            ;; (not all white text)
            (should (> colored 0))

            ;; Colored text should be a reasonable portion (not just 2 chars)
            ;; With ANSI codes, we expect at least 10% of text to be colored
            (should (> colored (/ total 10))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-mode-line-click-renders-colors ()
  "Test that clicking mode-line icon renders colors properly.
This reproduces the bug where mode-line click shows white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let* ((location "Paris")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (wttrin-favorite-location location))

        ;; Mock the async fetch to return ANSI-coded data
        (cl-letf (((symbol-function 'float-time)
                   (lambda () now))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (funcall callback test-wttrin-ansi-sample-with-colors)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          ;; Simulate mode-line click by calling wttrin
          ;; (wttrin-mode-line-click just calls this)
          (wttrin location)

          ;; Give async callback time to execute
          ;; (In real execution this is async, but our mock is synchronous)
          (should (get-buffer "*wttr.in*"))

          (with-current-buffer "*wttr.in*"
            ;; Buffer should not contain raw ANSI codes
            (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
              (should-not (test-wttrin-ansi--has-ansi-escape-codes buffer-text)))

            ;; Check that text has color properties (not all white)
            (let* ((counts (test-wttrin-ansi--count-colored-text))
                   (colored (car counts)))
              ;; Should have colored text (proving colors are rendered)
              (should (> colored 0))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-cached-data-preserves-colors ()
  "Test that cached weather data preserves color information.
Verifies that cache doesn't strip ANSI codes or color properties."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let* ((location "Berlin")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0))

        ;; Pre-populate cache with ANSI-coded data
        (puthash cache-key (cons now test-wttrin-ansi-sample-with-colors)
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
              (should (> colored (/ total 10)))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-buffer-face-mode-doesnt-strip-colors ()
  "Test that buffer-face-mode doesn't strip xterm-color face properties.
This reproduces the bug where weather buffer shows mostly white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (progn
        (wttrin--display-weather "Paris" test-wttrin-ansi-sample-with-colors)

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
            ;; If this fails with ~5% or less, buffer-face-mode is interfering
            (should (> percentage 10.0))

            ;; With face-remap-add-relative fix, we get ~30-40 colored chars
            ;; (our test data is small, so absolute count is low)
            ;; The key is the percentage, not absolute count
            (should (> colored 20)))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-mode-line-doesnt-pollute-main-cache ()
  "Test that mode-line weather fetch doesn't pollute main cache with plain text.
This reproduces the bug where clicking mode-line shows white text."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let* ((location "Berlin")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (plain-text-weather "Berlin: ☀️ +20°C Clear")  ; No ANSI codes
             (ansi-weather test-wttrin-ansi-sample-with-colors))  ; Has ANSI codes

        ;; Simulate mode-line storing plain-text in cache (the bug)
        ;; This shouldn't happen, but let's verify the system is resilient
        (puthash cache-key (cons now plain-text-weather) wttrin--cache)

        (cl-letf (((symbol-function 'float-time)
                   (lambda () (+ now 100.0)))  ; Within TTL, will use cache
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     ;; Should never be called since cache is fresh
                     (error "Should not fetch when cache is fresh"))))

          ;; Call wttrin - it will use cached data
          (wttrin location)

          (should (get-buffer "*wttr.in*"))

          (with-current-buffer "*wttr.in*"
            ;; Even if cache has plain text, should we handle it gracefully?
            ;; At minimum, buffer should exist and not error
            (should (> (buffer-size) 0))

            ;; The real fix: mode-line should use separate cache or namespace
            ;; For now, document that plain-text cache = no colors
            (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
              ;; Should contain the weather data (even if not colored)
              (should (string-match-p "Berlin" buffer-text))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-full-scenario-mode-line-then-click ()
  "Test full scenario: mode-line fetch, then user clicks to open buffer.
This is the exact user workflow that exposes the bug."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let* ((location "Tokyo")
             (now 1000.0)
             (wttrin-favorite-location location)
             (mode-line-fetch-count 0)
             (main-fetch-count 0))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () now))
                  ((symbol-function 'url-retrieve)
                   (lambda (url callback)
                     ;; Mode-line uses url-retrieve directly
                     (setq mode-line-fetch-count (1+ mode-line-fetch-count))
                     ;; Simulate async callback with plain-text format
                     (with-temp-buffer
                       (insert "HTTP/1.1 200 OK\n\n")
                       (insert "Tokyo: ☀️ +25°C Clear")  ; Plain text, no ANSI
                       (funcall callback nil))))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     ;; Main buffer fetch should get ANSI codes
                     (setq main-fetch-count (1+ main-fetch-count))
                     (funcall callback test-wttrin-ansi-sample-with-colors)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          ;; Step 1: Mode-line fetches weather (simulated)
          (wttrin--mode-line-fetch-weather)
          ;; Mode-line should have fetched
          (should (= mode-line-fetch-count 1))

          ;; Step 2: User clicks mode-line icon (calls wttrin)
          (wttrin location)
          ;; Main fetch should have happened (cache miss)
          (should (= main-fetch-count 1))

          ;; Step 3: Verify buffer has colors
          (should (get-buffer "*wttr.in*"))

          (with-current-buffer "*wttr.in*"
            (let* ((counts (test-wttrin-ansi--count-colored-text))
                   (colored (car counts))
                   (total (cdr counts)))
              ;; Should have colored text
              (should (> colored 0))
              ;; Should be substantial (>10%)
              (should (> colored (/ total 10)))))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-cache-stores-ansi-codes ()
  "Test that cache stores raw ANSI codes, not filtered text.
This verifies the cache workflow preserves ANSI codes for later filtering."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let* ((location "Vienna")
             (cache-key (wttrin--make-cache-key location))
             (now 1000.0)
             (fetch-count 0))

        (cl-letf (((symbol-function 'float-time)
                   (lambda () now))
                  ((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_location callback)
                     (setq fetch-count (1+ fetch-count))
                     ;; Simulate fetch returning ANSI codes
                     (funcall callback test-wttrin-ansi-sample-with-colors)))
                  ((symbol-function 'wttrin--cleanup-cache-if-needed)
                   (lambda () nil)))

          ;; Fetch weather (will cache the result)
          (wttrin location)

          ;; Verify fetch was called
          (should (= fetch-count 1))

          ;; Check what's in the cache
          (let* ((cached (gethash cache-key wttrin--cache))
                 (cached-data (cdr cached)))

            ;; Cache should have data
            (should cached)
            (should cached-data)

            ;; CRITICAL: Cache should store RAW ANSI codes
            ;; NOT filtered text with face properties
            (should (stringp cached-data))
            (should (string-match-p "\x1b\\[" cached-data))

            ;; Verify it's the original ANSI string
            (should (string-match-p "Partly cloudy" cached-data)))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-fetch-returns-ansi-codes ()
  "Test that wttrin-fetch-raw-string returns data with ANSI codes.
This verifies the fetch function returns unfiltered data from wttr.in."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (let ((location "Prague")
            (callback-data nil)
            (url-retrieve-called nil)
            (wttrin-unit-system "u"))  ; Set unit system for URL generation

        ;; Mock url-retrieve to simulate wttr.in response
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url callback)
                     (setq url-retrieve-called t)
                     ;; Verify URL has ANSI format flag
                     ;; wttr.in uses ?mA, ?uA, or ?A for ANSI colored output
                     (should (or (string-match-p "\\?mA" url)
                                 (string-match-p "\\?uA" url)
                                 (string-match-p "\\?A" url)))
                     ;; Simulate HTTP response with ANSI codes
                     (with-temp-buffer
                       (insert "HTTP/1.1 200 OK\n\n")
                       (insert test-wttrin-ansi-sample-with-colors)
                       (funcall callback nil)))))

          ;; Call fetch
          (wttrin-fetch-raw-string
           location
           (lambda (data)
             (setq callback-data data)))

          ;; Verify url-retrieve was called
          (should url-retrieve-called)

          ;; Verify callback received ANSI codes
          (should callback-data)
          (should (string-match-p "\x1b\\[" callback-data))))
    (test-wttrin-ansi-teardown)))

(ert-deftest test-wttrin-ansi-build-url-includes-ansi-flag ()
  "Test that wttrin--build-url includes ANSI color flag in URL.
The 'A' flag tells wttr.in to include ANSI color codes in the response."
  (test-wttrin-ansi-setup)
  (unwind-protect
      (progn
        ;; Test with metric system
        (let ((wttrin-unit-system "m"))
          (let ((url (wttrin--build-url "Berlin")))
            ;; Should have ?mA flag (metric + ANSI)
            (should (string-match-p "\\?mA" url))))

        ;; Test with USCS system
        (let ((wttrin-unit-system "u"))
          (let ((url (wttrin--build-url "London")))
            ;; Should have ?uA flag (USCS + ANSI)
            (should (string-match-p "\\?uA" url))))

        ;; Test with no unit system
        (let ((wttrin-unit-system nil))
          (let ((url (wttrin--build-url "Paris")))
            ;; Should have just ?A flag (ANSI)
            (should (string-match-p "\\?A" url)))))
    (test-wttrin-ansi-teardown)))

(provide 'test-wttrin-ansi-color-rendering)
;;; test-wttrin-ansi-color-rendering.el ends here
