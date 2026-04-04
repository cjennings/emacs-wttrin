;;; test-integration-debug.el --- Integration test with debug enabled -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Comprehensive integration test that:
;; 1. Enables debug mode
;; 2. Mocks weather fetch to avoid network calls
;; 3. Tests mode-line display with real weather data
;; 4. Verifies debug log captures key events

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;; Sample weather data from wttr.in custom format API
(defconst test-wttrin-sample-weather-data
  "Berkeley, CA: ☀️ +62°F Clear"
  "Sample weather data in wttr.in custom format.")

;;; Setup and Teardown

(defun test-integration-debug-setup ()
  "Set up test environment with debug enabled."
  ;; Enable debug mode
  (setq wttrin-debug t)
  ;; Load debug module if not already loaded
  (unless (featurep 'wttrin-debug)
    (require 'wttrin-debug))
  ;; Clear any existing debug log
  (wttrin-debug-clear-log)
  ;; Clear cache
  (wttrin-clear-cache)
  ;; Set test configuration
  (setq wttrin-favorite-location "Berkeley, CA")
  (setq wttrin-mode-line-startup-delay 1)  ; Minimum valid value
  (setq wttrin-unit-system "m"))

(defun test-integration-debug-teardown ()
  "Clean up after tests."
  (when (boundp 'wttrin-mode-line-mode)
    (wttrin-mode-line-mode -1))
  (wttrin-clear-cache)
  (wttrin-debug-clear-log)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil))

;;; Mock URL Fetching

(defvar test-wttrin--original-fetch-url nil
  "Original wttrin--fetch-url function for restoration after test.")

(defun test-integration-debug-mock-fetch (url callback)
  "Mock version of wttrin--fetch-url that returns fake weather data.
URL is ignored. CALLBACK is called with mock data."
  (when (featurep 'wttrin-debug)
    (wttrin--debug-log "MOCK-FETCH: Called with URL: %s" url))
  ;; Call callback directly (synchronous) since run-at-time doesn't work well in batch mode
  (when (featurep 'wttrin-debug)
    (wttrin--debug-log "MOCK-FETCH: Calling callback with mock data"))
  (funcall callback test-wttrin-sample-weather-data))

(defmacro test-integration-debug-with-mocked-fetch (&rest body)
  "Execute BODY with wttrin--fetch-url mocked to return test data."
  `(let ((test-wttrin--original-fetch-url (symbol-function 'wttrin--fetch-url)))
     (unwind-protect
         (progn
           (fset 'wttrin--fetch-url #'test-integration-debug-mock-fetch)
           ,@body)
       (fset 'wttrin--fetch-url test-wttrin--original-fetch-url))))

;;; Integration Tests

(ert-deftest test-integration-debug-mode-line-fetch-and-display-logs-events ()
  "Test the mode-line weather fetch and display pipeline with debug logging.

Context: The mode-line weather feature fetches weather data asynchronously
and updates the mode-line string with an emoji icon and tooltip.

Components integrated:
- `wttrin--mode-line-fetch-weather' (async fetch trigger)
- `wttrin--mode-line-update-display' (mode-line string formatting)
- `wttrin--debug-log' (debug event capture)

Validates that a successful fetch sets the mode-line string with an emoji,
stores tooltip data, and logs fetch-start, data-received, display-update,
and emoji-extraction events to the debug log."
  (test-integration-debug-setup)
  (unwind-protect
      (test-integration-debug-with-mocked-fetch
       ;; Clear debug log
       (wttrin-debug-clear-log)

       ;; Fetch weather for mode-line
       (wttrin--mode-line-fetch-weather)

       ;; Wait for async callback to complete (mocked, so should be fast)
       (sleep-for 0.1)

       ;; Verify mode-line string was set
       (should wttrin-mode-line-string)
       (should (stringp wttrin-mode-line-string))
       (should (string-match-p "☀" wttrin-mode-line-string))  ; Should contain emoji

       ;; Verify cache was populated with weather data
       (should wttrin--mode-line-cache)
       (should (string= test-wttrin-sample-weather-data (cdr wttrin--mode-line-cache)))

       ;; Verify debug log captured key events
       (let ((log-messages (mapcar #'cdr wttrin--debug-log)))
         ;; Should have logged the fetch start
         (should (seq-some (lambda (msg) (string-match-p "mode-line-fetch: Starting" msg))
                          log-messages))
         ;; Should have logged receiving data
         (should (seq-some (lambda (msg) (string-match-p "mode-line-fetch: Received data" msg))
                          log-messages))
         ;; Should have logged display update
         (should (seq-some (lambda (msg) (string-match-p "mode-line-display:" msg))
                          log-messages))
         ;; Should have logged emoji extraction
         (should (seq-some (lambda (msg) (string-match-p "Extracted emoji" msg))
                          log-messages))))
    (test-integration-debug-teardown)))

(ert-deftest test-integration-debug-full-weather-query-creates-buffer ()
  "Test the full weather query pipeline from fetch through buffer display.

Context: When a user calls `wttrin-query', the system fetches weather data,
processes it through ANSI filtering, and displays it in a dedicated buffer.

Components integrated:
- `wttrin-query' (user-facing entry point)
- `wttrin--fetch-url' (HTTP fetch, mocked)
- `wttrin--display-weather' (buffer creation and content rendering)
- `wttrin--debug-log' (debug event capture)

Validates that a full weather query creates the *wttr.in* buffer and
that the debug log records the mock fetch call."
  (test-integration-debug-setup)
  (unwind-protect
      (progn
        ;; Clear debug log
        (wttrin-debug-clear-log)

        ;; Mock full weather fetch (synchronous for batch mode)
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (url callback)
                     (when (featurep 'wttrin-debug)
                       (wttrin--debug-log "MOCK-FETCH: Full weather query for URL: %s" url))
                     ;; Call directly instead of using run-at-time (doesn't work in batch)
                     (funcall callback testutil-wttrin-sample-full-weather))))

          ;; Start the query (now synchronous with mocked fetch)
          (wttrin-query "Berkeley, CA")

          ;; Verify buffer was created
          (should (get-buffer "*wttr.in*"))

          ;; Verify debug log shows mock was called
          (let ((log-messages (mapcar #'cdr wttrin--debug-log)))
            ;; Should have logged the mock fetch
            (should (seq-some (lambda (msg) (string-match-p "MOCK-FETCH: Full weather query" msg))
                             log-messages)))))
    ;; Cleanup
    (when (get-buffer "*wttr.in*")
      (kill-buffer "*wttr.in*"))
    (test-integration-debug-teardown)))

(ert-deftest test-integration-debug-mode-line-mode-toggle-updates-global-string ()
  "Test enabling and disabling wttrin-mode-line-mode updates global state.

Context: `wttrin-mode-line-mode' is a global minor mode that adds a weather
widget to the Emacs mode-line. Toggling it should cleanly add/remove state.

Components integrated:
- `wttrin-mode-line-mode' (global minor mode toggle)
- `wttrin--mode-line-fetch-weather' (initial data fetch)
- `global-mode-string' (Emacs mode-line display list)

Validates that enabling the mode sets `wttrin-mode-line-string' and adds it
to `global-mode-string', and that disabling clears both."
  (test-integration-debug-setup)
  (unwind-protect
      (test-integration-debug-with-mocked-fetch
       ;; Clear debug log
       (wttrin-debug-clear-log)

       ;; Enable mode-line mode
       (wttrin-mode-line-mode 1)
       (should wttrin-mode-line-mode)

       ;; Manually trigger the initial fetch instead of waiting for timer
       ;; (timers don't process well in batch mode)
       (wttrin--mode-line-fetch-weather)

       ;; Verify mode-line string is set
       (should wttrin-mode-line-string)

       ;; Verify global-mode-string contains our widget
       (should (member 'wttrin-mode-line-string global-mode-string))

       ;; Disable mode-line mode
       (wttrin-mode-line-mode -1)
       (should-not wttrin-mode-line-mode)

       ;; Verify mode-line string is cleared
       (should-not wttrin-mode-line-string)

       ;; Verify removed from global-mode-string
       (should-not (member 'wttrin-mode-line-string global-mode-string)))
    (test-integration-debug-teardown)))

(ert-deftest test-integration-debug-error-handling-logs-no-data-received ()
  "Test that network errors are handled gracefully and logged to debug.

Context: When wttr.in is unreachable or returns an error, the fetch callback
receives nil. The mode-line handler should not crash and should log the error.

Components integrated:
- `wttrin--mode-line-fetch-weather' (fetch trigger)
- `wttrin--fetch-url' (HTTP fetch, mocked to return nil)
- `wttrin--debug-log' (error event capture)

Validates that a nil fetch response does not crash and that a
\"No data received\" message is recorded in the debug log."
  (test-integration-debug-setup)
  (unwind-protect
      (progn
        ;; Clear debug log
        (wttrin-debug-clear-log)

        ;; Mock fetch that returns nil (simulating network error)
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (url callback)
                     (when (featurep 'wttrin-debug)
                       (wttrin--debug-log "MOCK-FETCH: Simulating error for URL: %s" url))
                     (run-at-time 0 nil (lambda () (funcall callback nil))))))

          ;; Try to fetch (should handle error gracefully)
          (wttrin--mode-line-fetch-weather)

          ;; Wait for async callback
          (sleep-for 0.1)

          ;; Verify error was logged
          (let ((log-messages (mapcar #'cdr wttrin--debug-log)))
            (should (seq-some (lambda (msg) (string-match-p "No data received" msg))
                             log-messages)))))
    (test-integration-debug-teardown)))

(ert-deftest test-integration-debug-log-inspection-stores-timestamped-entries ()
  "Test that the debug log stores structured entries that can be inspected.

Context: The debug log is an alist of (timestamp . message) pairs used for
diagnosing issues. It should support programmatic inspection and clearing.

Components integrated:
- `wttrin--debug-log' (log writing)
- `wttrin-debug-clear-log' (log clearing)
- `wttrin--debug-log' variable (log storage as alist)

Validates that log entries are (string . string) cons cells, that formatted
messages are stored correctly, and that clearing empties the log."
  (test-integration-debug-setup)
  (unwind-protect
      (progn
        ;; Clear and add some test log entries
        (wttrin-debug-clear-log)
        (wttrin--debug-log "Test message 1")
        (wttrin--debug-log "Test message 2 with arg: %s" "value")

        ;; Verify log structure
        (should (= 2 (length wttrin--debug-log)))
        (should (consp (car wttrin--debug-log)))  ; Each entry is (timestamp . message)
        (should (stringp (caar wttrin--debug-log)))  ; Timestamp is string
        (should (stringp (cdar wttrin--debug-log)))  ; Message is string

        ;; Verify messages
        (let ((messages (mapcar #'cdr wttrin--debug-log)))
          (should (member "Test message 1" messages))
          (should (seq-some (lambda (msg) (string-match-p "Test message 2.*value" msg))
                           messages)))

        ;; Clear log
        (wttrin-debug-clear-log)
        (should (= 0 (length wttrin--debug-log))))
    (test-integration-debug-teardown)))

(provide 'test-integration-debug)
;;; test-integration-debug.el ends here
