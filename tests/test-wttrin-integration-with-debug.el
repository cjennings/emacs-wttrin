;;; test-wttrin-integration-with-debug.el --- Integration test with debug enabled -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Comprehensive integration test that:
;; 1. Enables debug mode
;; 2. Mocks weather fetch to avoid network calls
;; 3. Tests mode-line display with real weather data
;; 4. Verifies debug log captures key events

;;; Code:

(require 'ert)
(require 'wttrin)

;; Sample weather data from wttr.in custom format API
(defconst test-wttrin-sample-weather-data
  "Berkeley, CA: ☀️ +62°F Clear"
  "Sample weather data in wttr.in custom format.")

(defconst test-wttrin-sample-full-weather
  "Weather for Berkeley, CA

     \\    /      Clear
      .-.       62 °F
   ― (   ) ―    ↑ 5 mph
      `-'       10 mi
     /    \\     0.0 in"
  "Sample full weather display data.")

;;; Setup and Teardown

(defun test-wttrin-setup ()
  "Set up test environment with debug enabled."
  ;; Enable debug mode
  (setq wttrin-debug t)
  ;; Clear any existing debug log
  (when (featurep 'wttrin-debug)
    (wttrin--debug-clear-log))
  ;; Clear cache
  (wttrin-clear-cache)
  ;; Set test configuration
  (setq wttrin-mode-line-favorite-location "Berkeley, CA")
  (setq wttrin-mode-line-startup-delay 0)
  (setq wttrin-unit-system "m"))

(defun test-wttrin-teardown ()
  "Clean up after tests."
  (when (boundp 'wttrin-mode-line-mode)
    (wttrin-mode-line-mode -1))
  (wttrin-clear-cache)
  (when (featurep 'wttrin-debug)
    (wttrin--debug-clear-log))
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-tooltip-data nil))

;;; Mock URL Fetching

(defvar test-wttrin--original-fetch-url nil
  "Original wttrin--fetch-url function for restoration after test.")

(defun test-wttrin-mock-fetch (url callback)
  "Mock version of wttrin--fetch-url that returns fake weather data.
URL is ignored. CALLBACK is called with mock data."
  (when (featurep 'wttrin-debug)
    (wttrin--debug-log "MOCK-FETCH: Called with URL: %s" url))
  ;; Simulate async by using run-at-time with 0 delay
  (run-at-time
   0 nil
   (lambda ()
     (when (featurep 'wttrin-debug)
       (wttrin--debug-log "MOCK-FETCH: Calling callback with mock data"))
     (funcall callback test-wttrin-sample-weather-data))))

(defmacro with-mocked-fetch (&rest body)
  "Execute BODY with wttrin--fetch-url mocked to return test data."
  `(let ((test-wttrin--original-fetch-url (symbol-function 'wttrin--fetch-url)))
     (unwind-protect
         (progn
           (fset 'wttrin--fetch-url #'test-wttrin-mock-fetch)
           ,@body)
       (fset 'wttrin--fetch-url test-wttrin--original-fetch-url))))

;;; Integration Tests

(ert-deftest test-wttrin-debug-integration-mode-line-fetch-and-display ()
  "Integration test: Fetch weather and verify mode-line display with debug logging."
  (test-wttrin-setup)
  (unwind-protect
      (with-mocked-fetch
       ;; Clear debug log
       (wttrin--debug-clear-log)

       ;; Fetch weather for mode-line
       (wttrin--mode-line-fetch-weather)

       ;; Wait for async callback to complete (mocked, so should be fast)
       (sleep-for 0.1)

       ;; Verify mode-line string was set
       (should wttrin-mode-line-string)
       (should (stringp wttrin-mode-line-string))
       (should (string-match-p "☀" wttrin-mode-line-string))  ; Should contain emoji

       ;; Verify tooltip data was set
       (should wttrin--mode-line-tooltip-data)
       (should (string= test-wttrin-sample-weather-data wttrin--mode-line-tooltip-data))

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
    (test-wttrin-teardown)))

(ert-deftest test-wttrin-debug-integration-full-weather-query ()
  "Integration test: Query full weather and verify debug logging."
  (test-wttrin-setup)
  (unwind-protect
      (progn
        ;; Clear debug log
        (wttrin--debug-clear-log)

        ;; Mock full weather fetch
        (cl-letf (((symbol-function 'wttrin--fetch-url)
                   (lambda (url callback)
                     (when (featurep 'wttrin-debug)
                       (wttrin--debug-log "MOCK-FETCH: Full weather query for URL: %s" url))
                     (run-at-time 0 nil
                                  (lambda ()
                                    (funcall callback test-wttrin-sample-full-weather))))))

          ;; Start the query (async, so we'll check results after delay)
          (wttrin-query "Berkeley, CA")

          ;; Wait for async operations
          (sleep-for 0.2)

          ;; Verify buffer was created
          (should (get-buffer "*wttr.in*"))

          ;; Verify debug log shows URL building
          (let ((log-messages (mapcar #'cdr wttrin--debug-log)))
            ;; Should have logged fetch starting
            (should (seq-some (lambda (msg) (string-match-p "wttrin--fetch-url: Starting" msg))
                             log-messages))
            ;; Should have logged success
            (should (seq-some (lambda (msg) (string-match-p "Successfully fetched" msg))
                             log-messages)))))
    ;; Cleanup
    (when (get-buffer "*wttr.in*")
      (kill-buffer "*wttr.in*"))
    (test-wttrin-teardown)))

(ert-deftest test-wttrin-debug-integration-mode-line-mode-toggle ()
  "Integration test: Toggle mode-line mode and verify debug logging."
  (test-wttrin-setup)
  (unwind-protect
      (with-mocked-fetch
       ;; Clear debug log
       (wttrin--debug-clear-log)

       ;; Enable mode-line mode
       (wttrin-mode-line-mode 1)
       (should wttrin-mode-line-mode)

       ;; Wait longer for initial fetch and callback (async operations take time)
       (sleep-for 0.3)

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
    (test-wttrin-teardown)))

(ert-deftest test-wttrin-debug-integration-error-handling ()
  "Integration test: Verify debug logging captures errors correctly."
  (test-wttrin-setup)
  (unwind-protect
      (progn
        ;; Clear debug log
        (wttrin--debug-clear-log)

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
    (test-wttrin-teardown)))

(ert-deftest test-wttrin-debug-integration-log-inspection ()
  "Integration test: Verify debug log can be inspected programmatically."
  (test-wttrin-setup)
  (unwind-protect
      (progn
        ;; Clear and add some test log entries
        (wttrin--debug-clear-log)
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
        (wttrin--debug-clear-log)
        (should (= 0 (length wttrin--debug-log))))
    (test-wttrin-teardown)))

(provide 'test-wttrin-integration-with-debug)
;;; test-wttrin-integration-with-debug.el ends here
