;;; test-wttrin-error-propagation.el --- Tests for error info through callbacks -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Tests that error information flows from the fetch layer through callbacks
;; to the display layer, so users see specific error messages instead of
;; "Perhaps the location was misspelled?" for every failure.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-error-propagation-setup ()
  "Setup for error propagation tests."
  (testutil-wttrin-setup)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

(defun test-wttrin-error-propagation-teardown ()
  "Teardown for error propagation tests."
  (testutil-wttrin-teardown)
  (when (get-buffer "*wttr.in*")
    (kill-buffer "*wttr.in*")))

;;; --------------------------------------------------------------------------
;;; handle-fetch-callback passes error info to callback
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-error-propagation-network-error-reaches-callback ()
  "Network error description should be passed as second callback argument."
  (let ((received-error nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'message) #'ignore))
      (wttrin--handle-fetch-callback
       '(:error (error "Connection refused"))
       (lambda (_data &optional error-msg)
         (setq received-error error-msg)))
      (should received-error)
      (should (string-match-p "network" (downcase received-error))))))

(ert-deftest test-wttrin-error-propagation-http-error-reaches-callback ()
  "HTTP error description should be passed as second callback argument."
  (let ((received-error nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'wttrin--extract-http-status)
               (lambda () 404))
              ((symbol-function 'message) #'ignore))
      (wttrin--handle-fetch-callback
       nil
       (lambda (_data &optional error-msg)
         (setq received-error error-msg)))
      (should received-error)
      (should (string-match-p "not found\\|404" (downcase received-error))))))

(ert-deftest test-wttrin-error-propagation-success-no-error ()
  "Successful fetch should pass nil as the error argument."
  (let ((received-error 'not-called))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () "weather data"))
              ((symbol-function 'wttrin--extract-http-status)
               (lambda () 200)))
      (wttrin--handle-fetch-callback
       nil
       (lambda (_data &optional error-msg)
         (setq received-error error-msg)))
      (should-not received-error))))

;;; --------------------------------------------------------------------------
;;; get-cached-or-fetch forwards error to caller's callback
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-error-propagation-cache-miss-forwards-error ()
  "When fetch fails with no cache, the error message should reach the caller."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((received-error nil))
        (cl-letf (((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_query callback)
                     (funcall callback nil "Network error — check your connection"))))
          (let ((wttrin--force-refresh t))
            (wttrin--get-cached-or-fetch
             "Paris"
             (lambda (_data &optional error-msg)
               (setq received-error error-msg)))
            (should received-error)
            (should (string-match-p "network" (downcase received-error))))))
    (test-wttrin-error-propagation-teardown)))

(ert-deftest test-wttrin-error-propagation-stale-cache-no-error ()
  "When fetch fails but stale cache exists, serve the data without an error."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((received-data nil)
            (received-error 'not-called))
        (testutil-wttrin-add-to-cache "Paris" "old weather" 9999)
        (cl-letf (((symbol-function 'wttrin-fetch-raw-string)
                   (lambda (_query callback)
                     (funcall callback nil "Network error")))
                  ((symbol-function 'message) #'ignore))
          (let ((wttrin--force-refresh t))
            (wttrin--get-cached-or-fetch
             "Paris"
             (lambda (data &optional error-msg)
               (setq received-data data)
               (setq received-error error-msg)))
            ;; Should get stale data, not an error
            (should received-data)
            (should-not received-error))))
    (test-wttrin-error-propagation-teardown)))

;;; --------------------------------------------------------------------------
;;; display-weather shows specific error message
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-error-propagation-display-shows-specific-error ()
  "When an error message is provided, display-weather should show it
instead of the generic 'Perhaps the location was misspelled?' text."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (wttrin--display-weather "Paris" nil "Network error — check your connection")
          (should displayed-message)
          (should (string-match-p "network" (downcase displayed-message)))
          ;; Should NOT show the generic message
          (should-not (string-match-p "misspelled" (downcase displayed-message)))))
    (test-wttrin-error-propagation-teardown)))

(ert-deftest test-wttrin-error-propagation-display-falls-back-to-generic ()
  "When no error message is provided, display-weather should show the
generic message for backward compatibility."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (wttrin--display-weather "Paris" nil)
          (should displayed-message)
          (should (string-match-p "Cannot retrieve" displayed-message))))
    (test-wttrin-error-propagation-teardown)))

;;; --------------------------------------------------------------------------
;;; End-to-end: full query chain
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-error-propagation-e2e-404-shows-not-found ()
  "Full chain: user queries a bad location, API returns 404, user sees
'not found' — not the generic 'misspelled' message."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url callback)
                     (with-current-buffer (generate-new-buffer " *test-404*")
                       (insert "HTTP/1.1 404 Not Found\r\n\r\nUnknown location")
                       (funcall callback nil))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (wttrin-query "Nonexistent Place")
          (should displayed-message)
          (should (string-match-p "not found\\|404" (downcase displayed-message)))))
    (test-wttrin-error-propagation-teardown)))

(ert-deftest test-wttrin-error-propagation-e2e-network-error-shows-network ()
  "Full chain: network is down, user sees 'network error'."
  (test-wttrin-error-propagation-setup)
  (unwind-protect
      (let ((displayed-message nil))
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url callback)
                     (with-temp-buffer
                       (funcall callback '(:error (error "Connection refused"))))))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq displayed-message (apply #'format fmt args)))))
          (let ((wttrin--force-refresh t))
            (wttrin-query "Paris"))
          (should displayed-message)
          (should (string-match-p "network" (downcase displayed-message)))))
    (test-wttrin-error-propagation-teardown)))

;;; --------------------------------------------------------------------------
;;; Passthrough layers
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin-error-propagation-fetch-raw-string-forwards-error ()
  "wttrin-fetch-raw-string is a passthrough — error-msg from the fetch
layer must not be dropped."
  (let ((received-error nil))
    (cl-letf (((symbol-function 'wttrin--fetch-url)
               (lambda (_url callback)
                 (funcall callback nil "test error message"))))
      (wttrin-fetch-raw-string
       "Paris"
       (lambda (_data &optional error-msg)
         (setq received-error error-msg)))
      ;; fetch-raw-string calls fetch-url which calls handle-fetch-callback
      ;; which calls our callback — but we mocked fetch-url to call directly,
      ;; so the error-msg should pass through
      (should (equal received-error "test error message")))))

(ert-deftest test-wttrin-error-propagation-http-3xx-shows-unexpected ()
  "HTTP 3xx that reaches us (unusual — url-retrieve follows redirects)
should still produce an error message, not silently return nil."
  (let ((received-error nil))
    (cl-letf (((symbol-function 'wttrin--extract-response-body)
               (lambda () nil))
              ((symbol-function 'wttrin--extract-http-status)
               (lambda () 301))
              ((symbol-function 'message) #'ignore))
      (wttrin--handle-fetch-callback
       nil
       (lambda (_data &optional error-msg)
         (setq received-error error-msg)))
      (should received-error)
      (should (string-match-p "301\\|unexpected" (downcase received-error))))))

(provide 'test-wttrin-error-propagation)
;;; test-wttrin-error-propagation.el ends here
