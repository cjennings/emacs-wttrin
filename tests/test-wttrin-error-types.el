;;; test-wttrin-error-types.el --- Tests for wttrin's typed error hierarchy -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the define-error hierarchy (wttrin-error and children),
;; the wttrin--error-message constructor, the wttrin-error-type accessor,
;; and the error-class tagging of the async fetch callback's error-msg.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin-error-types-setup ()
  "Setup for error-type tests."
  (testutil-wttrin-setup))

(defun test-wttrin-error-types-teardown ()
  "Teardown for error-type tests."
  (testutil-wttrin-teardown))

;;; Hierarchy

(ert-deftest test-wttrin-error-types-normal-parent-inherits-error ()
  "wttrin-error is a child of the built-in error condition."
  (should (memq 'error (get 'wttrin-error 'error-conditions)))
  (should (memq 'wttrin-error (get 'wttrin-error 'error-conditions))))

(ert-deftest test-wttrin-error-types-normal-children-inherit-parent-and-error ()
  "Each child condition inherits both wttrin-error and error."
  (dolist (child '(wttrin-invalid-input
                   wttrin-network-error
                   wttrin-not-found-error
                   wttrin-service-error
                   wttrin-parse-error))
    (let ((conds (get child 'error-conditions)))
      (should (memq child conds))
      (should (memq 'wttrin-error conds))
      (should (memq 'error conds)))))

;;; wttrin--error-message constructor

(ert-deftest test-wttrin-error-types-normal-error-message-carries-type-property ()
  "wttrin--error-message returns the formatted string tagged with its type."
  (let ((msg (wttrin--error-message 'wttrin-network-error "Network error")))
    (should (string= "Network error" msg))
    (should (eq 'wttrin-network-error (get-text-property 0 'wttrin-error-type msg)))))

(ert-deftest test-wttrin-error-types-normal-error-message-formats-arguments ()
  "wttrin--error-message applies format arguments like `format'."
  (let ((msg (wttrin--error-message 'wttrin-not-found-error
                                    "Location not found (HTTP %d)" 404)))
    (should (string= "Location not found (HTTP 404)" msg))
    (should (eq 'wttrin-not-found-error (wttrin-error-message-type msg)))))

;;; wttrin-error-type accessor

(ert-deftest test-wttrin-error-types-normal-error-type-reads-tag ()
  "wttrin-error-type returns the class symbol from a tagged message."
  (let ((msg (wttrin--error-message 'wttrin-service-error "boom")))
    (should (eq 'wttrin-service-error (wttrin-error-message-type msg)))))

(ert-deftest test-wttrin-error-types-boundary-error-type-plain-string-is-nil ()
  "An untagged string has no error type."
  (should (null (wttrin-error-message-type "just a string"))))

(ert-deftest test-wttrin-error-types-boundary-error-type-nil-is-nil ()
  "nil has no error type."
  (should (null (wttrin-error-message-type nil))))

(ert-deftest test-wttrin-error-types-boundary-error-type-empty-string-is-nil ()
  "An empty string has no error type."
  (should (null (wttrin-error-message-type ""))))

;;; Synchronous signal site

(ert-deftest test-wttrin-error-types-error-build-url-nil-signals-invalid-input ()
  "A nil query signals the specific wttrin-invalid-input condition."
  (should-error (wttrin--build-url nil) :type 'wttrin-invalid-input)
  (should-error (wttrin--build-url nil) :type 'wttrin-error))

;;; Async classification — error-msg handed to the callback carries the class

(defun test-wttrin-error-types--capture-error-msg (status &optional status-code)
  "Run the fetch callback with STATUS, return the error-msg it receives.
STATUS-CODE, when non-nil, is the HTTP status the buffer reports."
  (let ((captured 'unset))
    (cl-letf (((symbol-function 'wttrin--extract-response-body) (lambda () nil))
              ((symbol-function 'wttrin--extract-http-status) (lambda () status-code))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (wttrin--handle-fetch-callback
       status
       (lambda (_data &optional error-msg) (setq captured error-msg))))
    captured))

(ert-deftest test-wttrin-error-types-error-network-failure-tagged-network ()
  "A network-level failure tags the error-msg as wttrin-network-error."
  (let ((msg (test-wttrin-error-types--capture-error-msg
              '(:error (error "Network unreachable")))))
    (should (eq 'wttrin-network-error (wttrin-error-message-type msg)))))

(ert-deftest test-wttrin-error-types-error-http-404-tagged-not-found ()
  "An HTTP 4xx tags the error-msg as wttrin-not-found-error."
  (let ((msg (test-wttrin-error-types--capture-error-msg nil 404)))
    (should (eq 'wttrin-not-found-error (wttrin-error-message-type msg)))))

(ert-deftest test-wttrin-error-types-error-http-500-tagged-service ()
  "An HTTP 5xx tags the error-msg as wttrin-service-error."
  (let ((msg (test-wttrin-error-types--capture-error-msg nil 500)))
    (should (eq 'wttrin-service-error (wttrin-error-message-type msg)))))

(ert-deftest test-wttrin-error-types-error-2xx-empty-body-tagged-parse ()
  "A 2xx response with no usable body tags the error-msg as wttrin-parse-error."
  (let ((msg (test-wttrin-error-types--capture-error-msg nil 200)))
    (should (eq 'wttrin-parse-error (wttrin-error-message-type msg)))))

(ert-deftest test-wttrin-error-types-error-missing-status-tagged-parse ()
  "An unreadable response (no status, no body) tags the error-msg as wttrin-parse-error."
  (let ((msg (test-wttrin-error-types--capture-error-msg nil nil)))
    (should (eq 'wttrin-parse-error (wttrin-error-message-type msg)))))

(provide 'test-wttrin-error-types)
;;; test-wttrin-error-types.el ends here
