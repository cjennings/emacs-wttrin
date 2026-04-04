;;; test-wttrin--mode-line-start.el --- Tests for wttrin--mode-line-start -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;;; Commentary:

;; Unit tests for wttrin--mode-line-start function.
;; Tests that starting mode-line display sets up placeholder, timers, and scheduling.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Setup and Teardown

(defun test-wttrin--mode-line-start-setup ()
  "Setup for mode-line-start tests."
  (testutil-wttrin-setup)
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-timer nil)
  (setq wttrin--buffer-refresh-timer nil))

(defun test-wttrin--mode-line-start-teardown ()
  "Teardown for mode-line-start tests."
  (testutil-wttrin-teardown)
  (when (timerp wttrin--mode-line-timer)
    (cancel-timer wttrin--mode-line-timer))
  (when (timerp wttrin--buffer-refresh-timer)
    (cancel-timer wttrin--buffer-refresh-timer))
  (setq wttrin-mode-line-string nil)
  (setq wttrin--mode-line-cache nil)
  (setq wttrin--mode-line-timer nil)
  (setq wttrin--buffer-refresh-timer nil))

;;; Normal Cases

(ert-deftest test-wttrin--mode-line-start-normal-shows-placeholder-immediately ()
  "Starting mode-line should show the hourglass placeholder right away."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-emoji-font nil))
        ;; Mock run-at-time so no real timers fire
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat _func) (list 'mock-timer))))
          (wttrin--mode-line-start)
          ;; Placeholder should be visible
          (should wttrin-mode-line-string)
          (should (string-match-p "⏳" (substring-no-properties wttrin-mode-line-string)))))
    (test-wttrin--mode-line-start-teardown)))

(ert-deftest test-wttrin--mode-line-start-normal-schedules-delayed-initial-fetch ()
  "Initial weather fetch should be scheduled after startup-delay seconds."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-startup-delay 5)
            (scheduled-calls nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (time repeat func)
                     (push (list :time time :repeat repeat :func func) scheduled-calls)
                     (list 'mock-timer))))
          (wttrin--mode-line-start)
          ;; One of the calls should be the delayed initial fetch
          (let ((initial-fetch (seq-find (lambda (call)
                                           (and (= (plist-get call :time) 5)
                                                (null (plist-get call :repeat))))
                                         scheduled-calls)))
            (should initial-fetch)
            (should (eq (plist-get initial-fetch :func)
                        #'wttrin--mode-line-fetch-weather)))))
    (test-wttrin--mode-line-start-teardown)))

(ert-deftest test-wttrin--mode-line-start-normal-creates-repeating-mode-line-timer ()
  "A repeating timer should be created for periodic mode-line refresh."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-mode-line-refresh-interval 3600)
            (scheduled-calls nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (time repeat func)
                     (push (list :time time :repeat repeat :func func) scheduled-calls)
                     (list 'mock-timer))))
          (wttrin--mode-line-start)
          ;; Should have a repeating timer for mode-line fetch
          (let ((repeating-fetch (seq-find (lambda (call)
                                             (and (equal (plist-get call :repeat) 3600)
                                                  (eq (plist-get call :func)
                                                      #'wttrin--mode-line-fetch-weather)))
                                           scheduled-calls)))
            (should repeating-fetch))))
    (test-wttrin--mode-line-start-teardown)))

(ert-deftest test-wttrin--mode-line-start-normal-creates-buffer-refresh-timer ()
  "A repeating timer should be created for periodic buffer cache refresh."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (wttrin-refresh-interval 3600)
            (scheduled-calls nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (time repeat func)
                     (push (list :time time :repeat repeat :func func) scheduled-calls)
                     (list 'mock-timer))))
          (wttrin--mode-line-start)
          ;; Should have a repeating timer for buffer cache refresh
          (let ((buffer-refresh (seq-find (lambda (call)
                                            (and (equal (plist-get call :repeat) 3600)
                                                 (eq (plist-get call :func)
                                                     #'wttrin--buffer-cache-refresh)))
                                          scheduled-calls)))
            (should buffer-refresh))))
    (test-wttrin--mode-line-start-teardown)))

;;; Boundary Cases

(ert-deftest test-wttrin--mode-line-start-boundary-nil-location-skips-setup ()
  "When no favorite location is set, no timers or placeholder should be created."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location nil)
            (run-at-time-called nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat _func)
                     (setq run-at-time-called t)
                     (list 'mock-timer))))
          (wttrin--mode-line-start)
          (should-not run-at-time-called)
          (should-not wttrin-mode-line-string)
          (should-not wttrin--mode-line-timer)))
    (test-wttrin--mode-line-start-teardown)))

(ert-deftest test-wttrin--mode-line-start-boundary-cancels-existing-timers ()
  "Starting when timers already exist should cancel old timers before creating new ones."
  (test-wttrin--mode-line-start-setup)
  (unwind-protect
      (let ((wttrin-favorite-location "Paris")
            (cancelled-timers nil))
        ;; Create fake existing timers
        (setq wttrin--mode-line-timer (run-at-time 99999 nil #'ignore))
        (setq wttrin--buffer-refresh-timer (run-at-time 99999 nil #'ignore))
        (let ((old-ml-timer wttrin--mode-line-timer)
              (old-buf-timer wttrin--buffer-refresh-timer))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat _func) (list 'mock-timer))))
            (wttrin--mode-line-start)
            ;; Old timers should have been replaced
            (should-not (eq wttrin--mode-line-timer old-ml-timer))
            (should-not (eq wttrin--buffer-refresh-timer old-buf-timer)))))
    (test-wttrin--mode-line-start-teardown)))

(provide 'test-wttrin--mode-line-start)
;;; test-wttrin--mode-line-start.el ends here
