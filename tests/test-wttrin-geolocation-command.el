;;; test-wttrin-geolocation-command.el --- Tests for the external-command geolocation provider -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:
;; Unit tests for the generic external-command geolocation provider:
;; `wttrin-geolocation--parse-coordinates' (pure JSON -> "LAT,LNG"), the
;; routing/fallback in `wttrin-geolocation-detect' when
;; `wttrin-geolocation-command' is set, and an end-to-end run of
;; `wttrin-geolocation--detect-via-command' against a real shell command.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wttrin)
(require 'wttrin-geolocation)

;;; wttrin-geolocation--parse-coordinates

(ert-deftest test-wttrin-geolocation-command-normal-parse-coordinates ()
  "Normal: numeric lat/lng become a \"LAT,LNG\" string."
  (should (equal "41.32,-71.81"
                 (wttrin-geolocation--parse-coordinates
                  "{\"lat\": 41.32, \"lng\": -71.81}"))))

(ert-deftest test-wttrin-geolocation-command-boundary-parse-ignores-extra-keys ()
  "Boundary: extra keys (accuracy, address) are ignored."
  (should (equal "1.5,2.5"
                 (wttrin-geolocation--parse-coordinates
                  "{\"lat\":1.5,\"lng\":2.5,\"accuracy_m\":11.5,\"address\":\"x\"}"))))

(ert-deftest test-wttrin-geolocation-command-error-parse-malformed-json ()
  "Error: malformed JSON returns nil."
  (should (null (wttrin-geolocation--parse-coordinates "not json")))
  (should (null (wttrin-geolocation--parse-coordinates "")))
  (should (null (wttrin-geolocation--parse-coordinates nil))))

(ert-deftest test-wttrin-geolocation-command-error-parse-missing-or-nonnumeric ()
  "Error: missing or non-numeric coordinates return nil."
  (should (null (wttrin-geolocation--parse-coordinates "{\"lat\":1.0}")))
  (should (null (wttrin-geolocation--parse-coordinates
                 "{\"lat\":\"x\",\"lng\":\"y\"}"))))

;;; wttrin-geolocation--parse-address

(ert-deftest test-wttrin-geolocation-command-normal-parse-address ()
  "Normal: the address key is returned verbatim."
  (should (equal "Westerly, Rhode Island, USA"
                 (wttrin-geolocation--parse-address
                  "{\"lat\":1.0,\"lng\":2.0,\"address\":\"Westerly, Rhode Island, USA\"}"))))

(ert-deftest test-wttrin-geolocation-command-boundary-parse-address-label-synonym ()
  "Boundary: a `label' key is accepted when `address' is absent."
  (should (equal "Westerly, RI"
                 (wttrin-geolocation--parse-address
                  "{\"lat\":1.0,\"lng\":2.0,\"label\":\"Westerly, RI\"}"))))

(ert-deftest test-wttrin-geolocation-command-error-parse-address-absent ()
  "Error: no address or label, malformed JSON, or empty string returns nil."
  (should (null (wttrin-geolocation--parse-address "{\"lat\":1.0,\"lng\":2.0}")))
  (should (null (wttrin-geolocation--parse-address "not json")))
  (should (null (wttrin-geolocation--parse-address
                 "{\"lat\":1.0,\"lng\":2.0,\"address\":\"\"}"))))

;;; wttrin-geolocation-detect — routing and fallback

(ert-deftest test-wttrin-geolocation-command-normal-detect-uses-command ()
  "Normal: with a command set that succeeds, its coordinates are used."
  (let ((got 'none)
        (ip-called nil)
        (wttrin-geolocation-command "ignored-in-mock"))
    (cl-letf (((symbol-function 'wttrin-geolocation--detect-via-command)
               (lambda (cb) (funcall cb "1.0,2.0")))
              ((symbol-function 'wttrin-geolocation--detect-via-ip)
               (lambda (cb) (setq ip-called t) (funcall cb "City, ST"))))
      (wttrin-geolocation-detect (lambda (r &optional _a) (setq got r))))
    (should (equal "1.0,2.0" got))
    (should-not ip-called)))

(ert-deftest test-wttrin-geolocation-command-boundary-detect-falls-back-to-ip ()
  "Boundary: with a command that fails (nil), detection falls back to IP."
  (let ((got 'none)
        (wttrin-geolocation-command "ignored-in-mock"))
    (cl-letf (((symbol-function 'wttrin-geolocation--detect-via-command)
               (lambda (cb) (funcall cb nil)))
              ((symbol-function 'wttrin-geolocation--detect-via-ip)
               (lambda (cb) (funcall cb "City, ST"))))
      (wttrin-geolocation-detect (lambda (r &optional _a) (setq got r))))
    (should (equal "City, ST" got))))

(ert-deftest test-wttrin-geolocation-command-boundary-detect-no-command-uses-ip ()
  "Boundary: with no command set, the IP path runs directly."
  (let ((got 'none)
        (cmd-called nil)
        (wttrin-geolocation-command nil))
    (cl-letf (((symbol-function 'wttrin-geolocation--detect-via-command)
               (lambda (cb) (setq cmd-called t) (funcall cb "9.0,9.0")))
              ((symbol-function 'wttrin-geolocation--detect-via-ip)
               (lambda (cb) (funcall cb "City, ST"))))
      (wttrin-geolocation-detect (lambda (r &optional _a) (setq got r))))
    (should (equal "City, ST" got))
    (should-not cmd-called)))

;;; wttrin-geolocation--detect-via-command — real process

(defun test-wttrin-geolocation-command--run-sync (command)
  "Run `wttrin-geolocation--detect-via-command' with COMMAND, wait, return coords.
The address (second callback argument) is ignored here; a separate test covers it."
  (let ((result 'pending)
        (wttrin-geolocation-command command))
    (wttrin-geolocation--detect-via-command
     (lambda (r &optional _a) (setq result r)))
    (with-timeout (5 (error "detect-via-command timed out"))
      (while (eq result 'pending)
        (accept-process-output nil 0.05)))
    result))

(ert-deftest test-wttrin-geolocation-command-integration-real-command-success ()
  "Integration: a command printing lat/lng JSON resolves to \"LAT,LNG\".
Components: wttrin-geolocation--detect-via-command (real make-process),
the shell (real), wttrin-geolocation--parse-coordinates (real)."
  (should (equal "1.5,2.5"
                 (test-wttrin-geolocation-command--run-sync
                  "echo '{\"lat\":1.5,\"lng\":2.5}'"))))

(ert-deftest test-wttrin-geolocation-command-integration-real-command-passes-address ()
  "Integration: a command printing lat/lng plus an address passes both to the callback.
Components: detect-via-command (real make-process), the shell, the coord and
address parsers (real)."
  (let ((coords 'pending)
        (address 'pending)
        (wttrin-geolocation-command
         "echo '{\"lat\":1.5,\"lng\":2.5,\"address\":\"Westerly, RI\"}'"))
    (wttrin-geolocation--detect-via-command
     (lambda (c &optional a) (setq coords c address a)))
    (with-timeout (5 (error "detect-via-command timed out"))
      (while (eq coords 'pending)
        (accept-process-output nil 0.05)))
    (should (equal "1.5,2.5" coords))
    (should (equal "Westerly, RI" address))))

(ert-deftest test-wttrin-geolocation-command-integration-real-command-nonzero-exit ()
  "Integration: a command exiting non-zero yields nil (caller falls back to IP)."
  (should (null (test-wttrin-geolocation-command--run-sync "exit 1"))))

(ert-deftest test-wttrin-geolocation-command-integration-real-command-bad-output ()
  "Integration: a zero-exit command printing non-JSON yields nil."
  (should (null (test-wttrin-geolocation-command--run-sync "echo not-json"))))

(ert-deftest test-wttrin-geolocation-command-error-spawn-failure-yields-nil ()
  "Error: when the process cannot spawn, the callback receives nil."
  (let ((result 'pending)
        (wttrin-geolocation-command "whatever"))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "spawn failed"))))
      (wttrin-geolocation--detect-via-command (lambda (r) (setq result r))))
    (should (null result))))

(provide 'test-wttrin-geolocation-command)
;;; test-wttrin-geolocation-command.el ends here
