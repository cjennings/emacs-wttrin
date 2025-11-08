;;; test-wttrin--add-buffer-instructions.el --- Tests for wttrin--add-buffer-instructions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;;; Commentary:
;; Unit tests for wttrin--add-buffer-instructions function.
;; Tests adding user instructions to buffer content.

;;; Code:

(require 'ert)
(require 'wttrin)

;;; Normal Cases

(ert-deftest test-wttrin--add-buffer-instructions-normal-empty-buffer ()
  "Test adding instructions to empty buffer."
  (with-temp-buffer
    (wttrin--add-buffer-instructions)
    (should (string= "\n\nPress: [a] for another location [g] to refresh [q] to quit"
                     (buffer-string)))))

(ert-deftest test-wttrin--add-buffer-instructions-normal-with-existing-content ()
  "Test adding instructions to buffer with existing content."
  (with-temp-buffer
    (insert "Weather: Sunny\nTemperature: 20°C")
    (wttrin--add-buffer-instructions)
    (should (string= "Weather: Sunny\nTemperature: 20°C\n\nPress: [a] for another location [g] to refresh [q] to quit"
                     (buffer-string)))))

(ert-deftest test-wttrin--add-buffer-instructions-normal-preserves-point ()
  "Test that point is moved to end after adding instructions."
  (with-temp-buffer
    (insert "Some content")
    (goto-char (point-min))
    (wttrin--add-buffer-instructions)
    (should (= (point) (point-max)))))

(ert-deftest test-wttrin--add-buffer-instructions-normal-idempotent-check ()
  "Test that calling function twice adds instructions twice (not idempotent)."
  (with-temp-buffer
    (insert "Weather")
    (wttrin--add-buffer-instructions)
    (let ((first-result (buffer-string)))
      (wttrin--add-buffer-instructions)
      ;; Should add instructions again, not check if they already exist
      (should-not (string= first-result (buffer-string)))
      ;; Check for two occurrences of "Press:" by counting matches
      (let ((count 0))
        (with-temp-buffer
          (insert first-result)
          (goto-char (point-min))
          (while (search-forward "Press:" nil t)
            (setq count (1+ count))))
        ;; After first call, should have 1 occurrence
        (should (= 1 count)))
      ;; After second call, check for 2 occurrences
      (let ((count 0))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "Press:" nil t)
            (setq count (1+ count))))
        (should (= 2 count))))))

;;; Boundary Cases

(ert-deftest test-wttrin--add-buffer-instructions-boundary-point-at-beginning ()
  "Test adding instructions when point is at beginning of buffer."
  (with-temp-buffer
    (insert "Weather data here")
    (goto-char (point-min))
    (wttrin--add-buffer-instructions)
    (should (string-suffix-p "Press: [a] for another location [g] to refresh [q] to quit"
                             (buffer-string)))))

(ert-deftest test-wttrin--add-buffer-instructions-boundary-point-in-middle ()
  "Test adding instructions when point is in middle of buffer."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char (point-min))
    (forward-line 1)
    (wttrin--add-buffer-instructions)
    (should (string-suffix-p "Press: [a] for another location [g] to refresh [q] to quit"
                             (buffer-string)))))

(ert-deftest test-wttrin--add-buffer-instructions-boundary-buffer-with-trailing-newlines ()
  "Test adding instructions to buffer that already ends with newlines."
  (with-temp-buffer
    (insert "Weather\n\n\n")
    (wttrin--add-buffer-instructions)
    (should (string= "Weather\n\n\n\n\nPress: [a] for another location [g] to refresh [q] to quit"
                     (buffer-string)))))

(ert-deftest test-wttrin--add-buffer-instructions-boundary-very-large-buffer ()
  "Test adding instructions to large buffer."
  (with-temp-buffer
    (insert (make-string 10000 ?x))
    (wttrin--add-buffer-instructions)
    (goto-char (point-max))
    (should (looking-back "Press: \\[a\\] for another location \\[g\\] to refresh \\[q\\] to quit" nil))))

;;; Error Cases

(ert-deftest test-wttrin--add-buffer-instructions-error-read-only-buffer ()
  "Test that function signals error when buffer is read-only."
  (with-temp-buffer
    (insert "Some content")
    (read-only-mode 1)
    (should-error (wttrin--add-buffer-instructions)
                  :type 'buffer-read-only)))

(ert-deftest test-wttrin--add-buffer-instructions-error-narrowed-buffer ()
  "Test adding instructions to narrowed buffer adds at narrowed end."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4")
    (goto-char (point-min))
    (forward-line 1)
    (let ((start (point)))
      (forward-line 1)
      (narrow-to-region start (point))
      (wttrin--add-buffer-instructions)
      ;; Instructions should be added at end of narrowed region
      (widen)
      (goto-char start)
      (forward-line 1)
      (should (looking-at-p "\n\nPress:")))))

(provide 'test-wttrin--add-buffer-instructions)
;;; test-wttrin--add-buffer-instructions.el ends here
