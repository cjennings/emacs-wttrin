;;; test-wttrin--mode-line-forecast.el --- Tests for the tooltip forecast -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Craig Jennings

;;; Commentary:

;; Unit tests for the mode-line tooltip forecast: parsing wttr.in's j1 JSON
;; into a day list, formatting up to `wttrin-mode-line-tooltip-forecast-days'
;; lines, and appending the block to `wttrin--mode-line-tooltip' output.

;;; Code:

(require 'ert)
(require 'wttrin)
(require 'testutil-wttrin)

;;; Fixture

(defconst test-wttrin--forecast-j1
  "{\"weather\": [
     {\"date\": \"2026-07-01\", \"mintempC\": \"22\", \"maxtempC\": \"31\",
      \"mintempF\": \"71\", \"maxtempF\": \"88\",
      \"hourly\": [{\"time\": \"0\", \"weatherDesc\": [{\"value\": \"Clear\"}]},
                   {\"time\": \"1200\", \"weatherDesc\": [{\"value\": \"Partly cloudy\"}]},
                   {\"time\": \"2100\", \"weatherDesc\": [{\"value\": \"Clear\"}]}]},
     {\"date\": \"2026-07-02\", \"mintempC\": \"20\", \"maxtempC\": \"26\",
      \"mintempF\": \"68\", \"maxtempF\": \"79\",
      \"hourly\": [{\"time\": \"1200\", \"weatherDesc\": [{\"value\": \"Light rain\"}]}]},
     {\"date\": \"2026-07-03\", \"mintempC\": \"21\", \"maxtempC\": \"31\",
      \"mintempF\": \"70\", \"maxtempF\": \"88\",
      \"hourly\": [{\"time\": \"1200\", \"weatherDesc\": [{\"value\": \"Sunny\"}]}]}]}"
  "Trimmed wttr.in ?format=j1 response: three days, the keys the code reads.")

(defun test-wttrin--forecast-days ()
  "Return the parsed day list from the fixture."
  (wttrin--forecast-parse test-wttrin--forecast-j1))

;;; --------------------------------------------------------------------------
;;; wttrin--forecast-parse
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--forecast-parse-normal-three-days ()
  "Normal: the fixture parses into a list of three day alists."
  (let ((days (test-wttrin--forecast-days)))
    (should (= 3 (length days)))
    (should (equal "2026-07-01" (alist-get 'date (car days))))))

(ert-deftest test-wttrin--forecast-parse-error-malformed-json ()
  "Error: malformed JSON returns nil instead of signaling."
  (should-not (wttrin--forecast-parse "{not json")))

(ert-deftest test-wttrin--forecast-parse-error-missing-weather-key ()
  "Error: JSON without a weather array returns nil."
  (should-not (wttrin--forecast-parse "{\"nearest_area\": []}")))

(ert-deftest test-wttrin--forecast-parse-boundary-nil-and-empty ()
  "Boundary: nil and empty-string input return nil."
  (should-not (wttrin--forecast-parse nil))
  (should-not (wttrin--forecast-parse "")))

;;; --------------------------------------------------------------------------
;;; wttrin--forecast-format
;;; --------------------------------------------------------------------------

(ert-deftest test-wttrin--forecast-format-normal-three-days-fahrenheit ()
  "Normal: unit system \"u\" renders three labeled lines with °F ranges."
  (let* ((wttrin-unit-system "u")
         (text (wttrin--forecast-format (test-wttrin--forecast-days) 3))
         (lines (split-string text "\n")))
    (should (= 3 (length lines)))
    (should (string-match-p "\\`Today 71-88°F Partly cloudy\\'" (nth 0 lines)))
    (should (string-match-p "\\`Tomorrow 68-79°F Light rain\\'" (nth 1 lines)))
    ;; 2026-07-03 is a Friday; the third day is labeled by weekday.
    (should (string-match-p "\\`Fri 70-88°F Sunny\\'" (nth 2 lines)))))

(ert-deftest test-wttrin--forecast-format-normal-metric ()
  "Normal: a non-\"u\" unit system renders °C ranges."
  (let* ((wttrin-unit-system "m")
         (text (wttrin--forecast-format (test-wttrin--forecast-days) 2)))
    (should (string-match-p "Today 22-31°C" text))
    (should (string-match-p "Tomorrow 20-26°C" text))))

(ert-deftest test-wttrin--forecast-format-boundary-day-counts ()
  "Boundary: 0 days is nil; 1 day is one line; >3 caps at the data."
  (let ((wttrin-unit-system "u")
        (days (test-wttrin--forecast-days)))
    (should-not (wttrin--forecast-format days 0))
    (should (= 1 (length (split-string (wttrin--forecast-format days 1) "\n"))))
    (should (= 3 (length (split-string (wttrin--forecast-format days 5) "\n"))))))

(ert-deftest test-wttrin--forecast-format-boundary-missing-hourly-desc ()
  "Boundary: a day with no hourly data renders the temps without a description."
  (let* ((wttrin-unit-system "u")
         (day '((date . "2026-07-01") (mintempF . "71") (maxtempF . "88")
                (mintempC . "22") (maxtempC . "31") (hourly . ())))
         (text (wttrin--forecast-format (list day) 1)))
    (should (equal "Today 71-88°F" text))))

(ert-deftest test-wttrin--forecast-format-error-nil-days ()
  "Error: nil day list returns nil."
  (should-not (wttrin--forecast-format nil 3)))

;;; --------------------------------------------------------------------------
;;; tooltip integration
;;; --------------------------------------------------------------------------

(defun test-wttrin--forecast-tooltip-fixture (option-days)
  "Return the tooltip with fresh current + forecast caches and OPTION-DAYS."
  (let ((wttrin-unit-system "u")
        (wttrin-mode-line-tooltip-forecast-days option-days)
        (wttrin--mode-line-cache (cons (float-time) "Paris: ☀️ +61°F Clear"))
        (wttrin--mode-line-forecast-cache
         (cons (float-time) (test-wttrin--forecast-days))))
    (wttrin--mode-line-tooltip)))

(ert-deftest test-wttrin--forecast-tooltip-normal-appends-block ()
  "Normal: with the option at 3, the tooltip carries the forecast lines."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((tooltip (test-wttrin--forecast-tooltip-fixture 3)))
        (should (string-match-p "Paris" tooltip))
        (should (string-match-p "Today 71-88°F Partly cloudy" tooltip))
        (should (string-match-p "Fri 70-88°F Sunny" tooltip))
        (should (string-match-p "Updated" tooltip)))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin--forecast-tooltip-boundary-option-zero-unchanged ()
  "Boundary: with the option at 0 (default), the tooltip has no forecast."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((tooltip (test-wttrin--forecast-tooltip-fixture 0)))
        (should (string-match-p "Paris" tooltip))
        (should-not (string-match-p "Today" tooltip)))
    (testutil-wttrin-teardown)))

(ert-deftest test-wttrin--forecast-tooltip-boundary-empty-forecast-cache ()
  "Boundary: option on but no forecast cache yet leaves the tooltip as before."
  (testutil-wttrin-setup)
  (unwind-protect
      (let ((wttrin-mode-line-tooltip-forecast-days 3)
            (wttrin--mode-line-cache (cons (float-time) "Paris: ☀️ +61°F Clear"))
            (wttrin--mode-line-forecast-cache nil))
        (let ((tooltip (wttrin--mode-line-tooltip)))
          (should (string-match-p "Paris" tooltip))
          (should-not (string-match-p "Today" tooltip))))
    (testutil-wttrin-teardown)))

(provide 'test-wttrin--mode-line-forecast)
;;; test-wttrin--mode-line-forecast.el ends here
