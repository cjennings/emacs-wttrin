;;; coverage-summary.el --- Whole-project coverage summary from a SimpleCov report -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch helper for `make coverage-summary'.  After `make coverage' writes an
;; undercover SimpleCov JSON report, this prints a per-file table, a project
;; number, and the source files present on disk but absent from the report.
;;
;; The value here is the missing-file detection: a module no test imports never
;; appears in the SimpleCov output, so it silently fails to drag the number
;; down.  This script counts such a file as 0% and weights the project number
;; by file rather than by line, so untested modules are visible.
;;
;; Self-contained on purpose — lives in the project's tracked =scripts/= (so CI
;; can reach it) and must run with nothing but stock Emacs (`json' is built in).
;; The SimpleCov
;; JSON shape it parses is:
;;   { <suite>: { "coverage": { <abs-path>: [null | 0 | int, ...] } } }
;; where a null entry is a non-executable line, 0 is executable-but-unhit, and
;; any positive integer is a hit.  Data unions across multiple suite keys.
;;
;; CLI contract (mirrors the dotemacs original):
;;   emacs --batch -l coverage-summary.el \
;;     --eval '(cj/coverage-print-module-summary REPORT SRC-DIR PROJECT-ROOT)'

;;; Code:

(require 'json)
(require 'seq)

(defun cj/coverage-summary--parse-file (report-file)
  "Parse REPORT-FILE (SimpleCov JSON) into per-file (COVERED . TOTAL) counts.

Keys are absolute source-file paths.  TOTAL counts executable lines (numeric
entries); COVERED counts hit lines (entries greater than zero).  Data unions
across every top-level suite key.  Signals `user-error' when REPORT-FILE is
missing or malformed."
  (unless (file-exists-p report-file)
    (user-error "Coverage report not found: %s" report-file))
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (data (condition-case err
                   (json-read-file report-file)
                 (error (user-error "Malformed coverage JSON in %s: %s"
                                    report-file (error-message-string err)))))
         ;; path -> (covered-set . total-set), line numbers held in hash sets so
         ;; unioning across suites never double-counts a shared line.
         (acc (make-hash-table :test 'equal)))
    (maphash
     (lambda (_suite section)
       (when (hash-table-p section)
         (let ((coverage (gethash "coverage" section)))
           (when (hash-table-p coverage)
             (maphash
              (lambda (path hits-list)
                (let* ((cell (or (gethash path acc)
                                 (puthash path
                                          (cons (make-hash-table :test 'eql)
                                                (make-hash-table :test 'eql))
                                          acc)))
                       (covered (car cell))
                       (total (cdr cell))
                       (line 1))
                  (dolist (hits hits-list)
                    (when (numberp hits)
                      (puthash line t total)
                      (when (> hits 0) (puthash line t covered)))
                    (setq line (1+ line)))))
              coverage)))))
     data)
    (let ((result (make-hash-table :test 'equal)))
      (maphash (lambda (path cell)
                 (puthash path
                          (cons (hash-table-count (car cell))
                                (hash-table-count (cdr cell)))
                          result))
               acc)
      result)))

(defun cj/coverage-summary--under-dir (table source-dir project-root)
  "Filter TABLE to files under SOURCE-DIR, re-keyed relative to PROJECT-ROOT."
  (let ((result (make-hash-table :test 'equal))
        (source-dir (file-name-as-directory (expand-file-name source-dir)))
        (project-root (file-name-as-directory (expand-file-name project-root))))
    (maphash
     (lambda (path counts)
       (let ((abs (expand-file-name path)))
         (when (string-prefix-p source-dir abs)
           (puthash (file-relative-name abs project-root) counts result))))
     table)
    result))

(defun cj/coverage-summary--source-files (source-dir project-root)
  "Return *.el files directly under SOURCE-DIR, relative to PROJECT-ROOT.
Sorted.  Compiled files and subdirectories are out of scope, as are generated
package files (`*-autoloads.el', `*-pkg.el') -- a build tool writes those, no
test covers them, and counting them as untested source skews the number."
  (let ((source-dir (file-name-as-directory (expand-file-name source-dir)))
        (project-root (file-name-as-directory (expand-file-name project-root))))
    (sort (seq-remove
           (lambda (p) (string-match-p "\\(?:-autoloads\\|-pkg\\)\\.el\\'" p))
           (mapcar (lambda (p) (file-relative-name p project-root))
                   (directory-files source-dir t "\\.el\\'")))
          #'string<)))

(defun cj/coverage-summary--missing (tracked source-dir project-root)
  "Return source files present on disk but absent from TRACKED.
TRACKED is a list of project-relative paths (the report's keys under
SOURCE-DIR).  The difference is the set of files no test exercised."
  (seq-difference
   (cj/coverage-summary--source-files source-dir project-root)
   tracked
   #'string=))

(defun cj/coverage-summary--file-pct (covered total)
  "Return COVERED/TOTAL as a percentage.
A file with no executable lines (TOTAL 0) is 100% — nothing left uncovered."
  (if (> total 0) (/ (* 100.0 covered) total) 100.0))

(defun cj/coverage-summary--project-pct (report-file source-dir project-root)
  "Return the unit-weighted project coverage percentage.
Every tracked file contributes its own percentage; every source file missing
from REPORT-FILE contributes 0%.  The result is the mean over all files under
SOURCE-DIR, so an untested module drags the number down instead of vanishing."
  (let* ((tracked (cj/coverage-summary--under-dir
                   (cj/coverage-summary--parse-file report-file)
                   source-dir project-root))
         (keys (let (ks) (maphash (lambda (k _v) (push k ks)) tracked) ks))
         (missing (cj/coverage-summary--missing keys source-dir project-root))
         (score 0.0)
         (total-count (+ (hash-table-count tracked) (length missing))))
    (maphash (lambda (_k counts)
               (setq score (+ score (cj/coverage-summary--file-pct
                                     (car counts) (cdr counts)))))
             tracked)
    (if (> total-count 0) (/ score total-count) 0.0)))

(defun cj/coverage-summary-text (report-file source-dir project-root)
  "Return a whole-project coverage summary for SOURCE-DIR from REPORT-FILE."
  (let* ((tracked (cj/coverage-summary--under-dir
                   (cj/coverage-summary--parse-file report-file)
                   source-dir project-root))
         (rel-src (file-relative-name
                   (expand-file-name source-dir)
                   (file-name-as-directory (expand-file-name project-root))))
         (keys (let (ks) (maphash (lambda (k _v) (push k ks)) tracked) ks))
         (missing (cj/coverage-summary--missing keys source-dir project-root))
         (pct (cj/coverage-summary--project-pct report-file source-dir project-root)))
    (with-temp-buffer
      (insert (format "Coverage summary for %s\n\n" rel-src))
      (dolist (path (sort keys #'string<))
        (let* ((counts (gethash path tracked))
               (covered (car counts))
               (total (cdr counts)))
          (insert (format "  %6.1f%%  %s  (%d/%d lines)\n"
                          (cj/coverage-summary--file-pct covered total)
                          path covered total))))
      (insert (format "\nProject coverage: %.1f%% (%d tracked, %d missing, %d total; missing files count as 0%%)\n"
                      pct (hash-table-count tracked) (length missing)
                      (+ (hash-table-count tracked) (length missing))))
      (insert (format "\nNot in coverage report: %d file%s\n"
                      (length missing) (if (= 1 (length missing)) "" "s")))
      (if missing
          (progn
            (insert "These files had no coverage entry; they count as 0% in project coverage.\n")
            (dolist (path (sort missing #'string<))
              (insert (format "  %s\n" path))))
        (insert "Every source file appears in the coverage report.\n"))
      (buffer-string))))

(defun cj/coverage-print-module-summary (report-file source-dir project-root)
  "Print a whole-project coverage summary for SOURCE-DIR from REPORT-FILE."
  (princ "\n")
  (princ (cj/coverage-summary-text report-file source-dir project-root)))

(provide 'coverage-summary)
;;; coverage-summary.el ends here
