;;; test-init.el --- Test fixture for wttrin integration tests -*- lexical-binding: t; -*-

;; This is a minimal init.el for testing wttrin with debug enabled

;; Enable debug mode BEFORE loading wttrin
(setq wttrin-debug t)

;; Configure wttrin
(setq wttrin-default-locations '("Berkeley, CA" "New Orleans, LA"))
(setq wttrin-unit-system "m")  ; Metric
(setq wttrin-favorite-location "Berkeley, CA")
(setq wttrin-mode-line-startup-delay 0)  ; No delay for tests
(setq wttrin-mode-line-refresh-interval 3600)  ; 1 hour

;; Load wttrin (assumes it's in load-path)
(require 'wttrin)

;; Don't auto-enable mode-line in tests (we'll do it explicitly)
(setq wttrin-mode-line-auto-enable nil)

(provide 'test-init)
;;; test-init.el ends here
