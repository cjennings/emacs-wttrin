# Makefile for wttrin
#
# Usage:
#   make help              - Show this help message
#   make test              - Run all tests
#   make test-unit         - Run unit tests only
#   make test-file FILE=test-foo.el  - Run specific test file
#   make test-name TEST=test-foo-*   - Run tests matching pattern
#   make validate-parens   - Check for unbalanced parentheses
#   make validate          - Load wttrin.el to verify it compiles
#   make compile           - Byte-compile wttrin.el
#   make lint              - Run all linters (checkdoc, package-lint, elisp-lint)
#   make clean             - Remove test artifacts and compiled files
#   make clean-compiled    - Remove .elc/.eln files only
#   make clean-tests       - Remove test artifacts only

# Emacs binary to use (override with: make EMACS=emacs29 test)
EMACS ?= emacs

# Directories
TEST_DIR = tests
PROJECT_ROOT = .

# Test files
UNIT_TESTS = $(filter-out $(TEST_DIR)/test-integration-%.el, $(wildcard $(TEST_DIR)/test-*.el))
INTEGRATION_TESTS = $(wildcard $(TEST_DIR)/test-integration-%.el)
ALL_TESTS = $(UNIT_TESTS) $(INTEGRATION_TESTS)

# Source files
MAIN_FILE = wttrin.el
TEST_UTIL_FILES = $(wildcard $(TEST_DIR)/testutil-*.el)

# Emacs batch flags
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp
EMACS_TEST = $(EMACS_BATCH) -L $(PROJECT_ROOT) -L $(TEST_DIR)

# Colors for output
COLOR_GREEN = \033[0;32m
COLOR_RED = \033[0;31m
COLOR_BLUE = \033[0;34m
COLOR_YELLOW = \033[0;33m
COLOR_RESET = \033[0m

.PHONY: help test test-all test-unit test-integration test-file test-name \
        validate-parens validate compile lint \
        clean clean-compiled clean-tests

# Default target
.DEFAULT_GOAL := help

help:
	@echo "wttrin Makefile Targets:"
	@echo ""
	@echo "  $(COLOR_BLUE)Testing:$(COLOR_RESET)"
	@echo "    make test              - Run all tests ($(words $(ALL_TESTS)) files)"
	@echo "    make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "    make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "    make test-file FILE=<filename>  - Run specific test file"
	@echo "    make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo ""
	@echo "  $(COLOR_BLUE)Validation:$(COLOR_RESET)"
	@echo "    make validate-parens   - Check for unbalanced parentheses"
	@echo "    make validate          - Load wttrin.el to verify it compiles"
	@echo "    make compile           - Byte-compile wttrin.el"
	@echo "    make lint              - Run all linters (checkdoc, package-lint, elisp-lint)"
	@echo ""
	@echo "  $(COLOR_BLUE)Utilities:$(COLOR_RESET)"
	@echo "    make clean             - Remove test artifacts and compiled files"
	@echo "    make clean-compiled    - Remove .elc/.eln files only"
	@echo "    make clean-tests       - Remove test artifacts only"
	@echo ""
	@echo "Examples:"
	@echo "  make test-file FILE=test-wttrin--build-url.el"
	@echo "  make test-name TEST=test-wttrin--build-url-*"
	@echo "  make EMACS=emacs29 test   # Use specific Emacs version"

# ============================================================================
# Testing Targets
# ============================================================================

test: test-all

test-all:
	@echo "$(COLOR_BLUE)Running all tests ($(words $(ALL_TESTS)) files)...$(COLOR_RESET)"
	@$(MAKE) test-unit
	@if [ $(words $(INTEGRATION_TESTS)) -gt 0 ]; then \
		$(MAKE) test-integration; \
	fi
	@echo "$(COLOR_GREEN)✓ All tests complete$(COLOR_RESET)"

test-unit:
	@echo "$(COLOR_BLUE)Running unit tests ($(words $(UNIT_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All unit tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed unit test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

test-integration:
	@if [ $(words $(INTEGRATION_TESTS)) -eq 0 ]; then \
		echo "$(COLOR_YELLOW)No integration tests found$(COLOR_RESET)"; \
		exit 0; \
	fi
	@echo "$(COLOR_BLUE)Running integration tests ($(words $(INTEGRATION_TESTS)) files)...$(COLOR_RESET)"
	@failed=0; \
	for test in $(INTEGRATION_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(COLOR_GREEN)✓ All integration tests passed$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_RED)✗ $$failed integration test file(s) failed$(COLOR_RESET)"; \
		exit 1; \
	fi

test-file:
ifndef FILE
	@echo "$(COLOR_RED)Error: FILE parameter required$(COLOR_RESET)"
	@echo "Usage: make test-file FILE=test-wttrin--build-url.el"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests in $(FILE)...$(COLOR_RESET)"
	@$(EMACS_TEST) -l ert -l $(TEST_DIR)/$(FILE) -f ert-run-tests-batch-and-exit
	@echo "$(COLOR_GREEN)✓ Tests in $(FILE) complete$(COLOR_RESET)"

test-name:
ifndef TEST
	@echo "$(COLOR_RED)Error: TEST parameter required$(COLOR_RESET)"
	@echo "Usage: make test-name TEST=test-wttrin--build-url-*"
	@exit 1
endif
	@echo "$(COLOR_BLUE)Running tests matching pattern: $(TEST)...$(COLOR_RESET)"
	@$(EMACS_TEST) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(test)) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "$(COLOR_GREEN)✓ Tests matching '$(TEST)' complete$(COLOR_RESET)"

# ============================================================================
# Validation Targets
# ============================================================================

validate-parens:
	@echo "$(COLOR_BLUE)Checking for unbalanced parentheses...$(COLOR_RESET)"
	@echo "  Checking $(MAIN_FILE)..."; \
	$(EMACS_BATCH) --eval "(condition-case err \
		(progn \
			(find-file \"$(MAIN_FILE)\") \
			(check-parens) \
			(kill-emacs 0)) \
		(error (progn \
			(message \"ERROR: %s\" err) \
			(kill-emacs 1))))" 2>&1 > /dev/null && \
		echo "$(COLOR_GREEN)✓ $(MAIN_FILE) has balanced parentheses$(COLOR_RESET)" || \
		(echo "$(COLOR_RED)✗ $(MAIN_FILE) has unbalanced parentheses$(COLOR_RESET)" && exit 1)

validate:
	@echo "$(COLOR_BLUE)Loading wttrin.el to verify compilation...$(COLOR_RESET)"
	@$(EMACS_BATCH) -L $(PROJECT_ROOT) \
		--eval "(condition-case err \
			(progn \
				(load-file \"$(MAIN_FILE)\") \
				(message \"OK: %s\" \"$(MAIN_FILE)\")) \
			(error (progn \
				(message \"ERROR loading %s: %s\" \"$(MAIN_FILE)\" err) \
				(kill-emacs 1))))" && \
		echo "$(COLOR_GREEN)✓ $(MAIN_FILE) loaded successfully$(COLOR_RESET)" || \
		(echo "$(COLOR_RED)✗ $(MAIN_FILE) failed to load$(COLOR_RESET)" && exit 1)

compile:
	@echo "$(COLOR_BLUE)Byte-compiling wttrin.el...$(COLOR_RESET)"
	@$(EMACS_BATCH) -L $(PROJECT_ROOT) \
		--eval "(progn \
			(setq byte-compile-error-on-warn nil) \
			(batch-byte-compile))" $(MAIN_FILE)
	@echo "$(COLOR_GREEN)✓ Compilation complete$(COLOR_RESET)"

lint:
	@echo "$(COLOR_BLUE)Running linters on wttrin.el...$(COLOR_RESET)"
	@echo "$(COLOR_YELLOW)Note: checkdoc, package-lint, and elisp-lint must be installed$(COLOR_RESET)"
	@$(EMACS_BATCH) -L $(PROJECT_ROOT) \
		--eval "(progn \
			(require 'checkdoc nil t) \
			(require 'package-lint nil t) \
			(require 'elisp-lint nil t) \
			(find-file \"$(MAIN_FILE)\") \
			(when (featurep 'checkdoc) \
				(checkdoc-current-buffer t)) \
			(when (featurep 'package-lint) \
				(package-lint-current-buffer)) \
			(when (featurep 'elisp-lint) \
				(elisp-lint-file \"$(MAIN_FILE)\")))" && \
		echo "$(COLOR_GREEN)✓ All linting checks passed$(COLOR_RESET)" || \
		echo "$(COLOR_YELLOW)⚠ Linting issues found$(COLOR_RESET)"

# ============================================================================
# Utility Targets
# ============================================================================

clean: clean-tests clean-compiled
	@echo "$(COLOR_GREEN)✓ Clean complete$(COLOR_RESET)"

clean-compiled:
	@echo "$(COLOR_BLUE)Removing compiled files (.elc, .eln)...$(COLOR_RESET)"
	@find $(PROJECT_ROOT) -type f \( -name "*.eln" -o -name "*.elc" \) -delete
	@echo "$(COLOR_GREEN)✓ Compiled files removed$(COLOR_RESET)"

clean-tests:
	@echo "$(COLOR_BLUE)Removing test artifacts...$(COLOR_RESET)"
	@rm -rf $(HOME)/.temp-emacs-tests
	@echo "$(COLOR_GREEN)✓ Test artifacts removed$(COLOR_RESET)"
