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

.PHONY: help test test-all test-unit test-integration test-file test-name \
        validate-parens validate compile lint \
        clean clean-compiled clean-tests

# Default target
.DEFAULT_GOAL := help

help:
	@echo "wttrin Makefile Targets:"
	@echo ""
	@echo "  Testing:"
	@echo "    make test              - Run all tests ($(words $(ALL_TESTS)) files)"
	@echo "    make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "    make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "    make test-file FILE=<filename>  - Run specific test file"
	@echo "    make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo ""
	@echo "  Validation:"
	@echo "    make validate-parens   - Check for unbalanced parentheses"
	@echo "    make validate          - Load wttrin.el to verify it compiles"
	@echo "    make compile           - Byte-compile wttrin.el"
	@echo "    make lint              - Run all linters (checkdoc, package-lint, elisp-lint)"
	@echo ""
	@echo "  Utilities:"
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
	@echo "Running all tests ($(words $(ALL_TESTS)) files)..."
	@$(MAKE) test-unit
	@if [ $(words $(INTEGRATION_TESTS)) -gt 0 ]; then \
		$(MAKE) test-integration; \
	fi
	@echo "✓ All tests complete"

test-unit:
	@echo "Running unit tests ($(words $(UNIT_TESTS)) files)..."
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "✓ All unit tests passed"; \
	else \
		echo "✗ $$failed unit test file(s) failed"; \
		exit 1; \
	fi

test-integration:
	@if [ $(words $(INTEGRATION_TESTS)) -eq 0 ]; then \
		echo "No integration tests found"; \
		exit 0; \
	fi
	@echo "Running integration tests ($(words $(INTEGRATION_TESTS)) files)..."
	@failed=0; \
	for test in $(INTEGRATION_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_TEST) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "✓ All integration tests passed"; \
	else \
		echo "✗ $$failed integration test file(s) failed"; \
		exit 1; \
	fi

test-file:
ifndef FILE
	@echo "Error: FILE parameter required"
	@echo "Usage: make test-file FILE=test-wttrin--build-url.el"
	@exit 1
endif
	@echo "Running tests in $(FILE)..."
	@$(EMACS_TEST) -l ert -l $(TEST_DIR)/$(FILE) -f ert-run-tests-batch-and-exit
	@echo "✓ Tests in $(FILE) complete"

test-name:
ifndef TEST
	@echo "Error: TEST parameter required"
	@echo "Usage: make test-name TEST=test-wttrin--build-url-*"
	@exit 1
endif
	@echo "Running tests matching pattern: $(TEST)..."
	@$(EMACS_TEST) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(test)) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "✓ Tests matching '$(TEST)' complete"

# ============================================================================
# Validation Targets
# ============================================================================

validate-parens:
	@echo "Checking for unbalanced parentheses..."
	@echo "  Checking $(MAIN_FILE)..."; \
	$(EMACS_BATCH) --eval "(condition-case err \
		(progn \
			(find-file \"$(MAIN_FILE)\") \
			(check-parens) \
			(kill-emacs 0)) \
		(error (progn \
			(message \"ERROR: %s\" err) \
			(kill-emacs 1))))" 2>&1 > /dev/null && \
		echo "✓ $(MAIN_FILE) has balanced parentheses" || \
		(echo "✗ $(MAIN_FILE) has unbalanced parentheses" && exit 1)

validate:
	@echo "Loading wttrin.el to verify compilation..."
	@$(EMACS_BATCH) -L $(PROJECT_ROOT) \
		--eval "(condition-case err \
			(progn \
				(load-file \"$(MAIN_FILE)\") \
				(message \"OK: %s\" \"$(MAIN_FILE)\")) \
			(error (progn \
				(message \"ERROR loading %s: %s\" \"$(MAIN_FILE)\" err) \
				(kill-emacs 1))))" && \
		echo "✓ $(MAIN_FILE) loaded successfully" || \
		(echo "✗ $(MAIN_FILE) failed to load" && exit 1)

compile:
	@echo "Byte-compiling wttrin.el..."
	@$(EMACS_BATCH) -L $(PROJECT_ROOT) \
		--eval "(progn \
			(setq byte-compile-error-on-warn nil) \
			(batch-byte-compile))" $(MAIN_FILE)
	@echo "✓ Compilation complete"

lint:
	@echo "Running linters on wttrin.el..."
	@echo "Note: checkdoc, package-lint, and elisp-lint must be installed"
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
		echo "✓ All linting checks passed" || \
		echo "⚠ Linting issues found"

# ============================================================================
# Utility Targets
# ============================================================================

clean: clean-tests clean-compiled
	@echo "✓ Clean complete"

clean-compiled:
	@echo "Removing compiled files (.elc, .eln)..."
	@find $(PROJECT_ROOT) -type f \( -name "*.eln" -o -name "*.elc" \) -delete
	@echo "✓ Compiled files removed"

clean-tests:
	@echo "Removing test artifacts..."
	@rm -rf $(HOME)/.temp-emacs-tests
	@echo "✓ Test artifacts removed"
