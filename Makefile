# Makefile for wttrin
#
# Usage:
#   make help              - Show this help message
#   make test              - Run all tests (smoke + unit + integration)
#   make test-smoke        - Run smoke tests only
#   make test-unit         - Run unit tests only
#   make test-integration  - Run integration tests only
#   make test-file FILE=test-foo.el  - Run specific test file
#   make test-name TEST=test-foo-*   - Run tests matching pattern
#   make deps              - Install dependencies via Eask
#   make coverage          - Generate simplecov JSON at .coverage/simplecov.json
#   make validate-parens   - Check for unbalanced parentheses
#   make validate          - Load wttrin.el to verify it compiles
#   make compile           - Byte-compile source files
#   make lint              - Run all linters (checkdoc, package-lint, elisp-lint)
#   make clean             - Remove test artifacts and compiled files

# Emacs binary to use (override with: make EMACS=emacs29 test)
EMACS ?= emacs
# Eask binary (override if installed elsewhere)
EASK ?= $(shell command -v eask 2>/dev/null || echo "$(HOME)/.local/bin/eask")

# Include local overrides if present
-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif

# Directories
TEST_DIR = tests
PROJECT_ROOT = .

# Test files
SMOKE_TESTS = $(wildcard $(TEST_DIR)/test-*-smoke.el)
INTEGRATION_TESTS = $(sort $(wildcard $(TEST_DIR)/test-*-integration-*.el) $(wildcard $(TEST_DIR)/test-integration-*.el))
UNIT_TESTS = $(filter-out $(INTEGRATION_TESTS) $(SMOKE_TESTS), $(wildcard $(TEST_DIR)/test-*.el))
ALL_TESTS = $(SMOKE_TESTS) $(UNIT_TESTS) $(INTEGRATION_TESTS)

# Source files
MAIN_FILE = wttrin.el
SOURCE_FILES = wttrin.el wttrin-debug.el wttrin-geolocation.el
TEST_UTIL_FILES = $(wildcard $(TEST_DIR)/testutil-*.el)

# Coverage configuration
COVERAGE_DIR = .coverage
COVERAGE_FILE = $(COVERAGE_DIR)/simplecov.json

# Plain emacs invocation (no package archives, used for parens-check)
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp

# Eask-managed emacs invocation (deps on load-path, archives configured)
EASK_EMACS = $(EMACS_ENV) $(EASK) emacs --batch -q -L $(PROJECT_ROOT) -L $(TEST_DIR)

.PHONY: help test test-all test-smoke test-unit test-integration test-file test-name \
        deps install-deps validate-parens validate compile lint \
        coverage coverage-clean \
        clean clean-compiled clean-tests

# Default target
.DEFAULT_GOAL := help

help:
	@echo "wttrin Makefile Targets:"
	@echo ""
	@echo "  Testing:"
	@echo "    make test              - Run all tests ($(words $(ALL_TESTS)) files: smoke -> unit -> integration)"
	@echo "    make test-smoke        - Run smoke tests only ($(words $(SMOKE_TESTS)) files)"
	@echo "    make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "    make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "    make test-file FILE=<filename>  - Run specific test file"
	@echo "    make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo ""
	@echo "  Coverage:"
	@echo "    make coverage          - Generate simplecov JSON at $(COVERAGE_FILE)"
	@echo "    make coverage-clean    - Delete the coverage report file"
	@echo ""
	@echo "  Validation:"
	@echo "    make validate-parens   - Check for unbalanced parentheses"
	@echo "    make validate          - Load wttrin.el to verify it compiles"
	@echo "    make compile           - Byte-compile source files"
	@echo "    make lint              - Run all linters (checkdoc, package-lint, elisp-lint)"
	@echo ""
	@echo "  Setup:"
	@echo "    make deps              - Install dependencies via Eask (runtime + dev)"
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
# Setup Targets
# ============================================================================

deps: install-deps

install-deps:
	@if ! command -v $(EASK) >/dev/null 2>&1; then \
		echo "[x] Eask not found. Install from: https://emacs-eask.github.io/"; \
		exit 1; \
	fi
	@echo "[i] Installing dependencies via Eask (runtime + dev)..."
	@$(EMACS_ENV) $(EASK) install-deps --dev
	@echo "[v] Dependencies installed"

# ============================================================================
# Testing Targets
# ============================================================================

test: test-all

test-all:
	@echo "Running all tests ($(words $(ALL_TESTS)) files: smoke -> unit -> integration)..."
	@$(MAKE) test-smoke
	@$(MAKE) test-unit
	@if [ $(words $(INTEGRATION_TESTS)) -gt 0 ]; then \
		$(MAKE) test-integration; \
	fi
	@echo "[v] All tests complete"

test-smoke:
	@if [ $(words $(SMOKE_TESTS)) -eq 0 ]; then \
		echo "No smoke tests found"; \
		exit 0; \
	fi
	@echo "Running smoke tests ($(words $(SMOKE_TESTS)) files)..."
	@failed=0; \
	for test in $(SMOKE_TESTS); do \
		echo "  Testing $$test..."; \
		$(EASK_EMACS) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[v] Smoke tests passed"; \
	else \
		echo "[x] Smoke tests failed - package cannot load properly"; \
		exit 1; \
	fi

test-unit:
	@echo "Running unit tests ($(words $(UNIT_TESTS)) files)..."
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		$(EASK_EMACS) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[v] All unit tests passed"; \
	else \
		echo "[x] $$failed unit test file(s) failed"; \
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
		$(EASK_EMACS) -l ert -l $$test -f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[v] All integration tests passed"; \
	else \
		echo "[x] $$failed integration test file(s) failed"; \
		exit 1; \
	fi

test-file:
ifndef FILE
	@echo "Error: FILE parameter required"
	@echo "Usage: make test-file FILE=test-wttrin--build-url.el"
	@exit 1
endif
	@echo "Running tests in $(FILE)..."
	@$(EASK_EMACS) -l ert -l $(TEST_DIR)/$(FILE) -f ert-run-tests-batch-and-exit
	@echo "[v] Tests in $(FILE) complete"

test-name:
ifndef TEST
	@echo "Error: TEST parameter required"
	@echo "Usage: make test-name TEST=test-wttrin--build-url-*"
	@exit 1
endif
	@echo "Running tests matching pattern: $(TEST)..."
	@$(EASK_EMACS) \
		-l ert \
		$(foreach test,$(ALL_TESTS),-l $(test)) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "[v] Tests matching '$(TEST)' complete"

# ============================================================================
# Coverage
# ============================================================================
#
# Each unit-test file runs in its own Emacs process (matching `make
# test-unit'); run-coverage-file.el instruments the source files before
# they're loaded, and undercover merges per-file results into a single
# simplecov JSON.

coverage: coverage-clean $(COVERAGE_DIR)
	@echo "[i] Cleaning .elc files so undercover can instrument source..."
	@find . -name "*.elc" -delete
	@echo "[i] Running coverage across $(words $(UNIT_TESTS)) unit-test file(s)..."
	@echo "    (slower than 'make test-unit' - each file runs in its own Emacs)"
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Coverage: $$test..."; \
		$(EASK_EMACS) \
			-l ert \
			-l $(TEST_DIR)/run-coverage-file.el \
			-l $$test \
			-f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -gt 0 ]; then \
		echo "[!] $$failed test file(s) failed during coverage run"; \
		exit 1; \
	fi
	@if [ -f $(COVERAGE_FILE) ]; then \
		echo "[v] Coverage report: $(COVERAGE_FILE) ($$(du -h $(COVERAGE_FILE) | cut -f1))"; \
	else \
		echo "[!] No coverage file produced; check that undercover is installed"; \
		exit 1; \
	fi

coverage-clean:
	@rm -f $(COVERAGE_FILE)

$(COVERAGE_DIR):
	@mkdir -p $(COVERAGE_DIR)

# ============================================================================
# Validation Targets
# ============================================================================

validate-parens:
	@echo "Checking for unbalanced parentheses..."
	@for src in $(SOURCE_FILES); do \
		echo "  Checking $$src..."; \
		$(EMACS_BATCH) --eval "(condition-case err \
			(progn \
				(find-file \"$$src\") \
				(check-parens) \
				(kill-emacs 0)) \
			(error (progn \
				(message \"ERROR: %s\" err) \
				(kill-emacs 1))))" 2>&1 > /dev/null && \
			echo "  [v] $$src has balanced parentheses" || \
			{ echo "  [x] $$src has unbalanced parentheses"; exit 1; }; \
	done

validate:
	@echo "Loading $(MAIN_FILE) to verify compilation..."
	@$(EASK_EMACS) \
		--eval "(condition-case err \
			(progn \
				(load-file \"$(MAIN_FILE)\") \
				(message \"OK: %s\" \"$(MAIN_FILE)\")) \
			(error (progn \
				(message \"ERROR loading %s: %s\" \"$(MAIN_FILE)\" err) \
				(kill-emacs 1))))" && \
		echo "[v] $(MAIN_FILE) loaded successfully" || \
		(echo "[x] $(MAIN_FILE) failed to load" && exit 1)

compile:
	@echo "Byte-compiling source files..."
	@$(EASK_EMACS) \
		--eval "(progn \
			(setq byte-compile-error-on-warn nil) \
			(batch-byte-compile))" $(SOURCE_FILES)
	@echo "[v] Compilation complete"

lint:
	@echo "Running linters on $(MAIN_FILE)..."
	@$(EASK_EMACS) \
		--eval "(progn \
			(require 'checkdoc nil t) \
			(require 'package-lint nil t) \
			(require 'elisp-lint nil t) \
			(find-file \"$(MAIN_FILE)\") \
			(when (featurep 'checkdoc) \
				(message \"-- checkdoc --\") \
				(checkdoc-current-buffer t)) \
			(when (featurep 'package-lint) \
				(message \"-- package-lint --\") \
				(package-lint-current-buffer)) \
			(when (featurep 'elisp-lint) \
				(message \"-- elisp-lint --\") \
				(elisp-lint-file \"$(MAIN_FILE)\")))" || true
	@echo "[i] Lint complete (informational)"

# ============================================================================
# Utility Targets
# ============================================================================

clean: clean-tests clean-compiled
	@echo "[v] Clean complete"

clean-compiled:
	@echo "Removing compiled files (.elc, .eln)..."
	@find $(PROJECT_ROOT) -type f \( -name "*.eln" -o -name "*.elc" \) -delete
	@echo "[v] Compiled files removed"

clean-tests:
	@echo "Removing test artifacts..."
	@rm -rf $(HOME)/.temp-emacs-tests
	@echo "[v] Test artifacts removed"
