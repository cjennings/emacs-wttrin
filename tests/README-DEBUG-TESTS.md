# Wttrin Debug Integration Tests

This directory contains comprehensive integration tests with debug logging enabled.

## Running the Tests

```bash
cd /path/to/wttrin
emacs --batch --eval "(progn
  (package-initialize)
  (add-to-list 'load-path \".\")
  (setq wttrin-debug t)
  (load-file \"wttrin.el\")
  (load-file \"tests/test-wttrin-integration-with-debug.el\")
  (ert-run-tests-batch-and-exit))"
```

## What the Tests Show

The integration tests demonstrate:

1. **Debug logging captures all key events**:
   - URL fetch starting
   - Data received (with byte count)
   - Mode-line display updates
   - Emoji extraction
   - Error handling

2. **Example debug output from a successful fetch**:
```
[wttrin-debug 11:51:46.490] mode-line-fetch: Starting fetch for Berkeley, CA
[wttrin-debug 11:51:46.490] mode-line-fetch: URL = https://wttr.in/Berkeley%2C%20CA?m&format=%l:+%c+%t+%C
[wttrin-debug 11:51:46.490] mode-line-fetch: Received data = "Berkeley, CA: ☀️ +62°F Clear"
[wttrin-debug 11:51:46.490] mode-line-display: Updating display with: "Berkeley, CA: ☀️ +62°F Clear"
[wttrin-debug 11:51:46.490] mode-line-display: Extracted emoji = "☀", font = Noto Color Emoji
[wttrin-debug 11:51:46.490] mode-line-display: Complete. mode-line-string set = YES
```

## Using Debug Mode in Your Configuration

### Enable Debug Before Loading

```emacs-lisp
;; In your init.el, BEFORE (require 'wttrin):
(setq wttrin-debug t)
(require 'wttrin)
```

### Or Enable Later

```emacs-lisp
M-x debug-wttrin-enable
```

### View Debug Log

```emacs-lisp
M-x wttrin-debug-show-log
```

This opens a buffer showing all debug events with timestamps.

### Clear Debug Log

```emacs-lisp
M-x wttrin-debug-clear-log
```

## Test Fixtures

- `fixtures/test-init.el` - Minimal init file with debug enabled for manual testing

## Troubleshooting

If wttrin isn't loading in your configuration:

1. **Enable debug mode** (set `wttrin-debug` to `t` before loading)
2. **Check dependencies**: Run `M-x package-list-packages` and ensure `xterm-color` is installed
3. **View debug log**: Run `M-x wttrin-debug-show-log` after trying to use wttrin
4. **Check for errors**: Look in `*Messages*` buffer for any error messages

The debug log will show you exactly where the process stops or fails.
