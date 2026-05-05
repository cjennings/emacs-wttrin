#!/usr/bin/env python3
"""Print a per-file and overall coverage summary from undercover's simplecov JSON.

Usage:
    python3 scripts/coverage-summary.py [path]

If `path` is omitted, defaults to `.coverage/simplecov.json`.
Exit code is 0 on success, 1 if the JSON is missing or malformed.
"""

import json
import os
import sys


def main(path: str) -> int:
    try:
        with open(path) as f:
            data = json.load(f)
    except FileNotFoundError:
        print(f"error: {path} not found; run `make coverage` first", file=sys.stderr)
        return 1
    except json.JSONDecodeError as exc:
        print(f"error: {path} is not valid JSON: {exc}", file=sys.stderr)
        return 1

    try:
        suite = data["undercover.el"]["coverage"]
    except (KeyError, TypeError):
        print(f"error: {path} does not look like an undercover simplecov report",
              file=sys.stderr)
        return 1

    print(f'{"File":<30} {"Lines":>7} {"Covered":>8} {"Coverage":>10}')
    print("-" * 60)

    total_lines = 0
    total_covered = 0
    for fname, lines in suite.items():
        relevant = [l for l in lines if l is not None]
        covered = sum(1 for l in relevant if l > 0)
        pct = 100.0 * covered / len(relevant) if relevant else 0.0
        total_lines += len(relevant)
        total_covered += covered
        short = os.path.basename(fname)
        print(f"{short:<30} {len(relevant):>7} {covered:>8} {pct:>9.2f}%")

    print("-" * 60)
    overall = 100.0 * total_covered / total_lines if total_lines else 0.0
    print(f'{"TOTAL":<30} {total_lines:>7} {total_covered:>8} {overall:>9.2f}%')
    return 0


if __name__ == "__main__":
    target = sys.argv[1] if len(sys.argv) > 1 else ".coverage/simplecov.json"
    sys.exit(main(target))
