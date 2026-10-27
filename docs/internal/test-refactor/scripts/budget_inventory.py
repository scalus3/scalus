#!/usr/bin/env python3
"""Inventory of on-chain evaluation sites in tests and their budget-pin state (spec P3 "Method",
step 1, and [BP-1] to [BP-9]).

Usage: budget_inventory.py <repo-root>

Writes docs/internal/test-refactor/p3/budget-inventory.csv with one row per call site:
module, file, line, suite, test, api, state. Text-based heuristic; the state of a site comes from
the API it calls and from the text of its enclosing test block:

  exact+fee    assertEvalWithBudgetAndFee
  exact        assertEvalWithBudget(s), assertBudgetEquals, or a raw evaluation whose test block
               contains an ExUnits(...) literal
  upper-bound  assertEvalWithinBudget, assertBudgetWithin, assertBudgetClose
  property     checkEval*, or any site whose test block uses forAll
  tx-fee       a transaction submit whose test block asserts on `.fee`
  forced-fee   a transaction submit whose test block forces the fee (minFee(Coin(...)))
  none         everything else
"""
import csv
import re
import sys
from collections import Counter
from pathlib import Path

API = re.compile(
    r"\b(assertEvalWithBudgetAndFee|assertEvalWithBudgets?|assertEvalWithinBudget|assertEval(?:Eq|NotEq|Success|Fails\w*|Result)?"
    r"|checkEval\w*|assertBudget(?:Equals|Within|Close)|evaluateDebug|evaluateDeBruijnedTerm|runValidator\w*"
    r"|submit(?:Sync|Tx|OrFail|Built|WithRefScript|With|WithRetry|Async)?)\("
)
TEST = re.compile(r'^\s*(?:test|property|it)\(\s*(?:s?"((?:[^"\\]|\\.)*)")')
SUITE = re.compile(r"^\s*(?:final\s+|abstract\s+)?(?:class|object|trait)\s+(\w+)")
EXACT = {"assertEvalWithBudget", "assertEvalWithBudgets", "assertBudgetEquals"}
UPPER = {"assertEvalWithinBudget", "assertBudgetWithin", "assertBudgetClose"}


def blocks(lines):
    """Maps each line index to (suite, test name, start, end) of its enclosing test block."""
    starts = [i for i, l in enumerate(lines) if TEST.match(l)]
    suite, out = "", {}
    bounds = list(zip(starts, starts[1:] + [len(lines)]))
    for i, l in enumerate(lines):
        m = SUITE.match(l)
        if m and not l.startswith(" " * 8):
            suite = m.group(1)
        out[i] = suite
    return bounds, out


def main() -> None:
    root = Path(sys.argv[1]).resolve()
    rows, states = [], Counter()
    for path in sorted(root.glob("**/src/test/**/*.scala")):
        rel = path.relative_to(root).as_posix()
        if rel.startswith(".claude/") or "/target/" in rel:
            continue
        lines = path.read_text(errors="replace").splitlines()
        bounds, suites = blocks(lines)
        for i, line in enumerate(lines):
            for m in API.finditer(line):
                api = m.group(1)
                if line.lstrip().startswith(("def ", "protected", "private", "inline", "//", "*")):
                    continue
                start, end = next(((s, e) for s, e in bounds if s <= i < e), (0, len(lines)))
                block = "\n".join(lines[start:end])
                test = TEST.match(lines[start]).group(1) if TEST.match(lines[start]) else ""
                if api == "assertEvalWithBudgetAndFee":
                    state = "exact+fee"
                elif api in EXACT:
                    state = "exact"
                elif api in UPPER:
                    state = "upper-bound"
                elif api.startswith("checkEval") or "forAll" in block:
                    state = "property"
                elif api.startswith("submit"):
                    if re.search(r"minFee\(\s*Coin\(", block):
                        state = "forced-fee"
                    elif re.search(r"\.fee\b[^\n]*(==|shouldBe|assert)|assert[^\n]*\.fee\b", block):
                        state = "tx-fee"
                    else:
                        state = "none"
                elif "ExUnits(" in block:
                    state = "exact"
                else:
                    state = "none"
                states[state] += 1
                rows.append((rel.split("/src/")[0], rel, i + 1, suites[i], test, api, state))
    out = root / "docs/internal/test-refactor/p3/budget-inventory.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["module", "file", "line", "suite", "test", "api", "state"])
        w.writerows(rows)
    print(f"{len(rows)} sites:", dict(states.most_common()))


if __name__ == "__main__":
    main()
