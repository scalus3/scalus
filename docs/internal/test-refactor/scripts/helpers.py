#!/usr/bin/env python3
"""Helper inventory of test sources (spec [P0-10], [P0-11]).

Usage: helpers.py <repo-root> <out-dir>

Writes:
  shared-helpers.csv – object/trait/class/def defined in a test file that is not a suite,
                       with the number of other test files that mention the name.
  repeated-defs.csv  – def names defined in 3 or more test files: candidates for one shared helper.

Text-based on purpose: no compiler needed, good enough to rank candidates for the P2 agents.
"""
import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

TYPE_DEF = re.compile(r"^\s*(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:final\s+|sealed\s+|abstract\s+|case\s+)*(object|trait|class)\s+(\w+)", re.M)
DEF = re.compile(r"^\s*(?:(?:private|protected)(?:\[\w+\])?\s+)?(?:inline\s+|override\s+)*def\s+(\w+)", re.M)
SUITE = re.compile(r"extends\s+(?:\w+\s+with\s+)*(?:AnyFunSuite|ScalaCheckSuite|AnyFlatSpec|AnyWordSpec)")
SKIP_DEFS = {"apply", "unapply", "toString", "equals", "hashCode", "main", "test", "assert"}


def test_files(root: Path):
    for p in root.glob("**/src/test/**/*.scala"):
        if "target" in p.parts or ".claude" in p.relative_to(root).parts:
            continue
        yield p


def main() -> None:
    root = Path(sys.argv[1]).resolve()
    out = Path(sys.argv[2])
    files = {p.relative_to(root).as_posix(): p.read_text(errors="replace") for p in test_files(root)}
    words = {f: set(re.findall(r"\b\w+\b", t)) for f, t in files.items()}

    # [P0-10] Definitions in non-suite files are shared helpers; count the files that use them.
    shared = []
    for f, text in files.items():
        if SUITE.search(text):
            continue
        types = [(k, n) for k, n in TYPE_DEF.findall(text)]
        type_names = {n for _, n in types}
        # A file uses this helper file only if it mentions one of its types (import, extends, Foo.x).
        clients = {g for g, ws in words.items() if g != f and ws & type_names}
        for kind, name in dict.fromkeys(types):
            shared.append((f, kind, name, sum(1 for g in clients if name in words[g])))
        for name in dict.fromkeys(DEF.findall(text)):
            if name in SKIP_DEFS or name in type_names:
                continue
            shared.append((f, "def", name, sum(1 for g in clients if name in words[g])))
    shared.sort(key=lambda r: (-r[3], r[0]))
    with (out / "shared-helpers.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["file", "kind", "name", "using_files"])
        w.writerows(shared)

    # [P0-11] The same def name in 3+ files: likely copied setup.
    where = defaultdict(set)
    for f, text in files.items():
        for name in DEF.findall(text):
            if name not in SKIP_DEFS:
                where[name].add(f)
    repeated = sorted(((n, len(fs), ";".join(sorted(fs))) for n, fs in where.items() if len(fs) >= 3),
                      key=lambda r: -r[1])
    with (out / "repeated-defs.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["def", "files", "paths"])
        w.writerows(repeated)


if __name__ == "__main__":
    main()
