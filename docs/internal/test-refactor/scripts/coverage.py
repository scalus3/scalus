#!/usr/bin/env python3
"""Coverage baseline and coverage gate (spec [P0-8], [CG-1] to [CG-4]).

  coverage.py baseline <repo-root> <out-dir>
      Reads every <module>/target/scala-*/scoverage-report/scoverage.xml and writes
      <out-dir>/summary.csv (per module and package) and <out-dir>/uncovered.tsv.gz (every statement
      that no test invoked).

  coverage.py flaky <baseline-dir> <second-run-dir>
      Writes <baseline-dir>/flaky.tsv: the statements whose coverage differs between two runs of
      the same code (unseeded property tests). The gate ignores them.

  coverage.py gate <repo-root> <baseline-dir> [<package-prefix> ...]
      Builds the same uncovered set from the current reports and prints every statement that was
      covered in the baseline and is uncovered now. Exit code 1 if there is any. With package
      prefixes, only those packages count.

A statement is keyed by (source path relative to the repo, start offset, end offset). Test
refactoring does not change main code, so the keys stay stable between the baseline and the gate.
How to produce the reports: see docs/internal/test-refactor/coverage.md.
"""
import csv
import gzip
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict
from pathlib import Path


def reports(root: Path):
    for xml in sorted(root.glob("**/target/scala-*/scoverage-report/scoverage.xml")):
        if ".claude" in xml.relative_to(root).parts:
            continue
        yield xml.relative_to(root).as_posix().split("/target/")[0], xml


def statements(root: Path):
    """Yields (module, package, key, line, branch, method, invoked) for every non-ignored statement."""
    prefix = str(root) + "/"
    for module, xml in reports(root):
        for st in ET.parse(xml).getroot().iter("statement"):
            if st.get("ignored") == "true":
                continue
            src = st.get("source", "").removeprefix(prefix)
            key = (src, int(st.get("start")), int(st.get("end")))
            yield (module, st.get("package"), key, int(st.get("line")), st.get("branch") == "true",
                   st.get("method"), int(st.get("invocation-count", "0")) > 0)


def baseline(root: Path, out: Path) -> None:
    out.mkdir(parents=True, exist_ok=True)
    summary = defaultdict(lambda: [0, 0, 0, 0])
    uncovered = []
    for module, package, key, line, branch, method, invoked in statements(root):
        s = summary[(module, package)]
        s[0] += 1
        s[1] += invoked
        if branch:
            s[2] += 1
            s[3] += invoked
        if not invoked:
            uncovered.append((module, package, key[0], line, key[1], key[2], "branch" if branch else "", method))
    with (out / "summary.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["module", "package", "statements", "invoked", "statement_pct", "branches",
                    "branches_invoked", "branch_pct"])
        for (module, package), (n, i, b, bi) in sorted(summary.items()):
            w.writerow([module, package, n, i, f"{100 * i / n:.1f}", b, bi, f"{100 * bi / b:.1f}" if b else ""])
    with gzip.open(out / "uncovered.tsv.gz", "wt", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["module", "package", "source", "line", "start", "end", "branch", "method"])
        w.writerows(sorted(uncovered))


def uncovered_keys(base: Path) -> set:
    with gzip.open(base / "uncovered.tsv.gz", "rt") as f:
        return {(r["source"], int(r["start"]), int(r["end"])) for r in csv.DictReader(f, delimiter="\t")}


def flaky(base: Path, other: Path) -> None:
    diff = uncovered_keys(base) ^ uncovered_keys(other)
    with (base / "flaky.tsv").open("w", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["source", "start", "end"])
        w.writerows(sorted(diff))
    print(f"{len(diff)} flaky statements")


def gate(root: Path, base: Path, prefixes: list[str]) -> int:
    before = uncovered_keys(base)
    flaky_file = base / "flaky.tsv"
    if flaky_file.exists():
        with flaky_file.open() as f:
            before |= {(r["source"], int(r["start"]), int(r["end"])) for r in csv.DictReader(f, delimiter="\t")}
    lost = []
    for module, package, key, line, branch, method, invoked in statements(root):
        if invoked or key in before:
            continue
        if prefixes and not any(package.startswith(p) for p in prefixes):
            continue
        lost.append(f"{key[0]}:{line} {'branch ' if branch else ''}{method} [{key[1]}-{key[2]}]")
    for entry in sorted(set(lost)):
        print("LOST", entry)
    print(f"{len(set(lost))} statements lost coverage")
    return 1 if lost else 0


if __name__ == "__main__":
    cmd, root = sys.argv[1], Path(sys.argv[2]).resolve()  # for "flaky", root is the baseline dir
    if cmd == "baseline":
        baseline(root, Path(sys.argv[3]))
    elif cmd == "flaky":
        flaky(Path(sys.argv[2]), Path(sys.argv[3]))
    elif cmd == "gate":
        sys.exit(gate(root, Path(sys.argv[3]), sys.argv[4:]))
    else:
        sys.exit(f"unknown command {cmd}")
