#!/usr/bin/env python3
"""Convert sbt JUnit XML test reports to timings.csv (spec [P0-2]).

Usage: timings.py <repo-root> <out.csv>

Each row: module, suite, test, seconds. A suite-level row (test = "*") holds the suite total,
which includes setup time that no single test reports.
"""
import csv
import sys
import xml.etree.ElementTree as ET
from pathlib import Path


def module_of(report: Path, root: Path) -> str:
    # <module>/[jvm/]target/[scala-x/]test-reports/TEST-*.xml -> path before "/target/"
    rel = report.relative_to(root).as_posix()
    return rel.split("/target/")[0]


def main() -> None:
    root = Path(sys.argv[1]).resolve()
    out = Path(sys.argv[2])
    rows = []
    for report in sorted(root.glob("**/target/**/test-reports/*.xml")):
        if "plutus-conformance" in report.parts:
            continue
        suite = ET.parse(report).getroot()
        module = module_of(report, root)
        name = suite.get("name", report.stem)
        rows.append((module, name, "*", float(suite.get("time", 0))))
        for case in suite.iter("testcase"):
            rows.append((module, name, case.get("name", ""), float(case.get("time", 0))))
    rows.sort(key=lambda r: -r[3])
    with out.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["module", "suite", "test", "seconds"])
        w.writerows(rows)


if __name__ == "__main__":
    main()
