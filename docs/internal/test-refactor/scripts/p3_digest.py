#!/usr/bin/env python3
"""Split the P2 findings by scope for the P3 agents (spec P3 "Method", step 1).

Usage: p3_digest.py <repo-root>

Reads docs/internal/test-refactor/reports/*.json and writes
docs/internal/test-refactor/p3/findings-by-scope/<scope>.jsonl: one finding per line, with its
cluster added, `main-bug` left out (those live in findings.md). Also writes, in p3/:
  summary.csv            count, verdicts and estimated lines removed per scope and module
  helper-mentions.jsonl  findings of any scope whose action names a helper or asks for one
  cross-cluster.jsonl    findings joined across clusters and scopes by evidence file and by the
                         repeated def names of repeated-defs.csv
"""
import re
import csv
import json
import sys
from collections import defaultdict
from pathlib import Path


def main() -> None:
    root = Path(sys.argv[1]).resolve()
    base = root / "docs/internal/test-refactor"
    out = base / "p3/findings-by-scope"
    out.mkdir(parents=True, exist_ok=True)
    by_scope = defaultdict(list)
    for report in sorted((base / "reports").glob("*.json")):
        data = json.loads(report.read_text())
        for f in data.get("findings", []):
            if f.get("scope") == "main-bug":
                continue
            by_scope[f["scope"]].append({"cluster": data.get("cluster", report.stem), **f})
    summary = defaultdict(lambda: {"n": 0, "lines": 0, "CONFIRMED": 0, "NARROWED": 0, "UNVERIFIED": 0})
    for scope, findings in by_scope.items():
        with (out / f"{scope}.jsonl").open("w") as fh:
            for f in findings:
                fh.write(json.dumps(f, ensure_ascii=False) + "\n")
                s = summary[(scope, f.get("module", ""))]
                s["n"] += 1
                s["lines"] += f.get("linesRemoved") or 0
                s[f.get("verdict", "UNVERIFIED")] = s.get(f.get("verdict", "UNVERIFIED"), 0) + 1
    with (base / "p3/summary.csv").open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["scope", "module", "findings", "lines_removed", "confirmed", "narrowed", "unverified"])
        for (scope, module), s in sorted(summary.items()):
            w.writerow([scope, module, s["n"], s["lines"], s["CONFIRMED"], s["NARROWED"], s["UNVERIFIED"]])
    everything = [f for fs in by_scope.values() for f in fs]
    helper_re = re.compile(r"helper|EvalTestKit|ScalusTest|testkit|ArbitraryInstances|shared (setup|fixture)|extract|reuse", re.I)
    with (base / "p3/helper-mentions.jsonl").open("w") as fh:
        for f in everything:
            if f["scope"] not in ("extract-helper", "adopt-helper") and helper_re.search(f.get("action", "")):
                fh.write(json.dumps(f, ensure_ascii=False) + "\n")

    def files(f):
        return {e.split(":")[0].split("/")[-1] for e in f.get("evidence", [])}

    groups = defaultdict(list)
    for f in everything:
        for name in files(f):
            groups["file:" + name].append(f)
    with (base / "repeated-defs.csv").open() as fh:
        defs = [r["def"] for r in csv.DictReader(fh) if len(r["def"]) > 3]
    for d in defs:
        pat = re.compile(rf"\b{re.escape(d)}\b")
        for f in everything:
            if pat.search(f.get("claim", "") + " " + f.get("action", "")):
                groups["def:" + d].append(f)
    with (base / "p3/cross-cluster.jsonl").open("w") as fh:
        for key, fs in sorted(groups.items()):
            clusters = {f["cluster"] for f in fs}
            scopes = {f["scope"] for f in fs}
            if len(clusters) > 1 or len(scopes) > 1:
                fh.write(json.dumps({"key": key, "clusters": sorted(clusters), "scopes": sorted(scopes),
                                     "findings": [f.get("id") for f in fs]}, ensure_ascii=False) + "\n")

    for scope, findings in sorted(by_scope.items(), key=lambda kv: -len(kv[1])):
        print(f"{scope:24} {len(findings):4}")


if __name__ == "__main__":
    main()
