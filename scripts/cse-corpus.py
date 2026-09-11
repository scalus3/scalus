#!/usr/bin/env python3
"""Snapshot generated blueprints and compare CSE revisions, without rebuilding them.

Run `sbtn scalusExamplesJVM/blueprint` before each snapshot. Example:
  python3 scripts/cse-corpus.py snapshot target/cse-review/after.json
  python3 scripts/cse-corpus.py compare target/cse-review/before.json target/cse-review/after.json
"""

import argparse
import json
from pathlib import Path


def snapshot(output: Path, root: Path) -> None:
    records = {}
    files = sorted(root.rglob("*.json"))
    if not files:
        raise SystemExit(f"No blueprints found in {root}; run scalusExamplesJVM/blueprint first")
    for path in files:
        for index, validator in enumerate(json.loads(path.read_text())["validators"]):
            records[f"{path.relative_to(root)}:{index}"] = {
                key: validator[key] for key in ("title", "hash", "compiledCode")
            }
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(records, indent=2) + "\n")
    print(f"Saved {len(records)} validators from {len(files)} blueprints to {output}")


def compare(before: Path, after: Path) -> None:
    old, new = (json.loads(path.read_text()) for path in (before, after))
    if old.keys() != new.keys():
        raise SystemExit(f"Corpus changed: removed={old.keys() - new.keys()}, added={new.keys() - old.keys()}")
    changed = sum(old[key]["hash"] != new[key]["hash"] for key in old)
    old_total = sum(len(bytes.fromhex(v["compiledCode"])) for v in old.values())
    new_total = sum(len(bytes.fromhex(v["compiledCode"])) for v in new.values())
    print(f"Hashes changed: {changed}/{len(old)}. Bytes: {old_total} -> {new_total} ({new_total - old_total:+d}).\n")
    print("| Validator | Before bytes | After bytes | Delta | Hash changed |")
    print("|---|---:|---:|---:|:---:|")
    for key, a in old.items():
        b = new[key]
        x, y = (len(bytes.fromhex(v["compiledCode"])) for v in (a, b))
        print(f"| {a['title']} | {x} | {y} | {y - x:+d} | {'yes' if a['hash'] != b['hash'] else 'no'} |")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    capture = commands.add_parser("snapshot")
    capture.add_argument("output", type=Path)
    capture.add_argument("--root", type=Path, default=Path(
        "scalus-examples/jvm/target/scala-3.3.8/resource_managed/main/META-INF/scalus/blueprints"
    ))
    diff = commands.add_parser("compare")
    diff.add_argument("before", type=Path)
    diff.add_argument("after", type=Path)
    args = parser.parse_args()
    if args.command == "snapshot":
        snapshot(args.output, args.root)
    else:
        compare(args.before, args.after)
