#!/usr/bin/env python3
"""Attributes a JFR recording of a test run to methods, Scalus code and test suites ([PR-6]).

Usage: profile_analyze.py <recording.jfr> [<top-n>] [--focus <method-substring>]

`jfr view hot-methods` shows only the top frame (for example BoxesRunTime.equals2), which does
not say which test or which Scalus code pays for it. This script reads the raw samples and reports:

  - self CPU per method (top frame)
  - inclusive CPU per scalus.* method (the method is anywhere on the stack)
  - CPU per test suite (the outermost frame whose class looks like a test suite)
  - allocation weight per class and per first scalus.* frame
  - GC pause total
"""
from __future__ import annotations

import re
import subprocess
import sys
from collections import Counter

SUITE = re.compile(r"(Test|Spec|Suite)(\$|$)")


UNITS = {"bytes": 1, "byte": 1, "kB": 2**10, "MB": 2**20, "GB": 2**30}
TIME_UNITS = {"ns": 1e-6, "us": 1e-3, "ms": 1.0, "s": 1000.0}
FRAME = re.compile(r"^\s+([\w.$/<>]+)\.([\w$<>]+)\(")


def events(jfr: str, names: str):
    """Streams `jfr print` text output (the JSON export of a large recording is several GB).
    Yields one dict per event: frames (top first, raw `Class.method`), objectClass, weight (bytes),
    pause_ms."""
    proc = subprocess.Popen(["jfr", "print", "--stack-depth", "1024", "--events", names, jfr],
                            stdout=subprocess.PIPE, text=True, bufsize=1 << 20)
    ev = None
    in_stack = False
    for line in proc.stdout:
        if line.startswith("jdk."):
            ev = {"frames": [], "objectClass": "?", "weight": 0, "pause_ms": 0.0}
            in_stack = False
        elif ev is None:
            continue
        elif line.startswith("}"):
            yield ev
            ev = None
        elif in_stack:
            if line.strip() == "]":
                in_stack = False
            else:
                m = FRAME.match(line)
                if m:
                    ev["frames"].append((m.group(1), m.group(2)))
        else:
            key, _, value = line.strip().partition(" = ")
            if key == "stackTrace":
                in_stack = True
            elif key == "objectClass":
                ev["objectClass"] = value.split(" (")[0]
            elif key == "weight":
                num, _, unit = value.partition(" ")
                ev["weight"] = float(num.replace(",", "")) * UNITS.get(unit, 1)
            elif key == "sumOfPauses":
                num, _, unit = value.partition(" ")
                ev["pause_ms"] = float(num.replace(",", "")) * TIME_UNITS.get(unit, 1)
    proc.wait()


ANONFUN = re.compile(r"^\$anonfun\$(.+?)(\$\d+)*$")


def frames(ev) -> list[str]:
    """Frames top first, as `pkg.Class.method`. Lambda frames are dropped (the next frame is the
    real `$anonfun$` method) and `$anonfun$name$3` becomes `name (lambda)`."""
    result = []
    for cls, name in ev["frames"]:
        cls = cls.replace("/", ".")
        if "$$Lambda" in cls:
            continue
        a = ANONFUN.match(name)
        if a:
            name = a.group(1).split("$")[-1] + " (lambda)"
        result.append(f"{cls}.{name}")
    return result


def suite_of(fs: list[str]) -> str | None:
    for f in reversed(fs):  # outermost first
        cls = f.rsplit(".", 1)[0]
        if cls.startswith(("scalus.", "examples.")) and SUITE.search(cls.split(".")[-1]):
            return cls.split("$")[0]
    return None


def pct_table(title: str, counter: Counter, total: float, n: int, unit: str = "%") -> str:
    lines = [f"### {title}", "", "| share | item |", "|---|---|"]
    for item, v in counter.most_common(n):
        lines.append(f"| {100 * v / total:5.1f} % | `{item}` |")
    return "\n".join(lines) + "\n"


def main() -> None:
    args = sys.argv[1:]
    focus = None
    if "--focus" in args:
        i = args.index("--focus")
        focus = args[i + 1]
        del args[i:i + 2]
    jfr = args[0]
    n = int(args[1]) if len(args) > 1 else 25
    if focus:
        return focus_report(jfr, focus, n)

    cpu = list(events(jfr, "jdk.ExecutionSample"))
    self_c, scalus_incl, suite_c, scalus_self = Counter(), Counter(), Counter(), Counter()
    for ev in cpu:
        fs = frames(ev)
        if not fs:
            continue
        self_c[fs[0]] += 1
        first_scalus = next((f for f in fs if f.startswith("scalus.")), None)
        if first_scalus:
            scalus_self[first_scalus] += 1
        for f in set(fs):
            if f.startswith("scalus.") and not SUITE.search(f.rsplit(".", 1)[0].split(".")[-1]):
                scalus_incl[f] += 1
        suite_c[suite_of(fs) or "(no test frame: harness, JIT, GC threads)"] += 1
    total = max(len(cpu), 1)

    alloc = events(jfr, "jdk.ObjectAllocationSample")
    by_class, by_site = Counter(), Counter()
    for ev in alloc:
        w = ev["weight"]
        by_class[ev["objectClass"]] += w
        site = next((f for f in frames(ev) if f.startswith("scalus.")), "(non-scalus)")
        by_site[site] += w
    alloc_total = max(sum(by_class.values()), 1)

    gcs = list(events(jfr, "jdk.GarbageCollection"))
    gc_ms = sum(e["pause_ms"] for e in gcs)

    print(f"CPU samples: {len(cpu)}; allocation sampled: {alloc_total / 2**30:.1f} GiB; "
          f"GC: {len(gcs)} collections, {gc_ms:.0f} ms of pauses\n")
    print(pct_table("CPU by test suite (outermost suite frame)", suite_c, total, n))
    print(pct_table("CPU inclusive by scalus.* method", scalus_incl, total, n))
    print(pct_table("CPU self by first scalus.* frame", scalus_self, total, n))
    print(pct_table("CPU self by top frame", self_c, total, n))
    print(pct_table("Allocation by class", by_class, alloc_total, n))
    print(pct_table("Allocation by first scalus.* frame", by_site, alloc_total, n))


def focus_report(jfr: str, focus: str, n: int) -> None:
    """For CPU samples whose stack contains `focus`: share per suite and per direct caller."""
    cpu = list(events(jfr, "jdk.ExecutionSample"))
    hit, suites, callers = 0, Counter(), Counter()
    for ev in cpu:
        fs = frames(ev)
        idx = next((i for i, f in enumerate(fs) if focus in f), None)
        if idx is None:
            continue
        hit += 1
        suites[suite_of(fs) or "(no test frame)"] += 1
        # first frame below the focus method that is not the focus method itself
        caller = next((f for f in fs[idx + 1:] if focus not in f), "(root)")
        callers[caller] += 1
    print(f"{hit} of {len(cpu)} CPU samples ({100 * hit / max(len(cpu), 1):.1f} %) contain `{focus}`\n")
    print(pct_table("By test suite", suites, max(hit, 1), n))
    print(pct_table("By direct caller", callers, max(hit, 1), n))


if __name__ == "__main__":
    main()
