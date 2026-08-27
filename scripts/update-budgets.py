#!/usr/bin/env python3
"""
update-budgets.py — Automate budget updates after optimization changes.

Usage:
    ./update-budgets.py          # Run sbtn quick, update budgets, verify
    ./update-budgets.py --dry    # Parse and report only, don't update files

Runs `sbtn quick`, collects budget mismatches, reports statistics,
updates expected budgets in test files, and loops until all tests pass.
Formats ExUnits as: ExUnits(memory = 129_528, steps = 37_067868)
"""

import re
import sys
import subprocess
from pathlib import Path

INT_MAX = 2_147_483_647
PROJECT_ROOT = Path(__file__).resolve().parent.parent
MAX_ITERATIONS = 25

# Directories containing test files
TEST_DIRS = [
    PROJECT_ROOT / "scalus-core" / "shared" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-core" / "jvm" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-examples" / "shared" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-examples" / "jvm" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-testkit" / "shared" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-testkit" / "jvm" / "src" / "test" / "scala",
    PROJECT_ROOT / "scalus-design-patterns" / "src" / "test" / "scala",
]

ANSI_RE = re.compile(r"\x1b\[[0-9;]*[a-zA-Z]|\[0J")


# ── Formatting ──────────────────────────────────────────────────────────


def fmt_num(n: int) -> str:
    """Format number with _ before last 6 digits and L suffix if > Int.MaxValue."""
    s = str(n)
    formatted = s[:-6] + "_" + s[-6:] if len(s) >= 7 else s
    return formatted + "L" if n > INT_MAX else formatted


def fmt_exunits(mem: int, steps: int) -> str:
    return f"ExUnits(memory = {fmt_num(mem)}, steps = {fmt_num(steps)})"


def strip_num(s: str) -> int:
    return int(s.replace("_", "").rstrip("L"))


def strip_ansi(text: str) -> str:
    return ANSI_RE.sub("", text)


# ── Parsing ─────────────────────────────────────────────────────────────


def parse_failures(output: str) -> tuple[dict, list]:
    """Parse test output for budget mismatches.

    Returns:
        exunits_map: {(old_mem, old_steps): (new_mem, new_steps)}
        size_mismatches: [(old_size, new_size, filename, line)]
    """
    text = strip_ansi(output)
    exunits_map: dict[tuple[int, int], tuple[int, int]] = {}

    # Pattern 1: assertEvalWithBudget / assertEvalWithinBudget output
    #   "expected: ExUnits(mem,steps),\n...\nbut got: ExUnits(mem,steps);"
    for m in re.finditer(
        r"expected: ExUnits\((\d+),(\d+)\),.*?but got: ExUnits\((\d+),(\d+)\);",
        text,
        re.DOTALL,
    ):
        old_mem, old_steps, new_mem, new_steps = map(int, m.groups())
        exunits_map[(old_mem, old_steps)] = (new_mem, new_steps)

    # Pattern 2: standard assert(x == y) with ExUnits
    #   "ExUnits(actual_mem, actual_steps) did not equal ExUnits(expected_mem, expected_steps)"
    for m in re.finditer(
        r"ExUnits\((\d+),\s*(\d+)\) did not equal ExUnits\((\d+),\s*(\d+)\)", text
    ):
        actual_mem, actual_steps, expected_mem, expected_steps = map(int, m.groups())
        exunits_map[(expected_mem, expected_steps)] = (actual_mem, actual_steps)

    # Pattern 3: plain integer mismatches (e.g. bitSize assertions)
    #   "83 did not equal 123 (ExprSizeAndBudgetTest.scala:74)"
    size_mismatches = []
    for m in re.finditer(
        r"(\d+) did not equal (\d+) \((\w+\.scala):(\d+)\)", text
    ):
        actual, expected, filename, line = m.groups()
        actual_int, expected_int = int(actual), int(expected)
        # Skip if it looks like ExUnits components (already handled above)
        if actual_int == expected_int:
            continue
        key_fwd = (expected_int, actual_int)
        key_rev = (actual_int, expected_int)
        if key_fwd not in exunits_map and key_rev not in exunits_map:
            size_mismatches.append((expected_int, actual_int, filename, int(line)))

    return exunits_map, size_mismatches


def parse_failing_classes(output: str) -> list[str]:
    """Extract fully-qualified failing test class names from sbt output."""
    text = strip_ansi(output)
    classes = []
    in_block = False
    for line in text.split("\n"):
        cleaned = re.sub(r"^\[(?:error|info)\]\s*", "", line).strip()
        if cleaned == "Failed tests:":
            in_block = True
            continue
        if in_block:
            # Class names are tab-indented after "Failed tests:"
            if re.match(r"^[a-z][\w.]*[A-Z]\w*$", cleaned):
                classes.append(cleaned)
            else:
                in_block = False
    return list(dict.fromkeys(classes))  # dedupe, preserve order


# ── Statistics ──────────────────────────────────────────────────────────


def report_statistics(
    exunits_map: dict[tuple[int, int], tuple[int, int]], size_mismatches: list
):
    if not exunits_map and not size_mismatches:
        print("  No budget changes detected.")
        return

    decreases = 0
    increases = 0
    mem_pcts = []
    steps_pcts = []

    for (old_mem, old_steps), (new_mem, new_steps) in exunits_map.items():
        mem_delta = new_mem - old_mem
        steps_delta = new_steps - old_steps
        mem_pct = (mem_delta / old_mem * 100) if old_mem else 0
        steps_pct = (steps_delta / old_steps * 100) if old_steps else 0
        mem_pcts.append(mem_pct)
        steps_pcts.append(steps_pct)

        if mem_delta <= 0 and steps_delta <= 0:
            decreases += 1
        else:
            increases += 1

    total = len(exunits_map)
    print(f"  ExUnits changes: {total}")
    print(f"    Decreases (improvements): {decreases}")
    if increases:
        print(f"    Increases (REGRESSIONS):   {increases}")
    if mem_pcts:
        print(
            f"    Memory:  avg {sum(mem_pcts)/len(mem_pcts):+.2f}%  "
            f"range [{min(mem_pcts):+.2f}%, {max(mem_pcts):+.2f}%]"
        )
        print(
            f"    Steps:   avg {sum(steps_pcts)/len(steps_pcts):+.2f}%  "
            f"range [{min(steps_pcts):+.2f}%, {max(steps_pcts):+.2f}%]"
        )

    if size_mismatches:
        print(f"  Size changes: {len(size_mismatches)}")
        for old_sz, new_sz, fname, line in size_mismatches:
            pct = (new_sz - old_sz) / old_sz * 100 if old_sz else 0
            print(f"    {fname}:{line}  {old_sz} -> {new_sz} ({pct:+.1f}%)")

    if increases:
        print()
        print("  Regressions:")
        for (old_mem, old_steps), (new_mem, new_steps) in exunits_map.items():
            if new_mem > old_mem or new_steps > old_steps:
                mem_pct = (new_mem - old_mem) / old_mem * 100 if old_mem else 0
                steps_pct = (
                    (new_steps - old_steps) / old_steps * 100 if old_steps else 0
                )
                print(
                    f"    {fmt_exunits(old_mem, old_steps)} -> "
                    f"{fmt_exunits(new_mem, new_steps)}"
                    f"  (mem {mem_pct:+.3f}%, steps {steps_pct:+.3f}%)"
                )


# ── File Updates ────────────────────────────────────────────────────────


def find_test_files() -> list[Path]:
    files = []
    for d in TEST_DIRS:
        if d.exists():
            files.extend(d.rglob("*.scala"))
    return files


EXUNITS_RE = re.compile(
    r"ExUnits\(\s*(?:memory\s*=\s*)?([0-9_]+L?)\s*,\s*(?:steps\s*=\s*)?([0-9_]+L?)\s*\)"
)


def replace_exunits(
    exunits_map: dict[tuple[int, int], tuple[int, int]],
) -> list[str]:
    """Replace ExUnits values in all test files. Returns list of updated file paths."""
    if not exunits_map:
        return []

    updated = []

    def replacer(m: re.Match) -> str:
        mem_int = strip_num(m.group(1))
        steps_int = strip_num(m.group(2))
        if (mem_int, steps_int) in exunits_map:
            new_mem, new_steps = exunits_map[(mem_int, steps_int)]
            return fmt_exunits(new_mem, new_steps)
        return m.group(0)

    for fpath in find_test_files():
        content = fpath.read_text()
        new_content = EXUNITS_RE.sub(replacer, content)
        if new_content != content:
            fpath.write_text(new_content)
            updated.append(str(fpath.relative_to(PROJECT_ROOT)))

    return updated


def replace_sizes(size_mismatches: list) -> list[str]:
    """Replace plain integer size assertions in test files."""
    if not size_mismatches:
        return []

    updated = []

    for fpath in find_test_files():
        content = fpath.read_text()
        new_content = content

        for old_sz, new_sz, fname, line_no in size_mismatches:
            if fpath.name == fname:
                lines = new_content.split("\n")
                idx = line_no - 1
                if 0 <= idx < len(lines):
                    old_line = lines[idx]
                    new_line = old_line.replace(str(old_sz), str(new_sz))
                    if new_line != old_line:
                        lines[idx] = new_line
                        new_content = "\n".join(lines)

        # Also update test name strings that reference the old size
        for old_sz, new_sz, fname, _ in size_mismatches:
            if fpath.name == fname:
                new_content = re.sub(
                    rf"size is {old_sz}",
                    f"size is {new_sz}",
                    new_content,
                )

        if new_content != content:
            fpath.write_text(new_content)
            updated.append(str(fpath.relative_to(PROJECT_ROOT)))

    return list(dict.fromkeys(updated))


# ── Test Runners ────────────────────────────────────────────────────────


def run_sbtn(cmd: str, timeout: int = 1800) -> tuple[str, int]:
    """Run an sbtn command and return (output, returncode)."""
    print(f"  $ sbtn {cmd}")
    result = subprocess.run(
        ["sbtn", cmd],
        capture_output=True,
        text=True,
        timeout=timeout,
        cwd=PROJECT_ROOT,
    )
    return result.stdout + result.stderr, result.returncode


def run_specific_tests(classes: list[str]) -> tuple[str, int]:
    """Run specific test classes across the right sbt subprojects."""
    core_classes = []
    example_classes = []

    for cls in classes:
        if "benchmarks" in cls or cls.startswith("scalus.examples"):
            example_classes.append(cls)
        else:
            core_classes.append(cls)

    output = ""
    returncode = 0

    if core_classes:
        out, rc = run_sbtn(f"scalusJVM/testOnly {' '.join(core_classes)}")
        output += out
        if rc != 0:
            returncode = rc

    if example_classes:
        out, rc = run_sbtn(f"scalusExamplesJVM/testOnly {' '.join(example_classes)}")
        output += out
        if rc != 0:
            returncode = rc

    return output, returncode


# ── Main ────────────────────────────────────────────────────────────────


def main():
    dry_run = "--dry" in sys.argv

    all_exunits: dict[tuple[int, int], tuple[int, int]] = {}
    all_sizes: list = []

    # Step 1: Run sbtn quick
    print("\n[1/4] Running sbtn quick...")
    output, rc = run_sbtn("quick")

    if rc == 0:
        print("  All tests passed! Nothing to update.")
        return 0

    # Step 2: Iteratively fix failures
    print(f"\n[2/4] Fixing budget mismatches (up to {MAX_ITERATIONS} iterations)...")
    failing_classes: list[str] = []
    total_updated_files: set[str] = set()

    for iteration in range(MAX_ITERATIONS):
        exunits_map, size_mismatches = parse_failures(output)
        new_classes = parse_failing_classes(output)

        if new_classes:
            failing_classes = new_classes

        n_ex = len(exunits_map)
        n_sz = len(size_mismatches)
        print(f"\n  --- Iteration {iteration + 1} ---")
        print(f"  Budget mismatches: {n_ex} ExUnits, {n_sz} sizes")
        if failing_classes:
            print(f"  Failing: {', '.join(failing_classes)}")

        if not exunits_map and not size_mismatches:
            if failing_classes:
                print("  No budget-related mismatches found, but tests still failing.")
                print("  This may be a non-budget test failure.")
            break

        all_exunits.update(exunits_map)
        all_sizes.extend(size_mismatches)

        if dry_run:
            print("  (dry run — not updating files)")
            break

        # Apply replacements
        updated = replace_exunits(exunits_map)
        updated += replace_sizes(size_mismatches)
        updated = list(dict.fromkeys(updated))

        if updated:
            for f in updated:
                print(f"    Updated: {f}")
            total_updated_files.update(updated)
        else:
            print("  WARNING: No files were updated despite mismatches found.")
            print("  Unrecognized ExUnits format in test files?")
            break

        if not failing_classes:
            print("  Could not determine failing test classes. Running sbtn quick...")
            output, rc = run_sbtn("quick")
        else:
            output, rc = run_specific_tests(failing_classes)

        if rc == 0:
            print("  All previously-failing tests now pass!")
            break
    else:
        print(f"\n  WARNING: Did not converge after {MAX_ITERATIONS} iterations")

    # Step 3: Report statistics
    print(f"\n[3/4] Budget change statistics ({len(all_exunits)} total changes):")
    report_statistics(all_exunits, all_sizes)

    if dry_run:
        print(f"\n  Files that would be updated: {len(total_updated_files)}")
        return 0

    # Step 4: Final verification
    print("\n[4/4] Final verification with sbtn quick...")
    output, rc = run_sbtn("quick")

    if rc == 0:
        print("  All tests passed!")
        print(f"\n  Updated {len(total_updated_files)} files:")
        for f in sorted(total_updated_files):
            print(f"    {f}")
        return 0

    # If still failing, try one more iteration cycle on the full suite
    exunits_map, size_mismatches = parse_failures(output)
    new_classes = parse_failing_classes(output)

    if exunits_map or size_mismatches:
        print(f"  Found {len(exunits_map)} more ExUnits mismatches from full test suite.")
        print("  Running additional fix iterations...")

        for iteration in range(MAX_ITERATIONS):
            all_exunits.update(exunits_map)
            all_sizes.extend(size_mismatches)

            updated = replace_exunits(exunits_map)
            updated += replace_sizes(size_mismatches)
            updated = list(dict.fromkeys(updated))

            if updated:
                for f in updated:
                    print(f"    Updated: {f}")
                total_updated_files.update(updated)
            else:
                break

            if new_classes:
                output, rc = run_specific_tests(new_classes)
            else:
                output, rc = run_sbtn("quick")

            if rc == 0:
                break

            exunits_map, size_mismatches = parse_failures(output)
            new_classes = parse_failing_classes(output)

            if not exunits_map and not size_mismatches:
                break

        # Final check
        output, rc = run_sbtn("quick")
        if rc == 0:
            print("\n  All tests passed!")
            print(f"\n  Total statistics ({len(all_exunits)} changes):")
            report_statistics(all_exunits, all_sizes)
            print(f"\n  Updated {len(total_updated_files)} files:")
            for f in sorted(total_updated_files):
                print(f"    {f}")
            return 0

    new_classes = parse_failing_classes(output)
    print("  Some tests still failing after all iterations:")
    for cls in new_classes:
        print(f"    {cls}")
    print(f"\n  Total statistics ({len(all_exunits)} changes):")
    report_statistics(all_exunits, all_sizes)
    return 1


if __name__ == "__main__":
    sys.exit(main())
