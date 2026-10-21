# Coverage baseline and gate

Spec: `[P0-4]` to `[P0-9]`, `[CG-1]` to `[CG-4]` in
`docs/superpowers/specs/2026-10-20-test-refactoring-design.md`. Findings: F-9, F-10.

## Constraints

- **Scala 3.3.8 (the CI default) works.** It needs `coverage-exclude.sbt`, which
  `scripts/coverage-exclude.sh` generates (git-ignored). Without it, the probes break `@Compile`
  code (F-9).
- **Off-chain code only.** The 146 excluded files hold `@Compile` code or plugin intrinsics. Their
  on-chain logic runs as UPLC in the CEK machine and is invisible to scoverage anyway (`[P0-7]`).
- **Opt-in.** Push CI never runs this (`[P0-9]`). The plugin is inert without the `coverage` command.
- **One-shot sbt.** The scripts use a one-shot `sbt`; never run them while an `sbtn` server is live
  in the same checkout.

## Produce the reports

```bash
docs/internal/test-refactor/scripts/coverage-run.sh /tmp/cov-run            # all of jvm/test
docs/internal/test-refactor/scripts/coverage-run.sh /tmp/cov-run scalusJVM/test   # one project
```

The script regenerates `coverage-exclude.sbt`, deletes old measurements (they accumulate across
runs), runs the tests with `coverage`, runs `coverageReport`, and writes the uncovered set to the
given directory.

## Baseline

`coverage-run.sh docs/internal/test-refactor/baseline` writes it directly.

- `baseline/summary.csv` – statements and branches per module and package.
- `baseline/uncovered.tsv.gz` – every statement no test invoked, keyed by source, start, end.
  P2 agents read it for coverage gaps (check C2).

## Gate

After a unit's change, run `coverage-run.sh` (any output directory) to refresh the reports, then:

```bash
python3 docs/internal/test-refactor/scripts/coverage.py gate . docs/internal/test-refactor/baseline \
    scalus.cardano.ledger scalus.cardano.txbuilder   # optional package prefixes
```

It prints each `LOST` statement and exits with 1 if there is any. Verified on 2026-10-20: with 10
rows removed from a copy of the baseline, it reported exactly those 10 statements.

## Flaky statements

Coverage is not fully deterministic: unseeded property tests draw different values on each run.
Two clean runs of the same commit on 2026-10-21 differed in 39 statements, for example `DefaultFun.encode`
(`DefaultFun.scala:1613`) and `Certificate.scriptHashOption` (`Certificate.scala:116`).

- `baseline/flaky.tsv` lists the statements that differed between two baseline runs. The gate
  ignores them.
- Two runs do not find every flaky statement. If the gate reports a loss, run the reports once
  more on the unchanged base commit. A statement that flips there too goes into `flaky.tsv`; it is
  not a loss.

```bash
python3 docs/internal/test-refactor/scripts/coverage.py baseline . /tmp/second-run
python3 docs/internal/test-refactor/scripts/coverage.py flaky docs/internal/test-refactor/baseline /tmp/second-run
```
