# CAPE Submission Guide

How to submit Scalus benchmarks to [UPLC-CAPE](https://github.com/IntersectMBO/UPLC-CAPE).

## Overview

CAPE compares smart contract compilers (Scalus, Aiken, Plinth, Plutarch, plu-ts) across
standardized benchmarks. Each submission provides a compiled `.uplc` file and metadata.
CI verifies correctness against `cape-tests.json` and measures execution costs.

Live results: https://intersectmbo.github.io/UPLC-CAPE/

## Scenarios

| Scenario                     | Mode  | Type       | Description                  |
|------------------------------|-------|------------|-------------------------------|
| `factorial`                  | open  | Synthetic  | Any algorithm allowed        |
| `factorial_naive_recursion`  | fixed | Synthetic  | Prescribed naive recursion   |
| `fibonacci`                  | open  | Synthetic  | Any algorithm allowed        |
| `fibonacci_naive_recursion`  | fixed | Synthetic  | Prescribed naive recursion   |
| `ecd`                        | fixed | Synthetic  | Prescribed Euclidean GCD     |
| `htlc`                       | open  | Validator  | Hashed time-locked contract  |
| `linear_vesting`             | open  | Validator  | Linear token vesting         |
| `two_party_escrow`           | open  | Validator  | Deposit/accept/refund escrow |

**Fixed mode**: must implement the exact prescribed algorithm. No tail-call optimization,
iterative loops, or memoization beyond what the compiler does automatically.

**Open mode**: complete freedom in algorithm and optimization approach.

## Scalus Source Code

All CAPE benchmark implementations live in this directory:

```
scalus-examples/jvm/src/main/scala/scalus/examples/cape/
  CapeScenarios.scala           # Registry of all 8 scenarios: name, program, metadata
  CapeMetadata.scala            # Renders CAPE-compliant metadata.json (schema + source repo)
  GenerateSubmissions.scala     # @main: writes Scalus_<version>_nau dirs into a UPLC-CAPE clone
  CompareWithLeaderboard.scala  # @main: ranks every submission per scenario by total fee
  factorial/
    FactorialBase.scala       # @Compile naive recursion (fixed mode)
    FactorialOpen.scala       # Hand-crafted UPLC (open mode)
    FactorialContract.scala   # Compiles both into Program values
  fibonacci/
    FibonacciBase.scala       # @Compile naive recursion (fixed mode)
    FibonacciOpen.scala       # Hand-crafted UPLC (open mode)
    FibonacciContract.scala   # Compiles both into Program values
  ecd/
    EcdBase.scala              # @Compile prescribed naive recursive GCD (fixed mode)
    EcdContract.scala          # Compiles into a Program value
  htlc/
    HtlcValidator.scala        # @Compile validator
    HtlcContract.scala         # Compiles into a Program value
  linearvesting/
    LinearVestingValidator.scala  # @Compile validator
    LinearVestingContract.scala   # Compiles into a Program value
  twopartyescrow/
    TwoPartyEscrowValidator.scala  # @Compile validator
    TwoPartyEscrowContract.scala   # Compiles into a Program value
```

None of these expose a per-scenario `@main` anymore; `GenerateSubmissions` drives
compilation for all of them through `CapeScenarios.all`.

Tests and test data:
```
scalus-examples/jvm/src/test/resources/cape/
  README.md                            # Provenance: vendored from UPLC-CAPE, commit + schema
  factorial/cape-tests.json
  factorial_naive_recursion/cape-tests.json
  fibonacci/cape-tests.json
  fibonacci_naive_recursion/cape-tests.json
  ecd/cape-tests.json
  htlc/cape-tests.json
  linear_vesting/cape-tests.json
  two_party_escrow/cape-tests.json
scalus-examples/jvm/src/test/scala/scalus/examples/cape/
  CapeTestSuite.scala             # Shared cape-tests.json loader + ScriptContext builder;
                                   # also defines CapeHarness, which runs a case and returns
                                   # the execution budget
  factorial/FactorialCapeTest.scala
  fibonacci/FibonacciCapeTest.scala
  ecd/EcdCapeTest.scala
  htlc/HtlcCapeTest.scala
  linearvesting/LinearVestingCapeTest.scala
  twopartyescrow/TwoPartyEscrowCapeTest.scala
```

## Submission Workflow

### Prerequisites

- [Nix](https://nixos.org/download) installed. `scripts/cape.sh` (in the UPLC-CAPE clone)
  shells out to Nix-provided tooling for `verify`/`measure`.
- A local clone of [UPLC-CAPE](https://github.com/IntersectMBO/UPLC-CAPE), rebased on
  `origin/main`. Fork it first if you don't already have write access:

  ```sh
  gh repo fork IntersectMBO/UPLC-CAPE --clone ../UPLC-CAPE
  cd ../UPLC-CAPE && git fetch origin && git rebase origin/main
  ```

### 1. Ensure tests pass

```sh
sbtn scalusExamplesJVM/test
```

### 2. Run the driver

From the Scalus repo root:

```sh
scripts/cape-submit.sh ../UPLC-CAPE
```

Pass an explicit version as a second argument to override `BuildInfo.version` (e.g. when
running from a worktree whose build version is a `-SNAPSHOT`):

```sh
scripts/cape-submit.sh ../UPLC-CAPE 1.1.0
```

This runs the whole submission in one shot:

1. Generates `submissions/<scenario>/Scalus_<version>_nau/` (`.uplc`, `metadata.json`,
   `README.md`) for all 8 `CapeScenarios.all` entries in the target clone
   (`GenerateSubmissions`).
2. Runs `cape submission verify` and `cape submission measure` on each of the 8 dirs, routing
   `min_plutus_version`-gated dirs through CAPE's preview measure path (see below).
3. Prints a leaderboard ranking every submission per scenario by total fee (CAPE's
   execution fee + Conway reference-script fee) (`CompareWithLeaderboard`).

**The `min_plutus_version` gate**: 7 of the 8 scenarios (all except `factorial` (open)) carry
`min_plutus_version: 1.60.0.0` in their `metadata.json`, because they use PV11/vanRossem-only
features (CIP-153 `Value` builtins and/or case-on-builtins) that CAPE's production evaluator,
pinned to `plutus-core-1.45.0.0`, predates and can't even parse. `cape submission verify`/`measure`
treat that field as a request to route the submission through CAPE's **preview** track instead of
refusing it outright. The driver follows that routing automatically, so those 7 dirs measure and
verify against the preview evaluator, and appear in CAPE's preview report rather than its
production report, until upstream promotes its production evaluator past vanRossem. Only
`factorial` (open) stays PV9-compatible and is measured on the current/production track.

### 3. Review

- Read the printed leaderboard. If Scalus is behind another compiler on a scenario,
  investigate before opening the PR.
- Spot-check a generated `metadata.json`/`README.md`/`metrics.json`.

### 4. Commit and open the PR

```sh
cd ../UPLC-CAPE
git add submissions/
git commit -m "Add Scalus <version> submissions"
git push origin main   # or your fork's branch
gh pr create --title "Add Scalus <version> benchmarks" --body "Updated Scalus CAPE submissions"
```

CI will:
1. Validate `.uplc` files parse correctly
2. Run all `cape-tests.json` test cases against submissions
3. Validate `metadata.json` against schema
4. Generate a preview report at `https://intersectmbo.github.io/UPLC-CAPE/pr-{N}/`

## Adding a New Scenario

When CAPE adds a new scenario (e.g. `streaming_payments`):

1. **Vendor the fixture**: copy `scenarios/{scenario}/cape-tests.json` from the UPLC-CAPE
   clone to `scalus-examples/jvm/src/test/resources/cape/{scenario}/cape-tests.json`, and
   update the commit hash/schema version noted in that directory's `README.md`.
2. **Add the implementation** under
   `scalus-examples/jvm/src/main/scala/scalus/examples/cape/{scenario}/`:
   - Fixed-mode/synthetic scenario: `{Name}Base.scala` – `@Compile` of the prescribed
     algorithm.
   - Validator scenario: `{Name}Validator.scala` – `@Compile` validator.
   - Open-mode scenario (optional, alongside a `Base`): `{Name}Open.scala` – hand-crafted
     UPLC.
   - `{Name}Contract.scala` – compiles the above into `Program` values. No `@main` needed.
3. **Add a harness test** at
   `scalus-examples/jvm/src/test/scala/scalus/examples/cape/{scenario}/{Name}CapeTest.scala`:
   load the fixture with `CapeTestSuite.load("/cape/{scenario}/cape-tests.json")`, run each
   case with `CapeHarness.run(program, c)`, and pin the resulting `ExUnits` per case. See
   `ecd/EcdCapeTest.scala` for the pattern.
4. **Register the scenario** in `CapeScenarios.all` (`CapeScenarios.scala`): add a
   `CapeScenario` with the scenario `name`, `program`, `implementationNotes`,
   `readmeApproach`, and `sourceSubdir`.
   - Decide `minPlutusVersion` (`Some("1.60.0.0")` if the compiled output uses PV11 features -
     CIP-153 `Value` builtins and/or case-on-builtins; `None` if it's PV9-compatible), and update
     `CapeMetadataTest`'s gated/ungated sets to match.
5. Run `sbtn scalusExamplesJVM/test`, capture budgets, and update the expected `ExUnits`
   in the new test.
6. Follow the submission workflow above.

## Test Format

Fixtures are vendored 1:1 from UPLC-CAPE's `scenarios/{scenario}/cape-tests.json` (see
`scalus-examples/jvm/src/test/resources/cape/README.md` for the pinned commit).

Synthetic benchmarks (factorial, fibonacci, ecd) use simple UPLC integer inputs/outputs:
```json
{
  "inputs": [{"type": "uplc", "value": "(con integer 10)"}],
  "expected": {"type": "value", "content": "(con integer 3628800)"}
}
```

Validator benchmarks (htlc, linear_vesting, two_party_escrow) use ScriptContext with
patches:
```json
{
  "inputs": [{"type": "script_context", "script_context": {"baseline": "spending", "patches": [...]}}],
  "expected": {"type": "value", "content": "(con unit ())"}
}
```

For error cases: `"expected": {"type": "error"}`.
