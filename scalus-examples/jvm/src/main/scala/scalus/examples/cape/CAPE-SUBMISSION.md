# CAPE Submission Guide

How to submit Scalus benchmarks to [UPLC-CAPE](https://github.com/IntersectMBO/UPLC-CAPE).

## Overview

CAPE compares smart contract compilers (Scalus, Aiken, Plinth, Plutarch, plu-ts) across
standardized benchmarks. Each submission provides a compiled `.uplc` file and metadata.
CI verifies correctness against `cape-tests.json` and measures execution costs.

Live results: https://intersectmbo.github.io/UPLC-CAPE/

## Scenarios

| Scenario                    | Mode  | Type       | Description                         |
|-----------------------------|-------|------------|-------------------------------------|
| `factorial`                 | open  | Synthetic  | Any algorithm allowed               |
| `factorial_naive_recursion` | fixed | Synthetic  | Prescribed naive recursion          |
| `fibonacci`                 | open  | Synthetic  | Any algorithm allowed               |
| `fibonacci_naive_recursion` | fixed | Synthetic  | Prescribed naive recursion          |
| `ecd`                       | fixed | Synthetic  | Prescribed Euclidean GCD            |
| `two_party_escrow`          | open  | Validator  | Real-world smart contract           |

**Fixed mode**: must implement the exact prescribed algorithm. No tail-call optimization,
iterative loops, or memoization beyond what the compiler does automatically.

**Open mode**: complete freedom in algorithm and optimization approach.

## Scalus Source Code

All CAPE benchmark implementations live in this directory:

```
scalus-examples/jvm/src/main/scala/scalus/examples/cape/
  factorial/
    FactorialBase.scala      # @Compile naive recursion (fixed mode)
    FactorialOpen.scala       # Hand-crafted UPLC (open mode)
    FactorialContract.scala   # Compilation + @main for CAPE submission
  fibonacci/
    FibonacciBase.scala       # @Compile naive recursion (fixed mode)
    FibonacciOpen.scala       # Hand-crafted UPLC (open mode)
    FibonacciContract.scala   # Compilation + @main for CAPE submission
  twopartyescrow/
    TwoPartyEscrowValidator.scala  # @Compile validator
    TwoPartyEscrowContract.scala   # Compilation + @main for CAPE submission
```

Tests and test data:
```
scalus-examples/jvm/src/test/resources/cape/
  factorial/cape-tests.json
  fibonacci/cape-tests.json
  two_party_escrow/cape-tests.json
scalus-examples/jvm/src/test/scala/scalus/examples/cape/
  factorial/FactorialCapeTest.scala
  fibonacci/FibonacciCapeTest.scala
  twopartyescrow/TwoPartyEscrowCapeTest.scala
```

## Step-by-Step Submission

### 1. Ensure tests pass

```sh
sbtn scalusExamplesJVM/test
```

### 2. Generate UPLC files

Each contract has a `@main` method that writes `.uplc` and `metadata.json` to `cape-submissions/`:

```sh
# Factorial (both base and open)
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.factorial.compileFactorial"

# Fibonacci (both base and open)
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.fibonacci.compileFibonacci"

# Two-Party Escrow
sbtn "scalusExamplesJVM/runMain scalus.examples.cape.twopartyescrow.compileTwoPartyEscrow"
```

This creates files under `cape-submissions/` in the project root:
```
cape-submissions/
  factorial/
    factorial.uplc
    metadata.json
  factorial_naive_recursion/
    factorial_naive_recursion.uplc
    metadata.json
  fibonacci/
    fibonacci.uplc
    metadata.json
  fibonacci_naive_recursion/
    fibonacci_naive_recursion.uplc
    metadata.json
  two_party_escrow/
    two_party_escrow.uplc
    metadata.json
```

### 3. Fork and clone UPLC-CAPE

```sh
gh repo fork IntersectMBO/UPLC-CAPE --clone
cd UPLC-CAPE
```

### 4. Create submission directories

Directory naming convention: `{Compiler}_{Version}_{Handle}[_{variant}]`

```sh
# Example for Scalus version 0.15.1, handle "nau"
VERSION="0.15.1"
HANDLE="nau"

for scenario in factorial factorial_naive_recursion fibonacci fibonacci_naive_recursion two_party_escrow; do
  mkdir -p "submissions/$scenario/Scalus_${VERSION}_${HANDLE}"
done
```

### 5. Copy files into submission directories

```sh
SCALUS_ROOT=/path/to/scalus2

for scenario in factorial factorial_naive_recursion fibonacci fibonacci_naive_recursion two_party_escrow; do
  DEST="submissions/$scenario/Scalus_${VERSION}_${HANDLE}"
  cp "$SCALUS_ROOT/cape-submissions/$scenario/"* "$DEST/"
done
```

### 6. Update metadata.json

The `@main` methods generate a simplified metadata.json. For CAPE submission,
the file must conform to the [metadata schema](https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/TEMPLATE/metadata.schema.json).

Required format:

```json
{
  "compiler": {
    "name": "Scalus",
    "version": "0.15.1",
    "commit_hash": "<40-char git commit hash of Scalus>"
  },
  "compilation_config": {
    "optimization_level": "release",
    "target": "uplc",
    "flags": ["Options.release"]
  },
  "contributors": [
    {
      "name": "Your Name",
      "organization": "Your Org",
      "contact": "@github_handle"
    }
  ],
  "submission": {
    "date": "2025-12-01T00:00:00Z",
    "source_available": true,
    "source_repository": "https://github.com/AncientMariner/scalus2",
    "source_commit_hash": "<40-char git commit hash of scalus2>",
    "implementation_notes": "Scalus @Compile with Options.release (all optimizations, no traces)"
  }
}
```

For open-mode hand-crafted UPLC submissions, adjust `implementation_notes`:
```
"implementation_notes": "Hand-crafted UPLC with pfix combinator and CaseConstrApply optimization"
```

### 7. Write README.md

Each submission directory needs a `README.md`. Use the
[template](https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/TEMPLATE/benchmark-README-template.md).

Key sections:
- Implementation approach (compiled from Scala / hand-crafted UPLC)
- Link to source files in the scalus2 repository
- Build instructions (reference this guide)

### 8. Verify locally (requires Nix)

```sh
# Verify a single submission
cape submission verify submissions/factorial/Scalus_${VERSION}_${HANDLE}

# Verify all submissions
cape submission verify --all --verbose

# Measure performance
cape submission measure submissions/factorial/Scalus_${VERSION}_${HANDLE}
```

The `measure` command generates `metrics.json` automatically.

### 9. Submit PR

```sh
git add submissions/
git commit -m "Add Scalus ${VERSION} submissions"
git push origin main
gh pr create --title "Add Scalus ${VERSION} benchmarks" --body "Updated Scalus CAPE submissions"
```

CI will:
1. Validate `.uplc` files parse correctly
2. Run all `cape-tests.json` test cases against submissions
3. Validate `metadata.json` against schema
4. Generate a preview report at `https://intersectmbo.github.io/UPLC-CAPE/pr-{N}/`

## Adding a New Scenario

When CAPE adds a new scenario (e.g., `ecd`, `streaming_payments`):

1. Get `cape-tests.json` from `scenarios/{scenario}/cape-tests.json` in the UPLC-CAPE repo
2. Copy it to `scalus-examples/jvm/src/test/resources/cape/{scenario}/cape-tests.json`
3. Create source files under `scalus-examples/jvm/src/main/scala/scalus/examples/cape/{scenario}/`:
   - `{Name}Base.scala` (for fixed mode) - `@Compile` with prescribed algorithm
   - `{Name}Open.scala` (for open mode) - optimized implementation
   - `{Name}Contract.scala` - compilation and `@main` for CAPE output
4. Create test file under `scalus-examples/jvm/src/test/scala/scalus/examples/cape/{scenario}/`
5. Run tests, capture budgets, update expected values
6. Follow the submission steps above

## Test Format

Synthetic benchmarks (factorial, fibonacci) use simple UPLC integer inputs/outputs:
```json
{
  "inputs": [{"type": "uplc", "value": "(con integer 10)"}],
  "expected": {"type": "value", "content": "(con integer 3628800)"}
}
```

Validator benchmarks (two_party_escrow) use ScriptContext with patches:
```json
{
  "inputs": [{"type": "script_context", "script_context": {"baseline": "spending", "patches": [...]}}],
  "expected": {"type": "value", "content": "(con unit ())"}
}
```

For error cases: `"expected": {"type": "error"}`.
