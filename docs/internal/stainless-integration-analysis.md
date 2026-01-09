# EPFL Stainless Integration Feasibility Analysis for Scalus

## Executive Summary

This document analyzes the feasibility of using EPFL Stainless for formal verification of Scalus
smart contracts. **The integration is feasible**, with Stainless v0.9.8.7 supporting Scala 3.3.3 (
compatible with Scalus's 3.3.7).

## What is Stainless?

[Stainless](https://github.com/epfl-lara/stainless) is EPFL LARA's verification framework for
higher-order Scala programs that:

- Takes Scala code with specifications (annotations) as input
- Proves or disproves their validity using SMT solvers (Z3, cvc5, Princess)
- Checks termination of recursive functions
- Works with a [Pure Scala subset](https://epfl-lara.github.io/stainless/purescala.html)

### Key Stainless Features

| Feature         | Syntax                             | Description                            |
|-----------------|------------------------------------|----------------------------------------|
| Preconditions   | `require(condition)`               | Must hold when function is called      |
| Postconditions  | `ensuring(result => condition)`    | Must hold after function returns       |
| ADT Invariants  | `@invariant`                       | Case class properties that always hold |
| Loop Invariants | `(while(...){...}) invariant(...)` | Loop preservation conditions           |
| Ghost Code      | `@ghost`                           | Verification-only code, not compiled   |
| Termination     | Automatic                          | Checks recursive functions terminate   |

## Scala Version Compatibility

| Component                   | Scala Version | Status                  |
|-----------------------------|---------------|-------------------------|
| **Scalus**                  | 3.3.7 (LTS)   | Current                 |
| **Stainless v0.9.8.7**      | 3.3.3         | ✅ Compatible            |
| Stainless v0.9.9.0          | 3.5.2         | Requires Scalus upgrade |
| Stainless v0.9.9.2 (latest) | 3.7.2         | Requires Scalus upgrade |

**Recommendation**: Use Stainless v0.9.8.7 for PoC - no Scala version changes needed.

## Language Feature Compatibility

| Feature            | Scalus                | Stainless                   | Compatible?       |
|--------------------|-----------------------|-----------------------------|-------------------|
| Pure functions     | Yes                   | Required                    | ✅                 |
| Case classes/ADTs  | Yes                   | Yes                         | ✅                 |
| Enums              | Yes (`derives`)       | Yes (limited)               | ⚠️                |
| Pattern matching   | Yes                   | Yes                         | ✅                 |
| Recursion          | Yes                   | Yes (checked)               | ✅                 |
| `inline` methods   | Heavy use             | Not supported               | ❌                 |
| `BigInt`           | Yes                   | Yes                         | ✅                 |
| Custom collections | `scalus.prelude.List` | `stainless.collection.List` | ⚠️ Mapping needed |
| Extension methods  | Yes                   | Limited                     | ⚠️                |
| Macros             | `@Compile`            | Not supported               | ❌                 |
| Type derivation    | `derives FromData`    | Not supported               | ❌                 |

## Pros of Using Stainless

### 1. Strong Verification Guarantees

- Proves absence of runtime errors
- Verifies preconditions/postconditions hold
- Checks termination (crucial for on-chain execution budgets)

### 2. Natural Fit for Smart Contracts

- Pure functional subset matches Plutus constraints
- ADT support aligns with Scalus data types
- [EPFL smart contracts project](https://github.com/epfl-lara/smart) for Ethereum

### 3. Existing Scalus Patterns Are Verification-Friendly

```scala
// Scalus already uses require() - same as Stainless!
require(tx.isSignedBy(config.committer), "Unsigned")
require(sha3_256(preimage) == config.image, "Invalid preimage")
```

### 4. Catches Critical Bugs

- Integer overflow/underflow in calculations
- Missing case handling in pattern matches
- Unreachable code detection
- Infinite recursion

### 5. Documentation via Specifications

- Postconditions serve as executable documentation
- Invariants formalize contract assumptions

## Cons of Using Stainless

### 1. Scala Version Trade-off

- v0.9.8.7 (Scala 3.3.3) compatible but older
- Latest features require Scala 3.5.0+ upgrade
- Scalus upgrade affects compiler plugin, JS/Native builds

### 2. Subset Restrictions

- No `inline` methods (Scalus uses extensively)
- No macros (Scalus's `@Compile` relies on them)
- No custom extractors with side effects

### 3. Separate Toolchain

- Cannot run Stainless on `@Compile`-annotated code directly
- Requires maintaining verification-friendly versions
- CI/CD complexity increases

### 4. Missing Plutus Semantics

- No models for Cardano builtins (`sha3_256`, `blake2b_224`)
- No models for `Data` type and serialization
- No models for `Value`, `TxInfo`, etc.

### 5. Learning Curve

- Developers must learn specification patterns
- Writing good specifications is non-trivial
- False positives require workarounds

## Integration Approaches

### Approach 1: Direct Integration (High Effort)

Upgrade Scalus to Scala 3.5.0+ and make contracts directly verifiable.

**Challenges**:

- Massive breaking change
- `inline` removal breaks compilation model
- Plugin needs rewrite

### Approach 2: Dual Source (Moderate Effort)

Maintain verification-friendly source alongside compilable source.

```
src/
  main/scala/         # Compilable Scalus code
  verification/scala/ # Stainless-friendly specifications
```

### Approach 3: SIR-Level Verification (Recommended)

Use Scalus's SIR as the verification target:

1. Compile Scala → SIR (existing pipeline)
2. Translate SIR → Stainless trees (new)
3. Run Stainless verification
4. Report results mapped to original source

### Approach 4: Custom Verification Tool (Long-term)

Build verification tool using Inox (Stainless's backend) directly.

## PoC Plan: Verified Linear Vesting

### Target Function (from VestingValidator.scala)

```scala
def linearVesting(vestingDatum: Config, timestamp: BigInt): BigInt = {
  val min = vestingDatum.startTimestamp
  val max = vestingDatum.startTimestamp + vestingDatum.duration
  if timestamp < min then 0
  else if timestamp >= max then vestingDatum.initialAmount
  else vestingDatum.initialAmount * (timestamp - vestingDatum.startTimestamp) / vestingDatum.duration
}
```

### Properties to Verify

| Property      | Description                                                |
|---------------|------------------------------------------------------------|
| Bounds        | `0 <= result <= initialAmount`                             |
| Zero at start | `linearVesting(config, startTimestamp) == 0`               |
| Full at end   | `linearVesting(config, start + duration) == initialAmount` |
| Monotonic     | `t1 <= t2 => vesting(t1) <= vesting(t2)`                   |

### PoC Project Structure

```
scalus-stainless-poc/
├── build.sbt                         # Scala 3.3.3, Stainless v0.9.8.7
├── project/
│   ├── build.properties              # sbt 1.9.x
│   └── plugins.sbt                   # sbt-stainless plugin
├── src/main/scala/scalus/stainless/
│   ├── Types.scala                   # VestingConfig case class
│   └── LinearVesting.scala           # Verified function
├── stainless.conf                    # Solver configuration
└── README.md                         # Results
```

### build.sbt

```scala
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .enablePlugins(StainlessPlugin)
  .settings(
    name := "scalus-stainless-poc",
    libraryDependencies += "ch.epfl.lara" %% "stainless-library" % "0.9.8.7"
  )
```

### project/plugins.sbt

```scala
addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % "0.9.8.7")
```

### Verified Code Example

```scala
package scalus.stainless

import stainless.lang._
import stainless.annotation._

case class VestingConfig(
                          beneficiary: BigInt,
                          startTimestamp: BigInt,
                          duration: BigInt,
                          initialAmount: BigInt
                        ) {
  require(duration > BigInt(0))
  require(initialAmount >= BigInt(0))
}

object LinearVesting {
  def linearVesting(config: VestingConfig, timestamp: BigInt): BigInt = {
    require(config.duration > BigInt(0))
    require(config.initialAmount >= BigInt(0))

    val min = config.startTimestamp
    val max = config.startTimestamp + config.duration

    if (timestamp < min) BigInt(0)
    else if (timestamp >= max) config.initialAmount
    else config.initialAmount * (timestamp - config.startTimestamp) / config.duration
  } ensuring { result =>
    result >= BigInt(0) && result <= config.initialAmount
  }

  @pure
  def atStartIsZero(config: VestingConfig): Boolean = {
    require(config.duration > BigInt(0))
    require(config.initialAmount >= BigInt(0))
    linearVesting(config, config.startTimestamp) == BigInt(0)
  }.holds

  @pure
  def atEndIsFull(config: VestingConfig): Boolean = {
    require(config.duration > BigInt(0))
    require(config.initialAmount >= BigInt(0))
    linearVesting(config, config.startTimestamp + config.duration) == config.initialAmount
  }.holds

  @pure
  def monotonic(config: VestingConfig, t1: BigInt, t2: BigInt): Boolean = {
    require(config.duration > BigInt(0))
    require(config.initialAmount >= BigInt(0))
    require(t1 <= t2)
    linearVesting(config, t1) <= linearVesting(config, t2)
  }.holds
}
```

## Scalus Architecture Summary

### Compilation Pipeline

```
Scala Source → SIR → UPLC → Plutus Script (V1/V2/V3)
```

### Key Files

| Component       | Location                                                              |
|-----------------|-----------------------------------------------------------------------|
| SIR Definition  | `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIR.scala`     |
| SIR Types       | `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRType.scala` |
| Compiler Plugin | `scalus-plugin/src/main/scala/scalus/compiler/plugin/`                |
| Prelude         | `scalus-core/shared/src/main/scala/scalus/prelude/`                   |
| Vesting Example | `scalus-examples/jvm/src/main/scala/scalus/examples/vesting/`         |
| HTLC Example    | `scalus-examples/shared/src/main/scala/scalus/examples/htlc/`         |

### Typical Validator Pattern

```scala
@Compile
object MyValidator extends Validator {
  inline override def spend(
                             datum: Option[Data],
                             redeemer: Data,
                             tx: TxInfo,
                             ownRef: TxOutRef
                           ): Unit = {
    val config = datum.getOrFail("No datum").to[Config]
    require(tx.isSignedBy(config.owner), "Not signed")
    // ... validation logic
  }
}
```

## Recommendations

### Short-term (PoC)

1. Create standalone Stainless project with Scala 3.3.3
2. Verify `linearVesting` with bounds/monotonicity properties
3. Document patterns and integration effort

### Medium-term

1. Develop Plutus builtin models for Stainless
2. Create verification library with common lemmas
3. Explore SIR-level verification translator

### Long-term

1. Build SIR→Stainless translation layer
2. Evaluate Scala 3.5+ upgrade path for latest Stainless
3. Integrate verification into CI pipeline

## Conclusion

Stainless integration is **feasible** for Scalus smart contracts:

- **Scala compatibility**: v0.9.8.7 works with Scala 3.3.3 (Scalus uses 3.3.7)
- **Pure functional style**: Scalus contracts align well with Stainless requirements
- **Existing patterns**: `require()` is already used, matching Stainless syntax

**Main challenges**:

1. Heavy `inline` usage incompatible with Stainless
2. Missing Plutus builtin models
3. Cannot verify `@Compile`-annotated code directly

**Recommended approach**: Start with standalone PoC, then develop SIR-level verification for deeper
integration.

## References

- [Stainless GitHub](https://github.com/epfl-lara/stainless)
- [Stainless Documentation](https://epfl-lara.github.io/stainless/)
- [Pure Scala Subset](https://epfl-lara.github.io/stainless/purescala.html)
- [Stainless Smart Contracts](https://github.com/epfl-lara/smart)
- [Verification Conditions](https://epfl-lara.github.io/stainless/verification.html)
- [Stainless Releases](https://github.com/epfl-lara/stainless/releases)