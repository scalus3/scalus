Source: https://scalus.org/docs/get-started/migrating-to-1.0

# Migrating from 0.18 to 1.0

Scalus 1.0.0-M1 is the first milestone of the 1.0 line. Artifact coordinates are unchanged
(`org.scalus:scalus_3`, `org.scalus:scalus-cardano-ledger_3`, ...) – update the version and the
compiler plugin together:

```scala
libraryDependencies += "org.scalus" %% "scalus" % "1.0.0"
addCompilerPlugin("org.scalus" %% "scalus-plugin" % "1.0.0")
```

## Stability promise

From 1.0.0-M1 onward, `scalus-core`, `scalus-cardano-ledger` and
`scalus-bloxbean-cardano-client-lib` are the stable surface, checked with MiMa on every build.
Most APIs will stay binary compatible across the 1.x line; some parts will likely still have
breaking changes in 1.x releases, always behind a deprecation cycle. `scalus-testkit` is
best-effort; `*.internal` packages and compiler internals carry no compatibility promise.

## On-chain behavior changes

These change generated code, so **every script hash changes** when you recompile.

- **Protocol version 11 (van Rossem) is the default** compile and evaluation target. Budgets drop
  roughly 30% and scripts shrink roughly 20%. To reproduce pre-PV11 output for an already
  deployed contract, use `Options.plomin` or set
  `targetProtocolVersion = MajorProtocolVersion.plominPV`.
- **`BigInt./` and `BigInt.%` now match Scala semantics** (truncated division,
  `quotientInteger`/`remainderInteger`). Previously `/` compiled to floor division
  (`divideInteger`), which differs for negative operands.
- **Hand-written `Eq` instances are rejected** in on-chain code: `===` always compiles to
  structural equality and never calls the instance body. Use `Eq.derived` for case classes,
  enums and sealed traits, or wrap a structural comparison in `Eq.structural(...)`.

## Removed API

| Removed | Use instead |
|---|---|
| `scalus.compiler.intrinsics.ReprTag` | `scalus.compiler.UplcRepresentation` |
| `scalus.ScalusDebug` | `scalus.compiler.ScalusDebug` |
| `scalus.CompileDerivations` | `scalus.compiler.CompileDerivations` |
| `scalus.serialization.flat` `w7l` | `word7Bytes` |
| `scalus.bloxbean.TxEvaluator` | `ScalusTransactionEvaluator` or `scalus.cardano.ledger.PlutusScriptEvaluator` |
| `Builtins.multiIndexArray` | `Builtins.indexArray` per element (it was a Scalus invention no Plutus release implements) |

## Binary compatibility notes

- **`SIRVersion` is 6.0**: precompiled `.sir` artifacts must be recompiled with the 1.0 compiler
  plugin.
- **`scalus.serialization.flat`** was converted from a package object to top-level definitions.
  Scala sources compile unchanged; recompile anything that linked against the old binary names.
- **Provider traits changed**: `BlockchainReader.findUtxo`/`findUtxos` moved onto the
  effect-polymorphic `BlockchainReaderTF`, which gained an abstract `mapF`; `EmulatorBase` gained
  the applied-transaction log (`appliedTxLog`, `appliedTxIndex`, `clearAppliedTxs`). Custom
  provider or emulator implementations must implement the new members; users of the built-in
  providers are unaffected.
- `ImmutableEmulator` and `Emulator` constructors gained parameters (evaluator mode, transaction
  log) – use the factory methods rather than the constructors.
