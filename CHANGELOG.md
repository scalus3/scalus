# Changelog

## 0.14.0 (2025-12-19)

### Added

- Plutus V4 language support with Case instructions on boolean, integer, list, Data, and Pair types
- CIP-0138 Plutus Core Builtin Type Array implementation
- CIP-0156 Plutus Core Builtin Function `multiIndexArray`
- JIT compiler with 6x speedup over CEK interpreter
- `TxBuilder.complete` with iterative balancing, automatic UTxO selection, and async support
- `TxBuilder` staking and governance implementation with delegation, DRep registration, and voting
- `Data` pattern matching support
- `copy` method support for case classes in Scalus scripts
- `dropList` builtin implementation with saturating CostingInteger arithmetic
- cross-platform Ed25519 signing with type-safe opaque types (VerificationKey, Signature, SigningKey)
- `addr` and `stake` string interpolators for bech32 address parsing
- `ByteString.utf8` string interpolator support in SIR compiler
- `Blueprint.fromJson(InputStream)` overload for efficient parsing
- `Transaction.utxos` method for transaction chaining
- `Emulator` for transaction testing
- `StrictIf` optimization for if-then-else expressions
- `preprod` and `preview` constants in `CardanoInfo`
- SIR function application operator `$` with comprehensive test suite
- collateral with native tokens support per Babbage specification
- delayed redeemer support for minting operations
- `TermSanitizer` to sanitize variable names in UPLC terms
- field selection for Data subtypes (I, B, List, Map, Constr)
- `toLedgerValue` method for Value conversion
- `SortedMap.from` and `Value.unsafeFromSortedMap` methods
- `MultiAsset.onlyPositive` method
- Plutus conformance tests updated from 1.40.0.0 to 1.53.0.0
- comprehensive ledger rules validation tests

### Changed

- **BREAKING**: `mint` API unified - use `mint(script, assets, redeemer)` for attached scripts,
  `mint(policyId, assets, redeemer)` for reference scripts
- **BREAKING**: `Provider` consolidated to async-only, `AsyncProvider` renamed to `Provider`
- **BREAKING**: `scalus.sir` package moved to `scalus.compiler.sir`
- **BREAKING**: `builtin.Data` uses `scalus.prelude.List` instead of `scala.collection.immutable.List`
- **BREAKING**: `ChangeOutputDiffHandler` now accepts `Value` instead of `Coin`
- renamed `NodeEmulator` to `Emulator` and moved to scalus-cardano-ledger
- renamed `AsyncBlockfrostProvider` to `BlockfrostProvider`
- renamed `LucidEvolutionKeyPair` to `LucidKeyPair`
- `TxBuilder` async methods renamed: `completeAsync` to `complete` (returns `Future[TxBuilder]`)
- deprecated `Environment` type alias in favor of `CardanoInfo`
- deprecated `PaymentBuilder` in favor of `TxBuilder`
- deprecated `mintAndAttach` in favor of `mint(script, assets, redeemer)`
- deprecated `SpendWithDelayedRedeemer` in favor of `Spend` with delayed redeemer witness
- migrated sttp from v3 to v4
- updated jsoniter-scala-core to 2.38.6
- updated bcprov-jdk18on to 1.83
- updated pprint to 0.9.6
- optimized CEK machine with mutable HashMap for builtins initialization
- optimized list transformations using view for better performance
- JIT compiler eliminates VCon allocations for cost-calculating builtins

### Fixed

- floor division edge case on Scala.js for `Long.MinValue / -1`
- redeemer index recalculation after adding inputs in `TxBuilder.complete`
- collateral calculation precision in `calculateRequiredCollateral`
- inline datum handling for PlutusV1 scripts
- deterministic ordering in topological sort during SIR lowering
- wildcard case generation for product types in pattern matching
- `TransactionSigner` preserves existing witnesses when signing
- delayed redeemers recomputed after `complete` adds inputs
- `Interop.toScalusData` preserves order in Map
- zero-ada change output kept if it contains assets
- `SIRType.show` deterministic by removing hashcodes from type variable display
- collateral index output calculation in `PlutusScriptsTransactionMutator`
- various ledger rule validators: `ValueNotConservedUTxO`, `MissingRequiredDatums`,
  `MissingOrExtraScriptHashes`, `FeesOk`, `OutputsHaveNotEnoughCoins`
- `balanceFeeAndChange` correctly handling modifications that invalidate result
- bootstrap address attributes size validation in transactions

## 0.13.0 (2025-10-22)

### Added

- new pattern matching based on decision trees
- primitive constant pattern matching in `SimpleSirToUplcLowering` and `SirToUplc110Lowering`
  backends
- LazyVal transformation to float lazy lets closer to their usage points
- lambda barrier for let-floating optimization to prevent work duplication
- `evalPlutusScripts` to JScalus for JavaScript platform
- LinkedList pattern implementation with initialization and de-initialization
- PlutusScriptEvaluator benchmark for performance testing
- Fibonacci lookup implementation using pre-packed ByteString
- UTXO indexers using delayed redeemers
- `SortedMap.getOrFail` accepts custom error messages
- Ledger Rules Framework: TraitObjectScanner for automatic discovery of rule implementations
- `compileWithOptions` method accepting explicit compiler options

### Changed

- updated Scala version to 3.3.7
- **BREAKING**: repackaged txbuilder from `scalus.cardano.ledger.txbuilder` to
  `scalus.cardano.txbuilder`
- **BREAKING**: `ProtocolParams.costModels` type changed to `CostModels`
- **BREAKING**: removed `semanticVariant` from `MachineParams`
- moved `SlotConfig` to scalus-core
- **BREAKING**: replaced `bloxbean.SlotConfig` with `cardano.ledger.SlotConfig`
- renamed `UTxO` to `Utxos` for consistency
- updated jsoniter-scala-core to 2.38.3
- inheritance via inline override for validators
- updated builtin cost models to use CardanoInfo (reduces scalus-cardano-ledger-opt-bundle.js by ~
  100k)
- renamed `builtinsMeaning` to `getBuiltinRuntime` in Cek and PlutusVM
- pre-compute TxInfo for Plutus versions to optimize redeemer evaluation
- replaced ProtocolParams instantiation with CardanoInfo for consistency
- consolidated compile method handling in compiler plugin
- made validator helper methods public to support inheritance
- replaced `BuiltinsMeaning` with `CardanoBuiltins`
- removed deprecated methods and cleaned up code
- improved error diagnostics for wildcard case positioning
- enhanced SIRLinker debug logging and error handling
- modified base validator traits to use inline abstract methods
- disabled linker in compiler plugin (functionality moved to separate compilation phase)
- improved script witness computation and UTxO handling
- optimized ByteString handling and improved offchain method documentation
- refactored LinkedList contract to use failures instead of bools
- refactored HTLC and Vault validators for improved clarity

### Fixed

- Word64 flat format encoding
- `Term.Constr.tag` encoding as unsigned Word64 type
- pattern matching on primitive constants with default case
- `@` pattern bindings in nested positions
- duplicate wildcard cases in pattern match compilation
- incorrect typing of tail of list pattern
- bug in alphaEquality (case arguments may be variables)
- type substitution in constructors
- `Interop.getMintValue` resolves all BlocksValidation fails
- Boolean data representation compatibility with Plutus
- compilation on Windows
- bug with incorrect handling of `SIR.Const`
- non-recursive let bindings scoping bug
- performance regression in LinkedList tests
- S3LoweringDataAccess regression
- validation for non-negative integers in flat serialization
- Data CBOR encoding for large negative integers (Java adds an extra 0 byte)

## 0.12.1 (2025-10-08)

### Added

- Address constructors for Credentials
- Transaction and TransactionOutput constructors
- `NormalizedInterval` implementation for interval operations
- ledger rules: `missingRequiredDatums`, `validateOutputBootAddrAttrsTooBig`,
  `hasExactSetOfRedeemers`
- upstream tx editor and tx builder
- additional functions to StdlibTestKit

### Changed

- refactored and improved offchain MultiAsset, Coin and Value
- refactored HtlcValidator to follow scalus style
- deprecated prelude `orFail`, replaced `==` with `===` in several places
- simplified datum retrieval in Escrow, HelloCardano, and Vesting scripts
- refactored TransactionWitnessSet.apply
- moved BuildInfo to `scalus.utils`
- replaced Hex.hexToBytes calls with extension method
- rewritten patterns tests using UPLC

### Fixed

- not all tree is traversed during Apply handling in CaseConstrApply optimization pass
- plutus scripts error propagation to low level tx balancer
- wrong redeemers sorting in ScriptContext
- low level tx balancer fee change re-looping
- hardcoded change output values in transaction building
- budget calculation: wrong sorting order of redeemers

## 0.12.0 (2025-09-27)

### Added

- Cardano ledger rules implementation and validation
- experimental UPLC JIT compiler for improved performance
- `Show` typeclass implementation for better debugging in smart contracts
- new `Betting`, `Escrow` and `Simple transfer` examples
- CBOR codecs reserialize to the same bytes with `KeepRaw`
- experimental transaction builder with intention-based API
- `monocle` optics support for lens-based data manipulation
- Ed25519 compatibility for Cardano key derivation and message signing
- BLS12-381 scalar field support
- property-based testing utilities with ScalaCheck integration
- enhanced plutus script evaluation on JavaScript platform

### Changed

- major backwards compatible package reorganizations
- moved Cardano ledger types to `scalus.cardano.ledger`
- moved protocol parameters and cost models to appropriate packages
- transaction builder redesigned with cleaner intention-based API
- improved Value implementation using SortedMap
- enhanced error messages and debugging support
- updated build system and dependency management

### Fixed

- script data hash generation for PlutusV1 cost model serialization order
- various transaction building and balancing issues
- improved error handling in script evaluation
- better handling of native tokens in transaction building

## 0.11.0 (2025-07-28)

### Added

- new more efficient UPLC codegen
- CIP-57 Blueprints support
- improved Scalus standard library and tests
- `PlutusScriptEvaluator` for evaluating Plutus scripts using Scalus ledger domain types
- `ScriptDataHashGenerator` for computing script integrity hash
- added and improved Cardano Ledger rules
- Arbitrary instances for Plutus domain types
- Vesting example

### Fixed

- avoid expensive G1/G2 zero/generator constants initialization

### Changed

- Plutus `Value` uses `SortedMap` instead of `AssocMap`
- improved Cardano Ledger classes

## 0.10.4 (2025-07-16)

### Fixed

- large BigInt encoding occasionally added an extra byte to the CBOR encoding
- mint value doesn't contain 0 Ada in Plutus V3 scripts

## 0.10.3 (2025-07-16)

### Added

- updated to a new Maven Central repository

## 0.10.2 (2025-07-07)

### Added

- lazy cost model loading in `ScalusTransactionEvaluator`

### Fixed

- BLS builtins serialization
- cost parameters for `divideInteger`, `modInteger`, `quotientInteger`, `remainderInteger`

## 0.10.1 (2025-06-04)

### Added

- more Cardano ledger rules

### Changed

- moved Cardano Ledger domain types to scalus project
- refactor Hash types in ledger domain types

## 0.10.0 (2025-05-28)

### Added

- improved Scalus standard library
- Scalus Design Patterns project
- Scalus Examples: HTLC, PaymentSplitter
- Scalus benchmarks: Knights and Clausify
- new UPLC 1.1 generation `SirToUplc110Lowering` (experimental)
- Cardano Ledger domain types with CBOR codecs (experimental)
- Scalus Bilinear Accumulator implementation (experimental)

### Changed

- simplify `ToData`/`FromData` derivation allowing Scala 3 `derives`
- updated to Scala 3.3.6, Scala.js 1.19.0

### Fixed

- support for Scala argumentless functions in Scalus compiler

## 0.9.0 (2025-04-17)

### Added

- all Plutus V3 builtins from the Plomin hard fork
- improved Scalus standard library
- support for TupleN types
- support for destructuring: val (a, b) = Foo(2, “b”)
- BLS12-381 primitives work on JVM and Javascript
- Groth16 SNARK implementation works on JVM and Javascript
- Cardano native scripts support: Timelock
- advanced UPLC optimization: case/constr instead of apply
- generate better error traces for UPLC
- PaymentSplitter example
- simple Validator trait for writing validators
- Scalus Testkit: a simple test framework for testing Plutus scripts

### Changed

- better SIR code generation (remove recursivity)
- better UPLC optimizations: eta-reduce
- better error messages in the compiler
- renamed `Maybe` to `Option` to follow Scala naming conventions
- cardano-client-lib 0.6.4

### Fixed

- bug with Select fields during linking of SIR

## 0.8.5 (2025-02-16)

### Added

- Scalus Scala Native support with all Plutus V1/V2 builtins
- a C example using libscalus native library for script evaluation and budget calculation
- compile synthetic DataType.apply as a primary constructor
- better error messages during Scalus on-chain script compilation
- async-profiler support for profiling

### Changed

- dependencies updated: Scala 3.3.5, Scala.js 1.18.2
- speedup MemoryUsage calculation

### Fixed

- removed unnecessary bitcoin-s-crypto dependency
- Data.unit type is Data, not Constr

## 0.8.4 (2025-01-05)

### Added

- CIP-117: fail on non-unit results in Plutus V3 scripts

### Fixed

- OptimizingSirToUplcLowering threw an exception when generateErrorTraces was enabled

## 0.8.3 (2024-12-20)

### Added

- customizable UPLC Inliner optimization pass
- Prelude List functions: single, cons, length, map2
- experimental Groth16 SNARK implementation
- Data `toCbor` and `fromCbor` extensions
- add generated ScalaDocs to website

### Changed

- code is updated to Scala 3.5+
- use latest dependencies

### Fixed

- BLS12-381 G1/G2 compressed zero points encoding

## 0.8.2 (2024-10-11)

Nothing changed, just pushed the wrong commit under 0.8.1.

## 0.8.1 (2024-10-11)

### Fixed

- cost model for `integerToBytestring` and `bytestringToInteger`
- flat encoding of SIR `Data` was wrong for `Map` and `List` types

## 0.8.0 (2024-10-03)

### Added

- initial Plutus V3 support:
    - all builtins work on JVM
    - V3 `ScriptContext` etc for script compilation
    - V3 script evaluation and cost calculation
- Cardano Client Lib can use `ScalusTransactionEvaluator` for V3 script evaluation
- improved `deriveEnum` for `ToData` and `FromData` (see Tutorial)

### Changed

- few renames and API changes in `scalus-bloxbean-cardano-client-lib` `Interop` module

## 0.7.2 (2024-08-11)

### Added

- `VM.evaluateDebug` with useful info about the execution: budget, costs, builtins, logs
- `eval`, `evalDebug`, `show`, `showHightlighted` extensions for `Term` and `Program`
- `toUplcOptimized` extension for `SIR` to generate optimized UPLC code
- `?` tracing operator and `log` function similar to Aiken's `?` operator

### Changed

- don't remove top-level eta-expansions in `compile` method

### Fixed

- fieldAsData/field didn't work for aliaed types
- small fix in `OptimizingSirToUplcLowering` with tracing errors
- multiple inner matches in pattern matching code generation

## 0.7.1 (2024-08-04)

### Added

- implemented all Plutus V1/V2 builtins on JVM and JavaScript platforms
- passing all Plutus Conformance Tests on JVM and JavaScript platforms
- `OptimizingSirToUplcLowering` SIR to UPLC lowering with built-in optimizations
- PlutusV3 constr/case support
- `Data.to*` extension methods

### Changed

- speedup `UplcParser` by 10x

### Fixed

- use Java 11 on Github CI and release builds

## 0.7.0 (2024-05-29)

Scalus CEK and cost calculation implementation is now feature complete.

We were able to validate transactions from whole Cardano Epoch 484 using Scalus and Cardano Client
Lib.

### Added

- `ScalusTransactionEvaluator` - Cardano Client Lib (CCL) `TransactionEvaluator` implementation. You
  can now use Scalus
  to evaluate scripts and their costs off-chain during transaction building.
- SIR and UPLC optimizations: `RemoveRecursivity`, `EtaReduce`
- `evaluateScriptRestricting` mode
- Advanced documentation
- `Data` JSON codec

### Changed

- optimized From/ToData instances to generate less code
- `PlutusV1Params`, `PlutusV2Params` are now classes with Long fields
- Removed type parameter from `Interval`
- bytesToHex uses lowercase hex characters

### Fixed

- memoryUsageInteger was wrong in certain cases
- sliceByteString wrong order of parameters
- Data CBOR encoding of large integers must be chunked by 64 byte chunks
- typo in `ConstAboveDiagonal` cost calculation implementation

## 0.6.1 (2024-04-18)

### Added

- compile `==`, `!=` operators as Plutus builtins for `Boolean`, `ByteString`, `String` and `Data`
  types
- `++` operator, `size` and `length` functions for `ByteString`

### Fixed

- wrong `ConstAboveDiagonal` cost calculation used in division

## 0.6.0 (2024-04-05)

### Added

- fast debruijned CEK machine implementation
- CEK machine benchmarking, runs on par with high performance Haskell implementation
- CEK machine execution budget calculation
- cost model for CEK machine loading from Cardano CLI and Blockfrost API JSONs
- updated to Scala 3.3.3
- updated to Scala.js 1.16.0, 3x reduction in JS bundle size

### Changed

- lots of internal refactoring

## 0.5.0 (2024-02-17)

### Added

- better error messages in the compiler
- Scalus Intermediate Representation (SIR) now has a version number (requires recompilation of
  existing SIR)
- SIR and UPLC pretty printers can print normal and syntax highlighted code using XTerm colors
- UPLC pretty printer outputs a better indented code
- added Blake2b 224 builtin implementation on JVM
- updated dependencies: Scala 3.3.2
- use uplc cli from Plutus 1.15.0.1

### Changed

- moved scalus.uplc.Data to scalus.builtin.Data
- renamed scalus.builtins to scalus.builtin

### Fixed

- Plutus Data Map can have duplicate keys; fixed Data CBOR codec to handle this case.
