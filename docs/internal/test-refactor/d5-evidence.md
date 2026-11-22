# D5 – Evidence and research for the shared generators

*Informative.* This file holds the measurements and the research behind `d5-generators.md`. It
states no rules. A lesson names the rule group of `d5-generators.md` that it supports.

## 1. Measured gaps: what the generators never or rarely produce (owner question 4)

Measured with seeds 0 to 9 999 and sizes 0 to 100, the same sampling as `GenStatsTest`
(scratch probe, not committed; log in the session notes folder). "Never" means 0 of 10 000
samples. A row says what the generator produces. Example tests may still cover the value: for
instance `DataCborCodecTest` and `DataCborDepthTest` test the tag-102 form by hand.

| Generator | Gap | Measured | Why it matters |
|---|---|---|---|
| `Data` (`arbData`) | `Constr` tag 0 | never (`Gen.posNum` starts at 1) | Tag 0 is the most common tag on chain (`Option.None`, first enum case). |
| `Data` | `Constr` tag 128 or more (CBOR tag-102 form) | never (max tag 100) | A third encoding path, `DataApi.scala:162-165`, is never round-tripped by a property. |
| `Data` | `B` longer than 100 bytes | never (weight 0 on 1 000 to 10 000) | Chunked bytes (>64) are hit, but no large datum. |
| `Data` | CBOR size | max 1 587 bytes, p99 803 | Data always fits a transaction; large-datum paths are never tested. |
| `v1.Value` | Two independent values share a non-ADA policy | 0.00 % | In a two-operand law, `Value.plus` never merges two token maps of one policy; it merges only lovelace. |
| `v1.Value` | Negative amount | 69 % of samples | A real output never holds one. |
| `v2.TxOut` | Lovelace ≤ 0 | 60 % | Same. |
| `v1.Interval` | All finite bounds are POSIX times before 2100 | 3.3 % | 67 % of samples hold a bound above 2^64; real validity ranges are rare. |
| `v1.Interval` | Point interval, or width under 1 h | 0.4 %, 0.9 % | Validators check short windows. |
| `v3.TxInfo` | Reference inputs, certificates, withdrawals, redeemers, datums | never (fixed `Nil`/empty) | Script contexts that use them get no property input. |
| `v3.TxInfo` | Size | median 18 inputs, 19 outputs, 19 signers; outputs, mint, signers and input refs alone exceed 16 384 bytes in 25 % | 15.4 s per 10 000 samples (`gen-stats.md`); no real transaction is that large. |
| `v3.TxInfo` | `mint` | drawn from `Arbitrary[Value]`: can hold lovelace, 69 % hold a negative amount | A real mint never holds lovelace. |
| `Coin` | 0, and anything below 1 000 ADA (10^9 lovelace) | 0.00 %, 0.00 % (0.01 % fall in 1 000 to 10^6 ADA) | Uniform on `[0, 2^53]`: median 4.5 × 10^15 lovelace. |
| `Coin` | Above 2^53 (max supply 4.5 × 10^16) | never | The upper edge of real amounts. |
| `ExUnits` | Above 1 000 | never | A real script uses 10^5 to 10^7 memory units; `maxTxExecutionUnits` memory is 16.5 × 10^6. |
| `TransactionOutput` | Stake address as output address | 34 % | A node rejects it (F-35); `CborSerializationTest` pins the lax decoder. |
| `TransactionBody` | No certificates, no collateral inputs | never (both 1 to 4 always) | The empty case is never tested; cardano-ledger adds withdrawals to only 25 % of its transactions (`Constants.hs`). |
| `TransactionBody` | `ttl` before validity start | 41 % | Such a transaction is never valid. |
| `Transaction` | Any value at size 0 to 2 | never (100 % discard), 13 % discard at size 5 | `ArbitraryDerivation` falls back to `Gen.fail` (H-36); small transactions do not exist. |
| `Program` | Version 1.0.0 or 1.1.0 | never (major and minor 1 to 100, patch ≥ 1) | The only versions on chain. |
| `Term` | A term whose evaluation reduces and succeeds | 2 % (root `Apply`, `Force`, `Case`, `Constr`: 41 of 2 000) | 38 % "success" are values (`LamAbs`, `Delay`, `Const`, `Builtin`); `Case` succeeds 0 of 279. |
| `Term`, `Constant` | String, BLS and BuiltinValue constants | never (F-29) | Three constant kinds are not round-tripped. |

How the tests use the generators (survey of all test sources):

- Most suites run ScalaTest's default of 10 cases. `EvalTestKit` and `ScalusTest` set no
  `PropertyCheckConfiguration`. A few suites set 100 to 100 000.
- 665 calls `Arbitrary.arbitrary[T].sample.get`, mostly in the ledger rules tests, use an unseeded
  RNG. A failure there prints no seed and no value.
- No property decodes malformed or non-canonical CBOR.
- No property uses a mainnet fixture as input.
- ScalaTest shrinks against the bounds of the generator: a failure of `Gen.choose(1000, 2000)`
  reports `0` ("11 shrinks, orig: 1343"); `Gen.listOfN(3, Gen.choose(5, 9))` reports `List(0)`.

## 2. Research: what other projects do (owner question 3)

| Source | Practice | Lesson for Scalus |
|---|---|---|
| cardano-ledger `Mary/Arbitrary.hs:229-231` (tag conway-1.22.1.0, 226b002d5) | `Arbitrary MaryValue` uses `genPositiveInt` amounts; `Arbitrary MultiAsset` (mint) is signed. Coin is non-negative (`Core/Arbitrary.hs:362-364`, "Cannot be negative"). | `gen.value`. |
| same, `Mary/Arbitrary.hs:52` and `PolicyID` instance | 30 % of asset names come from 10 fixed strings; policy ids also draw from 10 fixed hashes. | Force key collisions (`gen.cover`). |
| same, `genPositiveInt` | `oneof [choose (1, maxBound), getPositive <$> arbitrary]`: half full range, half small and size-scaled. | Two parts in one generator (`gen.cover`). |
| same, `genMultiAsset` | If the value is not small enough for the compact form, retry with `scale (div 2)`. | Rescale, do not filter (`gen.size`). |
| same, `genMultiAssetToFail`, `genMultiAssetZero`, `genMultiAssetNestedEmpty` | Named generators for negative and decoder tests, next to the valid `Arbitrary`. | Named families (`gen.family`). |
| same, `Core/Arbitrary.hs:254-256` (`TxIx`) | "starting with Conway, we only deserialize TxIx within Word16 range". | Format bounds belong in the `given`; protocol limits do not (`gen.size`). |
| same, `Core/Arbitrary.hs:393-410` (`Addr`) | 80 % base, 15 % no stake reference, 5 % pointer; 20 % bootstrap. | Realistic weights (`gen.cover`). |
| cardano-ledger `Shelley/Constants.hs`, `Generic/GenState.hs:203-243` | One record of caps and frequencies: at most 5 inputs, at most 3 certificates per transaction, withdrawals none 75 %, few 20 %, many 5 %. | Caps by construction (`gen.size`). |
| cardano-ledger `Constrained/Conway/Utxo.hs:225-239` | `genUtxoExecContext` generates the transaction, then sets `maxTxSize` to its size. | A rule test can fit the parameters to the value (`gen.size`). |
| constrained-generators `docs/Manual.md`, `docs/DesignPrinciples.md` (3895396) | One `Specification` gives both `genFromSpec` and `conformsToSpec` (Manual). `prop3` mixes 90 % constrained and 10 % `arbitrary` values, so that an implication is not always true (DesignPrinciples). | Pair each well-formed generator with a check (`gen.family`, `gen.quality`). |
| cardano-ledger `CddlSpec.hs` | Arbitrary values are encoded and validated against the CDDL (`huddleRoundTripArbitraryValidate`). | Independent oracle; a non-goal of D5. |
| cardano-ledger `Binary/Twiddle.hs` | `twiddle` re-encodes a value with other valid CBOR forms (definite or indefinite length, and so on); decoding must give the same value. | CBOR edges; a non-goal of D5. |
| Plutus `QuickCheck/Builtin.hs:95-135` (ac40fae4de) | Integers by magnitude band (1-127, 128-32 767, …, up to 16 × Int64 max), both ends of each band preferred, 4 % zero. | Magnitude bands (`gen.cover`). |
| Plutus `QuickCheck/Builtin.hs:175-183` | `Constr` tag: 60 % 0-2, 30 % 3-5, 10 % up to Int64 max. | Realistic core plus wide part (`gen.cover`). |
| Plutus `Hedgehog/Interesting.hs`, `TypeEvalCheck.hs`, `NEAT` | Terms generated with their expected value; exhaustive enumeration of small well-typed terms. | Evaluating-term family (`gen.family`). |
| QuickCheck 2.19 | `cover`, `tabulate` and `checkCoverage` (with `Confidence`) make a minimum share of a class part of the property. | Asserted coverage (`gen.quality`). |
| Hypothesis health checks | `filter_too_much`, `too_slow`, `data_too_large`, `large_base_example` fail a test whose generator discards or costs too much. | Discard and cost limits (`gen.quality`). |
| Hedgehog README | "Integrated shrinking, shrinks obey invariants by construction." ScalaCheck 1.19 shrinks by type. | `gen.shrink`. |
| jqwik user guide | `EdgeCasesMode.FIRST`: "all edge cases being generated before jqwik starts with randomly generated samples". | Edges first (`gen.runs`). |
| Groce et al., "Swarm testing", ISSTA 2012 | Each test omits some features at random; one feature can block the behaviour another one needs. | Optional parts often absent (`gen.cover`). |
| Goldstein et al., "Property-Based Testing in Practice", ICSE 2024; "Tyche", UIST 2024 | Developers want realistic, well-spread generators with edge cases, but seldom check the distribution; the papers ask for tools that show it. | GenStats and the threshold test (`gen.quality`). |
| ScalaCheck 1.19 `Gen.chooseNum` | `minT`, `maxT`, 0, 1, -1 and the specials get 50 % in total; the other 50 % is uniform. | Edges exist; magnitude bands do not (`gen.cover`). |

