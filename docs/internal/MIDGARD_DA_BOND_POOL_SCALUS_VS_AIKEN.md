# Midgard DA bond pool: Scalus vs Aiken

Date: 2026-10-07, updated 2026-10-09.

## What was compared

- **Aiken:** `onchain/aiken/validators/da-bond-pool.ak` from Anastasia-Labs/midgard,
  branch `colll78/canonical-v1-watcher-l1-source-checkpoint`, commit `17fdffd9b`.
  It was built with the local `aiken v1.1.23+8949565`, `aiken build -t silent`, default
  env. Midgard CI pins the fork `v1.1.23+5adf783`, so its output can differ slightly.
- **Scalus:** `scalus-examples/.../midgard/DaBondPoolValidator.scala`, compiled with
  `Options.release`.
- **Evaluation:** mainnet protocol parameters and the PV11 cost model
  (`PlutusVM.makePlutusV3VM()`, `CardanoInfo.mainnet`).
- **Parameters:** the same values, applied the same way: both scripts take the four Aiken
  parameters as separate `Data` arguments (`DaBondPoolContract.applyParams`).

## Behaviour

`DaBondPoolDifferentialTest` ports the 68 Aiken tests in `da-bond-pool.test.ak`. One
Aiken test holds 2 contexts, so there are 69 contexts. Five extra cases cover the safety
checks the `scalus:contract-test` negative-test convention asks for and the Aiken suite
lacks: unbounded and over-long validity ranges, a pool input holding two NFTs, and the
stray input's own script run. Every context runs on both scripts.

- Aiken script: 74/74 match the expected verdict. On the 69 ported cases this confirms
  that the ported fixtures are the same as the Aiken ones.
- Scalus script: 74/74 give the same verdict as the Aiken script.
- Each of the 60 reject cases also runs a traced Scalus build and asserts the guard that
  rejects it, so a case rejected for another reason fails the test.
- The Scalus ExUnits of every accepted case and the fee and ExUnits of every Emulator step
  are pinned, on Scala 3.3.8 and 3.9.0.

## Script size

| | Applied script | Reference-script fee |
|---|---:|---:|
| Aiken | 4342 B | 65,130 lovelace |
| Scalus (Scala 3.3.8) | 2431 B (−44%) | 36,465 lovelace |
| Scalus (Scala 3.9.0) | 2414 B | 36,210 lovelace |

## Memory and script fee per arm (unit contexts, `DaBondPoolDifferentialTest`)

"Script cost" is the ExUnits fee plus the reference-script fee. It leaves out the
size fee of the rest of the transaction. Scala 3.3.8.

| case | Aiken mem | Scalus mem | mem ratio | Aiken ExUnits fee | Scalus ExUnits fee | Aiken script cost | Scalus script cost | cost ratio |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| InitPool | 92,100 | 50,711 | 0.55 | 7,657 | 4,551 | 72,787 | 41,016 | 0.56 |
| TopUp (Bonded) | 152,093 | 65,975 | 0.43 | 12,570 | 6,072 | 77,700 | 42,537 | 0.55 |
| Slash (Bonded) | 273,007 | 132,537 | 0.49 | 22,760 | 11,841 | 87,890 | 48,306 | 0.55 |
| BeginWithdraw | 284,301 | 115,905 | 0.41 | 23,096 | 10,457 | 88,226 | 46,922 | 0.53 |
| CancelWithdraw | 255,541 | 105,594 | 0.41 | 20,709 | 9,569 | 85,839 | 46,034 | 0.54 |
| CompleteWithdraw | 296,563 | 117,150 | 0.40 | 24,084 | 10,519 | 89,214 | 46,984 | 0.53 |

## Total transaction fee (Emulator, reference scripts, `DaBondPoolEmulatorTest`)

The fee is what `TxBuilder` sets and the Emulator accepts: size fee + ExUnits fee +
reference-script fee. Scala 3.3.8.

| step | Aiken fee | Scalus fee | saved | fee ratio | Aiken mem | Scalus mem | mem ratio |
|---|---:|---:|---:|---:|---:|---:|---:|
| publish (one time) | 357,429 | 273,345 | 84,084 | 0.76 | – | – | – |
| InitPool | 252,009 | 219,761 | 32,248 | 0.87 | 95,694 | 50,711 | 0.53 |
| TopUp | 254,113 | 218,950 | 35,163 | 0.86 | 152,093 | 65,975 | 0.43 |
| BeginWithdraw | 275,167 | 233,422 | 41,745 | 0.85 | 294,183 | 120,326 | 0.41 |
| CancelWithdraw | 272,076 | 231,830 | 40,246 | 0.85 | 265,423 | 110,015 | 0.41 |
| CompleteWithdraw | 273,292 | 231,053 | 42,239 | 0.85 | 296,638 | 117,150 | 0.39 |

The reference-script fee accounts for 28,665 lovelace of each per-tx difference, which is
68–89% of it. The rest comes from ExUnits.

## Changes from the `scalus:` skill review (2026-10-08 and 2026-10-09)

The first port measured memory ratio 0.47–0.64 and fee ratio 0.87–0.89. The
`scalus:contract` safe API lowered the Scalus side further, with every case still agreeing
with Aiken:

| Change | Script size | Effect |
|---|---:|---|
| first port | 2749 B | – |
| `findUniqueOrFail`, `hasNft`, `scriptHashOrFail`, `findInputOrFail` | 2732 B | memory 2–5% lower on every arm |
| `hasInlineDatum` for every continuing datum | 2698 B | script cost 597–841 lovelace lower per arm |
| four `Data` parameters, as in Aiken, instead of one `DaBondPoolParams` record | 2662 B | memory 800–1,300 lower per arm, tx fee 628–665 lower |
| `validFromOrFail` / `validToOrFail` instead of a port of Aiken's range normalizer | 2485 B | tx fee 2,600–3,200 lower, memory up to 7,200 lower on the withdraw arms |
| `getLovelace` (one `lookupCoin` builtin) instead of `lovelaceAmount` (a `Data` map walk) | 2431 B | memory 6–10% lower per tx, tx fee 1,000–1,300 lower |

The range normalizer handled inclusive and exclusive bounds on both sides and an empty range.
The ledger never builds those cases: `transValidityInterval` (cardano-ledger
`Conway/TxInfo.hs:793-805`) always makes the lower bound inclusive and the upper bound
exclusive, and phase 1 (`inInterval`, `Allegra/Scripts.hs:442`) rejects a transaction whose
range is empty. So the inclusive upper bound is `validTo - 1`, and only the 480 s length
check stays. The test fixtures now use ledger-shaped ranges (`ledgerRange`); Aiken normalizes
them to the same bounds as its `interval.between` fixtures. On a range the ledger cannot build
(an exclusive lower or an inclusive upper bound) the two scripts can disagree.

A `scalus:smart-contract-security-review` pass found no issue specific to the port.

## Differences in the port

1. **Shape checks.** Aiken `expect x: T = data` fully checks the redeemer, the pool
   datum, the DA params datum and the correction-lock datum. The Scalus port does not
   copy these checks. Each decoded UTxO is authenticated by an NFT whose policy fixes its
   datum shape, and every redeemer field fails at its first use. Part of the ExUnits gap comes from these checks, and this report
   does not measure that part separately.
2. **Validity ranges.** The Scalus port reads the bounds the way the ledger builds them (see
   above); Aiken normalizes any bound shape.
3. **Correction lock.** Aiken decodes the lock datum and compares it with `Idle`. Scalus
   checks only that the constructor index is 0.
4. **Value builtins.** `Options.release` keeps `valueBuiltins = true`. At PV11, Scalus
   lowers `withoutLovelace`, `quantityOf`, `hasNft` and `getLovelace` to the CIP-153 Value builtins.
   Aiken 1.1.23 with stdlib v3.1.0 does not use them. Part of the memory gap comes from
   this.

## Findings in Scalus

1. **Lowering bug, fixed:** `DaBondPoolDatum.Bonded.toData` (an enum case with no fields)
   failed with `LoweringException: Unsupported conversion ... from ProdDataList to
   DataConstr`. Fixed on master by PR #384 (`078b71043`); the port now uses the bare form,
   with the same budget as the earlier `(Bonded: DaBondPoolDatum).toData` workaround.
2. **`lovelaceAmount` was slower than `getLovelace`:** at PV11 `getLovelace` is one
   `lookupCoin` builtin, while `lovelaceAmount` walked the `Data` map. `lovelaceAmount` is now
   deprecated on master.
3. **Typed parameters:** `ParameterizedValidator[A]` expects the parameter in its
   lowered form (a field list), not as `Constr` Data. So `program $ params.toData`
   fails at run time with `HeadList` / `DropList` errors.
4. **Several runtime parameters:** neither `Validator` nor `DataParameterizedValidator`
   takes more than one. The port defines its own entry point,
   `validate(initRef, hubOraclePolicyId, daParamsPolicyId, parameters)(scData)`, and
   dispatches to `mint` and `spend` the way `Validator.validateScriptContext` does. Four
   `Data` arguments are cheaper than one record decoded on chain.

## Reproduce

```bash
sbt "scalusExamplesJVM/testOnly scalus.examples.midgard.*"
```

Both tests print their tables to stdout.
