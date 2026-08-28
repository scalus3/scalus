# Scalus existing on-chain API surface (research input for the stdlib design)

Research date: 2026-08-26. Worktree: `.claude/worktrees/stdlib-api-research`, branch `master`.

Purpose: inventory everything that already exists on-chain, so a new high-level "smart contract
standard library" does not duplicate or contradict it. Every claim below carries `file:line` and
quoted source.

Legend for the **Kind** column:

| Kind | Meaning |
| --- | --- |
| **HL** | High-level check/query of the kind the new stdlib is about (works on `TxInfo`/`TxOut`/`Value`, encodes a domain concept) |
| **P** | Low-level primitive (collection, typeclass, arithmetic, codec) |
| **T** | Type / data declaration only (no behaviour) |
| **OFF** | Off-chain-only (`@Ignore`), not compiled to UPLC |

Scope decisions (edges the task list did not name explicitly):

- `prelude/Order.scala` is included (public API, inseparable from `Ord`).
- `prelude/EqMacros.scala` and `prelude/LogMacros.scala` are **macro internals** — not user-facing
  API, listed only as the implementation of `Eq.derived` / `log`. Not catalogued member by member.
- `prelude/bls12_381/*` and `onchain/plutus/crypto/*` (MerkleTree, MPF, accumulators) are **out of
  scope** for this report (cryptography, not transaction-shape checks).
- `v2/package.scala` is included even though not listed: it is part of the complete surface (it is
  pure re-export, see §A.4).
- `v3/Contexts.scala` also carries a large block of `export`s from v1/v2 (lines 18–42) which are
  type aliases, not new API.

Coverage for §C (examples sweep): all 91 `main` sources under `scalus-examples/` and all 14 under
`scalus-design-patterns/` were read. Five contain no on-chain code and are excluded —
`scalus-examples/jvm/.../examples/SendTx.scala`, `.../platform/App.scala`,
`.../platform/ScalusPlatform.scala`, `scalus-examples/js/.../MintingPolicyJS.scala`,
`scalus-examples/lottery-complete/.../offchain/LotteryTransactions.scala` — verified by
`grep -ln "@Compile"` over those five paths returning no matches. Recall for the signatory findings
in §C.7 was cross-checked with `grep -rln signatories` over both modules: 13 files match, all 13 are
cited.

---

## A. Existing on-chain API surface

### A.0 The high-level API that ALREADY exists (the short list)

This is the subset a new stdlib must build on / not contradict. Full detail in the sections below.

| Signature | File:line | Failure mode |
| --- | --- | --- |
| `TxInfo.findOwnInput(outRef: TxOutRef): Option[TxInInfo]` | v3/Contexts.scala:938 | returns `Option.None` |
| `TxInfo.findOwnInputOrFail(outRef, message = "Tx input not found"): TxInInfo` | v3/Contexts.scala:960 | **fails** (`getOrFail`) |
| `TxInfo.findOwnDatum(datumHash: DatumHash): Option[Datum]` | v3/Contexts.scala:983 | returns `Option.None` |
| `TxInfo.findOwnScriptOutputs(scriptHash: ValidatorHash): List[v2.TxOut]` | v3/Contexts.scala:1002 | returns `List.Nil` |
| `TxInfo.findOwnInputsByCredential(cred: Credential): List[TxInInfo]` | v3/Contexts.scala:1022 | returns `List.Nil` |
| `TxInfo.findOwnOutputsByCredential(cred: Credential): List[v2.TxOut]` | v3/Contexts.scala:1041 | returns `List.Nil` |
| `TxInfo.findOwnInputs(pred: TxInInfo => Boolean): List[TxInInfo]` | v3/Contexts.scala:1058 | returns `List.Nil` |
| `TxInfo.findOwnOutputs(pred: v2.TxOut => Boolean): List[v2.TxOut]` | v3/Contexts.scala:1075 | returns `List.Nil` |
| `TxInfo.isSignedBy(pubKeyHash: PubKeyHash): Boolean` | v3/Contexts.scala:1082 | returns `false` |
| `TxInfo.getValidityStartTime: BigInt` | v3/Contexts.scala:1102 | returns `BigInt(0)` on infinite bound |
| `Utils.findInput(inputs, outRef): Option[TxInInfo]` | v3/Contexts.scala:1141 | `Option.None` |
| `Utils.findDatum(outputs, datum, datumHash): Option[Datum]` | v2/Contexts.scala:274 (re-exported v3:1145) | `Option.None` |
| `Utils.findScriptOutputs(outputs, scriptHash): List[TxOut]` | v2/Contexts.scala:300 (re-exported v3:1145) | `List.Nil` |
| `Utils.getAdaFromOutputs(outputs: List[v2.TxOut]): Lovelace` | v3/Contexts.scala:1161 | `BigInt(0)` if empty |
| `Utils.getAdaFromInputs(inputs: List[TxInInfo]): Lovelace` | v3/Contexts.scala:1179 | `BigInt(0)` if empty |
| `OutputDatum.inlineOrFail[A: FromData]: A` | v2/Contexts.scala:82 / :94 | **fails** (and compile-error on statically-wrong receiver) |
| `Interval.contains(time): Boolean` | v1/Contexts.scala:221 | `false` |
| `Interval.isEntirelyAfter/isEntirelyBefore/isEntirelyBetween` | v1/Contexts.scala:245/256/267 | `false` |
| `Interval.isNever` / `nonNever` | v1/Contexts.scala:273/292 | boolean |
| `IntervalBound.finite(default)` / `finiteOrFail(message)` | v1/Contexts.scala:130/133 | default / **fails** |
| `Credential.pubKeyOption` / `scriptOption` | v1/Contexts.scala:503/507 | `Option.None` |
| `Value.quantityOf(cs, tn): BigInt` | v1/Value.scala:795 | `0` if absent |
| `Value.getLovelace` / `lovelaceAmount` | v1/Value.scala:701 / :719 | `0` / silently returns **another token's amount** when ADA is absent, fails on an empty value (see §C.14) |
| `Value.containsAtLeast(other): Boolean` | v1/Value.scala:824 | **fails** on negative amounts |
| `Value.hasOnly(cs, tn, amount): Boolean` | v1/Value.scala:942 | `false` |
| `Value.isPositive` / `isZero` / `nonZero` | v1/Value.scala:758/734/750 | boolean |
| `Value.insertCoin` / `withoutLovelace` / `tokens` / `flatten` / `policyIds` | v1/Value.scala:863/984/901/1015/1049 | total |

**Observation for the design:** the existing high-level layer is almost entirely *query* shaped
(`find…: Option`/`List`), not *assert* shaped. The only "fail" variants that exist today are
`findOwnInputOrFail` (v3:960), `OutputDatum.inlineOrFail` (v2:82/94), `IntervalBound.finiteOrFail`
(v1:133), `Option.getOrFail` (Option.scala:171), `SortedMap.getOrFail` (SortedMap.scala:659) and
`List.at`/`!!` (List.scala:457/393). There is **no** `mustBeSignedBy`, no `mustMintExactly`, no
"exactly one output" combinator, no value-conservation check.

---

### A.1 `v3/Contexts.scala` (1182 lines)

#### Types (no behaviour)

| Symbol | Line | Kind | Note |
| --- | --- | --- | --- |
| `case class TxId(hash: ByteString)` `@UplcRepr(ProductCaseOneElement)` | 45–46 | T | v3 TxId ≠ v1 TxId (see §C.1) |
| `case class TxOutRef(id: TxId, idx: BigInt)` | 58 | T | v3 TxOutRef ≠ v1 TxOutRef (see §C.1) |
| `type Lovelace = BigInt` | 69 | T | |
| `type ColdCommitteeCredential/HotCommitteeCredential/DRepCredential = Credential` | 70–74 | T | |
| `enum DRep` | 76–79 | T | |
| `enum Delegatee` | 125–128 | T | |
| `enum TxCert` (11 cases) | 173–184 | T | |
| `enum Voter`, `enum Vote` | 361–364, 407–408 | T | |
| `case class GovernanceActionId`, `Committee`, `ProtocolVersion`, `ProposalProcedure` | 451, 468, 488, 635 | T | |
| `type Constitution = Option[ScriptHash]`, `type ChangedParameters = Data` | 486, 503 | T | |
| `enum GovernanceAction` (7 cases) | 505–524 | T | |
| `enum ScriptPurpose` (6 cases) | 658–664 | T | |
| `enum ScriptInfo` (6 cases) | 746–752 | T | the *v3* discriminator used by `Validator` |
| `case class TxInInfo(outRef: TxOutRef, resolved: v2.TxOut)` | 836–839 | T | |
| `case class TxInfo(...16 fields...)` | 854–871 | T | all defaulted except `inputs`, `id` |
| `case class ScriptContext(txInfo, redeemer, scriptInfo)` | 1108–1112 | T | |
| `val TxInfo.placeholder` | 916 | P | test/scaffolding value |

Every one of those objects also declares `given Eq`, `given Ord`, `given ToData`, `given FromData`.
They are **P** (codecs/typeclasses), and they dominate the line count: ~800 of the 1182 lines of
`v3/Contexts.scala` are hand-written `Eq`/`Ord` instances.

#### `TxInfo` extension methods — the existing high-level layer

```scala
// v3/Contexts.scala:938
def findOwnInput(outRef: TxOutRef): Option[TxInInfo] = {
    Utils.findInput(self.inputs, outRef)
}
```

```scala
// v3/Contexts.scala:960
inline def findOwnInputOrFail(
    outRef: TxOutRef,
    inline message: String = "Tx input not found"
): TxInInfo = {
    self.findOwnInput(outRef).getOrFail(message)
}
```

```scala
// v3/Contexts.scala:1022
def findOwnInputsByCredential(cred: Credential): List[TxInInfo] =
    self.inputs.filter(_.resolved.address.credential === cred)

// v3/Contexts.scala:1041
def findOwnOutputsByCredential(cred: Credential): List[v2.TxOut] =
    self.outputs.filter(_.address.credential === cred)
```

```scala
// v3/Contexts.scala:1082
def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
    self.signatories.contains(pubKeyHash)
```

```scala
// v3/Contexts.scala:1102
def getValidityStartTime: BigInt = self.validRange.from.boundType match
    case IntervalBoundType.Finite(t) => t
    case _                           => BigInt(0)
```

| Method | Line | Kind | Semantics / failure mode |
| --- | --- | --- | --- |
| `findOwnInput(outRef)` | 938 | **HL** | delegates to `Utils.findInput`; linear `find` on `===`; `None` if absent |
| `findOwnInputOrFail(outRef, message)` | 960 | **HL** | `getOrFail` ⇒ UPLC `error` with message; note **default parameter** (violates Tier-0 interop rule, but this is on-chain code, exempt) |
| `findOwnDatum(datumHash)` | 983 | **HL** | `Utils.findDatum`: map first, then scans outputs' inline datums re-hashing each (`data.dataHash === datumHash`) |
| `findOwnScriptOutputs(scriptHash)` | 1002 | **HL** | matches only `Credential.ScriptCredential`; ignores staking part |
| `findOwnInputsByCredential(cred)` | 1022 | **HL** | full-`Credential` equality (pubkey or script) |
| `findOwnOutputsByCredential(cred)` | 1041 | **HL** | same, on outputs. **Note the naming inconsistency**: doc comments at 1017 and 1036 say `getOwnInputsByCredential` / `getOwnOutputsByCredential`, the methods are `findOwn…` |
| `findOwnInputs(pred)` | 1058 | **HL** | thin alias for `inputs.filter` |
| `findOwnOutputs(pred)` | 1075 | **HL** | thin alias for `outputs.filter` |
| `isSignedBy(pubKeyHash)` | 1082 | **HL** | `List.contains` ⇒ linear scan with `Eq[PubKeyHash]`; `false` when absent (never fails) |
| `getValidityStartTime` | 1102 | **HL** | **silently returns 0** for `NegInf`/`PosInf` — a deadline check written as `getValidityStartTime >= deadline` is unsound on an open lower bound |

#### `object Utils` (v3)

```scala
// v3/Contexts.scala:1141
def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
    inputs.find(_.outRef === outRef)
}

// v3/Contexts.scala:1145
export scalus.cardano.onchain.plutus.v2.Utils.{findDatum, findScriptOutputs}

// v3/Contexts.scala:1161
def getAdaFromOutputs(outputs: List[v2.TxOut]): Lovelace = {
    outputs.map(_.value.getLovelace).foldLeft(BigInt(0))(_ + _)
}

// v3/Contexts.scala:1179
def getAdaFromInputs(inputs: List[TxInInfo]): Lovelace = {
    inputs.map(_.resolved.value.getLovelace).foldLeft(BigInt(0))(_ + _)
}
```

| Symbol | Line | Kind | Note |
| --- | --- | --- | --- |
| `Utils.findInput` | 1141 | **HL** | duplicate of v1:821 and v2:259 (see §C.2) |
| `Utils.findDatum` (exported from v2) | 1145 | **HL** | |
| `Utils.findScriptOutputs` (exported from v2) | 1145 | **HL** | |
| `Utils.getAdaFromOutputs` | 1161 | **HL** | allocates an intermediate `List` via `map` then folds — 2 traversals |
| `Utils.getAdaFromInputs` | 1179 | **HL** | same shape |

Note the asymmetry: v3 `Utils` **has** ADA-sum helpers, v1/v2 `Utils` do **not**, and there is no
`getValueFromOutputs` (multi-asset) counterpart at all.

---

### A.2 `v3/Validator.scala` (244 lines)

Three sibling traits, all `@Compile`, all with an identical `scriptInfo` dispatch body:

| Trait | Line | Entry point | Purpose |
| --- | --- | --- | --- |
| `trait Validator` | 9 | `validate(scData: Data): Unit` (11), `validateScriptContext(sc)` (15) | plain validator |
| `trait ParameterizedValidator[A]` | 91 | `validate(param: A)(scData: Data): Unit` (93) | typed compile-time parameter |
| `trait DataParameterizedValidator` | 179 | `validate(param: Data)(scData: Data): Unit` (181) | `Data`-applied parameter (UPLC-level `applyArg`) |

Abstract members every implementer must supply (all `inline def … : Unit`):

| Member | Validator | ParameterizedValidator | DataParameterizedValidator |
| --- | --- | --- | --- |
| `spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef)` | 31 | 110 (`param` first) | 198 |
| `mint(redeemer: Data, policyId: PolicyId, tx: TxInfo)` | 42 | 122 | 210 |
| `reward(redeemer: Data, stakingKey: Credential, tx: TxInfo)` | 51 | 132 | 217 |
| `certify(redeemer: Data, cert: TxCert, tx: TxInfo)` | 60 | 143 | 224 |
| `vote(redeemer: Data, voter: Voter, tx: TxInfo)` | 69 | 153 | 231 |
| `propose(proposalProcedure: ProposalProcedure, tx: TxInfo)` | 79 | 163 (has a body: `fail("Empty Validator.propose")`) | 238 |

Kind: **HL** (the entry-point abstraction), but purely structural — no checks.

Two inconsistencies worth carrying into the design:

1. `ParameterizedValidator.propose` at line 163–169 is the **only** member with a default body:
   ```scala
   inline def propose(
       param: A,
       proposalProcedure: ProposalProcedure,
       tx: TxInfo
   ): Unit = {
       fail("Empty Validator.propose")
   }
   ```
   All other purposes in all three traits are abstract, with the `fail(...)` default **commented
   out** (e.g. Validator.scala:37–40, 47–49, 56–58). So a contract that only spends must still write
   five empty stubs.
2. The three traits are copy-paste of one another; the dispatch match at 16–29, 95–107 and 184–195
   is the same code three times (see §C.3).

The return type is `Unit`: validation is expressed as "throw or return", not as `Boolean`.

---

### A.3 `v2/Contexts.scala` (307 lines)

| Symbol | Line | Kind | Note |
| --- | --- | --- | --- |
| `enum OutputDatum { NoOutputDatum, OutputDatumHash(datumHash), OutputDatum(datum) }` | 13–16 | T | |
| `OutputDatum.inlineOrFail[A: FromData]: A` | 82 | **HL** | delegates to the 2-arg form with `"Expected inline datum"` |
| `OutputDatum.inlineOrFail[A: FromData](inline message: String): A` | 94 | **HL** | `case OutputDatum(datum) => datum.to[A]; case _ => fail(message)`; because it is `inline`, a statically-known `NoOutputDatum`/`OutputDatumHash` receiver is a **compile error** |
| `case class TxOut(address, value, datum = NoOutputDatum, referenceScript = Option.None)` | 100–105 | T | the `TxOut` v3 uses too (v3 exports it, v3:40) |
| `case class TxInInfo(outRef, resolved)` | 129 | T | distinct from v1 and v3 `TxInInfo` |
| `case class TxInfo(...12 fields...)` | 147–160 | T | `withdrawals: SortedMap[StakingCredential, BigInt]` (v3 uses `SortedMap[Credential, Lovelace]`) |
| `val TxInfo.placeholder` | 164 | P | |
| `TxInfo.findOwnInput(outRef)` | 213 | **HL** | |
| `TxInfo.findOwnDatum(datumHash)` | 217 | **HL** | |
| `TxInfo.findOwnScriptOutputs(scriptHash)` | 221 | **HL** | |
| `case class ScriptContext(txInfo, purpose)` | 229–232 | T | v2 uses `ScriptPurpose`, v3 uses `ScriptInfo` |
| `Utils.findInput` | 259 | **HL** | |
| `Utils.findDatum` | 274 | **HL** | the canonical implementation, re-exported by v3 |
| `Utils.findScriptOutputs` | 300 | **HL** | the canonical implementation, re-exported by v3 |

`Utils.findDatum` body (v2/Contexts.scala:274–289), quoted because it is the only two-source datum
resolver in the codebase:

```scala
def findDatum(
    outputs: List[TxOut],
    datum: SortedMap[DatumHash, Datum],
    datumHash: DatumHash
): Option[Datum] = {
    datum.get(datumHash) match
        case Option.Some(datum) => Option.Some(datum)
        case Option.None =>
            outputs.findMap { output =>
                output.datum match
                    case OutputDatum.OutputDatum(data) =>
                        if data.dataHash === datumHash then Option.Some(data)
                        else Option.None
                    case _ => Option.None
            }
}
```

Note: the fallback path calls `data.dataHash` (blake2b-256 over the serialised datum,
`uplc/builtin/DataApi.scala:26`) **once per output** — an expensive scan. `TxInfo.findOwnDatum`
inherits that cost.

`v2/Contexts.scala:29` and `:33` compare with `==` rather than `===` inside `Eq[OutputDatum]`:
```scala
case OutputDatumHash(datumHash2) => datumHash == datumHash2
```
(structural lowering makes this correct on-chain, but it is inconsistent with every other `Eq` in
the file, which uses `===`.)

### A.4 `v2/package.scala` (28 lines)

Pure re-export surface — 24 `export` lines from v1 (`Address`, `Closure`, `Credential`, `PolicyId`,
`DCert`, `Datum`, `DatumHash`, `IntervalBoundType`, `Interval`, `IntervalBound`, `PosixTime`,
`PosixTimeRange`, `PubKeyHash`, `Redeemer`, `RedeemerHash`, `ScriptHash`, `ScriptPurpose`,
`StakingCredential`, `TokenName`, `TxId`, `TxOutRef`, `ValidatorHash`, `Value`). Kind: **T**.
No new API.

---

### A.5 `v1/Contexts.scala` (841 lines)

#### Type aliases (lines 12–24) — Kind **T**

`Hash`, `ValidatorHash`, `Datum`, `DatumHash`, `Redeemer`, `ScriptHash`, `RedeemerHash`, `PolicyId`,
`TokenName`, `PosixTime`, `PosixTimeRange`, `Closure` — all `ByteString`/`Data`/`BigInt`/`Boolean`
aliases. **`ScriptHash`, `ValidatorHash`, `PolicyId`, `DatumHash`, `TokenName` are all the same
type** (`ByteString`), so the compiler cannot catch a policy-id/script-hash mix-up.

#### `Interval` / `IntervalBound` / `IntervalBoundType` — the time API

| Symbol | Line | Kind | Semantics |
| --- | --- | --- | --- |
| `enum IntervalBoundType { NegInf, Finite(time), PosInf }` | 31–34 | T | |
| `case class IntervalBound(boundType, isInclusive: Closure)` | 82 | T | |
| `IntervalBound.negInf` / `posInf` | 98 / 101 | P | inclusive infinities |
| `IntervalBound.finiteInclusive(time)` / `finiteExclusive(time)` | 104 / 108 | P | |
| `IntervalBound.min(lhs, rhs)` / `max(lhs, rhs)` | 114 / 123 | P | ties return lhs |
| `IntervalBound.finite(default: PosixTime): PosixTime` | 130 | **HL** | returns `default` on infinite bound |
| `IntervalBound.finiteOrFail(message: String): PosixTime` | 133 | **HL** | **fails** on infinite bound |
| `case class Interval(from, to)` | 145 | T | |
| `Interval.always` / `never` | 161 / 166 | P | `never = Interval(posInf, negInf)` |
| `Interval.after(time)` | 171 | P | `[time, +∞]` inclusive |
| `Interval.before(time)` | 176 | P | `[-∞, time]` inclusive |
| `Interval.entirelyBefore(time)` | 182 | P | `[-∞, time)` exclusive |
| `Interval.between(from, to)` | 188 | P | `[from, to]` |
| `Interval.entirelyBetween(from, to)` | 194 | P | `(from, to)` |
| `Interval.hull(lhs, rhs)` / `intersection(lhs, rhs)` | 200 / 209 | P | |
| `Interval.contains(time): Boolean` | 221 | **HL** | honours inclusivity on both ends; a `PosInf` lower bound ⇒ `false` |
| `Interval.isEntirelyAfter(time): Boolean` | 245 | **HL** | `false` when lower bound is infinite |
| `Interval.isEntirelyBefore(time): Boolean` | 256 | **HL** | `false` when upper bound is infinite |
| `Interval.isEntirelyBetween(after, before): Boolean` | 267 | **HL** | conjunction of the two |
| `Interval.isNever` / `nonNever` | 273 / 292 | **HL** | detects empty interval, incl. the open `(t, t+1)` case (284) |

There is **no** `Interval.after(time)` *predicate* named symmetrically with `isEntirelyAfter` — the
constructor and the predicate share the "after" word with different meanings. This is a naming trap
the new API should not inherit.

#### `Credential`, `Address`, `PubKeyHash`, `TxId`, `TxOutRef`

| Symbol | Line | Kind | Note |
| --- | --- | --- | --- |
| `case class TxId(hash: Hash)` + `toString = s"txid#..."` | 404–405 | T | v1 TxId is a `Hash` wrapper with default `ToData.derived` (413) |
| `TxId` `txid"…"` string interpolator | 416–418 | P | |
| `case class TxOutRef(id: TxId, idx: BigInt)` | 422 | T | |
| `TxOutRef.toOffchain: TransactionInput` `@Ignore` | 441 | OFF | |
| `case class PubKeyHash(hash: Hash)` `@UplcRepr(ProductCaseOneElement)` | 448–449 | T | `ToData` is *bare bytes* (460), not Constr |
| `PubKeyHash` `pkh"…"` interpolator | 464–466 | P | |
| `enum Credential { PubKeyCredential(hash: PubKeyHash), ScriptCredential(hash: ValidatorHash) }` | 469–471 | T | note the **asymmetric payload types** |
| `Credential.pubKeyOption: Option[PubKeyHash]` | 503 | **HL** | `None` for script |
| `Credential.scriptOption: Option[ValidatorHash]` | 507 | **HL** | `None` for pubkey |
| `enum StakingCredential { StakingHash(cred), StakingPtr(a,b,c) }` | 513–515 | T | |
| `case class Address(credential, stakingCredential: Option[StakingCredential])` | 548–551 | T | |
| `Address.fromCredential(credential)` | 570 | P | staking = `None` |
| `Address.fromScriptHash(script)` | 577 | P | |
| `Address.fromPubKeyHash(pubKey)` | 584 | P | |

There is **no** `Address.paymentCredential` accessor, no `Address.isScript`, no
`Address.scriptHashOption` — only `Credential.scriptOption` on the nested field.

#### v1 `TxOut`, `TxInInfo`, `TxInfo`, `ScriptPurpose`, `ScriptContext`, `Utils`

| Symbol | Line | Kind |
| --- | --- | --- |
| `case class TxOut(address, value, datumHash: Option[DatumHash])` | 589 | T |
| `case class TxInInfo(outRef, resolved)` | 616–619 | T |
| `case class TxInfo(...10 fields...)`; `withdrawals: List[(StakingCredential, BigInt)]`, `data: List[(DatumHash, Datum)]` (plain `List`, not `SortedMap`) | 635–645 | T |
| `TxInfo.placeholder` | 667 | P |
| `TxInfo.findOwnInput(outRef): Option[TxInInfo]` | 717 | **HL** |
| `TxInfo.findOwnScriptOutputs(scriptHash): List[TxOut]` | 728 | **HL** |
| `enum ScriptPurpose { Minting, Spending, Rewarding, Certifying }` | 734–738 | T |
| `case class ScriptContext(txInfo, purpose)` | 793 | T |
| `Utils.findInput(inputs, outRef)` | 821 | **HL** |
| `Utils.findScriptOutputs(outputs, scriptHash)` | 834 | **HL** |
| `enum DCert` (7 cases) | 298–306 | T |

v1 has **no** `findOwnDatum` (v1 stores datums as `List[(DatumHash, Datum)]`, no `SortedMap.get`).

Bug note (pre-existing, not introduced by this research): the hand-written `Eq[DCert]` at
v1/Contexts.scala:310–341 shadows its own binders and therefore compares each field to **itself**:

```scala
// v1/Contexts.scala:312-315
case DCert.DelegRegKey(cred) =>
    y match
        case DCert.DelegRegKey(cred) => cred === cred
        case _                       => false
```
The same shadowing appears in `Eq[ScriptPurpose]` at v1/Contexts.scala:743–760, e.g. line 747
`case ScriptPurpose.Minting(curSymbol) => curSymbol === curSymbol`. (On-chain `Eq` is lowered
structurally, so the bodies are never executed there — but off-chain these instances return `true`
for any two same-constructor values.)

---

### A.6 `v1/Value.scala` (1199 lines)

`Value` is `@UplcRepr(ProductCaseOneElement) case class Value private (toSortedMap: SortedMap[PolicyId, SortedMap[TokenName, BigInt]])`
(line 15–16). Private constructor ⇒ all construction goes through the companion.

#### Constructors and constants

| Signature | Line | Kind | Note |
| --- | --- | --- | --- |
| `val zero: Value` | 46 | P | built via `PairList.PairNil.toList` for cheaper UPLC |
| `def apply(cs: PolicyId, tn: TokenName, v: BigInt): Value` | 74 | P | `v == 0` ⇒ `zero` |
| `def lovelace(v: BigInt): Value` | 94 | P | |
| `def unsafeFromList(list)` | 121 | P | no validation |
| `def unsafeFromSortedMap(sm)` | 156 | P | no validation |
| `def fromList(list)` | 188 | P | filters zero amounts and empty policies |
| `def fromStrictlyAscendingListWithNonZeroAmounts(list)` | 240 | P | **fails** (`RequirementError`) on zero amount / empty token list |
| `val adaPolicyId: PolicyId = ByteString.empty` | 262 | P | |
| `val adaTokenName: TokenName = ByteString.empty` | 271 | P | |

#### Comparison / arithmetic (companion functions)

| Signature | Line | Kind | Note |
| --- | --- | --- | --- |
| `equalsAssets(a, b): Boolean` | 309 | P | treats absent as zero; tail-recursive |
| `eq(a, b): Boolean` / `nonEq(a, b)` | 351 / 374 | P | `eq` = `a.toSortedMap === b.toSortedMap` |
| `negate(v)` | 413 | P | PV11 ⇒ `scaleValue` builtin |
| `plus(a, b)` | 439 | P | PV11 ⇒ `unionValue` |
| `minus(a, b)` | 464 | P | PV11 ⇒ `scaleValue`+`unionValue` |
| `multiply(v, factor)` | 507 | P | `factor == 0` ⇒ `zero` |
| `debugToString(v)` `@Ignore` | 541 | OFF | |
| `given valueEq: Eq[Value]` | 576 | P | |
| `val valueOrd: Ord[Value]` | 595 | P | **not** a `given` — must be summoned explicitly (see the `given Ord[Value] = Value.valueOrd` lines in v1:607, v1:693, v2:117, v2:194, v3:895) |
| `given valueToData` / `valueFromData` | 613 / 626 | P | |
| `def valueFromDataWithValidation: FromData[Value]` | 646 | P | **fails** on zero amounts / empty token lists |
| `private binaryOpTokens` / `binaryOpValues` | 1051 / 1095 | P | merge-join over two sorted lists |

#### Extension methods on `Value`

| Signature | Line | Kind | Failure mode / semantics |
| --- | --- | --- | --- |
| `unary_-` | 665 | P | alias for `negate` |
| `+(other)` / `-(other)` / `*(factor)` | 668 / 671 / 674 | P | aliases |
| `showDebug` `@Ignore` | 678 | OFF | |
| `getLovelace: BigInt` | 701 | **HL** | `quantityOf(adaPolicyId, adaTokenName)`; `0` if absent |
| `lovelaceAmount: BigInt` | 719 | **HL** | `v.toSortedMap.toPairList.head._2.toPairList.head._2` — **assumes lovelace is the first entry**; on a value without lovelace it returns the *wrong* token's amount or fails on empty |
| `isZero` / `nonZero` | 734 / 750 | **HL** | `toSortedMap.isEmpty` |
| `isPositive` | 758 | **HL** | non-zero **and** every amount `> 0` |
| `quantityOf(cs, tn): BigInt` | 795 | **HL** | `0` when absent; PV11 ⇒ `lookupCoin` builtin |
| `containsAtLeast(other): Boolean` | 824 | **HL** | **`require`-fails** if either side holds a negative amount; PV11 ⇒ `valueContains` |
| `insertCoin(cs, tn, amount): Value` | 863 | **HL** | REPLACES (not adds); `amount == 0` deletes and keeps canonical form |
| `tokens(cs): SortedMap[TokenName, BigInt]` | 901 | **HL** | empty map when policy absent. **Doc comment at 896–898 is wrong** — it shows `value.quantityOf(...)` returning a `SortedMap` |
| `hasOnly(cs, tn, amount): Boolean` | 942 | **HL** | exact-mint check: `amount` of `tn` and **no other token under `cs`**; other policies unconstrained; single `equalsData` |
| `withoutLovelace: Value` | 984 | **HL** | `insertCoin(ada, ada, 0)` |
| `flatten: List[(PolicyId, TokenName, BigInt)]` | 1015 | **HL** | |
| `policyIds: List[PolicyId]` | 1049 | **HL** | `toSortedMap.keys` |
| `toLedgerValue: ledger.Value` `@Ignore` (in `private trait ValueOffchainOps`) | 1175 | OFF | throws `IllegalArgumentException` on Long overflow / bad hash sizes |

**Gaps relevant to the new design:** there is no `Value.lt/leq/gt/geq` (only `containsAtLeast`), no
`Value.assetsOf(policy).size`, no "value conservation" helper, no `Value.hasNft(policy)`,
no `Value.geqIgnoringAda`.

---

### A.7 `prelude/List.scala` (1469 lines)

`enum List[+A] { Nil, Cons(head, tail) }` (11–13). All members are **P**.

#### Companion

| Signature | Line | Note |
| --- | --- | --- |
| `inline def empty[A]: List[A]` | 28 | |
| `def unboxedNil[A]: List[A]` | 45 | opts A into native UPLC element repr; fails at lowering for a TypeVar |
| `inline def single[A](a)` | 48 | |
| `def apply[A](args: A*)` | 66 | off-chain only |
| `def from[A](i: IterableOnce[A])` / `from[A](i: java.lang.Iterable[A])` | 85 / 110 | off-chain |
| `def range(from, to)` / `rangeUntil(from, to)` | 132 / 155 | inclusive / exclusive |
| `def fill[A](value, times)` | 178 | |
| `def map2[A,B,C](a, b)(f)` | 209 | |
| `given listToData` / `listPairToData` / `listFromData` / `listPairsFromData` | 218 / 231 / 251 / 261 | |
| `given listEq[A: Eq]` / `listOrd[A: Ord]` / `showList[T: Show]` | 278 / 290 / 303 | |

#### Extensions

| Signature | Line | Note / failure mode |
| --- | --- | --- |
| `A.+:(list)` | 316 | cons operator |
| `(A: Ord).sort` / `quicksort` | 341 / 367 | quicksort |
| `List[List[A]].flatten` | 385 | |
| `!!(idx): A` | 393 | alias for `at` |
| `isEmpty` / `nonEmpty` | 405 / 419 | |
| `isDefinedAt(index)` | 437 | |
| `at(index): A` | 457 | **throws `NoSuchElementException`** on negative or out-of-range index (458, 462) |
| `get(index): Option[A]` | 487 | `None` on out of range |
| `contains[B >: A](elem)(using Eq[B])` | 518 | `find(_ === elem).isDefined` — linear |
| `groupBy[K: Ord](keyExtractor)` | 536 | |
| `groupMap[K: Ord, B](keyExtractor, valueExtractor)` | 560 | |
| `groupMapReduce[K: Ord, B](keyExtractor, valueExtractor, reducer)` | 606 | |
| `zip[B](other)` | 645 | truncates to shorter |
| `prepended` / `prependedAll` / `++:` | 669 / 687 / 696 | |
| `appended` / `:+` / `appendedAll` / `:++` / `concat` / `++` | 714 / 719 / 737 / 740 / 743 / 746 | |
| `map` / `flatMap` / `filter` / `filterNot` / `filterMap` | 764 / 786 / 805 / 827 / 850 | |
| `find(predicate): Option[A]` | 875 | |
| `findMap[B](mapper): Option[B]` | 897 | |
| `foldLeft` / `foldRight` | 924 / 947 | |
| `exists` / `forall` / `count` | 967 / 986 / 1004 | |
| `indexOf(elem)` / `indexOfOption(elem)` | 1027 / 1048 | `indexOf` returns `-1` when absent |
| `last: A` / `lastOption` | 1071 / 1087 | `last` **fails** `"last of empty list"` |
| `length` / `size` | 1103 / 1106 | O(n) fold |
| `head: A` / `headOption` | 1121 / 1136 | `head` **fails** `"head of empty list"` |
| `tail` | 1156 | |
| `drop` / `dropRight` / `dropWhile` | 1178 / 1202 / 1228 | |
| `deleteFirst(elem)` | 1254 | |
| `take` / `takeRight` / `takeWhile` | 1281 / 1305 / 1331 | |
| `distinct` / `diff(other)` | 1354 / 1382 | O(n²) |
| `init` / `reverse` / `foreach` | 1404 / 1419 / 1430 | |
| `asScala` / `toScalaList` `@Ignore` | 1449 / 1464 | OFF |

Note the two different index failure modes in one file: `at`/`!!` **throws**, `get` returns `Option`,
`indexOf` returns `-1`, `indexOfOption` returns `Option`.

### A.8 `prelude/Option.scala` (431 lines) — all **P**

| Signature | Line | Failure mode |
| --- | --- | --- |
| `enum Option[+A] { None, Some(a) }` | 15 | |
| `inline def apply[A](x)` | 36 | `null` ⇒ `None` |
| `inline def empty[A]` | 48 | |
| `given optionEq` / `emptyOptionEq` / `optionOrd` / `optionFromData` / `optionToData` | 50 / 63 / 65 / 76 / 81 | |
| `Option[Option[A]].flatten` | 104 | |
| `isEmpty` / `nonEmpty` / `isDefined` | 123 / 139 / 149 | |
| `inline def getOrFail(inline message: String = "None.getOrFail"): A` | 171 | **fails** |
| `def get: A` | 218 | **fails** `"None.get"` |
| `getOrElse[B >: A](default)` | 233 | |
| `orElse[B >: A](alternative)` | 251 | |
| `map` / `flatMap` / `filter` / `filterNot` | 272 / 294 / 315 / 337 | |
| `contains[B >: A](elem)(using Eq[B])` | 355 | |
| `exists(p)` / `forall(p)` | 373 / 392 | `forall` on `None` ⇒ `true` |
| `inline def find(p) = filter(p)` | 413 | |
| `asScala` `@Ignore` | 427 | OFF |

### A.9 `prelude/SortedMap.scala` (751 lines) — all **P**

`case class SortedMap[A, B] private (toList: List[(A, B)])` (17). Private constructor.

| Signature | Line | Note |
| --- | --- | --- |
| `empty` / `singleton(key, value)` | 32 / 47 | |
| `inline unsafeFromList(lst)` | 65 | no sort, no dedup |
| `fromList[A: Ord, B](lst)` | 84 | sorts; first occurrence wins (`insertIfDoesNotExist`, 85) |
| `fromStrictlyAscendingList[A: Ord, B](lst)` | 119 | **fails** if not strictly ascending |
| `from[A: Ord, B](it)` | 151 | off-chain iterable |
| `union[A: Ord, B, C](lhs, rhs): SortedMap[A, These[B, C]]` | 177 | merge-join |
| `unionMap[A: Ord, B, C, D](…)` | 242 | |
| `given sortedMapEq` / `sortedMapOrd` / `sortedMapFromData` / `sortedMapToData` | 283 / 289 / 296 / 334 | |
| `def sortedMapFromDataWithValidation[A: FromData: Ord, B: FromData]` | 317 | **fails** on non-ascending input |
| `toPairList` | 360 | |
| `isEmpty` / `nonEmpty` / `length` / `size` | 372 / 384 / 396 / 408 | |
| `keys` / `values` | 421 / 436 | |
| `forall` / `exists` | 454 / 470 | |
| `mapValues` / `filterKeys` / `filter` / `filterNot` | 486 / 501 / 517 / 532 | |
| `find` / `findMap` | 548 / 573 | |
| `foldLeft` / `foldRight` | 590 / 606 | |
| `get(key): Option[B]` (requires `Ord`) | 625 | early-exits on `Order.Greater` |
| `inline getOrFail(...)` | 659 | **fails** |
| `at(key): B` | 680 | **fails** `"Undefined key in SortedMap.at"` |
| `contains(key)` | 696 | |
| `insert(key, value)` / `delete(key)` | 711 / 738 | ordered insert/delete |

### A.10 `prelude/AssocMap.scala` (198 lines) — all **P**

`case class AssocMap[A, B](toList: List[(A, B)])` (11) — **public** constructor, `Eq`-keyed, unordered.

| Signature | Line |
| --- | --- |
| `empty` / `singleton` / `unsafeFromList` / `fromList[A: Eq, B]` | 19 / 20 / 21 / 23 |
| `given AssocMapFromData[A: FromData: Eq, B]` / `assocMapToData` | 30 / 44 |
| `toPairList` / `isEmpty` / `nonEmpty` / `length` / `size` | 66–70 |
| `keys` / `values` / `mapValues` / `forall` / `exists` | 71 / 74 / 77 / 79 / 80 |
| `filterKeys` / `filter` / `filterNot` / `find` / `foldLeft` / `foldRight` | 82 / 87 / 90 / 93 / 96 / 99 |
| `get(key)` (requires `Eq`) / `contains` / `insert` / `delete` | 111 / 122 / 124 / 137 |
| `union[A: Eq, B, C](lhs, rhs): AssocMap[A, These[B, C]]` | 148 |
| `inline given assocMapNoEq[A, B]: Eq[AssocMap[A, B]]` | 178 | compile-error guard: order-dependent equality |
| `object AssocMapEq.equals[A: Eq, B: Eq](lhs, rhs)` | 192–193 | explicit order-sensitive equality |

`AssocMap` and `SortedMap` are near-identical APIs over different invariants (§C.4).

### A.11 `prelude/PairList.scala` (200 lines) — all **P**

`enum PairList[+A, +B] { PairNil, PairCons(head: (A, B), tail) }` (24). A `List[(A,B)]` with a
UPLC-native pair representation.

| Signature | Line |
| --- | --- |
| `empty` / `single(a, b)` / `from(it)` | 33 / 35 / 38 |
| `head: (A, B)` / `tail` | 54 / 65 | (both **fail** on `PairNil`) |
| `toList` / `unsafeToSortedMap` / `unsafeToAssocMap` | 74 / 81 / 86 |
| `isEmpty` / `nonEmpty` / `length` | 88 / 92 / 94 |
| `mapValues` / `map` / `filter` / `filterNot` | 103 / 107 / 111 / 117 |
| `foldLeft` / `foldRight` / `forall` / `exists` / `find` / `findMap` | 120 / 124 / 128 / 132 / 136 / 142 |
| `prepended` / `++` / `asScala` | 149 / 151 / 156 |
| `given pairListToData` / `pairListFromData` / `pairListEq` | 161 / 171 / 178 |
| `List[(A,B)].toPairList` | 196–197 |

### A.12 `prelude/DataOps.scala` (9 lines)

Contains only `type BuiltinData = scalus.uplc.builtin.Data` (DataOps.scala:5). Everything else in the
file is commented out (DataOps.scala:3, 6, 8, 9). The actual `Data` operations live elsewhere:

- `uplc/builtin/Data.scala:90` — `inline def toData: Data = summon[ToData[A]](a)`
- `uplc/builtin/Data.scala:97` — `inline def to[A](using inline ev: FromData[A]): A = ev(data)`
- `uplc/builtin/DataApi.scala:26` — `inline def dataHash: ByteString`

### A.13 `prelude/Prelude.scala` (272 lines)

| Signature | Line | Kind | Note |
| --- | --- | --- | --- |
| `A.let[B](fn)` / `A.also[B](callback)` | 11 / 12 | P | scope functions |
| `Boolean.?` | 26 | P | traces only when `false` (macro) |
| `BigInt.to(other)` / `BigInt.until(other)` | 29 / 30 | P | ranges |
| `inline def log(inline args: Any*): Unit` | 34 | P | macro `LogMacros.logMacro` |
| `inline def identity[A](value)` | 35 | P | |
| `Prelude.encodeHexByteString` / `encodeHex` | 39 / 57 | P | |
| `Prelude.showByteStringBigInt` / `showBigInt` | 61 / 91 | P | |
| `inline def require(requirement: Boolean, message: String): Unit` | 109 | **HL** | throws `RequirementError` |
| `inline def require(requirement: Boolean): Unit` | 125 | **HL** | |
| `inline def fail(message: String): Nothing` | 137 | **HL** | throws `OnchainError` |
| `inline def fail(): Nothing` | 147 | **HL** | |
| `inline def impossible(): Nothing` | 156 | **HL** | throws `ImpossibleLedgerStateError` |
| `inline def ??? : Nothing` | 163 | P | |
| `enum These[+A, +B] { This, That, These }` | 165 | T | |
| `case class Rational(numerator, denominator)` | 190 | T | |
| `Rational.rationalNoEq` (compile error) | 202 | P | `Rational` has no usable `Eq` |
| `given Ord[Rational]` | 208 | P | cross-multiplication |
| `Rational.isZero` / `checkDenominator()` / `normalize` | 217 / 219 / 226 | P | `checkDenominator` **fails** on zero |
| `object RationalEq.equals(a, b)` | 240 | P | cross-multiplication equality |
| `scala.Seq[A].asScalus` / `scala.Option[A].asScalus` `@Ignore` | 260 / 269 | OFF | |

`require`/`fail`/`impossible` are the **only** failure vocabulary. There is no `expect`, no
`assertEq`, no error-code discipline.

### A.14 `prelude/Eq.scala` (85 lines), `Ord.scala` (182), `Order.scala` (39), `Show.scala` (115), `Math.scala` (384) — all **P**

`Eq.scala`:

| Signature | Line | Note |
| --- | --- | --- |
| `@FunctionalInterface trait Eq[-A] extends ((A, A) => Boolean)` | 19 | on-chain, `Eq` is a **marker** — instance bodies are never executed, `===` lowers to structural comparison |
| `A.===(y)(using Eq[A])` / `A.!==(y)` | 24 / 25 | |
| `Eq.apply[A: Eq]` / `Eq.derived[A]` (macro) | 29 / 32 | |
| `Eq.structural[A](f)` `@Ignore` | 44 | the only sanctioned way to hand-write an instance |
| `given Eq[Unit/BigInt/String/Boolean/Data]` | 46–50 | |
| `given Eq[(A,B)]` / `Eq[(A,B,C)]` | 52 / 55 | |
| `Eq.by` / `eqv` / `notEqv` / `orElse` / `orElseBy` | 63 / 66 / 67 / 73 / 80 | `by`, `orElse`, `orElseBy` are `@Ignore` (off-chain only) |

`Ord.scala`: `trait Ord[-A] extends ((A, A) => Order)` (10); `A.<=>(other)` (15); `given Ord` for
`Unit`(21) / `BigInt`(23) / `Boolean`(43) / `Data`(65) / tuples (145, 148); monomorphic `BigInt`
comparison fast path `< <= > >= equiv nonEquiv` (32–39); generic `A: Ord` comparison operators
(152–157); `Ord.by` (161); `compare/lt/lteq/gt/gteq/equiv/nonEquiv/orElse/orElseBy` (164–176);
`keyPairOrd` (180).

`Order.scala`: `enum Order { Less, Equal, Greater }` (5–6); `isLess`(27) `isLessEqual`(28)
`isGreater`(29) `isGreaterEqual`(30) `isEqual`(31) `nonEqual`(32); `inline infix def
ifEqualThen(other: => Order): Order`(34) — the lexicographic combinator used in every `Ord` instance.

`Show.scala`: `trait Show[A] extends (A => String)`(18); `trait ShowByteString[A]`(23);
`A.show`(27); `A.showByteString`(29); `given` instances for `Unit/BigInt/ByteString/String/Boolean/Data`
(35–44, 107–112).

`Math.scala`: `Math.abs`(26) `min`(38) `max`(50) `clamp`(70) `gcd`(91) `sqrt`(115) `isSqrt`(162)
`pow`(184) `exp2`(211) `log2`(240) `log`(274); plus `BigInt` extension aliases `absolute`(291)
`minimum`(301) `maximum`(311) `clamp`(323) `gcf`(333) `sqRoot`(340) `isSqrt`(350) `pow`(360)
`exp2`(367) `log2`(374) `logarithm`(384).

`Varargs.scala` (15 lines): `@UplcRepr(ProductCaseOneElement) case class Varargs[T](list: List[T])`
(Varargs.scala:9–10), plus `extension [T](seq: scala.collection.immutable.Seq[T]) def list: List[T]`
(Varargs.scala:12–15). Kind **P**.

### A.15 `onchain/Errors.scala` (13 lines)

```scala
class OnchainError(msg: String) extends RuntimeException(msg) {
    def this() = this("ERROR")
}

class RequirementError(msg: String) extends OnchainError(msg) {
    def this() = this("Requirement error")
}

class ImpossibleLedgerStateError(msg: String) extends OnchainError(msg) {
    def this() = this("impossible ledger state error")
}
```
(Errors.scala:3, :7, :11 for the three classes.) Kind: **T**. These are the *off-chain* throw
targets of `fail`/`require`/`impossible`; on-chain they all lower to the same UPLC `error` term, so
the class distinction is invisible to a validator.

### A.16 `onchain/plutus/package.scala` (19 lines)

```scala
type ScriptContext = v1.ScriptContext | v2.ScriptContext | v3.ScriptContext

object ScriptContext {
    def foldMap[T](sc: ScriptContext)(
        f1: v1.ScriptContext => T,
        f2: v2.ScriptContext => T,
        f3: v3.ScriptContext => T
    ): T = sc match {
        case sc1: v1.ScriptContext => f1(sc1)
        case sc2: v2.ScriptContext => f2(sc2)
        case sc3: v3.ScriptContext => f3(sc3)
    }
}
```
(package.scala:5 for the union type, :8–17 for `foldMap`.) Kind: **P**. The only cross-version
abstraction that exists — and it is a `foldMap`, not a common interface.

---

## B. `scalus-design-patterns` catalogue

Seven pattern modules (`scalus/patterns/`) plus seven usage examples (`scalus/examples/`). Path
prefix for every entry below: `scalus-design-patterns/src/main/scala/`.

### B.1 Summary

| Pattern | File | Problem it solves | On-chain? |
| --- | --- | --- | --- |
| `ParameterValidation` / `ParameterValidationOnChain` | `scalus/patterns/ParameterValidation.scala` | Prove a credential/address is a specific parameterised script instance | off-chain object + `@Compile` on-chain object |
| `StakeValidator` | `scalus/patterns/StakeValidator.scala` | "Withdraw-zero trick": delegate per-UTxO spend logic to a once-per-tx staking script | `@Compile` |
| `UtxoIndexer` | `scalus/patterns/UtxoIndexer.scala` | Match inputs to outputs by off-chain-computed index, avoiding O(n²) scans | `@Compile` |
| `TransactionLevelMinterValidator` | `scalus/patterns/TransactionLevelMinterValidator.scala` | Couple spend endpoint to the minting endpoint so heavy logic runs once | `@Compile` |
| `NormalizedInterval` | `scalus/patterns/NormalizedInterval.scala` | Collapse `Interval`'s 3×3×2 bound space into 4 inclusive cases | `@Compile` |
| `MerkelizedValidator` | `scalus/patterns/MerkelizedValidator.scala` | Offload expensive computation to a stake validator; spend validators read its verified redeemer | `@Compile` |
| `LinkedList` | `scalus/patterns/LinkedList.scala` | UTxO-based on-chain singly linked list (NFT per node, inline `Element` datum) | `@Compile` |

### B.2 `ParameterValidation.scala` (163 lines)

Off-chain `object ParameterValidation` (line 41, **not** `@Compile`):

| Signature | Line |
| --- | --- |
| `def computeScriptHashV3(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash` | 52 |
| `def computeScriptHashV2(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash` | 66 |
| `def computeScriptHashV1(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash` | 80 |

All three are the same body with a different `Script.PlutusVn` constructor (§C.5).

On-chain `@Compile object ParameterValidationOnChain` (line 92):

| Signature | Line | Failure mode |
| --- | --- | --- |
| `inline def verifyScriptCredential(credential: Credential, expectedHash: ValidatorHash): Unit` | 104 | **fails** twice: `getOrFail(ExpectedScriptCredential)` then `require(..., ScriptHashMismatch)` |
| `inline def verifyAddressScript(address: Address, expectedHash: ValidatorHash): Unit` | 119 | delegates to the above |
| `inline def findOutputsToScript(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut]` | 135 | returns `List.Nil` |
| `inline def isExpectedScript(credential: Credential, expectedHash: ValidatorHash): Boolean` | 152 | `false` |
| `inline val ExpectedScriptCredential`, `ScriptHashMismatch` | 161, 162 | message constants |

**Re-implemented primitives.** `findOutputsToScript` (ParameterValidation.scala:135–141)
re-implements `v2.Utils.findScriptOutputs` (v2/Contexts.scala:300–306) and
`TxInfo.findOwnOutputsByCredential` (v3/Contexts.scala:1041–1042):

```scala
// scalus/patterns/ParameterValidation.scala:135
inline def findOutputsToScript(
    outputs: List[TxOut],
    scriptHash: ValidatorHash
): List[TxOut] = {
    val scriptCred = Credential.ScriptCredential(scriptHash)
    outputs.filter(_.address.credential === scriptCred)
}
```
vs.
```scala
// scalus-core .../plutus/v2/Contexts.scala:300
def findScriptOutputs(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut] = {
    outputs.filter { output =>
        output.address.credential match
            case Credential.ScriptCredential(hash) => hash === scriptHash
            case _                                 => false
    }
}
```
Different strategy (build-a-credential-and-compare vs. pattern-match), identical semantics.
**Stdlib candidate.**

`verifyScriptCredential` / `isExpectedScript` are the assert- and boolean- forms of one operation
core does not have at all — core offers only `Credential.scriptOption: Option[ValidatorHash]`
(v1/Contexts.scala:507). **Stdlib candidate** (`Credential.isScript(hash)` / `expectScript(hash)`).

### B.3 `StakeValidator.scala` (74 lines)

| Signature | Line | Failure mode |
| --- | --- | --- |
| `def spend(withdrawalScriptHash: ValidatorHash, withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean, txInfo: TxInfo): Unit` | 32 | **fails** ×3 |
| `def spendMinimal(withdrawalScriptHash: ValidatorHash, txInfo: TxInfo): Unit` | 51 | **fails** (`MissingWithdrawal`) |
| `def withdraw[T](withdrawalValidator: (T, ValidatorHash, TxInfo) => Boolean, redeemer: T, credential: Credential, txInfo: TxInfo): Unit` | 59 | **fails** ×2 |
| 5 × `inline val` message constants | 68–72 | |

**Re-implemented primitives** (StakeValidator.scala:37–41):

```scala
val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)

val redeemer = txInfo.redeemers.getOrFail(scriptPurpose, MissingRedeemer)
val withdrawalAmount = txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)
```

"Look up the reward redeemer / reward withdrawal of a staking script" is a `ScriptCredential` →
`ScriptPurpose.Rewarding` → `SortedMap.getOrFail` dance core provides no helper for. It reappears
verbatim in `MerkelizedValidator` (§C.6). **Stdlib candidate:** `TxInfo.rewardRedeemerOf(scriptHash)`,
`TxInfo.withdrawalOf(scriptHash)`.

`withdraw` re-implements script-hash extraction from a `Credential` (StakeValidator.scala:65) — the
same `credential.scriptOption.getOrFail(...)` as `ParameterValidationOnChain.verifyScriptCredential`
(ParameterValidation.scala:108):

```scala
// scalus/patterns/StakeValidator.scala:65
val validatorHash = credential.scriptOption.getOrFail(PubKeyCredentialNotSupported)
```
```scala
// scalus/patterns/ParameterValidation.scala:108
val actualHash = credential.scriptOption.getOrFail(ExpectedScriptCredential)
```

### B.4 `UtxoIndexer.scala` (256 lines)

| Signature | Line | Failure mode |
| --- | --- | --- |
| `def validateInput(ownRef: TxOutRef, inputIdx: BigInt, tx: TxInfo, validator: TxInInfo => Boolean): Unit` | 24 | **fails** (`at` out-of-range, `InputIndexMismatch`, `ValidatorFailed`) |
| `def oneToOne(ownRef, inputIdx, outputIdx, tx, validator: (TxInInfo, TxOut) => Boolean): Unit` | 36 | **fails** |
| `def oneToMany(ownRef, inputIdx, outputIndices: List[BigInt], tx, perOutputValidator, collectiveValidator): Unit` | 55 | **fails** |
| `def multiOneToOneNoRedeemer(indexPairs: List[(BigInt, BigInt)], scriptHash, tx, validator): Unit` | 77 | **fails** (`UnprocessedIndexPairs`) |
| `def multiOneToOneWithRedeemer[A](indexPairs, spendingScriptHash, stakeScriptHash, tx, redeemerCoercerAndStakeExtractor: Data => (A, Credential), validator): Unit` | 98 | **fails** (`UnprocessedRedeemers`) |
| private `validateAndCollectOutputs` (`@tailrec`, default arg `acc = List.Nil`) | 130 | |
| private `processMultipleInputs` (`@tailrec`, default arg `currentIdx = BigInt(0)`) | 147 | |
| private `filterAndCoerceRedeemers` | 192 | |
| private `processMultipleInputsWithRedeemers` (`@tailrec`) | 210 | |
| 10 × `inline val` message constants | 245–255 | |

**Re-implemented primitives:**

1. Index-based own-input lookup replacing `TxInfo.findOwnInput` — three times, identically:

```scala
// scalus/patterns/UtxoIndexer.scala:30-31   (validateInput)
val input = tx.inputs.at(inputIdx)
require(input.outRef === ownRef, InputIndexMismatch)
```
```scala
// scalus/patterns/UtxoIndexer.scala:43-44   (oneToOne)
val input = tx.inputs.at(inputIdx)
require(input.outRef === ownRef, InputIndexMismatch)
```
```scala
// scalus/patterns/UtxoIndexer.scala:63-64   (oneToMany)
val input = tx.inputs.at(inputIdx)
require(input.outRef === ownRef, InputIndexMismatch)
```
This is the *O(1)* alternative to `TxInfo.findOwnInput` (v3/Contexts.scala:938), which is O(n). Core
offers no `findOwnInputAt(idx, ownRef)`. **Stdlib candidate** — and the examples confirm demand
(§C.8).

2. Filtering inputs by a script credential — reimplements `TxInfo.findOwnInputsByCredential`
(v3/Contexts.scala:1022), fused into a traversal so it can count and index in one pass:

```scala
// scalus/patterns/UtxoIndexer.scala:161
if input.resolved.address.credential === scriptCredential then
```
```scala
// scalus/patterns/UtxoIndexer.scala:227
require(
  input.resolved.address.credential === spendingCredential,
  InputNotFromSpendingScript
)
```

3. `Credential.ScriptCredential(hash)` construction, twice (UtxoIndexer.scala:83, 106–107).

### B.5 `TransactionLevelMinterValidator.scala` (50 lines)

| Signature | Line | Failure mode |
| --- | --- | --- |
| `def spend(minterScriptHash: ValidatorHash, minterRedeemerValidator: Redeemer => Boolean, minterTokensValidator: SortedMap[TokenName, BigInt] => Boolean, txInfo: TxInfo): Unit` | 27 | **fails** ×3 |
| `def spendMinimal(minterScriptHash: ValidatorHash, txInfo: TxInfo): Unit` | 43 | **fails** (`MissingMint`) |
| 4 × `inline val` message constants | 46–49 | |

**Re-implemented primitive** — `Value.tokens(cs)` (v1/Value.scala:901–902), character for character:

```scala
// scalus/patterns/TransactionLevelMinterValidator.scala:34
val tokens = txInfo.mint.toSortedMap.get(minterScriptHash).getOrElse(SortedMap.empty)
```
```scala
// scalus-core .../plutus/v1/Value.scala:901
def tokens(cs: PolicyId): SortedMap[TokenName, BigInt] =
    v.toSortedMap.get(cs).getOrElse(SortedMap.empty)
```
**Direct duplication of an existing core method.**

`spendMinimal` (line 44) reaches into `Value`'s internal map for a "policy is present" check `Value`
does not expose:
```scala
def spendMinimal(minterScriptHash: ValidatorHash, txInfo: TxInfo): Unit =
    txInfo.mint.toSortedMap.getOrFail(minterScriptHash, MissingMint)
```
**Stdlib candidate:** `Value.mintsUnder(policyId): Boolean` / `Value.tokensOrFail(policyId)`.

Note: the parameter is named `minterScriptHash: ValidatorHash` but used as a `PolicyId` — legal only
because `ValidatorHash`, `PolicyId` and `ScriptHash` are all `ByteString` aliases
(v1/Contexts.scala:13, 17, 19).

### B.6 `NormalizedInterval.scala` (231 lines)

| Signature | Line | Failure mode |
| --- | --- | --- |
| `enum NormalizedInterval { ClosedRange(lower, upper), FromNegInf(upper), ToPosInf(lower), Always } derives ToData, FromData` | 14–18 | |
| `extension (self: Interval) inline def tryNormalize: Option[NormalizedInterval]` | 65 | `Option.None` on improper interval |
| `extension (self: Interval) inline def normalize: NormalizedInterval` | 113 | **fails** `"Improper interval encountered"` |
| `given Show[NormalizedInterval]` / `Eq` / `Ord` | 118 / 143 / 163 | |
| `def tryNormalizedInterval(interval: Interval): Option[NormalizedInterval]` | 186 | |
| `def normalizedInterval(interval: Interval): NormalizedInterval` | 218 | **fails** |
| private `resolveLower(lower, isInclusive)` / `resolveUpper(upper, isInclusive)` | 224 / 228 | |

**Re-implemented primitives:** all of `tryNormalizedInterval` (186–216) is a second, independent
decision procedure over the same `IntervalBoundType` × `isInclusive` space that `Interval.contains` /
`isEntirelyAfter` / `isEntirelyBefore` / `isNever` already walk (v1/Contexts.scala:221–287).
`Option.None` for an improper interval restates `Interval.isNever`:

```scala
// scalus/patterns/NormalizedInterval.scala:202-207
case IntervalBoundType.Finite(upperTime) =>
    val lower = resolveLower(lowerTime, interval.from.isInclusive)
    val upper = resolveUpper(upperTime, interval.to.isInclusive)

    if lower > upper then Option.None
    else Option.Some(ClosedRange(lower, upper))
```
```scala
// scalus-core .../plutus/v1/Contexts.scala:273-287
def isNever: Boolean =
    self.from <=> self.to match
        case Order.Greater => true
        case Order.Equal   => !(self.from.isInclusive && self.to.isInclusive)
        case Order.Less =>
            val isOpenInterval = !self.from.isInclusive && !self.to.isInclusive
            if isOpenInterval then
                self.from.boundType match
                    case IntervalBoundType.Finite(fromTime) =>
                        self.to.boundType match
                            case IntervalBoundType.Finite(toTime) =>
                                fromTime + 1 === toTime
                            case _ => false
                    case _ => false
            else false
```

`resolveLower`/`resolveUpper` (224–230) are the exclusive→inclusive conversion `Interval.contains`
performs inline (v1/Contexts.scala:225, 233):
```scala
// NormalizedInterval.scala:224
private def resolveLower(lower: BigInt, isInclusive: Boolean): BigInt =
    if isInclusive then lower else lower + 1
```
```scala
// v1/Contexts.scala:224-225
case IntervalBoundType.Finite(from) =>
    if self.from.isInclusive then time >= from else time > from
```

### B.7 `MerkelizedValidator.scala` (82 lines)

| Signature | Line | Failure mode |
| --- | --- | --- |
| `def getStakeRedeemer(stakeValidatorHash: ValidatorHash, txInfo: TxInfo): Redeemer` | 56 | **fails** (`MissingStakeRedeemer`) |
| `def verifyAndGetRedeemer(stakeValidatorHash: ValidatorHash, txInfo: TxInfo): Redeemer` | 72 | **fails** ×2 |
| `inline val MissingStakeRedeemer` / `MissingWithdrawal` | 80 / 81 | |

**Re-implemented primitives** — a near-copy of `StakeValidator.spend`'s lookup half:

```scala
// scalus/patterns/MerkelizedValidator.scala:72-78
def verifyAndGetRedeemer(stakeValidatorHash: ValidatorHash, txInfo: TxInfo): Redeemer =
    val scriptCredential = Credential.ScriptCredential(stakeValidatorHash)
    // Verify withdrawal exists (stake validator executed)
    txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)
    // Return the redeemer
    val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)
    txInfo.redeemers.getOrFail(scriptPurpose, MissingStakeRedeemer)
```
```scala
// scalus/patterns/StakeValidator.scala:32-41
def spend(
    withdrawalScriptHash: ValidatorHash,
    withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean,
    txInfo: TxInfo
): Unit =
    val scriptCredential = Credential.ScriptCredential(withdrawalScriptHash)
    val scriptPurpose = ScriptPurpose.Rewarding(scriptCredential)

    val redeemer = txInfo.redeemers.getOrFail(scriptPurpose, MissingRedeemer)
    val withdrawalAmount = txInfo.withdrawals.getOrFail(scriptCredential, MissingWithdrawal)
```
Same three operations, different order, different message constants: `MissingRedeemer`
(StakeValidator.scala:68, `"There isn't a redeemer for the script purpose"`) vs
`MissingStakeRedeemer` (MerkelizedValidator.scala:80,
`"Stake validator redeemer not found in txInfoRedeemers"`); `MissingWithdrawal` declared twice with
**different text** — StakeValidator.scala:69 `"There isn't a withdrawal for the script credential"`
vs MerkelizedValidator.scala:81 `"Stake validator withdrawal not found"`.

### B.8 `LinkedList.scala` (563 lines)

Types: `type RootKey = TokenName`(17), `NodeKey`(20), `NodeKeyPrefix`(23), `NodeKeyPrefixLength`(26),
`Link = Option[NodeKey]`(29); `enum ElementData { Root(data), Node(data) }`(35);
`case class Element(data: ElementData, link: Link)`(56).

| Signature | Line | Failure mode |
| --- | --- | --- |
| `def init(rootOut: TxOut, txMint: Value, policyId: PolicyId, rootKey: RootKey): Unit` | 79 | **fails** ×4 |
| `def deinit(rootInput: TxInInfo, txMint, policyId, rootKey): Unit` | 100 | **fails** ×4 |
| `def insert(anchorInput, contAnchorOutput, newElementOutput, txMint, policyId, rootKey, prefix, prefixLen): Unit` | 130 | **fails** ×7 |
| `def appendUnordered(...same 8 params...)` | 202 | **fails** ×6 |
| `def prependUnordered(rootInput, contRootOutput, newElementOutput, txMint, policyId, rootKey, prefix, prefixLen)` | 260 | **fails** ×6 |
| `def remove(anchorInput, removingNodeInput, contAnchorOutput, txMint, policyId, rootKey, prefix, prefixLen)` | 315 | **fails** ×5 |
| `def removeHead(rootInput, headNodeInput, contRootOutput, txMint, policyId, rootKey, prefix, prefixLen)` | 376 | **fails** ×11 |
| `def requireListTokensMintedOrBurned(policyId: PolicyId, txMint: Value): Unit` | 422 | **fails** |
| `def validateElementUpdate(elementInputIndex, contElementOutputIndex, elementInputOutref, txInputs, txOutputs, txMint, policyId, rootKey, prefix, prefixLen)` | 440 | **fails** ×8 |
| private `validateAnchorAssetName` | 486 | |
| private `extractKey` / `hasPrefix` | 502 / 505 | |
| private `authenticateElementUtxoAndGetInfo(output, policyId): (Address, TokenName, ElementData, Link)` | 512 | **fails** ×4 |
| private `validateThreeElements(...)` | 531 | **fails** ×4 |

**Re-implemented primitives** — `authenticateElementUtxoAndGetInfo` (512–529) packs four of them:

```scala
// scalus/patterns/LinkedList.scala:516-526
val datum = output.datum match
    case OutputDatum.OutputDatum(d) => d
    case _                          => fail("Element UTxO must have inline datum")
require(output.referenceScript === None, "Element UTxO must not have a reference script")
val (assetName, qty) = output.value.toSortedMap.get(policyId) match
    case None => fail("Element UTxO must contain a list NFT")
    case Some(tokens) =>
        tokens.toList match
            case List.Cons((assetName, qty), List.Nil) => (assetName, qty)
            case _ => fail("Element UTxO must contain exactly one list NFT")
require(qty == BigInt(1), "NFT quantity must be exactly 1")
```

| Fragment | Re-implements | Core equivalent |
| --- | --- | --- |
| 516–518 inline-datum extraction | `OutputDatum.inlineOrFail` | v2/Contexts.scala:94; identical semantics, but LinkedList returns raw `Data` and decodes at line 527 |
| 520 `output.value.toSortedMap.get(policyId)` | `Value.tokens(cs)` | v1/Value.scala:901 |
| 523–525 "exactly one token under the policy" | *nothing in core* | closest is `Value.hasOnly(cs, tn, amount)` (v1/Value.scala:942), which needs the token name up front; here the name is the *output* |
| 526 `require(qty == BigInt(1), ...)` | *nothing in core* | NFT-quantity assertion |

`requireListTokensMintedOrBurned` (422–426) and the head of `validateElementUpdate` (452–455) are
two opposite checks over one expression, written twice:

```scala
// scalus/patterns/LinkedList.scala:422-426
def requireListTokensMintedOrBurned(policyId: PolicyId, txMint: Value): Unit =
    txMint.toSortedMap.get(policyId) match
        case None => fail("No list tokens minted or burned in this transaction")
        case Some(m) =>
            require(!m.isEmpty, "No list tokens minted or burned in this transaction")
```
```scala
// scalus/patterns/LinkedList.scala:452-455
txMint.toSortedMap.get(policyId) match
    case None => ()
    case Some(m) =>
        require(m.isEmpty, "No list tokens may be minted or burned during update")
```

`validateElementUpdate` (457–462) repeats the `UtxoIndexer` index-then-check idiom:
```scala
// scalus/patterns/LinkedList.scala:457-462
val elemIn = txInputs.at(elementInputIndex)
require(
  elemIn.outRef === elementInputOutref,
  "Input index does not match elementInputOutref"
)
val elemOut = txOutputs.at(contElementOutputIndex)
```
vs `UtxoIndexer.scala:43-46` — same two lines, different message constant.

### B.9 The seven pattern *examples*

| Example | File | What it calls into `scalus.patterns.*` | What it still hand-rolls around the call |
| --- | --- | --- | --- |
| `StakeValidatorExample` | `scalus/examples/StakeValidatorExample.scala` | `StakeValidator.spend(...)`, `StakeValidator.withdraw(...)` | own-script-hash boilerplate at :25–26 |
| `StakeValidatorPaymentSplitterExample` | `scalus/examples/StakeValidatorPaymentSplitterExample.scala` | `StakeValidator.spendMinimal(ownScriptHash, tx)` (:89–91 preamble) | the whole reward-side verification: input-classifying `foldLeft` (:118–129), output-classifying `foldLeft` (:138–157), remainder check (:159–169) |
| `TransactionLevelMinterValidatorExample` | `scalus/examples/TransactionLevelMinterValidatorExample.scala` | `TransactionLevelMinterValidator.spend(...)` (:51–61) | index-based own-input check (:36–43), own-script-hash (:46), script-input counting `foldRight` (:69–74) |
| `UtxoIndexerExample` | `scalus/examples/UtxoIndexerExample.scala` | `UtxoIndexer.oneToOne(...)` | nothing on-chain; the *off-chain* half hand-rolls the index computation (`inputs.toSeq.indexOf`, `outputs.indexWhere`) |
| `ParameterValidationExample` | `scalus/examples/ParameterValidationExample.scala` | `ParameterValidationOnChain.verifyAddressScript(...)`, off-chain `ParameterValidation.computeScriptHashV3(...)` | does **not** extend `Validator`/`ParameterizedValidator` — hand-rolls `ScriptContext`/`ScriptInfo` dispatch at :56–60 and :116–119; hand-rolls `Value.hasOnly` at :125–132; royalty split `foldLeft` at :72–80 |
| `BatchAuctionExample` | `scalus/examples/BatchAuctionExample.scala` | `MerkelizedValidator.verifyAndGetRedeemer(...)` | own-script-hash (:83–85), output-membership `exists` (:100–105, :114–121), inputs `foldLeft` with a hand-rolled `inlineOrFail` (:139–153) |
| `MultiPoolDexExample` | `scalus/examples/MultiPoolDexExample.scala` | `MerkelizedValidator.verifyAndGetRedeemer(order.poolScriptHash, tx)` | signatory check via `.exists` (:214–217), owner-payout `foldLeft` (:192–197), constant-product math (:148–156) |

The recurring shape is: the pattern library covers the *coupling mechanism* (withdraw-zero, index
pairing, script-hash parameterisation) but every example still hand-writes the *shape checks*
around it.

---

## C. Same conceptual operation implemented more than once

Path prefixes used below (all relative to the repo root):

- `CORE` = `scalus-core/shared/src/main/scala/scalus/cardano/onchain/`
- `PAT` = `scalus-design-patterns/src/main/scala/scalus/`
- `EX` = `scalus-examples/jvm/src/main/scala/scalus/examples/`
- `EXS` = `scalus-examples/shared/src/main/scala/scalus/examples/`
- `EXL` = `scalus-examples/lottery-complete/src/main/scala/lottery/onchain/`

### C.0 Index of duplications

| # | Operation | Distinct implementations | Worst offender |
| --- | --- | --- | --- |
| C.1 | Ledger types per Plutus version | 3× `TxId`, 3× `TxOutRef`, 3× `TxInInfo`, 3× `TxInfo`, 2× `TxOut`, 2× `ScriptContext` | inherent to the ledger spec, but v3 `TxId` uses a *different* Data encoding than v1 |
| C.2 | `findInput` | 3 in core + 1 hand-rolled in an example | `CORE plutus/v1/Contexts.scala:821`, `v2:259`, `v3:1141` |
| C.3 | Validator `scriptInfo` dispatch | 3 in core + 2 hand-rolled | `CORE plutus/v3/Validator.scala:16`, `:95`, `:184` |
| C.4 | Ordered/unordered key-value map API | `SortedMap` vs `AssocMap` (≈25 members each) | `CORE plutus/prelude/SortedMap.scala` vs `AssocMap.scala` |
| C.5 | `computeScriptHash` | 3 (V1/V2/V3) | `PAT patterns/ParameterValidation.scala:52/66/80` |
| C.6 | Reward redeemer/withdrawal lookup | 2 in patterns | `PAT patterns/StakeValidator.scala:37` vs `MerkelizedValidator.scala:73` |
| C.7 | "Is the tx signed by X" | **6 spellings** | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:170` |
| C.8 | Find own input | **4 spellings** | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:158` |
| C.9 | Outputs by address/credential | **4 spellings** | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:167` |
| C.10 | "Exactly one" | **4 idioms** | scattered |
| C.11 | Inline-datum extraction | 2 (`inlineOrFail` vs manual match) | `EXL LotteryValidator.scala:79` vs `EX lottery/LotteryValidator.scala:100` |
| C.12 | Own script hash from credential | 2 (`scriptOption.getOrFail` vs manual match) | `EX editablenft:136` ≡ `EX decentralizedidentity:249` |
| C.13 | Exact-mint check | **5 idioms** | `EX MembershipToken.scala:106` |
| C.14 | Lovelace read | 2 in core (`getLovelace`/`lovelaceAmount`) + 8 hand-rolled folds | `CORE plutus/v1/Value.scala:701` vs `:719` |
| C.15 | Deadline check | 2 (`isEntirelyBefore/After` vs manual bound extraction) | `EX htlc/HtlcValidator.scala:57` |
| C.16 | `Value.tokens` | 2 (core method + verbatim re-expression) | `PAT patterns/TransactionLevelMinterValidator.scala:34` |
| C.17 | Whole-file forks | 4 pairs/families | lottery, auction, paymentsplitter, setbench |
| C.18 | Value/`Data` equality | 2 (`===` vs `.toData ==`) | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:96` |
| C.19 | Failure idiom | 3 (`require`/`fail` vs raw `throw` vs local `check`) | `EXS MintingPolicy.scala:98` |

### C.1 The same ledger concept, three type definitions

| Concept | v1 | v2 | v3 |
| --- | --- | --- | --- |
| `TxId` | `CORE plutus/v1/Contexts.scala:404` `case class TxId(hash: Hash)`, `ToData.derived` (:413) | re-exported (`v2/package.scala:24`) | `CORE plutus/v3/Contexts.scala:46` `@UplcRepr(ProductCaseOneElement) case class TxId(hash: ByteString)`, `ToData = bData(x.hash)` (:53) |
| `TxOutRef` | `v1:422` | re-exported (`v2/package.scala:25`) | `v3:58` (own `Eq`/`Ord`/codecs, `v3:62-67`) |
| `TxInInfo` | `v1:616` (`resolved: v1.TxOut`) | `v2:129` (`resolved: v2.TxOut`) | `v3:836` (`resolved: v2.TxOut`) |
| `TxOut` | `v1:589` (`datumHash: Option[DatumHash]`) | `v2:100` (`datum: OutputDatum`, `referenceScript`) | re-exports `v2.TxOut` (`v3:40`) |
| `TxInfo` | `v1:635` (10 fields, `data: List[(DatumHash, Datum)]`) | `v2:147` (12 fields, `data: SortedMap[...]`) | `v3:854` (16 fields, `fee: Lovelace` not `Value`) |
| `ScriptContext` | `v1:793` (`purpose: ScriptPurpose`) | `v2:229` (`purpose: ScriptPurpose`) | `v3:1108` (`redeemer` + `scriptInfo: ScriptInfo`) |
| discriminator | `enum ScriptPurpose` `v1:734` (4 cases) | same (re-exported) | `enum ScriptPurpose` `v3:658` (6 cases) **and** `enum ScriptInfo` `v3:746` (6 cases) |

`ScriptPurpose` and `ScriptInfo` in v3 are the same six constructors with `SpendingScript` carrying
an extra `datum` field — two enums for one concept:

```scala
// CORE plutus/v3/Contexts.scala:658-664
enum ScriptPurpose:
    case Minting(policyId: PolicyId)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(credential: Credential)
    case Certifying(index: BigInt, cert: TxCert)
    case Voting(voter: Voter)
    case Proposing(index: BigInt, procedure: ProposalProcedure)
```
```scala
// CORE plutus/v3/Contexts.scala:746-752
enum ScriptInfo:
    case MintingScript(policyId: PolicyId)
    case SpendingScript(txOutRef: TxOutRef, datum: Option[Datum] = Option.None)
    case RewardingScript(credential: Credential)
    case CertifyingScript(index: BigInt, cert: TxCert)
    case VotingScript(voter: Voter)
    case ProposingScript(index: BigInt, procedure: ProposalProcedure)
```
Their `Eq` (`:668` / `:756`) and `Ord` (`:697` / `:786`) instances are the same 40 lines twice.

### C.2 `findInput` — three copies in core, one hand-rolled

```scala
// CORE plutus/v1/Contexts.scala:821
def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
    inputs.find(_.outRef === outRef)
}
```
```scala
// CORE plutus/v2/Contexts.scala:259
def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
    inputs.find(_.outRef === outRef)
}
```
```scala
// CORE plutus/v3/Contexts.scala:1141
def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
    inputs.find(_.outRef === outRef)
}
```
Byte-identical bodies; only the `TxInInfo`/`TxOutRef` types differ. `findScriptOutputs` is likewise
duplicated between `v1:834` and `v2:300` (v3 re-exports v2's at `v3:1145`, so only v1 is the
straggler):

```scala
// CORE plutus/v1/Contexts.scala:834          // CORE plutus/v2/Contexts.scala:300
def findScriptOutputs(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut] = {
    outputs.filter { output =>
        output.address.credential match
            case Credential.ScriptCredential(hash) => hash === scriptHash
            case _                                 => false
    }
}
```

A fourth implementation exists in an example, hand-rolled with `.toData` equality instead of `Eq`:
```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:158-165
def findOwnInputOrFail(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo = {
    def go(inputs: List[TxInInfo]): TxInInfo = inputs match
        case List.Cons(head, tail) =>
            if head.outRef.toData == txOutRef.toData then head
            else go(tail)
        case List.Nil => fail("Own input not found")
    go(inputs)
}
```

### C.3 Validator dispatch — three copies in core, two hand-rolled

```scala
// CORE plutus/v3/Validator.scala:16-29 (trait Validator)
sc.scriptInfo match
    case ScriptInfo.MintingScript(policyId) =>
        mint(sc.redeemer, policyId, sc.txInfo)
    case ScriptInfo.SpendingScript(txOutRef, datum) =>
        spend(datum, sc.redeemer, sc.txInfo, txOutRef)
    ...
```
```scala
// CORE plutus/v3/Validator.scala:95-107 (trait ParameterizedValidator[A])
sc.scriptInfo match
    case ScriptInfo.MintingScript(policyId) =>
        mint(param, sc.redeemer, policyId, sc.txInfo)
    ...
```
```scala
// CORE plutus/v3/Validator.scala:184-195 (trait DataParameterizedValidator)
sc.scriptInfo match
    case ScriptInfo.MintingScript(policyId) =>
        mint(param, sc.redeemer, policyId, sc.txInfo)
    ...
```
`ParameterizedValidator` and `DataParameterizedValidator` differ **only** in `A` vs `Data`.

Two examples bypass the traits and rewrite the dispatch:
```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:51-57
inline def validate(scData: Data): Unit = {
    val sc = scData.to[ScriptContext]
    sc.scriptInfo match
        case ScriptInfo.SpendingScript(txOutRef, datum) =>
            spend(datum, sc.redeemer, sc.txInfo, txOutRef)
        case _ => fail("Only spending scripts are supported by this validator")
}
```
```scala
// PAT examples/ParameterValidationExample.scala:56-60
inline def validate(creatorPkh: PubKeyHash)(scData: Data): Unit = {
    val sc = scData.to[ScriptContext]
    sc.scriptInfo match
        case ScriptInfo.SpendingScript(_, datum) =>
```
(second copy in the same file at `:116-119` for the minting policy). The reason is visible in
`CORE plutus/v3/Validator.scala:37-40, 47-49, 56-58` — the `fail(...)` defaults are commented out,
so an implementer of one purpose must stub five others.

### C.4 `SortedMap` vs `AssocMap` — one API, two invariants

| Member | `SortedMap` | `AssocMap` |
| --- | --- | --- |
| `empty` / `singleton` / `unsafeFromList` / `fromList` | `SortedMap.scala:32/47/65/84` | `AssocMap.scala:19/20/21/23` |
| `toPairList` / `isEmpty` / `nonEmpty` / `length` / `size` | `:360/372/384/396/408` | `:66/67/68/69/70` |
| `keys` / `values` / `mapValues` / `forall` / `exists` | `:421/436/486/454/470` | `:71/74/77/79/80` |
| `filterKeys` / `filter` / `filterNot` / `find` / `foldLeft` / `foldRight` | `:501/517/532/548/590/606` | `:82/87/90/93/96/99` |
| `get` / `contains` / `insert` / `delete` | `:625/696/711/738` (needs `Ord`) | `:111/122/124/137` (needs `Eq`) |
| `union` | `:177` | `:148` |

The `keys`/`values` bodies are identical modulo the wrapper type:
```scala
// CORE plutus/prelude/SortedMap.scala:421
def keys: List[A] = self.toPairList.foldRight(List.empty[A]) { case ((k, _), acc) =>
```
```scala
// CORE plutus/prelude/AssocMap.scala:71
def keys: List[A] = self.toPairList.foldRight(List.empty[A]) { case ((k, _), acc) =>
```
`PairList` (`PairList.scala`) is a third traversal of the same shape (`forall`:128, `exists`:132,
`find`:136, `foldLeft`:120, `foldRight`:124, `filter`:111, `mapValues`:103).

### C.5 `computeScriptHash` V1/V2/V3

```scala
// PAT patterns/ParameterValidation.scala:52-55
def computeScriptHashV3(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
    val parameterized = params.foldLeft(baseProgram)(_ $ _)
    Script.PlutusV3(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
}
```
```scala
// PAT patterns/ParameterValidation.scala:66-69
def computeScriptHashV2(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
    val parameterized = params.foldLeft(baseProgram)(_ $ _)
    Script.PlutusV2(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
}
```
```scala
// PAT patterns/ParameterValidation.scala:80-83
def computeScriptHashV1(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
    val parameterized = params.foldLeft(baseProgram)(_ $ _)
    Script.PlutusV1(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
}
```

### C.6 Reward-script redeemer/withdrawal lookup — twice in `scalus-design-patterns`

Quoted in full in §B.7: `PAT patterns/StakeValidator.scala:37-41` vs
`PAT patterns/MerkelizedValidator.scala:73-78`. Same three operations, two message vocabularies.
`MerkelizedValidator.getStakeRedeemer` (`:56-59`) is a *third* copy of two of the three steps.

### C.7 "Is the transaction signed by X" — six spellings

| Spelling | Sites |
| --- | --- |
| `tx.isSignedBy(pkh)` (core, `CORE plutus/v3/Contexts.scala:1082`) | `EX escrow/EscrowValidator.scala:77`, `:134`, `:172`; `EX crowdfunding/Crowdfunding.scala:449`, `:688`; `EX factory/Factory.scala:118`, `:171`; `EX htlc/HtlcValidator.scala:60`, `:65`; `EX auction/Auction.scala:182`, `:345`, `:399`; `EX betting/BettingValidator.scala:128`, `:186`, `:208`; `EX pricebet/OracleValidator.scala:50`, `:94`; `EX pricebet/PricebetValidator.scala:89`, `:104`; `EX simpletransfer/SimpleTransferValidator.scala:65`, `:80`; `EX upgradeableproxy/UpgradeableProxyValidator.scala:87`; `EX vault/VaultValidator.scala:113`, `:168`; `EX vesting/VestingValidator.scala:75` |
| `tx.signatories.contains(x)` | `EX HelloCardano.scala:22`; `PAT examples/ParameterValidationExample.scala:92` |
| `tx.signatories.exists(_ === x)` | `EX decentralizedidentity/DecentralizedIdentityValidator.scala:120`, `:188`, `:265`, `:271`, `:300`, `:318`; `EX lottery/LotteryValidator.scala:110`, `:152`; `EXL LotteryValidator.scala:88`, `:128`; `EX pricebet/PricebetValidator.scala:147`; `PAT examples/MultiPoolDexExample.scala:215` |
| `signatories.find(_.hash == pkh).orFail(msg)` | `EX PreimageValidator.scala:25`, `:43` |
| hand-rolled recursion on `BuiltinList[Data]` | `EX PubKeyValidator.scala:17`, `:31`, `:46`; `EX PreimageValidator.scala:64` |
| hand-rolled recursion on `List[PubKeyHash]` with `.toData ==` | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:170` |
| "the first signatory *is* the actor" (no membership test at all) | `EX MembershipToken.scala:73-74`; `EX bilinearAccumulator/AllowlistValidator.scala:57` |

The seventh idiom is worth calling out separately because it is not the same predicate — it treats
`signatories.head` as an identity, and the two sites disagree on whether to guard the empty list:

```scala
// EX MembershipToken.scala:73-74
require(txInfo.signatories.length > 0, "No signatories")
val signer = txInfo.signatories.head
```
```scala
// EX bilinearAccumulator/AllowlistValidator.scala:57
val memberPkh = tx.signatories.head
```
(`List.head` fails with `"head of empty list"`, `CORE plutus/prelude/List.scala:1121`, so the
unguarded form fails with an unhelpful message rather than a domain one.)

```scala
// CORE plutus/v3/Contexts.scala:1082
def isSignedBy(pubKeyHash: PubKeyHash): Boolean =
    self.signatories.contains(pubKeyHash)
```
```scala
// EX decentralizedidentity/DecentralizedIdentityValidator.scala:119-122
require(
  tx.signatories.exists(_ === identityDatum.ownerPkh),
  "Must be signed by identity owner"
)
```
```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:170-180
def requireSignedBy(
    signatories: List[PubKeyHash],
    party: PubKeyHash,
    message: String
): Unit = {
    def go(signatories: List[PubKeyHash]): Unit = signatories match {
        case List.Nil              => fail(message)
        case List.Cons(head, tail) => if head.toData == party.toData then () else go(tail)
    }
    go(signatories)
}
```
Note `contains` (`CORE plutus/prelude/List.scala:518`) is itself `find(_ === elem).isDefined`, so
spellings 1–3 are literally the same code path; only 4–6 differ operationally.

### C.8 Find own input — four spellings

| Spelling | Sites |
| --- | --- |
| `tx.findOwnInputOrFail(ownRef)` (core) | `EX MembershipToken.scala:126`; `EX amm/AmmValidator.scala:213`; `EX betting/BettingValidator.scala:77`; `EX decentralizedidentity/…:248`; `EX editablenft/…:135`; `EX escrow/…:54`; `EX factory/FactoryExample.scala:60`; `EX linkedlist/…:177`; `EX lottery/…:76`; `EXL LotteryValidator.scala:59`; `EX pricebet/OracleValidator.scala:86`, `PricebetValidator.scala:57`; `EX simpletransfer/…:50`; `EX upgradeableproxy/…:51`; `EX vault/VaultValidator.scala:82`, `:117`, `:149`, `:198`; `EX vesting/…:53`; `EX setbench/SetBenchAccValidator.scala:45`, `SetBenchImtValidator.scala:25`, `SetBenchMpf16bValidator.scala:27`, `SetBenchMpf16oValidator.scala:26`; `PAT examples/BatchAuctionExample.scala:83`, `StakeValidatorExample.scala:25`, `StakeValidatorPaymentSplitterExample.scala:90` |
| `inputs.at(idx)` + `require(outRef === ownRef)` | `PAT patterns/UtxoIndexer.scala:30`, `:43`, `:63`; `PAT patterns/LinkedList.scala:457`; `EX auction/Auction.scala:119`, `:144`; `EX auction/UnfixedAuction.scala:39`, `:63`; `EX crowdfunding/Crowdfunding.scala:255`, `:283`, `:322`; `EX decentralizedidentity/…:89`; `EX editablenft/…:67`; `EX linkedlist/…:184`; `EX paymentsplitter/OptimizedPaymentSplitterValidator.scala:91` |
| `inputs.get(idx).getOrFail(msg)` + equality | `PAT examples/TransactionLevelMinterValidatorExample.scala:38-43` |
| `inputs.exists(_.outRef === ref)` (one-shot check) | `EX factory/Factory.scala:96`; `EX amm/AmmValidator.scala:160`; `EX auction/Auction.scala:393`; `EX pricebet/OracleValidator.scala:54` |
| hand-rolled recursion | `EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:158` |

```scala
// CORE plutus/v3/Contexts.scala:960
inline def findOwnInputOrFail(
    outRef: TxOutRef,
    inline message: String = "Tx input not found"
): TxInInfo = {
    self.findOwnInput(outRef).getOrFail(message)
}
```
```scala
// PAT patterns/UtxoIndexer.scala:43-44
val input = tx.inputs.at(inputIdx)
require(input.outRef === ownRef, InputIndexMismatch)
```
```scala
// PAT examples/TransactionLevelMinterValidatorExample.scala:38-43
val input = tx.inputs.get(sampleSpendRedeemer.ownIndex).getOrFail("Undefined ownIndex")
val ownCredential = input.resolved.address.credential
val outRef = input.outRef

// Validating that the found UTxO is in fact the spending UTxO.
require(ownRef === outRef)
```
The index-based form is deliberate (O(1) vs `findOwnInput`'s O(n)) and appears 15 times, always with
its own bespoke error message — the strongest single stdlib candidate.

### C.9 Outputs by address / credential — four spellings

```scala
// CORE plutus/v3/Contexts.scala:1041   (canonical)
def findOwnOutputsByCredential(cred: Credential): List[v2.TxOut] =
    self.outputs.filter(_.address.credential === cred)
```
```scala
// EX amm/AmmValidator.scala:122-128
inline def findPoolOutput(outputs: List[TxOut], addr: Address): TxOut = {
    val matching = outputs.filter(_.address === addr)
    matching match
        case List.Cons(out, List.Nil) => out
        case List.Nil                 => fail("No pool output found")
        case _                        => fail("Multiple pool outputs found")
}
```
```scala
// EX upgradeableproxy/UpgradeableProxyValidator.scala:61-65
val continuationOutput =
    tx.outputs
        .filter(out => out.address === ownInput.resolved.address)
        .headOption
        .getOrFail(MissingContinuation)
```
```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:167-168
def findOutputsByCredential(outputs: List[TxOut], cred: Credential): List[v2.TxOut] =
    outputs.filter(_.address.credential.toData == cred.toData)
```
Also `EX pricebet/PricebetValidator.scala:64-66`, `EX lottery/LotteryValidator.scala:91`,
`EXL LotteryValidator.scala:71`, `EX crowdfunding/Crowdfunding.scala:720`,
`EX auction/Auction.scala:422`, `EX auction/UnfixedAuction.scala:278`,
`EX betting/BettingValidator.scala:281`, `PAT patterns/ParameterValidation.scala:135`.

Note the semantic split: `findOwnOutputsByCredential` matches on the **payment credential only**
(staking part ignored), whereas `out.address === addr` matches the **full address** including the
staking part. Both appear as "find my continuation output"; they are not interchangeable.

### C.10 "Exactly one X" — four idioms

| Idiom | Example |
| --- | --- |
| `.length === BigInt(1)` then `.head` | `EX escrow/EscrowValidator.scala:85-86`, `EX setbench/SetBenchAccValidator.scala:47-48`, `SetBenchImtValidator.scala:62-63`, `SetBenchMpf16bValidator.scala:45-46`, `SetBenchMpf16oValidator.scala:44-45`, `EX vesting/VestingValidator.scala:101`, `EX pricebet/PricebetValidator.scala:67`, `EX lottery/LotteryValidator.scala:93-98`, `EXL LotteryValidator.scala:73-78`, `EX MembershipToken.scala:89` |
| `.size == BigInt(1)` (`==`, not `===`) | `EX vault/VaultValidator.scala:200`, `EX simpletransfer/SimpleTransferValidator.scala:57` |
| `match { case List.Cons(x, List.Nil) => x; case _ => fail(...) }` | `EX amm/AmmValidator.scala:124`, `:139`; `EX auction/Auction.scala:424`; `EX auction/UnfixedAuction.scala:280`; `EX betting/BettingValidator.scala:99`, `:246`, `:268`, `:284`; `EX crowdfunding/Crowdfunding.scala:722`; `EX factory/Factory.scala:143`, `:176`; `EX cape/twopartyescrow/…:93`; `PAT patterns/LinkedList.scala:523` |
| counting `foldLeft` | `EX auction/Auction.scala:276-284`; `PAT examples/TransactionLevelMinterValidatorExample.scala:69-74` |

```scala
// EX setbench/SetBenchImtValidator.scala:61-64
val outputs = txInfo.findOwnOutputsByCredential(contractAddr.credential)
require(outputs.length === BigInt(1), "Expected one continuing output")
val out = outputs.head
val outDatum = out.datum.inlineOrFail[ImtDatum]("Expected inline datum")
```
```scala
// EX setbench/SetBenchMpf16bValidator.scala:44-47
val outputs = txInfo.findOwnOutputsByCredential(contractAddr.credential)
require(outputs.length === BigInt(1), "Expected one continuing output")
val out = outputs.head
val outDatum = out.datum.inlineOrFail[SetBenchDatum]("Expected inline datum")
```
Four `setbench` validators (`SetBenchAccValidator.scala:45-49`, `SetBenchImtValidator.scala:61-64`,
`SetBenchMpf16bValidator.scala:43-47`, `SetBenchMpf16oValidator.scala:42-46`) repeat this exact
five-line "single continuing output + its datum" preamble.

### C.11 Inline-datum extraction — `inlineOrFail` vs manual match

The canonical helper (`CORE plutus/v2/Contexts.scala:94`) is used ~20 times, yet these hand-roll it:

```scala
// EXL LotteryValidator.scala:79-82
val newState = continuationOutput.datum match {
    case v2.OutputDatum.OutputDatum(datum) => datum.to[State]
    case _ => fail("continuation out must have an inline datum")
}
```
```scala
// EX lottery/LotteryValidator.scala:100-102   (the same contract, other copy)
val newState = continuationOutput.datum.inlineOrFail[State](
  "continuation out must have an inline datum"
)
```
Same error text, two implementations, in near-identical files (§C.17). Also:
`PAT patterns/LinkedList.scala:516-518`; `PAT examples/BatchAuctionExample.scala:143-146`;
`EX crowdfunding/Crowdfunding.scala:258-266`, `:288-299`, `:327-338`;
`EX auction/Auction.scala:122-130`, `:147-155`; `EX auction/UnfixedAuction.scala:42-50`, `:66-74`;
`EX betting/BettingValidator.scala:79-89`.

### C.12 Own script hash from a credential — two spellings

```scala
// EX decentralizedidentity/DecentralizedIdentityValidator.scala:249-252
val scriptAddress = ownInput.resolved.address
val policyId = scriptAddress.credential match
    case Credential.ScriptCredential(hash) => hash
    case _                                 => fail("Expected script credential")
```
```scala
// EX editablenft/EditableNftValidator.scala:136-139   (same code, same message)
val scriptAddress = ownInput.resolved.address
val policyId = scriptAddress.credential match
    case Credential.ScriptCredential(hash) => hash
    case _                                 => fail("Expected script credential")
```
vs the `Option`-based form built on `CORE plutus/v1/Contexts.scala:507`:
```scala
// EX paymentsplitter/OptimizedPaymentSplitterValidator.scala:97-99
val ownScriptHash =
    ownInput.resolved.address.credential.scriptOption
        .getOrFail("Own address must be Script")
```
Manual-match sites: `EX MembershipToken.scala:127`, `EX amm/AmmValidator.scala:135`, `:182`, `:215`,
`EX pricebet/OracleValidator.scala:124`, `EX decentralizedidentity/…:98`, `:130`, `:169`, `:203`,
`:277`, `:249`, `EX editablenft/…:136`, `EX lottery/LotteryValidator.scala:209`, `:264`,
`EXL LotteryValidator.scala:173`, `:221`, `EX paymentsplitter/PaymentSplitterValidator.scala:85`.
`scriptOption` sites: `EX factory/FactoryExample.scala:61`,
`EX paymentsplitter/OptimizedPaymentSplitterValidator.scala:97`, `:139`,
`PAT patterns/ParameterValidation.scala:108`, `PAT patterns/StakeValidator.scala:65`,
`PAT examples/BatchAuctionExample.scala:85`, `:136`, `StakeValidatorExample.scala:26`,
`StakeValidatorPaymentSplitterExample.scala:91`, `:113`,
`TransactionLevelMinterValidatorExample.scala:46`.

### C.13 Exact-mint check — five idioms

```scala
// CORE plutus/v1/Value.scala:942 — the canonical primitive
def hasOnly(cs: PolicyId, tn: TokenName, amount: BigInt): Boolean = { ... }
```

| Idiom | Sites |
| --- | --- |
| `mint.hasOnly(policy, name, n)` | `EX crowdfunding/Crowdfunding.scala:203`, `:409`, `:665`, `:715`, `:728`; `EX decentralizedidentity/…:108`, `:154`, `:222`; `EX factory/Factory.scala:102`; `EX MembershipToken.scala:81`; `EX auction/Auction.scala:405`; `EX auction/UnfixedAuction.scala:264`; `EX amm/AmmValidator.scala:165`, `:195`, `:202`; `EX pricebet/OracleValidator.scala:60`, `:67` |
| `mint.quantityOf(p, n) === k` | `EX MembershipToken.scala:133`; `EX factory/Factory.scala:181`; `EX betting/BettingValidator.scala:193`; `EX pricebet/OracleValidator.scala:128`; `PAT patterns/LinkedList.scala:90`, `:111`, `:162`, `:234`, `:291`, `:347`, `:405` |
| `mint.flatten.filter(policy).length === 1` | `EX MembershipToken.scala:106-112`; `PAT examples/ParameterValidationExample.scala:125-132` |
| `mint.tokens(p).forall(_._2 < 0)` (all-burn) | `EX crowdfunding/Crowdfunding.scala:232-236`, `:758-762`; `EX decentralizedidentity/…:232-235`; `EX auction/Auction.scala:453-457`; `EX auction/UnfixedAuction.scala:306-309` |
| build expected `Value` then compare token maps | `EX editablenft/EditableNftValidator.scala:98-103`, `:110-119`; `EXS MintingPolicy.scala:102` (`Value.equalsAssets`) |

```scala
// PAT examples/ParameterValidationExample.scala:125-132
// Verify no other tokens are minted under this policy (V011 protection)
val allMintedUnderPolicy = sc.txInfo.mint.flatten.filter { case (pid, _, _) =>
    pid === policyId
}
require(
  allMintedUnderPolicy.length === BigInt(1),
  "Only one token type may be minted"
)
```
```scala
// EX MembershipToken.scala:106-112
val allMinted = txInfo.mint.flatten.filter { case (pid, _, _) =>
    pid === policyId
}
require(allMinted.length === BigInt(1), "Expected exactly one burn entry")
allMinted.foreach { case (_, _, qty) =>
    require(qty === BigInt(-1), "Must burn exactly 1 token")
}
```
Both are `mint.hasOnly(policyId, name, ±1)` rewritten as an O(n) flatten+filter.

### C.14 Lovelace / value summation

Two lovelace readers in core, with different failure modes:
```scala
// CORE plutus/v1/Value.scala:701
def getLovelace: BigInt = quantityOf(adaPolicyId, adaTokenName)
```
```scala
// CORE plutus/v1/Value.scala:719
def lovelaceAmount: BigInt = v.toSortedMap.toPairList.head._2.toPairList.head._2
```
`lovelaceAmount` assumes ADA is the first entry; on a value without lovelace it silently returns
another token's amount, or fails on an empty value. `EX cape/twopartyescrow/…:118` uses
`lovelaceAmount`, everything else uses `getLovelace`.

Two core aggregate helpers exist (`CORE plutus/v3/Contexts.scala:1161`, `:1179`) and are used only by
`EX vesting/VestingValidator.scala:88-89` and `EX escrow/EscrowValidator.scala`. Everyone else
hand-writes the fold. "Sum lovelace of outputs matching a credential" appears **8 times** with 8
different accumulator shapes:

```scala
// EX betting/BettingValidator.scala:251-255
private inline def totalPaidTo(txInfo: TxInfo, pkh: PubKeyHash): BigInt =
    txInfo.outputs.foldLeft(BigInt(0)) { (acc, out) =>
        if out.address === Address.fromPubKeyHash(pkh) then acc + out.value.getLovelace
        else acc
    }
```
```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:114-119
val cred = Credential.PubKeyCredential(sellerKeyHash).toData
val sellerAda = outputs.foldLeft(BigInt(0)): (sum, out) =>
    if out.address.credential.toData == cred
    then sum + out.value.lovelaceAmount
    else sum
```
```scala
// PAT examples/MultiPoolDexExample.scala:192-197
val ownerCredential = Credential.PubKeyCredential(order.owner)
val ownerOutput = tx.outputs.foldLeft(BigInt(0)) { (sum, output) =>
    if output.address.credential === ownerCredential then
        sum + output.value.getLovelace
    else sum
}
```
Also `EX vault/VaultValidator.scala:158-159`; `PAT examples/ParameterValidationExample.scala:72-80`;
`EX crowdfunding/Crowdfunding.scala:457-460`; `EX auction/Auction.scala:277`, `:306`, `:355` (token
sums rather than lovelace); `EX cape/twopartyescrow/…:146-150`.

The three `Auction` token-sum folds are literally the same expression three times:
```scala
// EX auction/Auction.scala:277-278 / :306-307 / :355-356
… .value.tokens(scriptHash).values.foldLeft(BigInt(0))(_ + _)
```

### C.15 Deadline / validity-interval checks — two idioms

```scala
// EX crowdfunding/Crowdfunding.scala:195-198   (canonical)
require(
  txInfo.validRange.isEntirelyBefore(campaignDatum.deadline),
  "Donations must be before deadline"
)
```
```scala
// EX htlc/HtlcValidator.scala:56-64   (manual bound extraction for the same concept)
case Action.Timeout =>
    val validFrom = tx.validRange.from.finite(0)
    // validFrom is inclusive, hence 10 <= 10 is correct
    require(config.timeout <= validFrom, InvalidCommitterTimePoint)
case Action.Reveal(preimage) =>
    val validTo = tx.validRange.to.finiteOrFail(ValidRangeMustBeBound)
    // validTo is exclusive, hence 10 <= 10 is correct
    require(validTo <= config.timeout, InvalidReceiverTimePoint)
```
```scala
// EX decentralizedidentity/DecentralizedIdentityValidator.scala:193-199
val txStartTime = tx.getValidityStartTime
require(txStartTime >= delegDatum.validFrom, "Delegation not yet valid")

val txEndTime = tx.validRange.to.boundType match
    case IntervalBoundType.Finite(t) => t
    case _ => fail("Transaction must have a finite upper validity bound")
require(txEndTime <= delegDatum.validUntil, "Delegation expired")
```
The last block re-implements `IntervalBound.finiteOrFail` (`CORE plutus/v1/Contexts.scala:133`)
inline. And note `getValidityStartTime` (`CORE plutus/v3/Contexts.scala:1102`) returns `0` on an
infinite bound — using it as `>= validFrom` is unsound, which is exactly the trap
`EX vault/VaultValidator.scala:132-137` documents in a comment:
```scala
// Derive the request time from the validity interval's *upper* bound, not the lower bound.
// The lower bound (getValidityStartTime) can be backdated arbitrarily, which would let an
// attacker set finalizationDeadline in the past and finalize immediately, defeating the
// wait. The ledger guarantees the upper bound is >= now, so deadline >= now + waitTime.
val requestTime = tx.validRange.to.finiteOrFail(NoFinalizationUpperBound)
```
The same reasoning is restated as a comment in `EX pricebet/OracleValidator.scala:112-120`. A
security property currently carried by prose in two files, not by API.

### C.16 `Value.tokens` restated

```scala
// CORE plutus/v1/Value.scala:901-902
def tokens(cs: PolicyId): SortedMap[TokenName, BigInt] =
    v.toSortedMap.get(cs).getOrElse(SortedMap.empty)
```
```scala
// PAT patterns/TransactionLevelMinterValidator.scala:34
val tokens = txInfo.mint.toSortedMap.get(minterScriptHash).getOrElse(SortedMap.empty)
```
Sites that reach into `.toSortedMap.get(policy)` instead of calling `tokens`:
`PAT patterns/LinkedList.scala:423`, `:452`, `:520`; `PAT patterns/TransactionLevelMinterValidator.scala:34`, `:44`;
`EX factory/Factory.scala:142`, `:175`; `EX linkedlist/LinkedListValidator.scala:178`;
`EXS MintingPolicy.scala:89`.

### C.17 Whole-file forks (same contract, two implementations)

| Pair | Divergence |
| --- | --- |
| `EX lottery/LotteryValidator.scala` vs `EXL LotteryValidator.scala` | only the datum idiom (§C.11); everything else identical, incl. the `paysAtLeast` helper (`lottery/…:296-303` vs `EXL:248-254`) |
| `EX auction/Auction.scala` vs `EX auction/UnfixedAuction.scala` | intentional (the latter is a deliberately-vulnerable clone with the NFT-burn count check removed) — but the 12-line own-input+datum destructure is copy-pasted **four** times across the two files |
| `EX paymentsplitter/PaymentSplitterValidator.scala` vs `OptimizedPaymentSplitterValidator.scala` | intentional (spend-time vs reward-time), but the ADA-only guard, input-classifying fold and remainder math are near-verbatim |
| `EX setbench/SetBench{Acc,Imt,Mpf16b,Mpf16o}Validator.scala` | intentional benchmark family; the 5-line continuing-output preamble is identical in all four |

```scala
// EX auction/Auction.scala:119-130
val input = txInfo.inputs.at(inputIdx)
require(input.outRef === txOutRef, "Input index does not match txOutRef")

val (scriptHash, inputValue, currentDatum) = input.resolved match
    case TxOut(
          Address(Credential.ScriptCredential(sh), _),
          value,
          OutputDatum.OutputDatum(inlineDatum),
          _
        ) =>
        (sh, value, inlineDatum.to[Datum])
    case _ => fail("Auction input must have script credential and inline datum")
```
```scala
// EX auction/UnfixedAuction.scala:39-50   (byte-identical)
val input = txInfo.inputs.at(inputIdx)
require(input.outRef === txOutRef, "Input index does not match txOutRef")

val (scriptHash, inputValue, currentDatum) = input.resolved match
    case TxOut(
          Address(Credential.ScriptCredential(sh), _),
          value,
          OutputDatum.OutputDatum(inlineDatum),
          _
        ) =>
        (sh, value, inlineDatum.to[Datum])
    case _ => fail("Auction input must have script credential and inline datum")
```

```scala
// EX paymentsplitter/PaymentSplitterValidator.scala:67-70
require(
  input.resolved.value.withoutLovelace.isZero,
  "Contract input must contain only ADA"
)
```
```scala
// EX paymentsplitter/OptimizedPaymentSplitterValidator.scala:155-158   (identical)
require(
  input.resolved.value.withoutLovelace.isZero,
  "Contract input must contain only ADA"
)
```

### C.18 Structural equality — `===` vs `.toData ==`

```scala
// EX cape/twopartyescrow/TwoPartyEscrowValidator.scala:96
require(output.toData == expectedOutput.toData, "Output must match expected deposit output")
```
```scala
// EX upgradeableproxy/UpgradeableProxyValidator.scala:70-73   (same concept, via Eq)
require(
  continuationOutput.value === ownInput.resolved.value,
  ValueMustBePreserved
)
```
`.toData ==` also at `EX cape/twopartyescrow/…:162`, `:168`, `:177`, and
`EX auction/Auction.scala:337`. On-chain the two are equivalent (`Eq` lowers to structural
comparison, `CORE plutus/prelude/Eq.scala:11-16`), but off-chain and in tests they diverge, and
`.toData` forces an encoding the `Eq` path may avoid.

### C.19 Failure vocabulary — three idioms

```scala
// CORE plutus/prelude/Prelude.scala:109
inline def require(inline requirement: Boolean, inline message: String): Unit =
    if requirement then () else throw new RequirementError(message)
```
```scala
// EXS MintingPolicy.scala:98-99   (locally re-implemented)
inline def check(b: Boolean, inline msg: String): Unit =
    if b then () else throw new Exception(msg)
```
```scala
// EX PubKeyValidator.scala:18
if signatories.isEmpty then throw new RuntimeException("Signature not found")
```
Raw-`throw` sites: `EX PubKeyValidator.scala:18`, `:33`, `:48`; `EX PreimageValidator.scala:70`;
`EXS MintingPolicy.scala:50-53`, `:91-92`, `:99`, `:146-149`;
`EX amm/AmmValidator.scala` uses `fail` throughout. `require`/`fail`/`getOrFail` dominate everywhere
else, so this is legacy rather than a live split.

### C.20 Core `Eq` instances that compare a value to itself

Not strictly duplication, but two hand-written `Eq` instances in core shadow their binders and so
compare each field to itself (harmless on-chain, where `Eq` is a marker lowered structurally —
wrong off-chain):

```scala
// CORE plutus/v1/Contexts.scala:312-315
case DCert.DelegRegKey(cred) =>
    y match
        case DCert.DelegRegKey(cred) => cred === cred
        case _                       => false
```
```scala
// CORE plutus/v1/Contexts.scala:745-748
case ScriptPurpose.Minting(curSymbol) =>
    y match
        case ScriptPurpose.Minting(curSymbol) => curSymbol === curSymbol
        case _                                => false
```
(Every case of `Eq[DCert]` at `v1:310-341` and `Eq[ScriptPurpose]` at `v1:743-760` has this shape.)

---

## D. Take-aways for the new stdlib design

1. **The existing high-level layer is query-shaped, not assert-shaped.** Every `find*` returns
   `Option`/`List`; only `findOwnInputOrFail` (v3:960), `inlineOrFail` (v2:82/94) and
   `finiteOrFail` (v1:133) fail. Every validator therefore re-writes the same
   `require(x.length === 1, "…")` + `.head` glue — 15+ times (§C.10).
2. **The most-duplicated single operation is the index-based own-input check** (§C.8, 15 sites),
   which exists precisely because `findOwnInput` is O(n).
3. **`Value.tokens`, `Value.hasOnly` and `OutputDatum.inlineOrFail` all exist but are bypassed**
   (§C.11, §C.13, §C.16). Discoverability, not capability, is the gap.
4. **Missing outright**: exactly-one-output combinator; NFT-quantity assertion; "sum value paid to
   credential"; value-conservation helper; "policy present in mint"; assert-shaped credential /
   signatory / deadline checks; a safe `validityUpperBound` that encodes the anti-backdating
   property documented only in comments (§C.15).
5. **Three near-identical `Validator` traits** (§C.3) with commented-out defaults force five stub
   methods per contract, and two files opt out of the traits entirely.
6. **Type aliasing hides errors**: `ScriptHash`, `ValidatorHash`, `PolicyId`, `DatumHash`,
   `TokenName` are all `ByteString` (v1:12–20), which is why
   `TransactionLevelMinterValidator.spend(minterScriptHash: ValidatorHash, …)` can pass a
   `ValidatorHash` where a `PolicyId` is meant (§B.5).
