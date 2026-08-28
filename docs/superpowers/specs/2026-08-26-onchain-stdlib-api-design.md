# On-chain standard library API – design specification

**Status:** design, decisions fixed by the project owner. Documentation-only deliverable; no Scala
implementation lands with this document.
**Date:** 2026-08-26.
**Target:** PlutusV3 / PV11 (`vanRossemPV`) only.
**Research corpus:** `docs/internal/stdlib-research/` – master synthesis `00-RESEARCH-REPORT.md`
(cited as `00 §N`) over eight studies `01`–`08` (cited as `01 §P05`, `06 VP-1`, `07 I2`, …).

---

## 1. Summary

This specification adds nineteen operations to the existing on-chain surface in
`scalus.cardano.onchain.plutus.*` and `scalus.cardano.onchain.plutus.prelude.*`. The operations are
the ones the research corpus shows contract authors re-implement by hand, in incompatible spellings,
with a measurable rate of security-relevant divergence between the spellings.

The change is deliberately small in surface and conservative in shape:

- **No new types, no new package, no new `@Compile trait`, no new files.** Every operation goes into
  a companion object that already exists.
- **Fail-fast is the primary form.** `xOrFail(inline message)` fails the script; a plain `x` returns
  `Option` only where a caller genuinely branches on absence (`00 §Part 2(b)`).
- **Predicates return `Boolean` and are used with the existing `inline require(cond, msg)`**
  (`prelude/Prelude.scala:108`). There are no `requireX` wrapper functions: `require` is already
  `inline`, so a wrapper emits identical UPLC and buys nothing (§6.2).
- **The safety lives in the semantics of the predicate, not in a wrapper.** `tx.mint.hasOnly` is
  safe because it compares a whole sub-map, not because of how it is called.

Companion changes ship with it: two `Eq` instance bug fixes (`v1/Contexts.scala:310`, `:743`), a
rename of the eight `findOwn*` finders whose "Own" is a misnomer, harmonization of the collection
constructors on `singleton`, three deprecations of helpers that invent a value or drop tokens
(`getValidityStartTime`, `getAdaFromOutputs`, `getAdaFromInputs`), and a documentation pass that
names `===`, `tx.mint.hasOnly`, `contains`, `count` and `SortedMap` as the canonical forms (§8).

One blocker is open and gates a further group of operations that are **not** in this cut: the
script-context map key order (§9). It is narrower than it first looks - `redeemers` is keyed
positionally by the ledger, so no content-based `Ord` can be correct for it, and the fix is a
representation change scheduled for the next major. Nothing else in this specification depends on
it.

### 1.1 Goals

| # | Goal | Evidence |
|---|---|---|
| G1 | Give the six highest-scoring pitfall classes a named, greppable operation each | `06 §2` ranks DS-1, VP-1, VP-2, VP-3, AU-1, MI-1 as "addressable by API shape, not by developer discipline" |
| G2 | Collapse the near-duplicate spellings the corpus already contains | `01 §4` lists 30 near-duplicate pairs; `01 §P05` alone has six spellings of "exactly one continuing output" |
| G3 | Make the safe form the shortest form | `06 §6` lesson 1: "a safe helper next to an unsafe sibling with a similar name is not a safe API" |
| G4 | Cost-neutral or cheaper than the hand-rolled idiom it replaces | `07 §7` rules B5–B12; every operation below carries a cost basis |
| G5 | Close the discoverability gap on operations that already exist | `08 §6` item 5: `SortedMap.getOrFail` exists and vela wrote it twice anyway |

### 1.2 Non-goals

| # | Non-goal | Why |
|---|---|---|
| N1 | A new `SpendingValidator` trait with a mandatory `OwnInputPolicy` | `00 §Part 2(d)`: adding an abstract member to `Validator` (`v3/Validator.scala:9-88`) is source-breaking for 22+ in-repo validators and every wild project; the members are `inline def`s dispatched in an inline match |
| N2 | Opaque invariant types (`Beacon`, `Positive`, `AscendingIndices`) with validating `FromData` | `00 §Part 2(h)` / `07 §1.6`: under the V3 backend `fromData` is rewritten to the identity, so decoder validation runs on the JVM and is silently skipped on-chain – worst-case EV-2 divergence |
| N3 | A fused `requireContinuing(ownInput, value, datum)` | Owner decision; counter-evidence recorded in §6.1 |
| N4 | Off-chain / testkit work (mock transaction builders, fuzz generators) | `00 §Part 1` rows 75/76 – a separate workstream, flagged as the highest-leverage companion |
| N5 | Any implementation | This deliverable is the specification |
| N6 | PlutusV1/V2 parity | PV3 only, per the fixed constraint |

---

## 2. Evidence basis

Every operation below is justified by *distinct codebases that hand-roll it*, counted per the
convention in `00 §Evidence-count convention`. Five independent corpora contribute:

- **35 in-repo validators** (`01 §1.1`) – the Scalus examples, read in full, catalogued as 31
  patterns `P01`–`P31` with per-pattern file counts (`01 §3`) and 30 named near-duplicate pairs
  (`01 §4`). This is the best available signal for what a competent developer gets wrong with
  today's API.
- **6 independent Scalus projects in the wild** (`08 §1.1`) – binocular, vela, hydrozoa, cosmex,
  adastream, proofspace; ~5 900 LOC of on-chain Scala read in full, plus 3 Scalus-team templates
  counted separately. Produces a 24-row ranked helper table (`08 §4`) and a 21-entry bug register
  (`08 §5`).
- **12 DEX protocols** (`04 §1`) and **15 non-DEX protocols plus 7 libraries** (`05 §0.1`) – Aiken,
  Plutarch and Haskell sources, yielding a 20-row ranked helper table (`04 §2`), a 37-row
  cross-repo idiom matrix (`05 §7`), and six structurally distinct anti-double-satisfaction
  mechanisms (`04 §3`, `05 §7.1`).
- **The Aiken ecosystem** (`03`) – `aiken-lang/stdlib` (502 public declarations across 34 modules,
  406 of them `pub fn`, `03 §1`), `vodka`, `aiken-design-patterns`, `aiken/fuzz`, plus measured
  downstream call counts across six consumer repos (`03 §6.2`).
- **A 47-entry pitfall taxonomy** (`06 §2`) scored `frequency × severity`, grounded in the in-house
  `EXAMPLES_REVIEW.md` findings, the MLabs vulnerability register, the Cardano Developer Portal
  security curriculum, and Tweag's 276-findings-across-20-audits retrospective (`06 §Evidence base`).

Cost claims are grounded in `07`, which cites `file:line` in this repository or a number asserted by
a shipped test. The baseline throughout is the default compiler configuration:
`SirToUplcV3Lowering`, `targetLanguage = PlutusV3`, `targetProtocolVersion = vanRossemPV`,
`generateErrorTraces = true`, **`optimizeUplc = false`**, `valueBuiltins = true`
(`07 §Baseline assumptions`; `compiler/compiler.scala:8-45`,
`compiler/sir/SIRDefaultOptions.scala:11-21`).

---

## 3. Design principles

### 3.1 Composable primitives plus thin, documented sugar

The operations split into two kinds. **Primitives** do work no caller can do more cheaply: a fused
single-pass scan, a builtin-backed `Value` query, a bound projection. **Sugar** is a one-line
expansion over primitives, shipped because the name is the review vocabulary – a validator written
against `tx.mint.hasOnly` and an explicit single-own-input guard reads as its own security argument, and the absence
of such a call is what a reviewer greps for (`00 §Part 2(d)`: "which DS strategy is this validator
using?" becomes a grep, not a type).

Every sugar operation's scaladoc states the exact expansion, so the escape hatch is documented in
place and dropping to the primitives is a visible altitude change in a review diff
(`00 §Part 3`, "Moving between layers").

### 3.2 Fail-fast primary, `Option` only where a caller branches

`Option` allocates a real `constrData(0, mkCons(x, mkNilData()))` (`prelude/Option.scala:15-18`;
`typegens/DataConstrEmitter.scala:34-46`) and **no pass folds `Case` over a literal `Constr`**
(`07 §3.7`: `CaseConstrApply.scala:28-32` only *creates* such terms; `Inliner.scala:203-212`;
`PartialEvaluator.scala:41` folds only closed terms). The tax is measured at **326 483 cpu (miss) /
564 996 cpu (hit) ≈ 85 / 158 lovelace per call** (`07 §4.1`, `LIST_CONTAINS_IMPL_COST.md:33-40`).

The tax is *fixed per call*, not per element – at list length 20 a single `find`'s Option overhead
fades to 2 % (`07 §4.1`). So `Option` is not banned; it is restricted to the two categories
`00 §Part 2(b)` identifies:

1. **At-most-one semantics with a real `None` continuation** – `List.findUnique` (op 21),
   `findUnique` (op 2). SimpleTransfer's full-withdrawal path is the canonical case: withdrawing
   the whole balance legitimately leaves no continuing output (`01 §P30`).
2. **Ledger-shaped optionality** – the V3 spend datum is `Option[Datum]` by CIP-69;
   `Credential.pubKeyOption` / `scriptOption` (`v1/Contexts.scala:503`, `:507`) mirror a genuine
   two-case domain.

Everywhere else the `OrFail` form is the only form. The corpus shows the cost of getting this
backwards: `Option`-primary APIs produced a bare-`.get` epidemic – hydrozoa ×4
(`DisputeResolutionScript.scala:170, 405, 553, 591`), binocular ×3
(`TreasuryMovementValidator.scala:369, 412, 563`) – `08 §5` items 9 and 13.

### 3.3 Predicates plus `require`, never `requireX` wrappers

`require` is already `inline`:

```scala
inline def require(inline requirement: Boolean, inline message: String): Unit =
    if requirement then () else throw new RequirementError(message)
```
(`prelude/Prelude.scala:108-109`)

A `requireMintsExactly(policy, expected, msg)` wrapper therefore lowers to exactly the term
`require(tx.mint.hasOnly(policy, name, qty), msg)` already produces. It doubles the API surface, adds
a second name for every concept, and buys zero UPLC. The vocabulary is therefore:
`x` (predicate, `Boolean`), `xOrFail(inline msg)` (projection that fails), and the caller's own
`require`.

This also keeps the failure message at the call site, where it can name the protocol's obligation
rather than the library's mechanism.

### 3.4 Safety in semantics, not in naming discipline

`tx.mint.hasOnly(policy, name, qty)` compares the **whole sub-map** under `policy`
(`06 MI-1`: the Minswap fix was `Map.lookup curr (getValue v) == Just (Map.fromList [(tok,1)])`).
No `quantityOf`-based mint assertion ships at any name (§6.7). `findContinuingOutputOrFail` compares
the **full `Address`**, staking part included (`06 AU-4`). `valuePaidTo` takes an `Address`, never a
`Credential` (`06 AU-4` fix #3). None of these depend on the caller reading the scaladoc.

Where the loose form has genuine demand – vela's whole house style is credential-only continuing
checks, ~15 sites (`08 §3.2`) – the loose form is **not given a name**. It stays spelled out at the
call site so it appears in the review diff (§6.8). This is `06 R2` ("unsafe is a named argument,
never an omission") applied in the only way extensions permit.

### 3.5 Design for `optimizeUplc = false`

`SIRDefaultOptions.optimizeUplc = false` (`SIRDefaultOptions.scala:16`); only `Options.release` /
`releaseUntagged` turn it on (`compiler.scala:63-76`). Nothing in this specification may depend on
CSE, inlining, partial evaluation or SAT for its cost story (`07 R24`). `IntrinsicResolver`
substitution is the one optimisation that runs unconditionally
(`lowering/IntrinsicResolver.scala:166-204`), and every `Value` operation below is built on it.

There is also **no fusion of any kind** (`07 §3.6`, verified by exhaustion over
`uplc/transform/OptimizerPipelines.scala:7-61` and the SIR pass list). So any operation a user would
naturally write as a two-stage pipeline must ship as a one-stage name – that is the whole reason
`findUniqueOrFail` exists rather than `filter(...).singleOrFail(...)`.

---

## 4. The operations

**Count.** This specification adds **19** named operations, renames **8** existing ones whose names
are wrong, harmonizes **2** constructor names, and deprecates **3** hazardous helpers. Recount from
the tables below rather than from prose: the count moved from 34 to 43 and down to 17 as each group
was held to one standard, then to 18 and 19 when the datum-equality measurement and the `quantityOf` count each reinstated one; every earlier figure is now wrong.

**The standard.** An operation earns a name on exactly one of three grounds. Anything else is a
one-liner over what exists, and one-liners are documented as idioms (§4.1), not shipped.

- **(a) Fusion** – the expansion walks a structure twice or allocates where the operation does
  neither, and the difference is statable in cpu or lovelace.
- **(b) Footgun** – the obvious expansion is wrong in a way the corpus has already paid for, and the
  operation's semantics remove the mistake.
- **(c) Ledger fact** – the operation encodes something the ledger guarantees that the generic form
  silently drops, so a caller of the generic form cannot know it.

"It reads better" and "it is greppable" are not grounds. Both were tried on minting (§4.5) and
withdrawn once the wrapped call turned out to exist already.

**The three conventions that survive from §3.** Fail-fast primary: `xOrFail(inline message)`.
Predicates return `Boolean` for the existing `inline require`. No `requireX` wrappers. No `Option`
twins: the one candidate, `findUnique`, was deleted because its "two or more" case had to fail, so
its `Option` type lied (§6.4).

### 4.1 Already in the library – the idioms, not new operations

These are the checks the corpus hand-rolls that Scalus already has. The specification's job for
them is documentation and, where noted, a scaladoc warning. Every row was verified against source.

| Check | Existing form | Location |
|---|---|---|
| Exact mint **or burn**, nothing else under the policy | `tx.mint.hasOnly(policy, name, qty)` – `qty` is signed | `Value.scala:942`; its scaladoc already says "the recommended way to verify an exact mint" |
| Multi-asset exact mint | `tx.mint.tokens(policy) === expected` | `:901`; `===` is one `equalsData` |
| Everything under a policy | `tx.mint.tokens(policy)`; `.isEmpty` / `.nonEmpty` for both coupling directions | `SortedMap.scala:372,384` |
| Beacon present | `value.hasNft(policy, name)` (§4.4) | over `quantityOf` `:795`, builtin-backed at PV11 |
| ADA only | `value.withoutLovelace.isZero` | `:984`, `:734` |
| Deadline passed / not yet | `tx.validRange.isEntirelyAfter(t)` / `isEntirelyBefore(t)` | `v1/Contexts.scala:245,256`; **23 in-repo sites**, the dominant idiom |
| Signed by; all of; count | `tx.isSignedBy(pkh)`; `keys.forall(tx.isSignedBy)`; `keys.count(tx.isSignedBy)` | `v3:1082`; `List.scala:986,1004` |
| Single own input (DS-1) | `tx.inputs.findUniqueOrFail(_.resolved.address.credential === ownCred, msg)` – one pass, no accumulator, fails with a message and returns the input. Measured against `inputs.count(...) === BigInt(1)`: fee 3 175 vs 3 307 on 3 inputs, 6 289 vs 6 804 on 10 | replaces the `count` idiom `cape/htlc/HtlcValidator.scala:65-68` shipped |
| Inline datum decode | `out.datum.inlineOrFail[T](msg)` | `v2/Contexts.scala:82,94` |
| Inline datum equality | `out.hasInlineDatum(x)` (§4.4.1) – the wrap form `out.datum === OutputDatum.OutputDatum(x.toData)` under a name | `VestingValidator.scala:112`; **measured cheaper** than `out.datum.inlineOrFail[T](msg) === x`: 286 vs 461 lovelace ex-unit fee, 706 vs 1 136 with the reference-script fee (see below) |
| Enterprise address for a script | `Address.fromScriptHash(h)` | `v1:577`; `stakingCredential = None`, never compare against a real output |
| Credential projection | `cred.scriptOption` / `pubKeyOption` | `v1:503,507` |
| Any/every element | `List.contains` (intrinsic), `forall`, `count`, `at` | `List.scala:518,986,1004,457` |
| Structural equality, anywhere | `a === b` | lowers to `equalsData`; never write `a.toData == b.toData` (§8.3) |

**Why the wrap-and-compare form beats decode-and-compare for datum equality.** The intuition runs
the other way – decoding should be cheaper than constructing – so it was measured on the real
compiler (`PlutusV3.compile`, `evaluateDebug`, three-field case-class datum, `Options.release`):

| Form | Case | mem | cpu | ex-unit fee | script | ref-script fee | total |
|---|---|---|---|---|---|---|---|
| **wrap** `out.datum === OutputDatum.OutputDatum(x.toData)` | same datum | **2 465** | **1 988 178** | **286** | 28 B | 420 | **706** |
| **decode** `out.datum.inlineOrFail[T](msg) === x` | same datum | 4 625 | 2 684 905 | 461 | 45 B | 675 | 1 136 |
| **wrap** | rebuilt via `copy` | **8 655** | **3 910 984** | **782** | 86 B | 1 290 | **2 072** |
| **decode** | rebuilt via `copy` | 10 315 | 4 527 711 | 922 | 100 B | 1 500 | 2 422 |

Fees in lovelace at mainnet prices (0.0577 / mem, 0.0000721 / cpu, 15 / reference-script byte, the
`EqualsDataVsTypedComparisonTest` convention). Memory moves more than cpu – +88% and +19% – because
the strip-and-rewrap round trip described below allocates a fresh constr and list where the wrap
form allocates only the outer `OutputDatum` cell. Per comparison the decode form costs 140–175
lovelace more in execution and 255–430 lovelace more if the script is a reference script; a
validator that compares one continuing datum per spend pays that on every transaction.

The UPLC shows why. The wrap form is three builtins around one `equalsData`:
`equalsData(outDatum, constrData(2, mkCons(x, [])))`. The decode form must first take the
`OutputDatum` apart – `unConstrData`, `fstPair`, a three-way `case` on the tag, `sndPair`,
`headList` – and then `=== x` on the decoded `T` does **not** compare the original `Data`: the
lowering represents the decoded value as its field list, so it emits
`equalsData(constrData(0, sndPair(unConstrData(inner))), x)`, a strip-and-rewrap round trip. Nine
builtins against three, on the same `equalsData`. The decode form wins only on the *failure* path
(a hash or missing datum fails at `inlineOrFail` for 838 583 cpu instead of comparing for
3 392 683), which no fee depends on, and on the message: it says "not inline" where the wrap form's
`require` says "datum mismatch". Use the decode form when the datum's *fields* are needed anyway;
for pure equality the wrap form is the idiom.

The rewrap round trip is a lowering improvement candidate (§8.4): `d.to[T] === x` for a Data-backed
`x` could compare `d` directly, which would make the two forms cost the same.

### 4.2 Prelude – `prelude/List.scala`, `SortedMap.scala`, `AssocMap.scala`, `PairList.scala`

Two vocabulary decisions, then three operations.

**`singleton` builds, `singleOrFail` extracts.** The constructors are split today – `List.single`
(`:48`) and `PairList.single` (`:35`) versus `SortedMap.singleton` (`:47`) and `AssocMap.singleton`
(`:20`). Harmonize on `singleton`: it is the Plutus, Haskell and Aiken term, the two map types
already use it, and the two `single`s are `inline` one-liners that deprecate cleanly.

**`unique` and `single` are different words for different things.** `unique` is a property of an
element relative to the others – "the one that matches". `single` is a property of the collection –
size one. So `findUniqueOrFail(p, msg)` finds the unique match among many, and `singleOrFail(msg)`
returns the only element of a size-one collection. Two concepts, two names, no collision.

```scala
extension [A](self: List[A]) {

    /** The unique element satisfying `p`, or fail. Finds the first match and KEEPS SCANNING to
      * prove there is no second, in one pass with no intermediate list.
      *
      * `find` stops at the first match and silently accepts a second. Binocular states the bug
      * in its own source: "`filter` then match, NOT `find`: `find` stops at the first commitment
      * and would silently accept a TM carrying a second one"
      * (`TreasuryMovementValidator.scala:304-306`).
      */
    inline def findUniqueOrFail(p: A => Boolean, inline message: String): A

    /** The only element of a size-one list, or fail. Unlike `head`, fails on two or more. */
    inline def singleOrFail(inline message: String): A
}

extension [A, B](self: SortedMap[A, B]) {
    /** The only entry of a size-one map, or fail. `toPairList` is a zero-cost relabel (`07`). */
    inline def singleOrFail(inline message: String): (A, B)
}
// AssocMap.singleOrFail: same body, for symmetry.
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 1 | `List.findUniqueOrFail(p, msg): A` | **(a)+(b)** | **~55 sites, 4 spellings, 4 wild projects** (`08 §4` r1); 15+ in-repo `filter().length === 1` / `.head` sites (`02 §C.10`); WingRiders `passertSingleton`; Liqwid's `phasOnlyOneToken` vs `phasOneToken` distinction (`05 §8.1`) | 1 traversal, 0 allocations, tail-recursive. `filter(p).length === 1` is 2 traversals plus k `mkCons` (`07 I1`; `List.scala:805-808` is a non-tail `foldRight`). The full walk is the security property being paid for |
| 2 | `List.singleOrFail(msg): A` | (b) | vela `oneOrFail` ~30 sites, some on lists that arrive without a filter step – `tx.signatories`, a redeemer's index list, a datum field (`08 §2.2`) | `head` (`List.scala:1121`) accepts the first of five; this fails on 2+. One `head`/`tail`/`nullList` |
| 3 | `SortedMap.singleOrFail(msg): (A, B)`, and `AssocMap.singleOrFail` with the same body | **(a)** | replaces three earlier operations: `mintedOnlyOrFail`, `uniqueTokenOfOrFail`, `singleAssetApartFromAdaOrFail` (§4.9). Both binocular mint handlers hand-roll it as a `match` on `tokens(p).toList` (`TreasuryMovementValidator.scala:634-649`, `BitcoinValidator.scala:1405-1434`); aiken-stdlib `has_any_nft_strict`, assist `prove_nft` | `toPairList` is free; then one `PairCons(kv, PairNil)` match. No `Ord` lookup, no second traversal for the value |

**Implementation notes (landed).** The plugin accepts any `String` expression as a `fail`
message (`compileThrowException` compiles the constructor argument as an ordinary expression);
an early probe that suggested otherwise had resolved `fail` to ScalaTest's
`Assertions.fail(msg)(implicit pos)`, which outranks the prelude import inside a suite. So all
three shapes were open. Inlining the whole scan at each call site was measured and rejected: it duplicates the recursive `go` per site (release build, ten-element
list: 79 B for one site, **+64 B per further site**). The shape that ships is a public non-inline
`findUniqueOrElse(predicate, orElse: Unit => A)` compiled once as a module function, with
`findUniqueOrFail` an `inline` wrapper passing `_ => fail(message)`; that is 88 B for one site and
+18 B per further site, with equal or lower fees. The `Option`-returning shape
(`findUniqueOption(p).getOrFail(msg)`) was worse on both axes (117 B, +42 B). A plain non-inline
`findUniqueOrFail(p, message: String)` is the same size and the same cost in a release build, and
about 10% dearer with error traces on (28 694 vs 25 694 mem at one site), because the string is
threaded through every recursive step; the continuation was kept. The "no second match" check
inside the scan is `tail.forall(!p)`, not `tail.count(p) === 0`: `count` is not an intrinsic (it is
the prelude's `foldLeft` with an integer accumulator), cannot short-circuit, and measured +39% fee
and +27 B on a nine-element tail; the intrinsic `find(p).isEmpty` is within 3% of `forall` either
way but costs 21 B for the `Option`. `singleOrFail` stays
`inline`: its body is a two-level match, and inline beats the continuation at one site (24 vs
35 B) and at two (41 vs 45 B). It matches the tail (`Nil` / `_`) instead of calling `isEmpty`: the
inlined `isEmpty` intrinsic hit a lowering representation mismatch on a constructed list. Every
operation has an exact pin – ExUnits and the mainnet fee in lovelace – in its test
(`assertEvalWithBudgetAndFee`, argument passed as Data, test-kit options): `findUniqueOrFail` on
ten elements 25 694 / 4 809 547, 1 830 lovelace; `List.singleOrFail` 2 164 / 366 777, 152;
`SortedMap.singleOrFail` (including the map decode) 8 820 / 1 960 097, 651; `hasNft` 230;
`hasSameTokensAndAtLeastAda` 2 000; `scriptHashOrFail` 211; `hasInlineDatum` 274; on a
three-input, three-output transaction `findInputOrFail` 1 172, `findContinuingOutputOrFail` 2 680,
`valuePaidTo` 1 945, `valueSpentFrom` 2 249, `isSignedByAny` 1 402, `validFromOrFail` +
`validToOrFail` 924, `onlyBurnsUnder` 1 796, `hasPaidTagged` 2 825; `deriveTokenName` 500;
`divFloor` 327, `divCeil` 397. The `TxInfo` figures include decoding the transaction from Data.

The three idioms `singleOrFail` unlocks, each formerly a named operation:

```scala
val (name, qty) = tx.mint.tokens(ownPolicy).singleOrFail(msg)          // the one thing minted or burned
val (name, qty) = out.value.tokens(policy).singleOrFail(msg)           // beacon extraction from an output
val (policy, toks) = v.withoutLovelace.toSortedMap.singleOrFail(msg)   // exactly one non-ADA asset,
val (name, qty) = toks.singleOrFail(msg)                               //   in two steps
```

### 4.3 Credential projections – `v1/Contexts.scala`

```scala
extension (self: Credential) {
    inline def scriptHashOrFail(inline message: String): ValidatorHash
    inline def pubKeyHashOrFail(inline message: String): PubKeyHash
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 4 | `Credential.scriptHashOrFail(msg)` | (a), **conditional** | 10 in-repo files destructure `ScriptCredential(hash)` with `: @unchecked` (`01 §P20`); 3 wild projects, 10 sites (`08 §4` r3) | Expansion `scriptOption.getOrFail(msg)` allocates an `Option` that nothing folds away – `constrData` then `Case`, 326 483 / 564 996 cpu (`07 §4.1`). Direct match avoids it |
| 5 | `Credential.pubKeyHashOrFail(msg)` | (a), conditional | `01 §P16`, 8 files | same |

**Conditional** means: if the lowering ever folds `Some(x).getOrFail` to `x`, both operations are
one-liners and should be deleted. The `Address`-level twins from an earlier draft were pure
delegation to these through `.credential` and are gone (§4.9).

### 4.4 Value – `v1/Value.scala`

```scala
extension (self: Value) {

    /** Every non-ADA asset exactly equal to `expected`, and lovelace at least `expected`'s.
      *
      * Neither obvious spelling is right. `===` rejects a valid transaction whenever the builder
      * must add lovelace to clear min-ADA, and under VP-6 that is a griefing vector: inflate the
      * output's size and no valid spending transaction exists. `containsAtLeast` (`>=` on the
      * whole value) is VP-1, the rank-2 pitfall, and enables the DS-1 shape where one output
      * satisfies two "at least" obligations. Tokens exact, ADA open: the treasury predicate
      * (`treasury-contracts/lib/utilities.ak:69`).
      *
      * The ADA end is open above. An earlier draft added a `maxSurplus` bound; it had no support
      * anywhere in the corpus and was dropped. A caller that needs the bound writes the second
      * comparison at the call site.
      */
    def hasSameTokensAndAtLeastAda(expected: Value): Boolean
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 6 | `hasSameTokensAndAtLeastAda(expected)` | **(b)** | **10 protocols** in the min-ADA family (`05 §7`): treasury `equal_plus_min_ada`, WingRiders `pvalueOfWithOilCheck`, Minswap `min_pool_ada`, Sundae, Splash; binocular splits it into tokens-exact + ADA-monotone twice (`08 §2.3`) | two `withoutLovelace` (each one `insertCoin` builtin at PV11) + one `equalsData` + two `lookupCoin` + one compare. With CIP-168, `dropPolicies([ada])` replaces `withoutLovelace` with no signature change (§4.10) |

```scala
extension (self: Value) {

    /** Exactly one unit of `(policy, name)` is present. Other assets are tolerated.
      *
      * The strict twin – nothing else under `policy` – is `hasOnly(policy, name, 1)`, which
      * already exists. Aiken's `assets` module ships the same pair as `has_nft` / `has_nft_strict`.
      */
    inline def hasNft(policy: PolicyId, name: TokenName): Boolean =
        quantityOf(policy, name) === BigInt(1)
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 7 | `hasNft(policy, name)` | **(b)** | **26 in-repo sites** compare `quantityOf` to one (`=== BigInt(1)` ×24, `> 0` ×2); only 7 sites compare to any other quantity. Aiken stdlib `assets.has_nft` / `has_nft_strict` / `has_any_nft` (`03 §7.2`); Plutarch `phasOneTokenOfAssetClass`; Liqwid distinguishes `phasOneToken` from `phasOnlyOneToken` (`05 §8.1`) | identical to `quantityOf === 1`: one `lookupCoin` at PV11 plus `equalsInteger` |

`hasNft` was `hasToken` in an earlier draft and was deleted as a one-liner. It comes back on two
grounds the deletion missed. First, the one-liner has a compile trap: `quantityOf(p, n) === 1` does
not type-check (`Eq[BigInt | Int]` has no instance), so every site writes `=== BigInt(1)`, and two
in-repo sites drifted to `> 0`, which is a different predicate for any non-NFT token. Second, the
name states the invariant: "NFT" means exactly one, and a reviewer reading `hasNft` knows the
check is `=== 1`, where `hasToken` (the old name) reads as "any amount". It lives on `Value`, not
`TxOut`: the same predicate is used on inputs (`input.resolved.value`), outputs, reference inputs
and `tx.mint` ("mints exactly one"), and a `TxOut` twin would be a second name for the first case
only. `hasAnyNft(policy)` is not added: no in-repo site wants it.

`isAdaOnly` from an earlier draft is a one-liner over `withoutLovelace.isZero` (§4.1) and is gone.

#### 4.4.1 `TxOut.hasInlineDatum` – `v2/Contexts.scala`

```scala
extension (self: TxOut) {

    /** The output carries `a` as an inline datum.
      *
      * Compares the whole `OutputDatum` with `===`: one `equalsData` around one `constrData`.
      * Measured at 286 lovelace against 461 for `datum.inlineOrFail[A](msg) === a`, which takes
      * the `OutputDatum` apart and then rewraps the decoded value before comparing (§4.1).
      *
      * Use `datum.inlineOrFail` when the datum's FIELDS are needed. Use this when only its
      * equality is.
      */
    inline def hasInlineDatum[A: ToData](a: A): Boolean =
        self.datum === OutputDatum.OutputDatum(a.toData)
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 8 | `TxOut.hasInlineDatum(a)` | **(b)** | **8 in-repo sites, 4 spellings**: decode-typed (`Auction:444`, `UnfixedAuction:298`), decode-to-`Data` (`EscrowValidator:103`, `LinearVestingValidator:115`), wrap (`VestingValidator:119`, `SimpleTransfer:78,97`), `toData ==` (`TwoPartyEscrow:154`). The library's own examples cannot agree, and the obvious spelling is the 60% dearer one | identical UPLC to the wrap form; `ToData[Data]` is identity, so the sites that already hold a `Data` migrate to the same call |

This is the one deliberate exception to the "no wrappers over `===`" line of §3.3. It is admitted
because the measurement in §4.1 shows the one-liner it wraps is not the one people write: three of
the four in-repo spellings pay the unwrap, and nothing at a call site says the cheap form exists.
A predicate named for the check puts the cheap form where `require` wants a `Boolean`. It lives on
`TxOut`, not `OutputDatum`, because `out.hasInlineDatum(x)` reads as a sentence and
`out.datum.isInlineDatum(x)` stutters; §6.5's rejection of `TxOut.inlineDatumOrFail` does not apply,
since that was a pure delegation and this is the operation itself.

### 4.5 Minting – `v3/Contexts.scala`

One operation. The rest of the minting story is `tx.mint.hasOnly` and `tx.mint.tokens`, which
already exist, plus `singleOrFail` from §4.2.

The word **only** carries the security property. MI-1 – the Minswap `isUnity` class, ~$195M TVL at
risk in March 2022 – is precisely the failure of checking that a token *is* minted without checking
that nothing else was. `hasOnly` puts that at the call site; `quantityOf`-based mint checks are the
pattern that caused the incident, and no such helper ships.

```scala
extension (self: TxInfo) {

    /** At least one entry under `policy`, and every quantity under it is negative.
      *
      * Not `tokens(policy).forall(_._2 < 0)`: `forall` on an empty map is vacuously TRUE, so the
      * unguarded expansion passes a transaction that burns nothing. The emptiness guard is the
      * operation.
      */
    def onlyBurnsUnder(policy: PolicyId): Boolean
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 9 | `onlyBurnsUnder(policy)` | **(b) vacuous truth** | 5 in-repo, **3 character-for-character identical** (`01 §P12`: Auction `:452-457`, Crowdfunding `:758-762`, DID `:231-235`); vodka `check_policy_only_burn` at **14 downstream calls** (`03 §6.2`) | `tokens(policy)` + `nonEmpty` (one `nullList`) + one short-circuiting tail-recursive pass |

**Why the emptiness guard is the operation.** An auction closes by burning its state NFT so the
auction cannot be replayed. The natural spelling:

```scala
case Action.Close =>
    require(tx.isSignedBy(datum.seller), "seller only")
    require(tx.mint.tokens(ownPolicy).forall((_, qty) => qty < 0), "must burn the auction NFT")
    // pay out the winner ...
```

The attacker submits a `Close` transaction that **mints nothing at all**. `tx.mint.tokens(ownPolicy)`
is the empty map. `forall` over an empty map is `true` – there is no element for which the
predicate fails. The check passes, the payout runs, and the auction NFT is still in circulation:
the attacker holds it in their own wallet, builds a fresh UTxO at the auction script with a new
datum around it, and runs the auction again with a token the off-chain indexer trusts. The same
shape appears in Crowdfunding and DecentralizedIdentity in this repository; all three wrote the
guard by hand, character for character (`01 §P12`).

```scala
case Action.Close =>
    require(tx.isSignedBy(datum.seller), "seller only")
    require(tx.onlyBurnsUnder(ownPolicy), "must burn the auction NFT")
```

`onlyBurnsUnder` is `tokens(policy).nonEmpty && tokens(policy).forall(qty < 0)`. The first
conjunct is what the hand-written version forgets. The exact-quantity form,
`tx.mint.hasOnly(ownPolicy, nftName, -1)`, is stronger still and preferred when the name is known;
`onlyBurnsUnder` is for "everything under this policy is being burned", where the names are not
enumerable at the call site.

#### 4.5.1 The minting idioms, with signed quantities

```scala
require(tx.mint.hasOnly(ownPolicy, beacon, 1),  "must mint exactly one beacon")
require(tx.mint.hasOnly(ownPolicy, beacon, -1), "must burn exactly one beacon")
require(tx.mint.tokens(ownPolicy) === expected, "counter must advance and reward must match")
require(tx.mint.tokens(ownPolicy).isEmpty,      "this branch mints nothing")
require(tx.mint.tokens(mintScript).nonEmpty,    "must be accompanied by the mint")   // forwarding
val (name, qty) = tx.mint.tokens(ownPolicy).singleOrFail(OnlySingleNft)
```

The third line is the shape Fortuna uses on every block (`tunav2.ak:120-137`: burn counter *N*,
mint counter *N+1*, mint the reward) and lenfi on every batched borrow (`collateral.ak:505-759`).
Fortuna hand-sorts its expected list with an explicit `less_than_bytearray` branch because the
ledger's map is key-ordered; building `expected` with `SortedMap.fromList` removes that burden.
`fromList` is an on-chain insertion sort (`SortedMap.scala:84-98`), trivial at two or three entries.

A `mintsNothingUnder` and a `mintsAnythingUnder` were each one call over `tokens(p)` and are gone;
`isEmpty`/`nonEmpty` make both directions one-liners with no negation.

#### 4.5.2 What a minting policy does that a spending validator never does

Surveying ~20 protocols' minting policies as a category (rather than by frequency count) surfaced
seven things a mint handler does that a spend handler cannot:

1. Constrains the **residual set** – "no *other* name under my own policy". No spend analogue.
2. Runs **once per policy per transaction**, so it is the natural home for transaction-global
   invariants (the premise of `tx_level_minter`) and cannot use per-input DS-1 guards.
3. Handles **mint and burn simultaneously** as signed quantities in one map.
4. **Produces** token names rather than only testing them – 9 protocols compute the name.
5. Reads **other purposes' redeemers**, having no datum of its own.
6. Must respect **ledger key order** when building a multi-entry expected map (§9).
7. Is the one purpose where the policy *is* the whole validator.

Items 1–3 are covered by `hasOnly`, `tokens` and signed quantities. Item 4 is deliberately not:
the nine protocols use mutually incompatible schemes (Minswap `sha3_256(sha3_256(pidA++nameA) ++
…)`, Sundae `blake2b_256(txid ++ #"23" ++ idx) |> drop(4)`, byte-encoded counters), and only the
one-shot case is shared – that is `deriveTokenName` (§4.7). CIP-68 label pairing is a token-standard
convention whose four implementations disagree on shape (recorded-mint enforces a pair, Sundae mints
three names from one ident, Nebula swaps labels on the spend side) and belongs in a conventions
module, not here.

**`tx.mint.tokens(ownPolicy).isEmpty` is unsatisfiable from inside the policy's own mint handler.**
`Validator.mint` runs only when `ScriptInfo.MintingScript(policyId)` fires (`v3/Validator.scala:17-18`),
which requires that policy to be present in `tx.mint`, and Conway forbids zero quantities. The
check is meaningful only about *other* policies, or from the `spend` handler of the same script.

#### 4.5.3 The spend-path "infinite mint", and what this group does not fix

A **forwarding** minting policy delegates to another validator – "I allow minting whenever the
treasury script runs". 9 protocols use it; butane's entire mint policy is one coupling check
(`pointers.ak:21-23`). The hole is that a validator is a set of redeemer branches and the policy
trusts all of them, so the mint is constrained only as tightly as the **weakest** branch:

```scala
def spend(datum, redeemer, tx, ownRef) = redeemer.to[Action] match
    case Action.Withdraw(amount) => require(tx.mint.tokens(ownPolicy) === expected, "...")  // constrains
    case Action.Inspect          => require(tx.isSignedBy(owner), "owner only")             // says NOTHING
```

Spend with `Inspect` and mint ten billion tokens in the same transaction; nobody constrained the
mint. Plutonomicon's example is a redeemer named `WitnessMyState` (`06 MI-4`): the dangerous branch
is the one that looks like it does nothing, written by an author with no reason to think about
minting, possibly months after the policy.

**Nothing in this specification fixes it.** A name is not a mechanism – nothing makes the `Inspect`
author write `tokens(p).isEmpty`. The fix is structural and belongs to the deferred
`SpendingValidator` layer (§10.3): a total `def mintPolicy(r: R): SortedMap[TokenName, BigInt]` the
framework checks before dispatch, so `Inspect` writing `SortedMap.empty` is a deliberate act rather
than an omission. These operations supply the vocabulary; closing the omission is a framework
concern.

### 4.6 TxInfo – locating and summing (`v3/Contexts.scala`)

Three new operations and a rename family.

**The `findOwn*` names are wrong, all eight of them.** "Own" is a Plutus inheritance: there,
`findOwnInput :: ScriptContext -> Maybe TxInInfo` reads the spending purpose's own reference, so
the word is accurate. Scalus's versions take an explicit argument and find *any* input, output or
datum. The one-shot seed idiom, `findOwnInputOrFail(params.oneShotTxOutRef, …)`, makes the name
actively misleading. `Utils.findInput(inputs, outRef)` (`v3:1141`) already uses the right word.

| Deprecate (`1.1.1`) | Replacement | Note |
|---|---|---|
| `findOwnInput(ref)` (`:938`) | `findInput(ref): Option[TxInInfo]` | |
| `findOwnInputOrFail(ref, msg)` (`:960`) | **`findInputOrFail(ref, msg)`** | 33 in-repo sites. New body is direct tail recursion, no `Option` – which also makes an earlier `spends(ref)` operation redundant: `tx.findInputOrFail(seed, msg)` as a statement is the one-shot check, with a message |
| `findOwnDatum(hash)` (`:983`) | `findDatum(hash)` | |
| `findOwnScriptOutputs(hash)` (`:1002`) | `findOutputsByScriptHash(hash)` | plus the AU-4 warning below |
| `findOwnInputsByCredential(cred)` (`:1022`) | `findInputsByCredential(cred)` | |
| `findOwnOutputsByCredential(cred)` (`:1041`) | `findOutputsByCredential(cred)` | plus the AU-4 warning |
| `findOwnInputs(pred)` (`:1058`) | `inputs.filter(pred)` | one-liner; no replacement |
| `findOwnOutputs(pred)` (`:1075`) | `outputs.filter(pred)` | same |

The old names delegate to the new and carry `@deprecated`; MiMa is unaffected.

```scala
extension (self: TxInfo) {

    /** The unique output paying back to `ownInput`'s address, or fail.
      *
      * Compares the **whole** `Address`. The two existing finders that look similar –
      * `findOutputsByScriptHash` and `findOutputsByCredential` – match on the payment credential
      * alone, which leaves the staking part unconstrained: an attacker redirects the continuing
      * output's delegation rewards to themselves (AU-4). The corpus contains all three spellings
      * as a live divergence: full `address ===` (hydrozoa, binocular, cosmex),
      * `address.credential ===` (vela, ~15 sites), and `Address(cred, None)` reconstruction
      * (`08 §3.2`). This is the safe one, under the name that says what it checks.
      *
      * Expansion: `outputs.findUniqueOrFail(_.address === ownInput.resolved.address, message)`.
      */
    inline def findContinuingOutputOrFail(ownInput: TxInInfo, inline message: String): v2.TxOut

    /** Total value paid to `addr` across all outputs, as a whole `Value`. */
    def valuePaidTo(addr: Address): Value

    /** Total value spent from `addr` across all inputs, as a whole `Value`. */
    def valueSpentFrom(addr: Address): Value
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 10 | `findContinuingOutputOrFail(ownInput, msg)` | **(b)** | **the #2 pattern**: 14 in-repo files, 6 spellings, one (`UpgradeableProxy:62-65`) skipping the uniqueness check entirely (`01 §P05`); 5 wild projects, 15 sites (`08 §4` r4) | one `findUniqueOrFail` pass; `===` on `Address` is one `equalsData` |
| 11 | `valuePaidTo(addr)` | **(b)** | replaces `Utils.getAdaFromOutputs` (`v3:1161`), which sums lovelace only and which `01 §P17` names as "the direct cause of the P07 token-stripping hazards in Escrow and Vesting"; 9 codebases (`05 §7`) | one fold, `Value.plus` per matching output (CIP-153 `unionValue` at PV11) |
| 12 | `valueSpentFrom(addr)` | (b) | replaces `Utils.getAdaFromInputs` (`v3:1179`) on the same ground | as 11 over `inputs`; two operations rather than one parameterized, because the element types differ and merging would cost a `map(_.resolved)` |

Both sums are `Address`-first. A `Credential` overload was rejected (§6.8): where a payee's staking
choice is legitimately theirs, the loose form is one explicit line over `outputs.filter`, visible in
review.

**Foreign state is reached by token or not at all.** There is deliberately no
find-reference-input-by-address (`06 AU-1`, §6.10). The token-authenticated lookup is an idiom over
§4.2, not an operation:

```scala
val cfg = tx.referenceInputs
    .findUniqueOrFail(_.resolved.value.hasNft(cfgPolicy, cfgName), NoConfig)
    .resolved.datum.inlineOrFail[Config](BadConfig)
```

An earlier `findWithTokenOrFail` and `referenceDatumByToken` were this line under two names; the
"11 in-repo sites" cited for them turned out to be `01 §P15`, which asserts a token on an
*already-found* UTxO – a different check (§4.9).

### 4.7 Authorization, time and double-satisfaction (`v3/Contexts.scala`)

```scala
extension (self: TxInfo) {

    /** Any of `keys` signed the transaction. */
    def isSignedByAny(keys: List[PubKeyHash]): Boolean

    /** The validity range's lower bound, or fail if unbounded. INCLUSIVE: the ledger builds it
      * with `PV1.lowerBound` (closed), so this is the earliest time the transaction can be
      * included, and it is the value to compare against a "not before" deadline.
      */
    inline def validFromOrFail(inline message: String): PosixTime

    /** The validity range's upper bound, or fail if unbounded. EXCLUSIVE under Conway: the
      * ledger builds it with `PV1.strictUpperBound` (open), so the transaction cannot be
      * included AT or after this time. A timestamp written into a datum from this value is an
      * upper bound on the real posting time – it can be late, never early.
      */
    inline def validToOrFail(inline message: String): PosixTime

    /** An output pays exactly `value` to `addr` carrying `tag` as its datum – the DS-1 tagged
      * output defence, closing DS-2 as well because a `TxOutRef`-derived tag is globally unique.
      */
    def hasPaidTagged(addr: Address, value: Value, tag: OutputDatum): Boolean
}

extension (self: TxOutRef) {
    /** `blake2b_256(serialiseData(ref.toData))` – a name unique to this output reference. */
    def deriveTokenName: TokenName
}
```

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 13 | `isSignedByAny(keys)` | (a), **conditional** | the only prelude spelling is `keys.exists(tx.isSignedBy)`, and `exists` is `find(p).isDefined` (`List.scala:967`) | the measured `Option` tax, 326 483 / 564 996 cpu per call (`07 I2`). Direct tail recursion avoids it. If `exists` is ever made intrinsic like `contains`, delete this |
| 14 | `validFromOrFail(msg)` | **(c)** | 5 in-repo raw-bound sites; `getValidityStartTime` (deprecated, §8.2) is this with `0` substituted for failure, and caused 2 in-corpus bugs | one `boundType` match, identical to `finiteOrFail` |
| 15 | `validToOrFail(msg)` | **(c)** | Vault `:136`, HTLC `:62`, binocular `created == tx.validRange.to` (`TreasuryMovementValidator.scala:585-590`) all read it and reason about exclusivity in comments | same |
| 16 | `hasPaidTagged(addr, value, tag)` | **(b)** | JPG Store v3 hashes its own out-ref into the payout datum with the comment `// for double satisfaction` (`validators/ask.ak:45-50`); cardano-swaps and lenfi embed it (`04 §3`, `05 §7`) | one fold over outputs; three exact comparisons per output – whole `Address`, whole `Value` by `===` (never `>=`, VP-4), datum tag. Losing any one reopens DS-1 |
| 17 | `TxOutRef.deriveTokenName` | **(c)** | 6 in-repo one-shot sites (`01 §P13`); an off-chain mirror must produce the identical bytes, so the definition must be in one place | `serialiseData` + `blake2b_256`, two builtins. An earlier `asDatumTag` was the same digest wrapped in `OutputDatum` and is gone: write `OutputDatum.OutputDatum(ref.deriveTokenName.toData)` |

**Why `validFrom` / `validTo`.** The pair is what the caller already used to *set* the range:
`TxBuilder.validFrom(instant)` / `validTo(instant)` (`TxBuilder.scala:1429`, `:1442`, the latter
documented "exclusive"), Lucid and Evolution `validFrom` / `validTo`, and the locals users name
when they read it by hand (hydrozoa `val validFrom = tx.validRange.from.finite(0)`, `08 §2.1`).
The alternatives were checked: Aiken exposes the raw fields `validity_range.lower_bound` /
`upper_bound` and vodka wraps them as predicates `valid_after` / `valid_before` (our
`isEntirelyAfter` / `isEntirelyBefore`); Plutus has `ivFrom` / `ivTo`; Helios has
`tx.time_range.start` / `end`. "Lower/upper bound" is the Interval vocabulary, correct and
unfamiliar; `validFrom` / `validTo` is the transaction vocabulary every builder already teaches.

**Why the bound accessors are operations and `finiteOrFail` is not enough.** `IntervalBound.finiteOrFail`
(`v1/Contexts.scala:133`) returns the number and drops `isInclusive`. The ledger fixes inclusivity –
`Alonzo/Plutus/TxInfo.hs:265-267` builds `Interval (lowerBound t1) (strictUpperBound t2)`, and
`LedgerToPlutusTranslation.getInterval` (`:335-366`) mirrors it – so an accessor on `TxInfo` can
state the convention in its name and scaladoc where the generic accessor cannot. A caller writing a
datum timestamp from `to.finiteOrFail` has no way to know it is exclusive; a caller of
`validToOrFail` is told. That is ground (c).

Restoring these two also deletes a third: an earlier `validityWidthAtMost(ms)` existed only to fail
closed on an infinite bound, and `validToOrFail(m) - validFromOrFail(m) <= ms`
now fails closed for free.

**On the single-sided intervals – resolved.** Conway's `transValidityInterval`
(`eras/conway/impl/src/Cardano/Ledger/Conway/TxInfo.hs:793-805`, checked at the pinned tag) builds
`(Just start, Nothing)` as `PV1.from` (closed lower bound) and `(Nothing, Just ttl)` as
`Interval (LowerBound NegInf True) (strictUpperBound t)` (open upper bound), the same conventions as
the two-sided case. Under V3 the scaladoc states "inclusive" and "exclusive" unconditionally.

### 4.8 Rounding-explicit division (`prelude/Math.scala`)

```scala
object Math {
    inline def divFloor(a: BigInt, b: BigInt): BigInt = divideInteger(a, b)     // floors already
    inline def divCeil(a: BigInt, b: BigInt): BigInt = -divideInteger(-a, b)    // floor of the negation
}
extension (self: BigInt) {
    inline infix def divFloor(other: BigInt): BigInt = Math.divFloor(self, other)
    inline infix def divCeil(other: BigInt): BigInt = Math.divCeil(self, other)
}
// total divCeil installments        (infix, the intended reading)
// Math.divCeil(total, installments) (function form, same thing)
```

One builtin each, no comparison: `divideInteger` floors toward negative infinity, so
`divCeil(-7, 2) = -divideInteger(7, 2) = -3` and `divCeil(7, 2) = -(-4) = 4`. `inline` follows
`Math.abs` / `min` / `max` (one-expression bodies are inlined; `clamp`, `gcd`, `sqrt` are not),
and the extension-delegates-to-object shape is the file's pattern (`absolute` → `Math.abs`).

`infix` follows the prelude's existing precedent (`Order.ifEqualThen`, `Option.orFail`,
`Ord.<`). One trap to document: an alphanumeric infix operator has the **lowest** precedence, so
`total divCeil n * fee` parses as `total divCeil (n * fee)`. Parenthesize the operands when they
are expressions. `LinearVestingValidator.scala:120` defines its own `divCeil(x, y)` today and
migrates.

| # | Operation | Ground | Evidence | Cost basis |
|---|---|---|---|---|
| 18 | `a divCeil b` | **(b)** | Scala `/` truncates toward zero, so it rounds **up** for negatives. Fee cuts must round the protocol's share down and the user's obligation up, or a caller extracts a fraction per transaction (`06 AR-1`); 4 protocols (`05 §7`) | `divideInteger` on the negation, two `subtractInteger` |
| 19 | `a divFloor b` | (b) | same | `divideInteger` |

### 4.9 Deleted in review, with the replacement one-liner

Every deletion below was an operation in an earlier draft. Each is recorded with what replaces it,
so the reasoning survives if someone proposes it again.

| Was | Now | Why |
|---|---|---|
| `mintsOnlyToken(p, n, q)`, `mintedUnder(p)` | `tx.mint.hasOnly(p, n, q)`, `tx.mint.tokens(p)` | **already existed**; `hasOnly` takes a signed amount and documents the burn case |
| `mintsOne`, `burnsAll` | `hasOnly` with `1` / `-qty` | two names over one primitive, differing in who writes the sign |
| `mintsOnlyTokens(p, expected)` | `tx.mint.tokens(p) === expected` | one `equalsData` either way |
| `mintsNothingUnder`, `mintsAnythingUnder` | `tokens(p).isEmpty` / `.nonEmpty` | a predicate and its negation; `nonEmpty` exists (`SortedMap.scala:384`) |
| `mintedOnlyOrFail`, `mintedSingleUnder`, `uniqueTokenOfOrFail`, `singleAssetApartFromAdaOrFail` | `tokens(p).singleOrFail(msg)` | one primitive on the sub-map serves all four |
| `hasToken(p, n)` | **reinstated** as `Value.hasNft(p, n)` (§4.4) | deleted as a one-liner; the one-liner has a compile trap (`=== 1` fails) and 26 in-repo sites write it |
| `isAdaOnly` | `withoutLovelace.isZero` | |
| `OutputDatum.hasInline[A](x)` | **reinstated** as `TxOut.hasInlineDatum(x)` (§4.4.1) | deleted as a one-liner, then measured: the one-liner is 60% cheaper than what three of four in-repo spellings write |
| `Address.scriptHashOrFail`, `pubKeyHashOrFail` | `.credential.scriptHashOrFail(msg)` | pure delegation |
| `ownScriptHash(ownRef)` | `ownInput.resolved.address.credential.scriptHashOrFail(msg)` | all 10 sites already bind `ownInput`; the op was a second input scan |
| `findContinuingOutput` (Option), `findUnique` (Option) | `outputs.filter(...) match { Nil / Cons(x, Nil) / _ }` | the "2+" case had to fail, so the `Option` type lied (§6.4) |
| `continuingOutputs` | `findOutputsByCredential`-style `filter` | **already existed** as `findOwnOutputs`; every plural site reduces to one element |
| `findInputWithTokenOrFail`, `findReferenceInputWithTokenOrFail`, `findWithTokenOrFail`, `referenceDatumByToken` | the three-line idiom in §4.6 | one line over `findUniqueOrFail`; cited evidence was for a different check |
| `isSignedByAll`, `countSignedBy` | `keys.forall(tx.isSignedBy)`, `keys.count(...)` | `forall` is `@tailrec`; `count` **already existed** |
| `validityWidthAtMost(ms)` | `validToOrFail(m) - validFromOrFail(m) <= ms` | fails closed for free once the accessors exist |
| `hasSingleOwnInput(ownInput)` | `inputs.findUniqueOrFail(_.resolved.address.credential === ownCred, msg)` | one pass over §4.2's primitive; the signature also took a `TxInInfo` while every site compares a credential |
| `onlyScriptInputsFrom(allowed)` | `inputs.forall(...)` with `contains` | zero in-repo or wild sites; cited evidence was count helpers, not whitelists |
| `spends(ref)` | `tx.findInputOrFail(ref, msg)` as a statement | redundant once `findInputOrFail` is direct recursion |
| `asDatumTag` | `OutputDatum.OutputDatum(ref.deriveTokenName.toData)` | same digest, different wrapper |
| `requiring(p, msg)` | `val x = …; require(p(x), msg)` | `require` already covers it and dodges the lambda-inlining risk |
| `mintDeltaUnder(p, n)` | `tokens(p).get(n).getOrElse(0)` | |
| CIP-68 pairing, computed-name builders, name-shape predicates, whole-tx mint equality | see §4.5.2 | conventions, incompatible schemes, `ByteString` slicing, `tx.mint === v` respectively |

### 4.10 Forward compatibility with CIP-168

[CIP-168](https://cips.cardano.org/cip/CIP-0168) (status: Proposed, no cost figures, no target
version) adds four builtins over the CIP-153 `BuiltinValue`: `policies`, `keepPolicies`,
`dropPolicies`, `assetCount`. Nothing to build on today; the question is whether this API paints
itself in. It does not, for anything typed on `Value`:

| Ours | Today | With CIP-168 |
|---|---|---|
| `withoutLovelace` | `insertCoin(ada, 0)` | `dropPolicies([ada])` |
| `policyIds` | fold over the map | `policies` |
| `hasOnly(p, n, q)` | `equalsData` on the sub-map | `keepPolicies([p]) === expected` |
| `hasSameTokensAndAtLeastAda` | `withoutLovelace ===` + lovelace compare | `dropPolicies([ada]) ===` + compare |

The CIP's motivation – "validating protocol tokens independently of DeFi tokens" via
split-and-validate – is `hasSameTokensAndAtLeastAda` generalized from `[ada]` to any policy set.

**One tension, named now.** `Value.tokens(p)` returns a `SortedMap`, and the idioms
`tx.mint.tokens(p) === expected` and `tokens(p).singleOrFail(msg)` lean on it. `keepPolicies([p])`
returns a `BuiltinValue`, so a builtin-backed `tokens` would pay `unValueData` at the boundary – the
same cost the T7 work avoids for chained arithmetic. Two consequences: prefer the Value-typed form
where one exists (`hasOnly` over `tokens(p) === singleton`), and accept that `tokens(p)` is the one
operation that may not ride the builtin wave. Its cost today is one `SortedMap.get`, which is fine.

`assetCount` has no counterpart here. It is the guard for the token-dust / output-bricking pitfall
(RS-1, VP-6), and 5 protocols hand-roll a bounded-value-shape check (`05 §7` r17). Adding
`Value.assetCount: BigInt` with a fold now and the builtin later would be a pure implementation
swap. It is recorded as a candidate, not added.

---

## 5. Worked examples

Both files were read in full before rewriting. The rewrites below are illustrative of the API's
effect, not a proposed patch – this deliverable ships no code.

### 5.1 `scalus-examples/jvm/src/main/scala/scalus/examples/htlc/HtlcValidator.scala`

**Before** (`HtlcValidator.scala:48-67`, 20 lines):

```scala
inline def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
    val config = datum.getOrFail(InvalidDatum).to[Config]
    redeemer.to[Action] match
        case Action.Timeout =>
            val validFrom = tx.validRange.from.finite(0)
            // validFrom is inclusive, hence 10 <= 10 is correct
            require(config.timeout <= validFrom, InvalidCommitterTimePoint)
            require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
        case Action.Reveal(preimage) =>
            val validTo = tx.validToOrFail(ValidRangeMustBeBound)
            // validTo is exclusive, hence 10 <= 10 is correct
            require(validTo <= config.timeout, InvalidReceiverTimePoint)
            require(tx.isSignedBy(config.receiver), UnsignedReceiverTransaction)
            require(sha3_256(preimage) == config.image, InvalidReceiverPreimage)
}
```

**After** (2 lines change):

```scala
inline def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
    val config = datum.getOrFail(InvalidDatum).to[Config]
    redeemer.to[Action] match
        case Action.Timeout =>
            // validFrom is inclusive, hence 10 <= 10 is correct
            val validFrom = tx.validFromOrFail(ValidRangeMustBeBound)
            require(config.timeout <= validFrom, InvalidCommitterTimePoint)
            require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
        case Action.Reveal(preimage) =>
            // validTo is exclusive, hence 10 <= 10 is correct
            val validTo = tx.validToOrFail(ValidRangeMustBeBound)
            require(validTo <= config.timeout, InvalidReceiverTimePoint)
            require(tx.isSignedBy(config.receiver), UnsignedReceiverTransaction)
            require(sha3_256(preimage) == config.image, InvalidReceiverPreimage)
}
```

**Honest assessment.**

| Line | Change | Value |
|---|---|---|
| `:57` `validRange.from.finite(0)` → `validFromOrFail(msg)` | real, but **not a bug fix here** | HTLC is accidentally safe today: with an unbounded lower bound `finite(0)` yields 0, and the comparison `config.timeout <= 0` then *fails* for any positive timeout. The direction of this particular comparison happens to fail closed. The same primitive in `VestingValidator.scala:68` and `TwoPartyEscrowValidator.scala:83` is the direction that does not (`01 §P09`). The change buys a specific failure message and removes the reliance on comparison direction |
| `:62` `to.finiteOrFail(msg)` → `validToOrFail(msg)` | rename; the name now states exclusivity | `finiteOrFail` was already correct (`v1/Contexts.scala:303`). One less concept name; zero UPLC change |
| `:66` `sha3_256(preimage) == config.image` | **no change** | `ByteString ==` is one of the five types `compileEquality` supports and is already the cheap builtin (`07 I7b`) |
| `tx.isSignedBy` ×2 | **no change** | already `signatories.contains(pkh)` (`v3/Contexts.scala:1082-1083`), already intrinsic |

**What does not improve, and why.** Thirty-eight of the forty-one operations do not apply to HTLC at
all. HTLC has no continuing output, no mint, no value assertion, no own-input lookup, no reference
input and no datum-tagged payout. It is a two-branch terminal escrow: whoever satisfies the
condition signs and takes everything. The API is aimed at state machines, and HTLC is not one.

Two further honest notes. HTLC has **no DS-1 guard and no value check whatsoever**, and
the `count`-based single-own-input guard would be a genuine addition – but the reason it is not a live bug is
that the beneficiary must sign, so the output shape is theirs to choose and there is no third party
to defraud. Adding the guard would change semantics (it would forbid batching two HTLC claims), so
it belongs to the protocol author's judgement, not to a mechanical rewrite. And the inclusive /
exclusive reasoning survives only in the comments at `:58` and `:63`; nothing in this cut moves it
into the type. `06 TI-2` records the in-house HTLC finding that "two error-message strings have
inclusive/exclusive wording swapped", which stays unfixed here (`NormalizedInterval` promotion is
`00 §Part 1` row 50, EXTENDED).

**Budget.** No measurable change. Both bound reads lower to the same
match; `finite(0)` → `finiteOrFail(msg)` replaces a constant with an error branch, which
under the default `generateErrorTraces = true` adds one string constant plus `force(trace(…))`
(`07 I19`, `Lowering.scala:19-26`). The current pins are 29 355 / 12 138 878 for reveal-preimage and
25 922 / 9 249 707 for timeout (`HtlcTest.scala:87`, `:188`), with a script size of 366 B
(`HtlcTest.scala:64`). Expect a few tens of bytes and a few thousand cpu, in the noise. Per `07 R32`
that is the right frame: ExUnits are ~11.4 % of a real transaction fee and CPU alone is 3 %
(`authorized_collections.md:395-403`), so script size matters as much as the budget.

### 5.2 `scalus-examples/jvm/src/main/scala/scalus/examples/vesting/VestingValidator.scala`

**Before** (`VestingValidator.scala:41-122`, the relevant excerpts):

```scala
val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
val contractAddress = ownInput.address

// Reject spending more than one vesting UTxO at once: otherwise a single continuing
// output could satisfy several script inputs (double satisfaction) and the remaining
// locked funds of the extra inputs would be siphoned off.
require(
  txInfo.findOwnInputsByCredential(contractAddress.credential).length === BigInt(1),
  MultipleVestingInputs
)

val contractAmount = ownInput.value.getLovelace
val contractOutputs = txInfo.findOwnOutputsByCredential(contractAddress.credential)
val txEarliestTime = txInfo.getValidityStartTime
// ... beneficiary accounting ...
if requestedAmount === contractAmount then ()
else
    require(contractOutputs.length === BigInt(1), NotExactlyOneContractOutput)
    val contractOutput = contractOutputs.head
    require(contractOutput.address === ownInput.address, ContinuingAddressMismatch)
    require(
      contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
      ContinuingValueMismatch
    )
    require(contractOutput.datum === OutputDatum.OutputDatum(vestingDatum), InvalidDatum)
```

**After:**

```scala
val ownInputInfo = txInfo.findInputOrFail(txOutRef)
val ownInput = ownInputInfo.resolved

// Reject spending more than one vesting UTxO at once: otherwise a single continuing
// output could satisfy several script inputs (double satisfaction) and the remaining
// locked funds of the extra inputs would be siphoned off.
require(
  txInfo.inputs.count(_.resolved.address.credential === ownCred) === BigInt(1),
  MultipleVestingInputs
)

val contractAmount = ownInput.value.getLovelace
val txEarliestTime = txInfo.validFromOrFail(NoValidityLowerBound)
// ... beneficiary accounting UNCHANGED ...
if requestedAmount === contractAmount then ()
else
    val contractOutput =
        txInfo.findContinuingOutputOrFail(ownInputInfo, NotExactlyOneContractOutput)
    require(
      contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
      ContinuingValueMismatch
    )
    require(contractOutput.datum.hasInline(vestingConfig), InvalidDatum)
```

**Honest assessment.**

| Change | Value | Basis |
|---|---|---|
| `findOwnInputsByCredential(cred).length === 1` → `inputs.count(...) === 1` | **real, cost and clarity** | The old form is `outputs.filter(pred)` then `length`: 2 traversals plus k `mkCons`, and on the Data-packed `BuiltinList` representation **neither `filter` nor `length` has an intrinsic** (`07 I1`, `07 E23`; `IntrinsicResolver.scala:648-649`, `ListIntrinsics.scala:90-94`). The new form is one `count` fold with no allocation. It also names the DS-1 obligation, which is what `01 §P18` says is missing from 30 of the 35 in-repo validators |
| `getValidityStartTime` → `validFromOrFail` | **robustness, not a bug fix here** | Vesting is accidentally safe today for the same reason HTLC is: with an unbounded lower bound the old call yields 0, `linearVesting(config, 0)` returns 0, `availableAmount` becomes `0 - released ≤ 0`, and `requestedAmount <= availableAmount` fails because `requestedAmount > 0` was already required at `:51`. The bug bit elsewhere – Vault (deadline derived from the lower bound) and TwoPartyEscrow, which **writes the value into the datum** as `depositTime`, so an unbounded lower bound records 0 and the 30-minute refund window is already in the past (`01 §P09` hazard). The change converts an accidental fail-closed into an explicit one with a message, and removes a call to a deprecated method |
| 3 statements (`length === 1`, `.head`, `address ===`) → `findContinuingOutputOrFail` | **real, cost and correctness class** | 3 lines to 1; one fused scan instead of `filter` + `length` + `head` (the last of which allocates an `Option`, `List.scala:1121`). It also removes the `.head`-after-`length` pattern that `01 §P05` warns about: "safe, but copy-pasted 14 times and one mis-copied guard silently becomes an unchecked `.head`". Vesting's full-address check was already correct; the API makes correct the only option |
| `datum === OutputDatum.OutputDatum(vestingDatum)` → `datum.hasInline(vestingConfig)` | **naming only** | Byte-identical UPLC – both are one whole-tree `equalsData` (`07 I7`, pins at `ValueTest.scala:1619` vs `:1628`). The gain is that the operation is named and greppable; the loss is one `toData` round trip in the source that the compiler erases (`07 §1.6`) |
| beneficiary accounting (`:83-97`) | **no change, by design** | See below |
| `contractOutput.value === ownInput.value - Value.lovelace(requestedAmount)` | **no change** | Already the correct full-`Value` form, already the CIP-153 `unionValue` + `scaleValue` path (`07 §2.3`). `hasSameTokensAndAtLeastAda` is *not* appropriate here: this output's shape is fixed, so exact equality is what is wanted |
| error constants | **no change** | 12 `inline val` message constants remain. `01 §7` item 2 counts 9 in-repo files carrying such blocks (VaultValidator alone has 38). Nothing in this cut removes them |

**What does not improve, and why it matters.** The beneficiary accounting is the riskiest code in
this validator and the API leaves it alone:

```scala
val beneficiaryCred = Credential.PubKeyCredential(vestingConfig.beneficiary)
val adaInInputs = Utils.getAdaFromInputs(txInfo.findOwnInputsByCredential(beneficiaryCred))
val adaInOutputs = Utils.getAdaFromOutputs(txInfo.findOwnOutputsByCredential(beneficiaryCred))
require(adaInOutputs === requestedAmount + adaInInputs - txInfo.fee, BeneficiaryOutputMismatch)
```

Three reasons it is untouched.

1. **`valuePaidTo` / `valueSpentFrom` take an `Address`, this code has a `Credential`** (§4.6). No
   `Credential` overload ships, and `valuePaidToCredential` was explicitly rejected (§6.8). The loose
   form stays spelled out, which is the intended outcome – but it means this call site sees no
   improvement.
2. **Converting to `Value` arithmetic changes semantics.** `adaInOutputs === requestedAmount +
   adaInInputs - fee` is an ADA-only ledger identity. The `Value` form would additionally require
   the beneficiary's own native tokens to be conserved across the transaction, which is a stricter
   rule that may or may not be what the protocol wants. That is a protocol decision, not a
   mechanical rewrite.
3. **`01 §P17` flags the identity itself as fragile**: it "breaks if the beneficiary's own inputs
   carry tokens, and hard-codes the assumption that the beneficiary pays the fee." Neither is
   addressed by any operation in this cut.

`Utils.getAdaFromInputs` / `getAdaFromOutputs` (`v3/Contexts.scala:1179`, `:1161`) are named in
`01 §P17` as "the direct cause of the P07/P17 token-stripping hazards in Escrow and Vesting" and are
VP-2 (rank 3, scored 20). They stay, with a scaladoc pointer (§8.3). This is the clearest example in
the specification of an operation that documentation can only mitigate.

**Budget.** Two `filter`-based scans become one `count` fold and one fused unique-scan; one `head`
`Option` allocation disappears; one error branch is added. Directionally cheaper on the continuing
path, roughly neutral elsewhere. The current pins are 154 500 / 57 255 916 and
185 197 / 74 033 910 (`VestingTransactionTest.scala:142`, `:175`). Per `07 R34`, any implementation
must re-pin these.

---

## 6. Rejected alternatives

### 6.1 Fused `requireContinuing(ownInput, value, datum)`

**Decision: rejected.** The API ships the finder (`findContinuingOutputOrFail`, op 20) plus separate
`require`s.

**Counter-evidence, recorded in full.**

- `00 §Part 5 Q1` puts the question directly and its recommendation is the opposite of the decision:
  "**Recommendation: both, fused form documented as the default**."
- `01 §6` ranks the fused form **#2** of the top-15 API candidates, touching 12 files, with the
  reason: "the four checks are always written together and one is usually weakened."
- `06 §7` ranks it **#2** of the top-15 mitigations and specifies no partial variant: "`value` is a
  **required** argument; no `continuing(own, datum)` overload; no `ValuePolicy.Unchecked`". It is the
  named fix for VP-1 (rank 2, scored 25).
- **Users build it themselves when given only pieces.** cosmex wrote `expectNewState(ownOutput,
  ownInputAddress, newState, newValue)`, which fuses datum, address and value in one call
  (`08 §2.4`, `CosmexValidator.scala:219-249`) – the corpus' clearest continuing-output abstraction.
  vela wrote `getContinuingCdp` twice (`08 §2.2`).
- The in-repo weakening is documented: `01 §P07` finds 12 files checking continuing value across 4
  spellings, with "lovelace-only" as the high hazard; `01 §4` N11 pairs Vesting's full-`Value` delta
  against Pricebet's `getLovelace === x*2`.

**Mitigation.** `findContinuingOutputOrFail`'s scaladoc must enumerate the obligations the caller
still owes on the returned `TxOut`, naming the pitfall each one closes:

1. **value** – `out.value === ownInput.resolved.value - withdrawn + deposited`, full multi-asset
   (VP-1, VP-2). Not `getLovelace`.
2. **datum** – `out.datum.hasInline(expectedWholeDatum)`, built with `old.copy(changed = …)`
   (DT-1, DT-3). Not field-wise.
3. **address** – already done by this operation; do not re-check and do not loosen.
4. **exclusivity** – this operation proves the output is unique, not that your *input* is
   (the single-own-input guard, DS-1) and not that another instance is not sharing it (DS-2,
   `hasPaidTagged`).

That scaladoc is the only thing standing between this decision and VP-1, and it is weaker than a
required argument. Recorded as a risk (§10.2).

### 6.2 `requireX` wrappers as a layer

**Decision: rejected.** `require` is already `inline` (`prelude/Prelude.scala:108-109`), so
`requireMintsOnly(p, n, q, msg)` and `require(tx.mint.hasOnly(p, n, q), msg)` produce the identical
term. A wrapper layer would double the API surface, give every concept two names, and move the
failure message from the call site into the library, all for zero UPLC.

Note the shape this rules out. `00 §Part 1`'s whole L2 layer was specified as `require*` operations
(`00 §Part 3`, L2), and `06 §7` writes every one of its top-15 mitigations in `require*` form. This
decision re-expresses that layer as predicates. The safety argument has to survive the translation:
it does, because the safety was always in the *semantics* (whole-sub-map, full address, exact value)
and never in the wrapper.

### 6.3 A `requiring` checked-identity combinator

**Decision: rejected.** A combinator of the shape `x.requiring(p, msg): A` (evaluate `p(x)`, fail if
false, return `x`) reads well and chains. It is dropped for two reasons.

`require` already covers it in two lines with no new concept. And it takes a lambda, which walks
into the lambda-inlining constraint: `07 §5.3` item 1 establishes that **the lambda passed to a
combinator is never inlined**, because a recursive linked def lowers to
`Apply(LamAbs(f, body), Apply(LamAbs(f, f f), LamAbs(f, rhs')))`, that fixpoint is an `Apply` over a
non-builtin so `isValueForm = false` (`TermAnalysis.scala:135-148`), call sites are `OnceGuarded`
which requires a value form (`Inliner.scala:146`), and with ≥ 2 call sites `Many` rejects a `LamAbs`
outright (`:147-152`). A `requiring` marked `inline` might dodge this – scalac would expand the
lambda before the plugin sees it – but "might" is not a cost contract, and `00 §Part 2(g)` already
marks the analogous inline-CPS trick as **verify at implementation time**.

### 6.4 `findUnique(p): Option[A]` – and the reversal on `singleOrFail`

**Decision: `findUnique` rejected; `singleOrFail` accepted after an earlier rejection.**

`findUnique` was the one `Option`-returning twin kept through two review passes, for the at-most-one
case (SimpleTransfer: no continuing output is a full withdrawal, one is partial). Its "two or more"
case had to **fail** – two is never a legitimate `None` – so it was a hybrid: `None` for zero,
`Some` for one, `fail` for more. The type lied, and the specified signature had no message parameter
to fail with. The replacement makes all three outcomes visible with their own message:

```scala
tx.outputs.filter(_.address === ownAddr) match
    case Nil            => // full withdrawal
    case Cons(out, Nil) => // partial
    case _              => fail(TooManyContinuingOutputs)
```

`singleOrFail` was rejected in an earlier draft as redundant with `findUniqueOrFail`, on the
argument that every `oneOrFail` site is preceded by a `filter`. That is true of lists that are
*filtered* and false of collections that *arrive* at size one: `tx.mint.tokens(p)`, `tx.signatories`,
a redeemer's index list. On those there is no predicate step, so `findUniqueOrFail` does not apply
and `head` accepts the first of five. It is accepted on `List`, `SortedMap` and `AssocMap` (§4.2),
and on `SortedMap` it replaces three operations at once.

### 6.5 `TxOut.inlineDatumOrFail`

**Decision: rejected as redundant.** `OutputDatum.inlineOrFail[A: FromData](inline message)` already
exists at `v2/Contexts.scala:60-99` (`:82` for the default message, `:94` for the explicit one) and
is strictly better than a `TxOut`-level re-hang: being `inline`, the match is checked against the
receiver's static type, so calling it on a value the compiler knows is `NoOutputDatum` or
`OutputDatumHash` is rejected at compile time (`v2/Contexts.scala:70-73`).

The demand is nonetheless the second-largest single row in the wild corpus: **20 sites across 5
projects** hand-roll the `OutputDatum.OutputDatum(d) => d.to[T]` match (`08 §4` r2), and 18 in-repo
files do the same (`01 §P04`). vela wrote the identical three-line decode **5×** as
`getInlineCdpDatum` / `getInlineSpState` / `getInlineDepositDatum` (`08 §2.2`); binocular wrote
`extension (d: OutputDatum) inline def of[A: FromData]` (`08 §2.3`,
`TreasuryMovementValidator.scala:229-233`); hydrozoa wrote `inlineDatumOfType[T]` on `TxOut`
(`08 §2.1`, `TxOutExtensions.scala:20-22`).

**This is a discoverability problem, not a coverage gap**, and it is the same failure mode `08 §6`
item 5 records for `SortedMap.getOrFail` – which exists, and which vela wrote twice anyway. The fix
is documentation: §8.3 specifies an index of "the fail-fast form of every lookup" and a scaladoc
cross-reference from `TxOut` to `OutputDatum.inlineOrFail`.

Note one genuine gap this leaves open. binocular's `of[A]` carries a scaladoc explaining that it is
`inline` because "a non-inline generic would reference the companion's `derived$FromData` module,
which is not `@Compile`d for externally-defined types". That is a linker limitation
(`08 §3.6`), not an API gap, and it is listed in `00 §Part 4` under compiler fixes.

`TxOut.hasInlineDatum(a)` (§4.4.1) is not this. It is a predicate for datum *equality*, not a
re-hang of the decoder, and it exists because the decoder is the wrong tool for equality.

### 6.6 `TxInInfo.address` / `.value` / `.datum` shortcuts

**Decision: rejected.** `.resolved` stays explicit.

`TxInInfo` is `case class TxInInfo(outRef: TxOutRef, resolved: TxOut)` (`v2/Contexts.scala:129`).
Shortcuts would save four characters per access and would blur the one distinction that matters when
reading a validator: whether a check is about the *reference* (identity, binding, `outRef ===`) or
about the *resolved output* (address, value, datum). `06 IX-3`'s rule is "an index is only ever a
hint; the binding must be by `TxOutRef` or by beacon", and keeping `.resolved` visible keeps the two
halves visually distinct at every call site.

There is no measured cost argument either way: `TxInInfo.resolved` is field 1, one `tailList` +
`headList` at PV11, and the result is memoised as a scope-keyed lazy var so repeated reads of the
same scrutinee inside one lowering scope share the spine (`07 §1.3`,
`ProdDataListOps.scala:161-193`, `:164`).

### 6.7 Weak mint assertions and `requirePaidAtLeast`

**Decision: excluded.** Neither ships at any name.

For weak mint assertions the argument is §4.5.1: MI-1 is the costliest incident class on record
(~$195M TVL at risk, Minswap, March 2022 – `06 §5`), and `06 MI-1`'s API-level fix is categorical:
"*The stdlib exposes no `quantityOf`-based mint assertion at all.*" `00 §Part 2(i)` records that
vodka ships four strictness levels and they are heavily used (`03 §5.3`), and that EditableNft uses
the weak form deliberately on its spend path (`01 §P11`). The resolution is that the weak check
stays one `require` + `quantityOf` away – deliberately less convenient than the safe form, per
`06 §6` lesson 1.

For `requirePaidAtLeast` the argument is that **every DS-1 incident shape in the corpus routes
through an at-least payout check**. `06 DS-1`'s naive code is
`tx.outputs.exists(o => o.address === beneficiary && o.value.getLovelace >= price)`; PlutusTx's
`valuePaidTo` is named "the canonical DS-1 enabler" (`06 §6`); `06 VP-4` explains the mechanism –
"an output that satisfies 'at least' can satisfy two obligations at once". `00 §Part 5 Q8`'s
recommendation is to omit it, and `06 VP-4` proposes that if it ever ships the name must state the
danger (`payAtLeastRequiresExclusive`, so that "grepping for `AtLeast` finds every DS-1 candidate").

What ships instead: the query (`valuePaidTo`, op 26) and the safe obligation (`hasPaidTagged`,
op 36). `>=` remains writable explicitly by whoever accepts the risk, at the call site, in the
review diff.

### 6.8 `valuePaidToCredential(cred)`

**Decision: rejected as a named operation.**

Demand exists – vela's payout-to-address sum filters on `address.credential`
(`08 §2.2`, `stable/vela/CdpValidator.scala:311-317`), and Lottery matches the credential rather
than the address (`01 §P16`). The safe form (`Address`) and the unsafe form (`Credential`) differ
only in the staking part, and their consequences differ by AU-4: reward hijack plus a broken DS-1
guard (§4.6).

`06 §6` lesson 1 is the governing rule: "A safe helper next to an unsafe sibling with a similar name
is not a safe API. … The unsafe sibling must be *harder* to reach – a different (low-level) import,
a longer name that states the danger, or absent entirely." `valuePaidTo` and
`valuePaidToCredential` would sit side by side in the same companion object under near-identical
names. That is the exact shape of `validate_mint` / `validate_mint_minimal` and
`value_sent_to_datum` / `value_sent_to`, both named in `06 §6` as prior-art failures.

So the loose form stays **spelled out at the call site**:

```scala
tx.outputs
    .foldLeft(Value.zero)((acc, o) => if o.address.credential === cred then acc + o.value else acc)
```

Four lines rather than one, and visible in every review diff. This is the extension-only
approximation of `06 R2` ("unsafe is a named argument, never an omission"); `00 §Part 1` row 36 and
`00 §Part 2(c)` reach the same place for the continuing-output analogue via a *differently named*
operation (`requireSamePaymentCredential`), which this cut does not ship either.

### 6.9 Validating `FromData` decoders for invariant types

**Decision: rejected as unsound on this compiler.**

`06 IX-1` and `06 AR-2` both propose opaque types whose `FromData` instance rejects bad input:
`AscendingIndices`, `IndexPairs`, `Positive`, `NonNegative` – "so a redeemer carrying duplicates
never reaches user code", "the check is performed by the framework at decode time, exactly once".

**Under the default backend that decoder does not exist.** Two steps erase it (`07 §1.6`):

1. **Link time.** `SIRLinkerOptions.useUniversalDataConversion = (backend == SirToUplcV3Lowering)`
   (`compiler/sir/linking/SIRLinker.scala:20-21`). When true, the linker rewrites every
   `fromData`/`toData`-annotated `SIR.Apply` head into `UniversalDataConversion.fromData` / `.toData`
   (`SIRLinker.scala:300-318`) instead of linking the real given instance.
2. **Lowering.** `Lowering.lowerFromData` (`lowering/Lowering.scala:1089-1112`) returns a
   `ProxyLoweredValue(data)` whose `termInternal` is verbatim `data.termInternal(gctx)` (`:1096-1097`)
   – **the identity function**. Only the type and representation change.

So `fromData[T](d)` costs zero UPLC, and any validation embedded in the decoder would **run on the
JVM and be silently skipped on-chain**. That is the worst possible divergence: the developer's unit
tests pass, the deployed script does not check. `06 EV-2` names this class and lists six historical
instances, of which E3 is the closest analogue – "a `validate(datum).copy(…)` whose validation throws
silently succeeded".

This also matches the repo's own prior T9 decision: keep the lazy no-op `fromData`, make validation
an explicit opt-in `expect` (`00 §Part 2(h)`). Invariants are established by explicit checks at the
use site. Any future `Trusted[D]` design must be built on an explicit deep-check entry point, never
on a `FromData` instance.

Consequence for this cut: the strongest forms of `06 IX-1`, `06 AR-2` and `06 MI-2` (the opaque
`Beacon`) are unavailable, and the operations that would have depended on them are either absent
(`AscendingIndices`) or reduced to a documented idiom (`findInputOrFail` + `deriveTokenName`, §4.7).

### 6.10 Find reference input by address

**Decision: rejected.** Covered at §4.6. Foreign state is reached by token or not at all
(`06 AU-1`, `00 §Part 3` L1 exclusions).

---

## 7. Implementation rules

Binding on any implementation of the operations above. Each rule names the measurement that forces
it. All measurements are from `07` and cite a repo artifact.

| # | Rule | Measured basis |
|---|---|---|
| R1 | **Never use `exists` internally.** Use `contains` for equality scans, or hand-written direct tail recursion. | `List.exists` is `find(p).isDefined` (`prelude/List.scala:967`) and is **not** intrinsic. Measured to cost the full Option tax on V3: **326 483 cpu (miss) / 564 996 cpu (hit) ≈ 85 / 158 lovelace per call** (`07 §4.1`, `LIST_CONTAINS_IMPL_COST.md:33-40`). `List.contains` **is** intrinsic and is byte-identical to hand-written direct recursion (`ListIntrinsics.scala:257-258`; asserted at `ContainsImplBenchTest.scala:213-224`); the intrinsic drops both the `Option` and the implicit `Eq` closure (`IntrinsicResolver.scala:36-50`) |
| R2 | **Never use `filter(...).length` (or `.size`).** Use a fused single-pass scan that returns the element or the count. | No fusion exists anywhere in Scalus – verified by exhaustion over the pass lists (`07 §3.6`). `filter` is `foldRight(...)(Cons)` (`List.scala:805-808`), so `xs.filter(p).length` is 2 full traversals plus k `mkCons`, and on the Data-packed `BuiltinList` representation **neither has an intrinsic** (`07 I1`; `IntrinsicResolver.scala:648-649`, `ListIntrinsics.scala:90-94`) |
| R3 | **Direct tail recursion, never `foldRight`.** Build results with an accumulator plus one `reverse` if a list is genuinely needed. | `foldRight` is not tail-recursive (`List.scala:947-949`), and `map` / `filter` / `filterMap` are all built on it (`:765`, `:806`, `:851`) – `07 R9` |
| R4 | **No per-step tuple accumulators.** Return-position tuples are fine: one allocation. | `Tuple2` lowers to `ProdDataList` (`typegens/ProductCaseEmitter.scala:36-41`), so an accumulator tuple builds and tears down a Data list per element – the measured reason `dropRight` costs ~4x `drop` (8 421 / 1 841 559 vs 3 033 / 533 870 at length 0, `ListTest.scala:2332` vs `:2256`). `07 B7` targets per-step accumulators, not a single return (`00 §Part 2(g)`) |
| R5 | **All callbacks are `=> Unit`, never `=> Boolean`.** | `06 R1` / `06 EV-1`: a `Boolean` callback invites `a && b` chains, which is the `evaluation-order` footgun, and it throws away the failure message. The corpus contains the bug this creates: adastream's `require(expired && expired, "HTLC is not expired")` (`08 §5` item 1) |
| R6 | **No closures in public types.** Pass comparators and predicates as arguments, never store them in a field. | `containsFun` silently forces `ProductCaseUplcConstrOnly` / `SumCaseUplcConstrOnly` (`typegens/SirTypeUplcGenerator.scala:369-370`, `:494-496`). Such a value can never be `Data`: it loses `equalsData`, loses free `fromData` / `toData`, and loses every Data-shaped intrinsic (`07 A2`, `07 I23`) |
| R7 | **One `require` per obligation, never `&&`-chained.** | `06 EV-1`: "Every obligation is its own statement." Chained conjunctions short-circuit, so a security-relevant predicate on the right of `\|\|` or inside an untaken branch silently never runs. The in-corpus instance is adastream `contract.scala:242` – a duplicated conjunct that a chained form hid (`08 §5` item 1) |

Two further constraints that follow from `07` and bind the implementation without being separate
rules:

- **Match the intrinsic's method name and arity exactly.** `Value` intrinsic dispatch is by *simple
  name* against a 7-entry map and requires exact arity (`compiler/intrinsics/ValueIntrinsics.scala:99-107`,
  `IntrinsicResolver.scala:279-280`, `:297-298`). A wrapper with a different name, or a partially
  applied call, **silently** falls back to the portable loop with no warning (`07 D19`).
- **Bind a field once above a `match`, never inside two arms.** CSE will not hoist an
  `unConstrData` / `headList` chain across a `Case` branch or a `Delay`
  (`CommonSubexpressionElimination.scala:157-164`, `:406-416`), and the `genSelect` scope cache only
  shares within one lowering scope (`ProdDataListOps.scala:138-139`, `:164-165`) – `07 R29`.

Every new operation must arrive with an `ExUnits` pin in the `ListTest` / `SortedMapTest` /
`ValueTest` style. That corpus (220 + 71 + 69 pins, `07 §4.2`) is the project's cost regression net;
`07 R34`: "a combinator with no pin has no cost contract."

---

## 8. Fixes, deprecations and documentation changes

### 8.1 Bug fixes

**`Eq[DCert]` (`v1/Contexts.scala:310-341`) and `Eq[ScriptPurpose]` (`v1/Contexts.scala:743-760`)
compare every field to itself.**

The inner pattern binder shadows the outer one of the same name, so each field comparison is a
self-comparison and is unconditionally `true`:

```scala
// v1/Contexts.scala:312-315
case DCert.DelegRegKey(cred) =>
    y match
        case DCert.DelegRegKey(cred) => cred === cred   // inner `cred` shadows the outer one
        case _                       => false
```

**Every case** of both instances has this shape (`02 §C.20`), confirmed by execution: four probes
returned `true` for values differing in credential, epoch, policy id and `TxOutRef`. The v3
`Eq[ScriptPurpose]` is correct – it uses `lhs`/`rhs` prefixes – which is why it was not affected.

Impact is off-chain only. On-chain, `Eq` is a marker the lowering replaces with a structural
`equalsData` (`07 I7`, `LoweringEq.scala:56-60`, `:74-87`), so the broken bodies are never emitted.
That accident is not a reason to leave them: `06 EV-2` records "user-defined `Eq` instances silently
replaced by structural equality" as a shipped JVM/on-chain divergence, and these are that class.

Fix: rename the inner binders to the `lhs`/`rhs` convention v3 already uses, and add a regression
test asserting `a === b` is `false` for differing fields – the current suite has no such case, which
is why this survived.

### 8.2 Deprecations

All at version `1.1.1`, the latest tag, per the repo convention. `@deprecated` keeps the symbol, so
MiMa is unaffected; every deprecated member delegates to its replacement.

**`TxInfo.getValidityStartTime` (`v3/Contexts.scala:1102-1104`) → `validFromOrFail(msg)`.**

`06 TI-1` calls it "a live footgun in the current Scalus API" and ranks it 8th overall. Its scaladoc
documents the trap as a feature: "if the validity range has no finite lower bound … returns 0". A
transaction with **no** lower bound is then treated as happening at the Unix epoch and every "has
the deadline passed?" comparison flips. Two in-corpus bugs: Vault (`06 TI-1`; the counter-argument
is now in `VaultValidator.scala:132-137`) and TwoPartyEscrow, which writes the value into the datum
as `depositTime` so the 30-minute refund window is already in the past (`:82-84`, `:141`).

Four call sites, all outside `scalus-core`: `VestingValidator.scala:68`,
`DecentralizedIdentityValidator.scala:193`, `TwoPartyEscrowValidator.scala:83` (the one that fixes a
real bug), `scalus-utxo-cell/.../OnChainCellOps.scala:96`. Migrate in the same change if any module
compiles with `-Xfatal-warnings`.

**`IntervalBound.finite(default)` (`v1/Contexts.scala:130-132`) – soft deprecation.** The generic form
of the same invent-a-value trap, live at `HtlcValidator.scala:57` (`finite(0)`). Keep it in the
prelude; the scaladoc gains the warning and points at `finiteOrFail` and the two `TxInfo` accessors.

**`Utils.getAdaFromOutputs` (`v3:1161`) and `getAdaFromInputs` (`:1179`) → `valuePaidTo(addr).getLovelace`
/ `valueSpentFrom(addr).getLovelace`.** Lovelace-only by construction, and `01 §P17` names them as
the direct cause of the token-stripping hazards in Escrow and Vesting. The replacement sums the
whole `Value`; a caller that wants only ADA projects it afterwards, and a caller that forgets gets
the safe answer.

**The eight `findOwn*` members → their un-"Own"ed names.** Table in §4.6. "Own" is accurate in
Plutus, where the function reads the script's own purpose; in Scalus every one of them takes an
explicit argument and finds any input, output or datum. 33 in-repo sites use `findOwnInputOrFail`
alone; the migration is a rename.

**`List.single(a)` (`:48`) and `PairList.single(a, b)` (`:35`) → `singleton`.** Harmonizes the
constructor name across the four collection types (§4.2); the map types already use `singleton`.

### 8.3 Migration and documentation

**`scalus-design-patterns`: Boolean callbacks → `=> Unit`.**

Rule R5 applied to the existing pattern library. `06 §6` names these as the second of the two live
footguns, and `06 EV-1` gives the mapping. Thirteen callback parameters across four files:

| File | Members |
|---|---|
| `scalus-design-patterns/src/main/scala/scalus/patterns/UtxoIndexer.scala` | `:28`, `:41`, `:60-61`, `:81`, `:104`, `:134`, `:152`, `:216` – 9 callbacks |
| `.../StakeValidator.scala` | `:34` `withdrawalRedeemerValidator`, `:60` `withdrawalValidator` |
| `.../TransactionLevelMinterValidator.scala` | `:29` `minterRedeemerValidator`, `:30` `minterTokensValidator` |

New overloads plus deprecation of the old ones, not a breaking change.
`UtxoIndexer.multiOneToOneNoRedeemer`'s *algorithm* is preserved verbatim – `06 IX-2` endorses it
as already correct on both-direction coverage. Only the callback type changes.

Two scaladoc additions in the same pass: `StakeValidator.spendMinimal` gains the PU-1 warning
("proves it ran, not with *which* redeemer"; the `_minimal` sibling is one character from the safe
one), and the singular indexer patterns gain the Anastasia Labs caveat that they solve
`missed-input`, not double satisfaction (`06 IX-2` item 3).

**Documentation changes.**

| Statement | Why | Citation |
|---|---|---|
| **`===` is canonical. Never write `a.toData == b.toData`.** For any Data-backed type `a === b` already lowers to `equalsData`. If `===` does not compile, the type is missing an `Eq` instance – derive one, unless the type has value-vs-structure semantics (`Rational`), in which case the explicit comparator is the point. | The hand-written spelling appears in 4 wild projects – vela 9×, binocular 2×, cosmex 1× (`08 §3.3`). Both spellings pin to **901 mem / 1 653 665 cpu** (`ValueTest.scala:1619` vs `:1628`) | `07 I7`; `LoweringEq.scala:139-146` |
| **Fix `optimize-contract` skill rules O016 and O020.** They advise `equalsData` over typed `===` and are stale. The real advice is `07 I9`: make key types concrete to get `equalsInteger`. | Generic `===` behind a type variable measures **1 761 779 cpu vs 832 313** for a concrete clone | `07 §6`; `SORTEDMAP_LOOKUP_COST_FINDINGS.md:60-72` |
| **`tx.mint.hasOnly(policy, name, qty)` is the mint idiom, and `qty` is signed.** Show mint and burn in the same example block so the sign is visible at the point of learning. | The library's own scaladoc already recommends it; the spec's earlier `mintsOne`/`burnsAll` pair existed because the author had not read it | `Value.scala:905-914` |
| **Compare a continuing datum with `out.hasInlineDatum(expected)`. Use `inlineOrFail` when the fields are needed, not when equality is.** | 286 vs 461 lovelace per comparison, 706 vs 1 136 as a reference script (§4.1); the eight in-repo sites are migrated in the same change: `Auction:444`, `UnfixedAuction:298`, `EscrowValidator:103`, `LinearVestingValidator:115`, `VestingValidator:119`, `SimpleTransfer:78,97`, `TwoPartyEscrow:154`. The `contract` skill gains the same rule; today it shows neither form | §4.4.1 |
| **`contains` over `exists`.** | `contains` is intrinsic and drops the `Option` and the `Eq` closure; `exists` is `find(p).isDefined`, 326 K / 565 K cpu per call | `07 I2`; `List.scala:518,967` |
| **`count` over `filter().length`.** | 1 traversal, 0 allocations, versus 2 traversals and k `mkCons`; nothing fuses | `07 I1`; `List.scala:1004` |
| **`SortedMap` over `AssocMap`; `isEmpty` never `size == 0`.** | `SortedMap.union` is one linear merge, `AssocMap.union` is O(n·m); `size` is O(n) despite `inline` | `07 E20, E22` |
| **An index of the fail-fast form of every lookup.** | `SortedMap.getOrFail` exists and vela wrote it twice anyway (`08 §4` r19); `OutputDatum.inlineOrFail` exists and 5 projects hand-rolled it 20 times | `08 §6` item 5 |
| **Scaladoc warnings on the credential-only finders.** `findOutputsByScriptHash` and `findOutputsByCredential` match the payment credential alone and leave the staking part unconstrained (AU-4). Point at `findContinuingOutputOrFail`. | vela's ~15 `address.credential ===` sites are the live instance (`08 §3.2`) | `06 AU-4` |
| **Scaladoc warnings on the remaining traps.** `Address.fromScriptHash` / `fromPubKeyHash` produce `stakingCredential = None`, never compare against a real output. `List.find` stops at the first match. `Value.lovelaceAmount` (`v1:719`) silently returns another token's amount when ADA is absent. | Each is a documented in-corpus hazard | `01 §P06`, `08 §2.3`, `02 §A.6` |
| **State the plugin's default-`fail` for unimplemented purposes as a security property.** | It closes PU-1 / PU-3 / PU-4 by default and is undocumented | `06 §6` |

### 8.4 Compiler work that would shrink this API further

Three of the nineteen operations survive only on the `Option` / `exists` tax:
`Credential.scriptHashOrFail`, `Credential.pubKeyHashOrFail`, `isSignedByAny`. If the lowering made
`List.exists` intrinsic the way `contains` already is, and folded `Some(x).getOrFail` to `x`, all
three become one-liners and the set is **16**. That is a better fix than three names, and it follows
the repo's established preference for lowering improvements over API workarounds (the reverted
`SortedMap` Eq-get experiment reached the same conclusion). Recorded here so the next person to
touch `ListIntrinsics` knows what it unlocks.

A fourth, from the §4.1 measurement: `d.to[T] === x` lowers to
`equalsData(constrData(0, sndPair(unConstrData(d))), x)` – the decoded value is held as a field
list and rewrapped for comparison. Comparing `d` itself would drop two builtins per comparison and
also stop hard-coding the constructor tag as `0`, which today makes a wrong-tag datum with matching
fields compare equal under the decode form and unequal under the wrap form.

---

## 9. Blocker – script-context map key order (OPEN; a fix was tried and reverted)

> **STATUS: OPEN.** The redeemer-reading operations stay deferred. A fix was attempted
> (`02b6000b9`) and **reverted** (`e9bce861a`, `f1e890f3a`) after review found the approach itself
> unsound and the fix introduced a regression. This section records what is actually established, so
> the next attempt does not repeat the four wrong turns that preceded it.

### 9.1 The mechanism (established, not disputed)

`TxInfo.withdrawals`, `.redeemers` and `.votes` decode into `SortedMap`, whose `get` short-circuits
on `Order.Less` (`SortedMap.scala:625-638`) and whose `FromData` does not re-sort (`:296-308`). So a
lookup is correct **exactly when** `Ord` agrees with the order the ledger delivered. Where they
disagree, `get` returns `None` for a key that is present, silently.

The delivered order is the ledger's, because V3 converts map keys only after `Map.toList` has fixed
the order (`Conway/TxInfo.hs:692-694`):

```haskell
transMap transKey transValue =
  PV3.unsafeFromList . map (\(k, v) -> (transKey k, transValue v)) . Map.toList
```

Upstream declined to specify this ordering at all - IntersectMBO/plutus#5726: *"the ordering of
`ScriptPurpose` is not well-defined and left as an implementation detail"*, and, asked why scripts
would care, *"Scripts don't generally; but compiler makers and library builders do."* Aiken sidesteps
it by typing these fields as `Pairs`, not `Dict`, and `PlutusTx.AssocMap.lookup` is a linear `==`
scan. The exposure is specific to Scalus's short-circuiting `SortedMap.get`.

### 9.2 The finding that reframes the problem

**The three fields are not the same kind of thing**, and that is why a single `Ord`-alignment fix
cannot work:

- `withdrawals`, `data` and `votes` are keyed by **content**, and the ledger derives their order from
  that same content. A content-based `Ord` can track them.
- `redeemers` is keyed **positionally**. The ledger's map is
  `Map (PlutusPurpose AsIx era) (Data, ExUnits)` (`Alonzo/TxWits.hs:144-146`), and

  ```haskell
  -- eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Scripts.hs:283-285
  newtype AsIx ix it = AsIx {unAsIx :: ix}
    deriving newtype (Eq, Ord, ...)
  ```

  keeps only the `Word32` index - the `TxIn` / `PolicyID` / `AccountAddress` is a phantom parameter.
  So the order is `(constructor, index)`, and **no content-based `Ord` can be correct for it in
  general.** V3 escapes only because `Certifying` and `Proposing` carry the index inside the Plutus
  key; V1/V2 have no such escape.

Per-constructor, with the index source read from `redeemerPointerInverse` (`Conway/TxBody.hs:667-679`):

| Constructor | Index source | Scalus compares | Verdict |
|---|---|---|---|
| Spending | `Set.elemAt` on `Set TxIn` | `TxOutRef` content; inputs are a `TaggedSortedSet` | SAFE |
| Minting | `Set.elemAt` on `Set PolicyID` | policy id; `MultiAsset.assets` is a `SortedMap` | SAFE |
| Certifying V3 | list position | compares `idx` **first** | SAFE |
| Certifying V1/V2 | list position | `Ord[DCert]` content, no index | **BROKEN** |
| Rewarding V3 | `Map.elemAt`, ledger `Ord Credential` | ledger-aligned `Ord[Credential]` | SAFE |
| Rewarding V1/V2 | same ledger order | `Ord[StakingCredential]` | **BROKEN** |
| Voting | `Map.elemAt` on `Map Voter _` | `Ord[v3.Voter]` matches, but the builder re-sorts | Ord SAFE, builder **BROKEN** |
| Proposing | `OSet` position | compares `idx` first | SAFE |

Both V1/V2 breakages were reproduced by execution through the production translator.

### 9.3 Two independent defects in the translator

Neither is caused by `Ord`; both are live on `master` today.

1. **`getVotingProcedures` destroys a correct order.** `LedgerToPlutusTranslation.scala:1060,1063`
   re-sorts by `_._1.toString`, which puts `"ConstitutionalCommitteeHotKey"` before
   `"...HotScript"` - the reverse of the ledger - and an inner action index `10` before `2`. Note
   `Ordering[Voter]` (`Voter.scala:94-100`) is already correct and `vp.procedures` is already a
   `SortedMap`, so the sort replaces a right answer with a wrong one.
2. **`getScriptPurposeV3`'s `RedeemerTag.Voting` case** (`:902-905`) resolves the redeemer index
   against that same bad sort, so it can name a **different voter** than the node - making
   `ScriptInfo.VotingScript(voter)` wrong, not merely misordered.

### 9.4 Why the reverted attempt failed

Recorded because the failure modes are the instructive part:

- Its premise was one distinction short. It assumed two key types could carry two orders; in fact
  **three** orders are needed across **two** types, because V2 `withdrawals` needs Plutus order while
  `v1.ScriptPurpose.Rewarding` needs ledger order and both route through `Ord[StakingCredential]`.
- **It introduced a regression.** The votes ordering was *accidentally* consistent beforehand: the
  old `Ord[Credential]` was PubKey-first and the `toString` sort is also Key-first. Flipping `Ord`
  broke the coincidence.
- It fixed a symptom class (`Ord` alignment) that cannot cover the positional key of `redeemers`.

### 9.5 Recommended direction

Keep `SortedMap` with a ledger-aligned `Ord` for `withdrawals`, `data` and `votes`. Change
`v2.TxInfo.redeemers` and `v3.TxInfo.redeemers` to an **association list with linear `Eq` lookup**,
matching `PlutusTx.AssocMap` and Aiken's `Pairs`. That is the only shape correct for a positionally
keyed map, and it is immune to future ledger reordering.

Cost: lookup becomes a full scan instead of an early-terminating one. For realistic redeemer counts
(1-5) that is a handful of `equalsData` calls - `===` lowers to `equalsData` under V3Lowering - in
exchange for removing an entire class of silent `None`. It is a breaking `TxInfo` API change, so it
belongs to the next major. The two translator defects in §9.3 are independent and can land sooner.

### 9.6 Consequence for this specification

`spendRedeemerOrFail`, `withdrawalRedeemerOrFail` and the script half of any authorization helper
remain **deferred** and are not among the 28 operations. They should be built on whatever
representation §9.5 settles on, not on `SortedMap.get`.

Nothing else in this specification depends on the outcome. The other 28 operations touch `inputs`,
`outputs`, `mint`, `signatories`, `validRange` and datums - none of which is affected.

### 9.7 What the next attempt needs

- **A property test** asserting every field of `getTxInfoV2` / `getTxInfoV3` is ascending under its
  `Ord` for arbitrary transactions. This subsumes hand-written cases and would have caught all four
  defects.
- Targeted cases the bundled vectors cannot supply: two `Reward` redeemers under V2; two `Cert`
  redeemers with certificates in reverse content order under V2 and V3; a votes map with both a
  committee script voter and a committee key voter; an inner votes map with action indices 2 and 10.
- **Do not rely on the conformance vector sweep.** It examines 175 transactions and finds **zero**
  with a multi-entry withdrawals, redeemers or votes map, so it witnesses nothing about ordering.

### 9.8 Not yet investigated

`Ord[v3.TxCert]`, `Ord[Delegatee]`, `Ord[GovernanceAction]`, `Ord[ProposalProcedure]`,
`Ord[Address]`, `Ord[TxOut]` internal consistency. Also unverified: whether any off-chain code
constructs one of these maps and serialises it via `SortedMap.toData` (a grep found none outside the
translator, but no CBOR diff was run).

---
## 10. Risks and open questions

### 10.1 Internal inconsistencies found in the fixed decisions

These are recorded rather than resolved, per the brief.

**(a) The operation count is 40.** Group counts: 2 + 4 + 5 + 1 + 5 + 9 + 12 + 2 = 40, after the
owner dropped the `List[TxInInfo].findOrFail` operation (see (b)). Earlier drafts of this document
said "34", a figure inherited from `00 §Part 1`, where 34 rows carry a **CORE** verdict - but that is
a different list (it includes `requireContinuing`, `inputAt`, `outputAtWithToken`,
`requireStrictlyAscending`, `MultisigScript`, and the deferred redeemer operations, and excludes
`hasToken`, `divCeil`/`divFloor` and the four credential projections).

**(b) RESOLVED - `List[TxInInfo].findOrFail` is dropped.** As originally specified it could not live
in `prelude/List.scala`: it requires `TxInInfo` and `TxOutRef` from
`scalus.cardano.onchain.plutus.v2` / `v1`, and the prelude imports **nothing** from those packages
today (`prelude/List.scala:3-9` imports only `scalus.compiler.*`, `scalus.uplc.builtin.*` and
`scala.*`; a repo-wide grep for `import scalus.cardano.onchain.plutus.v[0-9]` under `prelude/`
returns nothing). Placing it there would invert the layering and make the prelude depend on the
ledger types.

The owner dropped the operation rather than relocate it, and nothing is lost:
`TxInfo.findOwnInputOrFail(outRef, msg)` (`v3/Contexts.scala:960`) already performs exactly this
lookup from the `TxInfo` side and has 23 in-repo users (`01 §P02`). A caller holding a bare
`List[TxInInfo]` writes `inputs.findUniqueOrFail(_.outRef === ref, msg)` with operation 1, which is
strictly stricter - it fails on a duplicate `outRef` rather than returning the first match. The
ledger already guarantees `outRef` uniqueness, so that extra walk buys no safety and costs one full
traversal; it is the correct trade only because the alternative (a second, laxer finder sitting
beside the strict one) is the API shape `06 §6` identifies as how incidents happen.

The prelude stays free of ledger types.

**(c) Two operations are specified in terms of an `Address` a caller may not have.** `valuePaidTo` /
`valueSpentFrom` take an `Address`, which is correct per `06 AU-4`, but the two in-repo call sites
that would use them (`VestingValidator.scala:85-89`, `VaultValidator.scala:158-160`) hold a
`Credential` built from a datum-stored `PubKeyHash`. With `valuePaidToCredential` rejected (§6.8),
those sites gain nothing. This is a deliberate consequence, not an error, but it means the measured
in-repo adoption of operations 26 and 27 will be lower than their evidence counts suggest.

**(d) RESOLVED - the value-preservation predicate matches the corpus.** An earlier draft specified
`equalsPlusMinAda(expected, maxSurplus)`. The `maxSurplus` parameter had no support in the corpus:
every observed implementation is unbounded above (`05 §2.2`, `04 §2` r10, `08 §2.3`). The owner
dropped the parameter and renamed the operation to `hasSameTokensAndAtLeastAda`, which states both
halves of the check - exact on tokens, at-least on ADA - so a reviewer can see which side is open
without reading the body. The residual risk of the open upper end is documented in §4.4.

### 10.2 Risks

| # | Risk | Detail | Mitigation |
|---|---|---|---|
| K1 | **VP-1 remains the caller's responsibility.** | The fused-continuation counter-evidence (§6.1) is substantial: `00 §Part 5 Q1` recommends the opposite, `01 §6` ranks the fused form #2 across 12 files with the explicit note "one is usually weakened", `06 §7` ranks it #2 of 15, and cosmex built `expectNewState` itself when given only the pieces. VP-1 is the rank-2 pitfall, scored 25, with four in-house instances. The scaladoc is the whole mitigation, and a scaladoc is weaker than a required argument | Enumerate the four remaining obligations in `findContinuingOutputOrFail`'s scaladoc (§6.1); add a `smart-contract-security-review` rule that flags a call to `findContinuingOutputOrFail` with no subsequent whole-`Value` comparison in the same branch |
| K2 | **`v3/Contexts.scala` grows past 1 182 lines.** | The file is already 1 182 lines with a single `extension (self: TxInfo)` block spanning `:921-1105`. Group 6 (9 ops) and Group 7 (12 ops) add ~21 operations plus scaladoc, plausibly 600–900 lines, putting the file near 2 000. `02 §A.1` already treats it as one of the largest files in the on-chain surface | The placement decision is fixed. Mitigations that do not violate it: split the `extension` block into several `extension (self: TxInfo)` blocks grouped by concern with section banners; keep scaladoc examples short (`07 R27`: long `require`/`fail` message strings cost real bytes under the default `generateErrorTraces = true`, though scaladoc itself is free). If the file becomes unmanageable, the decision to revisit is "one file per concern within `v3/`", not a new package |
| K3 | **`findUniqueOrFail` deliberately pays a full-list walk.** | It is O(n) even when the match is at position 0, and `tx.outputs` is attacker-influenceable in length (RS-3). An adversary can inflate a transaction's output count to raise every validator's cost. The measured per-element budget is ~1 034 543 cpu for an `equalsData` compare (`07 §4.1`) | This is the security property, not a defect: `find` silently accepts a second match (`08 §2.3`), which is a correctness failure, while a slow scan is a cost failure with a ceiling. Document the cost in the scaladoc; note that the escape hatch for hot paths is a redeemer-supplied index – but an index must be *bound*, and no indexed accessor ships in this cut (see K6) |
| K4 | **The deprecation may break `-Xfatal-warnings` builds.** | `@deprecated` on `getValidityStartTime` produces warnings at 4 call sites: 3 in `scalus-examples`, 1 in `scalus-utxo-cell` (§8.2). Any downstream project compiling those with `-Xfatal-warnings` fails | MiMa is unaffected – nothing is deleted, so no filter is needed (`00 §Part 4`: "Delete: Nothing"). Migrate all 4 in-repo sites in the same change. Announce in `CHANGELOG.md` with the replacement named. Note the version string must be the latest tag at the time of the change: `1.1.1` today |
| K5 | **The "no wrapper" decision loses the message-uniformity benefit.** | `01 §7` item 2 records 9 in-repo files carrying blocks of `inline val XyzError = "..."` (VaultValidator has 38) and observes that "a `require`-with-derived-message API would delete all of them." Predicates plus caller-supplied `require` keeps every one of those constants | Accepted. The counter-argument is that a library-supplied message names the *mechanism* ("mint not exact") while a caller-supplied one names the *obligation* ("only the campaign NFT may be minted"), and the second is what appears in a failed transaction's trace |
| K6 | **Index-bound accessors are absent, and the index idiom is dominant.** | `inputAt(idx, expectedRef)` is called "the strongest single stdlib candidate" with 15 in-repo sites (`02 §C.8`); index-in-redeemer is the dominant performance workaround in 5 of 6 wild projects (`08 §3.5`) and is used by 8 of 12 DEXes (`04 §3A`). Without a bound accessor, authors keep writing `tx.inputs.at(i)` plus a separate `require(... .outRef === ownRef)`, and `06 IX-1`/`IX-3` are the classes where they forget the second line | Out of scope by the fixed operation list. `List.at`'s existing PV11 out-of-range guard stays and is correct (`06 IX-1`: "Scalus's `at` already fails on out-of-range, which is correct. Keep it."). Candidate for the next cut |
| K7 | **`hasPaidTagged` imposes a datum on every payout, with a min-ADA cost.** | `06 DS-1` C2: "the payee must accept a datum on every payout; min-ADA impact". A tagged output is larger, so its min-ADA is higher, which interacts with VP-6 | Scaladoc must state the trade-off and the alternative (`hasSingleOwnInput` for DS-1 only, with the explicit note that it does **not** cover DS-2) |
| K8 | **Adding operations to MiMa-stable packages is one-way.** | `v1`, `v2`, `v3` and `prelude` are on the MiMa-checked surface. Once shipped, a signature cannot be changed. `hasSameTokensAndAtLeastAda` (should it later want a `maxSurplus` overload, §4.4) and `hasPaidTagged`'s three-argument shape are the two most likely to want revision after real use | Land the operations with `ExUnits` pins and at least one worked example each before the next release; treat the first release as the point of no return, per the repo's backwards-compatibility policy |
| K9 | **No operation in this cut addresses IX-1 or IX-2.** | `06 IX-1` (index-list duplicates, length mismatch, `zip` truncation) is rank 10, scored 15, with a **Critical** in-house instance: Crowdfunding's `zip` truncation "lets an attacker reclaim all donation UTxOs while paying out only a prefix". `requireStrictlyAscending` and `zipExact` are in `00 §Part 1` (rows 29, 41) but not in the fixed list | Crowdfunding's `requireStrictlyAscending` (`Crowdfunding.scala:619-639`) stays local. Candidate for the next cut, alongside K6 |

### 10.3 Open questions

1. **Where does operation 3 live?** §10.1(b) – the specified placement conflicts with the prelude's
   dependency direction and needs an owner decision before implementation.
2. **How are the redeemer-reading operations unblocked?** §9.5 - an `Ord`-alignment fix was tried
   and reverted; the recommended shape is an association-list `redeemers` field with linear `Eq`
   lookup, which is a next-major change. The two translator defects in §9.3 are independent and can
   land sooner.
3. **Does `hasSameTokensAndAtLeastAda` want a `maxSurplus` overload after real use?** §4.4 – the
   one-sided form matches the corpus and ships first; the bound can be added later but not removed.
4. **When do the deferred index operations (K6) and index-list hygiene operations (K9) ship?** Both
   have strong evidence and both were left out of this cut.
5. **Does the testkit workstream follow?** `00 §Part 5` roadmap note: the single most-used
   third-party API in the comparable ecosystem is a deterministic mock-transaction builder –
   `mocktail` and `virgin_*` outrank every validation helper by 3–5x in measured downstream calls
   (`03 §6.2`). "Whatever ships here should be followed by its testkit counterpart, or adoption of
   the validation layer will lag for want of a way to test it."
