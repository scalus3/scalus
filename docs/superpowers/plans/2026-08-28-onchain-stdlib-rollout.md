# On-chain stdlib API rollout plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the nineteen operations, eight renames, two constructor harmonizations, three
deprecations and two `Eq` fixes from the spec, then move every surface that teaches Scalus –
examples, design patterns, the site, the LLM artifacts and the skills – onto the refined API and
the pitfalls taxonomy, in dependency order.

**Architecture:** Four phases. Phase 1 is the API and is the only phase that touches `scalus-core`.
Phase 2 migrates the code that documents by example and produces the real fee deltas. Phases 3
and 4 rewrite prose and can run in parallel once Phase 2 is merged, because pages cite examples
and skills cite operations. Nothing in Phases 2–4 may start before Phase 1 compiles: the
operations do not exist yet.

**Tech Stack:** Scala 3.3, sbt (`sbtn`), ScalaTest + ScalaCheck, Scalus compiler plugin, MiMa,
Next.js/MDX site (`scalus-site`), `generateLlmsApi` sbt task, Claude Code skills (`scalus-skills`).

**Spec:** `docs/superpowers/specs/2026-08-26-onchain-stdlib-api-design.md` (§4 operations, §4.1
existing idioms, §4.9 deletions, §8 fixes/deprecations/docs). Research corpus:
`docs/internal/stdlib-research/`, in particular `06-pitfalls.md` (taxonomy, §7 top-15
mitigations, §9 skill cross-reference).

## Global Constraints

- **Dependency order is hard.** Phase 2 needs Phase 1 on the branch. Phases 3 and 4 need Phase 2
  merged, because they cite example line numbers and measured budgets that Phase 2 produces.
- **`docs/internal/stdlib-research/` is not a public source.** Derive site pages and skill rules
  from it; never link it from `scalus-site/content` or from a skill. It cites wild-corpus code by
  path and is 2 396 lines in the pitfalls file alone.
- **Budget pins.** Example tests pin ExUnits, some per compiler generation
  (`ScalaCompilerVersion.baseline(pre38, since38)`). Every validator Phase 2 touches must be
  re-measured on **both** 3.3.x and 3.8.x before its pins are updated. The `update-budgets` script
  rewrites by value across files and stomps shared literals; do the tail by hand.
- **Validator tests JIT from SIR.** Run `scalusExamplesJVM/clean` before trusting a green example
  suite after touching a validator.
- **`llms-api.txt` freshness gates `ci-release`.** Regenerate it in the same commit as any public
  API change (`sbtn generateLlmsApi`), or the release workflow blocks.
- **MiMa.** `@deprecated` keeps every symbol; no filters expected for Phase 1. Renames are
  additions plus deprecations, never removals.
- **Skills are one source.** `.claude/skills/*` are symlinks into `scalus-skills/skills/*`. Edit
  the latter only.
- **Commit style:** conventional commits, no co-author trailer, `sbtn scalafmtAll` before every
  commit. Direct to `master` once the branch is merged; rebase before push.
- **Do not touch** the g8 templates (`scalus3/*.g8`) or the starter repo in this plan. They pin a
  released version and follow the next release.

---

## Phase 1 – Core API (`scalus-core`)

Exit criterion: `sbtn quick` green, `sbtn mima` clean, `llms-api.txt` regenerated, every operation
has a unit test and a budget-pinned test on the real compiler.

### Task 1.1: Prelude – `singleton`, `singleOrFail`, `findUniqueOrFail`

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/List.scala`
  (`single` at `:48` → add `singleton`, deprecate `single`; add `singleOrFail`, `findUniqueOrFail`)
- Modify: `.../prelude/PairList.scala` (`single` at `:35` → `singleton`)
- Modify: `.../prelude/SortedMap.scala`, `.../prelude/AssocMap.scala` (add `singleOrFail`)
- Test: `scalus-core/shared/src/test/scala/scalus/prelude/ListTest.scala`, `SortedMapTest.scala`

- [x] `List.singleton(a)`; `@deprecated("use singleton", "1.1.1") inline def single` delegating.
- [x] `PairList.singleton(a, b)` likewise.
- [x] `List.findUniqueOrFail(p, inline message)`: one tail-recursive pass, keeps scanning after the
      first match, fails on zero and on two. Test: zero, one, two, match-not-first, match-last.
- [x] `List.singleOrFail(inline message)`: `Cons(x, Nil)` → `x`, else fail. Test: empty, one, two.
- [x] `SortedMap.singleOrFail` / `AssocMap.singleOrFail` via `toPairList`. Test: empty, one, two.
- [x] Budget test: `filter(p).length === 1` vs `findUniqueOrFail` on a 10-element list, pin both.
- [x] Grep the repo for `List.single(` and `PairList.single(` and migrate in-repo callers.

### Task 1.2: Credential projections

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/v1/Contexts.scala`
  (next to `scriptOption` `:503` / `pubKeyOption` `:507`)

- [x] `scriptHashOrFail(inline message)`, `pubKeyHashOrFail(inline message)`: direct match, no
      `Option`. Scaladoc marks both *conditional* on the `Some(x).getOrFail` fold (spec §4.3).
- [x] Budget test pinning against `scriptOption.getOrFail(msg)`.

### Task 1.3: `Value.hasSameTokensAndAtLeastAda` and `Value.hasNft`

**Files:**
- Modify: `.../v1/Value.scala` (near `hasOnly` `:942`)
- Test: `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/ValueTest.scala`

- [x] Implement as `withoutLovelace === expected.withoutLovelace && getLovelace >= expected.getLovelace`.
- [x] Property test: equal tokens + ADA ≥ → true; any token delta → false; ADA below → false.
- [x] Budget pin at PV11 (`insertCoin`-backed `withoutLovelace`).
- [x] `inline def hasNft(policy, name) = quantityOf(policy, name) === BigInt(1)`. Scaladoc names
      the strict twin `hasOnly(policy, name, 1)`. Test: 0, 1, 2 units; other assets present.

### Task 1.4: `TxOut.hasInlineDatum`

**Files:**
- Modify: `.../v2/Contexts.scala` (extension on `TxOut`, next to the `OutputDatum` extensions)
- Test: new `scalus-core/jvm/src/test/scala/scalus/uplc/eval/DatumEqualityBudgetTest.scala`

- [x] `inline def hasInlineDatum[A: ToData](a: A): Boolean = self.datum === OutputDatum.OutputDatum(a.toData)`.
- [x] Scaladoc carries the measured table (286 vs 461 lovelace) and the "fields vs equality" rule.
- [x] Budget test pins both forms (same datum and `copy`-rebuilt) with mem, cpu and fee, so the
      claim in the scaladoc is checked by CI. Reuse the scratch test from the spec session: it
      lived at `scalus-core/jvm/src/test/scala/scalus/uplc/eval/InlineDatumEqualityScratchTest.scala`
      and is reproduced in spec §4.1.

### Task 1.5: `TxInfo.onlyBurnsUnder`

**Files:**
- Modify: `.../v3/Contexts.scala` (`TxInfo` extension block)

- [x] `tokens(policy)` non-empty and every quantity `< 0`. Test: empty map → false (the vacuous
      case), all negative → true, one positive → false.
- [x] Scaladoc carries the spec §4.5 auction-close example (mint nothing, `forall` passes, NFT survives).

### Task 1.6: `findOwn*` renames and the three new `TxInfo` finders/sums

**Files:**
- Modify: `.../v3/Contexts.scala` (`:938`–`:1075`)

- [x] Add `findInput`, `findInputOrFail` (direct tail recursion, no `Option`), `findDatum`,
      `findOutputsByScriptHash`, `findInputsByCredential`, `findOutputsByCredential`.
- [x] `@deprecated(..., "1.1.1")` on all eight `findOwn*`, each delegating. `findOwnInputs` /
      `findOwnOutputs` deprecate to `inputs.filter` / `outputs.filter` with no replacement member.
- [x] `findContinuingOutputOrFail(ownInput, inline message)` = `outputs.findUniqueOrFail(_.address === ownInput.resolved.address, message)`.
      Scaladoc carries the AU-4 warning that the two credential-only finders leave staking open.
- [x] `valuePaidTo(addr)`, `valueSpentFrom(addr)`: whole-`Value` sums.
- [x] Deprecate `Utils.getAdaFromOutputs` / `getAdaFromInputs` (`:1161`, `:1179`) to the two sums.
- [x] Scaladoc warnings on `findOutputsByScriptHash` / `findOutputsByCredential` (payment credential
      only) and on `Address.fromScriptHash` / `fromPubKeyHash` (`stakingCredential = None`).
- [x] Test: continuing output found by full address; same credential, different staking → fails.

### Task 1.7: Authorization, time, double satisfaction

**Files:**
- Modify: `.../v3/Contexts.scala`

- [x] `isSignedByAny(keys)`: direct tail recursion over `signatories`; scaladoc marks it
      *conditional* on `List.exists` becoming intrinsic.
- [x] `validFromOrFail(inline message)`, `validToOrFail(inline message)`.
      Scaladoc states inclusive / exclusive. **Before writing "exclusive" unconditionally, check
      Conway's `transValidityInterval` for the `(None, Some(ttl))` single-sided case** (spec §4.7
      open line); if it differs, document the two-sided case only.
- [x] Deprecate `getValidityStartTime` (`:1102`) to `validFromOrFail`.
- [x] Soft-deprecate `IntervalBound.finite(default)` (`v1:130`): scaladoc warning only.
- [x] `hasPaidTagged(addr, value, tag: OutputDatum)`: whole address, whole value by `===`, datum
      tag. Test: value `>=` must NOT pass (VP-4).
- [x] `TxOutRef.deriveTokenName`: `blake2b_256(serialiseData(ref.toData))`. Test pins the bytes
      against an off-chain computation so the two sides can never drift.

### Task 1.8: `divCeil` / `divFloor`

**Files:**
- Modify: `.../prelude/Math.scala`

- [x] `Math.divFloor(a, b) = divideInteger(a, b)`, `Math.divCeil(a, b) = -divideInteger(-a, b)`, both `inline`
      (one builtin each, the `abs`/`min`/`max` precedent); `extension (self: BigInt) { inline infix def divCeil / divFloor }` delegating.
      Scaladoc warns that alphanumeric infix has the lowest precedence (`a divCeil n * fee` is `a divCeil (n * fee)`).
- [x] Property test over signed operands: `divFloor` ≤ exact ≤ `divCeil`, both agree with `/`
      when exact.
- [x] Migrate `LinearVestingValidator.scala:120`'s local `divCeil` in Phase 2.

### Task 1.9: `Eq[DCert]` / `Eq[ScriptPurpose]` fix

**Files:**
- Modify: `.../v1/Contexts.scala` (`:310-341`, `:743-760`)
- Test: `scalus-core/shared/src/test/scala/scalus/ledger/api/v1/` (the `Eq` suite)

- [x] Rename inner binders to `lhs`/`rhs` as the v3 instance already does.
- [x] Regression test: for every case, two values differing in one field compare `false`. The
      current suite has no such case, which is why the bug survived.

### Task 1.10: Close Phase 1

Note from execution: the deprecated `findOwnInputOrFail` keeps its original `Option`-based body,
so the budget pins of its 50 callers do not move until Phase 2 migrates each site to
`findInputOrFail` and re-measures it.

- [x] `sbtn generateLlmsApi`; commit `scalus-site/public/llms-api.txt` in the same change.
- [x] `sbtn mima` – expect clean (all changes additive or deprecations).
- [ ] `sbtn quick`, then `sbtn "clean; Test/compile"` once to surface deprecation warnings from
      in-repo callers of the deprecated members; migrate any inside `scalus-core` now, leave
      `scalus-examples` / `scalus-design-patterns` / `scalus-utxo-cell` for Phase 2.
- [x] CHANGELOG entry (`/changelog`) listing the operations, the renames and the deprecations.

---

## Phase 2 – Examples and design patterns

Exit criterion: no in-repo use of a deprecated member outside `scalus-core` tests; every
migrated validator re-measured on both compiler generations; `sbtn scalusExamplesJVM/clean test`
and `sbtn scalusDesignPatterns/test` green.

### Task 2.1: The `findOwn*` sweep

Usage across `scalus-examples`, `scalus-design-patterns`, `scalus-utxo-cell`, site and skills:
`findOwnInputOrFail` ×50, `findOwnOutputsByCredential` ×25, `findOwnInputsByCredential` ×16,
`findOwnScriptOutputs` ×9, `findOwnInput` ×8, `findOwnOutputs` ×3, `findOwnDatum` ×3,
`findOwnInputs` ×2.

- [x] Mechanical rename of the six that have a replacement member.
- [x] `findOwnInputs` / `findOwnOutputs` → `inputs.filter` / `outputs.filter` (5 sites).
- [x] While there, replace `findOwnOutputsByCredential(...)` used as a *continuing-output* check
      with `findContinuingOutputOrFail(ownInput, msg)` wherever the site compares against the own
      input's address. Sites that legitimately match on credential alone keep the credential finder
      and gain a one-line comment saying why.

### Task 2.2: Datum equality (8 sites, 4 spellings → `hasInlineDatum`)

- [x] `auction/Auction.scala:444`, `auction/UnfixedAuction.scala:298` (decode-typed).
- [x] `escrow/EscrowValidator.scala:103`, `cape/linearvesting/LinearVestingValidator.scala:115`
      (decode-to-`Data`).
- [x] `vesting/VestingValidator.scala:119`, `simpletransfer/SimpleTransferValidator.scala:78,97`
      (wrap form – already cheap, migrate for uniformity).
- [x] `cape/twopartyescrow/TwoPartyEscrowValidator.scala:154` (`toData ==` anti-idiom).
- [x] Record the before/after ExUnits of each in the commit message; these are the numbers the
      site page in Task 3.3 cites.

### Task 2.3: Time – `getValidityStartTime` (4 sites)

- [x] `vesting/VestingValidator.scala:68` → `validFromOrFail(msg)`.
- [x] `decentralizedidentity/DecentralizedIdentityValidator.scala:193`.
- [x] `cape/twopartyescrow/TwoPartyEscrowValidator.scala:83` – **this one fixes a real bug**
      (deposit time recorded as 0 when unbounded). Add a negative test: unbounded lower bound must
      fail, not deposit at epoch 0.
- [x] `scalus-utxo-cell/shared/.../OnChainCellOps.scala:96`.
- [x] `htlc/HtlcValidator.scala:57` `finite(0)` → `validFromOrFail`; `:62` → `validToOrFail`.

### Task 2.4: ADA-only sums (`getAdaFromOutputs` / `getAdaFromInputs`)

- [x] Grep and migrate every site to `valuePaidTo(addr)` / `valueSpentFrom(addr)`. Where the
      validator genuinely wants lovelace only, project with `.getLovelace` **after** the sum and
      add a comment; where it does not, the migration closes a token-stripping hazard (Escrow,
      Vesting per `01 §P17`) – add a negative test for each of those.

### Task 2.5: Minting sites → `hasOnly` / `tokens` / `singleOrFail` / `onlyBurnsUnder`

- [x] The three identical burn loops (`Auction:452-457`, `Crowdfunding:758-762`, `DID:231-235`)
      → `require(tx.onlyBurnsUnder(ownPolicy), msg)`.
- [x] Every hand-rolled "exactly one token minted under my policy" match → `tx.mint.hasOnly(...)`
      or `tx.mint.tokens(p).singleOrFail(msg)`.
- [x] The 26 `quantityOf(p, n) === BigInt(1)` / `> 0` sites → `hasNft(p, n)`; the two `> 0` sites
      get a review note, since `> 0` and `=== 1` differ for non-NFT tokens.
- [x] `filter(...).length === 1` / `.head` after `filter` → `findUniqueOrFail` / `singleOrFail`
      (the 15+ sites from `02 §C.10`).

### Task 2.6: Design patterns – Boolean callbacks → `=> Unit`

Execution note: done in place, not as overloads. `scalus-design-patterns` has MiMa disabled (no
baseline artifact yet), and overloads differing only in a lambda's result type are ambiguous for
`(_, _) => …` literals. CHANGELOG records it under Changed.

**Files:** `scalus-design-patterns/src/main/scala/scalus/patterns/UtxoIndexer.scala` (9
callbacks: `:28`, `:41`, `:60-61`, `:81`, `:104`, `:134`, `:152`, `:216`), `StakeValidator.scala`
(`:34`, `:60`), `TransactionLevelMinterValidator.scala` (`:29`, `:30`).

- [x] New overloads taking `=> Unit` callbacks; deprecate the Boolean ones. Preserve
      `multiOneToOneNoRedeemer`'s algorithm verbatim (`06 IX-2` endorses it).
- [x] Scaladoc: `StakeValidator.spendMinimal` gains the PU-1 warning; `oneToOne` / `oneToMany`
      gain the "solves `missed-input`, not double satisfaction" caveat.
- [x] Pattern sources use the renamed finders (they are among the `findOwnInputOrFail` sites).

### Task 2.7: Re-measure and pin

Execution note: every pin moved downward on 3.3.8 (e.g. HTLC CAPE 49 304/19 732 160 →
45 216/18 449 291 and 582 → 541 B; LinearVesting 72 239/33 350 416 → 67 851/32 019 547); on 3.8.4
only the three dual-baseline suites moved (AmmTest fees, NaivePaymentSplitter, Auction
end-with-winner, the last one upward by 2 203 mem). Re-pinned with a per-file log parser
(`repin.py`, scratch) rather than `update-budgets.py`, which rewrites by value across files.

- [x] `scalusExamplesJVM/clean`, then run the suites on 3.3.x and 3.8.x; update every pinned
      ExUnits by hand where the script does not cover it.
- [x] Keep a table of validator → (before, after) fee in the PR description. Task 3.3 needs it.

---

## Phase 3 – Site and LLM artifacts (`scalus-site`)

Exit criterion: no page shows a deprecated member; the security section is organized by the
taxonomy; `llms.txt` regenerated from content.

### Task 3.1: `security/common-vulnerabilities.mdx` → taxonomy structure

Today: ten numbered sections (dust, datum size, double satisfaction, staking, concurrency,
unauthorized transitions, oracles, infinite mint, parameterization, missing validation).

- [x] Reorganize by family – DS, VP, AU, MI, TI, DT, IX, PU, AR, RS, DE – using `06 §2`'s ranked
      order and `06 §7`'s fifteen mitigations as the outline. Every entry ends with the Scalus
      operation or idiom that closes it, with a code snippet from a migrated example.
- [x] Add the classes the page lacks today, in this priority: VP-1/VP-2 (`>=` and lovelace-only
      value checks – the two most frequent in-house bugs), MI-2 (one-shot seed not bound), TI-1
      (`getValidityStartTime` returns 0), DS-1 tagged-output defence, AU-4 staking on continuing
      outputs, EV-1 evaluation order, AR-1 rounding direction.
- [x] Keep the existing oracle and concurrency material; they map to DE-2 and RS-5.

### Task 3.2: New `security/safe-api-cheatsheet.mdx`

- [x] The spec §4.1 idioms table (already exists in the library) followed by the nineteen
      operations, one line each: signature, what it prevents, the one-liner it replaces.
- [x] The "fail-fast form of every lookup" index from spec §8.3: `getOrFail`, `inlineOrFail`,
      `finiteOrFail`, `singleOrFail`, `findUniqueOrFail`, `findInputOrFail`,
      `findContinuingOutputOrFail`, `scriptHashOrFail`, `validFromOrFail`.
- [x] Add to `security/_meta.js`.

### Task 3.3: Optimisation pages – the measured tables

- [x] `smart-contract-optimisations/measuring-performance.mdx` or a new
      `equality-and-lookups.mdx`: datum equality (286 vs 461 lovelace, with the UPLC explanation),
      `===` vs `toData ==` (identical, 901 mem / 1 653 665 cpu), `contains` vs `exists`
      (326 K / 565 K cpu `Option` tax), `count` vs `filter().length`, `SortedMap` vs `AssocMap`.
- [x] Cite the Phase 2 before/after table for the migrated validators.

### Task 3.4: Tutorial and reference pages

- [x] `smart-contracts/validators.mdx`, `in-depth-validator-look.mdx`, `htlc-tutorial.mdx`: use
      the new operations where the migrated example does.
- [x] `design-patterns/merkelized-validator.mdx:90`, `transaction-level-minting.mdx:62`,
      `withdraw-zero.mdx:65`: `findOwnInputOrFail` → `findInputOrFail`; the other four
      design-pattern pages follow their Phase 2 sources.
- [x] `security/datum-validation.mdx`: add the `hasInlineDatum` vs `inlineOrFail` rule and the
      wrong-tag note from spec §8.4.

### Task 3.5: LLM artifacts

- [x] `llms.txt` regenerates from content via `scalus-site/scripts/generate-llms.mjs` – run it and
      commit; check the new cheatsheet page is included.
- [x] `llms-api.txt` was regenerated in Task 1.10; confirm it lists the nineteen operations and
      marks the deprecated members.
- [x] `generate-llms-examples.mjs`: confirm the migrated examples are what it emits.

---

## Phase 4 – Skills (`scalus-skills/skills`)

Exit criterion: no skill recommends a deprecated member or the `toData ==` spelling; the
security-review skill's checklist carries the taxonomy IDs and looks for the twelve classes it
misses today.

### Task 4.1: `contract/SKILL.md` (90 lines)

- [x] **Line 47 recommends the anti-idiom**: "compare enums and case classes with
      `a.toData == b.toData`". Replace with: derive `Eq` and use `===` (identical UPLC, spec §8.3).
- [x] Add a "Safe API" rules block, one line each: `tx.mint.hasOnly(p, n, signedQty)` is the mint
      check; `out.hasInlineDatum(x)` for datum equality, `inlineOrFail` for fields;
      `findContinuingOutputOrFail` never a credential-only finder for the continuing output;
      `validFromOrFail` never `getValidityStartTime`; `singleOrFail` /
      `findUniqueOrFail` never `.head` after `filter`; `valuePaidTo` never `getAdaFromOutputs`.
- [x] Point at the cheatsheet page URL once Task 3.2 is live.

### Task 4.2: `smart-contract-security-review/SKILL.md` (259 lines) and `references/vulnerabilities.md` (1 578 lines)

- [x] Add the `06 §9` taxonomy ID to every V001–V025 row and the operation that fixes it.
- [x] Re-title V004 "Integer Overflow": on-chain `Integer` is unbounded; the real class is
      rounding direction (AR-1) → `divCeil` / `divFloor`.
- [x] Add the twelve missing classes: VP-1, VP-2, VP-5, VP-6, MI-2, IX-2, EV-1, AU-7, PU-3, PU-4,
      DE-4, RS-7. Four are Critical and two have occurred in-house.
- [x] V010: state the plugin's default-`fail` for unimplemented purposes as the *reason* it is a
      false positive, not as an unexplained rule.
- [x] V013: add the `getValidityStartTime`-returns-0 case with the two in-corpus bugs.
- [x] V014: signature ≠ authorization when the authority is a script.
- [x] The skill's own worked example (`SKILL.md:108`, `:127`; `vulnerabilities.md:686`, `:718`,
      `:728`) uses `getAdaFromInputs` / `getAdaFromOutputs`. Rewrite it on `valueSpentFrom` /
      `valuePaidTo`, and make the ADA-only sum the *finding* rather than the fix.

### Task 4.3: `optimize-contract/SKILL.md` (298 lines) and `references/patterns.md` (523 lines)

- [x] **O016 and O020 are stale**: they advise `equalsData` over typed `===`. `===` already lowers
      to `equalsData`. Replace with the `07 I9` advice: make key types concrete to get
      `equalsInteger` (1 761 779 vs 832 313 cpu behind a type variable).
- [x] New rules with numbers: datum equality (`hasInlineDatum` 286 vs 461), `contains` over
      `exists`, `count` over `filter().length`, `isEmpty` over `size == 0`, `SortedMap` over
      `AssocMap`, `findUniqueOrFail` over `filter` + `length`.

### Task 4.4: `contract-test/SKILL.md` (76 lines)

- [x] Add the negative-test convention Phase 2 introduces: every safety operation gets a test
      that the *unsafe* input fails (unbounded validity, `>=` value, wrong staking credential,
      second matching output).

### Task 4.5: Verify

- [ ] **Deferred to the next release.** The eval scaffolds from `validator.g8`, which pins the released
      Scalus (1.1.1); the operations the updated skills recommend do not exist there, so the generated
      validator cannot compile until a release ships them. Re-run the local smoke eval used for the LLM DX work (3 prompts) against the updated skills;
      expect the generated validator to use `hasOnly`, `hasInlineDatum` and
      `findContinuingOutputOrFail` unprompted.

---

## Out of scope, recorded

- `scalus3/*.g8` templates and the starter repository: follow the next release.
- The lowering improvements in spec §8.4 (`exists` intrinsic, `Some(x).getOrFail` fold,
  `d.to[T] === x` direct compare). Each would delete or cheapen operations here; tracked
  separately.
- The script-context map key-order blocker (spec §9) and the operations it gates.
- CIP-168 `assetCount` (spec §4.10): a candidate, not an operation.
