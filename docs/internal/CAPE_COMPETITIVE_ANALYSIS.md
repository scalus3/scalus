# CAPE competitive standings at Scalus 1.1.0

Date: 2026-08-24 (Task 12d final consistency sweep – see below). Scalus version: 1.1.0. Worktree
commit: `5d28bd0fc` (Task 12d regenerated all 8 submissions from a fully clean slate at this commit –
deleted every `submissions/*/Scalus_1.1.0_nau` dir in the clone, re-ran `GenerateSubmissions`, then
re-verified and re-measured all 8 end to end via `scripts/cape-submit.sh`). Every measured CPU/mem/size
figure below reproduced byte-for-byte against the prior Task 12a/12b/12c snapshot (captured at commit
`a23452c4d`, before two doc-only commits and one script-fix commit landed) – confirming the standings are
stable and not an artifact of a particular build. Clone: `/Users/nau/projects/lantr/UPLC-CAPE`, main at
`276738c3`.

**Note on `source_commit_hash`**: `5d28bd0fc` is a commit on the `worktree-cape-submissions` branch, not
yet on `origin/master` – it will not resolve on GitHub until the branch merges. See the Submission
checklist at the end of this document for the regenerate-from-merged-master step this implies.

**Update, 2026-08-25 (Task 13)**: fixed a real correctness bug in `factorial` (open) – it silently
returned `0` for `x >= 13` instead of computing the true factorial (see the dedicated note under
"Per-scenario standings" below and `FactorialOpen.scala`). The fix also changed which of two table
encodings is adopted, moving `factorial` onto CAPE's preview track (all 8 scenarios are now gated). The
`factorial (open)` row in the standings table below, and this document's `factorial`-related prose, are
updated to Task 13's numbers; every other row is unchanged from the Task 12d snapshot.

**Update, 2026-08-25 (Task 14)**: applied the same fix to `fibonacci` (open) – it was correct on the
fixture but errored (`CaseIndexOutOfBounds`, not a wrong answer) for every `n > 25`, since
case-on-builtin-integer has no entry past the table. Added a linear accumulator fallback (`go(k, a, b)
= if k <= 0 then b else go(k - 1, b, a + b)`), so the program is now correct for every integer input
(see the dedicated note under "Per-scenario standings" below and `FibonacciOpen.scala`). No gate change
– `fibonacci` was already on CAPE's preview track. The `fibonacci (open)` row in the standings table
below, and this document's `fibonacci`-related prose, are updated to Task 14's numbers; every other row
is unchanged from the Task 13 snapshot.

**Update, 2026-08-25 (canonical Value ordering fix + workaround revert)**: root-caused and fixed the
upstream fixture-builder bug that forced `linear_vesting`'s interim `valueBuiltins = false` build
(non-canonical `Value` key order; the fix + regenerated metrics for all 11 committed `linear_vesting`
submissions sit on the clone's `fix/canonical-value-ordering` branch, PR body in
`docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md`), then reverted the workaround. `linear_vesting`
now runs the default CIP-153 build and **leads its leaderboard on total fee by 17.1%** (was: 2nd,
13.7% behind). The `linear_vesting` row, its dedicated section, and its comparator block below are
updated; other rows are unchanged from the Task 14 snapshot (their measured numbers are unaffected -
only `linear_vesting` fixtures use multi-asset values).

**Update, 2026-08-25 (`linear_vesting` house-style refactor)**: rewrote `LinearVestingValidator` to
match the house style of `VestingValidator`/`HtlcValidator` - named `inline val` error messages,
`extends Validator`, fuller scaladoc, and `===` (structural `Eq`) in place of `.toData ==` throughout.
Measured each candidate idiom in isolation before adopting it (see the validator's own top-level
scaladoc and its per-helper doc comments for the numbers): named error constants, `extends Validator`,
`===` swaps, and building the interval helper on `IntervalBound.finiteOrFail` were all cost-neutral or
better; swapping the hand-rolled own-input/continuing-output/signature-check helpers for the
equivalent `TxInfo.findOwnInputOrFail`/`findOwnInputsByCredential`/`findOwnOutputsByCredential`/
`isSignedBy` prelude calls regressed budget (confirmed via a `FullUnlock`-path control case that never
touches the swapped code, ruling out a comparison-cost explanation in favor of fixed shared-library
call-chain overhead) and was rejected, keeping the hand-rolled shape with `===` comparisons instead.
Net effect: script 718 -> 714 bytes (all 6 measurement ExUnits pins unchanged bit-for-bit), total fee
41,646 -> 41,586 lovelace (-0.14%, all from the smaller reference-script fee), **leading by 17.2%**
(was 17.1%). All 29/29 `linear_vesting` CAPE fixtures still pass, each for its original reason.

**Update, 2026-08-25 (follow-up: three findings re-examined)**: three of the four rejections above
were re-investigated with sub-agent research rather than left as a bare cost measurement, changing
two of them:

- **`findOwnInputOrFail` adopted anyway.** Its own-input-lookup regression (+1,636 mem / +93,865
  steps on `PartialUnlock`, +300 mem / +48,000 steps on `FullUnlock`) is small next to the
  leaderboard margin, so it's now used regardless of cost - it's the canonical idiom and the
  numbers don't threaten 1st place. The double-satisfaction uniqueness guard stays a separate
  hand-rolled `requireSingleScriptInput` (a `findOwnInputsByCredential`-based version would still
  materialize a list where counting in place doesn't).
- **`finiteLowerBound`'s exclusive-bound `+1` branch was dead code - removed.** Traced
  `scalus.cardano.ledger.LedgerToPlutusTranslation.getInterval`: every finite-lower-bound case
  calls `IntervalBound.finiteInclusive`, never `finiteExclusive`; there is no ledger code path that
  ever produces a finite *exclusive* lower bound (unlike the upper bound, whose closure genuinely
  varies by protocol version, Conway vs. pre-Conway). CAPE's own fixture builder can't express an
  exclusive lower bound either (`CapeTestSuite.scala`'s `bound` helper hardcodes `true`). The
  validator now calls `range.from.finiteOrFail(...)` directly with no wrapper, which is both
  simpler and cheaper (one fewer branch).
- **`findContinuingOutput`'s first-match behavior confirmed correct, not a workaround - no
  change.** A `PartialUnlock` tx is already capped to one script input (`requireSingleScriptInput`)
  signed by that input's own beneficiary (`isSignedBy`/`requireSignedBy`), so nobody but the signer
  could be harmed by an unvalidated second same-credential output in their own transaction; unlike
  `VestingValidator`'s `NotExactlyOneContractOutput` gate (which exists because it checks a full
  `Value` equality that only makes sense against a single output), `linear_vesting` only ever
  checks `quantityOf(asset)`, so that rationale doesn't transfer.
- **`isSignedBy`'s regression mechanism identified, not just measured.** `PubKeyHash` is
  `@UplcRepr(ProductCaseOneElement)` (native representation), so `List.contains` already dispatches
  to the same short-circuiting native-list scan the hand-rolled version uses - ruling out "extra
  `Option` allocation" or "generic `find`+`isDefined` composition" as the cause. The real mechanism,
  traced through `SIRLinker.link`: any call to an external, non-inline `def` makes the linker wrap
  the *entire* compiled program in its own `Let` + self-application-fixpoint layer, a fixed
  per-invocation linking cost paid on every code path regardless of which branch actually calls it
  - the same root cause as `findOwnInputOrFail`'s regression above, now with a source-level
  citation instead of just an empirical delta. `requireSignedBy` stays hand-written.

Net effect of this round: script 714 -> 669 bytes, `PartialUnlock` `ExUnits(72135, 33771060) ->
ExUnits(71843, 33245629)`, `FullUnlock` `ExUnits(25950, 9699042) -> ExUnits(24322, 9127746)`, total
fee 41,586 -> **40,422 lovelace**, **leading by 19.5%** (was 17.2%). All 29/29 fixtures still pass.

**Update, 2026-08-25 (canonical helpers adopted; the library was fixed instead)**: the remaining
hand-rolled helpers were replaced by the prelude idioms after fixing the library/compiler cost that
had justified them (user directive: improve the library, never keep local copies):

- **Root cause of the `isSignedBy` regression found and fixed in the compiler.** The previous
  block's `SIRLinker` explanation was half-right: only *genuinely recursive* defs get a
  self-application fixpoint (`RemoveRecursivity` already strips the flag from non-recursive ones),
  and the optimizer already localizes *single-use* defs into the branch that uses them. The real
  cost was an intrinsic-dispatch miss: `TxInfo.signatories` (like every lazily-decoded ledger
  list) is in `PackedSumDataList` representation, and `IntrinsicResolver.representationNames` gave
  that repr a name with no registry entry, so `List.contains` silently lowered the generic
  `find(_ === elem).isDefined` prelude body (a data-encoded `Some` allocated per step, plus a dead
  `Eq` closure) instead of the `equalsData`-scan intrinsic. Fixed by letting `PackedSumDataList`
  fall back to the `BuiltinList` provider name, scoped to the structural-equality methods
  (`contains`/`indexOf`/`deleteFirst`/`distinct`/`diff`); a broader version that also routed
  `head`/`tail`/`isEmpty`/`at`/`drop` was tried and backed out (it mislabeled element
  representations - a runtime failure in MembershipToken - and regressed degenerate micro cases,
  despite cutting every realistic validator 2-13%; re-enabling the spine ops once the labeling
  bug is fixed is the recorded follow-up). The prelude `List.contains`/`List.count` bodies stay
  as canonical compositions by explicit user direction - intrinsics own the performance. Details
  in `docs/internal/CODEGEN_IMPROVEMENT_PLAN.md` (T17).
- **`findOwnInputOrFail` (already `inline`) now expands to a local fail-in-place loop** instead of
  `findOwnInput(outRef).getOrFail`: no `Option` round-trip and no shared top-level `find` binding
  (whose fixpoint construction every path paid at program start; `find` has no packed-list
  intrinsic). New `TxInfo.findOwnOutputByCredential` (first match, `Option`) and
  `findOwnOutputByCredentialOrFail` (inline local loop) cover the continuing-output lookup;
  adopting the latter removed the last `find` user from this validator.
- **Validator now fully canonical**: `requireSignedBy` -> `txInfo.isSignedBy`,
  `findContinuingOutput` -> `txInfo.findOwnOutputByCredentialOrFail`, `requireSingleScriptInput`
  -> `txInfo.inputs.count(_.resolved.address.credential === ownCred) === BigInt(1)`, datum match
  -> `continuing.datum.inlineOrFail[Data]` (user-requested). The hand-rolled helpers are deleted.
  Measured per swap (JVM harness): the output-lookup swap cut PartialUnlock by 3,192 mem /
  966,138 steps and FullUnlock by 300 mem / 48,000 steps (top-level `find` gone); the count and
  `inlineOrFail` swaps were bit-neutral against the hand-rolled forms they replaced at the time;
  reverting `count` to the canonical `foldLeft` composition then cost +1,096 mem / +216,787 steps
  on PartialUnlock vs a hand-rolled accumulator loop (~79 lovelace/eval, accepted by design - a
  packed-list `foldLeft` intrinsic is the follow-up if it ever matters); the intrinsic `contains`
  beats even a tuned prelude scan (-200 mem / -32,000 steps on FullUnlock).
- Also tested and rejected: `inline` on `isSignedBy` itself is bit-neutral (the optimizer already
  localizes the single-use wrapper), so it stays a plain `def`.

Net effect: script 669 -> 612 bytes, `PartialUnlock` `ExUnits(71843, 33245629) ->
ExUnits(64359, 31005409)` (-10.4% mem, -6.7% steps), `FullUnlock` `ExUnits(24322, 9127746) ->
ExUnits(23422, 8983746)`, total fee 40,422 -> **37,069 lovelace**, **leading by 26.2%**. All 29/29
fixtures still pass, each for its original reason.

**Update, 2026-08-26 (compiler fix under review on its own branch; Contexts helpers rejected)**:
the library/compiler changes that the previous block's numbers depended on are deliberately NOT on
this branch:

- **The `IntrinsicResolver` packed-list Eq-dispatch fix lives on its own branch**,
  `feat/intrinsic-eq-packed-list` (commit `16ff07722`, based on current master), pending review as
  scalus3/scalus#351. It is a genuine compiler fix, but it belongs in its own reviewable PR, not
  inside a CAPE submissions branch. Measured against master's own baselines it improves 73 of 74 moved pins
  (the one mixed move is Escrow's Refund path: mem -831, steps +1,222,625, ~ +40 lovelace).
  Once it merges to master and this branch rebases, the linear_vesting numbers below recover.
- **The `Contexts.scala` changes are rejected outright** (the `inline` local-loop
  `findOwnInputOrFail` reimplementation and the new
  `findOwnOutputByCredential`/`findOwnOutputByCredentialOrFail` helpers): they hand-roll loops
  inside the library to dodge linker overhead - a workaround at a different layer, not a fix. The
  right remedies are compiler-level (T17 spine-op dispatch, T17 let-sinking of multi-use
  top-level bindings). `Contexts.scala` is back to its master state.
- **The validator keeps the canonical idioms** with the plain library: `findOwnInputOrFail`
  (the standard `findOwnInput(outRef).getOrFail` form), `isSignedBy`, `List.count`, and a plain
  `txInfo.outputs.find(_.address.credential === cred).getOrFail(...)` for the first continuing
  output (`findOwnOutputByCredentialOrFail` has no library replacement). No hand-rolled helpers
  were reintroduced - the cost gap is compiler work, not validator work.

Cost of leaving the compiler fix off this branch (measured, accepted): script 661 bytes rather than
612, `PartialUnlock` `ExUnits(75067, 34043412)` rather than `ExUnits(64359, 31005409)`, `FullUnlock`
`ExUnits(26850, 9772742)` rather than `ExUnits(23422, 8983746)`, total fee **41,661 lovelace** rather
than 37,069 - still **1st on total fee, leading by 17.1%** over `Plinth_1.65.0.0_Unisay_preview`
(50,234). Rebasing onto master once #351 merges recovers the difference. All 29/29 fixtures pass.
Only `linear_vesting` changed: the other 7 regenerated scenarios are byte-stable against their
committed metrics.

**Update, 2026-08-26 (`htlc` gets the same canonical treatment; it costs us the memory crown)**:
`HtlcValidator` now `extends Validator` and uses `findOwnInputOrFail`, `isSignedBy`, `List.count`
and `Interval.isEntirelyBefore`/`isEntirelyAfter`. The hand-navigated `ScriptContext` entry point
(`unConstrData` + `dropList(offsetOf[TxInfo](_.validRange), ...)`) and the local
`finiteUpperBound`/`finiteLowerBound`/`requireSingleScriptInput`/`findOwnInput`/`requireSignedBy`
helpers are gone; only `pkhOf` remains, because no prelude equivalent exists.

The two `Interval` helpers are a strict correctness upgrade, not just a shortening: each compares
against the far end of the range (so a transaction cannot straddle the timeout), each accounts for
the bound's closure - which matters, since `LedgerToPlutusTranslation.getInterval` emits finite
*exclusive* upper bounds above protocol version 8 - and each returns `false` on an unbounded range,
which is what rejects `claim_infinite_upper_bound` / `refund_infinite_lower_bound`. All 25 fixtures
(4 measurements + 21 negative checks) still pass, each for its original reason. Note the CAPE
fixture builder only ever emits *inclusive* bounds, so the closure branches are exercised by the
ledger, not by the benchmark.

Unlike `linear_vesting`, this one is a measured **regression**, and it is large enough to matter.
Four builds, JVM harness, per-invocation `ExUnits` (claim / refund):

| Build | Size | Claim | Refund |
|---|---:|---|---|
| hand-rolled + raw-`Data` entry (was shipped) | 571 | `(45272, 18491569)` | `(42139, 17072747)` |
| prelude helpers + raw-`Data` entry | 606 | `(52292, 20178759)` | `(49159, 18759937)` |
| prelude helpers + `Validator` (**shipped**) | 597 | `(52732, 20521156)` | `(49799, 19134334)` |
| ... plus #351 cherry-picked | 582 | `(49304, 19732160)` | `(46371, 18345338)` |

Attribution is unambiguous: dropping the hand-navigated entry point costs only **440 mem** per
invocation and *saves* 9 bytes, so the lazy-`TxInfo` trick has stopped paying for itself. The other
**+7,020 mem** is the helper swap, and roughly half of it (**3,428 mem**, 15 bytes) is the same
`PackedSumDataList` intrinsic-dispatch miss that #351 fixes. Of the residue, only ~600 mem is the
two `Interval` helpers being non-`inline` (measured by marking them `inline`: 49,304 -> 48,704 mem,
582 -> 580 bytes - a real but minor prelude improvement, left as a follow-up because it moves pinned
budgets repo-wide). The rest is `findOwnInput`'s `Option` round-trip plus the closure and top-level
binding behind `List.count` - i.e. the same T17 compiler work `linear_vesting` already documents.

Net effect on the leaderboard: total fee 23,844 -> **26,569 lovelace**, still 2nd but now **11.5%**
behind `Plinth_1.65.0.0_Unisay_preview` (23,830) rather than 0.1%, and memory 174,822 -> 205,062,
which drops us from **mem #1 to mem #2** in a 15-entry field (Plinth's preview build is 183,704).
With #351 merged and rebased this recovers to 191,350 mem / 76,154,996 cpu / 582 bytes - roughly
25,400 lovelace, ~6.5% behind, and still mem #2. Closing the rest is compiler work, not validator
work; reverting to the hand-rolled shape is the only way to get the memory crown back today, and
that is exactly the trade the "improve the library, never keep local copies" directive rejects.

All 8 scenarios verify and measure green in the clone
(`submissions/<scenario>/Scalus_1.1.0_nau/{metadata,metrics}.json`, schema-validated). This snapshot
follows three fix rounds after the initial dry run found real bugs:

- `90a69066a` - `TermSanitizer` emitted variable names (`a-91533'653`) that the upstream reference UPLC
  parser rejects; fixed to emit reference-parser-safe names, and the driver now wraps CAPE CLI calls in
  `nix develop`.
- `664d7a6f2` - added a `min_plutus_version` gate so the 6 `@Compile`'d scenarios route to CAPE's
  preview evaluator (see below).
- `a14119f98` - interim `valueBuiltins = false` build for `linear_vesting` (reverted 2026-08-25 after
  the upstream fixture fix was prepared; see below).
- **(Task 12d, new)** – `scripts/cape-submit.sh`'s preview-measurement step piped
  `nix develop --command jq ... "$tmp_metrics" > "$d/metrics.json"` directly to the final `metrics.json`
  path. `nix develop` prints its dev-shell welcome banner ("🚀 UPLC-CAPE Development Shell" + "Synchronizing
  Cabal package index...") to the same stdout stream *before* running the wrapped command, and the `>`
  redirect captured that banner text ahead of `jq`'s real JSON output – silently corrupting every gated
  scenario's `metrics.json` with a non-JSON prefix. `cape submission verify`'s own schema check caught it
  ("Several files failed to parse") on the very first gated scenario (`ecd`) of this task's end-to-end run.
  Fixed by piping the `jq` output through `sed -n '/^{/,$p'` before the redirect, to drop everything before
  jq's first standalone `{` line. Re-ran the full driver after the fix: all 7 gated scenarios' `metrics.json`
  now start with `{` and pass schema-verify. The bug is fail-loud, never fail-silent: a banner-corrupted
  `metrics.json` is not valid JSON, so it is caught either by `check-jsonschema` (as it was here) or by
  `CompareWithLeaderboard.scala`'s own `ujson.read` (line 34), which throws on malformed input rather than
  producing a wrong number. So no previously-reported standings figure in this document could have been
  silently wrong because of it – either a run hit the corruption and visibly failed (requiring a re-run,
  as happened in this task), or it didn't, in which case the numbers are exactly as measured. Whether this
  is a long-standing intermittent bug (e.g. depending on whether Nix's Cabal-index sync has already run in
  the session) or a new regression was not root-caused; it reproduced on the very first gated scenario of
  this task's first end-to-end run and did not reproduce at all after the fix.

## Mainnet PV11 vs CAPE tooling lag

Scalus's `Options.release` targets Plutus V3 protocol version 11 (vanRossem): flexible case-on-builtins
and CIP-153 `Value` builtins. CAPE's pinned *production* evaluator is `plutus-core-1.45.0.0`, which
predates both features. Any Scalus scenario using them fails to even **parse** on the production
evaluator - not an evaluation mismatch, a missing-builtin/grammar gap in an older toolchain.

Fix: `metadata.json`'s `compilation_config.min_plutus_version` is set to `"1.60.0.0"` for every
`@Compile`'d scenario (`factorial_naive_recursion`, `fibonacci_naive_recursion`, `ecd`, `htlc`,
`linear_vesting`, `two_party_escrow`), plus both hand-crafted open-mode scenarios (`fibonacci` since Task
12a, `factorial` since Task 13) whose lookup tables use PV11 case-on-builtins directly. CAPE's own CLI
treats that as a request to route the submission to its **preview** evaluator track
(`cabal.project.preview`: `plutus-core ^>=1.65`) instead of refusing it outright. As of Task 13, all 8
Scalus scenarios are gated - `factorial` (open) was the last holdout (see its dedicated note below for why
it moved).

Practical effect: **all 8 of our rows sit in CAPE's preview report, not its production report**, until
upstream promotes its production evaluator past vanRossem. This is not a Scalus limitation - it's true
of every submission (any language) using post-1.45.0.0 Plutus Core features; several `_preview`-suffixed
submissions from other contributors are in the same position, which is also why most current
leaderboard leaders are themselves `_preview` entries (see per-scenario table below).

## linear_vesting: canonical Value ordering (RESOLVED 2026-08-25)

`linear_vesting` calls `unValueData` on the input's and output's `Value` (via `Value.quantityOf`).
CAPE's upstream Haskell test-fixture builder (`buildValue` in `lib/Cape/Tests.hs`, not
`ScriptContextBuilder.hs` as earlier notes said) folded lovelace and assets with `foldl' (<>)` over
the Data-backed `PlutusLedgerApi.Data.V3.Value`, whose Semigroup (`PlutusTx.Data.AssocMap.union`)
appends the left map's unmatched keys *after* the right map's entries. Any `{lovelace, assets}`
fixture value therefore carried the custom asset's currency symbol **before** ADA's empty one - the
reverse of CIP-153 canonical order (the empty bytestring is always lexicographically smallest). The
real `unValueData` builtin (plutus-core >= 1.65 `PlutusCore.Value.buildValueWith`, mirrored by
Scalus's `scalus-core/shared/src/main/scala/scalus/uplc/eval/BuiltinValueOps.scala:200-204`) rejects
non-canonical input outright, so the default CIP-153-builtin-backed lowering failed on 4 of
`linear_vesting`'s 29 CAPE tests - a fixture-builder bug, not a Scalus bug.

**Resolution**: the builder fix + regenerated metrics for all 11 committed `linear_vesting`
submissions are prepared on the `fix/canonical-value-ordering` branch of the local UPLC-CAPE clone;
the PR body (with root cause, byte-level evidence, and the full blast-radius table) is
`docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md`. The interim Scalus workaround
(`Options.releaseUntagged.copy(valueBuiltins = false)`, commit `a14119f98`) is **reverted**:
`LinearVestingContract` is back on plain `Options.releaseUntagged`, so `Value.quantityOf` lowers to
the CIP-153 `lookupCoin`/`unValueData` builtins again. Effect on our submission (measured by the
fixed preview builder, plutus-core 1.65): script 902 -> 718 bytes, summed measurement CPU
184,252,980 -> 155,791,716 (-15.4%), mem 499,864 -> 340,440 (-31.9%), total fee 55,657 -> 41,646
lovelace (-25.2%). All 29/29 CAPE tests pass (the 4 previously failing `partial_unlock_*` tests now
succeed against canonically ordered fixture values). This moved us from 2nd (13.7% behind
`Plinth_1.65.0.0_Unisay_preview` on total fee) to **1st, leading by 17.1%** - see the updated
standings below. Note the other submissions' re-measured numbers moved *up* 2-8% (the canonical
order costs map-walking validators one extra step to reach the vesting policy), which is part of why
the gap swung so far.

**Benchmark-only note**: `linear_vesting`'s validator resolves the continuing output by first match, not
uniqueness (`LinearVestingValidator.scala:141-149`) - the CAPE fixtures build measurement cases by
patching an extra output onto a baseline that already carries one at the same credential, and no fixture
exercises a "multiple continuing outputs" attack. This matches fixture behavior, not the stricter
"unique output" reading of the spec text; treat this scenario's cost figures as benchmark-only, not a
production-ready validator pattern.

## Per-scenario standings

CPU/mem in cost units (steps/8-byte words), summed over `included_in_aggregates` evaluations, from each
submission's `metrics.json`. Size in bytes. Delta is ours vs the row-1 leader; negative means we're
smaller/cheaper. "New" means Scalus had no submission for that scenario before 1.1.0.

| Scenario | Leader | Leader CPU | Ours CPU | ΔCPU | Ours mem | Δmem | Ours size | Δsize | Verdict |
|---|---|---:|---:|---:|---:|---:|---:|---:|---|
| ecd | Plinth_1.65.0.0_Unisay_preview | 18,747,572 | 24,747,572 | +32.0% | 108,896 (+52.5%) | | 53 (-89.4%) | | behind, analyzed (task 12c) |
| factorial (open) | Plutarch_1.11.0_SeungheonOh_exbudget | 37,001,975 | 4,545,903 | -87.7% | 24,219 (-82.4%) | | 91 (+127.5%) | | **leads** (now gated, see note) |
| factorial_naive_recursion | Plinth_1.65.0.0_Unisay_preview | 28,019,280 | 28,019,280 | 0.0% | 107,235 (0.0%) | | 34 (0.0%) | | tied |
| fibonacci (open) | Scalus_0.18.2_Unisay_preview | 14,567,412 | 3,961,677 | -72.8% | 20,121 (-15.7%) | | 156 (+231.9%) | | **leads** (now correct for all inputs) |
| fibonacci_naive_recursion | Scalus_0.18.2_Unisay_preview | 115,413,721,337 | 129,093,086,599 | +11.9% | 481,520,743 (+1.0%) | | 42 (-31.1%) | | tied w/ compliant leader (task 12c) |
| htlc | Plinth_1.65.0.0_Unisay_preview | 60,542,648 | 80,183,908 | +32.4% | 205,062 (+11.6%) | | 597 (+1.0%) | | behind on total fee (+11.5%); regressed by the prelude-helper refactor, see note |
| linear_vesting | Plinth_1.65.0.0_Unisay_preview | 143,737,440 | 157,028,524 | +9.2% | 353,968 (-22.9%) | | 661 (-25.9%) | | **leads** on total fee (-17.1%; CPU +9.2%; intrinsic fix pending review recovers ~4,600 lovelace) |
| two_party_escrow | Plinth_1.65.0.0_Unisay_preview | 168,065,744 | 197,737,600 | +17.7% | 483,057 (-9.9%) | | 1,174 (-10.4%) | | behind, tuned (task 12b) |

Notes on the table:

- **ecd - loss analysis (Task 12c, 2026-08-24)**: first-ever Scalus submission for this scenario
  (task 2 of this effort added it); this is the *fixed*-algorithm scenario (only compiler options
  may change it - `scenarios/ecd/ecd.md` prescribes `ecd a b | b == 0 = abs a | otherwise = ecd b (a
  \`mod\` b)` verbatim and forbids any deviation, e.g. no Stein's algorithm). Behind by 32.0% CPU but
  smallest script by far (53 vs 502 bytes).
  - **Profile diff** (14 measurement cases, `Term.evaluateDebug`'s per-`ExBudgetCategory` cost map,
    summed, both sides run on our own CEK): ours 24,747,572 vs. the leader
    `Plinth_1.65.0.0_Unisay_preview`'s `ecd.uplc` at 18,747,572 - exactly the 32.0% from the
    standings table. Every *builtin* cost is bit-for-bit identical between the two sides:
    `EqualsInteger` 2,459,651, `LessThanInteger` 634,060, `SubtractInteger` 101,208 on both; our
    `RemainderInteger` and the leader's `ModInteger` cost the *same* 4,367,253 despite the different
    builtin choice (same cost-model size class for these inputs). The entire 6,000,000 CPU gap is
    machine steps: `Var` +2,704,000, `LamAbs` +1,648,000, `Apply` +592,000, `Case` +528,000, `Constr`
    +528,000 (the leader has **zero** `Constr` steps across all 14 cases). 100% algorithm-neutral,
    100% recursion-encoding overhead - no builtin-choice or algorithmic difference contributes at all.
  - **Structural comparison** (both `.uplc` parsed with `UplcParser().parseProgram` and reprinted
    with Scalus's own pretty-printer for an apples-to-apples read): our compiled `EcdContract.program`
    is T2's self-application fixpoint - `case (constr 0 self b (remainderInteger a b)) [self]` per
    recursive call, the `CaseConstrApply`-encoded 3-arg self-call, paying one
    `Constr`+`Case`+2x`Var`+2x`Apply`+1x`LamAbs` per iteration for the self-reference plumbing. The
    leader's `ecd.uplc` has **no fixpoint combinator or self-application at all** for any of the 14
    test cases: it is a plain chain of 15 nested `case (equalsInteger 0 b_i) [(lam b_{i+1}
    <continue>)] <base case>` lets - i.e. the recursion is *statically unrolled to depth 15*, falling
    back to a genuine self-application fixpoint only beyond that depth (visible once at the very
    bottom of the file) - never reached by any of the 14 CAPE inputs, whose GCD depth is at most ~4.
    Fetched the actual submission source (`Unisay/plinth-cape-submissions@3efc19a`, `lib/Ecd.hs`): a
    plain 2-line `{-# INLINEABLE ecd #-}` recursive definition, no manual `unroll`/`peel` helper, no
    hand-written UPLC - compiled with one extra GHC plugin flag, `{-# OPTIONS_GHC -fplugin-opt
    Plinth.Plugin:inline-unconditional-growth=45 #-}`. Raising Plinth's unconditional-inlining
    AST-growth budget lets its inliner self-recursively inline `ecd` into its own call site
    repeatedly until the cumulative growth exceeds 45 nodes (~3 nodes/level x 15 levels matches the
    observed unroll depth exactly), then stops and falls back to real recursion.
  - **Classification**: **(d) recursion/encoding overhead** - this is exactly the "size-budgeted
    callsite inlining" + "`peel`/`unroll`-style bounded recursion unrolling" that
    `CODEGEN_IMPROVEMENT_PLAN.md` T15 already names as open work, and is **confirmed absent** in
    Scalus (plan section 4.7: no such pass exists today). Evidence line added to T15.
  - **Compiler-option experiment (negative result)**: tried `Options.releaseUntagged.copy(cceEnabled
    = true)`, `.copy(cseIterations = 4)`, and `.copy(cseIterations = 0)` against `EcdContract.program`
    - all three produced byte-identical script (53 bytes) and cpu (24,747,572) to the baseline. A
    single, non-repeated top-level recursive function gives CSE/CCE nothing to share; no existing
    `Options` knob touches recursion encoding. No option change adopted; this confirms the gap is
    new-pass work (T15), not a mistuned existing pass - stopping here per the task's fixed-mode
    scope (only compiler options may change, and none win).
  - **Expected gain if T15 landed**: up to the full 32.0% - the leader shows ~0 residual recursion
    overhead once unrolled for every in-scope `ecd` input; a bounded self-recursive unrolling pass
    with a comparable growth budget would let Scalus match it on this scenario.
- **factorial_naive_recursion**: genuinely tied with the current leader (Plinth preview) on CPU, mem,
  *and* size - this is a fixed-algorithm scenario (only compiler options may change it), and we're
  already at parity with the best-known compiled output.
- **factorial (open) - table rewrite (Task 12a, 2026-08-24)**: `FactorialOpen.scala` was rewritten from a
  self-application-fixpoint recursion to an O(1) lookup table: `0!..12!` packed as 4-byte big-endian
  entries in a constant `ByteString` (the scenario's 10 measurement cases only exercise `0 <= x <= 12`),
  decoded via `sliceByteString`/`byteStringToInteger` at the offset `x*4`; `x <= 0` (including the
  negative-input case) returns the constant `1` directly, matching the fixture's documented semantics.
  Uses only PV9-compatible builtins (`force(ifThenElse)`, no case-on-builtins) - **no `min_plutus_version`
  gate needed**, so this stays on CAPE's current/production evaluator track, directly comparable to the
  `Plutarch_..._exbudget` target with no track caveat. Went from **+1.3% behind** to **-60.2% ahead**
  (37,481,975 -> 14,714,596 CPU, clone-measured). A separate, smaller fix was tried and folded in along
  the way: before adopting the table, a self-application-combinator tweak (`(λr.f(r r)) $ (λr.f(r r))`
  instead of `(λr.r r) $ (λr.f(r r))` - i.e. skip the redundant "identity self-applier" wrapper Apply/Lam
  pair) closed the entire +1.3%/480K-CPU gap to Plutarch exactly (480,000 = 48,000 CPU/case x 10 cases,
  one fewer Apply-beta-reduction per top-level call, independent of `n`) - superseded by the table, which
  is strictly cheaper, but documents that even the *recursive* encoding had a cheap, byte-exact fix
  available.
- **factorial (open) - correctness fix + variant comparison (Task 13, 2026-08-25)**: Task 12a's table
  above was a **wrong answer for `x >= 13`**: `sliceByteString` clamps instead of erroring past the end of
  a 13-entry table, so `x > 12` silently read an empty slice and `byteStringToInteger` decoded that as `0`
  - not a benchmark-only quirk, a genuine correctness bug (the scenario spec, `scenarios/factorial/
  factorial.md` requirement 3, requires the program to "compute results dynamically, not have values
  hardcoded" for *all* inputs, not just the 10 fixture cases). Fixed by making the table a **memoized base
  case with a computed fallback**: `factorial(x) = if x < 0 then 1 else if x <= 12 then table(x) else x *
  factorial(x - 1)`, the `x >= 13` branch falling back to genuine self-application recursion that bottoms
  out on the table - correct for every integer input (verified: `FactorialCapeTest` now asserts `x =
  13/15/20/25` and negatives beyond the fixture, each against a real Scala `BigInt` factorial, not a
  hand-typed literal).
  - **Two table encodings were implemented and measured** across all 10 open-mode fixture cases (see
    `FactorialOpen.scala` for the full write-up): **`termA`** (PV9-compatible, the original
    `sliceByteString`/`byteStringToInteger` decode, now gated by a single `x <= 12` check) - 109-byte
    script, 18,413,280 summed steps, 36,090 summed mem. **`termB`** (PV11 case-on-builtin-integer, the
    `FibonacciOpen` technique) - 91-byte script, 4,545,903 summed steps, 24,219 summed mem. `termB` wins on
    every axis and is adopted as `term`.
  - **Negative-clamp hypothesis, confirmed by evaluation**: `sliceByteString`'s `from` argument clamps a
    negative start to `0` (it's `drop`/`take` internally, and `drop` on a negative count is a no-op in both
    Scala and Haskell), so `termA` needs only the one `x <= 12` guard, not a separate `x < 0` guard - every
    `x <= 0` case (including `factorial_negative`, `x = -100`, `x = -1`) already reads table entry `0`
    (`= 1`) through that same branch. Confirmed empirically, not assumed: a spike test
    (`FactorialVariantSpike`, deleted after capturing these numbers) ran both variants through
    `CapeHarness` against all 10 fixture cases including the negative one, and `termA`'s
    `factorial_negative` case passed the harness's `expectedTerm == (con integer 1)` equality check with
    no separate negative guard in the term.
  - **Gate consequence**: adopting `termB` means `factorial` (open) now needs `min_plutus_version =
    1.60.0.0` (case-on-builtins is PV11-only) - it was CAPE's *only* ungated Scalus submission (CAPE
    production-track-evaluable); it now joins the other 7 on CAPE's preview track. Traded for a ~4x CPU
    win (18,413,280 -> 4,545,903, i.e. `termA` vs `termB`) and a smaller script (109 -> 91 bytes); the
    alternative (keep `termA`, stay ungated) was measured and is strictly worse on every axis, so this
    isn't a close call.
  - **Net effect vs. the pre-Task-13 snapshot**: went from **-60.2% ahead** (14,714,596 CPU, wrong for `x
    >= 13`, ungated) to **-87.7% ahead** (4,545,903 CPU, correct for every input, gated/preview-track).
    Both numbers "lead" the `Plutarch_..._exbudget` row (37,001,975) on raw CPU, but only the *old* number
    was directly comparable on CAPE's production report; the new number is directly comparable only within
    CAPE's preview report (see "Mainnet PV11 vs CAPE tooling lag" above) - correctness was worth the track
    change.
- **fibonacci (open) - table rewrite (Task 12a, 2026-08-24)**: `FibonacciOpen.scala` was rewritten from
  the naive O(2^n) double-recursion (see the root-cause below) to an O(1) direct lookup: `fib(0)..fib(25)`
  (the scenario's 11 measurement cases only exercise `0 <= n <= 25`) are baked in as 26 UPLC 1.1.0
  "case-on-builtins" (PV11/vanRossem) branches, `case`d directly on the input `Integer` itself - the CEK
  machine selects branch `i` when the scrutinee is the raw integer `i`, so the selected branch returns
  with **zero further builtin calls** (no decode/slice step at all). An outer `case` on the `Bool` from
  `lessThanEqualsInteger n (-1)` routes `n < 0` (including the negative-input fixture case) to return `n`
  directly. Requires `min_plutus_version = 1.60.0.0` (case-on-builtins is PV11-only) - tried first without
  the gate (a PV9-compatible variant of the same table, decoded via
  `sliceByteString`/`byteStringToInteger` exactly like the `Scalus_0.18.2_Unisay_preview` leader's
  technique) which measured exactly **16,459,951** CPU, tying the older "prepacked" submission family but
  still 12.9% *behind* the 0.18.2 preview leader's 14,567,412 - not good enough, so the gate was adopted.
  A first case-on-builtins attempt (case on the `Bool`, `sliceByteString`/`byteStringToInteger` decode
  chain, structurally identical to the leader's own `fibonacci.uplc`) tied the leader **exactly**
  (14,567,412 CPU, 23,874 mem, both fields bit-for-bit identical) - a tie, not a win, so one more design
  step (casing directly on the integer, skipping the decode chain entirely) was tried and measured
  strictly cheaper. Final result: went from **~12,000x behind** to **-82.4% ahead**
  (14,567,412 -> 2,563,307 CPU, clone-measured against CAPE's preview evaluator, 11/11 tests pass). Script
  size grew from 47 to 98 bytes (26 inline integer constants instead of a fixpoint combinator + slice
  chain) - a fair trade given CPU is the primary competitive metric and size isn't scored.
  **Root-cause of the pre-Task-12a state (Task 11d, 2026-08-24, preserved for history)**: the
  175,016,233,630 sum was real, not a bug. Ruled out: (a) a generator/registry aliasing bug -
  `CapeScenarios.scala` correctly wired `"fibonacci"` to `FibonacciContract.openProgram` and
  `"fibonacci_naive_recursion"` to `.baseProgram` (distinct `case class` entries, no shared mutable
  state); re-running `GenerateSubmissions` reproduced a byte-identical `fibonacci.uplc` (47 bytes,
  self-application fixpoint + `CaseConstrApply`); (b) a stale measurement -
  `fibonacci/Scalus_1.1.0_nau/metrics.json`'s 11 per-case `cpu_units` values matched
  `FibonacciCapeTest.scala`'s pinned `expectedOpenBudgets` digit-for-digit (e.g. `fibonacci_25` ->
  159,193,455,218 in both), independently reproduced by the JVM CEK harness. The actual cause: the old
  `FibonacciOpen` (source docstring: "Uses naive recursion at the UPLC level") hand-wrote the *same*
  O(2^n) double-recursive algorithm as the `naive_recursion` baseline - just directly in UPLC for a
  smaller script (47 vs 42 bytes), not for asymptotic efficiency; `fibonacci_25` alone was 159.2B of the
  175.0B total (91%). This is what Task 12a's table rewrite above fixed.
- **fibonacci (open) - completeness fix + linear fallback (Task 14, 2026-08-25)**: Task 12a's table
  above was correct on the fixture (`0 <= n <= 25`) but incomplete for the rest of the integer domain -
  case-on-builtin-integer *errors* (`CaseIndexOutOfBounds`), rather than silently returning a wrong
  value, for any `n > 25` (unlike `factorial`'s Task 12a table, which used `sliceByteString`'s
  out-of-range clamp and so returned a silently-wrong `0`). Loud failure is better than a wrong answer,
  but it's still not "compute results dynamically" for every input, per `scenarios/fibonacci/
  fibonacci.md` requirement 3 (the same clause Task 13 cited for `factorial`). Fixed by adding a
  memoized-base-case-plus-computed-fallback structure, mirroring `factorial`'s Task 13 fix: `fibonacci(n)
  = if n < 0 then n else if n <= 25 then table(n) else go(n - 25, fib(24), fib(25))`, where `go(k, a, b)
  = if k <= 0 then b else go(k - 1, b, a + b)` is a **linear** accumulator loop (self-application
  recursion via `pfix`, `FactorialOpen`'s idiom), not the O(2^n) naive double recursion Task 11d's
  root-cause analysis already showed was catastrophic for this scenario. Correctness: with `a =
  fib(m - 1)`, `b = fib(m)`, each `go` step advances `m` by one while decrementing `k`, so `go(n - 25,
  fib(24), fib(25)) = fib(n)` for every `n >= 25` - verified (`FibonacciCapeTest`) against an independent
  iterative Scala fibonacci for `n = 26/30/40/60` and negatives beyond the fixture (`-100/-25/-2`), plus
  a three-way JVM/compiled-naive-UPLC/memoized-open agreement check over the full fixture domain
  (`-3..25`, cheap for naive double recursion - see the test file for why that range and not further).
  No gate change: `fibonacci` was already on CAPE's preview track since Task 12a, so this fix doesn't
  move any submission between tracks (unlike `factorial`'s Task 13, which did).
  - **Cost of the extra guard**: summed CPU went from 2,563,307 to 3,961,677 (+54.5%, one more
    `Term.Case` on the hot table-lookup path plus the now-reachable `go` fallback branch), summed mem
    from 14,111 to 20,121 (+42.6%), script size from 98 to 156 bytes (58 more bytes for `go`'s
    3-argument self-application fixpoint). Still **-72.8% ahead** of the leader
    (`Scalus_0.18.2_Unisay_preview`, 14,567,412 CPU) - nowhere close to the margin needed to lose the
    #1 spot, so the completeness fix was effectively free competitively.
- **fibonacci_naive_recursion caveat (unchanged by Task 12a)**: this is the *fixed*-algorithm scenario
  (only compiler options may change it - Task 12a's scope was the "open" scenarios only), so it still
  hand-recurses and still costs 129,093,086,599 CPU, 11.9% behind the true best
  (`Scalus_0.18.2_Unisay_preview`/`Scalus_0.17.0_Unisay_preview` at 115,413,721,337). The comparator's
  per-scenario verdict line (see the raw output below) prints "Scalus leads" for this scenario too, but
  that is triggered by `Scalus_0.18.2_Unisay_preview` - an older, different contributor's submission -
  ranking first, **not** our `Scalus_1.1.0_nau` row; the comparator (`CompareWithLeaderboard.scala`) only
  checks whether *any* row starting with `Scalus_` exists. Do not read "Scalus leads" here as "1.1.0
  leads." (For `fibonacci` (open), by contrast, "Scalus leads" is now literally true: our row is #1.)
- **fibonacci_naive_recursion - loss analysis and 0.18.2-vs-1.1.0 verdict (Task 12c, 2026-08-24)**:
  this is also a *fixed*-algorithm scenario; `scenarios/fibonacci_naive_recursion/
  fibonacci_naive_recursion.md` prescribes `fibonacci n | n <= 1 = n | otherwise = fibonacci (n-1) +
  fibonacci (n-2)` verbatim and explicitly states "No algorithmic optimizations beyond compiler's
  automatic optimizations" / "No deviations from the specified algorithm are permitted."
  - **Profile diff, ours vs. the algorithm-compliant leader**: profiled `FibonacciContract.baseProgram`
    against `Plinth_1.65.0.0_Unisay_preview`'s real `fibonacci_naive_recursion.uplc` on our own CEK,
    per the 11 measurement cases. Result: an **exact tie** - 129,093,086,599 total cpu / 481,520,743
    mem on both sides, matching **per case** (all 11, e.g. `fibonacci_25`: 117,422,538,753 both;
    `fibonacci_2`: 1,267,235 both) and matching **every single per-category cost line** in the
    `evaluateDebug` breakdown (`Step(Apply)` 29,894,576,000, `Builtin(SubtractInteger)`
    27,013,224,864, `Step(Var)` 19,218,080,000, `Builtin(AddInteger)` 13,506,612,432,
    `Builtin(LessThanEqualsInteger)` 11,700,928,203, ... down to `Startup` 1,100 - identical to the
    last digit on every line). Structurally, our compiled term is T2's self-application encoding
    (`[self self [subtractInteger n k]]` per recursive call, duplicated at both call sites rather
    than let-shared); the leader's term is the same self-application shape but shares the "self self"
    computation once via a `cse-3` let-binding reused at both call sites - a different arrangement
    that happens to cost exactly the same total machine steps once the let-binding's own scaffolding
    (`LamAbs`+`Apply`+2 extra `Var` reads) is accounted for. **Verdict: zero compiler-level gap on
    this scenario** - Scalus's naive-recursion fibonacci codegen is already cost-optimal relative to
    the best algorithm-compliant competitor.
  - **The 0.18.2-vs-1.1.0 delta explained (not a regression, not cost-model drift)**: the standings
    table's "Leader" row for this scenario, `Scalus_0.18.2_Unisay_preview` (115,413,721,337, ~11.9%
    below ours), is picked purely because `CompareWithLeaderboard.scala` ranks by lowest raw CPU with
    no algorithm-compliance check. Tracing it down: (1) it is a **third-party** submission from
    `Unisay/scalus-cape-submissions` (a different GitHub repo than `scalus3/scalus`, per its
    `metadata.json`/README), not a prior release of this project; (2) parsing and reprinting its
    `.uplc` shows a **3-way case** - `n<=1 -> n`, else `n==2 -> 1` (hardcoded), else recurse - where
    the prescribed algorithm has only 2 branches; (3) the profile confirms it: this row alone has a
    `Builtin(EqualsInteger)` cost (6,984,048,182) that is **absent** from both ours and the Plinth
    leader's profiles, plus proportionally lower `SubtractInteger`/`AddInteger`/`Apply`/`Var` counts
    from the recursive calls it skips; (4) our own `FibonacciBase.scala` has been byte-identical
    (plain 2-branch `if n <= 1 then n else fibonacci(n-1) + fibonacci(n-2)`, no `n == 2` special case)
    since the commit that first added it (`40f027e41`, `git log -p` over its whole history shows no
    other version ever existed) - ruling out a Scalus-side historical regression entirely; (5) the
    scenario spec's own "Algorithm Compliance" section states "No deviations from the specified
    algorithm are permitted," which a hardcoded `n == 2` fast path is. **Verdict: this is neither a
    codegen regression nor cost-model drift - it is an apples-to-oranges comparison against a
    third-party submission whose extra base case is arguably non-compliant with the scenario's own
    rules.** Against the algorithm-compliant leader (Plinth, implementing the identical prescribed
    recursion), Scalus 1.1.0 is at exact parity, confirmed to the last CEK machine step.
  - **Compiler-option experiment (negative + neutral results)**: `Options.releaseUntagged.copy(cceEnabled
    = true)` made things **worse** (148,310,462,599 cpu, +14.9% over baseline, 41 vs 42 bytes -
    smaller but far more expensive) - not adopted. `.copy(cseIterations = 4)` and `.copy(cseIterations
    = 0)` were both byte- and cpu-identical to the `cseIterations = 2` baseline (129,093,086,599) -
    CSE already converges within one iteration for this single-function scenario, consistent with the
    `ecd` finding above. No option change adopted; baseline (`Options.releaseUntagged`) is already
    optimal among the combinations tried and already ties the best compliant competitor.
- **htlc / two_party_escrow - loss analysis and tuning (Task 12b, 2026-08-24)**: profiled both
  scenarios' compiled programs against the leader's real `.uplc` on Scalus's own CEK
  (`Term.evaluateDebug`'s per-`ExBudgetCategory` cost map, summed over the measurement cases), per
  the task-12 workflow.
  *(The `htlc` half is superseded by the 2026-08-26 canonical-prelude refactor above, which deleted
  the `DropList`/`offsetOf` entry point this analysis produced; the profile diff and the rejected
  alternatives below remain the record of why that entry point existed and what it bought.)*
  - **htlc profile diff (leader's `.uplc` evaluates successfully on our CEK)**: our pre-tuning
    profile (4 measurement cases, 71,487,296 steps) vs. the leader's `Plinth_1.65.0.0_Unisay_preview`
    `htlc.uplc` run on the *same* CEK (59,669,720 steps) - `EqualsData` was identical on both sides
    (14,815,068 steps), but `SndPair` (10,507,408 vs 0) and `FstPair` (3,405,480 vs 0) were **entirely
    absent from the leader's profile** - the leader's README documents a "BuiltinCasing + dropList"
    decoder DSL (`lib/Plinth/Decoder.hs`) that skips straight to needed fields via `dropList` instead
    of `unConstrData` + `fstPair`(index)/`sndPair`(fields) pair-unpacking. The leader's budget on our
    own CEK (59.67M) is within ~2% of its real-evaluator-measured 60.54M, confirming the gap is
    **real extra work**, not a CEK-vs-real-evaluator cost-model artifact.
  - **Root cause**: `scData.to[ScriptContext]` (and `.to[TxInfo]`) go through `FromData.derived`
    (`FromDataMacros.scala`), a Scala quote-macro that unconditionally decodes *every* declared
    field via a fixed `.tail`-chain - for `TxInfo`'s 16 fields, this decodes `fee`/`mint`/
    `certificates`/`withdrawals`/`redeemers`/`data`/`id`/`votes`/`proposalProcedures`/
    `currentTreasuryAmount`/`treasuryDonation` even though neither validator reads them, and it never
    emits `dropList` regardless of PV11 (unlike `ProdDataListOps.genSelect`, which *does* choose
    `dropList` over `tailList` for hand-written `.field` selects on lazily-Data-represented values,
    field index >= 2). Classification: **(c) missing optimizer pass** - `derives FromData` doesn't
    route through the same dropList-capable lowering hand-written field access already has.
    Evidence line added to `CODEGEN_IMPROVEMENT_PLAN.md` T16 (usage-driven representation inference).
  - **Fix applied (both scenarios, open-mode source rewrite)**: replaced `scData.to[ScriptContext]`/
    `.to[TxInfo]` with hand-written `Builtins.unConstrData`/`Builtins.dropList`
    (`offsetOf[TxInfo](_.field)`-indexed) navigation that decodes only the 3 fields `htlc` reads
    (`inputs`, `validRange`, `signatories`) and the 4 `two_party_escrow` reads (`inputs`, `outputs`,
    `validRange`, `signatories`), skipping the other 11-12 `TxInfo` fields entirely; `two_party_escrow`
    additionally keeps `inputs`/`validRange` decode conditional on the redeemer action (Deposit never
    decodes `inputs`, Accept never decodes `validRange`), matching the pre-tuning code's per-branch
    laziness. Second fix (`two_party_escrow` only): `handleAccept`/`handleRefund` previously walked
    `outputs` **twice** - once via `foldLeft` to sum the counterparty's lovelace, once via
    `findOutputsByCredential(...).isEmpty` to check no script funds remain - fused into one
    `settleAndVerify` traversal that decodes each output's credential once and checks both conditions
    per element (a "repeated traversal, fuse/hoist" fix from the task-12b lore checklist).
  - **A same-session, contradicting experiment (documented, then reverted)**: switching every
    `a.toData == b.toData` credential/PubKeyHash/TxOutRef comparison to the hand-written narrow
    `Eq[...]`'s `===` (which compares raw `ByteString`s via `equalsByteString`, per `Contexts.scala`)
    produced **zero** measured change - byte-identical script and budgets. Inspecting the emitted
    UPLC showed why: when one operand is lazily-kept `Data` (from a list walk) and the other is a
    decoded native value, the lowering re-wraps the native side back to `Data` via `constrData` and
    still calls `equalsData` - the representation choice, not the source-level comparator, decides
    this. A second experiment - hand-decoding one level further, `TxInInfo`/`TxOut`/`Address` down to
    just `outRef`+`credential` via a local `InputView` case class - made `htlc` **worse** (571 -> 658
    bytes, 18.49M -> 23.82M steps) and was reverted; the existing lazy-Data-per-access default for a
    short input list already beat forcing eager materialization into a new product type. Both are
    real (c)/(b) compiler-representation findings, not source bugs - evidence lines added to
    `CODEGEN_IMPROVEMENT_PLAN.md` T8 (equality) and T16 (representation inference).
  - **Measured result**: `htlc` 72,360,224 -> 72,001,560 CPU (-0.5%; the DropList/TxInfo-field-skip
    win was largely offset by `EqualsData`, `SndPair`, `FstPair` staying at their pre-tuning values -
    those are dominated by decoding the *used* fields, e.g. each input's `TxOutRef`/`Address`/
    `Credential`, which this task's fixes don't touch), gap to leader 19.5% -> 18.9%.
    `two_party_escrow` 225,473,091 -> 197,737,600 CPU (**-12.3%**, clone/real-evaluator-measured), gap
    to leader 34.2% -> **17.7%** (roughly halved) - the bigger win here reflects `two_party_escrow`
    reading more of `TxInfo` (4 fields vs. htlc's 3, skipping 11 vs. htlc's 12) and having the
    double-traversal fusion, which htlc's simpler logic never had. All 26 htlc + 48 two_party_escrow
    CAPE fixture tests (25/47 real-evaluator-measured measurements+checks, plus the 2 script-size
    tests) still pass; no check was weakened.
  - **Residual gap classification**: what's left (18.9%/17.7%) is dominated by `EqualsData` (T8,
    representation-driven, confirmed unavoidable at the source level per the reverted `===`
    experiment above) and by per-input `TxOutRef`/`Address`/`Credential`/`Value` decode cost inherent
    to fields the validators *do* read (not a further field-skipping opportunity) - both are
    **compiler-level (b)/(c)**, not something more source tuning can close without the T8/T16 lowering
    work landing. Stopping here per the task's guidance ("stop when... remaining gap is clearly
    compiler-level").
- **linear_vesting**: leads on total fee since the canonical-Value-ordering fix + workaround revert,
  then the house-style refactor + follow-up (2026-08-25, see the resolved section above); raw CPU is
  still +6.1% vs the `_preview` leader (mem and size are well ahead, which is what wins the fee
  ranking).
- Current leaders on most validator scenarios are themselves `_preview` submissions (measured
  pre-vanRossem-mainnet, same evaluator track we're gated into - see above); ours are mainnet-track PV11
  builds. Compare like-for-like against the `_preview` row, not the non-`_preview` mainnet row further
  down each ranking.

## Known caveat: .uplc cosmetic name drift across JVM runs

Regenerating submissions in separate `sbtn`/JVM invocations can produce a *different* `.uplc` text for
the same source and options (confirmed for `two_party_escrow`: two regenerations days apart had
different MD5s). Root cause: non-deterministic fresh-variable-name allocation in the SIR-to-UPLC
pipeline's CSE/dedup tie-breaking, which is stable *within* one JVM/sbt session (repeated
`GenerateSubmissions` calls in the same session produced byte-identical output) but not *across*
separate launches. Confirmed **zero functional or cost effect**: running the reference evaluator
directly against both `.uplc` variants produced a byte-identical `measurements` block (same
`cpu_units`/`memory_units` sums, medians, `term_size`, fees - every field). Treat this as a
submission-reproducibility cosmetic caveat, not a correctness or cost regression: re-running the driver
may regenerate byte-different (but behaviorally identical) `.uplc` files for `@Compile`'d scenarios.

## Comparator output (verbatim)

```
== ecd
   1. Plinth_1.65.0.0_Unisay_preview                cpu=     18,747,572 mem=      71,396
   2. Plinth_1.64.0.0_Unisay_preview                cpu=     18,747,572 mem=      71,396
   3. Plinth_1.61.0.0_Unisay_preview                cpu=     22,731,572 mem=      96,296
   4. Scalus_1.1.0_nau                              cpu=     24,747,572 mem=     108,896
   5. Plinth_1.65.0.0_Unisay                        cpu=     29,148,998 mem=     107,557
   6. Plinth_1.64.0.0_Unisay                        cpu=     29,228,998 mem=     108,057
   7. Plinth_1.45.0.0_Unisay                        cpu=     36,668,998 mem=     154,557
  -> Scalus is BEHIND Plinth_1.65.0.0_Unisay_preview by 32.0% CPU
== factorial
   1. Scalus_1.1.0_nau                              cpu=      4,545,903 mem=      24,219
   2. Plutarch_1.11.0_SeungheonOh_exbudget          cpu=     37,001,975 mem=     137,290
   3. Scalus_0.16.0_Unisay                          cpu=     37,481,975 mem=     140,290
   4. Scalus_0.17.0_Unisay                          cpu=     37,481,975 mem=     140,290
   5. Plutarch_1.11.0_SeungheonOh_size              cpu=     37,481,975 mem=     140,290
   6. Scalus_0.12.1_Unisay                          cpu=     37,481,975 mem=     140,290
  -> Scalus leads
== factorial_naive_recursion
   1. Plinth_1.65.0.0_Unisay_preview                cpu=     28,019,280 mem=     107,235
   2. Scalus_1.1.0_nau                              cpu=     28,019,280 mem=     107,235
   3. Plinth_1.61.0.0_Unisay_preview                cpu=     28,019,280 mem=     107,235
   4. Plinth_1.64.0.0_Unisay_preview                cpu=     28,019,280 mem=     107,235
   5. Pebble_0.1.2_michele-nuzzi                    cpu=     30,365,890 mem=     110,942
   6. Scalus_0.18.2_Unisay_preview                  cpu=     32,819,280 mem=     137,235
   7. Scalus_0.17.0_Unisay_preview                  cpu=     32,819,280 mem=     137,235
   8. Plinth_1.65.0.0_Unisay                        cpu=     37,241,975 mem=     138,790
   9. Plinth_1.64.0.0_Unisay                        cpu=     37,481,975 mem=     140,290
  10. Scalus_0.16.0_Unisay                          cpu=     42,041,975 mem=     168,790
  11. Scalus_0.18.2_Unisay                          cpu=     42,281,975 mem=     170,290
  12. Scalus_0.17.0_Unisay                          cpu=     42,281,975 mem=     170,290
  13. Plutarch_1.11.0_SeungheonOh                   cpu=     42,281,975 mem=     170,290
  14. Plinth_1.45.0.0_Unisay                        cpu=     42,281,975 mem=     170,290
  15. Scalus_0.12.1_Unisay                          cpu=     42,521,975 mem=     171,790
  16. OpShin_1.0.0_nielstron                        cpu=     72,921,975 mem=     361,790
  -> Scalus is BEHIND Plinth_1.65.0.0_Unisay_preview by 0.0% CPU
== fibonacci
   1. Scalus_1.1.0_nau                              cpu=      3,961,677 mem=      20,121
   2. Scalus_0.18.2_Unisay_preview                  cpu=     14,567,412 mem=      23,874
   3. Scalus_0.16.0_Unisay_prepacked                cpu=     16,459,951 mem=      30,485
   4. Scalus_0.18.2_Unisay                          cpu=     16,459,951 mem=      30,485
   5. Scalus_0.12.1_nau_prepacked                   cpu=     16,459,951 mem=      30,485
   6. Plutarch_1.11.0_SeungheonOh_prepacked         cpu=     16,459,951 mem=      30,485
   7. Aiken_1.1.19_KtorZ_prepacked                  cpu=     22,521,989 mem=      49,432
   8. Plinth_1.61.0.0_Unisay_preview                cpu=     59,206,243 mem=     243,219
   9. Plinth_1.64.0.0_Unisay_preview                cpu=     59,206,243 mem=     243,219
  10. Plinth_1.65.0.0_Unisay_preview                cpu=     59,334,107 mem=     243,219
  11. Aiken_1.1.19_KtorZ_tailrec                    cpu=     67,838,022 mem=     256,986
  12. Plinth_1.65.0.0_Unisay                        cpu=     75,486,958 mem=     297,218
  13. Plinth_1.45.0.0_Unisay                        cpu=     85,087,094 mem=     358,018
  14. Plinth_1.64.0.0_Unisay                        cpu=     91,379,406 mem=     355,606
  15. OpShin_1.0.0_nielstron                        cpu=    240,131,414 mem=   1,306,476
  16. Plutarch_1.11.0_SeungheonOh_exbudget          cpu=175,015,705,630 mem= 641,935,762
  17. Scalus_0.16.0_Unisay                          cpu=175,016,233,630 mem= 641,939,062
  18. Scalus_0.17.0_Unisay                          cpu=175,016,233,630 mem= 641,939,062
  19. Plutarch_1.11.0_SeungheonOh_size              cpu=175,016,233,630 mem= 641,939,062
  20. Scalus_0.12.1_Unisay                          cpu=175,016,233,630 mem= 641,939,062
  -> Scalus leads
== fibonacci_naive_recursion
   1. Scalus_0.18.2_Unisay_preview                  cpu=115,413,721,337 mem= 476,772,473
   2. Scalus_0.17.0_Unisay_preview                  cpu=115,413,721,337 mem= 476,772,473
   3. Plinth_1.65.0.0_Unisay_preview                cpu=129,093,086,599 mem= 481,520,743
   4. Scalus_1.1.0_nau                              cpu=129,093,086,599 mem= 481,520,743
   5. Plinth_1.61.0.0_Unisay_preview                cpu=129,093,086,599 mem= 481,520,743
   6. Plinth_1.64.0.0_Unisay_preview                cpu=129,093,086,599 mem= 481,520,743
   7. Pebble_0.1.2_michele-nuzzi                    cpu=154,053,890,937 mem= 508,500,462
   8. Scalus_0.18.2_Unisay                          cpu=161,982,099,770 mem= 626,283,790
   9. Scalus_0.16.0_Unisay                          cpu=161,982,099,770 mem= 626,283,790
  10. Scalus_0.17.0_Unisay                          cpu=161,982,099,770 mem= 626,283,790
  11. Scalus_0.12.1_Unisay                          cpu=161,982,627,770 mem= 626,287,090
  12. Aiken_1.1.17_KtorZ                            cpu=170,746,089,630 mem= 615,250,662
  13. Plinth_1.65.0.0_Unisay                        cpu=170,746,233,630 mem= 615,251,562
  14. Plinth_1.64.0.0_Unisay                        cpu=175,016,233,630 mem= 641,939,062
  15. Plutarch_1.11.0_SeungheonOh                   cpu=200,639,929,630 mem= 802,087,162
  16. Plinth_1.45.0.0_Unisay                        cpu=213,452,041,630 mem= 882,162,862
  -> Scalus leads
== htlc  [re-captured 2026-08-26 after the prelude-helper refactor (script 597 bytes); the
          intrinsic fix pending review on feat/intrinsic-eq-packed-list would recover
          582 B / 191,350 mem / 76,154,996 cpu. The pre-refactor hand-rolled build measured
          571 B / 174,822 mem / 72,001,560 cpu = 23,844 total, i.e. mem #1 and 0.1% off the
          lead; the other scenarios' blocks above/below predate this]
   1. Plinth_1.65.0.0_Unisay_preview                total=   23,830 (exec=  14,965 #1 + ref=  8,865) mem=     183,704 (mem #1) cpu=     60,542,648
   2. Scalus_1.1.0_nau                              total=   26,569 (exec=  17,614 #2 + ref=  8,955) mem=     205,062 (mem #2) cpu=     80,183,908
   3. Plinth_1.65.0.0_Unisay                        total=   28,376 (exec=  17,981 #4 + ref= 10,395) mem=     206,188 (mem #3) cpu=     84,377,848
   4. Scalus_0.18.2_Unisay_preview                  total=   30,169 (exec=  19,534 #5 + ref= 10,635) mem=     234,598 (mem #5) cpu=     83,173,348
   5. Scalus_0.17.0_Unisay_preview                  total=   30,169 (exec=  19,534 #6 + ref= 10,635) mem=     234,598 (mem #6) cpu=     83,173,348
   6. Plinth_1.64.0.0_Unisay                        total=   31,826 (exec=  20,786 #7 + ref= 11,040) mem=     246,392 (mem #7) cpu=     91,108,232
   7. Plinth_1.61.0.0_Unisay_preview                total=   36,737 (exec=  24,752 #9 + ref= 11,985) mem=     325,844 (mem #13) cpu=     82,523,154
   8. Scalus_0.18.2_Unisay                          total=   39,538 (exec=  25,678 #11 + ref= 13,860) mem=     307,688 (mem #9) cpu=    109,896,704
   9. Scalus_0.17.0_Unisay                          total=   39,538 (exec=  25,678 #12 + ref= 13,860) mem=     307,688 (mem #10) cpu=    109,896,704
  10. Scalus_0.16.0_Unisay                          total=   43,175 (exec=  28,055 #14 + ref= 15,120) mem=     345,568 (mem #14) cpu=    112,554,304
  11. Plinth_1.45.0.0_Unisay                        total=   44,476 (exec=  25,846 #13 + ref= 18,630) mem=     319,172 (mem #12) cpu=    103,037,912
  12. Plinth_1.45.0.0_Unisay_asdata                 total=   60,849 (exec=  37,494 #15 + ref= 23,355) mem=     478,840 (mem #15) cpu=    136,812,312
  13. Plinth_1.65.0.0_Unisay_asdata                 total=   79,202 (exec=  21,977 #8 + ref= 57,225) mem=     262,954 (mem #8) cpu=     94,374,994
  14. Plinth_1.64.0.0_Unisay_preview                total=   80,671 (exec=  17,701 #3 + ref= 62,970) mem=     229,132 (mem #4) cpu=     62,125,938
  15. Plinth_1.64.0.0_Unisay_asdata                 total=   87,353 (exec=  25,328 #10 + ref= 62,025) mem=     311,354 (mem #11) cpu=    102,118,994
  -> BEHIND Plinth_1.65.0.0_Unisay_preview by 11.5% total fee (17.7% more exec)
== linear_vesting  [re-captured 2026-08-26 without the compiler fix on this branch (script
                    661 bytes; the intrinsic fix pending review on feat/intrinsic-eq-packed-list
                    would recover the 2026-08-25 numbers: 612 B / 37,069 total); the other
                    scenarios' blocks above/below predate this and their underlying numbers are
                    unchanged]
   1. Scalus_1.1.0_nau                              total=   41,661 (exec=  31,746 #1 + ref=  9,915) mem=     353,968 (mem #1) cpu=    157,028,524
   2. Plinth_1.65.0.0_Unisay_preview                total=   50,234 (exec=  36,854 #2 + ref= 13,380) mem=     459,098 (mem #2) cpu=    143,737,440
   3. Plinth_1.65.0.0_Unisay                        total=   60,846 (exec=  45,891 #3 + ref= 14,955) mem=     543,862 (mem #3) cpu=    201,238,122
   4. Plinth_1.64.0.0_Unisay                        total=   68,934 (exec=  52,974 #4 + ref= 15,960) mem=     620,048 (mem #4) cpu=    238,505,394
   5. Scalus_0.18.2_Unisay_preview                  total=   73,501 (exec=  57,301 #5 + ref= 16,200) mem=     700,543 (mem #5) cpu=    234,107,961
   6. Plinth_1.45.0.0_Unisay                        total=   89,539 (exec=  64,489 #6 + ref= 25,050) mem=     786,880 (mem #6) cpu=    264,713,442
   7. Scalus_0.18.2_Unisay                          total=   99,587 (exec=  78,542 #7 + ref= 21,045) mem=     950,908 (mem #7) cpu=    328,355,965
   8. Plinth_1.64.0.0_Unisay_preview                total=  139,866 (exec= 120,306 #8 + ref= 19,560) mem=   1,550,601 (mem #8) cpu=    427,674,790
   9. Plinth_1.61.0.0_Unisay_preview                total=  154,900 (exec= 134,845 #9 + ref= 20,055) mem=   1,739,417 (mem #9) cpu=    478,224,546
  10. Plinth_1.65.0.0_Unisay_plain                  total=  176,474 (exec= 153,644 #10 + ref= 22,830) mem=   1,912,860 (mem #10) cpu=    600,159,311
  11. Plinth_1.64.0.0_Unisay_plain                  total=  195,630 (exec= 172,095 #11 + ref= 23,535) mem=   2,179,360 (mem #11) cpu=    642,799,311
  12. Plinth_1.45.0.0_Unisay_plain                  total=  254,376 (exec= 211,056 #12 + ref= 43,320) mem=   2,708,014 (mem #12) cpu=    760,101,590
  -> LEADS by 17.1% total fee (exec #1)
== two_party_escrow
   1. Plinth_1.65.0.0_Unisay_preview                cpu=    168,065,744 mem=     536,464
   2. Scalus_1.1.0_nau                              cpu=    197,737,600 mem=     483,057
   3. Plinth_1.65.0.0_Unisay                        cpu=    228,000,651 mem=     596,443
   4. Scalus_0.18.2_Unisay_preview                  cpu=    231,874,447 mem=     619,461
   5. Scalus_0.18.2_Unisay                          cpu=    297,557,371 mem=     775,801
   6. Plinth_1.64.0.0_Unisay                        cpu=    376,623,075 mem=   1,021,313
   7. Plinth_1.45.0.0_Unisay                        cpu=    425,273,438 mem=   1,322,778
   8. Plinth_1.64.0.0_Unisay_preview                cpu=    666,435,923 mem=   2,667,358
   9. Plinth_1.61.0.0_Unisay_preview                cpu=    671,171,923 mem=   2,696,958
  10. Plinth_1.65.0.0_Unisay_asdata                 cpu=  1,009,439,972 mem=   3,465,199
  11. Plinth_1.64.0.0_Unisay_plain                  cpu=  1,081,092,952 mem=   3,910,704
  12. Plinth_1.45.0.0_Unisay_plain                  cpu=  1,121,945,527 mem=   4,271,530
  -> Scalus is BEHIND Plinth_1.65.0.0_Unisay_preview by 17.7% CPU
```

## Next steps

1. **(done 2026-08-25, PR pending user submission)** The `linear_vesting` fixture-builder bug is
   root-caused and fixed on the clone's `fix/canonical-value-ordering` branch (builder fix +
   regenerated metrics for all 11 committed `linear_vesting` submissions); the PR body and the exact
   push/open commands are in `docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md`. The interim
   `valueBuiltins = false` build is reverted (see the resolved section above).
2. Task 12 (loss analysis and tuning): close the CPU gaps on `ecd` (+32.0%), `htlc`, `linear_vesting`
   (+31.1%, partly interim-build overhead), and `two_party_escrow`. `factorial_naive_recursion` is
   already at parity; no action needed there. **Task 12a (2026-08-24)** closed both open-mode synthetic
   scenarios: `factorial` (open) went from +1.3% behind to -60.2% ahead (table rewrite, no gate needed)
   and `fibonacci` (open) went from ~12,000x behind to -82.4% ahead (table rewrite via PV11
   case-on-builtins, gated) - both now lead their scenario's leaderboard for real, not just via the
   comparator's same-Scalus-prefix false positive. **Task 12b (2026-08-24)** tuned the two remaining
   validator scenarios: `htlc` +19.5% -> +18.9% behind (72,360,224 -> 72,001,560 CPU, -0.5%) and
   `two_party_escrow` +34.2% -> +17.7% behind (225,473,091 -> 197,737,600 CPU, -12.3%, gap roughly
   halved) via narrower `ScriptContext`/`TxInfo` field decode (`dropList`-based, skipping unused
   `TxInfo` fields instead of `derives FromData`'s eager full-record decode) plus, for
   `two_party_escrow`, fusing its two separate `outputs` traversals into one. See the notes above
   (per-scenario standings section) for the full profile-diff evidence, the reverted
   `===`/`InputView` experiments, and the residual-gap classification (compiler-level, T8/T16 in
   `CODEGEN_IMPROVEMENT_PLAN.md`). **Task 12c (2026-08-24)** closed out the two remaining fixed-mode
   scenarios with analysis (fixed mode forbids algorithm changes, and no compiler-option change
   won for either): `ecd`'s entire 32.0% gap is recursion-encoding overhead (T2 self-application
   fixpoint vs. the leader's compiler-driven bounded-inlining unroll to depth 15, zero builtin-cost
   difference) - filed as evidence on T15 (size-budgeted callsite inlining / peel-unroll), no
   compiler-option experiment won, so the code is unchanged. `fibonacci_naive_recursion`'s 1.1.0 vs.
   0.18.2 delta is **not** a regression or cost-model drift: 1.1.0 ties the algorithm-compliant
   leader (`Plinth_1.65.0.0_Unisay_preview`) exactly, to the last CEK machine step across all 11
   cases; the `Scalus_0.18.2_Unisay_preview` row that ranks numerically first is a third-party
   submission (`Unisay/scalus-cape-submissions`) with a hardcoded `n == 2 -> 1` fast path the
   prescribed algorithm doesn't have. See the notes above for the full profile/structural evidence
   and the option-experiment results (both scenarios: no option beat the `Options.releaseUntagged`
   baseline).
3. Once all this lands and the min_plutus_version gate is no longer needed (upstream promotes past
   vanRossem), re-run the driver and refresh this snapshot.
4. **Task 12d (2026-08-24, final consistency sweep)**: `sbtn quick` green with no formatting changes;
   deleted all 8 `submissions/*/Scalus_1.1.0_nau` dirs in the clone and regenerated them from a clean
   slate at worktree commit `5d28bd0fc`; ran `scripts/cape-submit.sh` end to end (both the ungated
   `factorial` (current-track) and the 7 gated (preview-track) scenarios) – found and fixed a real driver
   defect (`nix develop`'s dev-shell banner leaking into `metrics.json` via a shell redirect, see the
   header note above); re-ran after the fix and all 8 scenarios verified, measured, and schema-validated
   green, reproducing every figure in the table below byte-for-byte. See the Submission checklist below
   for what remains before the PR opens.
5. **Task 13 (2026-08-25, factorial correctness fix)**: fixed the `x >= 13` silent-wrong-answer bug in
   `factorial` (open) described above - memoized base case + computed self-application-recursion
   fallback, correct for every integer input, verified by new `FactorialCapeTest` cases against a Scala
   `BigInt` factorial (`x = 13/15/20/25`, plus negatives beyond the fixture). Measured and compared two
   table encodings (`termA` PV9-compatible vs. `termB` PV11 case-on-builtins); `termB` won on every axis
   (91 vs 109 bytes, 4,545,903 vs 18,413,280 summed CPU) and is adopted, which moves `factorial` from
   CAPE's production track onto its preview track (`min_plutus_version = 1.60.0.0` added) - the last of
   the 8 scenarios to make that move, so all 8 are now gated/preview-track. Regenerated all 8 submissions
   and re-verified/re-measured `factorial` in the clone (preview track: 10/10 tests pass,
   `metadata.json`/`metrics.json` schema-valid); the other 7 dirs were regenerated but not re-verified in
   this task (unchanged source, no reason to expect drift). New clone-measured sum: 4,545,903 CPU, 24,219
   mem, 91-byte script - still leads `Plutarch_1.11.0_SeungheonOh_exbudget` (37,001,975) by a wider margin
   than before (-87.7% vs the prior -60.2%), now correct for every input instead of just the 10 fixture
   cases.
6. **Task 14 (2026-08-25, fibonacci completeness fix)**: fixed the `n > 25` `CaseIndexOutOfBounds` error in
   `fibonacci` (open) described above - memoized base case + a linear accumulator-loop fallback
   (`go(k, a, b) = if k <= 0 then b else go(k - 1, b, a + b)`, self-application recursion, not naive
   double recursion), correct for every integer input, verified by new `FibonacciCapeTest` cases against
   an independent iterative Scala fibonacci (`n = 26/30/40/60`, plus negatives beyond the fixture) and a
   three-way JVM/compiled-naive/memoized-open agreement check over `-3..25`. No gate change - `fibonacci`
   was already preview-track since Task 12a. Regenerated all 8 submissions and re-verified/re-measured
   both `fibonacci` and `fibonacci_naive_recursion` in the clone (preview track: 11/11 tests pass on both,
   `metadata.json`/`metrics.json` schema-valid); the other 6 dirs were regenerated but not re-verified in
   this task (unchanged source). `fibonacci_naive_recursion`'s clone-measured sum (129,093,086,599 CPU,
   481,520,743 mem) is bit-for-bit unchanged, confirming the `FibonacciBase.fibonacci` BigInt-literal-style
   edit (`n - BigInt(1)` -> `n - 1`) compiled to identical UPLC. New `fibonacci` clone-measured sum:
   3,961,677 CPU (+54.5% vs. the pre-fix 2,563,307), 20,121 mem (+42.6%), 156-byte script (was 98) - still
   **-72.8%** vs. the leader `Scalus_0.18.2_Unisay_preview` (14,567,412 CPU), nowhere near losing the #1
   spot, now correct for every input instead of just the 11 fixture cases.

## Submission checklist

What is done, automated, and reproducible as of this snapshot: source, tests, docs, and all 8 CAPE
submission artifacts in the clone (`/Users/nau/projects/lantr/UPLC-CAPE/submissions/*/Scalus_1.1.0_nau/`,
untracked in the clone's git, ready to add). What remains is manual, outward-facing work intentionally
left for the user (per this effort's scope – agents do not open PRs or file issues on this project's
behalf):

1. **Rebase/merge this work to `master`.** The worktree branch (`worktree-cape-submissions`) needs to
   land on `scalus3/scalus`'s `master` before its commit hash is resolvable on GitHub – required because
   every submission's `metadata.json` embeds `source_commit_hash` from `git rev-parse HEAD` at generation
   time (see the header note above: `5d28bd0fc` is not yet public).
2. **Regenerate from merged `master`**, not from this worktree: after the merge, `git rev-parse HEAD` on
   `master` gives a public, resolvable commit. Delete the 8 `Scalus_1.1.0_nau` dirs in the clone again and
   re-run `scripts/cape-submit.sh /Users/nau/projects/lantr/UPLC-CAPE 1.1.0` from a `master` checkout so
   every `metadata.json`'s `source_commit_hash`/README link points at a commit anyone can actually open.
   Expect the same measured numbers (this snapshot already proved reproducibility across two independent
   clean regenerations) but note the known `.uplc` cosmetic name-drift caveat above – a byte-different but
   functionally identical `.uplc` text is normal, not a re-verification failure.
3. **Commit the submissions in the clone** (not this repository) – `cd /Users/nau/projects/lantr/UPLC-CAPE`,
   add the 8 `submissions/*/Scalus_1.1.0_nau/` dirs, commit on a branch.
4. **Open the PR**: `gh pr create --title "Add Scalus 1.1.0 submissions (all 8 scenarios)" --body "..."`
   against the clone's `origin`, `IntersectMBO/UPLC-CAPE` (push a fork or a branch, per that repo's
   contribution flow) from that branch.
5. **Submit the prepared canonical-Value-ordering PR**: the fix is no longer just a drafted issue -
   the clone's `fix/canonical-value-ordering` branch carries the builder fix and the regenerated
   metrics for all 11 committed `linear_vesting` submissions, ready to push.
   `docs/internal/UPLC_CAPE_VALUE_CANONICALITY_PR.md` is the PR body and lists the exact
   `git push`/`gh pr create` commands. The interim build is already reverted on our side (see
   above); once the upstream PR merges, our `linear_vesting` submission measures green against
   upstream's own builder with no local patches.
6. **Optional**: flag the non-compliant `Scalus_0.18.2_Unisay_preview` row in `fibonacci_naive_recursion`
   upstream – it ranks first on CAPE's own leaderboard sort (lowest raw CPU) but implements a hardcoded
   `n == 2 -> 1` fast path the scenario's own "Algorithm Compliance" rule forbids (see the Task 12c
   analysis above). Not blocking – Scalus 1.1.0 already ties the best algorithm-compliant competitor
   exactly on this scenario – but worth a heads-up to CAPE's maintainers, since an algorithm-compliance
   check on submissions would change this scenario's leaderboard order.
