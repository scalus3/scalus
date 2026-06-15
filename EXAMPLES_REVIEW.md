# Scalus Blueprint Examples — Bug Review & Style-Unification Plan

Scope: the 21 Cardano Blueprint use cases
(<https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring>)
as implemented under `scalus-examples/jvm/src/main/scala/scalus/examples/`.
Reference style = **HTLC** (`htlc/`). Out of scope (not blueprint templates):
`setbench`, `cape`, `bilinearAccumulator`, `linkedlist`, `buidlerfest`, `knights`, `Groth16`,
`HelloCardano`, `MembershipToken`, `PubKeyValidator`, `MintingPolicy`.

> Status: **PLAN ONLY — no source edits made.** Bugs marked ✅ were spot-checked against source
> by the reviewer; others are high-confidence agent findings with precise `file:line` evidence and
> should be re-verified before fixing.

---

## 0. Template → directory map

| # | Template | Directory | On-chain? | Notes |
|---|----------|-----------|-----------|-------|
| 1 | Bet | `betting/` | yes | |
| 2 | Simple transfer | `simpletransfer/` | yes | no Transactions file |
| 3 | Token transfer | — | — | **appears unimplemented** (no native-token transfer example) |
| 4 | HTLC | `htlc/` | yes | **gold standard, clean** |
| 5 | Escrow | `escrow/` | yes | extra `EscrowOffchain.scala` (Bloxbean) |
| 6 | Auction | `auction/` | yes | monolith + `UnfixedAuction` (intentionally vulnerable) |
| 7 | Crowdfund | `crowdfunding/` | yes | monolith + `Endpoints` |
| 8 | Vault | `vault/` | yes | |
| 9 | Vesting | `vesting/` | yes | |
| 10 | Storage | `storage/` | **no** | off-chain only (by design) |
| 11 | Simple wallet | `simplewallet/` | **no** | off-chain only (by design) |
| 12 | Price Bet | `pricebet/` | yes | + oracle validator |
| 13 | Payment splitter | `paymentsplitter/` | yes | Naive + Optimized |
| 14 | Lottery | `lottery/` | yes | |
| 15 | Constant-product AMM | `amm/` | yes | `AmmOffchain` naming |
| 16 | Upgradeable Proxy | `upgradeableproxy/` | yes | `Offchain` naming, no Contract obj |
| 17 | Factory | `factory/` | yes | `FactoryExample.scala` naming |
| 18 | Decentralized identity | `decentralizedidentity/` | yes | |
| 19 | Editable NFT | `editablenft/` | yes | CIP-68 |
| 20 | Anonymous Data | `anonymousdata/` | yes | no Contract obj |
| 21 | Atomic Transactions | `atomictransactions/` | **no** | off-chain only (by design) |

---

## 1. The unified style ("HTLC standard")

Derived from `htlc/` + `CLAUDE.md`. This is the target every example is measured against.

### On-chain examples — 3 files + README
- **`XxxContract.scala`**: `object XxxContract extends Contract`; `private given Options = Options.release`;
  `lazy val compiled = PlutusV3.compile(XxxValidator.validate)`;
  `lazy val blueprint = Blueprint.plutusV3[Config, Action](title, description, version,
  license = Some("Apache License Version 2.0"), compiled)`.
- **`XxxValidator.scala`**: `@Compile object XxxValidator`; datum
  `case class Config(...) derives FromData, ToData`; redeemer
  `enum Action derives FromData, ToData`; error strings as
  `inline val Foo = "..."` collected at the bottom; checks via `require(cond, Foo)`; imports
  `scalus.cardano.onchain.plutus.v3.*` and `.prelude.*`. **Do not** add empty `@Compile object Config` /
  `@Compile object Action` companions — `derives FromData, ToData` is sufficient and the empty `@Compile`
  companions are redundant (verified by clean build; removed from htlc + vesting on this branch).
- **`XxxTransactions.scala`**: `case class XxxTransactions(env: CardanoInfo, contract: PlutusV3[Data => Unit])`;
  pure `TxBuilder` methods returning `Transaction`; **no** provider / `Future` / `.submit` / `Thread.sleep`;
  `private val scriptAddress`, `private val builder`.
- **`README.md`**: short, with a "How it works" section.

### Off-chain-only examples (`storage`, `simplewallet`, `atomictransactions`)
Do **not** force the Contract/Validator triad. Instead unify on a single off-chain convention:
`case class XxxTransactions(env: CardanoInfo)` with `TxBuilder` methods returning `Transaction`;
no `object`-with-`???`-placeholder shape; no hardcoded `CardanoInfo.mainnet`.

### Cross-cutting cleanups (apply everywhere)
1. **Hoist error strings** to `inline val` constants — near-universal deviation today.
2. **`license = Some("Apache License Version 2.0")`** — almost all currently `None`.
3. Remove leftover `// TODO`, `// ???`, `// Vxxx fix` mutation tags, section-banner ASCII comments.
4. Remove dead code: unused `given Eq[Config]` (escrow, vesting, …), unused error constants,
   `import scalus.{show as _, *}`, redundant `import …v3.Validator` already covered by `…v3.*`.
5. Brace/indentation per `CLAUDE.md` (`{}` for top-level/multi-line bodies; indentation for
   `if`/`match`/`for` under 20 lines; `then`/`do`).
6. **Tests:** keep exact `ExUnits` assertions and **re-baseline them on every validator change**
   (user decision, 2026-06-12: budget movement should be tracked, so a shifted assertion is the
   intended signal — do not relax to upper bounds). Re-baseline fee/script-size the same way. Always
   **add negative/adversarial tests** (see §3).
7. **Follow the project Scala style** (`CLAUDE.md`) and **prefer the Scalus standard library** over
   hand-rolled equivalents (user directive, 2026-06-15): when the on-chain prelude / ledger types already
   provide an identical or semantically-equivalent function (e.g. `===`/`!==`, `Value` ops, `Interval`
   helpers, `List`/`SortedMap`/`Option` combinators, `getValidityStartTime`), use it instead of a local
   re-implementation. Apply to every example as it is touched.

### Two open style decisions (need your call — see questions at end)
- **(A) Validator shape.** HTLC uses an explicit `inline def validate(scData) { ctx.scriptInfo match … }`.
  But ~15 examples use the `Validator` / `DataParameterizedValidator` base trait (overriding `spend`/`mint`).
  The trait pattern is the de-facto norm and is arguably cleaner. Recommendation: **adopt the base-trait
  pattern as the standard and update HTLC's description to match**, rather than rewriting 15 validators.
- **(B) Datum/redeemer names.** HTLC uses `Config`/`Action`. Many examples use domain names
  (`CampaignDatum`, `ProductDatum`, `AmmDatum`, `PricebetState`). These are often *clearer*.
  Recommendation: **keep meaningful domain names**; only require the redeemer to be an `enum` named
  by its role and datum/redeemer to `derive FromData, ToData`. Do not blanket-rename to `Config`/`Action`.

---

## 2. Bug catalogue (severity-ranked)

Legend: ✅ = reviewer-verified against source. Sev = High/Med/Low. Conf = agent confidence.

### Critical / High — value-preservation & authorization holes (the dominant theme)
These are mostly **missing value-preservation** or **missing double-satisfaction guards**, hidden because
every example's tests build only honest transactions.

| Example | Sev | ✅ | Finding (file:line) |
|---------|-----|----|---------------------|
| **AMM** | Critical | ✅ | Datum reserves never tied to pool output `Value` — `Value` imported but unused in `AmmValidator.scala`; swap/deposit/redeem validate only the datum transition. Pool fully drainable. Also: spend doesn't require the LP mint to occur, and the mint check sums all asset names under the policy (LP name unconstrained). |
| **Lottery** | Critical | ✅ | Winning-reveal branches (`LotteryValidator.scala:199-204`, `:241-247`) check only preimage hash + parity — **no output/value/destination/signature** check. Pot can be sent anywhere by anyone who saw the preimage. Empty-state reveals (`:100-181`) also don't preserve the locked value. No reveal deadline → reveal/timeout windows overlap. |
| **Crowdfunding** | Critical | ✅ FIXED | `handleReclaimSpend`: `reclaimerOutputIndices` length/uniqueness unchecked; `zip` truncation lets an attacker reclaim all donation UTxOs while paying out only a prefix, sweeping the rest as change. **Fixed** (this branch): require equal length + `requireDistinct(reclaimerOutputIndices)`; anyone-can-trigger is by design (funds routed to each donor). |
| **AnonymousData** | Critical | | Update/Delete (`AnonymousDataValidator.scala:204-256`) verify only Merkle membership of *some* participant, not ownership of the public `dataKey` → any member can overwrite/delete anyone's entry. Plus the write path requires a named signature in the same tx, breaking the core "unlinkable" claim (the two cannot both hold without a nullifier/ZK scheme). |
| **EditableNft** | Critical | ✅ | Seed UTxO never bound: `EditableNftValidator.scala:65` checks `tx.inputs.get(seedIndex).isDefined` but never compares to `param.seed` → one-shot mint defeated, NFT uniqueness broken. |
| **Vault** | High | | Wait-time bypass: deadline derived from `getValidityStartTime` (lower bound / 0 if unbounded), so a backdated `validFrom` makes `finalize` pass immediately (`VaultValidator.scala:139-151`). Also `deposit` doesn't constrain `status` and `finalize` has no signature check → anyone flips Idle→Pending and forces payout. Plus double satisfaction across same-owner vaults. README claims a recovery key that doesn't exist. |
| **PriceBet** | High | ✅ | `Win` authenticates the oracle reference input only by script credential (`PricebetValidator.scala:110-115`), never by a beacon NFT — `PricebetConfig` has no beacon field. Attacker creates a fake oracle UTxO with a winning rate and references it. |
| **DecentralizedIdentity** | High | | `PublishAttribute` (`DecentralizedIdentityValidator.scala:173-200`) accepts any datum-shaped delegation at the script address without requiring the delegation token → forged delegation, also defeats revocation. |
| **Betting** | High | | No timeout/reclaim path (`BettingValidator.scala` enum has only `Join`/`AnnounceWinner`) → funds lock if oracle silent or nobody joins; README documents a Timeout that isn't implemented. Plus `AnnounceWinner` double satisfaction (per-input lovelace `>=` against a shared output) and beacon token not burned. |
| **EditableNft** | High | ✅ | CIP-68 labels swapped **and** wrong-encoded: `userNftName="100"`, `refNftName="222"` (CIP-68: 100=ref, 222=user), and `ByteString.fromString("100")` is ASCII bytes, not the 4-byte CIP-68 label `0x000643b0`. Breaks all CIP-68 tooling interop. |
| **Vesting** | High | ✅ | No single-script-input guard (only `contractOutputs.length === 1` at `:101`, not inputs) → double satisfaction spending two vesting UTxOs with one shared continuing output. Also continuing-output check is lovelace-only → native tokens can be stripped. |
| **PaymentSplitter** | High | | Both validators reconcile only `getLovelace` → native tokens in a contract UTxO can be skimmed (no asset preservation). Naive validator also misses `reminder >= 0` that the Optimized one has. |
| **Auction** | High (Med conf) | | "Fixed" `AuctionValidator.handleEnd` DS guard counts NFTs only under its own script hash; because each instance is one-shot-parameterized (distinct hash), two same-seller instances ended together aren't detected → cross-instance double satisfaction; seller payout still `>=`. Test only covers the single-script case. |
| **UpgradeableProxy** | High | | Continuation located via `.headOption` and checked against own input value, not bound per-input (`UpgradeableProxy.scala:56-69`); `Call` needs no signature → two same-datum proxy UTxOs share one continuation, attacker pockets the other. |

### Medium / Low
| Example | Sev | Finding |
|---------|-----|---------|
| **Escrow** | Med | Deposit precondition uses `!=` instead of `===` against the wrong field (`EscrowValidator.scala:94-97`): idempotent re-deposit accepted; and if `initializationAmount == escrowAmount` the escrow can never be funded. |
| **Escrow** | Low | Balance/payout computed over credential-aggregated inputs/outputs → batching fails closed; fragile. |
| **Betting** | Low | Beacon asset name unconstrained on mint. |
| **PaymentSplitter** | Low | Payee outputs matched on payment credential only (staking-credential redirect of rewards). |
| **SimpleTransfer** | Low | `datum.get` instead of `getOrFail` (opaque error on hash/none datum). Otherwise **on-chain logic is sound** — cleanest after HTLC. |
| **Vault / many** | Low | `d.get` on datum `Option` instead of `getOrFail(Msg)` in several validators. |
| **HTLC** | Low | Two error-message strings have inclusive/exclusive wording swapped (`HtlcValidator.scala:81-82`); `finite`/`finiteOrFail` discard the bound's closure flag (relies on ledger convention). Logic is correct. |
| **Storage** | Low | `availableUtxos = userUtxos ++ prevTx.utxos` relies on selection skipping spent inputs; `// 4-byte big-endian` comment wrong (`BigInt.toByteArray` is minimal-width). No runtime bug (tests pass). |
| **SimpleWallet / AtomicTransactions** | Low | `???` placeholder addresses (objects throw on init, untested); `withdraw` comment says "withdraw all" but does one bare `.spend`; AtomicTransactions mixes `Value.ada`/`Value.lovelace` (7/9 lovelace, below min-UTxO) in a "same guarantee" demo. |

**Cross-cutting bug recommendation:** the recurring root causes are (1) **no value-preservation on continuing
outputs** (validate lovelace only, or validate datum only), and (2) **no "exactly one own script input" guard**
(double satisfaction). A shared `prelude` helper (e.g. `requireSingleOwnInput`, `requireValuePreserved`) plus a
reusable adversarial test harness would close most of these and prevent regressions.

---

## 3. Per-example plan

Each entry: **structure changes → bug fixes (if in scope) → cleanups → downstream refs**.
"Downstream refs" lists tests/docs that break on rename (examples are not MiMa-tracked; no deprecation
shims needed — they're discovered by package, not enumerated in `build.sbt`).

### htlc — reference (keep as-is, tiny polish)
- Fix swapped inclusive/exclusive wording in `HtlcValidator.scala:81-82`.
- Optionally add a one-line README note that committer ≠ receiver for cross-chain swaps.
- Refs: `HtlcTest.scala` (hardcoded ExUnits/size — candidate to relax).

### betting
- Structure: ✅ already 3-file + correct names. Move error strings to `inline val`; set Apache license;
  drop `import scalus.{show as _, *}` and redundant imports; remove `// ???`/`// TODO`/`// Vxxx` comments.
- Bugs: implement `Timeout`/`Reclaim` action (fixes fund-lock + matches README); add single-own-input guard
  to `AnnounceWinner`; require beacon burn + full-value payout; pin beacon asset name.
- Tests: replace happy-path-only `BettingValidatorTest` (has `// TODO: test wrong tx fails`) with negative cases.
- Refs: `BettingValidatorTest`, `BettingTransactionTest`, `docs/Examples.md`, `docs/design/cce-generalized-report.md`.

### simpletransfer
- Structure: rename datum `Parties`→`Config` (optional, per decision B); **add `SimpleTransferTransactions.scala`**
  (deposit/withdraw builders) — currently no off-chain code; update README.
- Bugs: `datum.get`→`getOrFail`; hoist + de-dup error strings; set Apache license; remove redundant `Validator` import.
- Refs: `SimpleTransferValidatorTest`, `docs/design/cce-generalized-report.md:215`.

### escrow
- Structure: **delete `EscrowOffchain.scala`** (Bloxbean/Blockfrost `main` with mnemonics/`Thread.sleep`/`sys.exit`)
  — `EscrowTransactions` already exists; update README. Hoist error strings; remove unused `given Eq[Config]`
  and `import scalus.{show as _, *}`.
- Bugs: fix deposit precondition to `=== initializationAmount`; compute balance from own input not credential sum;
  check specific continuing/payout output not credential aggregate. Reconcile "three-party"/"trusted intermediary"
  README vs the two-party implementation (add arbiter/timeout, or fix wording).
- Refs: `EscrowTest` (incl. its "known bug" test at `:142-164` and hardcoded `ExUnits`).

### auction
- Structure: largest refactor. Split `Auction.scala` (824 lines) into `AuctionContract.scala`
  (`extends Contract` + `Blueprint.plutusV3`), `AuctionValidator.scala` (`@Compile`, error constants),
  `AuctionTransactions.scala` (pure `TxBuilder`, **remove embedded `provider.submit`/`Future`**). Keep
  `UnfixedAuction.scala` as the intentional teaching vuln but document it clearly. Drop unused `inputValue` bindings.
- Bugs: fix cross-instance double satisfaction in the "fixed" validator (exact seller payout or burn-link per
  auction); **add a test with two genuinely-applied instances** (current "fix verification" gives false assurance).
- Refs: `AuctionValidatorTest`, `AuctionTestKitTest`, `DoubleSatisfactionAttackTest`, README.

### crowdfunding — ⚠️ Critical reclaim sweep FIXED (this branch); checked vs rosetta spec + our README
- Bug fixed (TDD, RED→GREEN): **reclaim zip-truncation / shared-output sweep**. `handleReclaimSpend` paired
  donations to refund outputs with `donationInputIndices.zip(reclaimerOutputIndices)`, which silently truncates to
  the shorter list, and never checked the outputs were distinct. An attacker could consume N donations (burning all
  N tokens, as `verifyDonationsBurned` demands) while supplying only a prefix of refund outputs — or aim several
  donations at one output — pocketing the surplus as change, with no signature required. Fix: `require` the index
  lists are equal length **and** added a `requireDistinct(reclaimerOutputIndices)` helper (pairwise-distinct,
  order-free so it doesn't constrain the off-chain output layout). Two new adversarial validator tests
  (truncation sweep; shared-output theft) demonstrate both attacks; rosetta + our README confirm reclaim must
  return every donation to its donor and may be triggered by anyone (correctness is in the routing, not a signer).
- Cleanups (touched file only): removed the duplicated `// 6.` comment and the redundant `v3.Validator` import
  (`v3.*` already covers it). No exact-`ExUnits` assertions in the suite (validator test asserts `size > 0`;
  emulator/scenario assert submit success), so nothing to re-baseline. All 8 emulator/scenario/scalacheck tests
  (incl. honest multi-donor reclaim) still pass — the fix is fail-closed for distinct donors.
- **Still open** (deferred, not security-critical theft): monolith split (`Crowdfunding.scala` 759 lines →
  Contract/Validator + `Endpoints`→`Transactions`, drop `Future`/`provider.submit`); hoist ~40 inline error
  strings; **Donate NFT/value-preservation** (the continuing campaign output's NFT isn't re-checked in
  `handleDonateSpend` — a griefing/lock vector, not direct theft); unify datum-`amount` vs lovelace accounting to
  remove the liveness lock; off-chain `reclaim` builds reclaimer indices with `indexWhere(address)` (first-match),
  correct only for distinct donors — now fail-closed on-chain for same-donor-twice, harden the builder to emit one
  distinct output per donation.
- Refs: `CrowdfundingEmulatorTest`, `CrowdfundingScalaCheckCommandTest`, `CrowdfundingScenarioTest`,
  `CrowdfundingValidatorTest`, README.

### vault — ✅ security bugs FIXED (this branch); cross-checked vs rosetta spec + our README
- Bugs fixed: **wait-time bypass** — deadline now anchored to the validity **upper** bound via the stdlib
  `tx.validRange.to.finiteOrFail(...)` (was `getValidityStartTime`, backdatable → instant finalize); off-chain
  `withdraw` sets `.validTo` accordingly. **Recovery key** — added a `recoveryKey` field to `State`; `Cancel` now
  requires the *recovery* signature (not the owner's) and a Pending request — implements the spec's core "survive a
  stolen owner key" property the README promised. **Deposit status** — `deposit` now requires `status` unchanged
  (`newDatum.status.toData == datum.status.toData`). `d.get`→`getOrFail`.
- Style/stdlib: dropped the hand-rolled `addressEquals` for `credential === Credential.PubKeyCredential(...)`;
  removed fully-qualified names (imports for `OutputDatum`, `ShelleyAddress`, `ByteString`); dropped dead
  `import scalus.*` / `ScriptCredential` / `Interval` / `Value` imports.
- Tests: added `recovery key cancels` + `owner cannot cancel` (recovery-key feature); added a `cancel` builder
  + `lockAndRequest` helper; reworked `validTo` timing; re-baselined 3 ExUnits. 7 tests pass.
- **Still open** (deferred): typed `PubKeyHash` owner/recovery instead of raw `ByteString`; one-own-input guard in
  finalize (double satisfaction across same-owner vaults); a `cancel`-only-during-wait-time bound.

### vesting — ✅ DONE (template, this branch)
- Structure: removed dead `given Eq[Config]` + redundant imports (`scalus.*`, `v3.Validator`); hoisted all error
  strings to `inline val` constants; fixed the misleading over-withdrawal message; Apache license. (Redeemer kept as
  `case class Action(amount)` per the "keep domain names" decision.)
- Bugs fixed (TDD, RED→GREEN): single-own-input guard (double satisfaction); full-`Value` preservation on the
  continuing output (native-token strip); pin continuing output to the exact own input address (staking-credential
  redirect). Three new negative tests in `VestingValidatorTest`.
- Tests: re-baselined two exact `ExUnits` budgets in `VestingTransactionTest` (cost rose because the validator now
  does more) and updated one negative test to the improved error string. README updated.
- **Not exercised by this example** (already had the 3-file structure): adding a missing `Contract`/`blueprint`
  object, `Offchain`/`Endpoints`→`Transactions` renames, monolith splits. A structural example (e.g. upgradeableproxy)
  should be done next to prove that riskier pattern before scaling.

> **Two operational notes for scaling (learned on vesting):**
> 1. **Stale incremental compilation gives false test results.** The validator tests JIT-compile the validator from
>    its SIR; after editing a validator, an incremental run can still show the *old* behavior. Always `clean` (or
>    confirm the example main recompiled) before trusting a validator test result. Factor the extra time into estimates.
> 2. **Exact `ExUnits` assertions re-baseline on every validator change.** Decide a suite-wide policy up front:
>    relax to an upper bound, or re-baseline each time. This branch re-baselined to preserve the existing style.

### storage (off-chain only)
- Structure: ✅ already conforms (`case class StorageTransactions(env, …)`, tested). Fix `// 4-byte big-endian`
  comment; consider passing only fresh UTxOs to the append loop; replace `throw new Exception` with `require`.
- Refs: `StorageTest`, README.

### simplewallet (off-chain only)
- Structure: reshape `object` with `???`/`mainnet` into `case class SimpleWalletTransactions(env: CardanoInfo)`
  (testable); rename file `SimpleWallet.scala`→`SimpleWalletTransactions.scala`. Complete `withdraw` (or fix its
  comment). Standardize on `.complete(...).sign(...)` vs `.build(changeTo=...)` across the off-chain trio.
- Refs: README only (no test today — add one).

### pricebet
- Structure: wrap both scripts in `Contract` objects with `Blueprint.plutusV3` (currently bare `def`s, no
  blueprint); fix `PriceBetContract` vs file-name casing; switch `Options.release.copy(generateErrorTraces=true)`
  → `Options.release` (or document); hoist error strings; remove unused `ZeroExchangeRateError`.
- Bugs: authenticate the oracle reference input by beacon NFT (add beacon policy/name to `PricebetConfig`) —
  the headline fix; harden `Join` against multi-input merge; validate `newState` (not inbound state) in oracle
  `Update`; fix README Win/deadline wording.
- Refs: `PricebetValidatorTest` (hardcoded ExUnits will need re-baselining).

### paymentsplitter
- Structure: refactor `PaymentSplitterContract.scala` to a `Contract` object using `Blueprint.plutusV3` (currently
  bare `lazy val`s + hand-rolled `Blueprint(...)`); rename `PaymentSplitterValidator.scala`→
  `NaivePaymentSplitterValidator.scala` (file/object mismatch); **add `PaymentSplitterTransactions.scala`**
  (off-chain currently lives only in tests); hoist error strings; remove `// TODO: think`; fix stale
  `@see StakeValidatorPaymentSplitterExample` scaladoc.
- Bugs: add native-token preservation (or reject non-ADA inputs); add `require(reminder >= 0)` to Naive.
- Refs: `NaivePaymentSplitterValidatorTest`, `OptimizedPaymentSplitterValidatorTest`, `PaymentSplitterTxBuilderTest`,
  `PaymentSplitterTestCases`, README.

### lottery — ⚠️ security bugs FIXED (this branch); checked against rosetta spec
- **Corrected the review's own framing** (verified against the rosetta Solidity reference): the winning-reveal
  branches require a *still-secret* preimage, so only the winner can execute them — no payout enforcement is needed
  there (the earlier "pot can be sent anywhere" claim was wrong for those). The reference's winner is
  `(len0 + len1) % 2` — **length-based, exactly like ours** — so the winner function is faithful and was kept
  (its cryptographic weakness is now documented, not "fixed").
- Bugs fixed (TDD): **Timeout payout theft** — the Timeout preimage is already public (revealed to reach the state),
  so anyone could claim the pot; now pinned to the stored revealer pkh. **Reveal/timeout overlap** — winning reveals
  must be `isEntirelyBefore(revealDeadline)` so they can't race a post-deadline Timeout. New theft negative test;
  ExUnits re-baselined (whole-script cost shift hit 9 assertions).
- **Documented simplifications** vs rosetta: symmetric reveal order (vs fixed P0-then-P1), single `revealDeadline`
  (vs `end_join`/`end_reveal`), atomic multisig join (no `end_join` no-show refund).
- **Still open** (style, not security): hoist error strings; factor the four near-identical reveal/lose branches
  (~120 lines dup); Apache license. The empty-state reveals DO preserve value (continuation checked) — not a bug.
- Refs: `LotteryValidatorTest`, `LotteryScenarioTest`, `LotteryScalaCheckCommandTest`.

### amm — ⚠️ Critical drain FIXED (this branch); secondary items remain
- Bug fixed (TDD): **value-preservation** — `spend` now binds the new datum reserves to the continuing pool
  output's actual token quantities (`poolOutput.value.quantityOf(t0/t1) === newDatum.r0/r1`), closing the
  full-drain hole where only the datum transition was checked. New negative test under-funds the continuation.
- **Still open** (secondary, not yet done): cross-bind spend↔mint LP deltas (spend doesn't require the LP mint to
  occur); restrict the LP asset name (the mint endpoint sums all names under the policy); validate fee params;
  hoist remaining error strings; convert `AmmContract` (bare `lazy val`, no blueprint) to `object … extends
  Contract` + `Blueprint.plutusV3`; rename `AmmOffchain.scala`→`AmmTransactions.scala`.
- Refs: `AmmTest` (`AmmContract.script`/`.withErrorTraces`, `AmmOffchain(...)`).

### upgradeableproxy — ✅ DONE (structural template, this branch)
- Structure: added `UpgradeableProxyContract.scala` (`object … extends Contract` + `Blueprint.plutusV3[ProxyDatum,
  ProxyRedeemer]`, Apache license); renamed `UpgradeableProxy.scala`→`UpgradeableProxyValidator.scala` and
  `UpgradeableProxyOffchain.scala`→`UpgradeableProxyTransactions.scala`; moved compilation out of the Transactions
  file (was `ProxyCompilation`/`lazy val ProxyContract`) into the Contract object; dropped redundant `scalus.*` +
  `v3.Validator` imports.
- Bug fixed (TDD, RED→GREEN): single-own-input guard (`findOwnInputsByCredential(...).length === 1`) closes the
  double-satisfaction hole where two proxy inputs shared one continuation. New negative test demonstrates the attack.
- Refs updated: `UpgradeableProxyTest` (`ProxyContract.withErrorTraces`→`UpgradeableProxyContract.compiled.withErrorTraces`);
  README file-name references + one-input constraint note.
- **Proved the riskier structural pattern** (add missing Contract/blueprint object + file renames + compilation move)
  on top of vesting's security+style pattern. Ready to scale to the remaining examples.

### factory
- Structure: split `FactoryExample.scala` into `FactoryValidator.scala` (`object FactoryValidator`) +
  `FactoryContract.scala` (`object FactoryContract extends Contract`); **add `FactoryTransactions.scala`**
  (README overstates off-chain coverage — there is none); replace bare `getOrFail("…")` strings with named
  constants. Error constants already mostly `inline val` ✅.
- Bugs: value/min-ADA preservation on the product output + assert exactly one script output (remove `find`-first
  ambiguity); guard `signatories.head`; reconsider redundant `validateDestroy` creator check / single-burn limit.
- Refs: `FactoryTest`; `FactoryContract.compile(FactoryExample.validate)` call site.

### decentralizedidentity
- Structure: convert `DecentralizedIdentityContract.scala` (9-line bare `lazy val`, no blueprint) to
  `object … extends Contract` + `Blueprint.plutusV3`; hoist ~30 error strings; add `@Compile object` companions;
  enum indentation style. Transactions file already correct ✅.
- Bugs: require the delegation reference input to actually hold the delegation token in `PublishAttribute`
  (token-possession check) — closes forgery + restores revocation. Optionally bind identity token name to seed hash.
- Refs: `DecentralizedIdentityTest`.

### editablenft — ✅ DONE (Critical bugs fixed, this branch)
- Bugs fixed (TDD): **seed binding** — root cause was a param-encoding mismatch (validator decoded the bare
  `TxOutRef` param as a `ReferenceNftParam` wrapper → garbage seed, masked by the old `isDefined`-only check);
  now decodes `param.to[TxOutRef]` and requires the seed is actually spent
  (`tx.inputs.at(seedIndex).outRef === seed` — O(1), and a wrong index fails closed).
  **CIP-68 labels** unswapped + real 4-byte encodings (ref = `0x000643b0`, user = `0x000de140`).
  **Hardened the empty mint `Burn` branch** — it now rejects any positive mint under the policy
  (`tx.mint.toSortedMap.get(policyId)` per-policy lookup, not `flatten`), closing a side door where
  `MintRedeemer.Burn` could mint fresh pairs and bypass the seed check entirely; the spend validator
  still enforces both-tokens-burned.
  Deduped the "Must burn ref NFT" message; hoisted all error strings to `inline val`.
- Tests: added "mint without spending seed" negative + "canonical CIP-68 labels" assertion; Burn ExUnits re-baselined.
- **Deferred:** Contract-object/blueprint conversion — the param'd dual (mint+spend) `DataParameterizedValidator`
  has a non-obvious blueprint shape; left `EditableNftContract` as the bare `lazy val`. Revisit as a focused change.

### anonymousdata
- Structure: add `AnonymousDataContract.scala` (`extends Contract` + blueprint for both main & gate — currently
  bare `lazy val`s, no blueprint); hoist error strings; split `AnonymousDataReader` into its own file; move
  `submitWithRetry` (`Thread.sleep`/`Future`) out of the Transactions builder.
- Bugs: redesign write authorization to bind `dataKey` to the signer (or a nullifier/ZK scheme) — currently any
  member can overwrite/delete any entry; add value-preservation to `findContinuingOutput`; per-version IV to avoid
  keystream reuse; fix/remove the broken `burn()` path. Honestly document the unlinkability limitation on writes.
- Refs: `AnonymousDataTest`.

### atomictransactions (off-chain only)
- Structure: reshape `object`+`???`+`mainnet` into `case class AtomicTransactions(env: CardanoInfo)` returning a
  completed `Transaction`; use consistent `Value.ada` units in the demo. Filename already fits the convention.
- Refs: README only (add a test).

### Token transfer (#3) — missing
- No native-token transfer example exists. Decide whether to add one (a `TokenTransferTransactions` off-chain
  demo, or a small validator) to complete the 21, or to document it as intentionally out of scope.

---

## 4. Suggested execution order (if changes are approved)

1. **Cross-cutting, low-risk, mechanical** (one PR): error-string `inline val` hoisting, Apache license,
   dead-code/import/comment cleanup, brace/indentation, relax brittle ExUnits assertions. No behavior change.
2. **Naming/structure alignment** (per-example PRs): rename `Offchain`/`Endpoints`/`Example` → `Transactions`/
   `Contract`/`Validator`; add missing `Contract`+`blueprint` objects; split monoliths (auction, crowdfunding);
   add missing Transactions files (simpletransfer, paymentsplitter, factory); reshape off-chain trio.
3. **Security fixes + adversarial tests** (per-example, highest value): value-preservation and single-own-input
   guards first (AMM, Lottery, Vesting, Crowdfunding, Vault, AnonymousData, EditableNft, DecentralizedIdentity,
   PriceBet, Betting, Auction, PaymentSplitter, UpgradeableProxy). Add a shared `prelude` helper + negative-test
   harness so the bug class can't regress.
