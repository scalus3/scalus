# Treasury Donation Support in TxBuilder

**Date:** 2026-07-03
**Status:** Draft — awaiting review

## Problem

Cardano's Conway era added two treasury-related fields to the transaction body:

- **`donation`** (CDDL key 23, `positive_coin`) — an amount of ADA moved into the
  treasury. Anyone can attach a donation to a transaction.
- **`current_treasury_value`** (CDDL key 22, `coin`) — a declared snapshot of the
  treasury pot at transaction time. The ledger checks it against the actual treasury,
  and it is surfaced in the Plutus script context of treasury-withdrawal governance
  actions.

Both fields are already fully modeled and CBOR-serialized in Scalus
(`TransactionBody.currentTreasuryValue` / `TransactionBody.donation`,
`scalus-core/shared/src/main/scala/scalus/cardano/ledger/TransactionBody.scala:63-66`),
and `donation` is already accounted for in value conservation
(`TxBalance.producedDonation`,
`scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/utils/TxBalance.scala:180-183`).

**The gap:** the transaction *builder* has no way to set either field. The sealed
`TransactionBuilderStep` vocabulary has no case for them, so
`TransactionStepsProcessor.processStep` cannot handle them and neither the low-level
`TransactionBuilder.build` nor the ergonomic `TxBuilder` fluent API can produce a
donation transaction. Today the only workaround is to hand-edit the `TransactionBody`
after building, bypassing the builder entirely.

## Goal

Give the builder first-class support for both Conway treasury fields, so users can write:

```scala
TxBuilder(...)
  .spend(utxo)
  .donateToTreasury(Coin(5_000_000))       // 5 ADA to the treasury
  .declareTreasuryValue(Coin(1_000_000_000)) // optional: declared pot snapshot
  .complete(...)
```

and have balancing automatically reduce the change output by the donation.

## Non-Goals

- No changes to balancing logic (donation is already handled; `currentTreasuryValue`
  is informational and does not affect value conservation).
- No treasury-withdrawal *governance action* work — that is a separate concern
  (`GovAction.TreasuryWithdrawals` already exists and is submitted via `SubmitProposal`).
- No offline validation that `currentTreasuryValue` matches the real treasury pot
  (the builder cannot know the pot; the ledger enforces this at submission).

## Design

The feature mirrors the existing single-field steps (`Fee`, `ValidityStartSlot`,
`ValidityEndSlot`) across four layers. Each step sets one body field once, with an
"already set" guard.

### 1. `TransactionBuilderStep` (new step cases)

File: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionBuilderStep.scala`

```scala
/** Donate ADA to the Cardano treasury (Conway `donation`, CDDL key 23).
  * Additive is NOT supported — the donation may be set only once.
  * The amount is reflected in value conservation, so the change output shrinks
  * by this amount automatically during balancing.
  */
case class Donate(amount: Coin) extends TransactionBuilderStep {
    require(amount.value > 0, "Donation amount must be positive")
}

/** Declare the current treasury value (Conway `current_treasury_value`, CDDL key 22).
  * Informational: the ledger checks it against the real treasury pot and exposes it
  * to treasury-withdrawal governance script contexts. Does not affect balancing.
  * May be set only once.
  */
case class DeclareCurrentTreasuryValue(value: Coin) extends TransactionBuilderStep
```

- `Donate` uses a construction-time `require`, matching `Mint`'s zero-guard. The CBOR
  layer independently rejects zero donations, so this keeps behavior consistent.
- `DeclareCurrentTreasuryValue` accepts any non-negative `Coin`.

### 2. `TransactionStepsProcessor` (dispatch + handlers)

File: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionStepsProcessor.scala`

Add two dispatch cases in `processStep`:

```scala
case donate: Donate =>
    useDonate(donate)

case declare: DeclareCurrentTreasuryValue =>
    useDeclareCurrentTreasuryValue(declare)
```

Add two handlers following the `useFee` pattern:

```scala
private def useDonate(step: Donate): Result[Unit] =
    ctx.transaction.body.value.donation match {
        case Some(existing) => Left(DonationAlreadySet(existing, step))
        case None =>
            modify0(unsafeCtxBodyL.refocus(_.donation).replace(Some(step.amount)))
            Ok
    }

private def useDeclareCurrentTreasuryValue(
    step: DeclareCurrentTreasuryValue
): Result[Unit] =
    ctx.transaction.body.value.currentTreasuryValue match {
        case Some(existing) => Left(CurrentTreasuryValueAlreadySet(existing, step))
        case None =>
            modify0(unsafeCtxBodyL.refocus(_.currentTreasuryValue).replace(Some(step.value)))
            Ok
    }
```

### 3. `StepError` (new error variants)

File: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/StepError.scala`

```scala
case class DonationAlreadySet(current: Coin, step: TransactionBuilderStep)
    extends StepError {
    override def explain: String =
        s"A treasury donation (${current.value}) is already set. " +
            "You cannot set the donation more than once."
}

case class CurrentTreasuryValueAlreadySet(current: Coin, step: TransactionBuilderStep)
    extends StepError {
    override def explain: String =
        s"The current treasury value (${current.value}) is already set. " +
            "You cannot set it more than once."
}
```

### 4. `TxBuilder` fluent API

File: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala`
(place near `withdrawRewards`, ~line 1072)

```scala
/** Donate ADA to the Cardano treasury. See [[TransactionBuilderStep.Donate]]. */
def donateToTreasury(amount: Coin): TxBuilder =
    addSteps(TransactionBuilderStep.Donate(amount))

/** Declare the current treasury value.
  * See [[TransactionBuilderStep.DeclareCurrentTreasuryValue]].
  */
def declareTreasuryValue(value: Coin): TxBuilder =
    addSteps(TransactionBuilderStep.DeclareCurrentTreasuryValue(value))
```

## Data Flow

1. User calls `donateToTreasury(amount)` → appends a `Donate` step.
2. `TransactionStepsProcessor` sets `body.donation = Some(amount)`.
3. During balancing, `TxBalance.produced` already includes the donation, so
   `calculateChangeValue` yields a smaller change and the change output shrinks by
   `amount`. No balancer change required.
4. `declareTreasuryValue(value)` sets `body.currentTreasuryValue`; it does not
   participate in balancing.

## Error Handling

- Setting either field twice returns `DonationAlreadySet` /
  `CurrentTreasuryValueAlreadySet` (wrapped as `SomeStepError` by `TransactionBuilder.build`).
- A zero or negative donation throws `IllegalArgumentException` at step construction
  (consistent with `Mint`).
- Insufficient funds to cover donation + fee surfaces through the existing
  `TxBalancingError.InsufficientFunds` path — no new handling needed.

## Testing

Unit tests (extend `TransactionBuilderTests` / `TxBuilderTest`):

- `Donate` sets `body.donation`.
- `DeclareCurrentTreasuryValue` sets `body.currentTreasuryValue`.
- Two `Donate` steps → `DonationAlreadySet`.
- Two `DeclareCurrentTreasuryValue` steps → `CurrentTreasuryValueAlreadySet`.
- `Donate(Coin.zero)` throws `IllegalArgumentException`.
- Fluent `donateToTreasury` / `declareTreasuryValue` produce the expected steps.

Integration / balancing test (existing `TxBuilder` + evaluator harness):

- Build and balance a transaction with a donation; assert the change output is reduced
  by exactly the donation amount and the balanced transaction passes value-conservation
  validation.
- Round-trip: build a donation transaction, serialize to CBOR, decode, and confirm
  `donation` / `currentTreasuryValue` survive.

## Compatibility

Additive only: new sealed-trait cases, new `StepError` cases, new `TxBuilder` methods.
Binary-compatible (MiMa-safe). Verify with `sbtn mima`.
```

Note: the `docs/superpowers/specs/` folder is a project-standard location for these
specs (see the existing blueprint-caching design doc). Run `git add` on the new file.
