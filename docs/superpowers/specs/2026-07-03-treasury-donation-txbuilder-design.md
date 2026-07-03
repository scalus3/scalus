# Treasury Donation Support in TxBuilder

**Date:** 2026-07-03
**Status:** Draft — awaiting review

## Problem

Cardano's Conway era added two treasury-related fields to the transaction body:

- **`donation`** (CDDL key 22, `positive_coin`) — an amount of ADA moved into the
  treasury. Anyone can attach a donation to a transaction.
- **`current_treasury_value`** (CDDL key 21, `coin`) — a declared snapshot of the
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
  .setCurrentTreasuryValue(Coin(1_000_000_000)) // optional: declared pot snapshot
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
/** Donate ADA to the Cardano treasury (Conway `donation`, CDDL key 22).
  * Additive is NOT supported — the donation may be set only once.
  * The amount is reflected in value conservation, so the change output shrinks
  * by this amount automatically during balancing.
  */
case class Donate(amount: Coin) extends TransactionBuilderStep {
    require(amount.value > 0, "Donation amount must be positive")
}

/** Declare the current treasury value (Conway `current_treasury_value`, CDDL key 21).
  * Informational: the ledger checks it against the real treasury pot and exposes it
  * to treasury-withdrawal governance script contexts. Does not affect balancing.
  * May be set only once.
  */
case class SetCurrentTreasuryValue(value: Coin) extends TransactionBuilderStep
```

- `Donate` uses a construction-time `require`, matching `Mint`'s zero-guard. The CBOR
  layer independently rejects zero donations, so this keeps behavior consistent.
- `SetCurrentTreasuryValue` accepts any non-negative `Coin`. Named to pair with the
  existing `SetCollateralReturn` step and the CML/cardano-sdk/CCL `setCurrentTreasuryValue`
  token (see Naming rationale below).

### 2. `TransactionStepsProcessor` (dispatch + handlers)

File: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionStepsProcessor.scala`

Add two dispatch cases in `processStep`:

```scala
case donate: Donate =>
    useDonate(donate)

case setValue: SetCurrentTreasuryValue =>
    useSetCurrentTreasuryValue(setValue)
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

private def useSetCurrentTreasuryValue(
    step: SetCurrentTreasuryValue
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
  * See [[TransactionBuilderStep.SetCurrentTreasuryValue]].
  */
def setCurrentTreasuryValue(value: Coin): TxBuilder =
    addSteps(TransactionBuilderStep.SetCurrentTreasuryValue(value))
```

## Data Flow

1. User calls `donateToTreasury(amount)` → appends a `Donate` step.
2. `TransactionStepsProcessor` sets `body.donation = Some(amount)`.
3. During balancing, `TxBalance.produced` already includes the donation, so
   `calculateChangeValue` yields a smaller change and the change output shrinks by
   `amount`. No balancer change required.
4. `setCurrentTreasuryValue(value)` sets `body.currentTreasuryValue`; it does not
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
- `SetCurrentTreasuryValue` sets `body.currentTreasuryValue`.
- Two `Donate` steps → `DonationAlreadySet`.
- Two `SetCurrentTreasuryValue` steps → `CurrentTreasuryValueAlreadySet`.
- `Donate(Coin.zero)` throws `IllegalArgumentException`.
- Fluent `donateToTreasury` / `setCurrentTreasuryValue` produce the expected steps.

Integration / balancing test (existing `TxBuilder` + evaluator harness):

- Build and balance a transaction with a donation; assert the change output is reduced
  by exactly the donation amount and the balanced transaction passes value-conservation
  validation.
- Round-trip: build a donation transaction, serialize to CBOR, decode, and confirm
  `donation` / `currentTreasuryValue` survive.

## Naming rationale

The public method names were chosen for maximum familiarity to users coming from other
Cardano tx-building libraries, verified against each library's source (2026-07-03):

| Library | Donation (key 22) | Current treasury value (key 21) |
|---|---|---|
| Lucid Evolution | `donateToTreasury(donation, currentTreasuryValue?)` (combined) | 2nd param of same call |
| Blaze | `setDonation(bigint)` | not on builder |
| Cardano Client Lib | `TransactionBody.setDonation(BigInteger)` | `setCurrentTreasuryValue(BigInteger)` |
| Evolution SDK | data model only (`donation`) | data model only (`currentTreasuryValue`) |
| MeshJS | not supported | not supported |
| CML (underlies Blaze/Lucid/cardano-sdk) | `set_donation` | `set_current_treasury_value` |

The *field* names (`donation`, `currentTreasuryValue`) are universal across the
ecosystem, so the data-model/step field names match everyone.

For the fluent methods:

- **`donateToTreasury`** matches Lucid Evolution — the closest analog to Scalus's
  chainable `TxBuilder` — and its verb form fits Scalus's existing idiom (`payTo`,
  `withdrawRewards`) better than the `setX` style used by Blaze/CML/CCL.
- **`setCurrentTreasuryValue`** matches the exact CML / cardano-sdk / Cardano Client Lib
  token, and the `SetCurrentTreasuryValue` step name pairs with the existing
  `SetCollateralReturn` step.

We keep two independent methods (rather than Lucid's single combined call) because the
two Conway fields are independent concerns and this matches Scalus's one-field-per-step
convention; the second is not forced whenever the first is used.

## Compatibility

Additive only: new sealed-trait cases, new `StepError` cases, new `TxBuilder` methods.
Binary-compatible (MiMa-safe). Verify with `sbtn mima`.
