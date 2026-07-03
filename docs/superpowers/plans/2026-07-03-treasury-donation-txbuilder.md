# Treasury Donation Support in TxBuilder — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let the transaction builder set the Conway `donation` and `current_treasury_value` fields, so users can donate ADA to the Cardano treasury.

**Architecture:** Add two single-field builder steps (`Donate`, `SetCurrentTreasuryValue`) mirroring the existing `Fee`/`ValidityStartSlot` pattern: a step case in `TransactionBuilderStep`, a dispatch case + handler in `TransactionStepsProcessor`, an "already set" `StepError`, and a fluent method on `TxBuilder`. The balancer already accounts for `donation` in value conservation, so no balancing changes are needed.

**Tech Stack:** Scala 3, sbt (`sbtn`), ScalaTest (`AnyFunSuite`), monocle lenses, borer CBOR.

## Global Constraints

- Scala 3 style per `CLAUDE.md`: braces for top-level/multi-line bodies; `then`/`do`; indentation for short `if`/`match`.
- Run `sbtn scalafmtAll` before every commit (ci runs `scalafmtCheckAll`).
- Do NOT add any `Co-Authored-By` trailer to commits.
- Conventional commit prefixes (`feat:`, `test:`, etc.).
- Additive change only — keep binary compatibility (verify with `sbtn mima`).
- Design reference: `docs/superpowers/specs/2026-07-03-treasury-donation-txbuilder-design.md`.

---

### Task 1: Low-level plumbing — steps, errors, processor handlers

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionBuilderStep.scala`
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/StepError.scala`
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionStepsProcessor.scala`
- Test: `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TransactionBuilderTests.scala`

**Interfaces:**
- Consumes: `TransactionBuilder.build(network, steps): Either[SomeBuildError, Context]`; `Context.transaction.body.value.{donation, currentTreasuryValue}: Option[Coin]`; `modify0`, `unsafeCtxBodyL`, `ctx`, `Ok` (in-scope inside the processor); `Coin`, `Coin.zero`, `Coin.ada`.
- Produces:
  - `TransactionBuilderStep.Donate(amount: Coin)` — sets body `donation`.
  - `TransactionBuilderStep.SetCurrentTreasuryValue(value: Coin)` — sets body `currentTreasuryValue`.
  - `StepError.DonationAlreadySet(current: Coin, step: TransactionBuilderStep)`.
  - `StepError.CurrentTreasuryValueAlreadySet(current: Coin, step: TransactionBuilderStep)`.

Note: adding a case to the sealed `TransactionBuilderStep` makes `processStep`'s match non-exhaustive until the dispatch cases are added, so the step, errors, and processor changes are implemented together in this one task before compiling.

- [ ] **Step 1: Write the failing tests**

Add to `TransactionBuilderTests.scala`. `Donate`, `SetCurrentTreasuryValue`, `DonationAlreadySet`, `CurrentTreasuryValueAlreadySet` are already in scope via the existing `import ...TransactionBuilderStep.{Mint, *}` and `import ...StepError.*`. `Cbor`, `Mainnet`, `Coin`, `Transaction` are already imported in this file.

```scala
// ---- Treasury donation steps ----

test("Donate sets the transaction body donation field") {
    val ctx = TransactionBuilder.build(Mainnet, Seq(Donate(Coin.ada(5)))).toOption.get
    assert(ctx.transaction.body.value.donation == Some(Coin.ada(5)))
}

test("SetCurrentTreasuryValue sets the current treasury value field") {
    val ctx = TransactionBuilder
        .build(Mainnet, Seq(SetCurrentTreasuryValue(Coin.ada(1000))))
        .toOption
        .get
    assert(ctx.transaction.body.value.currentTreasuryValue == Some(Coin.ada(1000)))
}

testBuilderStepsFail(
  label = "Donate twice fails with DonationAlreadySet",
  steps = Seq(Donate(Coin(1_000_000)), Donate(Coin(2_000_000))),
  error = DonationAlreadySet(Coin(1_000_000), Donate(Coin(2_000_000)))
)

testBuilderStepsFail(
  label = "SetCurrentTreasuryValue twice fails with CurrentTreasuryValueAlreadySet",
  steps = Seq(
    SetCurrentTreasuryValue(Coin(10)),
    SetCurrentTreasuryValue(Coin(20))
  ),
  error = CurrentTreasuryValueAlreadySet(Coin(10), SetCurrentTreasuryValue(Coin(20)))
)

test("Donate with zero amount throws IllegalArgumentException") {
    assertThrows[IllegalArgumentException] {
        Donate(Coin.zero)
    }
}

test("donation and currentTreasuryValue survive a CBOR round-trip") {
    val ctx = TransactionBuilder
        .build(
          Mainnet,
          Seq(Donate(Coin.ada(5)), SetCurrentTreasuryValue(Coin.ada(1000)))
        )
        .toOption
        .get
    val bytes = Cbor.encode(ctx.transaction).toByteArray
    val decoded = Cbor.decode(bytes).to[Transaction].value
    assert(decoded.body.value.donation == Some(Coin.ada(5)))
    assert(decoded.body.value.currentTreasuryValue == Some(Coin.ada(1000)))
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly scalus.cardano.txbuilder.TransactionBuilderTest"`
Expected: FAIL — compilation error, `Donate`/`SetCurrentTreasuryValue`/`DonationAlreadySet`/`CurrentTreasuryValueAlreadySet` not found.

- [ ] **Step 3: Add the step case classes**

In `TransactionBuilderStep.scala`, add these two cases inside `object TransactionBuilderStep`, immediately after the `Deferred` case class (before the closing `}` of the object):

```scala
    /** Donate ADA to the Cardano treasury (Conway `donation`, CDDL key 23).
      *
      * May be set only once. The amount participates in value conservation, so the change
      * output automatically shrinks by this amount during balancing.
      *
      * @throws IllegalArgumentException
      *   if amount is not positive
      */
    case class Donate(amount: Coin) extends TransactionBuilderStep {
        require(amount.value > 0, "Donation amount must be positive")
    }

    /** Declare the current treasury value (Conway `current_treasury_value`, CDDL key 22).
      *
      * Informational: the ledger checks it against the real treasury pot and exposes it to
      * treasury-withdrawal governance script contexts. Does not affect balancing. May be set
      * only once.
      */
    case class SetCurrentTreasuryValue(value: Coin) extends TransactionBuilderStep
```

- [ ] **Step 4: Add the StepError variants**

In `StepError.scala`, add inside `object StepError`, immediately after the `ValidityEndSlotAlreadySet` case class:

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

- [ ] **Step 5: Add processor dispatch cases**

In `TransactionStepsProcessor.scala`, in `processStep` (the `step match { ... }`), add these two cases immediately before the final `case _: TransactionBuilderStep.Deferred =>` case:

```scala
        case donate: Donate =>
            useDonate(donate)

        case setValue: SetCurrentTreasuryValue =>
            useSetCurrentTreasuryValue(setValue)
```

- [ ] **Step 6: Add processor handlers**

In `TransactionStepsProcessor.scala`, add these two private methods immediately after `useValidityEndSlot` (mirrors `useFee`):

```scala
    // -------------------------------------------------------------------------
    // Treasury donation steps
    // -------------------------------------------------------------------------

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
                modify0(
                  unsafeCtxBodyL.refocus(_.currentTreasuryValue).replace(Some(step.value))
                )
                Ok
        }
```

- [ ] **Step 7: Format**

Run: `sbtn scalafmtAll`
Expected: success, no diff on unrelated files.

- [ ] **Step 8: Run tests to verify they pass**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly scalus.cardano.txbuilder.TransactionBuilderTest"`
Expected: PASS — all six new tests green, no non-exhaustive-match warning.

- [ ] **Step 9: Commit**

```bash
git add scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionBuilderStep.scala \
        scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/StepError.scala \
        scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TransactionStepsProcessor.scala \
        scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TransactionBuilderTests.scala
git commit -m "feat(txbuilder): add treasury donation and current-treasury-value steps"
```

---

### Task 2: Fluent `TxBuilder` API + end-to-end balancing test

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala` (near `withdrawRewards`, ~line 1112)
- Test: `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TxBuilderTest.scala`

**Interfaces:**
- Consumes: `TransactionBuilderStep.Donate`, `TransactionBuilderStep.SetCurrentTreasuryValue` (from Task 1); `TxBuilder.addSteps(s: TransactionBuilderStep*): TxBuilder`; `txBuilder` factory (needs `given CardanoInfo`); `TxBuilder.build(changeTo: Address): TxBuilder`; `.transaction`; `genAdaOnlyPubKeyUtxo(party, min): Gen[Utxo]` where `Utxo(input, output)` (`utxo.output.value.coin.value`).
- Produces:
  - `TxBuilder.donateToTreasury(amount: Coin): TxBuilder`.
  - `TxBuilder.setCurrentTreasuryValue(value: Coin): TxBuilder`.

- [ ] **Step 1: Write the failing test**

Add to `TxBuilderTest.scala`. `txBuilder`, `Alice`, `genAdaOnlyPubKeyUtxo`, `Coin` are already imported; `given testEnv: CardanoInfo = CardanoInfo.mainnet` is already in the class.

```scala
test("donateToTreasury sets donation and reduces change, conserving value") {
    val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
    val donation = Coin.ada(5)

    val tx = txBuilder
        .spend(utxo)
        .donateToTreasury(donation)
        .setCurrentTreasuryValue(Coin.ada(1000))
        .build(changeTo = Alice.address)
        .transaction

    assert(tx.body.value.donation == Some(donation))
    assert(tx.body.value.currentTreasuryValue == Some(Coin.ada(1000)))

    // Value conservation: inputs == outputs + fee + donation
    val inputCoin = utxo.output.value.coin.value
    val outputsCoin = tx.body.value.outputs.map(_.value.value.coin.value).sum
    val fee = tx.body.value.fee.value
    assert(inputCoin == outputsCoin + fee + donation.value)

    // The completed (submittable) transaction serializes with the donation intact.
    // NOTE: only a COMPLETED tx round-trips — a raw from-scratch `TransactionBuilder.build`
    // output cannot be Cbor.encode'd (empty inputs / KeepRaw), which is unrelated to treasury.
    val decoded = Transaction.fromCbor(tx.toCbor)
    assert(decoded.body.value.donation == Some(donation))
    assert(decoded.body.value.currentTreasuryValue == Some(Coin.ada(1000)))
}
```

`Transaction` is already imported via `scalus.cardano.ledger.*`; `tx.toCbor` and
`Transaction.fromCbor` are the production serialization path (used by `BlockfrostProvider`).

- [ ] **Step 2: Run test to verify it fails**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly scalus.cardano.txbuilder.TxBuilderTest -- -z donateToTreasury"`
Expected: FAIL — compilation error, `donateToTreasury`/`setCurrentTreasuryValue` not members of `TxBuilder`.

- [ ] **Step 3: Add the fluent methods**

In `TxBuilder.scala`, add immediately after the second `withdrawRewards` overload (the one taking a `ScriptWitness`):

```scala
    /** Donate ADA to the Cardano treasury.
      *
      * See [[TransactionBuilderStep.Donate]]. The donation reduces the change output during
      * balancing.
      *
      * @param amount
      *   the amount to donate (must be positive)
      */
    def donateToTreasury(amount: Coin): TxBuilder =
        addSteps(TransactionBuilderStep.Donate(amount))

    /** Declare the current treasury value.
      *
      * See [[TransactionBuilderStep.SetCurrentTreasuryValue]]. Informational only; does not
      * affect balancing.
      *
      * @param value
      *   the declared treasury value
      */
    def setCurrentTreasuryValue(value: Coin): TxBuilder =
        addSteps(TransactionBuilderStep.SetCurrentTreasuryValue(value))
```

- [ ] **Step 4: Format**

Run: `sbtn scalafmtAll`
Expected: success. NOTE: `scalafmtAll` formats the whole repo and reflows some
pre-existing unrelated files (e.g. under `scalus-examples/platform/`). After formatting,
run `git status --porcelain` and `git checkout --` any file outside the two in Step 7 so
they are NOT part of this commit.

- [ ] **Step 5: Run test to verify it passes**

Run: `sbtn "scalusCardanoLedgerJVM/testOnly scalus.cardano.txbuilder.TxBuilderTest -- -z donateToTreasury"`
Expected: PASS.

- [ ] **Step 6: Verify binary compatibility**

Run: `sbtn mima`
Expected: PASS — no incompatibilities (all changes are additive).

- [ ] **Step 7: Commit**

```bash
git add scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/TxBuilder.scala \
        scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/txbuilder/TxBuilderTest.scala
git commit -m "feat(txbuilder): add donateToTreasury and setCurrentTreasuryValue fluent methods"
```

---

## Self-Review

**Spec coverage:**
- Step cases `Donate` / `SetCurrentTreasuryValue` → Task 1, Step 3. ✅
- Processor dispatch + handlers → Task 1, Steps 5-6. ✅
- `StepError` variants → Task 1, Step 4. ✅
- Fluent `donateToTreasury` / `setCurrentTreasuryValue` → Task 2, Step 3. ✅
- No balancer change (donation already in `TxBalance.produced`) → asserted by Task 2 value-conservation test. ✅
- Testing: field-set, double-set errors, zero rejection, CBOR round-trip (Task 1); fluent wiring + balancing/value-conservation (Task 2). ✅
- MiMa compatibility → Task 2, Step 6. ✅

**Placeholder scan:** none — every code step shows complete code and every run step shows the command and expected result.

**Type consistency:** `Donate(amount: Coin)`, `SetCurrentTreasuryValue(value: Coin)`, `DonationAlreadySet(current, step)`, `CurrentTreasuryValueAlreadySet(current, step)`, `donateToTreasury(amount)`, `setCurrentTreasuryValue(value)` are named identically across the spec, Task 1, and Task 2. Body fields `donation` / `currentTreasuryValue` match `TransactionBody`. ✅
