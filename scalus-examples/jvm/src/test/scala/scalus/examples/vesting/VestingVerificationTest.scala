package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.===
import scalus.cardano.onchain.plutus.v3.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.{Interval, IntervalBoundType, OutputDatum, ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOut, TxOutRef}
import scalus.uplc.Constant
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, CallsAsUnknowns, LeanProofs, LeanProofsSuite, Unfinished, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** Properties of [[VestingValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * Three groups of statements, from the most general to the most specific:
  *   - the vesting schedule, `linearVesting`, and its contract, for every datum and time, and the
  *     contracts of the library's `Value` that the validator relies on;
  *   - the validator on every script context, of any transaction: what it rejects before it reads
  *     the transaction;
  *   - withdrawals: who may withdraw, how much, what the validator's call of the schedule owes, and
  *     that everything can be withdrawn after the end. Each is stated of every script context,
  *     under the plain name; where Lean does not finish one, also of the contexts of a few inputs
  *     and outputs, under the name with `Limited`, the two side by side. Sample withdrawals,
  *     decided by running the validator, show that it accepts some.
  *
  * Two limits of what is proved:
  *   - the validator is compiled here with the tactic's options (`UplcBlaster.options`), which
  *     differ from those of [[VestingContract]]: Lean's model has no `Value` builtins yet. The
  *     statements are about the same source, not about the published script's bytes;
  *   - the validator walks the inputs and the outputs, and `UplcBlaster` cannot follow a loop over
  *     a list of unknown length. Of every context, the first, third and fourth guarantees are
  *     proved with the walks as unknowns (`CallsAsUnknowns`), resting on the tie of the validator
  *     to its program with unknowns (`ProofKind.BlasterOfUnknowns`); the fourth also on the
  *     contracts the library's `Value` functions state, which are proved here: those of `eq` and
  *     `lovelace` by `UplcBlaster`, and `minus`'s by hand, in the example's workspace. That
  *     everything can be withdrawn after the end is open.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]). The tests tagged `Unfinished` are about what Lean does
  * not finish, and keep nothing: they need Lean.
  */
class VestingVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    import VestingVerificationTest.*

    /** The example's own Lean workspace, `src/test/lean/LinearVesting`. It requires Scalus's Lean
      * library, and has that library's packages where the library's workspace has them.
      */
    override protected def leanWorkspace: Path = VestingVerificationTest.workspace

    /** The results of the statements below are kept beside this file: a run without Lean passes on
      * them, and sees at once where the validator's code is no longer the one they are of.
      */
    override protected def keptResultsFile: Option[Path] = Some(
      LeanProofs
          .inSources(
            "scalus-examples",
            "jvm",
            "src",
            "test",
            "scala",
            "scalus",
            "examples",
            "vesting"
          )
          .resolve("VestingVerificationTest.proofs.json")
    )

    // The schedule

    private val linearVesting = FunctionDef(VestingValidator.linearVesting)

    verifier.addFunction(linearVesting)

    /** The steps Lean's CEK may make per test of a statement about the schedule. It has no loop, so
      * its step count does not depend on its input.
      */
    private val scheduleBudget = 400

    private val zeroBeforeStart = verifier.statement(
      "vesting.schedule.zero_before_start",
      forAll[Config, BigInt]((config, time) =>
          (time < config.startTimestamp) ==>:
              (VestingValidator.linearVesting(config, time) == BigInt(0))
      )
    )

    test("nothing is vested before the start") {
        val result = verify(zeroBeforeStart, scheduleBudget)
        assert(result.meets(zeroBeforeStart), explain(zeroBeforeStart, result))
    }

    private val everythingFromTheEnd = verifier.statement(
      "vesting.schedule.everything_from_the_end",
      forAll[Config, BigInt]((config, time) =>
          (config.startTimestamp <= time
              && config.startTimestamp + config.duration <= time) ==>:
              (VestingValidator.linearVesting(config, time) == config.initialAmount)
      )
    )

    test("everything is vested from the end of the period on") {
        val result = verify(everythingFromTheEnd, scheduleBudget)
        assert(result.meets(everythingFromTheEnd), explain(everythingFromTheEnd, result))
    }

    private val scheduleReturns = verifier.statement(
      "vesting.schedule.returns",
      forAll[Config, BigInt]((config, time) =>
          succeeds(VestingValidator.linearVesting(config, time))
      )
    )

    test("the schedule returns for every datum and time") {
        // Whatever the datum's numbers are: it does not divide by a zero duration.
        val result = verify(scheduleReturns, scheduleBudget)
        assert(result.meets(scheduleReturns), explain(scheduleReturns, result))
    }

    /** The contract `linearVesting` states in its body, and that it returns: none where it states
      * no contract.
      */
    private val statedContractHolds = FunctionContract
        .inSource(linearVesting)
        .map(stated =>
            // A statement of the contract, and not its `prop`: so the verifier knows that it checks
            // the `ensuring` clause it was read from.
            verifier.statement(
              "vesting.schedule.stated_contract_holds",
              stated.returnsWhen((config, time) => true)
            )
        )

    private val vestedBelowTheAmount = verifier.refute(
      "vesting.schedule.vested_below_the_amount",
      contract(linearVesting)(
        expects = (config, time) => config.initialAmount >= BigInt(0),
        ensures = (config, time) => vested => vested < config.initialAmount
      ).prop
    )

    private val boundedWithoutCondition = verifier.refute(
      "vesting.schedule.bounded_without_condition",
      contract(linearVesting)(
        expects = (config, time) => true,
        ensures = (config, time) => vested => vested <= config.initialAmount
      ).prop
    )

    test("the schedule's contract, stated in its body, holds of its compiled program") {
        // `linearVesting` ends in `.ensuring(vested => ...)`: for an initial amount that is not
        // negative, what is vested lies between nothing and that amount. The verifier reads the
        // clause from the function's SIR. It owes its callers nothing, and here it is also shown
        // to return.
        val stated = statedContractHolds.getOrElse(fail("linearVesting states no contract"))
        val contract = verify(stated, scheduleBudget)
        assert(contract.meets(stated), explain(stated, contract))
        // negative control: a wrong postcondition. From the end on, everything is vested.
        val belowTheAmount = verify(vestedBelowTheAmount, scheduleBudget)
        assert(
          belowTheAmount.meets(vestedBelowTheAmount),
          explain(vestedBelowTheAmount, belowTheAmount)
        )
        // negative control: the bound needs its condition. A negative initial amount is below
        // what is vested before the start.
        val withoutCondition = verify(boundedWithoutCondition, scheduleBudget)
        assert(
          withoutCondition.meets(boundedWithoutCondition),
          explain(boundedWithoutCondition, withoutCondition)
        )
    }

    private val doesNotDecrease = verifier.statement(
      "vesting.schedule.does_not_decrease",
      forAll[Config, BigInt, BigInt]((config, earlier, later) =>
          (config.initialAmount >= BigInt(0) && earlier <= later) ==>:
              (VestingValidator.linearVesting(config, earlier) <=
                  VestingValidator.linearVesting(config, later))
      )
    )

    private val doesNotDecreaseForAnyAmount = verifier.refute(
      "vesting.schedule.does_not_decrease_for_any_amount",
      forAll[Config, BigInt, BigInt]((config, earlier, later) =>
          (earlier <= later) ==>:
              (VestingValidator.linearVesting(config, earlier) <=
                  VestingValidator.linearVesting(config, later))
      )
    )

    test("the vested amount does not decrease with time") {
        val holds = verify(doesNotDecrease, scheduleBudget)
        assert(holds.meets(doesNotDecrease), explain(doesNotDecrease, holds))
        // negative control: a negative initial amount is taken away over time
        val forAnyAmount = verify(doesNotDecreaseForAnyAmount, scheduleBudget)
        assert(
          forAnyAmount.meets(doesNotDecreaseForAnyAmount),
          explain(doesNotDecreaseForAnyAmount, forAnyAmount)
        )
    }

    // The library's `Value`

    // The fourth guarantee assumes the contracts that `Value.eq`, `Value.minus` and
    // `Value.lovelace` state in their code, at each call of them
    // (docs/design/verification-details/call-rule.md), and its proof cites those proved here. Of a
    // value, `getLovelace` reads the first entry only: the ada policy and token name are the empty
    // bytes, the least keys, so the search of a map for them ends at its first entry. Lean runs
    // `lovelace` and `eq` to their ends on values of any size, and proves their contracts.
    // `minus` walks both values to their ends, which no run of a budget follows: its contract is
    // proved by hand in the example's workspace, by induction over the maps.

    private val lovelace = FunctionDef(Value.lovelace)
    private val valueEq = FunctionDef(Value.eq)
    private val valueMinus = FunctionDef(Value.minus)

    verifier.addFunction(lovelace)
    verifier.addFunction(valueEq)
    verifier.addFunction(valueMinus)

    /** The steps of the contract of `lovelace`, and of `eq`: those `Budget.Auto` came to
      * (2026-11-27).
      */
    private val lovelaceBudget = 425
    private val valueEqBudget = 687

    /** The steps of a negative control of a contract: its first counterexample is real. */
    private val libraryControlBudget = 100

    /** The contracts `lovelace` and `eq` state in their code, and that they return, of every amount
      * and of every two values.
      */
    private val lovelaceContractHolds = FunctionContract
        .inSource(lovelace)
        .map(stated =>
            verifier
                .statement("value.lovelace.stated_contract_holds", stated.returnsWhen(v => true))
        )

    private val valueEqContractHolds = FunctionContract
        .inSource(valueEq)
        .map(stated =>
            verifier.statement(
              "value.eq.stated_contract_holds",
              stated.returnsWhen((a, b) => true)
            )
        )

    /** The contract `minus` states in its code, and that it returns, of every two values. */
    private val valueMinusContractHolds = FunctionContract
        .inSource(valueMinus)
        .map(stated =>
            verifier.statement(
              "value.minus.stated_contract_holds",
              stated.returnsWhen((a, b) => true)
            )
        )

    /** The proof by hand of `minus`'s contract, of runs of any number of steps:
      * `src/test/lean/LinearVesting/LinearVesting/Proofs/ValueMinus.lean`.
      */
    private def proveValueMinus(stated: Statement): VerificationResult =
        verifyByHand(
          stated,
          "LinearVesting.Proofs.ValueMinus",
          "LinearVesting.Proofs.ValueMinus.minus_contract"
        )

    private val lovelaceIsNotZero = verifier.refute(
      "value.lovelace.is_not_zero",
      contract(lovelace)(expects = v => true, ensures = v => value => !value.isZero).prop
    )

    private val equalLovelaceIsEqual = verifier.refute(
      "value.eq.equal_lovelace_is_equal",
      contract(valueEq)(
        expects = (a, b) => true,
        ensures = (a, b) => equal => equal || !(a.getLovelace === b.getLovelace)
      ).prop
    )

    test("Value.lovelace's contract, stated in its code, holds of every amount") {
        val stated = lovelaceContractHolds.getOrElse(fail("Value.lovelace states no contract"))
        val result = verify(stated, lovelaceBudget)
        assert(result.meets(stated), explain(stated, result))
        // negative control: of no lovelace, the value is zero
        val notZero = verify(lovelaceIsNotZero, libraryControlBudget)
        assert(notZero.meets(lovelaceIsNotZero), explain(lovelaceIsNotZero, notZero))
    }

    /** A sample of the contract stated of the sum: `blaster-uplc` refutes no statement of `minus`
      * of every two values, which walks them to their ends, so the control is of one, which Lean
      * runs.
      */
    private val minusOfTheSum = verifier.refute(
      "value.minus.lovelace_of_the_sum",
      Value.minus(Value.lovelace(BigInt(2)), Value.lovelace(BigInt(1))).getLovelace === BigInt(3)
    )

    /** The steps of the run of `minus` on the sample of its control. */
    private val minusControlBudget = 2000

    test("Value.eq's contract, stated in its code, holds of every two values") {
        val stated = valueEqContractHolds.getOrElse(fail("Value.eq states no contract"))
        val result = verify(stated, valueEqBudget)
        assert(result.meets(stated), explain(stated, result))
        // negative control: the converse, as values of the same lovelace may hold other tokens
        val converse = verify(equalLovelaceIsEqual, libraryControlBudget)
        assert(converse.meets(equalLovelaceIsEqual), explain(equalLovelaceIsEqual, converse))
    }

    test("Value.minus's contract, stated in its code, holds of every two values") {
        // Of values of any size, so of runs of any number of steps: proved by hand, on Lean's own
        // axioms (`ProofKind.LeanKernel`).
        val stated = valueMinusContractHolds.getOrElse(fail("Value.minus states no contract"))
        val result = proveValueMinus(stated)
        assert(result.meets(stated), explain(stated, result))
        assert(proofOf(result).kinds == Set(ProofKind.LeanKernel), explain(result))
        // Of a sample, the contract's tests hold as the Scalus CEK runs them: `minus` returns, a
        // guarantee of the statement and no premise, and the lovelace of the difference is the
        // difference.
        val withTokens = Value.lovelace(BigInt(5)) + Value(hex"aa", hex"bb", BigInt(7))
        assert(
          holdsAsImplicationOn(
            stated,
            Constant.Data(withTokens.toData),
            Constant.Data(Value.lovelace(BigInt(3)).toData)
          ),
          stated.name
        )
        // negative control: of the sum, which two lovelace less one refutes
        val ofTheSum = verify(minusOfTheSum, minusControlBudget)
        assert(ofTheSum.meets(minusOfTheSum), explain(minusOfTheSum, ofTheSum))
    }

    // The validator, on every script context

    // A statement over `forAllData[ScriptContext]` is of every context the ledger can give the
    // script: the variable is a context's `Data`, of any transaction, and the validator reads it
    // as it is. These are rejected before the transaction is read.

    /** The validator's entry point, compiled with the tactic's options. */
    private val validator =
        FunctionDef.named("vesting", (context: Data) => VestingValidator.validate(context))

    verifier.addFunction(validator)

    /** The steps for a context the validator rejects before it reads the transaction. */
    private val rejectionBudget = 600

    private val noDatumIsRejected = verifier.statement(
      "vesting.validator.no_datum_is_rejected",
      forAllData[ScriptContext](ctx => spendsWithoutDatum(ctx) ==>: fails(validator, ctx.toData))
    )

    test("an output without a datum cannot be spent") {
        val result = verify(noDatumIsRejected, rejectionBudget)
        assert(result.meets(noDatumIsRejected), explain(noDatumIsRejected, result))
    }

    private val nonPositiveAmountIsRejected = verifier.statement(
      "vesting.validator.non_positive_amount_is_rejected",
      forAllData[ScriptContext](ctx =>
          (spendsWithDatum(ctx) && ctx.redeemer.to[Action].amount <= BigInt(0)) ==>:
              fails(validator, ctx.toData)
      )
    )

    test("a withdrawal of a non-positive amount is rejected") {
        // Of an output with a datum, whatever the datum is. A redeemer that is no `Action` is none
        // the premise reads an amount of, and the validator rejects it too.
        val result = verify(nonPositiveAmountIsRejected, rejectionBudget)
        assert(
          result.meets(nonPositiveAmountIsRejected),
          explain(nonPositiveAmountIsRejected, result)
        )
    }

    private val onlySpending = verifier.statement(
      "vesting.validator.only_spending",
      forAllData[ScriptContext](ctx => !Prop(isSpending(ctx)) ==>: fails(validator, ctx.toData))
    )

    test("the script validates nothing but spending") {
        val result = verify(onlySpending, rejectionBudget)
        assert(result.meets(onlySpending), explain(onlySpending, result))
    }

    // Withdrawals

    /** The steps Lean's machine may make on a sample withdrawal, which it runs to its end. */
    private val sampleBudget = 20000

    /** The steps of the checks of spend's guarantees, in the rejecting form: a rejected spend of
      * two inputs ends within them.
      */
    private val guaranteesBudget = 3300

    /** The steps of the checks of the validator's call of the schedule. */
    private val obligationBudget = 3000

    /** A withdrawal of everything after the end of the period, accepted on the Lean machine. */
    private val sampleAccepted = verifier.statement(
      "vesting.withdrawal.sample_is_accepted",
      succeeds(validator, sampleOfEverything(signed))
    )

    /** The same, unsigned. */
    private val sampleUnsigned = verifier.statement(
      "vesting.withdrawal.sample_unsigned_is_rejected",
      fails(validator, sampleOfEverything(prelude.List.Nil))
    )

    /** Halfway through the period, a withdrawal of what has vested: it leaves exactly what has not.
      */
    private val sampleOfTheVested = verifier.statement(
      "vesting.withdrawal.sample_of_the_vested_is_accepted",
      succeeds(validator, sampleHalfway(halfOfTheAmount))
    )

    /** The same, of one lovelace more. */
    private val sampleOfMore = verifier.statement(
      "vesting.withdrawal.sample_of_more_is_rejected",
      fails(validator, sampleHalfway(halfOfTheAmount + BigInt(1)))
    )

    test("sample withdrawals are accepted on the Lean machine, and rejected unsigned or for more") {
        // Closed statements: Lean decides them by running the validator. They are what the
        // statements below are not vacuous of: the validator accepts some withdrawals, and the
        // bound of what stays locked is exact.
        for sample <- List(sampleAccepted, sampleUnsigned, sampleOfTheVested, sampleOfMore) do
            val result = verify(sample, sampleBudget)
            assert(result.meets(sample), explain(sample, result))
            assert(artifactOf(result).kind == ProofKind.LeanNative, sample.name)
    }

    /** The validator on a script context of its type: what the verifier's statements about its code
      * are of. `spend` is inlined into `validate`, so the verifier finds the clauses of `spend` in
      * the code of this function. The statements of its clauses are of every context, under the
      * plain names, and of the contexts of [[partialWithdrawalLimited]] (`partial_withdrawal`),
      * under names with the set's (`validate[partial_withdrawal]/...`); its obligations also of the
      * contexts of [[fewInputs]] (`few_inputs`).
      */
    private val validate = FunctionDef.named(
      "validate",
      (ctx: ScriptContext) => VestingValidator.validate(ctx.toData)
    )

    verifier.addFunction(validate)

    /** [[fewInputs]], as the verifier takes the contexts of the obligations it declares. */
    private val fewInputsData =
        limitedData[ScriptContext](ctx => fewInputs(ctx)).named("few_inputs")

    /** Every script context: the parameter a `forAllData[ScriptContext]` variable. */
    private val anyContext = unlimitedData[ScriptContext]

    // What holds of every spend the script accepts: the conditions `VestingValidator.spend` states
    // at its head with `Spec.ensures`, each as a statement about the compiled script, of every
    // script context, under the plain name; the fourth also of the contexts of a partial
    // withdrawal, under the name with `Limited`.
    //
    // `spend` is inline: the program is the script's entry point, `validate`, which reads the
    // datum, the redeemer and the transaction from the script context and gives them to `spend`.
    // So a statement is of a context: where the script accepts it, the condition holds of its
    // datum, redeemer and transaction. The validator's code is written in the statement, as
    // `succeeds(VestingValidator.validate(ctx.toData))`, not called as the registered `validator`:
    // so it is compiled with the condition, and what both read of the context is read once. Called,
    // the statement of the first condition did not come to the solver in ten minutes; written so,
    // it is checked as `verifier.guarantees` would check it, with the same program (2026-11-10).
    //
    // Each is stated with `==>?:`: wherever the condition is false, the script rejects. A
    // condition reads the datum, the redeemer, the spent input and the validity range as the
    // validator does, and fails where they cannot be read, where the validator rejects too; there
    // it says nothing, and the rejecting program ends at once. With `==>:` a condition would have
    // to return there as well: one that read a default instead lets every datum it cannot read
    // run on into the validator, which reads the datum after it walks the inputs, and Lean does
    // not finish.
    //
    // They are proved one by one. Each rejecting program runs the validator only where its
    // condition is false, to the check that rejects the spend: together, every way one of them is
    // false would run on into the validator, and Lean does not finish.

    /** The statements the verifier makes of spend's clauses themselves, over the contexts of the
      * statements below: what ties those to the clauses in `VestingValidator.spend`. They are not
      * proved: each of the statements below is the same statement as one of them, and checks its
      * clause so ([[Verifier.specifications]]). A clause changed in the code, or a statement here,
      * would leave the clause unchecked, and fail the suite.
      */
    verifier.guarantees(validate.ref, anyContext, Connective.Rejects)

    // spend: Spec.ensures(txInfo.isSignedBy(configOf(datum).beneficiary))
    private val beneficiarySigned = verifier.statement(
      "vesting.spend.beneficiary_signed",
      forAllData[ScriptContext](ctx => accepts(ctx) ==>?: signedByTheBeneficiary(ctx))
    )

    test("spend's first guarantee holds of every script context, with calls as unknowns") {
        // Lean does not finish the statement as it is: the validator walks the context's lists,
        // whose length is unknown. The tactic replaces each walk by an unknown value, and the
        // program left has no loop. The clause's isSignedBy(configOf(datum).beneficiary) and the
        // validator's isSignedBy(vestingConfig.beneficiary) check the same key, and are one
        // unknown: where it is false, the clause is false and the validator rejects, whatever the
        // other walks return.
        //
        // The proof rests on one step that is tested, not proved: that the validator does what
        // its program with unknowns does, with each unknown the value its walk returns
        // (ProofKind.BlasterOfUnknowns).
        val result = verify(
          VerificationRequest(
            beneficiarySigned,
            new CallsAsUnknowns(UplcBlaster(Budget.Auto, lean, 30.minutes)).withoutContracts
          )
        )
        assert(result.meets(beneficiarySigned), explain(beneficiarySigned, result))
        // It needs no contract of the functions the validator calls, and assumes none.
        assert(proofOf(result).assumed.isEmpty, explain(result))
    }

    // spend: Spec.ensures(amountOf(redeemer) > 0)
    //
    // Of every script context, without limits: the validator rejects a spend of no amount at its
    // first check, before it reads a list of the context. It needs no statement with limits.
    private val somethingIsWithdrawn = verifier.statement(
      "vesting.spend.something_is_withdrawn",
      forAllData[ScriptContext](ctx => accepts(ctx) ==>?: withdrawsSomething(ctx))
    )

    test("spend's second guarantee holds of every script context: something is withdrawn") {
        val result = verify(somethingIsWithdrawn, guaranteesBudget)
        assert(result.meets(somethingIsWithdrawn), explain(somethingIsWithdrawn, result))
    }

    // spend: Spec.ensures(lockedBy(txInfo, txOutRef) - amountOf(redeemer) >=
    //            configOf(datum).initialAmount - linearVesting(configOf(datum), validFrom))
    private val unvestedStaysLocked = verifier.statement(
      "vesting.spend.unvested_stays_locked",
      forAllData[ScriptContext](ctx => accepts(ctx) ==>?: keepsTheUnvestedLocked(ctx))
    )

    test("spend's third guarantee holds of every script context, with calls as unknowns") {
        // As the first: with each walk an unknown, the program has no loop. The clause's
        // lockedBy(txInfo, txOutRef) and the validator's lovelace of the spent output are one
        // walk, findInputOrFail and getLovelace of the same input, so the clause's bound is the
        // validator's check rearranged, whatever the walks return. It rests on the same tested
        // step (ProofKind.BlasterOfUnknowns).
        val result = verify(
          VerificationRequest(
            unvestedStaysLocked,
            new CallsAsUnknowns(UplcBlaster(Budget.Auto, lean, 60.minutes)).withoutContracts
          )
        )
        assert(result.meets(unvestedStaysLocked), explain(unvestedStaysLocked, result))
        assert(proofOf(result).assumed.isEmpty, explain(result))
    }

    // spend: Spec.ensures(amountOf(redeemer) === lockedBy(txInfo, txOutRef) ||
    //            carriesTheRest(txInfo, txOutRef, datum, amountOf(redeemer)))
    //
    // The step of a thread of withdrawals (docs/design/verification-details/multi-transaction.md):
    // the output a withdrawal leaves is the one the next withdrawal spends, and the next reads the
    // datum and the lovelace of it as the state.
    /** The budget of the fourth guarantee with calls as unknowns: its rejecting program runs the
      * validator to its last check. `Budget.Auto` climbed from 100 to 983 in an hour, each
      * counterexample a run the budget cut a few steps further on; at 2400 Lean proves it in two
      * minutes (2026-11-26).
      */
    private val stateIsCarriedBudget = 2400

    private val stateIsCarried = verifier.statement(
      "vesting.spend.state_is_carried",
      forAllData[ScriptContext](ctx => accepts(ctx) ==>?: carriesTheState(ctx))
    )

    test("spend's fourth guarantee holds of every script context, with the library's contracts") {
        // As the first and the third, with each walk an unknown. The clause reads the lovelace of
        // the output that keeps the rest; the validator compares that output's whole value with
        // the spent one's less the lovelace withdrawn. What ties the two is the library's: the
        // contracts that `Value.eq`, `Value.minus` and `Value.lovelace` state in their code, which
        // the statement assumes of the unknowns of their calls
        // (docs/design/verification-details/call-rule.md). The proof cites the proofs of those
        // contracts as lemmas, and of `linearVesting`'s, which the validator also calls: so those
        // proofs come first, and the guarantee rests on no contract that is not proved.
        val lemmas = List[(Option[Statement], Statement => VerificationResult)](
          statedContractHolds -> (verify(_, scheduleBudget)),
          valueEqContractHolds -> (verify(_, valueEqBudget)),
          lovelaceContractHolds -> (verify(_, lovelaceBudget)),
          valueMinusContractHolds -> proveValueMinus
        )
        lemmas.foreach { (statement, prove) =>
            val stated = statement.getOrElse(fail("a function states no contract"))
            // A contract its own test proved is a theorem already, and is not proved again.
            if !verifier.theorems.exists(_.statement.name == stated.name) then
                val proved = prove(stated)
                assert(proved.meets(stated), explain(stated, proved))
        }
        val result = verify(
          VerificationRequest(
            stateIsCarried,
            new CallsAsUnknowns(
              UplcBlaster(Budget.LeanSteps(stateIsCarriedBudget), lean, 60.minutes)
            )
          )
        )
        assert(result.meets(stateIsCarried), explain(stateIsCarried, result))
        val proof = proofOf(result)
        assert(proof.assumed.isEmpty, explain(result))
        assert(
          proof.usedLemmas.map(_.statement.name).toSet ==
              lemmas.flatMap(_._1).map(_.name).toSet,
          explain(result)
        )
    }

    // The contexts of a partial withdrawal bound the outputs that the fourth guarantee reads, and
    // the contexts of a few inputs leave free.
    verifier.guarantees(
      validate.ref,
      limitedData[ScriptContext](ctx => partialWithdrawalLimited(ctx)).named("partial_withdrawal"),
      Connective.Rejects
    )

    private val stateIsCarriedLimited = verifier.statement(
      "vesting.spend.state_is_carried_limited",
      forAllDataLimited[ScriptContext](ctx => partialWithdrawalLimited(ctx))(ctx =>
          accepts(ctx) ==>?: carriesTheState(ctx)
      )
    )

    // Lean finishes the fourth of no contexts: it is declared open.
    verifier.open(
      stateIsCarriedLimited,
      "Lean does not finish its symbolic run (the test of it is tagged Unfinished)"
    )

    test(
      "spend's fourth guarantee, of withdrawals of one input and two outputs, is not finished by " +
          "Lean",
      Unfinished
    ) {
        // What a partial withdrawal leaves, the next one spends: the rest, in one output at the
        // spent output's address, under the same datum, with as much less lovelace. The search
        // for its budget did not finish in 10 minutes (2026-11-24): the rejecting program runs
        // the validator to its end, its last check, over the outputs. Of every context it is
        // proved with the walks as unknowns and the library's contracts as their premises (the
        // test above). The time limit ends it.
        val result = verifyWithoutKeeping(
          stateIsCarriedLimited,
          UplcBlaster(Budget.LeanSteps(guaranteesBudget), lean, 60.seconds)
        )
        assert(result.isInconclusive, explain(stateIsCarriedLimited, result))
        val unfinished = result.why.getOrElse("")
        assert(unfinished.contains("did not finish"), unfinished)
    }

    private def proofOf(result: VerificationResult): Proof = result match
        case VerificationResult.Proven(proof)         => proof
        case VerificationResult.ProvenAssuming(proof) => proof
        case other                                    => fail(s"no proof: $other")

    /** The validator without its check of the datum of the output that keeps the rest. */
    private val withoutDatumCheck = FunctionDef.named(
      "vesting_without_datum_check",
      (context: Data) => VestingWithoutDatumCheck.validate(context)
    )

    verifier.addFunction(withoutDatumCheck)

    /** The negative control of the fourth guarantee: without the datum check, a withdrawal can
      * leave the rest under another datum.
      */
    private val stateIsCarriedWithoutDatumCheck = verifier.refute(
      "vesting.control.state_is_carried_without_datum_check",
      forAllData[ScriptContext](ctx =>
          succeeds(VestingWithoutDatumCheck.validate(ctx.toData)) ==>?: carriesTheState(ctx)
      )
    )

    test(
      "without the datum check, the fourth guarantee has a counterexample under the contracts, " +
          "which no context realizes"
    ) {
        // With the walks as unknowns and the library's contracts as premises, Lean finds a spend
        // the copy accepts and that leaves the rest under another datum: the premises do not
        // contradict each other, and the proof of the guarantee rests on the datum check. But the
        // unknowns' values are of no context: on the context Lean gives, the copy's walks return
        // other values, and the statement as written holds there. A refutation by unknowns is
        // confirmed only where Lean's context realizes them, so this one is no refutation, but one
        // whose context does is. The context that refutes the statement is the sample below,
        // which the copy accepts. What the test catches is a proof: that would say the premises
        // contradict each other, and so prove anything, the fourth guarantee among it.
        val result = verifyWithoutKeeping(
          stateIsCarriedWithoutDatumCheck,
          new CallsAsUnknowns(UplcBlaster(Budget.LeanSteps(stateIsCarriedBudget), lean, 10.minutes))
        )
        val spurious = result.isInconclusive &&
            result.why.exists(_.contains("values that their walks do not return"))
        assert(
          spurious || result.meets(stateIsCarriedWithoutDatumCheck),
          explain(stateIsCarriedWithoutDatumCheck, result)
        )
    }

    private val stateIsCarriedWithoutDatumCheckLimited = verifier.refute(
      "vesting.control.state_is_carried_without_datum_check_limited",
      forAllDataLimited[ScriptContext](ctx => partialWithdrawalLimited(ctx))(ctx =>
          succeeds(withoutDatumCheck, ctx.toData) ==>?: carriesTheState(ctx)
      )
    )

    test(
      "without the datum check, the fourth guarantee, of withdrawals of one input and two " +
          "outputs, is not finished by Lean",
      Unfinished
    ) {
        // The search for a counterexample did not finish in 10 minutes (2026-11-24): it is a
        // withdrawal the copy accepts, so its run goes to the copy's end. The samples below show
        // one. The time limit ends it.
        val result = verifyWithoutKeeping(
          stateIsCarriedWithoutDatumCheckLimited,
          UplcBlaster(Budget.LeanSteps(guaranteesBudget), lean, 60.seconds)
        )
        assert(result.isInconclusive, explain(stateIsCarriedWithoutDatumCheckLimited, result))
        val unfinished = result.why.getOrElse("")
        assert(unfinished.contains("did not finish"), unfinished)
    }

    /** A withdrawal halfway through the period of what has vested, which leaves the rest under a
      * datum of a period of no time: the validator rejects it, and the one without the datum check
      * accepts it.
      */
    private val sampleLeavingAnotherDatum = verifier.statement(
      "vesting.withdrawal.sample_leaving_another_datum_is_rejected",
      fails(validator, sampleHalfwayUnder(vestedAtOnce))
    )

    private val sampleLeavingAnotherDatumWithoutDatumCheck = verifier.statement(
      "vesting.control.sample_leaving_another_datum_is_accepted",
      succeeds(withoutDatumCheck, sampleHalfwayUnder(vestedAtOnce))
    )

    /** The next withdrawal, at the same time, spends what the first left: the validator accepts a
      * withdrawal of all of it, as the datum it reads has everything vested.
      */
    private val sampleOfTheRestUnderAnotherDatum = verifier.statement(
      "vesting.withdrawal.sample_of_the_rest_under_another_datum_is_accepted",
      succeeds(validator, sampleOfTheRestUnder(vestedAtOnce))
    )

    test("without the datum check, two withdrawals halfway through the period take everything") {
        // Each meets the first three guarantees: they are of the transaction's own datum. Only the
        // fourth sees the first withdrawal's fault.
        for sample <- List(
              sampleLeavingAnotherDatum,
              sampleLeavingAnotherDatumWithoutDatumCheck,
              sampleOfTheRestUnderAnotherDatum
            )
        do
            val result = verify(sample, sampleBudget)
            assert(result.meets(sample), explain(sample, result))
            assert(artifactOf(result).kind == ProofKind.LeanNative, sample.name)
        for context <- List(sampleHalfwayUnder(vestedAtOnce), sampleOfTheRestUnder(vestedAtOnce))
        do
            val ctx = context.to[ScriptContext]
            assert(signedByTheBeneficiary(ctx) && withdrawsSomething(ctx))
            assert(keepsTheUnvestedLocked(ctx))
        assert(!carriesTheState(sampleHalfwayUnder(vestedAtOnce).to[ScriptContext]))
        // Together they take the whole amount, of which half has vested.
        assert(
          halfOfTheAmount + halfOfTheAmount >
              VestingValidator.linearVesting(sampleConfig, BigInt(1500))
        )
    }

    test("spend's conditions return true on the samples the validator accepts") {
        // Stated with `==>?:`, a condition that failed wherever the validator returns would be
        // proved of every context. Read with `==>:` on a spend the validator accepts, each returns
        // `true` there: run on the Scalus CEK, without Lean.
        val accepted = List(sampleOfEverything(signed), sampleHalfway(halfOfTheAmount))
        for
            sample <- accepted
            statement <- List(
              beneficiarySigned,
              somethingIsWithdrawn,
              unvestedStaysLocked,
              stateIsCarried,
              stateIsCarriedLimited
            )
        do assert(holdsAsImplicationOn(statement, Constant.Data(sample)), statement.name)
    }

    /** A contract the schedule does not state: had it expected an initial amount that is not
      * negative, the validator's call would owe that, wherever a withdrawal reaches it.
      */
    verifier.contract(
      "vested_in_range",
      contract(linearVesting)(
        expects = (config, time) => config.initialAmount >= BigInt(0),
        ensures = (config, time) => vested => BigInt(0) <= vested && vested <= config.initialAmount
      )
    )

    /** What the validator's call of the schedule owes to `vested_in_range`, of every context. */
    private val spendObligations = verifier.obligations(validate.ref, anyContext)

    test(
      "the obligation of the schedule's call, of every script context, is not finished by Lean",
      Unfinished
    ) {
        val owed = spendObligations match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")
        val result = verifyWithoutKeeping(
          owed,
          UplcBlaster(Budget.LeanSteps(obligationBudget), lean, 60.seconds)
        )
        assert(result.isInconclusive, explain(owed, result))
        val unfinished = result.why.getOrElse("")
        assert(unfinished.contains("did not finish"), unfinished)
    }

    /** The same, of the contexts of [[fewInputs]]. */
    private val spendObligationsLimited = verifier.obligations(validate.ref, fewInputsData)

    test("the validator would not establish a precondition on the amount") {
        // Why the schedule states its bound with a condition. Of the contexts of a few inputs.
        val owed = spendObligationsLimited match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")
        // It is refuted: the validator reads the amount from the datum and does not check it.
        val result =
            verify(VerificationRequest(owed, UplcBlaster(Budget.LeanSteps(obligationBudget), lean)))
        // An obligation is declared to hold, as every one is: this one does not.
        assert(result.isRefuted, explain(owed, result))
        val values = counterexampleOf(result)
        val amounts = values.collect { case (_, scalus.uplc.Constant.Data(context)) =>
            initialAmountOf(context.to[ScriptContext])
        }
        assert(amounts.exists(_.exists(_ < 0)), values)
    }

    /** A contract of the validator that expects a datum whose initial amount `vested_in_range` asks
      * for.
      */
    private val spendOfAnAmount = verifier.contract(
      "spend_of_an_amount",
      contract(validate)(
        expects = ctx => initialAmountIsNotNegative(ctx),
        ensures = ctx => done => true
      )
    )

    /** The obligation of the schedule's call, where the validator's contract holds, of every
      * context.
      */
    private val obligationsOfAnAmount = verifier.obligations(spendOfAnAmount, anyContext)

    test(
      "where the validator's contract expects such an amount, the obligation, of every script " +
          "context, is not finished by Lean",
      Unfinished
    ) {
        val assumed = obligationsOfAnAmount match
            case CallObligations(List(assumed), Nil) => assumed
            case other                               => fail(s"expected one obligation, got $other")
        val result = verifyWithoutKeeping(
          assumed,
          UplcBlaster(Budget.LeanSteps(obligationBudget), lean, 60.seconds)
        )
        assert(result.isInconclusive, explain(assumed, result))
        val unfinished = result.why.getOrElse("")
        assert(unfinished.contains("did not finish"), unfinished)
    }

    /** The same obligation, of the contexts of [[fewInputs]]. */
    private val obligationsOfAnAmountLimited = verifier.obligations(spendOfAnAmount, fewInputsData)

    test("where the validator's contract expects such an amount, the obligation holds") {
        val assumed = obligationsOfAnAmountLimited match
            case CallObligations(List(assumed), Nil) => assumed
            case other                               => fail(s"expected one obligation, got $other")
        val result = verify(
          VerificationRequest(
            assumed,
            UplcBlaster(Budget.LeanSteps(obligationBudget), lean, 30.minutes)
          )
        )
        assert(result.meets(assumed), explain(assumed, result))
    }

    // Everything after the end of the period

    private val everythingAfterTheEnd = verifier.statement(
      "vesting.withdrawal.everything_after_the_end",
      forAllData[ScriptContext](ctx =>
          (takesEverything(ctx) && afterTheEnd(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    private val everythingAtAnyTime = verifier.refute(
      "vesting.withdrawal.everything_at_any_time",
      forAllData[ScriptContext](ctx => takesEverything(ctx) ==>: succeeds(validator, ctx.toData))
    )

    test(
      "after the end of the period the beneficiary withdraws everything, of every script " +
          "context, not finished by Lean",
      Unfinished
    ) {
        List(everythingAfterTheEnd, everythingAtAnyTime).foreach { statement =>
            val result = verifyWithoutKeeping(
              statement,
              UplcBlaster(Budget.LeanSteps(1922), lean, 60.seconds)
            )
            assert(result.isInconclusive, explain(statement, result))
            val unfinished = result.why.getOrElse("")
            assert(unfinished.contains("did not finish"), unfinished)
        }
    }

    private val everythingAfterTheEndLimited = verifier.statement(
      "vesting.withdrawal.everything_after_the_end_limited",
      forAllDataLimited[ScriptContext](ctx => fewInputsAndOutputs(ctx))(ctx =>
          (takesEverything(ctx) && afterTheEnd(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    private val everythingAtAnyTimeLimited = verifier.refute(
      "vesting.withdrawal.everything_at_any_time_limited",
      forAllDataLimited[ScriptContext](ctx => fewInputsAndOutputs(ctx))(ctx =>
          takesEverything(ctx) ==>: succeeds(validator, ctx.toData)
      )
    )

    test(
      "after the end of the period the beneficiary withdraws everything, of contexts of two " +
          "inputs and two outputs, not finished by Lean",
      Unfinished
    ) {
        // The funds are not stuck: of every context of at most two inputs and two outputs, a
        // withdrawal of everything, signed by the beneficiary and paying them what the validator
        // asks, is accepted from the end of the period on. The negative control: before the end of
        // the period not everything has vested, `everythingAtAnyTimeLimited`.
        //
        // An accepted run goes to the validator's end, and Lean runs the validator on every path
        // of every such context: the search for the budget starts at 1922 steps, where the limits
        // have every test end on the largest values that meet them, and Lean was still in the
        // validator's symbolic run after 16 minutes (2026-11-06). The time limit ends it.
        List(everythingAfterTheEndLimited, everythingAtAnyTimeLimited).foreach { statement =>
            val result = verifyWithoutKeeping(
              statement,
              UplcBlaster(Budget.LeanSteps(1922), lean, 60.seconds)
            )
            assert(result.isInconclusive, explain(statement, result))
            val unfinished = result.why.getOrElse("")
            assert(unfinished.contains("symbolically"), unfinished)
        }
    }
}

/** The values the statements of [[VestingVerificationTest]] are about. They are `inline`, so that
  * each is part of the expression it is used in, which the Scalus plugin compiles. They live in an
  * object: an `inline def` of the test class that uses another one would leave a reference to the
  * test's `this` in that expression.
  */
object VestingVerificationTest {

    /** The example's own Lean workspace, `src/test/lean/LinearVesting`, in which the suite runs its
      * checks.
      */
    def workspace: Path =
        LeanProofs.inSources("scalus-examples", "jvm", "src", "test", "lean", "LinearVesting")

    /** The beneficiary of every withdrawal here, a constant: its signature is compared with the
      * datum's.
      */
    inline def beneficiary: PubKeyHash =
        PubKeyHash(hex"11111111111111111111111111111111111111111111111111111111")

    inline def config(
        inline start: BigInt,
        inline duration: BigInt,
        inline amount: BigInt
    ): Config = Config(beneficiary, start, duration, amount)

    /** Whether `ctx` is of spending an output. */
    inline def isSpending(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, _) => true
        case _                               => false

    /** Whether `ctx` is of spending an output that has no datum. */
    inline def spendsWithoutDatum(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, prelude.Option.None) => true
        case _                                                 => false

    /** Whether `ctx` is of spending an output that has a datum. */
    inline def spendsWithDatum(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, prelude.Option.Some(_)) => true
        case _                                                    => false

    inline def vestingRef: TxOutRef =
        TxOutRef(TxId(hex"2222222222222222222222222222222222222222222222222222222222222222"), 0)

    /** The address of the samples' vesting output. */
    inline def vestingAddress: Address =
        Address(
          Credential.ScriptCredential(
            hex"33333333333333333333333333333333333333333333333333333333"
          ),
          prelude.Option.None
        )

    inline def beneficiaryAddress: Address =
        Address(Credential.PubKeyCredential(beneficiary), prelude.Option.None)

    /** The datum of the samples: 20 000 000 lovelace, vesting from 1000 for 1000. */
    inline def sampleConfig: Config = config(BigInt(1000), BigInt(1000), BigInt(20_000_000))

    /** What has vested of the samples' amount halfway through the period, at 1500. */
    inline def halfOfTheAmount: BigInt = BigInt(10_000_000)

    /** A sample withdrawal of `requested` at `time`: the transaction spends the vesting output, of
      * 20 000 000 lovelace under [[sampleConfig]], and pays the fee of 200 000.
      */
    inline def sample(
        inline requested: BigInt,
        inline time: BigInt,
        inline signatories: prelude.List[PubKeyHash],
        inline outputs: prelude.List[TxOut]
    ): Data =
        ScriptContext(
          TxInfo(
            inputs = prelude.List.Cons(
              TxInInfo(vestingRef, TxOut(vestingAddress, Value.lovelace(BigInt(20_000_000)))),
              prelude.List.Nil
            ),
            outputs = outputs,
            fee = BigInt(200_000),
            validRange = Interval.after(time),
            signatories = signatories,
            id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
          ),
          Action(requested).toData,
          ScriptInfo.SpendingScript(vestingRef, prelude.Option.Some(sampleConfig.toData))
        ).toData

    /** A withdrawal of everything after the end, at 2000, paid out less the fee. */
    inline def sampleOfEverything(inline signatories: prelude.List[PubKeyHash]): Data =
        sample(BigInt(20_000_000), BigInt(2000), signatories, payment(BigInt(19_800_000)))

    /** A withdrawal of `requested` halfway through the period, signed: paid out less the fee, with
      * the rest kept at the script under the same datum.
      */
    inline def sampleHalfway(inline requested: BigInt): Data =
        sample(
          requested,
          BigInt(1500),
          signed,
          prelude.List.Cons(
            TxOut(beneficiaryAddress, Value.lovelace(requested - BigInt(200_000))),
            prelude.List.Cons(
              TxOut(
                vestingAddress,
                Value.lovelace(BigInt(20_000_000) - requested),
                OutputDatum.OutputDatum(sampleConfig.toData)
              ),
              prelude.List.Nil
            )
          )
        )

    /** The beneficiary's signature alone. */
    inline def signed: prelude.List[PubKeyHash] =
        prelude.List.Cons(beneficiary, prelude.List.Nil)

    /** One output, of `paid` lovelace to the beneficiary. */
    inline def payment(inline paid: BigInt): prelude.List[TxOut] = prelude.List.Cons(
      TxOut(beneficiaryAddress, Value.lovelace(paid)),
      prelude.List.Nil
    )

    /** The contexts of the statements about withdrawals: of at most two inputs, as the vesting
      * output and one of the beneficiary's that pays the fee, of one signatory, and of inputs whose
      * values are of one policy of one token. Everything else of the context is free, the outputs
      * among it: the validator rejects a withdrawal that breaks a condition before it reads them,
      * and the statements are checked in the rejecting form, which runs only the runs it rejects
      * ([[UplcBlaster]], "The rejecting form").
      */
    inline def fewInputs(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length <= 2 && ctx.txInfo.signatories.length <= 1 &&
            ctx.txInfo.inputs.forall(i =>
                i.resolved.value.toSortedMap.length <= 1 &&
                    i.resolved.value.toSortedMap.values.forall(t => t.length <= 1)
            )

    /** The contexts of the statements of a withdrawal of everything: of two inputs and two outputs
      * at most, of one signatory, and of values of one policy of one token. An accepted run goes to
      * the validator's end, which walks the outputs too.
      */
    inline def fewInputsAndOutputs(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length <= 2 && ctx.txInfo.outputs.length <= 2 &&
            ctx.txInfo.signatories.length <= 1 &&
            ctx.txInfo.inputs.forall(i =>
                i.resolved.value.toSortedMap.length <= 1 &&
                    i.resolved.value.toSortedMap.values.forall(t => t.length <= 1)
            ) &&
            ctx.txInfo.outputs.forall(o =>
                o.value.toSortedMap.length <= 1 &&
                    o.value.toSortedMap.values.forall(t => t.length <= 1)
            )

    /** The contexts of the statements that a withdrawal carries the state: of one input, the
      * vesting output, out of which the withdrawal pays the fee; of two outputs, the beneficiary's
      * and the one that keeps the rest; of one signatory; and of values of one policy of one token.
      * Everything else of the context is free, the outputs' addresses and datums among it.
      */
    inline def partialWithdrawalLimited(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length == 1 && ctx.txInfo.outputs.length == 2 &&
            ctx.txInfo.signatories.length == 1 &&
            ctx.txInfo.inputs.forall(i =>
                i.resolved.value.toSortedMap.length <= 1 &&
                    i.resolved.value.toSortedMap.values.forall(t => t.length <= 1)
            ) &&
            ctx.txInfo.outputs.forall(o =>
                o.value.toSortedMap.length <= 1 &&
                    o.value.toSortedMap.values.forall(t => t.length <= 1)
            )

    /** A datum a withdrawal could leave where the validator did not check it: the samples', of a
      * period of no time, so that everything has vested from its start.
      */
    inline def vestedAtOnce: Config = config(BigInt(1000), BigInt(0), BigInt(20_000_000))

    /** Halfway through the period, a signed withdrawal of what has vested, paid out less the fee,
      * which leaves the rest at the script under `left`.
      */
    inline def sampleHalfwayUnder(inline left: Config): Data =
        sample(
          halfOfTheAmount,
          BigInt(1500),
          signed,
          prelude.List.Cons(
            TxOut(beneficiaryAddress, Value.lovelace(halfOfTheAmount - BigInt(200_000))),
            prelude.List.Cons(
              TxOut(
                vestingAddress,
                Value.lovelace(BigInt(20_000_000) - halfOfTheAmount),
                OutputDatum.OutputDatum(left.toData)
              ),
              prelude.List.Nil
            )
          )
        )

    /** The withdrawal after [[sampleHalfwayUnder]], at the same time: it spends the output that one
      * left, the second of its transaction, and takes all of it, signed and paid out less the fee.
      */
    inline def sampleOfTheRestUnder(inline left: Config): Data =
        ScriptContext(
          TxInfo(
            inputs = prelude.List.Cons(
              TxInInfo(
                restRef,
                TxOut(
                  vestingAddress,
                  Value.lovelace(halfOfTheAmount),
                  OutputDatum.OutputDatum(left.toData)
                )
              ),
              prelude.List.Nil
            ),
            outputs = payment(halfOfTheAmount - BigInt(200_000)),
            fee = BigInt(200_000),
            validRange = Interval.after(BigInt(1500)),
            signatories = signed,
            id = TxId(hex"7777777777777777777777777777777777777777777777777777777777777777")
          ),
          Action(halfOfTheAmount).toData,
          ScriptInfo.SpendingScript(restRef, prelude.Option.Some(left.toData))
        ).toData

    /** The output a sample withdrawal leaves at the script: the second of its transaction. */
    inline def restRef: TxOutRef =
        TxOutRef(TxId(hex"5555555555555555555555555555555555555555555555555555555555555555"), 1)

    /** spend's fourth condition, of the context `ctx`: a withdrawal of less than everything leaves
      * the rest in the one output at the address of the spent output, under the same datum, with as
      * much less lovelace. It fails where it cannot read them, as where there is no such output or
      * more than one. Of a context that is no spend it says nothing.
      */
    inline def carriesTheState(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, datum) =>
            VestingValidator
                .amountOf(ctx.redeemer) === VestingValidator.lockedBy(ctx.txInfo, ref) ||
            VestingValidator.carriesTheRest(
              ctx.txInfo,
              ref,
              datum,
              VestingValidator.amountOf(ctx.redeemer)
            )
        case _ => true

    /** That the script accepts `ctx`: its code, to be written in a statement as
      * `succeeds(accepts(ctx))`, where it is compiled with what the statement says of `ctx`.
      */
    inline def accepts(inline ctx: ScriptContext): Prop =
        succeeds(VestingValidator.validate(ctx.toData))

    /** spend's first condition, of the context `ctx`: the beneficiary named in the datum signed the
      * transaction. It reads the datum as the validator does, and fails where it cannot be read. Of
      * a context that is no spend, which the script rejects, it says nothing.
      */
    inline def signedByTheBeneficiary(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, datum) =>
            ctx.txInfo.isSignedBy(VestingValidator.configOf(datum).beneficiary)
        case _ => true

    /** spend's second condition, of the context `ctx`: the amount the redeemer asks for is
      * positive.
      */
    inline def withdrawsSomething(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, _) => VestingValidator.amountOf(ctx.redeemer) > BigInt(0)
        case _                               => true

    /** spend's third condition, of the context `ctx`: what stays locked, the lovelace of the spent
      * output less the amount withdrawn, is at least what has not vested at the earliest time the
      * transaction can be included.
      */
    inline def keepsTheUnvestedLocked(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, datum) =>
            VestingValidator.lockedBy(ctx.txInfo, ref) - VestingValidator.amountOf(ctx.redeemer) >=
                VestingValidator.configOf(datum).initialAmount -
                VestingValidator.linearVesting(
                  VestingValidator.configOf(datum),
                  ctx.txInfo.validFromOrFail(VestingValidator.NoValidityLowerBound)
                )
        case _ => true

    /** Whether the datum of `ctx`, a spend, has an initial amount that is not negative: true of a
      * context that is no spend. It fails where the datum cannot be read, as the validator does.
      */
    inline def initialAmountIsNotNegative(inline ctx: ScriptContext): Boolean =
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                VestingValidator.configOf(datum).initialAmount >= BigInt(0)
            case _ => true

    /** The initial amount of the datum of `ctx`, where it is a spend with a datum that has one: the
      * fourth field, an integer, as the validator reads it. The validator reads no more of the
      * datum before it calls the schedule, so a counterexample's other fields can be anything.
      */
    def initialAmountOf(ctx: ScriptContext): Option[BigInt] = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, prelude.Option.Some(Data.Constr(_, fields))) =>
            fields.toScalaList.lift(3).collect { case Data.I(amount) => amount }
        case _ => scala.None

    /** Whether `ctx` withdraws everything that is locked as the validator accepts a withdrawal: a
      * spend of the vesting output, the one input at its credential, under a datum it reads, of a
      * period that does not end before it starts, signed by the datum's beneficiary, who is paid
      * what is withdrawn and what they spend of their own, less the fee. It fails where the datum,
      * the redeemer or the spent input cannot be read: as a premise, it then does not hold.
      */
    inline def takesEverything(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, datum) =>
            val vesting = VestingValidator.configOf(datum)
            val requested = VestingValidator.amountOf(ctx.redeemer)
            val owner = Credential.PubKeyCredential(vesting.beneficiary)
            requested > BigInt(0) &&
            requested == VestingValidator.lockedBy(ctx.txInfo, ref) &&
            vesting.duration >= BigInt(0) && ctx.txInfo.isSignedBy(vesting.beneficiary) &&
            (ctx.txInfo.inputs.find(_.outRef === ref) match
                case prelude.Option.Some(own) =>
                    ctx.txInfo.inputs
                        .filter(_.resolved.address.credential === own.resolved.address.credential)
                        .length == BigInt(1)
                case prelude.Option.None => false) &&
            ctx.txInfo
                .findOutputsByCredential(owner)
                .foldLeft(Value.zero)(_ + _.value)
                .getLovelace ==
                requested + ctx.txInfo
                    .findInputsByCredential(owner)
                    .foldLeft(Value.zero)(_ + _.resolved.value)
                    .getLovelace - ctx.txInfo.fee
        case _ => false

    /** Whether `ctx` is of a time from the end of its datum's period on: its validity range starts
      * there or after.
      */
    inline def afterTheEnd(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, datum) =>
            val vesting = VestingValidator.configOf(datum)
            ctx.txInfo.validRange.from.boundType match
                case IntervalBoundType.Finite(time) =>
                    time >= vesting.startTimestamp + vesting.duration
                case _ => false
        case _ => false
}
