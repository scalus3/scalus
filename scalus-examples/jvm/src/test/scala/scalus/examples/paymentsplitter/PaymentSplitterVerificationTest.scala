package scalus.examples.paymentsplitter

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.===
import scalus.cardano.onchain.plutus.v3.{Address, Credential, PubKeyHash, ScriptContext, ScriptInfo, TxCert, TxId, TxInInfo, TxInfo, TxOut, TxOutRef, Value}
import scalus.uplc.Constant
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite, Unfinished, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** Properties of [[NaivePaymentSplitterValidator]] and [[OptimizedPaymentSplitterValidator]],
  * proved about their compiled UPLC with the `UplcBlaster` tactic (see
  * `docs/design/verification-details/uplc-blaster.md`).
  *
  * A payout splits the lovelace locked at the contract equally among its payees. The statements are
  * of every two payees, any keys, and of every script context the ledger can give the script
  * (`forAllData[ScriptContext]`), with what they are about stated as a premise: a payout, its
  * submitter, what it pays. The groups:
  *   - the naive validator: who may submit a payout, who may be paid, that every payee is paid,
  *     that each is paid exactly the split, that only ADA is split, and that an honest payout is
  *     accepted;
  *   - the optimized validator: that spending a contract output needs the stake validator to run
  *     and the redeemer to name that output, that the stake validator checks the same split, and
  *     that the contract's stake credential cannot be delegated.
  *
  * Sample payouts, decided by running the validators, show that they accept one.
  *
  * What is not proved:
  *   - a transaction of any shape. The validators fold over the inputs, the outputs and the payees,
  *     and the tactic cannot follow a loop over a list of unknown length: the statements of a
  *     payout are of two payees and of the contexts of two inputs, two outputs at most, and values
  *     of one token; those about its inputs, of any outputs. The statements of a spend of the
  *     optimized validator are of two inputs and two withdrawals at most;
  *   - the payee who submits a payout is paid the rest, their own input less the fee. That is the
  *     ledger's balance of the transaction, which the validators do not see;
  *   - the published scripts' bytes. The validators are compiled here with the tactic's options
  *     (`UplcBlaster.options`): the statements are about the same source.
  *
  * What Lean finishes of these: that the optimized validator's spend names its input and is
  * accepted with its withdrawal, its certificates, and the samples. A statement about a payout, of
  * the naive validator or the stake validator, has its runs walk the inputs and their values, and
  * then the outputs and the sums of the payees, and Lean does not finish it: its test is tagged
  * `Unfinished`, and shows it with a time limit.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]). The tests tagged `Unfinished` keep nothing: they need
  * Lean.
  */
class PaymentSplitterVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

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
            "paymentsplitter"
          )
          .resolve("PaymentSplitterVerificationTest.proofs.json")
    )

    /** The checks of each statement's last run are also written into the suite's Lean workspace, in
      * `KeptChecks/PaymentSplitterVerificationTest/<statement>/`: the Lean each is given,
      * `Check.lean`, and its programs. There an editor opens them with Lean's tooling, which finds
      * the library they import. The directory is git-ignored, and `lake build` does not build it.
      */
    override protected def keptChecksDirectory: Option[Path] = Some(
      leanWorkspace.resolve("KeptChecks").resolve("PaymentSplitterVerificationTest")
    )

    /** The statements, declared in the suite's verifier. */
    private val p = PaymentSplitterProofs(proofs.verifier)

    /** The time of a check of the optimized validator's spend, in the rejecting form. Its runs walk
      * two inputs and two withdrawals at most, so every one ends, and the search finds the budget,
      * which it doubles where a counterexample cannot be replayed: Lean's restriction of a context
      * does not check the order of the keys of a map whose keys are not bytes, as the withdrawals',
      * nor does the replay, which takes one with two such keys for no context. That the redeemer
      * names the spent input was proved so at 1730 steps, in 490 s (2026-11-07).
      */
    private val spendTime = 30.minutes

    // The naive validator

    /** That Lean does not finish `statement` at `steps` steps in a minute: the time limit ends it.
      * The naive validator's runs walk the inputs and the values of the contract's inputs before
      * anything else, and then the outputs and the sums of the payees: the paths of Lean's symbolic
      * run multiply past what the tests here wait for.
      */
    private def notFinished(statement: Statement, steps: Int): Unit = {
        val reason = inconclusive(statement, UplcBlaster(Budget.LeanSteps(steps), lean, 60.seconds))
        assert(reason.contains("did not finish"), reason)
    }

    test("only a payee submits a payout, not finished by Lean", Unfinished) {
        // An accepted payout spends an input of a payee, and none of another key than a payee's
        // and the contract's, whatever the transaction pays. The validator rejects a payout that
        // does not in its walk of the inputs, before it reads an output, so the outputs are free.
        // Its rejected runs walk both inputs and the values of the contract's (`withoutLovelace`,
        // `getLovelace`), and the budget search climbed by spurious counterexamples, each round
        // longer than the one before: 26 s at 1295 steps, 47 s at 1673, 108 s at 2099, and at
        // 3357 it did not finish in 27 minutes (2026-11-07).
        notFinished(p.onlyAPayeeSubmits, 2099)
        notFinished(p.noOtherKeySpends, 2099)
    }

    test(
      "the payees other than the submitter are paid exactly the split, not finished by Lean",
      Unfinished
    ) {
        // The payee who submits is paid the rest, so the other one is paid half the contract's
        // lovelace, rounded down: an accepted payout pays it no more and no less. The validator
        // rejects a payout that does not at its last check, after it walks the inputs, the
        // outputs and the sums of the payees, and the rejecting program was still in Lean's
        // symbolic run at 1983 steps after 10 minutes (2026-11-07). The negative control: the
        // split is rounded down, so an odd amount locked is not paid out in two exact halves.
        notFinished(p.otherPayeeIsPaidExactlyTheSplit, 1983)
        notFinished(p.splitIsExactlyHalf, 1983)
    }

    test("nobody but the payees is paid, not finished by Lean", Unfinished) {
        // Neither a key that is not a payee, nor a script, whatever the amounts. The validator
        // rejects such a payout in its walk of the outputs, or of the sums of the payees after
        // it: Lean was still in the symbolic run at about 1900 steps after 5 minutes, in the
        // rejecting form and as written (2026-11-07).
        notFinished(p.nobodyButThePayeesIsPaid, 1930)
    }

    test("every payee is paid, not finished by Lean", Unfinished) {
        // The validator rejects a payout that leaves a payee out after the sums of the payees:
        // Lean was still in the symbolic run at about 1900 steps after 5 minutes, in the rejecting
        // form and as written (2026-11-07).
        notFinished(p.everyPayeeIsPaid, 1938)
    }

    test("only ADA is split, not finished by Lean", Unfinished) {
        // A contract output that holds a token besides its lovelace is rejected: the payout
        // reconciles lovelace alone, so its submitter would keep the token. The validator rejects
        // it in its walk of the inputs, as above, with values of two tokens: Lean was still in the
        // symbolic run at 2374 steps after 10 minutes, with the outputs limited too (2026-11-07).
        notFinished(p.onlyAdaIsSplit, 2099)
    }

    test("an honest payout is accepted, not finished by Lean", Unfinished) {
        // The funds are not stuck: whatever else the transaction has, a payout by one payee that
        // pays the other the split is accepted, for any two different payees and any amounts. An
        // accepted run is the whole validator, 5569 steps on the sample, and Lean runs it on every
        // context within the limits: its first counterexample, at 1910 steps after 439 s, was
        // spurious, and it was still in the symbolic run at 2127 (2026-11-07). The negative
        // control: not with any amount to the other payee.
        notFinished(p.honestPayoutIsAccepted, 1910)
        notFinished(p.everyPayoutIsAccepted, 1910)
    }

    // The optimized validator

    test("the redeemer of a spend names the contract's output") {
        check(
          VerificationRequest(
            p.spendRedeemerNamesItsInput,
            UplcBlaster(Budget.Auto, lean, spendTime)
          )
        )
    }

    test(
      "a contract output is spent only with the stake validator's withdrawal, not finished by Lean",
      Unfinished
    ) {
        // The spending endpoint checks nothing of the payout itself: the withdrawal makes the
        // ledger run the stake validator, which does. An accepted spend withdraws from the stake
        // credential of the script of the input its redeemer names, whatever the parameter and
        // the outputs are; and that input is the one of the output it spends, the test above. The
        // rejecting program runs the claim and then the validator, each of which reads the named
        // input and looks its script up in the withdrawals, and the search for its budget climbed
        // by spurious counterexamples, each round longer than the one before: 12 s at 728 steps,
        // 16 s at 856, 42 s at 1115, 273 s at 1487, 707 s at 1661, and at 3322 it did not finish
        // in the 12 minutes left of 30 (2026-11-07). Stated of every input of the spent
        // reference, the claim searched them all, and at 3200 steps it did not finish in 15
        // minutes.
        notFinished(p.spendNeedsItsOwnWithdrawal, 1487)
    }

    /** The steps of the check that a spend with its withdrawal is accepted. A spend's runs end: on
      * two inputs, the redeemer naming the second and its script the second of two withdrawals, the
      * premise takes 1434 steps of Lean's machine and the validator 1347. The search does not come
      * to it: it climbed by spurious counterexamples to 1146 steps, where a counterexample with two
      * withdrawals cannot be replayed to count its steps, and doubled the budget past what Lean
      * finishes (2026-11-07).
      */
    private val acceptedSpendBudget = 1600

    test("a spend with its withdrawal is accepted") {
        // With the withdrawal, and the redeemer naming the contract's output, the spend is
        // accepted. Stated so, Lean runs the validator on every context within the limits, the
        // premise pruning none: a withdrawal's context too, whose stake validator walks the
        // parameter's payees and the outputs, lists of any length here, and the budget cuts.
        check(
          VerificationRequest(
            p.spendWithItsWithdrawalIsAccepted,
            UplcBlaster(Budget.LeanSteps(acceptedSpendBudget), lean, spendTime)
          )
        )
    }

    test(
      "the stake validator pays the payees other than the submitter exactly the split, not " +
          "finished by Lean",
      Unfinished
    ) {
        // Whatever the redeemer claims: an accepted payout pays the other payee the split, which
        // is half the contract's lovelace, rounded down. As the naive validator, the stake
        // validator walks the inputs and the outputs first: Lean was still in the symbolic run at
        // about 1900 steps after 5 minutes, in the rejecting form and as written (2026-11-07).
        notFinished(p.stakeValidatorPaysExactlyTheSplit, 1965)
    }

    test("the stake validator accepts an honest payout, not finished by Lean", Unfinished) {
        // An accepted run is the whole stake validator, 3652 steps on the sample: Lean was still
        // in the symbolic run at 1908 steps after 10 minutes (2026-11-07).
        notFinished(p.stakeValidatorAcceptsAnHonestPayout, 1908)
    }

    test("the contract's stake credential is registered and deregistered, and not delegated") {
        // Of every context: an accepted certificate is a registration or a deregistration, so a
        // delegation is rejected, whatever the transaction is.
        check(p.stakeIsNeverDelegated)
        check(p.stakeRegistrationIsAccepted)
    }

    // Samples

    test("the claims return true on the sample payouts the validators accept") {
        // Stated with `==>?`, a claim that failed wherever the validator returns would be proved
        // of every context. Read with `==>` on a payout the validator accepts, each claim returns
        // `true` there: run on the Scalus CEK, without Lean.
        val payees = Constant.Data(PaymentSplitterVerificationTest.samplePayees)
        def holdsOn(statement: Statement, context: Data): Unit =
            assert(holdsAsImplicationOn(statement, payees, Constant.Data(context)), statement.name)
        List(
          p.onlyAPayeeSubmits,
          p.noOtherKeySpends,
          p.otherPayeeIsPaidExactlyTheSplit,
          p.nobodyButThePayeesIsPaid,
          p.everyPayeeIsPaid,
          p.onlyAdaIsSplit
        ).foreach(holdsOn(_, PaymentSplitterVerificationTest.samplePayout))
        holdsOn(p.spendNeedsItsOwnWithdrawal, PaymentSplitterVerificationTest.sampleSpend)
        holdsOn(p.spendRedeemerNamesItsInput, PaymentSplitterVerificationTest.sampleSpend)
        holdsOn(p.stakeValidatorPaysExactlyTheSplit, PaymentSplitterVerificationTest.sampleReward)
    }

    test("sample payouts are accepted on the Lean machine") {
        // Closed statements: Lean decides them by running the validators. They are what the
        // statements above are not vacuous of: the validators accept an honest payout.
        assert(artifactOf(check(p.sampleNaivePayoutIsAccepted)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(p.sampleSpendIsAccepted)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(p.sampleRewardIsAccepted)).kind == ProofKind.LeanNative)
    }
}

/** The statements about [[NaivePaymentSplitterValidator]] and
  * [[OptimizedPaymentSplitterValidator]], by name, declared in `verifier`, and the validators they
  * are about. A statement declared with `refute` is a negative control: it is expected to be
  * refuted. [[PaymentSplitterVerificationTest]] says why each holds, and checks them.
  */
final class PaymentSplitterProofs(val verifier: Verifier) {
    import PaymentSplitterVerificationTest.*

    /** The naive validator, applied to its payees and then to a script context. */
    val naive: FunctionDef[(Data, Data), Unit] = FunctionDef.named(
      "naiveSplitter",
      (payees: Data, context: Data) => NaivePaymentSplitterValidator.validate(payees)(context)
    )

    /** The optimized validator, applied to its payees and then to a script context. */
    val optimized: FunctionDef[(Data, Data), Unit] = FunctionDef.named(
      "optimizedSplitter",
      (payees: Data, context: Data) => OptimizedPaymentSplitterValidator.validate(payees)(context)
    )

    verifier.addFunction(naive)
    verifier.addFunction(optimized)

    // A rejection is stated as what an accepted context is, `succeeds(validator, …) ==> claim`,
    // which the tactic checks in the rejecting form: a program that runs the validator only where
    // the claim is false, and so ends where the validator rejects that context. Stated as
    // `premise ==> fails(validator, …)`, the validator is run on every context within the limits,
    // the premise pruning none, and an accepted run goes through all of it. A claim that reads
    // the context as the validator does, and fails where the validator cannot read it, is stated
    // with `==>?`: it holds where the claim fails, and the samples show the claims true where the
    // validators accept.

    val onlyAPayeeSubmits: Statement = verifier.statement(
      "payment_splitter.naive.only_a_payee_submits",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => twoInputs(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>
                  (!isSpending(ctx) || ctx.txInfo.inputs.exists(input =>
                      isAPayee(payees, input.resolved.address.credential)
                  ))
          )
      )
    )

    val noOtherKeySpends: Statement = verifier.statement(
      "payment_splitter.naive.no_other_key_spends",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => twoInputs(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>?
                  (!isSpending(ctx) || !spendsAnInputOfNeither(payees, ctx, spentCredential(ctx)))
          )
      )
    )

    val otherPayeeIsPaidExactlyTheSplit: Statement = verifier.statement(
      "payment_splitter.naive.other_payee_is_paid_exactly_the_split",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>? (!isSpending(ctx) ||
                  !someOtherPayeeIsNotPaidTheSplit(payees, ctx, spentCredential(ctx)))
          )
      )
    )

    val splitIsExactlyHalf: Statement = verifier.refute(
      "payment_splitter.naive.split_is_exactly_half",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>? (!isSpending(ctx) ||
                  !someOtherPayeeIsNotPaidExactlyAShare(payees, ctx, spentCredential(ctx)))
          )
      )
    )

    val nobodyButThePayeesIsPaid: Statement = verifier.statement(
      "payment_splitter.naive.nobody_but_the_payees_is_paid",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>
                  (!isSpending(ctx) || ctx.txInfo.outputs.forall(output =>
                      isAPayee(payees, output.address.credential)
                  ))
          )
      )
    )

    val everyPayeeIsPaid: Statement = verifier.statement(
      "payment_splitter.naive.every_payee_is_paid",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>
                  (!isSpending(ctx) || payees.forall(payee => isPaid(ctx, payee)))
          )
      )
    )

    val onlyAdaIsSplit: Statement = verifier.statement(
      "payment_splitter.naive.only_ada_is_split",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => twoInputsWithTokens(ctx))(ctx =>
              succeeds(naive, (payees.toData, ctx.toData)) ==>? (!isSpending(ctx) ||
                  !spendsAContractInputWithTokens(payees, ctx, spentCredential(ctx)))
          )
      )
    )

    val honestPayoutIsAccepted: Statement = verifier.statement(
      "payment_splitter.naive.honest_payout_is_accepted",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              (isSpending(ctx) && areDifferent(payees) &&
                  isAPayoutOfTheSplit(payees, ctx, spentCredential(ctx))) ==>
                  succeeds(naive, (payees.toData, ctx.toData))
          )
      )
    )

    val everyPayoutIsAccepted: Statement = verifier.refute(
      "payment_splitter.naive.every_payout_is_accepted",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              (isSpending(ctx) && areDifferent(payees) &&
                  isAPayout(payees, ctx, spentCredential(ctx))) ==>
                  succeeds(naive, (payees.toData, ctx.toData))
          )
      )
    )

    val spendNeedsItsOwnWithdrawal: Statement = verifier.statement(
      "payment_splitter.optimized.spend_needs_its_own_withdrawal",
      forAll[Data](parameter =>
          forAllDataLimited[ScriptContext](ctx => aSpend(ctx))(ctx =>
              succeeds(optimized, (parameter, ctx.toData)) ==>?
                  (!isSpending(ctx) || isWithdrawnFrom(ctx, namedInput(ctx)))
          )
      )
    )

    val spendWithItsWithdrawalIsAccepted: Statement = verifier.statement(
      "payment_splitter.optimized.spend_with_its_withdrawal_is_accepted",
      forAll[Data](parameter =>
          forAllDataLimited[ScriptContext](ctx => aSpend(ctx))(ctx =>
              (isSpending(ctx) && namesTheSpentInput(ctx) && isWithdrawnFrom(ctx, namedInput(ctx)))
                  ==> succeeds(optimized, (parameter, ctx.toData))
          )
      )
    )

    val spendRedeemerNamesItsInput: Statement = verifier.statement(
      "payment_splitter.optimized.spend_redeemer_names_its_input",
      forAll[Data](parameter =>
          forAllDataLimited[ScriptContext](ctx => aSpend(ctx))(ctx =>
              succeeds(optimized, (parameter, ctx.toData)) ==>?
                  (!isSpending(ctx) || namesTheSpentInput(ctx))
          )
      )
    )

    val stakeValidatorPaysExactlyTheSplit: Statement = verifier.statement(
      "payment_splitter.optimized.stake_validator_pays_exactly_the_split",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              succeeds(optimized, (payees.toData, ctx.toData)) ==> (!isRewarding(ctx) ||
                  !someOtherPayeeIsNotPaidTheSplit(payees, ctx, rewardedCredential(ctx)))
          )
      )
    )

    val stakeValidatorAcceptsAnHonestPayout: Statement = verifier.statement(
      "payment_splitter.optimized.stake_validator_accepts_an_honest_payout",
      forAllDataLimited[prelude.List[ByteString]](payees => payees.length == 2)(payees =>
          forAllDataLimited[ScriptContext](ctx => aPayout(ctx))(ctx =>
              (isRewarding(ctx) && areDifferent(payees) && claimsAnHonestPayout(payees, ctx)) ==>
                  succeeds(optimized, (payees.toData, ctx.toData))
          )
      )
    )

    val stakeIsNeverDelegated: Statement = verifier.statement(
      "payment_splitter.optimized.stake_is_never_delegated",
      forAll[Data](parameter =>
          forAllData[ScriptContext](ctx =>
              succeeds(optimized, (parameter, ctx.toData)) ==>
                  (!isCertifying(ctx) || registersOrDeregisters(ctx))
          )
      )
    )

    val stakeRegistrationIsAccepted: Statement = verifier.statement(
      "payment_splitter.optimized.stake_registration_is_accepted",
      forAll[Data](parameter =>
          forAllData[ScriptContext](ctx =>
              registersOrDeregisters(ctx) ==> succeeds(optimized, (parameter, ctx.toData))
          )
      )
    )

    val sampleNaivePayoutIsAccepted: Statement = verifier.statement(
      "payment_splitter.samples.naive_payout_is_accepted",
      succeeds(naive, (samplePayees, samplePayout))
    )

    val sampleSpendIsAccepted: Statement = verifier.statement(
      "payment_splitter.samples.spend_is_accepted",
      succeeds(optimized, (samplePayees, sampleSpend))
    )

    val sampleRewardIsAccepted: Statement = verifier.statement(
      "payment_splitter.samples.reward_is_accepted",
      succeeds(optimized, (samplePayees, sampleReward))
    )
}

/** The values and the premises the statements of [[PaymentSplitterVerificationTest]] are about.
  * They are `inline`, so that each is part of the expression it is used in, which the Scalus plugin
  * compiles. They live in an object: an `inline def` of the test class that uses another one would
  * leave a reference to the test's `this` in that expression.
  *
  * A premise reads the context as the validators do, and fails where they cannot read it, as where
  * the output a payout spends is not among its inputs: as a premise, it then does not hold.
  */
object PaymentSplitterVerificationTest {

    // The contexts of the statements

    /** The contexts of a payout: of two inputs, as the one of the payee who submits it and one of
      * the contract, of two outputs at most, and of values of one token, ADA in a payout. The
      * validators walk the inputs, the outputs and the values.
      */
    inline def aPayout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length == 2 && ctx.txInfo.outputs.length <= 2 &&
            ctx.txInfo.inputs.forall(input =>
                input.resolved.value.toSortedMap.length <= 1 &&
                    input.resolved.value.toSortedMap.values.forall(tokens => tokens.length <= 1)
            ) &&
            ctx.txInfo.outputs.forall(output =>
                output.value.toSortedMap.length <= 1 &&
                    output.value.toSortedMap.values.forall(tokens => tokens.length <= 1)
            )

    /** The contexts of a payout's inputs: of two inputs, of values of one token, and of any
      * outputs. A payout that the naive validator rejects for its inputs it rejects before it reads
      * an output, and a limit on the outputs would only multiply the shapes Lean runs.
      */
    inline def twoInputs(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length == 2 &&
            ctx.txInfo.inputs.forall(input =>
                input.resolved.value.toSortedMap.length <= 1 &&
                    input.resolved.value.toSortedMap.values.forall(tokens => tokens.length <= 1)
            )

    /** The contexts of [[twoInputs]], with the values of the inputs of two tokens at most: ADA and
      * another.
      */
    inline def twoInputsWithTokens(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length == 2 &&
            ctx.txInfo.inputs.forall(input =>
                input.resolved.value.toSortedMap.length <= 2 &&
                    input.resolved.value.toSortedMap.values.forall(tokens => tokens.length <= 1)
            )

    /** The contexts of the optimized validator's spend: of two inputs at most, which it walks to
      * the one its redeemer names, and two withdrawals at most, which it looks its own up in.
      */
    inline def aSpend(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.inputs.length <= 2 && ctx.txInfo.withdrawals.toList.length <= 2

    // The purposes

    /** Whether `ctx` is of spending an output. */
    inline def isSpending(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, _) => true
        case _                               => false

    /** Whether `ctx` is of a withdrawal. */
    inline def isRewarding(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.RewardingScript(_) => true
        case _                             => false

    /** Whether `ctx` is of a certificate. */
    inline def isCertifying(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.CertifyingScript(_, _) => true
        case _                                 => false

    /** Whether `ctx` is of a certificate that registers or deregisters a stake credential. */
    inline def registersOrDeregisters(inline ctx: ScriptContext): Boolean = ctx.scriptInfo match
        case ScriptInfo.CertifyingScript(_, cert) =>
            cert match
                case TxCert.RegStaking(_, _)   => true
                case TxCert.UnRegStaking(_, _) => true
                case _                         => false
        case _ => false

    /** The reference of the output `ctx` spends. */
    inline def spentRef(inline ctx: ScriptContext): TxOutRef = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, _) => ref
        case _                                 => prelude.fail("not a spend")

    /** The credential of the contract: of the output `ctx` spends, the first input of its
      * reference, as the naive validator finds it.
      */
    inline def spentCredential(inline ctx: ScriptContext): Credential =
        ctx.txInfo.findInputOrFail(spentRef(ctx)).resolved.address.credential

    /** The credential of the contract: of the stake validator, whose withdrawal `ctx` is run for.
      */
    inline def rewardedCredential(inline ctx: ScriptContext): Credential = ctx.scriptInfo match
        case ScriptInfo.RewardingScript(credential) => credential
        case _                                      => prelude.fail("not a withdrawal")

    // A payout

    /** Whether `credential` is the key of one of `payees`. */
    inline def isAPayee(
        inline payees: prelude.List[ByteString],
        inline credential: Credential
    ): Boolean = credential match
        case Credential.PubKeyCredential(key) => payees.contains(key.hash)
        case Credential.ScriptCredential(_)   => false

    /** Whether the two payees of `payees` are different keys. */
    inline def areDifferent(inline payees: prelude.List[ByteString]): Boolean =
        payees.at(0) != payees.at(1)

    /** Whether `payee` submits `ctx`: it spends an input of the payee's key. */
    inline def submits(inline ctx: ScriptContext, inline payee: ByteString): Boolean = {
        val key = Credential.PubKeyCredential(PubKeyHash(payee))
        ctx.txInfo.inputs.exists(input => input.resolved.address.credential === key)
    }

    /** Whether `ctx` pays the key `payee`: it has an output to it. */
    inline def isPaid(inline ctx: ScriptContext, inline payee: ByteString): Boolean = {
        val key = Credential.PubKeyCredential(PubKeyHash(payee))
        ctx.txInfo.outputs.exists(output => output.address.credential === key)
    }

    /** The lovelace `ctx` pays the key `payee`, in all its outputs to it. */
    inline def paidTo(inline ctx: ScriptContext, inline payee: ByteString): BigInt = {
        val key = Credential.PubKeyCredential(PubKeyHash(payee))
        ctx.txInfo.outputs.foldLeft(BigInt(0))((sum, output) =>
            if output.address.credential === key then sum + output.value.getLovelace else sum
        )
    }

    /** The lovelace locked at `contract` that `ctx` spends: of its inputs at `contract` that are
      * not of a payee.
      */
    inline def lockedAt(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): BigInt = {
        val at = contract
        ctx.txInfo.inputs.foldLeft(BigInt(0))((sum, input) =>
            val credential = input.resolved.address.credential
            if credential === at && !isAPayee(payees, credential) then
                sum + input.resolved.value.getLovelace
            else sum
        )
    }

    /** Whether `paid` is the split of `locked` among `n` payees: `locked` divided by `n`, rounded
      * down, which is `n * paid <= locked < n * paid + n`.
      */
    inline def isTheSplit(inline n: BigInt, inline locked: BigInt, inline paid: BigInt): Boolean =
        n * paid <= locked && locked < n * paid + n

    /** Whether a payee that does not submit `ctx` is paid other than the split of what is locked at
      * `contract`.
      */
    inline def someOtherPayeeIsNotPaidTheSplit(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val locked = lockedAt(payees, ctx, contract)
        payees.exists(payee =>
            !submits(ctx, payee) && !isTheSplit(payees.length, locked, paidTo(ctx, payee))
        )
    }

    /** Whether a payee that does not submit `ctx` is paid other than an exact share of what is
      * locked at `contract`: of which the payees' shares make up all of it.
      */
    inline def someOtherPayeeIsNotPaidExactlyAShare(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val locked = lockedAt(payees, ctx, contract)
        payees.exists(payee => !submits(ctx, payee) && payees.length * paidTo(ctx, payee) != locked)
    }

    /** Whether `ctx` spends an input of another key than a payee's, and than `contract`. */
    inline def spendsAnInputOfNeither(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val at = contract
        ctx.txInfo.inputs.exists(input =>
            val credential = input.resolved.address.credential
            !isAPayee(payees, credential) && !(credential === at)
        )
    }

    /** Whether `ctx` spends an input at `contract`, not of a payee, that holds a token besides its
      * lovelace.
      */
    inline def spendsAContractInputWithTokens(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val at = contract
        ctx.txInfo.inputs.exists(input =>
            val credential = input.resolved.address.credential
            credential === at && !isAPayee(payees, credential) &&
            !input.resolved.value.withoutLovelace.isZero
        )
    }

    /** Whether `ctx` is a payout of what is locked at `contract` among `payees`: one input of a
      * payee, who submits it, and every other one at `contract`, of ADA alone; every output to a
      * payee, and every payee paid. What it pays each is not said.
      */
    inline def isAPayout(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val at = contract
        ctx.txInfo.inputs
            .filter(input => isAPayee(payees, input.resolved.address.credential))
            .length == BigInt(1) &&
        ctx.txInfo.inputs.forall(input =>
            val credential = input.resolved.address.credential
            isAPayee(payees, credential) ||
            (credential === at && input.resolved.value.withoutLovelace.isZero)
        ) &&
        ctx.txInfo.outputs.forall(output => isAPayee(payees, output.address.credential)) &&
        payees.forall(payee => isPaid(ctx, payee))
    }

    /** Whether `ctx` is a payout ([[isAPayout]]) that pays each payee who does not submit it the
      * split of what is locked at `contract`.
      */
    inline def isAPayoutOfTheSplit(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext,
        inline contract: Credential
    ): Boolean = {
        val at = contract
        isAPayout(payees, ctx, at) && !someOtherPayeeIsNotPaidTheSplit(payees, ctx, at)
    }

    // The optimized validator

    /** The input of `ctx` that the redeemer of its spend names, by its index. */
    inline def namedInput(inline ctx: ScriptContext): TxInInfo =
        ctx.txInfo.inputs.at(ctx.redeemer.to[SpendRedeemer].ownInputIndex)

    /** Whether the redeemer of `ctx`, a spend, names the input of the output it spends. */
    inline def namesTheSpentInput(inline ctx: ScriptContext): Boolean =
        namedInput(ctx).outRef === spentRef(ctx)

    /** Whether `ctx` withdraws from the stake credential of the script that `input` is at. */
    inline def isWithdrawnFrom(inline ctx: ScriptContext, inline input: TxInInfo): Boolean =
        input.resolved.address.credential match
            case Credential.ScriptCredential(hash) =>
                ctx.txInfo.withdrawals.get(Credential.ScriptCredential(hash)).isDefined
            case Credential.PubKeyCredential(_) => false

    /** Whether the redeemer of `ctx`, a withdrawal, claims an honest payout among `payees`, as the
      * stake validator checks it: the contract is a script, whose inputs are of ADA alone, and one
      * input is of a payee, the one that the redeemer names; every output is to a payee, the others
      * are each paid the split in one output, and the one who submits it at least that; and the
      * redeemer claims what is locked, the split and the number of payees.
      */
    inline def claimsAnHonestPayout(
        inline payees: prelude.List[ByteString],
        inline ctx: ScriptContext
    ): Boolean = {
        val claim = ctx.redeemer.to[SplitVerificationRedeemer]
        val contract = rewardedCredential(ctx)
        val submitter = claim.payeeWithChange.hash
        val key = Credential.PubKeyCredential(claim.payeeWithChange)
        val n = payees.length
        (contract match
            case Credential.ScriptCredential(_) => true
            case _                              => false
        ) &&
        isAPayout(payees, ctx, contract) && submits(ctx, submitter) &&
        ctx.txInfo.outputs.forall(output =>
            output.address.credential === key || output.value.getLovelace == claim.splitPerPayee
        ) &&
        payees.forall(payee =>
            payee == submitter || ctx.txInfo.outputs
                .filter(output =>
                    output.address.credential === Credential.PubKeyCredential(PubKeyHash(payee))
                )
                .length == BigInt(1)
        ) &&
        paidTo(ctx, submitter) >= claim.splitPerPayee &&
        claim.sumContractInputs == lockedAt(payees, ctx, contract) && claim.nPayed == n &&
        isTheSplit(n, claim.sumContractInputs, claim.splitPerPayee)
    }

    // The samples

    /** The keys and the script of the sample transactions. */
    inline def first: ByteString = hex"11111111111111111111111111111111111111111111111111111111"

    inline def second: ByteString = hex"22222222222222222222222222222222222222222222222222222222"

    inline def script: ByteString = hex"33333333333333333333333333333333333333333333333333333333"

    /** The contract's output that a payout spends, and the input of the payee who submits it. */
    inline def contractRef: TxOutRef =
        TxOutRef(TxId(hex"4444444444444444444444444444444444444444444444444444444444444444"), 0)

    inline def payerRef: TxOutRef =
        TxOutRef(TxId(hex"6666666666666666666666666666666666666666666666666666666666666666"), 0)

    /** The parameter of both validators in the samples: the two payees, by their keys. */
    inline def samplePayees: Data =
        prelude.List.Cons(first, prelude.List.Cons(second, prelude.List.Nil)).toData

    /** An output of `amount` lovelace to the key `key`. */
    inline def toKey(inline key: ByteString, inline amount: BigInt): TxOut =
        TxOut(
          Address(Credential.PubKeyCredential(PubKeyHash(key)), prelude.Option.None),
          Value.lovelace(amount)
        )

    /** The sample transaction: the payee `first` spends an input of 5 ADA and the contract's output
      * of 100 ADA, and pays `second` its split, 50 ADA, and itself the rest less the fee of 0.2
      * ADA. It withdraws nothing from the contract's stake credential, or `withdrawals`.
      */
    inline def sampleTx(
        inline withdrawals: prelude.SortedMap[Credential, BigInt]
    ): TxInfo =
        TxInfo(
          inputs = prelude.List.Cons(
            TxInInfo(payerRef, toKey(first, BigInt(5_000_000))),
            prelude.List.Cons(
              TxInInfo(
                contractRef,
                TxOut(
                  Address(Credential.ScriptCredential(script), prelude.Option.None),
                  Value.lovelace(BigInt(100_000_000))
                )
              ),
              prelude.List.Nil
            )
          ),
          outputs = prelude.List.Cons(
            toKey(first, BigInt(54_800_000)),
            prelude.List.Cons(toKey(second, BigInt(50_000_000)), prelude.List.Nil)
          ),
          fee = BigInt(200_000),
          withdrawals = withdrawals,
          id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
        )

    /** The sample payout, spending the contract's output with the naive validator. */
    inline def samplePayout: Data =
        ScriptContext(
          sampleTx(prelude.SortedMap.empty[Credential, BigInt]),
          BigInt(0).toData,
          ScriptInfo.SpendingScript(contractRef, prelude.Option.None)
        ).toData

    /** The withdrawal of nothing from the contract's stake credential. */
    inline def contractWithdrawal: prelude.SortedMap[Credential, BigInt] =
        prelude.SortedMap.singleton(Credential.ScriptCredential(script), BigInt(0))

    /** The sample payout's spend of the contract's output with the optimized validator, whose
      * redeemer names the input of that output.
      */
    inline def sampleSpend: Data =
        ScriptContext(
          sampleTx(contractWithdrawal),
          SpendRedeemer(BigInt(1)).toData,
          ScriptInfo.SpendingScript(contractRef, prelude.Option.None)
        ).toData

    /** The sample payout's run of the optimized validator for its withdrawal, whose redeemer claims
      * the payout.
      */
    inline def sampleReward: Data =
        ScriptContext(
          sampleTx(contractWithdrawal),
          SplitVerificationRedeemer(
            PubKeyHash(first),
            BigInt(100_000_000),
            BigInt(50_000_000),
            BigInt(2)
          ).toData,
          ScriptInfo.RewardingScript(Credential.ScriptCredential(script))
        ).toData
}
