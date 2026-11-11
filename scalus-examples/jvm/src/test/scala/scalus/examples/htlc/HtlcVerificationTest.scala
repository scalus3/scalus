package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{Interval, IntervalBound, IntervalBoundType, PubKeyHash, ScriptContext, ScriptInfo, TxId, TxInfo, TxOutRef}
import scalus.uplc.builtin.Builtins.{constrData, headList, mkCons, sha3_256, tailList, unConstrData}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite, Unfinished, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** Properties of [[HtlcValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * The statements are of every script context the ledger can give the script
  * (`forAllData[ScriptContext]`), with what they are about stated as a premise: a refund, a reveal,
  * a validity range. The validator reads the datum, the redeemer, the validity range and the
  * signatures, so a statement holds whatever the rest of the transaction is. Four groups:
  *   - the validator on every script context: what it rejects before it reads the transaction;
  *   - a refund: who may take it, and from when, and that the committer can;
  *   - a reveal: who may make it, until when and of what, and that the receiver can;
  *   - the two together, and the bounds: there is no time at which both are accepted, and whether a
  *     bound of the validity range is inclusive does not matter.
  *
  * Sample spends, decided by running the validator with the real hash, show that it accepts a
  * refund and a reveal.
  *
  * What is not proved:
  *   - anything of the hash. To the solver `sha3_256` is a function it knows nothing of, so a proof
  *     holds whatever the function is, and says nothing of how hard a preimage is to find. Nor can
  *     a counterexample name a preimage of an image, as a test below shows: that a reveal is
  *     accepted is shown by the samples, not by a negative control;
  *   - of a transaction of more than two signatures, what rests on the search of the signatures.
  *     The tactic cannot follow a loop over a list of unknown length. Lean proves of every context
  *     what the validator decides by the time, before it searches the signatures. The rest, whose
  *     tests of every context are tagged `Unfinished`, is stated again beside them, of the contexts
  *     of two signatures at most, with `Limited` in its name, where Lean proves it;
  *   - where the funds go. The validator asks for a signature and constrains no output;
  *   - the published script's bytes. The validator is compiled here with the tactic's options
  *     (`UplcBlaster.options`), not those of [[HtlcContract]]: the statements are about the same
  *     source.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]). The tests about the tactic keep nothing: one is canceled
  * there, and those tagged `Unfinished` need Lean.
  */
class HtlcVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    import HtlcVerificationTest.*

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
            "htlc"
          )
          .resolve("HtlcVerificationTest.proofs.json")
    )

    /** The validator's entry point, compiled with the tactic's options. */
    private val validator =
        FunctionDef.named("htlc", (context: Data) => HtlcValidator.validate(context))

    // The statements are declared by name in the suite's verifier, each before the test that
    // checks it, and call the validator. One declared with `refute` is a negative control: it is
    // expected to be refuted.
    verifier.addFunction(validator)

    /** The steps Lean's CEK may make per test of a statement about a refund or a reveal. The
      * validator has no loop but its search of the signatures, which there are two at most, so
      * every run ends within them.
      */
    private val budget = 4000

    /** The steps for a context the validator rejects before it reads the transaction. Lean runs
      * each test's program on its own, without the premise, and so also on the contexts that pass
      * the check: those runs go on into the signatures, of any number here, and the budget keeps
      * them short.
      */
    private val rejectionBudget = 600

    // The validator, on every script context

    private val outputWithoutADatumIsRejected = verifier.statement(
      "htlc.context.output_without_a_datum_is_rejected",
      forAllData[ScriptContext](ctx => spendsWithoutDatum(ctx) ==>: fails(validator, ctx.toData))
    )

    test("an output without a datum cannot be spent") {
        check(outputWithoutADatumIsRejected, rejectionBudget)
    }

    private val onlySpendingIsValidated = verifier.statement(
      "htlc.context.only_spending_is_validated",
      forAllData[ScriptContext](ctx => !Prop(isSpending(ctx)) ==>: fails(validator, ctx.toData))
    )

    test("the script validates nothing but spending") {
        check(onlySpendingIsValidated, rejectionBudget)
    }

    private val redeemerOfNeitherActionIsRejected = verifier.statement(
      "htlc.context.redeemer_of_neither_action_is_rejected",
      forAllData[ScriptContext](ctx =>
          (Prop(spendsWithDatum(ctx)) && !Prop(
            unConstrData(ctx.redeemer).fst == BigInt(0)
                || unConstrData(ctx.redeemer).fst == BigInt(1)
          )) ==>: fails(validator, ctx.toData)
      )
    )

    test("a redeemer that is neither action is rejected") {
        // Of an output with a datum, whatever the datum is. `Timeout` is the redeemer of tag 0 and
        // `Reveal` the one of tag 1. For a redeemer that is not a constructor the premise's test
        // fails, so its negation holds, and the validator fails as well.
        check(redeemerOfNeitherActionIsRejected, rejectionBudget)
    }

    // Each statement about a refund or a reveal is of every script context. The validator checks
    // the time before it searches the signatures, so what it rejects for the time it rejects of
    // every context, and Lean proves it so. What rests on the search of the signatures Lean does
    // not prove of every context: its counterexample is a list of more signatures than the
    // budget's runs search, on which the statement holds, and the test of it is tagged
    // `Unfinished`. Such a statement is stated again beside it, of the contexts of two signatures
    // at most, under the name with `Limited` (`forAllContextsMax2Signatures`), where Lean proves
    // it.

    /** The time of a check of a statement of every context that Lean does not prove: it gives its
      * spurious counterexample within a minute or two.
      */
    private val unfinishedTime = 5.minutes

    /** That Lean's counterexample to `statement`, of every context, is spurious: the statement
      * holds there, on a list of signatures longer than the budget's runs search.
      */
    private def spurious(statement: Statement): Unit = {
        val reason = inconclusive(
          statement,
          UplcBlaster(Budget.LeanSteps(budget), lean, unfinishedTime)
        )
        assert(reason.contains("is spurious"), reason)
    }

    // A refund

    private val refundRejectedUnlessTheCommitterSigns = verifier.statement(
      "htlc.refund.rejected_unless_the_committer_signs",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && !signedByTheCommitter(ctx)) ==>: fails(validator, ctx.toData)
      )
    )
    private val refundRejectedWhoeverSigns = verifier.refute(
      "htlc.refund.rejected_whoever_signs",
      forAllData[ScriptContext](ctx => isRefund(ctx) ==>: fails(validator, ctx.toData))
    )

    test(
      "nobody but the committer refunds, of every script context, not finished by Lean",
      Unfinished
    ) {
        // Lean's counterexample is a refund signed by more keys than the budget's runs search,
        // none of them the committer's.
        spurious(refundRejectedUnlessTheCommitterSigns)
    }

    private val refundRejectedUnlessTheCommitterSignsLimited = verifier.statement(
      "htlc.refund.rejected_unless_the_committer_signs_limited",
      forAllContextsMax2Signatures(ctx =>
          (isRefund(ctx) && !signedByTheCommitter(ctx)) ==>: fails(validator, ctx.toData)
      )
    )
    private val refundRejectedWhoeverSignsLimited = verifier.refute(
      "htlc.refund.rejected_whoever_signs_limited",
      forAllContextsMax2Signatures(ctx => isRefund(ctx) ==>: fails(validator, ctx.toData))
    )

    test("nobody but the committer refunds, of two signatures at most") {
        // Of every context of two signatures at most: one that nobody signed, one that another key
        // signed, and one that two other keys signed, whenever.
        check(refundRejectedUnlessTheCommitterSignsLimited, budget)
        // negative controls: with any signature, the solver finds a refund that is accepted, of
        // these contexts and of every one
        check(refundRejectedWhoeverSignsLimited, budget)
        check(refundRejectedWhoeverSigns, budget)
    }

    private val refundRejectedBeforeTheTimeout = verifier.statement(
      "htlc.refund.rejected_before_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && !startsFromTheTimeout(ctx)) ==>: fails(validator, ctx.toData)
      )
    )
    private val refundRejectedAtTheTimeout = verifier.refute(
      "htlc.refund.rejected_at_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && !startsAfterTheTimeout(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test("a refund is rejected unless its validity range starts at the timeout or later") {
        // Whoever signs it, and however many sign it: the validator checks the time first. The
        // committer too waits for the timeout. A range that starts at no time, at an infinity,
        // says nothing of when the transaction is not valid yet.
        check(refundRejectedBeforeTheTimeout, budget)
        // negative control: at the timeout itself a refund is accepted
        check(refundRejectedAtTheTimeout, budget)
    }

    private val committerRefundsFromTheTimeout = verifier.statement(
      "htlc.refund.committer_refunds_from_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && startsFromTheTimeout(ctx) && signedByTheCommitter(ctx)) ==>:
              succeeds(validator, ctx.toData)
      )
    )
    private val committerRefundsAtAnyTime = verifier.refute(
      "htlc.refund.committer_refunds_at_any_time",
      forAllData[ScriptContext](ctx =>
          (isRefund(ctx) && signedByTheCommitter(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    test(
      "from the timeout on, the committer refunds, of every script context, not finished by Lean",
      Unfinished
    ) {
        // Lean's counterexample is a refund signed by more keys than the budget's runs search,
        // the committer's late among them.
        spurious(committerRefundsFromTheTimeout)
    }

    private val committerRefundsFromTheTimeoutLimited = verifier.statement(
      "htlc.refund.committer_refunds_from_the_timeout_limited",
      forAllContextsMax2Signatures(ctx =>
          (isRefund(ctx) && startsFromTheTimeout(ctx) && signedByTheCommitter(ctx)) ==>:
              succeeds(validator, ctx.toData)
      )
    )
    private val committerRefundsAtAnyTimeLimited = verifier.refute(
      "htlc.refund.committer_refunds_at_any_time_limited",
      forAllContextsMax2Signatures(ctx =>
          (isRefund(ctx) && signedByTheCommitter(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    test("from the timeout on, the committer refunds, of two signatures at most") {
        // The funds are not stuck with a receiver who does not reveal, whatever else the
        // transaction has: an upper bound of its validity range, and another signature before or
        // after the committer's.
        check(committerRefundsFromTheTimeoutLimited, budget)
        // negative controls: not at every time, of these contexts and of every one
        check(committerRefundsAtAnyTimeLimited, budget)
        check(committerRefundsAtAnyTime, budget)
    }

    // A reveal

    private val revealRejectedUnlessTheReceiverSigns = verifier.statement(
      "htlc.reveal.rejected_unless_the_receiver_signs",
      forAllData[ScriptContext](ctx =>
          (isReveal(ctx) && !signedByTheReceiver(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test(
      "nobody but the receiver reveals, of every script context, not finished by Lean",
      Unfinished
    ) {
        spurious(revealRejectedUnlessTheReceiverSigns)
    }

    private val revealRejectedUnlessTheReceiverSignsLimited = verifier.statement(
      "htlc.reveal.rejected_unless_the_receiver_signs_limited",
      forAllContextsMax2Signatures(ctx =>
          (isReveal(ctx) && !signedByTheReceiver(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test("nobody but the receiver reveals, of two signatures at most") {
        // Of every context of two signatures at most, whatever is revealed, and whenever. That a
        // reveal the receiver signed is accepted, the samples show.
        check(revealRejectedUnlessTheReceiverSignsLimited, budget)
    }

    private val revealRejectedAfterTheTimeout = verifier.statement(
      "htlc.reveal.rejected_after_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isReveal(ctx) && !endsByTheTimeout(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test("a reveal is rejected unless its validity range ends by the timeout") {
        // Whoever signs it, however many sign it, and whatever is revealed: the validator checks
        // the time first. A range that ends at no time, at an infinity, could be included after
        // the timeout. That one which ends at the timeout is accepted, the samples show.
        check(revealRejectedAfterTheTimeout, budget)
    }

    private val revealRejectedWithoutAPreimage = verifier.statement(
      "htlc.reveal.rejected_without_a_preimage",
      forAllData[ScriptContext](ctx =>
          (isReveal(ctx) && !revealsAPreimage(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test(
      "a reveal of what is not a preimage of the image is rejected, of every script context, not " +
          "finished by Lean",
      Unfinished
    ) {
        // The validator checks the signature before the hash.
        spurious(revealRejectedWithoutAPreimage)
    }

    private val revealRejectedWithoutAPreimageLimited = verifier.statement(
      "htlc.reveal.rejected_without_a_preimage_limited",
      forAllContextsMax2Signatures(ctx =>
          (isReveal(ctx) && !revealsAPreimage(ctx)) ==>: fails(validator, ctx.toData)
      )
    )

    test("a reveal of what is not a preimage of the image is rejected, of two signatures at most") {
        // Whoever signs it, and whenever.
        check(revealRejectedWithoutAPreimageLimited, budget)
    }

    private val noRevealIsAccepted = verifier.statement(
      "htlc.reveal.no_reveal_is_accepted",
      forAllData[ScriptContext](ctx =>
          (isReveal(ctx) && endsByTheTimeout(ctx) && signedByTheReceiver(ctx)) ==>:
              fails(validator, ctx.toData)
      )
    )

    test("a counterexample cannot name a preimage of an image") {
        // Why a reveal that is accepted is shown by the samples, and not by a negative control.
        // The solver knows nothing of the hash, so to "no reveal is accepted" it gives a
        // counterexample whose datum has an image it takes for the hash of the preimage. Replayed
        // on the Scalus CEK, with the real hash, that reveal is rejected: the counterexample is
        // spurious, and the statement is not refuted. The tactic says that the budget is not at
        // fault, and names the hash. Of every context, as of two signatures at most.
        val spurious = inconclusive(
          noRevealIsAccepted,
          UplcBlaster(Budget.LeanSteps(budget), lean, 2.minutes)
        )
        assert(spurious.contains("is spurious"), spurious)
        assert(spurious.contains("The programs apply sha3_256"), spurious)
        assert(!spurious.contains("needs more than"), spurious)
    }

    private val receiverRevealsUpToTheTimeout = verifier.statement(
      "htlc.reveal.receiver_reveals_up_to_the_timeout",
      forAllData[ScriptContext](ctx =>
          (isReveal(ctx) && endsByTheTimeout(ctx) && signedByTheReceiver(ctx) &&
              revealsAPreimage(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    test(
      "up to the timeout, the receiver reveals any preimage of the image, of every script " +
          "context, not finished by Lean",
      Unfinished
    ) {
        spurious(receiverRevealsUpToTheTimeout)
    }

    private val receiverRevealsUpToTheTimeoutLimited = verifier.statement(
      "htlc.reveal.receiver_reveals_up_to_the_timeout_limited",
      forAllContextsMax2Signatures(ctx =>
          (isReveal(ctx) && endsByTheTimeout(ctx) && signedByTheReceiver(ctx) &&
              revealsAPreimage(ctx)) ==>: succeeds(validator, ctx.toData)
      )
    )

    test(
      "up to the timeout, the receiver reveals any preimage of the image, of two signatures at most"
    ) {
        // The receiver who knows the secret is paid, whatever else the transaction has: a lower
        // bound of its validity range, and another signature before or after the receiver's.
        check(receiverRevealsUpToTheTimeoutLimited, budget)
    }

    // A refund and a reveal, and the bounds

    // Two script contexts: not of one transaction, which the validator only ever sees, but of two,
    // each a run of the validator of its own, the receiver's reveal and the committer's refund of
    // outputs under one datum. The statement compares the two runs: no time is in the window of
    // both, so the committer cannot refund while the receiver may still reveal. It follows from
    // the two statements of the time above, a reveal ending by the timeout and a refund starting
    // at it or later; stated so, the solver does the arithmetic of the bounds.
    private val revealEndsBeforeRefundBegins = verifier.statement(
      "htlc.reveal_and_refund.reveal_ends_before_refund_begins",
      forAllData[ScriptContext](revealed =>
          forAllData[ScriptContext](refunded =>
              (isReveal(revealed) && isRefund(refunded) && datumOf(revealed) == datumOf(refunded)
                  && startsBeforeTheEnd(refunded, revealed)) ==>:
                  (fails(validator, revealed.toData) || fails(validator, refunded.toData))
          )
      )
    )

    test("an accepted reveal ends before any accepted refund begins") {
        // Two spends of outputs under one datum, each with what it has besides, signed by any
        // keys: where the refund's validity range starts before the reveal's ends, one of them is
        // rejected. A transaction is valid at a time before the end of its range and not before
        // its start, so there is no time at which both a reveal and a refund are valid. That a
        // reveal that ends at the timeout and a refund that starts at it are both accepted, the
        // samples show. The validator checks the time before it searches the signatures.
        check(revealEndsBeforeRefundBegins, budget)
    }

    private val closureOfTheLowerBoundDoesNotMatter = verifier.statement(
      "htlc.bounds.closure_of_the_lower_bound_does_not_matter",
      forAllData[ScriptContext](ctx =>
          succeeds(validator, ctx.toData) <=> succeeds(validator, withTheLowerBoundFlipped(ctx))
      )
    )
    private val closureOfTheUpperBoundDoesNotMatter = verifier.statement(
      "htlc.bounds.closure_of_the_upper_bound_does_not_matter",
      forAllData[ScriptContext](ctx =>
          succeeds(validator, ctx.toData) <=> succeeds(validator, withTheUpperBoundFlipped(ctx))
      )
    )

    test(
      "whether a bound of the validity range is inclusive does not matter to the validator, of " +
          "every script context, not finished by Lean",
      Unfinished
    ) {
        // An accepted run searches the signatures, of a list of any length: the statement is
        // neither proved nor refuted. With Lean-blaster 6c27ff9 Lean's counterexample was a
        // list longer than the budget's runs search, in about three minutes each; with a069197
        // the solver does not finish in the time limit (2026-11-11).
        List(closureOfTheLowerBoundDoesNotMatter, closureOfTheUpperBoundDoesNotMatter).foreach {
            statement =>
                val reason = inconclusive(
                  statement,
                  UplcBlaster(Budget.LeanSteps(budget), lean, unfinishedTime)
                )
                assert(reason.contains("did not finish"), reason)
        }
    }

    private val closureOfTheLowerBoundDoesNotMatterLimited = verifier.statement(
      "htlc.bounds.closure_of_the_lower_bound_does_not_matter_limited",
      forAllContextsMax2Signatures(ctx =>
          succeeds(validator, ctx.toData) <=> succeeds(validator, withTheLowerBoundFlipped(ctx))
      )
    )
    private val closureOfTheUpperBoundDoesNotMatterLimited = verifier.statement(
      "htlc.bounds.closure_of_the_upper_bound_does_not_matter_limited",
      forAllContextsMax2Signatures(ctx =>
          succeeds(validator, ctx.toData) <=> succeeds(validator, withTheUpperBoundFlipped(ctx))
      )
    )

    test(
      "whether a bound of the validity range is inclusive does not matter to the validator, of " +
          "two signatures at most"
    ) {
        // It accepts a context where it accepts the one whose bound is the other way. That a
        // refund's lower bound is inclusive, and a reveal's upper one exclusive, is how the ledger
        // gives them, and what the comparisons with the timeout rely on.
        check(closureOfTheLowerBoundDoesNotMatterLimited, budget)
        check(closureOfTheUpperBoundDoesNotMatterLimited, budget)
    }

    // Samples

    private val sampleRevealIsAccepted = verifier.statement(
      "htlc.samples.reveal_is_accepted",
      succeeds(
        validator,
        sampleSpending(
          Action.Reveal(hex"cafe").toData,
          Interval.entirelyBefore(BigInt(900)),
          signedBy(receiver)
        )
      )
    )
    private val sampleRevealUpToTheTimeoutIsAccepted = verifier.statement(
      "htlc.samples.reveal_up_to_the_timeout_is_accepted",
      succeeds(
        validator,
        sampleSpending(
          Action.Reveal(hex"cafe").toData,
          Interval.entirelyBefore(BigInt(1000)),
          signedBy(receiver)
        )
      )
    )
    private val sampleRefundIsAccepted = verifier.statement(
      "htlc.samples.refund_is_accepted",
      succeeds(
        validator,
        sampleSpending(Action.Timeout.toData, Interval.after(BigInt(1000)), signedBy(committer))
      )
    )
    private val unsignedSampleRevealIsRejected = verifier.statement(
      "htlc.samples.unsigned_reveal_is_rejected",
      fails(
        validator,
        sampleSpending(
          Action.Reveal(hex"cafe").toData,
          Interval.entirelyBefore(BigInt(900)),
          unsigned
        )
      )
    )
    private val unsignedSampleRefundIsRejected = verifier.statement(
      "htlc.samples.unsigned_refund_is_rejected",
      fails(
        validator,
        sampleSpending(Action.Timeout.toData, Interval.after(BigInt(1000)), unsigned)
      )
    )
    private val sampleRevealOfAnotherSecretIsRejected = verifier.statement(
      "htlc.samples.reveal_of_another_secret_is_rejected",
      fails(
        validator,
        sampleSpending(
          Action.Reveal(hex"beef").toData,
          Interval.entirelyBefore(BigInt(900)),
          signedBy(receiver)
        )
      )
    )
    private val sampleRevealAfterTheTimeoutIsRejected = verifier.statement(
      "htlc.samples.reveal_after_the_timeout_is_rejected",
      fails(
        validator,
        sampleSpending(
          Action.Reveal(hex"cafe").toData,
          Interval.entirelyBefore(BigInt(1001)),
          signedBy(receiver)
        )
      )
    )

    test("sample reveals and refunds are accepted on the Lean machine, and not unsigned or late") {
        // Closed statements: Lean decides them by running the validator, with the real hash, on
        // a transaction that the ledger's types build. They are what the statements above are not
        // vacuous of: the receiver's reveal of the secret is accepted, up to the timeout, and the
        // committer's refund from it, so that a reveal which ends at the timeout and a refund
        // which starts at it are both accepted.
        def kind(statement: Statement): ProofKind = artifactOf(check(statement, budget)).kind
        assert(kind(sampleRevealIsAccepted) == ProofKind.LeanNative)
        assert(kind(sampleRevealUpToTheTimeoutIsAccepted) == ProofKind.LeanNative)
        assert(kind(sampleRefundIsAccepted) == ProofKind.LeanNative)
        assert(kind(unsignedSampleRevealIsRejected) == ProofKind.LeanNative)
        assert(kind(unsignedSampleRefundIsRejected) == ProofKind.LeanNative)
        // The receiver's reveal of another secret is not accepted, nor one after the timeout.
        assert(kind(sampleRevealOfAnotherSecretIsRejected) == ProofKind.LeanNative)
        assert(kind(sampleRevealAfterTheTimeoutIsRejected) == ProofKind.LeanNative)
    }
}

/** The values and the premises the statements of [[HtlcVerificationTest]] are about. They are
  * `inline`, so that each is part of the expression it is used in, which the Scalus plugin
  * compiles. They live in an object: an `inline def` of the test class that uses another one would
  * leave a reference to the test's `this` in that expression.
  *
  * A premise reads the context as the validator does, and fails where the validator cannot read it,
  * as where the datum is not a [[Config]]: as a premise, it then does not hold.
  */
object HtlcVerificationTest {

    /** `body` of every script context of two signatures at most: the statements with `Limited` in
      * their names. The validator searches the signatures, and the tactic cannot follow a loop over
      * a list of unknown length: within the limit, its runs end.
      */
    inline def forAllContextsMax2Signatures(inline body: ScriptContext => Prop | Boolean): Prop =
        forAllDataLimited[ScriptContext](ctx => ctx.txInfo.signatories.length <= 2)(body)

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

    /** The datum of the output `ctx` spends. */
    inline def datumOf(inline ctx: ScriptContext): Data = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(_, datum) => datum.getOrFail("no datum")
        case _                                   => prelude.fail("not a spend")

    /** The datum of the output `ctx` spends, read as the validator reads it. */
    inline def configOf(inline ctx: ScriptContext): Config = datumOf(ctx).to[Config]

    /** Whether `ctx` is a refund: a spend of an output with a datum, with the redeemer `Timeout`.
      */
    inline def isRefund(inline ctx: ScriptContext): Boolean =
        spendsWithDatum(ctx) && (ctx.redeemer.to[Action] match
            case Action.Timeout   => true
            case Action.Reveal(_) => false)

    /** Whether `ctx` is a reveal: a spend of an output with a datum, with the redeemer `Reveal`. */
    inline def isReveal(inline ctx: ScriptContext): Boolean =
        spendsWithDatum(ctx) && (ctx.redeemer.to[Action] match
            case Action.Timeout   => false
            case Action.Reveal(_) => true)

    /** Whether the datum's committer signed `ctx`. */
    inline def signedByTheCommitter(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.isSignedBy(configOf(ctx).committer)

    /** Whether the datum's receiver signed `ctx`. */
    inline def signedByTheReceiver(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.isSignedBy(configOf(ctx).receiver)

    /** Whether the validity range of `ctx` starts at a time, the datum's timeout or later. */
    inline def startsFromTheTimeout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => configOf(ctx).timeout <= time
            case _                              => false

    /** Whether the validity range of `ctx` starts at a time after the datum's timeout. */
    inline def startsAfterTheTimeout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.validRange.from.boundType match
            case IntervalBoundType.Finite(time) => configOf(ctx).timeout < time
            case _                              => false

    /** Whether the validity range of `ctx` ends at a time, the datum's timeout or earlier. */
    inline def endsByTheTimeout(inline ctx: ScriptContext): Boolean =
        ctx.txInfo.validRange.to.boundType match
            case IntervalBoundType.Finite(time) => time <= configOf(ctx).timeout
            case _                              => false

    /** Whether the redeemer of `ctx`, a reveal, reveals a preimage of the datum's image. */
    inline def revealsAPreimage(inline ctx: ScriptContext): Boolean =
        ctx.redeemer.to[Action] match
            case Action.Reveal(preimage) => sha3_256(preimage) == configOf(ctx).image
            case Action.Timeout          => false

    /** Whether the validity range of `refunded` starts at a time before the one at which the range
      * of `revealed` ends. A range that starts or ends at no time the validator rejects anyway.
      */
    inline def startsBeforeTheEnd(
        inline refunded: ScriptContext,
        inline revealed: ScriptContext
    ): Boolean =
        refunded.txInfo.validRange.from.boundType match
            case IntervalBoundType.Finite(from) =>
                revealed.txInfo.validRange.to.boundType match
                    case IntervalBoundType.Finite(to) => from < to
                    case _                            => false
            case _ => false

    /** `context` with the validity range of its transaction `range`, both as `Data`. It is rebuilt
      * from the context's `Data`, as the ledger encodes a context: the range is the eighth field of
      * the transaction, and the rest is passed on as it is. A context of the ledger's types built
      * again with `toData` would encode every list of it again, which over lists of any length does
      * not end.
      */
    inline def withTheRange(inline context: Data, inline range: Data): Data = {
        val parts = unConstrData(context).snd
        val fields = unConstrData(headList(parts)).snd
        val second = tailList(fields)
        val third = tailList(second)
        val fourth = tailList(third)
        val fifth = tailList(fourth)
        val sixth = tailList(fifth)
        val seventh = tailList(sixth)
        val eighth = tailList(seventh)
        val transaction = constrData(
          0,
          mkCons(
            headList(fields),
            mkCons(
              headList(second),
              mkCons(
                headList(third),
                mkCons(
                  headList(fourth),
                  mkCons(
                    headList(fifth),
                    mkCons(
                      headList(sixth),
                      mkCons(headList(seventh), mkCons(range, tailList(eighth)))
                    )
                  )
                )
              )
            )
          )
        )
        constrData(0, mkCons(transaction, tailList(parts)))
    }

    /** `ctx` with the lower bound of its validity range inclusive where it is exclusive, and the
      * other way, as `Data`.
      */
    inline def withTheLowerBoundFlipped(inline ctx: ScriptContext): Data = {
        val range = ctx.txInfo.validRange
        withTheRange(
          ctx.toData,
          Interval(IntervalBound(range.from.boundType, !range.from.isInclusive), range.to).toData
        )
    }

    /** `ctx` with the upper bound of its validity range inclusive where it is exclusive, and the
      * other way, as `Data`.
      */
    inline def withTheUpperBoundFlipped(inline ctx: ScriptContext): Data = {
        val range = ctx.txInfo.validRange
        withTheRange(
          ctx.toData,
          Interval(range.from, IntervalBound(range.to.boundType, !range.to.isInclusive)).toData
        )
    }

    /** The committer and the receiver of the sample transactions. */
    inline def committer: PubKeyHash =
        PubKeyHash(hex"11111111111111111111111111111111111111111111111111111111")

    inline def receiver: PubKeyHash =
        PubKeyHash(hex"22222222222222222222222222222222222222222222222222222222")

    /** The datum of the sample transactions: its image is the hash of `cafe`, its timeout 1000. */
    inline def sample: Config = Config(committer, receiver, sha3_256(hex"cafe"), BigInt(1000))

    inline def htlcRef: TxOutRef =
        TxOutRef(TxId(hex"3333333333333333333333333333333333333333333333333333333333333333"), 0)

    inline def unsigned: prelude.List[PubKeyHash] = prelude.List.Nil

    /** The signature of `key` alone. */
    inline def signedBy(inline key: PubKeyHash): prelude.List[PubKeyHash] =
        prelude.List.Cons(key, prelude.List.Nil)

    /** The script context of spending the output locked under [[sample]], as the ledger's types
      * build it: a transaction without inputs and outputs.
      */
    inline def sampleSpending(
        inline redeemer: Data,
        inline range: Interval,
        inline signatories: prelude.List[PubKeyHash]
    ): Data = {
        ScriptContext(
          TxInfo(
            inputs = prelude.List.Nil,
            validRange = range,
            signatories = signatories,
            id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
          ),
          redeemer,
          ScriptInfo.SpendingScript(htlcRef, prelude.Option.Some(sample.toData))
        ).toData
    }
}
