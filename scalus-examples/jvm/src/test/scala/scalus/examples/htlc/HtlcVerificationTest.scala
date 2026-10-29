package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.{IntervalBound, IntervalBoundType, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext, ScriptInfo, TxId, TxInfo, TxOutRef}
import scalus.compiler.Compile
import scalus.examples.vesting.VestingVerificationTest.{noDatum, scriptContext, someDatum, spendingInfo}
import scalus.uplc.builtin.Builtins.{constrData, mkCons, mkNilData, sha3_256, unConstrData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** What the validator does not read of a script context that spends its output: the fields of the
  * transaction other than its validity range and its signatures, the bound of the validity range
  * that the action does not need, and the reference of the output. Each is any `Data`, so a
  * statement over an `Unread` holds whatever they are.
  */
case class Unread(
    inputs: Data,
    referenceInputs: Data,
    outputs: Data,
    fee: Data,
    mint: Data,
    certificates: Data,
    withdrawals: Data,
    redeemers: Data,
    data: Data,
    id: Data,
    votes: Data,
    proposalProcedures: Data,
    currentTreasuryAmount: Data,
    treasuryDonation: Data,
    otherBound: Data,
    ref: Data
) derives FromData,
      ToData

@Compile
object Unread

/** Properties of [[HtlcValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * The validator reads the datum, the redeemer, one bound of the validity range and the signatures.
  * The statements are about every datum, any committer, receiver, image and timeout, and about a
  * transaction whose other parts are any `Data` ([[Unread]]): that the validator reads nothing else
  * is so proved, and not only seen in its source. Four groups:
  *   - the validator on every script context of some form, whatever the transaction is;
  *   - a refund: who may take it, and from when, and that the committer can;
  *   - a reveal: who may make it, until when and of what, and that the receiver can;
  *   - the two together: there is no time at which both are accepted.
  *
  * What is not proved:
  *   - anything of the hash. To the solver `sha3_256` is a function it knows nothing of, so a proof
  *     holds whatever the function is, and says nothing of how hard a preimage is to find;
  *   - whether a bound of the validity range is inclusive. The validator reads the time of a bound
  *     and not that, as the last group shows: that the lower bound is inclusive and the upper one
  *     exclusive is the ledger's convention;
  *   - a transaction of any number of signatures: the validator searches them, and the tactic
  *     cannot follow a loop over a list of unknown length. Here a transaction has two at most;
  *   - where the funds go. The validator asks for a signature and constrains no output;
  *   - the published script's bytes. The validator is compiled here with the tactic's options
  *     (`UplcBlaster.options`), not those of [[HtlcContract]]: the statements are about the same
  *     source.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]). One test is about the tactic and keeps nothing: it is
  * canceled there.
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
      * validator has no loop but its search of the signatures, which here are two at most: a refund
      * takes about 750 steps and a reveal about 830, where the tactic finds the budget.
      */
    private val budget = 4000

    /** The steps for a context the validator rejects before it reads the transaction. Lean runs
      * each test's program on its own, without the premise, and so also on the contexts that pass
      * the check: those runs go on into a transaction that is any `Data`, and the budget keeps them
      * short. At the budget of a whole spend the three statements took nearly three minutes
      * together, and at this one a quarter of a minute.
      */
    private val rejectionBudget = 600

    // The validator, on every script context

    private val outputWithoutADatumIsRejected = verifier.statement(
      "htlc.context.output_without_a_datum_is_rejected",
      forAll[Data, Data, Data]((txInfo, redeemer, ref) =>
          fails(validator, scriptContext(txInfo, redeemer, spendingInfo(ref, noDatum)))
      )
    )

    test("an output without a datum cannot be spent") {
        check(outputWithoutADatumIsRejected, rejectionBudget)
    }

    private val onlySpendingIsValidated = verifier.statement(
      "htlc.context.only_spending_is_validated",
      forAll[Data, Data, Data]((txInfo, redeemer, scriptInfo) =>
          !Prop(unConstrData(scriptInfo).fst == BigInt(1)) ==>
              fails(validator, scriptContext(txInfo, redeemer, scriptInfo))
      )
    )

    test("the script validates nothing but spending") {
        // Spending is the script info of tag 1. For a script info that is not a constructor the
        // premise's test fails, so its negation holds, and the validator fails as well.
        check(onlySpendingIsValidated, rejectionBudget)
    }

    private val redeemerOfNeitherActionIsRejected = verifier.statement(
      "htlc.context.redeemer_of_neither_action_is_rejected",
      forAll[Data, Data, Data]((txInfo, ref, datum) =>
          forAll[Data](redeemer =>
              !Prop(
                unConstrData(redeemer).fst == BigInt(0)
                    || unConstrData(redeemer).fst == BigInt(1)
              ) ==> fails(
                validator,
                scriptContext(txInfo, redeemer, spendingInfo(ref, someDatum(datum)))
              )
          )
      )
    )

    test("a redeemer that is neither action is rejected") {
        // `Timeout` is the redeemer of tag 0 and `Reveal` the one of tag 1.
        check(redeemerOfNeitherActionIsRejected, rejectionBudget)
    }

    // A refund

    private val refundRejectedWhenAnotherKeySigns = verifier.statement(
      "htlc.refund.rejected_when_another_key_signs",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, signer) =>
              !Prop(signer == config.committer.hash) ==>
                  fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
          )
      )
    )
    private val refundRejectedWhenUnsigned = verifier.statement(
      "htlc.refund.rejected_when_unsigned",
      forAll[Unread, Config, BigInt]((rest, config, from) =>
          fails(validator, refund(config, from, unsigned, rest))
      )
    )
    private val refundRejectedWhenTwoOtherKeysSign = verifier.statement(
      "htlc.refund.rejected_when_two_other_keys_sign",
      forAll[Unread, Config, BigInt]((rest, config, from) =>
          forAll[ByteString, ByteString]((first, second) =>
              !Prop(first == config.committer.hash || second == config.committer.hash) ==>
                  fails(
                    validator,
                    refund(
                      config,
                      from,
                      signedByBoth(PubKeyHash(first), PubKeyHash(second)),
                      rest
                    )
                  )
          )
      )
    )
    private val refundRejectedWhoeverSigns = verifier.refute(
      "htlc.refund.rejected_whoever_signs",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, signer) =>
              fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
          )
      )
    )

    test("nobody but the committer refunds") {
        // For every datum and time, and whatever else the transaction has: one that another key
        // signed, one that nobody signed, and one that two other keys signed.
        check(refundRejectedWhenAnotherKeySigns, budget)
        check(refundRejectedWhenUnsigned, budget)
        check(refundRejectedWhenTwoOtherKeysSign, budget)
        // negative control: with any signature, the solver finds a refund that is accepted
        check(refundRejectedWhoeverSigns, budget)
    }

    private val refundRejectedBeforeTheTimeout = verifier.statement(
      "htlc.refund.rejected_before_the_timeout",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, signer) =>
              (from < config.timeout) ==>
                  fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
          )
      )
    )
    private val refundRejectedAtTheTimeout = verifier.refute(
      "htlc.refund.rejected_at_the_timeout",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, signer) =>
              (from <= config.timeout) ==>
                  fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
          )
      )
    )

    test("a refund is rejected before the timeout") {
        // Whoever signs it: the committer too waits for the timeout.
        check(refundRejectedBeforeTheTimeout, budget)
        // negative control: at the timeout itself a refund is accepted
        check(refundRejectedAtTheTimeout, budget)
    }

    private val refundRejectedFromNegInf = verifier.statement(
      "htlc.refund.rejected_from_neg_inf",
      forAll[Unread, Config]((rest, config) =>
          fails(
            validator,
            spending(
              config,
              Action.Timeout.toData,
              IntervalBound.negInf.toData,
              rest.otherBound,
              signedBy(config.committer),
              rest
            )
          )
      )
    )
    private val refundRejectedFromPosInf = verifier.statement(
      "htlc.refund.rejected_from_pos_inf",
      forAll[Unread, Config]((rest, config) =>
          fails(
            validator,
            spending(
              config,
              Action.Timeout.toData,
              IntervalBound.posInf.toData,
              rest.otherBound,
              signedBy(config.committer),
              rest
            )
          )
      )
    )

    test("a refund needs the lower bound of its validity range") {
        // A transaction whose range starts at no time says nothing of when it is not valid yet,
        // whatever its upper bound is.
        check(refundRejectedFromNegInf, budget)
        check(refundRejectedFromPosInf, budget)
    }

    private val committerRefundsFromTheTimeout = verifier.statement(
      "htlc.refund.committer_refunds_from_the_timeout",
      forAll[Unread, Config, BigInt]((rest, config, from) =>
          (config.timeout <= from) ==>
              succeeds(validator, refund(config, from, signedBy(config.committer), rest))
      )
    )
    private val committerRefundsAfterAnotherSignature = verifier.statement(
      "htlc.refund.committer_refunds_after_another_signature",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, other) =>
              (config.timeout <= from) ==> succeeds(
                validator,
                refund(config, from, signedByBoth(PubKeyHash(other), config.committer), rest)
              )
          )
      )
    )
    private val committerRefundsAtAnyTime = verifier.refute(
      "htlc.refund.committer_refunds_at_any_time",
      forAll[Unread, Config, BigInt]((rest, config, from) =>
          succeeds(validator, refund(config, from, signedBy(config.committer), rest))
      )
    )

    test("from the timeout on, the committer refunds") {
        // The funds are not stuck with a receiver who does not reveal, whatever else the
        // transaction has: an upper bound of its validity range too.
        check(committerRefundsFromTheTimeout, budget)
        // The committer's signature is found after another one.
        check(committerRefundsAfterAnotherSignature, budget)
        // negative control: not at every time
        check(committerRefundsAtAnyTime, budget)
    }

    // A reveal

    private val revealRejectedWhenAnotherKeySigns = verifier.statement(
      "htlc.reveal.rejected_when_another_key_signs",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              forAll[ByteString](signer =>
                  !Prop(signer == config.receiver.hash) ==> fails(
                    validator,
                    reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                  )
              )
          )
      )
    )
    private val revealRejectedWhenUnsigned = verifier.statement(
      "htlc.reveal.rejected_when_unsigned",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              fails(validator, reveal(config, preimage, to, unsigned, rest))
          )
      )
    )
    private val revealRejectedWhenTwoOtherKeysSign = verifier.statement(
      "htlc.reveal.rejected_when_two_other_keys_sign",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              forAll[ByteString, ByteString]((first, second) =>
                  !Prop(first == config.receiver.hash || second == config.receiver.hash) ==>
                      fails(
                        validator,
                        reveal(
                          config,
                          preimage,
                          to,
                          signedByBoth(PubKeyHash(first), PubKeyHash(second)),
                          rest
                        )
                      )
              )
          )
      )
    )
    private val revealRejectedWhoeverSigns = verifier.refute(
      "htlc.reveal.rejected_whoever_signs",
      forAll[Unread](rest =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt, ByteString]((timeout, to, signer) =>
                  fails(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedBy(PubKeyHash(signer)),
                      rest
                    )
                  )
              )
          )
      )
    )

    test("nobody but the receiver reveals") {
        // For every datum, preimage and time, also a preimage of the image, and whatever else the
        // transaction has: one that another key signed, one that nobody signed, and one that two
        // other keys signed.
        check(revealRejectedWhenAnotherKeySigns, budget)
        check(revealRejectedWhenUnsigned, budget)
        check(revealRejectedWhenTwoOtherKeysSign, budget)
        // negative control: with any signature, the solver finds a reveal that is accepted. The
        // image is written as the hash of the preimage: a counterexample is replayed with the
        // real hash, of which the solver cannot name a preimage.
        check(revealRejectedWhoeverSigns, budget)
    }

    private val revealRejectedAfterTheTimeout = verifier.statement(
      "htlc.reveal.rejected_after_the_timeout",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              forAll[ByteString](signer =>
                  (config.timeout < to) ==> fails(
                    validator,
                    reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                  )
              )
          )
      )
    )
    private val revealRejectedAtTheTimeout = verifier.refute(
      "htlc.reveal.rejected_at_the_timeout",
      forAll[Unread](rest =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt]((timeout, to) =>
                  (timeout <= to) ==> fails(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedBy(PubKeyHash(receiver)),
                      rest
                    )
                  )
              )
          )
      )
    )

    test("a reveal is rejected when its validity range ends after the timeout") {
        // Whoever signs it, and whatever is revealed.
        check(revealRejectedAfterTheTimeout, budget)
        // negative control: a range that ends at the timeout is accepted
        check(revealRejectedAtTheTimeout, budget)
    }

    private val revealRejectedUpToPosInf = verifier.statement(
      "htlc.reveal.rejected_up_to_pos_inf",
      forAll[Unread, Config, ByteString]((rest, config, preimage) =>
          fails(
            validator,
            spending(
              config,
              Action.Reveal(preimage).toData,
              rest.otherBound,
              IntervalBound.posInf.toData,
              signedBy(config.receiver),
              rest
            )
          )
      )
    )
    private val revealRejectedUpToNegInf = verifier.statement(
      "htlc.reveal.rejected_up_to_neg_inf",
      forAll[Unread, Config, ByteString]((rest, config, preimage) =>
          fails(
            validator,
            spending(
              config,
              Action.Reveal(preimage).toData,
              rest.otherBound,
              IntervalBound.negInf.toData,
              signedBy(config.receiver),
              rest
            )
          )
      )
    )

    test("a reveal needs the upper bound of its validity range") {
        // A transaction whose range ends at no time could be included after the timeout, whatever
        // its lower bound is.
        check(revealRejectedUpToPosInf, budget)
        check(revealRejectedUpToNegInf, budget)
    }

    private val revealRejectedWithoutAPreimage = verifier.statement(
      "htlc.reveal.rejected_without_a_preimage",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              forAll[ByteString](signer =>
                  !Prop(sha3_256(preimage) == config.image) ==> fails(
                    validator,
                    reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                  )
              )
          )
      )
    )

    test("a reveal of what is not a preimage of the image is rejected") {
        // Whoever signs it, and whenever. That a reveal is not rejected for another reason is the
        // statement of the test after the next: no counterexample shows it here.
        check(revealRejectedWithoutAPreimage, budget)
    }

    private val noRevealIsAccepted = verifier.statement(
      "htlc.reveal.no_reveal_is_accepted",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              fails(validator, reveal(config, preimage, to, signedBy(config.receiver), rest))
          )
      )
    )

    test("a counterexample cannot name a preimage of an image") {
        // Why the negative controls write the image as the hash of the preimage. The solver
        // knows nothing of the hash, so to "no reveal is accepted" it gives a counterexample with
        // an image it takes for the hash of the preimage. Replayed on the Scalus CEK, with the
        // real hash, that reveal is rejected: the counterexample is spurious, and the statement
        // is not refuted. The tactic says that the budget is not at fault, and names the hash.
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
      forAll[Unread](rest =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt]((timeout, to) =>
                  (to <= timeout) ==> succeeds(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedBy(PubKeyHash(receiver)),
                      rest
                    )
                  )
              )
          )
      )
    )
    private val receiverRevealsAfterAnotherSignature = verifier.statement(
      "htlc.reveal.receiver_reveals_after_another_signature",
      forAll[Unread](rest =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt, ByteString]((timeout, to, other) =>
                  (to <= timeout) ==> succeeds(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedByBoth(PubKeyHash(other), PubKeyHash(receiver)),
                      rest
                    )
                  )
              )
          )
      )
    )
    private val receiverRevealsAtAnyTime = verifier.refute(
      "htlc.reveal.receiver_reveals_at_any_time",
      forAll[Unread](rest =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt]((timeout, to) =>
                  succeeds(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedBy(PubKeyHash(receiver)),
                      rest
                    )
                  )
              )
          )
      )
    )

    test("up to the timeout, the receiver reveals any preimage of the image") {
        // The receiver who knows the secret is paid: for every committer, receiver and secret,
        // and whatever else the transaction has, a lower bound of its validity range too.
        check(receiverRevealsUpToTheTimeout, budget)
        // The receiver's signature is found after another one.
        check(receiverRevealsAfterAnotherSignature, budget)
        // negative control: not at every time
        check(receiverRevealsAtAnyTime, budget)
    }

    // A refund and a reveal

    private val revealEndsBeforeRefundBegins = verifier.statement(
      "htlc.reveal_and_refund.reveal_ends_before_refund_begins",
      forAll[Unread, Unread]((revealed, refunded) =>
          forAll[Config, ByteString, ByteString]((config, preimage, revealer) =>
              forAll[ByteString, BigInt, BigInt]((refunder, to, from) =>
                  (from < to) ==> (fails(
                    validator,
                    reveal(config, preimage, to, signedBy(PubKeyHash(revealer)), revealed)
                  ) || fails(
                    validator,
                    refund(config, from, signedBy(PubKeyHash(refunder)), refunded)
                  ))
              )
          )
      )
    )
    private val revealEndsStrictlyBeforeRefundBegins = verifier.refute(
      "htlc.reveal_and_refund.reveal_ends_strictly_before_refund_begins",
      forAll[Unread, Unread]((revealed, refunded) =>
          forAll[ByteString, ByteString, ByteString]((committer, receiver, preimage) =>
              forAll[BigInt, BigInt, BigInt]((timeout, to, from) =>
                  (from <= to) ==> (fails(
                    validator,
                    reveal(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      preimage,
                      to,
                      signedBy(PubKeyHash(receiver)),
                      revealed
                    )
                  ) || fails(
                    validator,
                    refund(
                      locked(committer, receiver, sha3_256(preimage), timeout),
                      from,
                      signedBy(PubKeyHash(committer)),
                      refunded
                    )
                  ))
              )
          )
      )
    )

    test("an accepted reveal ends before any accepted refund begins") {
        // Two runs of the validator on one datum: two transactions, each with what it has
        // besides, signed by any two keys. A transaction is valid at a time before the end of its
        // range and not before its start, so there is no time at which both a reveal and a
        // refund are valid.
        check(revealEndsBeforeRefundBegins, budget)
        // negative control: a reveal that ends at the timeout and a refund that begins at it are
        // both accepted
        check(revealEndsStrictlyBeforeRefundBegins, budget)
    }

    private val refundIgnoresWhetherItsLowerBoundIsInclusive = verifier.statement(
      "htlc.bounds.refund_ignores_whether_its_lower_bound_is_inclusive",
      forAll[Unread](rest =>
          forAll[Config, BigInt, ByteString]((config, from, signer) =>
              succeeds(
                validator,
                refundFrom(config, from, true, signedBy(PubKeyHash(signer)), rest)
              ) <=> succeeds(
                validator,
                refundFrom(config, from, false, signedBy(PubKeyHash(signer)), rest)
              )
          )
      )
    )
    private val revealIgnoresWhetherItsUpperBoundIsInclusive = verifier.statement(
      "htlc.bounds.reveal_ignores_whether_its_upper_bound_is_inclusive",
      forAll[Unread](rest =>
          forAll[Config, ByteString, BigInt]((config, preimage, to) =>
              forAll[ByteString](signer =>
                  succeeds(
                    validator,
                    revealUpTo(config, preimage, to, true, signedBy(PubKeyHash(signer)), rest)
                  ) <=> succeeds(
                    validator,
                    revealUpTo(config, preimage, to, false, signedBy(PubKeyHash(signer)), rest)
                  )
              )
          )
      )
    )

    test("whether a bound of the validity range is inclusive does not matter to the validator") {
        // It accepts a transaction with the one closure of a bound where it accepts it with the
        // other. That a refund's lower bound is inclusive, and a reveal's upper one exclusive, is
        // how the ledger gives them, and what the comparisons with the timeout rely on.
        check(refundIgnoresWhetherItsLowerBoundIsInclusive, budget)
        check(revealIgnoresWhetherItsUpperBoundIsInclusive, budget)
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

    test("a sample reveal and a sample refund are accepted on the Lean machine, and not unsigned") {
        // Closed statements: Lean decides them by running the validator, with the real hash, on
        // a transaction that the ledger's types build.
        def kind(statement: Statement): ProofKind = artifactOf(check(statement, budget)).kind
        assert(kind(sampleRevealIsAccepted) == ProofKind.LeanNative)
        assert(kind(sampleRefundIsAccepted) == ProofKind.LeanNative)
        assert(kind(unsignedSampleRevealIsRejected) == ProofKind.LeanNative)
        assert(kind(unsignedSampleRefundIsRejected) == ProofKind.LeanNative)
        // The receiver's reveal of another secret is not accepted either.
        assert(kind(sampleRevealOfAnotherSecretIsRejected) == ProofKind.LeanNative)
    }
}

/** The values the statements of [[HtlcVerificationTest]] are about. They are `inline`, so that each
  * is part of the expression it is used in, which the Scalus plugin compiles. They live in an
  * object: an `inline def` of the test class that uses another one would leave a reference to the
  * test's `this` in that expression.
  */
object HtlcVerificationTest {

    /** The committer and the receiver of the sample transactions. */
    inline def committer: PubKeyHash =
        PubKeyHash(hex"11111111111111111111111111111111111111111111111111111111")

    inline def receiver: PubKeyHash =
        PubKeyHash(hex"22222222222222222222222222222222222222222222222222222222")

    /** The datum of the sample transactions: its image is the hash of `cafe`. */
    inline def sample: Config = Config(committer, receiver, sha3_256(hex"cafe"), BigInt(1000))

    /** A datum from the keys of its two parties. */
    inline def locked(
        inline committer: ByteString,
        inline receiver: ByteString,
        inline image: ByteString,
        inline timeout: BigInt
    ): Config = Config(PubKeyHash(committer), PubKeyHash(receiver), image, timeout)

    inline def htlcRef: TxOutRef =
        TxOutRef(TxId(hex"3333333333333333333333333333333333333333333333333333333333333333"), 0)

    inline def unsigned: prelude.List[PubKeyHash] = prelude.List.Nil

    /** The signature of `key` alone. */
    inline def signedBy(inline key: PubKeyHash): prelude.List[PubKeyHash] =
        prelude.List.Cons(key, prelude.List.Nil)

    /** Two signatures, in this order. */
    inline def signedByBoth(
        inline first: PubKeyHash,
        inline second: PubKeyHash
    ): prelude.List[PubKeyHash] =
        prelude.List.Cons(first, prelude.List.Cons(second, prelude.List.Nil))

    /** The script context of spending the output locked under `config`, with `redeemer`, in a
      * transaction whose validity range is from the bound `from` to the bound `to`, both encoded,
      * and which `signatories` signed. It is written as the ledger encodes it, field by field: what
      * the validator does not read is `unread`, any `Data`.
      */
    inline def spending(
        inline config: Config,
        inline redeemer: Data,
        inline from: Data,
        inline to: Data,
        inline signatories: prelude.List[PubKeyHash],
        inline unread: Unread
    ): Data = {
        scriptContext(
          constrData(
            0,
            mkCons(
              unread.inputs,
              mkCons(
                unread.referenceInputs,
                mkCons(
                  unread.outputs,
                  mkCons(
                    unread.fee,
                    mkCons(
                      unread.mint,
                      mkCons(
                        unread.certificates,
                        mkCons(
                          unread.withdrawals,
                          mkCons(
                            constrData(0, mkCons(from, mkCons(to, mkNilData()))),
                            mkCons(
                              signatories.toData,
                              mkCons(
                                unread.redeemers,
                                mkCons(
                                  unread.data,
                                  mkCons(
                                    unread.id,
                                    mkCons(
                                      unread.votes,
                                      mkCons(
                                        unread.proposalProcedures,
                                        mkCons(
                                          unread.currentTreasuryAmount,
                                          mkCons(unread.treasuryDonation, mkNilData())
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          redeemer,
          spendingInfo(unread.ref, someDatum(config.toData))
        )
    }

    /** A refund in a transaction valid from `from`, a bound that is inclusive or not. Its upper
      * bound is the one of `unread`.
      */
    inline def refundFrom(
        inline config: Config,
        inline from: BigInt,
        inline inclusive: Boolean,
        inline signatories: prelude.List[PubKeyHash],
        inline unread: Unread
    ): Data = {
        spending(
          config,
          Action.Timeout.toData,
          IntervalBound(IntervalBoundType.Finite(from), inclusive).toData,
          unread.otherBound,
          signatories,
          unread
        )
    }

    /** A refund in a transaction valid from `from`, inclusive, as the ledger gives the bound. */
    inline def refund(
        inline config: Config,
        inline from: BigInt,
        inline signatories: prelude.List[PubKeyHash],
        inline unread: Unread
    ): Data = refundFrom(config, from, true, signatories, unread)

    /** A reveal of `preimage` in a transaction valid up to `to`, a bound that is inclusive or not.
      * Its lower bound is the one of `unread`.
      */
    inline def revealUpTo(
        inline config: Config,
        inline preimage: ByteString,
        inline to: BigInt,
        inline inclusive: Boolean,
        inline signatories: prelude.List[PubKeyHash],
        inline unread: Unread
    ): Data = {
        spending(
          config,
          Action.Reveal(preimage).toData,
          unread.otherBound,
          IntervalBound(IntervalBoundType.Finite(to), inclusive).toData,
          signatories,
          unread
        )
    }

    /** A reveal of `preimage` in a transaction valid up to `to`, exclusive, as the ledger gives the
      * bound.
      */
    inline def reveal(
        inline config: Config,
        inline preimage: ByteString,
        inline to: BigInt,
        inline signatories: prelude.List[PubKeyHash],
        inline unread: Unread
    ): Data = revealUpTo(config, preimage, to, false, signatories, unread)

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
