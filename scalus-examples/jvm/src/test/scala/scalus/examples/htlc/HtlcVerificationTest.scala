package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.{IntervalBound, IntervalBoundType, PubKeyHash}
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
import scalus.verify.uplcblaster.LeanProofs

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
class HtlcVerificationTest extends AnyFunSuite with LeanProofs {
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

    test("the contexts built from Data are the ledger's encoding") {
        // The statements below say that the validator fails, which a malformed context would make
        // true for the wrong reason.
        val ref = TxOutRef(TxId(hex"00"), 0)
        val datum = Config(committer, receiver, hex"ab", BigInt(7)).toData
        val timeout: Action = Action.Timeout
        assert(timeoutRedeemer == timeout.toData)
        def expected(datum: prelude.Option[Data]) = {
            ScriptContext(
              TxInfo.placeholder,
              timeout.toData,
              ScriptInfo.SpendingScript(ref, datum)
            ).toData
        }
        def built(datum: Data) = {
            scriptContext(
              TxInfo.placeholder.toData,
              timeoutRedeemer,
              spendingInfo(ref.toData, datum)
            )
        }
        assert(built(noDatum) == expected(prelude.Option.None))
        assert(built(someDatum(datum)) == expected(prelude.Option.Some(datum)))
    }

    test("a refund and a reveal written field by field are the ledger's encoding") {
        // The statements about them write the transaction out, so that what the validator does
        // not read can be any `Data`. With the fields of a transaction that the ledger's types
        // build, they are the context of that transaction.
        val from = IntervalBound(IntervalBoundType.Finite(BigInt(5)), true)
        val to = IntervalBound(IntervalBoundType.Finite(BigInt(9)), false)
        val txInfo = TxInfo(
          inputs = prelude.List.Nil,
          validRange = Interval(from, to),
          signatories = signedBy(committer),
          id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
        )
        val fields = unConstrData(txInfo.toData).snd.toList
        def unread(otherBound: Data) = Unread(
          inputs = fields(0),
          referenceInputs = fields(1),
          outputs = fields(2),
          fee = fields(3),
          mint = fields(4),
          certificates = fields(5),
          withdrawals = fields(6),
          redeemers = fields(9),
          data = fields(10),
          id = fields(11),
          votes = fields(12),
          proposalProcedures = fields(13),
          currentTreasuryAmount = fields(14),
          treasuryDonation = fields(15),
          otherBound = otherBound,
          ref = htlcRef.toData
        )
        def expected(redeemer: Data) = {
            ScriptContext(
              txInfo,
              redeemer,
              ScriptInfo.SpendingScript(htlcRef, prelude.Option.Some(sample.toData))
            ).toData
        }
        assert(
          refund(sample, BigInt(5), signedBy(committer), unread(to.toData)) ==
              expected(timeoutRedeemer)
        )
        assert(
          reveal(sample, hex"cafe", BigInt(9), signedBy(committer), unread(from.toData)) ==
              expected(Action.Reveal(hex"cafe").toData)
        )
    }

    test("an output without a datum cannot be spent") {
        proven(
          forAll[Data, Data, Data]((txInfo, redeemer, ref) =>
              fails(validator, scriptContext(txInfo, redeemer, spendingInfo(ref, noDatum)))
          ),
          rejectionBudget,
          validator
        )
    }

    test("the script validates nothing but spending") {
        // Spending is the script info of tag 1. For a script info that is not a constructor the
        // premise's test fails, so its negation holds, and the validator fails as well.
        proven(
          forAll[Data, Data, Data]((txInfo, redeemer, scriptInfo) =>
              !Prop(unConstrData(scriptInfo).fst == BigInt(1)) ==>
                  fails(validator, scriptContext(txInfo, redeemer, scriptInfo))
          ),
          rejectionBudget,
          validator
        )
    }

    test("a redeemer that is neither action is rejected") {
        // `Timeout` is the redeemer of tag 0 and `Reveal` the one of tag 1.
        proven(
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
          ),
          rejectionBudget,
          validator
        )
    }

    // A refund

    test("nobody but the committer refunds") {
        // For every datum and time, and whatever else the transaction has: one that another key
        // signed, one that nobody signed, and one that two other keys signed.
        proven(
          forAll[Unread](rest =>
              forAll[Config, BigInt, ByteString]((config, from, signer) =>
                  !Prop(signer == config.committer.hash) ==>
                      fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
              )
          ),
          budget,
          validator
        )
        proven(
          forAll[Unread, Config, BigInt]((rest, config, from) =>
              fails(validator, refund(config, from, unsigned, rest))
          ),
          budget,
          validator
        )
        proven(
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
          ),
          budget,
          validator
        )
        // negative control: with any signature, the solver finds a refund that is accepted
        refuted(
          forAll[Unread](rest =>
              forAll[Config, BigInt, ByteString]((config, from, signer) =>
                  fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
              )
          ),
          budget,
          validator
        )
    }

    test("a refund is rejected before the timeout") {
        // Whoever signs it: the committer too waits for the timeout.
        proven(
          forAll[Unread](rest =>
              forAll[Config, BigInt, ByteString]((config, from, signer) =>
                  (from < config.timeout) ==>
                      fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
              )
          ),
          budget,
          validator
        )
        // negative control: at the timeout itself a refund is accepted
        refuted(
          forAll[Unread](rest =>
              forAll[Config, BigInt, ByteString]((config, from, signer) =>
                  (from <= config.timeout) ==>
                      fails(validator, refund(config, from, signedBy(PubKeyHash(signer)), rest))
              )
          ),
          budget,
          validator
        )
    }

    test("a refund needs the lower bound of its validity range") {
        // A transaction whose range starts at no time says nothing of when it is not valid yet,
        // whatever its upper bound is.
        proven(
          forAll[Unread, Config]((rest, config) =>
              fails(
                validator,
                spending(
                  config,
                  timeoutRedeemer,
                  IntervalBound.negInf.toData,
                  rest.otherBound,
                  signedBy(config.committer),
                  rest
                )
              )
          ),
          budget,
          validator
        )
        proven(
          forAll[Unread, Config]((rest, config) =>
              fails(
                validator,
                spending(
                  config,
                  timeoutRedeemer,
                  IntervalBound.posInf.toData,
                  rest.otherBound,
                  signedBy(config.committer),
                  rest
                )
              )
          ),
          budget,
          validator
        )
    }

    test("from the timeout on, the committer refunds") {
        // The funds are not stuck with a receiver who does not reveal, whatever else the
        // transaction has: an upper bound of its validity range too.
        proven(
          forAll[Unread, Config, BigInt]((rest, config, from) =>
              (config.timeout <= from) ==>
                  succeeds(validator, refund(config, from, signedBy(config.committer), rest))
          ),
          budget,
          validator
        )
        // The committer's signature is found after another one.
        proven(
          forAll[Unread](rest =>
              forAll[Config, BigInt, ByteString]((config, from, other) =>
                  (config.timeout <= from) ==> succeeds(
                    validator,
                    refund(config, from, signedByBoth(PubKeyHash(other), config.committer), rest)
                  )
              )
          ),
          budget,
          validator
        )
        // negative control: not at every time
        refuted(
          forAll[Unread, Config, BigInt]((rest, config, from) =>
              succeeds(validator, refund(config, from, signedBy(config.committer), rest))
          ),
          budget,
          validator
        )
    }

    // A reveal

    test("nobody but the receiver reveals") {
        // For every datum, preimage and time, also a preimage of the image, and whatever else the
        // transaction has: one that another key signed, one that nobody signed, and one that two
        // other keys signed.
        proven(
          forAll[Unread](rest =>
              forAll[Config, ByteString, BigInt]((config, preimage, to) =>
                  forAll[ByteString](signer =>
                      !Prop(signer == config.receiver.hash) ==> fails(
                        validator,
                        reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                      )
                  )
              )
          ),
          budget,
          validator
        )
        proven(
          forAll[Unread](rest =>
              forAll[Config, ByteString, BigInt]((config, preimage, to) =>
                  fails(validator, reveal(config, preimage, to, unsigned, rest))
              )
          ),
          budget,
          validator
        )
        proven(
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
          ),
          budget,
          validator
        )
        // negative control: with any signature, the solver finds a reveal that is accepted. The
        // image is written as the hash of the preimage: a counterexample is replayed with the
        // real hash, of which the solver cannot name a preimage.
        refuted(
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
          ),
          budget,
          validator
        )
    }

    test("a reveal is rejected when its validity range ends after the timeout") {
        // Whoever signs it, and whatever is revealed.
        proven(
          forAll[Unread](rest =>
              forAll[Config, ByteString, BigInt]((config, preimage, to) =>
                  forAll[ByteString](signer =>
                      (config.timeout < to) ==> fails(
                        validator,
                        reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                      )
                  )
              )
          ),
          budget,
          validator
        )
        // negative control: a range that ends at the timeout is accepted
        refuted(
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
          ),
          budget,
          validator
        )
    }

    test("a reveal needs the upper bound of its validity range") {
        // A transaction whose range ends at no time could be included after the timeout, whatever
        // its lower bound is.
        proven(
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
          ),
          budget,
          validator
        )
        proven(
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
          ),
          budget,
          validator
        )
    }

    test("a reveal of what is not a preimage of the image is rejected") {
        // Whoever signs it, and whenever. That a reveal is not rejected for another reason is the
        // statement of the test after the next: no counterexample shows it here.
        proven(
          forAll[Unread](rest =>
              forAll[Config, ByteString, BigInt]((config, preimage, to) =>
                  forAll[ByteString](signer =>
                      !Prop(sha3_256(preimage) == config.image) ==> fails(
                        validator,
                        reveal(config, preimage, to, signedBy(PubKeyHash(signer)), rest)
                      )
                  )
              )
          ),
          budget,
          validator
        )
    }

    test("a counterexample cannot name a preimage of an image") {
        // Why the negative controls write the image as the hash of the preimage. The solver
        // knows nothing of the hash, so to "no reveal is accepted" it gives a counterexample with
        // an image it takes for the hash of the preimage. Replayed on the Scalus CEK, with the
        // real hash, that reveal is rejected: the counterexample is spurious, and the statement
        // is not refuted. The tactic says that the budget is not at fault, and names the hash.
        val spurious = inconclusive(
          forAll[Unread](rest =>
              forAll[Config, ByteString, BigInt]((config, preimage, to) =>
                  fails(validator, reveal(config, preimage, to, signedBy(config.receiver), rest))
              )
          ),
          budget,
          2.minutes,
          validator
        )
        assert(spurious.contains("is spurious"), spurious)
        assert(spurious.contains("The programs apply sha3_256"), spurious)
        assert(!spurious.contains("needs more than"), spurious)
    }

    test("up to the timeout, the receiver reveals any preimage of the image") {
        // The receiver who knows the secret is paid: for every committer, receiver and secret,
        // and whatever else the transaction has, a lower bound of its validity range too.
        proven(
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
          ),
          budget,
          validator
        )
        // The receiver's signature is found after another one.
        proven(
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
          ),
          budget,
          validator
        )
        // negative control: not at every time
        refuted(
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
          ),
          budget,
          validator
        )
    }

    // A refund and a reveal

    test("an accepted reveal ends before any accepted refund begins") {
        // Two runs of the validator on one datum: two transactions, each with what it has
        // besides, signed by any two keys. A transaction is valid at a time before the end of its
        // range and not before its start, so there is no time at which both a reveal and a
        // refund are valid.
        proven(
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
          ),
          budget,
          validator
        )
        // negative control: a reveal that ends at the timeout and a refund that begins at it are
        // both accepted
        refuted(
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
          ),
          budget,
          validator
        )
    }

    test("whether a bound of the validity range is inclusive does not matter to the validator") {
        // It accepts a transaction with the one closure of a bound where it accepts it with the
        // other. That a refund's lower bound is inclusive, and a reveal's upper one exclusive, is
        // how the ledger gives them, and what the comparisons with the timeout rely on.
        proven(
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
          ),
          budget,
          validator
        )
        proven(
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
          ),
          budget,
          validator
        )
    }

    // Samples

    test("a sample reveal and a sample refund are accepted on the Lean machine, and not unsigned") {
        // Closed statements: Lean decides them by running the validator, with the real hash, on
        // a transaction that the ledger's types build.
        def kind(statement: Prop): ProofKind = proven(statement, budget, validator)
        val revealed = succeeds(
          validator,
          sampleSpending(
            Action.Reveal(hex"cafe").toData,
            Interval.entirelyBefore(BigInt(900)),
            signedBy(receiver)
          )
        )
        assert(kind(revealed) == ProofKind.LeanNative)
        val refunded = succeeds(
          validator,
          sampleSpending(timeoutRedeemer, Interval.after(BigInt(1000)), signedBy(committer))
        )
        assert(kind(refunded) == ProofKind.LeanNative)
        val unsignedReveal = fails(
          validator,
          sampleSpending(
            Action.Reveal(hex"cafe").toData,
            Interval.entirelyBefore(BigInt(900)),
            unsigned
          )
        )
        assert(kind(unsignedReveal) == ProofKind.LeanNative)
        val unsignedRefund = fails(
          validator,
          sampleSpending(timeoutRedeemer, Interval.after(BigInt(1000)), unsigned)
        )
        assert(kind(unsignedRefund) == ProofKind.LeanNative)
        // The receiver's reveal of another secret is not accepted either.
        val wrong = fails(
          validator,
          sampleSpending(
            Action.Reveal(hex"beef").toData,
            Interval.entirelyBefore(BigInt(900)),
            signedBy(receiver)
          )
        )
        assert(kind(wrong) == ProofKind.LeanNative)
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

    /** `Action.Timeout` as the ledger encodes it. It is written out: `Action.Timeout.toData` does
      * not lower ("Unsupported conversion for Action$.Timeout from ProdDataList to DataConstr",
      * https://github.com/scalus3/scalus/issues/377).
      */
    inline def timeoutRedeemer: Data = constrData(0, mkNilData())

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
          timeoutRedeemer,
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
