package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOut, TxOutRef}
import scalus.uplc.builtin.Builtins.{constrData, mkCons, mkNilData, unConstrData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{LeanProofs, Unfinished, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** The numbers of a withdrawal of the shape of [[VestingVerificationTest.withdrawal]], as one value
  * a statement can quantify over.
  */
case class Withdrawal(
    start: BigInt,
    duration: BigInt,
    amount: BigInt,
    locked: BigInt,
    requested: BigInt,
    time: BigInt,
    fee: BigInt,
    paid: BigInt,
    signer: ByteString
) derives FromData,
      ToData

@Compile
object Withdrawal

/** Properties of [[VestingValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * Three groups of statements, from the most general to the most specific:
  *   - the vesting schedule, `linearVesting`, and its contract, for every datum and time;
  *   - the validator on every script context of some form, whatever the transaction is;
  *   - the validator on withdrawals of one shape, see [[withdrawal]]: who may withdraw, how much,
  *     and that a vested amount can be withdrawn.
  *
  * Two limits of what is proved:
  *   - the validator is compiled here with the tactic's options (`UplcBlaster.options`), which
  *     differ from those of [[VestingContract]]: Lean's model has no `Value` builtins yet. The
  *     statements are about the same source, not about the published script's bytes;
  *   - a statement about withdrawals says nothing about transactions of another shape. The
  *     validator walks the inputs and the outputs, and the tactic cannot follow a loop over a list
  *     of unknown length.
  *
  * The tests need Lean: without `lake` and a built workspace they are canceled (see
  * [[LeanProofs]]).
  */
class VestingVerificationTest extends AnyFunSuite with LeanProofs {
    import VestingVerificationTest.*

    /** The example's own Lean workspace, `src/test/lean/LinearVesting`. It requires Scalus's Lean
      * library, and has that library's packages where the library's workspace has them.
      */
    override protected def leanWorkspace: Path =
        LeanProofs.inSources("scalus-examples", "jvm", "src", "test", "lean", "LinearVesting")

    test("the example's Lean workspace has the Lean and the packages of Scalus's library") {
        // The two workspaces share the clones of the packages and what is built of them. A
        // manifest that names another revision would check it out for both, and another Lean
        // would build them again, each time the other workspace is used.
        val library = LeanProofs.librarySources
        assert(LeanProofs.pinned(library).nonEmpty)
        assert(LeanProofs.pinned(leanWorkspace) == LeanProofs.pinned(library))
        assert(LeanProofs.toolchain(leanWorkspace) == LeanProofs.toolchain(library))
    }

    // The schedule

    private val linearVesting = FunctionDef(VestingValidator.linearVesting)

    /** The steps Lean's CEK may make per test of a statement about the schedule. It has no loop, so
      * its step count does not depend on its input.
      */
    private val scheduleBudget = 400

    test("nothing is vested before the start") {
        proven(
          forAll[Config, BigInt]((config, time) =>
              (time < config.startTimestamp) ==>
                  call(linearVesting, (config, time))(vested => vested == BigInt(0))
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("everything is vested from the end of the period on") {
        proven(
          forAll[Config, BigInt]((config, time) =>
              (config.startTimestamp <= time
                  && config.startTimestamp + config.duration <= time) ==>
                  call(linearVesting, (config, time))(vested => vested == config.initialAmount)
          ),
          scheduleBudget,
          linearVesting
        )
    }

    test("the schedule returns for every datum and time") {
        // Whatever the datum's numbers are: it does not divide by a zero duration.
        proven(
          forAll[Config, BigInt]((config, time) => succeeds(linearVesting, (config, time))),
          scheduleBudget,
          linearVesting
        )
    }

    test("the schedule's contract, stated in its body, holds of its compiled program") {
        // `linearVesting` ends in `.ensuring(vested => ...)`: for an initial amount that is not
        // negative, what is vested lies between nothing and that amount. The verifier reads the
        // clause from the function's SIR. It owes its callers nothing, and here it is also shown
        // to return.
        val stated = Contract
            .inSource(linearVesting)
            .getOrElse(fail("linearVesting states no contract"))
        proven(stated.returnsWhen((config, time) => true).prop, scheduleBudget, linearVesting)
        // negative control: a wrong postcondition. From the end on, everything is vested.
        refuted(
          contract(linearVesting)(
            expects = (config, time) => config.initialAmount >= BigInt(0),
            ensures = (config, time) => vested => vested < config.initialAmount
          ).prop,
          scheduleBudget,
          linearVesting
        )
        // negative control: the bound needs its condition. A negative initial amount is below
        // what is vested before the start.
        refuted(
          contract(linearVesting)(
            expects = (config, time) => true,
            ensures = (config, time) => vested => vested <= config.initialAmount
          ).prop,
          scheduleBudget,
          linearVesting
        )
    }

    test("the specification is not part of the script") {
        // The clause is checked where the code runs as Scala,
        val halfway = VestingValidator.linearVesting(
          config(BigInt(0), BigInt(10), BigInt(100)),
          BigInt(5)
        )
        assert(halfway == BigInt(50))
        // and kept in the SIR, with the function that checks it and that function's error.
        val sir = linearVesting(Representation.Sir).toString
        assert(sir.contains("Spec$.ensuring"))
        assert(sir.contains("a postcondition does not hold"))
        // It is gone from the program. Compiled with error traces, a program has the message of
        // every error it can raise, as the validator has its own, and none for a clause. The
        // tactic's options leave messages out, so its programs would not show one.
        given Options = Options.debug
        val schedule = PlutusV3.compile(VestingValidator.linearVesting).program.show
        val script = PlutusV3.compile(VestingValidator.validate).program.show
        assert(script.contains(VestingValidator.DatumNotFound))
        assert(!schedule.contains("postcondition"))
        assert(!script.contains("postcondition"))
    }

    test("the vested amount does not decrease with time") {
        proven(
          forAll[Config, BigInt, BigInt]((config, earlier, later) =>
              (config.initialAmount >= BigInt(0) && earlier <= later) ==>
                  call(linearVesting, (config, earlier))(before =>
                      call(linearVesting, (config, later))(after => before <= after)
                  )
          ),
          scheduleBudget,
          linearVesting
        )
        // negative control: a negative initial amount is taken away over time
        refuted(
          forAll[Config, BigInt, BigInt]((config, earlier, later) =>
              (earlier <= later) ==>
                  call(linearVesting, (config, earlier))(before =>
                      call(linearVesting, (config, later))(after => before <= after)
                  )
          ),
          scheduleBudget,
          linearVesting
        )
    }

    // The validator, on every script context

    /** The validator's entry point, compiled with the tactic's options. */
    private val validator =
        FunctionDef.named("vesting", (context: Data) => VestingValidator.validate(context))

    /** The steps for a context the validator rejects before it reads the transaction. */
    private val rejectionBudget = 600

    test("the contexts built from Data are the ledger's encoding") {
        // The statements below say that the validator fails, which a malformed context would make
        // true for the wrong reason.
        val ref = TxOutRef(TxId(hex"00"), 0)
        val datum = config(BigInt(1), BigInt(2), BigInt(3)).toData
        val redeemer = Action(BigInt(4)).toData
        def expected(datum: prelude.Option[Data]) =
            ScriptContext(
              TxInfo.placeholder,
              redeemer,
              ScriptInfo.SpendingScript(ref, datum)
            ).toData
        def built(datum: Data) =
            scriptContext(TxInfo.placeholder.toData, redeemer, spendingInfo(ref.toData, datum))
        assert(built(noDatum) == expected(prelude.Option.None))
        assert(built(someDatum(datum)) == expected(prelude.Option.Some(datum)))
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

    test("a withdrawal of a non-positive amount is rejected") {
        proven(
          forAll[Data, Data]((txInfo, ref) =>
              forAll[Data, BigInt]((datum, requested) =>
                  (requested <= BigInt(0)) ==> fails(
                    validator,
                    scriptContext(
                      txInfo,
                      Action(requested).toData,
                      spendingInfo(ref, someDatum(datum))
                    )
                  )
              )
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

    // Withdrawals

    /** The steps for a withdrawal: the validator runs to its end on one that it accepts. */
    private val withdrawalBudget = 12000

    /** The steps for a withdrawal that the validator rejects at its check of the amount, the last
      * one before it reads the outputs. The rejection takes just over 1600 steps.
      */
    private val earlyReleaseBudget = 1700

    test("a sample withdrawal is accepted on the Lean machine, and rejected unsigned") {
        // Closed statements: Lean decides them by running the validator.
        val accepted = succeeds(
          validator,
          withdrawal(
            BigInt(1000),
            BigInt(1000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(2000),
            BigInt(200_000),
            signed,
            payment(BigInt(19_800_000))
          )
        )
        assert(proven(accepted, withdrawalBudget, validator) == ProofKind.LeanNative)
        val unsigned = fails(
          validator,
          withdrawal(
            BigInt(1000),
            BigInt(1000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(20_000_000),
            BigInt(2000),
            BigInt(200_000),
            prelude.List.Nil,
            payment(BigInt(19_800_000))
          )
        )
        assert(proven(unsigned, withdrawalBudget, validator) == ProofKind.LeanNative)
    }

    test("nobody withdraws without the beneficiary's signature") {
        // For every datum, amount and time, and whatever the outputs are. The validator fails
        // before it reads the outputs, so they can be any Data.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, Data]((fee, outputs) =>
                      fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          prelude.List.Nil,
                          outputs.to[prelude.List[TxOut]]
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
        // negative control: with the signature, the solver finds a withdrawal that is accepted
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
    }

    test("what stays locked is at least what has not vested yet") {
        // A withdrawal that would leave less is rejected, even when the beneficiary signs it.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      (locked - requested < amount - VestingValidator.linearVesting(
                        config(start, duration, amount),
                        time
                      )) ==> fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator,
          linearVesting
        )
        // negative control: leaving exactly what has not vested is accepted
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                  forAll[BigInt, BigInt]((fee, paid) =>
                      (locked - requested <= amount - VestingValidator.linearVesting(
                        config(start, duration, amount),
                        time
                      )) ==> fails(
                        validator,
                        withdrawal(
                          start,
                          duration,
                          amount,
                          locked,
                          requested,
                          time,
                          fee,
                          signed,
                          payment(paid)
                        )
                      )
                  )
              )
          ),
          withdrawalBudget,
          validator,
          linearVesting
        )
    }

    /** What stays locked is at least what has not vested yet, whatever the outputs are: the
      * statement of the test before, with the outputs any `Data`.
      */
    private def lockedForAnyOutputs: Prop =
        forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
            forAll[BigInt, BigInt, BigInt]((locked, requested, time) =>
                forAll[BigInt, Data]((fee, outputs) =>
                    (locked - requested < amount - VestingValidator.linearVesting(
                      config(start, duration, amount),
                      time
                    )) ==> fails(
                      validator,
                      withdrawal(
                        start,
                        duration,
                        amount,
                        locked,
                        requested,
                        time,
                        fee,
                        signed,
                        outputs.to[prelude.List[TxOut]]
                      )
                    )
                )
            )
        )

    test("the same holds whatever the outputs are, at a budget that ends before they are read") {
        // The validator rejects such a withdrawal at its check of the amount, before it reads the
        // outputs, so they can be any Data, as for an unsigned withdrawal. But Lean runs each
        // test's program on its own, without the premise, and so also the runs that pass the
        // check. Those go on to search the outputs, a list of unknown length, with a choice at
        // every element.
        //
        // The budget keeps those runs short: it is just beyond the steps of the rejection, which
        // is all the statement is about, and a proof at any budget holds without the budget.
        // Every 100 steps more let them read another output, and double the time: 12 s at 1700,
        // 27 s at 1800, 62 s at 1900 and 3 minutes at 2000. At 1600 the rejection itself is cut,
        // and Lean's counterexample is spurious.
        //
        // So the budget follows the validator's code. Where a change of the compiler or of the
        // validator makes the rejection longer, the result here is a spurious counterexample,
        // and the budget wants raising; where it makes it shorter, the proof gets slower.
        proven(lockedForAnyOutputs, earlyReleaseBudget, validator, linearVesting)
    }

    test("at the budget of a whole withdrawal, the same is not finished by Lean", Unfinished) {
        // The budget of the tests around this one lets the runs that pass the check go far into
        // the outputs. Under Lean's own limit of work Lean gives the run of the second test up,
        // after about 17 s on the machine this was written on, and after the same work on any
        // other.
        val leansLimit = UplcBlaster(withdrawalBudget, lean, 5.minutes)
            .withMaxHeartbeats(UplcBlaster.leanMaxHeartbeats)
        val givenUp = inconclusive(lockedForAnyOutputs, leansLimit, validator, linearVesting)
        assert(givenUp.contains("maximum number of heartbeats"), givenUp)
        // Under the tactic's limit, twice that, the run gets past that point and goes on: it
        // gave no result in 8 minutes. The time limit ends it.
        val unfinished = inconclusive(
          lockedForAnyOutputs,
          withdrawalBudget,
          30.seconds,
          validator,
          linearVesting
        )
        assert(unfinished.contains("did not finish"), unfinished)
    }

    test("the guarantees stated on spend hold of every withdrawal the script accepts") {
        // `VestingValidator.spend` states them at its head, with `Spec.ensures`: the beneficiary
        // signed, something is withdrawn, and what stays locked is at least what has not vested.
        // `spend` is inlined into `validate`, so the verifier finds its clauses in the code of
        // the function that builds a withdrawal and validates it. Here the transaction has one
        // signature, of any key.
        requireLean()
        val spends = FunctionDef.named(
          "spends",
          (w: Withdrawal) =>
              VestingValidator.validate(
                withdrawal(
                  w.start,
                  w.duration,
                  w.amount,
                  w.locked,
                  w.requested,
                  w.time,
                  w.fee,
                  prelude.List.Cons(PubKeyHash(w.signer), prelude.List.Nil),
                  payment(w.paid)
                )
              )
        )
        val verifier = Verifier.empty
        verifier.addFunction(linearVesting)
        verifier.addFunction(spends)
        val stated = verifier.guarantees(spends.ref)
        assert(stated.unsupported.isEmpty, stated.unsupported)
        assert(
          stated.statements.map(_.name) ==
              List("spends/ensures#1", "spends/ensures#2", "spends/ensures#3")
        )
        stated.statements.foreach { guarantee =>
            guarantee.origin match
                case Origin.Guarantee(function, line) => assert(function == spends.ref && line > 0)
                case other => fail(s"expected a guarantee's origin, got $other")
        }
        // They are proved together. Each statement says that the validator returns, so one by
        // one the tactic would run the validator once for every clause.
        val together = stated.together.getOrElse(fail("no statement of the three clauses"))
        assert(together.name == "spends/ensures")
        verifier.verify(together, UplcBlaster(withdrawalBudget, lean)) match
            case VerificationResult.Proven(_) =>
            case other => fail(s"expected a proof of ${together.name}, got $other")
    }

    test("the validator would not establish a precondition on the amount") {
        // Why the schedule states its bound with a condition: had its contract expected an
        // initial amount that is not negative, the validator's call would owe that, wherever a
        // withdrawal reaches it.
        requireLean()
        val withdraw = FunctionDef.named(
          "withdraw",
          (w: Withdrawal) =>
              VestingValidator.validate(
                withdrawal(
                  w.start,
                  w.duration,
                  w.amount,
                  w.locked,
                  w.requested,
                  w.time,
                  w.fee,
                  signed,
                  payment(w.paid)
                )
              )
        )
        val verifier = Verifier.empty
        verifier.addFunction(linearVesting)
        verifier.addFunction(withdraw)
        verifier.contract(
          "vested_in_range",
          contract(linearVesting)(
            expects = (config, time) => config.initialAmount >= BigInt(0),
            ensures =
                (config, time) => vested => BigInt(0) <= vested && vested <= config.initialAmount
          )
        )
        val tactic = UplcBlaster(withdrawalBudget, lean)

        // The verifier finds the call, in the validator's own source.
        val owed = verifier.obligations(withdraw.ref) match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")
        owed.origin match
            case Origin.Obligation(caller, callee, "vested_in_range", line) =>
                assert(caller == withdraw.ref && callee == linearVesting.ref && line > 0)
            case other => fail(s"expected an obligation's origin, got $other")

        // It is refuted: the validator reads the amount from the datum and does not check it.
        verifier.verify(owed, tactic) match
            case VerificationResult.Refuted(proof) =>
                val values = proof.artifact.asInstanceOf[UplcBlaster.Artifact].counterexample.toMap
                val amount = values.collectFirst {
                    case (name, value) if name.endsWith(".amount") => integer(value)
                }
                assert(amount.exists(_ < 0), values)
            case other => fail(s"expected a refutation, got $other")

        // Where the withdrawal's own contract expects such an amount, the obligation is proved.
        val assumes = verifier.contract(
          "withdraw_of_an_amount",
          contract(withdraw)(expects = w => w.amount >= BigInt(0), ensures = w => done => true)
        )
        val List(assumed) = verifier.obligations(assumes).statements
        verifier.verify(assumed, tactic) match
            case VerificationResult.Proven(_) =>
            case other                        => fail(s"expected a proof, got $other")
    }

    test("after the end of the period the beneficiary withdraws everything that is locked") {
        // The funds are not stuck: such a withdrawal, paid out less the fee, is always accepted.
        proven(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, time, fee) =>
                  (duration >= BigInt(0) && start + duration <= time && locked > BigInt(0)
                      && locked <= amount) ==> succeeds(
                    validator,
                    withdrawal(
                      start,
                      duration,
                      amount,
                      locked,
                      locked,
                      time,
                      fee,
                      signed,
                      payment(locked - fee)
                    )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
        // negative control: before the end of the period not everything has vested
        refuted(
          forAll[BigInt, BigInt, BigInt]((start, duration, amount) =>
              forAll[BigInt, BigInt, BigInt]((locked, time, fee) =>
                  (duration >= BigInt(0) && locked > BigInt(0) && locked <= amount) ==> succeeds(
                    validator,
                    withdrawal(
                      start,
                      duration,
                      amount,
                      locked,
                      locked,
                      time,
                      fee,
                      signed,
                      payment(locked - fee)
                    )
                  )
              )
          ),
          withdrawalBudget,
          validator
        )
    }
}

/** The values the statements of [[VestingVerificationTest]] are about. They are `inline`, so that
  * each is part of the expression it is used in, which the Scalus plugin compiles. They live in an
  * object: an `inline def` of the test class that uses another one would leave a reference to the
  * test's `this` in that expression.
  */
object VestingVerificationTest {

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

    /** A script context, as the ledger encodes it, from the encodings of its three fields. */
    inline def scriptContext(
        inline txInfo: Data,
        inline redeemer: Data,
        inline scriptInfo: Data
    ): Data =
        constrData(0, mkCons(txInfo, mkCons(redeemer, mkCons(scriptInfo, mkNilData()))))

    /** The script info of spending `ref`, whose datum is the encoded `Option[Data]`. */
    inline def spendingInfo(inline ref: Data, inline datum: Data): Data =
        constrData(1, mkCons(ref, mkCons(datum, mkNilData())))

    inline def noDatum: Data = constrData(1, mkNilData())

    inline def someDatum(inline datum: Data): Data =
        constrData(0, mkCons(datum, mkNilData()))

    inline def vestingRef: TxOutRef =
        TxOutRef(TxId(hex"2222222222222222222222222222222222222222222222222222222222222222"), 0)

    /** The script context of a withdrawal of `requested` lovelace at `time`.
      *
      * The transaction's only input is the vesting output, which holds `locked` lovelace under the
      * datum `config(start, duration, amount)`. Its validity range starts at `time`.
      */
    inline def withdrawal(
        inline start: BigInt,
        inline duration: BigInt,
        inline amount: BigInt,
        inline locked: BigInt,
        inline requested: BigInt,
        inline time: BigInt,
        inline fee: BigInt,
        inline signatories: prelude.List[PubKeyHash],
        inline outputs: prelude.List[TxOut]
    ): Data =
        ScriptContext(
          TxInfo(
            inputs = prelude.List.Cons(
              TxInInfo(
                vestingRef,
                TxOut(
                  Address(
                    Credential.ScriptCredential(
                      hex"33333333333333333333333333333333333333333333333333333333"
                    ),
                    prelude.Option.None
                  ),
                  Value.lovelace(locked)
                )
              ),
              prelude.List.Nil
            ),
            outputs = outputs,
            fee = fee,
            validRange = Interval.after(time),
            signatories = signatories,
            id = TxId(hex"5555555555555555555555555555555555555555555555555555555555555555")
          ),
          Action(requested).toData,
          ScriptInfo.SpendingScript(
            vestingRef,
            prelude.Option.Some(config(start, duration, amount).toData)
          )
        ).toData

    /** The beneficiary's signature alone. */
    inline def signed: prelude.List[PubKeyHash] =
        prelude.List.Cons(beneficiary, prelude.List.Nil)

    /** One output, of `paid` lovelace to the beneficiary. */
    inline def payment(inline paid: BigInt): prelude.List[TxOut] = prelude.List.Cons(
      TxOut(
        Address(Credential.PubKeyCredential(beneficiary), prelude.Option.None),
        Value.lovelace(paid)
      ),
      prelude.List.Nil
    )
}
