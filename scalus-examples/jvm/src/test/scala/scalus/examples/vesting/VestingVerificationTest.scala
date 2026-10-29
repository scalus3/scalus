package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.compiler.Compile
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v3.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.{Interval, ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOut, TxOutRef}
import scalus.uplc.builtin.Builtins.{constrData, mkCons, mkNilData, unConstrData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite, Unfinished, UplcBlaster}

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
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]). The test tagged `Unfinished` is about what Lean does not
  * finish, and keeps nothing: it needs Lean.
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
          (time < config.startTimestamp) ==>
              call(linearVesting, (config, time))(vested => vested == BigInt(0))
      )
    )

    test("nothing is vested before the start") {
        check(zeroBeforeStart, scheduleBudget)
    }

    private val everythingFromTheEnd = verifier.statement(
      "vesting.schedule.everything_from_the_end",
      forAll[Config, BigInt]((config, time) =>
          (config.startTimestamp <= time
              && config.startTimestamp + config.duration <= time) ==>
              call(linearVesting, (config, time))(vested => vested == config.initialAmount)
      )
    )

    test("everything is vested from the end of the period on") {
        check(everythingFromTheEnd, scheduleBudget)
    }

    private val scheduleReturns = verifier.statement(
      "vesting.schedule.returns",
      forAll[Config, BigInt]((config, time) => succeeds(linearVesting, (config, time)))
    )

    test("the schedule returns for every datum and time") {
        // Whatever the datum's numbers are: it does not divide by a zero duration.
        check(scheduleReturns, scheduleBudget)
    }

    /** The contract `linearVesting` states in its body, and that it returns: none where it states
      * no contract.
      */
    private val statedContractHolds = FunctionContract
        .inSource(linearVesting)
        .map(stated =>
            verifier.statement(
              "vesting.schedule.stated_contract_holds",
              stated.returnsWhen((config, time) => true).prop
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
        check(
          statedContractHolds.getOrElse(fail("linearVesting states no contract")),
          scheduleBudget
        )
        // negative control: a wrong postcondition. From the end on, everything is vested.
        check(vestedBelowTheAmount, scheduleBudget)
        // negative control: the bound needs its condition. A negative initial amount is below
        // what is vested before the start.
        check(boundedWithoutCondition, scheduleBudget)
    }

    private val doesNotDecrease = verifier.statement(
      "vesting.schedule.does_not_decrease",
      forAll[Config, BigInt, BigInt]((config, earlier, later) =>
          (config.initialAmount >= BigInt(0) && earlier <= later) ==>
              call(linearVesting, (config, earlier))(before =>
                  call(linearVesting, (config, later))(after => before <= after)
              )
      )
    )

    private val doesNotDecreaseForAnyAmount = verifier.refute(
      "vesting.schedule.does_not_decrease_for_any_amount",
      forAll[Config, BigInt, BigInt]((config, earlier, later) =>
          (earlier <= later) ==>
              call(linearVesting, (config, earlier))(before =>
                  call(linearVesting, (config, later))(after => before <= after)
              )
      )
    )

    test("the vested amount does not decrease with time") {
        check(doesNotDecrease, scheduleBudget)
        // negative control: a negative initial amount is taken away over time
        check(doesNotDecreaseForAnyAmount, scheduleBudget)
    }

    // The validator, on every script context

    /** The validator's entry point, compiled with the tactic's options. */
    private val validator =
        FunctionDef.named("vesting", (context: Data) => VestingValidator.validate(context))

    verifier.addFunction(validator)

    /** The steps for a context the validator rejects before it reads the transaction. */
    private val rejectionBudget = 600

    private val noDatumIsRejected = verifier.statement(
      "vesting.validator.no_datum_is_rejected",
      forAll[Data, Data, Data]((txInfo, redeemer, ref) =>
          fails(validator, scriptContext(txInfo, redeemer, spendingInfo(ref, noDatum)))
      )
    )

    test("an output without a datum cannot be spent") {
        check(noDatumIsRejected, rejectionBudget)
    }

    private val nonPositiveAmountIsRejected = verifier.statement(
      "vesting.validator.non_positive_amount_is_rejected",
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
      )
    )

    test("a withdrawal of a non-positive amount is rejected") {
        check(nonPositiveAmountIsRejected, rejectionBudget)
    }

    private val onlySpending = verifier.statement(
      "vesting.validator.only_spending",
      forAll[Data, Data, Data]((txInfo, redeemer, scriptInfo) =>
          !Prop(unConstrData(scriptInfo).fst == BigInt(1)) ==>
              fails(validator, scriptContext(txInfo, redeemer, scriptInfo))
      )
    )

    test("the script validates nothing but spending") {
        // Spending is the script info of tag 1. For a script info that is not a constructor the
        // premise's test fails, so its negation holds, and the validator fails as well.
        check(onlySpending, rejectionBudget)
    }

    // Withdrawals

    /** The steps for a withdrawal: the validator runs to its end on one that it accepts. */
    private val withdrawalBudget = 12000

    private val sampleAccepted = verifier.statement(
      "vesting.withdrawal.sample_is_accepted",
      succeeds(
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
    )

    private val sampleUnsigned = verifier.statement(
      "vesting.withdrawal.sample_unsigned_is_rejected",
      fails(
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
    )

    test("a sample withdrawal is accepted on the Lean machine, and rejected unsigned") {
        // Closed statements: Lean decides them by running the validator.
        assert(artifactOf(check(sampleAccepted, withdrawalBudget)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(sampleUnsigned, withdrawalBudget)).kind == ProofKind.LeanNative)
    }

    private val unsignedIsRejected = verifier.statement(
      "vesting.withdrawal.unsigned_is_rejected",
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
      )
    )

    private val signedIsRejected = verifier.refute(
      "vesting.withdrawal.signed_is_rejected",
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
      )
    )

    test("nobody withdraws without the beneficiary's signature") {
        // For every datum, amount and time, and whatever the outputs are. The validator fails
        // before it reads the outputs, so they can be any Data.
        check(unsignedIsRejected, withdrawalBudget)
        // negative control: with the signature, the solver finds a withdrawal that is accepted
        check(signedIsRejected, withdrawalBudget)
    }

    private val lockedAtLeastUnvested = verifier.statement(
      "vesting.withdrawal.locked_at_least_unvested",
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
      )
    )

    private val lockedMoreThanUnvested = verifier.refute(
      "vesting.withdrawal.locked_more_than_unvested",
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
      )
    )

    test("what stays locked is at least what has not vested yet") {
        // A withdrawal that would leave less is rejected, even when the beneficiary signs it.
        check(lockedAtLeastUnvested, withdrawalBudget)
        // negative control: leaving exactly what has not vested is accepted
        check(lockedMoreThanUnvested, withdrawalBudget)
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

    private val lockedForAnyOutputsAtFoundBudget = verifier.statement(
      "vesting.withdrawal.locked_for_any_outputs",
      lockedForAnyOutputs
    )

    /** The same statement, checked at the budget of a whole withdrawal, which Lean does not finish.
      * It is a statement of its own: one that is proved cannot also be inconclusive.
      */
    private val lockedForAnyOutputsAtWithdrawalBudget = verifier.statement(
      "vesting.withdrawal.locked_for_any_outputs.at_withdrawal_budget",
      lockedForAnyOutputs
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
        // The tactic finds that budget, from the counterexamples that smaller ones give: about
        // 1610 steps where this was written. So the proof does not hang on a number that
        // follows the validator's code.
        val found = artifactOf(check(lockedForAnyOutputsAtFoundBudget)).budget
        assert(found < withdrawalBudget / 4, found)
    }

    test("at the budget of a whole withdrawal, the same is not finished by Lean", Unfinished) {
        // The budget of the tests around this one lets the runs that pass the check go far into
        // the outputs. Under Lean's own limit of work Lean gives the run of the second test up,
        // after about 17 s on the machine this was written on, and after the same work on any
        // other.
        val leansLimit = UplcBlaster(Budget.LeanSteps(withdrawalBudget), lean, 5.minutes)
            .withMaxHeartbeats(UplcBlaster.leanMaxHeartbeats)
        val givenUp = inconclusive(lockedForAnyOutputsAtWithdrawalBudget, leansLimit)
        assert(givenUp.contains("maximum number of heartbeats"), givenUp)
        // Under the tactic's limit, twice that, the run gets past that point and goes on: it
        // gave no result in 8 minutes. The time limit ends it.
        val unfinished = inconclusive(
          lockedForAnyOutputsAtWithdrawalBudget,
          UplcBlaster(Budget.LeanSteps(withdrawalBudget), lean, 30.seconds)
        )
        assert(unfinished.contains("did not finish"), unfinished)
        // It is the symbolic run of the validator that does not end, before the solver is asked.
        assert(unfinished.contains("symbolically"), unfinished)
    }

    /** The validator on a withdrawal with one signature, of any key. `spend` is inlined into
      * `validate`, so the verifier finds the clauses of `spend` in the code of this function.
      */
    private val spends = FunctionDef.named(
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

    verifier.addFunction(spends)

    /** What `VestingValidator.spend` states at its head, with `Spec.ensures`: the beneficiary
      * signed, something is withdrawn, and what stays locked is at least what has not vested.
      */
    private val spendGuarantees = verifier.guarantees(spends.ref)

    test("the guarantees stated on spend hold of every withdrawal the script accepts") {
        // A clause that is not read would be missing from what is proved.
        assert(spendGuarantees.unsupported.isEmpty, spendGuarantees.unsupported)
        // They are proved together. Each statement says that the validator returns, so one by
        // one the tactic would run the validator once for every clause.
        val together = spendGuarantees.together.getOrElse(fail("spend states no guarantee"))
        check(together, withdrawalBudget)
    }

    /** The validator on a withdrawal signed by the beneficiary. */
    private val withdraw = FunctionDef.named(
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

    verifier.addFunction(withdraw)

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

    /** What the validator's call of the schedule owes to `vested_in_range`. */
    private val withdrawalObligations = verifier.obligations(withdraw.ref)

    /** A contract of the withdrawal that expects the amount `vested_in_range` asks for. */
    private val withdrawOfAnAmount = verifier.contract(
      "withdraw_of_an_amount",
      contract(withdraw)(expects = w => w.amount >= BigInt(0), ensures = w => done => true)
    )

    /** The same obligation, where the withdrawal's contract holds. */
    private val obligationsOfAnAmount = verifier.obligations(withdrawOfAnAmount)

    test("the validator would not establish a precondition on the amount") {
        // Why the schedule states its bound with a condition.
        val owed = withdrawalObligations match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")
        // It is refuted: the validator reads the amount from the datum and does not check it.
        val values = counterexampleOf(
          check(
            VerificationRequest(
              owed,
              UplcBlaster(Budget.LeanSteps(withdrawalBudget), lean),
              ExpectedOutcome.Falsified
            )
          )
        )
        val amount = values.collectFirst {
            case (name, value) if name.endsWith(".amount") => integer(value)
        }
        assert(amount.exists(_ < 0), values)

        // Where the withdrawal's own contract expects such an amount, the obligation is proved.
        val assumed = obligationsOfAnAmount match
            case CallObligations(List(assumed), Nil) => assumed
            case other                               => fail(s"expected one obligation, got $other")
        check(assumed, withdrawalBudget)
    }

    private val everythingAfterTheEnd = verifier.statement(
      "vesting.withdrawal.everything_after_the_end",
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
      )
    )

    private val everythingAtAnyTime = verifier.refute(
      "vesting.withdrawal.everything_at_any_time",
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
      )
    )

    test("after the end of the period the beneficiary withdraws everything that is locked") {
        // The funds are not stuck: such a withdrawal, paid out less the fee, is always accepted.
        check(everythingAfterTheEnd, withdrawalBudget)
        // negative control: before the end of the period not everything has vested
        check(everythingAtAnyTime, withdrawalBudget)
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
