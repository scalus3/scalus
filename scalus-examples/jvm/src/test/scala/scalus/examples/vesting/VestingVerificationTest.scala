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
import scalus.verify.uplcblaster.{Budget, LeanProofs, LeanProofsSuite, Unfinished, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*

/** Properties of [[VestingValidator]], proved about its compiled UPLC with the `UplcBlaster` tactic
  * (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * Three groups of statements, from the most general to the most specific:
  *   - the vesting schedule, `linearVesting`, and its contract, for every datum and time;
  *   - the validator on every script context, of any transaction: what it rejects before it reads
  *     the transaction;
  *   - the validator on every script context of a few inputs and outputs: who may withdraw, how
  *     much, what its call of the schedule owes, and that everything can be withdrawn after the
  *     end. Sample withdrawals, decided by running the validator, show that it accepts some.
  *
  * Two limits of what is proved:
  *   - the validator is compiled here with the tactic's options (`UplcBlaster.options`), which
  *     differ from those of [[VestingContract]]: Lean's model has no `Value` builtins yet. The
  *     statements are about the same source, not about the published script's bytes;
  *   - a statement about withdrawals says nothing about transactions of more inputs or outputs than
  *     its limits. The validator walks the inputs and the outputs, and the tactic cannot follow a
  *     loop over a list of unknown length.
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
      forAllData[ScriptContext](ctx => spendsWithoutDatum(ctx) ==> fails(validator, ctx.toData))
    )

    test("an output without a datum cannot be spent") {
        check(noDatumIsRejected, rejectionBudget)
    }

    private val nonPositiveAmountIsRejected = verifier.statement(
      "vesting.validator.non_positive_amount_is_rejected",
      forAllData[ScriptContext](ctx =>
          (spendsWithDatum(ctx) && ctx.redeemer.to[Action].amount <= BigInt(0)) ==>
              fails(validator, ctx.toData)
      )
    )

    test("a withdrawal of a non-positive amount is rejected") {
        // Of an output with a datum, whatever the datum is. A redeemer that is no `Action` is none
        // the premise reads an amount of, and the validator rejects it too.
        check(nonPositiveAmountIsRejected, rejectionBudget)
    }

    private val onlySpending = verifier.statement(
      "vesting.validator.only_spending",
      forAllData[ScriptContext](ctx => !Prop(isSpending(ctx)) ==> fails(validator, ctx.toData))
    )

    test("the script validates nothing but spending") {
        check(onlySpending, rejectionBudget)
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
        assert(artifactOf(check(sampleAccepted, sampleBudget)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(sampleUnsigned, sampleBudget)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(sampleOfTheVested, sampleBudget)).kind == ProofKind.LeanNative)
        assert(artifactOf(check(sampleOfMore, sampleBudget)).kind == ProofKind.LeanNative)
    }

    /** The validator on a script context of its type: what the verifier's statements about its code
      * are of. `spend` is inlined into `validate`, so the verifier finds the clauses of `spend` in
      * the code of this function.
      */
    private val spends = FunctionDef.named(
      "spends",
      (ctx: ScriptContext) => VestingValidator.validate(ctx.toData)
    )

    verifier.addFunction(spends)

    /** The contexts of the verifier's statements about withdrawals: of at most two inputs, as the
      * vesting output and one of the beneficiary's that pays the fee, of one signatory, and of
      * inputs whose values are of one policy of one token. Everything else of the context is free,
      * the outputs among it: the validator rejects a withdrawal that breaks a clause before it
      * reads them, and the statements are checked in the rejecting form, which runs only the runs
      * it rejects ([[UplcBlaster]], "The rejecting form").
      */
    private val fewInputs = limitedData[ScriptContext](ctx =>
        ctx.txInfo.inputs.length <= 2 && ctx.txInfo.signatories.length <= 1 &&
            ctx.txInfo.inputs.forall(i =>
                i.resolved.value.toSortedMap.length <= 1 &&
                    i.resolved.value.toSortedMap.values.forall(t => t.length <= 1)
            )
    )

    /** What `VestingValidator.spend` states at its head, with `Spec.ensures`: the beneficiary
      * signed, something is withdrawn, and what stays locked is at least what has not vested.
      *
      * Each is stated with `==>?` ([[Connective.Rejects]]): wherever a clause is false, the
      * validator rejects the spend. A clause reads the datum, the redeemer, the spent input and the
      * validity range as the validator does, and fails where they cannot be read, where the
      * validator rejects the spend too; there it says nothing, and the rejecting program ends at
      * once. With `==>` a clause would have to return there as well: one that read a default
      * instead lets every datum it cannot read run on into the validator, which reads the datum
      * after it walks the inputs, and Lean does not finish.
      */
    private val spendGuarantees = verifier.guarantees(spends.ref, fewInputs, Connective.Rejects)

    test("spend's guarantees hold: the signature of spends of two inputs, the amount of any") {
        // A clause that is not read would be missing from what is proved.
        assert(spendGuarantees.unsupported.isEmpty, spendGuarantees.unsupported)
        val List(signed, _, _) = spendGuarantees.statements
        // They are proved one by one. Each rejecting program runs the validator only where its
        // clause is false, to the check that rejects the spend: together, every way one of them is
        // false would run on into the validator, and Lean does not finish.
        check(signed, guaranteesBudget)
        // The second holds of every script context, without limits: the validator rejects a spend
        // of no amount at its first check, before it reads a list of the context.
        val List(_, something, _) = spendGuaranteesOnAnyContext.statements
        check(something, guaranteesBudget)
    }

    test("spend's clauses return true on the samples the validator accepts") {
        // Stated with `==>?`, a clause that failed wherever the validator returns would be proved
        // of every context. Read with `==>` on a spend the validator accepts, each returns `true`
        // there: run on the Scalus CEK, without Lean.
        val accepted = List(sampleOfEverything(signed), sampleHalfway(halfOfTheAmount))
        for
            sample <- accepted
            statement <- spendGuarantees.statements ++ spendGuaranteesOnAnyContext.statements
        do assert(holdsAsImplicationOn(statement, Constant.Data(sample)), statement.name)
    }

    test("the third guarantee, of what stays locked, is not finished by Lean", Unfinished) {
        // What stays locked is at least what has not vested. Its rejecting program did not end its
        // symbolic run in 30 minutes, at 3300 or at 3800 steps, written as the clause is or as the
        // validator's check, with the outputs free or of at most two; and the search for its
        // budget came, by 1173, 1327, 1406 and 1450 steps, each a spurious counterexample of a
        // minute or two, to 2517, which it did not finish in the time left (2026-11-06). The first
        // two take 149 s and 14 s. The time limit ends it.
        val List(_, _, unvested) = spendGuarantees.statements
        val unfinished = inconclusive(
          unvested,
          UplcBlaster(Budget.LeanSteps(guaranteesBudget), lean, 60.seconds)
        )
        assert(unfinished.contains("symbolically"), unfinished)
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

    /** What the validator's call of the schedule owes to `vested_in_range`. */
    private val spendObligations = verifier.obligations(spends.ref, fewInputs)

    /** A contract of the validator that expects a datum whose initial amount `vested_in_range` asks
      * for.
      */
    private val spendOfAnAmount = verifier.contract(
      "spend_of_an_amount",
      contract(spends)(
        expects = ctx => initialAmountIsNotNegative(ctx),
        ensures = ctx => done => true
      )
    )

    /** The same obligation, where the validator's contract holds. */
    private val obligationsOfAnAmount = verifier.obligations(spendOfAnAmount, fewInputs)

    test("the validator would not establish a precondition on the amount") {
        // Why the schedule states its bound with a condition.
        val owed = spendObligations match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")
        // It is refuted: the validator reads the amount from the datum and does not check it.
        val values = counterexampleOf(
          check(
            VerificationRequest(
              owed,
              UplcBlaster(Budget.LeanSteps(obligationBudget), lean),
              ExpectedOutcome.Falsified
            )
          )
        )
        val amounts = values.collect { case (_, scalus.uplc.Constant.Data(context)) =>
            initialAmountOf(context.to[ScriptContext])
        }
        assert(amounts.exists(_.exists(_ < 0)), values)
    }

    test("where the validator's contract expects such an amount, the obligation holds") {
        val assumed = obligationsOfAnAmount match
            case CallObligations(List(assumed), Nil) => assumed
            case other                               => fail(s"expected one obligation, got $other")
        check(
          VerificationRequest(
            assumed,
            UplcBlaster(Budget.LeanSteps(obligationBudget), lean, 30.minutes)
          )
        )
    }

    private val everythingAfterTheEnd = verifier.statement(
      "vesting.withdrawal.everything_after_the_end",
      forAllDataLimited[ScriptContext](ctx =>
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
      )(ctx => (takesEverything(ctx) && afterTheEnd(ctx)) ==> succeeds(validator, ctx.toData))
    )

    private val everythingAtAnyTime = verifier.refute(
      "vesting.withdrawal.everything_at_any_time",
      forAllDataLimited[ScriptContext](ctx =>
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
      )(ctx => takesEverything(ctx) ==> succeeds(validator, ctx.toData))
    )

    test(
      "after the end of the period the beneficiary withdraws everything, not finished by Lean",
      Unfinished
    ) {
        // The funds are not stuck: of every context of at most two inputs and two outputs, a
        // withdrawal of everything, signed by the beneficiary and paying them what the validator
        // asks, is accepted from the end of the period on. The negative control: before the end of
        // the period not everything has vested, `everythingAtAnyTime`.
        //
        // An accepted run goes to the validator's end, and Lean runs the validator on every path
        // of every such context: the search for the budget starts at 1922 steps, where the limits
        // have every test end on the largest values that meet them, and Lean was still in the
        // validator's symbolic run after 16 minutes (2026-11-06). The time limit ends it.
        val unfinished = inconclusive(
          everythingAfterTheEnd,
          UplcBlaster(Budget.LeanSteps(1922), lean, 60.seconds)
        )
        assert(unfinished.contains("symbolically"), unfinished)
    }

    // On every script context

    /** The validator on a typed script context, as [[spends]], under a name of its own: the
      * verifier's statements of it are of every context, without limits, and Lean does not finish
      * them. The validator walks the inputs, and where it accepts the outputs, lists of any length,
      * and the tactic cannot follow a loop over one: a run goes on to the end of the budget on
      * every length. They are what the limits of the statements above are for.
      */
    private val spendsOnAnyContext = FunctionDef.named(
      "spends_on_any_context",
      (ctx: ScriptContext) => VestingValidator.validate(ctx.toData)
    )

    verifier.addFunction(spendsOnAnyContext)

    /** Every script context: the parameter a `forAllData[ScriptContext]` variable. */
    private val anyContext = unlimitedData[ScriptContext]

    private val spendGuaranteesOnAnyContext =
        verifier.guarantees(spendsOnAnyContext.ref, anyContext, Connective.Rejects)

    test(
      "spend's first and third guarantees, on every script context, are not finished",
      Unfinished
    ) {
        // The second is proved of every context, with the first above.
        val List(signed, _, unvested) = spendGuaranteesOnAnyContext.statements
        List(signed, unvested).foreach { statement =>
            val unfinished = inconclusive(
              statement,
              UplcBlaster(Budget.LeanSteps(guaranteesBudget), lean, 60.seconds)
            )
            assert(unfinished.contains("did not finish"), unfinished)
        }
    }

    private val spendObligationsOnAnyContext =
        verifier.obligations(spendsOnAnyContext.ref, anyContext)

    private val spendOnAnyContextOfAnAmount = verifier.contract(
      "spend_on_any_context_of_an_amount",
      contract(spendsOnAnyContext)(
        expects = ctx => initialAmountIsNotNegative(ctx),
        ensures = ctx => done => true
      )
    )

    private val obligationsOnAnyContextOfAnAmount =
        verifier.obligations(spendOnAnyContextOfAnAmount, anyContext)

    test(
      "the obligation of the schedule's call, on every script context, is not finished",
      Unfinished
    ) {
        val statements =
            spendObligationsOnAnyContext.statements ++ obligationsOnAnyContextOfAnAmount.statements
        assert(statements.size == 2)
        statements.foreach { statement =>
            val unfinished = inconclusive(
              statement,
              UplcBlaster(Budget.LeanSteps(obligationBudget), lean, 60.seconds)
            )
            assert(unfinished.contains("did not finish"), unfinished)
        }
    }

    private val everythingAfterTheEndOnAnyContext = verifier.statement(
      "vesting.withdrawal.everything_after_the_end.any_context",
      forAllData[ScriptContext](ctx =>
          (takesEverything(ctx) && afterTheEnd(ctx)) ==> succeeds(validator, ctx.toData)
      )
    )

    test("after the end of the period, on every script context, not finished by Lean", Unfinished) {
        val unfinished = inconclusive(
          everythingAfterTheEndOnAnyContext,
          UplcBlaster(Budget.LeanSteps(1922), lean, 60.seconds)
        )
        assert(unfinished.contains("did not finish"), unfinished)
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
