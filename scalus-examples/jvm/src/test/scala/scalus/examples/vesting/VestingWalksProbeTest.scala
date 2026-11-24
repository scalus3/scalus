package scalus.examples.vesting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.===
import scalus.cardano.onchain.plutus.v3.{Credential, OutputDatum, ScriptContext, ScriptInfo, Value}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanProofsSuite, UplcBlaster}

import java.nio.file.Path
import scala.concurrent.duration.*
import scala.util.Try

/** The probe of "calls as unknowns" (docs/design/verification-details/multi-transaction.md, "The
  * cost"): spend's third guarantee, of every script context, about the validator with the results
  * of its walks over lists given ([[VestingWalksAsUnknowns]]). Of the validator itself Lean does
  * not finish it, of every context nor of contexts of two inputs (`VestingVerificationTest`). The
  * guarantee needs nothing of a walk but that the validator and the guarantee make the same one, so
  * it is a statement of every value of the walks.
  */
class VestingWalksProbeTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    import VestingVerificationTest.*
    import VestingWalksProbeTest.*

    override protected def leanWorkspace: Path = VestingVerificationTest.workspace

    // spend: Spec.ensures(lockedBy(txInfo, txOutRef) - amountOf(redeemer) >=
    //            configOf(datum).initialAmount - linearVesting(configOf(datum), validFrom))
    private val unvestedStaysLockedGivenWalks = verifier.statement(
      "vesting.probe.unvested_stays_locked_given_walks",
      forAll[ScriptContext, Walks]((ctx, walks) =>
          acceptsGiven(ctx, walks) ==>?: keepsTheUnvestedLockedGiven(ctx, walks)
      )
    )

    test("spend's third guarantee holds of every context, with the walks given") {
        report(
          unvestedStaysLockedGivenWalks,
          check(
            VerificationRequest(
              unvestedStaysLockedGivenWalks,
              UplcBlaster(Budget.Auto, lean, 30.minutes)
            )
          )
        )
    }

    /** The negative control: a withdrawal leaves what was locked whole. */
    private val nothingIsWithdrawnGivenWalks = verifier.refute(
      "vesting.probe.nothing_is_withdrawn_given_walks",
      forAll[ScriptContext, Walks]((ctx, walks) =>
          acceptsGiven(ctx, walks) ==>?: leavesTheInitialAmountGiven(ctx, walks)
      )
    )

    test("a withdrawal that takes nothing is refuted, with the walks given") {
        report(
          nothingIsWithdrawnGivenWalks,
          check(
            VerificationRequest(
              nothingIsWithdrawnGivenWalks,
              UplcBlaster(Budget.Auto, lean, 30.minutes)
            )
          )
        )
    }

    // spend: Spec.ensures(amountOf(redeemer) === lockedBy(txInfo, txOutRef) ||
    //            carriesTheRest(txInfo, txOutRef, datum, amountOf(redeemer)))
    //
    // The clause reads the lovelace of the output that keeps the rest, which the validator does
    // not: it compares the whole value. So the statement needs one fact of the library's Value code,
    // stated of the walks' values: the lovelace of `v - lovelace(a)` is that of `v` less `a`.
    private val stateIsCarriedGivenWalks = verifier.statement(
      "vesting.probe.state_is_carried_given_walks",
      forAll[ScriptContext, Walks]((ctx, walks) =>
          acceptsGiven(ctx, walks) ==>?: carriesTheStateGiven(ctx, walks, true)
      )
    )

    test("spend's fourth guarantee holds of every context, with the walks given and the lemma") {
        report(
          stateIsCarriedGivenWalks,
          check(
            VerificationRequest(
              stateIsCarriedGivenWalks,
              UplcBlaster(Budget.Auto, lean, 30.minutes)
            )
          )
        )
    }

    /** The control of the lemma: without it, the value the validator checks says nothing of the
      * lovelace the clause reads.
      */
    private val stateIsCarriedWithoutTheLemma = verifier.refute(
      "vesting.probe.state_is_carried_without_the_lemma",
      forAll[ScriptContext, Walks]((ctx, walks) =>
          acceptsGiven(ctx, walks) ==>?: carriesTheStateGiven(ctx, walks, false)
      )
    )

    test("spend's fourth guarantee is refuted without the lemma, with the walks given") {
        report(
          stateIsCarriedWithoutTheLemma,
          check(
            VerificationRequest(
              stateIsCarriedWithoutTheLemma,
              UplcBlaster(Budget.Auto, lean, 30.minutes)
            )
          )
        )
    }

    /** The control of the datum check: without it, a withdrawal leaves the rest under another
      * datum, as `VestingVerificationTest`'s control says of the validator.
      */
    private val stateIsCarriedWithoutDatumCheck = verifier.refute(
      "vesting.probe.state_is_carried_without_datum_check",
      forAll[ScriptContext, Walks]((ctx, walks) =>
          succeeds(VestingWalksAsUnknowns.validateWithoutDatumCheck(ctx.toData, walks)) ==>?:
              carriesTheStateGiven(ctx, walks, true)
      )
    )

    test("spend's fourth guarantee is refuted without the datum check, with the walks given") {
        report(
          stateIsCarriedWithoutDatumCheck,
          check(
            VerificationRequest(
              stateIsCarriedWithoutDatumCheck,
              UplcBlaster(Budget.Auto, lean, 30.minutes)
            )
          )
        )
    }

    /** Says how `statement` was decided: the budget, the time, and a refutation's values. */
    private def report(statement: Statement, result: => VerificationResult): Unit = {
        val start = System.nanoTime()
        val decided = result
        val seconds = (System.nanoTime() - start) / 1_000_000_000L
        val artifact = artifactOf(decided)
        val outcome = decided match
            case VerificationResult.Proven(_)              => "proven"
            case VerificationResult.ProvenWithin(_, bound) => s"proven within $bound"
            case VerificationResult.Refuted(_) => s"refuted, values ${counterexampleOf(decided)}"
            case other                         => other.toString
        info(s"${statement.name}: $outcome, budget ${artifact.budget}, $seconds s")
    }

    test("the probe, given what its walks return, accepts the samples the validator accepts") {
        // What the probe is of the validator, on the samples: with each walk's result as the walk
        // computes it on the JVM, it accepts what the validator accepts, and rejects the others.
        val samples = List(
          sampleOfEverything(signed) -> true,
          sampleOfEverything(prelude.List.Nil) -> false,
          sampleHalfway(halfOfTheAmount) -> true,
          sampleHalfway(halfOfTheAmount + BigInt(1)) -> false,
          sampleHalfwayUnder(vestedAtOnce) -> false,
          sampleOfTheRestUnder(vestedAtOnce) -> true
        )
        for (context, accepted) <- samples do
            assert(Try(VestingValidator.validate(context)).isSuccess == accepted, context)
            val walks = walksOf(context.to[ScriptContext])
            assert(
              Try(VestingWalksAsUnknowns.validate(context, walks)).isSuccess == accepted,
              walks
            )
            assert(
              Try(VestingWithoutDatumCheck.validate(context)).isSuccess ==
                  Try(VestingWalksAsUnknowns.validateWithoutDatumCheck(context, walks)).isSuccess,
              walks
            )
            assert(
              lovelaceOfTheRestFollows(walks, context.to[ScriptContext].redeemer.to[Action].amount)
            )
    }
}

object VestingWalksProbeTest {

    /** That the probe accepts `ctx` with the walks' results `walks`. */
    inline def acceptsGiven(inline ctx: ScriptContext, inline walks: Walks): Prop =
        succeeds(VestingWalksAsUnknowns.validate(ctx.toData, walks))

    /** What `lockedBy` returns, of the walks' results: it fails where the spent output is no input.
      */
    inline def lockedGiven(inline walks: Walks): BigInt =
        if walks.ownInputFound then walks.locked else prelude.fail("Tx input not found")

    /** spend's third condition, as `keepsTheUnvestedLocked` states it, with the walks given. */
    inline def keepsTheUnvestedLockedGiven(
        inline ctx: ScriptContext,
        inline walks: Walks
    ): Boolean =
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                lockedGiven(walks) - VestingValidator.amountOf(ctx.redeemer) >=
                    VestingValidator.configOf(datum).initialAmount -
                    VestingValidator.linearVesting(
                      VestingValidator.configOf(datum),
                      ctx.txInfo.validFromOrFail(VestingValidator.NoValidityLowerBound)
                    )
            case _ => true

    /** spend's fourth condition, as `carriesTheState` states it, with the walks given;
      * `withTheLemma` adds the lemma as a premise.
      */
    inline def carriesTheStateGiven(
        inline ctx: ScriptContext,
        inline walks: Walks,
        inline withTheLemma: Boolean
    ): Boolean =
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                val requested = VestingValidator.amountOf(ctx.redeemer)
                inline if withTheLemma then
                    !lovelaceOfTheRestFollows(walks, requested) || carriesTheRestGiven(
                      walks,
                      datum,
                      requested
                    )
                else carriesTheRestGiven(walks, datum, requested)
            case _ => true

    /** `amountOf(redeemer) === lockedBy(...) || carriesTheRest(...)`, with the walks given. */
    inline def carriesTheRestGiven(
        inline walks: Walks,
        inline datum: prelude.Option[Data],
        inline requested: BigInt
    ): Boolean =
        requested === lockedGiven(walks) ||
            (walks.continuingDatum.getOrFail(VestingValidator.NotExactlyOneContractOutput) ===
                OutputDatum.OutputDatum(datum.getOrFail(VestingValidator.DatumNotFound)) &&
                walks.continuingLocked === walks.locked - requested)

    /** The lemma of the library's `Value`, of the walks' values: where the value of the output that
      * keeps the rest is the spent output's less `requested` lovelace, its lovelace is the spent
      * output's less `requested`.
      */
    inline def lovelaceOfTheRestFollows(inline walks: Walks, inline requested: BigInt): Boolean =
        !walks.valueKept || walks.continuingLocked === walks.locked - requested

    /** A wrong condition: what stays locked is the whole initial amount. */
    inline def leavesTheInitialAmountGiven(
        inline ctx: ScriptContext,
        inline walks: Walks
    ): Boolean =
        ctx.scriptInfo match
            case ScriptInfo.SpendingScript(_, datum) =>
                lockedGiven(walks) - VestingValidator.amountOf(ctx.redeemer) >=
                    VestingValidator.configOf(datum).initialAmount
            case _ => true

    /** What the walks of [[VestingValidator.spend]] return on `ctx`, computed on the JVM with the
      * functions the validator calls. Of a context the validator does not read so far, the walks it
      * does not make are given as failing.
      */
    def walksOf(ctx: ScriptContext): Walks = ctx.scriptInfo match
        case ScriptInfo.SpendingScript(ref, prelude.Option.Some(datum)) =>
            val tx = ctx.txInfo
            val config = datum.to[Config]
            val requested = ctx.redeemer.to[Action].amount
            val beneficiary = Credential.PubKeyCredential(config.beneficiary)
            tx.inputs.find(_.outRef === ref) match
                case prelude.Option.Some(own) =>
                    val credential = own.resolved.address.credential
                    val continuing = tx.outputs.filter(_.address === own.resolved.address)
                    val single = continuing match
                        case prelude.List.Cons(output, prelude.List.Nil) =>
                            prelude.Option.Some(output)
                        case _ => prelude.Option.None
                    Walks(
                      ownInputFound = true,
                      vestingInputIsUnique = tx.inputs
                          .filter(_.resolved.address.credential === credential)
                          .length == 1,
                      locked = own.resolved.value.getLovelace,
                      signed = tx.isSignedBy(config.beneficiary),
                      adaInInputs = tx
                          .findInputsByCredential(beneficiary)
                          .foldLeft(Value.zero)(_ + _.resolved.value)
                          .getLovelace,
                      adaInOutputs = tx
                          .findOutputsByCredential(beneficiary)
                          .foldLeft(Value.zero)(_ + _.value)
                          .getLovelace,
                      continuingDatum = single.map(_.datum),
                      valueKept = single match
                          case prelude.Option.Some(output) =>
                              output.value === own.resolved.value - Value.lovelace(requested)
                          case prelude.Option.None => false
                      ,
                      continuingLocked = single match
                          case prelude.Option.Some(output) => output.value.getLovelace
                          case prelude.Option.None         => BigInt(0)
                    )
                case prelude.Option.None => noWalks
        case _ => noWalks

    private val noWalks: Walks =
        Walks(
          false,
          false,
          BigInt(0),
          false,
          BigInt(0),
          BigInt(0),
          prelude.Option.None,
          false,
          BigInt(0)
        )

}
