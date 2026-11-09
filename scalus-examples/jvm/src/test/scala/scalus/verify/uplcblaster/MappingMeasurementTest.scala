package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.examples.htlc.{HtlcValidator, HtlcVerificationTest as Htlc}
import scalus.examples.oneshotmint.{Mint, MintingPolicy, MintingPolicyVerificationTest as M}
import scalus.examples.vesting.{Config, VestingValidator, VestingVerificationTest as V}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.verify.*
import scalus.verify.Props.*

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.duration.*

/** How the mapping of a variable changes what a check costs: statements of the example suites, each
  * checked under several plans of how its variables reach Lean ([[MappingPlan.Fixed]]), from every
  * node by fields to the root as one `Data`. It is how the number of rule 3,
  * [[MappingPlan.mostByFields]], was measured (docs/design/verification-details/uplc-blaster.md,
  * "Mapping a variable", step 4), and how it is measured again after a change of Blaster, of the
  * Lean model or of the compiler.
  *
  * It runs only where `SCALUS_MEASURE_PLANS` is set, and Lean runs: it takes about two hours, and
  * proves nothing a suite does not. Each check is given `SCALUS_MEASURE_MINUTES` (8) minutes, and
  * finds its budget. Each line of the file `SCALUS_MEASURE_PLANS` names, tab-separated: the
  * statement, the plan, the check's Lean variables, how many of them are an enum's tag, the size of
  * its programs in bytes, whether it is the check of the tactic's own plan, which the suites'
  * statements have, the result, the budget the tactic came to, the seconds it took, and why it is
  * inconclusive.
  *
  * {{{
  * SCALUS_MEASURE_PLANS=/tmp/plans.tsv sbt "scalusExamplesJVM/testOnly *MappingMeasurementTest"
  * }}}
  */
class MappingMeasurementTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    private val out = sys.env.get("SCALUS_MEASURE_PLANS").map(Path.of(_))
    private val limit = sys.env.get("SCALUS_MEASURE_MINUTES").map(_.toInt).getOrElse(8).minutes

    /** Whether the measurements run here. */
    private def measuring(): Unit = {
        assume(out.isDefined, "measured only where SCALUS_MEASURE_PLANS names a file")
        assume(leanAvailable, "requires lake")
        warmedUp
        header()
    }

    private final case class Plan(name: String, fixed: MappingPlan.Fixed)

    /** Every node by fields: of types that do not contain themselves. */
    private val byFields = Plan("by-fields", MappingPlan.Fixed.onlyAsData())

    /** The tactic's own plan. */
    private val ownPlan = Plan("own", MappingPlan.Fixed.none)

    private def rootData(root: String) = Plan("root-data", MappingPlan.Fixed.onlyAsData(root))

    /** The plans of a script context, from the root as Data to every node by fields. Each maps the
      * nodes it names as Data and every other node by fields, whatever the tactic would decide.
      */
    private val contextPlans = List(
      rootData("ctx"),
      Plan(
        "txInfo+scriptInfo-data",
        MappingPlan.Fixed.onlyAsData("ctx.txInfo", "ctx.scriptInfo")
      ),
      Plan("scriptInfo-data", MappingPlan.Fixed.onlyAsData("ctx.scriptInfo")),
      Plan(
        "cert+procedure-data",
        MappingPlan.Fixed.onlyAsData(
          "ctx.scriptInfo.CertifyingScript.cert",
          "ctx.scriptInfo.ProposingScript.procedure"
        )
      ),
      byFields,
      ownPlan
    )

    /** The names of the columns, at the head of a file that has none yet. */
    private def header(): Unit =
        if out.forall(file => !Files.exists(file) || Files.size(file) == 0) then
            line(
              "statement",
              "plan",
              "vars",
              "tags",
              "leafBytes",
              "sameAsOwn",
              "result",
              "budget",
              "s",
              "reason"
            )

    private def line(fields: Any*): Unit = {
        val text = fields.mkString("\t") + "\n"
        print(text)
        Files.writeString(
          out.get,
          text,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
    }

    private def describe(result: VerificationResult): (String, String, String) = result match
        case VerificationResult.Proven(proof) =>
            ("proven", budgetOf(proof), "")
        case VerificationResult.ProvenWithin(proof, _) => ("within", budgetOf(proof), "")
        case VerificationResult.Refuted(proof)         => ("refuted", budgetOf(proof), "")
        case VerificationResult.Unsupported(report) =>
            ("unsupported", "", report.toString.take(300).replace('\n', ' '))
        case VerificationResult.Inconclusive(reason) =>
            ("inconclusive", "", reason.take(300).replace('\n', ' '))
        case VerificationResult.Failed(reason) =>
            ("failed", "", reason.take(300).replace('\n', ' '))

    private def budgetOf(proof: Proof): String = proof.artifact match
        case artifact: UplcBlaster.Artifact => artifact.budget.toString
        case _                              => ""

    /** `prop` under each of `plans`. */
    private def measure(
        name: String,
        prop: Prop,
        functions: List[FunctionDef[?, ?]],
        expected: ExpectedOutcome,
        plans: List[Plan]
    ): Unit = {
        val table = FunctionTable(functions*)
        // What a check is made of: its variables, their premises and its programs.
        def content(l: UplcBlaster.Lowered) = (
          l.binders.map(b => PropIdentity.written(b.name) -> b.tp.show),
          l.predicates,
          l.leaves.map(leaf => scalus.utils.Hex.bytesToHex(leaf.cborEncoded))
        )
        val own = UplcBlaster.lower(prop, table).toOption.map(content)
        plans.foreach { case Plan(planName, fixed) =>
            val lowered = UplcBlaster.lower(prop, table, fixed)
            val same = (own, lowered) match
                case (Some(ownContent), Right(l)) => (content(l) == ownContent).toString
                case _                            => "-"
            val (vars, tags, leafBytes) = lowered match
                case Right(l) =>
                    (
                      l.binders.size.toString,
                      l.binders.count(_.name.endsWith(".case")).toString,
                      l.leaves.map(_.flatEncoded.length).sum.toString
                    )
                case Left(reason) => ("-", "-", reason.take(120))
            val verifier = Verifier.empty
            functions.foreach(verifier.addFunction)
            val statement = expected match
                case ExpectedOutcome.Valid     => verifier.statement(name, prop)
                case ExpectedOutcome.Falsified => verifier.refute(name, prop)
            val tactic = UplcBlaster(Budget.Auto, lean, limit).withFixedPlan(fixed)
            val started = System.nanoTime
            val result = verifier.verify(statement, tactic)
            val seconds = (System.nanoTime - started) / 1_000_000_000L
            val (kind, budget, reason) = describe(result)
            line(name, planName, vars, tags, leafBytes, same, kind, budget, seconds, reason)
        }
    }

    // The validators, on every script context

    private val htlc = FunctionDef.named("htlc", (context: Data) => HtlcValidator.validate(context))
    private val vesting =
        FunctionDef.named("vesting", (context: Data) => VestingValidator.validate(context))

    /** A check that starts the Lean server, so that the first measured check does not. */
    private lazy val warmedUp: Unit = {
        val verifier = Verifier.empty
        verifier.verify(
          verifier.statement("warm_up", forAll[BigInt](x => x + BigInt(1) > x)),
          UplcBlaster(Budget.Auto, lean, limit)
        )
    }

    test("htlc and vesting, on every script context") {
        measuring()
        measure(
          "htlc.context.output_without_a_datum_is_rejected",
          forAll[ScriptContext](ctx => Htlc.spendsWithoutDatum(ctx) ==>: fails(htlc, ctx.toData)),
          List(htlc),
          ExpectedOutcome.Valid,
          contextPlans
        )
        measure(
          "htlc.context.only_spending_is_validated",
          forAll[ScriptContext](ctx => !Prop(Htlc.isSpending(ctx)) ==>: fails(htlc, ctx.toData)),
          List(htlc),
          ExpectedOutcome.Valid,
          contextPlans
        )
        measure(
          "vesting.validator.no_datum_is_rejected",
          forAll[ScriptContext](ctx => V.spendsWithoutDatum(ctx) ==>: fails(vesting, ctx.toData)),
          List(vesting),
          ExpectedOutcome.Valid,
          contextPlans
        )
        measure(
          "vesting.validator.only_spending",
          forAll[ScriptContext](ctx => !Prop(V.isSpending(ctx)) ==>: fails(vesting, ctx.toData)),
          List(vesting),
          ExpectedOutcome.Valid,
          contextPlans
        )
    }

    // Small case classes, by fields today

    private val linearVesting = FunctionDef(VestingValidator.linearVesting)

    test("vesting's schedule, over a Config") {
        measuring()
        val plans = List(byFields, rootData("config"), ownPlan)
        measure(
          "vesting.schedule.zero_before_start",
          forAll[Config, BigInt]((config, time) =>
              (time < config.startTimestamp) ==>:
                  call(linearVesting, (config, time))(vested => vested == BigInt(0))
          ),
          List(linearVesting),
          ExpectedOutcome.Valid,
          plans
        )
        measure(
          "vesting.schedule.returns",
          forAll[Config, BigInt]((config, time) => succeeds(linearVesting, (config, time))),
          List(linearVesting),
          ExpectedOutcome.Valid,
          plans
        )
        measure(
          "vesting.schedule.does_not_decrease",
          forAll[Config, BigInt, BigInt]((config, earlier, later) =>
              (config.initialAmount >= BigInt(0) && earlier <= later) ==>:
                  call(linearVesting, (config, earlier))(before =>
                      call(linearVesting, (config, later))(after => before <= after)
                  )
          ),
          List(linearVesting),
          ExpectedOutcome.Valid,
          plans
        )
        measure(
          "vesting.schedule.does_not_decrease_for_any_amount",
          forAll[Config, BigInt, BigInt]((config, earlier, later) =>
              (earlier <= later) ==>:
                  call(linearVesting, (config, earlier))(before =>
                      call(linearVesting, (config, later))(after => before <= after)
                  )
          ),
          List(linearVesting),
          ExpectedOutcome.Falsified,
          plans
        )
    }

    private val mints = FunctionDef.named(
      "mints",
      (m: Mint) => MintingPolicy.mint(M.context(m), m.seedId, m.seedIdx, M.tokens(m))
    )

    test("the one-shot policy, over a Mint") {
        measuring()
        val plans = List(byFields, rootData("m"), ownPlan)
        measure(
          "one_shot_mint.mint.seed_mints_the_tokens",
          forAll[Mint](m =>
              (M.spendsTheSeed(m) && M.firstIsOwn(m) && m.token == m.name &&
                  m.quantity == m.amount) ==>: succeeds(mints, m)
          ),
          List(mints),
          ExpectedOutcome.Valid,
          plans
        )
        measure(
          "one_shot_mint.mint.seed_mints_any_quantity",
          forAll[Mint](m =>
              (M.spendsTheSeed(m) && M.firstIsOwn(m) && m.token == m.name) ==>: succeeds(mints, m)
          ),
          List(mints),
          ExpectedOutcome.Falsified,
          plans
        )
        measure(
          "one_shot_mint.mint.burns_without_the_seed",
          forAll[Mint](m =>
              (!M.spendsTheSeed(m) && M.firstIsOwn(m) && m.quantity < BigInt(0)) ==>:
                  succeeds(mints, m)
          ),
          List(mints),
          ExpectedOutcome.Valid,
          plans
        )
        measure(
          "one_shot_mint.mint.accepted_only_with_the_seed",
          forAll[Mint](m => succeeds(mints, m) ==>: M.spendsTheSeed(m)),
          List(mints),
          ExpectedOutcome.Falsified,
          plans
        )
    }
}
