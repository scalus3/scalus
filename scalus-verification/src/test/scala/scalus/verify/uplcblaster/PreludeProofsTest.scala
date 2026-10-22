package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
// Wildcard, not a selective import: `List.foldLeft` and friends are extension methods that a
// selective import would leave out of scope. This shadows scala.List and scala.Option here.
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.{Constant, PlutusV3, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*

/** Properties of `Math` and of prelude data structures, proved with [[UplcBlaster]] about each
  * function's own compiled program.
  *
  * These restate the former hand-written Lean suite (`Math.lean`, `Data.lean`). A call is total and
  * its conclusion is read strongly (design doc §6.2), so every property also claims that the
  * function returns; the separate totality theorems of the Lean files are implied. Every function
  * with a property also has a negative control, declared with `refute`, which must be refuted;
  * `gcd`, its unoptimized form and `sqrt` have samples only. A function's samples are checked on
  * the Scalus CEK, and proved through Lean on the Lean model, so the two machines are checked
  * against each other.
  */
class PreludeProofsTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    /** These are proofs about the prelude's code, and their results are kept beside this file. */
    override protected def keptResultsFile: scala.Option[java.nio.file.Path] = scala.Some(
      LeanProofs
          .inSources(
            "scalus-verification",
            "src",
            "test",
            "scala",
            "scalus",
            "verify",
            "uplcblaster"
          )
          .resolve("PreludeProofsTest.proofs.json")
    )
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val abs = FunctionDef.named("abs", (x: BigInt) => Math.abs(x))
    private val min = FunctionDef.named("min", (x: BigInt, y: BigInt) => Math.min(x, y))
    private val max = FunctionDef.named("max", (x: BigInt, y: BigInt) => Math.max(x, y))
    private val clamp = FunctionDef(Math.clamp)
    private val exp2 = FunctionDef(Math.exp2)
    private val gcd = FunctionDef(Math.gcd)
    private val sqrt = FunctionDef(Math.sqrt)

    /** `gcd` compiled without the UPLC optimizer, for codegen-equivalence proofs. */
    private val gcdUnoptimized = FunctionDef.fromCompiled(
      FunctionDef.synthetic[(BigInt, BigInt), BigInt]("gcd_unoptimized", 2),
      PlutusV3.compile((x: BigInt, y: BigInt) => Math.gcd(x, y))(using
        UplcBlaster.options.copy(optimizeUplc = false, uplcOptimizers = Seq.empty)
      )
    )

    private val optDoubleOrDefault = FunctionDef.named(
      "opt_double_or_default",
      (x: BigInt) =>
          val o = if x > 0 then Option.Some(x) else Option.None
          o match
              case Option.Some(v) => v * 2
              case Option.None    => BigInt(-1)
    )

    private val listSum2 = FunctionDef.named(
      "list_sum2",
      (a: BigInt, b: BigInt) =>
          List.Cons(a, List.Cons(b, List.Nil)).foldLeft(BigInt(0))((acc, x) => acc + x)
    )

    Seq(abs, min, max, clamp, exp2, gcd, gcdUnoptimized, sqrt, optDoubleOrDefault, listSum2)
        .foreach(verifier.addFunction)

    /** One function's samples: a conjunction of closed calls, each with its expected result,
      * declared as the statement `prelude.<name>.samples`.
      */
    private final class Samples(
        name: String,
        val function: FunctionDef[?, ?],
        val prop: Prop,
        val budget: Int
    ) {
        val statement: Statement = verifier.statement(s"prelude.$name.samples", prop)
    }

    private val samples = Seq(
      Samples(
        "abs",
        abs,
        callRef(abs.ref, BigInt(-7))(r => r == BigInt(7))
            && callRef(abs.ref, BigInt(0))(r => r == BigInt(0))
            && callRef(abs.ref, BigInt(9))(r => r == BigInt(9)),
        60
      ),
      Samples(
        "min",
        min,
        callRef(min.ref, (BigInt(3), BigInt(5)))(r => r == BigInt(3))
            && callRef(min.ref, (BigInt(5), BigInt(3)))(r => r == BigInt(3)),
        60
      ),
      Samples(
        "max",
        max,
        callRef(max.ref, (BigInt(3), BigInt(5)))(r => r == BigInt(5))
            && callRef(max.ref, (BigInt(5), BigInt(3)))(r => r == BigInt(5)),
        60
      ),
      Samples(
        "clamp",
        clamp,
        callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))
            && callRef(clamp.ref, (BigInt(-9), BigInt(1), BigInt(5)))(r => r == BigInt(1))
            && callRef(clamp.ref, (BigInt(3), BigInt(1), BigInt(5)))(r => r == BigInt(3)),
        80
      ),
      Samples(
        "exp2",
        exp2,
        callRef(exp2.ref, BigInt(10))(r => r == BigInt(1024))
            && callRef(exp2.ref, BigInt(0))(r => r == BigInt(1))
            && callRef(exp2.ref, BigInt(-1))(r => r == BigInt(0)),
        200
      ),
      Samples(
        "gcd",
        gcd,
        callRef(gcd.ref, (BigInt(12), BigInt(18)))(r => r == BigInt(6))
            && callRef(gcd.ref, (BigInt(-19), BigInt(14)))(r => r == BigInt(1))
            && callRef(gcd.ref, (BigInt(0), BigInt(5)))(r => r == BigInt(5)),
        600
      ),
      Samples(
        "gcd_unoptimized",
        gcdUnoptimized,
        callRef(gcdUnoptimized.ref, (BigInt(12), BigInt(18)))(r => r == BigInt(6))
            && callRef(gcdUnoptimized.ref, (BigInt(-19), BigInt(14)))(r => r == BigInt(1)),
        600
      ),
      Samples(
        "sqrt",
        sqrt,
        callRef(sqrt.ref, BigInt(10000))(r => r == BigInt(100))
            && callRef(sqrt.ref, BigInt(0))(r => r == BigInt(0)),
        1000
      ),
      Samples(
        "opt_double_or_default",
        optDoubleOrDefault,
        callRef(optDoubleOrDefault.ref, BigInt(5))(r => r == BigInt(10))
            && callRef(optDoubleOrDefault.ref, BigInt(-5))(r => r == BigInt(-1)),
        160
      ),
      Samples(
        "list_sum2",
        listSum2,
        callRef(listSum2.ref, (BigInt(3), BigInt(4)))(r => r == BigInt(7))
            && callRef(listSum2.ref, (BigInt(-1), BigInt(1)))(r => r == BigInt(0)),
        320
      )
    )

    // abs
    private val absIsNonNegative = verifier.statement(
      "prelude.abs.non_negative",
      forAll[BigInt](x => callRef(abs.ref, x)(r => r >= BigInt(0)))
    )
    private val absIsTheArgumentOrItsNegation = verifier.statement(
      "prelude.abs.argument_or_negation",
      forAll[BigInt](x => callRef(abs.ref, x)(r => r == x || r == -x))
    )
    // negative control: abs(0) = 0
    private val absIsPositive = verifier.refute(
      "prelude.abs.positive",
      forAll[BigInt](x => callRef(abs.ref, x)(r => r > BigInt(0)))
    )

    // min and max
    private val minIsALowerBound = verifier.statement(
      "prelude.min.lower_bound",
      forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r <= x && r <= y))
    )
    private val minIsAnArgument = verifier.statement(
      "prelude.min.an_argument",
      forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r == x || r == y))
    )
    // negative control
    private val minIsAnUpperBound = verifier.refute(
      "prelude.min.upper_bound",
      forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r >= x && r >= y))
    )
    private val maxIsAnUpperBound = verifier.statement(
      "prelude.max.upper_bound",
      forAll[BigInt, BigInt]((x, y) => callRef(max.ref, (x, y))(r => r >= x && r >= y))
    )
    private val minPlusMaxIsTheSum = verifier.statement(
      "prelude.min_max.sum",
      forAll[BigInt, BigInt]((x, y) =>
          callRef(min.ref, (x, y))(a => callRef(max.ref, (x, y))(b => a + b == x + y))
      )
    )
    // negative control
    private val maxIsAtMostTheFirst = verifier.refute(
      "prelude.max.at_most_the_first",
      forAll[BigInt, BigInt]((x, y) => callRef(max.ref, (x, y))(r => r <= x))
    )

    // clamp
    private val clampStaysInRange = verifier.statement(
      "prelude.clamp.in_range",
      forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
          (lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
      )
    )
    private val clampKeepsAValueInRange = verifier.statement(
      "prelude.clamp.keeps_a_value_in_range",
      forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
          (lo <= x && x <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => r == x)
      )
    )
    // negative control: without lo <= hi the range is empty
    private val clampStaysInAnyRange = verifier.refute(
      "prelude.clamp.in_any_range",
      forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
          callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
      )
    )

    // exp2
    private val exp2OfANegativeExponentIsZero = verifier.statement(
      "prelude.exp2.negative_exponent",
      forAll[BigInt](e => (e < BigInt(0)) ==> callRef(exp2.ref, e)(r => r == BigInt(0)))
    )
    // negative control
    private val exp2OfANegativeExponentIsOne = verifier.refute(
      "prelude.exp2.negative_exponent_one",
      forAll[BigInt](e => (e < BigInt(0)) ==> callRef(exp2.ref, e)(r => r == BigInt(1)))
    )

    // an Option match
    private val aPositiveValueIsDoubled = verifier.statement(
      "prelude.option_match.doubles_a_positive_value",
      forAll[BigInt](x => (x > BigInt(0)) ==> callRef(optDoubleOrDefault.ref, x)(r => r == x * 2))
    )
    private val otherwiseTheDefault = verifier.statement(
      "prelude.option_match.defaults_otherwise",
      forAll[BigInt](x =>
          (x <= BigInt(0)) ==> callRef(optDoubleOrDefault.ref, x)(r => r == BigInt(-1))
      )
    )
    // negative control: a non-positive value is not doubled
    private val everyValueIsDoubled = verifier.refute(
      "prelude.option_match.doubles_every_value",
      forAll[BigInt](x => callRef(optDoubleOrDefault.ref, x)(r => r == x * 2))
    )

    // a fold
    private val theFoldIsTheSum = verifier.statement(
      "prelude.list_fold.sum",
      forAll[BigInt, BigInt]((a, b) => callRef(listSum2.ref, (a, b))(r => r == a + b))
    )
    // negative control
    private val theFoldIsTheProduct = verifier.refute(
      "prelude.list_fold.product",
      forAll[BigInt, BigInt]((a, b) => callRef(listSum2.ref, (a, b))(r => r == a * b))
    )

    samples.foreach { sample =>
        test(s"the samples of ${sample.function.name} hold on the Scalus CEK") {
            val lowered = UplcBlaster.lower(sample.prop, FunctionTable(sample.function)) match
                case Right(lowered) => lowered
                case Left(reason)   => fail(reason)
            lowered.leaves.foreach { leaf =>
                leaf.term.evaluateDebug match
                    case success: Result.Success =>
                        assert(success.term == Term.Const(Constant.Bool(true)))
                    case failure: Result.Failure => fail(failure.exception)
            }
        }
        test(s"the samples of ${sample.function.name} hold on the Lean model") {
            // A closed statement: Lean decides it by running the programs.
            assert(artifactOf(check(sample.statement, sample.budget)).kind == ProofKind.LeanNative)
        }
    }

    test("abs is a non-negative magnitude") {
        check(absIsNonNegative, 100)
        check(absIsTheArgumentOrItsNegation, 100)
        check(absIsPositive, 100)
    }

    test("min is a lower bound and one of its arguments") {
        check(minIsALowerBound, 100)
        check(minIsAnArgument, 100)
        check(minIsAnUpperBound, 100)
    }

    test("max is an upper bound, and min + max is the sum") {
        check(maxIsAnUpperBound, 100)
        check(minPlusMaxIsTheSum, 120)
        check(maxIsAtMostTheFirst, 100)
    }

    test("clamp stays in range, and keeps a value already in range") {
        check(clampStaysInRange, 120)
        check(clampKeepsAValueInRange, 120)
        check(clampStaysInAnyRange, 120)
    }

    test("exp2 of a negative exponent is zero") {
        check(exp2OfANegativeExponentIsZero, 60)
        check(exp2OfANegativeExponentIsOne, 60)
    }

    test("an Option match doubles a positive value and defaults otherwise") {
        check(aPositiveValueIsDoubled, 160)
        check(otherwiseTheDefault, 160)
        check(everyValueIsDoubled, 160)
    }

    test("a fold over a two-element list is the sum") {
        check(theFoldIsTheSum, 320)
        check(theFoldIsTheProduct, 320)
    }
}
