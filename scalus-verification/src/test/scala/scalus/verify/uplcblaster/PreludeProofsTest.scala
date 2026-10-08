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
  * has a negative control, which must be refuted. Its samples are checked on the Scalus CEK, and
  * proved through Lean on the Lean model, so the two machines are checked against each other.
  */
class PreludeProofsTest extends AnyFunSuite with LeanProofs {

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

    /** One function's samples: a conjunction of closed calls, each with its expected result. */
    private final case class Samples(
        function: FunctionDef[?, ?],
        prop: Prop,
        budget: Int
    )

    private val samples = Seq(
      Samples(
        abs,
        callRef(abs.ref, BigInt(-7))(r => r == BigInt(7))
            && callRef(abs.ref, BigInt(0))(r => r == BigInt(0))
            && callRef(abs.ref, BigInt(9))(r => r == BigInt(9)),
        60
      ),
      Samples(
        min,
        callRef(min.ref, (BigInt(3), BigInt(5)))(r => r == BigInt(3))
            && callRef(min.ref, (BigInt(5), BigInt(3)))(r => r == BigInt(3)),
        60
      ),
      Samples(
        max,
        callRef(max.ref, (BigInt(3), BigInt(5)))(r => r == BigInt(5))
            && callRef(max.ref, (BigInt(5), BigInt(3)))(r => r == BigInt(5)),
        60
      ),
      Samples(
        clamp,
        callRef(clamp.ref, (BigInt(9), BigInt(1), BigInt(5)))(r => r == BigInt(5))
            && callRef(clamp.ref, (BigInt(-9), BigInt(1), BigInt(5)))(r => r == BigInt(1))
            && callRef(clamp.ref, (BigInt(3), BigInt(1), BigInt(5)))(r => r == BigInt(3)),
        80
      ),
      Samples(
        exp2,
        callRef(exp2.ref, BigInt(10))(r => r == BigInt(1024))
            && callRef(exp2.ref, BigInt(0))(r => r == BigInt(1))
            && callRef(exp2.ref, BigInt(-1))(r => r == BigInt(0)),
        200
      ),
      Samples(
        gcd,
        callRef(gcd.ref, (BigInt(12), BigInt(18)))(r => r == BigInt(6))
            && callRef(gcd.ref, (BigInt(-19), BigInt(14)))(r => r == BigInt(1))
            && callRef(gcd.ref, (BigInt(0), BigInt(5)))(r => r == BigInt(5)),
        600
      ),
      Samples(
        gcdUnoptimized,
        callRef(gcdUnoptimized.ref, (BigInt(12), BigInt(18)))(r => r == BigInt(6))
            && callRef(gcdUnoptimized.ref, (BigInt(-19), BigInt(14)))(r => r == BigInt(1)),
        600
      ),
      Samples(
        sqrt,
        callRef(sqrt.ref, BigInt(10000))(r => r == BigInt(100))
            && callRef(sqrt.ref, BigInt(0))(r => r == BigInt(0)),
        1000
      ),
      Samples(
        optDoubleOrDefault,
        callRef(optDoubleOrDefault.ref, BigInt(5))(r => r == BigInt(10))
            && callRef(optDoubleOrDefault.ref, BigInt(-5))(r => r == BigInt(-1)),
        160
      ),
      Samples(
        listSum2,
        callRef(listSum2.ref, (BigInt(3), BigInt(4)))(r => r == BigInt(7))
            && callRef(listSum2.ref, (BigInt(-1), BigInt(1)))(r => r == BigInt(0)),
        320
      )
    )

    samples.foreach { case Samples(function, prop, budget) =>
        test(s"the samples of ${function.name} hold on the Scalus CEK") {
            val lowered = UplcBlaster.lower(prop, FunctionTable(function)) match
                case Right(lowered) => lowered
                case Left(reason)   => fail(reason)
            lowered.leaves.foreach { leaf =>
                leaf.term.evaluateDebug match
                    case success: Result.Success =>
                        assert(success.term == Term.Const(Constant.Bool(true)))
                    case failure: Result.Failure => fail(failure.exception)
            }
        }
        test(s"the samples of ${function.name} hold on the Lean model") {
            // A closed statement: Lean decides it by running the programs.
            assert(proven(prop, budget, function) == ProofKind.LeanNative)
        }
    }

    test("abs is a non-negative magnitude") {
        proven(forAll[BigInt](x => callRef(abs.ref, x)(r => r >= BigInt(0))), 100, abs)
        proven(forAll[BigInt](x => callRef(abs.ref, x)(r => r == x || r == -x)), 100, abs)
        // negative control: abs(0) = 0
        refuted(forAll[BigInt](x => callRef(abs.ref, x)(r => r > BigInt(0))), 100, abs)
    }

    test("min is a lower bound and one of its arguments") {
        proven(
          forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r <= x && r <= y)),
          100,
          min
        )
        proven(
          forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r == x || r == y)),
          100,
          min
        )
        // negative control
        refuted(
          forAll[BigInt, BigInt]((x, y) => callRef(min.ref, (x, y))(r => r >= x && r >= y)),
          100,
          min
        )
    }

    test("max is an upper bound, and min + max is the sum") {
        proven(
          forAll[BigInt, BigInt]((x, y) => callRef(max.ref, (x, y))(r => r >= x && r >= y)),
          100,
          max
        )
        proven(
          forAll[BigInt, BigInt]((x, y) =>
              callRef(min.ref, (x, y))(a => callRef(max.ref, (x, y))(b => a + b == x + y))
          ),
          120,
          min,
          max
        )
        // negative control
        refuted(
          forAll[BigInt, BigInt]((x, y) => callRef(max.ref, (x, y))(r => r <= x)),
          100,
          max
        )
    }

    test("clamp stays in range, and keeps a value already in range") {
        proven(
          forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
              (lo <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
          ),
          120,
          clamp
        )
        proven(
          forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
              (lo <= x && x <= hi) ==> callRef(clamp.ref, (x, lo, hi))(r => r == x)
          ),
          120,
          clamp
        )
        // negative control: without lo <= hi the range is empty
        refuted(
          forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
              callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
          ),
          120,
          clamp
        )
    }

    test("exp2 of a negative exponent is zero") {
        proven(
          forAll[BigInt](e => (e < BigInt(0)) ==> callRef(exp2.ref, e)(r => r == BigInt(0))),
          60,
          exp2
        )
        // negative control
        refuted(
          forAll[BigInt](e => (e < BigInt(0)) ==> callRef(exp2.ref, e)(r => r == BigInt(1))),
          60,
          exp2
        )
    }

    test("an Option match doubles a positive value and defaults otherwise") {
        proven(
          forAll[BigInt](x =>
              (x > BigInt(0)) ==> callRef(optDoubleOrDefault.ref, x)(r => r == x * 2)
          ),
          160,
          optDoubleOrDefault
        )
        proven(
          forAll[BigInt](x =>
              (x <= BigInt(0)) ==> callRef(optDoubleOrDefault.ref, x)(r => r == BigInt(-1))
          ),
          160,
          optDoubleOrDefault
        )
        // negative control: a non-positive value is not doubled
        refuted(
          forAll[BigInt](x => callRef(optDoubleOrDefault.ref, x)(r => r == x * 2)),
          160,
          optDoubleOrDefault
        )
    }

    test("a fold over a two-element list is the sum") {
        proven(
          forAll[BigInt, BigInt]((a, b) => callRef(listSum2.ref, (a, b))(r => r == a + b)),
          320,
          listSum2
        )
        // negative control
        refuted(
          forAll[BigInt, BigInt]((a, b) => callRef(listSum2.ref, (a, b))(r => r == a * b)),
          320,
          listSum2
        )
    }
}
