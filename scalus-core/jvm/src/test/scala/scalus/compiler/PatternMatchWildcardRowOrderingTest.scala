package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Builtins
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.toUplc

import scala.language.implicitConversions

/** Regression tests for audit finding M2 (docs/local/audits/2026_07_10/M2.md): a wildcard row
  * written between constructor cases must keep its first-match priority in every constructor group,
  * and must reach constructor groups created after it as well as the default branch.
  */
class PatternMatchWildcardRowOrderingTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("failed wildcard guards run once when a constructor branch falls through") {
        import scalus.cardano.onchain.plutus.prelude.Option
        given Options = Options(
          targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
          optimizeUplc = false
        )
        val compiled = compile { (x: Option[BigInt], first: Boolean, last: Boolean) =>
            x match
                case Option.Some(v) if v > BigInt(10)    => BigInt(1)
                case _ if Builtins.trace("first")(first) => BigInt(2)
                case Option.Some(v) if v > BigInt(20)    => BigInt(3)
                case _ if Builtins.trace("last")(last)   => BigInt(4)
                case _                                   => BigInt(5)
        }
        val uplc = compiled.toUplc()
        val some5 = compile { Option.Some(BigInt(5)) }.toUplc()
        val some15 = compile { Option.Some(BigInt(15)) }.toUplc()
        val none = compile { Option.None: Option[BigInt] }.toUplc()

        def check(
            arg: Term,
            first: Boolean,
            last: Boolean,
            expected: Int,
            traces: List[String]
        ): Unit = {
            (uplc $ arg $ first.asTerm $ last.asTerm).evaluateDebug match
                case Result.Success(value, _, _, logs) =>
                    assert(value == BigInt(expected).asTerm)
                    // Debug logs append execution budgets to each trace message.
                    assert(logs.map(_.takeWhile(_ != ':')).toList == traces)
                case Result.Failure(error, _, _, _) => fail(error)
        }

        for arg <- List(some5, none) do {
            check(arg, false, false, 5, List("first", "last"))
            check(arg, false, true, 4, List("first", "last"))
            check(arg, true, false, 2, List("first"))
        }
        check(some15, false, false, 1, Nil)
    }

    test("guarded wildcard row between constructor cases keeps first-match priority") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: Option[BigInt], flag: Boolean) =>
            x match
                case Option.Some(v) if v > BigInt(10) => BigInt(1)
                case _ if flag                        => BigInt(2)
                case Option.Some(_)                   => BigInt(3)
                case Option.None                      => BigInt(4)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        val some15 = compile { Option.Some(BigInt(15)) }.toUplc()
        val some5 = compile { Option.Some(BigInt(5)) }.toUplc()
        val none = compile { Option.None: Option[BigInt] }.toUplc()

        assert((uplc $ some15 $ true.asTerm).evaluate == 1.asTerm)
        assert((uplc $ some15 $ false.asTerm).evaluate == 1.asTerm)
        // wildcard row is written before `case Option.Some(_)` and must win
        assert((uplc $ some5 $ true.asTerm).evaluate == 2.asTerm)
        // constructor group created after the wildcard row must still contain it
        assert((uplc $ none $ true.asTerm).evaluate == 2.asTerm)
        assert((uplc $ some5 $ false.asTerm).evaluate == 3.asTerm)
        assert((uplc $ none $ false.asTerm).evaluate == 4.asTerm)
    }

    test("unguarded wildcard-in-column row between constructor rows keeps its priority") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: (Option[BigInt], String)) =>
            x match
                case (Option.Some(_), "x") => BigInt(1)
                case (_, "b")              => BigInt(2)
                case (Option.Some(_), _)   => BigInt(3)
                case (Option.None, _)      => BigInt(4)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        val someX = compile { (Option.Some(BigInt(5)), "x") }.toUplc()
        val someB = compile { (Option.Some(BigInt(5)), "b") }.toUplc()
        val someZ = compile { (Option.Some(BigInt(5)), "z") }.toUplc()
        val noneB = compile { (Option.None: Option[BigInt], "b") }.toUplc()
        val noneZ = compile { (Option.None: Option[BigInt], "z") }.toUplc()

        assert((uplc $ someX).evaluate == 1.asTerm)
        assert((uplc $ someB).evaluate == 2.asTerm)
        assert((uplc $ noneB).evaluate == 2.asTerm)
        assert((uplc $ someZ).evaluate == 3.asTerm)
        assert((uplc $ noneZ).evaluate == 4.asTerm)
    }

    test("default branch for a constructor without own cases sees earlier wildcard rows") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: Option[BigInt], flag: Boolean) =>
            x match
                case Option.Some(v) if v > BigInt(10) => BigInt(1)
                case _ if flag                        => BigInt(2)
                case Option.Some(_)                   => BigInt(3)
                case _                                => BigInt(5)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)

        val some15 = compile { Option.Some(BigInt(15)) }.toUplc()
        val some5 = compile { Option.Some(BigInt(5)) }.toUplc()
        val none = compile { Option.None: Option[BigInt] }.toUplc()

        assert((uplc $ some15 $ false.asTerm).evaluate == 1.asTerm)
        assert((uplc $ some5 $ true.asTerm).evaluate == 2.asTerm)
        // None has no constructor case: it goes through the default branch,
        // which must include the earlier `case _ if flag` row
        assert((uplc $ none $ true.asTerm).evaluate == 2.asTerm)
        assert((uplc $ some5 $ false.asTerm).evaluate == 3.asTerm)
        assert((uplc $ none $ false.asTerm).evaluate == 5.asTerm)
    }
}
