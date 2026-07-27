package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

class PatternMatchAuditTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    // Scenario 1: same constant in two cases, first has a guard.
    // Scala: ("a", flag=false) => 2 (falls through to second `case "a"`)
    test("guard fallthrough to later case with same constant") {
        val compiled = compile { (x: String, flag: Boolean) =>
            x match
                case "a" if flag => BigInt(1)
                case "a"         => BigInt(2)
                case _           => BigInt(3)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val aT = compile { "a" }.toUplc()
        val bT = compile { "b" }.toUplc()
        val t = compile { true }.toUplc()
        val f = compile { false }.toUplc()
        assert((uplc $ aT $ t).evaluate == 1.asTerm)
        assert((uplc $ aT $ f).evaluate == 2.asTerm) // suspected bug: yields 3
        assert((uplc $ bT $ t).evaluate == 3.asTerm)
    }

    // Scenario 2: duplicate constant in first tuple component with different second components.
    // Scala: ("a","y") => 2
    test("duplicate constant in tuple rows") {
        val compiled = compile { (x: (String, String)) =>
            x match
                case ("a", "x") => BigInt(1)
                case ("a", "y") => BigInt(2)
                case _          => BigInt(0)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val ax = compile { ("a", "x") }.toUplc()
        val ay = compile { ("a", "y") }.toUplc()
        val zz = compile { ("z", "z") }.toUplc()
        assert((uplc $ ax).evaluate == 1.asTerm)
        assert((uplc $ ay).evaluate == 2.asTerm) // suspected bug: yields 0
        assert((uplc $ zz).evaluate == 0.asTerm)
    }

    // Scenario 3: guarded wildcard row BETWEEN constructor cases.
    // Scala semantics: top-to-bottom, first match wins.
    test("guarded wildcard between constructor cases") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: Option[BigInt], flag: Boolean) =>
            x match
                case Option.Some(v) if v > BigInt(10) => BigInt(1)
                case _ if flag                        => BigInt(2)
                case Option.Some(_)                   => BigInt(3)
                case Option.None                      => BigInt(4)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val some5 = compile { Option.Some(BigInt(5)) }.toUplc()
        val some20 = compile { Option.Some(BigInt(20)) }.toUplc()
        val none = compile { Option.None: Option[BigInt] }.toUplc()
        val t = compile { true }.toUplc()
        val f = compile { false }.toUplc()
        assert((uplc $ some20 $ f).evaluate == 1.asTerm)
        assert((uplc $ some5 $ t).evaluate == 2.asTerm) // suspected bug: yields 3
        assert((uplc $ none $ t).evaluate == 2.asTerm) // suspected bug: yields 4
        assert((uplc $ some5 $ f).evaluate == 3.asTerm)
        assert((uplc $ none $ f).evaluate == 4.asTerm)
    }

    // Scenario 4: Or-pattern with a big shared action at index >= 1 used from 2 leaves
    // (forces ByReference embedding of the action).
    test("or-pattern with big shared action (ByReference embedding)") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: These[BigInt, BigInt]) =>
            x match
                case These.These(a, b) => a + b
                case These.This(_) | These.That(_) =>
                    BigInt(1) + BigInt(2) + BigInt(3) + BigInt(4) + BigInt(5) + BigInt(6)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val thisArg = compile { These.This(BigInt(7)): These[BigInt, BigInt] }.toUplc()
        val thatArg = compile { These.That(BigInt(8)): These[BigInt, BigInt] }.toUplc()
        val theseArg = compile { These.These(BigInt(1), BigInt(2)) }.toUplc()
        assert((uplc $ theseArg).evaluate == 3.asTerm)
        assert((uplc $ thisArg).evaluate == 21.asTerm)
        assert((uplc $ thatArg).evaluate == 21.asTerm)
    }

    // Scenario 5: control — guard fallthrough within SAME constructor (expected to work).
    test("guard fallthrough within same constructor") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: Option[BigInt], flag: Boolean) =>
            x match
                case Option.Some(v) if flag => v + BigInt(100)
                case Option.Some(v)         => v
                case Option.None            => BigInt(-1)
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val some5 = compile { Option.Some(BigInt(5)) }.toUplc()
        val t = compile { true }.toUplc()
        val f = compile { false }.toUplc()
        assert((uplc $ some5 $ t).evaluate == 105.asTerm)
        assert((uplc $ some5 $ f).evaluate == 5.asTerm)
    }

    // Scenario 6: ByReference action WITH bound pattern variables
    // (big action, index >= 1, duplicated into 2 constructor groups via guarded wildcard).
    test("big guarded wildcard action with binder duplicated across groups") {
        import scalus.cardano.onchain.plutus.prelude.*
        val compiled = compile { (x: These[BigInt, BigInt], flag: Boolean) =>
            x match
                case These.This(v) => v
                case _ if flag =>
                    BigInt(1) + BigInt(2) + BigInt(3) + BigInt(4) + BigInt(5) + BigInt(6)
                case These.That(v)     => v
                case These.These(a, b) => a + b
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val thisArg = compile { These.This(BigInt(7)): These[BigInt, BigInt] }.toUplc()
        val thatArg = compile { These.That(BigInt(8)): These[BigInt, BigInt] }.toUplc()
        val theseArg = compile { These.These(BigInt(1), BigInt(2)) }.toUplc()
        val t = compile { true }.toUplc()
        val f = compile { false }.toUplc()
        assert((uplc $ thisArg $ f).evaluate == 7.asTerm)
        assert((uplc $ thisArg $ t).evaluate == 7.asTerm)
        assert((uplc $ thatArg $ t).evaluate == 21.asTerm) // Scala: wildcard-guard row wins
        assert((uplc $ thatArg $ f).evaluate == 8.asTerm)
        assert((uplc $ theseArg $ t).evaluate == 21.asTerm) // suspected bug: yields 3
        assert((uplc $ theseArg $ f).evaluate == 3.asTerm)
    }
}
