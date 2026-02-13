package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.builtin.{Data, ToData}
import scalus.uplc.eval.PlutusVM
import scalus.uplc.*

@Compile
object LogTestDefs {
    enum Color derives ToData:
        case Red, Green, Blue

    case class Point(x: BigInt, y: BigInt) derives ToData
}

class LogTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("log with single string") {
        val sir = scalus.compiler.compile {
            log("simple message")
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(logs.exists(_.contains("simple message")))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with label and BigInt") {
        val sir = scalus.compiler.compile {
            log("check", BigInt(42))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(logs.exists(l => l.contains("check") && l.contains("42")))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with multiple BigInt args") {
        val sir = scalus.compiler.compile {
            log("values", BigInt(1), BigInt(2), BigInt(3))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(
                  logs.exists(l =>
                      l.contains("values") && l.contains("1") && l.contains("2") && l
                          .contains("3")
                  )
                )
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with mixed types (Bool + BigInt)") {
        val sir = scalus.compiler.compile {
            log("mixed", true, BigInt(42))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(
                  logs.exists(l => l.contains("mixed") && l.contains("True") && l.contains("42"))
                )
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with enum via ToData bridge") {
        val sir = scalus.compiler.compile {
            val c: LogTestDefs.Color = LogTestDefs.Color.Red
            log("color", c)
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(logs.exists(l => l.contains("color")))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with case class via ToData bridge") {
        val sir = scalus.compiler.compile {
            log("point", LogTestDefs.Point(BigInt(1), BigInt(2)))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                assert(logs.exists(l => l.contains("point")))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log with zero args is no-op") {
        val sir = scalus.compiler.compile {
            log()
            BigInt(1)
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(term, _, _, logs) =>
                assert(logs.isEmpty)
                assert(term == Term.Const(Constant.Integer(1)))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("log produces single trace entry with all args concatenated") {
        val sir = scalus.compiler.compile {
            log("a", BigInt(1), BigInt(2))
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                // Should produce exactly one trace entry
                assert(logs.size == 1)
                // The single entry should contain all args separated by spaces
                val entry = logs.head
                assert(entry.contains("a"))
                assert(entry.contains("1"))
                assert(entry.contains("2"))
            case _ =>
                fail(s"Expected success, but got: $result")
    }

    test("string literals are unquoted, string expressions are quoted via Show") {
        val sir = scalus.compiler.compile {
            val s: String = "hello"
            log("label", s)
        }
        val uplc = sir.toUplc()
        val result = uplc.evaluateDebug
        result match
            case scalus.uplc.eval.Result.Success(_, _, _, logs) =>
                val entry = logs.head
                // "label" is a literal → unquoted
                assert(entry.startsWith("label"))
                // s is an expression → quoted via Show[String]
                assert(entry.contains("\"hello\""))
            case _ =>
                fail(s"Expected success, but got: $result")
    }
}
