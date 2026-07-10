package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.Builtins
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

import scala.language.implicitConversions

/** Audit finding E3: `x.copy(...)` must evaluate the receiver `x` exactly once (Scala semantics),
  * even when every field is overridden explicitly. The receiver's traces/errors must not be
  * dropped.
  */
class CopyReceiverEvalTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    case class Box(a: BigInt, b: BigInt)

    // All fields explicit → no copy$default getter references the receiver → the receiver was
    // dropped. Scala evaluates it once, so the trace must appear exactly once.
    test("copy with all fields explicit evaluates the receiver exactly once") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("recv")(Box(x, x)).copy(a = x + BigInt(1), b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(
          result.logs.count(_.contains("recv")) == 1,
          s"expected receiver evaluated once, logs=${result.logs}"
        )
        assert((uplc $ one).evaluate == 2.asTerm) // a = 1 + 1
    }

    // All fields explicit, receiver expression itself errors (impure, not hoisted to a val). Scala
    // evaluates it eagerly, so the script must fail. The bug drops the receiver, so it succeeds.
    test("copy with all fields explicit fails when the receiver errors") {
        val compiled = compile { (x: BigInt) =>
            ((throw new RuntimeException("boom")): Box).copy(a = x, b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluateDebug.isFailure)
    }

    // Guard: a defaulted field keeps the receiver referenced via copy$default; the fix must NOT
    // double-evaluate it. Trace count stays exactly 1. (Passes before and after — regression guard.)
    test("copy with a defaulted field evaluates the receiver exactly once") {
        val compiled = compile { (x: BigInt) =>
            Builtins.trace("recv")(Box(x, x)).copy(a = x + BigInt(1)).b
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        val result = (uplc $ one).evaluateDebug
        assert(result.isSuccess)
        assert(
          result.logs.count(_.contains("recv")) == 1,
          s"expected receiver evaluated once, logs=${result.logs}"
        )
    }

    // Control: pure receiver (a local val) needs no sequencing and behaves correctly.
    test("copy on a pure receiver works") {
        val compiled = compile { (x: BigInt) =>
            val box = Box(x, x)
            box.copy(a = x + BigInt(1), b = x).a
        }
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val one = compile { BigInt(1) }.toUplc()
        assert((uplc $ one).evaluate == 2.asTerm)
    }
}
