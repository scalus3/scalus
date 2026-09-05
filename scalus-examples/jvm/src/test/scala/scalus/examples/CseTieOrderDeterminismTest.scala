package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data

/** The smallest Scalus source that used to compile to two different scripts in two different JVM
  * runs: two field-access chains over two parameters bound in the same scope, each chain used
  * twice. After lowering, the two CSE candidates have the same size, the same first 60 printed
  * characters, and the same bind point, so their relative extraction order was whatever the
  * candidate hash map yielded - and `DefaultFun.hashCode` is a per-run identity hash.
  *
  * See docs/internal/UPLC_OPTIMIZER_DETERMINISM.md.
  */
@Compile
object SameScopeFieldChains {
    def validate(d1: Data, d2: Data): Boolean = {
        val a = addInteger(
          unIData(headList(tailList(tailList(sndPair(unConstrData(d1)))))),
          unIData(headList(tailList(tailList(sndPair(unConstrData(d1))))))
        )
        val b = multiplyInteger(
          unIData(headList(tailList(tailList(sndPair(unConstrData(d2)))))),
          unIData(headList(tailList(tailList(sndPair(unConstrData(d2))))))
        )
        equalsInteger(a, b)
    }
}

class CseTieOrderDeterminismTest extends AnyFunSuite {
    test("same-scope tied CSE candidates compile to one pinned script") {
        given Options = Options.releaseUntagged
        val program = PlutusV3.compile(SameScopeFieldChains.validate).program
        // The d1 chain is bound first because it occurs first; before the fix, the d2-first
        // variant was produced in some JVM runs.
        assert(
          program.cborByteString.toHex ==
              "58390101009800aba1aab9eaba2488888c8c8cdc399b80002002337040020026eb4c018c010c010c014dd50011bad3005300330033004375400401"
        )
    }
}
