package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude as P
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.{SIR, TargetLoweringBackend}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.{PlutusVM, Result}

import scala.annotation.nowarn

/** Checks that the deprecated `scalus.prelude` and `scalus.builtin` members behave like the
  * canonical ones they forward to.
  */
@nowarn("cat=deprecation")
class DeprecatedPackageObjectsTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    private val b = BigInt(12)
    private val o = BigInt(8)

    test("scalus.prelude BigInt forwarders return the canonical results") {
        assert(scalus.prelude.absolute(BigInt(-5)) == P.absolute(BigInt(-5)))
        assert(scalus.prelude.minimum(b)(o) == P.minimum(b)(o))
        assert(scalus.prelude.maximum(b)(o) == P.maximum(b)(o))
        assert(scalus.prelude.clamp(b)(0, 5) == P.clamp(b)(0, 5))
        assert(scalus.prelude.gcf(b)(o) == P.gcf(b)(o))
        assert(scalus.prelude.sqRoot(BigInt(17)) == P.sqRoot(BigInt(17)))
        assert(scalus.prelude.isSqrt(BigInt(16))(4) == P.isSqrt(BigInt(16))(4))
        assert(scalus.prelude.pow(BigInt(2))(10) == P.pow(BigInt(2))(10))
        assert(scalus.prelude.exp2(BigInt(3)) == P.exp2(BigInt(3)))
        assert(scalus.prelude.log2(BigInt(8)) == P.log2(BigInt(8)))
        assert(scalus.prelude.logarithm(BigInt(100))(10) == P.logarithm(BigInt(100))(10))
    }

    test("scalus.prelude generic forwarders return the canonical results") {
        assert(scalus.prelude.===(b)(b) == P.===(b)(b))
        assert(scalus.prelude.!==(b)(o) == P.!==(b)(o))
        assert(scalus.prelude.<=>(b)(o) == P.<=>(b)(o))
        assert(scalus.prelude.show(b) == P.show(b))
        assert(scalus.prelude.asScalus(scala.Seq(b)) == P.asScalus(scala.Seq(b)))
        assert(scalus.prelude.asScalus(scala.Option(b)) == P.asScalus(scala.Option(b)))
        assert(scalus.prelude.list(scala.Seq(b)) == P.list(scala.Seq(b)))
    }

    test("scalus.prelude forwarders compile on-chain") {
        val sir = compile {
            import scalus.prelude.*
            val x = BigInt(3)
            x === BigInt(3) && x.exp2 === BigInt(8) && x.maximum(5) === BigInt(
              5
            ) && x.absolute === BigInt(3)
        }
        sir.toUplc().evaluateDebug match
            case Result.Success(term, _, _, _) => assert(term == true.asTerm)
            case other                         => fail(s"unexpected result: $other")
    }

    test("scalus.prelude.? traces the same text as the canonical ?") {
        val viaOld = compile {
            val ok = P.===(BigInt(4))(BigInt(3))
            scalus.prelude.?(ok)
        }
        val viaNew = compile {
            val ok = P.===(BigInt(4))(BigInt(3))
            P.?(ok)
        }
        def logs(sir: SIR): Seq[String] = sir.toUplc().evaluateDebug match
            case Result.Success(_, _, _, logs) => logs
            case other                         => fail(s"unexpected result: $other")
        assert(logs(viaNew) == Seq("ok ? False"))
        assert(logs(viaOld) == logs(viaNew))
    }

    test("scalus.builtin BLS aliases point to scalus.uplc.builtin.bls12_381") {
        val g1: scalus.builtin.G1Element = scalus.builtin.G1Element.zero
        assert(g1 == scalus.uplc.builtin.bls12_381.G1Element.zero)
        val g2: scalus.builtin.G2Element = scalus.builtin.G2Element.zero
        assert(g2 == scalus.uplc.builtin.bls12_381.G2Element.zero)
    }
}
