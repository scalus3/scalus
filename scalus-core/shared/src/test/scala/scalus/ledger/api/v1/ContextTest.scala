package scalus.ledger.api.v1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.fromData
import scalus.builtin.Data.toData
import scalus.prelude.*
import scalus.compiler.sir.SIR
import scalus.uplc.Constant
import scalus.uplc.Term
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scalus.uplc.test.ArbitraryInstances
import scala.language.implicitConversions

class ContextTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances
    with scalus.ledger.api.v1.ArbitraryInstances {
    test("PubKeyHash Eq") {
        assert(PubKeyHash(ByteString.empty) === PubKeyHash(ByteString.empty))
        assert(PubKeyHash(hex"aa") !== PubKeyHash(ByteString.empty))
        assertEval(
          compile(new PubKeyHash(ByteString.empty) === new PubKeyHash(ByteString.empty)),
          true
        )
        assertEval(
          compile(new PubKeyHash(hex"aa") === new PubKeyHash(ByteString.empty)),
          false
        )
        assertEval(
          compile(
            new PubKeyHash(hex"aa") === new PubKeyHash(hex"aa")
          ),
          true
        )
    }

    test("Interval Eq") {
        import scalus.compiler.sir.SirDSL.*
        import scalus.compiler.sir.*
        val sir = compile { (d: Data) =>
            val i = fromData[Interval](d)
            i === i
        }

        forAll { (i: Interval) =>
            assert(i === i)
            val d = i.toData
            val applied = sir $ SIR.Const(Constant.Data(d), SIRType.Data, AnnotationsDecl.empty)
            assertEval(applied, true)
        }
    }

    private def assertEval(sir: SIR, expected: Term) = {
        given PlutusVM = PlutusVM.makePlutusV2VM()
        val term = sir.toUplc()
        assert(term.evaluate == expected)
    }
}
