package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.BLS12_381_G1_Element.g1
import scalus.uplc.builtin.BLS12_381_G2_Element.g2
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{Builtins, ByteString, Data, ToData}
import scalus.compiler.compile
import scalus.uplc.*

import scala.annotation.nowarn

@Compile
object TotoDataInstances {
    given Data.ToData[BigInt] = (a: BigInt) => builtin.Builtins.iData(a)
    given Data.ToData[String] = (a: String) => builtin.Builtins.iData(1)
}

case class Test(a: BigInt, b: String)
@Compile
object Test {
    import TotoDataInstances.given
    given Data.ToData[Test] = ToData.derived
}

class CustomError extends Exception("custom error")

class CompilerPluginToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile error") {
        val sir = compile {
            @nowarn
            def err(msg: String): Nothing = throw new RuntimeException(msg)
            err("test")
        }
        // println(sir)
        // println(sir.pretty.render(100))
        // println(sir.toUplcOptimized().showHighlighted)
    }

    test("g1 string interpolator compiles to bls12_381_G1_uncompress call") {
        val sir = compile {
            // G1 generator point
            g1"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
        }
        val term = sir.toUplcOptimized()
        // Verify it compiles to Apply(Builtin(bls12_381_G1_uncompress), Const(ByteString))
        assert(term.isInstanceOf[Term.Apply])
        val apply = term.asInstanceOf[Term.Apply]
        assert(apply.f.isInstanceOf[Term.Builtin])
        assert(apply.f.asInstanceOf[Term.Builtin].bn == DefaultFun.Bls12_381_G1_uncompress)
        assert(apply.arg.isInstanceOf[Term.Const])
        assert(apply.arg.asInstanceOf[Term.Const].const.isInstanceOf[Constant.ByteString])
    }

    test("g1 string interpolator supports spaces") {
        val sir = compile {
            // G1 zero point with spaces for readability
            g1"c0000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
        }
        val term = sir.toUplcOptimized()
        assert(term.isInstanceOf[Term.Apply])
        val apply = term.asInstanceOf[Term.Apply]
        assert(apply.f.asInstanceOf[Term.Builtin].bn == DefaultFun.Bls12_381_G1_uncompress)
    }

    test("g2 string interpolator compiles to bls12_381_G2_uncompress call") {
        val sir = compile {
            // G2 generator point
            g2"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
        }
        val term = sir.toUplcOptimized()
        // Verify it compiles to Apply(Builtin(bls12_381_G2_uncompress), Const(ByteString))
        assert(term.isInstanceOf[Term.Apply])
        val apply = term.asInstanceOf[Term.Apply]
        assert(apply.f.isInstanceOf[Term.Builtin])
        assert(apply.f.asInstanceOf[Term.Builtin].bn == DefaultFun.Bls12_381_G2_uncompress)
        assert(apply.arg.isInstanceOf[Term.Const])
        assert(apply.arg.asInstanceOf[Term.Const].const.isInstanceOf[Constant.ByteString])
    }

    test("g2 string interpolator supports spaces") {
        val sir = compile {
            // G2 zero point with spaces for readability
            g2"c0000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
        }
        val term = sir.toUplcOptimized()
        assert(term.isInstanceOf[Term.Apply])
        val apply = term.asInstanceOf[Term.Apply]
        assert(apply.f.asInstanceOf[Term.Builtin].bn == DefaultFun.Bls12_381_G2_uncompress)
    }
