package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SirDSL.given

import scala.language.implicitConversions

class SirDSLTypingTest extends AnyFunSuite:

    test("type list of ints moved to SIR via implicit conversion") {
        val sir: SIR = List(1, 2, 3)

        assert(sir.tp == SIRType.BuiltinList(SIRType.Integer))

    }

    test("list type is created without unfilled proxies") {
        val stp = SIRType.List(SIRType.Integer)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("pair type is created without unfilled proxies") {
        val stp = SIRType.BuiltinPair(SIRType.Integer, SIRType.Boolean)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("function type is created without unfilled proxies") {
        val stp = SIRType.Fun(SIRType.Integer, SIRType.Boolean)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("list type fron macro is created without unfilled proxies") {
        val stp = SIRType.List(SIRType.Integer)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("builtin.List is used in constants") {

        val sir: SIR = List(1, 2, 3)

        assert(sir.tp == SIRType.BuiltinList(SIRType.Integer))
        assert(sir.isInstanceOf[SIR.Const])
    }

    // test("calculate types extract pair types") {
    //    val pv = SIR.Var("p", (Data,Data))
    //
    //    val k = LamAbs(pv, SIRBuiltins.sndPair $ pv)
    //
    //    println(s"k.tp: ${k.tp.show } ")
    //
    // }
