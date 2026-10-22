package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.compiler.compile
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}
import scalus.uplc.eval.{Log, NoBudgetSpender, PlutusVM}

import scala.util.Try

class JITTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("a failing script fails and keeps its trace") {
        val uplc: Term = compile:
            ((i: BigInt) => if i > 0 then throw new Exception("Not implemented") else i + 1)(2)
        .toUplc(true)

        assert(uplc.evaluateDebug.isFailure)
        val logger = Log()
        val compiled = nativestack.JIT.jitUplc(uplc)
        val r = Try(compiled(logger, NoBudgetSpender, summon[PlutusVM].machineParams))
        assert(r.isFailure)
        assert(logger.getLogs.mkString == "Not implemented")
    }

    test("mkCons of a constr value fails, also on a (list value), as on the CEK machine") {
        val mkCons = Term.Force(Term.Builtin(DefaultFun.MkCons))
        val constr = Term.Constr(Word64.Zero, Nil)
        for uni <- List(DefaultUni.BuiltinValue, DefaultUni.Integer) do
            val uplc = Term.Apply(Term.Apply(mkCons, constr), Term.Const(Constant.List(uni, Nil)))
            assert(uplc.evaluateDebug.isFailure)
            val compiled = nativestack.JIT.jitUplc(uplc)
            val r = Try(compiled(Log(), NoBudgetSpender, summon[PlutusVM].machineParams))
            assert(r.isFailure, s"JIT result for (list $uni): $r")
    }
}
