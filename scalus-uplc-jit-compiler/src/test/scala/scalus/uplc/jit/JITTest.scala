package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.Term
import scalus.uplc.eval.{Log, NoBudgetSpender, PlutusVM}
import scalus.uplc.jit.hybrid.HybridJIT
import scalus.uplc.jit.mincont
import scalus.uplc.jit.nativestack.JIT

import scala.util.Try

class JITTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    val jits = List(JIT, mincont.JIT, HybridJIT)

    for jit <- jits do {
        test(s"UPLC JIT compilation works for ${jit.getClass.getName}") {
            val uplc: Term = compile:
                ((i: BigInt) => if i > 0 then throw new Exception("Not implemented") else i + 1)(2)
            .toUplc(true)

            assert(uplc.evaluateDebug.isFailure)
            val logger = Log()
            val r = Try(jit.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams))
            assert(r.isFailure)
            assert(logger.getLogs.mkString == "Not implemented")
        }
    }
}
