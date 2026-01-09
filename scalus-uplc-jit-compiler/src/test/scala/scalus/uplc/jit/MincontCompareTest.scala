package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.compiler.compile
import scalus.uplc.eval.{Log, NoBudgetSpender, PlutusVM}

class MincontCompareTest extends AnyFunSuiteLike {
    test("Mincont if-then-else works") {
        val term = compile {
            val x = BigInt(5)
            if x <= BigInt(10) then BigInt(1) else BigInt(2)
        }.toUplc(true)

//        println(s"UPLC: ${term.show}")
        val result = JITImplementation.Mincont
            .compile(term)
            .apply(Log(), NoBudgetSpender, PlutusVM.makePlutusV3VM().machineParams)
//        println(s"Mincont result: $result")
        assert(result == BigInt(1))
    }
}
