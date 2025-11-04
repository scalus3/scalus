package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.{Constant, Term}

class SimpleDebugTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    val jits = List(nativestack.JIT, mincont.JIT, hybrid.HybridJIT)

    for jit <- jits do {
        test(s"Simple constant for ${jit.getClass.getName}") {
            val term = compile {
                BigInt(42)
            }.toUplc(true)
            println(s"UPLC: ${term.show}")
            val result =
                jit.jitUplc(term).apply(Log(), NoBudgetSpender, summon[PlutusVM].machineParams)
            assert(result == BigInt(42))
        }

        test(s"Simple comparison for ${jit.getClass.getName}") {
            val term = compile {
                val x = BigInt(5)
                x <= BigInt(10)
            }.toUplc(true)
            println(s"UPLC: ${term.show}")
            val result = jit
                .jitUplc(term)
                .apply(
                  Log(),
                  NoBudgetSpender,
                  summon[PlutusVM].machineParams
                )
            println(s"Result: $result")
            assert(result == true)
        }

        test(s"Simple if-then-else for  ${jit.getClass.getName}") {
            val term = compile {
                val x = BigInt(5)
                if x <= BigInt(10) then BigInt(1) else BigInt(2)
            }.toUplc(true)
            println(s"UPLC: ${term.show}")
            val result = jit
                .jitUplc(term)
                .apply(
                  Log(),
                  NoBudgetSpender,
                  summon[PlutusVM].machineParams
                )
            println(s"Result: $result")
            assert(result == BigInt(1))
        }
    }
}
