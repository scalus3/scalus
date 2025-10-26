package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DeBruijnedProgram

import java.nio.file.{Files, Paths}

object TestJITCompilationHang {
    def main(args: Array[String]): Unit = {
        val stream = getClass.getClassLoader.getResourceAsStream("auction_1-1.flat")
        if stream == null then throw new RuntimeException("auction_1-1.flat not found in resources")
        val bytes = stream.readAllBytes()
        stream.close()
        val program = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram

        println("Program loaded successfully")
        println(s"Starting JIT compilation at ${System.currentTimeMillis()}")

        val start = System.currentTimeMillis()
        println("Calling JIT.jitUplc...")
        val jitted = JIT.jitUplc(program.term)
        val end = System.currentTimeMillis()

        println(s"JIT compilation completed in ${end - start} ms")

        // Try running it
        val spender = CountingBudgetSpender()
        val result = jitted(NoLogger, spender, MachineParams.defaultPlutusV2PostConwayParams)
        println(s"Execution completed")
        println(s"Budget: ${spender.getSpentBudget.showJson}")
        println(s"Result: $result")
    }
}

class JITCompilationHangTest extends AnyFunSuite {

    test("acution_1.flat is jit-complied") {
        TestJITCompilationHang.main(Array.empty[String])
    }

}
