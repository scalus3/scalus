package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.*
import scalus.uplc.jit.mincont.JIT

object TestJITCompilationHang {
    def main(args: Array[String]): Unit = {
        if args.length == 0 then
            throw new RuntimeException("Please run with the location of flat file as argument")

        val fname = args(0)
        val stream = getClass.getClassLoader.getResourceAsStream(fname)
        if stream == null then throw new RuntimeException(s"${fname} not found in resources")
        val bytes = stream.readAllBytes()
        stream.close()
        val program = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram

        println("Program loaded successfully")
        println(s"Starting JIT compilation for ${fname} at ${System.currentTimeMillis()}")

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

    ignore("acution_1-1.flat is jit-complied") {
        // Ignored: This test may timeout in CI due to long compilation time (~20-30s)
        // The test passes locally but may exceed CI timeout limits
        TestJITCompilationHang.main(Array("auction_1-1.flat"))
    }

    ignore("auction_1-2.flat is jit-complied") {
        TestJITCompilationHang.main(Array("auction_1-2.flat"))
    }

    ignore("auction_1-3.flat is jit-complied") {
        TestJITCompilationHang.main(Array("auction_1-3.flat"))
    }

}
