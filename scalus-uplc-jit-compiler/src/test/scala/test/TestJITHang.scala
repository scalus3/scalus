package test

import scalus.*
import scalus.uplc.{DeBruijnedProgram, DefaultFun, Program, Term}
import scalus.uplc.eval.*
import java.nio.file.{Files, Paths}

object TestJITHang {
    def main(args: Array[String]): Unit = {
        val path = Paths.get("../bench/src/main/resources/data/auction_1-1.flat")
        val bytes = Files.readAllBytes(path)
        val program = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram
        
        println("Program loaded successfully")
        println(s"Starting JIT compilation at ${System.currentTimeMillis()}")
        
        val start = System.currentTimeMillis()
        println("Calling JIT.jitUplc...")
        val jitted = JIT.jitUplc(program.term)
        val end = System.currentTimeMillis()
        
        println(s"JIT completed in ${end - start} ms")
        
        // Try running it
        val spender = CountingBudgetSpender()
        jitted(NoLogger, spender, MachineParams.defaultPlutusV2PostConwayParams)
        println(spender.getSpentBudget.showJson)
    }
}
