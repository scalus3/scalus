package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.cardano.ledger.ExUnits
import scalus.uplc.{BenchData, DeBruijnedProgram}
import scalus.uplc.jit.{UplcCompiler, UplcEvaluator}

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Evaluation of JIT-compiled scripts, with the parameters of [[CekJVMBenchmark]]: a PlutusV3
  * machine with the mainnet cost model and protocol version, costing on. Flat decode and compile
  * run once, in setup. A script past the JIT's call-depth limit falls back to the CEK machine.
  *
  * The same loop as plutus-vm-benchmark's `pvb.scalus.JITBenchmark`. The JIT recurses on the JVM
  * stack, so it needs `-Xss64m` (the `bench` project sets it). Scripts: [[BenchData]].
  */
@State(Scope.Thread)
class JITBenchmark {
    @Param(Array("auction_1-1.flat", "auction_1-2.flat", "auction_1-3.flat", "auction_1-4.flat"))
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var evaluator: UplcEvaluator = null
    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    @Setup
    def setup(): Unit = {
        val term = DeBruijnedProgram.fromFlatEncoded(BenchData.read(file)).toProgram.term
        // the code is generated for one protocol version: the one of the machine that runs it
        evaluator = UplcCompiler.compile(term, protocolVersion = vm.protocolVersion).withFallback()
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT(): RestrictingBudgetSpender = {
        val spender = RestrictingBudgetSpender(ExUnits.enormous)
        evaluator.run(spender, NoLogger)
        // returned, so that JMH consumes the result and cannot drop the evaluation
        spender
    }
}
