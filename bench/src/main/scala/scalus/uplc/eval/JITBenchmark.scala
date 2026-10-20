package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.jit.{CompiledTerm, UplcValue}
import scalus.uplc.jit.nativestack.JIT

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Evaluation time of JIT-compiled scripts, with the machine of the JIT's scope: PlutusV3, builtin
  * semantics variant E, protocol version 11.
  */
@State(Scope.Thread)
class JITBenchmark:
    @Param(
      Array(
        "auction_1-1.flat",
        "auction_1-2.flat",
        "auction_1-3.flat",
        "auction_1-4.flat"
      )
    )
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var compiled: CompiledTerm | Null = null
    private val params =
        PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV).machineParams

    @Setup
    def setup(): Unit = {
        val bytes = Files.readAllBytes(Paths.get(s"src/main/resources/data/$file"))
        compiled = JIT.jitUplc(DeBruijnedProgram.fromFlatEncoded(bytes).toProgram.term)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT(): UplcValue = {
        // returned, so that JMH consumes the result and cannot drop the evaluation
        compiled.nn(NoLogger, NoBudgetSpender, params)
    }
