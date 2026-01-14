package scalus.uplc.eval

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalus.*
import scalus.uplc.DeBruijnedProgram

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Benchmark for CekMachine running on JVM.
  *
  * This follows the structure of Plutus `validation` benchmarks. Those can be found at:
  * https://github.com/input-output-hk/plutus/blob/086d16a771a894a10e5a99e17d4082ea32af8a89/plutus-benchmark/validation/bench/BenchCek.hs
  *
  * The idea is to benchmark the evaluation of some UPLC programs (smart contracts) using the
  * CekMachine implementation on the JVM. Don't account for deserialization time, only evaluation
  * time.
  */
@State(Scope.Benchmark)
class CekJVMBenchmark:
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
    private var path: Path = null
    private var program: DeBruijnedProgram = null
    private val vm = PlutusVM.makePlutusV1VM()

    @Setup
    def readProgram() = {
        path = Paths.get(s"src/main/resources/data/$file")
        val bytes = Files.readAllBytes(path)
        program = DeBruijnedProgram.fromFlatEncoded(bytes)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def bench() = {
        vm.evaluateScript(program, RestrictingBudgetSpender(ExBudget.enormous), NoLogger)
    }
