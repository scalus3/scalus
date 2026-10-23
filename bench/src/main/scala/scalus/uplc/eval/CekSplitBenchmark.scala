package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.uplc.{DeBruijnedProgram, Term}

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Splits the plutus-vm-benchmark ranking loop (flat decode + CEK eval, PlutusV3, PV11, counting
  * spender) into its two halves, so a gap to another VM can be assigned to decode or to eval.
  *
  * The scripts are read from `src/main/resources/data` (78 of them), or from the directory in the
  * system property `bench.data`, for example plutus-vm-benchmark's `data/plutus_use_cases` (all
  * 89):
  * {{{
  * sbtn "bench/Jmh/run -prof gc -jvmArgsAppend -Dbench.data=<dir> -p file=auction_1-1.flat .*CekSplitBenchmark.*"
  * }}}
  * The VM is `makePlutusV3VM()`: the mainnet cost model and protocol version, as the harness uses.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CekSplitBenchmark {
    @Param(Array("auction_1-1.flat"))
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var flat: Array[Byte] = null
    private var term: Term = null
    private val vm = PlutusVM.makePlutusV3VM()

    @Setup
    def setup(): Unit = {
        flat = Files.readAllBytes(
          Paths.get(sys.props.getOrElse("bench.data", "src/main/resources/data"), file)
        )
        term = DeBruijnedProgram.fromFlatEncoded(flat).term
    }

    @Benchmark
    def decode(): Term = DeBruijnedProgram.fromFlatEncoded(flat).term

    @Benchmark
    def eval(): Term = vm.evaluateDeBruijnedTerm(term, CountingBudgetSpender(), NoLogger)

    @Benchmark
    def decodeEval(): Term =
        vm.evaluateDeBruijnedTerm(
          DeBruijnedProgram.fromFlatEncoded(flat).term,
          CountingBudgetSpender(),
          NoLogger
        )
}
