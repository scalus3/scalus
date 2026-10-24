package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.cardano.ledger.ExUnits
import scalus.uplc.{BenchData, DeBruijnedProgram, Term}

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** The CEK machine on the scripts of plutus-vm-benchmark, with its parameters: a PlutusV3 machine
  * with the mainnet cost model and protocol version, costing on: a restricting spender with a
  * budget too large to run out (`ExUnits.enormous`), as the other VMs of plutus-vm-benchmark use.
  *
  *   - `bench`: flat decode + evaluation, the ranking loop of plutus-vm-benchmark.
  *   - `benchEval`: evaluation of the program decoded in setup, its eval-only loop.
  *   - `decode`: flat decode alone, so a gap to another VM can be assigned to decode or to eval.
  *
  * The method names are those of plutus-vm-benchmark's harness (`pvb.scalus.CekJVMBenchmark`), so
  * the results of both line up. Scripts: [[BenchData]].
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CekJVMBenchmark {
    @Param(Array("auction_1-1.flat", "auction_1-2.flat", "auction_1-3.flat", "auction_1-4.flat"))
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var flat: Array[Byte] = null
    private var term: Term = null
    private val vm = PlutusVM.makePlutusV3VM()

    @Setup
    def setup(): Unit = {
        flat = BenchData.read(file)
        term = DeBruijnedProgram.fromFlatEncoded(flat).term
    }

    @Benchmark
    def bench(): Term =
        vm.evaluateDeBruijnedTerm(
          DeBruijnedProgram.fromFlatEncoded(flat).term,
          RestrictingBudgetSpender(ExUnits.enormous),
          NoLogger
        )

    @Benchmark
    def benchEval(): Term =
        vm.evaluateDeBruijnedTerm(term, RestrictingBudgetSpender(ExUnits.enormous), NoLogger)

    @Benchmark
    def decode(): Term = DeBruijnedProgram.fromFlatEncoded(flat).term
}
