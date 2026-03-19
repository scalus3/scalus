package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.Benchmark
import scalus.uplc.DeBruijnedProgram
import scalus.uplc.eval.*

import scala.util.Using

/** Regression tests for JIT compilation of Plutus use-case benchmarks from
  * https://saib-inc.github.io/cardano-plutus-vm-benchmark/
  *
  * JIT compilation is slow (~20s per script), so we test only previously-failing scripts.
  *
  * Fixed issues:
  *   1. Case on Bool (UPLC 1.1.0 / CIP-0137): Scala 3 erasure error with Boolean/Tuple2 — Fixed by
  *      checking isInstanceOf[Boolean] before pattern match in CaseHelper
  *   2. Missing unapplied builtins: AppendByteString, Blake2b_256, QuotientInteger, DivideInteger —
  *      Fixed by adding unapplied forms to BuiltinEmitter/BuiltinSnippets
  *   3. DivideInteger used truncated division (toward 0) instead of floor division (toward -∞) —
  *      Fixed in BuiltinSnippets and BuiltinAppliedGenerator
  *   4. JIT runtime exceptions (e.g. ArithmeticException) not wrapped as MachineError — Fixed by
  *      catching NonFatal in HybridJIT execution boundary
  */
class JITPlutusUseCasesBenchmarkTest extends AnyFunSuite {

    private val params = MachineParams.defaultPlutusV2PostConwayParams

    private def loadScript(name: String): DeBruijnedProgram = {
        val stream =
            getClass.getClassLoader.getResourceAsStream(s"data/plutus_use_cases/$name.flat")
        assert(stream != null, s"Resource data/plutus_use_cases/$name.flat not found")
        val bytes = Using.resource(stream)(_.readAllBytes())
        DeBruijnedProgram.fromFlatEncoded(bytes)
    }

    private def jitCompileAndRun(name: String): Any = {
        val program = loadScript(name)
        val jitted = hybrid.HybridJIT.jitUplc(program.toProgram.term)
        jitted(NoLogger, NoBudgetSpender, params)
    }

    // Smoke test
    test("HybridJIT compiles and runs auction_1-1") {
        val result = jitCompileAndRun("auction_1-1")
        assert(result != null)
    }

    // Previously failed with missing unapplied QuotientInteger/DivideInteger
    for name <- List(
          "stablecoin_1-1",
          "stablecoin_1-3",
          "stablecoin_1-5",
          "stablecoin_2-1",
          "stablecoin_2-3",
          "uniswap-3",
          "uniswap-5"
        )
    do {
        test(s"HybridJIT compiles and runs $name (was: missing unapplied builtin)".taggedAs(Benchmark)) {
            val result = jitCompileAndRun(name)
            assert(result != null)
        }
    }

    // Previously failed with Case-on-Bool erasure error and/or missing builtins
    for name <- List(
          "coop-1",
          "coop-2",
          "coop-3",
          "coop-4",
          "coop-5",
          "coop-6",
          "coop-7",
          "guardrail-sorted-large",
          "guardrail-sorted-small",
          "guardrail-unsorted-large",
          "guardrail-unsorted-small"
        )
    do {
        test(s"HybridJIT compiles and runs $name (was: Case-on-Bool / missing builtin)".taggedAs(Benchmark)) {
            val result = jitCompileAndRun(name)
            assert(result != null)
        }
    }
}
