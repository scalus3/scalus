package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Program

/** CSE must stay fast on a large real script.
  *
  * The stablecoin_1-5 use case (13 KB of Flat) needs 151 extractions. In Scalus 1.2.0 to 1.5.0, one
  * CSE run on it took about 14 s on an M-series laptop; now it takes about 0.2 s. The threshold is
  * far from both times, so a slow CI machine does not make the test flaky.
  */
class CsePerformanceTest extends AnyFunSuite {

    private val ThresholdMs = 2000

    test("CSE shares the stablecoin use-case script within the time threshold") {
        val bytes = getClass
            .getResourceAsStream("/data/plutus_use_cases/stablecoin_1-5.flat")
            .readAllBytes()
        val program = Program.fromFlatEncoded(bytes)

        def run(): (Program, Long) = {
            val start = System.nanoTime()
            val shared =
                program.copy(term = new CommonSubexpressionElimination().apply(program.term))
            (shared, (System.nanoTime() - start) / 1_000_000)
        }

        val (shared, _) = run() // JIT warm-up
        val bestMs = (1 to 3).map(_ => run()._2).min

        // A pass that does nothing is fast too, so also check that sharing happened.
        assert(shared.flatEncoded.length < program.flatEncoded.length)
        assert(bestMs < ThresholdMs, s"CSE took $bestMs ms, threshold $ThresholdMs ms")
    }
}
