package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
import scalus.uplc.{Constant, NamedDeBruijn, Program, Term}
import scalus.uplc.Term.*

/** CSE must stay fast on large terms. Each threshold is far from both the slow and the fast time.
  * CI runners take up to 16 times as long as an M-series laptop, so each threshold is at least 5
  * times the slowest CI time measured.
  */
class CsePerformanceTest extends AnyFunSuite {

    /** The best of three timed runs, after one warm-up run. */
    private def bestMs(run: () => Unit): Long = {
        run()
        (1 to 3).map { _ =>
            val start = System.nanoTime()
            run()
            (System.nanoTime() - start) / 1_000_000
        }.min
    }

    test("CSE shares the stablecoin use-case script within the time threshold") {
        // 13 KB of Flat and 151 extractions. In Scalus 1.2.0 to 1.5.0, one CSE run took about
        // 14 s on an M-series laptop; now it takes about 20 ms. This guards against the whole
        // slowdown coming back; the two tests below guard its two causes one by one.
        val thresholdMs = 2000
        val bytes = getClass
            .getResourceAsStream("/data/plutus_use_cases/stablecoin_1-5.flat")
            .readAllBytes()
        val program = Program.fromFlatEncoded(bytes)
        val shared = program.copy(term = CommonSubexpressionElimination(program.term))
        // A pass that does nothing is fast too, so also check that sharing happened.
        assert(shared.flatEncoded.length < program.flatEncoded.length)

        val ms = bestMs(() => CommonSubexpressionElimination(program.term))
        info(s"best of 3: $ms ms, threshold $thresholdMs ms")
        assert(ms < thresholdMs, s"CSE took $ms ms, threshold $thresholdMs ms")
    }

    test("CSE extracts independent candidates without a collect pass for each") {
        // lam f. Constr(0, [f 0, f 0, ..., f 999, f 999, Constr(1, [0, 1, ..., 19999])])
        // 1,000 independent calls, each used twice, and 20,000 unique constants. One pass over
        // the 26,000 nodes per extraction took about 7 s on an M-series laptop; now about 35 ms,
        // and up to 554 ms on CI.
        val thresholdMs = 3000
        val f = Var(NamedDeBruijn("f"))
        def int(i: Int): Term = Const(Constant.Integer(i))
        val calls = (0 until 1000).toList.flatMap(i => List.fill(2)(Apply(f, int(i))))
        val unique = Constr(Word64.One, (0 until 20000).toList.map(int))
        val term = LamAbs("f", Constr(Word64.Zero, calls :+ unique))

        val cse = new CommonSubexpressionElimination()
        cse(term)
        assert(cse.logs.size == 1000)

        val ms = bestMs(() => CommonSubexpressionElimination(term))
        info(s"best of 3: $ms ms, threshold $thresholdMs ms")
        assert(ms < thresholdMs, s"CSE took $ms ms, threshold $thresholdMs ms")
    }

    test("CSE keys do not collide on terms that differ only in variable names") {
        // Constr(0, [v0, v0, v1, v1, ..., v19999, v19999]): nothing to share, one pass. When every
        // variable had the same key hash, each lookup compared it with the variables seen so far:
        // that took about 7 s on an M-series laptop; now about 12 ms. With 10,000 variables, CI
        // took up to 71 ms.
        val thresholdMs = 1500
        val uses = (0 until 20000).toList.flatMap(i => List.fill(2)(Var(NamedDeBruijn(s"v$i"))))
        val term = Constr(Word64.Zero, uses)

        val ms = bestMs(() => CommonSubexpressionElimination(term))
        info(s"best of 3: $ms ms, threshold $thresholdMs ms")
        assert(ms < thresholdMs, s"CSE took $ms ms, threshold $thresholdMs ms")
    }
}
