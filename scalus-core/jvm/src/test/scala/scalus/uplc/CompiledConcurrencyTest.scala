package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{fail as onchainFail, List}
import scalus.compiler.Options
import scalus.uplc.builtin.Data
import scalus.uplc.transform.{Optimizer, V1V2Optimizer, V3Optimizer}

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import scala.util.Try

/** Copies of one compiled script (`withErrorTraces`, `withOptions`) share the original's optimizer.
  * Lowering several copies on different threads at once must still work.
  */
class CompiledConcurrencyTest extends AnyFunSuite {
    private val threads = 8
    private val rounds = 100

    /** Runs `rounds` rounds of `threads` tasks that start together, and returns every result. */
    private def runConcurrently[A](task: Int => A): Seq[A] = {
        val pool = Executors.newFixedThreadPool(threads)
        try
            (1 to rounds).flatMap { _ =>
                val start = new CountDownLatch(1)
                val futures = (0 until threads).map { i =>
                    pool.submit { () =>
                        start.await()
                        Try(task(i))
                    }
                }
                start.countDown()
                futures.map(_.get(60, TimeUnit.SECONDS).get)
            }
        finally pool.shutdownNow()
    }

    private val compiled = PlutusV3.compile((d: Data) => {
        val xs = d.to[List[BigInt]]
        val sum = xs.foldLeft(BigInt(0))((acc, x) => acc + x)
        val count = xs.foldLeft(BigInt(0))((acc, _) => acc + 1)
        if sum > count * 2 then sum
        else onchainFail("sum is too small")
    })(using Options.release)

    test("copies of one PlutusV3 script lower concurrently") {
        // Reference results and logs, lowered on one thread from fresh copies.
        val expectedTraced = compiled.withErrorTraces.program
        val tracedLogs = compiled.optimizer.logs
        val expectedPlain = compiled.withOptions(compiled.options).program
        val plainLogs = compiled.optimizer.logs
        assert(tracedLogs.nonEmpty && plainLogs.nonEmpty, "the optimizer must run and log")

        // Each task makes its own copy, so each forces its own lazy `program` with the shared
        // optimizer.
        val results = runConcurrently { i =>
            if i % 2 == 0 then compiled.withErrorTraces.program == expectedTraced
            else compiled.withOptions(compiled.options).program == expectedPlain
        }
        assert(results.forall(identity), "a concurrently lowered program differs")
        // The copies share `compiled.optimizer`; its logs must be those of one whole run.
        val logs = compiled.optimizer.logs
        assert(logs == tracedLogs || logs == plainLogs, "the logs mix several runs")
    }

    private def checkSharedOptimizer(optimizer: Optimizer, fresh: () => Optimizer, term: Term) = {
        val reference = fresh()
        val expected = reference(term)
        val expectedLogs = reference.logs
        assert(expectedLogs.nonEmpty, "the term must give the optimizer some work")

        val results = runConcurrently(_ => optimizer(term))
        assert(results.forall(_ == expected), "a concurrent run gave a different term")
        // Every run optimizes the same term, so the logs of the last run are the reference logs.
        assert(optimizer.logs == expectedLogs)
    }

    test("one V3Optimizer instance is safe to share between threads") {
        val unoptimized = compiled.withOptions(Options.release.copy(optimizeUplc = false))
        checkSharedOptimizer(new V3Optimizer(), () => new V3Optimizer(), unoptimized.program.term)
    }

    test("one V1V2Optimizer instance is safe to share between threads") {
        val unoptimized =
            PlutusV2.compile((d: Data) => {
                val xs = d.to[List[BigInt]]
                val sum = xs.foldLeft(BigInt(0))((acc, x) => acc + x)
                val count = xs.foldLeft(BigInt(0))((acc, _) => acc + 1)
                if sum > count * 2 then sum
                else onchainFail("sum is too small")
            })(using Options.release.copy(optimizeUplc = false))
        checkSharedOptimizer(
          new V1V2Optimizer(),
          () => new V1V2Optimizer(),
          unoptimized.program.term
        )
    }
}
