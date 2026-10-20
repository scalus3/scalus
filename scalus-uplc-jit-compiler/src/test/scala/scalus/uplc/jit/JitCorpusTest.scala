package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.eval.{CountingBudgetSpender, NoLogger}

import scala.util.Try

/** The corpus ([[JitCorpus]]) is what the spec says it is ([MS-1] to [MS-3]), and the CEK machine
  * of the JIT's scope evaluates every script: a script that fails on the CEK machine would compare
  * only failures, which checks little.
  */
class JitCorpusTest extends AnyFunSuite {

    test("the corpus has the 89 plutus_use_cases scripts and 12 to 20 Scalus validators") {
        assert(JitCorpus.plutusUseCases.size == 89)
        val scalus = JitCorpus.scalus.size
        assert(scalus >= 12 && scalus <= 20, s"$scalus Scalus scripts")
    }

    test("the CEK machine evaluates every corpus script successfully") {
        val failed = JitCorpus.all.filter { s =>
            Try(
              JitCorpus.vm.evaluateDeBruijnedTerm(s.program.term, CountingBudgetSpender(), NoLogger)
            ).isFailure
        }
        assert(failed.isEmpty, s"failed on the CEK machine: ${failed.map(_.name)}")
    }
}
