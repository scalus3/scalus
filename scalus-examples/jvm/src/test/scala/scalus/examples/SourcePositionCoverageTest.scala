package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.Term
import scalus.uplc.Term.*

/** End-to-end guard for source-position coverage of compiled UPLC. The SIR carries positions only
  * on some nodes and the UPLC optimizer rebuilds spines without them, so without the final
  * position-fill pass (see `CompiledPlutus.toUplc`) the majority of evaluated nodes — and thus of
  * the execution budget — would have no source location and be invisible to
  * profiling/source-traces.
  */
class SourcePositionCoverageTest extends AnyFunSuite {

    private given scalus.compiler.Options = scalus.compiler.Options.default.copy(noWarn = true)

    private def countNodes(t: Term): (Int, Int) = {
        var located = 0
        var empty = 0
        def walk(t: Term): Unit = {
            if t.annotation.isEmpty then empty += 1 else located += 1
            t match
                case Apply(f, a, _)     => walk(f); walk(a)
                case LamAbs(_, b, _)    => walk(b)
                case Force(b, _)        => walk(b)
                case Delay(b, _)        => walk(b)
                case Constr(_, args, _) => args.foreach(walk)
                case Case(s, cs, _)     => walk(s); cs.foreach(walk)
                case _                  => ()
        }
        walk(t)
        (located, empty)
    }

    test("compiled HelloCardano has source positions on (almost) all UPLC nodes") {
        val term = HelloCardanoContract.compiled.withErrorTraces.program.term
        val (located, empty) = countNodes(term)
        val total = located + empty
        // Only the outermost program wrapper (with no positioned descendant) may stay empty.
        assert(located.toDouble / total > 0.9, s"only $located/$total nodes located ($empty empty)")
    }
}
