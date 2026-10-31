package scalus.uplc

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalus.cardano.ledger.Word64
import scalus.uplc.eval.NoLogger
import scalus.uplc.Term.*
import scalus.uplc.internal.TermTraversal
import scalus.uplc.internal.TermTraversal.foldLeft
import scalus.uplc.transform.{CaseConstrApply, V3Optimizer}

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** The performance gate of the Term traversal refactoring: hand-written walks against
  * [[TermTraversal]] on the Plutus `validation` benchmark programs.
  *
  * Spec: docs/superpowers/specs/2026-10-27-term-traversal-design.md, rules `[GT-4]` to `[GT-9]`.
  * Each `old*` method runs a copy of the code as it was before the migration `[GT-6]`, so one JMH
  * run compares both. `optimizerV3` runs the real pipeline: its "old" number is the run on master,
  * recorded in the commit that adds this file.
  *
  * {{{
  * sbtn "bench/Jmh/run -prof gc .*TermTraversalBenchmark.*"
  * }}}
  * Pass rule: the new mean time is inside the error bars of the old mean, or lower `[GT-7]`, and
  * `gc.alloc.rate.norm` is not higher `[GT-8]`.
  */
@State(Scope.Benchmark)
class TermTraversalBenchmark {
    import TermTraversalBenchmark.*

    @Param(Array("auction_1-1.flat", "future-increase-margin-5.flat", "stablecoin_1-5.flat"))
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var term: Term = null

    @Setup
    def readProgram(): Unit = {
        val deBruijned = DeBruijnedProgram.fromFlatEncoded(BenchData.read(file)).term
        term = DeBruijn.fromDeBruijnTerm(deBruijned)
        // Benchmark only pairs that agree.
        require(oldFreeVars(term) == TermTraversal.freeVars(term), "freeVars differ")
        require(oldUsesConstrOrCase(term) == term.usesConstrOrCase, "usesConstrOrCase differ")
        require(oldCaseConstrApply(term) == newCaseConstrApply(term), "CaseConstrApply differ")
        require(oldCountNodes(term) == newCountNodes(term), "node counts differ")
    }

    /** The `foldLeft` combinator itself on a full walk, against hand-written recursion. */
    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def countNodesOld(): Int = oldCountNodes(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def countNodesNew(): Int = newCountNodes(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def freeVarsOld(): Set[String] = oldFreeVars(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def freeVarsNew(): Set[String] = TermTraversal.freeVars(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def usesConstrOrCaseOld(): Boolean = oldUsesConstrOrCase(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def usesConstrOrCaseNew(): Boolean = term.usesConstrOrCase

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def caseConstrApplyOld(): Term = oldCaseConstrApply(term)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def caseConstrApplyNew(): Term = newCaseConstrApply(term)
}

/** The full `V3Optimizer` pipeline `[GT-5]`. Only one script: the larger ones take 6 to 12 s per
  * run. There is no old copy; compare with the number recorded on master.
  */
@State(Scope.Benchmark)
class TermTraversalPipelineBenchmark {
    private var term: Term = null

    @Setup
    def readProgram(): Unit = {
        val deBruijned =
            DeBruijnedProgram.fromFlatEncoded(BenchData.read("auction_1-1.flat")).term
        term = DeBruijn.fromDeBruijnTerm(deBruijned)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def optimizerV3(): Term = new V3Optimizer().apply(term)
}

object TermTraversalBenchmark {

    // ---- old: copies of the hand-written code on master 75197c80b [GT-6] ----------------------

    /** `TermAnalysis.freeVars`. */
    def oldFreeVars(term: Term): Set[String] = term match
        case Var(NamedDeBruijn(n, _), _)      => Set(n)
        case LamAbs(n, body, _)               => oldFreeVars(body) - n
        case Apply(f, a, _)                   => oldFreeVars(f) ++ oldFreeVars(a)
        case Force(t, _)                      => oldFreeVars(t)
        case Delay(t, _)                      => oldFreeVars(t)
        case Constr(_, args, _)               => args.flatMap(oldFreeVars).toSet
        case Case(scrut, cases, _)            => oldFreeVars(scrut) ++ cases.flatMap(oldFreeVars)
        case _: Const | _: Builtin | _: Error => Set.empty

    /** `Term.usesConstrOrCase`. The new side calls the production method, so the gate measures the
      * code that runs. These scripts have no `constr` or `case`, so both walk the whole tree.
      */
    def oldUsesConstrOrCase(term: Term): Boolean = term match
        case _: Constr | _: Case                       => true
        case _: Var | _: Const | _: Error | _: Builtin => false
        case LamAbs(_, body, _)                        => oldUsesConstrOrCase(body)
        case Force(body, _)                            => oldUsesConstrOrCase(body)
        case Delay(body, _)                            => oldUsesConstrOrCase(body)
        case Apply(f, arg, _) => oldUsesConstrOrCase(f) || oldUsesConstrOrCase(arg)

    /** `CaseConstrApply.extractPass`, without the logger. */
    def oldCaseConstrApply(term: Term): Term = {
        def go(term: Term): Term = term match
            case _: Apply =>
                term.applyToList match
                    case (f, args) if args.sizeCompare(2) > 0 =>
                        Case(Constr(Word64.Zero, args.map(go)), go(f) :: Nil)
                    case (f, args) =>
                        args.foldLeft(go(f)) { case (acc, arg) => Apply(acc, go(arg)) }
            case LamAbs(name, body, ann) => LamAbs(name, go(body), ann)
            case Force(t, ann)           => Force(go(t), ann)
            case Delay(t, ann)           => Delay(go(t), ann)
            case Constr(tag, args, ann)  => Constr(tag, args.map(arg => go(arg)), ann)
            case Case(scrutinee, cases, ann) =>
                Case(
                  go(scrutinee),
                  cases.map(go),
                  ann
                )
            case _: Var | _: Const | _: Builtin | _: Error => term
        go(term)
    }

    /** A recursive count of all nodes, in the style of the hand-written walks. */
    def oldCountNodes(term: Term): Int = term match
        case LamAbs(_, body, _)    => 1 + oldCountNodes(body)
        case Apply(f, arg, _)      => 1 + oldCountNodes(f) + oldCountNodes(arg)
        case Force(t, _)           => 1 + oldCountNodes(t)
        case Delay(t, _)           => 1 + oldCountNodes(t)
        case Constr(_, args, _)    => 1 + args.map(oldCountNodes).sum
        case Case(scrut, cases, _) => 1 + oldCountNodes(scrut) + cases.map(oldCountNodes).sum
        case _: Var | _: Const | _: Builtin | _: Error => 1

    // ---- new: the same functions on TermTraversal ---------------------------------------------

    /** The migrated `CaseConstrApply` itself, with no logging, like the old copy. */
    def newCaseConstrApply(term: Term): Term = new CaseConstrApply(NoLogger).apply(term)

    def newCountNodes(term: Term): Int = term.foldLeft(0)((n, _) => n + 1)
}
