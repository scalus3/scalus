package scalus.uplc.jit

import scalus.cardano.ledger.ExUnits
import scalus.uplc.eval.*
import scalus.uplc.{DeBruijn, Term}

import scala.util.Try

/** Runs one term with the CEK machine and with the JIT, and compares the two.
  *
  * Shared by [[JitShapeTest]], [[JitConformanceTest]] and [[JitVsCekCheck]], so that all of them
  * decide "same result, same budget" in one way.
  */
object JitCekDiff {

    /** What one evaluation produced: its result, the budget per category, and the total. */
    final case class Run[A](
        result: Try[A],
        costs: Map[ExBudgetCategory, ExUnits],
        total: ExUnits
    )

    /** Evaluates `term` with the CEK machine of `vm`. */
    def runCek(vm: PlutusVM, term: Term): Run[Term] = {
        val spender = TallyingBudgetSpender(CountingBudgetSpender())
        val result =
            Try(vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term), spender, NoLogger))
        Run(result, spender.costs.toMap, spender.getSpentBudget)
    }

    /** Evaluates the compiled term with `params`. A compile failure is the caller's to report. */
    def runJit(compiled: CompiledTerm, params: MachineParams): Run[UplcValue] = {
        val spender = TallyingBudgetSpender(CountingBudgetSpender())
        val result = Try(compiled(NoLogger, spender, params))
        Run(result, spender.costs.toMap, spender.getSpentBudget)
    }

    /** Whether the CEK run and the JIT run agree: both fail with the same exception
      * ([[sameFailure]]), or both succeed with the same value ([[matches]]); and both spend the
      * same budget, per category, also when they fail.
      */
    def same(cek: Run[Term], jit: Run[UplcValue]): Boolean =
        cek.result.isSuccess == jit.result.isSuccess &&
            cek.result.toOption.forall(t => jit.result.toOption.exists(matches(t, _))) &&
            (cek.result.failed.toOption zip jit.result.failed.toOption).forall(sameFailure) &&
            cek.costs == jit.costs && cek.total == jit.total

    /** Whether the JIT failed as the CEK machine did: the same exception class, and for a
      * `BuiltinError` (a builtin that failed) also the same class of its cause.
      */
    def sameFailure(cek: Throwable, jit: Throwable): Boolean =
        cek.getClass == jit.getClass && ((cek, jit) match
            case (c: BuiltinError, j: BuiltinError) =>
                c.builtin == j.builtin && c.cause.getClass == j.cause.getClass
            case _ => true)

    /** Whether the JIT value `actual` is the value of the result term `expected`.
      *
      * A constant compares by value, a `constr` by tag and arguments. Any other term (a `lam`, a
      * `delay`, a partly applied builtin) cannot be compared by value: the JIT holds it as a JVM
      * function, so it only checks that `actual` is a function of the same kind.
      */
    def matches(expected: Term, actual: UplcValue): Boolean = expected match
        case Term.Const(c, _) => c == actual
        case Term.Constr(tag, args, _) =>
            actual match
                case ConstrValue(t, as) =>
                    t == tag.value && as.size == args.size &&
                    args.lazyZip(as).forall((e, a) => matches(e, a))
                case _ => false
        case _ => isFunction(actual)

    /** Whether `expected` is a term that [[matches]] compares only by shape (a function). */
    def isFunctionLike(expected: Term): Boolean = expected match
        case Term.Const(_, _)        => false
        case Term.Constr(_, args, _) => args.exists(isFunctionLike)
        case _                       => true

    def isFunction(actual: UplcValue): Boolean = actual match
        case _: LamValue | _: DelayValue => true
        case _                           => false
}
