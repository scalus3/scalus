package scalus.uplc.jit

import scalus.cardano.ledger.{Language, MajorProtocolVersion, Word64}
import scalus.uplc.eval.*
import scalus.uplc.jit.nativestack.StackThresholdException
import scalus.uplc.{BuiltinSemanticsVariant, Constant, DeBruijn, DefaultUni, Term}

/** Evaluates a UPLC term on the calling thread, as the CEK machine of `vm` does: the same result
  * term, the same exception and the same spent budget.
  *
  * The budget spender and the logger are by-name parameters: an evaluator evaluates them once per
  * run, and [[UplcCompiler.Compiled.withFallback]] once more for its CEK run.
  */
trait UplcEvaluator {

    /** Evaluates the term and returns its result term, as `vm.evaluateDeBruijnedTerm` does. */
    def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Term

    /** [[evaluate]], then the check of `PlutusVM.evaluateScript`: a PlutusV3 or V4 script must
      * return unit, or it fails with `InvalidReturnValue` (spec [EV-9]).
      *
      * A function result is not unit, so it fails with `InvalidReturnValue` too, as on the CEK
      * machine. The term in that error is a placeholder (`error`): the JIT has no term for a
      * function ([[FunctionResultError]]).
      */
    final def evaluateScript(budgetSpender: => BudgetSpender, logger: => Logger)(using
        vm: PlutusVM
    ): Unit = {
        val result =
            try evaluate(budgetSpender, logger)
            catch case _: FunctionResultError => throw new InvalidReturnValue(Term.Error())
        if !vm.isResultValid(result) then throw new InvalidReturnValue(result)
    }
}

/** The result of a JIT evaluation has no term: it is a function or holds one, or it is a list with
  * an element that is not a constant. The CEK machine returns the term of a function, which it
  * keeps with its environment; the compiled code has none (spec [EV-8]). Scripts return unit, a
  * bool or `Data`.
  */
final class FunctionResultError(value: String)
    extends JitEvaluationFailure(s"the result has no term in the JIT: $value")

/** Compiles UPLC terms into JVM code that evaluates them as the CEK machine does.
  *
  * A staged JIT: it compiles a UPLC script to JVM bytecode with Scala 3 staging when the script is
  * loaded, and the JVM's JIT compiles that bytecode to machine code.
  *
  * Scope: builtin semantics variant E at protocol version 11 (spec [SC-1], [SC-2]). Every
  * evaluation checks that its `vm` is such a machine ([EV-1], [EV-2]).
  *
  * {{{
  * given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
  * val evaluator = UplcCompiler.compile(term).withFallback()   // a staging compile; maxDepth 25 000
  * var spender: RestrictingBudgetSpender = null
  * evaluator.evaluateScript(
  *   { spender = RestrictingBudgetSpender(budget); spender },  // the last one holds the budget
  *   Log()
  * )
  * }}}
  */
object UplcCompiler {

    /** Compiles `term`, a term with named variables (`Program.term`, not De Bruijn indices).
      *
      * `maxDepth` is the call-depth limit of the compiled code: a UPLC application nested that deep
      * throws `StackThresholdException`. The caller chooses it for the stack of its threads; the
      * default, 25 000 ([[nativestack.NativeStackContext.DefaultMaxDepth]]), fits a 64 MB stack. A
      * limit the stack cannot hold lets the JVM stack overflow first, with a `StackOverflowError`,
      * which no evaluator catches.
      *
      * Needs no machine: the compiled code reads the step and builtin costs from the machine
      * parameters of each evaluation. Safe to call from any thread: the staging compiles run one at
      * a time on a dedicated thread ([[StagingCompilerThread]]).
      */
    def compile(
        term: Term,
        maxDepth: Int = nativestack.NativeStackContext.DefaultMaxDepth
    ): Compiled =
        Compiled(term, nativestack.JIT.jitUplc(term, maxDepth))

    /** A compiled term: evaluates it with the compiled code only. Safe to evaluate from many
      * threads at once.
      *
      * @param code
      *   the compiled code itself, without the scope check: for benchmarks and tests that pass
      *   their own spender, logger and machine parameters
      */
    final class Compiled private[UplcCompiler] (term: Term, val code: CompiledTerm)
        extends UplcEvaluator {

        /** Evaluates with the compiled code; `budgetSpender` and `logger` are evaluated once. A
          * script deeper than `maxDepth` throws `StackThresholdException` ([[withFallback]]
          * evaluates it with the CEK machine instead). A function result throws
          * [[FunctionResultError]].
          */
        def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Term = {
            checkScope(vm)
            val value = code(logger, budgetSpender, vm.machineParams)
            toTerm(value).getOrElse(throw new FunctionResultError(JitRuntime.describe(value)))
        }

        /** This evaluator, with a fallback: a script that reaches the call-depth limit is evaluated
          * again, from the start, with the CEK machine of the `vm` of the evaluation, and with
          * `budgetSpender` and `logger` evaluated a second time ([EV-4], [EV-5]). A spent budget
          * and written logs cannot be undone, so the CEK run needs new ones: the spender and the
          * logger of the last run hold the budget and the logs of the result.
          */
        def withFallback(): UplcEvaluator = CekFallback(this, term)
    }

    /** [[Compiled]] with the CEK machine as fallback on the call-depth limit (see
      * [[Compiled.withFallback]]).
      */
    private final class CekFallback(compiled: Compiled, term: Term) extends UplcEvaluator {

        /** The term for the CEK machine, made on the first fallback. */
        private lazy val deBruijned: Term = DeBruijn.deBruijnTerm(term)

        def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Term =
            try compiled.evaluate(budgetSpender, logger)
            catch
                case _: StackThresholdException =>
                    vm.evaluateDeBruijnedTerm(deBruijned, budgetSpender, logger)
    }

    /** Rejects a machine other than the one the JIT implements ([EV-1], [EV-2]): PlutusV3 or V4,
      * builtin semantics variant E, protocol version 11.
      */
    private def checkScope(vm: PlutusVM): Unit = {
        require(
          vm.language == Language.PlutusV3 || vm.language == Language.PlutusV4,
          s"the JIT implements PlutusV3 and V4, not ${vm.language}"
        )
        require(
          vm.semanticVariant == BuiltinSemanticsVariant.E,
          s"the JIT implements builtin semantics variant E, not ${vm.semanticVariant}"
        )
        require(
          vm.protocolVersion == MajorProtocolVersion.vanRossemPV,
          s"the JIT implements protocol version 11, not ${vm.protocolVersion}"
        )
    }

    /** The result term of a JIT value, as the CEK machine discharges its result ([EV-7]): a
      * constant becomes `Term.Const`, a `ConstrValue` `Term.Constr` of its fields, a `ListValue` of
      * constants a list constant of element type `BuiltinValue` (`Cek.scala` `dischargeCekValue`).
      * `None` for a value with a function in it, or a list with a non-constant element.
      */
    private def toTerm(v: UplcValue): Option[Term] = v match
        case c: Constant => Some(Term.Const(c))
        case ConstrValue(tag, as) =>
            val fields = as.map(toTerm)
            if fields.forall(_.isDefined) then Some(Term.Constr(Word64(tag), fields.map(_.get)))
            else None
        case ListValue(elems) =>
            val constants = elems.collect { case c: Constant => c }
            if constants.size == elems.size then
                Some(Term.Const(Constant.List(DefaultUni.BuiltinValue, constants)))
            else None
        case _ => None
}
