package scalus.uplc.jit

import scalus.cardano.ledger.{Language, MajorProtocolVersion, Word64}
import scalus.uplc.eval.*
import scalus.uplc.jit.nativestack.StackThresholdException
import scalus.uplc.{BuiltinSemanticsVariant, Constant, DeBruijn, Term}

/** Evaluates a UPLC term on the calling thread, as the CEK machine of `vm` does: the same result
  * term, the same exception and the same spent budget.
  *
  * The budget spender and the logger are by-name parameters: an evaluator evaluates them once per
  * run, and [[UplcCompiler.Compiled.withFallback]] once more for its CEK run.
  */
trait UplcEvaluator {

    /** Evaluates the term and returns its result term, as `vm.evaluateDeBruijnedTerm` does. */
    def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Term

    /** Evaluates the term for its budget and logs only, and discards the result ([EV-14] to
      * [EV-17]).
      *
      * It spends and logs what [[evaluate]] spends and logs, and throws what it throws, with one
      * exception: a result with no term, such as a function, returns normally here, as it does on
      * the CEK machine. Use it to measure or estimate the cost of a script.
      */
    def run(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Unit

    /** This evaluator, applied to `args`: an evaluator of `[[term (con a1)] .. (con an)]` that
      * reuses the compiled code ([EV-18] to [EV-24]). A validator compiles once, unapplied, and
      * runs with the script context of each transaction:
      * {{{
      * val validator = UplcCompiler.compile(program.term).withFallback()
      * validator.applyTo(Constant.Data(scriptContext)).evaluateScript(spender, logger)
      * }}}
      *
      * It returns and throws what the CEK machine does on the applied term, with the same budget
      * per category. One difference ([EV-20]): it spends the Apply steps of the arguments before
      * the startup cost, not after. A budget below `startup + n · applyCost` therefore fails on
      * another step than on the CEK machine, with the same spent budget. The calls that apply the
      * arguments do not count toward `maxDepth` ([EV-24]).
      */
    def applyTo(args: Constant*): UplcEvaluator

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
  * Scope: builtin semantics variant E at protocol version 11 or 12 (spec [SC-1], [SC-2]). A compile
  * generates code for one protocol version, 11 by default ([SC-3], [EV-12]); every evaluation
  * checks that its `vm` is a variant-E machine of that version ([EV-1], [EV-2]).
  *
  * {{{
  * given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
  * val evaluator = UplcCompiler.compile(term).withFallback()   // a staging compile; maxDepth 25 000
  * // for Dijkstra: UplcCompiler.compile(term, protocolVersion = MajorProtocolVersion.dijkstraPV),
  * // evaluated with a vm of PV12
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
      * `protocolVersion` is the machine the code is generated for: van Rossem (11, the default) or
      * Dijkstra (12); any other throws `IllegalArgumentException` ([SC-2]). The two differ only in
      * `case` on `Data`, which code for 11 rejects and code for 12 performs ([SC-3]). The default
      * is 11 because mainnet runs it and `PlutusVM` takes it as its own default. The result
      * evaluates only with a `vm` of the same protocol version ([EV-2]).
      *
      * Needs no machine (no `PlutusVM`, [EV-11]): the compiled code reads the step and builtin
      * costs from the machine parameters of each evaluation. Safe to call from any thread: the
      * staging compiles run one at a time on a dedicated thread ([[StagingCompilerThread]]).
      */
    def compile(
        term: Term,
        maxDepth: Int = nativestack.NativeStackContext.DefaultMaxDepth,
        protocolVersion: MajorProtocolVersion = MajorProtocolVersion.vanRossemPV
    ): Compiled =
        Compiled(term, nativestack.JIT.jitUplc(term, maxDepth, protocolVersion), protocolVersion)

    /** A compiled term: evaluates it with the compiled code only. Safe to evaluate from many
      * threads at once.
      *
      * @param code
      *   the compiled code itself, without the scope check: for benchmarks and tests that pass
      *   their own spender, logger and machine parameters
      * @param protocolVersion
      *   the protocol version the code was generated for: every evaluation needs a `vm` of it
      */
    final class Compiled private[UplcCompiler] (
        term: Term,
        val code: CompiledTerm,
        val protocolVersion: MajorProtocolVersion
    ) extends UplcEvaluator {

        /** Evaluates with the compiled code; `budgetSpender` and `logger` are evaluated once. A
          * script deeper than `maxDepth` throws `StackThresholdException` ([[withFallback]]
          * evaluates it with the CEK machine instead). A function result throws
          * [[FunctionResultError]]. A `vm` of another protocol version than [[protocolVersion]]
          * throws `IllegalArgumentException` before anything runs ([EV-2]).
          */
        def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Term = resultTerm(value(Nil, budgetSpender, logger))

        def run(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Unit = value(Nil, budgetSpender, logger): Unit

        def applyTo(args: Constant*): UplcEvaluator = Applied(this, args.toList)

        /** This evaluator, with a fallback: a script that reaches the call-depth limit is evaluated
          * again, from the start, with the CEK machine of the `vm` of the evaluation, and with
          * `budgetSpender` and `logger` evaluated a second time ([EV-4], [EV-5]). A spent budget
          * and written logs cannot be undone, so the CEK run needs new ones: the spender and the
          * logger of the last run hold the budget and the logs of the result.
          */
        def withFallback(): UplcEvaluator = CekFallback(this, Nil)

        /** The term for the CEK machine, made on the first fallback. One per compile: the fallbacks
          * of all the applied evaluators share it.
          */
        private[UplcCompiler] lazy val deBruijned: Term = DeBruijn.deBruijnTerm(term)

        /** The value of the compiled term applied to `args`, with the CEK machine's budget per
          * category. `budgetSpender` and `logger` are evaluated once, after the check of `vm`
          * ([EV-1], [EV-2], [EV-22]).
          *
          * The CEK machine spends on `[[term a1] a2]`: Startup, Apply, Apply, the steps of `term`,
          * Const, the steps of the call with `a1`, Const, the steps of the call with `a2`. The
          * compiled code spends Startup and the steps of `term` itself, so the Apply steps can only
          * go before it ([EV-20]). The calls happen outside the code and its call-depth counter
          * ([EV-24]).
          */
        private[UplcCompiler] def value(
            args: List[Constant],
            budgetSpender: => BudgetSpender,
            logger: => Logger
        )(using vm: PlutusVM): UplcValue = {
            checkScope(vm, protocolVersion)
            val spender = budgetSpender
            val params = vm.machineParams
            args.foreach(_ => JitBudget.spendApply(spender, params))
            args.foldLeft(code(logger, spender, params)) { (fun, arg) =>
                JitBudget.spendConst(spender, params)
                fun match
                    case l: LamValue => l.apply(arg)
                    case other       => JitRuntime.cannotApply(other)
            }
        }
    }

    /** [[Compiled]] applied to `args`, in order (see [[UplcEvaluator.applyTo]]). */
    private final class Applied(compiled: Compiled, args: List[Constant]) extends UplcEvaluator {

        def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Term = resultTerm(compiled.value(args, budgetSpender, logger))

        def run(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Unit =
            compiled.value(args, budgetSpender, logger): Unit

        // [EV-21]: applyTo(a).applyTo(b) is applyTo(a, b)
        def applyTo(more: Constant*): UplcEvaluator = Applied(compiled, args ++ more)
    }

    /** [[Compiled]], applied to `args`, with the CEK machine as fallback on the call-depth limit
      * (see [[Compiled.withFallback]]); `args` is empty until [[applyTo]].
      */
    private final class CekFallback(compiled: Compiled, args: List[Constant])
        extends UplcEvaluator {

        def evaluate(budgetSpender: => BudgetSpender, logger: => Logger)(using
            vm: PlutusVM
        ): Term =
            // compiled.value checks `vm` first ([EV-1], [EV-2]): a vm of another protocol
            // version fails there, with IllegalArgumentException, and never reaches the CEK
            // machine below. That is deliberate ([EV-13]): the fallback stands in for the compiled
            // code, so it accepts exactly the machines the compiled code accepts.
            try resultTerm(compiled.value(args, budgetSpender, logger))
            catch
                case _: StackThresholdException =>
                    vm.evaluateDeBruijnedTerm(cekTerm, budgetSpender, logger)

        def run(budgetSpender: => BudgetSpender, logger: => Logger)(using vm: PlutusVM): Unit =
            // as evaluate above: compiled.value checks `vm` first ([EV-14])
            try compiled.value(args, budgetSpender, logger): Unit
            catch
                case _: StackThresholdException =>
                    vm.runDeBruijnedTerm(cekTerm, budgetSpender, logger)

        // [EV-21], [EV-23]: the applied evaluator falls back to the CEK machine on all the args
        def applyTo(more: Constant*): UplcEvaluator = CekFallback(compiled, args ++ more)

        /** `[[term (con a1)] .. (con an)]`, for the CEK machine. A constant has no variables, so
          * the De Bruijn indices of `term` stay valid.
          */
        private def cekTerm: Term =
            args.foldLeft(compiled.deBruijned)((fun, arg) => Term.Apply(fun, Term.Const(arg)))
    }

    /** The result term of `value`, or [[FunctionResultError]] for a value with no term ([EV-8]). */
    private def resultTerm(value: UplcValue): Term =
        toTerm(value).getOrElse(throw new FunctionResultError(JitRuntime.describe(value)))

    /** Rejects a machine other than the one the code was compiled for ([EV-1], [EV-2]): PlutusV3 or
      * V4, builtin semantics variant E, and the protocol version `compiledFor` of the compile. Code
      * for PV11 rejects `case` on `Data` and code for PV12 performs it, so running either with a
      * machine of the other version would give the result of the wrong machine.
      */
    private def checkScope(vm: PlutusVM, compiledFor: MajorProtocolVersion): Unit = {
        require(
          vm.language == Language.PlutusV3 || vm.language == Language.PlutusV4,
          s"the JIT implements PlutusV3 and V4, not ${vm.language}"
        )
        require(
          vm.semanticVariant == BuiltinSemanticsVariant.E,
          s"the JIT implements builtin semantics variant E, not ${vm.semanticVariant}"
        )
        require(
          vm.protocolVersion == compiledFor,
          s"the script was JIT-compiled for protocol version ${compiledFor.version}, " +
              s"but the vm has protocol version ${vm.protocolVersion.version}"
        )
    }

    /** The result term of a JIT value, as the CEK machine discharges its result ([EV-7]): a
      * constant becomes `Term.Const`, a `ConstrValue` `Term.Constr` of its fields (`Cek.scala`
      * `dischargeCekValue`). `None` for a value with a function in it.
      */
    private def toTerm(v: UplcValue): Option[Term] = v match
        case c: Constant => Some(Term.Const(c))
        case ConstrValue(tag, as) =>
            val fields = as.map(toTerm)
            if fields.forall(_.isDefined) then Some(Term.Constr(Word64(tag), fields.map(_.get)))
            else None
        case _ => None
}
