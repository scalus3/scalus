package scalus.uplc.transform

import scalus.cardano.ledger.ExUnits
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.eval.*
import scalus.uplc.transform.TermAnalysis.freeVars

/** Compile-time partial evaluator for UPLC terms.
  *
  * Evaluates closed subexpressions (no free variables) at compile time using the CEK machine and
  * replaces them with their result. This covers saturated builtin applications, lambda applications
  * with constant arguments, case/constr elimination on known constructors, and any composition of
  * the above.
  *
  * ==Safety==
  *
  * A budget cap prevents expensive computations from slowing the compiler. Failed evaluations
  * (runtime errors, budget exceeded) leave the original term unchanged, preserving semantics.
  *
  * @see
  *   [[Inliner]] which calls this after inlining and beta-reduction
  */
object PartialEvaluator {

    /** Default budget cap for partial evaluation attempts. */
    val DefaultMaxBudget: ExUnits = ExUnits(memory = 1_000_000, steps = 1_000_000_000)

    private lazy val vm: PlutusVM = PlutusVM.makePlutusV3VM()

    /** Try to partially evaluate a closed term.
      *
      * Returns `Some(result)` if the term is closed, reducible, and CEK evaluation succeeds within
      * budget producing a constant. Returns `None` otherwise.
      */
    def tryEval(term: Term, maxBudget: ExUnits = DefaultMaxBudget): Option[Term] = {
        // Skip terms already in normal form
        if isValue(term) then return None

        // Must be closed (no free variables)
        if term.freeVars.nonEmpty then return None

        // Must contain reducible structure
        if !hasReducibleOp(term) then return None

        // Don't fold expressions containing Trace — it has logging side effects
        if containsTrace(term) then return None

        try
            val debruijned = DeBruijn.deBruijnTerm(term)
            val budgetSpender = new RestrictingBudgetSpender(maxBudget)
            val result = vm.evaluateDeBruijnedTerm(debruijned, budgetSpender, NoLogger)
            result match
                case Const(c, _) if isFlatEncodable(c) => Some(result)
                case _                                 => None
        catch case _: Exception => None // Budget exceeded, runtime error, etc.
    }

    /** Check if a constant can be flat-encoded (needed for script serialization).
      *
      * BLS12-381 group elements and ML results cannot be flat-encoded and must not be folded at
      * compile time.
      */
    private def isFlatEncodable(c: Constant): Boolean = c match
        case _: Constant.BLS12_381_G1_Element | _: Constant.BLS12_381_G2_Element |
            _: Constant.BLS12_381_MlResult =>
            false
        case _ => true

    /** A term is already a value (in normal form). */
    private def isValue(term: Term): Boolean = term match
        case _: Const | _: LamAbs | _: Delay | _: Builtin => true
        case _                                            => false

    /** Check if a term contains at least one reducible operation at the top level.
      *
      * Only recurses into Constr args because Constr is not filtered by `isValue` but can contain
      * reducible subexpressions. Other wrappers (Delay, LamAbs) are already filtered by `isValue`.
      */
    private def hasReducibleOp(term: Term): Boolean = term match
        case _: Apply | _: Force | _: Case => true
        case Constr(_, args, _)            => args.exists(hasReducibleOp)
        case _                             => false

    /** Check if a term contains a Trace builtin anywhere in its tree.
      *
      * Trace has a logging side effect that would be lost during compile-time evaluation (since we
      * use NoLogger), so we must preserve such expressions for runtime.
      */
    private def containsTrace(term: Term): Boolean = term match
        case Builtin(DefaultFun.Trace, _) => true
        case Builtin(_, _)                => false
        case Apply(f, arg, _)             => containsTrace(f) || containsTrace(arg)
        case Force(t, _)                  => containsTrace(t)
        case Delay(t, _)                  => containsTrace(t)
        case LamAbs(_, body, _)           => containsTrace(body)
        case Constr(_, args, _)           => args.exists(containsTrace)
        case Case(scrut, cases, _)        => containsTrace(scrut) || cases.exists(containsTrace)
        case _: Var | _: Const | _: Error => false
}
