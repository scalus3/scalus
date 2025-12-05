package scalus.uplc.jit

import scalus.uplc.jit.mincont.ContinuationJitRepr
import scalus.uplc.jit.mincont.ContinuationJitRepr.{Apply, Return}

import scala.quoted.*

/** Helper for generating case dispatch code at JIT compile time.
  *
  * Note: In the future, external UPLC attribution could allow static dispatch at JIT compile time,
  * avoiding runtime type checks.
  */
object CaseHelper {

    /** Generate case dispatch code that selects and evaluates the appropriate branch.
      *
      * For Constr (tagged tuple), selects branch by tag and applies constructor args. For builtin
      * types (Integer, Bool, Unit), selects branch by index and returns directly.
      *
      * @param scrutinee
      *   Expression that evaluates to the scrutinee value
      * @param branches
      *   List of branch expressions
      * @return
      *   Expression that selects and processes the appropriate branch
      */
    def genCaseDispatch(
        scrutinee: Expr[Any],
        branches: List[Expr[Any]]
    )(using Quotes): Expr[Any] = {
        val branchCount = branches.size
        val branchCountExpr = Expr(branchCount)

        // Build an IndexedSeq for O(1) branch selection
        val branchesExpr: Expr[IndexedSeq[Any]] = '{
            IndexedSeq(${ Varargs(branches) }*)
        }

        '{
            val cases = $branchesExpr
            $scrutinee match
                case (tag: Long, args: List[Any @unchecked]) =>
                    // Constr case - select branch and apply args
                    if tag < 0 || tag >= $branchCountExpr then
                        throw new JitEvaluationFailure(
                          s"Case expression missing branch for tag: $tag"
                        )
                    args.foldLeft(cases(tag.toInt))((f, a) => f.asInstanceOf[Any => Any].apply(a))
                case i: BigInt =>
                    // Integer case - select branch directly
                    val bc = $branchCountExpr
                    if i < 0 || i >= bc then
                        throw new JitEvaluationFailure(
                          s"Case index $i out of bounds for $bc branches"
                        )
                    cases(i.toInt)
                case b: Boolean =>
                    // Bool case - False=0, True=1
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 2 then
                        throw new JitEvaluationFailure(
                          s"Case on boolean requires 1 or 2 branches, but $bc provided"
                        )
                    val index = if b then 1 else 0
                    if index >= bc then
                        throw new JitEvaluationFailure(
                          s"Case on boolean $b requires ${if b then 2 else 1} branches"
                        )
                    cases(index)
                case _: Unit =>
                    // Unit case - exactly 1 branch
                    val bc = $branchCountExpr
                    if bc != 1 then
                        throw new JitEvaluationFailure(
                          s"Case on unit requires exactly 1 branch, but $bc provided"
                        )
                    cases(0)
                case other =>
                    throw new JitEvaluationFailure(
                      s"Non-constructor value in case expression: $other"
                    )
        }
    }

    /** Generate case dispatch code for mincont JIT (continuation-based).
      *
      * Similar to genCaseDispatch but works with ContinuationJitRepr for stack-safe evaluation.
      *
      * @param scrutineeCont
      *   Continuation expression that evaluates to the scrutinee value
      * @param branches
      *   List of branch continuation expressions
      * @return
      *   Continuation expression that selects and processes the appropriate branch
      */
    def genCaseDispatchCont(
        scrutineeCont: Expr[ContinuationJitRepr],
        branches: List[Expr[ContinuationJitRepr]]
    )(using Quotes): Expr[ContinuationJitRepr] = {
        val branchCount = branches.size
        val branchCountExpr = Expr(branchCount)

        // Build an IndexedSeq for O(1) branch selection
        val branchesExpr: Expr[IndexedSeq[ContinuationJitRepr]] = '{
            IndexedSeq(${ Varargs(branches) }*)
        }

        '{
            val cases = $branchesExpr
            val scrutinee = ContinuationJitRepr.eval($scrutineeCont)
            scrutinee match
                case (tag: Long, args: List[Any @unchecked]) =>
                    // Constr case - select branch and apply args via continuations
                    if tag < 0 || tag >= $branchCountExpr then
                        throw new JitEvaluationFailure(
                          s"Case expression missing branch for tag: $tag"
                        )
                    args.foldLeft[ContinuationJitRepr](cases(tag.toInt))((f, a) =>
                        Apply(f, Return(a))
                    )
                case i: BigInt =>
                    // Integer case - select branch directly
                    val bc = $branchCountExpr
                    if i < 0 || i >= bc then
                        throw new JitEvaluationFailure(
                          s"Case index $i out of bounds for $bc branches"
                        )
                    cases(i.toInt)
                case b: Boolean =>
                    // Bool case - False=0, True=1
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 2 then
                        throw new JitEvaluationFailure(
                          s"Case on boolean requires 1 or 2 branches, but $bc provided"
                        )
                    val index = if b then 1 else 0
                    if index >= bc then
                        throw new JitEvaluationFailure(
                          s"Case on boolean $b requires ${if b then 2 else 1} branches"
                        )
                    cases(index)
                case _: Unit =>
                    // Unit case - exactly 1 branch
                    val bc = $branchCountExpr
                    if bc != 1 then
                        throw new JitEvaluationFailure(
                          s"Case on unit requires exactly 1 branch, but $bc provided"
                        )
                    cases(0)
                case other =>
                    throw new JitEvaluationFailure(
                      s"Non-constructor value in case expression: $other"
                    )
        }
    }
}
