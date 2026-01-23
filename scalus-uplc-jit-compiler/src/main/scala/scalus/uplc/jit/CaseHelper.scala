package scalus.uplc.jit

import scalus.uplc.builtin.{BuiltinPair, Data}
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
                case list: List[Any @unchecked] =>
                    // List case - Cons=0 (head, tail), Nil=1
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 2 then
                        throw new JitEvaluationFailure(
                          s"Case on list requires 1 or 2 branches, but $bc provided"
                        )
                    list match
                        case head :: tail =>
                            // Cons branch (index 0) - apply head and tail
                            val consBranch = cases(0).asInstanceOf[Any => Any]
                            consBranch(head).asInstanceOf[Any => Any](tail)
                        case Nil =>
                            // Nil branch (index 1) - no arguments
                            if bc < 2 then
                                throw new JitEvaluationFailure(
                                  s"Case on list requires 1 or 2 branches, but $bc provided"
                                )
                            cases(1)
                case data: Data =>
                    // Data case - Constr=0, Map=1, List=2, I=3, B=4
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 5 then
                        throw new JitEvaluationFailure(
                          s"Case on data requires 1 to 5 branches, but $bc provided"
                        )
                    data match
                        case Data.Constr(tag, args) =>
                            // Constr branch (index 0) - apply tag (BigInt) and args (List[Data])
                            if bc < 1 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 1 branch for Constr"
                                )
                            val constrBranch = cases(0).asInstanceOf[Any => Any]
                            constrBranch(tag).asInstanceOf[Any => Any](args)
                        case Data.Map(entries) =>
                            // Map branch (index 1) - apply entries as List[BuiltinPair[Data, Data]]
                            if bc < 2 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 2 branches for Map"
                                )
                            val mapBranch = cases(1).asInstanceOf[Any => Any]
                            mapBranch(entries.map(BuiltinPair.apply))
                        case Data.List(elements) =>
                            // List branch (index 2) - apply elements as List[Data]
                            if bc < 3 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 3 branches for List"
                                )
                            val listBranch = cases(2).asInstanceOf[Any => Any]
                            listBranch(elements)
                        case Data.I(integer) =>
                            // I branch (index 3) - apply integer value
                            if bc < 4 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 4 branches for I"
                                )
                            val iBranch = cases(3).asInstanceOf[Any => Any]
                            iBranch(integer)
                        case Data.B(bs) =>
                            // B branch (index 4) - apply bytestring value
                            if bc < 5 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires 5 branches for B"
                                )
                            val bBranch = cases(4).asInstanceOf[Any => Any]
                            bBranch(bs)
                case pair: BuiltinPair[?, ?] =>
                    // Pair case - exactly 1 branch receiving both elements
                    val bc = $branchCountExpr
                    if bc != 1 then
                        throw new JitEvaluationFailure(
                          s"Case on pair requires exactly 1 branch, but $bc provided"
                        )
                    val pairBranch = cases(0).asInstanceOf[Any => Any]
                    pairBranch(pair.fst).asInstanceOf[Any => Any](pair.snd)
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
                case list: List[Any @unchecked] =>
                    // List case - Cons=0 (head, tail), Nil=1
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 2 then
                        throw new JitEvaluationFailure(
                          s"Case on list requires 1 or 2 branches, but $bc provided"
                        )
                    list match
                        case head :: tail =>
                            // Cons branch (index 0) - apply head and tail via continuations
                            Apply(Apply(cases(0), Return(head)), Return(tail))
                        case Nil =>
                            // Nil branch (index 1) - no arguments
                            if bc < 2 then
                                throw new JitEvaluationFailure(
                                  s"Case on list requires 1 or 2 branches, but $bc provided"
                                )
                            cases(1)
                case data: Data =>
                    // Data case - Constr=0, Map=1, List=2, I=3, B=4
                    val bc = $branchCountExpr
                    if bc == 0 || bc > 5 then
                        throw new JitEvaluationFailure(
                          s"Case on data requires 1 to 5 branches, but $bc provided"
                        )
                    data match
                        case Data.Constr(tag, args) =>
                            // Constr branch (index 0) - apply tag (BigInt) and args (List[Data])
                            if bc < 1 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 1 branch for Constr"
                                )
                            Apply(Apply(cases(0), Return(tag)), Return(args))
                        case Data.Map(entries) =>
                            // Map branch (index 1) - apply entries as List[BuiltinPair[Data, Data]]
                            if bc < 2 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 2 branches for Map"
                                )
                            Apply(cases(1), Return(entries.map(BuiltinPair.apply)))
                        case Data.List(elements) =>
                            // List branch (index 2) - apply elements as List[Data]
                            if bc < 3 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 3 branches for List"
                                )
                            Apply(cases(2), Return(elements))
                        case Data.I(integer) =>
                            // I branch (index 3) - apply integer value
                            if bc < 4 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires at least 4 branches for I"
                                )
                            Apply(cases(3), Return(integer))
                        case Data.B(bs) =>
                            // B branch (index 4) - apply bytestring value
                            if bc < 5 then
                                throw new JitEvaluationFailure(
                                  s"Case on data requires 5 branches for B"
                                )
                            Apply(cases(4), Return(bs))
                case pair: BuiltinPair[?, ?] =>
                    // Pair case - exactly 1 branch receiving both elements
                    val bc = $branchCountExpr
                    if bc != 1 then
                        throw new JitEvaluationFailure(
                          s"Case on pair requires exactly 1 branch, but $bc provided"
                        )
                    Apply(Apply(cases(0), Return(pair.fst)), Return(pair.snd))
                case other =>
                    throw new JitEvaluationFailure(
                      s"Non-constructor value in case expression: $other"
                    )
        }
    }
}
