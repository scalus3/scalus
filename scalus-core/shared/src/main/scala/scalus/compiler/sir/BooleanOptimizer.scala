package scalus.compiler.sir

import scalus.compiler.sir.SIR.*
import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, SIR, SIRType}
import scalus.uplc.Constant

/** Provides optimization functionality for boolean expressions in SIR. Applies standard boolean
  * algebra transformations to reduce the number of nodes and simplify expressions.
  */
object BooleanOptimizer:

    /** Optimizes boolean expressions in SIR by applying various boolean algebra laws.
      *
      * Implemented optimizations:
      *   - Double negation elimination: Not(Not(a)) => a
      *   - Reverse De Morgan's laws (reduces node count):
      *     - Or(Not(a), Not(b)) => Not(And(a, b))
      *     - And(Not(a), Not(b)) => Not(Or(a, b))
      *   - Negation of conditionals: If(Not(cond), t, f) => If(cond, f, t)
      *   - Constant folding for boolean operations
      *   - Identity and annihilation properties
      *   - Idempotent operations
      *   - Complementation law
      *
      * @param sir
      *   The SIR expression to optimize
      * @return
      *   The optimized SIR expression
      */
    def optimize(sir: SIR): SIR =
        sir match
            case Decl(data, term) =>
                Decl(data, optimize(term))
            case ansir: AnnotatedSIR =>
                optimizeExpr(ansir)

    private def optimizeExpr(sir: AnnotatedSIR): AnnotatedSIR =
        // Recursively optimize the expression
        val optimized: AnnotatedSIR = sir match
            // Double negation elimination
            case Not(Not(a, _), _) =>
                optimizeExpr(a)

            // Correct conditional negation optimization: If(Not(cond), t, f) => If(cond, f, t)
            case IfThenElse(Not(cond, _), t, f, tp, anns) =>
                optimizeExpr(
                  IfThenElse(optimizeExpr(cond), optimizeExpr(f), optimizeExpr(t), tp, anns)
                )

            // Reverse De Morgan: Or(Not(a), Not(b)) => Not(And(a, b))
            case Or(Not(a, _), Not(b, _), anns) =>
                val optA = optimizeExpr(a)
                val optB = optimizeExpr(b)
                optimizeExpr(Not(And(optA, optB, anns), anns))

            // Reverse De Morgan: And(Not(a), Not(b)) => Not(Or(a, b))
            case And(Not(a, _), Not(b, _), anns) =>
                val optA = optimizeExpr(a)
                val optB = optimizeExpr(b)
                optimizeExpr(Not(Or(optA, optB, anns), anns))

            // Recursive optimization of And expressions
            case And(a, b, anns) =>
                val optA = optimizeExpr(a)
                val optB = optimizeExpr(b)
                optimizeAndExpression(optA, optB, anns)

            // Recursive optimization of Or expressions
            case Or(a, b, anns) =>
                val optA = optimizeExpr(a)
                val optB = optimizeExpr(b)
                optimizeOrExpression(optA, optB, anns)

            // Recursive optimization of Not expressions
            case Not(a, anns) =>
                optimizeNotExpression(optimizeExpr(a), anns)

            // Recursive optimization of IfThenElse
            case IfThenElse(cond, t, f, tp, anns) =>
                val optCond = optimizeExpr(cond)
                val optT = optimizeExpr(t)
                val optF = optimizeExpr(f)
                optimizeIfExpression(optCond, optT, optF, tp, anns)

            // Recursive optimization for Apply
            case Apply(f, arg, tp, anns) =>
                Apply(optimizeExpr(f), optimizeExpr(arg), tp, anns)

            // Recursive optimization for Select
            case Select(scrutinee, field, tp, anns) =>
                Select(optimize(scrutinee), field, tp, anns)

            // Recursive optimization for Match
            case Match(scrutinee, cases, tp, anns) =>
                val optScrutinee = optimizeExpr(scrutinee)
                val optCases = cases.map { case Case(pattern, body, anns) =>
                    Case(pattern, optimize(body), anns)
                }
                Match(optScrutinee, optCases, tp, anns)

            // Recursive optimization for Let
            case Let(bindings, body, flags, anns) =>
                // Optimize the body of the let expression
                Let(bindings, optimize(body), flags, anns)

            // Recursive optimization for LamAbs
            case LamAbs(param, term, typeParams, anns) =>
                LamAbs(param, optimize(term), typeParams, anns)

            // Other node types remain unchanged
            case _ => sir

        // Return the most optimized form
        optimized

    /** Optimizes AND expressions by applying boolean algebra laws.
      *
      * @param a
      *   The first operand (already optimized)
      * @param b
      *   The second operand (already optimized)
      * @param anns
      *   Annotations for the AND expression
      * @return
      *   Optimized SIR expression
      */
    private def optimizeAndExpression(
        a: AnnotatedSIR,
        b: AnnotatedSIR,
        anns: AnnotationsDecl
    ): AnnotatedSIR =
        (a, b) match
            // Constant folding
            case (Const(Constant.Bool(false), _, _), _) =>
                Const(Constant.Bool(false), SIRType.Boolean, anns)
            case (_, Const(Constant.Bool(false), _, _)) =>
                Const(Constant.Bool(false), SIRType.Boolean, anns)
            case (Const(Constant.Bool(true), _, _), _) => b
            case (_, Const(Constant.Bool(true), _, _)) => a

            // Idempotent: a && a => a
            case _ if a == b => a

            // Complementation: a && !a => false
            case (_, Not(inner, _)) if a == inner =>
                Const(Constant.Bool(false), SIRType.Boolean, anns)
            case (Not(inner, _), _) if b == inner =>
                Const(Constant.Bool(false), SIRType.Boolean, anns)

            // Default case: cannot optimize further
            case _ => And(a, b, anns)

    /** Optimizes OR expressions by applying boolean algebra laws.
      *
      * @param a
      *   The first operand (already optimized)
      * @param b
      *   The second operand (already optimized)
      * @param anns
      *   Annotations for the OR expression
      * @return
      *   Optimized SIR expression
      */
    private def optimizeOrExpression(
        a: AnnotatedSIR,
        b: AnnotatedSIR,
        anns: AnnotationsDecl
    ): AnnotatedSIR =
        (a, b) match
            // Constant folding
            case (Const(Constant.Bool(true), _, _), _) =>
                Const(Constant.Bool(true), SIRType.Boolean, anns)
            case (_, Const(Constant.Bool(true), _, _)) =>
                Const(Constant.Bool(true), SIRType.Boolean, anns)
            case (Const(Constant.Bool(false), _, _), _) => b
            case (_, Const(Constant.Bool(false), _, _)) => a

            // Idempotent: a || a => a
            case _ if a == b => a

            // Complementation: a || !a => true
            case (_, Not(inner, _)) if a == inner =>
                Const(Constant.Bool(true), SIRType.Boolean, anns)
            case (Not(inner, _), _) if b == inner =>
                Const(Constant.Bool(true), SIRType.Boolean, anns)

            // Default case: cannot optimize further
            case _ => Or(a, b, anns)

    /** Optimizes NOT expressions by applying boolean algebra laws.
      *
      * @param a
      *   The operand (already optimized)
      * @param anns
      *   Annotations for the NOT expression
      * @return
      *   Optimized SIR expression
      */
    private def optimizeNotExpression(a: AnnotatedSIR, anns: AnnotationsDecl): AnnotatedSIR =
        a match
            // Constant folding
            case Const(Constant.Bool(b), _, _) => Const(Constant.Bool(!b), SIRType.Boolean, anns)

            // Double negation already handled in main optimize function

            // Default case: keep the Not
            case _ => Not(a, anns)

    /** Optimizes IF expressions by applying boolean algebra laws.
      *
      * @param cond
      *   The condition (already optimized)
      * @param t
      *   The "then" branch (already optimized)
      * @param f
      *   The "else" branch (already optimized)
      * @param tp
      *   The type of the expression
      * @param anns
      *   Annotations for the IF expression
      * @return
      *   Optimized SIR expression
      */
    private def optimizeIfExpression(
        cond: AnnotatedSIR,
        t: AnnotatedSIR,
        f: AnnotatedSIR,
        tp: SIRType,
        anns: AnnotationsDecl
    ): AnnotatedSIR =
        cond match
            // Constant condition
            case Const(Constant.Bool(true), _, _)  => t
            case Const(Constant.Bool(false), _, _) => f

            // If both branches are the same, no need for conditional
            case _ if t == f => t

            // Note: If(Not(cond), t, f) => If(cond, f, t) is now handled in the main optimize function

            // Default case: keep the if-then-else
            case _ => IfThenElse(cond, t, f, tp, anns)

/** Additional optimization suggestions that could be implemented:
  *
  *   1. Common subexpression elimination
  *   2. Short-circuit evaluation where possible
  *   3. Absorptive properties: a && (a || b) => a, a || (a && b) => a
  *   4. Distributive property: a && (b || c) => (a && b) || (a && c) (only when it reduces nodes)
  *   5. XOR optimizations if XOR is implemented
  *   6. Truth table simplification for complex expressions
  *   7. Boolean expression canonicalization (e.g., to CNF or DNF form)
  *   8. Propagation of constants through expressions
  *   9. Optimization of common patterns like (a && b) || (a && !b) => a
  *   10. Identify broader forms of De Morgan application: And(a, Not(Or(b, c))) => And(a,
  *       And(Not(b), Not(c)))
  */
