package scalus
package sir
package lowering
package simple

import scalus.showShort
import scalus.sir.SIR.Pattern
import scalus.uplc.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Base class for simple lowering from Scalus Intermediate Representation [[SIR]] to UPLC [[Term]].
  *
  * This class contains common functionality shared between different lowering strategies.
  * Subclasses must implement the encoding-specific methods for constructors, pattern matching, and
  * field selection.
  *
  * @param sir
  *   the Scalus Intermediate Representation to lower
  * @param generateErrorTraces
  *   whether to generate error traces
  */
abstract class BaseSimpleLowering(sir: SIR, generateErrorTraces: Boolean = false):

    protected def builtinTerms = Meaning.allBuiltins.forcedBuiltins

    protected var zCombinatorNeeded: Boolean = false
    protected val decls = mutable.HashMap.empty[String, DataDecl]

    /** Lower the SIR to UPLC Term */
    def lower(): Term =
        // Apply let floating to optimize lazy let bindings
        val transformed = LetFloating(sir)
        val term = lowerInner(transformed)
        if zCombinatorNeeded then
            Term.Apply(Term.LamAbs("__z_combinator__", term), ExprBuilder.ZTerm)
        else term

    /** Find all constructors for a given SIR type. Used in Match expressions to determine the
      * complete set of constructors.
      */
    protected def findConstructors(sirType: SIRType): Seq[ConstrDecl] =
        sirType match
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                optParent match
                    case None         => Seq(constrDecl)
                    case Some(parent) => findConstructors(parent)
            case SIRType.SumCaseClass(decl, _) =>
                decl.constructors
            case SIRType.TypeLambda(_, t) => findConstructors(t)
            case SIRType.TypeProxy(ref)   => findConstructors(ref)
            case _ =>
                throw new IllegalArgumentException(
                  s"Expected case class type, got ${sirType}"
                )

    /** Find the constructor declaration for a given SIR type. Used in Select expressions to
      * determine field access.
      */
    @tailrec
    protected final def findConstructorDecl(sirType: SIRType, anns: AnnotationsDecl): ConstrDecl =
        sirType match
            case SIRType.CaseClass(constrDecl, _, _) => constrDecl
            case SIRType.SumCaseClass(decl, _) =>
                if decl.constructors.length == 1 then decl.constructors.head
                else
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Expected case class type, got ${sirType} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
            case SIRType.TypeLambda(_, t) => findConstructorDecl(t, anns)
            case _ =>
                val pos = anns.pos
                throw new IllegalArgumentException(
                  s"Expected case class type, got ${sirType} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )

    /** Expand wildcard patterns to explicit constructor patterns. Returns the expanded list of
      * cases sorted by constructor order.
      */
    protected def expandAndSortCases(
        matchExpr: SIR.Match,
        constructors: Seq[ConstrDecl]
    ): List[SIR.Case] =
        val cases = matchExpr.cases
        val anns = matchExpr.anns
        var idx = 0
        val allConstructors = constructors.toSet
        val matchedConstructors = mutable.HashSet.empty[String]
        val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]
        val isUnchecked = anns.data.contains("unchecked")
        val existsWildcardCase = cases.exists(_.pattern == Pattern.Wildcard)
        val enhancedCases =
            if isUnchecked && cases.length < allConstructors.size && !existsWildcardCase
            then
                cases :+ SIR.Case(
                  Pattern.Wildcard,
                  SIR.Error("Unexpected case", anns),
                  anns
                )
            else cases

        val casesIter = enhancedCases.iterator

        while casesIter.hasNext do
            val currentCase = casesIter.next()
            currentCase match
                case c @ SIR.Case(Pattern.Constr(constrDecl, _, _), _, _) =>
                    matchedConstructors += constrDecl.name
                    expandedCases += c
                case SIR.Case(Pattern.Const(_), _, anns) =>
                    throw new IllegalArgumentException(
                      s"Constant pattern not supported at ${anns.pos.file}:${anns.pos.startLine}, ${anns.pos.startColumn}"
                    )
                case SIR.Case(Pattern.Wildcard, rhs, anns) =>
                    // If we have a wildcard case, it must be the last one
                    if idx != enhancedCases.length - 1 then {
                        println(
                          s"Wildcard case must be the last in match expression at ${anns.pos.file}:${anns.pos.startLine}, ${anns.pos.startColumn}\n" +
                              s"Match expression: ${matchExpr.showShort}\n" +
                              s"Cases: ${enhancedCases.map(_.pattern.show).mkString(", ")}"
                        )
                        throw new IllegalArgumentException(
                          s"Wildcard case must be the last and only one in match expression at ${anns.pos.file}:${anns.pos.startLine}, ${anns.pos.startColumn}"
                        )
                    } else
                        // Convert Wildcard to the rest of the cases/constructors
                        val missingConstructors =
                            allConstructors.filter(c => !matchedConstructors.contains(c.name))
                        missingConstructors.foreach { constrDecl =>
                            val bindings = getWildcardBindings(constrDecl)
                            val typeArgs = constrDecl.typeParams.map(_ => SIRType.FreeUnificator)
                            expandedCases += SIR.Case(
                              Pattern.Constr(constrDecl, bindings, typeArgs),
                              rhs,
                              anns
                            )
                            matchedConstructors += constrDecl.name
                        }
            idx += 1
        end while

        // Sort the cases by the same order as the constructors
        constructors.map { constr =>
            val optExpandedCase = expandedCases.find(_.pattern match {
                case Pattern.Constr(constrDecl, _, _) => constrDecl.name == constr.name
                case _                                => false
            })
            optExpandedCase.getOrElse(
              throw new IllegalArgumentException(
                s"Missing case for constructor ${constr.name} at ${anns.pos.file}: ${anns.pos.startLine}, ${anns.pos.startColumn}"
              )
            )
        }.toList

    /** Get bindings for wildcard pattern. Override in subclasses if needed. SimpleSirToUplcLowering
      * uses actual parameter names. SirToUplc110Lowering uses unused binding names.
      */
    protected def getWildcardBindings(constrDecl: ConstrDecl): List[String] =
        constrDecl.params.map(_.name)

    /** Check if a SIR type is a primitive type that supports constant pattern matching. */
    protected def isPrimitiveType(sirType: SIRType): Boolean = sirType match {
        case SIRType.Boolean    => true
        case SIRType.Integer    => true
        case SIRType.ByteString => true
        case SIRType.String     => true
        case _                  => false
    }

    /** Lower a match expression on primitive types with constant patterns. */
    protected def lowerPrimitiveMatch(matchExpr: SIR.Match): Term = {
        val scrutinee = matchExpr.scrutinee
        val scrutineeTerm = lowerInner(scrutinee)
        val isUnchecked = matchExpr.anns.data.contains("unchecked")

        scrutinee.tp match {
            case SIRType.Boolean =>
                lowerBooleanMatch(scrutineeTerm, matchExpr.cases, isUnchecked, matchExpr.anns)
            case SIRType.Integer =>
                lowerIntegerMatch(scrutineeTerm, matchExpr.cases, isUnchecked, matchExpr.anns)
            case SIRType.ByteString =>
                lowerByteStringMatch(scrutineeTerm, matchExpr.cases, isUnchecked, matchExpr.anns)
            case SIRType.String =>
                lowerStringMatch(scrutineeTerm, matchExpr.cases, isUnchecked, matchExpr.anns)
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported primitive type for constant matching: ${scrutinee.tp}"
                )
        }
    }

    /** Lower Boolean constant pattern match. */
    protected def lowerBooleanMatch(
        scrutineeTerm: Term,
        cases: List[SIR.Case],
        isUnchecked: Boolean,
        anns: AnnotationsDecl
    ): Term = {
        def processCases(cases: List[SIR.Case], matchedValues: Set[Boolean]): Term = cases match {
            case Nil =>
                if isUnchecked then
                    lowerInner(SIR.Error("Non-exhaustive pattern match for Boolean", anns))
                else
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Non-exhaustive pattern match for Boolean at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
            case SIR.Case(SIR.Pattern.Const(constValue), body, caseAnns) :: rest =>
                constValue.uplcConst match {
                    case Constant.Bool(boolValue) =>
                        val newMatched = matchedValues + boolValue
                        val isExhaustive = newMatched.contains(true) && newMatched.contains(false)

                        if rest.isEmpty && isExhaustive then {
                            // Last case and exhaustive - just return the body
                            lowerInner(body)
                        } else {
                            // Generate if-then-else
                            val thenBranch = lowerInner(body)
                            val elseBranch = processCases(rest, newMatched)
                            val ifThenElse = !(builtinTerms(
                              DefaultFun.IfThenElse
                            ) $ scrutineeTerm $ ~thenBranch $ ~elseBranch)
                            // if constValue is true: if scrutinee then body else rest
                            // if constValue is false: if scrutinee then rest else body
                            if boolValue then ifThenElse
                            else
                                !(builtinTerms(
                                  DefaultFun.IfThenElse
                                ) $ scrutineeTerm $ ~elseBranch $ ~thenBranch)
                        }
                    case _ =>
                        val pos = caseAnns.pos
                        throw new IllegalArgumentException(
                          s"Expected Boolean constant, got ${constValue.uplcConst} at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                }
            case SIR.Case(SIR.Pattern.Wildcard, body, caseAnns) :: rest =>
                if rest.nonEmpty then {
                    val pos = caseAnns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard pattern must be the last case at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
                lowerInner(body)
            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, caseAnns) :: _ =>
                val pos = caseAnns.pos
                throw new IllegalArgumentException(
                  s"Constructor pattern not supported for Boolean at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }

        processCases(cases, Set.empty)
    }

    /** Lower Integer constant pattern match. */
    protected def lowerIntegerMatch(
        scrutineeTerm: Term,
        cases: List[SIR.Case],
        isUnchecked: Boolean,
        anns: AnnotationsDecl
    ): Term = {
        def processCases(cases: List[SIR.Case]): Term = cases match {
            case Nil =>
                if isUnchecked then
                    lowerInner(SIR.Error("Non-exhaustive pattern match for Integer", anns))
                else {
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Non-exhaustive pattern match for Integer at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
            case SIR.Case(SIR.Pattern.Const(constValue), body, caseAnns) :: rest =>
                val constTerm = lowerInner(constValue)
                val comparison =
                    !(builtinTerms(DefaultFun.EqualsInteger) $ scrutineeTerm $ constTerm)
                val thenBranch = lowerInner(body)
                val elseBranch = processCases(rest)
                !(builtinTerms(DefaultFun.IfThenElse) $ comparison $ ~thenBranch $ ~elseBranch)
            case SIR.Case(SIR.Pattern.Wildcard, body, caseAnns) :: rest =>
                if rest.nonEmpty then {
                    val pos = caseAnns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard pattern must be the last case at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
                lowerInner(body)
            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, caseAnns) :: _ =>
                val pos = caseAnns.pos
                throw new IllegalArgumentException(
                  s"Constructor pattern not supported for Integer at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }

        processCases(cases)
    }

    /** Lower ByteString constant pattern match. */
    protected def lowerByteStringMatch(
        scrutineeTerm: Term,
        cases: List[SIR.Case],
        isUnchecked: Boolean,
        anns: AnnotationsDecl
    ): Term = {
        def processCases(cases: List[SIR.Case]): Term = cases match {
            case Nil =>
                if isUnchecked then
                    lowerInner(SIR.Error("Non-exhaustive pattern match for ByteString", anns))
                else {
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Non-exhaustive pattern match for ByteString at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
            case SIR.Case(SIR.Pattern.Const(constValue), body, caseAnns) :: rest =>
                val constTerm = lowerInner(constValue)
                val comparison =
                    !(builtinTerms(DefaultFun.EqualsByteString) $ scrutineeTerm $ constTerm)
                val thenBranch = lowerInner(body)
                val elseBranch = processCases(rest)
                !(builtinTerms(DefaultFun.IfThenElse) $ comparison $ ~thenBranch $ ~elseBranch)
            case SIR.Case(SIR.Pattern.Wildcard, body, caseAnns) :: rest =>
                if rest.nonEmpty then {
                    val pos = caseAnns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard pattern must be the last case at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
                lowerInner(body)
            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, caseAnns) :: _ =>
                val pos = caseAnns.pos
                throw new IllegalArgumentException(
                  s"Constructor pattern not supported for ByteString at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }

        processCases(cases)
    }

    /** Lower String constant pattern match. */
    protected def lowerStringMatch(
        scrutineeTerm: Term,
        cases: List[SIR.Case],
        isUnchecked: Boolean,
        anns: AnnotationsDecl
    ): Term = {
        def processCases(cases: List[SIR.Case]): Term = cases match {
            case Nil =>
                if isUnchecked then
                    lowerInner(SIR.Error("Non-exhaustive pattern match for String", anns))
                else {
                    val pos = anns.pos
                    throw new IllegalArgumentException(
                      s"Non-exhaustive pattern match for String at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
            case SIR.Case(SIR.Pattern.Const(constValue), body, caseAnns) :: rest =>
                val constTerm = lowerInner(constValue)
                val comparison =
                    !(builtinTerms(DefaultFun.EqualsString) $ scrutineeTerm $ constTerm)
                val thenBranch = lowerInner(body)
                val elseBranch = processCases(rest)
                !(builtinTerms(DefaultFun.IfThenElse) $ comparison $ ~thenBranch $ ~elseBranch)
            case SIR.Case(SIR.Pattern.Wildcard, body, caseAnns) :: rest =>
                if rest.nonEmpty then {
                    val pos = caseAnns.pos
                    throw new IllegalArgumentException(
                      s"Wildcard pattern must be the last case at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                    )
                }
                lowerInner(body)
            case SIR.Case(SIR.Pattern.Constr(_, _, _), _, caseAnns) :: _ =>
                val pos = caseAnns.pos
                throw new IllegalArgumentException(
                  s"Constructor pattern not supported for String at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }

        processCases(cases)
    }

    /** Lower a constructor expression. Must be implemented by subclasses. */
    protected def lowerConstr(
        name: String,
        data: DataDecl,
        args: List[SIR],
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term

    /** Lower a match expression. Must be implemented by subclasses. */
    protected def lowerMatch(matchExpr: SIR.Match): Term

    /** Lower a field selection expression. Must be implemented by subclasses. */
    protected def lowerSelect(
        scrutinee: SIR,
        field: String,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term

    /** Main lowering function that dispatches to encoding-specific implementations */
    protected def lowerInner(sir: SIR): Term =
        sir match
            case SIR.Decl(data, body) =>
                decls(data.name) = data
                lowerInner(body)
            case SIR.Constr(name, data, args, tp, anns) =>
                lowerConstr(name, data, args, tp, anns)
            case m @ SIR.Match(scrutinee, cases, tp, anns) =>
                lowerMatch(m)
            case SIR.Var(name, _, _)            => Term.Var(NamedDeBruijn(name))
            case SIR.ExternalVar(_, name, _, _) => Term.Var(NamedDeBruijn(name))
            case SIR.Let(bindings, body, flags, anns) if !flags.isRec =>
                val loweredBody = lowerInner(body)
                val letResult = bindings.foldRight(loweredBody) {
                    case (Binding(name, tp, rhs), body) =>
                        Term.Apply(Term.LamAbs(name, body), lowerInner(rhs))
                }
                letResult
            case SIR.Let(Binding(name, tp, rhs) :: Nil, body, flags, _) if flags.isRec =>
                /*  let rec f x = f (x + 1)
                    in f 0
                    (\f -> f 0) (Z (\f. \x. f (x + 1)))
                 */
                zCombinatorNeeded = true
                val fixed =
                    Term.Apply(
                      Term.Var(NamedDeBruijn("__z_combinator__")),
                      Term.LamAbs(name, lowerInner(rhs))
                    )
                Term.Apply(Term.LamAbs(name, lowerInner(body)), fixed)
            case SIR.Let(bindings, body, flags, _) =>
                // TODO: implement mutual recursion
                sys.error(s"Mutually recursive bindings are not supported: $bindings")
            case SIR.LamAbs(name, term, tps, _) => Term.LamAbs(name.name, lowerInner(term))
            case SIR.Apply(f, arg, _, _)        => Term.Apply(lowerInner(f), lowerInner(arg))
            case SIR.Select(scrutinee, field, tp, anns) =>
                lowerSelect(scrutinee, field, tp, anns)
            case SIR.Const(const, _, _) => Term.Const(const)
            case SIR.And(lhs, rhs, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Or(lhs, rhs, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty),
                    rhs,
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Not(term, anns) =>
                lowerInner(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty),
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.IfThenElse(cond, t, f, _, _) =>
                !(builtinTerms(DefaultFun.IfThenElse) $ lowerInner(cond) $ ~lowerInner(
                  t
                ) $ ~lowerInner(f))
            case SIR.Cast(term, tp, anns) =>
                lowerInner(term)
            case SIR.Builtin(bn, _, _) => builtinTerms(bn)
            case SIR.Error(msg, _, _) =>
                if generateErrorTraces
                then !(builtinTerms(DefaultFun.Trace) $ lowerInner(msg) $ ~Term.Error)
                else Term.Error
