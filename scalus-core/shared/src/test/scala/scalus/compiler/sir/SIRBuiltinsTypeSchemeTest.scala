package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.{CaseClass, Fun, Primitive, SumCaseClass, TypeLambda, TypeVar}
import scalus.uplc.{DefaultFun, Meaning}

import scala.util.{Failure, Success, Try}

/** The type of each builtin is written twice: as a SIR type in [[SIRBuiltins]], and as a
  * [[scalus.uplc.TypeScheme]] in the builtin runtime. [[SIRType.fromUplcTypeScheme]] converts the
  * second form to the first, so the two tables can be compared.
  */
class SIRBuiltinsTypeSchemeTest extends AnyFunSuite {

    /** The first difference between two types, or `None` when they are equal up to the names of the
      * bound type variables.
      *
      * The comparison is structural. It does not use `SIRUnify` or `~=~`: they accept any type for
      * a type variable, so they also accept the type of `sndPair` for `fstPair`.
      *
      * Data declarations are compared by name. Their constructors are not visited, because a
      * recursive declaration refers to itself.
      *
      * @param bound
      *   the binders in scope, innermost first: a variable of `left`, with the variable that
      *   `right` binds at the same position
      */
    private def difference(
        left: SIRType,
        right: SIRType,
        bound: List[(TypeVar, TypeVar)] = Nil
    ): Option[String] = {
        def firstDifference(lefts: List[SIRType], rights: List[SIRType]): Option[String] =
            if lefts.length != rights.length then
                Some(s"${lefts.length} type arguments, not ${rights.length}")
            else lefts.zip(rights).flatMap((l, r) => difference(l, r, bound)).headOption

        (left, right) match
            case (TypeLambda(leftParams, leftBody), TypeLambda(rightParams, rightBody)) =>
                if leftParams.length != rightParams.length then
                    Some(s"${leftParams.length} type parameters, not ${rightParams.length}")
                else
                    val params = leftParams.zip(rightParams)
                    params
                        .collectFirst {
                            case (l, r) if l.kind != r.kind =>
                                s"type parameter ${l.show} is ${l.kind}, ${r.show} is ${r.kind}"
                        }
                        .orElse(difference(leftBody, rightBody, params ++ bound))
            case (l: TypeVar, r: TypeVar) =>
                bound.find((boundLeft, boundRight) => boundLeft == l || boundRight == r) match
                    case Some((boundLeft, boundRight)) if boundLeft == l && boundRight == r =>
                        if l.kind == r.kind then None
                        else Some(s"type variable ${l.show} is ${l.kind}, ${r.show} is ${r.kind}")
                    case Some(_) =>
                        Some(s"${l.show} and ${r.show} are bound at different positions")
                    case None => Some(s"${l.show} or ${r.show} is not bound")
            case (Fun(leftIn, leftOut), Fun(rightIn, rightOut)) =>
                difference(leftIn, rightIn, bound).orElse(difference(leftOut, rightOut, bound))
            case (SumCaseClass(leftDecl, leftArgs), SumCaseClass(rightDecl, rightArgs)) =>
                if leftDecl.name != rightDecl.name then
                    Some(s"${leftDecl.name} is not ${rightDecl.name}")
                else firstDifference(leftArgs, rightArgs)
            case (
                  CaseClass(leftConstr, leftArgs, leftParent),
                  CaseClass(rightConstr, rightArgs, rightParent)
                ) =>
                if leftConstr.name != rightConstr.name then
                    Some(s"${leftConstr.name} is not ${rightConstr.name}")
                else
                    firstDifference(leftArgs, rightArgs).orElse {
                        (leftParent, rightParent) match
                            case (None, None)       => None
                            case (Some(l), Some(r)) => difference(l, r, bound)
                            case _ => Some(s"only one ${leftConstr.name} has a parent")
                    }
            case (l: Primitive, r: Primitive) if l == r => None
            case _ => Some(s"${left.show} is not ${right.show}")
    }

    test("the SIR type of each builtin is the conversion of its UPLC type scheme") {
        val differences = DefaultFun.values.toList.flatMap { fn =>
            val sirType = SIRBuiltins.fromUplc(fn).tp
            val scheme = Meaning.allBuiltins.getBuiltinRuntime(fn).typeScheme
            Try(SIRType.fromUplcTypeScheme(scheme)) match
                case Failure(e) => Some(s"$fn: no conversion of $scheme: $e")
                case Success(converted) =>
                    difference(sirType, converted).map { d =>
                        s"$fn: $d\n    SIRBuiltins: ${sirType.show}\n    TypeScheme:  ${converted.show}"
                    }
        }
        if differences.nonEmpty then
            fail(
              s"${differences.size} of ${DefaultFun.values.length} builtins differ:\n" +
                  differences.mkString("\n")
            )
    }

    test("fromUplc returns the builtin that it is asked for") {
        // Two builtins can have one type, so the comparison of the types does not show a wrong row.
        val wrong = DefaultFun.values.toList.filter(fn => SIRBuiltins.fromUplc(fn).bn != fn)
        assert(wrong.isEmpty, s"fromUplc returns another builtin for: ${wrong.mkString(", ")}")
    }

    test("the comparison tells the type of fstPair from the type of sndPair") {
        // They differ only in the bound variable that they return.
        assert(difference(SIRBuiltins.fstPair.tp, SIRBuiltins.sndPair.tp).nonEmpty)
    }
}
