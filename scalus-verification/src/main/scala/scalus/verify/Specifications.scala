package scalus.verify

import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, ConstrDecl, DataDecl, SIR, SIRType, TypeBinding}
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.Constant

/** Reads the contract a function's body states with [[scalus.cardano.onchain.plutus.prelude.Spec]],
  * from the function's SIR.
  *
  * The clauses are at the head of the body, and on its last expression or around the whole of it:
  *
  * {{{
  * def f(x: A): R = {            def f(x: A): R = {
  *     Spec.expects(c1)              Spec.expects(c1)
  *     Spec.ensures(p)               ...
  *     (e).ensuring(r => q)      }.ensuring(r => q)
  * }
  * }}}
  *
  * The contract is the one `Props.contract` builds from the same conditions (prop-semantics.md §6):
  * over the function's parameters, the conjunction of the `expects` conditions is the precondition,
  * and each of the others a guarantee, `whenReturns(f, parameters)(r => q)`. A clause further
  * inside the body, in a branch or after a local value, is not part of the contract;
  * [[Verifier.guarantees]] reads it.
  */
private[verify] object Specifications {

    private val annotations = AnnotationsDecl.empty

    private def constant(value: Boolean): AnnotatedSIR =
        SIR.Const(Constant.Bool(value), SIRType.Boolean, annotations)

    /** The clauses written as statements that a body starts with, and what follows them. */
    private final case class Head(
        expected: List[AnnotatedSIR],
        ensured: List[(SIR.Var, SIR)],
        rest: SIR
    )

    def read[A, R](function: FunctionDef[A, R]): Option[Contract[A, R]] = {
        val body = Obligations.body(function, function(Representation.Sir))
        val head = statements(body.term)
        // The postcondition of the result is on what follows the clauses, or around them all.
        val (inner, ofResult) = ensuring(head.rest, function.name)
        val within = if ofResult.isEmpty then Head(Nil, Nil, inner) else statements(inner)
        val preconditions = head.expected ++ within.expected
        val ofParameters = head.ensured ++ within.ensured
        Option.when(preconditions.nonEmpty || ofParameters.nonEmpty || ofResult.nonEmpty) {
            // A condition is an expression of the function's body, so the definitions around the
            // function are around it too.
            def expression[T](sir: SIR): PropExpr[T] = PropExpr.SIRExpr[T](body.wrapped(sir))
            val variables = body.variables
            def guarantee(result: SIR.Var, condition: SIR): Prop =
                Prop.Call[A, R](
                  function.ref,
                  argument(variables),
                  new PropExpr.Ident[R](result.name, variables.size.toLong, result.tp),
                  total = false,
                  Prop.Bool(expression(condition))
                )
            // A condition of the parameters alone holds wherever the function returns, whatever
            // it returns.
            val returned = SIR.Var("$result", inner.tp, annotations)
            val guarantees =
                ofParameters.map((unit, condition) =>
                    guarantee(returned, Obligations.bind(unit, Obligations.unit, condition))
                ) ++ ofResult.map(guarantee(_, _))
            Contract.of[A, R](
              function.ref,
              variables,
              Prop.Bool(
                expression(
                  preconditions.reduceOption(SIR.And(_, _, annotations)).getOrElse(constant(true))
                )
              ),
              if guarantees.isEmpty then List(Prop.Bool(PropExpr.SIRExpr(constant(true))))
              else guarantees
            )
        }
    }

    /** `Spec.expects(c)` and `Spec.ensures(c)` statements at the start of `term`, in order. */
    private def statements(term: SIR): Head = term match
        case SIR.Decl(_, inner)    => statements(inner)
        case SIR.Cast(inner, _, _) => statements(inner)
        case SIR.Let(bindings, body, flags, anns) =>
            val clauses = bindings.map(binding => clause(binding.value))
            val leading = clauses.takeWhile(_.nonEmpty).flatten
            val here = Head(
              leading.collect { case Left(condition) => condition },
              leading.collect { case Right(stated) => stated },
              term
            )
            if leading.size < bindings.size then
                // What follows is the rest of this let, clauses removed.
                here.copy(rest = SIR.Let(bindings.drop(leading.size), body, flags, anns))
            else
                val further = statements(body)
                Head(
                  here.expected ++ further.expected,
                  here.ensured ++ further.ensured,
                  further.rest
                )
        case other => Head(Nil, Nil, other)

    /** A clause written as a statement: the condition of an `expects`, or the unused parameter and
      * the condition of an `ensures`.
      */
    private def clause(sir: SIR): Option[Either[AnnotatedSIR, (SIR.Var, SIR)]] = sir match
        case SIR.Apply(SIR.ExternalVar(_, EraseSpecifications.Expects, _, _), condition, _, _) =>
            Some(Left(condition))
        case SIR.Apply(
              SIR.ExternalVar(_, EraseSpecifications.Ensures, _, _),
              SIR.LamAbs(unit, condition, Nil, _),
              _,
              _
            ) =>
            Some(Right(unit -> condition))
        case SIR.Cast(inner, _, _) => clause(inner)
        case _                     => None

    /** What `ensuring` is applied to, and the result variable and condition of that clause; or
      * `term` itself, when it is no `ensuring`.
      */
    private def ensuring(term: SIR, name: String): (SIR, Option[(SIR.Var, SIR)]) = term match
        case SIR.Decl(_, inner)    => ensuring(inner, name)
        case SIR.Cast(inner, _, _) => ensuring(inner, name)
        case SIR.Apply(
              SIR.Apply(SIR.ExternalVar(_, EraseSpecifications.Ensuring, _, _), body, _, _),
              condition,
              _,
              _
            ) =>
            condition match
                case SIR.LamAbs(result, post, Nil, _) => body -> Some(result -> post)
                case other =>
                    throw new IllegalArgumentException(
                      s"the postcondition of $name must be a function literal, as in " +
                          s"body.ensuring(r => ...): $other"
                    )
        case other => other -> None

    /** A function's argument from its parameters: the parameter itself, or the tuple of several, as
      * a call written out passes them. A tactic takes the tuple apart again, and never builds it.
      */
    private def argument[A](variables: List[PropExpr.Ident[Any]]): PropExpr[A] = variables match
        case List(one) => one.asInstanceOf[PropExpr[A]]
        case several =>
            val name = s"scala.Tuple${several.size}"
            val constructor = ConstrDecl(
              name,
              several.zipWithIndex.map((variable, index) =>
                  TypeBinding(s"_${index + 1}", variable.tp)
              ),
              Nil,
              Nil,
              annotations
            )
            PropExpr.SIRExpr[A](
              SIR.Constr(
                name,
                DataDecl(name, List(constructor), Nil, annotations),
                several.map(variable => SIR.Var(variable.name, variable.tp, annotations)),
                SIRType.CaseClass(constructor, Nil, None),
                annotations
              )
            )
}
