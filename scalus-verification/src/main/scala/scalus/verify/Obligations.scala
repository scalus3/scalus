package scalus.verify

import scalus.compiler.sir.{AnnotatedSIR, AnnotationsDecl, Binding, SIR, SIRPosition, SIRType}
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.Constant

/** The obligations a function's calls put on it: at every call of a function with a contract, the
  * arguments must satisfy that contract's precondition (prop-semantics.md §6).
  *
  * For a call `f(a, b)` in the body of `g`, the obligation is, over `g`'s parameters,
  *
  * {{{
  * denotes(reach) ==> Prop(check)
  * }}}
  *
  * Both tests are cut out of `g`'s SIR along the path to the call: the bindings before it stay, a
  * branch that does not lead to it returns `true`, and the call itself becomes
  * `let x = a; y = b in true` in `reach`, and `let x = a; y = b in expects_f` in `check`. So
  * `reach` runs what `g` runs before the call, and fails when `g` does: an obligation holds where
  * the call is not reached. A runtime `require` before the call therefore counts for the caller.
  *
  * The cut is conservative: code evaluated beside the path, as `h(a)` in `h(a) + f(b)`, is left
  * out, so an obligation can ask more than `g` needs when that code would fail first.
  */
private[verify] object Obligations {

    /** A call of `callee`, with the arguments of one full application. */
    final case class Site(node: SIR.Apply, callee: String, arguments: List[AnnotatedSIR])

    /** The parameters of a function and its body, and the definitions around it, innermost first.
      */
    final case class Body(
        parameters: List[SIR.Var],
        term: SIR,
        wrappers: List[SIR => SIR]
    ) {

        /** The parameters, as the variables of a statement about the function. */
        def variables: List[PropExpr.Ident[Any]] =
            parameters.zipWithIndex.map((parameter, index) =>
                new PropExpr.Ident[Any](parameter.name, index.toLong, parameter.tp)
            )

        /** `sir`, an expression of the body, inside the definitions around the function. */
        def wrapped(sir: SIR): SIR = wrappers.foldLeft(sir)((inner, wrap) => wrap(inner))
    }

    /** The body of `function` in the SIR its entry was compiled from. A method of an `@Compile`
      * object is compiled as a lambda that calls it, with its definition among the module
      * definitions around the lambda; another function is the lambda itself.
      */
    def body(function: FunctionDef[?, ?], sir: SIR): Body = {
        @annotation.tailrec
        def unwrap(
            current: SIR,
            wrappers: List[SIR => SIR],
            definition: Option[SIR]
        ): (SIR, List[SIR => SIR], Option[SIR]) = current match
            case SIR.Decl(data, term) =>
                unwrap(term, (inner => SIR.Decl(data, inner)) :: wrappers, definition)
            case SIR.Let(bindings, term, flags, anns) =>
                val own = bindings.find(_.name == function.name).map(_.value)
                unwrap(
                  term,
                  (inner => SIR.Let(bindings, inner, flags, anns)) :: wrappers,
                  definition.orElse(own)
                )
            case root => (root, wrappers, definition)
        val (root, wrappers, definition) = unwrap(sir, Nil, None)
        val (parameters, term) = lambda(definition.getOrElse(root), function.arity, function.name)
        Body(parameters, term, wrappers)
    }

    private def lambda(sir: SIR, arity: Int, name: String): (List[SIR.Var], SIR) =
        if arity == 0 then Nil -> sir
        else
            sir match
                case SIR.LamAbs(parameter, term, Nil, _) =>
                    val (rest, body) = lambda(term, arity - 1, name)
                    (parameter :: rest) -> body
                case other =>
                    throw new IllegalArgumentException(
                      s"$name is not a monomorphic function of its declared parameters: $other"
                    )

    /** The calls in `term` of the functions in `arities`, by name, and for each whether it sits
      * inside a function value, where a variable of the path is not a parameter of the caller.
      */
    def sites(term: SIR, arities: Map[String, Int]): List[(Site, Boolean)] = {
        val found = List.newBuilder[(Site, Boolean)]
        def visit(sir: SIR, inLambda: Boolean): Unit = sir match
            case apply: SIR.Apply =>
                // The applications of one call, outermost first, and its arguments in order.
                def spine(
                    current: AnnotatedSIR,
                    nodes: List[SIR.Apply]
                ): (AnnotatedSIR, List[SIR.Apply]) =
                    current match
                        case node: SIR.Apply => spine(node.f, node :: nodes)
                        case head            => head -> nodes
                val (head, nodes) = spine(apply, Nil)
                head match
                    case SIR.ExternalVar(_, name, _, _)
                        if arities.get(name).exists(_ <= nodes.size) =>
                        val arity = arities(name)
                        val applied = nodes.take(arity)
                        found += Site(applied.last, name, applied.map(_.arg)) -> inLambda
                    case _ => visit(head, inLambda)
                nodes.foreach(node => visit(node.arg, inLambda))
            case SIR.Decl(_, term) => visit(term, inLambda)
            case SIR.Let(bindings, body, _, _) =>
                bindings.foreach(binding => visit(binding.value, inLambda))
                visit(body, inLambda)
            case SIR.LamAbs(_, term, _, _) => visit(term, inLambda = true)
            case SIR.IfThenElse(condition, ifTrue, ifFalse, _, _) =>
                visit(condition, inLambda)
                visit(ifTrue, inLambda)
                visit(ifFalse, inLambda)
            case SIR.And(left, right, _) =>
                visit(left, inLambda)
                visit(right, inLambda)
            case SIR.Or(left, right, _) =>
                visit(left, inLambda)
                visit(right, inLambda)
            case SIR.Not(inner, _)                 => visit(inner, inLambda)
            case SIR.Select(scrutinee, _, _, _)    => visit(scrutinee, inLambda)
            case SIR.Constr(_, _, arguments, _, _) => arguments.foreach(visit(_, inLambda))
            case SIR.Match(scrutinee, cases, _, _) =>
                visit(scrutinee, inLambda)
                cases.foreach(caze => visit(caze.body, inLambda))
            case SIR.Cast(inner, _, _)    => visit(inner, inLambda)
            case SIR.Error(message, _, _) => visit(message, inLambda)
            case _: SIR.Var | _: SIR.ExternalVar | _: SIR.Const | _: SIR.Builtin => ()
        visit(term, inLambda = false)
        found.result()
    }

    private val annotations = AnnotationsDecl.empty
    private val isTrue: AnnotatedSIR = SIR.Const(Constant.Bool(true), SIRType.Boolean, annotations)

    /** `term` cut along the path to `site`, with `atSite` in place of the call and `true` on every
      * branch that does not lead to it. `None` when `term` does not contain the call outside a
      * function value.
      */
    def slice(term: SIR, site: SIR.Apply, atSite: AnnotatedSIR): Option[SIR] = {
        def conditional(condition: AnnotatedSIR, ifTrue: AnnotatedSIR, ifFalse: AnnotatedSIR) =
            SIR.IfThenElse(condition, ifTrue, ifFalse, SIRType.Boolean, annotations)
        def any(sir: SIR): Option[SIR] = sir match
            case SIR.Decl(data, term)    => any(term).map(SIR.Decl(data, _))
            case annotated: AnnotatedSIR => path(annotated)
        def path(sir: AnnotatedSIR): Option[AnnotatedSIR] =
            if sir eq site then Some(atSite)
            else
                sir match
                    case SIR.Let(bindings, body, flags, anns) =>
                        // A binding's value is evaluated after the bindings before it.
                        val inBinding = bindings.iterator.zipWithIndex
                            .flatMap((binding, index) => any(binding.value).map(_ -> index))
                            .nextOption()
                        inBinding match
                            case Some((value: AnnotatedSIR, 0)) => Some(value)
                            case Some((value, index)) =>
                                Some(SIR.Let(bindings.take(index), value, flags, anns))
                            case None => any(body).map(SIR.Let(bindings, _, flags, anns))
                    case SIR.IfThenElse(condition, ifTrue, ifFalse, _, _) =>
                        path(condition)
                            .orElse(path(ifTrue).map(conditional(condition, _, isTrue)))
                            .orElse(path(ifFalse).map(conditional(condition, isTrue, _)))
                    case SIR.And(left, right, _) =>
                        path(left).orElse(path(right).map(conditional(left, _, isTrue)))
                    case SIR.Or(left, right, _) =>
                        path(left).orElse(path(right).map(conditional(left, isTrue, _)))
                    case SIR.Not(inner, _) => path(inner)
                    case SIR.Match(scrutinee, cases, _, anns) =>
                        path(scrutinee).orElse {
                            val sliced = cases.map(caze => any(caze.body))
                            Option.when(sliced.exists(_.nonEmpty)) {
                                val bodies = cases
                                    .zip(sliced)
                                    .map((caze, body) => caze.copy(body = body.getOrElse(isTrue)))
                                SIR.Match(scrutinee, bodies, SIRType.Boolean, anns)
                            }
                        }
                    case SIR.Apply(function, argument, _, _) =>
                        path(function).orElse(path(argument))
                    case SIR.Select(scrutinee, _, _, _) => any(scrutinee).map(annotated)
                    case SIR.Constr(_, _, arguments, _, _) =>
                        arguments.iterator.flatMap(any).nextOption().map(annotated)
                    case SIR.Cast(inner, _, _)    => path(inner)
                    case SIR.Error(message, _, _) => path(message)
                    case _                        => None
        any(term)
    }

    /** A sliced term in a position that takes no declaration: slices keep declarations outside. */
    private def annotated(sir: SIR): AnnotatedSIR = sir match
        case value: AnnotatedSIR => value
        case SIR.Decl(_, term)   => annotated(term)

    /** The arguments of a call bound to the variables of the callee's contract, around `body`. */
    def bind(
        variables: List[PropExpr.Ident[?]],
        arguments: List[AnnotatedSIR],
        body: SIR
    ): AnnotatedSIR =
        SIR.Let(
          variables
              .zip(arguments)
              .map((variable, argument) => Binding(variable.name, variable.tp, argument)),
          body,
          SIR.LetFlags.None,
          annotations
        )

    val truth: SIR = isTrue

    /** The name of `Spec.ensuring` in SIR: a clause is a call of it with the clause's body and its
      * condition.
      */
    val Ensuring: String = EraseSpecifications.Ensuring

    /** The name in SIR of the function behind `Spec.ensures`: a clause is a call of it with
      * `_ => condition`.
      */
    val Ensures: String = EraseSpecifications.Ensures

    val unit: AnnotatedSIR = SIR.Const(Constant.Unit, SIRType.Unit, annotations)

    /** `condition` about the value of `stated`, named `result`. */
    def bind(result: SIR.Var, stated: AnnotatedSIR, condition: SIR): AnnotatedSIR =
        SIR.Let(
          List(Binding(result.name, result.tp, stated)),
          condition,
          SIR.LetFlags.None,
          annotations
        )

    /** The line of a call in its source file, counted from one, or zero when it has none. */
    def line(site: Site): Int = {
        val position: SIRPosition = site.node.anns.pos
        if position.file.isEmpty then 0 else position.startLine + 1
    }
}
