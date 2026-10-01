package scalus.verify

import scala.quoted.*

/** Builds the runtime [[Prop]] object from statement syntax.
  *
  * The compiler inlines from the inside out: the leaves of a statement are expanded before the
  * binders around them. A leaf (a test, `denotes`, `equal`, a call's argument or an `existsLet`
  * witness) finds the variables of enclosing binders in its expression and compiles a closed lambda
  * over them, `(v1, ..., vn) => e`, never an expression with free variables. At runtime
  * [[Props.openVariables]] removes the lambda's parameters and names their occurrences after the
  * binders. A binder (`forAll`, `exists`, `existsLet` and a call's result) then finds its lambda's
  * body free of its parameters, and keeps the body, which builds the rest of the statement. A
  * parameter still used there computes the statement itself, which is a compile error. A body of
  * type `Boolean` is itself a leaf: one test over the binder's parameters and the enclosing ones.
  *
  * A binder and a leaf name a variable the same way, from its symbol ([[variableName]]).
  */
private[verify] object PropMacro {

    def forAll[A: Type](body: Expr[A => Prop | Boolean])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A]), "forAll")((idents, inner) => universal(idents, inner))

    def forAll2[A: Type, B: Type](body: Expr[(A, B) => Prop | Boolean])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A], Type.of[B]), "forAll")((idents, inner) =>
            universal(idents, inner)
        )

    def forAll3[A: Type, B: Type, C: Type](body: Expr[(A, B, C) => Prop | Boolean])(using
        Quotes
    ): Expr[Prop] =
        binders(body, List(Type.of[A], Type.of[B], Type.of[C]), "forAll")((idents, inner) =>
            universal(idents, inner)
        )

    /** Universal quantifiers over `idents`, outermost first, around `body`. */
    private def universal(idents: List[Expr[PropExpr.Ident[?]]], body: Expr[Prop])(using
        Quotes
    ): Expr[Prop] =
        idents.foldRight(body)((ident, inner) =>
            '{ Prop.Forall($ident.asInstanceOf[PropExpr.Ident[Any]], $inner) }
        )

    def exists[A: Type](body: Expr[A => Prop | Boolean])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A]), "exists")((idents, inner) =>
            '{ Prop.Exists(${ idents.head.asExprOf[PropExpr.Ident[A]] }, None, $inner) }
        )

    def existsLet[A: Type](witness: Expr[A], body: Expr[A => Prop | Boolean])(using
        Quotes
    ): Expr[Prop] =
        binders(body, List(Type.of[A]), "existsLet")((idents, inner) =>
            '{
                Prop.Exists(
                  ${ idents.head.asExprOf[PropExpr.Ident[A]] },
                  Some(${ leaf(witness) }),
                  $inner
                )
            }
        )

    def call[A: Type, R: Type](
        fn: Expr[FunctionRef[A, R]],
        arg: Expr[A],
        body: Expr[R => Prop | Boolean],
        total: Boolean
    )(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[R]), "call")((idents, inner) =>
            '{
                Prop.Call[A, R](
                  $fn,
                  ${ leaf(arg) },
                  ${ idents.head.asExprOf[PropExpr.Ident[R]] },
                  ${ Expr(total) },
                  $inner
                )
            }
        )

    def callDef[A: Type, R: Type](
        fn: Expr[FunctionDef[A, R]],
        arg: Expr[A],
        body: Expr[R => Prop | Boolean],
        total: Boolean
    )(using Quotes): Expr[Prop] = call('{ $fn.ref }, arg, body, total)

    def test(b: Expr[Boolean])(using Quotes): Expr[Prop] = '{ Prop.Bool(${ leaf(b) }) }

    def denotes[A: Type](e: Expr[A])(using Quotes): Expr[Prop] = '{ Prop.Denotes(${ leaf(e) }) }

    def equal[A: Type](a: Expr[A], b: Expr[A])(using Quotes): Expr[Prop] =
        '{ Prop.Equal(${ leaf(a) }, ${ leaf(b) }) }

    /** Passes the binders' identifiers and the statement of the lambda literal `body`
      * ([[statementOf]]) to `build`.
      */
    private def binders(body: Expr[Any], types: List[Type[?]], kind: String)(
        build: (List[Expr[PropExpr.Ident[?]]], Expr[Prop]) => Expr[Prop]
    )(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        rejectCompiledCode()
        val (params, rhs) = lambdaOf(
          body.asTerm,
          Some(types.size),
          s"$kind requires a lambda literal of ${types.size} parameters"
        )
        val statement = statementOf(rhs, params.map(_.symbol).toSet, kind)
        val idents = params.zip(types).map((param, tpe) => ident(param.symbol, param.pos, tpe))
        build(idents, statement)
    }

    /** The statement a lambda's body states about its parameters `symbols`.
      *
      * A body of type `Boolean` is one test, compiled as a leaf over the lambda's parameters and
      * the enclosing binders' variables, so it may use them anywhere. A body of type `Prop` must
      * have its leaves closed over the lambda's parameters already.
      */
    private def statementOf(using
        Quotes
    )(
        rhs: quotes.reflect.Term,
        symbols: Set[quotes.reflect.Symbol],
        kind: String
    ): Expr[Prop] = {
        import quotes.reflect.*
        val bodyType = rhs.tpe.widen
        if bodyType <:< TypeRepr.of[Boolean] then '{ Prop.Bool(${ leaf(rhs.asExprOf[Boolean]) }) }
        else if bodyType <:< TypeRepr.of[Prop] then
            checkClosed(rhs, symbols)
            rhs.changeOwner(Symbol.spliceOwner).asExprOf[Prop]
        else
            report.errorAndAbort(
              s"the body of $kind is a statement in one branch and a Boolean test in another. " +
                  choiceHint,
              rhs.pos
            )
    }

    /** The identifier of the statement variable a lambda's parameter stands for. */
    private def ident(using
        Quotes
    )(
        symbol: quotes.reflect.Symbol,
        pos: quotes.reflect.Position,
        tpe: Type[?]
    ): Expr[PropExpr.Ident[?]] = {
        val name = variableName(symbol)
        val id = positionId(pos)
        tpe match
            case '[t] =>
                '{
                    new PropExpr.Ident[t](
                      ${ Expr(name) },
                      ${ Expr(id) },
                      Props.variableType(scalus.compiler.compile((value: t) => value))
                    )
                }
    }

    /** The parameters and body of the lambda literal `term`, of `arity` parameters when given. */
    private def lambdaOf(using
        Quotes
    )(
        term: quotes.reflect.Term,
        arity: Option[Int],
        message: => String
    ): (List[quotes.reflect.ValDef], quotes.reflect.Term) = {
        import quotes.reflect.*
        strip(term) match
            case Lambda(params, rhs) if arity.forall(_ == params.size) => params -> rhs
            case other => report.errorAndAbort(message, other.pos)
    }

    /** `∀ args. expects(args) ==> whenReturns(fn, args)(r => ensures(args)(r))`, or a total `call`
      * in place of `whenReturns` (design doc §3.7), with the function and its totality.
      *
      * The arguments are the parameters of `expects`, whose types the overload of `Props.contract`
      * fixed. `ensures` names its own parameters, and its leaves were compiled before this macro,
      * closed over them. At runtime they are renamed after the parameters of `expects`, so both
      * lambdas speak of the same variables.
      */
    def contract[Arg: Type, R: Type](
        fn: Expr[FunctionDef[Arg, R]],
        expects: Expr[Any],
        ensures: Expr[Any],
        total: Boolean
    )(using Quotes): Expr[Contract] = {
        import quotes.reflect.*
        val (params, pre) = lambdaOf(expects.asTerm, None, "expects must be a lambda literal")
        val arity = params.size
        val (ensureParams, result) = lambdaOf(
          ensures.asTerm,
          Some(arity),
          s"ensures must be a lambda literal of $arity parameters"
        )
        // `call` binds the result; the parameters of `ensures` may be used only in its leaves.
        val (_, post) = lambdaOf(
          result,
          Some(1),
          "ensures must return a lambda literal of the function's result, as in (x, y) => r => ..."
        )
        if post.tpe.widen <:< TypeRepr.of[Prop] then
            checkClosed(post, ensureParams.map(_.symbol).toSet)
        val precondition = statementOf(pre, params.map(_.symbol).toSet, "expects")
        val idents = params.map(param => ident(param.symbol, param.pos, param.tpt.tpe.asType))
        val call = PropMacro.call[Arg, R](
          '{ $fn.ref },
          argumentOf[Arg](params),
          result.asExprOf[R => Prop | Boolean],
          total
        )
        val renames = Expr(
          ensureParams
              .map(param => variableName(param.symbol))
              .zip(params.map(param => variableName(param.symbol)))
              .toMap
        )
        val prop = universal(
          idents,
          '{ Prop.Implies($precondition, Props.renameVariables($call, $renames)) }
        )
        '{ Contract($fn.ref, ${ Expr(total) }, $prop) }
    }

    /** A function's argument from the parameters of a lambda that stand for it: the parameter
      * itself, or the tuple of several, as a call passes them.
      */
    private def argumentOf[Arg: Type](using
        Quotes
    )(
        params: List[quotes.reflect.ValDef]
    ): Expr[Arg] = {
        import quotes.reflect.*
        params.map(param => Ref(param.symbol)) match
            case List(one) => one.asExprOf[Arg]
            case refs =>
                val tuple = Ref(defn.TupleClass(refs.size).companionModule)
                Select.overloaded(tuple, "apply", params.map(_.tpt.tpe), refs).asExprOf[Arg]
    }

    /** `∀ args. when(args) ==> succeeds(fn, args)`, or `==> fails(fn, args)`: the function returns,
      * or fails, on every argument that satisfies `when`. `succeeds` is the total call
      * `call(fn, args)(_ => true)`, and `fails` is its negation, so this is only syntax over
      * existing statements.
      */
    def returnsOrFailsWhen[Arg: Type, R: Type](
        fn: Expr[FunctionDef[Arg, R]],
        when: Expr[Any],
        fails: Boolean
    )(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        val kind = if fails then "failsWhen" else "returnsWhen"
        val (params, condition) = lambdaOf(when.asTerm, None, s"$kind requires a lambda literal")
        val premise = statementOf(condition, params.map(_.symbol).toSet, kind)
        val idents = params.map(param => ident(param.symbol, param.pos, param.tpt.tpe.asType))
        val returns =
            call[Arg, R]('{ $fn.ref }, argumentOf[Arg](params), '{ (_: R) => true }, total = true)
        val conclusion = if fails then '{ Prop.Not($returns) } else returns
        universal(idents, '{ Prop.Implies($premise, $conclusion) })
    }

    private val choiceHint =
        "An if or match that chooses between statements is not supported: state each case with " +
            "==>, as (c ==> p) && (!c ==> q). An if or match over Boolean tests is itself one test."

    /** Reports every use of `symbols` left in a statement body, where the leaves have already
      * replaced their own uses. A remaining use computes the statement itself.
      */
    private def checkClosed(using
        Quotes
    )(
        rhs: quotes.reflect.Term,
        symbols: Set[quotes.reflect.Symbol]
    ): Unit = {
        import quotes.reflect.*
        val uses = new TreeAccumulator[List[Ident]] {
            override def foldTree(found: List[Ident], tree: Tree)(owner: Symbol): List[Ident] =
                tree match
                    case ident: Ident if symbols.contains(ident.symbol) => ident :: found
                    case _ => foldOverTree(found, tree)(owner)
        }.foldTree(Nil, rhs)(Symbol.spliceOwner).reverse
        uses.foreach { ident =>
            report.error(
              s"${ident.name} is a variable of the statement: it can be used in the statement's " +
                  s"tests and expressions, not to compute the statement itself. $choiceHint",
              ident.pos
            )
        }
        // The body cannot be built with a variable out of its scope.
        if uses.nonEmpty then throw new scala.quoted.runtime.StopMacroExpansion
    }

    /** An expression of the statement compiled to SIR. When it uses variables of enclosing binders,
      * it is compiled as a closed lambda over them, whose parameters are removed at runtime.
      */
    private def leaf[A: Type](e: Expr[A])(using Quotes): Expr[PropExpr[A]] = {
        import quotes.reflect.*
        rejectCompiledCode()
        val variables = binderVariables(e.asTerm)
        if variables.isEmpty then
            val closed = e.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A]
            '{ PropExpr.SIRExpr[A](scalus.compiler.compile($closed)) }
        else
            val names = variables.map(variableName)
            val methodType = MethodType(variables.map(_.name))(
              _ => variables.map(symbol => symbol.termRef.widen),
              _ => TypeRepr.of[A]
            )
            val lambda = Lambda(
              Symbol.spliceOwner,
              methodType,
              (method, params) => {
                  val replacements = variables.zip(params).toMap
                  new TreeMap {
                      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
                          case ident: Ident if replacements.contains(ident.symbol) =>
                              Ref(replacements(ident.symbol).symbol)
                          case _ => super.transformTerm(tree)(owner)
                  }.transformTerm(e.asTerm)(method).changeOwner(method)
              }
            )
            '{
                PropExpr.SIRExpr[A](
                  Props.openVariables(
                    scalus.compiler.compile(${ lambda.asExpr }),
                    ${ Expr(names) }
                  )
                )
            }
    }

    /** Rejects a statement written inside an `@Compile` object, class or trait.
      *
      * Code there is compiled by the Scalus plugin, which would meet the Scala code this macro
      * builds `Prop` values with, and fail on it. Statements in compiled code are to be read from
      * its SIR, with pseudo-functions (prop-capture.md, "Statements in SIR"); until then they are
      * written outside it.
      */
    private def rejectCompiledCode()(using Quotes): Unit = {
        import quotes.reflect.*
        val compile = TypeRepr.of[scalus.compiler.Compile]
        def annotated(symbol: Symbol): Boolean =
            !symbol.isNoSymbol && symbol.annotations.exists(_.tpe <:< compile)
        // Only classes carry @Compile. Reading the annotations of an enclosing value whose type is
        // still being inferred would be a cyclic reference.
        val classes = Iterator
            .iterate(Symbol.spliceOwner)(_.owner)
            .takeWhile(symbol => !symbol.isNoSymbol && !symbol.isPackageDef)
            .filter(_.isClassDef)
        classes.find(owner => annotated(owner) || annotated(owner.companionModule)) match
            case Some(owner) =>
                report.errorAndAbort(
                  s"a statement cannot be built inside the @Compile ${owner.name.stripSuffix("$")}, " +
                      "whose code the Scalus plugin compiles. State it outside compiled code, as a " +
                      "Props statement or an external contract(f)(expects, ensures)"
                )
            case None => ()
    }

    /** The variables of enclosing binders an expression uses, in order of first use: parameters of
      * lambdas around the expression. Parameters of lambdas inside it are its own.
      */
    private def binderVariables(using
        Quotes
    )(
        term: quotes.reflect.Term
    ): List[quotes.reflect.Symbol] = {
        import quotes.reflect.*
        val defined = new TreeAccumulator[Set[Symbol]] {
            override def foldTree(found: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] =
                tree match
                    case definition: Definition =>
                        foldOverTree(found + definition.symbol, tree)(owner)
                    case _ => foldOverTree(found, tree)(owner)
        }.foldTree(Set.empty, term)(Symbol.spliceOwner)
        new TreeAccumulator[List[Symbol]] {
            override def foldTree(found: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] =
                tree match
                    case ident: Ident
                        if isLambdaParameter(ident.symbol) && !defined.contains(ident.symbol) =>
                        if found.contains(ident.symbol) then found else found :+ ident.symbol
                    case _ => foldOverTree(found, tree)(owner)
        }.foldTree(Nil, term)(Symbol.spliceOwner)
    }

    private def isLambdaParameter(using Quotes)(symbol: quotes.reflect.Symbol): Boolean = {
        import quotes.reflect.*
        symbol.isTerm && symbol.flags.is(Flags.Param) && symbol.owner.isAnonymousFunction
    }

    /** The name of a statement variable: the parameter's name and the offset of its declaration. */
    private def variableName(using Quotes)(symbol: quotes.reflect.Symbol): String =
        s"${symbol.name}_${symbol.pos.map(_.start).getOrElse(0)}"

    private def strip(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term = {
        import quotes.reflect.*
        term match
            case Inlined(_, Nil, inner)     => strip(inner)
            case inlined @ Inlined(_, _, _) =>
                // The expansion may use the bindings, which a statement cannot keep.
                report.errorAndAbort(
                  "a statement's lambda inside an inline expansion with bindings is not supported",
                  inlined.pos
                )
            case Typed(inner, _)   => strip(inner)
            case Block(Nil, inner) => strip(inner)
            case other             => other
    }

    /** An identifier for a binder, from its source position. */
    private def positionId(using Quotes)(pos: quotes.reflect.Position): Long =
        (pos.start.toLong << 32) | (pos.end.toLong & 0xffffffffL)
}
