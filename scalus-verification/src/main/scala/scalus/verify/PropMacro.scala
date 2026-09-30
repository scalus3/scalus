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
  * parameter still used there computes the statement itself, which is a compile error.
  *
  * A binder and a leaf name a variable the same way, from its symbol ([[variableName]]).
  */
private[verify] object PropMacro {

    def forAll[A: Type](body: Expr[A => Prop])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A]), "forAll")((idents, inner) => universal(idents, inner))

    def forAll2[A: Type, B: Type](body: Expr[(A, B) => Prop])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A], Type.of[B]), "forAll")((idents, inner) =>
            universal(idents, inner)
        )

    def forAll3[A: Type, B: Type, C: Type](body: Expr[(A, B, C) => Prop])(using
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

    def exists[A: Type](body: Expr[A => Prop])(using Quotes): Expr[Prop] =
        binders(body, List(Type.of[A]), "exists")((idents, inner) =>
            '{ Prop.Exists(${ idents.head.asExprOf[PropExpr.Ident[A]] }, None, $inner) }
        )

    def existsLet[A: Type](witness: Expr[A], body: Expr[A => Prop])(using Quotes): Expr[Prop] =
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
        body: Expr[R => Prop],
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
        body: Expr[R => Prop],
        total: Boolean
    )(using Quotes): Expr[Prop] = call('{ $fn.ref }, arg, body, total)

    def test(b: Expr[Boolean])(using Quotes): Expr[Prop] = '{ Prop.Bool(${ leaf(b) }) }

    def denotes[A: Type](e: Expr[A])(using Quotes): Expr[Prop] = '{ Prop.Denotes(${ leaf(e) }) }

    def equal[A: Type](a: Expr[A], b: Expr[A])(using Quotes): Expr[Prop] =
        '{ Prop.Equal(${ leaf(a) }, ${ leaf(b) }) }

    /** Checks that the leaves of the lambda literal `body` have closed over its parameters, and
      * passes the binders' identifiers and the lambda's body to `build`.
      */
    private def binders(body: Expr[Any], types: List[Type[?]], kind: String)(
        build: (List[Expr[PropExpr.Ident[?]]], Expr[Prop]) => Expr[Prop]
    )(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        val (params, rhs) = strip(body.asTerm) match
            case Lambda(params, rhs) if params.size == types.size => params -> rhs
            case other =>
                report.errorAndAbort(
                  s"$kind requires a lambda literal of ${types.size} parameters",
                  other.pos
                )
        val symbols = params.map(_.symbol).toSet
        new TreeTraverser {
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
                case ident: Ident if symbols.contains(ident.symbol) =>
                    report.error(
                      s"${ident.name} is a variable of the statement: it can be used in the " +
                          "statement's tests and expressions, not to compute the statement itself. " +
                          "An if or match that chooses between statements is not supported; for " +
                          "one Boolean test, write Prop(...) around the whole condition",
                      ident.pos
                    )
                case _ => traverseTreeChildren(tree)(owner)
        }.traverseTree(rhs)(Symbol.spliceOwner)
        val idents = params.zip(types).map { (param, tpe) =>
            val name = variableName(param.symbol)
            val id = positionId(param.pos)
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
        build(idents, rhs.changeOwner(Symbol.spliceOwner).asExprOf[Prop])
    }

    /** An expression of the statement compiled to SIR. When it uses variables of enclosing binders,
      * it is compiled as a closed lambda over them, whose parameters are removed at runtime.
      */
    private def leaf[A: Type](e: Expr[A])(using Quotes): Expr[PropExpr[A]] = {
        import quotes.reflect.*
        val variables = binderVariables(e.asTerm)
        if variables.isEmpty then '{ PropExpr.SIRExpr[A](scalus.compiler.compile($e)) }
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
