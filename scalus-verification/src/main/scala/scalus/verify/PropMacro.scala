package scalus.verify

import scalus.uplc.builtin.{Data, FromData}

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

    def forAllData[A: Type](body: Expr[A => Prop | Boolean], decode: Expr[FromData[A]])(using
        Quotes
    ): Expr[Prop] =
        binders(body, List(Type.of[A]), "forAllData", Some(decode))((idents, inner) =>
            universal(idents, inner)
        )

    /** [[forAllData]] of the values that meet the limits `limit` says ([[limitsOf]]). */
    def forAllDataLimited[A: Type](
        limit: Expr[A => Boolean],
        body: Expr[A => Prop | Boolean],
        decode: Expr[FromData[A]]
    )(using Quotes): Expr[Prop] = {
        val limits = limitsOf(limit)
        binders(body, List(Type.of[A]), "forAllData", Some(decode), Some(limits))((idents, inner) =>
            universal(idents, inner)
        )
    }

    /** The limits that the lambda literal `limit` says of its parameter
      * ([[Props.forAllDataLimited]]): limits on the length of its collections, joined with `&&`.
      * Each is `maxCollectionLength(x) <= n`, or a comparison of the length of a collection of `x`
      * with `n`, `isEmpty` or `nonEmpty` of one, or `forall` over a list's elements of limits of
      * each element. A collection is named by the fields from `x` to it. Anything else is a compile
      * error: a limit is of the shape of the values, and another condition belongs in the
      * statement. A bound is any expression without the variable, of `BigInt` or `Int`, evaluated
      * where the statement is built.
      */
    private def limitsOf[A: Type](limit: Expr[A => Boolean])(using Quotes): Expr[Limits] = {
        import quotes.reflect.*
        val (params, rhs) = lambdaOf(
          limit.asTerm,
          Some(1),
          "the limit of forAllDataLimited must be a lambda literal of the variable"
        )
        val variable = params.head.symbol
        val limits = Expr.ofList(limitsIn(rhs, variable, Nil, Map.empty, variable.owner))
        '{ Limits($limits) }
    }

    /** What the limits of [[limitsOf]] say, and how to write another condition. */
    private val limitForms =
        "a limit of forAllDataLimited bounds the length of the variable's collections: " +
            "maxCollectionLength(x) <= n, x.a.b.length <= n (or <, ==, >=, >), isEmpty, " +
            "nonEmpty, and forall over a list, or over a map's keys or values, of the limits of " +
            "each of them, joined with &&. A " +
            "condition of another kind is the statement's: write it in its premise"

    /** The limits `term` says of `variable`, whose collections are at `prefix`. `proxies` are the
      * values of the bindings of the inline expansions `term` is in, such as the list that
      * `nonEmpty` is called on. `limit` is the lambda of the limits: a bound is computed where the
      * statement is built, outside it, and so may use nothing of it.
      */
    private def limitsIn(using
        Quotes
    )(
        term: quotes.reflect.Term,
        variable: quotes.reflect.Symbol,
        prefix: List[Expr[Step]],
        proxies: Map[quotes.reflect.Symbol, quotes.reflect.Term],
        limit: quotes.reflect.Symbol
    ): List[Expr[Limit]] = {
        import quotes.reflect.*
        def refuse(at: Tree): Nothing = report.errorAndAbort(limitForms, at.pos)
        // The values of the bindings of the inline expansions met so far.
        val scope = scala.collection.mutable.Map.from(proxies)
        // `term` without its wrappers, with the bindings of its inline expansions in `scope`.
        def unwrapped(term: Term): Term = term match
            case Inlined(_, bindings, inner) =>
                bindings.foreach {
                    case binding @ ValDef(_, _, Some(value)) => scope(binding.symbol) = value
                    case _                                   => refuse(term)
                }
                unwrapped(inner)
            case Typed(inner, _)                         => unwrapped(inner)
            case Block(Nil, inner)                       => unwrapped(inner)
            case Ident(_) if scope.contains(term.symbol) => unwrapped(scope(term.symbol))
            case other                                   => other
        val here = unwrapped(term)
        // The class of the values of `term`, through aliases: `v2.Value` is `v1.Value`.
        def classOf(term: Term): Symbol = term.tpe.widen.dealias.typeSymbol
        // A map, whose `Data` is its entries: `toList` and `toPairList` are the map itself.
        def isMap(term: Term): Boolean =
            Set(
              "scalus.cardano.onchain.plutus.prelude.SortedMap",
              "scalus.cardano.onchain.plutus.prelude.AssocMap"
            ).contains(classOf(term).fullName)
        val entries = Set("toList", "toPairList")
        // The steps from `variable` to the collection `term` is, where it is one of its parts.
        def path(term: Term): Option[List[Expr[Step]]] = unwrapped(term) match
            case reference @ Ident(_) if reference.symbol == variable => Some(prefix)
            case Select(map, name) if entries(name) && isMap(map)     => path(map)
            case Apply(function, List(map)) if entries(function.symbol.name) && isMap(map) =>
                path(map)
            case Select(qualifier, name) if classOf(qualifier).caseFields.exists(_.name == name) =>
                path(qualifier).map(_ :+ '{ Step.Field(${ Expr(name) }) })
            case _ => None
        // The collection a call of the method `name` is on: its receiver, or the argument of an
        // extension method.
        def receiver(term: Term, names: Set[String]): Option[Term] =
            unwrapped(term) match
                case Select(qualifier, name) if names(name) => Some(qualifier)
                case Apply(function, List(argument)) if names(function.symbol.name) =>
                    function match
                        case Select(qualifier, _) if path(qualifier).isDefined => Some(qualifier)
                        case _                                                 => Some(argument)
                case Apply(function, Nil) if names(function.symbol.name) =>
                    receiver(function, names)
                case TypeApply(function, _) if names(function.symbol.name) =>
                    receiver(function, names)
                case _ => None
        // What a comparison bounds: every collection, or the collection at a path.
        def measured(term: Term): Option[Option[List[Expr[Step]]]] =
            unwrapped(term) match
                case Apply(function, List(argument))
                    if function.symbol.name == "maxCollectionLength" =>
                    // Of the variable itself: every collection of a part of it is no limit here.
                    if prefix.nonEmpty || !path(argument).exists(_.isEmpty) then refuse(term)
                    Some(None)
                case other => receiver(other, Set("length", "size")).flatMap(path).map(Some(_))
        // Whether `term` uses what is of the limit's lambda: a variable, an element, or a value
        // of an inline expansion in it.
        def mentions(term: Term): Boolean = {
            def inLimit(symbol: Symbol): Boolean =
                !symbol.isNoSymbol && (symbol == limit || inLimit(symbol.maybeOwner))
            var found = false
            new TreeTraverser {
                override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                    if inLimit(tree.symbol) then found = true
                    else super.traverseTree(tree)(owner)
            }.traverseTree(term)(Symbol.spliceOwner)
            found
        }
        def bound(term: Term): Expr[BigInt] =
            if mentions(term) then refuse(term)
            else if term.tpe <:< TypeRepr.of[BigInt] then term.asExprOf[BigInt]
            else if term.tpe <:< TypeRepr.of[Int] then
                val n = term.asExprOf[Int]
                '{ BigInt($n) }
            else refuse(term)
        def length(path: List[Expr[Step]], min: Expr[Int], max: Expr[Option[Int]]) = {
            val steps = Expr.ofList(path)
            '{ Limit.Length($steps, $min, $max) }
        }
        // A bound is counted as a number of elements of `BigInt`, which is never out of range.
        def compared(at: Term, measure: Option[List[Expr[Step]]], op: String, n: Expr[BigInt]) =
            (measure, op) match
                case (None, "<=")       => '{ Limit.MaxCollectionLength(Limits.count($n)) }
                case (None, "<")        => '{ Limit.MaxCollectionLength(Limits.count($n - 1)) }
                case (None, _)          => refuse(at)
                case (Some(path), "<=") => length(path, '{ 0 }, '{ Some(Limits.count($n)) })
                case (Some(path), "<")  => length(path, '{ 0 }, '{ Some(Limits.count($n - 1)) })
                case (Some(path), "==") =>
                    length(path, '{ Limits.count($n) }, '{ Some(Limits.count($n)) })
                case (Some(path), ">=") => length(path, '{ Limits.count($n) }, '{ None })
                case (Some(path), ">")  => length(path, '{ Limits.count($n + 1) }, '{ None })
                case _                  => refuse(at)
        // `term` negated: `!inner`, as `nonEmpty` is `!isEmpty`.
        def negated(term: Term): Option[Term] = unwrapped(term) match
            case Select(inner, "unary_!")             => Some(inner)
            case Apply(Select(inner, "unary_!"), Nil) => Some(inner)
            case _                                    => None
        // `list.forall(element => body)`: the list, the element and the body.
        def forall(term: Term): Option[(Term, Symbol, Term)] = unwrapped(term) match
            case Apply(function, List(predicate)) if function.symbol.name == "forall" =>
                val list = function match
                    case Select(list, _)               => Some(list)
                    case TypeApply(Select(list, _), _) => Some(list)
                    case Apply(_, List(list))          => Some(list)
                    case _                             => None
                list.map { list =>
                    val (params, body) = lambdaOf(
                      predicate,
                      Some(1),
                      "the limit of each element of a list is a lambda literal of the element"
                    )
                    (list, params.head.symbol, body)
                }
            case _ => None
        val flipped = Map("<=" -> ">=", "<" -> ">", "==" -> "==", ">=" -> "<=", ">" -> "<")
        here match {
            case Apply(Select(left, "&&"), List(right)) =>
                limitsIn(left, variable, prefix, scope.toMap, limit) ++
                    limitsIn(right, variable, prefix, scope.toMap, limit)
            case Apply(Select(left, op), List(right)) if flipped.contains(op) =>
                (measured(left), measured(right)) match
                    case (Some(measure), None) => List(compared(here, measure, op, bound(right)))
                    case (None, Some(measure)) =>
                        List(compared(here, measure, flipped(op), bound(left)))
                    case _ => refuse(here)
            case _ =>
                receiver(here, Set("isEmpty"))
                    .flatMap(path)
                    .map(path => List(length(path, '{ 0 }, '{ Some(0) })))
                    .orElse(
                      negated(here)
                          .flatMap(receiver(_, Set("isEmpty")))
                          .flatMap(path)
                          .map(path => List(length(path, '{ 1 }, '{ None })))
                    )
                    .orElse(
                      receiver(here, Set("nonEmpty"))
                          .flatMap(path)
                          .map(path => List(length(path, '{ 1 }, '{ None })))
                    )
                    .orElse(forall(here).flatMap { (list, element, body) =>
                        // The elements of a list, or the keys or the values of a map.
                        val parts = receiver(list, Set("keys", "values"))
                            .filter(isMap)
                            .flatMap(map =>
                                path(map).map(_ :+ (unwrapped(list).symbol.name match
                                    case "keys" => '{ Step.Keys }
                                    case _      => '{ Step.Values }))
                            )
                            .orElse(path(list).map(_ :+ '{ Step.Elements }))
                        parts.map(limitsIn(body, element, _, scope.toMap, limit))
                    })
                    .getOrElse(refuse(here))
        }
    }

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

    /** [[DataEncoding.of]] of `A`, from its SIR type as a statement variable has it. */
    def dataEncoding[A: Type](using Quotes): Expr[DataEncoding] =
        '{ DataEncoding.of(Props.variableType(scalus.compiler.compile((value: A) => value))) }

    def denotes[A: Type](e: Expr[A])(using Quotes): Expr[Prop] = '{ Prop.Denotes(${ leaf(e) }) }

    def equal[A: Type](a: Expr[A], b: Expr[A])(using Quotes): Expr[Prop] =
        '{ Prop.Equal(${ leaf(a) }, ${ leaf(b) }) }

    /** Passes the binders' identifiers and the statement of the lambda literal `body`
      * ([[statementOf]]) to `build`.
      */
    private def binders(
        body: Expr[Any],
        types: List[Type[?]],
        kind: String,
        decode: Option[Expr[Any]] = None,
        limits: Option[Expr[Limits]] = None
    )(
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
        val idents = params.zip(types).map { (param, tpe) =>
            val decoder =
                if kind == "forAllData" then
                    refuseDataRepresented(List(param), "the variable of forAllData, one already,")
                    decode
                else if quantifying.contains(kind) then
                    encodedDecoder(param, TypeRepr.of(using tpe))
                else
                    refuseDataRepresented(List(param), "a call's result, the function's,")
                    None
            ident(param.symbol, param.pos, tpe, decoder, limits)
        }
        build(idents, statement)
    }

    /** The binders whose variables a statement quantifies over, which range over the `Data`
      * encodings of their values where their parameters are written `A @UplcRepr(Data)`. Another
      * binder, as a call's result, is given its value.
      */
    private val quantifying = Set("forAll", "exists", "existsLet")

    /** The decoder of the variable that `param` of a quantifier over `binderType` stands for, where
      * the parameter is written `A @UplcRepr(UplcRepresentation.Data)`: the `FromData[A]` given
      * here. A parameter's own type, as written, keeps its annotations: the binder's type argument
      * has lost them by the time the macro runs. `A` is the binder's type: a function's parameter
      * may be of a wider type, which the variable would range over instead.
      */
    private def encodedDecoder(using
        Quotes
    )(param: quotes.reflect.ValDef, binderType: quotes.reflect.TypeRepr): Option[Expr[Any]] = {
        import quotes.reflect.*
        dataRepresented(param.tpt.tpe).map { underlying =>
            if !(underlying =:= binderType) then
                report.errorAndAbort(
                  s"the parameter is of ${underlying.show}, and the quantifier of " +
                      s"${binderType.show}: a variable over Data encodings ranges over the " +
                      "quantifier's type, so write the parameter's type as it",
                  param.pos
                )
            underlying.asType match
                case '[u] =>
                    Expr
                        .summon[FromData[u]]
                        .getOrElse(
                          report.errorAndAbort(
                            s"a variable of ${Type.show[u]} @UplcRepr(Data) is read from its " +
                                s"Data, which needs a FromData[${Type.show[u]}]",
                            param.pos
                          )
                        )
        }
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

    /** The identifier of the statement variable a lambda's parameter stands for: over the `Data`
      * encodings of its values ([[Props.forAllData]]) where it has a `decode`, the `FromData` that
      * reads them, of the values that meet `limits` where they are given
      * ([[Props.forAllDataLimited]]).
      */
    private def ident(using
        Quotes
    )(
        symbol: quotes.reflect.Symbol,
        pos: quotes.reflect.Position,
        tpe: Type[?],
        decode: Option[Expr[Any]],
        limits: Option[Expr[Limits]] = None
    ): Expr[PropExpr.Ident[?]] = {
        val name = variableName(symbol)
        val id = positionId(pos)
        tpe match
            case '[t] =>
                val variable = '{
                    new PropExpr.Ident[t](
                      ${ Expr(name) },
                      ${ Expr(id) },
                      Props.variableType(scalus.compiler.compile((value: t) => value))
                    )
                }
                decode match
                    case None => variable
                    case Some(reader) =>
                        val reads = reader.asExprOf[FromData[t]]
                        val limited = limits.getOrElse('{ Limits.none })
                        '{
                            DataEncoding.encodedVariable(
                              $variable,
                              scalus.compiler.compile((data: Data) => data.to[t](using $reads)),
                              $limited
                            )
                        }
    }

    /** The type of a statement variable written `A @UplcRepr(UplcRepresentation.Data)`, which
      * ranges over the `Data` encodings of the values of `A`, as [[Props.forAllData]]: `A`. None
      * for another type: another representation is the compiler's, as it was.
      */
    private def dataRepresented(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
        import quotes.reflect.*
        def isData(argument: Term) =
            argument.symbol.name == "Data" &&
                argument.symbol.owner == TypeRepr
                    .of[scalus.compiler.UplcRepresentation]
                    .typeSymbol
                    .companionModule
                    .moduleClass
        // An alias is followed by hand: `dealias` drops the annotation too.
        def written(tpe: TypeRepr): TypeRepr = tpe match
            case alias: TypeRef if alias.typeSymbol.isAliasType =>
                written(alias.translucentSuperType)
            case other => other
        written(tpe) match
            case AnnotatedType(underlying, annotation @ Apply(_, List(argument)))
                if annotation.tpe <:< TypeRepr.of[scalus.compiler.UplcRepr] && isData(argument) =>
                Some(underlying)
            case _ => None
    }

    /** The type of a statement variable as code has it: without the representation that makes it a
      * variable over `Data` encodings ([[dataRepresented]]), which the statement reads and the code
      * does not.
      */
    private def codeType(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
        dataRepresented(tpe).getOrElse(tpe)

    /** Refuses `@UplcRepr(UplcRepresentation.Data)` on the parameters `params`, which stand for
      * `what`: a value given to the statement, not one it quantifies over.
      */
    private def refuseDataRepresented(using
        Quotes
    )(params: List[quotes.reflect.ValDef], what: String): Unit =
        params.foreach(param =>
            if dataRepresented(param.tpt.tpe).isDefined then
                quotes.reflect.report.errorAndAbort(
                  s"$what takes no @UplcRepr(UplcRepresentation.Data): only a variable that a " +
                      "statement quantifies over ranges over Data encodings",
                  param.pos
                )
        )

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
      * in place of `whenReturns` (design doc §3.7), as a [[FunctionContract]].
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
    )(using Quotes): Expr[FunctionContract[Arg, R]] = {
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
        // A contract's variables are its function's arguments, which callers instantiate.
        refuseDataRepresented(params, "a contract's variable, its function's argument,")
        refuseDataRepresented(ensureParams, "a contract's variable, its function's argument,")
        val idents = params.map(param => ident(param.symbol, param.pos, param.tpt.tpe.asType, None))
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
        '{
            FunctionContract.of[Arg, R](
              $fn.ref,
              ${ Expr.ofList(idents) },
              $precondition,
              List(Props.renameVariables($call, $renames))
            )
        }
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
                Select
                    .overloaded(tuple, "apply", params.map(param => codeType(param.tpt.tpe)), refs)
                    .asExprOf[Arg]
    }

    /** `∀ args. when(args) ==> succeeds(fn, args)`, or `==> fails(fn, args)`: the function returns,
      * or fails, on every argument that satisfies `when`. `succeeds` is the total call
      * `call(fn, args)(_ => true)`, and `fails` is its negation, so this is only syntax over
      * existing statements.
      */
    def returnsOrFailsWhen[Arg: Type, R: Type](
        fn: Expr[FunctionRef[Arg, R]],
        when: Expr[Any],
        fails: Boolean
    )(using Quotes): Expr[Prop] = {
        val (params, outcome) = outcomeWhen[Arg, R](fn, when, fails)
        universal(
          params.map { param =>
              val binderType = codeType(param.tpt.tpe)
              ident(param.symbol, param.pos, binderType.asType, encodedDecoder(param, binderType))
          },
          outcome
        )
    }

    /** `contract` with the guarantee that its function returns, or fails, where `when` holds. The
      * guarantee names the arguments after the parameters of `when`; the contract renames them.
      */
    def contractClause[Arg: Type, R: Type](
        contract: Expr[FunctionContract[Arg, R]],
        when: Expr[Any],
        fails: Boolean
    )(using Quotes): Expr[FunctionContract[Arg, R]] =
        '{
            val declared = $contract
            ${
                val (params, outcome) = outcomeWhen[Arg, R]('{ declared.function }, when, fails)
                refuseDataRepresented(params, "a contract's variable, its function's argument,")
                val names = Expr(params.map(param => variableName(param.symbol)))
                '{ declared.withClause($names, $outcome) }
            }
        }

    /** The parameters of the lambda literal `when`, and `when ==> succeeds(fn, parameters)` or
      * `when ==> fails(fn, parameters)` over them.
      */
    private def outcomeWhen[Arg: Type, R: Type](using
        Quotes
    )(
        fn: Expr[FunctionRef[Arg, R]],
        when: Expr[Any],
        fails: Boolean
    ): (List[quotes.reflect.ValDef], Expr[Prop]) = {
        import quotes.reflect.*
        val kind = if fails then "failsWhen" else "returnsWhen"
        val (params, condition) = lambdaOf(when.asTerm, None, s"$kind requires a lambda literal")
        val premise = statementOf(condition, params.map(_.symbol).toSet, kind)
        val returns =
            call[Arg, R](fn, argumentOf[Arg](params), '{ (_: R) => true }, total = true)
        val conclusion = if fails then '{ Prop.Not($returns) } else returns
        params -> '{ Prop.Implies($premise, $conclusion) }
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
              _ => variables.map(symbol => codeType(symbol.termRef.widen)),
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
