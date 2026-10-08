package scalus.verify

import scalus.compiler.sir.{SIR, SIRType}
import scalus.compiler.sir.linking.Wrappers
import scala.language.implicitConversions

enum PropExpr[A] {
    case Ident(name: String, id: Long, tp: SIRType)
    case SIRExpr(sir: SIR)
}

/** A logical statement about Scalus code: typed quantifiers, connectives, tests and calls.
  *
  * See `docs/design/verification-overview.md`, §3. Statements are built with the combinators in
  * [[Props]] ([[Props.forAll]], [[Props.exists]], [[Props.existsLet]], [[Props.call]],
  * [[Props.whenReturns]], [[Props.denotes]], [[equal]]) and the connectives below. Source lambdas
  * are syntax sugar; the runtime form uses explicit [[PropExpr.Ident]] values.
  *
  * This is the runtime representation of a statement. The compiler captures source syntax into
  * explicit logical nodes and SIR terms; proving tactics consume those nodes directly.
  *
  * Leaves built from Scala expressions contain a [[PropExpr.SIRExpr]] compiled by the Scalus
  * compiler. A call names its function in a [[FunctionTable]].
  *
  * Semantics (§3.3): the `Bool` leaf contains Boolean code whose result is interpreted under
  * on-chain semantics. An error is not `true`. [[denotes]] states separately that an expression
  * terminates without an error. Connectives are classical. `!` on a `Prop` is logical negation; `!`
  * inside a Boolean expression is part of the test. The two differ when the expression can fail.
  *
  * Precedence: Scala ranks an operator by its first character, so `==>` binds like `==` and `<=>`
  * like `<`, both tighter than `&&` and `||`. `p && q ==> r` therefore means `p && (q ==> r)`.
  * Parenthesize, or use the alphanumeric [[implies]] and [[iff]], which bind loosest of all.
  */
enum Prop {

    /** A compiled Boolean expression. */
    case Bool(expr: PropExpr[Boolean])

    /** A compiled expression whose evaluation must terminate successfully. */
    case Denotes[A](expr: PropExpr[A])

    /** Two compiled expressions that must be equal. */
    case Equal[A](left: PropExpr[A], right: PropExpr[A])

    /** A call of the function `fn` names in the [[FunctionTable]], continuing with its result. With
      * `total`, the call must return (total correctness). Without it, a call that fails satisfies
      * the proposition (partial correctness).
      */
    case Call[A, R](
        fn: FunctionRef[A, R],
        arg: PropExpr[A],
        result: PropExpr.Ident[R],
        total: Boolean,
        body: Prop
    )
    case Forall[A](ident: PropExpr.Ident[A], body: Prop)
    case Exists[A](ident: PropExpr.Ident[A], witness: Option[PropExpr[A]], body: Prop)
    case And(a: Prop, b: Prop)
    case Or(a: Prop, b: Prop)
    case Implies(a: Prop, b: Prop)
    case Iff(a: Prop, b: Prop)
    case Not(a: Prop)

    def &&(q: Prop): Prop = And(this, q)
    def ||(q: Prop): Prop = Or(this, q)
    def ==>(q: Prop): Prop = Implies(this, q)
    def <=>(q: Prop): Prop = Iff(this, q)
    def unary_! : Prop = Not(this)

    /** [[==>]] with the lowest precedence: `p && q implies r` means `(p && q) ==> r`. */
    infix def implies(q: Prop): Prop = Implies(this, q)

    /** [[<=>]] with the lowest precedence. */
    infix def iff(q: Prop): Prop = Iff(this, q)
}

/** The contract of a function, built by [[Props.contract]] or [[Props.totalContract]], and declared
  * with [[Verifier.contract]].
  *
  * It says `∀ variables. expects ==> guarantees`: `variables` stand for the function's arguments,
  * `expects` is what every caller must establish, and `guarantees` is what the function then
  * provides, the postcondition last. `returnsWhen` and `failsWhen` on a contract add a guarantee:
  * where a condition holds, the function returns, or fails. A contract is built only by those
  * forms, so it always has this shape.
  */
final class Contract[A, R] private[verify] (
    val function: FunctionRef[A, R],
    val variables: List[PropExpr.Ident[?]],
    val expects: Prop,
    val guarantees: List[Prop]
) {

    /** The contract as one statement. */
    def prop: Prop =
        variables.foldRight(Prop.Implies(expects, guarantees.reduceRight(Prop.And(_, _)))) {
            case (variable: PropExpr.Ident[t], inner) => Prop.Forall[t](variable, inner)
        }

    /** This contract with one more guarantee. `guarantee` speaks of the arguments under `names`, in
      * order, which are renamed after the contract's variables.
      */
    private[verify] def withClause(names: List[String], guarantee: Prop): Contract[A, R] =
        new Contract(
          function,
          variables,
          expects,
          Props.renameVariables(guarantee, names.zip(variables.map(_.name)).toMap) :: guarantees
        )

    override def toString: String = s"Contract(${function.name})"
}

/** The clauses that state a contract's outcome. They are extensions here, in the contract's own
  * scope, so `contract.returnsWhen(...)` needs no import.
  */
object Contract {

    /** The constructor, for the code the contract macro generates. */
    private[verify] def of[A, R](
        function: FunctionRef[A, R],
        variables: List[PropExpr.Ident[?]],
        expects: Prop,
        guarantees: List[Prop]
    ): Contract[A, R] = new Contract(function, variables, expects, guarantees)

    /** The contract `function` states in its own body, with `Spec.expects` and `ensuring`
      * ([[scalus.cardano.onchain.plutus.prelude.Spec]]), or `None` when it states none. It is the
      * contract [[Props.contract]] builds from the same conditions, so it is declared, proved and
      * owed by callers in the same way. Add `returnsWhen` or `failsWhen` to it as to any contract.
      */
    def inSource[A, R](function: FunctionDef[A, R]): Option[Contract[A, R]] =
        Specifications.read(function)

    extension [A, R](contract: Contract[A, R]) {

        /** The function returns on every argument that satisfies the precondition and `when`. With
          * [[failsWhen]] it states which outcome the function has; arguments that satisfy neither
          * condition are left open. `returnsWhen(_ => true)` makes the contract total.
          *
          * {{{
          * contract(div10)(expects = x => true, ensures = x => r => r <= BigInt(10))
          *     .returnsWhen(x => x != BigInt(0))
          *     .failsWhen(x => x == BigInt(0))
          * }}}
          */
        inline def returnsWhen(inline when: A => Prop | Boolean): Contract[A, R] =
            ${ PropMacro.contractClause('contract, 'when, false) }

        /** The function fails on every argument that satisfies the precondition and `when`. */
        inline def failsWhen(inline when: A => Prop | Boolean): Contract[A, R] =
            ${ PropMacro.contractClause('contract, 'when, true) }
    }

    extension [A, B, R](contract: Contract[(A, B), R]) {

        /** [[returnsWhen]] for a function of two parameters. */
        inline def returnsWhen(inline when: (A, B) => Prop | Boolean): Contract[(A, B), R] =
            ${ PropMacro.contractClause('contract, 'when, false) }

        /** [[failsWhen]] for a function of two parameters. */
        inline def failsWhen(inline when: (A, B) => Prop | Boolean): Contract[(A, B), R] =
            ${ PropMacro.contractClause('contract, 'when, true) }
    }

    extension [A, B, C, R](contract: Contract[(A, B, C), R]) {

        /** [[returnsWhen]] for a function of three parameters. */
        inline def returnsWhen(
            inline when: (A, B, C) => Prop | Boolean
        ): Contract[(A, B, C), R] = ${ PropMacro.contractClause('contract, 'when, false) }

        /** [[failsWhen]] for a function of three parameters. */
        inline def failsWhen(
            inline when: (A, B, C) => Prop | Boolean
        ): Contract[(A, B, C), R] = ${ PropMacro.contractClause('contract, 'when, true) }
    }
}

object Props {

    /** Constructs an explicit universal quantifier from an identifier and a proposition. */
    def forAllSIR[A](ident: PropExpr.Ident[A], body: Prop): Prop = Prop.Forall(ident, body)

    /** Every value of `A` satisfies the body. The body is a statement about the lambda's parameter,
      * such as `forAll[BigInt](x => denotes(BigInt(10) / x) ==> (x != BigInt(0)))`, or a Boolean
      * test, such as `forAll[BigInt](x => Math.abs(x) >= 0)`. A Boolean body is one test, whatever
      * it contains: `if`, `match` and local `val`s included. In a statement, the parameter may be
      * used in its tests and expressions, not to compute the statement itself.
      */
    inline def forAll[A: Quantifiable](inline body: A => Prop | Boolean): Prop =
        ${ PropMacro.forAll[A]('body) }

    /** A statement about two universally quantified values:
      * `forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x)` is `∀ x. ∀ y. Bool(...)`.
      */
    inline def forAll[A: Quantifiable, B: Quantifiable](
        inline body: (A, B) => Prop | Boolean
    ): Prop =
        ${ PropMacro.forAll2[A, B]('body) }

    /** A statement about three universally quantified values. */
    inline def forAll[A: Quantifiable, B: Quantifiable, C: Quantifiable](
        inline body: (A, B, C) => Prop | Boolean
    ): Prop = ${ PropMacro.forAll3[A, B, C]('body) }

    /** Constructs an explicit existential quantifier from an identifier and a proposition. */
    def existsSIR[A](
        ident: PropExpr.Ident[A],
        witness: Option[PropExpr[A]],
        body: Prop
    ): Prop = Prop.Exists(ident, witness, body)

    /** Some value of `A` satisfies the body. */
    inline def exists[A: Quantifiable](inline body: A => Prop | Boolean): Prop =
        ${ PropMacro.exists[A]('body) }

    /** Some value of `A` satisfies the body, and `witness` supplies that value. */
    inline def existsLet[A: Quantifiable](inline witness: A)(
        inline body: A => Prop | Boolean
    ): Prop =
        ${ PropMacro.existsLet[A]('witness, 'body) }

    /** `fn` applied to `arg` returns, and its result satisfies the body. */
    inline def callRef[A, R](fn: FunctionRef[A, R], inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = ${ PropMacro.call('fn, 'arg, 'body, true) }

    /** Names a method of a `@Compile` object and states a property of its result. */
    inline def call[A, R](inline f: A => R, inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, B, R](inline f: (A, B) => R, inline arg: (A, B))(
        inline body: R => Prop | Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, B, C, R](inline f: (A, B, C) => R, inline arg: (A, B, C))(
        inline body: R => Prop | Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, R](fn: FunctionDef[A, R], inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = ${ PropMacro.callDef('fn, 'arg, 'body, true) }

    /** Whenever `fn` applied to `arg` returns, its result satisfies the body. */
    inline def whenReturnsRef[A, R](fn: FunctionRef[A, R], inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = ${ PropMacro.call('fn, 'arg, 'body, false) }

    inline def whenReturns[A, R](inline f: A => R, inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, B, R](inline f: (A, B) => R, inline arg: (A, B))(
        inline body: R => Prop | Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, B, C, R](inline f: (A, B, C) => R, inline arg: (A, B, C))(
        inline body: R => Prop | Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, R](fn: FunctionDef[A, R], inline arg: A)(
        inline body: R => Prop | Boolean
    ): Prop = ${ PropMacro.callDef('fn, 'arg, 'body, false) }

    /** The contract of a function of one parameter: for every argument that satisfies `expects`, a
      * result the function returns satisfies `ensures` (partial correctness, design doc §3.7):
      * `∀ x. expects(x) ==> whenReturns(fn, x)(r => ensures(x)(r))`. A failing call satisfies it;
      * use [[totalContract]] to claim that the function returns.
      *
      * {{{
      * contract(div10)(expects = x => x != BigInt(0), ensures = x => r => r * x <= BigInt(10))
      * }}}
      */
    inline def contract[A: Quantifiable, R](fn: FunctionDef[A, R])(
        inline expects: A => Prop | Boolean,
        inline ensures: A => R => Prop | Boolean
    ): Contract[A, R] = ${ PropMacro.contract('fn, 'expects, 'ensures, false) }

    /** The contract of a function of two parameters; see [[contract]]. */
    inline def contract[A: Quantifiable, B: Quantifiable, R](fn: FunctionDef[(A, B), R])(
        inline expects: (A, B) => Prop | Boolean,
        inline ensures: (A, B) => R => Prop | Boolean
    ): Contract[(A, B), R] = ${ PropMacro.contract('fn, 'expects, 'ensures, false) }

    /** The contract of a function of three parameters; see [[contract]].
      *
      * {{{
      * contract(clamp)(
      *   expects = (x, lo, hi) => lo <= hi,
      *   ensures = (x, lo, hi) => r => lo <= r && r <= hi
      * )
      * }}}
      */
    inline def contract[A: Quantifiable, B: Quantifiable, C: Quantifiable, R](
        fn: FunctionDef[(A, B, C), R]
    )(
        inline expects: (A, B, C) => Prop | Boolean,
        inline ensures: (A, B, C) => R => Prop | Boolean
    ): Contract[(A, B, C), R] = ${ PropMacro.contract('fn, 'expects, 'ensures, false) }

    /** The contract of a function of one parameter, with totality: for every argument that
      * satisfies `expects`, the function returns, and its result satisfies `ensures`.
      */
    inline def totalContract[A: Quantifiable, R](fn: FunctionDef[A, R])(
        inline expects: A => Prop | Boolean,
        inline ensures: A => R => Prop | Boolean
    ): Contract[A, R] = ${ PropMacro.contract('fn, 'expects, 'ensures, true) }

    /** The total contract of a function of two parameters; see [[totalContract]]. */
    inline def totalContract[A: Quantifiable, B: Quantifiable, R](fn: FunctionDef[(A, B), R])(
        inline expects: (A, B) => Prop | Boolean,
        inline ensures: (A, B) => R => Prop | Boolean
    ): Contract[(A, B), R] = ${ PropMacro.contract('fn, 'expects, 'ensures, true) }

    /** The total contract of a function of three parameters; see [[totalContract]]. */
    inline def totalContract[A: Quantifiable, B: Quantifiable, C: Quantifiable, R](
        fn: FunctionDef[(A, B, C), R]
    )(
        inline expects: (A, B, C) => Prop | Boolean,
        inline ensures: (A, B, C) => R => Prop | Boolean
    ): Contract[(A, B, C), R] = ${ PropMacro.contract('fn, 'expects, 'ensures, true) }

    /** `e` returns a value: [[denotes]], under the name that pairs with [[fails]]. */
    inline def succeeds[A](inline e: A): Prop = denotes(e)

    /** `e` does not return a value: `!denotes(e)`. It is about how the evaluation of `e` ends, not
      * about a Boolean it returns: `fails(x > 0)` holds for no `x`. A bounded tactic proves it by
      * showing that `e` fails within its budget, which it tells apart from an exhausted one.
      *
      * {{{
      * forAll[BigInt](x => (x == BigInt(0)) ==> fails(BigInt(10) / x))
      * forAll[BigInt](x => (x < BigInt(0)) ==> fails { require(x >= BigInt(0)); x * BigInt(2) })
      * }}}
      */
    inline def fails[A](inline e: A): Prop = !denotes(e)

    /** `fn` applied to `arg` returns: the total call `call(fn, arg)(_ => true)`. It is [[succeeds]]
      * of an expression for a function in the table, which runs its own program.
      */
    inline def succeeds[A, R](fn: FunctionDef[A, R], inline arg: A): Prop =
        call(fn, arg)(_ => true)

    /** `fn` applied to `arg` does not return: `!succeeds(fn, arg)`. */
    inline def fails[A, R](fn: FunctionDef[A, R], inline arg: A): Prop = !succeeds(fn, arg)

    /** A function of one parameter returns on every argument that satisfies `when`:
      * `∀ x. when(x) ==> succeeds(fn, x)`.
      */
    inline def returnsWhen[A: Quantifiable, R](fn: FunctionDef[A, R])(
        inline when: A => Prop | Boolean
    ): Prop = ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, false) }

    /** A function of two parameters returns on all arguments that satisfy `when`. */
    inline def returnsWhen[A: Quantifiable, B: Quantifiable, R](fn: FunctionDef[(A, B), R])(
        inline when: (A, B) => Prop | Boolean
    ): Prop = ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, false) }

    /** A function of three parameters returns on all arguments that satisfy `when`. */
    inline def returnsWhen[A: Quantifiable, B: Quantifiable, C: Quantifiable, R](
        fn: FunctionDef[(A, B, C), R]
    )(inline when: (A, B, C) => Prop | Boolean): Prop =
        ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, false) }

    /** A function of one parameter fails on every argument that satisfies `when`:
      * `∀ x. when(x) ==> fails(fn, x)`. It states what the function must reject, as its runtime
      * `require`s do in its code.
      *
      * {{{
      * failsWhen(div10)(x => x == BigInt(0))
      * }}}
      */
    inline def failsWhen[A: Quantifiable, R](fn: FunctionDef[A, R])(
        inline when: A => Prop | Boolean
    ): Prop = ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, true) }

    /** A function of two parameters fails on all arguments that satisfy `when`. */
    inline def failsWhen[A: Quantifiable, B: Quantifiable, R](fn: FunctionDef[(A, B), R])(
        inline when: (A, B) => Prop | Boolean
    ): Prop = ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, true) }

    /** A function of three parameters fails on all arguments that satisfy `when`. */
    inline def failsWhen[A: Quantifiable, B: Quantifiable, C: Quantifiable, R](
        fn: FunctionDef[(A, B, C), R]
    )(inline when: (A, B, C) => Prop | Boolean): Prop =
        ${ PropMacro.returnsOrFailsWhen('{ fn.ref }, 'when, true) }

    /** `prop` with the statement variables in `names` renamed, in its expressions and binders. */
    private[verify] def renameVariables(prop: Prop, names: Map[String, String]): Prop = {
        def expression[A](expr: PropExpr[A]): PropExpr[A] = expr match
            case PropExpr.SIRExpr(sir)    => PropExpr.SIRExpr(SIR.renameFreeVars(sir, names))
            case ident: PropExpr.Ident[A] => identifier(ident)
        def identifier[A](ident: PropExpr.Ident[A]): PropExpr.Ident[A] =
            names
                .get(ident.name)
                .fold(ident)(name => new PropExpr.Ident[A](name, ident.id, ident.tp))
        def loop(current: Prop): Prop = current match
            case Prop.Bool(expr)         => Prop.Bool(expression(expr))
            case Prop.Denotes(expr)      => Prop.Denotes(expression(expr))
            case Prop.Equal(left, right) => Prop.Equal(expression(left), expression(right))
            case Prop.Call(fn, arg, result, total, body) =>
                Prop.Call(fn, expression(arg), identifier(result), total, loop(body))
            case Prop.Forall(ident, body) => Prop.Forall(identifier(ident), loop(body))
            case Prop.Exists(ident, witness, body) =>
                Prop.Exists(identifier(ident), witness.map(expression), loop(body))
            case Prop.And(left, right)     => Prop.And(loop(left), loop(right))
            case Prop.Or(left, right)      => Prop.Or(loop(left), loop(right))
            case Prop.Implies(left, right) => Prop.Implies(loop(left), loop(right))
            case Prop.Iff(left, right)     => Prop.Iff(loop(left), loop(right))
            case Prop.Not(inner)           => Prop.Not(loop(inner))
        if names.isEmpty then prop else loop(prop)
    }

    /** The SIR type of a statement variable, from the compiled identity lambda `(v: A) => v`. */
    private[verify] def variableType(identity: SIR): SIRType =
        compiledLambdas("a statement variable", identity, 1)._1.head.tp

    /** An expression of the statement from the closed lambda it was compiled as: the lambda's body,
      * with each parameter named after the statement variable it stands for, in order.
      */
    private[verify] def openVariables(lambda: SIR, names: List[String]): SIR = {
        val (params, body) = compiledLambdas("an expression of a statement", lambda, names.size)
        SIR.renameFreeVars(body, params.map(_.name).zip(names).toMap)
    }

    /** The parameters of a compiled curried lambda of `arity` parameters, and its body.
      *
      * Compiler supplied definitions surround a lambda when its body references a `@Compile`
      * method; they move inside all of its parameters. The statement keeps those references as
      * `ExternalVar`s, which a backend resolves from its function table using the representation it
      * consumes.
      */
    private def compiledLambdas(kind: String, sir: SIR, arity: Int): (List[SIR.Var], SIR) = {
        def parameters(current: SIR, remaining: Int): (List[SIR.Var], SIR) =
            if remaining == 0 then Nil -> current
            else
                current match
                    case SIR.LamAbs(param, term, Nil, _) =>
                        val (rest, body) = parameters(term, remaining - 1)
                        (param :: rest) -> body
                    case other =>
                        throw new IllegalArgumentException(
                          s"$kind requires a monomorphic SIR lambda of $arity parameters: $other"
                        )

        val (wrappers, root) = Wrappers.of(sir)
        root match
            case lambda: SIR.LamAbs =>
                val (params, term) = parameters(lambda, arity)
                params -> wrappers(term)
            case other =>
                throw new IllegalArgumentException(
                  s"$kind requires a monomorphic SIR lambda: $other"
                )
    }

    inline def denotes[A](inline e: A): Prop = ${ PropMacro.denotes('e) }

    inline def equal[A](inline a: A, inline b: A): Prop = ${ PropMacro.equal('a, 'b) }

    implicit inline def booleanToProp(inline b: Boolean): Prop = ${ PropMacro.test('b) }

    extension (inline b: Boolean) {
        inline def ==>(q: Prop): Prop = Prop.Implies(Prop(b), q)
        infix inline def implies(q: Prop): Prop = Prop.Implies(Prop(b), q)
    }
}

object Prop {

    /** Constructs a proposition from a Boolean expression without executing it. */
    inline def apply(inline b: Boolean): Prop = ${ PropMacro.test('b) }
}
