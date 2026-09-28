package scalus.verify

import scalus.compiler.sir.{SIR, SIRType}
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

object Props {

    /** Constructs an explicit universal quantifier from an identifier and a proposition. */
    def forAllSIR[A](ident: PropExpr.Ident[A], body: Prop): Prop = Prop.Forall(ident, body)

    /** Source syntax for a Boolean universal property. The Scala lambda is compiled to SIR. */
    inline def forAll[A: Quantifiable](inline body: A => Boolean): Prop =
        ${ PropMacro.forAll[A]('body) }

    /** Unpacks the compiled lambda and keeps its SIR variable in the Boolean body. */
    private[verify] def compiledForAll[A](lambda: SIR, id: Long): Prop = lambda match
        case SIR.LamAbs(param, term, Nil, _) =>
            forAllSIR(
              PropExpr.Ident[A](param.name, id, param.tp),
              Prop.Bool(PropExpr.SIRExpr(term))
            )
        case other =>
            throw new IllegalArgumentException(s"forAll requires a monomorphic SIR lambda: $other")

    /** Constructs an explicit existential quantifier from an identifier and a proposition. */
    def existsSIR[A](
        ident: PropExpr.Ident[A],
        witness: Option[PropExpr[A]],
        body: Prop
    ): Prop = Prop.Exists(ident, witness, body)

    /** Some value of `A` makes the Boolean body hold. */
    inline def exists[A: Quantifiable](inline body: A => Boolean): Prop =
        ${ PropMacro.exists[A]('body) }

    /** Some value of `A` makes the Boolean body hold, and `witness` supplies that value. */
    inline def existsLet[A: Quantifiable](inline witness: A)(inline body: A => Boolean): Prop =
        ${ PropMacro.existsLet[A]('witness, 'body) }

    /** Unpacks the compiled lambda and keeps its SIR variable in the Boolean body. */
    private[verify] def compiledExists[A](
        lambda: SIR,
        id: Long,
        witness: Option[PropExpr[A]]
    ): Prop = lambda match
        case SIR.LamAbs(param, term, Nil, _) =>
            existsSIR(
              PropExpr.Ident[A](param.name, id, param.tp),
              witness,
              Prop.Bool(PropExpr.SIRExpr(term))
            )
        case other =>
            throw new IllegalArgumentException(s"exists requires a monomorphic SIR lambda: $other")

    /** `fn` applied to `arg` returns, and its result satisfies the Boolean body. */
    inline def callRef[A, R](fn: FunctionRef[A, R], inline arg: A)(
        inline body: R => Boolean
    ): Prop = ${ PropMacro.call('fn, 'arg, 'body, true) }

    /** Names a method of a `@Compile` object and states a property of its result. */
    inline def call[A, R](inline f: A => R, inline arg: A)(
        inline body: R => Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, B, R](inline f: (A, B) => R, inline arg: (A, B))(
        inline body: R => Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, B, C, R](inline f: (A, B, C) => R, inline arg: (A, B, C))(
        inline body: R => Boolean
    ): Prop = callRef(FunctionRef(f), arg)(body)

    inline def call[A, R](fn: FunctionDef[A, R], inline arg: A)(
        inline body: R => Boolean
    ): Prop = ${ PropMacro.callDef('fn, 'arg, 'body, true) }

    /** Whenever `fn` applied to `arg` returns, its result satisfies the Boolean body. */
    inline def whenReturnsRef[A, R](fn: FunctionRef[A, R], inline arg: A)(
        inline body: R => Boolean
    ): Prop = ${ PropMacro.call('fn, 'arg, 'body, false) }

    inline def whenReturns[A, R](inline f: A => R, inline arg: A)(
        inline body: R => Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, B, R](inline f: (A, B) => R, inline arg: (A, B))(
        inline body: R => Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, B, C, R](inline f: (A, B, C) => R, inline arg: (A, B, C))(
        inline body: R => Boolean
    ): Prop = whenReturnsRef(FunctionRef(f), arg)(body)

    inline def whenReturns[A, R](fn: FunctionDef[A, R], inline arg: A)(
        inline body: R => Boolean
    ): Prop = ${ PropMacro.callDef('fn, 'arg, 'body, false) }

    /** Unpacks the compiled continuation lambda and keeps its result variable in the body. */
    private[verify] def compiledCall[A, R](
        fn: FunctionRef[A, R],
        arg: PropExpr[A],
        lambda: SIR,
        id: Long,
        total: Boolean
    ): Prop = lambda match
        case SIR.LamAbs(param, term, Nil, _) =>
            Prop.Call(
              fn,
              arg,
              PropExpr.Ident[R](param.name, id, param.tp),
              total,
              Prop.Bool(PropExpr.SIRExpr(term))
            )
        case other =>
            throw new IllegalArgumentException(s"call requires a monomorphic SIR lambda: $other")

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
