package scalus.verify

import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** A logical statement about Scalus code: typed quantifiers, connectives, atoms and calls.
  *
  * See `docs/design/verification-overview.md`, §3. Statements are built with the combinators of
  * this package ([[forAll]], [[exists]], [[existsWith]], [[call]], [[whenReturns]], [[denotes]],
  * [[equal]]) and the connectives below. Quantifiers use higher-order abstract syntax: the bound
  * variable is the parameter of an ordinary Scala lambda, so the body is ordinary Scala that scalac
  * type-checks.
  *
  * A value of this type is the statement's runtime form. [[Prop.check]] evaluates it on drawn
  * values, which gives evidence, never proof. The proving tactics start from the statement's SIR
  * instead, because a Scala closure cannot be inspected at runtime.
  *
  * Leaves built from Scala expressions (atoms, `denotes`, `equal`) carry a debug name: their source
  * text and position, recorded at compile time. A call names its function in a [[FunctionTable]].
  *
  * Semantics (§3.3): an atom holds iff it evaluates to `true`, and an exception is not `true`.
  * [[denotes]] states separately that an expression evaluates without an exception. Connectives are
  * classical. `!` on a `Prop` is logical negation; `!` inside a Boolean expression is part of the
  * atom. The two differ when the expression can fail.
  *
  * Precedence: Scala ranks an operator by its first character, so `==>` binds like `==` and `<=>`
  * like `<`, both tighter than `&&` and `||`. `p && q ==> r` therefore means `p && (q ==> r)`.
  * Parenthesize, or use the alphanumeric [[implies]] and [[iff]], which bind loosest of all.
  */
enum Prop {

    /** A Boolean expression. `name` is its source text and position, used only in reports. */
    case Atom(name: String, value: () => Boolean)
    case Denotes(name: String, value: () => Any)
    case Equal(name: String, a: () => Any, b: () => Any)

    /** A call of the function `fn` names in the [[FunctionTable]], continuing with its result. With
      * `total`, the call must return (total correctness). Without it, a call that fails satisfies
      * the proposition (partial correctness).
      */
    case Call[A, R](fn: FunctionRef[A, R], arg: () => A, total: Boolean, body: R => Prop)
    case Forall[A](quantifiable: Quantifiable[A], body: A => Prop)
    case Exists[A](quantifiable: Quantifiable[A], witness: Option[() => A], body: A => Prop)
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

/** For every value of `A`, `body` holds. */
def forAll[A: Quantifiable](body: A => Prop): Prop = Prop.Forall(summon[Quantifiable[A]], body)

/** Some value of `A` makes `body` hold. */
def exists[A: Quantifiable](body: A => Prop): Prop =
    Prop.Exists(summon[Quantifiable[A]], None, body)

/** Some value of `A` makes `body` hold, and `witness` is one.
  *
  * The witness may mention the variables bound around it, which makes it a Skolem function. The
  * statement is then checked, and later proved, in that Skolemized form: `body(witness)` must hold.
  * No search, and no quantifier alternation for a solver to face.
  */
def existsWith[A: Quantifiable](witness: => A)(body: A => Prop): Prop =
    Prop.Exists(summon[Quantifiable[A]], Some(() => witness), body)

/** `fn` applied to `arg` returns, and its result satisfies `body` (total correctness). */
def call[A, R](fn: FunctionRef[A, R], arg: => A)(body: R => Prop): Prop =
    Prop.Call(fn, () => arg, total = true, body)

/** [[call]] through a table entry. The statement stores only the entry's name. */
def call[A, R](fn: FunctionDef[A, R], arg: => A)(body: R => Prop): Prop = call(fn.ref, arg)(body)

/** Whenever `fn` applied to `arg` returns, its result satisfies `body` (partial correctness). A
  * call that fails satisfies it. This is the shape of a function contract's postcondition (§3.7).
  */
def whenReturns[A, R](fn: FunctionRef[A, R], arg: => A)(body: R => Prop): Prop =
    Prop.Call(fn, () => arg, total = false, body)

/** [[whenReturns]] through a table entry. The statement stores only the entry's name. */
def whenReturns[A, R](fn: FunctionDef[A, R], arg: => A)(body: R => Prop): Prop =
    whenReturns(fn.ref, arg)(body)

/** `e` evaluates without an exception, the JVM image of an on-chain error. */
inline def denotes[A](inline e: A): Prop = ${ PropMacro.denotes('e) }

/** `a` and `b` are equal values. */
inline def equal[A](inline a: A, inline b: A): Prop = ${ PropMacro.equal('a, 'b) }

/** Makes a Boolean expression an atom explicitly, e.g. to negate it at the `Prop` level. */
inline def atom(inline b: Boolean): Prop = ${ PropMacro.atom('b) }

/** A Boolean expression where a [[Prop]] is expected is an atom, named after its source text.
  *
  * The expression is evaluated when the atom is, not when the statement is built, so an exception
  * from it makes the atom false instead of escaping. A premise that fails therefore makes its
  * implication hold, as on-chain.
  */
implicit inline def booleanToProp(inline b: Boolean): Prop = ${ PropMacro.atom('b) }

extension (inline b: Boolean) {

    /** `cond ==> p` with a Boolean condition, without an explicit [[atom]]. */
    inline def ==>(q: Prop): Prop = Prop.Implies(atom(b), q)

    /** [[==>]] with the lowest precedence: `a > 0 && b > 0 implies p` groups the conjunction. */
    infix inline def implies(q: Prop): Prop = Prop.Implies(atom(b), q)
}

object Prop {

    /** A call made while checking, as it appears in a counterexample. `function` is the full name;
      * the string form shows its last segment.
      */
    final case class CallResult(function: FunctionRef[?, ?], argument: Any, result: Any) {
        override def toString: String = {
            val args = argument match
                case tuple: Tuple => tuple.productIterator.mkString(", ")
                case single       => single.toString
            s"${function.displayName}($args) = $result"
        }
    }

    /** The result of [[Prop.check]]: evidence from drawn values, never a proof. */
    enum CheckResult {

        /** No drawn value falsified the statement. */
        case Passed

        /** `counterexample` holds the values bound on the falsifying path, outermost first,
          * including witnesses and [[CallResult]]s. `falsifiedBy` names the leaf or call that
          * decided it, and `failure` is the exception behind it, if there was one.
          */
        case Falsified(counterexample: List[Any], falsifiedBy: String, failure: Option[Throwable])

        /** Neither established nor refuted, typically an existential no drawn value satisfied. */
        case Undetermined(reason: String)
    }

    /** [[check]] of a statement that calls no function. */
    def check(p: Prop): CheckResult = check(p, FunctionTable.empty)

    /** [[check]] with 100 samples and seed 0. */
    def check(p: Prop, functions: FunctionTable): CheckResult = check(p, functions, 100, 0L)

    /** Evaluates `p` on drawn values: each quantifier tries its edge cases, then random values
      * derived from `seed`, so a run is reproducible. Calls go to the `scalacheck` representation
      * of each function in `functions`; a call to a function missing from it, or without that
      * representation, throws.
      *
      * The outermost quantifier draws `samples` values. A nested one draws a quarter as many as its
      * parent, but at least 4, so the cost stays polynomial in the nesting depth.
      *
      * The evaluation is three-valued. A universal holds when no drawn value falsifies it; an
      * existential without a witness holds when a drawn value satisfies it, and is undetermined
      * otherwise. An exception while evaluating a leaf, a call or a quantifier body makes that part
      * false. That is the semantics, not a fallback: on the JVM, a Scalus program signals an
      * on-chain error with an exception. Only non-fatal exceptions are caught.
      */
    def check(p: Prop, functions: FunctionTable, samples: Int, seed: Long): CheckResult = {
        require(samples > 0, s"samples must be positive, got $samples")
        new Evaluator(functions, samples, Seed(seed)).eval(p, 0, Nil) match
            case Truth.True => CheckResult.Passed
            case Truth.False(trail, by, failure) =>
                CheckResult.Falsified(trail.reverse, by, failure)
            case Truth.Unknown(reason) => CheckResult.Undetermined(reason)
    }

    private enum Truth {
        case True

        /** `trail` holds the bound values, innermost first. */
        case False(trail: List[Any], by: String, failure: Option[Throwable])
        case Unknown(reason: String)
    }

    private final class Evaluator(functions: FunctionTable, samples: Int, initialSeed: Seed) {
        private var seed = initialSeed

        def eval(p: Prop, depth: Int, trail: List[Any]): Truth = p match
            case Atom(name, value) =>
                attempt(value()) match
                    case Right(true)  => Truth.True
                    case Right(false) => Truth.False(trail, name, None)
                    case Left(error)  => Truth.False(trail, name, Some(error))
            case Denotes(name, value) =>
                attempt(value()) match
                    case Right(_)    => Truth.True
                    case Left(error) => Truth.False(trail, s"denotes $name", Some(error))
            case Equal(name, a, b) =>
                attempt(a() == b()) match
                    case Right(true)  => Truth.True
                    case Right(false) => Truth.False(trail, name, None)
                    case Left(error)  => Truth.False(trail, name, Some(error))
            case call: Call[a, r] => evalCall(call, depth, trail)
            case Not(a) =>
                eval(a, depth, trail) match
                    case Truth.True     => Truth.False(trail, "the negated proposition holds", None)
                    case _: Truth.False => Truth.True
                    case unknown        => unknown
            case And(a, b) =>
                eval(a, depth, trail) match
                    case Truth.True             => eval(b, depth, trail)
                    case falsified: Truth.False => falsified
                    case unknown =>
                        eval(b, depth, trail) match
                            case falsified: Truth.False => falsified
                            case _                      => unknown
            case Or(a, b) =>
                eval(a, depth, trail) match
                    case Truth.True     => Truth.True
                    case _: Truth.False => eval(b, depth, trail)
                    case unknown =>
                        eval(b, depth, trail) match
                            case Truth.True => Truth.True
                            case _          => unknown
            case Implies(a, b)     => eval(Or(Not(a), b), depth, trail)
            case Iff(a, b)         => eval(And(Implies(a, b), Implies(b, a)), depth, trail)
            case forall: Forall[a] => evalForall(forall.quantifiable, forall.body, depth, trail)
            case exists: Exists[a] =>
                exists.witness match
                    case Some(witness) =>
                        attempt(witness()) match
                            case Right(w) => evalBody(exists.body, w, depth, trail)
                            case Left(error) =>
                                Truth.False(trail, "the existential's witness", Some(error))
                    case None => evalExists(exists.quantifiable, exists.body, depth, trail)

        private def evalCall[A, R](call: Call[A, R], depth: Int, trail: List[Any]): Truth = {
            // outside `attempt`: a function missing from the table, or without a scalacheck
            // representation, is a setup error, not a property of the code
            val impl = functions(call.fn).scalacheck.getOrElse(
              throw new NoSuchElementException(
                s"function ${call.fn.name} has no scalacheck representation, which Prop.check needs"
              )
            )
            val returned = attempt(call.arg()).flatMap(arg => attempt(impl(arg)).map((arg, _)))
            returned match
                case Right((arg, result)) =>
                    val bound = CallResult(call.fn, arg, result) :: trail
                    attempt(call.body(result)) match
                        case Right(p) => eval(p, depth, bound)
                        case Left(error) =>
                            Truth.False(
                              bound,
                              s"the continuation of ${call.fn.displayName}",
                              Some(error)
                            )
                case Left(error) =>
                    if call.total then
                        Truth.False(trail, s"${call.fn.displayName} did not return", Some(error))
                    else Truth.True
        }

        private def evalForall[A](
            q: Quantifiable[A],
            body: A => Prop,
            depth: Int,
            trail: List[Any]
        ): Truth = {
            @tailrec
            def loop(values: List[A], unknown: Option[Truth]): Truth = values match
                case Nil => unknown.getOrElse(Truth.True)
                case x :: rest =>
                    evalBody(body, x, depth, trail) match
                        case falsified: Truth.False => falsified
                        case Truth.True             => loop(rest, unknown)
                        case other                  => loop(rest, unknown.orElse(Some(other)))
            loop(draw(q, depth), None)
        }

        private def evalExists[A](
            q: Quantifiable[A],
            body: A => Prop,
            depth: Int,
            trail: List[Any]
        ): Truth = {
            @tailrec
            def loop(values: List[A]): Truth = values match
                case Nil =>
                    Truth.Unknown(
                      "no drawn value satisfies the existential; supply one with existsWith"
                    )
                case x :: rest =>
                    evalBody(body, x, depth, trail) match
                        case Truth.True => Truth.True
                        case _          => loop(rest)
            loop(draw(q, depth))
        }

        private def evalBody[A](body: A => Prop, x: A, depth: Int, trail: List[Any]): Truth = {
            val bound = x :: trail
            attempt(body(x)) match
                case Right(p)    => eval(p, depth + 1, bound)
                case Left(error) => Truth.False(bound, "a quantifier's body", Some(error))
        }

        private def draw[A](q: Quantifiable[A], depth: Int): List[A] = {
            val count = if depth == 0 then samples else math.max(4, samples >> (2 * depth).min(30))
            val edges = q.edgeCases.take(count)
            val randomCount = count - edges.size
            val randoms = List.tabulate(randomCount) { i =>
                val params = Gen.Parameters.default.withSize(1 + i * 100 / randomCount)
                val value = q.gen.pureApply(params, seed)
                seed = seed.next
                value
            }
            edges ++ randoms
        }
    }

    private def attempt[T](thunk: => T): Either[Throwable, T] =
        try Right(thunk)
        catch case NonFatal(error) => Left(error)
}
