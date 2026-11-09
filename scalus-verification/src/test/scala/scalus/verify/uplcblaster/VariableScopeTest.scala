package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.sir.SIR
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*

/** A statement's variable hides an outer one of its name in its scope, as a Scala lambda's
  * parameter does: a test, an `existsLet`'s witness and a call's argument read the innermost
  * variable of a name. `lower` names the variables of every scope apart before it compiles a test,
  * so a value that a test builds for a case-class variable, decodes for a variable over `Data`
  * encodings or binds to a witness is the right variable's, and a witness is not captured by an
  * inner variable of its name.
  *
  * The macros name a variable after its lambda's parameter and the place of the lambda in its file.
  * So one inline helper nested in itself gives two variables one name, as do two files with a
  * lambda of one parameter name at one place, and a statement built by hand.
  */
class VariableScopeTest extends AnyFunSuite {
    import VariableScopeTest.*

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** The names of the variables `prop` quantifies over, outermost first. */
    private def variables(prop: Prop): List[String] = prop match
        case Prop.Forall(ident, body)    => ident.name :: variables(body)
        case Prop.Exists(ident, _, body) => ident.name :: variables(body)
        case Prop.Implies(left, right)   => variables(left) ++ variables(right)
        case _                           => Nil

    /** `prop` with the names in `renames` replaced, in its variables and its expressions alike, as
      * a statement built by hand could name them.
      */
    private def named(prop: Prop, renames: Map[String, String]): Prop = {
        def ident[T](variable: PropExpr.Ident[T]): PropExpr.Ident[T] =
            renames
                .get(variable.name)
                .fold(variable)(name => new PropExpr.Ident[T](name, variable.id, variable.tp))
        def expression[T](expr: PropExpr[T]): PropExpr[T] = expr match
            case variable: PropExpr.Ident[T] => ident(variable)
            case PropExpr.SIRExpr(sir)       => PropExpr.SIRExpr(SIR.renameFreeVars(sir, renames))
        prop match
            case Prop.Bool(expr)         => Prop.Bool(expression(expr))
            case Prop.Denotes(expr)      => Prop.Denotes(expression(expr))
            case Prop.Equal(left, right) => Prop.Equal(expression(left), expression(right))
            case Prop.Call(fn, arg, result, total, body) =>
                Prop.Call(fn, expression(arg), ident(result), total, named(body, renames))
            case Prop.Forall(variable, body) => Prop.Forall(ident(variable), named(body, renames))
            case Prop.Exists(variable, witness, body) =>
                Prop.Exists(ident(variable), witness.map(expression), named(body, renames))
            case Prop.And(left, right) => Prop.And(named(left, renames), named(right, renames))
            case Prop.Or(left, right)  => Prop.Or(named(left, renames), named(right, renames))
            case Prop.Implies(left, right) =>
                Prop.Implies(named(left, renames), named(right, renames))
            case Prop.Rejects(left, right) =>
                Prop.Rejects(named(left, renames), named(right, renames))
            case Prop.Iff(left, right) => Prop.Iff(named(left, renames), named(right, renames))
            case Prop.Not(inner)       => Prop.Not(named(inner, renames))
    }

    /** `inner` inside `outer`, a quantifier whose variable is renamed to that of `inner`. */
    private def hiding(outer: Prop, inner: Prop): Prop = (outer, inner) match
        case (Prop.Forall(hidden, body), Prop.Forall(variable, _)) =>
            forAllSIR(hidden.copy(name = variable.name), body)
        case (Prop.Exists(hidden, witness @ Some(_), body), Prop.Forall(variable, _)) =>
            Prop.Exists(hidden.copy(name = variable.name), witness, body)
        case other => fail(s"not two quantifiers: $other")

    /** What the test of `prop` over `arguments.size` variables evaluates to on `arguments`. */
    private def evaluated(prop: Prop, arguments: Term*): Term = {
        val lowered =
            UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)
        val leaf = lowered.leafBinders.indexWhere(_.size == arguments.size)
        assert(leaf >= 0, s"${lowered.leafBinders} for ${lowered.binders.map(_.name)}")
        val applied = arguments.foldLeft(lowered.leaves(leaf))(_ $ _)
        applied.term.evaluateDebug match
            case success: Result.Success => success.term
            case failure: Result.Failure => fail(s"the test failed: ${failure.exception}")
    }

    private val isTrue = Term.Const(Constant.Bool(true))
    private val isFalse = Term.Const(Constant.Bool(false))

    /** Of a BigInt `x`: it is 7. */
    private val seven: Prop = forAll[BigInt](x => x == BigInt(7))

    test("a variable hides an outer one of its name and type") {
        val prop = hiding(forAll[BigInt](o => seven), seven)
        // the outer integer, then the inner one
        assert(evaluated(prop, BigInt(5).asTerm, BigInt(7).asTerm) == isTrue)
        assert(evaluated(prop, BigInt(7).asTerm, BigInt(5).asTerm) == isFalse)
    }

    test("a variable hides an outer case-class variable of its name") {
        val prop = hiding(forAll[BlasterOwned](o => seven), seven)
        // the outer variable's two fields, a byte string and an amount of 5, then the inner integer
        val outer = List(ByteString.empty.asTerm, BigInt(5).asTerm)
        assert(evaluated(prop, (outer :+ BigInt(7).asTerm)*) == isTrue)
        assert(evaluated(prop, (outer :+ BigInt(5).asTerm)*) == isFalse)
    }

    test("a variable hides an outer variable over Data encodings of its name") {
        val prop = hiding(forAllData[BlasterOwned](o => seven), seven)
        val owned =
            Term.Const(Constant.Data(BlasterOwned(BlasterOwner(ByteString.empty), 7).toData))
        // the outer variable's Data, whose amount is 7, then the inner integer
        assert(evaluated(prop, owned, BigInt(7).asTerm) == isTrue)
        assert(evaluated(prop, owned, BigInt(5).asTerm) == isFalse)
    }

    test("a variable hides the witness of an outer existsLet of its name") {
        // the witness has two different fields, and the inner variable's test is that they are equal
        val equal: Prop = forAll[BlasterPair](p => p.a == p.b)
        val prop = hiding(existsLet(BlasterPair(BigInt(1), BigInt(2)))(w => equal), equal)
        // the inner variable's two fields
        assert(evaluated(prop, BigInt(3).asTerm, BigInt(3).asTerm) == isTrue)
        assert(evaluated(prop, BigInt(3).asTerm, BigInt(4).asTerm) == isFalse)
    }

    test("one inline helper nested in itself gives its variables one name, and each test its own") {
        val prop = positive(_ => positive(_ => Prop(true)))
        // the macros name both alike, and lower names them apart, written as before
        assert(variables(prop).distinct.size == 1, variables(prop))
        val lowered =
            UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)
        assert(lowered.binders.map(_.name).distinct.size == 2, lowered.binders)
        assert(lowered.binders.map(binder => PropIdentity.written(binder.name)) == List("x", "x"))
        // the inner `x > 0`, over the outer value, then the inner one
        assert(evaluated(prop, BigInt(5).asTerm, BigInt(-1).asTerm) == isFalse)
        assert(evaluated(prop, BigInt(-1).asTerm, BigInt(5).asTerm) == isTrue)
    }

    test("an existsLet's witness reads the outer variable, not an inner one of its name") {
        // for every x, w = x is such that every y is w: false, unless the witness is captured
        val written = forAll[BigInt](x => existsLet(x)(w => forAll[BigInt](y => w == y)))
        val List(x, _, y) = variables(written): @unchecked
        val prop = named(written, Map(y -> x))
        // the outer x, then the inner variable, which has x's name
        assert(evaluated(prop, BigInt(1).asTerm, BigInt(2).asTerm) == isFalse)
        assert(evaluated(prop, BigInt(2).asTerm, BigInt(2).asTerm) == isTrue)
    }

    test("a variable named as a field of an outer one reads its own value, and the field its own") {
        // the outer `z` of BlasterOwned, and the inner integer `z.amount`
        val written = forAll[BlasterOwned](o => forAll[BigInt](n => o.amount == n))
        val List(o, n) = variables(written): @unchecked
        val prop = named(written, Map(o -> "z", n -> "z.amount"))
        // the outer variable's two fields, then the inner integer
        assert(
          evaluated(prop, ByteString.empty.asTerm, BigInt(5).asTerm, BigInt(7).asTerm) == isFalse
        )
        assert(
          evaluated(prop, ByteString.empty.asTerm, BigInt(7).asTerm, BigInt(7).asTerm) == isTrue
        )
    }

    test("through an inline helper, a test cannot read the variable of an outer expansion") {
        val prop = positive(a => positive(b => a == b))
        val refused = UplcBlaster.lower(prop, FunctionTable.empty)
        assert(refused.left.exists(_.contains("which is no variable of the statement")), refused)
    }
}

object VariableScopeTest {

    /** Of every positive integer `x`, the statement `k(x)`. */
    inline def positive(inline k: BigInt => Prop): Prop =
        forAll[BigInt](x => (x > BigInt(0)) ==> k(x))
}
