package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*

/** A statement's variable hides an outer one of its name in the tests in its scope, as a Scala
  * lambda's parameter does. Lean's variables are named by their places, and a test's program takes
  * them as parameters of their own names, so the innermost of a name is the one a test reads: also
  * where the outer one is a case class, whose value a test builds from its fields, a variable over
  * `Data` encodings, whose value a test decodes, or the variable of an `existsLet`, which a test
  * binds to its witness.
  *
  * The macros name a variable after its lambda's parameter and the place of the lambda in its file.
  * So one inline helper nested in itself gives two variables one name, as do two files with a
  * lambda of one parameter name at one place, and a statement built by hand.
  */
class VariableScopeTest extends AnyFunSuite {
    import VariableScopeTest.*

    private given PlutusVM = PlutusVM.makePlutusV3VM()

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
        val lowered =
            UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)
        assert(lowered.binders.map(_.name).distinct.size == 1, lowered.binders)
        // the inner `x > 0`, over the outer value, then the inner one
        assert(evaluated(prop, BigInt(5).asTerm, BigInt(-1).asTerm) == isFalse)
        assert(evaluated(prop, BigInt(-1).asTerm, BigInt(5).asTerm) == isTrue)
    }
}

object VariableScopeTest {

    /** Of every positive integer `x`, the statement `k(x)`. */
    inline def positive(inline k: BigInt => Prop): Prop =
        forAll[BigInt](x => (x > BigInt(0)) ==> k(x))
}
