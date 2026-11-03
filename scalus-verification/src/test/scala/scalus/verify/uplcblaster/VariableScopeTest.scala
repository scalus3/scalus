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
  * where the outer one is a case class, whose value a test builds from its fields, or a variable
  * over `Data` encodings, whose value a test decodes.
  *
  * The macros name a variable after its lambda's parameter and the place of the lambda in its file,
  * so two variables have one name only when built by hand, or when two files have a lambda of one
  * parameter name at one place.
  */
class VariableScopeTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    /** `inner`, a statement of a BigInt `x` that it equals itself, true of every value, inside a
      * quantifier over a BlasterOwned, or over its Data encodings, renamed to `x`'s name.
      */
    private def hiding(outer: Prop, inner: Prop): Prop = (outer, inner) match
        case (Prop.Forall(owned, body), Prop.Forall(variable, _)) =>
            forAllSIR(owned.copy(name = variable.name), body)
        case other => fail(s"not two quantifiers: $other")

    /** What the one test of `prop` evaluates to on `arguments`, the values of its scope. */
    private def evaluated(prop: Prop, arguments: Term*): Term = {
        val lowered =
            UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)
        assert(lowered.leaves.size == 1, lowered.leaves)
        assert(lowered.leafBinders.head.size == arguments.size, lowered.binders)
        val applied = arguments.foldLeft(lowered.leaves.head)(_ $ _)
        applied.term.evaluateDebug match
            case success: Result.Success => success.term
            case failure: Result.Failure => fail(s"the test failed: ${failure.exception}")
    }

    private val inner: Prop = forAll[BigInt](x => x == x)

    test("a variable hides an outer case-class variable of its name") {
        val prop = hiding(forAll[BlasterOwned](o => inner), inner)
        // the outer variable's two fields, a byte string and an integer, then the inner integer
        val result = evaluated(prop, ByteString.empty.asTerm, BigInt(5).asTerm, BigInt(7).asTerm)
        assert(result == Term.Const(Constant.Bool(true)))
    }

    test("a variable hides an outer variable over Data encodings of its name") {
        val prop = hiding(forAllData[BlasterOwned](o => inner), inner)
        val owned = BlasterOwned(BlasterOwner(ByteString.empty), BigInt(5)).toData
        // the outer variable's Data, then the inner integer
        val result = evaluated(prop, Term.Const(Constant.Data(owned)), BigInt(7).asTerm)
        assert(result == Term.Const(Constant.Bool(true)))
    }
}
