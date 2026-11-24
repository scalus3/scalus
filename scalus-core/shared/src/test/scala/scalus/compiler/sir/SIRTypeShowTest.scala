package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.{Fun, TypeLambda, TypeVar, TypeVarKind}

class SIRTypeShowTest extends AnyFunSuite {

    private val A = TypeVar("A", Some(1L), TypeVarKind.Fixed)

    /** `([A] =>> A -> A) -> Int`: a function that takes a polymorphic function. */
    private val takesPolymorphicFunction = Fun(TypeLambda(List(A), Fun(A, A)), SIRType.Integer)

    test("show prints the result type of a function that takes a polymorphic function") {
        assert(takesPolymorphicFunction.show.endsWith(") -> Int"))
    }

    test("showDebug prints the result type of a function that takes a polymorphic function") {
        assert(takesPolymorphicFunction.showDebug.endsWith(") -> Int"))
    }

    /** `([A] =>> A) -> Int`: a function whose argument is a type lambda. */
    private val takesTypeLambda = Fun(TypeLambda(List(A), A), SIRType.Integer)

    /** `[A] =>> (A -> Int)`: a type lambda over a function. */
    private val typeLambdaOverFunction = TypeLambda(List(A), Fun(A, SIRType.Integer))

    test("show tells a type lambda in argument position from a type lambda over the function") {
        assert(takesTypeLambda.show != typeLambdaOverFunction.show)
    }

    test(
      "showDebug tells a type lambda in argument position from a type lambda over the function"
    ) {
        assert(takesTypeLambda.showDebug != typeLambdaOverFunction.showDebug)
    }
}
