package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.*

/** Audit findings T5/T6 (UPLC correctness audit): variable capture and shadowing in type-level
  * renaming and substitution.
  *
  * T5: `SIRType.substitute` removed shadowed binders from the environment but performed no capture
  * avoidance — substituting `A -> Fun(B, _)` into `[B] =>> Fun(A, B)` captured the replacement's
  * `B` under the lambda's binder.
  *
  * T6: `RenamingTypeVars.inType` renamed a shadowing TypeLambda binder using the outer rename map
  * while the body (correctly) kept the original variable — the binder bound nothing and the body's
  * occurrences dangled.
  */
class TypeVarCaptureTest extends AnyFunSuite {

    private val A = TypeVar("A", Some(1L), TypeVarKind.Fixed)
    private val B = TypeVar("B", Some(2L), TypeVarKind.Fixed)

    test("T6: renaming does not touch a shadowing TypeLambda binder or its bound occurrences") {
        val A1 = TypeVar("A", Some(101L), TypeVarKind.Fixed)
        val B1 = TypeVar("B", Some(102L), TypeVarKind.Fixed)
        // [A] =>> Fun(A, B), rename {A -> A1, B -> B1}: A is shadowed, B is free
        val tl = TypeLambda(scala.List(A), Fun(A, B))
        val tvGen = createMinimalTypeVarGenerationContext(0L, scala.List(tl, A1, B1))
        val ctx = RenamingTypeVars.makeContext(Map(A -> A1, B -> B1), tvGen)
        RenamingTypeVars.inType(tl, ctx) match
            case TypeLambda(scala.List(p), Fun(x, y)) =>
                assert(p == A, "shadowing binder must not be renamed")
                assert(x == A, "occurrence bound by the shadowing binder must not be renamed")
                assert(y == B1, "free variable in the body must be renamed")
            case other => fail(s"Unexpected renaming result: $other")
    }

    test("T6: renaming still renames free variables under a non-shadowing TypeLambda") {
        val B1 = TypeVar("B", Some(102L), TypeVarKind.Fixed)
        val tl = TypeLambda(scala.List(A), Fun(A, B))
        val tvGen = createMinimalTypeVarGenerationContext(0L, scala.List(tl, B1))
        val ctx = RenamingTypeVars.makeContext(Map(B -> B1), tvGen)
        assert(RenamingTypeVars.inType(tl, ctx) == TypeLambda(scala.List(A), Fun(A, B1)))
    }

    test("T5: substitute alpha-renames a binder that would capture the replacement") {
        // [B] =>> Fun(A, B) under A -> Fun(B, Integer):
        // the replacement's B is free and must not be captured by the lambda's B
        val tl = TypeLambda(scala.List(B), Fun(A, B))
        val replacement = Fun(B, Integer)
        SIRType.substitute(tl, Map(A -> replacement), Map.empty) match
            case TypeLambda(scala.List(p), Fun(in, out)) =>
                assert(p != B, "binder must be freshened to avoid capturing the replacement")
                assert(in == replacement, "replacement must keep its original free variable")
                assert(out == p, "occurrences bound by the binder must follow the renaming")
            case other => fail(s"Unexpected substitution result: $other")
    }

    test("T5: substitute keeps the binder when there is no capture") {
        val tl = TypeLambda(scala.List(B), Fun(A, B))
        assert(
          SIRType.substitute(tl, Map(A -> Integer), Map.empty) ==
              TypeLambda(scala.List(B), Fun(Integer, B))
        )
    }

    test("T5: substitute does not substitute under a shadowing binder") {
        val tl = TypeLambda(scala.List(A), Fun(A, B))
        assert(SIRType.substitute(tl, Map(A -> Integer), Map.empty) == tl)
    }
}
