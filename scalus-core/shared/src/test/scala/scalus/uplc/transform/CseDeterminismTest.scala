package scalus.uplc
package transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.transform.CommonSubexpressionElimination.TermKey

/** Guards against run-dependent optimizer output.
  *
  * `DefaultFun` extends `java.lang.Enum`, whose `hashCode` is identity-based and differs between
  * JVM runs and between environments. If that hash leaks into the CSE/CCE candidate hash maps, the
  * iteration order of candidates - and hence the extraction order of candidates that tie on the
  * `(size, key.toString)` sort key - becomes run-dependent, and so does the compiled script.
  *
  * See docs/internal/UPLC_OPTIMIZER_DETERMINISM.md for the investigation.
  */
class CseDeterminismTest extends AnyFunSuite {

    /** A field-access chain whose printed form is longer than the 60 characters `showShort` keeps,
      * so two chains over different variables tie on the sort key.
      */
    private def chain(v: String): Term =
        Apply(
          Var(NamedDeBruijn("__HeadList")),
          Apply(
            Var(NamedDeBruijn("__TailList")),
            Apply(
              Var(NamedDeBruijn("__TailList")),
              Apply(
                Var(NamedDeBruijn("__SndPair")),
                Apply(Builtin(UnConstrData), Var(NamedDeBruijn(v)))
              )
            )
          )
        )

    private def add(a: Term, b: Term): Term = Apply(Apply(Builtin(AddInteger), a), b)

    test("TermKey.structuralHash of a builtin term is a fixed value, not an identity hash") {
        // (HeadList.ordinal * 31 + 7) * 31 + 4 with ordinal 35. Re-pin if DefaultFun ordinals ever change.
        // Before the fix this was `HeadList.hashCode`-based, i.e. an identity hash.
        assert(new TermKey(Force(Builtin(HeadList))).hashCode == 33856)
    }

    test("tied candidates are extracted in first-occurrence order") {
        // Two same-size candidates over x and y, bound in the same scope, each occurring twice.
        // They tie on (size, showShort), so their relative order used to be hash-map order.
        val a = chain("x")
        val b = chain("y")
        val term = LamAbs("x", LamAbs("y", add(add(a, a), add(b, b))))

        val result = CommonSubexpressionElimination(term)

        result match
            case LamAbs(
                  _,
                  LamAbs(_, Apply(LamAbs(_, Apply(LamAbs(_, _, _), inner, _), _), outer, _), _),
                  _
                ) =>
                assert(outer ~=~ a, s"expected the x chain to be bound first, got ${result.show}")
                assert(inner ~=~ b, s"expected the y chain to be bound second, got ${result.show}")
            case _ => fail(s"unexpected shape: ${result.show}")
    }
}
