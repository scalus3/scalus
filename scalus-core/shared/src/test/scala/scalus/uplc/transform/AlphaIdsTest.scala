package scalus.uplc
package transform

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.internal.TermTraversal.foldLeft
import scalus.uplc.transform.CommonSubexpressionElimination.AlphaTermKey

import scala.language.implicitConversions

class AlphaIdsTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private def sameId(a: Term, b: Term): Boolean = {
        val ids = new AlphaIds
        ids(a) == ids(b)
    }

    test("terms that differ only in bound names and annotations share an ID") {
        for (a, b) <- List(("x", "y"), ("i0", "amount"), ("x_cse", "x")) do
            val first = λ(a)(AddInteger $ vr(a) $ vr"capture")
            val second = λ(b)(AddInteger $ vr(b) $ vr"capture")
            assert(sameId(first, second))
        val annotated = Const(Constant.Integer(1), UplcAnnotation(functionName = "validate"))
        assert(sameId(annotated, Const(Constant.Integer(1))))
    }

    test("free names and their order are significant") {
        def candidate(a: String, b: String): Term =
            λ("bound")(Constr(Word64.Zero, List(vr(a), vr(b))))
        assert(sameId(candidate("x", "y"), candidate("x", "y")))
        assert(!sameId(candidate("x", "y"), candidate("y", "x")))
        assert(!sameId(candidate("x", "y"), candidate("x", "z")))
        assert(!sameId(candidate("x", "y"), candidate("x", "x")))
    }

    test("shadowing distinguishes inner and outer references") {
        val shadowed = λ("x")(λ("x")(vr"x"))
        assert(sameId(shadowed, λ("a")(λ("b")(vr"b"))))
        assert(!sameId(shadowed, λ("a")(λ("b")(vr"a"))))
    }

    test("a free variable never equals a bound one") {
        assert(sameId(λ("x")(vr"i0"), λ("y")(vr"i0")))
        assert(!sameId(λ("x")(vr"i0"), λ("x")(vr"x")))
    }

    test("constants, builtins, tags and node kinds are significant") {
        val pairs = List[(Term, Term)](
          (1: Term) -> (2: Term),
          Builtin(AddInteger) -> Builtin(SubtractInteger),
          Constr(Word64.Zero, List(1)) -> Constr(Word64.One, List(1)),
          Constr(Word64.Zero, List(vr"x")) -> Case(vr"x", Nil),
          Delay(vr"x") -> Force(vr"x"),
          (vr"f" $ vr"x") -> Constr(Word64.Zero, List(vr"f", vr"x")),
          Error() -> Delay(Error())
        )
        for (a, b) <- pairs do assert(!sameId(a, b), s"${a.show} and ${b.show}")
    }

    /** Small name, constant and builtin pools, so equal and alpha-equal subterms are common. */
    private val smallTerms: Gen[Term] = {
        val names = Gen.oneOf("x", "y", "z")
        def gen(size: Int): Gen[Term] =
            val leaf = Gen.oneOf(
              names.map(n => Var(NamedDeBruijn(n))),
              Gen.oneOf(1, 2).map(i => Const(Constant.Integer(i))),
              Gen.oneOf(AddInteger, SubtractInteger).map(Builtin(_)),
              Gen.const(Error())
            )
            if size <= 1 then leaf
            else
                val sub = gen(size / 2)
                Gen.frequency(
                  1 -> leaf,
                  3 -> (for n <- names; b <- sub yield LamAbs(n, b)),
                  3 -> (for f <- sub; a <- sub yield Apply(f, a)),
                  1 -> sub.map(Delay(_)),
                  1 -> sub.map(Force(_)),
                  1 -> (for t <- Gen.oneOf(0L, 1L); as <- Gen.listOfN(2, sub)
                  yield Constr(Word64(t), as)),
                  1 -> (for s <- sub; bs <- Gen.listOfN(2, sub) yield Case(s, bs))
                )
        Gen.sized(n => gen(math.max(n, 8)))
    }

    test("IDs agree with AlphaTermKey equality on every pair of subterms") {
        forAll(smallTerms) { term =>
            val subterms = term.foldLeft(Vector.empty[Term])(_ :+ _)
            val ids = new AlphaIds
            for a <- subterms; b <- subterms do
                val expected = new AlphaTermKey(a) == new AlphaTermKey(b)
                assert(
                  (ids(a) == ids(b)) == expected,
                  s"${a.showShort} vs ${b.showShort}: AlphaTermKey says $expected"
                )
        }
    }
}
