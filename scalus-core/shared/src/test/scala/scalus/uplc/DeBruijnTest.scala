package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.test.ArbitraryInstances

class DeBruijnTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:

    /** A variable carrying a De Bruijn index. */
    private def ix(name: String, index: Int): Term = Var(NamedDeBruijn(name, index))

    test("deBruijnTerm") {
        assert(DeBruijn.deBruijnTerm(lam("x", "x", "y")(vr"x")) == lam("x", "x", "y")(ix("x", 2)))
    }

    test("fromDeBruijnTerm") {
        val deBruijnedTerm = DeBruijn.deBruijnTerm(lam("x", "x", "y")(vr"x"))
        assert(deBruijnedTerm == lam("x", "x", "y")(ix("x", 2)))
        assert(DeBruijn.fromDeBruijnTerm(deBruijnedTerm) == lam("i0", "i1", "i2")(ix("i1", 2)))
    }

    test("deBruijnTerm numbers free variables -1, -2, ... in traversal order, per occurrence") {
        val t = lam("x")(vr"f" $ vr"x") $ (vr"g" $ vr"f")
        assert(
          DeBruijn.deBruijnTerm(t) ==
              (lam("x")(ix("f", -1) $ ix("x", 1)) $ (ix("g", -2) $ ix("f", -3)))
        )
    }

    test("deBruijnTerm resolves a binder shadowed and then restored") {
        // (\x -> (\x -> x) x): the inner x is the inner binder, the outer one after it the outer.
        val t = lam("x")(lam("x")(vr"x") $ vr"x")
        assert(DeBruijn.deBruijnTerm(t) == lam("x")(lam("x")(ix("x", 1)) $ ix("x", 1)))
    }

    test("deBruijnTerm indexes variables inside Constr and Case") {
        val t = lam("a", "b")(Case(vr"a", List(Constr(Word64.Zero, List(vr"b")))))
        assert(
          DeBruijn.deBruijnTerm(t) ==
              lam("a", "b")(Case(ix("a", 2), List(Constr(Word64.Zero, List(ix("b", 1))))))
        )
    }

    test("deBruijnTerm with throwOnFreeVariable names the variable and the scope") {
        val e = intercept[IllegalArgumentException](
          DeBruijn.deBruijnTerm(lam("a", "b")(vr"c"), true)
        )
        assert(
          e.getMessage == "Unresolved variable 'c' in De Bruijn conversion. Available variables in scope: [b, a]"
        )
    }

    test("deBruijnTerm with throwOnFreeVariable lists a shadowed name once") {
        val e = intercept[IllegalArgumentException](
          DeBruijn.deBruijnTerm(lam("x", "x", "y")(vr"c"), true)
        )
        assert(e.getMessage.endsWith("Available variables in scope: [y, x]"))
    }

    test("fromDeBruijnTerm keeps the name of a free variable") {
        val t = lam("_")(ix("free", -1) $ ix("_", 1))
        assert(DeBruijn.fromDeBruijnTerm(t) == lam("i0")(ix("free", -1) $ ix("i0", 1)))
    }

    test("deBruijnTerm and fromDeBruijnTerm resolve binders 200 levels deep") {
        val names = (0 until 200).map(i => s"v$i")
        val deBruijned = DeBruijn.deBruijnTerm(lam(names.head, names.tail*)(vr"v0" $ vr"v199"))
        assert(deBruijned == lam(names.head, names.tail*)(ix("v0", 200) $ ix("v199", 1)))
        val binders = (0 until 200).map(i => s"i$i")
        assert(
          DeBruijn.fromDeBruijnTerm(deBruijned) ==
              lam(binders.head, binders.tail*)(ix("i0", 200) $ ix("i199", 1))
        )
    }

    test("fromDeBruijnTerm rejects an index with no binder") {
        assertThrows[IndexOutOfBoundsException](DeBruijn.fromDeBruijnTerm(lam("x")(ix("x", 2))))
    }

    test("fromDeBruijnTerm(deBruijnTerm(t)) == t") {
        forAll { (t: Term) =>
            val deBruijnedTerm = DeBruijn.deBruijnTerm(t)
            val namedTerm = DeBruijn.fromDeBruijnTerm(deBruijnedTerm)
            val deBruijnedTerm2 = DeBruijn.deBruijnTerm(namedTerm)
            val namedTerm2 = DeBruijn.fromDeBruijnTerm(deBruijnedTerm2)
            assert(namedTerm == namedTerm2)
        }
    }
