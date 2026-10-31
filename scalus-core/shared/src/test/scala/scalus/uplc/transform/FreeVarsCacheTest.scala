package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.Term.*
import scalus.uplc.internal.TermTraversalTest.genTerm
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.uplc.{NamedDeBruijn, Term}

class FreeVarsCacheTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    given PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

    test("agrees with TermAnalysis.freeVars on generated terms") {
        forAll(genTerm) { t =>
            assert(new FreeVarsCache()(t) == t.freeVars)
        }
    }

    test("agrees with TermAnalysis.freeVars when one cache answers many queries") {
        // Inliner queries subterms first and then parents rebuilt over them, so most children
        // are already cached. Every child of every node must still be counted.
        forAll(genTerm, genTerm) { (a, b) =>
            val cache = new FreeVarsCache()
            assert(cache(a) == a.freeVars)
            val parents = List(
              Apply(a, b),
              Case(b, List(a, b)),
              Constr(Word64.Zero, List(b, a)),
              LamAbs("x", Apply(a, Var(NamedDeBruijn("x"))))
            )
            for parent <- parents do assert(cache(parent) == parent.freeVars)
        }
    }

    test("a subtree shared at two places counts for both") {
        val shared: Term = Apply(Var(NamedDeBruijn("y")), Var(NamedDeBruijn("x")))
        // Under the lambda, x is bound; outside it, x is free. The same object appears in both.
        val term = Apply(LamAbs("x", shared), shared)
        assert(new FreeVarsCache()(term) == Set("x", "y"))
    }
}
