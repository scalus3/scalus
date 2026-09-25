package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.uplc.builtin.{ByteString, Data}
import scalus.verify.Prop.{CallResult, CheckResult}

class PropTest extends AnyFunSuite {

    private val clamp = FunctionDef(Math.clamp)
    private val div10 = FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)
    private val functions = FunctionTable(clamp, div10)

    private def falsifiedWith(result: CheckResult): List[Any] = result match
        case CheckResult.Falsified(counterexample, _, _) => counterexample
        case other                                       => fail(s"expected Falsified, got $other")

    private def falsifiedBy(result: CheckResult): String = result match
        case CheckResult.Falsified(_, by, _) => by
        case other                           => fail(s"expected Falsified, got $other")

    test("a true universal passes") {
        assert(Prop.check(forAll[BigInt](x => Math.abs(x) >= 0)) == CheckResult.Passed)
    }

    test("a false universal is falsified at an edge case") {
        assert(falsifiedWith(Prop.check(forAll[BigInt](x => Math.abs(x) > 0))) == List(BigInt(0)))
    }

    test("an atom is named after its source text and position") {
        val by = falsifiedBy(Prop.check(forAll[BigInt](x => Math.abs(x) > 0)))
        assert(by.startsWith("Math.abs(x) > 0 (PropTest.scala:"), by)
        val premise = falsifiedBy(Prop.check(atom(1 > 2) || atom(2 > 3)))
        assert(premise.startsWith("2 > 3 (PropTest.scala:"), premise)
    }

    test("the counterexample lists every bound value, outermost first") {
        val p = forAll[BigInt](x => forAll[BigInt](y => Math.min(x, y) + Math.max(x, y) != x))
        val List(x, y) = falsifiedWith(Prop.check(p)): @unchecked
        assert(x == BigInt(0) && y == BigInt(0))
    }

    test("existsWith is checked in its Skolemized form") {
        val guarded = forAll[BigInt](z =>
            forAll[BigInt](x =>
                (x != BigInt(0)) ==> existsWith(x * (Math.abs(z) + 1))(y => x * y > z)
            )
        )
        assert(Prop.check(guarded) == CheckResult.Passed)

        // without the guard, x = 0 breaks it; the witness is part of the counterexample
        val unguarded = forAll[BigInt](z =>
            forAll[BigInt](x => existsWith(x * (Math.abs(z) + 1))(y => x * y > z))
        )
        val List(z, x, y) = falsifiedWith(Prop.check(unguarded)): @unchecked
        assert(x == BigInt(0) && y == BigInt(0) && z == BigInt(0))
    }

    test("an existential without a witness is found by search, or left undetermined") {
        assert(Prop.check(exists[BigInt](y => y > 5)) == CheckResult.Passed)
        assert(Prop.check(exists[BigInt](y => y * y < 0)).isInstanceOf[CheckResult.Undetermined])
    }

    test("an atom that throws is false, and a failing premise makes the implication hold") {
        assert(
          Prop.check(forAll[BigInt](x => (BigInt(10) / x > 0) ==> (x > 0))) == CheckResult.Passed
        )
        Prop.check(forAll[BigInt](x => BigInt(10) / x > -100)) match
            case CheckResult.Falsified(List(x), _, Some(_: ArithmeticException)) =>
                assert(x == BigInt(0))
            case other => fail(s"expected a falsification by ArithmeticException, got $other")
    }

    test("denotes states totality separately") {
        val p = forAll[BigInt](x => denotes(BigInt(10) / x))
        assert(falsifiedWith(Prop.check(p)) == List(BigInt(0)))
        assert(falsifiedBy(Prop.check(p)).startsWith("denotes BigInt(10) / x"))
    }

    test("negating a failing atom differs from negating inside it") {
        def failing: Boolean = BigInt(10) / BigInt(0) > 0
        assert(Prop.check(!atom(failing)) == CheckResult.Passed)
        assert(Prop.check(atom(!failing)).isInstanceOf[CheckResult.Falsified])
    }

    test("connectives are classical") {
        val p = forAll[Boolean](a => forAll[Boolean](b => (atom(a) && b) <=> !(!atom(a) || !b)))
        assert(Prop.check(p) == CheckResult.Passed)
        assert(Prop.check(forAll[Boolean](a => atom(a) || !atom(a))) == CheckResult.Passed)
        val selfRefuting = forAll[Boolean](a => atom(a) ==> !atom(a))
        assert(Prop.check(selfRefuting).isInstanceOf[CheckResult.Falsified])
    }

    test("equal compares values") {
        val concatEmpty = forAll[ByteString](b => equal(b.concat(ByteString.empty), b))
        assert(Prop.check(concatEmpty) == CheckResult.Passed)
        val by = falsifiedBy(Prop.check(forAll[Data](d => equal(d, Data.I(0)))))
        assert(by.startsWith("d equals Data.I(0)"), by)
    }

    test("implies binds looser than && and ||") {
        val p = forAll[BigInt](x => x > 0 && x < 10 implies (x != BigInt(20)))
        assert(Prop.check(p) == CheckResult.Passed)
    }

    test("a run is reproducible for a given seed") {
        // no edge case is 42 mod 100, so only random values can falsify this
        val p = forAll[BigInt](x => x % 100 != 42)
        assert(
          Prop.check(p, FunctionTable.empty, 200, 42L) == Prop.check(
            p,
            FunctionTable.empty,
            200,
            42L
          )
        )
    }

    test("a method reference is named as SIR names it, a lambda by its synthetic name") {
        assert(clamp.name == "scalus.cardano.onchain.plutus.prelude.Math$.clamp", clamp.name)
        assert(clamp.ref.displayName == "clamp")
        assert(div10.name == "div10")
    }

    test("an entry carries one representation per proof method") {
        assert(clamp.available == Set("scalacheck", "sir", "uplc"))
        assert(clamp(Representation.Uplc).term.toString.nonEmpty)
        assert(clamp.scalacheck.map(_((BigInt(14), BigInt(0), BigInt(10)))) == Some(BigInt(10)))
        val mapped = clamp.withLeanMapping("fun x lo hi => max lo (min x hi)")
        assert(mapped.available == Set("scalacheck", "sir", "uplc", "lean-mapping"))
        assertThrows[NoSuchElementException](clamp(Representation.LeanMapping))
    }

    test("a function without a scalacheck representation cannot be checked") {
        val bytesOnly = FunctionDef
            .qualified[BigInt, BigInt]("some.pkg.Script.f")
            .withRepresentation(Representation.Uplc, div10(Representation.Uplc))
        val error = intercept[NoSuchElementException](
          Prop.check(forAll[BigInt](x => call(bytesOnly, x)(_ => true)), FunctionTable(bytesOnly))
        )
        assert(error.getMessage.contains("no scalacheck representation"), error.getMessage)
    }

    test("a function contract holds with whenReturns") {
        val contract = forAll[BigInt](x =>
            forAll[BigInt](lo =>
                forAll[BigInt](hi =>
                    (lo <= hi) ==> whenReturns(clamp, (x, lo, hi))(r => lo <= r && r <= hi)
                )
            )
        )
        assert(Prop.check(contract, functions) == CheckResult.Passed)
    }

    test("call requires the call to return; whenReturns does not") {
        Prop.check(forAll[BigInt](x => call(div10, x)(_ => true)), functions) match
            case CheckResult.Falsified(List(x), by, Some(_: ArithmeticException)) =>
                assert(x == BigInt(0) && by == "div10 did not return")
            case other => fail(s"expected div10 to fail at 0, got $other")
        val partial = forAll[BigInt](x => whenReturns(div10, x)(r => r * x <= 10))
        assert(Prop.check(partial, functions) == CheckResult.Passed)
    }

    test("a call appears in the counterexample") {
        val p = forAll[BigInt](x => call(clamp, (x, BigInt(0), BigInt(10)))(r => r != x))
        Prop.check(p, functions) match
            case CheckResult.Falsified(List(x, call: CallResult), _, None) =>
                assert(x == BigInt(0) && call.function == clamp.ref && call.result == BigInt(0))
                assert(call.toString == "clamp(0, 0, 10) = 0")
            case other => fail(s"expected a falsification at x = 0, got $other")
    }

    test("a call to a function missing from the table is an error, not a falsification") {
        assertThrows[NoSuchElementException](
          Prop.check(forAll[BigInt](x => call(div10, x)(_ => true)))
        )
    }

    test("two different functions under one name are rejected") {
        val other = FunctionDef.named("div10", (x: BigInt) => x)
        assertThrows[IllegalArgumentException](FunctionTable(div10, other))
    }

    test("a synthetic name cannot look like a qualified one") {
        assertThrows[IllegalArgumentException](FunctionDef.named("my.div", (x: BigInt) => x))
    }
}
