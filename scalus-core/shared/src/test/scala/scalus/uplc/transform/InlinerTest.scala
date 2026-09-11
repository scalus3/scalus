package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.transform.TermAnalysis.freeVars
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.time.SpanSugar.*
import scalus.cardano.ledger.Word64
import scalus.uplc.eval.PlutusVM
import scalus.uplc.builtin.{ByteString, Data}

import scala.language.implicitConversions

class InlinerTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("inliner should inline identity Var") {
        val term = LamAbs("x", vr"x" $ vr"x") $ vr"y"
        val expected = vr"y" $ vr"y"
        assert(Inliner(term) == expected)
    }

    test("constants should remain unchanged") {
        val constTerm: Term = 42
        assert(Inliner(constTerm) == constTerm)

        val strTerm: Term = "hello"
        assert(Inliner(strTerm) == strTerm)
    }

    test("builtins should remain unchanged") {
        val addTerm: Term = AddInteger
        assert(Inliner(addTerm) == addTerm)

        val mulTerm: Term = MultiplyInteger
        assert(Inliner(mulTerm) == mulTerm)
    }

    test("identity function should be eliminated") {
        // (λx.x) 42 => 42
        val term = λ("x")(vr"x") $ 42
        assert(Inliner(term) == 42.asTerm)

        // (λx.x) "hello" => "hello"
        val strTerm = λ("x")(vr"x") $ "hello"
        assert(Inliner(strTerm) == "hello".asTerm)
    }

    test("nested identity functions should all be eliminated") {
        // (λx.x) ((λy.y) 42) => 42
        val term = λ("x")(vr"x") $ (λ("y")(vr"y") $ 42)
        assert(Inliner(term) == 42.asTerm)
    }

    test("variable substitution should work correctly") {
        // (λx. x + x) y => y + y (free variable prevents constant folding, tests pure substitution)
        val term = λ("x")(AddInteger $ vr"x" $ vr"x") $ vr"y"

        val expected = AddInteger $ vr"y" $ vr"y"

        assert(Inliner(term) == expected)
    }

    test("should avoid name capture through alpha-renaming") {
        // (λx. λy. x) y
        // Should alpha-rename y in the inner lambda to avoid capture
        val term = λ("x", "y")(vr"x") $ vr"y"

        val result = Inliner(term)

        // The result should be λy_1. y where y_1 is a fresh name
        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0), _), _) =>
                assert(newName != "y")
            case _ =>
                fail(s"Unexpected result: $result")
    }

    test("should handle Constr and Case") {
        val constr = Constr(Word64.Zero, List(42))
        assert(Inliner(constr) == constr)

        val caseExpr = Case(
          constr,
          List(
            λ("x")(vr"x") $ vr"y", // Identity function application
            0
          )
        )

        val expectedCase = Case(
          constr,
          List(
            vr"y", // Identity function eliminated
            0
          )
        )

        assert(Inliner(caseExpr) == expectedCase)
    }

    test("should not inline non-pure terms") {
        // (λx. x + x) Error => (λx. x + x) Error
        val termWithError = λ("x")(AddInteger $ vr"x" $ vr"x") $ Error()
        assert(Inliner(termWithError) == termWithError)
    }

    test("should handle complex arithmetic expressions") {
        // (λx. λy. x + y) a b => a + b (tests multi-arg substitution without constant folding)
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ vr"a" $ vr"b"
        val expected = AddInteger $ vr"a" $ vr"b"
        assert(Inliner(term) == expected)
    }

    test("should properly handle substitution with potential capture") {
        // (λx.λy.x) y => λy'.y
        val term = λ("x")(λ("y")(vr"x")) $ vr"y"
        val result = Inliner(term)

        result match
            case LamAbs(newName, Var(NamedDeBruijn("y", 0), _), _) =>
                assert(newName != "y") // Should be renamed to avoid capture
            case _ => fail(s"Unexpected result: $result")
    }

    test("should handle substitution with multiple bound variables") {
        // (λx.λy.x y) y => λy'.y y
        val term = λ("x")(λ("y")(vr"x" $ vr"y")) $ vr"y"
        val result = Inliner(term)

        result match
            case LamAbs(
                  newName,
                  Apply(Var(NamedDeBruijn("y", 0), _), Var(NamedDeBruijn(y2, 0), _), _),
                  _
                ) =>
                assert(newName != "y") // Should be renamed
                assert(y2 == newName) // The bound y should refer to the new name
            case _ => fail(s"Unexpected result: $result")
    }

    test("should respect shadowing in substitution") {
        // (λx.λx.x) y => λx.x
        val term = λ("x")(λ("x")(vr"x")) $ vr"y"
        val result = Inliner(term)

        assert(
          result == λ("x")(vr"x")
        ) // The inner x shadows outer x, so y shouldn't be substituted
    }

    test("should handle multiple variable references") {
        // (λx. x + (x * x)) y => y + (y * y) (tests duplicating a variable reference)
        val term = λ("x")(
          AddInteger $ vr"x" $ (MultiplyInteger $ vr"x" $ vr"x")
        ) $ vr"y"

        val expected = AddInteger $ vr"y" $ (MultiplyInteger $ vr"y" $ vr"y")
        assert(Inliner(term) == expected)
    }

    test("should eliminate Force(Delay(t))") {
        // !(~42) => 42
        val term = !(~(42: Term))
        val expected: Term = 42
        assert(Inliner(term) == expected)
    }

    // Tests for Term.isPure extension method from TermAnalysis.
    // These tests verify that Force terms are correctly identified as impure
    // to prevent incorrect dead code elimination.
    test("should not eliminate Force of constant as dead code - it will error") {
        // (λx. 42) (Force (Const 1))
        // Force of non-delayed term errors at runtime, must be preserved
        // TermAnalysis.isPure correctly identifies Force(Const) as impure
        val term = λ("x")(42) $ Force(1.asTerm)
        val expected = λ("x")(42) $ Force(1.asTerm)
        assert(Inliner(term) == expected)
    }

    test("should not eliminate Force of builtin as dead code") {
        // (λx. 100) (Force AddInteger)
        // Forcing a builtin (not delayed) will error
        // TermAnalysis.isPure correctly identifies this as impure
        val term = λ("x")(100) $ Force(AddInteger)
        val expected = λ("x")(100) $ Force(AddInteger)
        assert(Inliner(term) == expected)
    }

    test("should not eliminate Force of variable as dead code") {
        // (λy. (λx. 42) (Force y))
        // We don't know if y is delayed, so Force y could error
        // TermAnalysis.isPure conservatively treats Force as impure
        val term = λ("y")(λ("x")(42) $ Force(vr"y"))
        val expected = λ("y")(λ("x")(42) $ Force(vr"y"))
        assert(Inliner(term) == expected)
    }

    test("constant duplication uses the exact count and includes binding execution fees") {
        val value = Data.I(0).asTerm
        def shared(n: Int) = λ(x => Constr(Word64.Zero, List.fill(n)(x))) $ value
        val twice = shared(2)
        val thrice = shared(3)
        // Sharing twice saves bits, but not enough to pay for executing the binding.
        assert(SharingCost.savingBits(value, 2) > 0)
        assert(Inliner(twice) == Constr(Word64.Zero, List.fill(2)(value)))
        assert(Inliner(thrice) == thrice)
        assert(Inliner(thrice).evaluate α_== thrice.evaluate)
    }

    test("occurrence counting includes guarded uses and excludes shadowed uses") {
        val value = Data.I(0).asTerm
        val shared = λ(x => Constr(Word64.Zero, List(x, Delay(x), λ(y => x)))) $ value
        assert(Inliner(shared) == shared)
        val input = λ(x => Constr(Word64.Zero, List(x, Delay(x), λ(x => x)))) $ value
        val expected = Constr(Word64.Zero, List(value, Delay(value), λ(x => x)))
        assert(Inliner(input) == expected)
    }

    test("fold through a shared constant without duplicating its remaining uses") {
        val value = Data.I(42).asTerm
        val term = λ(x => Constr(Word64.Zero, List(UnIData $ x, x, x, x))) $ value
        val expected = λ(x => Constr(Word64.Zero, List(42, x, x, x))) $ value
        assert(Inliner(term) == expected)
        assert(Inliner(expected) == expected)
    }

    test("reprice sharing after constant propagation removes uses") {
        val value = Data.I(0).asTerm
        val term = λ(x => Constr(Word64.Zero, List(UnIData $ x, x, x))) $ value
        assert(SharingCost.savingLovelace(value, 3) > 0)
        assert(Inliner(term) == Constr(Word64.Zero, List(0, value, value)))
    }

    test("constant propagation respects shadowing and leaves failed evaluation unchanged") {
        val value = Data.I(42).asTerm
        val term = λ(x => Constr(Word64.Zero, List(λ(x => UnIData $ x), UnBData $ x, x, x))) $ value
        assert(Inliner(term) == term)
    }

    test("constant propagation combines nested bindings") {
        val first = Data.I(40).asTerm
        val second = Data.I(2).asTerm
        val term = λ(x =>
            λ(y =>
                Constr(
                  Word64.Zero,
                  List(AddInteger $ (UnIData $ x) $ (UnIData $ y), x, x, x, y, y, y)
                )
            ) $ second
        ) $ first
        val expected =
            λ(x => λ(y => Constr(Word64.Zero, List(42, x, x, x, y, y, y))) $ second) $ first
        assert(Inliner(term) == expected)
    }

    test("constant folding does not expand shared bytestrings") {
        val value = ByteString.fromArray(Array.fill[Byte](1024)(42)).asTerm
        for directUses <- List(1, 4) do
            val term = λ(x =>
                Constr(Word64.Zero, (AppendByteString $ x $ x) :: List.fill(directUses)(x))
            ) $ value
            val optimized = Inliner(term)
            assert(optimized.evaluate α_== term.evaluate)
            assert(optimized.plutusV3.cborEncoded.length <= term.plutusV3.cborEncoded.length)
            assert(optimized == term)
    }

    test("small fold results remain available through large shared constants") {
        val value = ByteString.fromArray(Array.fill[Byte](1024)(42)).asTerm
        val term = λ(x => Constr(Word64.Zero, List(LengthOfByteString $ x, x, x))) $ value
        val expected = λ(x => Constr(Word64.Zero, List(1024, x, x))) $ value
        assert(Inliner(term) == expected)
        assert(Inliner(term).evaluate α_== term.evaluate)
    }

    test("repeated constants with unavailable Flat sizes retain their bindings") {
        import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
        val g1 = Constant.BLS12_381_G1_Element(G1Element.generator)
        val g2 = Constant.BLS12_381_G2_Element(G2Element.generator)
        for constant <- List(g1, g2, Constant.List(g1.tpe, List(g1))) do
            val value = Const(constant)
            val term = λ(x => Constr(Word64.Zero, List(x, x))) $ value
            assert(Inliner(term) == term)
            assert(Inliner(LamAbs("x", vr"x") $ value) == value)
    }

    test("retained constant chains complete without exponential body traversal") {
        val value = ("a" * 128).asTerm
        def chain(exposeLambda: Boolean): Term =
            (0 until 27).foldLeft(vr"free": Term) { (body, i) =>
                val name = s"constant$i"
                val ref = Var(NamedDeBruijn(name))
                val lambda = LamAbs(name, Constr(Word64.Zero, List(ref, ref, body)))
                val function = if exposeLambda then Force(Delay(lambda)) else lambda
                function $ value
            }
        val term = chain(exposeLambda = false)
        // Deliberately generous: this is a runaway-traversal guard, not a benchmark.
        failAfter(30.seconds) {
            assert(Inliner(term) == term)
            assert(Inliner(chain(exposeLambda = true)) == term)
        }
    }

    test("constant propagation handles newly exposed lambdas and shadowing") {
        val value = Data.I(42).asTerm
        val body = Constr(Word64.Zero, List(UnIData $ vr"x", vr"x", vr"x", vr"x"))
        val expected = LamAbs("x", Constr(Word64.Zero, List(42, vr"x", vr"x", vr"x"))) $ value
        val exposed = Force(Delay(LamAbs("x", body))) $ value
        assert(Inliner(exposed) == expected)
        assert(Inliner(exposed).evaluate α_== exposed.evaluate)
        val shadowed = LamAbs("x", LamAbs("x", body)) $ Data.I(7).asTerm $ value
        assert(Inliner(shadowed) == expected)
    }

    test("retained function bindings finish deferred lambda bodies") {
        val value = ("a" * 128).asTerm
        val body = Constr(Word64.Zero, List(vr"x", vr"x", AddInteger $ 1 $ 2))
        val folded = Constr(Word64.Zero, List(vr"x", vr"x", 3))
        val term = LamAbs("x", LamAbs("y", body)) $ value $ 0
        val expected = LamAbs("x", LamAbs("y", folded)) $ value $ 0
        assert(Inliner(term) == expected)
        assert(Inliner(term).evaluate α_== term.evaluate)

        val delayed = LamAbs("x", Delay(LamAbs("y", body))) $ value $ 0
        val expectedDelayed = LamAbs("x", Delay(LamAbs("y", folded))) $ value $ 0
        assert(Inliner(delayed) == expectedDelayed)
    }

    test("finished deferred bodies reprice their remaining constant uses") {
        val value = Data.I(42).asTerm
        val body = Constr(Word64.Zero, List.fill(3)(UnIData $ vr"x"))
        val term = LamAbs("x", LamAbs("y", body)) $ value $ 0
        assert(Inliner(term) == Constr(Word64.Zero, List.fill(3)(42.asTerm)))
    }

    test("unused impure bindings finish deferred bodies") {
        val term = LamAbs("x", LamAbs("y", AddInteger $ 1 $ 2)) $ Error() $ 0
        val expected = LamAbs("x", LamAbs("y", 3.asTerm)) $ Error() $ 0
        assert(Inliner(term) == expected)
    }

    test("non-lambda function expressions finish deferred lambda bodies") {
        val lambda = LamAbs("y", AddInteger $ 1 $ 2)
        val folded = LamAbs("y", 3.asTerm)
        assert(Inliner(Delay(lambda) $ 0) == (Delay(folded) $ 0))
        assert(Inliner(Force(lambda) $ 0) == (Force(folded) $ 0))
    }

    test("the sharing fee estimate charges the measured extra CEK work for a one-node value") {
        val value = Data.I(0).asTerm
        val params = scalus.cardano.ledger.CardanoInfo.mainnet.protocolParams
        for n <- List(2, 3, 20) do
            val duplicated = Constr(Word64.Zero, List.fill(n)(value))
            val shared = λ(x => Constr(Word64.Zero, List.fill(n)(x))) $ value
            val before = duplicated.evaluateDebug.budget
            val after = shared.evaluateDebug.budget
            val extraFee =
                (after.memory - before.memory) * params.executionUnitPrices.priceMemory.toDouble +
                    (after.steps - before.steps) * params.executionUnitPrices.priceSteps.toDouble
            val sizeFee =
                SharingCost.savingBits(value, n) * params.minFeeRefScriptCostPerByte.toDouble / 8
            assert(math.abs(SharingCost.savingLovelace(value, n) - (sizeFee - extraFee)) < 1e-9)
    }

    test("should inline small constant with multiple occurrences") {
        // (λx. x + x) 42 => 42 + 42
        // Two uses are cheap enough to duplicate under the shared fee estimate.
        val term = λ("x")(AddInteger $ vr"x" $ vr"x") $ 42
        val expected = 84.asTerm
        assert(Inliner(term) == expected)
    }

    test("should not inline large constant with multiple occurrences") {
        // (λx. pair x x) "a long string..." => (λx. pair x x) "a long string..."
        // Duplicating this constant costs more than retaining its binding.
        val largeStr: Term = "this is a long string that exceeds 64 bits in flat encoding"
        val term = λ("x")(Constr(Word64.Zero, List(vr"x", vr"x"))) $ largeStr
        assert(Inliner(term) == term)
    }

    test("should inline large constant with single occurrence") {
        // (λx. x) "a long string..." => "a long string..."
        // Even large constants are safe when used only once (identity is a special case,
        // so use a non-identity single-occurrence body)
        val largeStr: Term = "this is a long string that exceeds 64 bits in flat encoding"
        val term = λ("x")(Constr(Word64.Zero, List(vr"x"))) $ largeStr
        val expected = Constr(Word64.Zero, List(largeStr))
        assert(Inliner(term) == expected)
    }

    test("should inline builtin with multiple occurrences") {
        // (λx. x y (x z)) AddInteger => AddInteger y (AddInteger z)
        // Builtins are always safe to duplicate
        val term = λ("x")(vr"x" $ vr"y" $ (vr"x" $ vr"z")) $ AddInteger
        val expected = AddInteger $ vr"y" $ (AddInteger $ vr"z")
        assert(Inliner(term) == expected)
    }

    test("should eliminate unused pure Delay as dead code") {
        // (λx. 42) (Delay (Const 1))
        // Delay IS pure - can be safely eliminated
        // TermAnalysis.isPure correctly identifies Delay as pure
        val term = λ("x")(42) $ Delay(1.asTerm)
        val expected = 42.asTerm
        assert(Inliner(term) == expected)
    }

    // ========================================================================
    // Partial evaluation integration
    // ========================================================================

    test("should partially evaluate closed builtin application") {
        // addInteger 2 3 => 5 (closed term, folded by tryPartialEval)
        val term = AddInteger $ 2 $ 3
        assert(Inliner(term) == 5.asTerm)
    }

    test("should partially evaluate Case on known Constr") {
        // case (constr 0 [1, 2]) of [λa.λb. addInteger a b] => 3
        val term = Case(
          Constr(Word64.Zero, List(1, 2)),
          List(λ("a", "b")(AddInteger $ vr"a" $ vr"b"))
        )
        assert(Inliner(term) == 3.asTerm)
    }

    test("should optimize subexpressions inside Constr args") {
        // Constr(0, [(λx.x) 42]) => Constr(0, [42])
        val term = Constr(Word64.Zero, List(λ("x")(vr"x") $ 42))
        val expected = Constr(Word64.Zero, List[Term](42))
        assert(Inliner(term) == expected)
    }

    test("should cascade optimizations after substitution") {
        // (λf. f 2 3) addInteger => addInteger 2 3 => 5
        val term = λ("f")(vr"f" $ 2 $ 3) $ AddInteger
        assert(Inliner(term) == 5.asTerm)
    }

    // ========================================================================
    // Force/Delay interactions
    // ========================================================================

    test("should eliminate nested Force(Delay)") {
        // Force(Force(Delay(Delay(42)))) => 42
        val term = Force(Force(Delay(Delay(42.asTerm))))
        assert(Inliner(term) == 42.asTerm)
    }

    test("Force(Delay) revealed after optimization should be eliminated") {
        // Force((λx. Delay(x)) y) => after go: Force(Delay(y)) => y
        val term = Force(λ("x")(Delay(vr"x")) $ vr"y")
        assert(Inliner(term) == vr"y")
    }

    // ========================================================================
    // shouldInline: single-occurrence non-trivial terms
    // ========================================================================

    test("should inline single-occurrence Delay argument in direct position") {
        // x occurs once in direct position (OnceDirect) → safe to inline any term.
        // Delay(42) is the argument; after substitution: addInteger(Delay(42), 1)
        val term = λ("x")(AddInteger $ vr"x" $ 1) $ Delay(42.asTerm)
        val expected = AddInteger $ Delay(42.asTerm) $ 1.asTerm
        assert(Inliner(term) == expected)
    }

    test("should inline single-occurrence LamAbs argument in direct position") {
        // (λf. f 1) (λy. addInteger y 2)
        // f occurs once in direct position (OnceDirect) → inline.
        // After substitution: (λy. addInteger y 2) 1 → folds to 3.
        val term = λ("f")(vr"f" $ 1) $ λ("y")(AddInteger $ vr"y" $ 2)
        assert(Inliner(term) == 3.asTerm)
    }

    // ========================================================================
    // Guarded occurrence tests
    // ========================================================================

    test("should not inline non-value in guarded (Delay) position") {
        // λx. Delay(x) applied to a non-value (addInteger 1 2 → 3, but consider Error)
        // x occurs once under Delay → OnceGuarded. Error is not a value → not inlined.
        val term = λ("x")(Delay(vr"x")) $ Error()
        assert(Inliner(term) == term)
    }

    test("should inline value in guarded (Delay) position") {
        // λx. Delay(x) applied to Delay(42) — a value.
        // x occurs once under Delay → OnceGuarded. Delay(42) is a value → inlined.
        val term = λ("x")(Delay(vr"x")) $ Delay(42.asTerm)
        val expected = Delay(Delay(42.asTerm))
        assert(Inliner(term) == expected)
    }

    test("should not inline non-value in guarded (Case branch) position") {
        // x occurs only inside a Case branch → OnceGuarded.
        // (addInteger 1 2) is not a value → not inlined.
        // But the whole expression is closed, so tryPartialEval may fold it.
        val term = λ("x")(Case(Constr(Word64.Zero, Nil), List(vr"x"))) $ (AddInteger $ 1 $ 2)
        val result = Inliner(term)
        // After go: arg becomes 3 (small const, which IS a value), so OnceGuarded + value → inlined
        assert(result == 3.asTerm)
    }

    test("should inline value in guarded (LamAbs body) position") {
        // λx. λy. x applied to 42 — x occurs once under LamAbs → OnceGuarded.
        // 42 (small const) is a value → inlined.
        val term = λ("x")(λ("y")(vr"x")) $ 42
        val expected = λ("y")(42.asTerm)
        assert(Inliner(term) == expected)
    }

    test("should not inline non-value in guarded (LamAbs body) position") {
        // λx. λy. x applied to Error — x under LamAbs → OnceGuarded.
        // Error is not a value → not inlined. Also not pure → not dead code eliminated.
        val term = λ("x")(λ("y")(vr"x")) $ Error()
        assert(Inliner(term) == term)
    }

    // ========================================================================
    // Non-inlinable Apply not passed to tryPartialEval
    // ========================================================================

    // BUG: When shouldInline returns false, the Apply is returned without
    // calling tryPartialEval, even though the whole term may be closed.
    test("should partially evaluate non-inlinable closed Apply") {
        val longA: Term = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        val longB: Term = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        val arg = AppendString $ longA $ longB
        // After go(arg), arg folds to a large string constant (>64 bits).
        // shouldInline(largeConst, 2) = false, but tryPartialEval should fold the Apply
        // since the whole expression is closed and evaluates to a Const.
        val term = λ("x")(AppendString $ vr"x" $ vr"x") $ arg
        val result = Inliner(term)
        result match
            case Const(_, _) => succeed
            case _           => fail(s"Expected Const, got: ${result.show}")
    }

    // ========================================================================
    // Substitute correctness: freshName collision with body free vars
    // ========================================================================

    test("substitute should not capture body free vars during alpha-renaming") {
        val inliner = new Inliner()
        // substitute(λy. (y_0, x), "x", y)
        // replacement = Var("y"), so alpha-rename "y" must avoid both {"y"} and {"y_0"}
        val term = LamAbs("y", Constr(Word64.Zero, List(vr"y_0", vr"x")))
        val result = inliner.substitute(term, "x", vr"y")
        val freeVarsResult = result.freeVars
        assert(
          freeVarsResult.contains("y_0"),
          s"Free variable y_0 was captured! Result: $result, freeVars: $freeVarsResult"
        )
    }

    // ========================================================================
    // Substitution into Constr and Case
    // ========================================================================

    test("should substitute into Constr arguments") {
        // (λx. constr 0 [x, 1]) y => constr 0 [y, 1]
        val term = λ("x")(Constr(Word64.Zero, List(vr"x", 1))) $ vr"y"
        val expected = Constr(Word64.Zero, List(vr"y", 1.asTerm))
        assert(Inliner(term) == expected)
    }

    test("should substitute into Case branches") {
        // (λx. case scrutinee of [x, 0]) y => case scrutinee of [y, 0]
        val term = λ("x")(Case(vr"scrutinee", List(vr"x", 0))) $ vr"y"
        val expected = Case(vr"scrutinee", List(vr"y", 0.asTerm))
        assert(Inliner(term) == expected)
    }

    // ========================================================================
    // Semantic preservation for closed terms
    // ========================================================================

    test("optimization preserves semantics of closed arithmetic") {
        val terms = List(
          AddInteger $ 10 $ (MultiplyInteger $ 3 $ 4),
          λ("x")(AddInteger $ vr"x" $ vr"x") $ 21,
          λ("x", "y")(MultiplyInteger $ vr"x" $ vr"y") $ 6 $ 7,
          Case(
            Constr(Word64.Zero, List[Term](5, 10)),
            List(λ("a", "b")(AddInteger $ vr"a" $ vr"b"))
          )
        )
        terms.foreach { term =>
            val optimized = Inliner(term)
            val origResult = term.evaluate
            val optResult = optimized.evaluate
            assert(
              origResult α_== optResult,
              s"Semantics differ for ${term.show}: original=$origResult, optimized=$optResult"
            )
        }
    }

    test("optimization is idempotent") {
        val terms = List[Term](
          AddInteger $ 2 $ 3,
          λ("x")(vr"x") $ 42,
          λ("x")(AddInteger $ vr"x" $ vr"x") $ vr"y",
          Force(Delay(1.asTerm)),
          Case(Constr(Word64.Zero, List[Term](1)), List(λ("a")(vr"a"))),
          λ("x")(Constr(Word64.Zero, List(vr"x", vr"x"))) $ vr"z"
        )
        terms.foreach { term =>
            val once = Inliner(term)
            val twice = Inliner(once)
            assert(once == twice, s"Not idempotent for ${term.show}: once=$once, twice=$twice")
        }
    }

    test("optimization does not introduce new free variables") {
        val terms = List[Term](
          λ("x")(vr"x") $ vr"y",
          λ("x")(AddInteger $ vr"x" $ vr"z") $ vr"y",
          λ("x", "y")(vr"x") $ vr"y",
          Force(Delay(vr"a")),
          Case(vr"s", List(λ("x")(vr"x") $ vr"b", vr"c"))
        )
        terms.foreach { term =>
            val optimized = Inliner(term)
            val origFree = term.freeVars
            val optFree = optimized.freeVars
            assert(
              optFree.subsetOf(origFree),
              s"New free vars ${optFree -- origFree} in ${term.show} => ${optimized.show}"
            )
        }
    }

    // ========================================================================
    // Logging
    // ========================================================================

    // ========================================================================
    // DCE with builtin totality
    // ========================================================================

    test("should eliminate dead saturated total builtin") {
        // (λx. 42) (AddInteger $ 1 $ 2) => 42
        // AddInteger is total, so the saturated application is pure and can be eliminated
        val term = λ("x")(42) $ (AddInteger $ 1 $ 2)
        assert(Inliner(term) == 42.asTerm)
    }

    test("should not eliminate dead saturated partial builtin") {
        // (λx. 42) (DivideInteger $ 1 $ 0) stays unchanged
        // DivideInteger is partial, so the saturated application is impure
        val term = λ("x")(42) $ (DivideInteger $ 1 $ 0)
        assert(Inliner(term) == term)
    }

    test("should not eliminate dead Trace (side effect)") {
        // (λx. 42) (Force(Trace) $ "hello" $ 1) stays unchanged
        // Trace has a side effect (logging), must be preserved
        val term = λ("x")(42) $ (Force(Builtin(Trace)) $ "hello" $ 1)
        assert(Inliner(term) == term)
    }

    test("should produce log entries for optimizations") {
        val inliner = new Inliner()
        inliner(λ("x")(vr"x") $ (AddInteger $ 1 $ 2))
        val logs = inliner.logs
        assert(logs.nonEmpty, "Expected log entries for optimizations")
        assert(logs.exists(_.contains("Inlining identity function")))
    }
}
