package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.builtin.ByteString
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, Word64}

import scala.language.implicitConversions

class PartialEvaluatorTest extends AnyFunSuite {

    // ========================================================================
    // PartialEvaluator.tryEval — unit tests
    // ========================================================================

    // --- Arithmetic ---

    test("tryEval: addInteger 2 3 => 5") {
        val term = AddInteger $ 2 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(5.asTerm))
    }

    test("tryEval: subtractInteger 10 3 => 7") {
        val term = SubtractInteger $ 10 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(7.asTerm))
    }

    test("tryEval: multiplyInteger 4 5 => 20") {
        val term = MultiplyInteger $ 4 $ 5
        assert(PartialEvaluator.tryEval(term) == Some(20.asTerm))
    }

    test("tryEval: divideInteger 10 3 => 3") {
        val term = DivideInteger $ 10 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(3.asTerm))
    }

    test("tryEval: remainderInteger 10 3 => 1") {
        val term = RemainderInteger $ 10 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(1.asTerm))
    }

    test("tryEval: modInteger 10 3 => 1") {
        val term = ModInteger $ 10 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(1.asTerm))
    }

    // --- Comparison ---

    test("tryEval: equalsInteger 5 5 => true") {
        val term = EqualsInteger $ 5 $ 5
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    test("tryEval: equalsInteger 5 6 => false") {
        val term = EqualsInteger $ 5 $ 6
        assert(PartialEvaluator.tryEval(term) == Some(false.asTerm))
    }

    test("tryEval: lessThanInteger 1 2 => true") {
        val term = LessThanInteger $ 1 $ 2
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    test("tryEval: lessThanInteger 2 1 => false") {
        val term = LessThanInteger $ 2 $ 1
        assert(PartialEvaluator.tryEval(term) == Some(false.asTerm))
    }

    test("tryEval: lessThanEqualsInteger 2 2 => true") {
        val term = LessThanEqualsInteger $ 2 $ 2
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    // --- ByteString operations ---

    test("tryEval: appendByteString") {
        val a = ByteString.fromHex("dead").asTerm
        val b = ByteString.fromHex("beef").asTerm
        val term = AppendByteString $ a $ b
        assert(PartialEvaluator.tryEval(term) == Some(ByteString.fromHex("deadbeef").asTerm))
    }

    test("tryEval: lengthOfByteString") {
        val bs = ByteString.fromHex("deadbeef").asTerm
        val term = LengthOfByteString $ bs
        // deadbeef = 4 bytes (de, ad, be, ef)
        assert(PartialEvaluator.tryEval(term) == Some(4.asTerm))
    }

    test("tryEval: equalsByteString same => true") {
        val bs = ByteString.fromHex("dead").asTerm
        val term = EqualsByteString $ bs $ bs
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    // --- String operations ---

    test("tryEval: appendString") {
        val term = AppendString $ "hello" $ " world"
        assert(PartialEvaluator.tryEval(term) == Some("hello world".asTerm))
    }

    test("tryEval: equalsString same => true") {
        val term = EqualsString $ "abc" $ "abc"
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    // --- Lambda applications (CEK machine advantage) ---

    test("tryEval: (λx. addInteger x 1) 2 => 3") {
        val term = λ("x")(AddInteger $ vr"x" $ 1) $ 2
        assert(PartialEvaluator.tryEval(term) == Some(3.asTerm))
    }

    test("tryEval: (λx.λy. addInteger x y) 2 3 => 5") {
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ 2 $ 3
        assert(PartialEvaluator.tryEval(term) == Some(5.asTerm))
    }

    test("tryEval: (λx. multiplyInteger x x) 5 => 25") {
        val term = λ("x")(MultiplyInteger $ vr"x" $ vr"x") $ 5
        assert(PartialEvaluator.tryEval(term) == Some(25.asTerm))
    }

    test("tryEval: higher-order (λf.λx. f x) (λy. addInteger y 1) 2 => 3") {
        val term = λ("f", "x")(vr"f" $ vr"x") $ λ("y")(AddInteger $ vr"y" $ 1) $ 2
        assert(PartialEvaluator.tryEval(term) == Some(3.asTerm))
    }

    // --- Nested computations ---

    test("tryEval: addInteger (addInteger 1 2) (multiplyInteger 3 4) => 15") {
        val term = AddInteger $ (AddInteger $ 1 $ 2) $ (MultiplyInteger $ 3 $ 4)
        assert(PartialEvaluator.tryEval(term) == Some(15.asTerm))
    }

    test("tryEval: multiplyInteger (subtractInteger 10 3) (addInteger 1 1) => 14") {
        val term = MultiplyInteger $ (SubtractInteger $ 10 $ 3) $ (AddInteger $ 1 $ 1)
        assert(PartialEvaluator.tryEval(term) == Some(14.asTerm))
    }

    // --- Case/Constr ---

    test("tryEval: case (constr 0 [1, 2]) of [λa.λb. addInteger a b] => 3") {
        val scrut = Constr(Word64.Zero, List[Term](1, 2))
        val branch = λ("a", "b")(AddInteger $ vr"a" $ vr"b")
        val term = Case(scrut, List(branch))
        assert(PartialEvaluator.tryEval(term) == Some(3.asTerm))
    }

    test("tryEval: case (constr 1 [42]) of [λa.λb. a, λc. c] => 42") {
        val scrut = Constr(Word64.One, List[Term](42))
        val branch0 = λ("a", "b")(vr"a")
        val branch1 = λ("c")(vr"c")
        val term = Case(scrut, List(branch0, branch1))
        assert(PartialEvaluator.tryEval(term) == Some(42.asTerm))
    }

    // --- Error cases (should return None) ---

    test("tryEval: divideInteger 1 0 => None (division by zero)") {
        val term = DivideInteger $ 1 $ 0
        assert(PartialEvaluator.tryEval(term) == None)
    }

    test("tryEval: addInteger 2 x => None (free variable)") {
        val term = AddInteger $ 2 $ vr"x"
        assert(PartialEvaluator.tryEval(term) == None)
    }

    test("tryEval: partial application => None") {
        val term = AddInteger $ 2
        assert(PartialEvaluator.tryEval(term) == None)
    }

    test("tryEval: Const => None (already a value)") {
        assert(PartialEvaluator.tryEval(42.asTerm) == None)
    }

    test("tryEval: LamAbs => None (already a value)") {
        assert(PartialEvaluator.tryEval(λ("x")(vr"x")) == None)
    }

    test("tryEval: Builtin => None (already a value)") {
        assert(PartialEvaluator.tryEval(Builtin(AddInteger)) == None)
    }

    test("tryEval: Delay => None (already a value)") {
        assert(PartialEvaluator.tryEval(Delay(42.asTerm)) == None)
    }

    test("tryEval: Error => None") {
        assert(PartialEvaluator.tryEval(Error()) == None)
    }

    // --- Edge cases ---

    test("tryEval: addInteger 0 0 => 0") {
        val term = AddInteger $ 0 $ 0
        assert(PartialEvaluator.tryEval(term) == Some(0.asTerm))
    }

    test("tryEval: equalsInteger (-1) (-1) => true") {
        val term = EqualsInteger $ -1 $ -1
        assert(PartialEvaluator.tryEval(term) == Some(true.asTerm))
    }

    test("tryEval: large BigInt multiplication") {
        val big = BigInt("999999999999999999")
        val a: Term = big.asTerm
        val b: Term = big.asTerm
        val term = MultiplyInteger $ a $ b
        assert(PartialEvaluator.tryEval(term) == Some((big * big).asTerm))
    }

    test("tryEval: budget exceeded => None") {
        // Use a tiny budget that won't even cover the startup cost
        val term = AddInteger $ 1 $ 2
        val tinyBudget = ExUnits(memory = 1, steps = 1)
        assert(PartialEvaluator.tryEval(term, tinyBudget) == None)
    }

    // ========================================================================
    // Integration with Inliner
    // ========================================================================

    test("Inliner folds addInteger 2 3 => 5") {
        val term = AddInteger $ 2 $ 3
        assert(Inliner(term) == 5.asTerm)
    }

    test("Inliner: beta-reduce then fold") {
        // (λx. addInteger x 3) 2 => addInteger 2 3 => 5
        val term = λ("x")(AddInteger $ vr"x" $ 3) $ 2
        assert(Inliner(term) == 5.asTerm)
    }

    test("Inliner: multi-arg beta-reduce then fold") {
        // (λx.λy. addInteger x y) 2 3 => 5
        val term = λ("x", "y")(AddInteger $ vr"x" $ vr"y") $ 2 $ 3
        assert(Inliner(term) == 5.asTerm)
    }

    test("Inliner: nested builtins all fold") {
        // addInteger (addInteger 1 2) (multiplyInteger 3 4) => 15
        val term = AddInteger $ (AddInteger $ 1 $ 2) $ (MultiplyInteger $ 3 $ 4)
        assert(Inliner(term) == 15.asTerm)
    }

    test("Inliner: fold then dead code elimination") {
        // (λx. 42) (addInteger 2 3)
        // fold addInteger 2 3 => Const(5), then DCE since x is unused and Const(5) is pure
        val term = λ("x")(42) $ (AddInteger $ 2 $ 3)
        assert(Inliner(term) == 42.asTerm)
    }

    test("Inliner: identity elimination after fold") {
        // (λx. x) (addInteger 2 3) => 5
        val term = λ("x")(vr"x") $ (AddInteger $ 2 $ 3)
        assert(Inliner(term) == 5.asTerm)
    }

    test("Inliner: equalsInteger folds") {
        val term = EqualsInteger $ 1 $ 1
        assert(Inliner(term) == true.asTerm)
    }

    // --- Non-folding cases ---

    test("Inliner: free variable prevents fold") {
        val term = AddInteger $ vr"x" $ 3
        assert(Inliner(term) == (AddInteger $ vr"x" $ 3))
    }

    test("Inliner: both free variables prevents fold") {
        val term = AddInteger $ vr"x" $ vr"y"
        assert(Inliner(term) == (AddInteger $ vr"x" $ vr"y"))
    }

    test("Inliner: divideInteger 1 0 preserved (runtime error)") {
        val term = DivideInteger $ 1 $ 0
        assert(Inliner(term) == (DivideInteger $ 1 $ 0))
    }

    // --- Cascading across beta-reduction and fold ---

    test("Inliner: fold subexpr then beta-reduce and fold again") {
        // (λx. multiplyInteger x x) (addInteger 1 2)
        // fold addInteger 1 2 => 3, inline x => multiplyInteger 3 3 => fold => 9
        val term = λ("x")(MultiplyInteger $ vr"x" $ vr"x") $ (AddInteger $ 1 $ 2)
        assert(Inliner(term) == 9.asTerm)
    }

    test("Inliner: complex nested let-bindings fold") {
        // (λa. (λb. (λc. addInteger (multiplyInteger a b) c) 5) 3) 2
        // => addInteger (multiplyInteger 2 3) 5 => addInteger 6 5 => 11
        val term =
            λ("a")(
              λ("b")(λ("c")(AddInteger $ (MultiplyInteger $ vr"a" $ vr"b") $ vr"c") $ 5) $ 3
            ) $ 2
        assert(Inliner(term) == 11.asTerm)
    }

    // ========================================================================
    // freeVars tests (in TermAnalysis)
    // ========================================================================

    import TermAnalysis.freeVars

    test("freeVars: variable") {
        assert(vr"x".freeVars == Set("x"))
    }

    test("freeVars: constant") {
        assert(42.asTerm.freeVars == Set.empty)
    }

    test("freeVars: builtin") {
        assert(Builtin(AddInteger).freeVars == Set.empty)
    }

    test("freeVars: lambda binding") {
        assert(λ("x")(vr"x").freeVars == Set.empty)
    }

    test("freeVars: lambda with free var") {
        assert(λ("x")(vr"y").freeVars == Set("y"))
    }

    test("freeVars: apply") {
        assert((vr"f" $ vr"x").freeVars == Set("f", "x"))
    }

    test("freeVars: shadowing") {
        assert(λ("x")(λ("x")(vr"x")).freeVars == Set.empty)
    }

    test("freeVars: closed builtin application") {
        assert((AddInteger $ 1 $ 2).freeVars == Set.empty)
    }

    test("freeVars: mixed free and bound") {
        assert(λ("x")(AddInteger $ vr"x" $ vr"y").freeVars == Set("y"))
    }

    test("freeVars: Force/Delay") {
        assert(Force(Delay(vr"x")).freeVars == Set("x"))
        assert(Force(Delay(1.asTerm)).freeVars == Set.empty)
    }

    test("freeVars: Constr") {
        assert(Constr(Word64.Zero, List[Term](1, vr"x")).freeVars == Set("x"))
        assert(Constr(Word64.Zero, List[Term](1, 2)).freeVars == Set.empty)
    }

    test("freeVars: Case") {
        assert(Case(vr"s", List[Term](vr"x", 1)).freeVars == Set("s", "x"))
    }

    test("freeVars: Error") {
        assert(Error().freeVars == Set.empty)
    }
}
