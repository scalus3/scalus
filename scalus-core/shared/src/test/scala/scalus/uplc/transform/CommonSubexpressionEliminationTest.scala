package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.uplc.transform.CommonSubexpressionElimination.*
import DefaultFun.*
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.eval.PlutusVM
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions
import scala.util.Try

class CommonSubexpressionEliminationTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV3VM()
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    // ========================================================================
    // Basic extraction tests
    // ========================================================================

    test("should extract duplicated subexpression") {
        // add(mul(x, 2), mul(x, 2)) => let cse = mul(x, 2) in add(cse, cse)
        val mulX2 = MultiplyInteger $ vr"x" $ (2: Term)
        val term = AddInteger $ mulX2 $ mulX2

        val result = CommonSubexpressionElimination(term)

        // Result should be: Apply(LamAbs(__cse_0, AddInteger $ __cse_0 $ __cse_0), mul(x, 2))
        result match
            case Apply(LamAbs(cseName, body, _), expr, _) =>
                assert(cseName.startsWith("__cse_"))
                assert(expr ~=~ mulX2, s"Expected mul(x,2), got: ${expr.show}")
                // Body should use the cse variable twice
                body match
                    case Apply(Apply(Builtin(AddInteger, _), v1, _), v2, _) =>
                        assert(
                          v1 == Var(NamedDeBruijn(cseName)),
                          s"Expected Var($cseName), got: ${v1.show}"
                        )
                        assert(
                          v2 == Var(NamedDeBruijn(cseName)),
                          s"Expected Var($cseName), got: ${v2.show}"
                        )
                    case _ => fail(s"Unexpected body structure: ${body.show}")
            case _ => fail(s"Expected let-binding, got: ${result.show}")
    }

    test("should not extract single-occurrence subexpression") {
        val term = AddInteger $ (MultiplyInteger $ vr"x" $ vr"y") $ vr"z"
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract work-free terms (variables)") {
        // add(x, x) should remain unchanged -- Var is work-free
        val term = AddInteger $ vr"x" $ vr"x"
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract work-free terms (constants)") {
        val term = AddInteger $ (42: Term) $ (42: Term)
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract work-free terms (builtins)") {
        val term = Constr(Word64.Zero, List[Term](Builtin(AddInteger), Builtin(AddInteger)))
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract work-free terms (lambdas)") {
        // Lambda bodies with only variables are truly work-free at the lambda level
        val lam = λ("z")(vr"z")
        val term = Constr(Word64.Zero, List(lam, lam))
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract work-free terms (delays)") {
        val d = Delay(vr"x")
        val term = Constr(Word64.Zero, List(d, d))
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("does not share tiny Error terms") {
        val term = Constr(Word64.Zero, List[Term](Error(), Error()))
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    // ========================================================================
    // Lambda/Delay boundary tests
    // ========================================================================

    test("should extract inside lambda body when duplicated there") {
        // lam x. add(mul(x,2), mul(x,2))
        val mulX2 = MultiplyInteger $ vr"x" $ (2: Term)
        val term = λ("x")(AddInteger $ mulX2 $ mulX2)
        val result = CommonSubexpressionElimination(term)

        // Should be: lam x. let cse = mul(x,2) in add(cse, cse)
        result match
            case LamAbs(_, Apply(LamAbs(cseName, _, _), _, _), _) =>
                assert(cseName.startsWith("__cse_"))
            case _ => fail(s"Expected extraction inside lambda, got: ${result.show}")
    }

    test("should extract at common ancestor when occurrences span lambda boundary") {
        // Apply(LamAbs(x, mul(a, 2)), mul(a, 2))
        // One occurrence outside lambda, one inside => bind at root
        val mulA2 = MultiplyInteger $ vr"a" $ (2: Term)
        val term = (λ("x")(mulA2)) $ mulA2
        val result = CommonSubexpressionElimination(term)

        // Should be: let cse = mul(a, 2) in (lam x. cse) cse
        result match
            case Apply(LamAbs(cseName, Apply(LamAbs(_, _, _), _, _), _), _, _) =>
                assert(cseName.startsWith("__cse_"))
            case _ => fail(s"Expected top-level extraction, got: ${result.show}")
    }

    // ========================================================================
    // Nested duplicates
    // ========================================================================

    test("should extract largest candidate first") {
        // add(add(mul(x,y), 1), add(mul(x,y), 1))
        // Both add(mul(x,y), 1) and mul(x,y) are duplicated.
        // Largest (add(mul(x,y), 1)) should be extracted first.
        val mulXY = MultiplyInteger $ vr"x" $ vr"y"
        val addMul1 = AddInteger $ mulXY $ (1: Term)
        val term = AddInteger $ addMul1 $ addMul1

        val result = CommonSubexpressionElimination(term)

        // Should extract the larger expression
        result match
            case Apply(LamAbs(cseName, _, _), expr, _) =>
                assert(cseName.startsWith("__cse_"))
                // The extracted expression should be the larger one
                assert(
                  termSize(expr) >= termSize(mulXY),
                  s"Expected larger extraction, got: ${expr.show}"
                )
            case _ => fail(s"Expected extraction, got: ${result.show}")
    }

    // ========================================================================
    // Case branches
    // ========================================================================

    test("should not merge duplicates from sibling Case branches") {
        // case scrutinee of [mul(x,y), mul(x,y)]
        // Neither branch is an ancestor of the other, so there is no strict anchor.
        val mulXY = MultiplyInteger $ vr"x" $ vr"y"
        val term = Case(vr"scrutinee", List(mulXY, mulXY))

        val result = CommonSubexpressionElimination(term)

        assert(result ~=~ term, s"Expected unchanged branches, got: ${result.show}")
    }

    // ========================================================================
    // Semantic preservation
    // ========================================================================

    test("CSE preserves semantics of closed terms") {
        val terms = List(
          AddInteger $ (MultiplyInteger $ 3 $ 4) $ (MultiplyInteger $ 3 $ 4),
          AddInteger $ (AddInteger $ 1 $ 2) $ (AddInteger $ 1 $ 2),
          // Nested: add(add(mul(2,3), 1), add(mul(2,3), 1))
          AddInteger $
              (AddInteger $ (MultiplyInteger $ 2 $ 3) $ 1) $
              (AddInteger $ (MultiplyInteger $ 2 $ 3) $ 1)
        )
        terms.foreach { term =>
            val result = CommonSubexpressionElimination(term)
            val origEval = term.evaluate
            val cseEval = result.evaluate
            assert(
              origEval α_== cseEval,
              s"Semantics differ for ${term.show}:\n  original=${origEval.show}\n  CSE'd=${cseEval.show}\n  CSE result=${result.show}"
            )
        }
    }

    test("CSE preserves semantics with Case") {
        // case (constr 0 []) of [add(mul(2,3), mul(2,3))]
        val mulTwoThree = MultiplyInteger $ 2 $ 3
        val branch = AddInteger $ mulTwoThree $ mulTwoThree
        val term = Case(Constr(Word64.Zero, Nil), List(branch))

        val result = CommonSubexpressionElimination(term)
        val origEval = term.evaluate
        val cseEval = result.evaluate
        assert(
          origEval α_== cseEval,
          s"Semantics differ:\n  original=${origEval.show}\n  CSE'd=${cseEval.show}"
        )
    }

    // ========================================================================
    // Idempotency
    // ========================================================================

    test("CSE is idempotent") {
        val terms = List[Term](
          AddInteger $ (MultiplyInteger $ vr"x" $ vr"y") $ (MultiplyInteger $ vr"x" $ vr"y"),
          AddInteger $ vr"x" $ vr"y", // no duplicates
          λ("x")(
            AddInteger $ (MultiplyInteger $ vr"x" $ 2) $ (MultiplyInteger $ vr"x" $ 2)
          )
        )
        terms.foreach { term =>
            val once = CommonSubexpressionElimination(term)
            val twice = CommonSubexpressionElimination(once)
            assert(
              once ~=~ twice,
              s"Not idempotent for ${term.show}:\n  once=${once.show}\n  twice=${twice.show}"
            )
        }
    }

    // ========================================================================
    // No new free variables
    // ========================================================================

    test("CSE does not introduce free variables") {
        val terms = List[Term](
          AddInteger $ (MultiplyInteger $ vr"x" $ vr"y") $ (MultiplyInteger $ vr"x" $ vr"y"),
          λ("x")(
            AddInteger $ (MultiplyInteger $ vr"x" $ 2) $ (MultiplyInteger $ vr"x" $ 2)
          )
        )
        terms.foreach { term =>
            val result = CommonSubexpressionElimination(term)
            val origFree = term.freeVars
            val resultFree = result.freeVars
            assert(
              resultFree.subsetOf(origFree),
              s"New free vars ${resultFree -- origFree} in ${term.show} => ${result.show}"
            )
        }
    }

    // ========================================================================
    // TermKey tests
    // ========================================================================

    test("CSE shares alpha-equivalent applications and retains the original representative") {
        val first = vr"map" $ λ("x")(AddInteger $ vr"x" $ (1: Term)) $ vr"xs"
        val second = vr"map" $ λ("y")(AddInteger $ vr"y" $ (1: Term)) $ vr"xs"
        val term = Constr(Word64.Zero, List(first, second))
        CommonSubexpressionElimination(term) match
            case Apply(LamAbs(name, Constr(_, List(a, b), _), _), representative, _) =>
                assert(representative ~=~ first)
                assert(a ~=~ Var(NamedDeBruijn(name)))
                assert(b ~=~ Var(NamedDeBruijn(name)))
            case other => fail(s"Expected shared application, got ${other.show}")
    }

    test("CSE shares alpha-equivalent lambdas while keeping their capture in scope") {
        // Keep these out of the syntactic-let path, which visits only the body and argument.
        val left = Force(Delay(λ("left")(AddInteger $ vr"outer" $ vr"left")))
        val right = Force(Delay(λ("right")(AddInteger $ vr"outer" $ vr"right")))
        val term = λ("outer")(AddInteger $ (left $ (2: Term)) $ (right $ (3: Term))) $
            (10: Term)
        val cse = new CommonSubexpressionElimination()
        val result = cse(term)
        assert(cse.logs.nonEmpty)
        assert(result.freeVars.isEmpty)
        assert(result.evaluate ~=~ (25: Term))
        assert(result.evaluate ~=~ term.evaluate)
    }

    test("AlphaTermKey: bound renames have equal keys and hashes") {
        for (a, b) <- List(("x", "y"), ("i0", "amount"), ("x_cse", "x")) do
            val first = λ(a)(AddInteger $ vr(a) $ vr"capture")
            val second = λ(b)(AddInteger $ vr(b) $ vr"capture")
            val key1 = new AlphaTermKey(first)
            val key2 = new AlphaTermKey(second)
            assert(key1 == key2)
            assert(key2 == key1)
            assert(key1.hashCode == key2.hashCode)
            assert(Set(key1).contains(key2))
            assert(key1.term eq first)
            // CCE's shared structural key deliberately remains name-sensitive.
            assert(new TermKey(first) != new TermKey(second))
    }

    test("AlphaTermKey: free names and their order remain significant") {
        def candidate(a: String, b: String): Term =
            λ("bound")(Constr(Word64.Zero, List(vr(a), vr(b))))
        val xy = new AlphaTermKey(candidate("x", "y"))
        assert(xy != new AlphaTermKey(candidate("y", "x")))
        assert(xy != new AlphaTermKey(candidate("x", "z")))
        assert(xy != new AlphaTermKey(candidate("x", "x")))
    }

    test("AlphaTermKey: shadowing distinguishes inner and outer references") {
        val shadowed = new AlphaTermKey(λ("x")(λ("x")(vr"x")))
        val inner = new AlphaTermKey(λ("a")(λ("b")(vr"b")))
        val outer = new AlphaTermKey(λ("a")(λ("b")(vr"a")))
        assert(shadowed == inner)
        assert(shadowed.hashCode == inner.hashCode)
        assert(shadowed != outer)
    }

    test("AlphaTermKey: canonical binder names cannot capture a free i0") {
        val free = new AlphaTermKey(λ("x")(vr"i0"))
        val renamed = new AlphaTermKey(λ("y")(vr"i0"))
        val bound = new AlphaTermKey(λ("x")(vr"x"))
        assert(free == renamed)
        assert(free.hashCode == renamed.hashCode)
        // The cheap shape hash intentionally collides here; canonical equality must disambiguate.
        assert(free.hashCode == bound.hashCode)
        assert(free != bound)
        assert(Set(free, bound).size == 2)
    }

    test("AlphaTermKey: annotations are ignored and the original indices are retained") {
        val original = LamAbs(
          "x",
          Var(NamedDeBruijn("capture", 42), UplcAnnotation(functionName = "first"))
        )
        val renamed = λ("y")(vr"capture")
        val key = new AlphaTermKey(original)
        assert(key == new AlphaTermKey(renamed))
        assert(key.hashCode == new AlphaTermKey(renamed).hashCode)
        assert(key.term eq original)
    }

    test("TermKey: same structure, different annotations => equal") {
        val t1 = Apply(Builtin(AddInteger), 1.asTerm)
        val t2 = Apply(
          Builtin(AddInteger, UplcAnnotation.empty),
          Const(Constant.Integer(1), UplcAnnotation.empty)
        )
        val key1 = new TermKey(t1)
        val key2 = new TermKey(t2)
        assert(key1 == key2)
        assert(key1.hashCode == key2.hashCode)
    }

    test("TermKey: different structure => not equal") {
        val t1 = Apply(Builtin(AddInteger), 1.asTerm)
        val t2 = Apply(Builtin(MultiplyInteger), 1.asTerm)
        val key1 = new TermKey(t1)
        val key2 = new TermKey(t2)
        assert(key1 != key2)
    }

    // ========================================================================
    // termSize tests
    // ========================================================================

    test("termSize counts nodes correctly") {
        assert(termSize(vr"x") == 1)
        assert(termSize(42.asTerm) == 1)
        assert(termSize(λ("x")(vr"x")) == 2)
        assert(termSize(AddInteger $ vr"x" $ vr"y") == 5) // Apply(Apply(Builtin, Var), Var)
    }

    // ========================================================================
    // Edge cases
    // ========================================================================

    test("CSE on simple term with no duplicates returns same structure") {
        val term = AddInteger $ vr"x" $ vr"y"
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    test("CSE on single variable returns same") {
        val term = vr"x"
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    test("CSE on constant returns same") {
        val term: Term = 42
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    test("CSE on Error returns same") {
        val term = Error()
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    test("CSE on empty Constr returns same") {
        val term = Constr(Word64.Zero, Nil)
        val result = CommonSubexpressionElimination(term)
        assert(result ~=~ term)
    }

    // ========================================================================
    // Integration with optimizer pipeline
    // ========================================================================

    test("V3Optimizer with CSE preserves semantics") {
        // Build a term with duplicate subexpressions that can be CSE'd
        // (λf. add(f 3, f 3)) (λx. mul(x, x))
        // After inlining: add(mul(3,3), mul(3,3)) => add(9, 9) => 18
        val term = λ("f")(AddInteger $ (vr"f" $ 3) $ (vr"f" $ 3)) $ λ("x")(
          MultiplyInteger $ vr"x" $ vr"x"
        )
        val optimizer = new V3Optimizer(cseIterations = 2)
        val result = optimizer(term)
        val origEval = term.evaluate
        val optEval = result.evaluate
        assert(
          origEval α_== optEval,
          s"Semantics differ:\n  original=${origEval.show}\n  optimized=${optEval.show}"
        )
    }

    test("V3Optimizer with CSE disabled (0 iterations) preserves semantics") {
        val mulXY = MultiplyInteger $ vr"x" $ vr"y"
        val term = λ("x", "y")(AddInteger $ mulXY $ mulXY) $ 3 $ 4
        val optimizer = new V3Optimizer(cseIterations = 0, cceEnabled = false)
        val result = optimizer(term)
        val origEval = term.evaluate
        val optEval = result.evaluate
        assert(origEval α_== optEval)
    }

    // ========================================================================
    // Logging
    // ========================================================================

    test("CSE produces log entries when extracting") {
        val cse = new CommonSubexpressionElimination()
        val mulX2 = MultiplyInteger $ vr"x" $ (2: Term)
        cse(AddInteger $ mulX2 $ mulX2)
        assert(cse.logs.nonEmpty, "Expected log entries for CSE extraction")
        assert(cse.logs.exists(_.contains("CSE:")))
    }

    test("CSE produces no log entries when nothing to extract") {
        val cse = new CommonSubexpressionElimination()
        cse(AddInteger $ vr"x" $ vr"y")
        assert(cse.logs.isEmpty, s"Expected no log entries, got: ${cse.logs}")
    }

    // ========================================================================
    // Three or more occurrences
    // ========================================================================

    test("should extract expression with three occurrences") {
        val mulXY = MultiplyInteger $ vr"x" $ vr"y"
        val term = Constr(Word64.Zero, List(mulXY, mulXY, mulXY))
        val result = CommonSubexpressionElimination(term)

        // Should extract mul(x,y)
        result match
            case Apply(LamAbs(cseName, _, _), _, _) =>
                assert(cseName.startsWith("__cse_"))
            case _ => fail(s"Expected extraction, got: ${result.show}")
    }

    // ========================================================================
    // Force expressions (non-trivial)
    // ========================================================================

    test("should share repeated forced calls even when the binding saves no bits") {
        // Force(some_expr) appearing twice
        val forceExpr = Force(vr"f" $ vr"x")
        val term = Constr(Word64.Zero, List(forceExpr, forceExpr))
        val result = CommonSubexpressionElimination(term)
        // The binding saves no bits, but avoids repeating the unknown call and its force.
        assert(result ~!=~ term)
        assert(CommonSubexpressionElimination(result) ~=~ result)
    }

    // ========================================================================
    // Property-based tests
    // ========================================================================

    /** Generator for closed, evaluable terms with potential duplicate subexpressions. Builds terms
      * from saturated builtin applications on constants.
      */
    private val genClosedEvaluableTerm: Gen[Term] = {
        val genInt: Gen[Term] = Gen.choose(-100, 100).map(n => Const(Constant.Integer(n)))

        def genBinOp(depth: Int): Gen[Term] =
            if depth <= 0 then genInt
            else
                for
                    a <- genBinOp(depth - 1)
                    b <- genBinOp(depth - 1)
                    op <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
                yield op $ a $ b

        // Generate a term with intentional duplicates
        def genWithDuplicates: Gen[Term] =
            for
                sub <- genBinOp(1)
                op <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
            yield op $ sub $ sub

        Gen.oneOf(genBinOp(2), genWithDuplicates)
    }

    /** Generator for terms with guaranteed duplicate subexpressions. */
    private val genTermWithDuplicateSubexpr: Gen[Term] = {
        val genInt: Gen[Term] = Gen.choose(-100, 100).map(n => Const(Constant.Integer(n)))
        for
            a <- genInt
            b <- genInt
            op1 <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
            op2 <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
            sub = op1 $ a $ b
        yield op2 $ sub $ sub
    }

    test("property: CSE preserves semantics on closed evaluable terms") {
        forAll(genClosedEvaluableTerm) { term =>
            val cseResult = CommonSubexpressionElimination(term)
            val original = Try(term.evaluate).toOption
            val optimized = Try(cseResult.evaluate).toOption
            (original, optimized) match
                case (Some(a), Some(b)) =>
                    assert(
                      a α_== b,
                      s"Semantics differ for ${term.show}:\n  original=${a.show}\n  CSE'd=${b.show}"
                    )
                case (None, None) => succeed // both errored, ok
                case _ =>
                    fail(
                      s"Divergent behavior for ${term.show}:\n  original=$original\n  CSE'd=$optimized"
                    )
        }
    }

    test("property: CSE is idempotent on arbitrary terms") {
        forAll { (term: Term) =>
            val once = CommonSubexpressionElimination(term)
            val twice = CommonSubexpressionElimination(once)
            assert(
              once ~=~ twice,
              s"Not idempotent:\n  once=${once.show}\n  twice=${twice.show}"
            )
        }
    }

    test("property: CSE never introduces new free variables") {
        forAll { (term: Term) =>
            val result = CommonSubexpressionElimination(term)
            val origFree = term.freeVars
            val resultFree = result.freeVars
            assert(
              resultFree.subsetOf(origFree),
              s"New free vars ${resultFree -- origFree} in:\n  input=${term.show}\n  output=${result.show}"
            )
        }
    }

    test("property: CSE does not increase term size for terms with duplicates") {
        forAll(genTermWithDuplicateSubexpr) { term =>
            val result = CommonSubexpressionElimination(term)
            // CSE adds 2 nodes per extraction (LamAbs + Apply wrapper) but saves
            // (N-1) * subexprSize nodes. For N>=2 and subexprSize>=3 this is always a win.
            // We check that the result is not larger than original + small overhead.
            assert(
              termSize(result) <= termSize(term) + 2,
              s"CSE increased size from ${termSize(term)} to ${termSize(result)} for ${term.show}"
            )
        }
    }

    test("property: CSE preserves semantics on terms with guaranteed duplicates") {
        forAll(genTermWithDuplicateSubexpr) { term =>
            val result = CommonSubexpressionElimination(term)
            val original = Try(term.evaluate).toOption
            val optimized = Try(result.evaluate).toOption
            (original, optimized) match
                case (Some(a), Some(b)) =>
                    assert(a α_== b, s"Semantics differ:\n  original=${a.show}\n  CSE'd=${b.show}")
                case (None, None) => succeed
                case _ =>
                    fail(s"Divergent behavior:\n  original=$original\n  CSE'd=$optimized")
        }
    }

    test("property: TermKey hash consistency") {
        forAll { (term: Term) =>
            val key1 = new TermKey(term)
            val key2 = new TermKey(term)
            assert(key1 == key2, "Same term should produce equal keys")
            assert(key1.hashCode == key2.hashCode, "Equal keys must have equal hashCodes")
        }
    }

    // ========================================================================
    // Variable shadowing safety
    // ========================================================================

    test("CSE should not extract across shadowed variable bindings") {
        // case scrutinee of [
        //   lam x. headList(x)    // x is field from branch 0
        //   lam x. headList(x)    // x is field from branch 1, different binding
        // ]
        // CSE should NOT extract headList(x) because x is shadowed between branches
        val headList = Force(Builtin(DefaultFun.HeadList))
        val branch0 = λ("x")(headList $ vr"x")
        val branch1 = λ("x")(headList $ vr"x")
        val term = Case(vr"scrutinee", List(branch0, branch1))

        val cse = new CommonSubexpressionElimination()
        val result = cse(term)
        // headList(x) should NOT be extracted because x is bound by different lambdas
        assert(
          cse.logs.isEmpty || !cse.logs.exists(_.contains("headList")),
          s"CSE should not extract headList(x) across shadowed bindings: ${cse.logs}"
        )
    }

    test("CSE should not hoist partial builtins across Case boundaries") {
        // Simulates the NormalizedInterval pattern:
        // case unConstrData(x) of [
        //   lam f0. headList(sndPair(unConstrData(y)))    // branch 0: variant with fields
        //   lam f1. headList(sndPair(unConstrData(y)))    // branch 1: variant with fields
        //   lam f2. 42                                     // branch 2: variant with no fields
        // ]
        // headList(sndPair(unConstrData(y))) appears in 2 branches but not branch 2.
        // Hoisting above the Case would cause headList to fail on empty field list.
        val unConstr = Force(Force(Builtin(DefaultFun.UnConstrData)))
        val sndPair = Force(Force(Builtin(DefaultFun.SndPair)))
        val headList = Force(Builtin(DefaultFun.HeadList))
        val partialExpr = headList $ (sndPair $ (unConstr $ vr"y"))
        val branch0 = λ("f0")(partialExpr)
        val branch1 = λ("f1")(partialExpr)
        val branch2 = λ("f2")(42: Term)
        val term = Case(unConstr $ vr"x", List(branch0, branch1, branch2))

        val cse = new CommonSubexpressionElimination()
        val result = cse(term)
        // Should NOT extract headList(sndPair(unConstrData(y))) because it crosses
        // a Case boundary and contains partial builtins
        assert(
          cse.logs.isEmpty || !cse.logs.exists(_.contains("headList")),
          s"CSE should not hoist partial builtins across Case: ${cse.logs}"
        )
    }

    test("CSE on List.at compiled term preserves semantics") {
        import scalus.cardano.onchain.plutus.prelude.List
        import scalus.compiler.Options
        import scalus.uplc.PlutusV3
        val opts = Options(
          generateErrorTraces = true,
          optimizeUplc = true,
          cseIterations = 0, // disable CSE to get pre-CSE UPLC
          cceEnabled = false // disable CCE too
        )
        given Options = opts

        val compiled = PlutusV3.compile(List.singleton(BigInt(1)).at(0))
        val noCse = compiled.program.term

        val cse = new CommonSubexpressionElimination()
        val withCse = cse(noCse)

        assert(Try(noCse.evaluate).isSuccess, "Without CSE should succeed")
        assert(Try(withCse.evaluate).isSuccess, "With CSE should succeed")
    }

    // ========================================================================
    // termBits tests
    // ========================================================================

    private def unsupportedConstants: List[Constant] = {
        import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
        val g1 = Constant.BLS12_381_G1_Element(G1Element.generator)
        val g2 = Constant.BLS12_381_G2_Element(G2Element.generator)
        List(
          g1,
          g2,
          Constant.List(g1.tpe, List(g1)),
          Constant.Pair(Constant.Integer(1), g2),
          Constant.Array(g1.tpe, Vector(g1))
        )
    }

    test("CSE rejects constants whose Flat size is unavailable, including nested BLS values") {
        for constant <- unsupportedConstants do
            val repeated = Const(constant)
            val term = Constr(Word64.Zero, List(repeated, repeated))
            assert(CommonSubexpressionElimination(term) eq term)
    }

    test("unavailable sizes propagate through expressions and cannot become profitable") {
        val sizeOf = cachedTermBits()
        for constant <- unsupportedConstants do
            val c = Const(constant)
            for term <- List[Term](c, Delay(c), Apply(vr"f", c), Constr(Word64.Zero, List(c))) do
                assert(sizeOf(term).isEmpty)
                assert(SharingCost.savingLovelace(term, 3) == Double.NegativeInfinity)
    }

    test("empty BLS containers remain sizeable when the Flat encoder supports them") {
        val sizeOf = cachedTermBits()
        for tpe <- List(DefaultUni.BLS12_381_G1_Element, DefaultUni.BLS12_381_G2_Element) do
            val term = Const(Constant.List(tpe, Nil))
            assert(
              sizeOf(term).contains(summon[scalus.serialization.flat.Flat[Term]].bitSize(term))
            )
    }

    test("cached sizing encodes the same constant only once across different term wrappers") {
        var reads = 0
        val values = new IndexedSeq[Constant] {
            def length: Int = 3
            def apply(index: Int): Constant = {
                reads += 1
                Constant.Integer(index + 1000)
            }
        }
        val constant = Constant.Array(DefaultUni.Integer, values)
        val sizeOf = cachedTermBits()
        val first = sizeOf(Const(constant)).get
        val firstReads = reads
        assert(firstReads > 0)
        assert(sizeOf(Const(constant)).contains(first))
        assert(sizeOf(Delay(Const(constant))).contains(first + TermTagBits))
        assert(reads == firstReads)
        // Caches belong to a single pass, so a later invocation sizes the constant again.
        assert(cachedTermBits()(Const(constant)).contains(first))
        assert(reads > firstReads)
    }

    test("termBits agrees with the flat encoder on a de Bruijn term") {
        // Covers Var, Apply, LamAbs, Force, Delay, Builtin, Const.
        val named = λ(x => Force(Delay(AddInteger $ x $ (Force(Builtin(HeadList)) $ 42))))
        val db = DeBruijn.deBruijnTerm(named)
        assert(termBits(db) == summon[scalus.serialization.flat.Flat[Term]].bitSize(db))
    }

    test("termBits agrees with the flat encoder on arbitrary closed terms") {
        // The flat encoder is the oracle: close the term, assign de Bruijn indices, compare.
        // This is what pins termBits to the encoder as the encoder evolves.
        forAll { (t: Term) =>
            val closed = freeVars(t).foldLeft(t)((acc, n) => LamAbs(n, acc))
            val db = DeBruijn.deBruijnTerm(closed)
            whenever(maxVarIndex(db) < 128) {
                assert(termBits(db) == summon[scalus.serialization.flat.Flat[Term]].bitSize(db))
            }
        }
    }

    test("termBits agrees with Flat on index-zero names, lambdas and CCE holes") {
        val terms = List(
          vr"free",
          λ("x")(vr"x"),
          Force(Builtin(HeadList)) $ vr"__CCE_HOLE__",
          Var(NamedDeBruijn("largestSingleGroupIndex", 127))
        )
        for term <- terms do
            assert(termBits(term) == summon[scalus.serialization.flat.Flat[Term]].bitSize(term))
    }

    test("termBits deliberately uses a fixed variable width across index boundaries") {
        val term = Var(NamedDeBruijn("twoGroupIndex", 128))
        assert(termBits(term) + 8 == summon[scalus.serialization.flat.Flat[Term]].bitSize(term))
        // Named index-zero holes can be sized directly. Converting an open template to de Bruijn
        // introduces negative indices, which Flat rejects but the fixed-width estimate ignores.
        val template = Force(Builtin(HeadList)) $ vr"__CCE_HOLE__"
        val db = DeBruijn.deBruijnTerm(template)
        assert(maxVarIndex(db) < 0, "expected a negative index for the free hole sentinel")
        assertThrows[IllegalArgumentException] {
            summon[scalus.serialization.flat.Flat[Term]].bitSize(db)
        }
        assert(termBits(db) > 0)
    }

    private def maxVarIndex(t: Term): Int = t match
        case Var(n, _)          => n.index
        case Const(_, _)        => Int.MinValue
        case Builtin(_, _)      => Int.MinValue
        case Error(_)           => Int.MinValue
        case LamAbs(_, body, _) => maxVarIndex(body)
        case Apply(f, arg, _)   => maxVarIndex(f) max maxVarIndex(arg)
        case Force(inner, _)    => maxVarIndex(inner)
        case Delay(inner, _)    => maxVarIndex(inner)
        case Constr(_, args, _) => args.map(maxVarIndex).foldLeft(Int.MinValue)(_ max _)
        case Case(arg, cases, _) =>
            cases.map(maxVarIndex).foldLeft(maxVarIndex(arg))(_ max _)

    test("termBits agrees with the flat encoder on Constr, Case and Error") {
        val named = Case(
          Constr(Word64.Zero, List[Term](1: Term, Error())),
          List[Term](λ(a => λ(b => a)))
        )
        val db = DeBruijn.deBruijnTerm(named)
        assert(termBits(db) == summon[scalus.serialization.flat.Flat[Term]].bitSize(db))
    }
}
