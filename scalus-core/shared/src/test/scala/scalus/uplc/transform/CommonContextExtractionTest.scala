package scalus.uplc
package transform

import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.transform.TermAnalysis.freeVars
import scalus.uplc.transform.CommonContextExtraction.*
import DefaultFun.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.Word64
import scalus.uplc.eval.PlutusVM
import scalus.uplc.test.ArbitraryInstances
import org.scalacheck.Gen

import scala.language.implicitConversions
import scala.util.Try

class CommonContextExtractionTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // ========================================================================
    // decomposeRightSpine tests
    // ========================================================================

    test("decomposeRightSpine: simple Apply(f, x) produces (f(HOLE), x)") {
        val t = AddInteger $ vr"x"
        val result = decomposeRightSpine(t)
        result match
            case Some((template, leaf)) =>
                assert(leaf ~=~ vr"x")
                assert(template ~=~ (AddInteger $ holeSentinel))
            case None => fail("Expected decomposition")
    }

    test("decomposeRightSpine: nested Apply chain f(g(x)) produces (f(g(HOLE)), x)") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        // headList(unConstrData(x))
        val t = headList $ (unConstr $ vr"x")
        val result = decomposeRightSpine(t)
        result match
            case Some((template, leaf)) =>
                assert(leaf ~=~ vr"x")
                val expected = headList $ (unConstr $ holeSentinel)
                assert(template ~=~ expected, s"Expected ${expected.show}, got ${template.show}")
            case None => fail("Expected decomposition")
    }

    test("decomposeRightSpine: deeper chain f(g(h(x)))") {
        val headList = Force(Builtin(HeadList))
        val tailList = Force(Builtin(TailList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        // headList(tailList(unConstrData(x)))
        val t = headList $ (tailList $ (unConstr $ vr"x"))
        val result = decomposeRightSpine(t)
        result match
            case Some((template, leaf)) =>
                assert(leaf ~=~ vr"x")
                val expected = headList $ (tailList $ (unConstr $ holeSentinel))
                assert(template ~=~ expected, s"Expected ${expected.show}, got ${template.show}")
            case None => fail("Expected decomposition")
    }

    test("decomposeRightSpine: non-Apply returns None") {
        assert(decomposeRightSpine(vr"x").isEmpty)
        assert(decomposeRightSpine(42.asTerm).isEmpty)
        assert(decomposeRightSpine(Builtin(AddInteger)).isEmpty)
    }

    // ========================================================================
    // decompose tests
    // ========================================================================

    test("decompose: Apply(f, x) yields hole-at-f and hole-at-arg") {
        val t = AddInteger $ vr"x"
        val results = decompose(t).toList
        // Should have at least: (HOLE $ x, AddInteger) and (AddInteger $ HOLE, x)
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ (holeSentinel $ vr"x")) && (leaf ~=~ AddInteger)
          },
          s"Missing hole-at-f decomposition in ${results.map(_._1.show)}"
        )
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ (AddInteger $ holeSentinel)) && (leaf ~=~ vr"x")
          },
          s"Missing hole-at-arg decomposition in ${results.map(_._1.show)}"
        )
    }

    test("decompose: nested Apply yields deeper decompositions") {
        // f(g(x)) should yield decompositions with hole at f, at g(x), at g, and at x
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val t = headList $ (unConstr $ vr"x")
        val results = decompose(t).toList
        // Should include a deep decomposition: headList(unConstrData(HOLE)) with leaf x
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ (headList $ (unConstr $ holeSentinel))) && (leaf ~=~ vr"x")
          },
          s"Missing deep right-spine decomposition"
        )
        // Should include hole at function position: HOLE(unConstrData(x))
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ (holeSentinel $ (unConstr $ vr"x"))) && (leaf ~=~ headList)
          },
          s"Missing hole-at-f decomposition"
        )
    }

    test("decompose: Force yields hole at inner") {
        val t = Force(Builtin(HeadList))
        val results = decompose(t).toList
        assert(results.exists { case (tmpl, leaf) =>
            (tmpl ~=~ Force(holeSentinel)) && (leaf ~=~ Builtin(HeadList))
        })
    }

    test("decompose: Case yields hole at scrutinee") {
        val branch0: Term = λ("x")(vr"x")
        val branch1: Term = λ("x")(42)
        val t = Case(vr"s", List(branch0, branch1))
        val results = decompose(t).toList
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ Case(holeSentinel, List(branch0, branch1))) && (leaf ~=~ vr"s")
          },
          s"Missing hole-at-scrutinee decomposition"
        )
    }

    test("decompose: Constr yields hole at each arg") {
        val t = Constr(Word64.Zero, List[Term](vr"a", vr"b", vr"c"))
        val results = decompose(t).toList
        // Hole at position 0
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ Constr(Word64.Zero, List(holeSentinel, vr"b", vr"c"))) && (leaf ~=~ vr"a")
          },
          "Missing hole at arg 0"
        )
        // Hole at position 1
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ Constr(Word64.Zero, List(vr"a", holeSentinel, vr"c"))) && (leaf ~=~ vr"b")
          },
          "Missing hole at arg 1"
        )
        // Hole at position 2
        assert(
          results.exists { case (tmpl, leaf) =>
              (tmpl ~=~ Constr(Word64.Zero, List(vr"a", vr"b", holeSentinel))) && (leaf ~=~ vr"c")
          },
          "Missing hole at arg 2"
        )
    }

    test("decompose: leaf nodes return empty") {
        assert(decompose(vr"x").isEmpty)
        assert(decompose(42.asTerm).isEmpty)
        assert(decompose(Builtin(AddInteger)).isEmpty)
        assert(decompose(Error()).isEmpty)
    }

    test("decompose: LamAbs returns empty (scope boundary)") {
        assert(decompose(λ("x")(vr"x")).isEmpty)
    }

    // ========================================================================
    // matchTemplate tests
    // ========================================================================

    test("matchTemplate: matches simple Apply with hole at arg") {
        val template = AddInteger $ holeSentinel
        val term = AddInteger $ vr"x"
        val result = matchTemplate(template, term)
        assert(result.contains(vr"x") || result.exists(_ ~=~ vr"x"))
    }

    test("matchTemplate: matches Apply with hole at function") {
        val template = holeSentinel $ vr"x"
        val term = AddInteger $ vr"x"
        val result = matchTemplate(template, term)
        assert(result.exists(_ ~=~ AddInteger))
    }

    test("matchTemplate: matches Case with hole at scrutinee") {
        val branch: Term = λ("z")(vr"z")
        val template = Case(holeSentinel, List(branch))
        val term = Case(vr"x", List(branch))
        val result = matchTemplate(template, term)
        assert(result.exists(_ ~=~ vr"x"))
    }

    test("matchTemplate: matches Constr with hole at specific arg") {
        val template = Constr(Word64.Zero, List(vr"a", holeSentinel, vr"c"))
        val term = Constr(Word64.Zero, List[Term](vr"a", vr"b", vr"c"))
        val result = matchTemplate(template, term)
        assert(result.exists(_ ~=~ vr"b"))
    }

    test("matchTemplate: returns None on structural mismatch") {
        val template = AddInteger $ holeSentinel
        val term = MultiplyInteger $ vr"x"
        assert(matchTemplate(template, term).isEmpty)
    }

    test("matchTemplate: returns None when no hole in template") {
        val template = AddInteger $ vr"x"
        val term = AddInteger $ vr"x"
        assert(matchTemplate(template, term).isEmpty)
    }

    test("matchTemplate: returns None on arity mismatch") {
        val template = Constr(Word64.Zero, List(holeSentinel, vr"b"))
        val term = Constr(Word64.Zero, List[Term](vr"a", vr"b", vr"c"))
        assert(matchTemplate(template, term).isEmpty)
    }

    // ========================================================================
    // containsHole tests
    // ========================================================================

    test("containsHole: detects hole sentinel") {
        assert(containsHole(holeSentinel))
        assert(containsHole(AddInteger $ holeSentinel))
        assert(containsHole(Force(holeSentinel)))
        assert(!containsHole(vr"x"))
        assert(!containsHole(AddInteger $ vr"x"))
    }

    // ========================================================================
    // replaceHole tests
    // ========================================================================

    test("replaceHole replaces HOLE sentinel with replacement") {
        val template = AddInteger $ holeSentinel
        val result = replaceHole(template, vr"y")
        assert(result ~=~ (AddInteger $ vr"y"))
    }

    test("replaceHole in nested template") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val template = headList $ (unConstr $ holeSentinel)
        val result = replaceHole(template, vr"z")
        assert(result ~=~ (headList $ (unConstr $ vr"z")))
    }

    // ========================================================================
    // templateHeadName tests
    // ========================================================================

    test("templateHeadName: single builtin uses full name") {
        val template = Force(Builtin(HeadList)) $ holeSentinel
        assert(templateHeadName(template) == "HeadList")
    }

    test("templateHeadName: multiple builtins abbreviated") {
        val headList = Force(Builtin(HeadList))
        val template = headList $ (Force(Force(Builtin(UnConstrData))) $ holeSentinel)
        assert(templateHeadName(template) == "Hd_UnConstr")
    }

    test("templateHeadName: deep chain all abbreviated") {
        val hd = Force(Builtin(HeadList))
        val tl = Force(Builtin(TailList))
        val snd = Force(Force(Builtin(SndPair)))
        val unc = Force(Force(Builtin(UnConstrData)))
        val template = hd $ (tl $ (snd $ (unc $ holeSentinel)))
        assert(templateHeadName(template) == "Hd_Tl_Snd_UnConstr")
    }

    test("templateHeadName: variable head with multiple fns") {
        val template = vr"myFunc" $ (vr"g" $ holeSentinel)
        assert(templateHeadName(template) == "myFunc_g")
    }

    test("templateHeadName: single variable uses full name") {
        val template = vr"myFunc" $ holeSentinel
        assert(templateHeadName(template) == "myFunc")
    }

    test("templateHeadName: non-Apply returns fallback") {
        assert(templateHeadName(holeSentinel) == "app")
    }

    // ========================================================================
    // Basic extraction tests
    // ========================================================================

    test("should extract common context with different leaves") {
        // headList(unConstrData(x)) and headList(unConstrData(y))
        // => let f = \a -> headList(unConstrData(a)) in constr(f(x), f(y))
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain1 = headList $ (unConstr $ vr"x")
        val chain2 = headList $ (unConstr $ vr"y")
        val term = Constr(Word64.Zero, List(chain1, chain2))

        val cce = new CommonContextExtraction()
        val result = cce(term)

        assert(result ~!=~ term, s"Expected transformation, got: ${result.show}")
        assert(cce.logs.nonEmpty, "Expected log entries for CCE extraction")
        assert(cce.logs.exists(_.contains("CCE:")))
        // The extracted lambda should be named after the chain builtins (abbreviated)
        assert(
          cce.logs.exists(_.contains("as __cce_Hd_UnConstr")),
          s"Expected __cce_Hd_UnConstr in logs: ${cce.logs}"
        )
    }

    test("should not extract when only 1 occurrence") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val term = headList $ (unConstr $ vr"x")

        val result = CommonContextExtraction(term)
        assert(result ~=~ term, s"Expected unchanged, got: ${result.show}")
    }

    test("should not extract when all leaves are identical (CSE handles this)") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain = headList $ (unConstr $ vr"x")
        val term = Constr(Word64.Zero, List(chain, chain))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(
          cce.logs.isEmpty,
          s"CCE should not extract identical leaves (CSE handles it): ${cce.logs}"
        )
    }

    test("should not extract when template is too small") {
        // add(x) and add(y) — template is Apply(Builtin(Add), HOLE) = size 2
        val chain1 = AddInteger $ vr"x"
        val chain2 = AddInteger $ vr"y"
        val term = Constr(Word64.Zero, List(chain1, chain2))

        val result = CommonContextExtraction(term)
        assert(result ~=~ term, s"Expected unchanged (too small template), got: ${result.show}")
    }

    // ========================================================================
    // Shadowing safety
    // ========================================================================

    test("should not extract across shadowed variable bindings") {
        // case scrutinee of [
        //   lam z. headList(tailList(unConstrData(z)))    -- z bound by branch 0
        //   lam z. headList(tailList(unConstrData(z)))    -- z bound by branch 1
        // ]
        // Leaves are identical (both Var(z)) so distinct-leaf check filters it out
        val headList = Force(Builtin(HeadList))
        val tailList = Force(Builtin(TailList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val branch0 = λ("z")(headList $ (tailList $ (unConstr $ vr"z")))
        val branch1 = λ("z")(headList $ (tailList $ (unConstr $ vr"z")))
        val term = Case(vr"scrutinee", List(branch0, branch1))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(
          cce.logs.isEmpty,
          s"CCE should not extract when leaves are identical: ${cce.logs}"
        )
    }

    // ========================================================================
    // Conditional boundary safety
    // ========================================================================

    test("should not hoist partial builtins across Case boundaries") {
        val headList = Force(Builtin(HeadList))
        val tailList = Force(Builtin(TailList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain1 = headList $ (tailList $ (unConstr $ vr"x"))
        val chain2 = headList $ (tailList $ (unConstr $ vr"y"))
        val branch0 = chain1
        val branch1 = chain2
        val branch2: Term = 42
        val term = Case(vr"scrutinee", List(branch0, branch1, branch2))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(
          cce.logs.isEmpty || !cce.logs.exists(_.contains("CCE:")),
          s"CCE should not hoist partial builtins across Case: ${cce.logs}"
        )
    }

    // ========================================================================
    // Semantic preservation
    // ========================================================================

    test("CCE preserves semantics of closed terms with common context") {
        // add(mul(x, 2), 1) applied to different x values
        // Template: add(mul(HOLE, 2), 1) — same function context, different leaves
        val chain1 = AddInteger $ (MultiplyInteger $ (3: Term) $ (2: Term)) $ (1: Term)
        val chain2 = AddInteger $ (MultiplyInteger $ (5: Term) $ (2: Term)) $ (1: Term)
        val term = AddInteger $ chain1 $ chain2

        val result = CommonContextExtraction(term)
        val origEval = term.evaluate
        val cceEval = result.evaluate
        assert(
          origEval α_== cceEval,
          s"Semantics differ:\n  original=${origEval.show}\n  CCE'd=${cceEval.show}\n  CCE result=${result.show}"
        )
    }

    // ========================================================================
    // Idempotency
    // ========================================================================

    test("CCE is idempotent") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val terms = List[Term](
          Constr(
            Word64.Zero,
            List(headList $ (unConstr $ vr"x"), headList $ (unConstr $ vr"y"))
          ),
          AddInteger $ vr"x" $ vr"y" // no extraction target
        )
        terms.foreach { term =>
            val once = CommonContextExtraction(term)
            val twice = CommonContextExtraction(once)
            assert(
              once ~=~ twice,
              s"Not idempotent for ${term.show}:\n  once=${once.show}\n  twice=${twice.show}"
            )
        }
    }

    // ========================================================================
    // No new free variables
    // ========================================================================

    test("CCE does not introduce free variables (except __cce_N names)") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val terms = List[Term](
          Constr(
            Word64.Zero,
            List(headList $ (unConstr $ vr"x"), headList $ (unConstr $ vr"y"))
          )
        )
        terms.foreach { term =>
            val result = CommonContextExtraction(term)
            val origFree = term.freeVars
            val resultFree = result.freeVars.filterNot(_.startsWith("__cce_"))
            assert(
              resultFree.subsetOf(origFree),
              s"New free vars ${resultFree -- origFree} in ${term.show} => ${result.show}"
            )
        }
    }

    // ========================================================================
    // Integration with V3Optimizer
    // ========================================================================

    test("V3Optimizer with CCE preserves semantics") {
        // Same common context pattern as the semantic test, through the full optimizer
        val chain1 = AddInteger $ (MultiplyInteger $ (3: Term) $ (2: Term)) $ (1: Term)
        val chain2 = AddInteger $ (MultiplyInteger $ (5: Term) $ (2: Term)) $ (1: Term)
        val term = AddInteger $ chain1 $ chain2

        val optimizer = new V3Optimizer(cceEnabled = true)
        val result = optimizer(term)
        val origEval = term.evaluate
        val optEval = result.evaluate
        assert(
          origEval α_== optEval,
          s"Semantics differ:\n  original=${origEval.show}\n  optimized=${optEval.show}"
        )
    }

    // ========================================================================
    // Logging
    // ========================================================================

    test("CCE produces log entries when extracting") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain1 = headList $ (unConstr $ vr"x")
        val chain2 = headList $ (unConstr $ vr"y")
        val term = Constr(Word64.Zero, List(chain1, chain2))

        val cce = new CommonContextExtraction()
        cce(term)
        assert(cce.logs.nonEmpty, "Expected log entries for CCE extraction")
        assert(cce.logs.exists(_.contains("CCE:")))
    }

    test("CCE produces no log entries when nothing to extract") {
        val cce = new CommonContextExtraction()
        cce(AddInteger $ vr"x" $ vr"y")
        assert(cce.logs.isEmpty, s"Expected no log entries, got: ${cce.logs}")
    }

    // ========================================================================
    // Three or more occurrences with distinct leaves
    // ========================================================================

    test("should extract context with three distinct leaves") {
        val headList = Force(Builtin(HeadList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain1 = headList $ (unConstr $ vr"x")
        val chain2 = headList $ (unConstr $ vr"y")
        val chain3 = headList $ (unConstr $ vr"z")
        val term = Constr(Word64.Zero, List(chain1, chain2, chain3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected extraction, got: ${result.show}")
        assert(cce.logs.exists(_.contains("3 occ")))
    }

    // ========================================================================
    // Edge cases
    // ========================================================================

    test("CCE on simple term returns same structure") {
        val term = AddInteger $ vr"x" $ vr"y"
        val result = CommonContextExtraction(term)
        assert(result ~=~ term)
    }

    test("CCE on single variable returns same") {
        val term = vr"x"
        val result = CommonContextExtraction(term)
        assert(result ~=~ term)
    }

    test("CCE on constant returns same") {
        val term: Term = 42
        val result = CommonContextExtraction(term)
        assert(result ~=~ term)
    }

    test("CCE on Error returns same") {
        val term = Error()
        val result = CommonContextExtraction(term)
        assert(result ~=~ term)
    }

    // ========================================================================
    // Property-based tests
    // ========================================================================

    test("property: CCE is idempotent on arbitrary terms") {
        forAll { (term: Term) =>
            val once = CommonContextExtraction(term)
            val twice = CommonContextExtraction(once)
            assert(
              once ~=~ twice,
              s"Not idempotent:\n  once=${once.show}\n  twice=${twice.show}"
            )
        }
    }

    test("property: CCE never introduces new free variables (except __cce_N)") {
        forAll { (term: Term) =>
            val result = CommonContextExtraction(term)
            val origFree = term.freeVars
            val resultFree = result.freeVars.filterNot(_.startsWith("__cce_"))
            assert(
              resultFree.subsetOf(origFree),
              s"New free vars ${resultFree -- origFree} in:\n  input=${term.show}\n  output=${result.show}"
            )
        }
    }

    // ========================================================================
    // Property: semantic preservation on closed evaluable terms
    // ========================================================================

    /** Generator for closed evaluable terms with common contexts (different leaves). */
    private val genClosedTermWithCommonContext: Gen[Term] = {
        val genInt: Gen[Term] = Gen.choose(-100, 100).map(n => Const(Constant.Integer(n)))

        def genBinOp(depth: Int): Gen[Term] =
            if depth <= 0 then genInt
            else
                for
                    a <- genBinOp(depth - 1)
                    b <- genBinOp(depth - 1)
                    op <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
                yield op $ a $ b

        // Generate term with same context applied to different leaves:
        // op2(op1(a, b), op1(c, d)) where a!=c or b!=d
        for
            a <- genInt
            b <- genInt
            c <- genInt
            op1 <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
            op2 <- Gen.oneOf(AddInteger, MultiplyInteger, SubtractInteger)
            sub1 = op1 $ a $ b
            sub2 = op1 $ c $ b // same op1 and b, different a/c — common context op1(HOLE, b)
        yield op2 $ sub1 $ sub2
    }

    test("property: CCE preserves semantics on closed evaluable terms") {
        forAll(genClosedTermWithCommonContext) { term =>
            val cceResult = CommonContextExtraction(term)
            val original = Try(term.evaluate).toOption
            val optimized = Try(cceResult.evaluate).toOption
            (original, optimized) match
                case (Some(a), Some(b)) =>
                    assert(
                      a α_== b,
                      s"Semantics differ for ${term.show}:\n  original=${a.show}\n  CCE'd=${b.show}"
                    )
                case (None, None) => succeed // both errored, ok
                case _ =>
                    fail(
                      s"Divergent behavior for ${term.show}:\n  original=$original\n  CCE'd=$optimized"
                    )
        }
    }

    // ========================================================================
    // Overlapping / nested templates
    // ========================================================================

    test("should extract largest template when multiple sizes match") {
        // Three chains with the same deep template:
        // headList(tailList(unConstrData(x)))
        // headList(tailList(unConstrData(y)))
        // headList(tailList(unConstrData(z)))
        // The "big" template headList(tailList(unConstrData(HOLE))) should win
        // over smaller ones like tailList(unConstrData(HOLE))
        val headList = Force(Builtin(HeadList))
        val tailList = Force(Builtin(TailList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val chain1 = headList $ (tailList $ (unConstr $ vr"x"))
        val chain2 = headList $ (tailList $ (unConstr $ vr"y"))
        val chain3 = headList $ (tailList $ (unConstr $ vr"z"))
        val term = Constr(Word64.Zero, List(chain1, chain2, chain3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected extraction, got: ${result.show}")
        // After extraction, there should be exactly 1 let-binding (for the big template)
        // and 3 calls to it. Verify by checking the result structure.
        assert(cce.logs.size == 1, s"Expected 1 extraction, got ${cce.logs.size}: ${cce.logs}")
    }

    // ========================================================================
    // Multiple extractions in one pass
    // ========================================================================

    test("should extract two different templates in one pass") {
        val headList = Force(Builtin(HeadList))
        val tailList = Force(Builtin(TailList))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        val sndPair = Force(Force(Builtin(SndPair)))
        // Template A: headList(tailList(unConstrData(HOLE))) used with x, y, z
        val chainA1 = headList $ (tailList $ (unConstr $ vr"x"))
        val chainA2 = headList $ (tailList $ (unConstr $ vr"y"))
        val chainA3 = headList $ (tailList $ (unConstr $ vr"z"))
        // Template B: sndPair(unConstrData(HOLE)) used with a, b, c
        val chainB1 = sndPair $ (unConstr $ vr"a")
        val chainB2 = sndPair $ (unConstr $ vr"b")
        val chainB3 = sndPair $ (unConstr $ vr"c")
        val term = Constr(
          Word64.Zero,
          List(chainA1, chainA2, chainA3, chainB1, chainB2, chainB3)
        )

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected transformation, got: ${result.show}")
        // 2 primary extractions + possibly more for shared sub-templates (e.g., unConstrData(HOLE))
        assert(
          cce.logs.count(_.contains("CCE:")) >= 2,
          s"Expected at least 2 extractions, got: ${cce.logs}"
        )
    }

    // ========================================================================
    // Generalized extraction: left-spine Apply
    // ========================================================================

    test("should extract left-spine common context: f(x, z) and f(y, z)") {
        // Apply(Apply(f, HOLE), z) — hole at left child of outer Apply
        // We need template size >= 5 for profitability. Use a deeper structure:
        // Apply(Apply(Apply(g, Apply(f, HOLE)), z1), z2)
        // = g(f(x), z1, z2) and g(f(y), z1, z2)
        val g = vr"g"
        val f = vr"f"
        val chain1 = g $ (f $ vr"x") $ vr"z1" $ vr"z2"
        val chain2 = g $ (f $ vr"y") $ vr"z1" $ vr"z2"
        val chain3 = g $ (f $ vr"w") $ vr"z1" $ vr"z2"
        val term = Constr(Word64.Zero, List(chain1, chain2, chain3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        // Should extract the common context g(f(HOLE), z1, z2)
        assert(result ~!=~ term, s"Expected extraction, got: ${result.show}")
        assert(cce.logs.exists(_.contains("CCE:")), s"Expected CCE log: ${cce.logs}")
    }

    test("should extract Case scrutinee common context") {
        // Case(HOLE, [branch0, branch1]) — hole at scrutinee position
        // Need template size >= 5: Case node + 2 branches of size >= 1 each = size >= 4
        // Use slightly bigger branches to hit MinTemplateSize
        val branch0 = λ("x")(AddInteger $ vr"x" $ (1: Term))
        val branch1 = λ("x")(MultiplyInteger $ vr"x" $ (2: Term))
        val term1 = Case(vr"a", List(branch0, branch1))
        val term2 = Case(vr"b", List(branch0, branch1))
        val term3 = Case(vr"c", List(branch0, branch1))
        val term = Constr(Word64.Zero, List(term1, term2, term3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(
          result ~!=~ term,
          s"Expected extraction of Case scrutinee pattern, got: ${result.show}"
        )
        assert(cce.logs.exists(_.contains("CCE:")), s"Expected CCE log: ${cce.logs}")
    }

    test("should extract Constr argument common context") {
        // Constr(0, [a, HOLE, c]) — hole at middle arg
        // Need template size >= 5: 1 (Constr) + 1 (a) + 1 (HOLE) + 1 (c) = 4, need bigger args
        val a: Term = AddInteger $ vr"fixed1" $ (1: Term)
        val c: Term = MultiplyInteger $ vr"fixed2" $ (2: Term)
        val term1 = Constr(Word64.Zero, List(a, vr"x", c))
        val term2 = Constr(Word64.Zero, List(a, vr"y", c))
        val term3 = Constr(Word64.Zero, List(a, vr"z", c))
        val term = Constr(Word64(1), List(term1, term2, term3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected extraction of Constr arg pattern, got: ${result.show}")
        assert(cce.logs.exists(_.contains("CCE:")), s"Expected CCE log: ${cce.logs}")
    }

    test("should extract Force-wrapped common context") {
        // Force(Apply(f, HOLE)) — inner hole in Force-wrapped application
        // Template: Force(Apply(Apply(g, Apply(f, HOLE)), z))
        val g = Force(Builtin(HeadList))
        val f = Force(Force(Builtin(UnConstrData)))
        // g(f(x)) and g(f(y)) — same as existing right-spine, but verifying Force still works
        val chain1 = g $ (f $ vr"x")
        val chain2 = g $ (f $ vr"y")
        val chain3 = g $ (f $ vr"z")
        val term = Constr(Word64.Zero, List(chain1, chain2, chain3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected extraction with Force, got: ${result.show}")
    }

    // ========================================================================
    // Force wrapping
    // ========================================================================

    test("CCE handles Force terms in template correctly") {
        // The templates already use Force (Force(Builtin(HeadList)) etc.)
        // This test verifies multi-Force terms work
        val sndPair = Force(Force(Builtin(SndPair)))
        val unConstr = Force(Force(Builtin(UnConstrData)))
        // sndPair(unConstrData(x)) and sndPair(unConstrData(y))
        val chain1 = sndPair $ (unConstr $ vr"x")
        val chain2 = sndPair $ (unConstr $ vr"y")
        val chain3 = sndPair $ (unConstr $ vr"z")
        val term = Constr(Word64.Zero, List(chain1, chain2, chain3))

        val cce = new CommonContextExtraction()
        val result = cce(term)
        assert(result ~!=~ term, s"Expected extraction, got: ${result.show}")
    }

    // ========================================================================
    // Script size measurement
    // ========================================================================

    test("CCE on compiled field-accessor lambda preserves semantics") {
        import scalus.compiler.Options
        import scalus.uplc.PlutusV3
        import scalus.uplc.builtin.Data
        // Compile a lambda that accesses fields from two Data arguments at runtime.
        // The accessor chains (unConstrData, sndPair, headList, tailList) are
        // applied to different args, making them CCE candidates.
        val opts = Options(
          generateErrorTraces = false,
          optimizeUplc = true,
          cseIterations = 0,
          cceEnabled = false // get pre-CCE UPLC
        )
        given Options = opts

        val compiled = PlutusV3.compile { (a: Data, b: Data) =>
            val fa = a.toConstr.snd.head.toBigInt
            val fb = b.toConstr.snd.head.toBigInt
            fa + fb
        }
        val noCce = compiled.program.term

        val cce = new CommonContextExtraction()
        val withCce = cce(noCce)

        // CCE should have found common accessor chains on different leaves (a vs b)
        assert(cce.logs.nonEmpty, s"Expected CCE extractions, got none. Term: ${noCce.show}")

        // Build a test Data argument: Constr(0, [42])
        import scalus.cardano.onchain.plutus.prelude.List as PList
        val testData = Data.Constr(0, PList(Data.I(42)))
        val testArg = Const(Constant.Data(testData))
        val noCceApplied = noCce $ testArg $ testArg
        val withCceApplied = withCce $ testArg $ testArg

        val noCceResult = Try(noCceApplied.evaluate)
        val withCceResult = Try(withCceApplied.evaluate)
        assert(noCceResult.isSuccess, s"Without CCE should succeed: $noCceResult")
        assert(withCceResult.isSuccess, s"With CCE should succeed: $withCceResult")
        assert(
          noCceResult.get == withCceResult.get,
          s"Results should match: ${noCceResult.get} != ${withCceResult.get}"
        )
    }
}
