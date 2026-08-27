package scalus.uplc
package transform

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.{OutOfExBudgetError, PlutusVM, RestrictingBudgetSpender}
import scalus.uplc.transform.TermAnalysis.{freeVars, isPure}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/** Differential testing of [[EtaReduce]] against the CEK machine.
  *
  * Random closed UPLC terms — biased towards eta-redex shapes, the self-application fixpoint
  * encoding, let-bound lambdas, and the case-constr application encoding with near-miss mutations —
  * are reduced with [[EtaReduce]] and both the original and the reduced term are evaluated on the
  * CEK machine under a budget cap. The two evaluations must agree.
  *
  * ==What "agree" means==
  *
  * Eta-reduction intentionally changes the shape of function results: `λx. f x` evaluates to the
  * wrapper closure while `f` evaluates to the wrapped function's value. Both are lambdas with
  * identical applied behavior, so results are compared with [[compatibleResults]]: ground results
  * (constants, constructors of ground values) must be structurally equal; function-like results
  * must have the same head constructor.
  *
  * ==Classification of disagreements==
  *
  * The purity-only rule this pass has always had carries documented quirks on degenerate input
  * (e.g. `λx. (delay t) x → delay t` is observable under `Force`; `λx. c x → c` changes the
  * result's shape — both endorsed by existing [[EtaReduceTest]] cases; such wrappers error when
  * applied, so real programs never rely on them). To keep this test focused on the value-arity
  * extension, a disagreement is only a failure if the ''legacy'' purity-only algorithm
  * ([[legacyEtaReduce]], the pre-value-arity implementation kept here as a differential oracle)
  * does NOT produce the same disagreement. Pre-existing quirks are counted and reported instead.
  *
  * Budget is observed, not asserted: eta-reduction moves the evaluation of the wrapped term from
  * each application site to the definition site, so a never-applied wrapper can get cheaper or
  * costlier; the interesting signal is correctness.
  */
class EtaReduceCekConformanceTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    private val budgetLimit = ExUnits(memory = 1_000_000L, steps = 500_000_000L)

    private enum Outcome:
        case Ok(result: Term)
        case Failed
        case OutOfBudget

    /** deBruijn round-trip normalizes binder names so results compare structurally */
    private def normalized(t: Term): Term = DeBruijn.fromDeBruijnTerm(DeBruijn.deBruijnTerm(t))

    private def run(t: Term): (Outcome, ExUnits) =
        val spender = new RestrictingBudgetSpender(budgetLimit)
        try
            val r = vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(t), spender)
            (Outcome.Ok(normalized(r)), spender.getSpentBudget)
        catch
            case _: OutOfExBudgetError          => (Outcome.OutOfBudget, spender.getSpentBudget)
            case scala.util.control.NonFatal(_) => (Outcome.Failed, spender.getSpentBudget)

    /** Term with no residual computation and no function values inside: must compare equal. */
    private def isGround(t: Term): Boolean = t match
        case Const(_, _)        => true
        case Constr(_, args, _) => args.forall(isGround)
        case _                  => false

    /** Result agreement modulo eta: ground results must be equal; function-like results (the
      * wrapper closure vs the wrapped function's value) must at least have the same head.
      */
    private def compatibleResults(a: Term, b: Term): Boolean = (a, b) match
        // Compare constructor results field by field, so ground payloads inside them stay exact.
        case (Constr(ta, as, _), Constr(tb, bs, _)) =>
            ta == tb && as.sizeCompare(bs) == 0 &&
            as.lazyZip(bs).forall(compatibleResults)
        case _ if isGround(a) || isGround(b) => a ~=~ b
        // Eta-reduction replaces the wrapper closure `λx. F x` with F's own value, so a closure
        // on one side may legitimately face the FUNCTION it wrapped on the other: a builtin or a
        // partially applied builtin spine. That is eta-equivalence -- the only thing a context
        // can do with either is apply it, and both behave identically under application. This is
        // long-standing accepted behavior, pinned by the existing
        // "(lam x [(builtin unBData) x]) reduces to (builtin unBData)" unit test.
        //
        // A closure facing a NON-function (a delay, or anything ground) is NOT eta-equivalence
        // and stays a failure: `λx. e x` is only equal to `e` when `e` is a function.
        case (
              LamAbs(_, _, _) | Apply(_, _, _) | Force(_, _) | Builtin(_, _),
              LamAbs(_, _, _) | Apply(_, _, _) | Force(_, _) | Builtin(_, _)
            ) =>
            true
        case (Delay(_, _), Delay(_, _)) => true
        case _                          => false

    private def agree(o1: Outcome, o2: Outcome): Boolean = (o1, o2) match
        case (Outcome.Ok(r1), Outcome.Ok(r2))           => compatibleResults(r1, r2)
        case (Outcome.Failed, Outcome.Failed)           => true
        case (Outcome.OutOfBudget, Outcome.OutOfBudget) => true
        case _                                          => false

    /** The pre-value-arity eta-reduction algorithm (syntactic purity only), kept verbatim as the
      * differential oracle for classifying disagreements as pre-existing vs newly introduced.
      */
    private def legacyEtaReduce(term: Term): Term = term match
        case LamAbs(name1, Apply(f, Var(name2, _), _), _)
            if name1 == name2.name && !f.freeVars.contains(name1) && f.isPure =>
            legacyEtaReduce(f)
        case LamAbs(name, body, ann) =>
            val body1 = legacyEtaReduce(body)
            if body ~!=~ body1 then legacyEtaReduce(LamAbs(name, body1, ann)) else term
        case Apply(f, arg, ann) => Apply(legacyEtaReduce(f), legacyEtaReduce(arg), ann)
        case Force(t, ann)      => Force(legacyEtaReduce(t), ann)
        case Delay(t, ann)      => Delay(legacyEtaReduce(t), ann)
        // Traverses Constr/Case like the pass under test. The historical implementation did not,
        // but an oracle that cannot reach a subterm cannot testify about it: without this, every
        // pre-existing purity-rule quirk occurring under a Constr or Case would be misreported as
        // newly introduced. What is being isolated here is the effect of the value-arity RULE, not
        // of where each implementation happens to look.
        case Constr(tag, args, ann) => Constr(tag, args.map(legacyEtaReduce), ann)
        case Case(scrut, cases, ann) =>
            Case(legacyEtaReduce(scrut), cases.map(legacyEtaReduce), ann)
        case _ => term

    /** Rewrites every single-branch tag-0 `case (constr 0 args) [f]` into the equivalent apply
      * chain `[f args…]` (the CEK's own reduction rule for it, CIP-85), so the legacy purity-only
      * oracle — which predates the case-constr encoding — can be consulted about the plain-apply
      * equivalent of a case-constr term.
      */
    private def caseConstrToApply(term: Term): Term = term match
        case Case(Constr(tag, args, _), f :: Nil, _) if tag == Word64.Zero =>
            args.map(caseConstrToApply).foldLeft(caseConstrToApply(f))(Apply(_, _))
        case Case(scrut, cases, ann) =>
            Case(caseConstrToApply(scrut), cases.map(caseConstrToApply), ann)
        case Constr(tag, args, ann) => Constr(tag, args.map(caseConstrToApply), ann)
        case Apply(f, arg, ann)     => Apply(caseConstrToApply(f), caseConstrToApply(arg), ann)
        case LamAbs(n, body, ann)   => LamAbs(n, caseConstrToApply(body), ann)
        case Force(t, ann)          => Force(caseConstrToApply(t), ann)
        case Delay(t, ann)          => Delay(caseConstrToApply(t), ann)
        case _                      => term

    // ------------------------------------------------------------------
    // Generators
    // ------------------------------------------------------------------

    private val binderPool = List("a", "b", "c", "f", "s", "x")

    private def genName: Gen[String] = Gen.oneOf(binderPool)

    private def genConst: Gen[Term] = Gen.oneOf(
      Gen.chooseNum(-3L, 12L).map(i => Const(Constant.Integer(BigInt(i)))),
      Gen.oneOf(true, false).map(b => Const(Constant.Bool(b))),
      Gen.const(Const(Constant.Unit))
    )

    /** A lambda chain of the given arity with a ground body. */
    private def genLambdaOfArity(n: Int): Gen[Term] =
        for
            body <- genConst
            names <- Gen.listOfN(n, genName)
        yield names.foldRight(body)((nm, acc) => LamAbs(nm, acc))

    /** The self-application fixpoint encoding the compiler emits for a recursive function of the
      * given arity: `[(lam s [s s]) (lam s (lam p1 .. (lam pn body)))]`.
      */
    private def genFixpoint(arity: Int): Gen[Term] =
        for inner <- genLambdaOfArity(arity)
        yield λ("s")(vr"s" $ vr"s") $ λ("s")(inner)

    /** Candidate `f` for redex position: lambdas of assorted arity, fixpoints, and the degenerate
      * pure/impure shapes the analysis must reject or that exercise the legacy rule's quirks.
      */
    private def genRedexTarget(env: List[String]): Gen[Term] =
        val base = List(
          genLambdaOfArity(1),
          genLambdaOfArity(2),
          genLambdaOfArity(3),
          genFixpoint(1),
          genFixpoint(2),
          genFixpoint(3),
          genConst,
          Gen.const(Error()),
          Gen.const(Delay(Error()): Term),
          genConst.map(c => Delay(c): Term),
          Gen.const(Builtin(AddInteger): Term),
          genConst.map(c => AddInteger $ c),
          Gen.const(AddInteger $ 1 $ 2: Term),
          Gen.const(DivideInteger $ 1 $ 0: Term),
          Gen.const(Force(Builtin(HeadList)): Term),
          // let-bound lambda: [(lam x (lam p .. body)) rhs]
          genLambdaOfArity(2).map(l => λ("x")(l) $ Const(Constant.Integer(1)))
        )
        val withVars = if env.nonEmpty then Gen.oneOf(env).map(n => vr(n)) :: base else base
        Gen.oneOf(withVars(0), withVars(1), withVars.drop(2)*)

    /** A plain eta-wrapper `(lam b1 .. (lam bn [f b1 .. bn]))` over a generated target, wrapped in
      * a let so the target's arity is visible to the pass: `[(lam f wrapper) target]`.
      */
    private def genPlainWrapperLet(env: List[String]): Gen[Term] =
        for
            n <- Gen.chooseNum(1, 3)
            target <- genRedexTarget(env)
            binders = List("wa", "wb", "wc").take(n)
            inner = binders.foldLeft(vr"wf": Term)((acc, b) => acc $ Term.Var(NamedDeBruijn(b)))
            wrapper = binders.foldRight(inner)((b, acc) => LamAbs(b, acc))
        yield λ("wf")(wrapper) $ target

    /** A case-constr eta-wrapper over a generated target, optionally mutated into one of the
      * near-miss shapes that must NOT reduce.
      */
    private def genCaseConstrWrapperLet(env: List[String]): Gen[Term] =
        for
            n <- Gen.chooseNum(1, 3)
            target <- genRedexTarget(env)
            mutation <- Gen.oneOf(
              "correct",
              "wrongTag",
              "permuted",
              "dupField",
              "extraBranch",
              "constField",
              "capture",
              "dupBinder"
            )
            result <- {
                val binders0 = List("wa", "wb", "wc").take(n)
                val binders =
                    if mutation == "dupBinder" && n >= 2 then
                        binders0.head :: (binders0.tail.init :+ binders0.head)
                    else binders0
                val fields0: List[Term] = binders.map(b => Term.Var(NamedDeBruijn(b)))
                val fields = mutation match
                    case "permuted" if n >= 2 => fields0.reverse
                    case "dupField" if n >= 2 => fields0.head :: fields0.head :: fields0.drop(2)
                    case "constField"         => fields0.init :+ Const(Constant.Integer(7))
                    case _                    => fields0
                val tag = if mutation == "wrongTag" then Word64(1) else Word64.Zero
                val branch: Term =
                    if mutation == "capture" then vr"wf" $ Term.Var(NamedDeBruijn(binders.head))
                    else vr"wf"
                val branches =
                    if mutation == "extraBranch" then List(branch, Const(Constant.Integer(0)))
                    else List(branch)
                val wrapper = binders.foldRight(Case(Constr(tag, fields), branches): Term)(
                  (b, acc) => LamAbs(b, acc)
                )
                Gen.const(λ("wf")(wrapper) $ target)
            }
        yield result

    private def genTerm(depth: Int, env: List[String]): Gen[Term] =
        val leaves: List[(Int, Gen[Term])] = List(
          (4, genConst),
          (1, Gen.const(Error(): Term)),
          (1, Gen.const(Builtin(AddInteger): Term)),
          (1, Gen.const(Force(Builtin(HeadList)): Term)),
          (2, genLambdaOfArity(1))
        ) ++ (if env.nonEmpty then List((4, Gen.oneOf(env).map(n => vr(n)))) else Nil)
        if depth <= 0 then Gen.frequency(leaves*)
        else
            val recursive: List[(Int, Gen[Term])] = List(
              (
                3,
                for
                    n <- genName
                    body <- genTerm(depth - 1, n :: env)
                yield LamAbs(n, body)
              ),
              (
                3,
                for
                    f <- genTerm(depth - 1, env)
                    a <- genTerm(depth - 1, env)
                yield Apply(f, a)
              ),
              (1, genTerm(depth - 1, env).map(Force(_))),
              (2, genTerm(depth - 1, env).map(Delay(_))),
              (1, genTerm(depth - 1, env).map(t => Force(Delay(t)))),
              // let-binding
              (
                3,
                for
                    n <- genName
                    rhs <- genTerm(depth - 1, env)
                    body <- genTerm(depth - 1, n :: env)
                yield LamAbs(n, body) $ rhs
              ),
              (
                1,
                for
                    a <- genTerm(depth - 1, env)
                    b <- genTerm(depth - 1, env)
                yield AddInteger $ a $ b
              ),
              (
                1,
                for
                    tag <- Gen.chooseNum(0, 2)
                    args <- Gen.listOfN(2, genTerm(depth - 1, env))
                yield Constr(Word64(tag), args)
              ),
              (
                1,
                for
                    scrut <- genTerm(depth - 1, env)
                    branches <- Gen.listOfN(2, genTerm(depth - 1, env))
                yield Case(scrut, branches)
              ),
              // targeted eta shapes
              (5, genPlainWrapperLet(env)),
              (5, genCaseConstrWrapperLet(env)),
              // targeted shapes applied to arguments
              (
                3,
                for
                    wrapped <- Gen.oneOf(genPlainWrapperLet(env), genCaseConstrWrapperLet(env))
                    k <- Gen.chooseNum(0, 3)
                    args <- Gen.listOfN(k, genConst)
                yield args.foldLeft(wrapped)(Apply(_, _))
              )
            )
            Gen.frequency(leaves ++ recursive*)

    private val genClosedTerm: Gen[Term] = Gen.sized { size =>
        genTerm(depth = 2 + (size % 3), env = Nil)
    }

    // ------------------------------------------------------------------
    // The property
    // ------------------------------------------------------------------

    private val preExisting = ListBuffer.empty[String]
    private val budgetIncreases = ListBuffer.empty[String]
    private var reducedCount = 0
    private var checkedCount = 0

    private def checkAgreement(t: Term): Unit =
        checkedCount += 1
        val reduced = EtaReduce(t)
        if reduced ~!=~ t then
            reducedCount += 1
            val (o1, b1) = run(t)
            val (o2, b2) = run(reduced)
            if !agree(o1, o2) then
                // Classify: does the legacy purity-only rule produce the same disagreement,
                // either on the term itself or on its plain-apply equivalent (for the
                // case-constr encoding, which the legacy rule predates)?
                val legacy = legacyEtaReduce(t)
                val (oLegacy, _) = run(legacy)
                val plain = caseConstrToApply(t)
                val legacyPlain = legacyEtaReduce(plain)
                val (oPlain, _) = run(plain)
                val (oLegacyPlain, _) = run(legacyPlain)
                val reproducedDirectly = legacy ~!=~ t && !agree(o1, oLegacy)
                val reproducedOnPlainForm =
                    legacyPlain ~!=~ plain && !agree(oPlain, oLegacyPlain)
                if reproducedDirectly || reproducedOnPlainForm then
                    preExisting += s"pre-existing purity-rule quirk: ${t.show} -> ${reduced.show} ($o1 vs $o2)"
                else
                    fail(
                      s"""EtaReduce changed observable behavior (NOT reproduced by the legacy purity-only rule):
                         |original: ${t.show}
                         |reduced:  ${reduced.show}
                         |legacy:   ${legacy.show}
                         |original outcome: $o1
                         |reduced outcome:  $o2""".stripMargin
                    )
            else
                (o1, o2) match
                    case (Outcome.Ok(_), Outcome.Ok(_)) if b2.steps > b1.steps =>
                        budgetIncreases +=
                            s"${t.showShort}: steps ${b1.steps} -> ${b2.steps}, mem ${b1.memory} -> ${b2.memory}"
                    case _ => ()

    test("random terms: EtaReduce preserves CEK behavior (success/failure and result)") {
        forAll(genClosedTerm, minSuccessful(3000)) { t =>
            checkAgreement(t)
        }
        info(s"checked $checkedCount terms, $reducedCount were reduced")
        info(s"pre-existing purity-rule quirks observed: ${preExisting.size}")
        preExisting.take(3).foreach(s => info(s"  e.g. $s"))
        info(s"budget increases observed: ${budgetIncreases.size}")
        budgetIncreases.take(3).foreach(s => info(s"  e.g. $s"))
    }

    // ------------------------------------------------------------------
    // Deterministic corpus: the compiler's actual recursion shapes
    // ------------------------------------------------------------------

    /** `gcd` via the self-application fixpoint encoding, arity 2 — the `ecd` shape. */
    private val gcdFix: Term =
        val body =
            Force(
              Force(Builtin(IfThenElse)) $ (EqualsInteger $ vr"b" $ 0) $
                  Delay(vr"a") $
                  Delay((vr"s" $ vr"s") $ vr"b" $ (RemainderInteger $ vr"a" $ vr"b"))
            )
        λ("s")(vr"s" $ vr"s") $ λ("s")(λ("a", "b")(body))

    /** The compiler-emitted entry-point wrapper around it. */
    private val gcdWrapped: Term = λ("f")(λ("a", "b")(vr"f" $ vr"a" $ vr"b")) $ gcdFix

    private def int(i: Long): Term = Const(Constant.Integer(BigInt(i)))

    test("gcd fixpoint: wrapper is removed and behavior is preserved on all inputs") {
        val reduced = EtaReduce(gcdWrapped)
        assert(reduced == (λ("f")(vr"f") $ gcdFix))
        for (a, b) <- List((0L, 12L), (12L, 0L), (12L, 8L), (1071L, 462L), (7L, 7L)) do
            val (o1, b1) = run(gcdWrapped $ int(a) $ int(b))
            val (o2, b2) = run(reduced $ int(a) $ int(b))
            assert(agree(o1, o2), s"gcd($a, $b): $o1 vs $o2")
            (o1, o2) match
                case (Outcome.Ok(r1), Outcome.Ok(r2)) =>
                    assert(r1 ~=~ r2, s"gcd($a, $b): ${r1.show} vs ${r2.show}")
                    assert(
                      b2.steps <= b1.steps && b2.memory <= b1.memory,
                      s"gcd($a, $b): reduced must not cost more: $b1 vs $b2"
                    )
                case _ => fail(s"gcd($a, $b) did not evaluate: $o1 / $o2")
    }

    test("gcd fixpoint, case-constr encoded wrapper: removed, behavior preserved") {
        // (lam f (lam a (lam b (case (constr 0 a b) f)))) applied to the fixpoint
        val wrapper = λ("f")(
          λ("a", "b")(Case(Constr(Word64.Zero, List(vr"a", vr"b")), List(vr"f")))
        ) $ gcdFix
        val reduced = EtaReduce(wrapper)
        assert(reduced == (λ("f")(vr"f") $ gcdFix))
        for (a, b) <- List((12L, 8L), (1071L, 462L)) do
            val (o1, _) = run(wrapper $ int(a) $ int(b))
            val (o2, _) = run(reduced $ int(a) $ int(b))
            (o1, o2) match
                case (Outcome.Ok(r1), Outcome.Ok(r2)) => assert(r1 ~=~ r2)
                case _ => fail(s"gcd($a, $b) did not evaluate: $o1 / $o2")
    }

    test("unapplied wrapper: reduced program result behaves identically when applied later") {
        // The whole program is the let+wrapper; the reduced result must be a lambda with the
        // same applied behavior.
        val (o1, _) = run(gcdWrapped)
        val (o2, _) = run(EtaReduce(gcdWrapped))
        (o1, o2) match
            case (Outcome.Ok(r1), Outcome.Ok(r2)) =>
                assert(compatibleResults(r1, r2))
                for (a, b) <- List((12L, 8L), (0L, 5L)) do
                    val (ra, _) = run(r1 $ int(a) $ int(b))
                    val (rb, _) = run(r2 $ int(a) $ int(b))
                    assert(agree(ra, rb), s"applied results diverge for ($a, $b): $ra vs $rb")
            case _ => fail(s"wrapper program did not evaluate: $o1 / $o2")
    }
