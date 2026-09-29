package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRBuiltins, SIRType}
import scalus.uplc.{Constant, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.ByteString
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.verify.*
import scalus.verify.Props.*

import java.io.File
import java.nio.file.{Files, Path}

class UplcBlasterTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val leanDirectory = Path.of("scalus-verification", "src", "main", "lean")

    /** Whether Lean can run here: `lake` on the `PATH` and a built workspace. The ci-jvm shell has
      * neither, so the tests that run Lean are canceled there. Build the workspace with
      * `lake build` in the `lean` dev shell (see the module README) to run them.
      */
    private lazy val leanAvailable: Boolean =
        sys.env
            .getOrElse("PATH", "")
            .split(File.pathSeparator)
            .exists(directory => Files.isExecutable(Path.of(directory, "lake"))) &&
            Files.isRegularFile(
              leanDirectory.resolve(".lake/build/lib/lean/ScalusProofs/Prelude.olean")
            )

    /** Declares `prop` in a fresh verifier and runs [[UplcBlaster]] on it through Lean. */
    private def run(
        prop: Prop,
        budget: Int,
        functions: Seq[FunctionDef[?, ?]] = Nil
    ): (Verifier, Statement, VerificationResult) = {
        assume(leanAvailable, s"requires lake and a built Lean workspace in $leanDirectory")
        val verifier = Verifier.empty
        functions.foreach(verifier.addFunction)
        val statement = verifier.statement(prop)
        (verifier, statement, verifier.verify(statement, UplcBlaster(budget, leanDirectory)))
    }

    private def proven(prop: Prop, budget: Int, functions: FunctionDef[?, ?]*): Unit =
        run(prop, budget, functions) match
            case (verifier, statement, VerificationResult.Proven(proof)) =>
                assert(proof.artifact.kind == ProofKind.Blaster)
                assert(proof.artifact.asInstanceOf[UplcBlaster.Artifact].output.contains("✅ Valid"))
                assert(verifier.theorems.exists(_.statement eq statement))
            case (_, _, other) => fail(s"expected a proof, got $other")

    /** The replayed counterexample of a refuted statement. */
    private def refuted(prop: Prop, budget: Int): Map[String, Constant] =
        run(prop, budget) match
            case (verifier, _, VerificationResult.Refuted(proof)) =>
                assert(verifier.theorems.isEmpty)
                proof.artifact.asInstanceOf[UplcBlaster.Artifact].counterexample.toMap
            case (_, _, other) => fail(s"expected a refutation, got $other")

    private def integer(value: Constant): BigInt = value match
        case Constant.Integer(integer) => integer
        case other                     => fail(s"expected an integer, got $other")

    private def lowered(prop: Prop, functions: FunctionTable): UplcBlaster.Lowered =
        UplcBlaster.lower(prop, functions) match
            case Right(lowered) => lowered
            case Left(reason)   => fail(reason)

    /** The two binders of a two-value `forAll` and its body. */
    private def split2(prop: Prop): (PropExpr.Ident[?], PropExpr.Ident[?], Prop) = prop match
        case Prop.Forall(x, Prop.Forall(y, body)) => (x, y, body)
        case other => fail(s"expected two universal quantifiers, got $other")

    /** The Boolean body of a two-value `forAll`, over the variables `x` and `y` instead of its own.
      * The plugin gives every lambda parameter a unique SIR name.
      */
    private def bodyOver(x: PropExpr.Ident[?], y: PropExpr.Ident[?], prop: Prop): Prop =
        prop match
            case Prop.Forall(a, Prop.Forall(b, Prop.Bool(PropExpr.SIRExpr(sir)))) =>
                val renamed = SIR.renameFreeVars(sir, Map(a.name -> x.name, b.name -> y.name))
                Prop.Bool(PropExpr.SIRExpr(renamed))
            case other => fail(s"expected a two-value Boolean forAll, got $other")

    test("proves an addition identity through compiled UPLC and Lean Blaster") {
        proven(forAll[BigInt](x => x + BigInt(0) == x), budget = 40)
    }

    test("proves that addition is commutative and associative") {
        proven(forAll[BigInt, BigInt]((x, y) => x + y == y + x), budget = 40)
        proven(
          forAll[BigInt, BigInt, BigInt]((x, y, z) => (x + y) + z == x + (y + z)),
          budget = 60
        )
    }

    test("proves that min is a lower bound and min + max is the sum") {
        proven(
          forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x && Math.min(x, y) <= y),
          budget = 100
        )
        proven(
          forAll[BigInt, BigInt]((x, y) => Math.min(x, y) + Math.max(x, y) == x + y),
          budget = 100
        )
    }

    test("quantifies over Boolean values") {
        proven(
          forAll[Boolean, BigInt]((flag, x) =>
              if flag then Math.max(x, BigInt(0)) >= BigInt(0)
              else Math.min(x, BigInt(0)) <= BigInt(0)
          ),
          budget = 60
        )
    }

    test("a false property of min is refuted with a replayed counterexample") {
        val prop = forAll[BigInt, BigInt]((x, y) => Math.min(x, y) == x)
        val (x, y, _) = split2(prop)
        val counterexample = refuted(prop, budget = 60)
        assert(counterexample.keySet == Set(x.name, y.name))
        assert(integer(counterexample(x.name)) > integer(counterexample(y.name)), counterexample)
    }

    test("proves connectives over separately compiled tests, with each test read by polarity") {
        val (x, y, premise) = split2(forAll[BigInt, BigInt]((x, y) => x <= y))
        val isX = bodyOver(x, y, forAll[BigInt, BigInt]((x, y) => Math.min(x, y) == x))
        val isY = bodyOver(x, y, forAll[BigInt, BigInt]((x, y) => Math.min(x, y) == y))
        def both(body: Prop): Prop = Prop.Forall(x, Prop.Forall(y, body))

        val implication = both(premise ==> isX)
        assert(lowered(implication, FunctionTable.empty).leaves.size == 2)
        proven(implication, budget = 60)
        proven(both(premise <=> isX), budget = 60)
        proven(both(!premise ==> isY), budget = 60)
        proven(both(premise || isY), budget = 60)

        val counterexample = refuted(both(premise ==> isY), budget = 60)
        assert(integer(counterexample(x.name)) < integer(counterexample(y.name)), counterexample)
    }

    test("a falsification caused by too small a budget is reported as spurious") {
        run(forAll[BigInt, BigInt]((x, y) => Math.min(x, y) <= x), budget = 3) match
            case (_, _, VerificationResult.Inconclusive(reason)) =>
                assert(reason.contains("spurious"), reason)
            case (_, _, other) => fail(s"expected an inconclusive result, got $other")
    }

    test("closed statements, equality and denotes") {
        proven(equal(BigInt(2) + BigInt(3), BigInt(5)), budget = 40)
        proven(denotes(BigInt(7) / BigInt(2)), budget = 40)
        refuted(denotes(BigInt(7) / BigInt(0)), budget = 40)
    }

    test("requires a positive symbolic execution budget") {
        assertThrows[IllegalArgumentException](UplcBlaster(0))
    }

    test("lowers a universal prefix and a Boolean body to one n-argument UPLC predicate") {
        val annotations = AnnotationsDecl.empty
        val xName = "x"
        val yName = "y"
        val xVar = SIR.Var(xName, SIRType.Integer, annotations)
        val yVar = SIR.Var(yName, SIRType.Integer, annotations)
        val lessThanX = SIR.Apply(
          SIRBuiltins.lessThanInteger,
          xVar,
          SIRType.Fun(SIRType.Integer, SIRType.Boolean),
          annotations
        )
        val body = SIR.Apply(lessThanX, yVar, SIRType.Boolean, annotations)
        val prop = Prop.Forall(
          PropExpr.Ident[BigInt](xName, 1, SIRType.Integer),
          Prop.Forall(
            PropExpr.Ident[BigInt](yName, 2, SIRType.Integer),
            Prop.Bool(PropExpr.SIRExpr(body))
          )
        )

        val lower = lowered(prop, FunctionTable.empty)
        assert(lower.body == UplcBlaster.Formula.Test(0))
        val applied = lower.leaves.head $ BigInt(1).asTerm $ BigInt(2).asTerm
        applied.term.evaluateDebug match
            case success: Result.Success => assert(success.term == Term.Const(Constant.Bool(true)))
            case failure: Result.Failure => fail(failure.exception)
    }

    test("links a registered function into the UPLC predicate and proves it") {
        val clamp = FunctionDef(Math.clamp)
        val prop = forAll[BigInt](x => Math.clamp(x, x, x) == x)

        val applied = lowered(prop, FunctionTable(clamp)).leaves.head $ BigInt(7).asTerm
        applied.term.evaluateDebug match
            case success: Result.Success => assert(success.term == Term.Const(Constant.Bool(true)))
            case failure: Result.Failure => fail(failure.exception)

        proven(prop, 80, clamp)
    }

    test("compiles an unregistered @Compile function together with the test") {
        proven(forAll[BigInt](x => Math.clamp(x, x, x) == x), budget = 80)
    }

    test("proves a total call of a registered function") {
        val increment = FunctionDef.named("increment", (x: BigInt) => x + BigInt(1))
        proven(callRef(increment.ref, BigInt(41))(r => r == BigInt(42)), 40, increment)
    }

    test("statements outside the fragment are inconclusive") {
        val quantified = forAll[BigInt](x => x > 0) match
            case Prop.Forall(ident, body) =>
                Prop.Forall(ident, body && Prop.Exists(ident, None, body))
            case other => fail(s"expected a universal proposition, got $other")
        assert(
          UplcBlaster.lower(quantified, FunctionTable.empty).left.exists(_.contains("quantifier"))
        )

        // The tactic reports these without running Lean.
        val verifier = Verifier.empty
        val bytes = verifier.statement(forAll[ByteString](_ => true))
        verifier.verify(bytes, UplcBlaster(10, leanDirectory)) match
            case VerificationResult.Inconclusive(reason) =>
                assert(reason.contains("ByteString binder"), reason)
            case other => fail(s"expected an inconclusive result, got $other")

        val clamp = FunctionDef(Math.clamp)
        val tupled = UplcBlaster.lower(
          call(Math.clamp, (BigInt(2), BigInt(0), BigInt(10)))(r => r == BigInt(2)),
          FunctionTable(clamp)
        )
        assert(tupled.left.exists(_.contains("call argument")), tupled)
    }
}
