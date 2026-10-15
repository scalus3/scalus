package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import Props.*

class VerifierTest extends AnyFunSuite {

    private val increment = FunctionDef.synthetic[BigInt, BigInt]("increment")
    private final case class TestArtifact(kind: ProofKind, content: String) extends ProofArtifact

    private def tactic(execute: Goal => ExecutionResult): Tactic = new Tactic {
        type Prepared = Goal
        val name = "stub"
        def prepare(goal: Goal): Either[CompatibilityReport, Prepared] = Right(goal)
        def run(prepared: Prepared): ExecutionResult = execute(prepared)
    }

    test("a proved statement becomes a lemma and carries its dependencies") {
        val verifier = Verifier.empty
        verifier.addFunctions(FunctionTable(increment))
        val first = verifier.statement(
          "increment_positive",
          callRef(increment.ref, BigInt(1))(result => result > 0)
        )
        val firstRun = verifier.prepare(
          first,
          tactic { goal =>
              assert(goal.statement eq first)
              assert(goal.functions(increment.ref) eq increment)
              assert(goal.lemmas.isEmpty)
              VerificationResult.Proven(
                Proof(TestArtifact(ProofKind.LeanKernel, "proof term"))
              )
          }
        )
        assert(firstRun.exists(_.statement eq first))
        val prepared = firstRun.toOption.get
        assertThrows[IllegalArgumentException](Verifier.empty.prove(prepared))
        val firstResult: ExecutionResult = verifier.prove(prepared)
        val firstProof = firstResult match
            case VerificationResult.Proven(proof) => proof
            case other                            => fail(s"expected a proof, got $other")
        val lemma = verifier.theorems.head
        assert(lemma.proof eq firstProof)

        val second = verifier.statement(Prop(BigInt(2) > BigInt(1)))
        assert(second.name.matches("#[0-9a-f]{16}"))
        val blasterArtifact =
            TestArtifact(ProofKind.Blaster, "checked result")
        val secondResult = verifier.verify(
          second,
          tactic { goal =>
              assert(goal.lemmas == List(lemma))
              VerificationResult.Proven(Proof(blasterArtifact, List(lemma)))
          }
        )
        secondResult match
            case VerificationResult.Proven(proof) =>
                assert(proof.usedLemmas == List(lemma))
                assert(proof.artifact eq blasterArtifact)
                assert(proof.kinds == Set(ProofKind.Blaster, ProofKind.LeanKernel))
            case other => fail(s"expected a proof, got $other")
        assert(verifier.theorems.size == 2)
    }

    test("function resolution is left to the tactic and foreign statements fail first") {
        val verifier = Verifier.empty
        val callStatement = verifier.statement(
          "missing_function",
          callRef(increment.ref, BigInt(1))(result => result > 0)
        )
        val report = CompatibilityReport(
          List(
            CompatibilityIssue.UnsupportedFeature(
              List(callStatement.name),
              "function is not registered"
            )
          )
        )
        val unresolved = new Tactic {
            type Prepared = Nothing
            val name = "unsupported-stub"
            def prepare(goal: Goal): Either[CompatibilityReport, Prepared] = {
                assert(goal.functions.definitions.isEmpty)
                Left(report)
            }
            def run(prepared: Prepared): ExecutionResult = prepared
        }
        assert(verifier.prepare(callStatement, unresolved) == Left(report))
        assert(
          verifier.prove(callStatement, unresolved) ==
              VerificationResult.Unsupported(report)
        )

        val never = tactic(_ => fail("tactic should not run"))
        val foreign = Verifier.empty.statement("foreign", Prop(BigInt(1) > BigInt(0)))
        assertThrows[IllegalArgumentException](verifier.prove(foreign, never))
    }

    test("inconclusive results do not create theorems") {
        val verifier = Verifier.empty
        val statement = verifier.statement("sampled", Prop(BigInt(1) > BigInt(0)))
        val sampled =
            verifier.prove(
              statement,
              tactic(_ => VerificationResult.Inconclusive("10 samples passed"))
            )
        assert(sampled == VerificationResult.Inconclusive("10 samples passed"))
        assert(verifier.theorems.isEmpty)

        val undecided =
            verifier.prove(
              statement,
              tactic(_ => VerificationResult.Inconclusive("timeout"))
            )
        assert(undecided == VerificationResult.Inconclusive("timeout"))
        assert(verifier.theorems.isEmpty)
    }

    test("a confirmed refutation is returned without registering a theorem") {
        val verifier = Verifier.empty
        val statement = verifier.statement("false_claim", Prop(BigInt(0) > BigInt(1)))
        val proof = Proof(TestArtifact(ProofKind.Blaster, "counterexample"))
        val result = verifier.prove(statement, tactic(_ => VerificationResult.Refuted(proof)))
        assert(result == VerificationResult.Refuted(proof))
        assert(verifier.theorems.isEmpty)
    }

    test("an imported theorem is available in another verifier") {
        val source = Verifier.empty
        val statement = source.statement("proved", Prop(BigInt(1) > BigInt(0)))
        val result = source.prove(
          statement,
          tactic(_ =>
              VerificationResult.Proven(
                Proof(TestArtifact(ProofKind.LeanKernel, "term"))
              )
          )
        )
        result match
            case VerificationResult.Proven(_) => ()
            case other                        => fail(s"expected a proof, got $other")
        val theorem = source.theorems.head
        val target = Verifier.empty
        target.addTheorem(theorem)
        assert(target.theorems.toList == List(theorem))
        assertThrows[IllegalArgumentException](
          target.statement("proved", Prop(BigInt(0) > BigInt(1)))
        )
    }

    test("a statement without a name is named by its content, wherever it is stated") {
        val verifier = Verifier.empty
        val holds = verifier.statement(forAll[BigInt](x => x + BigInt(0) == x))
        assert(holds.byContent && holds.name.matches("#[0-9a-f]{16}"))
        // The same statement, written on another line, in another verifier, is the same one.
        assert(Verifier.empty.statement(forAll[BigInt](x => x + BigInt(0) == x)).name == holds.name)
        // Stated again in one verifier, it is the statement that was declared.
        assert(verifier.statement(forAll[BigInt](x => x + BigInt(0) == x)) eq holds)
        // Another statement, a variable renamed, or another test, is another name.
        val names = List(
          holds.name,
          verifier.statement(forAll[BigInt](y => y + BigInt(0) == y)).name,
          verifier.statement(forAll[BigInt](x => BigInt(0) + x == x)).name,
          verifier.statement(forAll[BigInt](x => x + BigInt(1) > x)).name,
          verifier.statement(callRef(increment.ref, BigInt(1))(result => result > 0)).name
        )
        assert(names.distinct == names)
        // The number the compiler makes a name unique with is another after another compilation:
        // `x_<offset>` of a binder, and `x-<count>` of a function's parameter in its SIR.
        def numbered(name: String): Prop = Prop.Forall(
          PropExpr.Ident[Boolean](name, 0L, SIRType.Boolean),
          Prop.Bool(
            PropExpr.SIRExpr[Boolean](SIR.Var(name, SIRType.Boolean, AnnotationsDecl.empty))
          )
        )
        def nameOf(prop: Prop): String = Verifier.empty.statement(prop).name
        assert(nameOf(numbered("flag-79636")) == nameOf(numbered("flag-81002")))
        assert(nameOf(numbered("flag_1042")) == nameOf(numbered("flag-81002")))
        assert(nameOf(numbered("flag-79636")) != nameOf(numbered("other-79636")))
        // A call binds its result, which is the same on another line too.
        assert(
          Verifier.empty.statement(callRef(increment.ref, BigInt(1))(result => result > 0)).name ==
              names.last
        )
        // A statement and a negative control of the same content are not one name.
        assertThrows[IllegalArgumentException](
          verifier.refute(forAll[BigInt](x => x + BigInt(0) == x))
        )
        val control = verifier.refute(forAll[BigInt](x => x + BigInt(1) == x))
        assert(control.expected == ExpectedOutcome.Falsified && control.byContent)
        // A name of one's own is no content's.
        assertThrows[IllegalArgumentException](verifier.statement("#1", Prop(true)))
        assertThrows[IllegalArgumentException](verifier.refute("#../..", Prop(false)))
        assert(!verifier.statement("plain", Prop(true)).byContent)
    }

    test("a statement stated again after it is proved is no lemma of its own") {
        val verifier = Verifier.empty
        val holds = verifier.statement(Prop(BigInt(1) > BigInt(0)))
        val proved =
            tactic(_ => VerificationResult.Proven(Proof(TestArtifact(ProofKind.LeanKernel, "t"))))
        assert(verifier.verify(holds, proved).isInstanceOf[VerificationResult.Proven])
        val again = verifier.statement(Prop(BigInt(1) > BigInt(0)))
        assert(again eq holds)
        verifier.verify(
          again,
          tactic { goal =>
              assert(goal.lemmas.isEmpty)
              VerificationResult.Inconclusive("not run")
          }
        )
        // A theorem imported into another verifier is what its content is stated as there.
        val target = Verifier.empty
        target.addTheorem(verifier.theorems.head)
        assert(target.statement(Prop(BigInt(1) > BigInt(0))) eq holds)
        target.verify(
          holds,
          tactic { goal =>
              assert(goal.lemmas.isEmpty)
              VerificationResult.Inconclusive("not run")
          }
        )
    }

    test("a negative control expects a refutation, and a request expects what was declared") {
        val verifier = Verifier.empty
        val claim = verifier.statement("holds", Prop(BigInt(1) > BigInt(0)))
        val control = verifier.refute("does_not_hold", Prop(BigInt(0) > BigInt(1)))
        assert(claim.expected == ExpectedOutcome.Valid)
        assert(control.expected == ExpectedOutcome.Falsified)
        val stub = tactic(_ => VerificationResult.Inconclusive("not run"))
        assert(VerificationRequest(claim, stub).expected == ExpectedOutcome.Valid)
        assert(VerificationRequest(control, stub).expected == ExpectedOutcome.Falsified)
        // A refuted control is no theorem, and its name is taken as any statement's is.
        val proof = Proof(TestArtifact(ProofKind.Blaster, "counterexample"))
        assert(
          verifier.prove(control, tactic(_ => VerificationResult.Refuted(proof))) ==
              VerificationResult.Refuted(proof)
        )
        assert(verifier.theorems.isEmpty)
        assertThrows[IllegalArgumentException](
          verifier.refute("holds", Prop(BigInt(0) > BigInt(1)))
        )
    }

    test("a contract is registered with its function") {
        val double = FunctionDef.named("double", (x: BigInt) => x * BigInt(2))
        val verifier = Verifier.empty
        val doubled = verifier.contract(
          "double_even",
          contract(double)(expects = x => true, ensures = x => r => r % BigInt(2) == BigInt(0))
        )
        doubled.origin match
            case Origin.Contract(declared) => assert(declared.function == double.ref)
            case other                     => fail(s"expected a contract's origin, got $other")
        assert(verifier.contracts(double.ref) == List(doubled))
        assert(verifier.contracts(increment.ref).isEmpty)
        val total = verifier.contract(
          "double_total",
          totalContract(double)(expects = x => true, ensures = x => r => r == x + x)
        )
        assert(verifier.contracts(double.ref) == List(doubled, total))
        assert(verifier.statement("plain", Prop(BigInt(1) > BigInt(0))).origin == Origin.Explicit)
    }
}
