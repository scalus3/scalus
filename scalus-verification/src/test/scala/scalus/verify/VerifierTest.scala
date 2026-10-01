package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import Props.*

class VerifierTest extends AnyFunSuite {

    private val increment = FunctionDef.synthetic[BigInt, BigInt]("increment")
    private final case class TestArtifact(kind: ProofKind, content: String) extends ProofArtifact

    private def tactic(run: Goal => VerificationResult): Tactic = new Tactic {
        val name = "stub"
        def discharge(goal: Goal): VerificationResult = run(goal)
    }

    test("a proved statement becomes a lemma and carries its dependencies") {
        val verifier = Verifier.empty
        verifier.addFunctions(FunctionTable(increment))
        val first = verifier.statement(
          "increment_positive",
          callRef(increment.ref, BigInt(1))(result => result > 0)
        )
        val firstResult = verifier.prove(
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
        val firstProof = firstResult match
            case VerificationResult.Proven(proof) => proof
            case other                            => fail(s"expected a proof, got $other")
        val lemma = verifier.theorems.head
        assert(lemma.proof eq firstProof)

        val second = verifier.statement(Prop(BigInt(2) > BigInt(1)))
        assert(second.name == "statement_1")
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
        val unresolved = tactic { goal =>
            assert(goal.functions.definitions.isEmpty)
            VerificationResult.Inconclusive("function is not registered")
        }
        assert(
          verifier.prove(callStatement, unresolved) ==
              VerificationResult.Inconclusive("function is not registered")
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
            verifier.prove(statement, tactic(_ => VerificationResult.Inconclusive("timeout")))
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

    test("generated statement names skip names already declared explicitly") {
        val verifier = Verifier.empty
        verifier.statement("statement_1", Prop(BigInt(1) > BigInt(0)))
        assert(verifier.statement(Prop(BigInt(2) > BigInt(0))).name == "statement_2")
        assert(verifier.statement(Prop(BigInt(3) > BigInt(0))).name == "statement_3")
    }

    test("a contract is registered with its function") {
        val double = FunctionDef.named("double", (x: BigInt) => x * BigInt(2))
        val verifier = Verifier.empty
        val doubled = verifier.contract(
          "double_even",
          contract(double)(expects = x => true, ensures = x => r => r % BigInt(2) == BigInt(0))
        )
        assert(doubled.origin == Origin.Contract(double.ref, total = false))
        assert(verifier.contracts(double.ref) == List(doubled))
        assert(verifier.contracts(increment.ref).isEmpty)
        val total = verifier.contract(
          "double_total",
          totalContract(double)(expects = x => true, ensures = x => r => r == x + x)
        )
        assert(total.origin == Origin.Contract(double.ref, total = true))
        assert(verifier.contracts(double.ref) == List(doubled, total))
        assert(verifier.statement("plain", Prop(BigInt(1) > BigInt(0))).origin == Origin.Explicit)
    }
}
