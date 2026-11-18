package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{require, List as PList, Spec}
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring
import scalus.compiler.Compile
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.uplc.Constant
import scalus.verify.*
import scalus.verify.Props.*

/** Functions whose clauses no statement can be made of as [[SpecifiedExamples]]' are. */
@Compile
object UnusualSpecifications {

    inline def handle(x: BigInt): Unit = {
        Spec.ensures(x >= BigInt(5))
        require(x >= BigInt(5))
    }

    /** A handler inlined at two places: its clause is twice in the code. */
    def twice(x: BigInt): Unit = {
        handle(x)
        handle(x + BigInt(1))
    }

    /** A clause inside a function value, of which no statement is made. */
    def eachGrows(xs: PList[BigInt]): PList[BigInt] =
        xs.map(x => (x + BigInt(1)).ensuring(r => r > x))

    /** A call of `clamp` inside a function value, whose obligation no statement is made of. */
    def clampsEach(xs: PList[BigInt]): PList[BigInt] =
        xs.map(x => SpecifiedExamples.clamp(x, BigInt(0), BigInt(10)))
}

/** How the clauses of the functions of a verifier stand ([[Verifier.specifications]]): checked by a
  * proof, open, owed by no caller, or unchecked. A stub tactic proves what it is told to, and says
  * what each statement is by a key it is given, so no Lean runs.
  */
class SpecificationCoverageTest extends AnyFunSuite {

    private final case class StubArtifact(kind: ProofKind) extends ProofArtifact

    /** A tactic that proves the statements `proves` names, and says that statements of one key are
      * the same statement.
      */
    private def stub(proves: Set[String], key: String => String = name => name): Tactic =
        new Tactic {
            type Prepared = Goal
            val name = "stub"
            def prepare(goal: Goal): Either[CompatibilityReport, Prepared] = Right(goal)
            def run(prepared: Prepared, earlier: Option[Kept]): ExecutionResult =
                if proves(prepared.statement.name) then
                    VerificationResult.Proven(Proof(StubArtifact(ProofKind.Blaster)))
                else VerificationResult.Inconclusive("not told to prove it")
            override def fingerprint(prepared: Prepared): Option[Fingerprint] =
                Some(Fingerprint(Map.empty, key(prepared.statement.name), Nil))
        }

    private def statusOf(verifier: Verifier, kind: Clause.Kind): ClauseStatus =
        verifier.specifications.filter(_.clause.kind == kind) match
            case List(only) => only.status
            case other      => fail(s"expected one clause of $kind, got $other")

    private val increment = FunctionDef(SpecifiedExamples.increment)
    private val checked = FunctionDef(SpecifiedExamples.checked)
    private val clamp = FunctionDef(SpecifiedExamples.clamp)
    private val guarded = FunctionDef(SpecifiedExamples.guarded)

    test("a clause is where the code states it, in the functions whose code has it") {
        val verifier = Verifier.empty
        verifier.addFunction(increment)
        verifier.addFunction(FunctionDef(SpecifiedExamples.plain))
        val List(specification) = verifier.specifications
        assert(specification.clause.kind == Clause.Kind.Ensuring)
        assert(specification.clause.file.endsWith("SpecificationsTest.scala"))
        assert(specification.clause.line == 23)
        assert(specification.functions == List("increment"))
        assert(
          specification.describe.startsWith("ensuring at SpecificationsTest.scala:23, in increment")
        )
    }

    test("an ensures is where its condition is, not where Spec.ensures is defined") {
        // `Spec.ensures` is inline: its call is at its definition in `Spec`, and only its condition
        // is where the function states it. Two clauses of one function are two clauses.
        val verifier = Verifier.empty
        verifier.addFunction(FunctionDef(SpecifiedExamples.bounded))
        val clauses = verifier.specifications.map(_.clause)
        assert(clauses.map(_.kind) == List(Clause.Kind.Ensures, Clause.Kind.Ensuring), clauses)
        assert(clauses.forall(_.file.endsWith("SpecificationsTest.scala")), clauses)
        assert(clauses.map(_.line) == List(96, 98), clauses)
    }

    test("a clause no statement states is unchecked, and a proof of its contract checks it") {
        val verifier = Verifier.empty
        verifier.addFunction(increment)
        assert(
          statusOf(verifier, Clause.Kind.Ensuring) match
              case ClauseStatus.Unchecked(Nil, missing) => missing.contains("no statement")
              case _                                    => false
        )
        val stated = FunctionContract.inSource(increment).getOrElse(fail("increment states none"))
        val holds = verifier.statement("increment_holds", stated)
        assert(
          statusOf(verifier, Clause.Kind.Ensuring).isInstanceOf[ClauseStatus.Unchecked]
        )
        assert(
          verifier
              .prove(holds, stub(Set("increment_holds")))
              .isInstanceOf[VerificationResult.Proven]
        )
        assert(
          statusOf(verifier, Clause.Kind.Ensuring) ==
              ClauseStatus.Checked(List("increment_holds"), "stub", Set(ProofKind.Blaster), Nil)
        )
    }

    test("a statement in other words checks a clause where the tactic finds it the same") {
        def verifierWith(same: Boolean): Verifier = {
            val verifier = Verifier.empty
            verifier.addFunction(checked)
            val List(clause) = verifier.guarantees(checked.ref).statements
            val written = verifier.statement(
              "in_other_words",
              forAll[BigInt, BigInt]((x, limit) => x + limit == limit + x)
            )
            // The tactic says the statement written here is the clause's, or another one.
            val key = (name: String) => if same && name == written.name then clause.name else name
            assert(
              verifier
                  .prove(written, stub(Set(written.name), key))
                  .isInstanceOf[VerificationResult.Proven]
            )
            verifier
        }
        assert(
          statusOf(verifierWith(same = true), Clause.Kind.Ensures) ==
              ClauseStatus.Checked(List("in_other_words"), "stub", Set(ProofKind.Blaster), Nil)
        )
        assert(statusOf(verifierWith(same = false), Clause.Kind.Ensures) match
            case ClauseStatus.Unchecked(List("checked/ensures#1"), _) => true
            case _                                                    => false)
    }

    test("a clause whose statement is declared open is open, and says why") {
        val verifier = Verifier.empty
        verifier.addFunction(checked)
        val List(clause) = verifier.guarantees(checked.ref).statements
        verifier.open(clause, "the solver does not finish it")
        assert(
          statusOf(verifier, Clause.Kind.Ensures) ==
              ClauseStatus.Open("checked/ensures#1", "the solver does not finish it")
        )
        // A proof wins over the declaration.
        verifier.prove(clause, stub(Set(clause.name)))
        assert(statusOf(verifier, Clause.Kind.Ensures).isInstanceOf[ClauseStatus.Checked])
    }

    test("a clause inlined twice into one function is two clauses, each of its own statement") {
        val twice = FunctionDef(UnusualSpecifications.twice)
        val verifier = Verifier.empty
        verifier.addFunction(twice)
        val clauses = verifier.specifications.map(_.clause)
        assert(clauses.map(_.occurrence) == List(1, 2), clauses)
        assert(clauses.map(_.line).distinct.size == 1, clauses)
        val List(first, second) = verifier.guarantees(twice.ref).statements
        verifier.prove(first, stub(Set(first.name)))
        // The second is stated by its own statement, and by the one of both together.
        assert(verifier.specifications.map(_.status) match
            case List(_: ClauseStatus.Checked, ClauseStatus.Unchecked(candidates, _)) =>
                candidates == List("twice/ensures", second.name)
            case _ => false)
    }

    test("a clause no statement can be made of says why, and can be declared open by its line") {
        val eachGrows = FunctionDef(UnusualSpecifications.eachGrows)
        val verifier = Verifier.empty
        verifier.addFunction(eachGrows)
        verifier.guarantees(eachGrows.ref)
        val List(specification) = verifier.specifications
        assert(specification.status match
            case ClauseStatus.Unchecked(Nil, missing) =>
                missing.contains("inside a function value") && missing.contains("verifier.open")
            case _ => false)
        verifier.open(eachGrows.ref, specification.clause.line, "a list of any length")
        assert(
          statusOf(verifier, Clause.Kind.Ensuring) == ClauseStatus.Open("", "a list of any length")
        )
    }

    test("a proof of a statement with limits checks the clause within them, and says so") {
        val positives = FunctionDef(SpecifiedExamples.positives)
        val verifier = Verifier.empty
        verifier.addFunction(positives)
        val List(limited) = verifier
            .guarantees(positives.ref, limitedData[PList[BigInt]](xs => xs.length <= 2))
            .statements
        verifier.prove(limited, stub(Set(limited.name)))
        assert(statusOf(verifier, Clause.Kind.Ensures) match
            case ClauseStatus.Checked(_, _, _, List(within)) => within.contains("at most 2")
            case _                                           => false)
    }

    test("a clause stays checked when another tactic of the same name tells statements otherwise") {
        val verifier = Verifier.empty
        verifier.addFunction(checked)
        val List(clause) = verifier.guarantees(checked.ref).statements
        val written = verifier.statement(
          "in_other_words",
          forAll[BigInt, BigInt]((x, limit) => x + limit == limit + x)
        )
        val same = (name: String) => if name == written.name then clause.name else name
        verifier.prove(written, stub(Set(written.name), same))
        // Another instance, of the same name, that tells every statement apart, proves another one.
        val other = verifier.statement("other", forAll[BigInt](x => x == x))
        verifier.prove(other, stub(Set(other.name)))
        assert(statusOf(verifier, Clause.Kind.Ensures) match
            case ClauseStatus.Checked(List("in_other_words"), _, _, _) => true
            case _                                                     => false)
    }

    test("code that is no function of its parameters is passed over, not failed on") {
        val verifier = Verifier.empty
        verifier.addFunction(
          FunctionDef(SpecifiedExamples.plain).withRepresentation(
            Representation.Sir,
            SIR.Const(Constant.Integer(1), SIRType.Integer, AnnotationsDecl.empty)
          )
        )
        assert(verifier.specifications.isEmpty)
    }

    test("an expects whose call no obligation can be made of is unchecked, and says why") {
        val verifier = Verifier.empty
        verifier.addFunction(clamp)
        val clampsEach = FunctionDef(UnusualSpecifications.clampsEach)
        verifier.addFunction(clampsEach)
        verifier.contract("clamp_stated", FunctionContract.inSource(clamp).get)
        val owed = verifier.obligations(clampsEach.ref)
        assert(owed.statements.isEmpty && owed.unsupported.nonEmpty, owed)
        assert(statusOf(verifier, Clause.Kind.Expects) match
            case ClauseStatus.Unchecked(_, missing) => missing.contains("no statement is made of")
            case _                                  => false)
    }

    test(
      "an expects is owed by no caller where none calls it, and by the obligations of those that do"
    ) {
        val verifier = Verifier.empty
        verifier.addFunction(clamp)
        assert(statusOf(verifier, Clause.Kind.Expects) == ClauseStatus.NotOwed)
        // A function here calls it, and owes it, with no obligation stated.
        verifier.addFunction(guarded)
        assert(statusOf(verifier, Clause.Kind.Expects) match
            case ClauseStatus.Unchecked(Nil, missing) =>
                missing.contains("state the obligations of guarded")
            case _ => false)
        // Its obligation, under the contract read from the code, stated and then proved.
        verifier.contract("clamp_stated", FunctionContract.inSource(clamp).get)
        val List(owed) = verifier.obligations(guarded.ref).statements
        assert(statusOf(verifier, Clause.Kind.Expects) match
            case ClauseStatus.Unchecked(List(name), missing) =>
                name == owed.name && missing.contains(s"prove ${owed.name}")
            case _ => false)
        verifier.prove(owed, stub(Set(owed.name)))
        assert(
          statusOf(verifier, Clause.Kind.Expects) ==
              ClauseStatus.Checked(List(owed.name), "stub", Set(ProofKind.Blaster), Nil)
        )
    }
}
