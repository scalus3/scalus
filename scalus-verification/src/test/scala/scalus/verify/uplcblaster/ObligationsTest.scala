package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{require, List as PList, Math}
import scalus.compiler.Compile
import scalus.verify.*
import scalus.verify.Props.*

/** Callers of `Math.clamp`, whose contract expects `lo <= hi`. */
@Compile
object ObligationExamples {
    def violates(x: BigInt): BigInt = Math.clamp(x, BigInt(10), BigInt(0))

    def guarded(x: BigInt, lo: BigInt, hi: BigInt): BigInt =
        if lo <= hi then Math.clamp(x, lo, hi) else lo

    def passes(x: BigInt, lo: BigInt, hi: BigInt): BigInt = Math.clamp(x, lo, hi)

    def checks(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        require(lo <= hi)
        Math.clamp(x, lo, hi)
    }

    def mapped(xs: PList[BigInt], lo: BigInt, hi: BigInt): PList[BigInt] =
        xs.map(x => Math.clamp(x, lo, hi))
}

class ObligationsTest extends AnyFunSuite with LeanProofs {

    private val clamp = FunctionDef(Math.clamp)
    private val violates = FunctionDef(ObligationExamples.violates)
    private val guarded = FunctionDef(ObligationExamples.guarded)
    private val passes = FunctionDef(ObligationExamples.passes)
    private val checks = FunctionDef(ObligationExamples.checks)
    private val mapped = FunctionDef(ObligationExamples.mapped)

    /** A verifier with `clamp`, its contract, and `functions`. */
    private def verifierWith(functions: FunctionDef[?, ?]*): Verifier = {
        val verifier = Verifier.empty
        (clamp +: functions).foreach(verifier.addFunction)
        verifier.contract(
          "clamp_in_range",
          contract(clamp)(
            expects = (x, lo, hi) => lo <= hi,
            ensures = (x, lo, hi) => r => lo <= r && r <= hi
          )
        )
        verifier
    }

    /** The one obligation of `caller`'s call of `clamp`. */
    private def obligation(verifier: Verifier, caller: FunctionDef[?, ?]): Statement =
        verifier.obligations(caller.ref) match
            case CallObligations(List(owed), Nil) => owed
            case other                            => fail(s"expected one obligation, got $other")

    private def prove(verifier: Verifier, statement: Statement): VerificationResult = {
        requireLean()
        verifier.verify(statement, UplcBlaster(Budget.LeanSteps(120), lean))
    }

    test("a call of a function with a contract gives one obligation, named after both") {
        val owed = obligation(verifierWith(violates), violates)
        assert(owed.name == "violates/clamp_in_range#1")
        owed.origin match
            case Origin.Obligation(caller, callee, "clamp_in_range", line) =>
                assert(caller == violates.ref && callee == clamp.ref && line > 0)
            case other => fail(s"expected an obligation's origin, got $other")
        // for every x: where the call is reached, clamp's precondition holds of its arguments
        owed.prop match
            case Prop.Forall(x, Prop.Implies(Prop.Denotes(_), Prop.Bool(_))) =>
                assert(x.tp == scalus.compiler.sir.SIRType.Integer)
            case other => fail(s"expected for all x, denotes(reach) ==> check, got $other")
    }

    test("a caller's own contract is the premise of its obligations") {
        val verifier = verifierWith(passes)
        val assumes = verifier.contract(
          "passes_in_range",
          contract(passes)(expects = (x, lo, hi) => lo <= hi, ensures = (x, lo, hi) => r => true)
        )
        verifier.obligations(assumes) match
            case CallObligations(List(owed), Nil) =>
                // named after the contract, so the function's own obligations can stand beside it
                assert(owed.name == "passes_in_range/clamp_in_range#1")
                assert(
                  verifier.obligations(passes.ref).statements.map(_.name) ==
                      List("passes/clamp_in_range#1")
                )
                owed.prop match
                    case Prop.Forall(
                          _,
                          Prop.Forall(
                            lo,
                            Prop.Forall(
                              hi,
                              Prop.Implies(Prop.Bool(PropExpr.SIRExpr(premise)), Prop.Implies(_, _))
                            )
                          )
                        ) =>
                        // the contract's variables are renamed after the function's parameters
                        assert(
                          premise.toString.contains(lo.name) && premise.toString.contains(hi.name),
                          premise
                        )
                    case other => fail(s"expected the caller's precondition as premise, got $other")
            case other => fail(s"expected one obligation, got $other")
        assertThrows[IllegalArgumentException](
          verifier.obligations(verifier.statement("plain", Prop(BigInt(1) > BigInt(0))))
        )
    }

    test("a call inside a function value is reported, not dropped") {
        val result = verifierWith(mapped).obligations(mapped.ref)
        assert(result.statements.isEmpty)
        assert(result.unsupported.size == 1, result.unsupported)
        assert(result.unsupported.head.contains("inside a function value"), result.unsupported)
    }

    test("an obligation is refuted when the arguments violate the precondition") {
        // clamp(x, 10, 0)
        val verifier = verifierWith(violates)
        prove(verifier, obligation(verifier, violates)) match
            case VerificationResult.Refuted(_) =>
            case other                         => fail(s"expected a refutation, got $other")
        // clamp(x, lo, hi), with nothing known about lo and hi
        val unknown = verifierWith(passes)
        prove(unknown, obligation(unknown, passes)) match
            case VerificationResult.Refuted(_) =>
            case other                         => fail(s"expected a refutation, got $other")
    }

    test("an obligation is proved when a branch, a contract or a runtime check establishes it") {
        // if lo <= hi then clamp(x, lo, hi) else lo
        val branch = verifierWith(guarded)
        assert(prove(branch, obligation(branch, guarded)).isInstanceOf[VerificationResult.Proven])

        // clamp(x, lo, hi), in a function that itself expects lo <= hi
        val assumed = verifierWith(passes)
        val assumes = assumed.contract(
          "passes_in_range",
          contract(passes)(expects = (x, lo, hi) => lo <= hi, ensures = (x, lo, hi) => r => true)
        )
        val List(owed) = assumed.obligations(assumes).statements
        assert(prove(assumed, owed).isInstanceOf[VerificationResult.Proven])

        // require(lo <= hi); clamp(x, lo, hi): where the check fails, the call is not reached
        val checked = verifierWith(checks)
        assert(prove(checked, obligation(checked, checks)).isInstanceOf[VerificationResult.Proven])
    }
}
