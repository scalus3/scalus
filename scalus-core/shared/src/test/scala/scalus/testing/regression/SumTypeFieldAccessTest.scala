package scalus.testing.regression

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.Term.asTerm
import scalus.uplc.Constant.given
import scalus.uplc.eval.*

// Test enum for field access patterns
enum TestStatus:
    case Pending
    case Done(result: BigInt)
    case Failed(code: BigInt, message: String)

@Compile
object TestStatus:
    // Approach 1: Pattern matching with binders (extracts fields via unapply)
    def eqWithBinders(lhs: TestStatus, rhs: TestStatus): Boolean =
        lhs match
            case TestStatus.Pending =>
                rhs match
                    case TestStatus.Pending => true
                    case _                  => false
            case TestStatus.Done(r1) =>
                rhs match
                    case TestStatus.Done(r2) => r1 === r2
                    case _                   => false
            case TestStatus.Failed(c1, m1) =>
                rhs match
                    case TestStatus.Failed(c2, m2) => c1 === c2 && m1 === m2
                    case _                         => false

    // Approach 2: Type test pattern + direct field access
    // This should work but currently fails with:
    // "Cannot generate select for Integer as it is a sum type"
    def eqDirectFieldAccess(lhs: TestStatus, rhs: TestStatus): Boolean =
        lhs match
            case TestStatus.Pending =>
                rhs match
                    case TestStatus.Pending => true
                    case _                  => false
            case lhsDone: TestStatus.Done =>
                rhs match
                    case rhsDone: TestStatus.Done => lhsDone.result === rhsDone.result
                    case _                        => false
            case lhsFailed: TestStatus.Failed =>
                rhs match
                    case rhsFailed: TestStatus.Failed =>
                        lhsFailed.code === rhsFailed.code && lhsFailed.message === rhsFailed.message
                    case _ => false

    def mkDone(r: BigInt): TestStatus = TestStatus.Done(r)
    def mkFailed(c: BigInt, m: String): TestStatus = TestStatus.Failed(c, m)

class SumTypeFieldAccessTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()
    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    test("Pattern matching with binders compiles and runs correctly") {
        import TestStatus.mkDone

        val sir = compile { (r1: BigInt, r2: BigInt) =>
            val s1 = mkDone(r1)
            val s2 = mkDone(r2)
            TestStatus.eqWithBinders(s1, s2)
        }

        val program = sir.toUplcOptimized(false).plutusV3
        val result = (program $ BigInt(42).asTerm $ BigInt(42).asTerm).term.evaluateDebug

        assert(result.isSuccess, s"eqWithBinders failed: $result")
    }

    test("Direct field access on type-test bound variable should compile") {
        import TestStatus.mkDone

        // This test documents the current limitation:
        // Direct field access on type-test bound variables for sum type children
        // fails with "Cannot generate select for Integer as it is a sum type"

        val sir = compile { (r1: BigInt, r2: BigInt) =>
            val s1 = mkDone(r1)
            val s2 = mkDone(r2)
            TestStatus.eqDirectFieldAccess(s1, s2)
        }

        // Print SIR to debug what types are being assigned
        println("=== SIR for eqDirectFieldAccess ===")
        println(sir.pretty.render(120))
        println("=== END SIR ===")

        val program = sir.toUplcOptimized(false).plutusV3
        val result = (program $ BigInt(42).asTerm $ BigInt(42).asTerm).term.evaluateDebug

        assert(result.isSuccess, s"eqDirectFieldAccess failed: $result")
    }

    test("Budget comparison: binders vs direct field access for sum types") {
        import TestStatus.mkFailed

        val sirBinders = compile { (c1: BigInt, m1: String, c2: BigInt, m2: String) =>
            val s1 = mkFailed(c1, m1)
            val s2 = mkFailed(c2, m2)
            TestStatus.eqWithBinders(s1, s2)
        }

        val sirDirect = compile { (c1: BigInt, m1: String, c2: BigInt, m2: String) =>
            val s1 = mkFailed(c1, m1)
            val s2 = mkFailed(c2, m2)
            TestStatus.eqDirectFieldAccess(s1, s2)
        }

        val programBinders = sirBinders.toUplcOptimized(false).plutusV3
        val programDirect = sirDirect.toUplcOptimized(false).plutusV3

        val resultBinders =
            (programBinders $ BigInt(42).asTerm $ "error".asTerm $ BigInt(
              42
            ).asTerm $ "error".asTerm).term.evaluateDebug
        val resultDirect =
            (programDirect $ BigInt(42).asTerm $ "error".asTerm $ BigInt(
              42
            ).asTerm $ "error".asTerm).term.evaluateDebug

        assert(resultBinders.isSuccess, s"Binders failed: $resultBinders")
        assert(resultDirect.isSuccess, s"Direct failed: $resultDirect")

        println(s"Sum type field access budget comparison (Failed variant):")
        println(
          s"  With binders:        memory=${resultBinders.budget.memory}, steps=${resultBinders.budget.steps}"
        )
        println(
          s"  Direct field access: memory=${resultDirect.budget.memory}, steps=${resultDirect.budget.steps}"
        )
        println(
          s"  Difference:          memory=${resultBinders.budget.memory - resultDirect.budget.memory}, steps=${resultBinders.budget.steps - resultDirect.budget.steps}"
        )
    }
}
