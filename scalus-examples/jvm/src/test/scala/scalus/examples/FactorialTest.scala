package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.testing.kit.ScalusTest
import scalus.uplc.*
import scalus.uplc.DefaultFun.{IfThenElse, LessThanEqualsInteger, MultiplyInteger, SubtractInteger}
import scalus.uplc.Term.{asTerm, λ}
import scalus.uplc.eval.*
import scalus.uplc.transform.CaseConstrApply

import scala.language.implicitConversions

class FactorialTest extends AnyFunSuite with ScalusTest {
    test("factorial") {

        /** UPLC-CAPE Factorial Open Mode Implementation
          *
          * Open mode uses optimized iterative implementation with tail recursion. Generates a
          * lambda function that accepts n as a parameter.
          */
        // Versioned factorial term for direct evaluation
        def versionedFactorialTerm: Term = {
            import scalus.uplc.TermDSL.given
            // pfix' implementation
            def pfix(f: Term => Term) = λ { r => r $ r } $ λ { r => f(r $ r) }

            // pfactorial using pfix'
            val factorial = pfix: r =>
                λ: x =>
                    !(!IfThenElse $ (LessThanEqualsInteger $ x $ 0) $
                        ~1.asTerm $
                        ~(MultiplyInteger $ x $ (r $ (SubtractInteger $ x $ 1))))

            factorial
        }

        // Compile the parameterized factorial function to UPLC Program
        given PlutusVM = PlutusVM.makePlutusV3VM()

        val optimized = CaseConstrApply(versionedFactorialTerm)
        val program = optimized.plutusV3
        (program $ 10.asTerm).term.evaluateDebug match {
            case Result.Success(term, budget, _, _) =>
                assert(term == asTerm(3628800))
                assert(budget == ExBudget.fromCpuAndMemory(7752456, 28362))
            case Result.Failure(err, budget, _, _) =>
                fail(s"Evaluation failed: $err")
        }
    }
}
