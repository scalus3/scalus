package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.compiler.sir.{AnnotationsDecl, SIR}
import scalus.uplc.{DeBruijn, Term}
import scalus.utils.ScalusSourcePos

/** Tests that CEK machine errors include source position information.
  *
  * Verifies the full pipeline: Scala source → SIR (with positions) → UPLC Term (with positions) →
  * CEK error (with position in message).
  */
class CekSourcePosTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("EvaluationFailure includes source position from Error term with explicit pos") {
        val pos = ScalusSourcePos("MyValidator.scala", 41, 4, 41, 30)
        val errorTerm = Term.Error(pos)
        val debruijned = DeBruijn.deBruijnTerm(errorTerm)

        val ex = intercept[EvaluationFailure] {
            summon[PlutusVM].evaluateDeBruijnedTerm(debruijned)
        }

        assert(ex.sourcePos == pos)
        assert(
          ex.getMessage.contains("MyValidator.scala:42"),
          s"Error message should contain 'MyValidator.scala:42' (1-based line), got: ${ex.getMessage}"
        )
    }

    test("Error term without position produces empty sourcePos") {
        val errorTerm = Term.Error() // default empty position
        val debruijned = DeBruijn.deBruijnTerm(errorTerm)

        val ex = intercept[EvaluationFailure] {
            summon[PlutusVM].evaluateDeBruijnedTerm(debruijned)
        }

        assert(ex.sourcePos.isEmpty, "sourcePos should be empty for term without position")
        // Message should NOT contain " at " since position is empty
        assert(
          !ex.getMessage.contains(" at "),
          s"Error message should not contain ' at ' for empty pos, got: ${ex.getMessage}"
        )
    }

    test("EvaluationFailure includes source position from compiled SIR.Error") {
        // SIR.Error preserves the annotation position through lowering
        val pos = ScalusSourcePos("Validator.scala", 10, 0, 10, 20)
        val annotations = AnnotationsDecl(pos)
        val sir = SIR.Error("validation failed", annotations)
        val uplc = sir.toUplc()
        val debruijned = DeBruijn.deBruijnTerm(uplc)

        val ex = intercept[EvaluationFailure] {
            summon[PlutusVM].evaluateDeBruijnedTerm(debruijned)
        }

        assert(!ex.sourcePos.isEmpty, "sourcePos should not be empty")
        assert(
          ex.getMessage.contains("Validator.scala:11"),
          s"Error message should contain 'Validator.scala:11', got: ${ex.getMessage}"
        )
    }

    test("BuiltinError includes source position via lastSourcePos") {
        // headList on empty list triggers a BuiltinError
        // lastSourcePos reports the position of the last term the CEK machine was computing.
        // In Apply(f, arg), the arg is the last computed term before the builtin fires,
        // so we attach the position to the arg (emptyList) to test this.
        val pos = ScalusSourcePos("Contract.scala", 24, 8, 24, 40)
        val emptyList = Term.Const(scalus.uplc.Constant.List(scalus.uplc.DefaultUni.Data, Nil), pos)
        val headListTerm = Term.Force(Term.Builtin(scalus.uplc.DefaultFun.HeadList))
        val term = Term.Apply(headListTerm, emptyList)
        val debruijned = DeBruijn.deBruijnTerm(term)

        val ex = intercept[BuiltinError] {
            summon[PlutusVM].evaluateDeBruijnedTerm(debruijned)
        }

        assert(
          ex.getMessage.contains("Contract.scala:25"),
          s"BuiltinError should contain 'Contract.scala:25', got: ${ex.getMessage}"
        )
    }

    test("evaluateDebug Failure result preserves source position in exception") {
        val pos = ScalusSourcePos("Script.scala", 5, 0, 5, 15)
        val errorTerm = Term.Error(pos)
        val debruijned = DeBruijn.deBruijnTerm(errorTerm)

        val result = debruijned.evaluateDebug
        assert(result.isFailure)
        result match
            case Result.Failure(ex: EvaluationFailure, _, _, _) =>
                assert(!ex.sourcePos.isEmpty, "sourcePos should not be empty in debug result")
                assert(
                  ex.getMessage.contains("Script.scala:6"),
                  s"Failure exception should contain 'Script.scala:6', got: ${ex.getMessage}"
                )
            case other => fail(s"Expected Failure with EvaluationFailure, got: $other")
    }

    test("compiled validator from external file reports position in that file") {
        // SampleValidator.positiveValidator is compiled in SampleValidator.scala
        // When it errors, the position should point to SampleValidator.scala, not this file.
        val uplc = SampleValidator.positiveValidator.toUplc()
        val applied = Term.Apply(uplc, Term.Const(scalus.uplc.Constant.Integer(BigInt(-1))))
        val debruijned = DeBruijn.deBruijnTerm(applied)

        val result = debruijned.evaluateDebug
        assert(result.isFailure, s"Expected failure for negative input, got: $result")
        result match
            case Result.Failure(ex: EvaluationFailure, _, _, _) =>
                assert(
                  !ex.sourcePos.isEmpty,
                  "sourcePos should not be empty for external validator error"
                )
                assert(
                  ex.sourcePos.file.contains("SampleValidator.scala"),
                  s"sourcePos file should point to SampleValidator.scala, got: ${ex.sourcePos.file}"
                )
                assert(
                  ex.getMessage.contains("SampleValidator.scala"),
                  s"Error message should reference SampleValidator.scala, got: ${ex.getMessage}"
                )
                // The throw is on line 17 of SampleValidator.scala
                val line1Based = ex.sourcePos.startLine + 1
                assert(
                  line1Based == 17,
                  s"Expected line 17 for positiveValidator throw, got: $line1Based"
                )
            case Result.Failure(ex, _, _, _) =>
                fail(s"Expected EvaluationFailure, got: ${ex.getClass.getName}: ${ex.getMessage}")
            case _ => fail("Expected failure result")
    }

    test("always-fails validator from external file reports position in that file") {
        val uplc = SampleValidator.alwaysFails.toUplc()
        val debruijned = DeBruijn.deBruijnTerm(uplc)

        val result = debruijned.evaluateDebug
        assert(result.isFailure)
        result match
            case Result.Failure(ex: EvaluationFailure, _, _, _) =>
                assert(
                  ex.sourcePos.file.contains("SampleValidator.scala"),
                  s"sourcePos file should point to SampleValidator.scala, got: ${ex.sourcePos.file}"
                )
                // The throw is on line 22 of SampleValidator.scala
                val line1Based = ex.sourcePos.startLine + 1
                assert(
                  line1Based == 22,
                  s"Expected line 22 for alwaysFails throw, got: $line1Based"
                )
            case Result.Failure(ex, _, _, _) =>
                fail(s"Expected EvaluationFailure, got: ${ex.getClass.getName}: ${ex.getMessage}")
            case _ => fail("Expected failure result")
    }

    test("headList on empty list reports position in SampleValidator.scala") {
        // SampleValidator.headOfEmptyList calls headList(mkNilData())
        // This triggers a BuiltinError — the lastSourcePos should point to SampleValidator.scala
        val uplc = SampleValidator.headOfEmptyList.toUplc()
        val debruijned = DeBruijn.deBruijnTerm(uplc)

        val result = debruijned.evaluateDebug
        assert(result.isFailure, s"Expected failure for head of empty list, got: $result")
        result match
            case Result.Failure(ex: BuiltinError, _, _, _) =>
                assert(
                  !ex.sourcePos.isEmpty,
                  "sourcePos should not be empty for headList on empty list"
                )
                assert(
                  ex.sourcePos.file.contains("SampleValidator.scala"),
                  s"sourcePos file should point to SampleValidator.scala, got: ${ex.sourcePos.file}"
                )
            case Result.Failure(ex, _, _, _) =>
                // Accept any StackTraceMachineError with position info
                ex match
                    case stme: StackTraceMachineError =>
                        assert(
                          stme.sourcePos.file.contains("SampleValidator.scala"),
                          s"sourcePos file should point to SampleValidator.scala, got: ${stme.sourcePos.file}"
                        )
                    case _ =>
                        fail(
                          s"Expected BuiltinError or StackTraceMachineError, got: ${ex.getClass.getName}: ${ex.getMessage}"
                        )
            case _ => fail("Expected failure result")
    }

    test("incomplete case match reports position in SampleValidator.scala") {
        // SampleValidator.incompleteCaseMatch only handles Cons, not Nil.
        // Use SumOfProducts lowering (V4 VM) so Case/Constr instructions are used,
        // which gives proper MissingCaseBranch errors with source positions.
        import scalus.compiler.sir.lowering.simple.SumOfProductsLowering
        import scalus.cardano.ledger.Word64
        val v4vm = PlutusVM.makePlutusV4VM()
        val sir = SampleValidator.incompleteCaseMatch
        val uplc = SumOfProductsLowering(sir, generateErrorTraces = false).lower()
        // Apply the function to Nil (Constr tag 0 = Nil in SumOfProducts encoding)
        val nilConstr = Term.Constr(Word64.Zero, Nil)
        val applied = Term.Apply(uplc, nilConstr)
        val debruijned = DeBruijn.deBruijnTerm(applied)

        val result = {
            val spenderLogger = TallyingBudgetSpenderLogger(CountingBudgetSpender())
            try
                val r = v4vm.evaluateDeBruijnedTerm(debruijned, spenderLogger, spenderLogger)
                Result.Success(
                  r,
                  spenderLogger.getSpentBudget,
                  spenderLogger.costs.toMap,
                  spenderLogger.getLogs.toSeq
                )
            catch
                case e: Exception =>
                    Result.Failure(
                      e,
                      spenderLogger.getSpentBudget,
                      spenderLogger.costs.toMap,
                      spenderLogger.getLogs.toSeq
                    )
        }
        assert(result.isFailure, s"Expected failure for Nil with incomplete match, got: $result")
        result match
            case Result.Failure(ex: StackTraceMachineError, _, _, _) =>
                assert(
                  !ex.sourcePos.isEmpty,
                  s"sourcePos should not be empty for incomplete case match, error: ${ex.getMessage}"
                )
                assert(
                  ex.sourcePos.file.contains("SampleValidator.scala"),
                  s"sourcePos file should point to SampleValidator.scala, got: ${ex.sourcePos.file}"
                )
            case Result.Failure(ex, _, _, _) =>
                fail(
                  s"Expected StackTraceMachineError, got: ${ex.getClass.getName}: ${ex.getMessage}"
                )
            case _ => fail("Expected failure result")
    }

    test("compiled function error reports source position in this file") {
        // End-to-end: compile a Scala function that throws,
        // lower to UPLC (positions propagated), evaluate, and verify
        // the error message points back to this source file and line.
        val validate = compile { (x: BigInt) =>
            if x > BigInt(0) then x
            else throw new Exception("must be positive") // error on this line
        }
        val uplc = validate.toUplc()
        val applied = Term.Apply(uplc, Term.Const(scalus.uplc.Constant.Integer(BigInt(-1))))
        val debruijned = DeBruijn.deBruijnTerm(applied)

        val result = debruijned.evaluateDebug
        assert(result.isFailure, s"Expected failure for negative input, got: $result")
        result match
            case Result.Failure(ex: EvaluationFailure, _, _, _) =>
                assert(
                  !ex.sourcePos.isEmpty,
                  "sourcePos should not be empty for compiled code error"
                )
                assert(
                  ex.sourcePos.file.contains("CekSourcePosTest.scala"),
                  s"sourcePos file should contain 'CekSourcePosTest.scala', got: ${ex.sourcePos.file}"
                )
                assert(
                  ex.getMessage.contains("CekSourcePosTest.scala"),
                  s"Error message should contain this file name, got: ${ex.getMessage}"
                )
            case Result.Failure(ex, _, _, _) =>
                fail(s"Expected EvaluationFailure, got: ${ex.getClass.getName}: ${ex.getMessage}")
            case _ => fail("Expected failure result")
    }
}
