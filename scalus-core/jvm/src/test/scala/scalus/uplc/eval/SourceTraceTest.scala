package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.uplc.Program

class SourceTraceTest extends AnyFunSuite:

    val plutusVM: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)

    test("evaluateScriptDebug should include source trace on failure") {
        val sir = compile {
            val x = BigInt(1) / BigInt(0) // Division by zero - will fail
            x
        }

        val uplc = sir.toUplc(generateErrorTraces = true, optimizeUplc = false)
        val prog = Program.plutusV3(uplc).deBruijnedProgram
        val result = plutusVM.evaluateScriptDebug(prog)

        result match
            case Result.Failure(ex, _, _, _) =>
                ex match
                    case e: StackTraceMachineError =>
                        val trace = e.sourceTrace
                        assert(trace.nonEmpty, "Source trace should not be empty")
                        info(s"Source trace has ${trace.size} positions")
                    case other =>
                        fail(
                          s"Expected StackTraceMachineError, got ${other.getClass.getSimpleName}"
                        )
            case _ =>
                fail("Expected failure, got success")
    }

    test("evaluateScriptProfile should include source trace on failure") {
        val sir = compile {
            val x = BigInt(1) / BigInt(0) // Division by zero - will fail
            x
        }

        val uplc = sir.toUplc(generateErrorTraces = true, optimizeUplc = false)
        val prog = Program.plutusV3(uplc).deBruijnedProgram
        val result = plutusVM.evaluateScriptProfile(prog)

        result match
            case Result.Failure(ex, _, _, _) =>
                ex match
                    case e: StackTraceMachineError =>
                        val trace = e.sourceTrace
                        assert(trace.nonEmpty, "Source trace should not be empty with profiling")
                        info(s"Source trace with profiling has ${trace.size} positions")
                    case other =>
                        fail(
                          s"Expected StackTraceMachineError, got ${other.getClass.getSimpleName}"
                        )
            case _ =>
                fail("Expected failure, got success")
    }

    test("source trace should be empty without debug or profiling") {
        val sir = compile {
            val x = BigInt(1) / BigInt(0) // Division by zero - will fail
            x
        }

        val uplc = sir.toUplc(generateErrorTraces = true, optimizeUplc = false)
        val prog = Program.plutusV3(uplc).deBruijnedProgram
        // Use evaluateDeBruijnedTerm without debug/profiling, wrapped in try/catch
        try
            plutusVM.evaluateDeBruijnedTerm(prog.term)
            fail("Expected failure, got success")
        catch
            case e: StackTraceMachineError =>
                val trace = e.sourceTrace
                assert(trace.isEmpty, "Source trace should be empty without debug/profiling")
            case other =>
                fail(s"Expected StackTraceMachineError, got ${other.getClass.getSimpleName}")
    }
end SourceTraceTest
