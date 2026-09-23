package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.builtin.{platform, ByteString, given}
import scala.language.implicitConversions

class ConfiguredEvaluationTest extends AnyFunSuite {
    private val vm = PlutusVM.makePlutusV3VM()

    private val integer = 42.asTerm.plutusV3.deBruijnedProgram
    private val tracedInteger = (!Trace $ "before failure" $ 42).plutusV3.deBruijnedProgram

    private def evaluate(
        program: DeBruijnedProgram,
        budgetSpender: BudgetSpender = new CountingBudgetSpender,
        profiling: Boolean = false,
        validateResult: Boolean = false
    ): Result =
        vm.runWithBudgetTracking(
          program.term,
          budgetSpender,
          profiling,
          tracing = true,
          validateResult = validateResult
        )

    test("successful trace retains bare logs") {
        val program = (!Trace $ "hello" $ ().asTerm).plutusV3.deBruijnedProgram
        val result = evaluate(program, validateResult = true)
        assert(result.isSuccess)
        assert(result.logs == Seq("hello")) // spec [TB-1]
        assert(result.logs == vm.evaluateScriptDebug(program).logs)
        assert(result.profile.isEmpty)
        assert(evaluate(integer).logs.isEmpty)
    }

    test("explicit and builtin failures retain spent budget") {
        Seq(Error(), DivideInteger $ 1 $ 0, AddInteger $ true $ 1).foreach { term =>
            val result = evaluate(term.plutusV3.deBruijnedProgram)
            assert(!result.isSuccess)
            assert(result.budget.memory > 0)
            assert(result.budget.steps > 0)
            assert(result.logs.isEmpty)
        }
    }

    test("zero and tiny memory and CPU limits are independently enforced") {
        Seq(
          ExUnits(0, Long.MaxValue),
          ExUnits(1, Long.MaxValue),
          ExUnits(Long.MaxValue, 0),
          ExUnits(Long.MaxValue, 1)
        ).foreach { limit =>
            val result = evaluate(integer, new RestrictingBudgetSpender(limit))
            assert(
              result.asInstanceOf[Result.Failure].exception.isInstanceOf[OutOfExBudgetError]
            )
            assert(result.budget == ExUnits(100, 100))
        }
        val counted = evaluate(integer)
        val limited = evaluate(integer, new RestrictingBudgetSpender(counted.budget))
        assert(limited.isSuccess)
        assert(limited.budget == counted.budget)
    }

    test("return validation retains completed budget and logs") {
        val unchecked = evaluate(tracedInteger)
        val checked = evaluate(tracedInteger, validateResult = true)
        assert(unchecked.isSuccess)
        assert(
          checked.asInstanceOf[Result.Failure].exception.isInstanceOf[InvalidReturnValue]
        )
        assert(checked.budget == unchecked.budget)
        assert(checked.logs == Seq("before failure")) // spec [TB-1]
        assert(checked.logs == unchecked.logs)
        assert(evaluate(integer).isSuccess)
        assert(!evaluate(integer, validateResult = true).isSuccess)
    }

    test("profiling preserves success, failure, budget and trace semantics") {
        val tracedFailure =
            (λ("ignored")(Error()) $ (!Trace $ "first" $ ())).plutusV3.deBruijnedProgram
        Seq(tracedInteger, tracedFailure).foreach { program =>
            Seq(false, true).foreach { validate =>
                val plain = evaluate(program, validateResult = validate)
                val profiled = evaluate(program, profiling = true, validateResult = validate)
                assert(plain.isSuccess == profiled.isSuccess)
                assert(plain.budget == profiled.budget)
                assert(plain.logs == profiled.logs)
                assert(plain.logs.nonEmpty)
                assert(plain.profile.isEmpty)
                assert(profiled.profile.nonEmpty)
            }
        }
        val exhausted =
            evaluate(integer, new RestrictingBudgetSpender(ExUnits.zero), profiling = true)
        assert(!exhausted.isSuccess)
        assert(exhausted.profile.nonEmpty)
    }

    test("input domain failures from arithmetic, indexing, empty lists, UTF-8 and signatures") {
        val emptyList = Constant.List(DefaultUni.Integer, Nil)
        val fixtures: Seq[Term] = Seq(
          DivideInteger $ 1 $ 0,
          QuotientInteger $ 1 $ 0,
          ModInteger $ 1 $ 0,
          RemainderInteger $ 1 $ 0,
          IndexByteString $ ByteString.empty $ 0,
          !HeadList $ emptyList,
          !TailList $ emptyList,
          DecodeUtf8 $ ByteString.fromHex("ff"),
          VerifyEd25519Signature $ ByteString.empty $ ByteString.empty $ ByteString.empty,
          VerifyEcdsaSecp256k1Signature $ ByteString.empty $ ByteString.empty $ ByteString.empty,
          VerifySchnorrSecp256k1Signature $ ByteString.empty $ ByteString.empty $ ByteString.empty
        )
        fixtures.foreach { term =>
            val result = evaluate(term.plutusV3.deBruijnedProgram)
            assert(
              result.asInstanceOf[Result.Failure].exception.isInstanceOf[BuiltinError],
              term.toString
            )
        }
        val pv11 = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
        val outOfBounds = (!IndexArray $ (!ListToArray $ emptyList) $ 0).plutusV3.deBruijnedProgram
        assert(
          !pv11
              .runWithBudgetTracking(
                outOfBounds.term,
                new CountingBudgetSpender,
                profiling = false,
                tracing = true,
                validateResult = false
              )
              .isSuccess
        )
    }

    test("a cause the machine does not own is still reported as a failure, not thrown") {
        // There is no whitelist of "expected" exceptions any more. A defect inside a builtin
        // comes back as a Result.Failure carrying the original exception, so a caller that asked
        // for a result always gets one; the JS facade turns this into INTERNAL_ERROR.
        val sentinel = new IllegalStateException("broken logger")
        val logger = new Logger {
            def log(message: String): Unit = throw sentinel
            def getLogs: Array[String] = Array.empty
        }
        val builtins =
            new CardanoBuiltins(vm.machineParams.builtinCostModel, platform, vm.semanticVariant)
        val cek =
            new CekMachine(vm.machineParams, NoBudgetSpender, logger, builtins.getBuiltinRuntime)
        val error = intercept[BuiltinError] {
            cek.evaluateTerm((!Trace $ "hello" $ ()).plutusV3.deBruijnedProgram.term)
        }
        assert(error.cause eq sentinel)

        val failure = evaluate((!Trace $ "hello" $ ()).plutusV3.deBruijnedProgram)
        assert(failure.isSuccess) // the real logger does not throw; the machine path is unchanged
    }
}
