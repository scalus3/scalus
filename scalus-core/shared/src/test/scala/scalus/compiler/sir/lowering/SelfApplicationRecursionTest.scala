package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.List
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

/** T2: the V3 lowering must encode single self-recursion via self-application
  * `(λf. body) ((λf. f f) (λf. rhs[f := f f]))` instead of the shared Z combinator. Proof of the
  * cost win: ExprSizeAndBudgetTest "T2 proof" tests.
  *
  * Note on PartialEvaluator: all tests except the constant-folding guard use plain `toUplc()`
  * (`optimizeUplc = false`), so no optimizer - and thus no `PartialEvaluator` - runs on them. The
  * budget floors below prove the recursion actually executes instead of being folded to a constant.
  */
class SelfApplicationRecursionTest extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV
    )

    test("simple self-recursion lowers without the Z combinator") {
        val uplc = compile {
            def rec(n: BigInt): BigInt =
                if n == BigInt(0) then BigInt(0)
                else rec(n - 1)
            rec(1000)
        }.toUplc()

        assert(!uplc.show.contains("__Z"), "lowered term must not reference the Z combinator")
        val result = uplc.evaluateDebug
        assert(result.isSuccess, s"evaluation failed: $result")
        assert(result.success.term == 0.asTerm)
        // Z encoding of this exact loop shape measured 505_993_433 steps / 2_205_601 mem.
        // Self-application measures 409_897_433 / 1_605_001 (-19% cpu, -27% mem).
        assert(result.budget.steps < 505_993_433L, s"budget did not improve: ${result.budget}")
        assert(result.budget.memory < 2_205_601L, s"budget did not improve: ${result.budget}")
        // Floor: 1000 iterations must actually run (a constant-folded term costs ~thousands)
        assert(result.budget.steps > 100_000_000L, s"loop did not run: ${result.budget}")
    }

    test("two-argument self-recursion lowers without the Z combinator") {
        val uplc = compile {
            def sum(n: BigInt, acc: BigInt): BigInt =
                if n == BigInt(0) then acc
                else sum(n - 1, acc + n)
            sum(100, 0)
        }.toUplc()

        assert(!uplc.show.contains("__Z"))
        val result = uplc.evaluateDebug
        assert(result.isSuccess, s"evaluation failed: $result")
        assert(result.success.term == 5050.asTerm)
        assert(result.budget.steps > 1_000_000L, s"loop did not run: ${result.budget}")
    }

    test("two independent recursive functions lower without the Z combinator") {
        val uplc = compile {
            def down(n: BigInt): BigInt =
                if n == BigInt(0) then BigInt(0) else down(n - 1)
            def up(n: BigInt): BigInt =
                if n == BigInt(10) then n else up(n + 1)
            down(10) + up(0)
        }.toUplc()

        assert(!uplc.show.contains("__Z"))
        val result = uplc.evaluateDebug
        assert(result.isSuccess, s"evaluation failed: $result")
        assert(result.success.term == 10.asTerm)
        assert(result.budget.steps > 1_000_000L, s"loops did not run: ${result.budget}")
    }

    test("optimizer can still constant-fold closed recursive computations") {
        // Regression guard: the self-application encoding must keep the fixpoint as a closed
        // argument-position subterm so Inliner+PartialEvaluator can fold closed recursive
        // computations at compile time (the first T2 encoding attempt broke this).

        // Trace-free (generateErrorTraces defaults to true, so disable explicitly):
        // the whole program folds to a constant.
        val uplc = compile(List.single(BigInt(1)).last)
            .toUplc(using Options(generateErrorTraces = false, optimizeUplc = true))()
        assert(uplc == 1.asTerm, s"expected full constant fold, got: ${uplc.show}")

        // With error traces (the original ListTest regression scenario) the fold is partial:
        // `last`'s "last of empty list" trace blocks PartialEvaluator (it refuses terms
        // containing Trace), so the final match survives while the recursion folds away.
        val traced = compile(List.single(BigInt(1)).last)
            .toUplc(using Options(generateErrorTraces = true, optimizeUplc = true))()
        val result = traced.evaluateDebug
        assert(result.isSuccess, s"evaluation failed: $result")
        assert(
          result.budget.memory <= 2128L && result.budget.steps <= 691_881L,
          s"constant folding regressed: ${result.budget}, expected <= ExUnits(2128, 691881)"
        )
    }

    test("prelude List recursion (runtime helpers) lowers without the Z combinator") {
        val uplc = compile { (n: BigInt) =>
            List.range(1, n).map(x => x + 1).foldLeft(BigInt(0))(_ + _)
        }.toUplc()

        assert(!uplc.show.contains("__Z"))
        val applied = uplc $ 5.asTerm
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"evaluation failed: $result")
        // 2+3+4+5+6 = 20
        assert(result.success.term == 20.asTerm)
        assert(result.budget.steps > 1_000_000L, s"recursion did not run: ${result.budget}")
    }
}
