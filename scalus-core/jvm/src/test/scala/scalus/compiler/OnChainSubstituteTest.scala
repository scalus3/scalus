package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.*
import scalus.compiler.test.*
import scalus.uplc.*
import scalus.uplc.builtin.*
import scalus.uplc.eval.PlutusVM
import scalus.toUplc

class OnChainSubstituteTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("@OnChainSubstitute: end-to-end evaluation with substituted calls") {
        // UseSimpleContext.run(ctx) calls ctx.getValue and ctx.add(v)
        // Plugin rewrites these to SubstOps.getValue(ctx) and SubstOps.add(ctx, v)
        // On-chain, ctx is Data.I(42)
        val sir = compile {
            UseSimpleContext.run(Data.I(42).asInstanceOf[SimpleContext])
        }
        val result = sir.toUplc().evaluate
        // getValue(I(42)) = 42, add(I(42), 42) = 42 + 42 = 84
        assert(result == Term.Const(Constant.Integer(84)))
    }

    test("@OnChainSubstitute: works with different values") {
        val sir = compile {
            UseSimpleContext.run(Data.I(10).asInstanceOf[SimpleContext])
        }
        val result = sir.toUplc().evaluate
        // getValue(I(10)) = 10, add(I(10), 10) = 10 + 10 = 20
        assert(result == Term.Const(Constant.Integer(20)))
    }

    test("@OnChainSubstitute: direct evaluation of substitute methods") {
        val sir = compile {
            SubstOps.getValue(Data.I(42))
        }
        val result = sir.toUplc().evaluate
        assert(result == Term.Const(Constant.Integer(42)))
    }

    test("@OnChainSubstitute: add method works") {
        val sir = compile {
            SubstOps.add(Data.I(10), BigInt(5))
        }
        val result = sir.toUplc().evaluate
        assert(result == Term.Const(Constant.Integer(15)))
    }

    test("@OnChainSubstitute: nested type — ctx.txInfo.fee") {
        // ctx is Data.I(100). txInfo(ctx) returns ctx itself (identity).
        // fee(txInfo) = txInfo.toI = 100.
        // This tests chaining through two @OnChainSubstitute types.
        val sir = compile {
            UseSimpleContext.getFee(Data.I(100).asInstanceOf[SimpleContext])
        }
        val result = sir.toUplc().evaluate
        assert(result == Term.Const(Constant.Integer(100)))
    }
}
