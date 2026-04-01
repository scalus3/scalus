package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.prelude.*
import scalus.compiler.Options
import scalus.compiler.sir.lowering.SirToUplcV3Lowering

class TypeVarKindAnalysisTest extends AnyFunSuite {

    test("TypeVarKind analysis statistics for List.map") {
        val sir = compile {
            val l = List(BigInt(1), BigInt(2), BigInt(3))
            l.map(_ + BigInt(1))
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        assert(stats.total > 0, "should find some TypeVars")
    }

    test("TypeVarKind analysis statistics for foldLeft") {
        val sir = compile {
            val l = List(BigInt(1), BigInt(2), BigInt(3))
            l.foldLeft(BigInt(0))(_ + _)
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        assert(stats.total > 0)
    }

    test("TypeVarKind analysis statistics for SortedMap") {
        val sir = compile {
            val m = SortedMap.singleton[BigInt, BigInt](BigInt(1), BigInt(42))
            m.get(BigInt(1))
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        assert(stats.total > 0)
    }

    test("analysis does not break lowering for List.map") {
        val sir = compile {
            val l = List(BigInt(1), BigInt(2), BigInt(3))
            l.map(_ + BigInt(1)).filter(_ > BigInt(2))
        }
        val term = SirToUplcV3Lowering.fromOptions(sir, Options()).lower()
        assert(term != null)
    }

    test("analysis does not break lowering for foldLeft") {
        val sir = compile {
            val l = List(BigInt(1), BigInt(2), BigInt(3))
            l.foldLeft(BigInt(0))(_ + _)
        }
        val term = SirToUplcV3Lowering.fromOptions(sir, Options()).lower()
        assert(term != null)
    }

    test("Data.Map compare UPLC: original vs transformed") {
        import scalus.uplc.builtin.{Data, ByteString}
        import scalus.cardano.onchain.plutus.prelude.List as PList
        val sir = compile {
            Data.Map(PList((Data.I(BigInt(1)), Data.I(BigInt(10)))))
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir)
        info(s"Data.Map stats: $stats")
        val termOrig = new SirToUplcV3Lowering(sir).lower()
        val termNew = new SirToUplcV3Lowering(newSir).lower()
        info(s"Original UPLC:\n${termOrig.show}")
        info(s"Transformed UPLC:\n${termNew.show}")
        given vm: scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()
        val resultOrig = termOrig.evaluateDebug
        val resultNew = termNew.evaluateDebug
        info(s"Original: ${resultOrig}")
        info(s"Transformed: ${resultNew}")
        assert(resultNew.isSuccess, s"Transformed failed: ${resultNew}")
    }

    test("compare UPLC: original vs transformed SIR") {
        val sir = compile {
            List(BigInt(1), BigInt(2)).head
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        info(s"head stats: $stats")
        // Lower both
        val termOrig = new SirToUplcV3Lowering(sir).lower()
        val termNew = new SirToUplcV3Lowering(newSir).lower()
        info(s"Original UPLC:\n${termOrig.show}")
        info(s"Transformed UPLC:\n${termNew.show}")
        // Compare
        given vm: scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()
        val resultOrig = termOrig.evaluateDebug
        val resultNew = termNew.evaluateDebug
        info(s"Original result: ${resultOrig}")
        info(s"Transformed result: ${resultNew}")
        assert(resultNew.isSuccess, s"Transformed failed: ${resultNew}")
    }
}
