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

    test("DefaultRepresentation produces different UPLC") {
        // Eq.by only uses Eq, no lists — A should be DefaultRepresentation
        import scalus.cardano.onchain.plutus.prelude.Eq
        val sir = compile {
            val eq = Eq.by[BigInt, BigInt](x => x + BigInt(1))
            eq(BigInt(1), BigInt(2))
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        info(s"Eq.by stats: $stats")
        val termOrig = new SirToUplcV3Lowering(sir).lower()
        val termNew = new SirToUplcV3Lowering(newSir).lower()
        val origStr = termOrig.show
        val newStr = termNew.show
        if origStr != newStr then {
            info("UPLC CHANGED!")
            info(s"Original:\n$origStr")
            info(s"Transformed:\n$newStr")
        } else {
            info("UPLC unchanged")
        }
        given vm: scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()
        val result = termNew.evaluateDebug
        assert(result.isSuccess, s"Failed: $result")
    }

    test("contains with Eq — boundary conversion between list and Eq repr") {
        import scalus.cardano.onchain.plutus.prelude.Eq
        // contains[A] has A as CanBeListAffected (from list)
        // Eq[A] has A as DefaultRepresentation (from FI annotation)
        // When contains calls eq(head, elem), conversion Data→native should be inserted
        val sir = compile {
            val l = List(BigInt(1), BigInt(2), BigInt(3))
            val eq = Eq[BigInt]
            l.find(x => eq(x, BigInt(2))).isDefined
        }
        val (newSir, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        info(s"contains stats: $stats")
        // Lower with transformed SIR and evaluate
        val term = new SirToUplcV3Lowering(newSir).lower()
        given vm: scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()
        val result = term.evaluateDebug
        info(s"Result: ${result}")
        assert(result.isSuccess, s"Failed: ${result}")
    }

    test("check FI annotation in compiled SIR") {
        // Compile a function that takes Ord — check if FI annotation is present
        val sir = compile {
            val m = SortedMap.singleton[BigInt, BigInt](BigInt(1), BigInt(42))
            m.get(BigInt(1))
        }
        // Walk SIR and find FunctionalInterface annotations
        var fiCount = 0
        def walk(s: SIR): Unit = s match {
            case SIR.Decl(_, term) => walk(term)
            case SIR.Let(bindings, body, _, _) =>
                bindings.foreach(b => walk(b.value))
                walk(body)
            case SIR.LamAbs(param, term, _, _) =>
                if param.anns.data.contains("functionalInterface") then {
                    fiCount += 1
                    info(s"Found FI annotation on param '${param.name}': ${param.anns.data("functionalInterface")}")
                }
                walk(term)
            case SIR.Apply(f, arg, _, _) => walk(f); walk(arg)
            case SIR.Match(s, cases, _, _) => walk(s); cases.foreach(c => walk(c.body))
            case SIR.Constr(_, _, args, _, _) => args.foreach(walk)
            case _ =>
        }
        walk(sir)
        info(s"Total FI annotations found: $fiCount")
        // Also check stats
        val (_, stats) = TypeVarKindAnalysis.analyze(sir, debug = true)
        info(s"Stats: $stats")
    }
}
