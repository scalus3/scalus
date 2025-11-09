package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.ledger.api.v3.ScriptContext
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.sir.*

class FromDataEraseLinkerTest extends AnyFunSuite {

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    def containsSubterm(sir: SIR)(p: SIR => Boolean): Boolean = {
        sir match {
            case SIR.And(left, right, anns) =>
                p(sir) || containsSubterm(left)(p) || containsSubterm(right)(p)
            case SIR.Or(left, right, anns) =>
                p(sir) || containsSubterm(left)(p) || containsSubterm(right)(p)
            case SIR.Not(term, anns) => p(sir) || containsSubterm(term)(p)
            case SIR.IfThenElse(cond, thenBranch, elseBranch, tp, anns) =>
                p(sir) ||
                containsSubterm(cond)(p) || containsSubterm(thenBranch)(p) || containsSubterm(
                  elseBranch
                )(p)
            case SIR.Match(scrutinee, cases, tp, anns) =>
                p(sir) || containsSubterm(scrutinee)(p) || cases.exists { c =>
                    containsSubterm(c.body)(p)
                }
            case SIR.Select(scrutinee, field, tp, anns) =>
                p(sir) || containsSubterm(scrutinee)(p)
            case SIR.Var(name, tp, anns)                 => p(sir)
            case SIR.ExternalVar(module, name, tp, anns) => p(sir)
            case SIR.Let(bindings, body, flags, anns) =>
                p(sir) || bindings.exists { b =>
                    containsSubterm(b.value)(p)
                } || containsSubterm(body)(p)
            case SIR.Apply(f, arg, tp, anns) =>
                p(sir) || containsSubterm(arg)(p) || containsSubterm(f)(p)
            case SIR.LamAbs(param, body, tps, anns) =>
                p(sir) || p(param) || containsSubterm(body)(p)
            case SIR.Const(c, tp, anns) => p(sir)
            case SIR.Constr(name, dataDecl, args, tp, anns) =>
                p(sir) || args.exists(arg => containsSubterm(arg)(p))
            case SIR.Cast(term, tp, anns) =>
                p(sir) || containsSubterm(term)(p)
            case SIR.Builtin(bn, tp, anns)   => p(sir)
            case SIR.Decl(data, term)        => p(sir) || containsSubterm(term)(p)
            case SIR.Error(msd, anns, cause) => p(sir)

        }
    }

    test("compiled function does not contains FromData instances after linking") {
        val sir = compile { (data: Data) =>
            val sc = Data.fromData[ScriptContext](data)
            sc.scriptInfo match {
                case SpendingScript(ref, datum) =>
                    if datum.isEmpty then scalus.prelude.fail("Expected datum to be present")
                    else BigInt(1)
            }
        }
        // println(sir.pretty.render(100))
        val foundFromDate = containsSubterm(sir) {
            case SIR.ExternalVar(
                  _,
                  "scalus.ledger.api.v3.ScriptContext$.given_FromData_ScriptContext",
                  _,
                  _
                ) =>
                true
            case _ => false
        }
        assert(!foundFromDate, "Expected no FromData instances in the compiled SIR")
        // val uplc = SirToUplcV3Lowering(sir).lower()
        // println(uplc.pretty.render(100))

    }

}
