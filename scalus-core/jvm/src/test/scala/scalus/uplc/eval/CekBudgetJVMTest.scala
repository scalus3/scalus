package scalus.uplc
package eval

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.ExUnits
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.eval.CekMachineCosts.defaultMachineCosts.*

class CekBudgetJVMTest extends AnyFunSuite:
    private def check(term: Term, expected: ExUnits): Unit = {
        val expectedBudget = expected |+| startupCost
        test(
          s"Check machine budget for terms ${term.pretty.flatten.render(100)} is $expectedBudget"
        ) {
            given PlutusVM = PlutusVM.makePlutusV2VM()
            val res = UplcCli.evalFlat(term.plutusV2)
            (term.evaluateDebug, res) match
                case (Result.Success(t1, budget, _, _), UplcEvalResult.Success(t2, budget2)) =>
                    assert(
                      budget == expectedBudget,
                      s"for term $term"
                    )
                    assert(budget == budget2, "Budgets should match with cardano uplc eval")
                case r => fail(s"Unexpected result for term $term: $r")
        }
    }

    val h = Const(asConstant("Hello"))
    val id = LamAbs("x", Var(NamedDeBruijn("x")))
    val app = Apply(id, h)
    val delay = Delay(h)
    val force = Force(delay)
    val builtin = Builtin(DefaultFun.MkNilData)

    check(h, constCost)
    check(id, lamCost)
    check(app, applyCost |+| lamCost |+| varCost |+| constCost)
    check(delay, delayCost)
    check(force, forceCost |+| delayCost |+| constCost)
    check(builtin, builtinCost)
