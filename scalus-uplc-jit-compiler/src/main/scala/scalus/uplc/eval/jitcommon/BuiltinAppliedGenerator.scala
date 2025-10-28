package scalus.uplc.eval.jitcommon

import scalus.uplc.{Expr as _, *}
import scalus.uplc.DefaultFun.{AddInteger, SubtractInteger}
import scalus.uplc.eval.*
import scalus.uplc.eval.ExBudgetCategory.Step

import scala.quoted.*

object BuiltinAppliedGenerator {

    def isSupported(bn: DefaultFun): Boolean =
        bn match
            case AddInteger      => true
            case SubtractInteger => true
            case _               => false

    def addInteger(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[BigInt] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.addInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] + ${ y }.asInstanceOf[BigInt]
        }
    }

    def subtractInteger(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[BigInt] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.subtractInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] - ${ y }.asInstanceOf[BigInt]
        }
    }

}
