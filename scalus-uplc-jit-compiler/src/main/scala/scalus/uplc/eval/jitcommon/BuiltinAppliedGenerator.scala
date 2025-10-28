package scalus.uplc.eval.jitcommon

import scalus.builtin.{Builtins, ByteString, Data}
import scalus.uplc.{Expr as _, *}
import scalus.uplc.DefaultFun.*
import scalus.uplc.eval.*
import scalus.uplc.eval.ExBudgetCategory.Step

import scala.quoted.*

object BuiltinAppliedGenerator {

    // Two-argument builtins that can be optimized
    def isSupported2(bn: DefaultFun): Boolean =
        bn match
            // Integer operations
            case AddInteger            => true
            case SubtractInteger       => true
            case MultiplyInteger       => true
            case DivideInteger         => true
            case QuotientInteger       => true
            case RemainderInteger      => true
            case ModInteger            => true
            case EqualsInteger         => true
            case LessThanInteger       => true
            case LessThanEqualsInteger => true
            // ByteString operations
            case AppendByteString         => true
            case EqualsByteString         => true
            case LessThanByteString       => true
            case LessThanEqualsByteString => true
            // String operations
            case AppendString => true
            case EqualsString => true
            // Data operations
            case EqualsData => true
            case _          => false

    // One-argument builtins that can be optimized
    def isSupported1(bn: DefaultFun): Boolean =
        bn match
            // Hash functions
            case Sha2_256    => true
            case Sha3_256    => true
            case Blake2b_256 => true
            // Data destructors
            case UnConstrData => true
            case UnListData   => true
            case UnIData      => true
            case UnBData      => true
            // ByteString operations
            case LengthOfByteString => true
            // String operations
            case EncodeUtf8 => true
            case DecodeUtf8 => true
            // List operations
            case HeadList => true
            case TailList => true
            case _        => false

    // For backward compatibility
    def isSupported(bn: DefaultFun): Boolean = isSupported2(bn)

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

    def multiplyInteger(
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
              ${ params }.builtinCostModel.multiplyInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] * ${ y }.asInstanceOf[BigInt]
        }
    }

    def equalsInteger(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.equalsInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] == ${ y }.asInstanceOf[BigInt]
        }
    }

    def lessThanInteger(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.lessThanInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] < ${ y }.asInstanceOf[BigInt]
        }
    }

    def lessThanEqualsInteger(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.lessThanEqualsInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] <= ${ y }.asInstanceOf[BigInt]
        }
    }

    def equalsByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.equalsByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[ByteString] == ${ y }.asInstanceOf[ByteString]
        }
    }

    def equalsData(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.equalsData.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            scalus.builtin.Builtins.equalsData(
              ${ x }.asInstanceOf[Data],
              ${ y }.asInstanceOf[Data]
            )
        }
    }

    // Additional two-argument integer operations

    def divideInteger(
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
              ${ params }.builtinCostModel.divideInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[BigInt] / ${ y }.asInstanceOf[BigInt]
        }
    }

    def quotientInteger(
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
              ${ params }.builtinCostModel.quotientInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.quotientInteger(${ x }.asInstanceOf[BigInt], ${ y }.asInstanceOf[BigInt])
        }
    }

    def remainderInteger(
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
              ${ params }.builtinCostModel.remainderInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.remainderInteger(${ x }.asInstanceOf[BigInt], ${ y }.asInstanceOf[BigInt])
        }
    }

    def modInteger(
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
              ${ params }.builtinCostModel.modInteger.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.modInteger(${ x }.asInstanceOf[BigInt], ${ y }.asInstanceOf[BigInt])
        }
    }

    // ByteString operations

    def appendByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.appendByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.appendByteString(
              ${ x }.asInstanceOf[ByteString],
              ${ y }.asInstanceOf[ByteString]
            )
        }
    }

    def lessThanByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.lessThanByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.lessThanByteString(
              ${ x }.asInstanceOf[ByteString],
              ${ y }.asInstanceOf[ByteString]
            )
        }
    }

    def lessThanEqualsByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.lessThanEqualsByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.lessThanEqualsByteString(
              ${ x }.asInstanceOf[ByteString],
              ${ y }.asInstanceOf[ByteString]
            )
        }
    }

    // String operations

    def appendString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[String] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.appendString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[String] + ${ y }.asInstanceOf[String]
        }
    }

    def equalsString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Boolean] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.equalsString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            ${ x }.asInstanceOf[String] == ${ y }.asInstanceOf[String]
        }
    }

    // One-argument builtin generators

    def sha2_256(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.sha2_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.sha2_256(${ x }.asInstanceOf[ByteString])
        }
    }

    def sha3_256(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.sha3_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.sha3_256(${ x }.asInstanceOf[ByteString])
        }
    }

    def blake2b_256(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.blake2b_256.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.blake2b_256(${ x }.asInstanceOf[ByteString])
        }
    }

    def unConstrData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Any] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.unConstrData.constantCost,
              Nil
            )
            RuntimeHelper.unConstrData(${ x }.asInstanceOf[Data])
        }
    }

    def unListData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[Data]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.unListData.constantCost,
              Nil
            )
            RuntimeHelper.unListData(${ x }.asInstanceOf[Data])
        }
    }

    def unIData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[BigInt] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.unIData.constantCost,
              Nil
            )
            Builtins.unIData(${ x }.asInstanceOf[Data])
        }
    }

    def unBData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.unBData.constantCost,
              Nil
            )
            Builtins.unBData(${ x }.asInstanceOf[Data])
        }
    }

    def lengthOfByteString(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[BigInt] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.lengthOfByteString.constantCost,
              Nil
            )
            Builtins.lengthOfByteString(${ x }.asInstanceOf[ByteString])
        }
    }

    def encodeUtf8(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.encodeUtf8.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.encodeUtf8(${ x }.asInstanceOf[String])
        }
    }

    def decodeUtf8(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[String] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.decodeUtf8.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.decodeUtf8(${ x }.asInstanceOf[ByteString])
        }
    }

    def headList(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Any] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.headList.constantCost,
              Nil
            )
            ${ x }.asInstanceOf[List[?]].head
        }
    }

    def tailList(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[?]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              ${ params }.builtinCostModel.tailList.constantCost,
              Nil
            )
            ${ x }.asInstanceOf[List[?]].tail
        }
    }

}
