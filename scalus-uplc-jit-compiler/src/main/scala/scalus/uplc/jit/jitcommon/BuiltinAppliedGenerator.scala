package scalus.uplc.jit.jitcommon

import cats.syntax.semigroup.*
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.uplc.{Expr as _, *}
import scalus.uplc.DefaultFun.*
import scalus.uplc.eval.*
import scalus.uplc.eval.ExBudgetCategory.{BuiltinApp, Step}

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
            case ConsByteString           => true
            case IndexByteString          => true
            case EqualsByteString         => true
            case LessThanByteString       => true
            case LessThanEqualsByteString => true
            // String operations
            case AppendString => true
            case EqualsString => true
            // Data operations
            case EqualsData => true
            case ConstrData => true
            case MkPairData => true
            // List operations
            case MkCons => true
            case _      => false

    // One-argument builtins that can be optimized
    def isSupported1(bn: DefaultFun): Boolean =
        bn match
            // Hash functions
            case Sha2_256    => true
            case Sha3_256    => true
            case Blake2b_256 => true
            // Data destructors
            case UnConstrData => true
            case UnMapData    => true
            case UnListData   => true
            case UnIData      => true
            case UnBData      => true
            // Data constructors
            case IData         => true
            case BData         => true
            case ListData      => true
            case MapData       => true
            case SerialiseData => true
            // ByteString operations
            case LengthOfByteString => true
            // String operations
            case EncodeUtf8 => true
            case DecodeUtf8 => true
            // List operations
            case HeadList      => true
            case TailList      => true
            case NullList      => true
            case MkNilData     => true
            case MkNilPairData => true
            case _             => false

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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(AddInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(SubtractInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MultiplyInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(EqualsInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(LessThanInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(LessThanEqualsInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(EqualsByteString),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(EqualsData),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(DivideInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(QuotientInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(RemainderInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(ModInteger),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(AppendByteString),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(LessThanByteString),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(LessThanEqualsByteString),
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

    def consByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[ByteString] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(ConsByteString),
              ${ params }.builtinCostModel.consByteString.calculateCostFromMemory(
                Seq(
                  MemoryUsageJit.memoryUsage($x),
                  MemoryUsageJit.memoryUsage($y)
                )
              ),
              Nil
            )
            Builtins.consByteString(
              ${ x }.asInstanceOf[BigInt],
              ${ y }.asInstanceOf[ByteString]
            )
        }
    }

    def indexByteString(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[BigInt] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(IndexByteString),
              ${ params }.builtinCostModel.indexByteString.constantCost,
              Nil
            )
            Builtins.indexByteString(
              ${ x }.asInstanceOf[ByteString],
              ${ y }.asInstanceOf[BigInt]
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(AppendString),
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
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(EqualsString),
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

    // Data constructors

    def constrData(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Data] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(ConstrData),
              ${ params }.builtinCostModel.constrData.constantCost,
              Nil
            )
            Data.Constr(
              ${ x }.asInstanceOf[BigInt].longValue,
              ${ y }.asInstanceOf[List[Data]]
            )
        }
    }

    def mkPairData(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[scalus.builtin.BuiltinPair[Data, Data]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MkPairData),
              ${ params }.builtinCostModel.mkPairData.constantCost,
              Nil
            )
            scalus.builtin.BuiltinPair(
              ${ x }.asInstanceOf[Data],
              ${ y }.asInstanceOf[Data]
            )
        }
    }

    def mkCons(
        x: Expr[Any],
        y: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[Any]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost |+| $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MkCons),
              ${ params }.builtinCostModel.mkCons.constantCost,
              Nil
            )
            ${ x } :: ${ y }.asInstanceOf[List[Any]]
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(Sha2_256),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(Sha3_256),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(Blake2b_256),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(UnConstrData),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(UnListData),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(UnIData),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(UnBData),
              ${ params }.builtinCostModel.unBData.constantCost,
              Nil
            )
            Builtins.unBData(${ x }.asInstanceOf[Data])
        }
    }

    def iData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Data] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(IData),
              ${ params }.builtinCostModel.iData.constantCost,
              Nil
            )
            Data.I(${ x }.asInstanceOf[BigInt])
        }
    }

    def bData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Data] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(BData),
              ${ params }.builtinCostModel.bData.constantCost,
              Nil
            )
            Data.B(${ x }.asInstanceOf[ByteString])
        }
    }

    def listData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Data] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(ListData),
              ${ params }.builtinCostModel.listData.constantCost,
              Nil
            )
            Data.List(${ x }.asInstanceOf[List[Data]])
        }
    }

    def mapData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Data] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MapData),
              ${ params }.builtinCostModel.mapData.constantCost,
              Nil
            )
            Data.Map(
              ${ x }
                  .asInstanceOf[List[scalus.builtin.BuiltinPair[Data, Data]]]
                  .map(p => (p.fst, p.snd))
            )
        }
    }

    def nullList(
        x: Expr[Any],
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
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(NullList),
              ${ params }.builtinCostModel.nullList.constantCost,
              Nil
            )
            ${ x }.asInstanceOf[List[Any]].isEmpty
        }
    }

    def mkNilData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[Data]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MkNilData),
              ${ params }.builtinCostModel.mkNilData.constantCost,
              Nil
            )
            Nil
        }
    }

    def mkNilPairData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[scalus.builtin.BuiltinPair[Data, Data]]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(MkNilPairData),
              ${ params }.builtinCostModel.mkNilPairData.constantCost,
              Nil
            )
            Nil
        }
    }

    def unMapData(
        x: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[List[scalus.builtin.BuiltinPair[Data, Data]]] = {
        '{
            ${ budget }.spendBudget(
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(UnMapData),
              ${ params }.builtinCostModel.unMapData.constantCost,
              Nil
            )
            RuntimeHelper.unMapData(${ x }.asInstanceOf[Data])
        }
    }

    def serialiseData(
        x: Expr[Any],
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
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(SerialiseData),
              ${ params }.builtinCostModel.serialiseData.calculateCostFromMemory(
                Seq(MemoryUsageJit.memoryUsage($x))
              ),
              Nil
            )
            Builtins.serialiseData(${ x }.asInstanceOf[Data])
        }
    }

    def lengthOfByteString(
        x: Expr[Any],
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
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(LengthOfByteString),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(EncodeUtf8),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(DecodeUtf8),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(HeadList),
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
              Step(StepKind.Apply),
              $params.machineCosts.applyCost,
              Nil
            )
            ${ budget }.spendBudget(
              Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ budget }.spendBudget(
              BuiltinApp(TailList),
              ${ params }.builtinCostModel.tailList.constantCost,
              Nil
            )
            ${ x }.asInstanceOf[List[?]].tail
        }
    }

}
