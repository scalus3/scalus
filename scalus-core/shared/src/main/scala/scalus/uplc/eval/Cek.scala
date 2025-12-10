package scalus.uplc
package eval

import cats.syntax.group.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.uplc.Term.*

import scala.annotation.{switch, tailrec}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.{immutable, mutable}
import scala.util.Try
import scala.util.control.NonFatal

enum StepKind:
    case Const, Var, LamAbs, Apply, Delay, Force, Builtin, Constr, Case

enum ExBudgetCategory:
    case Startup
    case Step(kind: StepKind)
    case BuiltinApp(bi: DefaultFun)

case class CekMachineCosts(
    startupCost: ExUnits,
    varCost: ExUnits,
    constCost: ExUnits,
    lamCost: ExUnits,
    delayCost: ExUnits,
    forceCost: ExUnits,
    applyCost: ExUnits,
    builtinCost: ExUnits,
    constrCost: ExUnits,
    caseCost: ExUnits
)

object CekMachineCosts {
    val defaultMachineCostsA: CekMachineCosts = CekMachineCosts(
      startupCost = ExUnits(100, 100),
      varCost = ExUnits(100, 23000),
      constCost = ExUnits(100, 23000),
      lamCost = ExUnits(100, 23000),
      delayCost = ExUnits(100, 23000),
      forceCost = ExUnits(100, 23000),
      applyCost = ExUnits(100, 23000),
      builtinCost = ExUnits(100, 23000),
      constrCost = ExUnits(100, 23000),
      caseCost = ExUnits(100, 23000)
    )

    val defaultMachineCostsB: CekMachineCosts = CekMachineCosts(
      startupCost = ExUnits(100, 100),
      varCost = ExUnits(100, 16000),
      constCost = ExUnits(100, 16000),
      lamCost = ExUnits(100, 16000),
      delayCost = ExUnits(100, 16000),
      forceCost = ExUnits(100, 16000),
      applyCost = ExUnits(100, 16000),
      builtinCost = ExUnits(100, 16000),
      constrCost = ExUnits(100, 16000),
      caseCost = ExUnits(100, 16000)
    )

    val defaultMachineCostsC: CekMachineCosts = defaultMachineCostsB
    val defaultMachineCosts: CekMachineCosts = defaultMachineCostsC

    def fromMap(map: Map[String, Long]): CekMachineCosts = {
        def get(key: String) = {
            val cpu = s"${key}-exBudgetCPU"
            val memory = s"${key}-exBudgetMemory"
            ExUnits(
              memory = map.getOrElse(
                memory,
                throw new IllegalArgumentException(s"Missing key: $memory in $map")
              ),
              steps = map
                  .getOrElse(cpu, throw new IllegalArgumentException(s"Missing key: $cpu in $map"))
            )
        }

        CekMachineCosts(
          startupCost = get("cekStartupCost"),
          varCost = get("cekVarCost"),
          constCost = get("cekConstCost"),
          lamCost = get("cekLamCost"),
          delayCost = get("cekDelayCost"),
          forceCost = get("cekForceCost"),
          applyCost = get("cekApplyCost"),
          builtinCost = get("cekBuiltinCost"),
          constrCost = Try(get("cekConstrCost")).getOrElse(defaultMachineCosts.constrCost),
          caseCost = Try(get("cekCaseCost")).getOrElse(defaultMachineCosts.caseCost)
        )
    }

    def fromPlutusParams(params: PlutusParams): CekMachineCosts = {
        CekMachineCosts(
          startupCost = ExUnits(
            memory = params.`cekStartupCost-exBudgetMemory`,
            steps = params.`cekStartupCost-exBudgetCPU`
          ),
          varCost = ExUnits(
            memory = params.`cekVarCost-exBudgetMemory`,
            steps = params.`cekVarCost-exBudgetCPU`
          ),
          constCost = ExUnits(
            memory = params.`cekConstCost-exBudgetMemory`,
            steps = params.`cekConstCost-exBudgetCPU`
          ),
          lamCost = ExUnits(
            memory = params.`cekLamCost-exBudgetMemory`,
            steps = params.`cekLamCost-exBudgetCPU`
          ),
          delayCost = ExUnits(
            memory = params.`cekDelayCost-exBudgetMemory`,
            steps = params.`cekDelayCost-exBudgetCPU`
          ),
          forceCost = ExUnits(
            memory = params.`cekForceCost-exBudgetMemory`,
            steps = params.`cekForceCost-exBudgetCPU`
          ),
          applyCost = ExUnits(
            memory = params.`cekApplyCost-exBudgetMemory`,
            steps = params.`cekApplyCost-exBudgetCPU`
          ),
          builtinCost = ExUnits(
            memory = params.`cekBuiltinCost-exBudgetMemory`,
            steps = params.`cekBuiltinCost-exBudgetCPU`
          ),
          constrCost = ExUnits(
            memory = params.`cekConstrCost-exBudgetMemory`,
            steps = params.`cekConstrCost-exBudgetCPU`
          ),
          caseCost = ExUnits(
            memory = params.`cekCaseCost-exBudgetMemory`,
            steps = params.`cekCaseCost-exBudgetCPU`
          )
        )
    }

}

/** The Plutus CEK machine parameters.
  *
  * @param machineCosts
  *   The machine costs
  * @param builtinCostModel
  *   The builtin cost model
  */
case class MachineParams(
    machineCosts: CekMachineCosts,
    builtinCostModel: BuiltinCostModel
)

object MachineParams {

    lazy val defaultPlutusV1PostConwayParams: MachineParams = {
        fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV1)
    }

    lazy val defaultPlutusV2PostConwayParams: MachineParams = {
        fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV2)
    }

    lazy val defaultPlutusV3Params: MachineParams = {
        fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV3)
    }

    /** Creates default machine parameters for a given Plutus version and protocol version.
      *
      * @param language
      *   The plutus version
      * @param protocolVersion
      *   The protocol version
      * @return
      *   The machine parameters
      */
    def defaultParamsFor(
        language: Language,
        protocolVersion: MajorProtocolVersion
    ): MachineParams = {
        fromCostModels(CardanoInfo.mainnet.protocolParams.costModels, language, protocolVersion)
    }

    /** Creates default machine parameters for a given Plutus version and protocol version.
      *
      * @param plutus
      *   The plutus version
      * @param protocolVersion
      *   The protocol version
      * @return
      *   The machine parameters
      */
    @deprecated("Use the overload with MajorProtocolVersion", "0.12.0")
    def defaultParamsFor(
        plutus: Language,
        protocolVersion: ProtocolVersion
    ): MachineParams = defaultParamsFor(plutus, MajorProtocolVersion(protocolVersion.major))

    /** Creates `MachineParams` from a Cardano CLI protocol parameters JSON.
      *
      * @param json
      *   The Cardano CLI protocol parameters JSON
      * @param plutus
      *   The plutus version
      * @return
      *   The machine parameters
      */
    def fromCardanoCliProtocolParamsJson(
        json: String,
        plutus: Language
    ): MachineParams = {
        val pparams = ProtocolParams.fromCardanoCliJson(json)
        fromProtocolParams(pparams, plutus)
    }

    /** Creates `MachineParams` from a Blockfrost protocol parameters JSON.
      *
      * @param json
      *   The Blockfrost protocol parameters JSON
      * @param plutus
      *   The plutus version
      * @return
      *   The machine parameters
      */
    def fromBlockfrostProtocolParamsJson(
        json: String,
        plutus: Language
    ): MachineParams = {
        val pparams = ProtocolParams.fromBlockfrostJson(json)
        fromProtocolParams(pparams, plutus)
    }

    /** Creates [[MachineParams]] from a [[ProtocolParams]] and a [[Language]]
      */
    def fromProtocolParams(pparams: ProtocolParams, language: Language): MachineParams = {
        fromCostModels(pparams.costModels, language, pparams.protocolVersion.toMajor)
    }

    /** Creates MachineParams from [[CostModels]] and [[Language]].
      *
      * This function configures the Plutus virtual machine with the appropriate cost models and
      * semantic variants based on the protocol version and Plutus language version. This is crucial
      * for accurate script execution cost calculation and validation.
      *
      * @param costModels
      *   Cost models for different Plutus versions
      * @param language
      *   Plutus language version (V1, V2, or V3)
      * @param protocolVersion
      *   Major protocol version for semantic variant selection
      * @return
      *   Configured MachineParams for script execution
      */
    def fromCostModels(
        costModels: CostModels,
        language: Language,
        protocolVersion: MajorProtocolVersion
    ): MachineParams = {
        val params = language match
            case Language.PlutusV1 =>
                val costs = costModels.models(language.ordinal)
                PlutusV1Params.fromSeq(costs)
            case Language.PlutusV2 =>
                val costs = costModels.models(language.ordinal)
                PlutusV2Params.fromSeq(costs)
            case Language.PlutusV3 =>
                val costs = costModels.models(language.ordinal)
                PlutusV3Params.fromSeq(costs)
            case Language.PlutusV4 =>
                // Use V3 cost models for V4 until V4 cost models are available on-chain
                val costs = costModels.models.getOrElse(
                  language.ordinal,
                  costModels.models(Language.PlutusV3.ordinal)
                )
                PlutusV4Params.fromSeq(costs)

        val semvar = BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
          protocolVersion,
          language
        )
        val builtinCostModel = BuiltinCostModel.fromPlutusParams(params, language, semvar)
        val machineCosts = CekMachineCosts.fromPlutusParams(params)
        MachineParams(
          machineCosts = machineCosts,
          builtinCostModel = builtinCostModel,
        )
    }

}

class MachineError(msg: String) extends RuntimeException(msg)

class StackTraceMachineError(msg: String, val env: CekValEnv) extends MachineError(msg) {
    def getCekStack: Array[String] = env.view.reverse.map(_._1).toArray
}

class NonPolymorphicInstantiationMachineError(value: CekValue, env: CekValEnv)
    extends StackTraceMachineError(s"Non-polymorphic instantiation: $value", env)

class NonFunctionalApplicationMachineError(arg: CekValue, env: CekValEnv)
    extends StackTraceMachineError(s"Non-functional application: $arg", env)

class OpenTermEvaluatedMachineError(name: NamedDeBruijn, env: CekValEnv)
    extends StackTraceMachineError(
      s"Variable ${name.name}@${name.index} not found in environment: ${env.reverse.map(_._1).mkString(", ")}",
      env
    )

class BuiltinTermArgumentExpectedMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Expected builtin term argument, got $term", env)

class UnexpectedBuiltinTermArgumentMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Unexpected builtin term argument: $term", env)

class UnknownBuiltin(builtin: DefaultFun, env: CekValEnv)
    extends StackTraceMachineError(s"Unknown builtin: $builtin", env)

class EvaluationFailure(env: CekValEnv) extends StackTraceMachineError("Error evaluated", env)

class MissingCaseBranch(val tag: Word64, env: CekValEnv)
    extends StackTraceMachineError(
      s"Case expression missing the branch required by the scrutinee tag: $tag",
      env
    )
class NonConstrScrutinized(val value: CekValue, env: CekValEnv)
    extends StackTraceMachineError(
      s"A non-constructor value was scrutinized in a case expression: $value",
      env
    )

/** Base class for case-on-builtin errors (PlutusV4+) */
abstract class CaseOnBuiltinError(msg: String, env: CekValEnv)
    extends StackTraceMachineError(msg, env)

class CaseIndexOutOfBounds(val index: BigInt, val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case index $index out of bounds for $branchCount branches",
      env
    )

class CaseBoolBranchMissing(val value: Boolean, val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case on boolean $value requires ${
              if value then 2 else 1
          } branches, but only $branchCount provided",
      env
    )

class CaseUnitBranchMissing(val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case on unit requires exactly 1 branch, but $branchCount provided",
      env
    )

class CaseListBranchError(val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case on list requires 1 or 2 branches (cons, nil), but $branchCount provided",
      env
    )

class CaseDataBranchError(val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case on data requires 1 to 5 branches (Constr, Map, List, I, B), but $branchCount provided",
      env
    )

class CasePairBranchError(val branchCount: Int, env: CekValEnv)
    extends CaseOnBuiltinError(
      s"Case on pair requires exactly 1 branch, but $branchCount provided",
      env
    )

class InvalidReturnValue(val value: Term)
    extends RuntimeException(s"Invalid return value: expected Unit, got $value")

class BuiltinException(msg: String) extends MachineError(msg)

class DeserializationError(fun: DefaultFun, value: CekValue)
    extends BuiltinException(s"Deserialization error in $fun: $value")

class KnownTypeUnliftingError(expected: DefaultUni, actual: CekValue)
    extends BuiltinException(s"Expected type $expected, got $actual")

class BuiltinError(
    val builtin: DefaultFun,
    val term: Term,
    val cause: Throwable,
    env: CekValEnv
) extends StackTraceMachineError(s"Builtin error: $builtin $term, caused by $cause", env)

class OutOfExBudgetError(budget: ExUnits, env: CekValEnv)
    extends StackTraceMachineError(s"Out of budget: $budget", env)

type CekValEnv = immutable.Seq[(String, CekValue)]

// 'Values' for the modified CEK machine.
enum CekValue {
    case VCon(const: Constant)
    case VDelay(term: Term, env: CekValEnv)
    case VLamAbs(name: String, term: Term, env: CekValEnv)
    case VBuiltin(bn: DefaultFun, term: () => Term, runtime: BuiltinRuntime)
    case VConstr(tag: Word64, args: Seq[CekValue])

    override def toString: String = this match
        case VCon(const)            => s"VCon($const)"
        case VDelay(term, _)        => s"VDelay($term)"
        case VLamAbs(name, term, _) => s"VLamAbs($name, $term)"
        case VBuiltin(bn, _, _)     => s"VBuiltin($bn)"
        case VConstr(tag, args)     => s"VConstr($tag, ${args.mkString(", ")})"

    def asUnit: Unit = this match {
        case VCon(Constant.Unit) => ()
        case _                   => throw new KnownTypeUnliftingError(DefaultUni.Unit, this)
    }
    def asInteger: BigInt = this match {
        case VCon(Constant.Integer(i)) => i
        case _ => throw new KnownTypeUnliftingError(DefaultUni.Integer, this)
    }
    def asString: String = this match {
        case VCon(Constant.String(s)) => s
        case _                        => throw new KnownTypeUnliftingError(DefaultUni.String, this)
    }
    def asBool: Boolean = this match {
        case VCon(Constant.Bool(b)) => b
        case _                      => throw new KnownTypeUnliftingError(DefaultUni.Bool, this)
    }
    def asByteString: ByteString = this match {
        case VCon(Constant.ByteString(bs)) => bs
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ByteString, this)
    }
    def asData: Data = this match {
        case VCon(Constant.Data(d)) => d
        case _                      => throw new KnownTypeUnliftingError(DefaultUni.Data, this)
    }
    def asList: List[Constant] = this match {
        case VCon(Constant.List(_, l)) => l
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoList, this)
    }
    def asPair: (Constant, Constant) = this match {
        case VCon(Constant.Pair(l, r)) => (l, r)
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoPair, this)
    }
}

enum Result:
    val budget: ExUnits
    val logs: Seq[String]
    val costs: collection.Map[ExBudgetCategory, collection.Seq[ExUnits]]
    case Success(
        term: Term,
        budget: ExUnits,
        costs: Map[ExBudgetCategory, collection.Seq[ExUnits]],
        logs: Seq[String]
    )
    case Failure(
        exception: Throwable,
        budget: ExUnits,
        costs: Map[ExBudgetCategory, collection.Seq[ExUnits]],
        logs: Seq[String]
    )

    /** Compare this `Result` with `that` for semantic equality.
      *
      * For `Success` results this uses `Term.alphaEq` to compare the produced terms (so
      * alpha-equivalent terms are considered equal), and also requires budgets, costs and logs to
      * be equal.
      *
      * For `Failure` results this compares the runtime exception classes and messages (stack traces
      * are intentionally ignored), and also requires budgets, costs and logs to be equal.
      *
      * Returns `true` only when both sides are the same variant (`Success` or `Failure`) and all
      * compared components match as described above.
      *
      * @param that
      *   the other Result to compare against
      * @return
      *   true if results are semantically equal (alpha-equivalence for terms)
      */
    infix def alphaEq(that: Result): Boolean = (this, that) match
        case (Success(term1, budget1, costs1, logs1), Success(term2, budget2, costs2, logs2)) =>
            (term1 α_== term2) && budget1 == budget2 && costs1 == costs2 && logs1 == logs2
        case (Failure(ex1, budget1, costs1, logs1), Failure(ex2, budget2, costs2, logs2)) =>
            ex1.getClass == ex2.getClass &&
            Option(ex1.getMessage) == Option(ex2.getMessage) &&
            budget1 == budget2 && costs1 == costs2 && logs1 == logs2
        case _ => false

    def isSuccess: Boolean = this match
        case _: Success => true
        case _          => false

    def isFailure: Boolean = this match
        case _: Failure => true
        case _          => false

    /** Returns the `Success` result if this is a `Success`, otherwise throws
      * `NoSuchElementException`.
      */
    def success: Success = this match
        case s: Success => s
        case _          => throw new NoSuchElementException("Not a Success result")

    override def toString: String =
        import scalus.*

        val prices = CardanoInfo.mainnet.protocolParams.executionUnitPrices

        def sumBudget(budgets: collection.Seq[ExUnits]): ExUnits =
            budgets.foldLeft(ExUnits.zero)(_ |+| _)

        def showCosts =
            "kind, count, mem, cpu, fee\r\n" +
                costs.toArray.view
                    .map: (k, v) =>
                        val budgetSum = sumBudget(v)
                        (k, v.length, budgetSum, budgetSum.fee(prices))
                    .sortBy(_._4.value)(using Ordering[Long].reverse)
                    .map:
                        case (ExBudgetCategory.Startup, length, v, fee) =>
                            s"Startup, ${length}, ${v.memory}, ${v.steps}, ${fee.value}"
                        case (ExBudgetCategory.Step(kind), length, v, fee) =>
                            s"$kind, ${length}, ${v.memory}, ${v.steps}, ${fee.value}"
                        case (ExBudgetCategory.BuiltinApp(bn), length, v, fee) =>
                            s"$bn, ${length}, ${v.memory}, ${v.steps}, ${fee.value}"
                    .mkString("\r\n")

        this match
            case Success(term, budget, costs, logs) =>
                s"""Success executing script:
              | term: ${term.show}
              | budget: ${budget.showJson}
              | fee: ${budget.fee(prices).value} lovelace
              | costs:\n${showCosts}
              | logs: ${logs.mkString("\n")}""".stripMargin
            case Failure(exception, budget, costs, logs) =>
                s"""Failure executing script:
              | exception: ${exception.getMessage}
              | budget: ${budget.showJson}
              | costs:\n${showCosts}
              | logs: ${logs.mkString("\n")}""".stripMargin

type ArgStack = Seq[CekValue]

private enum Context {
    case FrameAwaitArg(f: CekValue, ctx: Context)
    case FrameAwaitFunTerm(env: CekValEnv, term: Term, ctx: Context)
    case FrameAwaitFunValue(value: CekValue, ctx: Context)
    case FrameForce(ctx: Context)
    case FrameConstr(env: CekValEnv, tag: Word64, rest: Seq[Term], args: ArgStack, ctx: Context)
    case FrameCases(env: CekValEnv, cases: Seq[Term], ctx: Context)
    case NoFrame
}

//private enum CekState {
//    case Return(ctx: Context, env: CekValEnv, value: CekValue)
//    case Compute(ctx: Context, env: CekValEnv, term: Term)
//    case Done(term: Term)
//}
private type CekState = Int

trait Logger {
    def log(msg: String): Unit
    def getLogs: Array[String]
}

object NoLogger extends Logger {
    def log(msg: String): Unit = ()
    val getLogs: Array[String] = Array.empty[String]
}

class Log extends Logger {
    private val logs: ArrayBuffer[String] = ArrayBuffer.empty[String]
    def getLogs: Array[String] = logs.toArray
    def log(msg: String): Unit = logs.append(msg)
    def clear(): Unit = logs.clear()
}

trait BudgetSpender {
    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit
    def getSpentBudget: ExUnits
}

object NoBudgetSpender extends BudgetSpender {
    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = ()
    def getSpentBudget: ExUnits = ExUnits.zero
}

class RestrictingBudgetSpender(val maxBudget: ExUnits) extends BudgetSpender {
    private var cpuLeft: Long = maxBudget.steps
    private var memoryLeft: Long = maxBudget.memory

    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        cpuLeft -= budget.steps
        memoryLeft -= budget.memory
        if cpuLeft < 0 || memoryLeft < 0 then throw new OutOfExBudgetError(maxBudget, env)
    }

    def getSpentBudget: ExUnits =
        ExUnits(maxBudget.memory - memoryLeft, maxBudget.steps - cpuLeft)

    def reset(): Unit = {
        cpuLeft = maxBudget.steps
        memoryLeft = maxBudget.memory
    }
}

final class TallyingBudgetSpender(val budgetSpender: BudgetSpender) extends BudgetSpender {
    val costs: HashMap[ExBudgetCategory, ExUnits] = HashMap.empty

    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
        costs.updateWith(cat) {
            case Some(b) => Some(b |+| budget)
            case None    => Some(budget)
        }
    }

    def getSpentBudget: ExUnits = budgetSpender.getSpentBudget
}

final class TallyingBudgetSpenderLogger(val budgetSpender: BudgetSpender)
    extends BudgetSpender
    with Logger {
    val costs: collection.mutable.Map[ExBudgetCategory, mutable.Buffer[ExUnits]] =
        mutable
            .HashMap[ExBudgetCategory, mutable.Buffer[ExUnits]]()
            .withDefault(_ => ArrayBuffer.empty[ExUnits])

    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
        costs.update(cat, costs(cat) += budget)
    }

    def getSpentBudget: ExUnits = budgetSpender.getSpentBudget

    private val _logs: ArrayBuffer[(String, ExUnits)] = ArrayBuffer.empty
    val logs: collection.IndexedSeq[(String, ExUnits)] = _logs
    def getLogs: Array[String] = _logs.map(_._1).toArray
    def getLogsWithBudget: Seq[String] =
        _logs.map((log, budget) => s"$log: ${budget.showJson}").toSeq
    def log(msg: String): Unit = _logs.append((msg, getSpentBudget))
}

final class CountingBudgetSpender extends BudgetSpender {
    private var cpu: CostingInteger = CostingInteger(0L)
    private var memory: CostingInteger = CostingInteger(0L)

    def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        cpu = cpu + CostingInteger(budget.steps)
        memory = memory + CostingInteger(budget.memory)
    }

    def getSpentBudget: ExUnits = ExUnits(memory.toLong, cpu.toLong)
}

/** CEK machine implementation based on Cardano Plutus CEK machine.
  *
  * The CEK machine is a stack-based abstract machine that is used to evaluate UPLC terms.
  *
  * @note
  *   The machine is stateless and can be reused for multiple evaluations. All the state is expected
  *   to be in the `budgetSpender` and `logger` implementations.
  *
  * @param params
  *   The machine parameters [[MachineParams]]
  * @param budgetSpender
  *   The budget spender implementation
  * @param logger
  *   The logger implementation
  * @param platformSpecific
  *   The platform specific implementation of certain functions used by builtins
  * @see
  *   https://github.com/input-output-hk/plutus/blob/41a7afebc4cee277bab702ee1678c070e5e38810/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek/Internal.hs
  * @example
  *   {{{
  *   val term = LamAbs("x", Apply(Var(NamedDeBruijn("x", 0)), Var(NamedDeBruijn("x", 0))))
  *   val cek = new CekMachine(MachineParams.defaultParams, NoBudgetSpender, NoLogger, JVMPlatformSpecific)
  *   val res = cek.evaluateTerm(term)
  *   }}}
  */
class CekMachine(
    val params: MachineParams,
    budgetSpender: BudgetSpender,
    logger: Logger,
    getBuiltinRuntime: DefaultFun => BuiltinRuntime,
    caseOnBuiltinsEnabled: Boolean = false
) {
    import CekValue.*
    import Context.*

    private var ctx: Context = NoFrame
    private var env: CekValEnv = ArraySeq.empty
    private var value: CekValue | Null = null
    private var term: Term | Null = null

    /** Evaluates a UPLC term.
      *
      * @param term
      *   The debruijned term to evaluate
      * @return
      *   The resulting term
      * @throws StackTraceMachineError
      */
    def evaluateTerm(term: Term): Term = {
        @tailrec
        def loop(state: CekState): Term = {
            (state: @switch) match
                case 0 => loop(computeCek(ctx, env, this.term))
                case 1 => loop(returnCek(ctx, env, value))
                case 2 => this.term
        }

        spendBudget(ExBudgetCategory.Startup, params.machineCosts.startupCost, ArraySeq.empty)
        loop(Compute(NoFrame, ArraySeq.empty, term))
    }

    private inline def Compute(ctx: Context, env: CekValEnv, term: Term): Int = {
        this.ctx = ctx
        this.env = env
        this.term = term
        0
    }
    private inline def Return(ctx: Context, env: CekValEnv, value: CekValue): Int = {
        this.ctx = ctx
        this.env = env
        this.value = value
        1
    }
    private inline def Done(term: Term): Int = {
        this.term = term
        2
    }

    private final def computeCek(ctx: Context, env: CekValEnv, term: Term): CekState = {
        val costs = params.machineCosts
        term match
            case Var(name) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Var), costs.varCost, env)
                Return(ctx, env, lookupVarName(env, name))
            case LamAbs(name, term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.LamAbs), costs.lamCost, env)
                Return(ctx, env, VLamAbs(name, term, env))
            case Apply(fun, arg) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Apply), costs.applyCost, env)
                Compute(FrameAwaitFunTerm(env, arg, ctx), env, fun)
            case Force(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Force), costs.forceCost, env)
                Compute(FrameForce(ctx), env, term)
            case Delay(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Delay), costs.delayCost, env)
                Return(ctx, env, VDelay(term, env))
            case Const(const) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Const), costs.constCost, env)
                Return(ctx, env, VCon(const))
            case Builtin(bn) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Builtin), costs.builtinCost, env)
                // The @term@ is a 'Builtin', so it's fully discharged.
                try
                    val meaning = getBuiltinRuntime(bn)
                    Return(ctx, env, VBuiltin(bn, () => term, meaning))
                catch case _: Exception => throw new UnknownBuiltin(bn, env)
            case Error => throw new EvaluationFailure(env)
            case Constr(tag, args) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Constr), costs.constrCost, env)
                args match
                    case arg :: rest =>
                        Compute(FrameConstr(env, tag, rest, ArraySeq.empty, ctx), env, arg)
                    case Nil => Return(ctx, env, VConstr(tag, Nil))
            case Case(scrut, cases) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Case), costs.caseCost, env)
                Compute(FrameCases(env, cases, ctx), env, scrut)
    }

    private def returnCek(ctx: Context, env: CekValEnv, value: CekValue): CekState = {
        ctx match
            case NoFrame                          => Done(dischargeCekValue(value))
            case FrameForce(ctx)                  => forceEvaluate(ctx, env, value)
            case FrameAwaitFunTerm(env, arg, ctx) => Compute(FrameAwaitArg(value, ctx), env, arg)
            case FrameAwaitArg(fun, ctx)          => applyEvaluate(ctx, env, fun, value)
            case FrameAwaitFunValue(arg, ctx)     => applyEvaluate(ctx, env, value, arg)
            case FrameConstr(env, tag, todo, evaled, ctx) =>
                val newEvaled = evaled :+ value
                todo match
                    case arg :: rest =>
                        Compute(FrameConstr(env, tag, rest, newEvaled, ctx), env, arg)
                    case Nil => Return(ctx, env, VConstr(tag, newEvaled))
            case FrameCases(env, cases, ctx) =>
                value match
                    case VConstr(tag, args) =>
                        require(tag.value < Int.MaxValue, s"Constructor tag too large: $tag")
                        val index = tag.value.toInt
                        if index < cases.size then
                            Compute(transferArgStack(args, ctx), env, cases(index))
                        else throw new MissingCaseBranch(tag, env)
                    case VCon(const) if caseOnBuiltinsEnabled =>
                        const match
                            case Constant.Integer(i) =>
                                if i >= 0 && i < cases.size then Compute(ctx, env, cases(i.toInt))
                                else throw new CaseIndexOutOfBounds(i, cases.size, env)
                            case Constant.Bool(b) =>
                                // Bool has exactly 2 constructors (False=0, True=1)
                                // Valid: 1 or 2 branches. Invalid: 0 or 3+ branches
                                if cases.size == 0 || cases.size > 2 then
                                    throw new CaseBoolBranchMissing(b, cases.size, env)
                                val index = if b then 1 else 0
                                if index < cases.size then Compute(ctx, env, cases(index))
                                else throw new CaseBoolBranchMissing(b, cases.size, env)
                            case Constant.Unit =>
                                // Unit has exactly 1 constructor (Unit=0)
                                // Valid: exactly 1 branch
                                if cases.size == 1 then Compute(ctx, env, cases(0))
                                else throw new CaseUnitBranchMissing(cases.size, env)
                            case list @ Constant.List(elemType, elements) =>
                                // List has 2 constructors: Cons=0 (head, tail), Nil=1
                                // Valid: 1 or 2 branches. Invalid: 0 or 3+ branches
                                if cases.size == 0 || cases.size > 2 then
                                    throw new CaseListBranchError(cases.size, env)
                                elements match
                                    case head :: tail =>
                                        // Non-empty list -> Cons branch (index 0)
                                        // Apply head and tail as arguments to the cons branch
                                        // Order: head first, then tail (like transferArgStack foldRight)
                                        val headVal = VCon(head)
                                        val tailVal = VCon(Constant.List(elemType, tail))
                                        val newCtx = FrameAwaitFunValue(
                                          headVal,
                                          FrameAwaitFunValue(tailVal, ctx)
                                        )
                                        Compute(newCtx, env, cases(0))
                                    case Nil =>
                                        // Empty list -> Nil branch (index 1), no arguments
                                        if cases.size < 2 then
                                            throw new CaseListBranchError(cases.size, env)
                                        Compute(ctx, env, cases(1))
                            case Constant.Data(data) =>
                                // Data has 5 constructors:
                                // Constr=0 (tag, args), Map=1, List=2, I=3, B=4
                                // Each branch receives the inner value(s) as arguments
                                if cases.size == 0 || cases.size > 5 then
                                    throw new CaseDataBranchError(cases.size, env)
                                data match
                                    case Data.Constr(tag, args) =>
                                        // Constr branch (index 0) receives tag (as Integer) and args (as list of Data)
                                        if cases.size < 1 then
                                            throw new CaseDataBranchError(cases.size, env)
                                        val tagVal = VCon(Constant.Integer(tag))
                                        val argsVal = VCon(
                                          Constant.List(DefaultUni.Data, args.map(Constant.Data.apply))
                                        )
                                        val newCtx = FrameAwaitFunValue(
                                          tagVal,
                                          FrameAwaitFunValue(argsVal, ctx)
                                        )
                                        Compute(newCtx, env, cases(0))
                                    case Data.Map(entries) =>
                                        // Map branch (index 1) receives entries as list of pairs
                                        if cases.size < 2 then
                                            throw new CaseDataBranchError(cases.size, env)
                                        val entriesVal = VCon(
                                          Constant.List(
                                            DefaultUni.Apply(
                                              DefaultUni.Apply(
                                                DefaultUni.ProtoPair,
                                                DefaultUni.Data
                                              ),
                                              DefaultUni.Data
                                            ),
                                            entries.map { case (k, v) =>
                                                Constant.Pair(Constant.Data(k), Constant.Data(v))
                                            }
                                          )
                                        )
                                        val newCtx = FrameAwaitFunValue(entriesVal, ctx)
                                        Compute(newCtx, env, cases(1))
                                    case Data.List(elements) =>
                                        // List branch (index 2) receives elements as list of Data
                                        if cases.size < 3 then
                                            throw new CaseDataBranchError(cases.size, env)
                                        val elementsVal = VCon(
                                          Constant.List(
                                            DefaultUni.Data,
                                            elements.map(Constant.Data.apply)
                                          )
                                        )
                                        val newCtx = FrameAwaitFunValue(elementsVal, ctx)
                                        Compute(newCtx, env, cases(2))
                                    case Data.I(integer) =>
                                        // I branch (index 3) receives the integer value
                                        if cases.size < 4 then
                                            throw new CaseDataBranchError(cases.size, env)
                                        val intVal = VCon(Constant.Integer(integer))
                                        val newCtx = FrameAwaitFunValue(intVal, ctx)
                                        Compute(newCtx, env, cases(3))
                                    case Data.B(bs) =>
                                        // B branch (index 4) receives the bytestring value
                                        if cases.size < 5 then
                                            throw new CaseDataBranchError(cases.size, env)
                                        val bsVal = VCon(Constant.ByteString(bs))
                                        val newCtx = FrameAwaitFunValue(bsVal, ctx)
                                        Compute(newCtx, env, cases(4))
                            case Constant.Pair(left, right) =>
                                // Pair has exactly 1 constructor that receives both elements
                                if cases.size != 1 then
                                    throw new CasePairBranchError(cases.size, env)
                                val leftVal = VCon(left)
                                val rightVal = VCon(right)
                                val newCtx = FrameAwaitFunValue(
                                  leftVal,
                                  FrameAwaitFunValue(rightVal, ctx)
                                )
                                Compute(newCtx, env, cases(0))
                            case _ => throw new NonConstrScrutinized(value, env)
                    case _ => throw new NonConstrScrutinized(value, env)
    }

    private def transferArgStack(args: ArgStack, ctx: Context): Context = {
        args.foldRight(ctx) { (arg, c) => FrameAwaitFunValue(arg, c) }
    }

    private def lookupVarName(env: CekValEnv, name: NamedDeBruijn): CekValue = {
        if name.index == 0 then
            throw MachineError(
              s"Got a de Bruijn index 0: $name, it should be > 0. Run `DeBruijn.deBruijnTerm(term)` first."
            )
        if name.index <= 0 || env.size < name.index then
            throw new OpenTermEvaluatedMachineError(name, env)
        else env(env.size - name.index)._2
    }

    private def applyEvaluate(
        ctx: Context,
        env: CekValEnv,
        fun: CekValue,
        arg: CekValue
    ): CekState = {
        fun match
            case VLamAbs(name, term, env) => Compute(ctx, env :+ (name, arg), term)
            case VBuiltin(fun, term, runtime) =>
                val term1 = () => Apply(term(), dischargeCekValue(arg))
                runtime.typeScheme match
                    case TypeScheme.Arrow(_, rest) =>
                        val runtime1 = runtime.copy(args = runtime.args :+ arg, typeScheme = rest)
                        val res = evalBuiltinApp(env, fun, term1, runtime1)
                        Return(ctx, env, res)
                    case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1(), env)
            case _ =>
                throw new NonFunctionalApplicationMachineError(fun, env)
    }

    /** `force` a term and proceed.
      *
      * If `value` is a delay then compute the body of `value`; if v is a builtin application then
      * check that it's expecting a type argument, and either calculate the builtin application or
      * stick a 'Force' on top of its 'Term' representation depending on whether the application is
      * saturated or not, if v is anything else, fail.
      */
    private def forceEvaluate(ctx: Context, env: CekValEnv, value: CekValue): CekState = {
        value match
            case VDelay(term, env) => Compute(ctx, env, term)
            case VBuiltin(bn, term, rt) =>
                val term1 = () => Force(term())
                rt.typeScheme match
                    // It's only possible to force a builtin application if the builtin expects a type
                    // argument next.
                    case TypeScheme.All(_, t) =>
                        val runtime1 = rt.copy(typeScheme = t)
                        // We allow a type argument to appear last in the type of a built-in function,
                        // otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
                        // application.
                        val res = evalBuiltinApp(env, bn, term1, runtime1)
                        Return(ctx, env, res)
                    case _ => throw new BuiltinTermArgumentExpectedMachineError(term1(), env)
            case _ =>
                throw new NonPolymorphicInstantiationMachineError(value, env)
    }

    private def evalBuiltinApp(
        env: CekValEnv,
        builtinName: DefaultFun,
        term: () => Term, // lazily discharge the term as it might not be needed
        runtime: BuiltinRuntime
    ): CekValue = {
        runtime.typeScheme match
            case TypeScheme.Type(_) | TypeScheme.TVar(_) | TypeScheme.App(_, _) =>
                spendBudget(ExBudgetCategory.BuiltinApp(builtinName), runtime.calculateCost, env)
                // eval the builtin and return result
                try {
                    // eval builtin when it's fully saturated, i.e. when all arguments were applied
                    runtime.apply(logger)
                } catch case NonFatal(e) => throw new BuiltinError(builtinName, term(), e, env)
            case _ => VBuiltin(builtinName, term, runtime)
    }

    /** Converts a 'CekValue' into a 'Term' by replacing all bound variables with the terms they're
      * bound to (which themselves have to be obtain by recursively discharging values).
      */
    private def dischargeCekValue(value: CekValue): Term = {
        def dischargeCekValEnv(env: CekValEnv, term: Term): Term = {
            def go(lamCnt: Int, term: Term): Term = {
                term match
                    case Var(name) =>
                        if lamCnt >= name.index
                        // the index n is less-than-or-equal than the number of lambdas we have descended
                        // this means that n points to a bound variable, so we don't discharge it.
                        then term
                        else
                            // index relative to (as seen from the point of view of) the environment
                            val relativeIdx = env.size - (name.index - lamCnt)
                            if env.isDefinedAt(relativeIdx) then
                                // var is in the env, discharge its value
                                dischargeCekValue(env(relativeIdx)._2)
                            else
                                // var is free, leave it alone
                                term

                    case LamAbs(name, body) => LamAbs(name, go(lamCnt + 1, body))
                    case Apply(fun, arg)    => Apply(go(lamCnt, fun), go(lamCnt, arg))
                    case Force(term)        => Force(go(lamCnt, term))
                    case Delay(term)        => Delay(go(lamCnt, term))
                    case _                  => term
            }

            go(0, term)
        }

        value match
            case VCon(const)       => Const(const)
            case VDelay(term, env) => dischargeCekValEnv(env, Delay(term))
            // `computeCek` turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
            // argument of 'computeCek' and hence we need to start discharging outside of the reassembled
            // lambda, otherwise @name@ could clash with the names that we have in @env@.
            case VLamAbs(name, term, env) => dischargeCekValEnv(env, LamAbs(name, term))
            case VBuiltin(_, term, _)     => term()
            case VConstr(tag, args)       => Constr(tag, args.map(dischargeCekValue).toList)
    }

    private def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
    }
}
