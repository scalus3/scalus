package scalus.uplc.jit

import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.DefaultFun.*
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.eval.*
import scalus.uplc.jit.JitRuntime.toCekValue
import scalus.uplc.{Constant, DefaultFun, DefaultUni, Term}

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** Hand-written builtins of the JIT: all 15 polymorphic builtins and the overrides
  * ([[BuiltinEmitter.Overrides]]), hot monomorphic builtins written by hand. Every other builtin
  * runs through [[CekBridge]].
  *
  * Each method is a copy of the `CardanoBuiltins` runtime of the same name in `Builtin.scala`, of
  * builtin semantics variant E: it matches the same cases, throws the same errors, and spends what
  * the CEK costing function computes, before the builtin runs, as `Cek.scala` `evalBuiltinApp`
  * does. So a wrong argument is costed first and fails after, as in the CEK machine. An argument is
  * a [[UplcValue]]: a constant is the `Constant` the CEK machine holds in `VCon`, and anything else
  * has memory usage 1, as `MemoryUsage.memoryUsage` gives a `CekValue` that is not a `VCon`.
  *
  * The bridge cannot run a polymorphic builtin: an argument can be a function, which has no
  * `CekValue` form, and its result can be one of its arguments. An override is allowed only for a
  * builtin whose variant-E costing function is a plain `DefaultCostingFun` or `ConstCostingFun`
  * (spec [BI-5]), which `BuiltinImplsTest` checks: a costing wrapper would have to be copied too.
  *
  * One method per builtin, named as the `DefaultFun` with a lower-case first letter (`HeadList` ->
  * `headList`, see [[BuiltinEmitter.implName]]). Each takes the logger, the budget spender and the
  * machine parameters of the evaluation, then one [[UplcValue]] per term argument, and returns a
  * [[UplcValue]]. Generated code calls them directly:
  * {{{
  * BuiltinImpls.headList(logger, budget, params, v1)
  * }}}
  * A misspelled name is not a compile error; `BuiltinImplsTest` guards the names.
  */
object BuiltinImpls {

    /** Memory usage of an argument, as `MemoryUsage.memoryUsage(CekValue)`: a constant by its
      * content, anything else 1.
      */
    private def mem(v: UplcValue): CostingInteger = v match
        case c: Constant => MemoryUsage.memoryUsage(c)
        case _           => CostingInteger(1L)

    /** Runs the body of builtin `fun`, after its cost is spent, as `Cek.scala` `evalBuiltinApp`
      * runs `runtime.f`: any failure becomes the CEK machine's `BuiltinError`, with the failure as
      * its cause. The cost is spent outside, so a budget overrun or a costing failure (`dropList`
      * on a non-integer count) is not wrapped, as in the CEK machine. The term of the error is the
      * bare builtin: the JIT does not keep the applied term the CEK machine discharges.
      */
    private inline def run(fun: DefaultFun)(inline body: UplcValue): UplcValue =
        try body
        catch case NonFatal(e) => throw new BuiltinError(fun, Term.Builtin(fun), e, Nil)

    private def constCost(b: BudgetSpender, fun: DefaultFun, cf: ConstCostingFun): Unit =
        JitBudget.spendBuiltinApp(b, fun, cf.constantCost)

    private def cost1(
        b: BudgetSpender,
        fun: DefaultFun,
        cf: DefaultCostingFun[OneArgument],
        x: UplcValue
    ): Unit = {
        val mx = mem(x)
        JitBudget.spendBuiltinApp(b, fun, ExUnits(cf.memory(mx).toLong, cf.cpu(mx).toLong))
    }

    private def cost2(
        b: BudgetSpender,
        fun: DefaultFun,
        cf: DefaultCostingFun[TwoArguments],
        x: UplcValue,
        y: UplcValue
    ): Unit = {
        val mx = mem(x)
        val my = mem(y)
        JitBudget.spendBuiltinApp(b, fun, ExUnits(cf.memory(mx, my).toLong, cf.cpu(mx, my).toLong))
    }

    private val True = Constant.Bool(true)
    private val False = Constant.Bool(false)
    private def bool(b: Boolean): Constant = if b then True else False

    /** `CekValue.asInteger` etc.: the content of a constant of the expected type, or the CEK
      * unlifting error.
      */
    private def asInteger(v: UplcValue): BigInt = v match
        case Constant.Integer(i) => i
        case _ => throw new KnownTypeUnliftingError(DefaultUni.Integer, toCekValue(v))

    private def asBool(v: UplcValue): Boolean = v match
        case Constant.Bool(b) => b
        case _                => throw new KnownTypeUnliftingError(DefaultUni.Bool, toCekValue(v))

    private def asString(v: UplcValue): String = v match
        case Constant.String(s) => s
        case _ => throw new KnownTypeUnliftingError(DefaultUni.String, toCekValue(v))

    private def asData(v: UplcValue): Data = v match
        case Constant.Data(d) => d
        case _                => throw new KnownTypeUnliftingError(DefaultUni.Data, toCekValue(v))

    private def asList(v: UplcValue): List[Constant] = v match
        case Constant.List(_, l) => l
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoList, toCekValue(v))

    private def asPair(v: UplcValue): (Constant, Constant) = v match
        case Constant.Pair(a, b) => (a, b)
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoPair, toCekValue(v))

    /** `CardanoBuiltins.dataListConstant`: the fields of a `Data` as a list constant, in one pass.
      */
    private def dataListConstant(values: PList[Data]): Constant.List = {
        val elems = ListBuffer.empty[Constant]
        values.foreach(d => elems += Constant.Data(d))
        Constant.List(DefaultUni.Data, elems.toList)
    }

    // Overrides

    def unConstrData(l: Logger, b: BudgetSpender, p: MachineParams, d: UplcValue): UplcValue = {
        constCost(b, UnConstrData, p.builtinCostModel.unConstrData)
        run(UnConstrData) {
            d match
                case Constant.Data(Data.Constr(i, ls)) =>
                    Constant.Pair(Constant.Integer(i), dataListConstant(ls))
                case _ => throw new DeserializationError(UnConstrData, toCekValue(d))
        }
    }

    def unBData(l: Logger, b: BudgetSpender, p: MachineParams, d: UplcValue): UplcValue = {
        constCost(b, UnBData, p.builtinCostModel.unBData)
        run(UnBData) {
            d match
                case Constant.Data(Data.B(bs)) => Constant.ByteString(bs)
                case _ => throw new DeserializationError(UnBData, toCekValue(d))
        }
    }

    // Polymorphic builtins

    def ifThenElse(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        c: UplcValue,
        t: UplcValue,
        f: UplcValue
    ): UplcValue = {
        constCost(b, IfThenElse, p.builtinCostModel.ifThenElse)
        run(IfThenElse) {
            if asBool(c) then t else f
        }
    }

    def chooseUnit(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        u: UplcValue,
        a: UplcValue
    ): UplcValue = {
        constCost(b, ChooseUnit, p.builtinCostModel.chooseUnit)
        run(ChooseUnit) {
            u match
                case Constant.Unit => a
                case _             => throw new DeserializationError(ChooseUnit, toCekValue(u))
        }
    }

    def trace(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        s: UplcValue,
        a: UplcValue
    ): UplcValue = {
        constCost(b, Trace, p.builtinCostModel.trace)
        run(Trace) {
            l.log(asString(s))
            a
        }
    }

    def fstPair(l: Logger, b: BudgetSpender, p: MachineParams, pair: UplcValue): UplcValue = {
        constCost(b, FstPair, p.builtinCostModel.fstPair)
        run(FstPair) {
            asPair(pair)._1
        }
    }

    def sndPair(l: Logger, b: BudgetSpender, p: MachineParams, pair: UplcValue): UplcValue = {
        constCost(b, SndPair, p.builtinCostModel.sndPair)
        run(SndPair) {
            asPair(pair)._2
        }
    }

    def chooseList(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        xs: UplcValue,
        e: UplcValue,
        ne: UplcValue
    ): UplcValue = {
        constCost(b, ChooseList, p.builtinCostModel.chooseList)
        run(ChooseList) {
            if asList(xs).isEmpty then e else ne
        }
    }

    /** The element type of the list must be the type of the new element, as in the CEK machine:
      * `mkCons` of an integer onto a list of `data` fails (conformance case `mkCons-fail`).
      */
    def mkCons(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        x: UplcValue,
        xs: UplcValue
    ): UplcValue = {
        constCost(b, MkCons, p.builtinCostModel.mkCons)
        run(MkCons) {
            (x, xs) match
                case (c: Constant, Constant.List(tp, elems)) =>
                    if c.tpe != tp then throw new KnownTypeUnliftingError(tp, toCekValue(x))
                    else Constant.List(tp, c :: elems)
                case (elem, ListValue(elems)) => ListValue(elem :: elems)
                // a list of element type BuiltinValue becomes a ListValue on the first cons
                case (elem, Constant.List(DefaultUni.BuiltinValue, _)) => ListValue(elem :: Nil)
                case _ => throw new DeserializationError(MkCons, toCekValue(xs))
        }
    }

    def headList(l: Logger, b: BudgetSpender, p: MachineParams, xs: UplcValue): UplcValue = {
        constCost(b, HeadList, p.builtinCostModel.headList)
        run(HeadList) {
            xs match
                case ListValue(elems) => elems.head
                case other            => asList(other).head
        }
    }

    def tailList(l: Logger, b: BudgetSpender, p: MachineParams, xs: UplcValue): UplcValue = {
        constCost(b, TailList, p.builtinCostModel.tailList)
        run(TailList) {
            xs match
                case ListValue(elems)          => ListValue(elems.tail)
                case Constant.List(tpe, elems) => Constant.List(tpe, elems.tail)
                case _ => throw new DeserializationError(TailList, toCekValue(xs))
        }
    }

    def nullList(l: Logger, b: BudgetSpender, p: MachineParams, xs: UplcValue): UplcValue = {
        constCost(b, NullList, p.builtinCostModel.nullList)
        run(NullList) {
            xs match
                case ListValue(elems) => bool(elems.isEmpty)
                case other            => bool(asList(other).isEmpty)
        }
    }

    def chooseData(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        d: UplcValue,
        constrCase: UplcValue,
        mapCase: UplcValue,
        listCase: UplcValue,
        iCase: UplcValue,
        bCase: UplcValue
    ): UplcValue = {
        constCost(b, ChooseData, p.builtinCostModel.chooseData)
        run(ChooseData) {
            Builtins.chooseData(asData(d), constrCase, mapCase, listCase, iCase, bCase)
        }
    }

    /** Costs the count as a literal, as `DropListCostingFun`, which matches the count as an integer
      * constant before the builtin runs: another first argument fails there, with a `MatchError`. A
      * count outside `Int` drops `Int.MaxValue` elements, as the CEK machine and Plutus do. Only a
      * list constant is accepted: the CEK machine fails on a `VList`.
      */
    def dropList(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        n: UplcValue,
        xs: UplcValue
    ): UplcValue = {
        val cf = p.builtinCostModel.dropList
        n match
            case Constant.Integer(count) =>
                val mn = MemoryUsage.memoryUsageLiteral(count)
                val mxs = mem(xs)
                val cost = ExUnits(cf.memory(mn, mxs).toLong, cf.cpu(mn, mxs).toLong)
                JitBudget.spendBuiltinApp(b, DropList, cost)
            case other => throw new MatchError(toCekValue(other))
        run(DropList) {
            val count = asInteger(n)
            xs match
                case Constant.List(tpe, elems) =>
                    val dropCount =
                        if count.signum <= 0 then 0
                        else if count.isValidInt then count.toInt
                        else Int.MaxValue
                    Constant.List(tpe, elems.drop(dropCount))
                case _ => throw new DeserializationError(DropList, toCekValue(xs))
        }
    }

    def lengthOfArray(l: Logger, b: BudgetSpender, p: MachineParams, a: UplcValue): UplcValue = {
        cost1(b, LengthOfArray, p.builtinCostModel.lengthOfArray, a)
        run(LengthOfArray) {
            a match
                case Constant.Array(_, arr) => Constant.Integer(BigInt(arr.length))
                case _ => throw new DeserializationError(LengthOfArray, toCekValue(a))
        }
    }

    def listToArray(l: Logger, b: BudgetSpender, p: MachineParams, xs: UplcValue): UplcValue = {
        cost1(b, ListToArray, p.builtinCostModel.listToArray, xs)
        run(ListToArray) {
            xs match
                case Constant.List(tpe, elems) => Constant.Array(tpe, elems.toVector)
                case _ => throw new DeserializationError(ListToArray, toCekValue(xs))
        }
    }

    def indexArray(
        l: Logger,
        b: BudgetSpender,
        p: MachineParams,
        a: UplcValue,
        i: UplcValue
    ): UplcValue = {
        cost2(b, IndexArray, p.builtinCostModel.indexArray, a, i)
        run(IndexArray) {
            a match
                case Constant.Array(_, arr) =>
                    val idx = asInteger(i)
                    if idx < 0 || idx >= arr.length then
                        throw new BuiltinException(
                          s"indexArray: index $idx out of bounds for array of length ${arr.length}"
                        )
                    arr(idx.toInt)
                case _ => throw new DeserializationError(IndexArray, toCekValue(a))
        }
    }
}
