package scalus.uplc.jit

import scalus.uplc.builtin.platform
import scalus.uplc.eval.*
import scalus.uplc.{BuiltinRuntime, BuiltinSemanticsVariant, CardanoBuiltins, DefaultFun, Term}

import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

/** Runs a monomorphic builtin with no hand-written JIT implementation through the CEK machine's own
  * runtime: the `CardanoBuiltins` of builtin semantics variant E.
  *
  * Each argument is wrapped as the CEK machine holds it: a constant in `CekValue.VCon`, with no
  * conversion by type ([[JitRuntime.toCekValue]]). Then the CEK costing function computes the cost
  * from these values, the CEK runtime computes the result, and the result constant is unwrapped. So
  * a bridged builtin has CEK semantics and CEK costing by construction, including the costing
  * wrappers (`StringByByteLengthCostingFun`, `ValueMaxDepthCostingFun`, `DataNodeCountCostingFun`),
  * and a new monomorphic Plutus builtin works in the JIT with no JIT code.
  *
  * An argument that is not a constant (a function that an ill-typed script passes) gets a stand-in
  * `CekValue` with the same memory usage, 1, on which the runtime fails to unlift, as the CEK
  * machine fails on the real value.
  *
  * Only monomorphic builtins: the result of a polymorphic builtin can be one of its arguments,
  * which can be a function. Those are all in [[BuiltinImpls]].
  *
  * Costs one `CekValue` per argument and result. If a bridged builtin turns out hot, write an
  * override in [[BuiltinImpls]].
  */
object CekBridge {

    /** The variant-E runtimes of all builtins for one cost model, indexed by `DefaultFun.ordinal`.
      */
    final class Runtimes private[CekBridge] (
        val params: MachineParams,
        val byOrdinal: Array[BuiltinRuntime]
    )

    /** The runtimes used last: the fast path. One immutable entry in a volatile field. */
    @volatile private var last: Runtimes | Null = null

    /** All runtimes built, by cost model. Building `CardanoBuiltins` creates about 100 runtimes, so
      * with only the one-entry `last`, two evaluations that alternate between two `MachineParams`
      * (two VMs, or concurrent evaluations) would rebuild them on every bridged call. Keyed by the
      * cost model, by value: equal cost models share runtimes. A program uses few cost models (one
      * per protocol parameter set); if it uses more than [[MaxCached]], the map is cleared and
      * refilled, so it cannot grow without bound.
      */
    private val byCostModel = ConcurrentHashMap[BuiltinCostModel, Runtimes]()

    private val MaxCached = 16

    /** The variant-E runtimes for the cost model of `params`.
      *
      * The generated code calls it at every bridged call site, not once in a prologue: a prologue
      * `val` would be captured by every lambda of the script and push large scripts over the
      * class-size limit. So the common case must be cheap: a volatile read and a reference compare.
      * A miss looks the cost model up by value, and builds the runtimes only for a cost model not
      * seen before.
      */
    def builtinsFor(params: MachineParams): Runtimes = {
        val l = last
        if l != null && (l.params eq params) then l
        else
            val key = ParamsKey(params)
            val known = byParams.get(key)
            val runtimes =
                if known != null then known
                else
                    // a new `params` object: the cost model by value, which hashes the whole model
                    val costModel = params.builtinCostModel
                    if byCostModel.size > MaxCached then byCostModel.clear()
                    if byParams.size > MaxCached then byParams.clear()
                    val shared = byCostModel.computeIfAbsent(costModel, cm => build(params, cm))
                    // `shared` may hold other but equal params; keep these, so the next call is a hit
                    val r =
                        if shared.params eq params then shared
                        else Runtimes(params, shared.byOrdinal)
                    byParams.put(key, r)
                    r
            last = runtimes
            runtimes
    }

    /** A `MachineParams` compared by reference: a lookup that does not hash the cost model. */
    private final class ParamsKey(val params: MachineParams) {
        override def hashCode: Int = System.identityHashCode(params)
        override def equals(other: Any): Boolean = other match
            case k: ParamsKey => k.params eq params
            case _            => false
    }

    /** The runtimes by `MachineParams` object: alternating evaluations with two `MachineParams`
      * miss `last` on every call, and a lookup by reference is cheaper than one by cost model.
      */
    private val byParams = ConcurrentHashMap[ParamsKey, Runtimes]()

    private def build(params: MachineParams, costModel: BuiltinCostModel): Runtimes = {
        val builtins = CardanoBuiltins(costModel, platform, BuiltinSemanticsVariant.E)
        Runtimes(params, DefaultFun.values.map(builtins.getBuiltinRuntime))
    }

    /** Runs builtin number `ordinal` on the JIT values `args`, as `Cek.scala` `evalBuiltinApp`
      * does: spends the cost of the saturated application, then runs the builtin.
      */
    def call(
        builtins: Runtimes,
        ordinal: Int,
        logger: Logger,
        budget: BudgetSpender,
        args: Array[AnyRef]
    ): UplcValue = {
        val fun = DefaultFun.fromOrdinal(ordinal)
        val rt = builtins.byOrdinal(ordinal)
        val cek = ArraySeq.tabulate[CekValue](args.length)(i =>
            JitRuntime.toCekValue(args(i).asInstanceOf[UplcValue])
        )
        JitBudget.spendBuiltinApp(budget, fun, rt.costFunction.calculateCost(cek*))
        // as `Cek.scala` `evalBuiltinApp`: a failure of the builtin is a `BuiltinError`
        val result =
            try rt.f(logger, cek)
            catch case NonFatal(e) => throw new BuiltinError(fun, Term.Builtin(fun), e, Nil)
        result match
            case CekValue.VCon(c) => c
            case other =>
                throw JitEvaluationFailure(s"$fun returned $other")
    }
}
