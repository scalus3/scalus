package scalus.uplc.eval

import scalus.cardano.ledger.{ExUnitPrices, ExUnits}

/** Profile entry for a single source location. */
case class SourceLocationProfile(
    file: String,
    line: Int,
    memory: Long,
    cpu: Long,
    count: Long
)

/** Profile entry for a single function (builtin). */
case class FunctionProfile(
    name: String,
    memory: Long,
    cpu: Long,
    count: Long
)

/** Which builtins are called from a given source location. */
case class LocationFunctionProfile(
    file: String,
    line: Int,
    functionName: String,
    memory: Long,
    cpu: Long,
    count: Long
)

/** Transition count between two source locations. */
case class SourceTransition(
    fromFile: String,
    fromLine: Int,
    toFile: String,
    toLine: Int,
    count: Long
)

/** One `trace` call and the budget spent when the machine reached it.
  *
  * @param message
  *   The traced text, exactly as [[Result.logs]] carries it
  * @param budget
  *   Everything spent from the start of the evaluation up to this trace. Cumulative, not the cost
  *   of the trace itself: the cost between two traces is the difference of their budgets.
  */
case class TraceProfile(message: String, budget: ExUnits)

/** Aggregated profiling data from a CEK evaluation.
  *
  * @param bySourceLocation
  *   Budget accumulated by source location (file:line), sorted by (mem, cpu) descending
  * @param byFunction
  *   Budget accumulated by function name, sorted by (mem, cpu) descending
  * @param byLocationFunction
  *   Builtins called from each source location, sorted by (mem, cpu) descending
  * @param transitions
  *   Execution flow: how many times control transitioned from one source location to another,
  *   sorted by count descending
  * @param totalBudget
  *   Total budget spent during profiled execution
  * @param prices
  *   Optional execution-unit prices. When set, the formatters derive a per-entry on-chain fee (in
  *   lovelace) from each entry's `(mem, cpu)` and render a `fee` column/field alongside count, mem
  *   and cpu. Set it with [[withPrices]] before formatting.
  * @param entryTrace
  *   The first distinct source locations executed, in order. Used to root the hot-path tree at the
  *   first contract location (the literal first step is often a framework/fallback location).
  * @param traces
  *   Every trace the script emitted, in order, each with the budget spent when it was emitted.
  *   Empty when the script traced nothing.
  */
case class ProfilingData(
    bySourceLocation: Seq[SourceLocationProfile],
    byFunction: Seq[FunctionProfile],
    byLocationFunction: Seq[LocationFunctionProfile],
    transitions: Seq[SourceTransition],
    totalBudget: ExUnits,
    prices: Option[ExUnitPrices] = None,
    entryTrace: Seq[(String, Int)] = Nil,
    traces: Seq[TraceProfile] = Nil
) {

    /** Attach execution-unit prices so the formatters emit a derived per-entry fee (lovelace). */
    def withPrices(prices: ExUnitPrices): ProfilingData = copy(prices = Some(prices))
}
