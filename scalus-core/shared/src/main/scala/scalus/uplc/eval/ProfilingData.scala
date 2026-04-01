package scalus.uplc.eval

import scalus.cardano.ledger.ExUnits

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

/** Aggregated profiling data from a CEK evaluation.
  *
  * @param bySourceLocation
  *   Budget accumulated by source location (file:line), sorted by (mem, cpu) descending
  * @param byFunction
  *   Budget accumulated by function name, sorted by (mem, cpu) descending
  * @param totalBudget
  *   Total budget spent during profiled execution
  */
case class ProfilingData(
    bySourceLocation: Seq[SourceLocationProfile],
    byFunction: Seq[FunctionProfile],
    totalBudget: ExUnits
)
