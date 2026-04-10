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

/** Transition count between two source locations. */
case class SourceTransition(
    fromFile: String,
    fromLine: Int,
    toFile: String,
    toLine: Int,
    count: Long
)

/** Aggregated profiling data from a CEK evaluation.
  *
  * @param bySourceLocation
  *   Budget accumulated by source location (file:line), sorted by (mem, cpu) descending
  * @param byFunction
  *   Budget accumulated by function name, sorted by (mem, cpu) descending
  * @param transitions
  *   Execution flow: how many times control transitioned from one source location to another,
  *   sorted by count descending
  * @param totalBudget
  *   Total budget spent during profiled execution
  */
case class ProfilingData(
    bySourceLocation: Seq[SourceLocationProfile],
    byFunction: Seq[FunctionProfile],
    transitions: Seq[SourceTransition],
    totalBudget: ExUnits
)
