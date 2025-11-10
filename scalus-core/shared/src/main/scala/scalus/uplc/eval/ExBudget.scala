package scalus.uplc.eval

import cats.kernel.Group
import scalus.cardano.ledger.ExUnits

@deprecated("Use Long directly instead of ExCPU", "0.13.0")
opaque type ExCPU <: Long = Long
object ExCPU {
    @deprecated("Use Long directly instead of ExCPU", "0.13.0")
    inline def apply(l: Long): ExCPU = l
}

@deprecated("Use Long directly instead of ExMemory", "0.13.0")
opaque type ExMemory <: Long = Long
object ExMemory {
    @deprecated("Use Long directly instead of ExMemory", "0.13.0")
    inline def apply(l: Long): ExMemory = l
}

/** Type alias for ExUnits. Use ExUnits directly instead.
  *
  * @deprecated
  *   Use scalus.cardano.ledger.ExUnits instead
  * @since 0.13.0
  */
@deprecated("Use scalus.cardano.ledger.ExUnits instead", "0.13.0")
type ExBudget = ExUnits

object ExBudget {

    /** The zero budget */
    val zero: ExUnits = ExUnits.zero
    val enormous: ExUnits = ExUnits(Long.MaxValue, Long.MaxValue)

    /** Constructs an 'ExBudget' from CPU and memory components.
      *
      * @param cpu
      *   CPU steps
      * @param memory
      *   Memory units
      * @return
      *   ExUnits instance with swapped parameter order (memory, steps)
      */
    def fromCpuAndMemory(cpu: Long, memory: Long): ExUnits = ExUnits(memory, cpu)

    /** Factory method matching old ExBudget constructor signature
      *
      * @deprecated
      *   Use ExUnits(memory, steps) directly
      */
    @deprecated("Use ExUnits(memory, steps) directly", "0.13.0")
    def apply(cpu: ExCPU, memory: ExMemory): ExUnits = ExUnits(memory, cpu)

    /// Cats Group instance for ExBudget (delegates to ExUnits Group instance)
    @deprecated("Use ExUnits directly", "0.13.0")
    given Group[ExUnits] = ExUnits.given_Group_ExUnits

    @deprecated("Use ExUnits directly", "0.13.0")
    given Ordering[ExUnits] = ExUnits.given_Ordering_ExUnits
}
