package scalus.uplc.jit

import scalus.builtin.*
import scalus.uplc.eval.*

object MemoryUsageJit {

    /** Calculate memory usage for JIT values directly without CekValue wrapper.
      *
      * @param a
      *   JIT value (BigInt, String, ByteString, Data, List, Pair, etc.)
      * @return
      *   Memory usage in CostingInteger units
      */
    def memoryUsage(a: Any): CostingInteger = a match
        case i: BigInt      => MemoryUsage.memoryUsageInteger(i)
        case bs: ByteString => MemoryUsage.memoryUsageByteString(bs)
        case s: String      => MemoryUsage.memoryUsageString(s)
        case d: Data        => MemoryUsage.memoryUsageData(d)
        case _: Boolean     => CostingInteger(1L)
        case ()             => CostingInteger(1L)
        case l: List[?] =>
            var acc = CostingInteger(0L)
            for elem <- l do acc = acc + memoryUsage(elem)
            acc
        case p: BuiltinPair[?, ?] =>
            CostingInteger(1L) + memoryUsage(p.fst) + memoryUsage(p.snd)
        case p: Tuple2[?, ?] =>
            CostingInteger(1L) + memoryUsage(p._1) + memoryUsage(p._2)
        // TODO: Create common constants file for BLS memory usage values (18, 36, 72)
        // These values come from Plutus specification
        case _: BLS12_381_G1_Element => CostingInteger(18L)
        case _: BLS12_381_G2_Element => CostingInteger(36L)
        case _: BLS12_381_MlResult   => CostingInteger(72L)
        case _                       => CostingInteger(1L) // Functions, builtins, etc.

}
