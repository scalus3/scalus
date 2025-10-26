package scalus.uplc.eval

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, BLS12_381_MlResult, BuiltinPair, ByteString, Data}

/** Memory usage calculation for JIT values without CekValue wrapper allocation.
  *
  * JIT represents values as native Scala types (BigInt, String, ByteString, Data, etc.) instead of
  * wrapped CekValue. This object provides memory usage calculation directly on these native types,
  * avoiding the overhead of creating VCon wrappers.
  */
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
        case _: Boolean     => 1
        case ()             => 1
        case l: List[?] =>
            var acc = 0L
            for elem <- l do acc += memoryUsage(elem)
            acc
        case p: BuiltinPair[?, ?] =>
            1 + memoryUsage(p.fst) + memoryUsage(p.snd)
        case p: Tuple2[?, ?] =>
            1 + memoryUsage(p._1) + memoryUsage(p._2)
        // TODO: Create common constants file for BLS memory usage values (18, 36, 72)
        // These values come from Plutus specification
        case _: BLS12_381_G1_Element => 18
        case _: BLS12_381_G2_Element => 36
        case _: BLS12_381_MlResult   => 72
        case _                       => 1 // Functions, builtins, etc.
}
