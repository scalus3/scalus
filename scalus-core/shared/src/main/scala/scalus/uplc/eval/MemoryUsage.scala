package scalus.uplc.eval

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.Constant
import scalus.uplc.eval.CekValue.*

trait MemoryUsage[A]:
    def memoryUsage(a: A): CostingInteger

object MemoryUsage {
    def integerLog2(i: BigInt): Long = {
        if i == 0 then return 0L
        // in Java first byte CAN be zero, in Haskell it can not
        val bytes =
            val bytes = i.toByteArray
            if bytes.head == 0 then bytes.tail else bytes
        bytes.headOption match {
            case None => throw new IllegalStateException("empty number?")
            case Some(u) =>
                val unsigned = java.lang.Byte.toUnsignedInt(u)
                val log2 = 32 - 1 - java.lang.Integer.numberOfLeadingZeros(unsigned)
                val r = log2.toLong + 8 * (bytes.length - 1)
                r
        }
    }

    def memoryUsageLiteralByteSize(i: BigInt): CostingInteger =
        val l = i.toLong
        if l == 0 then CostingInteger(0L)
        else CostingInteger((l - 1) / 8 + 1)

    /** Memory usage as literal value (for IntegerCostedLiterally).
      *
      * Returns the absolute value of i, saturating to Long.MaxValue for values that don't fit. This
      * matches Plutus behavior where fromIntegral narrows to maxBound::SatInt.
      */
    def memoryUsageLiteral(i: BigInt): CostingInteger =
        val absVal = i.abs
        if absVal.isValidLong then CostingInteger(absVal.toLong)
        else CostingInteger(Long.MaxValue)

    def memoryUsageInteger(i: BigInt): CostingInteger =
        if i.equals(BigInt(0)) then CostingInteger(1L)
        else
            val ceilLog2 = i.abs.bitLength - 1L
            CostingInteger(ceilLog2 / 64 + 1)

    // this mimics the Haskell implementation
    def memoryUsageInteger2(i: BigInt): CostingInteger =
        if i == 0 then CostingInteger(1L) else CostingInteger((integerLog2(i.abs) / 64) + 1)

    def memoryUsageByteString(bs: ByteString): CostingInteger =
        CostingInteger((bs.size - 1) / 8 + 1)

    def memoryUsageString(s: String): CostingInteger = CostingInteger(s.length.toLong)

    def memoryUsageStringByByteLength(s: String): CostingInteger =
        CostingInteger(s.getBytes(StandardCharsets.UTF_8).length.toLong / 4L)

    // The cost of each node of the 'Data' object (in addition to the cost of its content).
    private val dataNodeMem: CostingInteger = CostingInteger(4L)

    def memoryUsageData(d: Data): CostingInteger = {
        d match
            case Data.I(i)  => dataNodeMem + memoryUsageInteger(i)
            case Data.B(bs) => dataNodeMem + memoryUsageByteString(bs)
            case _          => sumDataNodes(d, dataNodeMem, withLeaves = true)
    }

    /** Counts the number of nodes in a `Data` value (each Constr/Map/List/I/B node is 1, ignoring
      * the size of integer/bytestring leaves). Matches Plutus `DataNodeCount`, used to cost
      * `unValueData`.
      */
    def memoryUsageDataNodeCount(d: Data): CostingInteger =
        sumDataNodes(d, CostingInteger(1L), withLeaves = false)

    /** `nodeCost` for each node of `d`, plus the memory usage of its integers and byte strings when
      * `withLeaves` is set. The nesting depth of `Data` is not limited, so the nodes still to visit
      * are kept on the heap, not on the stack.
      */
    private def sumDataNodes(
        d: Data,
        nodeCost: CostingInteger,
        withLeaves: Boolean
    ): CostingInteger = {
        var acc: CostingInteger = CostingInteger(0L)
        val pending = ArrayBuffer.empty[Data].addOne(d)
        while pending.nonEmpty do
            acc = acc + nodeCost
            pending.remove(pending.length - 1) match
                case Data.I(i)         => if withLeaves then acc = acc + memoryUsageInteger(i)
                case Data.B(bs)        => if withLeaves then acc = acc + memoryUsageByteString(bs)
                case Data.Constr(_, l) => l.foreach(pending += _)
                case Data.Map(l) =>
                    l.foreach { (k, v) =>
                        pending += k
                        pending += v
                    }
                case Data.List(l) => l.foreach(pending += _)
        acc
    }

    def memoryUsage(a: CekValue): CostingInteger = a match
        case VCon(const) => memoryUsage(const)
        case _           => CostingInteger(1L)

    def memoryUsage(a: Constant): CostingInteger = a match
        case Constant.Integer(i)     => memoryUsageInteger(i)
        case Constant.ByteString(bs) => memoryUsageByteString(bs)
        case Constant.String(s)      => memoryUsageString(s)
        case Constant.Unit           => CostingInteger(1L)
        case _: Constant.Bool        => CostingInteger(1L)
        case Constant.Data(d)        => memoryUsageData(d)
        // Lists use length for memory usage (like Plutus), not sum of element memory
        case Constant.List(_, l) => CostingInteger(l.size.toLong)
        // Arrays use length for memory usage (like Plutus)
        case Constant.Array(_, a) => CostingInteger(a.size.toLong)
        case Constant.Pair(a, b) =>
            CostingInteger(1L) + memoryUsage(a) + memoryUsage(b)
        case Constant.BLS12_381_G1_Element(_) => CostingInteger(18L)
        case Constant.BLS12_381_G2_Element(_) => CostingInteger(36L)
        case Constant.BLS12_381_MlResult(_)   => CostingInteger(72L)
        // BuiltinValue memory usage based on total number of tokens
        case Constant.BuiltinValue(v) => CostingInteger(BuiltinValueOps.totalSize(v).toLong)
}
