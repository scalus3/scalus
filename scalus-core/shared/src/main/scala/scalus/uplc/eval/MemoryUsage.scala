package scalus.uplc.eval

import scalus.builtin.ByteString
import scalus.builtin.Data
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

    def memoryUsageData(d: Data): CostingInteger = {
        val nodeMem: CostingInteger = CostingInteger(4L)
        val usage: CostingInteger = d match
            case Data.I(i)         => memoryUsageInteger(i)
            case Data.B(bs)        => memoryUsageByteString(bs)
            case Data.Constr(_, l) => sumList(l.toScalaList)
            case Data.Map(l) =>
                var acc: CostingInteger = CostingInteger(0L)
                val it = l.toScalaList.iterator
                while it.hasNext do
                    val t = it.next()
                    acc = acc + memoryUsageData(t._1) + memoryUsageData(t._2)
                acc
            case Data.List(l) => sumList(l.toScalaList)
        // The cost of each node of the 'Data' object (in addition to the cost of its content).
        nodeMem + usage
    }

    private def sumList(l: List[Data]): CostingInteger =
        var acc: CostingInteger = CostingInteger(0L)
        val it = l.iterator
        while it.hasNext do acc = acc + memoryUsageData(it.next())
        acc

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
        case Constant.List(tpe, l) =>
            var acc: CostingInteger = CostingInteger(0L)
            for d <- l do acc = acc + memoryUsage(d)
            acc
        case Constant.Pair(a, b) =>
            CostingInteger(1L) + memoryUsage(a) + memoryUsage(b)
        case Constant.BLS12_381_G1_Element(_) => CostingInteger(18L)
        case Constant.BLS12_381_G2_Element(_) => CostingInteger(36L)
        case Constant.BLS12_381_MlResult(_)   => CostingInteger(72L)
}
