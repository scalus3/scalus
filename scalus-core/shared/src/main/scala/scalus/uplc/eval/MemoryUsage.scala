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
            case None    => throw new IllegalStateException("empty number?")
            case Some(u) =>
                val unsigned = java.lang.Byte.toUnsignedInt(u)
                val log2 = 32 - 1 - java.lang.Integer.numberOfLeadingZeros(unsigned)
                val r = log2.toLong + 8 * (bytes.length - 1)
                r
        }
    }

    def memoryUsageLiteralByteSize(i: BigInt): CostingInteger =
        val l = i.toLong
        if l == 0 then 0
        else (l - 1) / 8 + 1

    def memoryUsageLiteral(i: BigInt): CostingInteger =
        i.toLong.abs

    def memoryUsageInteger(i: BigInt): CostingInteger =
        if i.equals(BigInt(0)) then 1L
        else
            val ceilLog2 = i.abs.bitLength - 1L
            ceilLog2 / 64 + 1

    // this mimics the Haskell implementation
    def memoryUsageInteger2(i: BigInt): CostingInteger =
        if i == 0 then 1 else (integerLog2(i.abs) / 64) + 1

    def memoryUsageByteString(bs: ByteString): CostingInteger = (bs.size - 1) / 8 + 1

    def memoryUsageString(s: String): CostingInteger = s.length

    def memoryUsageData(d: Data): CostingInteger = {
        val nodeMem = 4L
        val usage = d match
            case Data.I(i)         => memoryUsageInteger(i)
            case Data.B(bs)        => memoryUsageByteString(bs)
            case Data.Constr(_, l) => sumList(l)
            case Data.Map(l)       =>
                var acc = 0L
                val it = l.iterator
                while it.hasNext do
                    val t = it.next()
                    acc += memoryUsageData(t._1) + memoryUsageData(t._2)
                acc
            case Data.List(l) => sumList(l)
        // The cost of each node of the 'Data' object (in addition to the cost of its content).
        nodeMem + usage
    }

    private def sumList(l: List[Data]): CostingInteger =
        var acc = 0L
        val it = l.iterator
        while it.hasNext do acc += memoryUsageData(it.next())
        acc

    def memoryUsage(a: CekValue): CostingInteger = a match
        case VCon(const) => memoryUsage(const)
        case _           => 1

    def memoryUsage(a: Constant): CostingInteger = a match
        case Constant.Integer(i)     => memoryUsageInteger(i)
        case Constant.ByteString(bs) => memoryUsageByteString(bs)
        case Constant.String(s)      => memoryUsageString(s)
        case Constant.Unit           => 1
        case _: Constant.Bool        => 1
        case Constant.Data(d)        => memoryUsageData(d)
        case Constant.List(tpe, l)   =>
            var acc = 0L
            for d <- l do acc += memoryUsage(d)
            acc
        case Constant.Pair(a, b)              => 1 + memoryUsage(a) + memoryUsage(b)
        case Constant.BLS12_381_G1_Element(_) => 18
        case Constant.BLS12_381_G2_Element(_) => 36
        case Constant.BLS12_381_MlResult(_)   => 72
}
