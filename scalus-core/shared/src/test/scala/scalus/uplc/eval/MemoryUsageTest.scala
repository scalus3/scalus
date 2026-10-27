package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.Data
import scalus.uplc.test.ArbitraryInstances

class MemoryUsageTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    test("memoryUsageInteger(0) == 1") {
        assert(MemoryUsage.memoryUsageInteger(0).toLong == 1L)
        assert(MemoryUsage.memoryUsageInteger2(0).toLong == 1L)
    }

    test("memoryUsageInteger(2^64) == 1") {
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64) - 1).toLong == 1L)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64) - 1).toLong == 1L)
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64)).toLong == 2L)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64)).toLong == 2L)
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64) + 1).toLong == 2L)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64) + 1).toLong == 2L)
    }

    test("MemoryUsage.memoryUsageInteger(x) == MemoryUsage.memoryUsageInteger2(x)") {
        forAll { (x: BigInt) =>
            assert(
              MemoryUsage.memoryUsageInteger(x).toLong == MemoryUsage.memoryUsageInteger2(x).toLong
            )
            assert(
              MemoryUsage.memoryUsageInteger(-x).toLong == MemoryUsage
                  .memoryUsageInteger2(-x)
                  .toLong
            )
        }
    }

    test("memory usage of Data is that of the recursive definition it replaced") {
        forAll { (d: Data) =>
            assert(MemoryUsage.memoryUsageData(d).toLong == recursiveMemoryUsage(d))
            assert(MemoryUsage.memoryUsageDataNodeCount(d).toLong == recursiveNodeCount(d))
        }
    }

    // The recursive definitions that `memoryUsageData` and `memoryUsageDataNodeCount` replaced
    // (#398), kept as oracles: 4 per node plus the size of each leaf, and 1 per node.
    private def recursiveMemoryUsage(d: Data): Long = {
        val content = d match
            case Data.I(i)         => MemoryUsage.memoryUsageInteger(i).toLong
            case Data.B(bs)        => MemoryUsage.memoryUsageByteString(bs).toLong
            case Data.Constr(_, l) => l.toScalaList.map(recursiveMemoryUsage).sum
            case Data.Map(l) =>
                l.toScalaList.map((k, v) => recursiveMemoryUsage(k) + recursiveMemoryUsage(v)).sum
            case Data.List(l) => l.toScalaList.map(recursiveMemoryUsage).sum
        4L + content
    }

    private def recursiveNodeCount(d: Data): Long = {
        val children = d match
            case Data.I(_) | Data.B(_) => 0L
            case Data.Constr(_, l)     => l.toScalaList.map(recursiveNodeCount).sum
            case Data.Map(l) =>
                l.toScalaList.map((k, v) => recursiveNodeCount(k) + recursiveNodeCount(v)).sum
            case Data.List(l) => l.toScalaList.map(recursiveNodeCount).sum
        1L + children
    }
}
