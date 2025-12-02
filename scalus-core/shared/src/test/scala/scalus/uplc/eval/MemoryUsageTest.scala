package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
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
}
