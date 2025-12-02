package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite

/** JVM-only tests for CostingInteger that require resource loading */
class CostingIntegerJVMTest extends AnyFunSuite {

    test("DropListCostingFun from loaded JSON cost model") {
        // Load the actual cost model from JSON like the tests do
        val builtinCostModel = BuiltinCostModel.fromInputStream(
          getClass.getResourceAsStream("/builtinCostModelC.json")
        )
        val dropListCost = builtinCostModel.dropList
        assert(dropListCost != null, "dropList cost model should not be null")

        // Simulate the dropList-09 test case args
        // n = 10000000000000000000 (larger than Long.MaxValue)
        // list = [11,22,33,44,55,66,77,88,99]
        val n = BigInt("10000000000000000000")
        val listSize = 9

        // Calculate args like DropListCostingFun does
        val arg1Mem = MemoryUsage.memoryUsageLiteral(n)
        val listMemory = CostingInteger(1L) // simplified - actual list memory calculation

        println(s"memoryUsageLiteral(10^19) = ${arg1Mem.toLong}")
        println(s"Expected: ${Long.MaxValue}")
        assert(
          arg1Mem.toLong == Long.MaxValue,
          s"memoryUsageLiteral should saturate to MaxValue, got ${arg1Mem.toLong}"
        )

        // Now calculate the actual cost using the loaded cost model
        val argsMem = Seq(arg1Mem, listMemory)
        val cpuCost = dropListCost.cpu.calculateCost(argsMem)
        val memCost = dropListCost.memory.calculateCost(argsMem)

        println(s"CPU cost from loaded model: ${cpuCost.toLong}")
        println(s"Memory cost from loaded model: ${memCost.toLong}")

        assert(
          cpuCost.toLong == Long.MaxValue,
          s"CPU cost should be MaxValue, got ${cpuCost.toLong}"
        )
    }
}
