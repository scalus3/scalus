package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite

class CostingIntegerTest extends AnyFunSuite {

    // satPlus tests
    test("satPlus: normal addition") {
        assert(CostingInteger.satPlus(10L, 20L) == 30L)
        assert(CostingInteger.satPlus(-10L, -20L) == -30L)
        assert(CostingInteger.satPlus(10L, -5L) == 5L)
    }

    test("satPlus: positive overflow saturates to MaxValue") {
        assert(CostingInteger.satPlus(Long.MaxValue, 1L) == Long.MaxValue)
        assert(CostingInteger.satPlus(Long.MaxValue, Long.MaxValue) == Long.MaxValue)
        assert(CostingInteger.satPlus(Long.MaxValue - 100, 200L) == Long.MaxValue)
        // Edge case: just before overflow
        assert(CostingInteger.satPlus(Long.MaxValue - 100, 100L) == Long.MaxValue)
        // Edge case: exactly at boundary
        assert(CostingInteger.satPlus(Long.MaxValue - 100, 99L) == Long.MaxValue - 1)
    }

    test("satPlus: negative overflow saturates to MinValue") {
        assert(CostingInteger.satPlus(Long.MinValue, -1L) == Long.MinValue)
        assert(CostingInteger.satPlus(Long.MinValue, Long.MinValue) == Long.MinValue)
        assert(CostingInteger.satPlus(Long.MinValue + 100, -200L) == Long.MinValue)
    }

    // satMinus tests
    test("satMinus: normal subtraction") {
        assert(CostingInteger.satMinus(20L, 10L) == 10L)
        assert(CostingInteger.satMinus(-10L, -20L) == 10L)
        assert(CostingInteger.satMinus(0L, 0L) == 0L)
    }

    test("satMinus: positive overflow saturates to MaxValue") {
        assert(CostingInteger.satMinus(Long.MaxValue, -1L) == Long.MaxValue)
        assert(CostingInteger.satMinus(Long.MaxValue, Long.MinValue) == Long.MaxValue)
        assert(CostingInteger.satMinus(1L, Long.MinValue) == Long.MaxValue)
    }

    test("satMinus: negative overflow saturates to MinValue") {
        assert(CostingInteger.satMinus(Long.MinValue, 1L) == Long.MinValue)
        assert(CostingInteger.satMinus(Long.MinValue, Long.MaxValue) == Long.MinValue)
        assert(CostingInteger.satMinus(-1L, Long.MaxValue) == Long.MinValue)
    }

    // satMul tests
    test("satMul: normal multiplication") {
        assert(CostingInteger.satMul(10L, 20L) == 200L)
        assert(CostingInteger.satMul(-10L, 20L) == -200L)
        assert(CostingInteger.satMul(-10L, -20L) == 200L)
        assert(CostingInteger.satMul(1L, Long.MaxValue) == Long.MaxValue)
        assert(CostingInteger.satMul(Long.MaxValue, 1L) == Long.MaxValue)
    }

    test("satMul: zero cases") {
        assert(CostingInteger.satMul(0L, Long.MaxValue) == 0L)
        assert(CostingInteger.satMul(Long.MaxValue, 0L) == 0L)
        assert(CostingInteger.satMul(0L, Long.MinValue) == 0L)
        assert(CostingInteger.satMul(Long.MinValue, 0L) == 0L)
        assert(CostingInteger.satMul(0L, 0L) == 0L)
    }

    test("satMul: positive overflow saturates to MaxValue") {
        assert(CostingInteger.satMul(Long.MaxValue, 2L) == Long.MaxValue)
        assert(CostingInteger.satMul(2L, Long.MaxValue) == Long.MaxValue)
        assert(CostingInteger.satMul(Long.MaxValue, 1957L) == Long.MaxValue)
        assert(CostingInteger.satMul(Long.MaxValue, Long.MaxValue) == Long.MaxValue)
        // negative * negative = positive overflow
        assert(CostingInteger.satMul(Long.MinValue, -2L) == Long.MaxValue)
        assert(CostingInteger.satMul(-2L, Long.MinValue) == Long.MaxValue)
    }

    test("satMul: negative overflow saturates to MinValue") {
        assert(CostingInteger.satMul(Long.MaxValue, -2L) == Long.MinValue)
        assert(CostingInteger.satMul(-2L, Long.MaxValue) == Long.MinValue)
        assert(CostingInteger.satMul(Long.MinValue, 2L) == Long.MinValue)
        assert(CostingInteger.satMul(2L, Long.MinValue) == Long.MinValue)
    }

    // CostingInteger extension methods
    test("CostingInteger + uses satPlus") {
        val maxVal = CostingInteger(Long.MaxValue)
        val one = CostingInteger(1L)
        assert((maxVal + one).toLong == Long.MaxValue)

        val minVal = CostingInteger(Long.MinValue)
        val negOne = CostingInteger(-1L)
        assert((minVal + negOne).toLong == Long.MinValue)
    }

    test("CostingInteger - uses satMinus") {
        val maxVal = CostingInteger(Long.MaxValue)
        val negOne = CostingInteger(-1L)
        assert((maxVal - negOne).toLong == Long.MaxValue)

        val minVal = CostingInteger(Long.MinValue)
        val one = CostingInteger(1L)
        assert((minVal - one).toLong == Long.MinValue)
    }

    test("CostingInteger * uses satMul") {
        val maxVal = CostingInteger(Long.MaxValue)
        val two = CostingInteger(2L)
        assert((maxVal * two).toLong == Long.MaxValue)

        val slope = CostingInteger(1957L)
        assert((maxVal * slope).toLong == Long.MaxValue)
    }

    test("Combined operations: intercept + arg * slope with overflow") {
        val intercept = CostingInteger(116711L)
        val slope = CostingInteger(1957L)
        val arg = CostingInteger(Long.MaxValue)

        // arg * slope should saturate to MaxValue
        val product = arg * slope
        assert(
          product.toLong == Long.MaxValue,
          s"arg * slope should be MaxValue, got ${product.toLong}"
        )

        // intercept + MaxValue should saturate to MaxValue
        val result = intercept + product
        assert(
          result.toLong == Long.MaxValue,
          s"intercept + product should be MaxValue, got ${result.toLong}"
        )
    }

    test("OneVariableLinearFunction with overflow") {
        val intercept = CostingInteger(116711L)
        val slope = CostingInteger(1957L)
        val fn = OneVariableLinearFunction(intercept, slope)

        val arg = CostingInteger(Long.MaxValue)
        val result = fn(arg)

        assert(
          result.toLong == Long.MaxValue,
          s"Expected ${Long.MaxValue} but got ${result.toLong}"
        )
    }

    // Tests with specific large constants from dropList conformance tests
    test("satMul with large constants from cost model") {
        // dropList cost model uses: intercept=116711, slope=1957
        val intercept = 116711L
        val slope = 1957L

        // Test case from dropList-09: n = 10000000000000000000 (larger than Long.MaxValue)
        // memoryUsageLiteral should return Long.MaxValue for this
        val largeArg = Long.MaxValue

        val product = CostingInteger.satMul(largeArg, slope)
        assert(
          product == Long.MaxValue,
          s"MaxValue * 1957 should saturate to MaxValue, got $product"
        )

        val total = CostingInteger.satPlus(intercept, product)
        assert(total == Long.MaxValue, s"116711 + MaxValue should saturate to MaxValue, got $total")
    }

    test("satPlus: large positive numbers near MaxValue") {
        val nearMax = Long.MaxValue - 1000L
        assert(CostingInteger.satPlus(nearMax, 500L) == nearMax + 500L)
        assert(CostingInteger.satPlus(nearMax, 1000L) == Long.MaxValue)
        assert(CostingInteger.satPlus(nearMax, 1001L) == Long.MaxValue)
        assert(CostingInteger.satPlus(nearMax, 2000L) == Long.MaxValue)
    }

    test("satMul: edge cases with large numbers") {
        // sqrt(MaxValue) is approximately 3037000499
        val sqrtMax = 3037000499L
        // sqrtMax * sqrtMax should not overflow
        val product1 = CostingInteger.satMul(sqrtMax, sqrtMax)
        assert(product1 > 0, s"sqrtMax * sqrtMax should be positive, got $product1")
        assert(product1 < Long.MaxValue, s"sqrtMax * sqrtMax should not saturate")

        // (sqrtMax + 1) * (sqrtMax + 1) might overflow
        val sqrtMaxPlus1 = sqrtMax + 1
        val product2 = CostingInteger.satMul(sqrtMaxPlus1, sqrtMaxPlus1)
        // This should either be a valid positive number or saturate to MaxValue
        assert(
          product2 > 0 || product2 == Long.MaxValue,
          s"(sqrtMax+1)^2 should be positive or MaxValue, got $product2"
        )
    }

    test("Combined operations preserve saturation through chain") {
        val a = CostingInteger(Long.MaxValue)
        val b = CostingInteger(1000L)
        val c = CostingInteger(2000L)

        // (MaxValue * 1000) + 2000 should all saturate
        val step1 = a * b
        assert(step1.toLong == Long.MaxValue, s"MaxValue * 1000 should saturate")

        val step2 = step1 + c
        assert(step2.toLong == Long.MaxValue, s"MaxValue + 2000 should stay saturated")

        // Chain of additions starting from overflow
        val step3 = step2 + step2
        assert(step3.toLong == Long.MaxValue, s"MaxValue + MaxValue should saturate")
    }

    test("TwoArguments.LinearInX with MaxValue argument - simulates dropList-09") {
        // This simulates the exact flow in dropList-09:
        // dropList uses DropListCostingFun which calls:
        //   cpu = TwoArguments.LinearInX(intercept=116711, slope=1957)
        //   with arg = memoryUsageLiteral(10000000000000000000) = Long.MaxValue
        val intercept = CostingInteger(116711L)
        val slope = CostingInteger(1957L)
        val linearFn = OneVariableLinearFunction(intercept, slope)
        val twoArgModel = TwoArguments.LinearInX(linearFn)

        val arg1 = CostingInteger(Long.MaxValue)
        val arg2 = CostingInteger(1L) // second arg (list memory usage)

        val result = twoArgModel.apply(arg1, arg2)
        assert(
          result.toLong == Long.MaxValue,
          s"TwoArguments.LinearInX with MaxValue should return MaxValue, got ${result.toLong}"
        )
    }

    test("Full cost calculation path for dropList-09") {
        // Simulate the full cost calculation as done in DropListCostingFun
        val cpuIntercept = CostingInteger(116711L)
        val cpuSlope = CostingInteger(1957L)
        val cpuFn = OneVariableLinearFunction(cpuIntercept, cpuSlope)
        val cpuModel = TwoArguments.LinearInX(cpuFn)

        val memConstant = CostingInteger(4L)
        val memModel = TwoArguments.ConstantCost(memConstant)

        // Args: first is memoryUsageLiteral of large number, second is list memory
        val arg1Mem =
            CostingInteger(Long.MaxValue) // memoryUsageLiteral(10^19) saturates to MaxValue
        val arg2Mem = CostingInteger(704L) // approximate list memory

        val cpuCost = cpuModel.calculateCost(Seq(arg1Mem, arg2Mem))
        val memCost = memModel.calculateCost(Seq(arg1Mem, arg2Mem))

        assert(
          cpuCost.toLong == Long.MaxValue,
          s"CPU cost should be MaxValue, got ${cpuCost.toLong}"
        )
        assert(memCost.toLong == 4L, s"Memory cost should be 4, got ${memCost.toLong}")
    }

    test("satPlus: MaxValue + 1 should saturate") {
        val result = CostingInteger.satPlus(Long.MaxValue, 1L)
        assert(result == Long.MaxValue, s"MaxValue + 1 should saturate to MaxValue, got $result")
    }

    test("satPlus: MaxValue + MaxValue should saturate") {
        val result = CostingInteger.satPlus(Long.MaxValue, Long.MaxValue)
        assert(
          result == Long.MaxValue,
          s"MaxValue + MaxValue should saturate to MaxValue, got $result"
        )
    }

    test("ExUnits addition saturates") {
        import scalus.cardano.ledger.ExUnits
        val a = ExUnits(100L, Long.MaxValue)
        val b = ExUnits(50L, 1000L)
        val sum = a + b
        assert(sum.steps == Long.MaxValue, s"ExUnits steps should saturate, got ${sum.steps}")
        assert(sum.memory == 150L, s"ExUnits memory should be 150, got ${sum.memory}")
    }

    test("ExUnits |+| saturates") {
        import scalus.cardano.ledger.ExUnits
        import cats.syntax.all.*
        val a = ExUnits(100L, Long.MaxValue)
        val b = ExUnits(50L, 1000L)
        val sum = a |+| b
        assert(
          sum.steps == Long.MaxValue,
          s"ExUnits steps should saturate with |+|, got ${sum.steps}"
        )
        assert(sum.memory == 150L, s"ExUnits memory should be 150, got ${sum.memory}")
    }

}
