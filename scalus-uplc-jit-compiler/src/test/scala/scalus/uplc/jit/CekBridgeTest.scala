package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.eval.MachineParams

/** The bridge caches the CEK runtimes per cost model. Building them creates about 100 runtimes, so
  * a cache miss on every bridged call would make bridged builtins very slow.
  */
class CekBridgeTest extends AnyFunSuite {

    private val v1 = MachineParams.defaultPlutusV1PostConwayParams
    private val v3 = MachineParams.defaultPlutusV3Params

    test("alternating between two cost models does not rebuild the runtimes") {
        assert(v1.builtinCostModel != v3.builtinCostModel)
        val first1 = CekBridge.builtinsFor(v1).byOrdinal
        val first3 = CekBridge.builtinsFor(v3).byOrdinal
        for _ <- 1 to 3 do
            assert(CekBridge.builtinsFor(v1).byOrdinal eq first1)
            assert(CekBridge.builtinsFor(v3).byOrdinal eq first3)
    }

    test("equal cost models in distinct params share the runtimes") {
        val copy = v3.copy()
        assert(!(copy eq v3))
        assert(CekBridge.builtinsFor(copy).byOrdinal eq CekBridge.builtinsFor(v3).byOrdinal)
    }

    test("the same params hit the fast path and get the same runtimes") {
        val r = CekBridge.builtinsFor(v3)
        assert(CekBridge.builtinsFor(v3) eq r)
    }
}
