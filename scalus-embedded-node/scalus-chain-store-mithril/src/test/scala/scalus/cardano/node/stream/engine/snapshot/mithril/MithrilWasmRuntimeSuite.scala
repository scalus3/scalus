package scalus.cardano.node.stream.engine.snapshot.mithril

import org.scalatest.funsuite.AnyFunSuite

class MithrilWasmRuntimeSuite extends AnyFunSuite {

    test("the pinned mithril-client-wasm blob instantiates with everything stubbed") {
        val (rt, report) = MithrilWasmRuntime.instantiate(Map.empty)
        info(
          s"Mithril WASM instantiated: totalImports=${report.totalImports}, " +
              s"resolved=${report.resolvedByCaller}, stubbed=${report.stubbed}"
        )
        assert(report.totalImports > 0)
        assert(rt.instance != null)
    }

    test("exported MithrilClient class is reachable from the instance") {
        val (rt, _) = MithrilWasmRuntime.instantiate(Map.empty)
        // MithrilClient's constructor export lives at this wasm-bindgen-generated name.
        // The smoke check is just that it exists; calling it requires real imports.
        val exports = rt.instance.exports
        val hasCtor =
            Option(exports.function("mithrilclient_new")).isDefined ||
                Option(exports.function("__wbg_mithrilclient_free")).isDefined
        assert(hasCtor, "expected mithrilclient_* exports to be reachable")
    }

    test("driver: attempt mithrilclient_new, surfacing the first unimplemented host import") {
        val abi = new WbindgenAbi
        val (rt, report) = MithrilWasmRuntime.instantiate(abi.defaultImports)
        info(s"abi-bridged: resolved=${report.resolvedByCaller}, stubbed=${report.stubbed}")

        val (aggPtr, aggLen) = rt.passString("https://aggregator.example.test")
        val (keyPtr, keyLen) = rt.passString("genesis_vk_placeholder")

        val caught = scala.util.Try(
          rt.exportFn("mithrilclient_new")
              .apply(aggPtr.toLong, aggLen.toLong, keyPtr.toLong, keyLen.toLong, 0L)
        )
        info(s"mithrilclient_new outcome: $caught")
    }
}
