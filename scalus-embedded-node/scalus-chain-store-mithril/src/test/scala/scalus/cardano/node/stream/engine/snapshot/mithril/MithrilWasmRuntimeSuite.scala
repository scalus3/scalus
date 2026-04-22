package scalus.cardano.node.stream.engine.snapshot.mithril

import org.scalatest.funsuite.AnyFunSuite

class MithrilWasmRuntimeSuite extends AnyFunSuite {

    private val defaultImports = (new WbindgenAbi).defaultImports

    test("the pinned mithril-client-wasm blob instantiates + runs __wbindgen_start") {
        val (rt, report) = MithrilWasmRuntime.instantiate(defaultImports)
        info(
          s"Mithril WASM instantiated: totalImports=${report.totalImports}, " +
              s"resolved=${report.resolvedByCaller}, stubbed=${report.stubbed}"
        )
        assert(report.totalImports > 0)
        assert(rt.instance != null)
    }

    test("exported MithrilClient class is reachable from the instance") {
        val (rt, _) = MithrilWasmRuntime.instantiate(defaultImports)
        // MithrilClient's constructor export lives at this wasm-bindgen-generated name.
        val exports = rt.instance.exports
        val hasCtor =
            Option(exports.function("mithrilclient_new")).isDefined ||
                Option(exports.function("__wbg_mithrilclient_free")).isDefined
        assert(hasCtor, "expected mithrilclient_* exports to be reachable")
    }

    test("driver: attempt mithrilclient_new, surfacing the first unimplemented host import") {
        val (rt, report) = MithrilWasmRuntime.instantiate(defaultImports)
        info(s"abi-bridged: resolved=${report.resolvedByCaller}, stubbed=${report.stubbed}")

        val (aggPtr, aggLen) = rt.passString("https://aggregator.example.test")
        val (keyPtr, keyLen) = rt.passString("genesis_vk_placeholder")

        // externref handles: 0 = null, 1 = undefined. Rust's wasm-bindgen clients conventionally
        // accept `undefined` as "use defaults" but may reject `null`. Start with undefined.
        val undefinedHandle = 1L
        val caught = scala.util.Try(
          rt.exportFn("mithrilclient_new")
              .apply(aggPtr.toLong, aggLen.toLong, keyPtr.toLong, keyLen.toLong, undefinedHandle)
        )
        caught match {
            case scala.util.Failure(t) =>
                info(s"mithrilclient_new FAILED: ${t.getClass.getName}: ${t.getMessage}")
                // Print the full stack so TrapException's location is visible in CI logs.
                val sw = new java.io.StringWriter()
                t.printStackTrace(new java.io.PrintWriter(sw))
                info(sw.toString)
            case scala.util.Success(result) =>
                info(s"mithrilclient_new succeeded: ${result.toSeq}")
        }
    }
}
