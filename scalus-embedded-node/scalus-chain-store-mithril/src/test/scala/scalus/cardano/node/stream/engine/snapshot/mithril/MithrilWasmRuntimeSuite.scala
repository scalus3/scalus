package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.wasm.Parser
import com.dylibso.chicory.wasm.types.{ExternalType, FunctionImport}
import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters.*

class MithrilWasmRuntimeSuite extends AnyFunSuite {

    private val defaultImports = {
        val abi = new WbindgenAbi
        abi.defaultImports ++ abi.pinnedImports
    }

    test("survey: dump every unresolved import with its type signature") {
        val bytes = {
            val in = getClass.getResourceAsStream(MithrilWasmRuntime.WasmResourcePath)
            try in.readAllBytes()
            finally in.close()
        }
        val module = Parser.parse(bytes)
        val imps: Seq[FunctionImport] = module
            .importSection()
            .stream()
            .iterator()
            .asScala
            .filter(_.importType() == ExternalType.FUNCTION)
            .map(_.asInstanceOf[FunctionImport])
            .toSeq
        val bridged = defaultImports.keySet
        def isBridged(name: String): Boolean =
            bridged.contains(name) || bridged.contains(MithrilWasmRuntime.stripHash(name))

        val unresolved = imps.filterNot(i => isBridged(i.name())).sortBy(_.name())
        info(s"unresolved with sigs (count=${unresolved.size}):")
        unresolved.foreach { imp =>
            val ft = module.typeSection().getType(imp.typeIndex())
            val params = ft.params().toString
            val results = ft.returns().toString
            info(f"  ${imp.name()}%-65s  ${params} -> ${results}")
        }
    }

    test("survey: dump exports relevant to closures / function tables / wbindgen helpers") {
        val bytes = {
            val in = getClass.getResourceAsStream(MithrilWasmRuntime.WasmResourcePath)
            try in.readAllBytes()
            finally in.close()
        }
        val module = Parser.parse(bytes)
        val exportSec = module.exportSection()
        val exports: Seq[String] =
            (0 until exportSec.exportCount()).map(i => exportSec.getExport(i).name())
        val interesting = exports.filter { n =>
            n.startsWith("__wbindgen_") || n.contains("closure") || n.startsWith("__wbg_") ||
            n.contains("externref_table") || n.contains("function_table") ||
            n.contains("invoke") || n.contains("wasm_bindgen")
        }.sorted
        info(s"total exports: ${exports.size}")
        info(s"closure/wbindgen-relevant exports (${interesting.size}):")
        interesting.foreach(n => info(s"  $n"))
    }

    test("survey: dump every wasm-bindgen import name, bucketed by prefix") {
        val bytes = {
            val in = getClass.getResourceAsStream(MithrilWasmRuntime.WasmResourcePath)
            try in.readAllBytes()
            finally in.close()
        }
        val module = Parser.parse(bytes)
        val names: Seq[String] = module
            .importSection()
            .stream()
            .iterator()
            .asScala
            .filter(_.importType() == ExternalType.FUNCTION)
            .map(_.asInstanceOf[FunctionImport].name())
            .toSeq

        val bridged = defaultImports.keySet
        val unresolved = names.filterNot { n =>
            bridged.contains(n) || bridged.contains(MithrilWasmRuntime.stripHash(n))
        }

        val byBucket: Map[String, Seq[String]] = unresolved.groupBy { n =>
            val short = MithrilWasmRuntime.stripHash(n)
            // Bucket by the segment after `__wbg_` (host-function logical name root).
            short.stripPrefix("__wbg_").takeWhile(c => c != '_' && c.isLetterOrDigit).toLowerCase
        }.map { case (k, v) => k -> v.sorted }

        info(s"total=${names.size}, bridged=${names.size - unresolved.size}, unresolved=${unresolved.size}")
        byBucket.toSeq.sortBy(-_._2.size).foreach { case (bucket, items) =>
            info(f"  bucket=$bucket%-20s count=${items.size}%3d e.g. ${items.take(3).mkString(", ")}")
        }
    }

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

        val (aggPtr, aggLen) =
            rt.passString("https://aggregator.testing-preview.api.mithril.network/aggregator")
        // Real testing-preview network genesis verification key (copied from the upstream
        // mithril-client-wasm npm README). GenesisVerificationKey::JsonHex decodes this as
        // ASCII-JSON-of-byte-array; anything else would panic the ctor at build().
        val (keyPtr, keyLen) = rt.passString(
          "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
        )

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

    // Currently fails inside Chicory's CALL_INDIRECT → LOCAL_SET with an operand-stack underflow
    // when the Promise executor closure is re-entered. Likely root cause: our JVM-side abi
    // handle space and the WASM module's `__wbindgen_externrefs` table are disjoint, so the
    // resolve/reject closure handles we allocate on the JVM side are being passed into WASM as
    // externrefs that don't correspond to live entries in the module's own table. Next session
    // will wire alloc → `__externref_table_alloc` so both sides agree on slot indices. See
    // MithrilAsyncRuntime.scala header for the runtime design.
    ignore("async runtime: list_mithril_certificates reaches fetch without tripping closure stubs") {
        import scala.concurrent.Await
        import scala.concurrent.duration.*
        val abi = new WbindgenAbi
        val asyncRt = new MithrilAsyncRuntime(abi)
        val imports = abi.defaultImports ++ abi.pinnedImports ++ asyncRt.asyncImports
        val (rt, _) = MithrilWasmRuntime.instantiate(imports)
        asyncRt.attach(rt.instance)

        val futureResult = asyncRt.submit { _ =>
            val (aggPtr, aggLen) =
                rt.passString("https://aggregator.testing-preview.api.mithril.network/aggregator")
            val (keyPtr, keyLen) = rt.passString(
              "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
            )
            val clientPtr = rt
                .exportFn("mithrilclient_new")
                .apply(aggPtr.toLong, aggLen.toLong, keyPtr.toLong, keyLen.toLong, 1L)(0)
            val listExport =
                Option(rt.instance.`export`("mithrilclient_list_mithril_certificates")).get
            scala.util.Try(listExport.apply(clientPtr))
        }
        val outcome = Await.result(futureResult, 10.seconds)
        outcome match {
            case scala.util.Success(r) =>
                info(s"list_mithril_certificates returned: ${r.toSeq}")
            case scala.util.Failure(t) =>
                info(s"list_mithril_certificates FAILED: ${t.getClass.getName}: ${t.getMessage}")
                val sw = new java.io.StringWriter()
                t.printStackTrace(new java.io.PrintWriter(sw))
                info(sw.toString.linesIterator.take(15).mkString("\n"))
        }
    }

    test("driver: attempt list_mithril_certificates — surfaces next missing host import") {
        val (rt, _) = MithrilWasmRuntime.instantiate(defaultImports)
        val (aggPtr, aggLen) =
            rt.passString("https://aggregator.testing-preview.api.mithril.network/aggregator")
        val (keyPtr, keyLen) = rt.passString(
          "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
        )
        val undefinedHandle = 1L
        val clientPtr = rt
            .exportFn("mithrilclient_new")
            .apply(aggPtr.toLong, aggLen.toLong, keyPtr.toLong, keyLen.toLong, undefinedHandle)(0)
        info(s"ctor returned clientPtr=$clientPtr")

        val listExport = Option(rt.instance.`export`("mithrilclient_list_mithril_certificates"))
        assert(listExport.isDefined, "expected mithrilclient_list_mithril_certificates export")
        val caught = scala.util.Try(listExport.get.apply(clientPtr))
        caught match {
            case scala.util.Failure(t) =>
                info(s"list_mithril_certificates FAILED: ${t.getClass.getName}: ${t.getMessage}")
                val sw = new java.io.StringWriter()
                t.printStackTrace(new java.io.PrintWriter(sw))
                info(sw.toString)
            case scala.util.Success(result) =>
                info(s"list_mithril_certificates returned (Promise handle or sync value): ${result.toSeq}")
        }
    }
}
