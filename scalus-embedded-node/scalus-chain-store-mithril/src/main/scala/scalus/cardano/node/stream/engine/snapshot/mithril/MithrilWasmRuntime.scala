package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{HostFunction, Instance, Store, WasmFunctionHandle}
import com.dylibso.chicory.wasm.Parser
import com.dylibso.chicory.wasm.types.{ExternalType, FunctionImport, FunctionType}

import java.io.InputStream
import scala.jdk.CollectionConverters.*

/** Loads the pinned `mithril-client-wasm` blob into a Chicory runtime. The Mithril Rust crate is
  * compiled with `wasm-pack --target nodejs`, which produces a WASM module plus JS glue; this
  * class provides the host-function imports the glue normally supplies, so the same `.wasm` blob
  * runs inside a pure-JVM Chicory instance without any JS runtime.
  *
  * See `docs/local/claude/indexer/indexer-node.md` milestone M10b. This scaffolding is the first
  * step (P1: can we instantiate at all?). Every wasm-bindgen import is initially stubbed to
  * [[unimplementedImport]], which throws with a concrete name — so driving the module
  * incrementally reveals exactly which imports any given code path hits.
  */
final class MithrilWasmRuntime private (val instance: Instance) {

    /** Export lookup that throws if the name isn't present — nicer than the raw nullable. */
    def exportFn(name: String): com.dylibso.chicory.runtime.ExportFunction = {
        val fn = instance.`export`(name)
        if fn == null then throw new NoSuchElementException(s"WASM export '$name' not found")
        fn
    }

    /** wasm-bindgen string-passing: allocate WASM memory, write UTF-8 bytes, return `(ptr, len)`.
      * The `1` alignment argument matches the JS glue (see `passStringToWasm0`).
      */
    def passString(s: String): (Int, Int) = {
        val bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val malloc = exportFn("__wbindgen_malloc")
        val ptr = malloc.apply(bytes.length.toLong, 1L)(0).toInt
        instance.memory().write(ptr, bytes)
        (ptr, bytes.length)
    }

    /** Read a UTF-8 string of `len` bytes starting at `ptr` from WASM memory. */
    def readString(ptr: Int, len: Int): String = {
        val bytes = instance.memory().readBytes(ptr, len)
        new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
    }
}

object MithrilWasmRuntime {

    /** Canonical classpath location for the pinned blob. */
    val WasmResourcePath: String = "/mithril/mithril_client_wasm_bg.wasm"

    /** The wasm-bindgen module namespace Mithril's blob imports from. One per
      * `wasm-pack --target nodejs` convention.
      */
    val ImportModule: String = "__wbindgen_placeholder__"

    /** Instantiate the pinned blob against `imports`. Unresolved names get a
      * [[unimplementedImport]] stub so a call into them raises with the wasm-bindgen name, which
      * is how we discover the actual host-function demand set for a given code path.
      */
    def instantiate(
        imports: Map[String, WasmFunctionHandle]
    ): (MithrilWasmRuntime, InstantiationReport) = {
        val module = Parser.parse(loadWasmBytes())

        val fnImports: Seq[FunctionImport] =
            module
                .importSection()
                .stream()
                .iterator()
                .asScala
                .filter(_.importType() == ExternalType.FUNCTION)
                .map(_.asInstanceOf[FunctionImport])
                .toSeq

        val hostFunctions: Array[HostFunction] = fnImports.map { imp =>
            val fnType: FunctionType = module.typeSection().getType(imp.typeIndex())
            val handle: WasmFunctionHandle = imports.getOrElse(
              imp.name(),
              unimplementedImport(imp.name())
            )
            new HostFunction(imp.module(), imp.name(), fnType, handle)
        }.toArray

        val store = new Store()
        store.addFunction(hostFunctions*)
        val instance = store.instantiate("mithril", module)

        val report = InstantiationReport(
          totalImports = fnImports.size,
          resolvedByCaller = fnImports.count(i => imports.contains(i.name())),
          stubbed = fnImports.size - fnImports.count(i => imports.contains(i.name()))
        )
        (new MithrilWasmRuntime(instance), report)
    }

    final case class InstantiationReport(
        totalImports: Int,
        resolvedByCaller: Int,
        stubbed: Int
    )

    private def loadWasmBytes(): Array[Byte] = {
        val in: InputStream = getClass.getResourceAsStream(WasmResourcePath)
        if in == null then
            throw new IllegalStateException(
              s"WASM blob not found on classpath at $WasmResourcePath — " +
                  "see src/test/resources/mithril/README.md for how to stage a pinned build."
            )
        try in.readAllBytes()
        finally in.close()
    }

    /** Default `WasmFunctionHandle` for any import the caller hasn't supplied — throws on call
      * with the concrete wasm-bindgen import name so we can tell which imports a code path
      * actually exercises.
      */
    private def unimplementedImport(name: String): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
                throw new UnsupportedOperationException(
                  s"Mithril WASM called host import '$name' which is not yet implemented — " +
                      "see scalus-chain-store-mithril M10b.P2."
                )
        }
}
