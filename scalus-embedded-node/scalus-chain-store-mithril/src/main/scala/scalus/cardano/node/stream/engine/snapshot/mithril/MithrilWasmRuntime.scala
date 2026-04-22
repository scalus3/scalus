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
      *
      * Lookup is **hash-insensitive** in the wasm-bindgen sense: the 16-hex signature hash that
      * wasm-bindgen appends to every import (e.g. `__wbg_Error_52673b7de5a0ca89`) is stripped
      * before matching, so handlers registered under the short semantic name
      * (`__wbg_Error_`) survive pin bumps that only rotate the hash.
      *
      * Resolution precedence on each import: (1) exact full-name match (hash-specific pinning,
      * overrides everything else), (2) short-name match after `stripHash`, (3) unimplemented
      * stub that raises when called. So pinned overrides → defaults → error.
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

        def resolve(name: String): Option[WasmFunctionHandle] =
            imports.get(name).orElse(imports.get(stripHash(name)))

        val hostFunctions: Array[HostFunction] = fnImports.map { imp =>
            val fnType: FunctionType = module.typeSection().getType(imp.typeIndex())
            val handle: WasmFunctionHandle =
                resolve(imp.name()).getOrElse(unimplementedImport(imp.name()))
            new HostFunction(imp.module(), imp.name(), fnType, handle)
        }.toArray

        val store = new Store()
        store.addFunction(hostFunctions*)
        val instance = store.instantiate("mithril", module)

        // wasm-pack --target nodejs emits `__wbindgen_start` as an export (not as the module's
        // start section) that the JS glue calls right after instantiation — it initialises
        // Rust's runtime, registers the panic hook, and primes lazy statics. If we skip it,
        // the first real call trips on uninitialised runtime state and panics with an
        // uninformative TrapException.
        val startFn = instance.`export`("__wbindgen_start")
        if startFn != null then
            try startFn.apply()
            catch {
                case t: Throwable =>
                    logger.error(s"__wbindgen_start failed: ${t.getMessage}", t)
                    throw t
            }

        val report = InstantiationReport(
          totalImports = fnImports.size,
          resolvedByCaller = fnImports.count(i => resolve(i.name()).isDefined),
          stubbed = fnImports.size - fnImports.count(i => resolve(i.name()).isDefined)
        )
        (new MithrilWasmRuntime(instance), report)
    }

    /** wasm-bindgen appends `_[0-9a-f]{16}` to every import for global uniqueness. The hash is
      * a hash of the binding's Rust signature and changes on ABI bumps; the prefix is stable.
      * Stripping the hash gives the semantic short name we register handlers against.
      */
    private[mithril] def stripHash(name: String): String =
        HashSuffix.findFirstIn(name) match {
            case Some(m) => name.dropRight(m.length) + "_"
            case None    => name
        }

    private val HashSuffix = "_[0-9a-f]{16}$".r

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
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
                val argStr =
                    if args == null || args.length == 0 then "()"
                    else args.map(_.toLong).mkString("(", ", ", ")")
                val msg =
                    s"Mithril WASM called unimplemented host import '$name'$argStr — " +
                        "see scalus-chain-store-mithril M10b.P2."
                logger.warn(msg)
                throw new UnsupportedOperationException(msg)
            }
        }

    private val logger: scribe.Logger =
        scribe.Logger("scalus.cardano.node.stream.engine.snapshot.mithril.MithrilWasmRuntime")
}
