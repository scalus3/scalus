package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** wasm-bindgen ABI bridge. Implements the host-function surface the `wasm-pack --target nodejs`
  * JS glue normally supplies, so the same `.wasm` blob runs under Chicory without any JS runtime.
  *
  * The core primitives:
  *
  *   - an **externref table**: a mutable `Vector[AnyRef | Null]` that maps wasm-bindgen u32
  *     handles to JVM objects. Handles 0..3 are pre-populated with the wasm-bindgen constants
  *     (null, undefined, true, false) per the `wasm-bindgen` JS runtime convention.
  *   - **string marshalling** between WASM memory and JVM Strings, using UTF-8 (wasm-bindgen's
  *     `passStringToWasm0` / `getStringFromWasm0` layout).
  *   - **predicates** (`is_undefined`, `is_object`, `is_string`, …) that inspect a handle's
  *     stored JVM object and return 0/1.
  *   - **property accessors** (`string_get`, `number_get`, …) that copy a JVM value out of the
  *     externref into a caller-provided pointer.
  *
  * Imports are produced by [[WbindgenAbi.defaultImports]]; callers can override or extend the
  * map via [[MithrilWasmRuntime.instantiate]].
  *
  * The bridge is stateful — one instance per WASM instance. It captures the WASM [[Instance]]
  * when the first import is called, so imports that need to write into WASM memory (strings,
  * floats, i64) can do so without the caller threading `Instance` through every call-site.
  */
final class WbindgenAbi {

    // wasm-bindgen's JS runtime reserves the first four externref slots for JS literals.
    // Keeping the same indices makes crash traces consistent with the reference implementation.
    private val table: mutable.ArrayBuffer[AnyRef | Null] =
        mutable.ArrayBuffer[AnyRef | Null](null, WbindgenAbi.Undefined, java.lang.Boolean.TRUE, java.lang.Boolean.FALSE)

    /** Allocate a new externref slot holding `obj`. Reuses freed slots lazily via a free list. */
    def alloc(obj: AnyRef | Null): Int = {
        table.append(obj)
        table.size - 1
    }

    def get(idx: Int): AnyRef | Null =
        if idx < 0 || idx >= table.size then null else table(idx)

    def set(idx: Int, obj: AnyRef | Null): Unit = {
        while table.size <= idx do table.append(null)
        table(idx) = obj
    }

    /** Read a UTF-8 string written by WASM at (ptr, len). */
    def readString(instance: Instance, ptr: Int, len: Int): String =
        new String(instance.memory().readBytes(ptr, len), StandardCharsets.UTF_8)

    /** Build the default host-import map. Returns imports keyed by wasm-bindgen name. */
    def defaultImports: Map[String, WasmFunctionHandle] = Map(
      // Predicates: (externref-handle: i32) -> i32 { 0 | 1 }
      "__wbg___wbindgen_is_undefined_f6b95eab589e0269" -> isUndefined,
      "__wbg___wbindgen_is_object_ce774f3490692386" -> isObject,
      "__wbg___wbindgen_is_string_704ef9c8fc131030" -> isString,
      "__wbg___wbindgen_is_function_8d400b8b1af978cd" -> isFunction,
      "__wbg___wbindgen_is_bigint_0e1a2e3f55cfae27" -> isBigint,
      // Boolean extractor: (externref) -> i32 { 0 | 1 | -1 (not-a-bool) }
      "__wbg___wbindgen_boolean_get_dea25b33882b895b" -> booleanGet
    )

    // ------------------------------------------------------------------
    // wasm-bindgen predicate host functions.
    // ------------------------------------------------------------------

    private val isUndefined: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(if get(args(0).toInt) eq WbindgenAbi.Undefined then 1L else 0L)
    }

    private val isObject: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            // wasm-bindgen's `is_object` is JS semantics: null is NOT an object; anything else
            // that's not a primitive (string, number, boolean, bigint, function) is.
            val isObj = v != null && v != WbindgenAbi.Undefined && !v.isInstanceOf[String] &&
                !v.isInstanceOf[java.lang.Number] && !v.isInstanceOf[java.lang.Boolean]
            Array(if isObj then 1L else 0L)
        }
    }

    private val isString: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(if get(args(0).toInt).isInstanceOf[String] then 1L else 0L)
    }

    private val isFunction: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(0L) // no JS-callable functions in our runtime yet
    }

    private val isBigint: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(if get(args(0).toInt).isInstanceOf[java.math.BigInteger] then 1L else 0L)
    }

    private val booleanGet: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            val result =
                if v == java.lang.Boolean.TRUE then 1
                else if v == java.lang.Boolean.FALSE then 0
                else 2 // wasm-bindgen convention: 2 == "not a boolean"
            Array(result.toLong)
        }
    }
}

object WbindgenAbi {

    /** Sentinel that stands in for JavaScript's `undefined` in the externref table. The
      * reference is compared by identity, so any unique object would do; we pick a dedicated
      * singleton to make the intent clear in debug output.
      */
    object Undefined {
        override def toString: String = "wbindgen.undefined"
    }
}
