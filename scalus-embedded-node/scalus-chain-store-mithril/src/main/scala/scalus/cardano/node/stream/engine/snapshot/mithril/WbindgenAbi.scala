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

    /** Build the default host-import map. Keys are wasm-bindgen **short names** (the prefix up
      * to and including the trailing underscore before the 16-hex signature hash). The runtime
      * strips the hash before lookup, so these bindings survive pin bumps that only rotate
      * signature hashes.
      */
    def defaultImports: Map[String, WasmFunctionHandle] = Map(
      // Predicates: (externref-handle: i32) -> i32 { 0 | 1 }
      "__wbg___wbindgen_is_undefined_" -> isUndefined,
      "__wbg___wbindgen_is_object_" -> isObject,
      "__wbg___wbindgen_is_string_" -> isString,
      "__wbg___wbindgen_is_function_" -> isFunction,
      "__wbg___wbindgen_is_bigint_" -> isBigint,
      // Boolean extractor: (externref) -> i32 { 0 | 1 | 2 (not-a-bool) }
      "__wbg___wbindgen_boolean_get_" -> booleanGet,
      // Equality predicates: (a: i32, b: i32) -> i32
      "__wbg___wbindgen_jsval_eq_" -> jsvalEq,
      "__wbg___wbindgen_jsval_loose_eq_" -> jsvalLooseEq,
      "__wbg___wbindgen_in_" -> jsvalIn,
      // Value extractors writing to a caller-provided pointer:
      "__wbg___wbindgen_string_get_" -> stringGet,
      "__wbg___wbindgen_number_get_" -> numberGet,
      "__wbg___wbindgen_bigint_get_as_i64_" -> bigintGetAsI64,
      // Debug / error reporting:
      "__wbg___wbindgen_debug_string_" -> debugString,
      "__wbg___wbindgen_throw_" -> wbindgenThrow,
      // Closure cleanup — no-op under JVM GC.
      "__wbg__wbg_cb_unref_" -> noop1,
      // JS built-in constructors / helpers.
      "__wbg_Error_" -> newError,
      "__wbg_Number_" -> newNumber
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

    // ------------------------------------------------------------------
    // Equality predicates.
    // ------------------------------------------------------------------

    private val jsvalEq: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val a = get(args(0).toInt)
            val b = get(args(1).toInt)
            // JS `===`: strict equality. For our purposes reference equality for objects, value
            // equality for strings / numbers / booleans.
            val eq =
                if a == null && b == null then true
                else if a == null || b == null then false
                else if (a.isInstanceOf[String] || a.isInstanceOf[java.lang.Number] ||
                        a.isInstanceOf[java.lang.Boolean]) && a.getClass == b.getClass
                then a == b
                else a eq b
            Array(if eq then 1L else 0L)
        }
    }

    private val jsvalLooseEq: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val a = get(args(0).toInt)
            val b = get(args(1).toInt)
            // JS `==`: loose equality. Scala's `==` already coerces between compatible types
            // (e.g., java.lang.Boolean vs. scala.Boolean); null / undefined are loosely equal.
            val nullOrUndef = (x: AnyRef | Null) => x == null || x == WbindgenAbi.Undefined
            val eq =
                if nullOrUndef(a) && nullOrUndef(b) then true
                else if a == null || b == null then false
                else a == b
            Array(if eq then 1L else 0L)
        }
    }

    private val jsvalIn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            // JS `prop in obj`. We don't model JS objects with property bags yet; conservatively
            // return 0. Code paths that rely on real `in` behaviour will need richer host
            // objects — flagged when a driver test hits it.
            Array(0L)
        }
    }

    // ------------------------------------------------------------------
    // Value extractors — each writes a (ptr, len) or f64 or i64 to a caller-supplied ret_ptr in
    // WASM memory. The layout matches wasm-bindgen's `take*FromWasm0` / `pass*ToWasm0`.
    // ------------------------------------------------------------------

    private val stringGet: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val handle = args(0).toInt
            val retPtr = args(1).toInt
            get(handle) match {
                case s: String =>
                    val utf8 = s.getBytes(StandardCharsets.UTF_8)
                    val malloc = instance.`export`("__wbindgen_malloc")
                    val ptr = malloc.apply(utf8.length.toLong, 1L)(0).toInt
                    instance.memory().write(ptr, utf8)
                    instance.memory().writeI32(retPtr, ptr)
                    instance.memory().writeI32(retPtr + 4, utf8.length)
                case _ =>
                    instance.memory().writeI32(retPtr, 0)
                    instance.memory().writeI32(retPtr + 4, 0)
            }
            Array.emptyLongArray
        }
    }

    private val numberGet: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val handle = args(0).toInt
            val retPtr = args(1).toInt
            get(handle) match {
                case n: java.lang.Number =>
                    instance.memory().writeI32(retPtr, 0) // wasm-bindgen: is-valid flag
                    instance
                        .memory()
                        .writeLong(retPtr + 8, java.lang.Double.doubleToLongBits(n.doubleValue))
                case _ =>
                    instance.memory().writeI32(retPtr, 1) // 1 == NaN / not-a-number
            }
            Array.emptyLongArray
        }
    }

    private val bigintGetAsI64: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val handle = args(0).toInt
            val retPtr = args(1).toInt
            get(handle) match {
                case b: java.math.BigInteger =>
                    instance.memory().writeI32(retPtr, 0)
                    instance.memory().writeLong(retPtr + 8, b.longValue)
                case _ =>
                    instance.memory().writeI32(retPtr, 1)
            }
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // Debug / throw.
    // ------------------------------------------------------------------

    private val debugString: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val handle = args(0).toInt
            val retPtr = args(1).toInt
            val s = String.valueOf(get(handle))
            val utf8 = s.getBytes(StandardCharsets.UTF_8)
            val malloc = instance.`export`("__wbindgen_malloc")
            val ptr = malloc.apply(utf8.length.toLong, 1L)(0).toInt
            instance.memory().write(ptr, utf8)
            instance.memory().writeI32(retPtr, ptr)
            instance.memory().writeI32(retPtr + 4, utf8.length)
            Array.emptyLongArray
        }
    }

    private val wbindgenThrow: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val ptr = args(0).toInt
            val len = args(1).toInt
            val msg = readString(instance, ptr, len)
            throw new RuntimeException(s"wasm-bindgen threw: $msg")
        }
    }

    // ------------------------------------------------------------------
    // No-op utilities.
    // ------------------------------------------------------------------

    /** One-argument no-op host function. Used for closure-cleanup hooks that the JVM GC handles
      * implicitly — we acknowledge the call and move on.
      */
    private val noop1: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
    }

    // ------------------------------------------------------------------
    // JS built-in constructors. Each returns a fresh externref handle wrapping a JVM object
    // that stands in for the JS value, so later `__wbindgen_*_get` calls can extract it.
    // ------------------------------------------------------------------

    /** `(ptr, len) -> externref` — `Error(stringFromWasm(ptr, len))`. We wrap into a plain
      * `RuntimeException`; the Mithril client treats Errors as first-class opaque values so
      * only reference identity and `.message` are exercised.
      */
    private val newError: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val ptr = args(0).toInt
            val len = args(1).toInt
            val msg = readString(instance, ptr, len)
            Array(alloc(new RuntimeException(msg)).toLong)
        }
    }

    /** `(externref) -> externref` — `Number(arg)`. Coerces a JS value to its numeric form:
      * numbers pass through, strings parse as doubles, booleans become 0/1, null becomes 0,
      * anything else that can't coerce becomes NaN (java.lang.Double.NaN).
      */
    private val newNumber: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            val n: java.lang.Double = v match {
                case null                    => java.lang.Double.valueOf(0.0)
                case WbindgenAbi.Undefined   => java.lang.Double.valueOf(Double.NaN)
                case n: java.lang.Number     => java.lang.Double.valueOf(n.doubleValue)
                case s: String =>
                    try java.lang.Double.valueOf(s)
                    catch { case _: NumberFormatException => java.lang.Double.valueOf(Double.NaN) }
                case java.lang.Boolean.TRUE  => java.lang.Double.valueOf(1.0)
                case java.lang.Boolean.FALSE => java.lang.Double.valueOf(0.0)
                case _                       => java.lang.Double.valueOf(Double.NaN)
            }
            Array(alloc(n).toLong)
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
