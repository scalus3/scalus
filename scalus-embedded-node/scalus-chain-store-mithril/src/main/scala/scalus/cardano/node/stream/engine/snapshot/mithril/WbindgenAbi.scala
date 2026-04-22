package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}

import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** wasm-bindgen ABI bridge. Implements the host-function surface the `wasm-pack --target nodejs`
  * JS glue normally supplies, so the same `.wasm` blob runs under Chicory without any JS runtime.
  *
  * The core primitives:
  *
  *   - an **externref table**: a mutable `ArrayBuffer[AnyRef | Null]` that maps wasm-bindgen
  *     handles to JVM objects. Handles 0..3 are pre-populated with the wasm-bindgen constants
  *     (null, undefined, true, false) per the `wasm-bindgen` JS runtime convention.
  *   - **string marshalling** between WASM memory and JVM Strings, using UTF-8.
  *   - **predicates** (`is_undefined`, `is_object`, `is_string`, …).
  *   - **synthetic JS host types** ([[JsArray]], [[JsObject]], [[JsMap]], [[JsHeaders]],
  *     [[JsRequest]], [[JsUint8Array]], …) that stand in for the real JS values the wasm-bindgen
  *     glue would allocate, so the generic host-side `new_*`, `set_*`, `get_*` bridges can
  *     manipulate them without a JS engine.
  *
  * The async surface — `fetch`, `Promise`, `then`, `setTimeout`, `ReadableStream` — is wired to
  * [[MithrilWasmAsync]] in a follow-up. Everything in this file is **purely synchronous**: calls
  * into these host imports finish without suspending.
  */
final class WbindgenAbi {

    import WbindgenAbi.*

    // wasm-bindgen's JS runtime reserves the first four externref slots for JS literals.
    // Keeping the same indices makes crash traces consistent with the reference implementation.
    private val table: mutable.ArrayBuffer[AnyRef | Null] =
        mutable.ArrayBuffer[AnyRef | Null](
          null,
          WbindgenAbi.Undefined,
          java.lang.Boolean.TRUE,
          java.lang.Boolean.FALSE
        )

    /** In-process localStorage stand-in. Rust's `web_sys::window().local_storage()` is invoked
      * during client construction for optional certificate-verification caching; an empty map
      * is sufficient to let the caller fall through its "no cache" branch.
      */
    private val localStorageData: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.empty

    /** Allocate a new externref slot holding `obj`. */
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
      // ---- Predicates: (externref-handle: i32) -> i32 { 0 | 1 } ----
      "__wbg___wbindgen_is_undefined_" -> isUndefined,
      "__wbg___wbindgen_is_object_" -> isObject,
      "__wbg___wbindgen_is_string_" -> isString,
      "__wbg___wbindgen_is_function_" -> isFunction,
      "__wbg___wbindgen_is_bigint_" -> isBigint,
      "__wbg___wbindgen_boolean_get_" -> booleanGet,
      "__wbg___wbindgen_jsval_eq_" -> jsvalEq,
      "__wbg___wbindgen_jsval_loose_eq_" -> jsvalLooseEq,
      "__wbg___wbindgen_in_" -> jsvalIn,
      "__wbg___wbindgen_string_get_" -> stringGet,
      "__wbg___wbindgen_number_get_" -> numberGet,
      "__wbg___wbindgen_bigint_get_as_i64_" -> bigintGetAsI64,
      "__wbg___wbindgen_debug_string_" -> debugString,
      "__wbg___wbindgen_throw_" -> wbindgenThrow,
      "__wbg__wbg_cb_unref_" -> noop1,
      "__wbg_Error_" -> newError,
      "__wbg_Number_" -> newNumber,
      "__wbg_warn_" -> consoleWarn,
      "__wbg_error_" -> consoleError,
      "__wbg_log_" -> consoleLog,
      "__wbindgen_init_externref_table" -> noop0,
      // ---- Casts — identity at the handle level: caller has already boxed into externref. ----
      "__wbindgen_cast_" -> castIdentity,
      // ---- Static global accessors — all resolve to the singleton globalThis. ----
      "__wbg_static_accessor_GLOBAL_" -> staticGlobal,
      "__wbg_static_accessor_GLOBAL_THIS_" -> staticGlobal,
      "__wbg_static_accessor_SELF_" -> staticGlobal,
      "__wbg_static_accessor_WINDOW_" -> staticGlobal,
      // ---- Polymorphic reflection ----
      "__wbg_length_" -> lengthOf,
      "__wbg_get_" -> getIndexed,
      "__wbg_get_with_ref_key_" -> getWithRefKey,
      "__wbg_set_" -> setIndexed,
      "__wbg_has_" -> hasKey,
      "__wbg_call_" -> callJsFunction,
      "__wbg_isArray_" -> isArrayFn,
      "__wbg_isSafeInteger_" -> isSafeIntegerFn,
      "__wbg_iterator_" -> iteratorSymbol,
      "__wbg_entries_" -> entriesFn,
      "__wbg_next_" -> iteratorNext,
      "__wbg_done_" -> iterResultDone,
      "__wbg_value_" -> iterResultValue,
      "__wbg_key_" -> copyStringToRetPtr,
      "__wbg_stringify_" -> stringify,
      "__wbg_getTime_" -> getTime,
      "__wbg_postMessage_" -> noopAnyArgs,
      "__wbg_prototypesetcall_" -> noopAnyArgs,
      // ---- JS ctors ----
      "__wbg_new_" -> genericNew,
      "__wbg_new_0_" -> newArrayOrMap,
      "__wbg_new_no_args_" -> newObjectNoArgs,
      "__wbg_new_from_slice_" -> newUint8ArrayFromSlice,
      "__wbg_new_with_byte_offset_and_length_" -> newUint8ArrayView,
      "__wbg_new_with_str_and_init_" -> newRequest,
      // ---- Promise / microtask / timers — synchronous stand-ins. ----
      "__wbg_resolve_" -> promiseResolve,
      "__wbg_then_" -> promiseThen,
      "__wbg_queueMicrotask_" -> queueMicrotaskHandler,
      "__wbg_setTimeout_" -> setTimeoutHandler,
      "__wbg_clearTimeout_" -> clearTimeoutHandler,
      "__wbg_abort_" -> abortHandler,
      // ---- Uint8Array / ArrayBuffer accessors ----
      "__wbg_buffer_" -> uint8ArrayBuffer,
      "__wbg_byteOffset_" -> uint8ArrayByteOffset,
      "__wbg_byteLength_" -> anyByteLength,
      "__wbg_instanceof_Uint8Array_" -> isInstanceOfCls(classOf[JsUint8Array]),
      "__wbg_instanceof_ArrayBuffer_" -> isInstanceOfCls(classOf[JsArrayBuffer]),
      "__wbg_instanceof_Response_" -> isInstanceOfCls(classOf[JsResponse]),
      "__wbg_instanceof_Window_" -> isInstanceOfCls(classOf[JsGlobal.type]),
      // ---- Headers / Request configuration ----
      "__wbg_append_" -> headersAppend,
      "__wbg_set_body_" -> requestSetBody,
      "__wbg_set_cache_" -> requestSetCache,
      "__wbg_set_credentials_" -> requestSetCredentials,
      "__wbg_set_headers_" -> requestSetHeaders,
      "__wbg_set_method_" -> requestSetMethod,
      "__wbg_set_mode_" -> requestSetMode,
      "__wbg_set_signal_" -> requestSetSignal,
      "__wbg_signal_" -> abortControllerSignal,
      // ---- Response accessors — populated by fetch in the async bridge. ----
      "__wbg_status_" -> responseStatus,
      "__wbg_headers_" -> responseHeaders,
      "__wbg_url_" -> responseUrl,
      // ---- localStorage (in-memory stub; empty -> "no cache" branch in the client). ----
      "__wbg_localStorage_" -> localStorageGet,
      "__wbg_getItem_" -> localStorageGetItem,
      "__wbg_setItem_" -> localStorageSetItem,
      "__wbg_removeItem_" -> localStorageRemoveItem,
      // ---- crypto.getRandomValues — JVM SecureRandom. ----
      "__wbg_getRandomValues_" -> getRandomValues
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
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array(0L)
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
                else 2
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
            val nullOrUndef = (x: AnyRef | Null) => x == null || x == WbindgenAbi.Undefined
            val eq =
                if nullOrUndef(a) && nullOrUndef(b) then true
                else if a == null || b == null then false
                else a == b
            Array(if eq then 1L else 0L)
        }
    }

    private val jsvalIn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array(0L)
    }

    // ------------------------------------------------------------------
    // Value extractors.
    // ------------------------------------------------------------------

    private val stringGet: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val handle = args(0).toInt
            val retPtr = args(1).toInt
            get(handle) match {
                case s: String => writeStringRet(instance, retPtr, s)
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
                    instance.memory().writeI32(retPtr, 0)
                    instance
                        .memory()
                        .writeLong(retPtr + 8, java.lang.Double.doubleToLongBits(n.doubleValue))
                case _ =>
                    instance.memory().writeI32(retPtr, 1)
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
            writeStringRet(instance, retPtr, String.valueOf(get(handle)))
            Array.emptyLongArray
        }
    }

    private val wbindgenThrow: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val msg = readString(instance, args(0).toInt, args(1).toInt)
            WbindgenAbi.logger.warn(s"wasm-bindgen threw: $msg")
            throw new RuntimeException(s"wasm-bindgen threw: $msg")
        }
    }

    // ------------------------------------------------------------------
    // No-op utilities.
    // ------------------------------------------------------------------

    private val noop1: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
    }

    private val noop0: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
    }

    private val noopAnyArgs: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
    }

    // ------------------------------------------------------------------
    // console bridges.
    // ------------------------------------------------------------------

    private def consoleAt(level: scribe.Level)(v: AnyRef | Null): Unit = {
        val rendered = String.valueOf(v)
        level match {
            case scribe.Level.Warn  => WbindgenAbi.logger.warn(s"[wasm.console] $rendered")
            case scribe.Level.Error => WbindgenAbi.logger.error(s"[wasm.console] $rendered")
            case _                  => WbindgenAbi.logger.info(s"[wasm.console] $rendered")
        }
    }

    private val consoleWarn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            consoleAt(scribe.Level.Warn)(get(args(0).toInt))
            Array.emptyLongArray
        }
    }

    private val consoleError: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            consoleAt(scribe.Level.Error)(get(args(0).toInt))
            Array.emptyLongArray
        }
    }

    private val consoleLog: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            consoleAt(scribe.Level.Info)(get(args(0).toInt))
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // JS built-in constructors.
    // ------------------------------------------------------------------

    private val newError: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val msg = readString(instance, args(0).toInt, args(1).toInt)
            Array(alloc(new RuntimeException(msg)).toLong)
        }
    }

    private val newNumber: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            val n: java.lang.Double = v match {
                case null                    => java.lang.Double.valueOf(0.0)
                case WbindgenAbi.Undefined   => java.lang.Double.valueOf(Double.NaN)
                case n: java.lang.Number     => java.lang.Double.valueOf(n.doubleValue)
                case s: String =>
                    val trimmed = s.trim
                    if trimmed.isEmpty then java.lang.Double.valueOf(0.0)
                    else
                        try java.lang.Double.valueOf(trimmed)
                        catch case _: NumberFormatException => java.lang.Double.valueOf(Double.NaN)
                case java.lang.Boolean.TRUE  => java.lang.Double.valueOf(1.0)
                case java.lang.Boolean.FALSE => java.lang.Double.valueOf(0.0)
                case _                       => java.lang.Double.valueOf(Double.NaN)
            }
            Array(alloc(n).toLong)
        }
    }

    // ------------------------------------------------------------------
    // Casts & static accessors.
    // ------------------------------------------------------------------

    /** `__wbindgen_cast_*`: coerce between representations. Rust-side `u32 -> externref`,
      * `f64 -> externref`, `i64 -> externref`, `(ptr, len) -> externref-string`. Since our
      * externrefs are already unified on the JVM side, a cast is an identity/allocation —
      * (ptr, len) becomes a String handle, numeric primitives become boxed handles.
      */
    private val castIdentity: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val boxed: AnyRef = args.length match {
                case 1 =>
                    // Single-arg cast: either i64 bigint or f64 number. Can't tell from args alone,
                    // so treat long-encoded value as double iff the bits are a valid NaN/inf/double
                    // that differs from the integer interpretation. wasm-bindgen always picks one
                    // signature per call-site, so which branch we take is deterministic per hash;
                    // passing Double through BigInteger semantics for number-typed call-sites
                    // would break numeric equality, so we prefer Double here (Number is more
                    // common in serde_wasm_bindgen output than BigInt).
                    java.lang.Double.valueOf(java.lang.Double.longBitsToDouble(args(0).toLong))
                case 2 =>
                    // (ptr, len) string
                    readString(instance, args(0).toInt, args(1).toInt)
                case _ =>
                    throw new RuntimeException(s"unexpected cast arity: ${args.length}")
            }
            Array(alloc(boxed).toLong)
        }
    }

    /** All four `static_accessor_*` imports resolve to the same globalThis handle. */
    private val staticGlobal: WasmFunctionHandle = new WasmFunctionHandle {
        private var cached: Int = -1
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            if cached < 0 then cached = alloc(JsGlobal)
            Array(cached.toLong)
        }
    }

    // ------------------------------------------------------------------
    // Polymorphic reflection.
    // ------------------------------------------------------------------

    private val lengthOf: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val len = get(args(0).toInt) match {
                case s: String         => s.getBytes(StandardCharsets.UTF_8).length
                case a: JsArray        => a.items.size
                case b: JsArrayBuffer  => b.bytes.length
                case u: JsUint8Array   => u.byteLength
                case h: JsHeaders      => h.entries.size
                case _                 => 0
            }
            Array(len.toLong)
        }
    }

    private val getIndexed: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val container = get(args(0).toInt)
            val key = args(1).toLong
            val result = container match {
                case a: JsArray if key >= 0 && key < a.items.size => a.items(key.toInt)
                case o: JsObject =>
                    get(key.toInt) match {
                        case k: String => o.entries.getOrElse(k, WbindgenAbi.Undefined)
                        case _         => WbindgenAbi.Undefined
                    }
                case _ => WbindgenAbi.Undefined
            }
            Array(alloc(result).toLong)
        }
    }

    private val getWithRefKey: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val container = get(args(0).toInt)
            val key = get(args(1).toInt)
            val result = container match {
                case m: JsMap    => m.entries.getOrElse(key, WbindgenAbi.Undefined)
                case o: JsObject =>
                    key match {
                        case k: String => o.entries.getOrElse(k, WbindgenAbi.Undefined)
                        case _         => WbindgenAbi.Undefined
                    }
                case _ => WbindgenAbi.Undefined
            }
            Array(alloc(result).toLong)
        }
    }

    private val setIndexed: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val container = get(args(0).toInt)
            args.length match {
                case 3 =>
                    // Both (ref, i32, ref) [Array.set] and (ref, ref, ref) [Map.set / Reflect.set]
                    // reach this path — the middle arg is either an index or a key handle. We
                    // store the WASM-level int as an externref index; for Map/Object cases it's
                    // a handle into our table, and we treat String values as keys.
                    container match {
                        case a: JsArray =>
                            val idx = args(1).toInt
                            val v = get(args(2).toInt)
                            while a.items.size <= idx do a.items.append(null)
                            a.items(idx) = v
                        case m: JsMap =>
                            m.entries(get(args(1).toInt)) = get(args(2).toInt)
                        case o: JsObject =>
                            get(args(1).toInt) match {
                                case k: String => o.entries(k) = get(args(2).toInt)
                                case _         => ()
                            }
                        case u: JsUint8Array =>
                            // Typed-array element set: rarely used from Rust directly; stub.
                            ()
                        case _ => ()
                    }
                case _ => ()
            }
            // Some variants return a boolean/ref; safest is to push undefined so call sites that
            // ignore the result work, and call sites that expect true/false get Undefined -> 0.
            if args.length == 3 then Array.emptyLongArray
            else Array(alloc(WbindgenAbi.Undefined).toLong)
        }
    }

    private val hasKey: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val container = get(args(0).toInt)
            val key = get(args(1).toInt)
            val present = container match {
                case m: JsMap    => m.entries.contains(key)
                case o: JsObject => key match {
                        case k: String => o.entries.contains(k)
                        case _         => false
                    }
                case _ => false
            }
            Array(if present then 1L else 0L)
        }
    }

    private val callJsFunction: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            throw new UnsupportedOperationException(
              "wasm-bindgen __wbg_call_ invoked — JS function invocation is not yet implemented " +
                  "in the JVM bridge (needed once Promise/then machinery is wired)"
            )
    }

    private val isArrayFn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(if get(args(0).toInt).isInstanceOf[JsArray] then 1L else 0L)
    }

    private val isSafeIntegerFn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val safe = get(args(0).toInt) match {
                case n: java.lang.Number =>
                    val d = n.doubleValue
                    // Number.isSafeInteger: integral, finite, |x| <= 2^53 - 1.
                    !d.isNaN && !d.isInfinite && d == math.floor(d) &&
                    math.abs(d) <= 9007199254740991.0 // 2^53 - 1
                case _ => false
            }
            Array(if safe then 1L else 0L)
        }
    }

    private val iteratorSymbol: WasmFunctionHandle = new WasmFunctionHandle {
        private var cached: Int = -1
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            if cached < 0 then cached = alloc(JsIteratorSymbol)
            Array(cached.toLong)
        }
    }

    private val entriesFn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val it = get(args(0).toInt) match {
                case o: JsObject =>
                    val tuples = o.entries.iterator.map { case (k, v) =>
                        val pair = JsArray(mutable.ArrayBuffer[AnyRef | Null](k, v))
                        pair.asInstanceOf[AnyRef | Null]
                    }
                    new JsIterator(tuples)
                case a: JsArray =>
                    val tuples = a.items.iterator.zipWithIndex.map { case (v, i) =>
                        val pair = JsArray(
                          mutable.ArrayBuffer[AnyRef | Null](java.lang.Double.valueOf(i.toDouble), v)
                        )
                        pair.asInstanceOf[AnyRef | Null]
                    }
                    new JsIterator(tuples)
                case m: JsMap =>
                    val tuples = m.entries.iterator.map { case (k, v) =>
                        val pair = JsArray(mutable.ArrayBuffer[AnyRef | Null](k, v))
                        pair.asInstanceOf[AnyRef | Null]
                    }
                    new JsIterator(tuples)
                case _ => new JsIterator(Iterator.empty)
            }
            Array(alloc(it).toLong)
        }
    }

    private val iteratorNext: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val result = get(args(0).toInt) match {
                case it: JsIterator =>
                    if it.underlying.hasNext then JsIterResult(it.underlying.next(), done = false)
                    else JsIterResult(WbindgenAbi.Undefined, done = true)
                case _ => JsIterResult(WbindgenAbi.Undefined, done = true)
            }
            Array(alloc(result).toLong)
        }
    }

    private val iterResultDone: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case r: JsIterResult => Array(if r.done then 1L else 0L)
                case _               => Array(1L)
            }
    }

    private val iterResultValue: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case r: JsIterResult => Array(alloc(r.value).toLong)
                case _               => Array(alloc(WbindgenAbi.Undefined).toLong)
            }
    }

    /** `__wbg_key_(ret_ptr, iter_result, key_len)` — iterator-specific shape that writes the
      * String key into retPtr. Used for Map/Object iteration.
      */
    private val copyStringToRetPtr: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val retPtr = args(0).toInt
            val handle = args(1).toInt
            val str = get(handle) match {
                case s: String => s
                case _         => ""
            }
            writeStringRet(instance, retPtr, str)
            Array.emptyLongArray
        }
    }

    private val stringify: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            val json = WbindgenAbi.jsonStringify(v)
            Array(alloc(json).toLong)
        }
    }

    private val getTime: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case d: java.util.Date =>
                    Array(java.lang.Double.doubleToLongBits(d.getTime.toDouble))
                case _ =>
                    Array(java.lang.Double.doubleToLongBits(System.currentTimeMillis.toDouble))
            }
    }

    // ------------------------------------------------------------------
    // Constructors.
    // ------------------------------------------------------------------

    /** Zero-arg ctors: `new Array()`, `new Map()`, `new Object()`, `new Headers()`. The concrete
      * ctor a given hash resolves to is determined by the caller's use; distinguishing would
      * require per-hash dispatch. In practice the caller only writes to its result, and the
      * reads/writes we service (Array.push, Map.set, Headers.append, etc.) are routed through
      * [[setIndexed]]/[[headersAppend]] which treat the first type that matches as authoritative.
      * We default to [[JsObject]] — the most permissive shape — and let the first mutation
      * reshape it via the generic set/append pathway.
      */
    private val newArrayOrMap: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(JsObject(mutable.LinkedHashMap.empty)).toLong)
    }

    /** `new_no_args_(ptr, len)` — wasm-bindgen ctor with a string label. Used e.g. for
      * `new URL(str)`. Keep the string as the externref payload.
      */
    private val newObjectNoArgs: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val s = readString(instance, args(0).toInt, args(1).toInt)
            Array(alloc(s).toLong)
        }
    }

    private val newUint8ArrayFromSlice: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val ptr = args(0).toInt
            val len = args(1).toInt
            val bytes = instance.memory().readBytes(ptr, len)
            val buf = JsArrayBuffer(bytes)
            Array(alloc(JsUint8Array(buf, 0, len)).toLong)
        }
    }

    private val newUint8ArrayView: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val buf = get(args(0).toInt) match {
                case b: JsArrayBuffer => b
                case _ => JsArrayBuffer(Array.emptyByteArray)
            }
            val offset = args(1).toInt
            val length = args(2).toInt
            Array(alloc(JsUint8Array(buf, offset, length)).toLong)
        }
    }

    /** `new Request(url, init)` — the url is `(ptr, len)` and init is an optional externref. */
    private val newRequest: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val url = readString(instance, args(0).toInt, args(1).toInt)
            val _ = get(args(2).toInt) // init bag (currently ignored beyond setters)
            Array(alloc(new JsRequest(url)).toLong)
        }
    }

    /** Generic `__wbg_new_*` ctor — dispatches on arity since the short name `__wbg_new_` covers
      * many wasm-bindgen-emitted JS constructors that share the same signature shape:
      *
      *   - 0 args → empty container; we return [[JsObject]] which morphs into Array/Map/Headers
      *     on first mutation.
      *   - 1 arg  → wrap an existing externref (e.g. `new Uint8Array(arrayBuffer)`).
      *   - 2 args → `(ptr, len)` string ctor; the string itself becomes the payload.
      *
      * Callers that need precise semantics must register under a more specific short name
      * (e.g. `__wbg_new_no_args_`, `__wbg_new_with_str_and_init_`) which the runtime matches
      * first via direct-name lookup before falling through to this generic.
      */
    private val genericNew: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val boxed: AnyRef = args.length match {
                case 0 => JsObject(mutable.LinkedHashMap.empty)
                case 1 =>
                    get(args(0).toInt) match {
                        case b: JsArrayBuffer => JsUint8Array(b, 0, b.bytes.length)
                        case other            => JsObject(mutable.LinkedHashMap("wrapped" -> other))
                    }
                case 2 => readString(instance, args(0).toInt, args(1).toInt)
                case _ => JsObject(mutable.LinkedHashMap.empty)
            }
            Array(alloc(boxed).toLong)
        }
    }

    // ------------------------------------------------------------------
    // Promise / microtask / timers — synchronous stand-ins.
    //
    // We don't have a JS-Promise-compatible event loop (yet), so these bridges model the common
    // shape wasm-bindgen-futures produces for the *happy path* of a resolved chain:
    //
    //   - a [[JsPromise]] wraps an immediate result (or a deferred "pending" marker).
    //   - `resolve_` allocates a resolved promise.
    //   - `then_(p, onFulfilled)` we currently can't invoke a JS callback from Rust code, so
    //     we return a *new* pending promise that records the continuation — real execution of
    //     the chain will come when the async bridge (next commit) drives it.
    //   - `queueMicrotask_` records the microtask; the queue is drained synchronously.
    //   - `setTimeout_` / `clearTimeout_` are recorded but not actually fired.
    //
    // Any code path that requires *real* async resumption will hit a deliberate stub (`fetch_`,
    // etc.) that documents what's missing — so progress is bounded and visible.
    // ------------------------------------------------------------------

    private val promiseResolve: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            Array(alloc(JsPromise.resolved(v)).toLong)
        }
    }

    private val promiseThen: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = get(args(0).toInt) match {
                case p: JsPromise => p
                case other        => JsPromise.resolved(other)
            }
            // We can't actually invoke `onFulfilled` (a Rust-emitted JS closure handle) without
            // a JS call bridge. Return a pending promise carrying the upstream resolution so a
            // later async driver can wire the continuation. See __wbg_fetch_ stub.
            val next = new JsPromise(pending = true, value = p.value, error = p.error)
            Array(alloc(next).toLong)
        }
    }

    private val queueMicrotaskHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            // Accept the task; drop it on the floor for now. A future commit will wire this to
            // a real executor. Arity 1 variant returns a promise-like; arity 0 returns nothing.
            if args.length > 0 then {
                // no-op: the callback is a closure handle we can't invoke yet.
            }
            // Signature variants both exist (ret void vs ret externref); be permissive.
            Array.emptyLongArray
        }
    }

    private val setTimeoutHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            // Return a dummy timer handle (a boxed Long id). No actual scheduling yet.
            Array(alloc(java.lang.Long.valueOf(args(1).toLong)).toLong)
    }

    private val clearTimeoutHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
    }

    private val abortHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            get(args(0).toInt) match {
                case c: JsAbortController => c.abort()
                case s: JsAbortSignal     => s.aborted = true
                case _                    => ()
            }
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // Uint8Array / ArrayBuffer accessors.
    // ------------------------------------------------------------------

    private val uint8ArrayBuffer: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case u: JsUint8Array => Array(alloc(u.buffer).toLong)
                case _               => Array(alloc(JsArrayBuffer(Array.emptyByteArray)).toLong)
            }
    }

    private val uint8ArrayByteOffset: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case u: JsUint8Array => Array(u.byteOffset.toLong)
                case _               => Array(0L)
            }
    }

    private val anyByteLength: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case u: JsUint8Array   => Array(u.byteLength.toLong)
                case b: JsArrayBuffer  => Array(b.bytes.length.toLong)
                case _                 => Array(0L)
            }
    }

    private def isInstanceOfCls(cls: Class[?]): WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val v = get(args(0).toInt)
            val matches = v != null && cls.isInstance(v)
            Array(if matches then 1L else 0L)
        }
    }

    // ------------------------------------------------------------------
    // Headers / Request configuration.
    // ------------------------------------------------------------------

    /** `__wbg_append_(target, ptr_name, len_name, ptr_value, len_value)` — Headers.append. */
    private val headersAppend: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val name = readString(instance, args(1).toInt, args(2).toInt)
            val value = readString(instance, args(3).toInt, args(4).toInt)
            get(args(0).toInt) match {
                case h: JsHeaders => h.entries(name) = value
                case o: JsObject  => o.entries(name) = value
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetBody: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            get(args(0).toInt) match {
                case r: JsRequest => r.body = Some(get(args(1).toInt))
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetCache: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            get(args(0).toInt) match {
                case r: JsRequest => r.cache = args(1).toInt
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetCredentials: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            get(args(0).toInt) match {
                case r: JsRequest => r.credentials = args(1).toInt
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetHeaders: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val req = get(args(0).toInt)
            val hdrs = get(args(1).toInt)
            (req, hdrs) match {
                case (r: JsRequest, h: JsHeaders) => r.headers = h
                case (r: JsRequest, o: JsObject)  =>
                    val h = JsHeaders(mutable.LinkedHashMap.empty)
                    o.entries.foreach { case (k, v) => h.entries(k) = String.valueOf(v) }
                    r.headers = h
                case _ => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetMethod: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val method = readString(instance, args(1).toInt, args(2).toInt)
            get(args(0).toInt) match {
                case r: JsRequest => r.method = method
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetMode: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            get(args(0).toInt) match {
                case r: JsRequest => r.mode = args(1).toInt
                case _            => ()
            }
            Array.emptyLongArray
        }
    }

    private val requestSetSignal: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            (get(args(0).toInt), get(args(1).toInt)) match {
                case (r: JsRequest, s: JsAbortSignal) => r.signal = Some(s)
                case _                                => ()
            }
            Array.emptyLongArray
        }
    }

    private val abortControllerSignal: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val sig = get(args(0).toInt) match {
                case c: JsAbortController => c.signal
                case _                    => new JsAbortSignal()
            }
            Array(alloc(sig).toLong)
        }
    }

    // ------------------------------------------------------------------
    // Response accessors — stubs (populated when fetch is wired).
    // ------------------------------------------------------------------

    private val responseStatus: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case r: JsResponse => Array(r.status.toLong)
                case _             => Array(0L)
            }
    }

    private val responseHeaders: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            get(args(0).toInt) match {
                case r: JsResponse => Array(alloc(r.headers).toLong)
                case _             => Array(alloc(JsHeaders(mutable.LinkedHashMap.empty)).toLong)
            }
    }

    /** `__wbg_url_(ret_ptr, response_handle)` — writes the URL string to retPtr. */
    private val responseUrl: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val retPtr = args(0).toInt
            val url = get(args(1).toInt) match {
                case r: JsResponse => r.url
                case _             => ""
            }
            writeStringRet(instance, retPtr, url)
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // localStorage (in-memory stand-in).
    // ------------------------------------------------------------------

    private val localStorageGet: WasmFunctionHandle = new WasmFunctionHandle {
        private var cached: Int = -1
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            if cached < 0 then cached = alloc(JsLocalStorage)
            Array(cached.toLong)
        }
    }

    /** `__wbg_getItem_(ret_ptr, storage_handle, key_ptr, key_len)` — writes value or (0,0) to retPtr. */
    private val localStorageGetItem: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val retPtr = args(0).toInt
            val key = readString(instance, args(2).toInt, args(3).toInt)
            localStorageData.get(key) match {
                case Some(v) => writeStringRet(instance, retPtr, v)
                case None =>
                    instance.memory().writeI32(retPtr, 0)
                    instance.memory().writeI32(retPtr + 4, 0)
            }
            Array.emptyLongArray
        }
    }

    /** `__wbg_setItem_(storage, key_ptr, key_len, val_ptr, val_len)`. */
    private val localStorageSetItem: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val key = readString(instance, args(1).toInt, args(2).toInt)
            val value = readString(instance, args(3).toInt, args(4).toInt)
            localStorageData(key) = value
            Array.emptyLongArray
        }
    }

    /** `__wbg_removeItem_(storage, key_ptr, key_len)`. */
    private val localStorageRemoveItem: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val key = readString(instance, args(1).toInt, args(2).toInt)
            localStorageData.remove(key)
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // crypto.getRandomValues.
    // ------------------------------------------------------------------

    /** `__wbg_getRandomValues_(ptr, len)` — fill WASM memory with cryptographically random bytes. */
    private val getRandomValues: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val ptr = args(0).toInt
            val len = args(1).toInt
            val buf = new Array[Byte](len)
            WbindgenAbi.secureRandom.nextBytes(buf)
            instance.memory().write(ptr, buf)
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // Helpers.
    // ------------------------------------------------------------------

    /** Write a (ptr, len) pair for a newly-malloced UTF-8 copy of `s` at `retPtr`. */
    private def writeStringRet(instance: Instance, retPtr: Int, s: String): Unit = {
        val utf8 = s.getBytes(StandardCharsets.UTF_8)
        val malloc = instance.`export`("__wbindgen_malloc")
        val ptr = malloc.apply(utf8.length.toLong, 1L)(0).toInt
        instance.memory().write(ptr, utf8)
        instance.memory().writeI32(retPtr, ptr)
        instance.memory().writeI32(retPtr + 4, utf8.length)
    }
}

object WbindgenAbi {

    private val logger: scribe.Logger =
        scribe.Logger("scalus.cardano.node.stream.engine.snapshot.mithril.WbindgenAbi")

    private val secureRandom: java.security.SecureRandom = new java.security.SecureRandom()

    /** Sentinel that stands in for JavaScript's `undefined` in the externref table. */
    object Undefined {
        override def toString: String = "wbindgen.undefined"
    }

    /** Sentinel that stands in for JavaScript's globalThis / self / window. */
    object JsGlobal {
        override def toString: String = "wbindgen.globalThis"
    }

    /** Sentinel for the `Symbol.iterator` well-known symbol. */
    object JsIteratorSymbol {
        override def toString: String = "wbindgen.Symbol.iterator"
    }

    /** Sentinel for the localStorage root object. */
    object JsLocalStorage {
        override def toString: String = "wbindgen.localStorage"
    }

    /** Synthetic `Array` backed by a mutable ArrayBuffer. */
    final case class JsArray(items: mutable.ArrayBuffer[AnyRef | Null])

    /** Synthetic plain object (string-keyed). */
    final case class JsObject(entries: mutable.LinkedHashMap[String, AnyRef | Null])

    /** Synthetic `Map` (any-keyed). */
    final case class JsMap(entries: mutable.LinkedHashMap[AnyRef | Null, AnyRef | Null])

    /** Synthetic `Headers`. */
    final case class JsHeaders(entries: mutable.LinkedHashMap[String, String])

    /** Synthetic `ArrayBuffer`. */
    final case class JsArrayBuffer(bytes: Array[Byte])

    /** Synthetic `Uint8Array` view. */
    final case class JsUint8Array(buffer: JsArrayBuffer, byteOffset: Int, byteLength: Int)

    /** Synthetic JS `Iterator` (one-shot). */
    final class JsIterator(val underlying: Iterator[AnyRef | Null])

    /** Synthetic JS iterator-result shape `{ value, done }`. */
    final case class JsIterResult(value: AnyRef | Null, done: Boolean)

    /** Synthetic `AbortSignal` — `aborted` mutates when the driver timer fires. */
    final class JsAbortSignal(@volatile var aborted: Boolean = false)

    /** Synthetic `AbortController`. */
    final class JsAbortController {
        val signal: JsAbortSignal = new JsAbortSignal()
        def abort(): Unit = signal.aborted = true
    }

    /** Synthetic `Request` — a property-bag the host-side `fetch` will turn into an HTTP call. */
    final class JsRequest(val url: String) {
        var method: String = "GET"
        var headers: JsHeaders = JsHeaders(mutable.LinkedHashMap.empty)
        var body: Option[AnyRef | Null] = None
        var mode: Int = 0
        var credentials: Int = 0
        var cache: Int = 0
        var signal: Option[JsAbortSignal] = None
    }

    /** Synthetic `Response`. */
    final class JsResponse(
        val url: String,
        val status: Int,
        val headers: JsHeaders,
        val body: Array[Byte]
    )

    /** Synthetic `Promise`. Until the async bridge lands, only the resolved-value form is wired:
      * `pending=true` means a future continuation will populate `value` (or `error`), but the
      * current runtime can't drive that — any read side will trip the async-bridge stubs.
      */
    final class JsPromise(
        @volatile var pending: Boolean,
        @volatile var value: AnyRef | Null,
        @volatile var error: AnyRef | Null
    )

    object JsPromise {
        def resolved(v: AnyRef | Null): JsPromise = new JsPromise(pending = false, value = v, error = null)
    }

    /** Render a JVM value as JSON. Used by `JSON.stringify` bridge. For our synthetic host types
      * we emit something sensible; for unknown refs we fall back to `String.valueOf`.
      */
    def jsonStringify(v: AnyRef | Null): String = v match {
        case null                    => "null"
        case Undefined               => "undefined"
        case s: String               => escapeJsonString(s)
        case b: java.lang.Boolean    => if b.booleanValue then "true" else "false"
        case n: java.lang.Number     =>
            val d = n.doubleValue
            if d.isNaN || d.isInfinite then "null"
            else if d == d.toLong.toDouble && math.abs(d) < 1e15 then d.toLong.toString
            else d.toString
        case a: JsArray              =>
            a.items.iterator.map(jsonStringify).mkString("[", ",", "]")
        case o: JsObject             =>
            o.entries.iterator
                .map { case (k, v) => s"${escapeJsonString(k)}:${jsonStringify(v)}" }
                .mkString("{", ",", "}")
        case m: JsMap                =>
            m.entries.iterator
                .collect { case (k: String, v) => s"${escapeJsonString(k)}:${jsonStringify(v)}" }
                .mkString("{", ",", "}")
        case other                   => escapeJsonString(String.valueOf(other))
    }

    private def escapeJsonString(s: String): String = {
        val sb = new StringBuilder(s.length + 2)
        sb.append('"')
        s.foreach {
            case '"'                          => sb.append("\\\"")
            case '\\'                         => sb.append("\\\\")
            case '\n'                         => sb.append("\\n")
            case '\r'                         => sb.append("\\r")
            case '\t'                         => sb.append("\\t")
            case c if c < 0x20                => sb.append(f"\\u$c%04x")
            case c                            => sb.append(c)
        }
        sb.append('"')
        sb.toString
    }
}
