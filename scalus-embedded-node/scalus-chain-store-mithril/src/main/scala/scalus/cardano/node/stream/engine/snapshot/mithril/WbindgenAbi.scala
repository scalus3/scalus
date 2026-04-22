package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}
import scalus.cardano.ledger.Word64

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

    /** Cached `__wbindgen_malloc` export — looked up lazily the first time a handler needs to
      * copy a host string back into WASM memory. One lookup per WASM instance, not per call.
      */
    @volatile private var cachedMalloc: com.dylibso.chicory.runtime.ExportFunction = null

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
      // ---- Casts ----
      //
      // `__wbindgen_cast_*` has multiple distinct semantics per hash in this pin: (ptr,len)->String,
      // U64->BigInt, F64->Number, and two closure-wrap casts. The short-name fallback below
      // covers the two numeric casts (single-arg cast is double-per-convention); the string-cast
      // and closure-cast hashes are registered by full name via [[pinnedImports]].
      "__wbindgen_cast_" -> castNumericFallback,
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
      //
      // Hash-insensitive short names below handle the ctor variants whose semantics the JS
      // glue uniquely identifies by the short name. The 0-arg and (i32,i32) new ctors whose
      // short name is just `__wbg_new_` all have distinct semantics depending on the hash
      // (new Array / Object / Headers / Map / AbortController / Promise / Error / BroadcastChannel),
      // so those live in [[pinnedImports]] with hash-specific bindings. Pin bump: re-read the
      // upstream JS glue (shipped alongside the wasm blob) and update that map.
      "__wbg_new_0_" -> newDate,
      "__wbg_new_no_args_" -> newFunctionStub,
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

    /** Single-arg `__wbindgen_cast_*` fallback for the numeric casts (F64 and U64 BigInt).
      * Hash-specific pinnings in [[pinnedImports]] override this for the two-arg string cast
      * and the closure casts.
      */
    private val castNumericFallback: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val boxed: AnyRef = args.length match {
                case 1 =>
                    java.lang.Double.valueOf(java.lang.Double.longBitsToDouble(args(0).toLong))
                case _ =>
                    throw new RuntimeException(
                      s"unexpected cast arity: ${args.length} — hash-specific pinned handler " +
                          "should have matched first; see WbindgenAbi.pinnedImports"
                    )
            }
            Array(alloc(boxed).toLong)
        }
    }

    /** All four `static_accessor_*` imports resolve to the same globalThis handle. */
    private val staticGlobal: WasmFunctionHandle = singletonHandle(JsGlobal)

    /** Builds a zero-arg WASM import that always returns the same externref handle — the handle
      * is allocated lazily on first call (we can't alloc during field init without ordering
      * worries, and these are rarely invoked before real work).
      */
    private def singletonHandle(singleton: AnyRef): WasmFunctionHandle =
        new WasmFunctionHandle {
            @volatile private var cached: Int = -1
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
                if cached < 0 then cached = alloc(singleton)
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

    /** `__wbg_set_*` — polymorphic over Array.set (ref, i32, ref), Map.set / Reflect.set
      * (ref, ref, ref). The middle arg is either an index or a key handle depending on
      * container type; we resolve that locally.
      */
    private val setIndexed: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            if args.length == 3 then {
                (get(args(0).toInt), get(args(1).toInt), get(args(2).toInt)) match {
                    case (a: JsArray, _, v)           => arraySet(a, args(1).toInt, v)
                    case (m: JsMap, k, v)             => m.entries(k) = v
                    case (o: JsObject, k: String, v)  => o.entries(k) = v
                    case _                            => ()
                }
                Array.emptyLongArray
            } else Array(alloc(WbindgenAbi.Undefined).toLong)
        }
    }

    private def arraySet(a: JsArray, idx: Int, v: AnyRef | Null): Unit = {
        while a.items.size <= idx do a.items.append(null)
        a.items(idx) = v
    }

    private val hasKey: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val present = (get(args(0).toInt), get(args(1).toInt)) match {
                case (m: JsMap, k)               => m.entries.contains(k)
                case (o: JsObject, k: String)    => o.entries.contains(k)
                case _                           => false
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

    private val iteratorSymbol: WasmFunctionHandle = singletonHandle(JsIteratorSymbol)

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

    /** Zero-arg ctors, keyed by hash via [[pinnedImports]]. Each produces a fresh synthetic host
      * value; the factory evaluates its by-name argument per call so mutable containers aren't
      * shared across ctor invocations.
      */
    private def zeroArgCtor(value: => AnyRef): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
                Array(alloc(value).toLong)
        }

    private val newDate = zeroArgCtor(new java.util.Date())
    private val newObject = zeroArgCtor(JsObject(mutable.LinkedHashMap.empty))
    private val newArray = zeroArgCtor(JsArray(mutable.ArrayBuffer.empty))
    private val newHeaders = zeroArgCtor(JsHeaders(mutable.LinkedHashMap.empty))
    private val newAbortController = zeroArgCtor(new JsAbortController())
    private val newMap = zeroArgCtor(JsMap(mutable.LinkedHashMap.empty))

    /** `new Uint8Array(buffer)` — 1-arg ctor building a view over the whole buffer. */
    private val newUint8ArrayFromBuffer: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val buf = get(args(0).toInt) match {
                case b: JsArrayBuffer => b
                case u: JsUint8Array  => u.buffer
                case _                => JsArrayBuffer(Array.emptyByteArray)
            }
            Array(alloc(JsUint8Array(buf, 0, buf.bytes.length)).toLong)
        }
    }

    /** `new BroadcastChannel(name)` — Mithril's feedback receiver uses this cross-tab API; on
      * JVM we retain the channel name but don't implement actual cross-process broadcast.
      */
    private val newBroadcastChannel: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val name = readString(instance, args(0).toInt, args(1).toInt)
            Array(alloc(JsBroadcastChannel(name)).toLong)
        }
    }

    /** `new Function(sourceString)` — JavaScript dynamic code-compilation, typically invoked
      * as a feature-detection probe (`try { new Function("return this")(); } catch {…}`). We
      * can't evaluate arbitrary JS source; raise so wasm-bindgen's handleError wrapper catches
      * the failure and the caller falls through to its `catch` branch.
      */
    private val newFunctionStub: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val src = readString(instance, args(0).toInt, args(1).toInt)
            throw new UnsupportedOperationException(
              s"new Function($src) — JS dynamic code compilation not supported on JVM; " +
                  "caller is expected to treat this as a feature-detection failure"
            )
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

    /** Hash-specific bindings for the currently pinned upstream mithril-client-wasm 0.9.11. The
      * source of truth is the npm package's `mithril_client_wasm.js` shipped alongside the
      * wasm blob at `src/test/resources/mithril/`; re-read it on pin bump to refresh this map.
      *
      * Categorised:
      *   - Zero-arg ctors whose short name `__wbg_new_` ambiguously covers many JS builtins.
      *   - Two-arg `(i32,i32)` ctors that share the same short name but construct very
      *     different values (Error / BroadcastChannel / Promise-with-executor).
      *   - One-arg `(ref)` Uint8Array-from-buffer ctor.
      *   - Two closure-creating `__wbindgen_cast_*` hashes + the (ptr,len)->String cast.
      */
    def pinnedImports: Map[String, WasmFunctionHandle] = Map(
      // Zero-arg ctors (all strip to `__wbg_new_` so must be dispatched by full hash).
      "__wbg_new_0_23cedd11d9b40c9d" -> newDate,
      "__wbg_new_1ba21ce319a06297" -> newObject,
      "__wbg_new_25f239778d6112b9" -> newArray,
      "__wbg_new_3c79b3bb1b32b7d3" -> newHeaders,
      "__wbg_new_881a222c65f168fc" -> newAbortController,
      "__wbg_new_b546ae120718850e" -> newMap,
      // 1-arg `(ref) -> ref`.
      "__wbg_new_6421f6084cc5bc5a" -> newUint8ArrayFromBuffer,
      // 2-arg `(i32, i32) -> ref` — distinct semantics per hash.
      "__wbg_new_df1173567d5ff028" -> newError,
      "__wbg_new_b3dd747604c3c93e" -> newBroadcastChannel,
      "__wbg_new_ff12d2b041fb48f1" -> newPromiseWithExecutor,
      // `__wbindgen_cast_*` hashes with non-numeric semantics.
      "__wbindgen_cast_2241b6af4c4b2941" -> castStringFromPtrLen,
      "__wbindgen_cast_4625c577ab2ec9ee" -> castU64ToBigInt,
      "__wbindgen_cast_17a320bf0cb03ca7" -> castOneArgClosure,
      "__wbindgen_cast_7fcb4b52657c40f7" -> castZeroArgClosure
    )

    private val castStringFromPtrLen: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(readString(instance, args(0).toInt, args(1).toInt)).toLong)
    }

    private val castU64ToBigInt: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(Word64(args(0).toLong).toBigInteger).toLong)
    }

    /** Closure-cast factory: `(fnPtrA: u32, fnPtrB: u32) -> Externref` that wraps the two
      * pointers into a [[JsClosure]] targeting the named invoke/destroy exports. Each pinned
      * `__wbindgen_cast_*` hash corresponds to one Rust closure shape (fixed arity + export
      * pair); the factory lets us register both variants with one line each.
      */
    private def closureCast(
        arity: Int,
        invokeExport: String,
        destroyExport: String
    ): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
                Array(
                  alloc(
                    JsClosure(args(0).toInt, args(1).toInt, invokeExport, destroyExport, arity)
                  ).toLong
                )
        }

    private val castOneArgClosure: WasmFunctionHandle = closureCast(
      arity = 1,
      invokeExport = "wasm_bindgen__convert__closures_____invoke__hc0a74f7bb86030d0",
      destroyExport = "wasm_bindgen__closure__destroy__h99811cac73495ece"
    )

    private val castZeroArgClosure: WasmFunctionHandle = closureCast(
      arity = 0,
      invokeExport = "wasm_bindgen__convert__closures_____invoke__h6b7e05d46d107c93",
      destroyExport = "wasm_bindgen__closure__destroy__hea47394e049eff9b"
    )

    /** `new Promise(executor)` — the executor closure invokes
      * `wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08(a, b, resolve, reject)`
      * synchronously from the Promise ctor. Wiring the real executor call requires the async
      * runtime (next commit); today we capture the closure info onto the returned Promise so
      * the bridge can drive it when it lands.
      */
    private val newPromiseWithExecutor: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = new JsPromise(pending = true, value = null, error = null)
            p.executor = Some(
              JsClosure(
                args(0).toInt,
                args(1).toInt,
                invokeExport = "wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08",
                destroyExport = "",
                arity = 2
              )
            )
            Array(alloc(p).toLong)
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

    /** Accept the task; drop it. A real executor will come with the async bridge. Both the
      * `void`-returning and `externref`-returning signature variants are in the module; be
      * permissive and let WASM ignore the (non-existent) result for the externref variant.
      */
    private val queueMicrotaskHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array.emptyLongArray
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

    private val requestSetCache: WasmFunctionHandle = requestSetIntField((r, v) => r.cache = v)

    private val requestSetCredentials: WasmFunctionHandle =
        requestSetIntField((r, v) => r.credentials = v)

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

    private val requestSetMode: WasmFunctionHandle = requestSetIntField((r, v) => r.mode = v)

    /** Factory for `__wbg_set_{mode,cache,credentials}_*` — all have signature
      * `(request, i32) -> ()` and just stash the int on the [[JsRequest]] for the fetch bridge
      * to consult later. The int semantics come from `web_sys::RequestMode` etc. — they're
      * opaque to us until a real fetch is wired.
      */
    private def requestSetIntField(setter: (JsRequest, Int) => Unit): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
                get(args(0).toInt) match {
                    case r: JsRequest => setter(r, args(1).toInt)
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

    private val localStorageGet: WasmFunctionHandle = singletonHandle(JsLocalStorage)

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
        val ptr = malloc(instance).apply(utf8.length.toLong, 1L)(0).toInt
        instance.memory().write(ptr, utf8)
        instance.memory().writeI32(retPtr, ptr)
        instance.memory().writeI32(retPtr + 4, utf8.length)
    }

    private def malloc(instance: Instance): com.dylibso.chicory.runtime.ExportFunction = {
        if cachedMalloc == null then cachedMalloc = instance.`export`("__wbindgen_malloc")
        cachedMalloc
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
      *
      * Carries:
      *   - `executor`: the `new Promise(executor)` closure (wiring deferred to async bridge).
      *   - `continuations`: `.then(onFulfilled, onRejected)` callbacks waiting for this promise.
      */
    final class JsPromise(
        @volatile var pending: Boolean,
        @volatile var value: AnyRef | Null,
        @volatile var error: AnyRef | Null
    ) {
        var executor: Option[JsClosure] = None
        val continuations: mutable.ArrayBuffer[(Option[JsClosure], Option[JsClosure])] =
            mutable.ArrayBuffer.empty
    }

    object JsPromise {
        def resolved(v: AnyRef | Null): JsPromise = new JsPromise(pending = false, value = v, error = null)
    }

    /** A wasm-bindgen-emitted JS closure — a Rust function pointer pair plus the export names
      * that dispatch into it. On JVM, invoking the closure means calling the `invokeExport`
      * function on the Chicory [[com.dylibso.chicory.runtime.Instance]] with
      * `(fnPtrA, fnPtrB, ...args)`; destruction is a no-op (JVM GC handles the Scala side; the
      * Rust side's ref-count decrement goes via `__wbg_cb_unref_` which we already stub).
      */
    final case class JsClosure(
        fnPtrA: Int,
        fnPtrB: Int,
        invokeExport: String,
        destroyExport: String,
        arity: Int
    )

    /** Synthetic `BroadcastChannel`. No cross-process broadcast on JVM; the receiver API is
      * enough for Mithril's in-client feedback loop to stay internally consistent.
      */
    final case class JsBroadcastChannel(name: String)

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
