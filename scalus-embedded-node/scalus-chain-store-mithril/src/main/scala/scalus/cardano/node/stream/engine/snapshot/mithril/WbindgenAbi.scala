package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}
import scalus.cardano.ledger.Word64

import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** wasm-bindgen ABI bridge. Implements the host-function surface the `wasm-pack --target nodejs` JS
  * glue normally supplies, so the same `.wasm` blob runs under Chicory without any JS runtime.
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
final class WbindgenAbi(
    closureHashes: MithrilAsyncRuntime.ClosureHashes =
        MithrilAsyncRuntime.ClosureHashes.Release0_9_11
) {

    import WbindgenAbi.*

    /** Mirror of the WASM module's `__wbindgen_externrefs` table. Slot indices on this map are the
      * same indices the WASM module sees; allocation goes through the module's own
      * `__externref_table_alloc` export so the two sides agree on which slot a handle refers to.
      *
      * Slot 0 is conventionally `undefined`. Slots 1..3 (or wherever
      * [[__wbindgen_init_externref_table]] places them) hold the wasm-bindgen JS literal sentinels
      * (`undefined`, `null`, `true`, `false`).
      *
      * Pre-init (before the module instance is attached to the table allocator) the bootstrap slots
      * 0..3 are seeded with [null, undefined, true, false] — matching the JS-glue convention so the
      * externref handles `0`/`1` mean what wasm-bindgen expects when used as parameters to the very
      * first host calls during instantiation, before the module's runtime has run
      * `__wbindgen_init_externref_table`.
      */
    private val externrefs: mutable.HashMap[Int, AnyRef | Null] = mutable.HashMap(
      0 -> null,
      1 -> WbindgenAbi.Undefined,
      2 -> java.lang.Boolean.TRUE,
      3 -> java.lang.Boolean.FALSE
    )

    /** In-process localStorage stand-in. Rust's `web_sys::window().local_storage()` is invoked
      * during client construction for optional certificate-verification caching; an empty map is
      * sufficient to let the caller fall through its "no cache" branch.
      */
    private val localStorageData: mutable.LinkedHashMap[String, String] =
        mutable.LinkedHashMap.empty

    /** Cached `__wbindgen_malloc` export — looked up lazily the first time a handler needs to copy
      * a host string back into WASM memory. One lookup per WASM instance, not per call.
      */
    @volatile private var cachedMalloc: com.dylibso.chicory.runtime.ExportFunction = null

    /** Cached `__externref_table_alloc` export — used by [[alloc]] to allocate slots in the
      * module's own externref table so JVM and WASM agree on slot indices.
      */
    @volatile private var cachedExternrefAlloc: com.dylibso.chicory.runtime.ExportFunction = null

    /** Cached handle to the module's exported externref table. Needed because allocating a slot via
      * `__externref_table_alloc` is only half the JS-glue pattern — the other half is populating
      * the slot via `wasm.__wbindgen_externrefs.set(idx, value)`. We store the slot index itself as
      * the stored ref so `wasm.__wbindgen_externrefs.get(idx)` round-trips to `idx`, which is also
      * what our JVM-side `externrefs` map is keyed on.
      */
    @volatile private var cachedExternrefTable: com.dylibso.chicory.runtime.TableInstance = null

    /** The active WASM Instance, captured the first time any handler is invoked. Lets [[alloc]]
      * route through `__externref_table_alloc` without threading the Instance through every call
      * site. Set once per instantiation; remains constant thereafter.
      */
    @volatile private var currentInstance: Instance = null.asInstanceOf[Instance]

    private def captureInstance(instance: Instance): Unit =
        if currentInstance == null then currentInstance = instance

    /** Public hook so external import-providers (e.g. [[MithrilAsyncRuntime]]) can capture the
      * Instance via the same path as [[captured]] handlers do.
      */
    def captureForExtension(instance: Instance): Unit = captureInstance(instance)

    /** Allocate an externref slot. Once the WASM instance is attached (after the first host call),
      * allocations go through the module's `__externref_table_alloc` so the slot index we return is
      * the same one the module's `__wbindgen_externrefs` table reserves.
      *
      * Before attachment (the bootstrap window during instantiation, before any host call), we hand
      * out monotonic slots from a JVM-private counter. wasm-bindgen's
      * `__wbindgen_init_externref_table` runs early and the bootstrap counter is then
      * fast-forwarded past the module-issued indices to avoid collision.
      */
    def alloc(obj: AnyRef | Null): Int = {
        val slot = if currentInstance != null then {
            val s = externrefAlloc(currentInstance).apply()(0).toInt
            writeTableSlot(currentInstance, s)
            s
        } else {
            val s = nextBootstrapSlot
            nextBootstrapSlot += 1
            s
        }
        externrefs(slot) = obj
        slot
    }

    /** Store the slot index itself as the externref value at that slot in the module's table.
      * Mirrors the JS glue's `wasm.__wbindgen_externrefs.set(idx, obj)` step (minus the object — on
      * JVM we keep the object in our own map, and the table just holds the integer handle so
      * `table.get(idx)` round-trips back to `idx`).
      */
    private def writeTableSlot(instance: Instance, slot: Int): Unit = {
        if cachedExternrefTable == null then
            cachedExternrefTable = instance.exports.table("__wbindgen_externrefs")
        cachedExternrefTable.setRef(slot, slot, instance)
    }

    @volatile private var nextBootstrapSlot: Int = 4

    def get(idx: Int): AnyRef | Null = externrefs.getOrElse(idx, null)

    def set(idx: Int, obj: AnyRef | Null): Unit =
        externrefs(idx) = obj

    private def externrefAlloc(instance: Instance): com.dylibso.chicory.runtime.ExportFunction = {
        if cachedExternrefAlloc == null then
            cachedExternrefAlloc = instance.`export`("__externref_table_alloc")
        cachedExternrefAlloc
    }

    /** Read a UTF-8 string written by WASM at (ptr, len). */
    def readString(instance: Instance, ptr: Int, len: Int): String =
        new String(instance.memory().readBytes(ptr, len), StandardCharsets.UTF_8)

    /** Wrap every handler so the first call captures the WASM Instance — gives [[alloc]] access to
      * `__externref_table_alloc` without threading the Instance through call sites.
      */
    private def captured(h: WasmFunctionHandle): WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            captureInstance(instance)
            h.apply(instance, args.map(_.toLong)*)
        }
    }

    /** Build the default host-import map. Keys are wasm-bindgen **short names** (the prefix up to
      * and including the trailing underscore before the 16-hex signature hash). The runtime strips
      * the hash before lookup, so these bindings survive pin bumps that only rotate signature
      * hashes.
      */
    def defaultImports: Map[String, WasmFunctionHandle] =
        rawDefaultImports.view.mapValues(captured).toMap

    private def rawDefaultImports: Map[String, WasmFunctionHandle] = Map(
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
      "__wbindgen_init_externref_table" -> initExternrefTable,
      // ---- Casts ----
      //
      // `__wbindgen_cast_*` has multiple distinct semantics per hash in this pin: (ptr,len)->String,
      // U64->BigInt, F64->Number, and two closure-wrap casts. The short-name fallback below
      // covers the two numeric casts (single-arg cast is double-per-convention); the string-cast
      // and closure-cast hashes are registered by full name via [[pinnedImports]].
      "__wbindgen_cast_" -> castNumericFallback,
      // ---- Static global accessors — match the Node.js semantics the upstream JS glue uses.
      // In Node.js, GLOBAL and GLOBAL_THIS are defined and return the globalThis object;
      // SELF and WINDOW are undefined (they're browser-only), so those return 0 (null slot).
      "__wbg_static_accessor_GLOBAL_" -> staticGlobal,
      "__wbg_static_accessor_GLOBAL_THIS_" -> staticGlobal,
      "__wbg_static_accessor_SELF_" -> staticNullHandle,
      "__wbg_static_accessor_WINDOW_" -> staticNullHandle,
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
      "__wbg_prototypesetcall_" -> uint8ArrayPrototypeSetCall,
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
      // NOTE: two `__wbg_queueMicrotask_*` hashes exist with different return arities
      // (getter returns externref, invoke returns void). Short-name registration would
      // fail the collision detector. Hash-specific bindings below.
      "__wbg_queueMicrotask_9b549dfce8865860" -> queueMicrotaskGetterStub,
      "__wbg_queueMicrotask_fca69f5bfad613a5" -> queueMicrotaskInvokeStub,
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
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val callable = get(args(0).toInt) match {
                case null            => false
                case _: JsIterableFn => true
                case _: JsClosure    => true
                case ref: AnyRef     => ref eq JsQueueMicrotaskFn
            }
            Array(if callable then 1L else 0L)
        }
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
            // Upstream ABI: `(retPtr: i32, obj: externref)`. arg0 is the return pointer,
            // arg1 is the object. Writes `(ptr, len)` at retPtr for Some, `(0, 0)` for None.
            val retPtr = args(0).toInt
            get(args(1).toInt) match {
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
            // Upstream ABI: `(retPtr: i32, obj: externref)`. Writes `[tag i32, _padding, f64]`
            // at retPtr: tag is 1 for Some (a number), 0 for None. See the JS glue.
            val retPtr = args(0).toInt
            val handle = args(1).toInt
            get(handle) match {
                case n: java.lang.Number =>
                    instance.memory().writeI32(retPtr, 1)
                    instance
                        .memory()
                        .writeLong(retPtr + 8, java.lang.Double.doubleToLongBits(n.doubleValue))
                case _ =>
                    instance.memory().writeI32(retPtr, 0)
            }
            Array.emptyLongArray
        }
    }

    private val bigintGetAsI64: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val retPtr = args(0).toInt
            val handle = args(1).toInt
            get(handle) match {
                case b: java.math.BigInteger =>
                    instance.memory().writeI32(retPtr, 1)
                    instance.memory().writeLong(retPtr + 8, b.longValue)
                case _ =>
                    instance.memory().writeI32(retPtr, 0)
            }
            Array.emptyLongArray
        }
    }

    // ------------------------------------------------------------------
    // Debug / throw.
    // ------------------------------------------------------------------

    private val debugString: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            // Upstream ABI: `(retPtr: i32, obj: externref)`. Writes UTF-8 (ptr,len) at retPtr.
            val retPtr = args(0).toInt
            val handle = args(1).toInt
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

    /** `Uint8Array.prototype.set.call(wasmSlice, sourceTypedArray)` — copy `sourceTypedArray`'s
      * bytes into WASM linear memory at `(ptr, len)`. This is how `js_sys::Uint8Array::copy_to`
      * transfers an HTTP response body (or any typed-array payload) back into a Rust `Vec<u8>`.
      * Silently does nothing if the source isn't recognisable as a byte array.
      */
    private val uint8ArrayPrototypeSetCall: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val dstPtr = args(0).toInt
            val dstLen = args(1).toInt
            val (src, srcOffset, srcLen) = get(args(2).toInt) match {
                case u: JsUint8Array  => (u.buffer.bytes, u.byteOffset, u.byteLength)
                case b: JsArrayBuffer => (b.bytes, 0, b.bytes.length)
                case _                => (Array.emptyByteArray, 0, 0)
            }
            val n = math.min(dstLen, srcLen)
            if n > 0 then
                instance
                    .memory()
                    .write(dstPtr, java.util.Arrays.copyOfRange(src, srcOffset, srcOffset + n))
            Array.emptyLongArray
        }
    }

    /** Mirror of the upstream JS init: grow the externref table by 4 and seed it with
      * `[undefined, null, true, false]` at the four newly-allocated slots, plus `undefined` at slot
      * 0. Our JVM-side `externrefs` map is updated in lockstep so subsequent reads agree with what
      * WASM sees on the table.
      */
    private val initExternrefTable: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            // Mirror the upstream JS init exactly:
            //   const offset = table.grow(4);
            //   table.set(0, undefined);
            //   table.set(offset + 0, undefined);
            //   table.set(offset + 1, null);
            //   table.set(offset + 2, true);
            //   table.set(offset + 3, false);
            //
            // The grow-by-4 guarantees the four seed slots are contiguous — Rust code may
            // assume this layout, so using four separate `__externref_table_alloc` calls
            // (which pull from the free list and may return scattered indices) is wrong.
            if cachedExternrefTable == null then
                cachedExternrefTable = instance.exports.table("__wbindgen_externrefs")
            val offset = cachedExternrefTable.grow(4, 0, instance)
            writeTableSlotWithValue(instance, 0, 0)
            externrefs(0) = WbindgenAbi.Undefined
            writeTableSlotWithValue(instance, offset, offset)
            externrefs(offset) = WbindgenAbi.Undefined
            writeTableSlotWithValue(instance, offset + 1, offset + 1)
            externrefs(offset + 1) = null
            writeTableSlotWithValue(instance, offset + 2, offset + 2)
            externrefs(offset + 2) = java.lang.Boolean.TRUE
            writeTableSlotWithValue(instance, offset + 3, offset + 3)
            externrefs(offset + 3) = java.lang.Boolean.FALSE
            nextBootstrapSlot = math.max(nextBootstrapSlot, offset + 4)
            Array.emptyLongArray
        }
    }

    /** Like [[writeTableSlot]] but stores an explicit value (rather than the slot index). */
    private def writeTableSlotWithValue(instance: Instance, slot: Int, value: Int): Unit = {
        if cachedExternrefTable == null then
            cachedExternrefTable = instance.exports.table("__wbindgen_externrefs")
        cachedExternrefTable.setRef(slot, value, instance)
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
                case null                  => java.lang.Double.valueOf(0.0)
                case WbindgenAbi.Undefined => java.lang.Double.valueOf(Double.NaN)
                case n: java.lang.Number   => java.lang.Double.valueOf(n.doubleValue)
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
      * Hash-specific pinnings in [[pinnedImports]] override this for the two-arg string cast and
      * the closure casts.
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

    /** All four `static_accessor_*` imports resolve to a globalThis-sentinel externref. JS glue
      * allocates a fresh slot on every call; we match that behaviour so the externref table grows
      * in lockstep with what Rust expects.
      */
    private val staticGlobal: WasmFunctionHandle = freshHandle(JsGlobal)

    /** Accessor that returns 0 — the wasm-bindgen convention for a null / "not-defined" externref
      * handle. Used for `self` and `window` in Node.js, which are browser-only globals.
      */
    private val staticNullHandle: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = Array(0L)
    }

    /** Zero-arg WASM import that allocates a fresh externref slot holding `singleton` on every call
      * and returns that slot index. Mirrors the JS glue's `addToExternrefTable0` pattern — JS does
      * not cache.
      */
    private def freshHandle(singleton: AnyRef): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
                Array(alloc(singleton).toLong)
        }

    // ------------------------------------------------------------------
    // Polymorphic reflection.
    // ------------------------------------------------------------------

    private val lengthOf: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val len = get(args(0).toInt) match {
                case s: String        => s.getBytes(StandardCharsets.UTF_8).length
                case a: JsArray       => a.items.size
                case b: JsArrayBuffer => b.bytes.length
                case u: JsUint8Array  => u.byteLength
                case h: JsHeaders     => h.entries.size
                case _                => 0
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
            val result: AnyRef | Null =
                if key.eq(JsIteratorSymbol) then entriesFnFor(container)
                else
                    container match {
                        case m: JsMap => m.entries.getOrElse(key, WbindgenAbi.Undefined)
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

    /** Build a zero-arg [[JsIterableFn]] whose call produces a [[JsIterator]] over `container`.
      * Mirrors JS's `obj[Symbol.iterator]` method lookup: the return is callable and yields the
      * iterator on invocation (not the iterator itself).
      */
    private def entriesFnFor(container: AnyRef | Null): AnyRef | Null =
        container match {
            case _: JsHeaders | _: JsObject | _: JsArray | _: JsMap =>
                JsIterableFn(() => buildEntriesIterator(container))
            case _ => WbindgenAbi.Undefined
        }

    private def buildEntriesIterator(container: AnyRef | Null): JsIterator = {
        def tuple(k: AnyRef | Null, v: AnyRef | Null): AnyRef | Null =
            JsArray(mutable.ArrayBuffer[AnyRef | Null](k, v)).asInstanceOf[AnyRef | Null]
        val tuples: Iterator[AnyRef | Null] = container match {
            case h: JsHeaders => h.entries.iterator.map((k, v) => tuple(k, v))
            case o: JsObject  => o.entries.iterator.map((k, v) => tuple(k, v))
            case m: JsMap     => m.entries.iterator.map((k, v) => tuple(k, v))
            case a: JsArray =>
                a.items.iterator.zipWithIndex.map((v, i) =>
                    tuple(java.lang.Double.valueOf(i.toDouble), v)
                )
            case _ => Iterator.empty
        }
        new JsIterator(tuples)
    }

    /** `__wbg_set_*` — polymorphic over Array.set (ref, i32, ref), Map.set / Reflect.set (ref, ref,
      * ref). The middle arg is either an index or a key handle depending on container type; we
      * resolve that locally.
      */
    private val setIndexed: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            if args.length == 3 then {
                (get(args(0).toInt), get(args(1).toInt), get(args(2).toInt)) match {
                    case (a: JsArray, _, v)          => arraySet(a, args(1).toInt, v)
                    case (m: JsMap, k, v)            => m.entries(k) = v
                    case (o: JsObject, k: String, v) => o.entries(k) = v
                    case _                           => ()
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
                case (m: JsMap, k)            => m.entries.contains(k)
                case (o: JsObject, k: String) => o.entries.contains(k)
                case _                        => false
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

    private val iteratorSymbol: WasmFunctionHandle = freshHandle(JsIteratorSymbol)

    private val entriesFn: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(buildEntriesIterator(get(args(0).toInt))).toLong)
    }

    private val iteratorNext: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(iteratorNextResult(get(args(0).toInt))).toLong)
    }

    /** `__wbg_next_*` GETTER variant — returns the iterator's `next` method bound to the iterator.
      * Later `method.call(this, ...)` invocations run [[iteratorNextResult]].
      */
    private val iteratorNextGetter: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val target = get(args(0).toInt)
            Array(alloc(JsIterableFn(() => iteratorNextResult(target))).toLong)
        }
    }

    /** Pull the next element from `container` under the JS iterator protocol. Returns a
      * [[JsIterResult]] — this is always an object so `typeof it === "object"` holds, which
      * `js_sys::Iterator::looks_like_iterator` requires.
      */
    private def iteratorNextResult(container: AnyRef | Null): JsIterResult = container match {
        case it: JsIterator =>
            if it.underlying.hasNext then JsIterResult(it.underlying.next(), done = false)
            else JsIterResult(WbindgenAbi.Undefined, done = true)
        case _ => JsIterResult(WbindgenAbi.Undefined, done = true)
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

    /** `__wbg_key_(ret_ptr, iter_result, key_len)` — iterator-specific shape that writes the String
      * key into retPtr. Used for Map/Object iteration.
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

    /** `new BroadcastChannel(name)` — Mithril's feedback receiver uses this cross-tab API; on JVM
      * we retain the channel name but don't implement actual cross-process broadcast.
      */
    private val newBroadcastChannel: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val name = readString(instance, args(0).toInt, args(1).toInt)
            Array(alloc(JsBroadcastChannel(name)).toLong)
        }
    }

    /** `new Function(sourceString)` — JavaScript dynamic code-compilation, typically invoked as a
      * feature-detection probe (`try { new Function("return this")(); } catch {…}`). We can't
      * evaluate arbitrary JS source; raise so wasm-bindgen's handleError wrapper catches the
      * failure and the caller falls through to its `catch` branch.
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
                case _                => JsArrayBuffer(Array.emptyByteArray)
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

    /** `Map.set(k, v)` → returns the Map itself. Same arity as the other polymorphic `__wbg_set_*`
      * hashes (ref, ref, ref) but returns one externref where they return nothing.
      */
    private val mapSetReturning: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val container = get(args(0).toInt)
            val key = get(args(1).toInt)
            val value = get(args(2).toInt)
            container match {
                case m: JsMap =>
                    m.entries(key) = value
                case o: JsObject =>
                    key match {
                        case k: String => o.entries(k) = value
                        case _         => ()
                    }
                case _ => ()
            }
            Array(args(0)) // return the container handle unchanged
        }
    }

    /** Hash-specific bindings for the currently pinned upstream mithril-client-wasm 0.9.11. The
      * source of truth is the npm package's `mithril_client_wasm.js` shipped alongside the wasm
      * blob at `src/test/resources/mithril/`; re-read it on pin bump to refresh this map.
      *
      * Categorised:
      *   - Zero-arg ctors whose short name `__wbg_new_` ambiguously covers many JS builtins.
      *   - Two-arg `(i32,i32)` ctors that share the same short name but construct very different
      *     values (Error / BroadcastChannel / Promise-with-executor).
      *   - One-arg `(ref)` Uint8Array-from-buffer ctor.
      *   - Two closure-creating `__wbindgen_cast_*` hashes + the (ptr,len)->String cast.
      */
    def pinnedImports: Map[String, WasmFunctionHandle] =
        rawPinnedImports.view.mapValues(captured).toMap

    private def rawPinnedImports: Map[String, WasmFunctionHandle] = Map(
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
      closureHashes.promiseExecutorImport -> newPromiseWithExecutor,
      // `__wbindgen_cast_*` hashes with non-numeric semantics.
      "__wbindgen_cast_2241b6af4c4b2941" -> castStringFromPtrLen,
      "__wbindgen_cast_4625c577ab2ec9ee" -> castU64ToBigInt,
      closureHashes.oneArgClosureCastImport -> castOneArgClosure,
      closureHashes.zeroArgClosureCastImport -> castZeroArgClosure,
      // `__wbg_set_*` hash collisions: three variants return void (array/object/typed-array
      // element set), one returns a ref (`Map.set(k, v)` which returns the Map itself). The
      // short-name fallback only handles the void-returning shape; the ref-returning one
      // needs its own handler to avoid unbalancing the operand stack.
      "__wbg_set_efaaf145b9377369" -> mapSetReturning,
      // Two `__wbg_get_*` hashes collide on short name and return arity but differ in PARAM
      // shape: `(ref, i32) -> ref` is indexed access (Array[i]); `(ref, ref) -> ref` is
      // Reflect.get(container, key). They can't share one short-name handler because the
      // handler would need to know whether arg(1) is an i32 index or a ref handle. Route
      // them explicitly: short name stays on [[getIndexed]] (i32 key); ref-key variant
      // goes to [[getWithRefKey]].
      "__wbg_get_af9dab7e9603ea93" -> getWithRefKey,
      // Two `__wbg_next_*` hashes collide on short name AND return arity: one is the
      // property GETTER (`it.next`) returning the bound method, the other is the INVOKER
      // (`it.next()`) returning the IteratorNext. The short-name fallback defaults to the
      // invoker shape; the getter is pinned separately so `js_sys::Iterator::looks_like_iterator`
      // — which reads `it["next"]` and expects a Function — finds a callable.
      "__wbg_next_138a17bbf04e926c" -> iteratorNextGetter,
      "__wbg_next_3cfe5c0fe2a4cc53" -> iteratorNext
    )

    private val castStringFromPtrLen: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(readString(instance, args(0).toInt, args(1).toInt)).toLong)
    }

    private val castU64ToBigInt: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(Word64(args(0).toLong).toBigInteger).toLong)
    }

    /** Closure-cast factory: `(fnPtrA: u32, fnPtrB: u32) -> Externref` that wraps the two pointers
      * into a [[JsClosure]] targeting the named invoke/destroy exports. Each pinned
      * `__wbindgen_cast_*` hash corresponds to one Rust closure shape (fixed arity + export pair);
      * the factory lets us register both variants with one line each.
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
      invokeExport = closureHashes.oneArgClosureInvoke,
      destroyExport = closureHashes.oneArgClosureDestroy
    )

    private val castZeroArgClosure: WasmFunctionHandle = closureCast(
      arity = 0,
      invokeExport = closureHashes.zeroArgClosureInvoke,
      destroyExport = closureHashes.zeroArgClosureDestroy
    )

    /** `new Promise(executor)` placeholder — captures the closure pointers but does NOT invoke the
      * executor. The async runtime ([[MithrilAsyncRuntime]]) overrides this short name in its
      * overlay map with a version that actually runs the executor.
      */
    private val newPromiseWithExecutor: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = new JsPromise(pending = true, value = null, error = null)
            p.executor = Some(
              JsClosure(
                args(0).toInt,
                args(1).toInt,
                invokeExport = closureHashes.promiseExecutorInvoke,
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

    /** Sync-mode stub for `globalThis.queueMicrotask` (getter returning the function ref). Returns
      * a handle to an Undefined-ish sentinel since no real queue is running. The async runtime
      * overlay replaces this with a real implementation.
      */
    private val queueMicrotaskGetterStub: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(alloc(WbindgenAbi.Undefined).toLong)
    }

    /** Sync-mode stub for `queueMicrotask(cb)` invocation. Returns void. */
    private val queueMicrotaskInvokeStub: WasmFunctionHandle = new WasmFunctionHandle {
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
                case u: JsUint8Array  => Array(u.byteLength.toLong)
                case b: JsArrayBuffer => Array(b.bytes.length.toLong)
                case _                => Array(0L)
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
                case (r: JsRequest, o: JsObject) =>
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
      * `(request, i32) -> ()` and just stash the int on the [[JsRequest]] for the fetch bridge to
      * consult later. The int semantics come from `web_sys::RequestMode` etc. — they're opaque to
      * us until a real fetch is wired.
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

    private val localStorageGet: WasmFunctionHandle = freshHandle(JsLocalStorage)

    /** `__wbg_getItem_(ret_ptr, storage_handle, key_ptr, key_len)` — writes value or (0,0) to
      * retPtr.
      */
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

    /** `__wbg_getRandomValues_(ptr, len)` — fill WASM memory with cryptographically random bytes.
      */
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

        /** Scala-side settlement callbacks that fire when this promise transitions from pending to
          * either fulfilled or rejected. Used by [[MithrilAsyncRuntime]] to thread `.then(...)`
          * continuations and upstream-promise forwarding.
          */
        val pendingSettlers: mutable.ArrayBuffer[(AnyRef | Null, AnyRef | Null) => Unit] =
            mutable.ArrayBuffer.empty
    }

    object JsPromise {
        def resolved(v: AnyRef | Null): JsPromise =
            new JsPromise(pending = false, value = v, error = null)
    }

    /** A wasm-bindgen-emitted JS closure — a Rust function pointer pair plus the export names that
      * dispatch into it. On JVM, invoking the closure means calling the `invokeExport` function on
      * the Chicory [[com.dylibso.chicory.runtime.Instance]] with `(fnPtrA, fnPtrB, ...args)`;
      * destruction is a no-op (JVM GC handles the Scala side; the Rust side's ref-count decrement
      * goes via `__wbg_cb_unref_` which we already stub).
      */
    final case class JsClosure(
        fnPtrA: Int,
        fnPtrB: Int,
        invokeExport: String,
        destroyExport: String,
        arity: Int
    )

    /** Synthetic `BroadcastChannel`. No cross-process broadcast on JVM; the receiver API is enough
      * for Mithril's in-client feedback loop to stay internally consistent.
      */
    final case class JsBroadcastChannel(name: String)

    /** Zero-arg callable — stands in for a JS function looked up via `container[Symbol.iterator]`.
      * When invoked via `__wbg_call_` it produces an iterator (or other ref) by running `produce`.
      * Rust code uses this to duck-type a Headers / Map / Object as iterable; see
      * `js_sys::try_iter`.
      */
    final case class JsIterableFn(produce: () => AnyRef | Null)

    /** Sentinel returned by the `queueMicrotask` getter so later `fn.call(thisArg, cb)` sites can
      * recognise the target as "enqueue the callback". Lives here so [[WbindgenAbi.isFunction]] can
      * recognise it without cycling through `MithrilAsyncRuntime`.
      */
    object JsQueueMicrotaskFn {
        override def toString: String = "wbindgen.queueMicrotaskFn"
    }

    /** Render a JVM value as JSON. Used by `JSON.stringify` bridge. For our synthetic host types we
      * emit something sensible; for unknown refs we fall back to `String.valueOf`.
      */
    def jsonStringify(v: AnyRef | Null): String = v match {
        case null                 => "null"
        case Undefined            => "undefined"
        case s: String            => escapeJsonString(s)
        case b: java.lang.Boolean => if b.booleanValue then "true" else "false"
        case n: java.lang.Number =>
            val d = n.doubleValue
            if d.isNaN || d.isInfinite then "null"
            else if d == d.toLong.toDouble && math.abs(d) < 1e15 then d.toLong.toString
            else d.toString
        case a: JsArray =>
            a.items.iterator.map(jsonStringify).mkString("[", ",", "]")
        case o: JsObject =>
            o.entries.iterator
                .map { case (k, v) => s"${escapeJsonString(k)}:${jsonStringify(v)}" }
                .mkString("{", ",", "}")
        case m: JsMap =>
            m.entries.iterator
                .collect { case (k: String, v) => s"${escapeJsonString(k)}:${jsonStringify(v)}" }
                .mkString("{", ",", "}")
        case other => escapeJsonString(String.valueOf(other))
    }

    private def escapeJsonString(s: String): String = {
        val sb = new StringBuilder(s.length + 2)
        sb.append('"')
        s.foreach {
            case '"'           => sb.append("\\\"")
            case '\\'          => sb.append("\\\\")
            case '\n'          => sb.append("\\n")
            case '\r'          => sb.append("\\r")
            case '\t'          => sb.append("\\t")
            case c if c < 0x20 => sb.append(f"\\u$c%04x")
            case c             => sb.append(c)
        }
        sb.append('"')
        sb.toString
    }
}
