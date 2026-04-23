package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}
import scalus.cardano.node.stream.engine.snapshot.mithril.MithrilAsyncRuntime.{
    PromiseRejectCallback,
    PromiseResolveCallback
}
import scalus.cardano.node.stream.engine.snapshot.mithril.WbindgenAbi.*

import java.util.concurrent.{ArrayBlockingQueue, Executors, ThreadFactory}
import scala.concurrent.{ExecutionContext, Future, Promise as SPromise}

/** Asynchronous runtime for the Mithril WASM client.
  *
  * Owns a **single dedicated dispatcher thread** that is the only thread allowed to call into
  * the Chicory [[Instance]] — Chicory's VM is not thread-safe, so all WASM entry points and
  * host-function callbacks must run here. External callers submit work via [[submit]]; HTTP
  * completion and other I/O events post back to this thread to resume the Rust async state
  * machine.
  *
  * The runtime overlays a set of async-aware host imports on top of [[WbindgenAbi]]:
  *
  *   - `__wbg_new_ff12d2b041fb48f1` — `new Promise(executor)`: invokes the executor closure
  *     synchronously with synthesised `resolve` / `reject` callback handles, as JS does.
  *   - `__wbg_then_` / `__wbg_resolve_` — Promise composition.
  *   - `__wbg_call_` — invokes a [[JsClosure]] by looking up and calling the appropriate Rust
  *     `wasm_bindgen__convert__closures_____invoke__h*` export on the Instance, or invokes
  *     a synthetic resolve/reject callback.
  *   - `__wbg_queueMicrotask_` — enqueues onto an in-memory deque drained by [[drainMicrotasks]].
  *   - `__wbg_setTimeout_` — same as queueMicrotask today (no actual timing); revisit when any
  *     caller starts depending on real delays.
  *
  * See docs/local/claude/indexer/snapshot-bootstrap-m10.md for the overall M10b design.
  */
final class MithrilAsyncRuntime(
    val abi: WbindgenAbi,
    closureHashes: MithrilAsyncRuntime.ClosureHashes = MithrilAsyncRuntime.ClosureHashes.Release0_9_11
) {

    @volatile private var currentInstance: Instance = null.asInstanceOf[Instance]

    private val microtasks: ArrayBlockingQueue[() => Unit] = new ArrayBlockingQueue(1024)

    private val dispatcher = Executors.newSingleThreadExecutor(new ThreadFactory {
        def newThread(r: Runnable): Thread = {
            val t = new Thread(r, "mithril-wasm-dispatcher")
            t.setDaemon(true)
            t
        }
    })

    private given ExecutionContext = ExecutionContext.fromExecutorService(dispatcher)

    /** Register the instantiated WASM [[Instance]] — must be called exactly once, before any
      * `submit` or `drain` call.
      */
    def attach(instance: Instance): Unit = {
        require(currentInstance == null, "MithrilAsyncRuntime already attached")
        currentInstance = instance
    }

    /** Submit `body` for execution on the dispatcher thread; drain microtasks before returning
      * its result.
      */
    def submit[T](body: Instance => T): Future[T] = {
        require(currentInstance != null, "MithrilAsyncRuntime not attached — call attach() first")
        Future {
            val result = body(currentInstance)
            drainMicrotasks()
            result
        }
    }

    /** If `handle` refers to a [[JsPromise]], block on dispatcher thread (draining microtasks)
      * until it settles, then complete the returned Future with its value or error. Otherwise
      * complete with the raw value — some async APIs return a plain value sync-wrapped into a
      * Future for uniformity.
      */
    def awaitPromise[T](handle: Int)(decode: AnyRef => T): Future[T] = {
        val out = SPromise[T]()
        dispatcher.submit(new Runnable {
            def run(): Unit = {
                try {
                    drainMicrotasks()
                    val attempts = new java.util.concurrent.atomic.AtomicInteger(0)
                    while attempts.get() < MithrilAsyncRuntime.MaxDrainRounds do {
                        abi.get(handle) match {
                            case p: JsPromise =>
                                if !p.pending then {
                                    if p.error != null then
                                        out.failure(new RuntimeException(s"promise rejected: ${p.error}"))
                                    else out.success(decode(p.value))
                                    return
                                }
                            case other =>
                                out.success(decode(other))
                                return
                        }
                        drainMicrotasks()
                        attempts.incrementAndGet()
                    }
                    out.failure(
                      new RuntimeException(
                        s"MithrilAsyncRuntime: promise still pending after " +
                            s"${MithrilAsyncRuntime.MaxDrainRounds} drain rounds"
                      )
                    )
                } catch {
                    case t: Throwable => out.failure(t)
                }
            }
        })
        out.future
    }

    /** Host-import overlay. These override the synchronous stand-ins in [[WbindgenAbi]] with
      * real async-aware implementations; pass this map **last** when merging so it wins.
      *
      * Each handler is wrapped so the first call captures the live WASM Instance — same
      * convention as [[WbindgenAbi.captured]], and required so [[WbindgenAbi.alloc]] can route
      * through `__externref_table_alloc`.
      */
    def asyncImports: Map[String, WasmFunctionHandle] = {
        val raw: Seq[(String, WasmFunctionHandle)] = Seq(
          closureHashes.promiseExecutorImport -> newPromiseWithExecutor,
          "__wbg_then_" -> promiseThen,
          "__wbg_resolve_" -> promiseResolve,
          "__wbg_call_" -> callClosure,
          "__wbg_queueMicrotask_" -> queueMicrotask,
          "__wbg_setTimeout_" -> setTimeoutHandler
        )
        raw.map((name, h) => name -> wrapCapture(h)).toMap
    }

    private def wrapCapture(h: WasmFunctionHandle): WasmFunctionHandle =
        new WasmFunctionHandle {
            def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
                abi.captureForExtension(instance)
                h.apply(instance, args.map(_.toLong)*)
            }
        }

    // ------------------------------------------------------------------

    private def drainMicrotasks(): Unit = {
        var drained = 0
        while !microtasks.isEmpty && drained < MithrilAsyncRuntime.MaxMicrotasksPerDrain do {
            val task = microtasks.poll()
            if task != null then {
                try task()
                catch {
                    case t: Throwable =>
                        MithrilAsyncRuntime.logger.error(s"microtask failed: ${t.getMessage}", t)
                }
                drained += 1
            }
        }
    }

    /** `new Promise(executor)` — synthesise resolve/reject callbacks then invoke the executor
      * closure synchronously with them. The executor typically arranges an async event (fetch)
      * whose completion will later invoke resolve or reject.
      */
    private val newPromiseWithExecutor: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val promise = new JsPromise(pending = true, value = null, error = null)
            val resolveCb = PromiseResolveCallback(promise)
            val rejectCb = PromiseRejectCallback(promise)
            val resolveHandle = abi.alloc(resolveCb)
            val rejectHandle = abi.alloc(rejectCb)

            val executor = JsClosure(
              fnPtrA = args(0).toInt,
              fnPtrB = args(1).toInt,
              invokeExport = closureHashes.promiseExecutorInvoke,
              destroyExport = "",
              arity = 2
            )
            invokeClosure(instance, executor, Array(resolveHandle.toLong, rejectHandle.toLong))

            Array(abi.alloc(promise).toLong)
        }
    }

    /** `promise.then(onFulfilled[, onRejected])` — register continuations; if the promise is
      * already settled, schedule them as microtasks immediately.
      */
    private val promiseThen: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val upstream = abi.get(args(0).toInt) match {
                case p: JsPromise => p
                case other        => JsPromise.resolved(other)
            }
            val onFulfilled = closureFromHandle(args(1).toInt)
            val onRejected = if args.length >= 3 then closureFromHandle(args(2).toInt) else None

            val downstream = new JsPromise(pending = true, value = null, error = null)

            def fireFulfilled(value: AnyRef | Null): Unit =
                onFulfilled match {
                    case Some(cb) =>
                        microtasks.add(() =>
                            try {
                                val resultHandle = invokeClosure(
                                  currentInstance,
                                  cb,
                                  Array(abi.alloc(value).toLong)
                                )
                                settle(downstream, resultHandle)
                            } catch {
                                case t: Throwable => rejectWith(downstream, t)
                            }
                        )
                    case None => settleWith(downstream, value)
                }

            def fireRejected(err: AnyRef | Null): Unit =
                onRejected match {
                    case Some(cb) =>
                        microtasks.add(() =>
                            try {
                                val resultHandle = invokeClosure(
                                  currentInstance,
                                  cb,
                                  Array(abi.alloc(err).toLong)
                                )
                                settle(downstream, resultHandle)
                            } catch {
                                case t: Throwable => rejectWith(downstream, t)
                            }
                        )
                    case None => rejectRaw(downstream, err)
                }

            if !upstream.pending then {
                if upstream.error != null then fireRejected(upstream.error)
                else fireFulfilled(upstream.value)
            } else {
                upstream.continuations.append((
                  onFulfilled.map(_ => (() => fireFulfilled(upstream.value))),
                  onRejected.map(_ => (() => fireRejected(upstream.error)))
                ) match {
                    case (fOpt, rOpt) =>
                        // wrap the upstream settle callbacks back into JsClosure shape — not
                        // needed here since we directly use the Scala functions. Keeping shape
                        // as (Option[JsClosure], Option[JsClosure]) for interop with later
                        // callers; wrap `None` placeholders so settlement fires our Scala
                        // closures via the settle helper.
                        (None, None)
                })
                // Instead of trying to fit the scheduling into the JsClosure shape, record a
                // deferred action so settlement fires fireFulfilled / fireRejected directly.
                upstream.pendingSettlers.append((v: AnyRef | Null, e: AnyRef | Null) =>
                    if e != null then fireRejected(e) else fireFulfilled(v)
                )
            }

            Array(abi.alloc(downstream).toLong)
        }
    }

    private def settle(p: JsPromise, resultHandle: Long): Unit =
        settleWith(p, abi.get(resultHandle.toInt))

    private def settleWith(p: JsPromise, v: AnyRef | Null): Unit = {
        if !p.pending then return
        v match {
            case upstream: JsPromise =>
                if !upstream.pending then {
                    if upstream.error != null then rejectRaw(p, upstream.error)
                    else settleWith(p, upstream.value)
                } else {
                    upstream.pendingSettlers.append((uv, ue) =>
                        if ue != null then rejectRaw(p, ue) else settleWith(p, uv)
                    )
                }
            case other =>
                p.value = other
                p.pending = false
                firePendingSettlers(p)
        }
    }

    private def rejectWith(p: JsPromise, t: Throwable): Unit =
        rejectRaw(p, s"${t.getClass.getName}: ${t.getMessage}")

    private def rejectRaw(p: JsPromise, err: AnyRef | Null): Unit = {
        if !p.pending then return
        p.error = err
        p.pending = false
        firePendingSettlers(p)
    }

    private def firePendingSettlers(p: JsPromise): Unit = {
        val settlers = p.pendingSettlers.toSeq
        p.pendingSettlers.clear()
        settlers.foreach(fn => fn(p.value, p.error))
    }

    /** `Promise.resolve(v)` — if `v` is already a promise, pass through; else wrap. */
    private val promiseResolve: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = abi.get(args(0).toInt) match {
                case up: JsPromise => up
                case other         => JsPromise.resolved(other)
            }
            Array(abi.alloc(p).toLong)
        }
    }

    /** `fn.call(thisArg[, arg])` — dispatch through [[JsClosure]] (real Rust callback) or
      * synthetic resolve/reject callbacks.
      */
    private val callClosure: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val target = abi.get(args(0).toInt)
            // args(1) = thisArg (ignored on JVM). args(2..) = call args.
            val callArgs: Array[Long] =
                if args.length > 2 then Array.tabulate(args.length - 2)(i => args(i + 2).toLong)
                else Array.emptyLongArray
            target match {
                case closure: JsClosure =>
                    val resultHandle = invokeClosure(instance, closure, callArgs)
                    Array(resultHandle)
                case cb: PromiseResolveCallback =>
                    val v = if callArgs.nonEmpty then abi.get(callArgs(0).toInt) else null
                    settleWith(cb.promise, v)
                    Array(abi.alloc(WbindgenAbi.Undefined).toLong)
                case cb: PromiseRejectCallback =>
                    val e = if callArgs.nonEmpty then abi.get(callArgs(0).toInt) else null
                    rejectRaw(cb.promise, e)
                    Array(abi.alloc(WbindgenAbi.Undefined).toLong)
                case null =>
                    throw new RuntimeException(
                      s"__wbg_call_: target handle ${args(0).toInt} is null"
                    )
                case other =>
                    throw new RuntimeException(
                      s"__wbg_call_: target of type ${other.getClass.getName} is not callable"
                    )
            }
        }
    }

    private val queueMicrotask: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            // Signature variants: void or externref return. Arity-1 variant: (closure_handle).
            // We can't distinguish the closure/`queueMicrotask` getter form by arity, so check
            // the value at the handle — if it's a JsClosure, enqueue it.
            abi.get(args(0).toInt) match {
                case c: JsClosure =>
                    microtasks.add(() => invokeClosure(currentInstance, c, Array.emptyLongArray))
                case _ => ()
            }
            Array.emptyLongArray
        }
    }

    /** `setTimeout(callback, ms)` — schedule as a microtask (no delay for now). Returns a
      * dummy timer id.
      */
    private val setTimeoutHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            abi.get(args(0).toInt) match {
                case c: JsClosure =>
                    microtasks.add(() => invokeClosure(currentInstance, c, Array.emptyLongArray))
                case _ => ()
            }
            Array(abi.alloc(java.lang.Long.valueOf(args(1).toLong)).toLong)
        }
    }

    // ------------------------------------------------------------------

    private def closureFromHandle(h: Int): Option[JsClosure] = abi.get(h) match {
        case c: JsClosure => Some(c)
        case _            => None
    }

    /** Invoke the Rust-side closure by calling its `invokeExport` with `(fnPtrA, fnPtrB, ...args)`.
      * Returns the export's first result (a handle or 0), 0 for void-returning variants.
      */
    private def invokeClosure(
        instance: Instance,
        closure: JsClosure,
        args: Array[Long]
    ): Long = {
        val fn = instance.`export`(closure.invokeExport)
        if fn == null then
            throw new NoSuchElementException(
              s"WASM export '${closure.invokeExport}' not found (JsClosure invoke path)"
            )
        val allArgs = Array(closure.fnPtrA.toLong, closure.fnPtrB.toLong) ++ args
        val result = fn.apply(allArgs*)
        if result == null || result.isEmpty then 0L else result(0)
    }
}

object MithrilAsyncRuntime {

    /** Cap on microtask drain rounds in [[MithrilAsyncRuntime.awaitPromise]] — prevents a
      * runaway continuation chain from looping forever. If you trip this, check for an
      * accidentally re-armed continuation.
      */
    val MaxDrainRounds: Int = 1024

    /** Cap on microtasks drained in a single call — prevents a microtask that re-queues
      * itself indefinitely from starving the outer loop.
      */
    val MaxMicrotasksPerDrain: Int = 4096

    private val logger: scribe.Logger =
        scribe.Logger("scalus.cardano.node.stream.engine.snapshot.mithril.MithrilAsyncRuntime")

    /** Synthetic JS function produced by `new Promise(executor)` — when `resolve(v)` is called
      * from Rust, we mutate the associated [[JsPromise]] to its fulfilled state.
      */
    final case class PromiseResolveCallback(promise: JsPromise)

    /** Counterpart for `reject(err)`. */
    final case class PromiseRejectCallback(promise: JsPromise)

    /** Per-build closure-related hash mapping. wasm-bindgen rotates the 16-hex hash on every
      * Rust signature change AND between debug/release builds, so we keep a small struct
      * mapping the import names we register to the corresponding invoke-export names. Bump
      * this when refreshing the pinned WASM blob; the upstream JS glue
      * (mithril_client_wasm.js shipped alongside the .wasm) is the source of truth.
      */
    final case class ClosureHashes(
        promiseExecutorImport: String,
        promiseExecutorInvoke: String,
        oneArgClosureCastImport: String,
        oneArgClosureInvoke: String,
        oneArgClosureDestroy: String,
        zeroArgClosureCastImport: String,
        zeroArgClosureInvoke: String,
        zeroArgClosureDestroy: String
    )

    object ClosureHashes {

        /** Pinned upstream `@mithril-dev/mithril-client-wasm@0.9.11` release build. */
        val Release0_9_11: ClosureHashes = ClosureHashes(
          promiseExecutorImport = "__wbg_new_ff12d2b041fb48f1",
          promiseExecutorInvoke = "wasm_bindgen__convert__closures_____invoke__h2da143d4463a5f08",
          oneArgClosureCastImport = "__wbindgen_cast_17a320bf0cb03ca7",
          oneArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__hc0a74f7bb86030d0",
          oneArgClosureDestroy = "wasm_bindgen__closure__destroy__h99811cac73495ece",
          zeroArgClosureCastImport = "__wbindgen_cast_7fcb4b52657c40f7",
          zeroArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__h6b7e05d46d107c93",
          zeroArgClosureDestroy = "wasm_bindgen__closure__destroy__hea47394e049eff9b"
        )

        /** Locally-compiled debug build of mithril-client-wasm 0.9.11 with
          * `console_error_panic_hook` enabled. Used for diagnostic test paths only.
          */
        val Debug0_9_11: ClosureHashes = ClosureHashes(
          promiseExecutorImport = "__wbg_new_ff12d2b041fb48f1",
          promiseExecutorInvoke = "wasm_bindgen__convert__closures_____invoke__hf498985395075366",
          oneArgClosureCastImport = "__wbindgen_cast_2b3d1dcae2027ea1",
          oneArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__h83f64fd803aa6bb4",
          oneArgClosureDestroy = "wasm_bindgen__closure__destroy__he23eb76bd87c9db3",
          zeroArgClosureCastImport = "__wbindgen_cast_5c1cd1869e09aa29",
          zeroArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__ha680d4b0d17e7dc3",
          zeroArgClosureDestroy = "wasm_bindgen__closure__destroy__h4ed239079f93e789"
        )
    }
}
