package scalus.cardano.node.stream.engine.snapshot.mithril

import com.dylibso.chicory.runtime.{Instance, WasmFunctionHandle}
import scalus.cardano.node.stream.engine.snapshot.mithril.MithrilAsyncRuntime.{PromiseRejectCallback, PromiseResolveCallback}
import scalus.cardano.node.stream.engine.snapshot.mithril.WbindgenAbi.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.util.concurrent.{ArrayBlockingQueue, Executors, ThreadFactory}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise as SPromise}

/** Asynchronous runtime for the Mithril WASM client.
  *
  * Owns a **single dedicated dispatcher thread** that is the only thread allowed to call into the
  * Chicory [[Instance]] — Chicory's VM is not thread-safe, so all WASM entry points and
  * host-function callbacks must run here. External callers submit work via [[submit]]; HTTP
  * completion and other I/O events post back to this thread to resume the Rust async state machine.
  *
  * The runtime overlays a set of async-aware host imports on top of [[WbindgenAbi]]:
  *
  *   - `__wbg_new_ff12d2b041fb48f1` — `new Promise(executor)`: invokes the executor closure
  *     synchronously with synthesised `resolve` / `reject` callback handles, as JS does.
  *   - `__wbg_then_` / `__wbg_resolve_` — Promise composition.
  *   - `__wbg_call_` — invokes a [[JsClosure]] by looking up and calling the appropriate Rust
  *     `wasm_bindgen__convert__closures_____invoke__h*` export on the Instance, or invokes a
  *     synthetic resolve/reject callback.
  *   - `__wbg_queueMicrotask_` — enqueues onto an in-memory deque drained by [[drainMicrotasks]].
  *   - `__wbg_setTimeout_` — same as queueMicrotask today (no actual timing); revisit when any
  *     caller starts depending on real delays.
  *
  * See docs/local/claude/indexer/snapshot-bootstrap-m10.md for the overall M10b design.
  */
final class MithrilAsyncRuntime(
    val abi: WbindgenAbi,
    closureHashes: MithrilAsyncRuntime.ClosureHashes =
        MithrilAsyncRuntime.ClosureHashes.Release0_10_4
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

    /** Release the dispatcher thread. Idempotent. Subsequent `submit` / `awaitPromise` calls will
      * be rejected by the executor.
      */
    def close(): Unit = dispatcher.shutdown()

    /** Submit `body` for execution on the dispatcher thread; drain microtasks before returning its
      * result.
      */
    def submit[T](body: Instance => T): Future[T] = {
        require(currentInstance != null, "MithrilAsyncRuntime not attached — call attach() first")
        Future {
            val result = body(currentInstance)
            drainMicrotasks()
            result
        }
    }

    /** Post `body` to the dispatcher thread — used by I/O completions (HTTP responses, timer fires)
      * to funnel back onto the single-threaded WASM executor. Microtasks are drained after `body`
      * runs, so any promise settlement inside it fans out its `.then` chains before the next
      * dispatcher task.
      */
    private def postToDispatcher(body: => Unit): Unit = {
        dispatcher.submit(new Runnable {
            def run(): Unit = {
                try body
                catch {
                    case t: Throwable =>
                        MithrilAsyncRuntime.logger.error(
                          s"dispatcher task failed: ${t.getMessage}",
                          t
                        )
                }
                drainMicrotasks()
            }
        })
    }

    /** If `handle` refers to a [[JsPromise]], complete the returned Future when it settles (via
      * [[JsPromise.pendingSettlers]]). Otherwise complete with the raw value — some async APIs
      * return a plain value sync-wrapped into a Future for uniformity.
      *
      * Event-driven, not polled: the dispatcher thread returns immediately after registering the
      * settler, so pending HTTP completions queued on the dispatcher run freely and eventually
      * trigger the promise transition.
      */
    def awaitPromise[T](handle: Int)(decode: AnyRef | Null => T): Future[T] = {
        val out = SPromise[T]()
        dispatcher.submit(new Runnable {
            def run(): Unit = {
                try {
                    drainMicrotasks()
                    abi.get(handle) match {
                        case p: JsPromise if !p.pending =>
                            completeFromPromise(p, decode, out)
                        case p: JsPromise =>
                            p.pendingSettlers.append { (v, e) =>
                                if e != null then
                                    out.failure(new RuntimeException(s"promise rejected: $e"))
                                else
                                    try out.success(decode(v))
                                    catch { case t: Throwable => out.failure(t) }
                            }
                        case other =>
                            try out.success(decode(other))
                            catch { case t: Throwable => out.failure(t) }
                    }
                } catch {
                    case t: Throwable => out.failure(t)
                }
            }
        })
        out.future
    }

    private def completeFromPromise[T](
        p: JsPromise,
        decode: AnyRef | Null => T,
        out: SPromise[T]
    ): Unit =
        if p.error != null then out.failure(new RuntimeException(s"promise rejected: ${p.error}"))
        else
            try out.success(decode(p.value))
            catch { case t: Throwable => out.failure(t) }

    /** Host-import overlay. These override the synchronous stand-ins in [[WbindgenAbi]] with real
      * async-aware implementations; pass this map **last** when merging so it wins.
      *
      * Each handler is wrapped so the first call captures the live WASM Instance — same convention
      * as [[WbindgenAbi.captured]], and required so [[WbindgenAbi.alloc]] can route through
      * `__externref_table_alloc`.
      */
    def asyncImports: Map[String, WasmFunctionHandle] = {
        val raw: Seq[(String, WasmFunctionHandle)] = Seq(
          closureHashes.promiseExecutorImport -> newPromiseWithExecutor,
          "__wbg_then_" -> promiseThen,
          "__wbg_resolve_" -> promiseResolve,
          "__wbg_call_" -> callClosure,
          // Two upstream queueMicrotask variants SHARE arity (both (ref) -> ...) but differ
          // in return shape: the getter form returns a ref, the invocation form returns
          // void. Same short name — must be registered by full hash to disambiguate.
          "__wbg_queueMicrotask_9b549dfce8865860" -> queueMicrotaskGetter,
          "__wbg_queueMicrotask_fca69f5bfad613a5" -> queueMicrotaskInvoke,
          "__wbg_queueMicrotask_f8819e5ffc402f36" -> queueMicrotaskGetter,
          "__wbg_queueMicrotask_5d15a957e6aa920e" -> queueMicrotaskInvoke,
          "__wbg_setTimeout_" -> setTimeoutHandler,
          "__wbg_fetch_" -> fetchHandler,
          "__wbg_text_" -> responseText,
          "__wbg_arrayBuffer_" -> responseArrayBuffer,
          "__wbg_close_" -> streamClose,
          "__wbg_enqueue_" -> streamEnqueue,
          "__wbg_byobRequest_" -> streamByobRequest,
          "__wbg_respond_" -> streamByobRespond,
          "__wbg_view_" -> streamByobView
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
                upstream.continuations.append(
                  (
                    onFulfilled.map(_ => () => fireFulfilled(upstream.value)),
                    onRejected.map(_ => () => fireRejected(upstream.error))
                  ) match {
                      case (fOpt, rOpt) =>
                          // wrap the upstream settle callbacks back into JsClosure shape — not
                          // needed here since we directly use the Scala functions. Keeping shape
                          // as (Option[JsClosure], Option[JsClosure]) for interop with later
                          // callers; wrap `None` placeholders so settlement fires our Scala
                          // closures via the settle helper.
                          (None, None)
                  }
                )
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

    /** `fn.call(thisArg[, arg])` — dispatch through [[JsClosure]] (real Rust callback) or synthetic
      * resolve/reject callbacks.
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
                case fn: JsIterableFn =>
                    // Zero-arg bound-method invocation — e.g. `headers[Symbol.iterator]()`
                    // produces the iterator. Args beyond `this` are ignored; Rust's
                    // try_iter path always calls with zero args.
                    Array(abi.alloc(fn.produce()).toLong)
                case JsQueueMicrotaskFn =>
                    // `globalThis.queueMicrotask.call(thisArg, cb)` — treat as
                    // queueMicrotask(cb).
                    if callArgs.length >= 1 then {
                        abi.get(callArgs(0).toInt) match {
                            case c: JsClosure =>
                                microtasks.add(() =>
                                    invokeClosure(currentInstance, c, Array.emptyLongArray)
                                )
                            case _ => ()
                        }
                    }
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

    /** `globalThis.queueMicrotask` — getter for the function. Returns an externref to a sentinel
      * we'll later invoke when it's `.call(thisArg, cb)`-ed.
      */
    private val queueMicrotaskGetter: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] =
            Array(abi.alloc(JsQueueMicrotaskFn).toLong)
    }

    /** `queueMicrotask(cb)` — enqueue the closure onto our dispatcher. Returns void. */
    private val queueMicrotaskInvoke: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            abi.get(args(0).toInt) match {
                case c: JsClosure =>
                    microtasks.add(() => invokeClosure(currentInstance, c, Array.emptyLongArray))
                case _ => ()
            }
            Array.emptyLongArray
        }
    }

    /** `setTimeout(callback, ms)` — schedule as a microtask (no delay for now). Returns a dummy
      * timer id.
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

    // ------------------------------------------------------------------
    // fetch / Response body bridges.
    // ------------------------------------------------------------------

    private lazy val httpClient: HttpClient = HttpClient
        .newBuilder()
        .connectTimeout(Duration.ofSeconds(MithrilAsyncRuntime.HttpConnectTimeoutSeconds))
        .build()

    /** Handles both `fetch(request)` and `fetch(globalThis, request)` — the short-name collision is
      * resolved by arity. Settlement posts back to the dispatcher thread.
      */
    private val fetchHandler: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val requestHandle = if args.length >= 2 then args(1).toInt else args(0).toInt
            val promise = new JsPromise(pending = true, value = null, error = null)
            val promiseHandle = abi.alloc(promise)
            abi.get(requestHandle) match {
                case req: JsRequest =>
                    MithrilAsyncRuntime.logger.debug(
                      s"fetch ${req.method.toUpperCase} ${req.url}"
                    )
                    issueHttpRequest(req, promise)
                case other =>
                    postToDispatcher {
                        rejectRaw(promise, s"fetch: expected JsRequest, got $other")
                    }
            }
            Array(promiseHandle.toLong)
        }
    }

    private def issueHttpRequest(req: JsRequest, promise: JsPromise): Unit = {
        val httpReq =
            try buildHttpRequest(req)
            catch {
                case t: Throwable =>
                    postToDispatcher {
                        rejectRaw(promise, s"fetch setup failed: ${t.getMessage}")
                    }
                    return
            }
        httpClient
            .sendAsync(httpReq, HttpResponse.BodyHandlers.ofByteArray())
            .whenComplete { (resp, ex) =>
                if ex != null then {
                    MithrilAsyncRuntime.logger.warn(
                      s"fetch ${req.url} failed: ${unwrap(ex).getMessage}"
                    )
                    postToDispatcher {
                        rejectRaw(promise, s"fetch failed: ${unwrap(ex).getMessage}")
                    }
                } else {
                    MithrilAsyncRuntime.logger.debug(
                      s"fetch ${req.url} → ${resp.statusCode} (${resp.body.length} bytes)"
                    )
                    postToDispatcher {
                        settleWith(promise, toJsResponse(req.url, resp))
                    }
                }
            }
    }

    private def buildHttpRequest(req: JsRequest): HttpRequest = {
        val uri = URI.create(req.url)
        var builder = HttpRequest
            .newBuilder(uri)
            .timeout(Duration.ofSeconds(MithrilAsyncRuntime.HttpRequestTimeoutSeconds))
        req.headers.entries.foreach { case (name, value) =>
            if !MithrilAsyncRuntime.restrictedHeaders.contains(name.toLowerCase) then
                builder = builder.header(name, value)
        }
        val bodyPublisher = req.body match {
            case Some(s: String) => HttpRequest.BodyPublishers.ofString(s)
            case Some(u: JsUint8Array) =>
                val arr = java.util.Arrays.copyOfRange(
                  u.buffer.bytes,
                  u.byteOffset,
                  u.byteOffset + u.byteLength
                )
                HttpRequest.BodyPublishers.ofByteArray(arr)
            case Some(b: JsArrayBuffer) => HttpRequest.BodyPublishers.ofByteArray(b.bytes)
            case _                      => HttpRequest.BodyPublishers.noBody()
        }
        val method = Option(req.method).filter(_.nonEmpty).getOrElse("GET").toUpperCase
        builder = builder.method(method, bodyPublisher)
        builder.build()
    }

    private def toJsResponse(url: String, resp: HttpResponse[Array[Byte]]): JsResponse = {
        val headers = JsHeaders(mutable.LinkedHashMap.empty[String, String])
        var encoding: String | Null = null
        resp.headers.map.forEach { (name, values) =>
            // HTTP/2 pseudo-headers (`:status`, `:authority`, …) are exposed by
            // `java.net.http.HttpClient` but Rust's http crate rejects colon-prefixed names
            // as invalid — strip them to match browser-fetch semantics.
            if !values.isEmpty && !name.startsWith(":") then {
                // Strip content-encoding: Java's HttpClient does not auto-decompress, so
                // we decode host-side and omit the header so Rust sees raw bytes + matching
                // metadata.
                if name.equalsIgnoreCase("content-encoding") then encoding = values.get(0)
                else headers.entries(name) = values.get(0)
            }
        }
        val decodedBody =
            if encoding != null && encoding.nn.trim.equalsIgnoreCase("gzip") then
                decompressGzip(resp.body)
            else resp.body
        new JsResponse(
          url = url,
          status = resp.statusCode,
          headers = headers,
          body = decodedBody
        )
    }

    private def decompressGzip(data: Array[Byte]): Array[Byte] = {
        if data.isEmpty then data
        else {
            val in = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(data))
            try in.readAllBytes()
            finally in.close()
        }
    }

    private def unwrap(t: Throwable): Throwable = t match {
        case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
        case other                                                               => other
    }

    /** `response.text()` — promise resolves immediately since body is already buffered. */
    private val responseText: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = abi.get(args(0).toInt) match {
                case r: JsResponse =>
                    JsPromise.resolved(new String(r.body, java.nio.charset.StandardCharsets.UTF_8))
                case other =>
                    rejectedPromise(s"Response.text: expected JsResponse, got $other")
            }
            Array(abi.alloc(p).toLong)
        }
    }

    /** `response.arrayBuffer()` — return buffered body as [[JsArrayBuffer]]. */
    private val responseArrayBuffer: WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = {
            val p = abi.get(args(0).toInt) match {
                case r: JsResponse => JsPromise.resolved(JsArrayBuffer(r.body))
                case other =>
                    rejectedPromise(s"Response.arrayBuffer: expected JsResponse, got $other")
            }
            Array(abi.alloc(p).toLong)
        }
    }

    private def rejectedPromise(msg: String): JsPromise = {
        val p = new JsPromise(pending = true, value = null, error = null)
        rejectRaw(p, msg)
        p
    }

    /** ReadableStream placeholders — small JSON endpoints use `.text()` / `.arrayBuffer()`, so the
      * streaming path is dormant. Return inert values to satisfy the collision detector; real
      * implementations land with snapshot-download integration.
      */
    private def stubStream(result: => Array[Long]): WasmFunctionHandle = new WasmFunctionHandle {
        def apply(instance: Instance, args: Array[? <: Long]): Array[Long] = result
    }

    private val streamClose: WasmFunctionHandle = stubStream(Array.emptyLongArray)
    private val streamEnqueue: WasmFunctionHandle = stubStream(Array.emptyLongArray)
    private val streamByobRequest: WasmFunctionHandle = stubStream(Array(0L))
    private val streamByobRespond: WasmFunctionHandle = stubStream(Array.emptyLongArray)
    private val streamByobView: WasmFunctionHandle = stubStream(Array(0L))
}

object MithrilAsyncRuntime {

    /** Cap on microtasks drained in a single call — prevents a microtask that re-queues itself
      * indefinitely from starving the outer loop.
      */
    val MaxMicrotasksPerDrain: Int = 4096

    /** Connect-phase timeout for HTTP requests issued through the fetch bridge. */
    val HttpConnectTimeoutSeconds: Long = 30L

    /** End-to-end request timeout. Must accommodate the slowest aggregator endpoints (certificate
      * list on a cold mainnet can approach tens of seconds).
      */
    val HttpRequestTimeoutSeconds: Long = 120L

    /** Headers `java.net.http.HttpClient` refuses to let the caller set, plus `accept-encoding` —
      * we drop it from forwarded requests so the JDK client stays on its default (no compression)
      * and the Rust side always sees raw bytes.
      */
    val restrictedHeaders: Set[String] =
        Set(
          "host",
          "connection",
          "content-length",
          "upgrade",
          "expect",
          "transfer-encoding",
          "accept-encoding"
        )

    private val logger: scribe.Logger =
        scribe.Logger("scalus.cardano.node.stream.engine.snapshot.mithril.MithrilAsyncRuntime")

    /** Synthetic JS function produced by `new Promise(executor)` — when `resolve(v)` is called from
      * Rust, we mutate the associated [[JsPromise]] to its fulfilled state.
      */
    final case class PromiseResolveCallback(promise: JsPromise)

    /** Counterpart for `reject(err)`. */
    final case class PromiseRejectCallback(promise: JsPromise)

    /** Per-build closure-related hash mapping. wasm-bindgen rotates the 16-hex hash on every Rust
      * signature change AND between debug/release builds, so we keep a small struct mapping the
      * import names we register to the corresponding invoke-export names. Bump this when refreshing
      * the pinned WASM blob; the upstream JS glue (mithril_client_wasm.js shipped alongside the
      * .wasm) is the source of truth.
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

        /** Pinned upstream `@mithril-dev/mithril-client-wasm@0.10.4` release build.
          *
          * 0.10.4 rewrote the closure ABI: cast intrinsics now use zero-padded slot indices
          * (`__wbindgen_cast_0000000000000001`) instead of per-closure hex hashes, and a single
          * generic `__wbindgen_destroy_closure` replaces the per-closure destroy exports. The
          * destroy fields below hold that generic name so the interface is preserved; callers don't
          * need to know about the rename.
          */
        val Release0_10_4: ClosureHashes = ClosureHashes(
          promiseExecutorImport = "__wbg_new_typed_323f37fd55ab048d",
          promiseExecutorInvoke = "wasm_bindgen__convert__closures_____invoke__he20fbab6b1673584",
          oneArgClosureCastImport = "__wbindgen_cast_0000000000000001",
          oneArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__h023fd7017d50e9e0",
          oneArgClosureDestroy = "__wbindgen_destroy_closure",
          zeroArgClosureCastImport = "__wbindgen_cast_0000000000000002",
          zeroArgClosureInvoke = "wasm_bindgen__convert__closures_____invoke__hc4b57cdb0d692e9e",
          zeroArgClosureDestroy = "__wbindgen_destroy_closure"
        )
    }
}
