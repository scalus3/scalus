package scalus.cardano.n2n

import io.bullet.borer.{Cbor as Cborer, Decoder, Encoder}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}
import scalus.cardano.infra.{CancelSource, CancelToken, CancelledException}
import scalus.uplc.builtin.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/** Covers [[CborMessageStream]]: incremental decode across arbitrary chunk boundaries, multi
  * message pipelining in a single pull, clean EOF, malformed-bytes surfacing as
  * [[FrameDecodeException]], partial-message EOF surfacing as [[FrameDecodeException]].
  */
class CborMessageStreamSuite extends AnyFunSuite with ScalaFutures {

    implicit override val patienceConfig: PatienceConfig =
        PatienceConfig(timeout = Span(2, Seconds), interval = Span(10, Millis))

    /** Simple test message with a few mixed-width fields — exercises borer's `ArrayBasedCodecs`
      * derivation (via `Decoder.derived`) and ensures the serialized form is non-trivially
      * multi-byte.
      */
    private final case class Ping(id: Int, tag: String)

    private object Ping {
        import io.bullet.borer.derivation.ArrayBasedCodecs.*
        given Encoder[Ping] = deriveEncoder[Ping]
        given Decoder[Ping] = deriveDecoder[Ping]
    }

    /** Test-only byte-stream backing store: a queue of chunks with a [[Promise]]-chained pull
      * semantics. [[push]] deposits a chunk; each `pull()` call takes from the queue or suspends
      * until the next `push`/`eof`. This lets tests drive chunk arrival timing explicitly.
      */
    private final class QueueBytes extends MiniProtocolBytes {
        private val lock = new AnyRef
        private val chunks = mutable.ArrayDeque.empty[Option[ByteString]]
        private var pending: Option[Promise[Option[ByteString]]] = None
        private val source = CancelSource()

        def cancelScope: CancelToken = source.token

        def receive(cancel: CancelToken = cancelScope): Future[Option[ByteString]] =
            lock.synchronized {
                if cancel.isCancelled then Future.failed(CancelledException(TestReason))
                else if chunks.nonEmpty then Future.successful(chunks.removeHead())
                else {
                    val p = Promise[Option[ByteString]]()
                    pending = Some(p)
                    p.future
                }
            }

        def send(message: ByteString, cancel: CancelToken = cancelScope): Future[Unit] = Future.unit

        def push(chunk: ByteString): Unit = offer(Some(chunk))
        def eof(): Unit = offer(None)

        private def offer(value: Option[ByteString]): Unit = lock.synchronized {
            pending match {
                case Some(p) =>
                    pending = None
                    p.success(value)
                case None => chunks.append(value)
            }
        }
    }

    private val TestReason = "CborMessageStreamSuite cancel"

    private def encodePing(p: Ping): Array[Byte] = Cborer.encode(p).toByteArray

    test("decodes a message delivered in a single chunk") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        q.push(ByteString.unsafeFromArray(encodePing(Ping(1, "hello"))))
        assert(stream.receive().futureValue.contains(Ping(1, "hello")))
    }

    test("decodes a message split across every byte boundary — single byte at a time") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        val encoded = encodePing(Ping(42, "boundary-test-string"))
        val fut = stream.receive()
        // Feed one byte per push — all but the last produce NeedMore internally.
        for b <- encoded do q.push(ByteString.unsafeFromArray(Array(b)))
        assert(fut.futureValue.contains(Ping(42, "boundary-test-string")))
    }

    test("two messages concatenated in one chunk are decoded in order without re-pulling") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        val bytes = encodePing(Ping(1, "a")) ++ encodePing(Ping(2, "b"))
        q.push(ByteString.unsafeFromArray(bytes))
        assert(stream.receive().futureValue.contains(Ping(1, "a")))
        // Second receive must decode from the leftover buffer — do NOT push another chunk.
        assert(stream.receive().futureValue.contains(Ping(2, "b")))
    }

    test("clean EOF with empty buffer yields None") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        q.eof()
        assert(stream.receive().futureValue.isEmpty)
    }

    test("EOF mid-message surfaces FrameDecodeException") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        val encoded = encodePing(Ping(1, "truncated"))
        q.push(ByteString.unsafeFromArray(encoded.take(encoded.length - 2)))
        q.eof()
        val ex = stream.receive().failed.futureValue
        assert(ex.isInstanceOf[FrameDecodeException])
    }

    test("malformed CBOR surfaces FrameDecodeException without further pulls") {
        val q = new QueueBytes
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, q)
        // 0xFF is `stop` code — invalid as a top-level item; borer reports it as malformed.
        // Push at least two bytes so we're past the "need more input" boundary and into
        // genuine decode failure.
        q.push(ByteString.unsafeFromArray(Array[Byte](0xff.toByte, 0x00.toByte)))
        val ex = stream.receive().failed.futureValue
        assert(ex.isInstanceOf[FrameDecodeException])
    }

    test("send encodes via borer and delegates to handle") {
        val sent = new java.util.concurrent.atomic.AtomicReference[Option[ByteString]](None)
        val handle = new MiniProtocolBytes {
            def cancelScope: CancelToken = CancelToken.never
            def receive(cancel: CancelToken = cancelScope): Future[Option[ByteString]] =
                Future.failed(new UnsupportedOperationException)
            def send(message: ByteString, cancel: CancelToken = cancelScope): Future[Unit] = {
                val _ = sent.getAndSet(Some(message))
                Future.unit
            }
        }
        val stream = new CborMessageStream[Ping](MiniProtocolId.Handshake, handle)
        stream.send(Ping(7, "send-me")).futureValue
        val bytes = sent.get.get.bytes
        val decoded = Cborer.decode(bytes).to[Ping].value
        assert(decoded == Ping(7, "send-me"))
    }
}
