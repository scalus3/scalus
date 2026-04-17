package scalus.cardano.n2n

import io.bullet.borer.{Borer, Cbor as Cborer, Decoder, Encoder}
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString

import scala.concurrent.{ExecutionContext, Future}

/** Connection-fatal CBOR framing error on a mini-protocol stream. Carries the protocol on which the
  * failure occurred so the multiplexer's root-cancel path has enough context; the wrapped cause is
  * either a borer error (malformed bytes) or `null` (partial message at EOF — see message).
  */
final class FrameDecodeException(protocol: MiniProtocolId, message: String, cause: Throwable)
    extends RuntimeException(s"CBOR framing failure on $protocol: $message", cause) {
    def this(protocol: MiniProtocolId, cause: Throwable) =
        this(protocol, "decode error", cause)
}

/** CBOR-aware view of a [[MiniProtocolBytes]] stream. SDU boundaries do not align with CBOR message
  * boundaries: a single message can span multiple SDUs, or multiple messages can arrive in one SDU.
  * This class buffers raw bytes delivered by the underlying handle, tries to decode a full message
  * on each refill, and yields exactly one `M` per [[receive]] call regardless of where the bytes
  * came from.
  *
  * Decode policy:
  *   - `Cbor.decode(view).withPrefixOnly.to[M].valueAndInputEither` is invoked on the accumulated
  *     buffer. `withPrefixOnly` allows trailing bytes (pipelined messages) to remain after a
  *     successful decode.
  *   - Success → commit the consumed prefix, return the value. Leftover bytes stay in the buffer
  *     for the next call.
  *   - [[Borer.Error.UnexpectedEndOfInput]] → the buffer holds a complete prefix but not a complete
  *     message; pull another chunk from the underlying handle and retry.
  *   - Any other borer error → genuine malformed framing; surface [[FrameDecodeException]] via the
  *     returned `Future` so the caller (typically a state machine driver) can escalate via
  *     [[RoutingOps.escalateRoot]].
  *
  * Thread-safety: callers must not issue overlapping [[receive]] calls on the same stream — state
  * machines are single-consumer by construction. [[send]] is safe to call from any thread because
  * it delegates to [[MiniProtocolBytes.send]], which serialises through the mux write queue.
  *
  * See `docs/local/claude/indexer/n2n-transport.md` § *CBOR framing across SDUs*.
  */
final class CborMessageStream[M](
    protocol: MiniProtocolId,
    handle: MiniProtocolBytes,
    initialCapacity: Int = 512
)(using encoder: Encoder[M], decoder: Decoder[M], ec: ExecutionContext) {

    /** Protocol-level cancel scope — identical to the underlying handle's scope, re-exposed so
      * state machines don't need to keep the raw handle around.
      */
    def scope: CancelToken = handle.scope

    // Accumulator for bytes that arrived but haven't yet been decoded into a message. `storage` is
    // the backing array (grows geometrically on demand) and `size` is the logical length of
    // unconsumed bytes at the head. All mutations happen on the calling thread; single-consumer
    // discipline is a contract of this class.
    private var storage: Array[Byte] = new Array[Byte](initialCapacity)
    private var size: Int = 0

    /** Decode and return the next message from the stream, pulling additional SDU payloads as
      * needed.
      *
      *   - `Future(Some(m))` when a message decodes.
      *   - `Future(None)` when the underlying handle hits end-of-stream before a full message is
      *     available. If any bytes remain buffered at EOF the future fails with
      *     [[FrameDecodeException]] (partial message mid-stream is a framing error).
      *   - `Future.failed(FrameDecodeException)` on malformed CBOR.
      *   - `Future.failed(CancelledException)` on scope cancel.
      */
    def receive(cancel: CancelToken = scope): Future[Option[M]] = tryDecodeOrPull(cancel)

    /** Encode and send one message. Delegates to the underlying handle; the mux splits the encoded
      * bytes across SDUs if they exceed [[Sdu.MaxPayloadSize]].
      */
    def send(message: M, cancel: CancelToken = scope): Future[Unit] =
        handle.send(Cbor.encodeToByteString(message), cancel)

    private def tryDecodeOrPull(cancel: CancelToken): Future[Option[M]] = {
        if size > 0 then
            tryDecode() match {
                case DecodeOutcome.Decoded(m) => return Future.successful(Some(m))
                case DecodeOutcome.NeedMore   => () // fall through to pull
                case DecodeOutcome.Fatal(t) =>
                    return Future.failed(new FrameDecodeException(protocol, t))
            }
        // Buffer doesn't yet hold a full message (or is empty) — pull another chunk and retry.
        handle.receive(cancel).flatMap {
            case None =>
                if size == 0 then Future.successful(None)
                else
                    Future.failed(
                      new FrameDecodeException(
                        protocol,
                        s"partial CBOR message at EOF ($size bytes buffered)",
                        null
                      )
                    )
            case Some(chunk) =>
                appendChunk(chunk)
                tryDecodeOrPull(cancel)
        }
    }

    private sealed trait DecodeOutcome
    private object DecodeOutcome {
        final case class Decoded(m: M) extends DecodeOutcome
        case object NeedMore extends DecodeOutcome
        final case class Fatal(t: Throwable) extends DecodeOutcome
    }

    private def tryDecode(): DecodeOutcome = {
        // Copy out exactly the active prefix — borer's `decode(Array[Byte])` reads the whole
        // array regardless of `size`, so we need a right-sized view. `withPrefixOnly` lets borer
        // stop after the first message instead of failing on leftover bytes — crucial when
        // multiple pipelined messages arrive in the same SDU.
        val view = java.util.Arrays.copyOfRange(storage, 0, size)
        Cborer.decode(view).withPrefixOnly.to[M].valueAndInputEither match {
            case Right((value, input)) =>
                val cursorPos = input.cursor.toInt
                val leftover = size - cursorPos
                if leftover > 0 then System.arraycopy(storage, cursorPos, storage, 0, leftover)
                size = leftover
                DecodeOutcome.Decoded(value)
            case Left(_: Borer.Error.UnexpectedEndOfInput[?]) => DecodeOutcome.NeedMore
            case Left(err)                                    => DecodeOutcome.Fatal(err)
        }
    }

    private def appendChunk(chunk: ByteString): Unit = {
        val bytes = chunk.bytes
        ensureCapacity(size + bytes.length)
        System.arraycopy(bytes, 0, storage, size, bytes.length)
        size += bytes.length
    }

    private def ensureCapacity(needed: Int): Unit = {
        if needed > storage.length then {
            var newCap = storage.length * 2
            while newCap < needed do newCap *= 2
            val grown = new Array[Byte](newCap)
            System.arraycopy(storage, 0, grown, 0, size)
            storage = grown
        }
    }
}
