package scalus.verify.lean

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.io.{BufferedInputStream, EOFException, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, LinkedBlockingQueue}
import java.util.concurrent.atomic.AtomicLong

/** A JSON value kept as the bytes it was written with. The parameters and the result of a message
  * pass through [[JsonRpcSession]] so, and are read by whoever knows their shape.
  */
private[lean] final class RawJson(val bytes: Array[Byte]) {

    def as[A](using JsonValueCodec[A]): A = readFromArray[A](bytes)

    override def toString: String = new String(bytes, StandardCharsets.UTF_8)
}

private[lean] object RawJson {

    def of[A](value: A)(using JsonValueCodec[A]): RawJson = new RawJson(writeToArray(value))

    given JsonValueCodec[RawJson] = new JsonValueCodec[RawJson] {
        override def decodeValue(in: JsonReader, default: RawJson): RawJson =
            new RawJson(in.readRawValAsBytes())
        override def encodeValue(value: RawJson, out: JsonWriter): Unit =
            out.writeRawVal(value.bytes)
        override def nullValue: RawJson = null
    }
}

/** A JSON-RPC 2.0 session over a pair of streams: one end of a connection, in the framing of the
  * Language Server Protocol, where every message is a JSON object after a `Content-Length` header
  * that counts its bytes.
  *
  * It has the state of the conversation: the ids it has given out, the requests that wait for their
  * answers, and whether the other end is still there. It starts when it is created, and ends when
  * `input` closes.
  *
  * A thread reads `input` for as long as it is open. It completes the request an answer belongs to,
  * hands a notification to `onNotification`, and answers a request of the other end's with a null
  * result: nothing it could ask for is offered here, and it waits for an answer.
  *
  * Another thread writes `output`. Whoever sends a message hands it over and goes on, so no sender
  * waits for the other end to read: not a caller with a time limit, and not the reading thread,
  * which would stop reading while it waited, and leave the other end unable to write.
  *
  * `onNotification` runs on the reading thread, so it must not wait for an answer itself.
  */
private[lean] final class JsonRpcSession(
    input: InputStream,
    output: OutputStream,
    onNotification: (String, Option[RawJson]) => Unit
) {
    import JsonRpcSession.*

    private val in = new BufferedInputStream(input)
    private val ids = new AtomicLong(0)
    private val waiting = new ConcurrentHashMap[Long, CompletableFuture[Answer]]()
    private val outgoing = new LinkedBlockingQueue[Array[Byte]]()
    @volatile private var end: Option[String] = None

    private val reader = new Thread(() => read(), "scalus-lean-server-output")
    private val writer = new Thread(() => write(), "scalus-lean-server-input")
    reader.setDaemon(true)
    writer.setDaemon(true)
    reader.start()
    writer.start()

    /** Why the session ended, once it has: nothing is sent or answered after. */
    def ended: Option[String] = end

    /** Sends a request. The future is completed with its answer, or with the reason there will be
      * none; it never fails.
      */
    def request(method: String, params: Option[RawJson]): CompletableFuture[Answer] = {
        val id = ids.incrementAndGet()
        val answer = new CompletableFuture[Answer]()
        waiting.put(id, answer)
        send(writeToArray(Outgoing("2.0", Some(id), method, params)))
        // The session may have ended before the request was among those that wait.
        end.foreach(reason => finish(id, Left(reason)))
        answer
    }

    def notify(method: String, params: Option[RawJson]): Unit =
        send(writeToArray(Outgoing("2.0", None, method, params)))

    private def send(body: Array[Byte]): Unit = if end.isEmpty then outgoing.put(body)

    private def write(): Unit =
        try
            while true do
                val body = outgoing.take()
                val header = s"Content-Length: ${body.length}\r\n\r\n"
                output.write(header.getBytes(StandardCharsets.US_ASCII))
                output.write(body)
                output.flush()
        catch
            case error: IOException => close(s"cannot write to the server: ${error.getMessage}")
            // The session has ended, and `close` has said why.
            case _: InterruptedException => ()

    private def finish(id: Long, answer: Answer): Unit =
        Option(waiting.remove(id)).foreach(_.complete(answer))

    /** Ends the session: every request that waits gets `reason`, as will every later one. */
    private def close(reason: String): Unit = {
        if end.isEmpty then end = Some(reason)
        waiting.keySet.forEach(id => finish(id, Left(end.get)))
        writer.interrupt()
    }

    private def read(): Unit = {
        @annotation.tailrec
        def messages(): Nothing = {
            handle(message())
            messages()
        }
        var reason = "the reading of the server's output stopped"
        try messages()
        catch
            case _: EOFException    => reason = "the server closed its output"
            case error: IOException => reason = s"cannot read from the server: ${error.getMessage}"
            case error: JsonReaderException =>
                reason = s"the server sent what is not a message: ${error.getMessage}"
        finally close(reason)
    }

    /** The body of the next message. Its header is lines that end in `\r\n`, up to an empty one. */
    private def message(): Array[Byte] = {
        var length = -1
        var line = headerLine()
        while line.nonEmpty do
            val (name, value) = line.span(_ != ':')
            if name.trim.equalsIgnoreCase("Content-Length") then
                length = value
                    .drop(1)
                    .trim
                    .toIntOption
                    .getOrElse(throw new IOException(s"a header that is no length: $line"))
            line = headerLine()
        if length < 0 then throw new IOException("a message without a Content-Length header")
        val body = in.readNBytes(length)
        if body.length < length then throw new EOFException()
        body
    }

    private def headerLine(): String = {
        val line = new StringBuilder
        var byte = in.read()
        while byte != '\n' do
            if byte < 0 then throw new EOFException()
            if byte != '\r' then line.append(byte.toChar)
            byte = in.read()
        line.toString
    }

    private def handle(body: Array[Byte]): Unit = {
        val incoming = readFromArray[Incoming](body)
        (incoming.id, incoming.method) match
            case (Some(id), Some(_)) =>
                val answer = s"""{"jsonrpc":"2.0","id":$id,"result":null}"""
                send(answer.getBytes(StandardCharsets.UTF_8))
            // The ids of the requests sent here are numbers; another id answers nothing of ours.
            case (Some(id), None) =>
                id.toString.toLongOption.foreach { number =>
                    val answer: Answer = incoming.error match
                        case Some(error) => Left(s"${error.message} (${error.code})")
                        case None        => Right(incoming.result)
                    finish(number, answer)
                }
            case (None, Some(method)) => onNotification(method, incoming.params)
            case (None, None)         => ()
    }
}

private[lean] object JsonRpcSession {

    /** The result of a request, `None` where it is null, or why there is none: the error the other
      * end answered with, or the reason the session ended.
      */
    type Answer = Either[String, Option[RawJson]]

    private final case class Outgoing(
        jsonrpc: String,
        id: Option[Long],
        method: String,
        params: Option[RawJson]
    )

    private final case class ResponseError(code: Int, message: String)

    /** Any message that comes in: a request has an id and a method, a notification a method, and an
      * answer an id.
      */
    private final case class Incoming(
        id: Option[RawJson],
        method: Option[String],
        params: Option[RawJson],
        result: Option[RawJson],
        error: Option[ResponseError]
    )

    private given JsonValueCodec[Outgoing] = JsonCodecMaker.make
    private given JsonValueCodec[Incoming] = JsonCodecMaker.make
}
