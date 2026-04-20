package scalus.cardano.network.jvm

import io.bullet.borer.Cbor
import scalus.cardano.network.*
import scalus.cardano.network.infra.*
import scalus.cardano.network.handshake.{HandshakeMessage, NodeToNodeVersionData, RefuseReason}
import scalus.cardano.network.handshake.HandshakeMessage.*
import scalus.cardano.network.keepalive.KeepAliveMessage
import scalus.cardano.network.keepalive.KeepAliveMessage.*

import java.io.EOFException
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Test-only loopback peer for [[NodeToNodeClient]]. Accepts a single TCP connection on an
  * ephemeral port, parses SDU frames synchronously, replies to handshake with
  * `MsgAcceptVersion(v16)` (or a configurable refusal), and echoes keep-alive cookies. Runs in a
  * dedicated thread so tests can make real TCP connections without spinning up cardano-node.
  *
  * Scope: just enough to exercise the NodeToNodeClient happy path and a few fault paths. Not a
  * complete ouroboros responder — for realistic end-to-end, see `scalus-cardano-network-it` which talks to
  * yaci-devkit in M9.
  */
final class StubN2NResponder(behaviour: StubBehaviour = StubBehaviour.AcceptV16) {
    private val server: ServerSocket = new ServerSocket()
    server.setReuseAddress(true)
    server.bind(new InetSocketAddress("127.0.0.1", 0))

    private val running = new AtomicBoolean(true)
    private val connectedCount = new AtomicInteger(0)
    private val openSockets = ConcurrentHashMap.newKeySet[Socket]()
    private val thread: Thread = new Thread(() => acceptLoop(), "scalus-stub-n2n-responder")
    thread.setDaemon(true)
    thread.start()

    /** Port the responder is listening on. */
    def port: Int = server.getLocalPort

    /** Number of client connections accepted and serviced so far. */
    def connectionsAccepted: Int = connectedCount.get

    /** Stop accepting new connections, close the listener, and force-close every active handler
      * socket. Handlers exit immediately on their next read.
      */
    def shutdown(): Unit = {
        running.set(false)
        try server.close()
        catch { case NonFatal(_) => () }
        openSockets.asScala.foreach { s =>
            try s.close()
            catch { case NonFatal(_) => () }
        }
        openSockets.clear()
    }

    private def acceptLoop(): Unit = {
        while running.get && !server.isClosed do {
            try {
                val socket = server.accept()
                connectedCount.incrementAndGet()
                val handler =
                    new Thread(() => handleConnection(socket), s"stub-n2n-handler-$connectedCount")
                handler.setDaemon(true)
                handler.start()
            } catch {
                case _: java.net.SocketException if !running.get => () // expected on shutdown
                case NonFatal(t) =>
                    if running.get then System.err.println(s"stub accept error: $t")
            }
        }
    }

    private def handleConnection(socket: Socket): Unit = {
        openSockets.add(socket)
        try behaviour.run(socket)
        catch {
            case NonFatal(_) => () // connection closed or peer misbehaved; stub exits handler
        } finally {
            openSockets.remove(socket)
            try socket.close()
            catch { case NonFatal(_) => () }
        }
    }
}

/** Behaviour a [[StubN2NResponder]] exhibits per connection. Concrete impls do the read-SDU /
  * respond-SDU loop. Keeping this as a trait lets tests plug in fault-injecting variants (refusal,
  * malformed bytes, silent peer) without re-implementing the framing.
  */
trait StubBehaviour {
    def run(socket: Socket): Unit
}

object StubBehaviour {

    /** Read one SDU, decode as handshake, reply with `MsgAcceptVersion(v16)`; then echo keep-alive
      * cookies until EOF.
      */
    case object AcceptV16 extends StubBehaviour {
        def run(socket: Socket): Unit = StubWire.runAcceptingPeer(
          socket,
          acceptHandshake = true,
          refuseReason = None
        )
    }

    /** Refuse the handshake with `MsgRefuse(VersionMismatch(List(99)))`. Keep-alive loop is not
      * entered.
      */
    case object RefuseWithVersionMismatch extends StubBehaviour {
        def run(socket: Socket): Unit = StubWire.runAcceptingPeer(
          socket,
          acceptHandshake = false,
          refuseReason = Some(RefuseReason.VersionMismatch(List(99)))
        )
    }

    /** Send garbage bytes instead of a valid handshake reply. */
    case object MalformedHandshakeReply extends StubBehaviour {
        def run(socket: Socket): Unit = {
            val _ = StubWire.readOneSdu(socket) // consume the ProposeVersions frame
            // Send a valid SDU header then garbage CBOR for the payload.
            val garbage = Array[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte)
            StubWire.writeSdu(socket, MiniProtocolId.Handshake, Direction.Responder, garbage)
        }
    }

}

/** Wire-level helpers — synchronous I/O over a Socket, hand-crafted frames. */
private object StubWire {

    final case class RawSdu(proto: MiniProtocolId, direction: Direction, payload: Array[Byte])

    /** Read one complete SDU (header + payload). Throws `EOFException` on clean EOF. */
    def readOneSdu(socket: Socket): RawSdu = {
        val header = readFully(socket, Sdu.HeaderSize)
        val parsed = Sdu.parseHeader(header)
        val proto = parsed.protocol.getOrElse(
          throw new IllegalStateException(s"unknown protocol wire=${parsed.protocolWire}")
        )
        val payload = readFully(socket, parsed.length)
        RawSdu(proto, parsed.direction, payload)
    }

    def writeSdu(
        socket: Socket,
        proto: MiniProtocolId,
        direction: Direction,
        payload: Array[Byte]
    ): Unit = {
        val header = Sdu.encodeHeader(
          timestamp = 0,
          protocol = proto,
          direction = direction,
          length = payload.length
        )
        val out = socket.getOutputStream
        out.write(header)
        out.write(payload)
        out.flush()
    }

    def runAcceptingPeer(
        socket: Socket,
        acceptHandshake: Boolean,
        refuseReason: Option[RefuseReason]
    ): Unit = {
        val proposeFrame = readOneSdu(socket)
        require(
          proposeFrame.proto == MiniProtocolId.Handshake,
          s"first frame not handshake: $proposeFrame"
        )
        val propose = Cbor.decode(proposeFrame.payload).to[HandshakeMessage].value
        propose match {
            case MsgProposeVersions(_) => ()
            case other =>
                throw new IllegalStateException(s"expected MsgProposeVersions, got $other")
        }

        val reply: HandshakeMessage =
            if acceptHandshake then
                MsgAcceptVersion(
                  16,
                  NodeToNodeVersionData.V16(
                    networkMagic = NetworkMagic.YaciDevnet,
                    initiatorOnlyDiffusionMode = false,
                    peerSharing = 0,
                    query = false,
                    perasSupport = false
                  )
                )
            else MsgRefuse(refuseReason.getOrElse(RefuseReason.VersionMismatch(Nil)))
        writeSdu(
          socket,
          MiniProtocolId.Handshake,
          Direction.Responder,
          Cbor.encode(reply).toByteArray
        )
        if !acceptHandshake then return

        try {
            while !socket.isClosed do {
                val kaFrame = readOneSdu(socket)
                if kaFrame.proto == MiniProtocolId.KeepAlive then {
                    val msg = Cbor.decode(kaFrame.payload).to[KeepAliveMessage].value
                    msg match {
                        case MsgKeepAlive(cookie) =>
                            val resp = Cbor
                                .encode[KeepAliveMessage](MsgKeepAliveResponse(cookie))
                                .toByteArray
                            writeSdu(
                              socket,
                              MiniProtocolId.KeepAlive,
                              Direction.Responder,
                              resp
                            )
                        case MsgDone => return
                        case other =>
                            throw new IllegalStateException(s"unexpected keep-alive msg: $other")
                    }
                }
            }
        } catch {
            case _: EOFException => ()
        }
    }

    private def readFully(socket: Socket, n: Int): Array[Byte] = {
        val buf = new Array[Byte](n)
        var off = 0
        val in = socket.getInputStream
        while off < n do {
            val r = in.read(buf, off, n - off)
            if r < 0 then throw new EOFException(s"EOF after $off/$n bytes")
            off += r
        }
        buf
    }
}
