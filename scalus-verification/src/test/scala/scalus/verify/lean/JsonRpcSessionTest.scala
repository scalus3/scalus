package scalus.verify.lean

import org.scalatest.funsuite.AnyFunSuite

import java.io.{PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

/** A session with Lean's server, against a stand-in on piped streams: no Lean is run. */
class JsonRpcSessionTest extends AnyFunSuite {

    /** A session, and the other end of its connection, driven by the test. */
    private final class Peer {
        private val toPeer = new PipedOutputStream()
        private val peerInput = new PipedInputStream(toPeer, 1 << 16)
        private val peerOutput = new PipedOutputStream()
        private val toClient = new PipedInputStream(peerOutput, 1 << 16)

        val notifications = new LinkedBlockingQueue[(String, String)]()

        val session = new JsonRpcSession(
          toClient,
          toPeer,
          (method, params) => notifications.put(method -> params.fold("")(_.toString))
        )

        /** The next message the session sent, as its text. */
        def received(): String = {
            val header = new StringBuilder
            while !header.endsWith("\r\n\r\n") do header.append(peerInput.read().toChar)
            val length = header.toString.trim.stripPrefix("Content-Length:").trim.toInt
            new String(peerInput.readNBytes(length), UTF_8)
        }

        def send(message: String): Unit = {
            val body = message.getBytes(UTF_8)
            peerOutput.write(s"Content-Length: ${body.length}\r\n\r\n".getBytes(UTF_8))
            peerOutput.write(body)
            peerOutput.flush()
        }

        /** Closes the other end, which ends the session and its threads. */
        def close(): Unit = peerOutput.close()
    }

    /** A test with a peer, which is closed after it. */
    private def session(name: String)(body: Peer => Unit): Unit = test(name) {
        val peer = new Peer
        try body(peer)
        finally peer.close()
    }

    private def answer(
        future: java.util.concurrent.Future[JsonRpcSession.Answer]
    ): JsonRpcSession.Answer =
        future.get(10, TimeUnit.SECONDS)

    private def raw(json: String): Option[RawJson] = Some(new RawJson(json.getBytes(UTF_8)))

    session("a request is answered by the response that has its id") { peer =>
        val first = peer.session.request("first", raw("""{"x":1}"""))
        val second = peer.session.request("second", None)
        assert(peer.received() == """{"jsonrpc":"2.0","id":1,"method":"first","params":{"x":1}}""")
        assert(peer.received() == """{"jsonrpc":"2.0","id":2,"method":"second"}""")
        // answered in the other order
        peer.send("""{"jsonrpc":"2.0","id":2,"result":{"b":true}}""")
        peer.send("""{"jsonrpc":"2.0","id":1,"result":[1,2]}""")
        assert(answer(second).map(_.map(_.toString)) == Right(Some("""{"b":true}""")))
        assert(answer(first).map(_.map(_.toString)) == Right(Some("[1,2]")))
    }

    session("a null result is no result, and an error is the reason") { peer =>
        val empty = peer.session.request("shutdown", None)
        val refused = peer.session.request("unknown", None)
        peer.send("""{"jsonrpc":"2.0","id":1,"result":null}""")
        peer.send("""{"jsonrpc":"2.0","id":2,"error":{"code":-32601,"message":"no such method"}}""")
        assert(answer(empty) == Right(None))
        assert(answer(refused) == Left("no such method (-32601)"))
    }

    session("a notification is sent without an id, and one that comes in reaches the handler") {
        peer =>
            peer.session.notify("initialized", raw("{}"))
            assert(peer.received() == """{"jsonrpc":"2.0","method":"initialized","params":{}}""")
            peer.send("""{"jsonrpc":"2.0","method":"note","params":{"n":7}}""")
            assert(peer.notifications.poll(10, TimeUnit.SECONDS) == ("note" -> """{"n":7}"""))
    }

    session("a request of the other end's is answered with a null result") { peer =>
        peer.send(
          """{"jsonrpc":"2.0","id":"r-1","method":"client/registerCapability","params":{}}"""
        )
        assert(peer.received() == """{"jsonrpc":"2.0","id":"r-1","result":null}""")
        peer.send("""{"jsonrpc":"2.0","id":5,"method":"workspace/semanticTokens/refresh"}""")
        assert(peer.received() == """{"jsonrpc":"2.0","id":5,"result":null}""")
    }

    session("the length of a message counts its bytes, not its characters") { peer =>
        val verdict = "\"✅ Valid\""
        val asked = peer.session.request("echo", raw(verdict))
        assert(peer.received() == s"""{"jsonrpc":"2.0","id":1,"method":"echo","params":$verdict}""")
        peer.send(s"""{"jsonrpc":"2.0","id":1,"result":$verdict}""")
        assert(answer(asked).map(_.map(_.toString)) == Right(Some(verdict)))
    }

    session("a sender does not wait for the other end to read") { peer =>
        // More than the pipe holds, and nothing reads it: the messages wait in the session.
        val large = raw("\"" + "x" * (1 << 17) + "\"")
        val asked = (1 to 4).map(_ => peer.session.request("large", large))
        assert(asked.forall(!_.isDone))
        // The reading thread is not held up by them: it still answers a request of the peer's,
        // once the peer reads what was sent before it.
        peer.send("""{"jsonrpc":"2.0","id":9,"method":"workspace/inlayHint/refresh"}""")
        (1 to 4).foreach(_ => assert(peer.received().contains("\"method\":\"large\"")))
        assert(peer.received() == """{"jsonrpc":"2.0","id":9,"result":null}""")
    }

    session("when the other end closes, the requests that wait end, and so do later ones") { peer =>
        val waiting = peer.session.request("textDocument/waitForDiagnostics", None)
        peer.received()
        assert(peer.session.ended.isEmpty)
        peer.close()
        assert(answer(waiting) == Left("the server closed its output"))
        assert(peer.session.ended.contains("the server closed its output"))
        assert(answer(peer.session.request("later", None)) == Left("the server closed its output"))
    }

    session("what is not a message ends the session") { peer =>
        val waiting = peer.session.request("initialize", None)
        peer.send("not json")
        assert(answer(waiting).left.exists(_.startsWith("the server sent what is not a message")))
    }
}
