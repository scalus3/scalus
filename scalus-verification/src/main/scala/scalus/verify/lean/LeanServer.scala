package scalus.verify.lean

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, ConcurrentHashMap, TimeoutException}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A running Lean language server for one workspace: `lake serve`, spoken to over its standard
  * input and output (docs/design/verification-details/lean-server.md).
  *
  * It knows Lean's server and nothing of statements. A check is a text for the server's one
  * document, which is never written to disk: the first check opens it, and every later one is an
  * edit of it. So the workspace's libraries are loaded once, and Lean elaborates again only from
  * the first command that changed. The header of every check, its imports, should be the same text:
  * a changed header loads the libraries anew. So does the check after one that was given up,
  * because giving up closes the document.
  *
  * A server is created with [[LeanServer.start]], for a workspace directory, and ended with
  * [[close]]. One that is not closed is closed when the JVM exits.
  */
final class LeanServer private (
    process: Process,
    workspace: Path,
    files: Path,
    errors: Path
) extends AutoCloseable {
    import LeanServer.*
    import LeanServer.Lsp.{*, given}

    private val uri = workspace.resolve("ScalusCheck.lean").toUri.toString
    private val closed = new AtomicBoolean(false)

    /** The version of the document that the last check sent, and whether the document is open. Only
      * [[check]] changes them.
      */
    private var version = 0
    private var opened = false

    /** The processes below the server's own before a document was opened. What elaborates a
      * document, Lean's worker and what it starts, is not among them.
      */
    @volatile private var standing: Set[ProcessHandle] = Set.empty

    /** The diagnostics the server sent last, and the version of the document they are of. */
    @volatile private var published: (Int, List[Diagnostic]) = (0, Nil)

    private val session =
        new JsonRpcSession(process.getInputStream, process.getOutputStream, notified)

    /** A directory for the files a check refers to. It is removed by [[close]]. */
    def directory: Path = files

    /** The server's process. Lean's worker and the solver are below it. */
    private[verify] def handle: ProcessHandle = process.toHandle

    /** Elaborates `source` as the server's document, and returns what Lean reported about it.
      *
      * Checks do not overlap: a later version of the document ends the elaboration of the one
      * before. When `timeout` passes, or the waiting thread is interrupted, the check is given up,
      * and the server takes the next one.
      */
    def check(source: String, timeout: Option[FiniteDuration]): Result = synchronized {
        if closed.get then Result.Failed("the Lean server is closed")
        else
            val sent = edit(source)
            val waited =
                try await(elaborated(sent), timeout)
                catch
                    case interrupted: InterruptedException =>
                        giveUp()
                        throw interrupted
            waited match
                // The server answers a check that ends while it is being closed, and then
                // publishes an empty list for the document it closes. What is read here could be
                // that list.
                case Some(Right(_)) if closed.get =>
                    Result.Failed("the Lean server was closed during the check")
                case Some(Right(diagnostics)) => Result.Finished(messages(diagnostics))
                case Some(Left(reason))       => fail(reason)
                case None                     => giveUp()
    }

    /** Ends the server: it is asked to shut down, and whatever is left of its processes after a few
      * seconds is stopped. A check that runs in another thread fails. Closing again does nothing.
      */
    override def close(): Unit =
        if closed.compareAndSet(false, true) then
            forget(this)
            // Listed now: once `lake` has ended, nothing names the processes it started.
            val family = process.descendants().iterator().asScala.toList
            try
                // A session that has ended cannot ask, and nothing would answer.
                if process.isAlive && session.ended.isEmpty then
                    await(session.request("shutdown", None), Some(patience))
                    session.notify("exit", None)
                    process.waitFor(patience.length, patience.unit): Unit
            finally
                Processes.stop(process, family)
                files.synchronized(Directories.remove(files))

    /** Starts the conversation, or says why the server did not take part in it. */
    private def initialize(): Option[String] = {
        val params = InitializeParams(
          ProcessHandle.current().pid(),
          workspace.toUri.toString,
          ClientCapabilities()
        )
        await(session.request("initialize", Some(RawJson.of(params))), Some(startup)) match
            case Some(Right(_)) =>
                session.notify("initialized", Some(RawJson.of(InitializedParams())))
                standing = process.descendants().iterator().asScala.toSet
                None
            case Some(Left(reason)) => Some(s"the Lean server did not start: $reason$errorOutput")
            case None               => Some(s"the Lean server did not answer within $startup")
    }

    /** Sends `source` as the next version of the document, and returns that version. */
    private def edit(source: String): Int = {
        version += 1
        if !opened then
            val document = TextDocumentItem(uri, "lean4", version, source)
            // Lean's own field. By default the server builds what the document imports, inside
            // the check's time and again after every check that is given up. A workspace that is
            // not built is reported as an error of the header, as `lake env lean` reports it.
            val params = DidOpenParams(document, dependencyBuildMode = "never")
            session.notify("textDocument/didOpen", Some(RawJson.of(params)))
            opened = true
        else
            val params = DidChangeParams(
              VersionedTextDocumentIdentifier(uri, version),
              List(ContentChange(source))
            )
            session.notify("textDocument/didChange", Some(RawJson.of(params)))
        version
    }

    /** The diagnostics of version `sent` of the document, once it is elaborated.
      *
      * Lean's own request `waitForDiagnostics` is answered then, after the diagnostics are
      * published. They are taken as the answer arrives, on the thread that reads the server, so
      * nothing the server publishes later is taken for them. Lean publishes for every version, an
      * empty list where a version has none. A version without one was not elaborated as far as this
      * can tell, and is no check that found nothing.
      */
    private def elaborated(sent: Int): CompletableFuture[Either[String, List[Diagnostic]]] =
        session
            .request(
              "textDocument/waitForDiagnostics",
              Some(RawJson.of(WaitForDiagnosticsParams(uri, sent)))
            )
            .thenApply { answer =>
                val (publishedFor, diagnostics) = published
                answer.flatMap { _ =>
                    if publishedFor == sent then Right(diagnostics)
                    else Left(s"the Lean server published no diagnostics for version $sent")
                }
            }

    /** The answer, or `None` when none came within `limit`. */
    private def await[A](answer: CompletableFuture[A], limit: Option[FiniteDuration]): Option[A] =
        limit match
            case None => Some(answer.get())
            case Some(duration) =>
                try Some(answer.get(duration.length, duration.unit))
                catch case _: TimeoutException => None

    /** Gives up the check that runs, by closing the document. Lean ends the worker of a closed
      * document, and with it whatever the check started: Blaster's run, the solver, an evaluation.
      * The check counts as given up only when those processes have ended.
      *
      * An edit of the document would be answered sooner, and would end Blaster's run and the solver
      * as well. But an evaluation that does not look for its cancellation goes on after it, and
      * keeps a processor busy for as long as the server lives.
      *
      * The next check opens the document again, and loads the libraries again.
      */
    private def giveUp(): Result = {
        val workers = process.descendants().iterator().asScala.filterNot(standing).toList
        val document = DidCloseParams(TextDocumentIdentifier(uri))
        session.notify("textDocument/didClose", Some(RawJson.of(document)))
        opened = false
        if ended(workers) then Result.TimedOut
        else
            // Lean did not end them. They are none of the server's own processes.
            workers.foreach(_.destroyForcibly())
            if ended(workers) then Result.TimedOut
            else fail(s"the processes of a check that was given up did not end within $patience")
    }

    /** Whether `processes` have all ended, at the latest after [[patience]]. */
    private def ended(processes: List[ProcessHandle]): Boolean = {
        val all = CompletableFuture.allOf(processes.map(_.onExit())*)
        try
            all.get(patience.length, patience.unit)
            true
        catch case _: TimeoutException => false
    }

    private def fail(reason: String): Result =
        if closed.get then Result.Failed("the Lean server was closed during the check")
        else
            try Result.Failed(reason + errorOutput)
            finally close()

    /** The end of what the server wrote to its error output, to say why it failed. Only the end is
      * read, and bytes that are no text are let through: the server may have ended in the middle of
      * a character.
      */
    private def errorOutput: String = files.synchronized {
        if !Files.isRegularFile(errors) then ""
        else
            val channel = Files.newByteChannel(errors)
            val written =
                try
                    val length = math.min(channel.size(), 2000L).toInt
                    val bytes = ByteBuffer.allocate(length)
                    channel.position(channel.size() - length)
                    while bytes.hasRemaining && channel.read(bytes) >= 0 do ()
                    new String(bytes.array(), 0, bytes.position(), StandardCharsets.UTF_8).trim
                finally channel.close()
            if written.isEmpty then "" else s": ${written.takeRight(500)}"
    }

    private def messages(diagnostics: List[Diagnostic]): List[Message] =
        diagnostics
            .map { diagnostic =>
                Message(
                  diagnostic.range.start.line,
                  // The protocol leaves a missing severity to the client. An error is not
                  // overlooked.
                  diagnostic.severity.fold(Severity.Error)(severity),
                  diagnostic.message
                )
            }
            .sortBy(_.line)

    /** What the server says unasked. The diagnostics of the document are kept. */
    private def notified(method: String, params: Option[RawJson]): Unit =
        if method == "textDocument/publishDiagnostics" then
            params.map(_.as[PublishDiagnosticsParams]).foreach { sent =>
                if sent.uri == uri then
                    sent.version.foreach(current => published = (current, sent.diagnostics))
            }
}

object LeanServer {

    /** How serious a message is, as the Language Server Protocol counts it. */
    enum Severity {
        case Error, Warning, Information, Hint
    }

    /** What a command of the document reported: a diagnostic. `line` is that of the command,
      * counted from zero as the protocol does.
      */
    final case class Message(line: Int, severity: Severity, text: String)

    /** The outcome of a [[LeanServer.check]]. */
    enum Result {

        /** The document was elaborated: its messages, in the order of their lines. */
        case Finished(messages: List[Message])

        /** The time limit passed. The check is given up, and what it started has ended. The server
          * takes the next one, and loads the libraries again for it.
          */
        case TimedOut

        /** The server ended, or did not keep to the protocol. It is closed, and takes no further
          * check.
          */
        case Failed(reason: String)
    }

    /** Starts `lake serve` in the workspace `directory`, or says why it cannot. */
    def start(directory: Path): Either[String, LeanServer] =
        if !Files.isDirectory(directory) then
            Left(s"Lean workspace does not exist: ${directory.toAbsolutePath}")
        else launch(directory.toAbsolutePath)

    private def launch(workspace: Path): Either[String, LeanServer] = {
        val files = Files.createTempDirectory("scalus-lean-server-")
        val errors = files.resolve("server.err")
        val started =
            try
                Right(
                  new ProcessBuilder("lake", "serve")
                      .directory(workspace.toFile)
                      .redirectError(errors.toFile)
                      .start()
                )
            catch case error: IOException => Left(s"cannot start Lean: ${error.getMessage}")
        started match
            case Left(reason) =>
                Directories.remove(files)
                Left(reason)
            case Right(process) =>
                val server = new LeanServer(process, workspace, files, errors)
                // Closed where it does not come up, also where the wait for it is interrupted.
                var up = false
                try
                    remember(server)
                    val refused = server.initialize()
                    up = refused.isEmpty
                    refused.toLeft(server)
                finally if !up then server.close()
    }

    /** The time the server gets to start the conversation. */
    private val startup = 60.seconds

    /** The time the server gets to do what takes it a moment: end a worker, or shut down. */
    private val patience = 10.seconds

    /** The servers that are started and not closed, and the hook that closes them when the JVM
      * exits. The hook is there only while a server is open. One that stayed would keep the classes
      * of whatever started a server alive in a JVM that goes on, as sbt's does after a test run.
      *
      * Where the JVM is killed and no hook runs, a server ends by itself, because its input closes.
      */
    private val open = ConcurrentHashMap.newKeySet[LeanServer]()
    private var atExit: Option[Thread] = None
    private var exiting = false

    private def remember(server: LeanServer): Unit = open.synchronized {
        open.add(server)
        if atExit.isEmpty && !exiting then
            val hook = new Thread(() => closeAll(), "scalus-lean-server-shutdown")
            // The JVM tells that it is exiting only by refusing the hook. The server then ends
            // with the JVM, because its input closes.
            try
                Runtime.getRuntime.addShutdownHook(hook)
                atExit = Some(hook)
            catch case _: IllegalStateException => exiting = true
    }

    private def forget(server: LeanServer): Unit = open.synchronized {
        open.remove(server)
        if open.isEmpty && !exiting then
            // Refused likewise while the JVM exits, where the hook need not be taken back.
            try atExit.foreach(Runtime.getRuntime.removeShutdownHook)
            catch case _: IllegalStateException => exiting = true
            atExit = None
    }

    /** Closes every open server. One that fails to close does not keep the others open: its failure
      * is thrown when all have been tried.
      */
    private def closeAll(): Unit = {
        val servers = open.synchronized {
            exiting = true
            open.asScala.toList
        }
        val failures = servers.flatMap(server => Try(server.close()).failed.toOption)
        failures.headOption.foreach(failure => throw failure)
    }

    /** Whether `server` is open, and so closed when the JVM exits. */
    private[lean] def isOpen(server: LeanServer): Boolean = open.contains(server)

    /** A severity the protocol does not know is an error, as a missing one is. */
    private def severity(code: Int): Severity = code match
        case 2 => Severity.Warning
        case 3 => Severity.Information
        case 4 => Severity.Hint
        case _ => Severity.Error

    /** The messages of the Language Server Protocol that are used, with the fields that are. */
    private object Lsp {
        final case class ClientCapabilities()
        final case class InitializeParams(
            processId: Long,
            rootUri: String,
            capabilities: ClientCapabilities
        )
        final case class InitializedParams()
        final case class TextDocumentItem(
            uri: String,
            languageId: String,
            version: Int,
            text: String
        )
        final case class DidOpenParams(textDocument: TextDocumentItem, dependencyBuildMode: String)
        final case class VersionedTextDocumentIdentifier(uri: String, version: Int)
        final case class ContentChange(text: String)
        final case class DidChangeParams(
            textDocument: VersionedTextDocumentIdentifier,
            contentChanges: List[ContentChange]
        )
        final case class TextDocumentIdentifier(uri: String)
        final case class DidCloseParams(textDocument: TextDocumentIdentifier)
        final case class WaitForDiagnosticsParams(uri: String, version: Int)
        final case class Position(line: Int, character: Int)
        final case class Range(start: Position, end: Position)
        final case class Diagnostic(range: Range, severity: Option[Int], message: String)
        final case class PublishDiagnosticsParams(
            uri: String,
            version: Option[Int],
            diagnostics: List[Diagnostic]
        )

        given JsonValueCodec[InitializeParams] = JsonCodecMaker.make
        given JsonValueCodec[InitializedParams] = JsonCodecMaker.make
        given JsonValueCodec[DidOpenParams] = JsonCodecMaker.make
        given JsonValueCodec[DidChangeParams] = JsonCodecMaker.make
        given JsonValueCodec[DidCloseParams] = JsonCodecMaker.make
        given JsonValueCodec[WaitForDiagnosticsParams] = JsonCodecMaker.make
        given JsonValueCodec[PublishDiagnosticsParams] = JsonCodecMaker.make
    }
}
