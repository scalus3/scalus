package scalus.verify.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.verify.lean.LeanServer.{Message, Result, Severity}
import scalus.verify.uplcblaster.LeanProofs

import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** A Lean language server, started in the module's workspace. The tests need Lean: without `lake`
  * and a built workspace they are canceled (see [[LeanProofs]]).
  */
class LeanServerTest extends AnyFunSuite with LeanProofs {

    /** The header of every check here: one text, so the workspace is loaded once for a server. */
    private val header = "import ScalusProofs.Run\n"

    private val reports = header + "\n#eval 1 + 1\n"
    private val holds = header + "\nexample : 1 + 1 = 2 := by decide\n"
    private val fails = header + "\nexample : 1 + 1 = 3 := by decide\n"

    /** An evaluation that does not return. */
    private val endless =
        header + "\npartial def spin (n : Nat) : Nat := spin (n + 1)\n\n#eval spin 0\n"

    private val limit = Some(2.minutes)

    private def started(): LeanServer = {
        requireLean()
        LeanServer.start(leanDirectory) match
            case Right(server) => server
            case Left(reason)  => fail(reason)
    }

    /** The server's process and those below it, as they are now. */
    private def processes(server: LeanServer): List[ProcessHandle] =
        server.handle :: server.handle.descendants().iterator().asScala.toList

    test("a check returns what Lean reports, and so does the next") {
        val server = started()
        try
            server.check(reports, limit) match
                case Result.Finished(List(Message(2, Severity.Information, text))) =>
                    assert(text.trim == "2")
                case other => fail(s"expected one message, got $other")
            assert(server.check(holds, limit) == Result.Finished(Nil))
            server.check(fails, limit) match
                case Result.Finished(List(Message(2, Severity.Error, text))) =>
                    assert(text.contains("is false"), text)
                case other => fail(s"expected one error, got $other")
            // a header that cannot be loaded is an error of the check, not the end of the server
            server.check("import ScalusProofs.NoSuchModule\n", limit) match
                case Result.Finished(List(Message(0, Severity.Error, _))) =>
                case other => fail(s"expected an error on the first line, got $other")
            assert(server.check(holds, limit) == Result.Finished(Nil))
        finally server.close()
    }

    test("a check that does not finish is given up at its time limit, with what it started") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            val before = processes(server)
            assert(server.check(endless, Some(3.seconds)) == Result.TimedOut)
            // The worker that ran the evaluation has ended, so nothing of it goes on. The server
            // itself stays, and takes the next check.
            assert(server.handle.isAlive)
            assert(before.exists(!_.isAlive), before)
            assert(processes(server).sizeIs < before.size)
            server.check(reports, limit) match
                case Result.Finished(List(Message(2, Severity.Information, _))) =>
                case other => fail(s"expected one message, got $other")
        finally server.close()
    }

    test("a thread that is interrupted gives its check up, and the server takes the next") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            val interrupted = new CompletableFuture[Boolean]()
            val waiting = new Thread(() =>
                try
                    server.check(endless, None): Unit
                    interrupted.complete(false): Unit
                catch case _: InterruptedException => interrupted.complete(true): Unit
            )
            waiting.start()
            Thread.sleep(2000)
            waiting.interrupt()
            assert(interrupted.get(30, TimeUnit.SECONDS))
            assert(server.check(holds, limit) == Result.Finished(Nil))
        finally server.close()
    }

    test("closing ends the server's processes, also while a check runs") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            val running = processes(server)
            assert(running.sizeIs >= 2, running)
            assert(Files.isDirectory(server.directory))
            // while it is open, the JVM's exit would close it
            assert(LeanServer.isOpen(server))
            val unfinished =
                CompletableFuture.supplyAsync[Result](() => server.check(endless, None))
            Thread.sleep(2000)
            server.close()
            assert(unfinished.get(30, TimeUnit.SECONDS).isInstanceOf[Result.Failed])
            assert(running.forall(!_.isAlive), running.filter(_.isAlive))
            assert(!Files.exists(server.directory))
            // and nothing of it is left for the JVM's exit
            assert(!LeanServer.isOpen(server))
            // closed for good, and closing again does nothing
            assert(server.check(holds, limit) == Result.Failed("the Lean server is closed"))
        finally server.close()
    }

    test("a server that has ended fails its checks, with the reason") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            val running = processes(server)
            // from outside, as a crash would
            running.foreach(_.destroyForcibly())
            running.foreach(_.onExit().join())
            server.check(holds, limit) match
                case Result.Failed(reason) => assert(reason.nonEmpty)
                case other                 => fail(s"expected a failed check, got $other")
            assert(running.forall(!_.isAlive), running.filter(_.isAlive))
        finally server.close()
    }

    test("a server does not start where there is no workspace directory") {
        val missing = Path.of("no-such-lean-workspace")
        assert(LeanServer.start(missing).left.exists(_.startsWith("Lean workspace does not exist")))
    }
}
