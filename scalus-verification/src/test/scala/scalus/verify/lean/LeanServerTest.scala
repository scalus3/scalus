package scalus.verify.lean

import org.scalatest.funsuite.AnyFunSuite
import scalus.verify.lean.LeanServer.{Message, Progress, Reached, Result, Severity}
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

    /** An evaluation whose recursion is too deep for the stack of Lean's worker, which ends of it.
      */
    private val overflows =
        header + "\ndef deep : Nat → Nat\n  | 0 => 0\n  | n + 1 => deep n + 1\n\n#eval deep 1000000000\n"

    /** An evaluation that does not return. */
    private val endless =
        header + "\npartial def spin (n : Nat) : Nat := spin (n + 1)\n\n#eval spin 0\n"

    private val limit = Some(2.minutes)

    private def started(): LeanServer = startLean(leanWorkspace)

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
            server.check(endless, Some(3.seconds)) match
                case Result.TimedOut(progress) =>
                    // How far it had come: Lean was at the evaluation, in its worker. How long
                    // the worker had worked is not told on every system.
                    assert(progress.reached == Reached.Command("#eval spin 0"), progress)
                    assert(progress.spent > Duration.Zero, progress)
                    val worker = progress.started.find(_.name.startsWith("lean"))
                    assert(worker.exists(_.running > Duration.Zero), progress)
                    assert(worker.flatMap(_.working).forall(_ > Duration.Zero), progress)
                case other => fail(s"expected a check that is given up, got $other")
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

    test("a check whose worker ends has failed, and the server takes the next") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            server.check(overflows, limit) match
                case Result.Failed(reason) => assert(reason.contains("crashed"), reason)
                case other                 => fail(s"expected a failed check, got $other")
            // Lean's server has not ended, and it is not closed.
            assert(server.handle.isAlive)
            assert(!server.isClosed)
            server.check(reports, limit) match
                case Result.Finished(List(Message(2, Severity.Information, _))) =>
                case other => fail(s"expected one message, got $other")
        finally server.close()
    }

    test("a check waits for the one that runs, within its time limit and until interrupted") {
        val server = started()
        try
            assert(server.check(holds, limit) == Result.Finished(Nil))
            val first =
                CompletableFuture.supplyAsync[Result](() => server.check(endless, Some(10.seconds)))
            Thread.sleep(2000)
            // The time limit counts the wait: this check is given up before it starts.
            assert(
              server.check(holds, Some(1.second)) ==
                  Result.TimedOut(Progress(Reached.NotStarted, Duration.Zero, Nil))
            )
            val interrupted = new CompletableFuture[Boolean]()
            val waiting = new Thread(() =>
                try
                    server.check(holds, None): Unit
                    interrupted.complete(false): Unit
                catch case _: InterruptedException => interrupted.complete(true): Unit
            )
            waiting.start()
            Thread.sleep(1000)
            waiting.interrupt()
            assert(interrupted.get(5, TimeUnit.SECONDS))
            // The first check went on meanwhile, to its own time limit.
            assert(first.get(30, TimeUnit.SECONDS).isInstanceOf[Result.TimedOut])
            assert(server.check(holds, limit) == Result.Finished(Nil))
        finally server.close()
    }

    test("the files of a check are in a directory of the server's while the check runs") {
        val server = started()
        try
            var written = Option.empty[Path]
            def source(directory: Path): String = {
                written = Some(Files.writeString(directory.resolve("Leaf0.flat"), "00"))
                holds
            }
            assert(server.check(source, limit) == Result.Finished(Nil))
            assert(written.exists(_.startsWith(server.directory)), written)
            assert(written.forall(file => !Files.exists(file.getParent)), written)
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
            // closed for good, and closing again does nothing. The text of a check is not asked
            // for, as there is no directory left for its files.
            assert(server.check(holds, limit) == Result.Failed("the Lean server is closed"))
            assert(
              server.check(_ => fail("a closed server asked for a check's text"), limit) ==
                  Result.Failed("the Lean server is closed")
            )
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

    test("the servers of a workspace are one at a time, and another after one that has ended") {
        requireLean()
        val servers = LeanServers.in(leanWorkspace)
        def taken(): LeanServer = servers.server() match
            case Right(server) => server
            case Left(reason)  => fail(reason)
        try
            val first = taken()
            assert(taken() eq first)
            assert(first.check(holds, limit) == Result.Finished(Nil))
            // as a server that failed is
            first.close()
            val second = taken()
            assert(second ne first)
            assert(second.check(holds, limit) == Result.Finished(Nil))
            servers.close()
            assert(second.isClosed)
            assert(servers.server() == Left("the Lean servers of the workspace are closed"))
        finally servers.close()
    }

    test("the servers of a workspace that is not there give the reason") {
        val servers = LeanServers.in(Path.of("no-such-lean-workspace"))
        try assert(servers.server().left.exists(_.startsWith("Lean workspace does not exist")))
        finally servers.close()
    }
}
