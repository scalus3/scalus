package scalus.verify.lean

import java.nio.file.Path

/** Gives a tactic the Lean server that a check runs in. A tactic asks before every check, so the
  * provider decides how long a server lives, and a tactic neither starts nor ends one. A tactic can
  * be made, and can prepare its statements, where no Lean runs.
  *
  * [[LeanServers]] is the provider for a workspace. One server, for as long as it lives, is
  * `() => Right(server)`.
  */
@FunctionalInterface
trait LeanServerProvider {

    /** The server for the next check, or the reason there is none. */
    def server(): Either[String, LeanServer]

    /** The workspace the servers are of, where the provider says. What the results of their checks
      * rest on, on Lean's side, is read from its files ([[LeanWorkspace.environment]]), and a
      * tactic keeps its results only where it knows that.
      */
    def workspace: Option[Path] = None
}

/** The Lean servers of one workspace, one at a time: a server is started when a check first asks
  * for one, and another when that one has ended, as a server does that failed. So the statements
  * after one that ended Lean's server are still checked.
  *
  * Whoever creates it ends it with [[close]], which ends the server that runs.
  */
final class LeanServers private (directory: Path) extends LeanServerProvider with AutoCloseable {

    private var running: Option[LeanServer] = None
    private var closed = false

    override def workspace: Option[Path] = Some(directory)

    /** The server of the workspace that runs, or one started now, or the reason none starts. */
    override def server(): Either[String, LeanServer] = synchronized {
        if closed then Left("the Lean servers of the workspace are closed")
        else
            running.filterNot(_.isClosed) match
                case Some(server) => Right(server)
                case None =>
                    val started = LeanServer.start(directory)
                    running = started.toOption
                    started
    }

    /** Ends the server that runs. None is started after. Closing again does nothing. */
    override def close(): Unit = synchronized {
        closed = true
        running.foreach(_.close())
        running = None
    }
}

object LeanServers {

    /** The servers of the workspace `directory`. None is started before a check asks for one. */
    def in(directory: Path): LeanServers = new LeanServers(directory)
}
