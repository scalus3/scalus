package scalus.verify.lean

import scala.jdk.CollectionConverters.*

private[verify] object Processes {

    /** Stops `process` and the processes it started, where any still runs. `lake` runs Lean as a
      * child process, and Lean the solver, and a child outlives its parent. So the children are
      * listed while their parent is there to name them. One started between the listing and its
      * parent's end is not found.
      */
    def stop(process: Process): Unit = stop(process, Nil)

    /** [[stop]], with the processes `known` to be the process's own: listed before, where the
      * process may have ended since, and no longer names them.
      */
    def stop(process: Process, known: List[ProcessHandle]): Unit = {
        val children = (known ++ process.descendants().iterator().asScala).distinct
        // The children first. Ending the process also closes this side of its input, and that
        // waits for a thread that writes to it. A write the other side has stopped reading ends
        // only when no process is left that could read it, and a child could.
        children.foreach(_.destroyForcibly())
        children.foreach(_.onExit().join())
        process.destroyForcibly()
        process.waitFor()
    }
}
