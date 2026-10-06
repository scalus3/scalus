package scalus.verify.lean

import scala.jdk.CollectionConverters.*

private[verify] object Processes {

    /** Stops `process` and the processes it started, where any still runs: those it names now, and
      * the ones `known` to be its own from a listing made before. `lake` runs Lean as a child
      * process, and Lean the solver, and a child outlives its parent, which no longer names it once
      * it has ended. A child started between a listing and its parent's end is not found.
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
