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
        process.destroyForcibly()
        children.foreach(_.destroyForcibly())
        children.foreach(_.onExit().join())
        process.waitFor()
    }
}
