package scalus.cardano.node.stream.engine.snapshot

import java.nio.file.{Files, Path}

/** Shared helpers for the snapshot-module test suites + `[manual]` probes. Kept in a single place
  * to avoid the copy-paste drift we had when they lived in each suite.
  */
object TestUtils {

    /** Recursively delete a directory. Any IO exception is rethrown rather than silently swallowed
      * — mystery disappearing temp dirs are worse than a failing cleanup.
      */
    def deleteRecursively(dir: Path): Unit = {
        if !Files.exists(dir) then return
        import scala.jdk.CollectionConverters.*
        scala.util.Using.resource(Files.walk(dir)) { stream =>
            stream.iterator.asScala.toSeq.reverse.foreach(Files.deleteIfExists)
        }
    }

    /** Format a byte count with a SI-style unit suffix (B, KB, MB, GB, TB), two decimals. */
    def humanBytes(n: Long): String = {
        val units = Array("B", "KB", "MB", "GB", "TB")
        var v = n.toDouble
        var i = 0
        while v >= 1024.0 && i < units.length - 1 do { v /= 1024.0; i += 1 }
        f"$v%.2f ${units(i)}"
    }
}
