package scalus.verify.lean

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[verify] object Directories {

    /** Removes `directory` with what is in it. One that is not there is left at that. */
    def remove(directory: Path): Unit =
        if Files.exists(directory) then
            val paths = Using.resource(Files.walk(directory))(_.iterator().asScala.toList)
            paths.reverse.foreach(Files.deleteIfExists)
}
