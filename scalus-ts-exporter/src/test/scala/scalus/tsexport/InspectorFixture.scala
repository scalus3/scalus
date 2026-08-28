package scalus.tsexport

import java.io.File
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object InspectorFixture {
    val fixtureClasses: String = sys.props("tsexport.fixtures.classes")
    val fixtureClasspath: List[String] =
        sys.props("tsexport.fixtures.classpath").split(File.pathSeparator).toList
    val sourceRoot: String = sys.props("tsexport.sourceroot")

    def tastyFilesUnder(root: String): List[String] = {
        val p = Paths.get(root)
        if !Files.exists(p) then Nil
        else
            // Files.walk holds an open directory stream; close it
            Using.resource(Files.walk(p)) {
                _.iterator().asScala
                    .filter(_.toString.endsWith(".tasty"))
                    .map(_.toString)
                    .toList
                    .sorted
            }
    }
}
