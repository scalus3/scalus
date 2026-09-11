package scalus.examples.cape

import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.nio.file.Files
import java.util.Locale
import scala.jdk.CollectionConverters.*
import scala.util.Using

class CompareWithLeaderboardTest extends AnyFunSuite {
    test("CAPE prices stay fixed and execution fees round once over included evaluations") {
        val root = Files.createTempDirectory("cape-leaderboard")
        try {
            val submission = Files.createDirectories(root.resolve("submissions/scenario/_nau"))
            Files.writeString(
              submission.resolve("metrics.json"),
              """{
                |  "evaluations": [
                |    {"included_in_aggregates": true, "memory_units": 1, "cpu_units": 1},
                |    {"included_in_aggregates": true, "memory_units": 1, "cpu_units": 1},
                |    {"included_in_aggregates": false, "memory_units": 999999, "cpu_units": 999999}
                |  ],
                |  "measurements": {"script_size_bytes": 460801}
                |}""".stripMargin
            )
            val priceCheck =
                Files.createDirectories(root.resolve("submissions/scenario/price-check"))
            Files.writeString(
              priceCheck.resolve("metrics.json"),
              """{
                |  "evaluations": [
                |    {"included_in_aggregates": true, "memory_units": 10000000, "cpu_units": 10000000000}
                |  ],
                |  "measurements": {"script_size_bytes": 1}
                |}""".stripMargin
            )
            val output = new ByteArrayOutputStream()
            Console.withOut(output) {
                CompareWithLeaderboard(root.toString)
            }
            // Compare the rendered row using the same locale as the command's numeric formatting.
            val row = output.toString.linesIterator.find(_.contains("_nau")).get
            assert(
              row.contains(String.format(Locale.getDefault, "total=%,9d", Long.box(49197200L)))
            )
            assert(row.contains(String.format(Locale.getDefault, "exec=%,8d", Long.box(1L))))
            assert(row.contains(String.format(Locale.getDefault, "ref=%,7d", Long.box(49197199L))))
            assert(row.contains(String.format(Locale.getDefault, "mem=%,12d", Long.box(2L))))
            assert(row.contains(String.format(Locale.getDefault, "cpu=%,15d", Long.box(2L))))
            val priceRow = output.toString.linesIterator.find(_.contains("price-check")).get
            assert(
              priceRow.contains(String.format(Locale.getDefault, "total=%,9d", Long.box(1298015L)))
            )
            assert(
              priceRow.contains(String.format(Locale.getDefault, "exec=%,8d", Long.box(1298000L)))
            )
        } finally {
            Using.resource(Files.walk(root)) { paths =>
                paths.iterator.asScala.toSeq.reverse.foreach(Files.delete)
            }
        }
    }
}
