package scalus.testing.budget

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/** File access of the budget update run, JVM version: the only platform that writes (`[BP-8b]`). */
object BudgetFiles {

    /** Writes a generated table to `path`. */
    def writeTable(path: String, source: String): Unit = {
        val _ = Files.writeString(Path.of(path), source, UTF_8)
    }

    /** Appends one suite's report lines to `target/budgets/report.md` of the working directory. */
    def appendReport(lines: Seq[String]): Unit = {
        val report = Path.of("target", "budgets", "report.md")
        Files.createDirectories(report.getParent)
        val _ = Files.writeString(
          report,
          lines.mkString("", "\n", "\n"),
          UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
    }
}
