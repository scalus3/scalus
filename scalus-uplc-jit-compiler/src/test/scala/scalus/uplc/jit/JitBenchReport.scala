package scalus.uplc.jit

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Builds the decision table of spec section 4.4 from the results of [[JitBench]].
  *
  * Usage: `JitBenchReport <results dir>`. Reads `<label>.csv` and `<label>.summary` of every run,
  * and writes `report.md`. Expected labels: `jit-1` to `jit-3` (the JIT, three runs), optionally
  * `jit-quick`, `cek`, `reference` (the branch `worktree-jit-refactor-plan`), and any other run to
  * compare with jit.
  *
  * Every ratio is the geometric mean, over the scripts both runs compiled, of the per-script ratio
  * of the variant to jit; the jit value of a script is the mean of the three jit runs. A change of
  * +12 % means the variant, without the feature, is 12 % slower. The noise band is the spread of
  * the geometric means of the three jit runs.
  */
object JitBenchReport {

    private final case class Run(
        label: String,
        rows: Map[String, Row],
        summary: Map[String, String]
    )
    private final case class Row(
        compiled: Boolean,
        compileS: Double,
        bytes: Long,
        steadyS: Double,
        us: Double
    )

    def main(args: Array[String]): Unit = {
        val dir = Paths.get(args(0))
        val runs = Files
            .list(dir)
            .iterator()
            .asScala
            .map(_.getFileName.toString)
            .filter(_.endsWith(".summary"))
            .map(f => load(dir, f.stripSuffix(".summary")))
            .map(r => r.label -> r)
            .toMap
        val allOn = (1 to 3).flatMap(i => runs.get(s"jit-$i")).toList
        require(allOn.nonEmpty, "no jit run")
        val base: Map[String, Row] = allOn.head.rows.keys.flatMap { s =>
            val rs = allOn.map(_.rows(s))
            if rs.forall(_.compiled) then
                Some(
                  s -> Row(
                    true,
                    rs.map(_.compileS).sum / rs.size,
                    rs.head.bytes,
                    rs.map(_.steadyS).sum / rs.size,
                    rs.map(_.us).sum / rs.size
                  )
                )
            else None
        }.toMap

        def ratio(run: Run, f: Row => Double): Double = {
            val common = base.keys.filter(s => run.rows.get(s).exists(_.compiled)).toList
            JitBench.geoMean(common.map(s => f(run.rows(s)) / f(base(s))))
        }
        def pct(r: Double): String = f"${(r - 1) * 100}%+.1f %%"

        val out = collection.mutable.ListBuffer.empty[String]
        val geo = allOn.map(r => JitBench.geoMean(r.rows.values.map(_.us).toSeq))
        val noise = (geo.max - geo.min) / (geo.sum / geo.size)
        out += "# JIT bench results"
        out += ""
        out += f"All-on runs: ${allOn.size}; geo-mean eval ${geo.map(g => f"$g%.2f").mkString(", ")} us; noise band ${noise * 100}%.1f %%."
        out += ""
        out += "## Variants"
        out += ""
        out += "| Run | Compiled | Eval vs jit | Compile vs jit | Class bytes vs jit | Warm-up (s) | Warm-up windows |"
        out += "|---|---|---|---|---|---|---|"
        for label <- runs.keys.toList.sorted; run = runs(label) if run.rows.nonEmpty do
            val compiled = run.rows.values.count(_.compiled)
            val compile = if label == "cek" then "-" else pct(ratio(run, _.compileS))
            val bytes =
                if label == "cek" || label == "reference" then "-"
                else pct(ratio(run, _.bytes.toDouble))
            out += s"| $label | $compiled/${run.rows.size} | ${pct(ratio(run, _.us))} | $compile | $bytes | ${run.summary.getOrElse("warmup_s", "?")} | ${run.summary.getOrElse("warmup_windows", "?")} |"
        out += ""
        out += "## All-on against the CEK machine and the reference branch"
        out += ""
        for label <- List("cek", "reference"); run <- runs.get(label) if run.rows.nonEmpty do
            val eval = ratio(run, _.us)
            out += f"- jit is ${eval}%.2fx as fast as `$label` (geo-mean of per-script eval-time ratios $label / jit)."
            if label == "reference" then
                out += f"- compile time: jit is ${ratio(run, _.compileS)}%.2fx as fast as `reference`."
        val steady = base.values.map(_.steadyS).toList.sorted
        if steady.nonEmpty then
            out += f"- time to steady state, jit: median ${steady(steady.size / 2)}%.0f s per script, max ${steady.last}%.0f s; whole corpus: ${allOn.map(_.summary.getOrElse("warmup_s", "?")).mkString(", ")} s."
        runs.get("jit-quick").filter(_.rows.nonEmpty).foreach { q =>
            val r = ratio(q, _.us)
            val inside = math.abs(r - 1) <= noise
            out += f"- quick mode: jit-quick is ${pct(r)} against default jit; noise band ${noise * 100}%.1f %%: ${
                    if inside then "inside, [MS-14] allows quick mode"
                    else "outside, [MS-15]: default mode"
                }."
        }
        val text = out.mkString("\n") + "\n"
        Files.writeString(dir.resolve("report.md"), text)
        print(text)
    }

    private def load(dir: Path, label: String): Run = {
        val summaryLines = Files.readAllLines(dir.resolve(s"$label.summary")).asScala.toList
        val summary = summaryLines.flatMap { l =>
            l.split(" ", 2) match
                case Array(k, v) => Some(k -> v)
                case _           => None
        }.toMap + ("text" -> summaryLines.mkString(" "))
        // a broken variant ([MS-7]) has a summary and no csv
        val csv = dir.resolve(s"$label.csv")
        val rows =
            if !Files.exists(csv) then Map.empty[String, Row]
            else
                Files
                    .readAllLines(csv)
                    .asScala
                    .drop(1)
                    .map(_.split(",", -1))
                    .map(c =>
                        c(0) -> Row(
                          c(1).toBoolean,
                          c(2).toDouble,
                          c(3).toLong,
                          c(4).toDouble,
                          c(5).toDouble
                        )
                    )
                    .toMap
        Run(label, rows, summary)
    }
}
