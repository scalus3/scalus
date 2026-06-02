package scalus.uplc.eval

import scalus.uplc.builtin.platform

object ProfileFormatter {

    /** Max rows rendered per HTML table before collapsing into a "… and N more" note. */
    private val htmlRowCap = 200

    /** Max locations shown on each axis of the transition matrix (keeps it readable). */
    private val matrixCap = 24

    /** Max incoming edges kept per node in the By-Source-Location call tree. */
    private val treeChildCap = 50

    /** Renders profiling data as a formatted text table.
      *
      * @param data
      *   The profiling data
      * @param maxRows
      *   Maximum rows to display per section (default 50)
      * @return
      *   Formatted text output
      */
    def toText(data: ProfilingData, maxRows: Int = 50): String = {
        val effectiveMaxRows = math.max(1, maxRows)
        val sb = new StringBuilder

        sb.append("=== Profile by Source Location ===\n")
        if data.bySourceLocation.isEmpty then sb.append("  (no source locations recorded)\n")
        else
            val rows = data.bySourceLocation.take(effectiveMaxRows)
            val locWidth = math.max(8, rows.map(e => s"${shortFile(e.file)}:${e.line}".length).max)
            val countWidth = math.max(5, rows.map(_.count.toString.length).max)
            val memWidth = math.max(3, rows.map(_.memory.toString.length).max)
            val cpuWidth = math.max(3, rows.map(_.cpu.toString.length).max)

            sb.append(
              formatRow("location", "count", "mem", "cpu", locWidth, countWidth, memWidth, cpuWidth)
            )
            sb.append('\n')
            rows.foreach { e =>
                sb.append(
                  formatRow(
                    s"${shortFile(e.file)}:${e.line}",
                    e.count.toString,
                    e.memory.toString,
                    e.cpu.toString,
                    locWidth,
                    countWidth,
                    memWidth,
                    cpuWidth
                  )
                )
                sb.append('\n')
            }
            if data.bySourceLocation.size > effectiveMaxRows then
                sb.append(s"  ... and ${data.bySourceLocation.size - effectiveMaxRows} more\n")

        sb.append('\n')
        sb.append("=== Profile by Builtin ===\n")
        if data.byFunction.isEmpty then sb.append("  (no builtins recorded)\n")
        else
            val rows = data.byFunction.take(effectiveMaxRows)
            val nameWidth = math.max(8, rows.map(_.name.length).max)
            val countWidth = math.max(5, rows.map(_.count.toString.length).max)
            val memWidth = math.max(3, rows.map(_.memory.toString.length).max)
            val cpuWidth = math.max(3, rows.map(_.cpu.toString.length).max)

            sb.append(
              formatRow(
                "builtin",
                "count",
                "mem",
                "cpu",
                nameWidth,
                countWidth,
                memWidth,
                cpuWidth
              )
            )
            sb.append('\n')
            rows.foreach { e =>
                sb.append(
                  formatRow(
                    e.name,
                    e.count.toString,
                    e.memory.toString,
                    e.cpu.toString,
                    nameWidth,
                    countWidth,
                    memWidth,
                    cpuWidth
                  )
                )
                sb.append('\n')
            }
            if data.byFunction.size > effectiveMaxRows then
                sb.append(s"  ... and ${data.byFunction.size - effectiveMaxRows} more\n")

        if data.byLocationFunction.nonEmpty then
            sb.append('\n')
            sb.append("=== Builtins by Source Location ===\n")
            val lfRows = data.byLocationFunction.take(effectiveMaxRows)
            val lfLocWidth =
                math.max(8, lfRows.map(e => s"${shortFile(e.file)}:${e.line}".length).max)
            val lfNameWidth = math.max(8, lfRows.map(_.functionName.length).max)
            val lfCountWidth = math.max(5, lfRows.map(_.count.toString.length).max)
            val lfMemWidth = math.max(3, lfRows.map(_.memory.toString.length).max)
            val lfCpuWidth = math.max(3, lfRows.map(_.cpu.toString.length).max)

            sb.append(
              s"${"location".padTo(lfLocWidth, ' ')}  ${"function".padTo(lfNameWidth, ' ')}  ${"count".reverse.padTo(lfCountWidth, ' ').reverse}  ${"mem".reverse.padTo(lfMemWidth, ' ').reverse}  ${"cpu".reverse.padTo(lfCpuWidth, ' ').reverse}\n"
            )
            lfRows.foreach { e =>
                val loc = s"${shortFile(e.file)}:${e.line}"
                sb.append(
                  s"${loc.padTo(lfLocWidth, ' ')}  ${e.functionName.padTo(lfNameWidth, ' ')}  ${e.count.toString.reverse.padTo(lfCountWidth, ' ').reverse}  ${e.memory.toString.reverse.padTo(lfMemWidth, ' ').reverse}  ${e.cpu.toString.reverse.padTo(lfCpuWidth, ' ').reverse}\n"
                )
            }
            if data.byLocationFunction.size > effectiveMaxRows then
                sb.append(
                  s"  ... and ${data.byLocationFunction.size - effectiveMaxRows} more\n"
                )

        if data.transitions.nonEmpty then
            sb.append('\n')
            sb.append("=== Transitions (source location flow) ===\n")
            val rows = data.transitions.take(effectiveMaxRows)
            rows.foreach { t =>
                val from = s"${shortFile(t.fromFile)}:${t.fromLine}"
                val to = s"${shortFile(t.toFile)}:${t.toLine}"
                sb.append(f"  ${t.count}%8d  $from%s -> $to%s\n")
            }
            if data.transitions.size > effectiveMaxRows then
                sb.append(s"  ... and ${data.transitions.size - effectiveMaxRows} more\n")

        sb.append(s"\nTotal: mem=${data.totalBudget.memory} cpu=${data.totalBudget.steps}")
        sb.toString
    }

    /** Compact summary suitable for console / log output: total budget plus the top-N by CPU. */
    def summary(data: ProfilingData, topN: Int = 10): String = {
        val n = math.max(1, topN)
        val sb = new StringBuilder
        sb.append(s"Total: mem=${data.totalBudget.memory} cpu=${data.totalBudget.steps}\n")
        if data.byFunction.nonEmpty then
            sb.append(s"Top $n by CPU (builtin):\n")
            data.byFunction.sortBy(e => -e.cpu).take(n).foreach { e =>
                sb.append(
                  "  " + e.name.padTo(24, ' ') + s" cpu=${e.cpu} mem=${e.memory} n=${e.count}\n"
                )
            }
        if data.bySourceLocation.nonEmpty then
            sb.append(s"Top $n by CPU (source):\n")
            data.bySourceLocation.sortBy(e => -e.cpu).take(n).foreach { e =>
                val loc = s"${shortFile(e.file)}:${e.line}"
                sb.append(
                  "  " + loc.padTo(24, ' ') + s" cpu=${e.cpu} mem=${e.memory} n=${e.count}\n"
                )
            }
        sb.toString
    }

    /** Renders profiling data as CSV (one diffable file across all sections). */
    def toCsv(data: ProfilingData): String = {
        val sb = new StringBuilder
        sb.append("section,key,detail,count,mem,cpu\n")
        data.bySourceLocation.foreach(e =>
            sb.append(csvRow("location", s"${e.file}:${e.line}", "", e.count, e.memory, e.cpu))
        )
        data.byFunction.foreach(e =>
            sb.append(csvRow("builtin", e.name, "", e.count, e.memory, e.cpu))
        )
        data.byLocationFunction.foreach(e =>
            sb.append(
              csvRow(
                "loc-builtin",
                s"${e.file}:${e.line}",
                e.functionName,
                e.count,
                e.memory,
                e.cpu
              )
            )
        )
        data.transitions.foreach(t =>
            sb.append(
              csvRow(
                "edge",
                s"${t.fromFile}:${t.fromLine}",
                s"${t.toFile}:${t.toLine}",
                t.count,
                0,
                0
              )
            )
        )
        sb.append(csvRow("total", "", "", 0, data.totalBudget.memory, data.totalBudget.steps))
        sb.toString
    }

    /** Renders the full profiling data as JSON (machine-readable). */
    def toJson(data: ProfilingData): String = {
        val sb = new StringBuilder
        sb.append("{\n")
        sb.append(
          s"""  "totalBudget": { "mem": ${data.totalBudget.memory}, "cpu": ${data.totalBudget.steps} },\n"""
        )
        sb.append("  \"bySourceLocation\": [")
        sb.append(
          data.bySourceLocation
              .map(e =>
                  s"""{"file":"${jsonEsc(
                        e.file
                      )}","line":${e.line},"count":${e.count},"mem":${e.memory},"cpu":${e.cpu}}"""
              )
              .mkString(",")
        )
        sb.append("],\n")
        sb.append("  \"byFunction\": [")
        sb.append(
          data.byFunction
              .map(e =>
                  s"""{"name":"${jsonEsc(
                        e.name
                      )}","count":${e.count},"mem":${e.memory},"cpu":${e.cpu}}"""
              )
              .mkString(",")
        )
        sb.append("],\n")
        sb.append("  \"byLocationFunction\": [")
        sb.append(
          data.byLocationFunction
              .map(e =>
                  s"""{"file":"${jsonEsc(e.file)}","line":${e.line},"function":"${jsonEsc(
                        e.functionName
                      )}","count":${e.count},"mem":${e.memory},"cpu":${e.cpu}}"""
              )
              .mkString(",")
        )
        sb.append("],\n")
        sb.append("  \"transitions\": [")
        sb.append(
          data.transitions
              .map(t =>
                  s"""{"fromFile":"${jsonEsc(
                        t.fromFile
                      )}","fromLine":${t.fromLine},"toFile":"${jsonEsc(
                        t.toFile
                      )}","toLine":${t.toLine},"count":${t.count}}"""
              )
              .mkString(",")
        )
        sb.append("]\n}\n")
        sb.toString
    }

    /** Renders profiling data as a self-contained HTML page (no source annotation). */
    def toHtml(data: ProfilingData): String = toHtml(data, Map.empty)

    /** Renders profiling data as a self-contained, interactive HTML page.
      *
      * The page bundles its own CSS/JS (no external dependencies): sortable, filterable tables with
      * %-of-CPU bars, a transition matrix heatmap with a hot-edges table, and — when source text is
      * supplied — a per-line cost-annotated source view (the flagship view).
      *
      * @param data
      *   the profiling data
      * @param sources
      *   optional `file -> source lines` (line `n` is element `n - 1`); profiled files present here
      *   are rendered with per-line cost annotations
      */
    def toHtml(data: ProfilingData, sources: Map[String, IndexedSeq[String]]): String = {
        val totalCpu = math.max(1L, data.totalBudget.steps)

        // Build each section's inner HTML; only non-empty sections become tabs. Section titles
        // live on the tab buttons, so the panels themselves carry no <h2>.
        val sections = scala.collection.mutable.ArrayBuffer[(String, String, String)]()
        def section(id: String, title: String)(render: StringBuilder => Unit): Unit = {
            val b = new StringBuilder
            render(b)
            sections += ((id, title, b.toString))
        }

        section("src", "By Source Location")(appendSourceLocationTable(_, data, totalCpu))

        renderAnnotatedSource(data, sources).foreach(html =>
            sections += (("annotated", "Annotated Source", html))
        )

        section("fun", "By Builtin")(
          appendCostTable(
            _,
            Seq("Builtin"),
            data.byFunction.map(e => (Seq(e.name), e.count, e.memory, e.cpu)),
            totalCpu
          )
        )

        if data.byLocationFunction.nonEmpty then
            section("lf", "Builtins by Source Location")(
              appendCostTable(
                _,
                Seq("Location", "Function"),
                data.byLocationFunction.map(e =>
                    (
                      Seq(s"${shortFile(e.file)}:${e.line}", e.functionName),
                      e.count,
                      e.memory,
                      e.cpu
                    )
                ),
                totalCpu
              )
            )

        if data.transitions.nonEmpty then
            section("matrix", "Transition Matrix")(appendTransitionMatrix(_, data))
            section("edges", "Hot Edges")(appendHotEdges(_, data))

        val sb = new StringBuilder
        sb.append(
          "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><title>Scalus Profile</title>\n<style>\n"
        )
        sb.append(htmlStyle)
        sb.append("\n</style></head><body>\n<h1>Scalus CEK Machine Profile</h1>\n")
        sb.append(
          s"""<p class="summary">Total budget: mem=${data.totalBudget.memory}, cpu=${data.totalBudget.steps}</p>\n"""
        )
        sb.append(
          "<input id=\"filter\" class=\"filter\" placeholder=\"filter rows by text…\" oninput=\"scalusFilter(this.value)\">\n"
        )

        sb.append("<nav class=\"tabs\">\n")
        sections.zipWithIndex.foreach { case ((id, title, _), i) =>
            val active = if i == 0 then " active" else ""
            sb.append(
              s"""<button class="tab-btn$active" data-tab="$id">${escapeHtml(title)}</button>\n"""
            )
        }
        sb.append("</nav>\n")

        sections.zipWithIndex.foreach { case ((id, _, content), i) =>
            val hidden = if i == 0 then "" else " hidden"
            sb.append(s"""<section class="tab-panel" id="$id"$hidden>\n""")
            sb.append(content)
            sb.append("</section>\n")
        }

        sb.append("<script>\n")
        sb.append(htmlScript)
        sb.append("\n</script>\n</body></html>")
        sb.toString
    }

    /** Source files referenced by `data`, loaded best-effort (only files readable from the current
      * working directory) so the HTML report can annotate them with per-line cost.
      *
      * @param include
      *   keep only files whose path satisfies this predicate. Use it to skip framework/library
      *   sources and annotate only your own contract — e.g. `!_.contains("/scalus-core/")`. For
      *   projects that depend on Scalus as a published artifact, library sources live in jars and
      *   are already excluded (they are not readable files).
      */
    def loadSources(
        data: ProfilingData,
        include: String => Boolean = _ => true
    ): Map[String, IndexedSeq[String]] =
        data.bySourceLocation
            .map(_.file)
            .distinct
            .filter(include)
            .filter(platform.fileExists)
            .map(f =>
                f -> new String(platform.readFile(f), "UTF-8").split("\r?\n", -1).toIndexedSeq
            )
            .toMap

    /** Write a self-contained, source-annotated HTML report to `path` (creating parent dirs).
      *
      * @param include
      *   source-file filter for the annotated-source view (see [[loadSources]])
      */
    def writeHtml(
        data: ProfilingData,
        path: String,
        include: String => Boolean = _ => true
    ): Unit =
        writeTo(path, toHtml(data, loadSources(data, include)))

    /** Write the CSV rendering to `path` (creating parent dirs). */
    def writeCsv(data: ProfilingData, path: String): Unit = writeTo(path, toCsv(data))

    /** Write the JSON rendering to `path` (creating parent dirs). */
    def writeJson(data: ProfilingData, path: String): Unit = writeTo(path, toJson(data))

    private def writeTo(path: String, content: String): Unit = {
        val sep = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))
        if sep > 0 then platform.createDirectories(path.substring(0, sep))
        platform.writeFile(path, content.getBytes("UTF-8"))
    }

    /** The "By Source Location" table. Each location cell is the root of a lazily-expanded tree of
      * *incoming* transitions: click to reveal where control flowed from to reach this location,
      * then keep drilling (from-from, …). The graph is embedded as compact JS data (O(edges)) and
      * each level is built on click, so deep/cyclic graphs don't explode the HTML. A node already
      * present on the current expansion path is shown as a leaf marked `↺ (loop)` rather than
      * expanded again, which cuts recursive loops.
      */
    private def appendSourceLocationTable(
        sb: StringBuilder,
        data: ProfilingData,
        totalCpu: Long
    ): Unit = {
        val rows = data.bySourceLocation
        if rows.isEmpty then sb.append("<p>(none recorded)</p>\n")
        else
            // Stable id for every location appearing in the table or in a transition endpoint.
            val allLocs =
                (rows.map(e => (e.file, e.line)) ++
                    data.transitions.flatMap(t =>
                        Seq((t.fromFile, t.fromLine), (t.toFile, t.toLine))
                    )).distinct
            val idOf = allLocs.zipWithIndex.toMap
            // Incoming edges per destination id (hottest first, capped).
            val inByDest: Map[Int, Seq[(Int, Long)]] =
                data.transitions
                    .groupBy(t => idOf((t.toFile, t.toLine)))
                    .view
                    .mapValues(ts =>
                        ts.map(t => (idOf((t.fromFile, t.fromLine)), t.count))
                            .sortBy(-_._2)
                            .take(treeChildCap)
                    )
                    .toMap

            // Embed the graph as JS data (labels + incoming adjacency) for lazy expansion.
            // Labels are escapeHtml'd (so they are safe both inside this inline <script> — no
            // </script> breakout — and when the JS re-inserts them via innerHTML, which decodes the
            // entities) then jsonEsc'd (valid JS string literal). Ids/counts are integers.
            sb.append("<script>\nvar SCALUS_LABEL={")
            sb.append(
              allLocs.zipWithIndex
                  .map((loc, i) =>
                      s"""$i:"${jsonEsc(escapeHtml(s"${shortFile(loc._1)}:${loc._2}"))}""""
                  )
                  .mkString(",")
            )
            sb.append("};\nvar SCALUS_IN={")
            sb.append(
              inByDest
                  .map((destId, ins) =>
                      s"$destId:[${ins.map((fid, c) => s"[$fid,$c]").mkString(",")}]"
                  )
                  .mkString(",")
            )
            sb.append("};\n</script>\n")

            val maxCpu = math.max(1L, rows.map(_.cpu).max)
            sb.append(
              "<table class=\"sortable\"><thead><tr><th>Location</th><th>Count</th><th>Memory</th><th>CPU</th><th>%cpu</th></tr></thead><tbody>\n"
            )
            rows.take(htmlRowCap).foreach { e =>
                val loc = s"${shortFile(e.file)}:${e.line}"
                val id = idOf((e.file, e.line))
                val hasIncoming = inByDest.get(id).exists(_.nonEmpty)
                val locCell =
                    if !hasIncoming then escapeHtml(loc)
                    else
                        s"""<span class="treenode" data-id="$id" data-path=""><span class="mark">▶</span> ${escapeHtml(
                              loc
                            )}</span><div class="subtree" hidden></div>"""
                val pct = oneDecimalPct(e.cpu, totalCpu)
                val barW = (e.cpu * 120 / maxCpu).toInt
                sb.append(
                  s"<tr><td>$locCell</td><td>${e.count}</td><td>${e.memory}</td><td>${e.cpu}</td>" +
                      s"<td><span class='bar' style='width:${barW}px'></span> $pct%</td></tr>\n"
                )
            }
            sb.append("</tbody></table>\n")
            if rows.size > htmlRowCap then
                sb.append(s"<p>… and ${rows.size - htmlRowCap} more rows</p>\n")
    }

    /** A sortable/filterable cost table: label column(s) + count, mem, cpu and a %-of-CPU bar. */
    private def appendCostTable(
        sb: StringBuilder,
        labelHeaders: Seq[String],
        rows: Seq[(Seq[String], Long, Long, Long)],
        totalCpu: Long
    ): Unit = {
        if rows.isEmpty then sb.append("<p>(none recorded)</p>\n")
        else
            val maxCpu = math.max(1L, rows.map(_._4).max)
            sb.append("<table class=\"sortable\"><thead><tr>")
            labelHeaders.foreach(h => sb.append(s"<th>${escapeHtml(h)}</th>"))
            sb.append(
              "<th>Count</th><th>Memory</th><th>CPU</th><th>%cpu</th></tr></thead><tbody>\n"
            )
            rows.take(htmlRowCap).foreach { case (labels, count, mem, cpu) =>
                val pct = oneDecimalPct(cpu, totalCpu)
                val barW = (cpu * 120 / maxCpu).toInt
                sb.append("<tr>")
                labels.foreach(l => sb.append(s"<td>${escapeHtml(l)}</td>"))
                sb.append(s"<td>$count</td><td>$mem</td><td>$cpu</td>")
                sb.append(s"<td><span class='bar' style='width:${barW}px'></span> $pct%</td>")
                sb.append("</tr>\n")
            }
            sb.append("</tbody></table>\n")
            if rows.size > htmlRowCap then
                sb.append(s"<p>… and ${rows.size - htmlRowCap} more rows</p>\n")
    }

    /** Per-line cost-annotated source: each line shown with its cpu/mem/count and a heat background
      * proportional to its share of the file's CPU. Only files present in `sources` are rendered.
      */
    private def renderAnnotatedSource(
        data: ProfilingData,
        sources: Map[String, IndexedSeq[String]]
    ): Option[String] = {
        val byFile = data.bySourceLocation.groupBy(_.file)
        val files =
            sources.keys.filter(byFile.contains).toSeq.sortBy(f => -byFile(f).map(_.cpu).sum)
        if files.isEmpty then return None
        val sb = new StringBuilder
        files.foreach { file =>
            val lines = sources(file)
            val perLine = byFile(file).map(e => e.line -> e).toMap
            val maxCpu = math.max(1L, byFile(file).map(_.cpu).max)
            sb.append(s"<h3>${escapeHtml(file)}</h3>\n<table class='src'>\n")
            lines.zipWithIndex.foreach { case (text, i) =>
                val ln = i + 1
                val e = perLine.get(ln)
                val cpu = e.map(_.cpu).getOrElse(0L)
                val mem = e.map(_.memory).getOrElse(0L)
                val cnt = e.map(_.count).getOrElse(0L)
                val rowStyle =
                    if cpu == 0 then ""
                    else
                        s" style='background:rgba(220,40,40,${0.08 + 0.5 * (cpu.toDouble / maxCpu)})'"
                val gut = if cpu == 0 then "" else s"$cpu cpu / $mem mem / $cnt×"
                sb.append(
                  s"<tr$rowStyle><td class='ln'>$ln</td><td class='cost'>$gut</td><td class='code'>${escapeHtml(text)}</td></tr>\n"
                )
            }
            sb.append("</table>\n")
        }
        Some(sb.toString)
    }

    /** Transition matrix heatmap: top-N locations by CPU on each axis; cell = transition count. */
    private def appendTransitionMatrix(sb: StringBuilder, data: ProfilingData): Unit = {
        val byLocCpu = data.bySourceLocation.map(e => (e.file, e.line) -> e.cpu).toMap
        val nodes = data.transitions
            .flatMap(t => Seq((t.fromFile, t.fromLine), (t.toFile, t.toLine)))
            .distinct
            .sortBy(loc => -byLocCpu.getOrElse(loc, 0L))
            .take(matrixCap)
        if nodes.isEmpty then return
        val idx = nodes.zipWithIndex.toMap
        val cells = scala.collection.mutable.HashMap[(Int, Int), Long]()
        var maxCount = 1L
        data.transitions.foreach { t =>
            for fi <- idx.get((t.fromFile, t.fromLine)); ti <- idx.get((t.toFile, t.toLine)) do
                val v = cells.getOrElse((fi, ti), 0L) + t.count
                cells((fi, ti)) = v
                if v > maxCount then maxCount = v
        }
        sb.append(
          s"<p>Top ${nodes.size} locations by CPU; cell = transition count (row = from, column = to).</p>\n"
        )
        sb.append("<table class='matrix'><tr><th>from \\ to</th>")
        nodes.indices.foreach(i => sb.append(s"<th>${i + 1}</th>"))
        sb.append("</tr>\n")
        nodes.zipWithIndex.foreach { case (loc, ri) =>
            sb.append(
              s"<tr><th class='rowhdr'>${ri + 1}. ${escapeHtml(shortFile(loc._1))}:${loc._2}</th>"
            )
            nodes.indices.foreach { ci =>
                val v = cells.getOrElse((ri, ci), 0L)
                val style =
                    if v == 0 then ""
                    else
                        s" style='background:rgba(40,120,220,${0.1 + 0.7 * (v.toDouble / maxCount)})'"
                sb.append(s"<td$style>${if v == 0 then "" else v.toString}</td>")
            }
            sb.append("</tr>\n")
        }
        sb.append("</table>\n<ol class='legend'>\n")
        nodes.foreach(loc => sb.append(s"<li>${escapeHtml(shortFile(loc._1))}:${loc._2}</li>\n"))
        sb.append("</ol>\n")
    }

    /** Sortable hot-edges table (from → to, count), the full edge list behind the matrix. */
    private def appendHotEdges(sb: StringBuilder, data: ProfilingData): Unit = {
        sb.append(
          "<table class='sortable'><thead><tr><th>From</th><th>To</th><th>Count</th></tr></thead><tbody>\n"
        )
        data.transitions.take(htmlRowCap).foreach { t =>
            sb.append(
              s"<tr><td>${escapeHtml(shortFile(t.fromFile))}:${t.fromLine}</td><td>${escapeHtml(shortFile(t.toFile))}:${t.toLine}</td><td>${t.count}</td></tr>\n"
            )
        }
        sb.append("</tbody></table>\n")
        if data.transitions.size > htmlRowCap then
            sb.append(s"<p>… and ${data.transitions.size - htmlRowCap} more edges</p>\n")
    }

    private def csvRow(
        section: String,
        key: String,
        detail: String,
        count: Long,
        mem: Long,
        cpu: Long
    ): String =
        s"${csvEscape(section)},${csvEscape(key)},${csvEscape(detail)},$count,$mem,$cpu\n"

    private def csvEscape(s: String): String =
        if s.exists(c => c == ',' || c == '"' || c == '\n' || c == '\r') then
            "\"" + s.replace("\"", "\"\"") + "\""
        else s

    private def jsonEsc(s: String): String =
        s.flatMap {
            case '"'          => "\\\""
            case '\\'         => "\\\\"
            case '\n'         => "\\n"
            case '\r'         => "\\r"
            case '\t'         => "\\t"
            case c if c < ' ' => "\\u%04x".format(c.toInt)
            case c            => c.toString
        }

    private def formatRow(
        col1: String,
        count: String,
        mem: String,
        cpu: String,
        col1Width: Int,
        countWidth: Int,
        memWidth: Int,
        cpuWidth: Int
    ): String = {
        s"${col1.padTo(col1Width, ' ')}  ${count.reverse.padTo(countWidth, ' ').reverse}  ${mem.reverse.padTo(memWidth, ' ').reverse}  ${cpu.reverse.padTo(cpuWidth, ' ').reverse}"
    }

    /** Shorten file path to just the filename without extension. */
    private def shortFile(path: String): String = {
        val sep = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))
        val name = if sep >= 0 then path.substring(sep + 1) else path
        val dot = name.lastIndexOf('.')
        if dot > 0 then name.substring(0, dot) else name
    }

    private def escapeHtml(s: String): String =
        s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

    /** `num/denom` as a percentage with one decimal, computed with integer math so the result is
      * independent of the platform default locale (`f"%.1f"` would emit `,` on e.g. de_DE and break
      * the rendered tables / CSS).
      */
    private def oneDecimalPct(num: Long, denom: Long): String =
        val permille = if denom <= 0 then 0L else num * 1000 / denom
        s"${permille / 10}.${permille % 10}"

    private val htmlStyle: String =
        """body { font-family: monospace; margin: 20px; background: #fafafa; color: #222; }
h1 { font-size: 1.4em; } h2 { color: #333; margin-top: 1.4em; } h3 { color: #555; margin: 0.6em 0 0.2em; }
.filter { width: 320px; padding: 4px 8px; margin: 8px 0; font-family: monospace; }
table { border-collapse: collapse; margin-bottom: 12px; }
th, td { padding: 3px 10px; text-align: right; border: 1px solid #ddd; }
td:first-child, th:first-child { text-align: left; }
thead th { background: #f0f0f0; cursor: pointer; user-select: none; }
thead th:hover { background: #e0e8f0; }
tbody tr:hover { background: #e8f4fd; }
.bar { display: inline-block; height: 10px; background: #4a90d9; vertical-align: middle; }
.summary { font-size: 1.1em; margin: 10px 0; }
table.matrix td { min-width: 22px; text-align: center; }
table.matrix th.rowhdr { text-align: left; font-weight: normal; }
ol.legend { color: #555; font-size: 0.9em; }
table.src { border: none; width: 100%; }
table.src td { border: none; padding: 0 8px; }
table.src td.ln { color: #999; text-align: right; user-select: none; }
table.src td.cost { color: #b00; text-align: right; white-space: nowrap; }
table.src td.code { text-align: left; white-space: pre; }
nav.tabs { position: sticky; top: 0; background: #fafafa; padding: 8px 0; margin-bottom: 12px; border-bottom: 1px solid #ccc; z-index: 10; }
.tab-btn { font-family: monospace; font-size: 0.95em; padding: 5px 12px; margin: 0 4px 0 0; border: 1px solid #ccc; background: #eee; color: #333; cursor: pointer; border-radius: 4px 4px 0 0; }
.tab-btn:hover { background: #e0e8f0; }
.tab-btn.active { background: #4a90d9; border-color: #4a90d9; color: #fff; }
.tab-panel[hidden] { display: none; }
.treenode { cursor: pointer; }
.treenode .mark { color: #4a90d9; font-size: 0.8em; }
.subtree { margin-left: 14px; border-left: 1px solid #ddd; padding-left: 8px; }
.treeleaf { color: #999; }"""

    private val htmlScript: String =
        """function scalusFilter(q){q=(q||'').toLowerCase();document.querySelectorAll('table.sortable tbody tr').forEach(function(tr){tr.style.display=tr.textContent.toLowerCase().indexOf(q)>=0?'':'none';});}
function scalusBuildChildren(container,id,path){
  var ins=(typeof SCALUS_IN!=='undefined'&&SCALUS_IN[id])||[];
  if(!ins.length){container.innerHTML='<div class="treeleaf">(no incoming)</div>';return;}
  var h='';
  for(var i=0;i<ins.length;i++){
    var cid=ins[i][0],cnt=ins[i][1],lbl=(typeof SCALUS_LABEL!=='undefined'&&SCALUS_LABEL[cid])||('#'+cid);
    if(path.indexOf(cid)>=0){
      h+='<div class="treeleaf">↺ '+lbl+' ×'+cnt+' (loop)</div>';
    }else{
      h+='<div class="treerow"><span class="treenode" data-id="'+cid+'" data-path="'+path.join(',')+'"><span class="mark">▶</span> '+lbl+' ×'+cnt+'</span><div class="subtree" hidden></div></div>';
    }
  }
  container.innerHTML=h;
}
function scalusToggle(node){
  var sub=node.nextElementSibling;
  if(!sub)return;
  if(node.getAttribute('data-built')!=='1'){
    var id=+node.getAttribute('data-id');
    var path=(node.getAttribute('data-path')||'').split(',').filter(Boolean).map(Number);
    scalusBuildChildren(sub,id,path.concat([id]));
    node.setAttribute('data-built','1');
  }
  sub.hidden=!sub.hidden;
  var m=node.querySelector('.mark');if(m)m.textContent=sub.hidden?'▶':'▼';
}
document.addEventListener('click',function(e){
  var t=e.target;
  while(t&&t.nodeType===1){if(t.classList&&t.classList.contains('treenode')){scalusToggle(t);return;}t=t.parentNode;}
});
function scalusTab(id){
  document.querySelectorAll('.tab-panel').forEach(function(p){p.hidden=(p.id!==id);});
  document.querySelectorAll('.tab-btn').forEach(function(b){b.classList.toggle('active', b.getAttribute('data-tab')===id);});
}
document.querySelectorAll('.tab-btn').forEach(function(b){b.addEventListener('click',function(){scalusTab(b.getAttribute('data-tab'));});});
document.querySelectorAll('table.sortable thead th').forEach(function(th){
  th.addEventListener('click',function(){
    var ci=Array.prototype.indexOf.call(th.parentNode.children, th);
    var tb=th.closest('table').querySelector('tbody');
    var rows=Array.prototype.slice.call(tb.querySelectorAll('tr'));
    var asc=th.getAttribute('data-asc')!=='1';
    th.setAttribute('data-asc',asc?'1':'0');
    rows.sort(function(a,b){
      var x=((a.children[ci]||{}).textContent||'').trim();
      var y=((b.children[ci]||{}).textContent||'').trim();
      var nx=parseFloat(x.replace(/[^0-9.\-]/g,'')), ny=parseFloat(y.replace(/[^0-9.\-]/g,''));
      var c=(!isNaN(nx)&&!isNaN(ny))?(nx-ny):(x<y?-1:(x>y?1:0));
      return asc?c:-c;
    });
    rows.forEach(function(r){tb.appendChild(r);});
  });
});"""
}
