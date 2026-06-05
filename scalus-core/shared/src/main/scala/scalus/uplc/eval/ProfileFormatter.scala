package scalus.uplc.eval

import scalus.cardano.ledger.{ExUnitPrices, ExUnits}
import scalus.uplc.builtin.platform

object ProfileFormatter {

    /** On-chain fee (lovelace) for `(mem, cpu)` under `prices`, or `None` when no prices are set.
      * Reuses the ledger fee formula so it matches what a transaction would actually be charged.
      */
    private def feeLovelace(prices: Option[ExUnitPrices], mem: Long, cpu: Long): Option[Long] =
        prices.map(p => ExUnits(memory = mem, steps = cpu).fee(p).value)

    /** Estimated cost carried by each transition edge A→B: the target node's self-cost distributed
      * across B's incoming edges by traversal frequency —
      * `edgeCost(A→B) = selfCost(B) · count(A→B) / Σ_X count(X→B)`. Always finite (no recursion),
      * and the incoming edges of B sum back (up to integer rounding) to B's self-cost. Returns one
      * `(edge, mem, cpu)` per transition.
      */
    private def edgeCosts(data: ProfilingData): Seq[(SourceTransition, Long, Long)] = {
        val selfByLoc = data.bySourceLocation.map(e => (e.file, e.line) -> (e.memory, e.cpu)).toMap
        val inDeg = data.transitions.groupMapReduce(t => (t.toFile, t.toLine))(_.count)(_ + _)
        data.transitions.map { t =>
            val (mem, cpu) = selfByLoc.getOrElse((t.toFile, t.toLine), (0L, 0L))
            val d = inDeg.getOrElse((t.toFile, t.toLine), 0L)
            if d <= 0 then (t, 0L, 0L) else (t, mem * t.count / d, cpu * t.count / d)
        }
    }

    /** NOTE: currently unwired — the Inclusive column was removed from the report because the
      * context-insensitive recurrence below degenerates on real UPLC (share-dilution collapses
      * entry/gateway nodes to their self-cost, and the synthetic compile-boundary root
      * ([[scalus.compiler.CompiledProgramRoot]]) plus any annotation-fallback hub fuse unrelated
      * locations into one SCC that all report the cycle's lump sum). Kept for reference while a
      * per-frame (Callgrind-style) replacement is considered.
      *
      * Inclusive (cumulative) cost per source location over the call graph (`data.transitions` =
      * caller→callee call edges): a node's self-cost plus the cost of everything it calls, with
      * each callee's inclusive cost shared back to its callers by call frequency. Returns one
      * `(mem, cpu)` per location.
      *
      * The call graph can still contain cycles (recursion, higher-order calls), on which the naive
      * recurrence diverges, so strongly-connected components are collapsed with Tarjan's algorithm
      * and the inclusive cost is accumulated over the resulting DAG (Tarjan emits SCCs in
      * reverse-topological order, so each component's successors are already done when it is
      * processed). Every node in a recursive nest shares the nest's total; roots sum to the budget.
      */
    private def inclusiveCosts(data: ProfilingData): Map[(String, Int), (Long, Long)] = {
        val nodes: Array[(String, Int)] =
            (data.bySourceLocation.map(e => (e.file, e.line)) ++
                data.transitions.flatMap(t =>
                    Seq((t.fromFile, t.fromLine), (t.toFile, t.toLine))
                )).distinct.toArray
        val n = nodes.length
        if n == 0 then return Map.empty
        val nodeIdx = nodes.zipWithIndex.toMap
        val selfMem = new Array[Long](n)
        val selfCpu = new Array[Long](n)
        data.bySourceLocation.foreach { e =>
            val i = nodeIdx((e.file, e.line)); selfMem(i) = e.memory; selfCpu(i) = e.cpu
        }
        val succ = Array.fill(n)(scala.collection.mutable.ArrayBuffer[Int]())
        data.transitions.foreach { t =>
            val u = nodeIdx((t.fromFile, t.fromLine)); val v = nodeIdx((t.toFile, t.toLine))
            if u != v then succ(u) += v
        }
        val succA = succ.map(_.distinct.toArray)

        // Iterative Tarjan SCC (explicit stacks so deep graphs don't blow the JVM stack).
        val index = Array.fill(n)(-1)
        val low = new Array[Int](n)
        val onStack = new Array[Boolean](n)
        val tStack = scala.collection.mutable.ArrayBuffer[Int]()
        val sccId = Array.fill(n)(-1)
        val sccOrder = scala.collection.mutable.ArrayBuffer[Int]() // emission order = reverse topo
        var counter = 0
        var sccCount = 0
        val work = scala.collection.mutable.ArrayBuffer[(Int, Int)]() // (node, next succ index)
        var start = 0
        while start < n do
            if index(start) == -1 then
                work += ((start, 0))
                while work.nonEmpty do
                    val (v, pi) = work.last
                    if pi == 0 then
                        index(v) = counter; low(v) = counter; counter += 1
                        tStack += v; onStack(v) = true
                    if pi < succA(v).length then
                        work(work.length - 1) = (v, pi + 1)
                        val w = succA(v)(pi)
                        if index(w) == -1 then work += ((w, 0))
                        else if onStack(w) then low(v) = math.min(low(v), index(w))
                    else
                        if low(v) == index(v) then
                            var done = false
                            while !done do
                                val w = tStack.remove(tStack.length - 1)
                                onStack(w) = false; sccId(w) = sccCount
                                if w == v then done = true
                            sccOrder += sccCount; sccCount += 1
                        work.remove(work.length - 1)
                        if work.nonEmpty then
                            val parent = work.last._1
                            low(parent) = math.min(low(parent), low(v))
            start += 1

        // Condensation: aggregate cross-SCC edge counts, external in-degree, per-SCC self cost.
        val condCount = scala.collection.mutable.HashMap[(Int, Int), Long]()
        data.transitions.foreach { t =>
            val u = sccId(nodeIdx((t.fromFile, t.fromLine)))
            val v = sccId(nodeIdx((t.toFile, t.toLine)))
            if u != v then condCount((u, v)) = condCount.getOrElse((u, v), 0L) + t.count
        }
        val inDegExt = new Array[Long](sccCount)
        val outEdges = Array.fill(sccCount)(scala.collection.mutable.ArrayBuffer[(Int, Long)]())
        condCount.foreach { case ((u, v), c) => inDegExt(v) += c; outEdges(u) += ((v, c)) }
        val sccMem = new Array[Long](sccCount)
        val sccCpu = new Array[Long](sccCount)
        var i = 0
        while i < n do
            sccMem(sccId(i)) += selfMem(i); sccCpu(sccId(i)) += selfCpu(i); i += 1

        // Accumulate inclusive cost over the DAG in reverse-topological (emission) order.
        val incMem = new Array[Long](sccCount)
        val incCpu = new Array[Long](sccCount)
        sccOrder.foreach { u =>
            var m = sccMem(u); var c = sccCpu(u)
            outEdges(u).foreach { case (v, cnt) =>
                val d = inDegExt(v)
                if d > 0 then { m += incMem(v) * cnt / d; c += incCpu(v) * cnt / d }
            }
            incMem(u) = m; incCpu(u) = c
        }

        nodes.indices.map(j => nodes(j) -> (incMem(sccId(j)), incCpu(sccId(j)))).toMap
    }

    /** `lovelace` rendered as ADA with 6 decimals, using integer math so the output is independent
      * of the platform default locale (1 ADA = 1_000_000 lovelace).
      */
    private def adaString(lovelace: Long): String = {
        val frac = (math.abs(lovelace) % 1_000_000L).toString.reverse.padTo(6, '0').reverse
        s"${lovelace / 1_000_000L}.$frac"
    }

    /** Max rows rendered per HTML table before collapsing into a "… and N more" note. */
    private val htmlRowCap = 200

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

        val totalFee = feeLovelace(data.prices, data.totalBudget.memory, data.totalBudget.steps)
            .map(f => s" fee=$f lovelace (${adaString(f)} ADA)")
            .getOrElse("")
        sb.append(s"\nTotal: mem=${data.totalBudget.memory} cpu=${data.totalBudget.steps}$totalFee")
        sb.toString
    }

    /** Compact summary suitable for console / log output: total budget plus the top-N by CPU. */
    def summary(data: ProfilingData, topN: Int = 10): String = {
        val n = math.max(1, topN)
        val sb = new StringBuilder
        // ` fee=<lovelace>` for an entry's (mem, cpu), or "" when no prices are attached.
        def feeStr(mem: Long, cpu: Long): String =
            feeLovelace(data.prices, mem, cpu).map(f => s" fee=$f").getOrElse("")
        sb.append(
          s"Total: mem=${data.totalBudget.memory} cpu=${data.totalBudget.steps}${feeStr(data.totalBudget.memory, data.totalBudget.steps)}\n"
        )
        if data.byFunction.nonEmpty then
            sb.append(s"Top $n by CPU (builtin):\n")
            data.byFunction.sortBy(e => -e.cpu).take(n).foreach { e =>
                sb.append(
                  "  " + e.name.padTo(
                    24,
                    ' '
                  ) + s" cpu=${e.cpu} mem=${e.memory} n=${e.count}${feeStr(e.memory, e.cpu)}\n"
                )
            }
        if data.bySourceLocation.nonEmpty then
            sb.append(s"Top $n by CPU (source):\n")
            data.bySourceLocation.sortBy(e => -e.cpu).take(n).foreach { e =>
                val loc = s"${shortFile(e.file)}:${e.line}"
                sb.append(
                  "  " + loc.padTo(
                    24,
                    ' '
                  ) + s" cpu=${e.cpu} mem=${e.memory} n=${e.count}${feeStr(e.memory, e.cpu)}\n"
                )
            }
        sb.toString
    }

    /** Renders profiling data as CSV (one diffable file across all sections). When the data carries
      * execution unit prices, a trailing `fee` column (lovelace) is added.
      */
    def toCsv(data: ProfilingData): String = {
        def fee(mem: Long, cpu: Long): Option[Long] = feeLovelace(data.prices, mem, cpu)
        val sb = new StringBuilder
        sb.append(
          s"section,key,detail,count,mem,cpu${if data.prices.isDefined then ",fee" else ""}\n"
        )
        data.bySourceLocation.foreach(e =>
            sb.append(
              csvRow(
                "location",
                s"${e.file}:${e.line}",
                "",
                e.count,
                e.memory,
                e.cpu,
                fee(e.memory, e.cpu)
              )
            )
        )
        data.byFunction.foreach(e =>
            sb.append(csvRow("builtin", e.name, "", e.count, e.memory, e.cpu, fee(e.memory, e.cpu)))
        )
        data.byLocationFunction.foreach(e =>
            sb.append(
              csvRow(
                "loc-builtin",
                s"${e.file}:${e.line}",
                e.functionName,
                e.count,
                e.memory,
                e.cpu,
                fee(e.memory, e.cpu)
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
                0,
                fee(0, 0)
              )
            )
        )
        sb.append(
          csvRow(
            "total",
            "",
            "",
            0,
            data.totalBudget.memory,
            data.totalBudget.steps,
            fee(data.totalBudget.memory, data.totalBudget.steps)
          )
        )
        sb.toString
    }

    /** Renders the full profiling data as JSON (machine-readable). When the data carries execution
      * unit prices, every entry and the total also include a derived `"fee"` in lovelace.
      */
    def toJson(data: ProfilingData): String = {
        // `,"fee":<lovelace>` for an entry's (mem, cpu), or "" when no prices are attached.
        def feeJson(mem: Long, cpu: Long): String =
            feeLovelace(data.prices, mem, cpu).map(f => s""","fee":$f""").getOrElse("")
        val sb = new StringBuilder
        sb.append("{\n")
        sb.append(
          s"""  "totalBudget": { "mem": ${data.totalBudget.memory}, "cpu": ${data.totalBudget.steps}${feeJson(
                data.totalBudget.memory,
                data.totalBudget.steps
              )} },\n"""
        )
        sb.append("  \"bySourceLocation\": [")
        sb.append(
          data.bySourceLocation
              .map(e =>
                  s"""{"file":"${jsonEsc(
                        e.file
                      )}","line":${e.line},"count":${e.count},"mem":${e.memory},"cpu":${e.cpu}${feeJson(
                        e.memory,
                        e.cpu
                      )}}"""
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
                      )}","count":${e.count},"mem":${e.memory},"cpu":${e.cpu}${feeJson(
                        e.memory,
                        e.cpu
                      )}}"""
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
                      )}","count":${e.count},"mem":${e.memory},"cpu":${e.cpu}${feeJson(
                        e.memory,
                        e.cpu
                      )}}"""
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
      * %-of-CPU bars, a cost-ranked hot-edges table, and — when source text is supplied — a
      * per-line cost-annotated source view (the flagship view).
      *
      * @param data
      *   the profiling data
      * @param sources
      *   optional `file -> source lines` (line `n` is element `n - 1`); profiled files present here
      *   are rendered with per-line cost annotations
      * @param title
      *   optional label identifying what was profiled (e.g. the contract and test-case name). When
      *   non-empty it is shown in the page `<title>` and as a subtitle above the report.
      */
    def toHtml(
        data: ProfilingData,
        sources: Map[String, IndexedSeq[String]],
        title: String = ""
    ): String = {
        val totalCpu = math.max(1L, data.totalBudget.steps)

        // Build each section's inner HTML; only non-empty sections become tabs. Section titles
        // live on the tab buttons, so the panels themselves carry no <h2>.
        val sections = scala.collection.mutable.ArrayBuffer[(String, String, String)]()
        def section(id: String, title: String)(render: StringBuilder => Unit): Unit = {
            val b = new StringBuilder
            render(b)
            sections += ((id, title, b.toString))
        }

        section("src", "By Source Location")(
          appendSourceLocationTable(_, data, totalCpu, sources.keySet)
        )

        renderHotPaths(data, sources.keySet).foreach(html =>
            sections += (("hotpaths", "Hot Paths", html))
        )

        renderAnnotatedSource(data, sources).foreach(html =>
            sections += (("annotated", "Annotated Source", html))
        )

        section("fun", "By Builtin")(
          appendCostTable(
            _,
            Seq("Builtin"),
            data.byFunction.map(e => (Seq(e.name), e.count, e.memory, e.cpu)),
            totalCpu,
            data.prices
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
                totalCpu,
                data.prices
              )
            )

        if data.transitions.nonEmpty then section("edges", "Hot Edges")(appendHotEdges(_, data))

        val sb = new StringBuilder
        val pageTitle =
            if title.isEmpty then "Scalus Profile" else s"Scalus Profile — ${escapeHtml(title)}"
        sb.append(
          s"""<!DOCTYPE html>\n<html><head><meta charset="utf-8"><title>$pageTitle</title>\n<style>\n"""
        )
        sb.append(htmlStyle)
        sb.append("\n</style></head><body>\n<h1>Scalus CEK Machine Profile</h1>\n")
        if title.nonEmpty then sb.append(s"""<p class="subtitle">${escapeHtml(title)}</p>\n""")
        val totalFeeStr =
            feeLovelace(data.prices, data.totalBudget.memory, data.totalBudget.steps)
                .map(f => s", fee=$f lovelace (${adaString(f)} ADA)")
                .getOrElse("")
        sb.append(
          s"""<p class="summary">Total budget: mem=${data.totalBudget.memory}, cpu=${data.totalBudget.steps}$totalFeeStr</p>\n"""
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
      * @param title
      *   optional label identifying what was profiled (e.g. the contract and test-case name); shown
      *   in the page `<title>` and as a subtitle above the report (see [[toHtml]])
      */
    def writeHtml(
        data: ProfilingData,
        path: String,
        include: String => Boolean = _ => true,
        title: String = ""
    ): Unit =
        writeTo(path, toHtml(data, loadSources(data, include), title))

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
        totalCpu: Long,
        annotatedFiles: Set[String]
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
            // Estimated cost on each transition edge (target self-cost × entry-frequency share),
            // shown as fee when priced else cpu; used on tree branches and the Downstream column.
            val edgeCostList = edgeCosts(data)
            val edgeDisplayCost: Map[(String, Int, String, Int), Long] =
                edgeCostList.map { case (t, mem, cpu) =>
                    (t.fromFile, t.fromLine, t.toFile, t.toLine) ->
                        feeLovelace(data.prices, mem, cpu).getOrElse(cpu)
                }.toMap
            // Downstream cost a location pushes into its callees = Σ over its outgoing edges.
            val downstreamByLoc: Map[(String, Int), Long] =
                edgeCostList
                    .groupMapReduce(c => (c._1.fromFile, c._1.fromLine))(c =>
                        feeLovelace(data.prices, c._2, c._3).getOrElse(c._3)
                    )(_ + _)
            // Incoming edges per destination id (hottest first, capped): (sourceId, count, edgeCost).
            val inByDest: Map[Int, Seq[(Int, Long, Long)]] =
                data.transitions
                    .groupBy(t => idOf((t.toFile, t.toLine)))
                    .view
                    .mapValues(ts =>
                        ts.map(t =>
                            (
                              idOf((t.fromFile, t.fromLine)),
                              t.count,
                              edgeDisplayCost((t.fromFile, t.fromLine, t.toFile, t.toLine))
                            )
                        ).sortBy(-_._2)
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
                      s"$destId:[${ins.map((fid, c, cost) => s"[$fid,$c,$cost]").mkString(",")}]"
                  )
                  .mkString(",")
            )
            sb.append(s"};\nvar SCALUS_FEE=${data.prices.isDefined};\n</script>\n")

            val maxCpu = math.max(1L, rows.map(_.cpu).max)
            val feeHeader = if data.prices.isDefined then "<th>Fee (lov)</th>" else ""
            val unit = if data.prices.isDefined then "lov" else "cpu"
            val hasEdges = data.transitions.nonEmpty
            val dsHeader = if hasEdges then s"<th>Downstream ($unit)</th>" else ""
            sb.append(
              s"<table class=\"sortable\"><thead><tr><th>Location</th><th>Count</th><th>Memory</th><th>CPU</th>$feeHeader$dsHeader<th>%cpu</th></tr></thead><tbody>\n"
            )
            rows.take(htmlRowCap).foreach { e =>
                val loc = s"${shortFile(e.file)}:${e.line}"
                val id = idOf((e.file, e.line))
                val hasIncoming = inByDest.get(id).exists(_.nonEmpty)
                // Jump to this line in the Annotated Source tab (only when its source is annotated).
                val goto =
                    if annotatedFiles.contains(e.file) then
                        s"""<a class="goto" title="show in Annotated Source" onclick="scalusGoto('${locAnchor(
                              e.file,
                              e.line
                            )}')">⇲</a> """
                    else ""
                val locCell =
                    if !hasIncoming then goto + escapeHtml(loc)
                    else
                        goto +
                            s"""<span class="treenode" data-id="$id" data-path=""><span class="mark">▶</span> ${escapeHtml(
                                  loc
                                )}</span><div class="subtree" hidden></div>"""
                val pct = oneDecimalPct(e.cpu, totalCpu)
                val barW = (e.cpu * 120 / maxCpu).toInt
                val feeCell =
                    feeLovelace(data.prices, e.memory, e.cpu).map(f => s"<td>$f</td>").getOrElse("")
                val dsCell =
                    if !hasEdges then ""
                    else s"<td>${downstreamByLoc.getOrElse((e.file, e.line), 0L)}</td>"
                sb.append(
                  s"<tr><td>$locCell</td><td>${e.count}</td><td>${e.memory}</td><td>${e.cpu}</td>$feeCell$dsCell" +
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
        totalCpu: Long,
        prices: Option[ExUnitPrices]
    ): Unit = {
        if rows.isEmpty then sb.append("<p>(none recorded)</p>\n")
        else
            val maxCpu = math.max(1L, rows.map(_._4).max)
            val feeHeader = if prices.isDefined then "<th>Fee (lov)</th>" else ""
            sb.append("<table class=\"sortable\"><thead><tr>")
            labelHeaders.foreach(h => sb.append(s"<th>${escapeHtml(h)}</th>"))
            sb.append(
              s"<th>Count</th><th>Memory</th><th>CPU</th>$feeHeader<th>%cpu</th></tr></thead><tbody>\n"
            )
            rows.take(htmlRowCap).foreach { case (labels, count, mem, cpu) =>
                val pct = oneDecimalPct(cpu, totalCpu)
                val barW = (cpu * 120 / maxCpu).toInt
                sb.append("<tr>")
                labels.foreach(l => sb.append(s"<td>${escapeHtml(l)}</td>"))
                sb.append(s"<td>$count</td><td>$mem</td><td>$cpu</td>")
                feeLovelace(prices, mem, cpu).foreach(f => sb.append(s"<td>$f</td>"))
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
                val gut =
                    if cpu == 0 then ""
                    else
                        val feeStr =
                            feeLovelace(data.prices, mem, cpu).map(f => s" / $f lov").getOrElse("")
                        s"$cpu cpu / $mem mem / $cnt×$feeStr"
                // Anchor profiled lines so the By Source Location tab can jump straight here.
                val anchor = if e.isDefined then s" id=\"${locAnchor(file, ln)}\"" else ""
                sb.append(
                  s"<tr$anchor$rowStyle><td class='ln'>$ln</td><td class='cost'>$gut</td><td class='code'>${escapeHtml(text)}</td></tr>\n"
                )
            }
            sb.append("</table>\n")
        }
        Some(sb.toString)
    }

    /** Hottest control-flow edges, ranked by estimated cost (see [[edgeCosts]]). The Est. Memory /
      * Est. CPU / Fee are the target node's self-cost distributed across its incoming edges by
      * traversal frequency, so an edge's columns answer "how much budget flows across this call".
      */
    private def appendHotEdges(sb: StringBuilder, data: ProfilingData): Unit = {
        val feeHeader = if data.prices.isDefined then "<th>Fee (lov)</th>" else ""
        sb.append(
          s"<table class='sortable'><thead><tr><th>From</th><th>To</th><th>Count</th><th>Est. Memory</th><th>Est. CPU</th>$feeHeader</tr></thead><tbody>\n"
        )
        edgeCosts(data)
            .sortBy { case (_, _, cpu) => -cpu }
            .take(htmlRowCap)
            .foreach { case (t, mem, cpu) =>
                val feeCell =
                    feeLovelace(data.prices, mem, cpu).map(f => s"<td>$f</td>").getOrElse("")
                sb.append(
                  s"<tr><td>${escapeHtml(shortFile(t.fromFile))}:${t.fromLine}</td><td>${escapeHtml(shortFile(t.toFile))}:${t.toLine}</td><td>${t.count}</td><td>$mem</td><td>$cpu</td>$feeCell</tr>\n"
                )
            }
        sb.append("</tbody></table>\n")
        if data.transitions.size > htmlRowCap then
            sb.append(s"<p>… and ${data.transitions.size - htmlRowCap} more edges</p>\n")
    }

    /** "Hot Paths": where the budget flows from the program entry, as a bounded tree of the
      * heaviest call edges. Grown best-first (heaviest edge first) from `data.entry` up to a node
      * cap, so it branches — showing the several hot paths, not a single spine. Each node is
      * reached via its heaviest incoming edge (cycles add a node once), and each edge is labelled
      * with the cost it carries (the [[edgeCosts]] estimate, fee when priced). Anchoring at the
      * real entry keeps the contract's own code at the root; descending by per-edge cost sidesteps
      * the share-dilution that makes a node's *total* inclusive cost a poor guide here.
      */
    private def renderHotPaths(data: ProfilingData, contractFiles: Set[String]): Option[String] = {
        // Root at the first executed location that belongs to a contract file (so the contract's own
        // entry is the root, not the framework/annotation-fallback step that happens to run first);
        // fall back to the literal first step when no contract files are known.
        val entry = data.entryTrace
            .find(n => contractFiles.contains(n._1))
            .orElse(data.entryTrace.headOption)
            .getOrElse(return None)
        val unit = if data.prices.isDefined then "lov" else "cpu"
        val adj = scala.collection.mutable.HashMap[
          (String, Int),
          scala.collection.mutable.ArrayBuffer[((String, Int), Long)]
        ]()
        edgeCosts(data).foreach { case (t, mem, cpu) =>
            val c = feeLovelace(data.prices, mem, cpu).getOrElse(cpu)
            if c > 0 then
                adj.getOrElseUpdate(
                  (t.fromFile, t.fromLine),
                  scala.collection.mutable.ArrayBuffer()
                )
                    += (((t.toFile, t.toLine), c))
        }
        if !adj.contains(entry) then return None
        val selfOf = data.bySourceLocation
            .map(e =>
                (e.file, e.line) -> feeLovelace(data.prices, e.memory, e.cpu).getOrElse(e.cpu)
            )
            .toMap

        val maxNodes = 40
        val inTree = scala.collection.mutable.HashSet[(String, Int)](entry)
        val children =
            scala.collection.mutable.HashMap[
              (String, Int),
              scala.collection.mutable.ArrayBuffer[((String, Int), Long)]
            ]()
        // Best-first: always grow the heaviest pending edge whose source is already in the tree.
        // Supply the ordering via `given` rather than positionally: `PriorityQueue.empty` takes it
        // as a context-bound/using param, and passing it positionally fails on Scala 3.5+ (3.8.x)
        // while the explicit `given` type lets `Ordering.by(_._1)` infer on both 3.3.x and 3.8.x.
        given Ordering[(Long, (String, Int), (String, Int))] = Ordering.by(_._1)
        val pq =
            scala.collection.mutable.PriorityQueue.empty[(Long, (String, Int), (String, Int))]
        adj(entry).foreach { case (to, c) => pq.enqueue((c, entry, to)) }
        while inTree.size < maxNodes && pq.nonEmpty do
            val (c, from, to) = pq.dequeue()
            if !inTree.contains(to) then
                inTree += to
                children.getOrElseUpdate(from, scala.collection.mutable.ArrayBuffer()) += ((to, c))
                adj.getOrElse(to, Nil).foreach { case (t2, c2) =>
                    if !inTree.contains(t2) then pq.enqueue((c2, to, t2))
                }

        val sb = new StringBuilder
        sb.append(
          s"<p>Where the budget flows from the program entry: the heaviest call edges (cost in $unit carried by each call), branching to show the top paths.</p>\n"
        )
        def renderNode(node: (String, Int), carried: String): Unit = {
            val loc = escapeHtml(s"${shortFile(node._1)}:${node._2}")
            sb.append(s"<li><span class='loc'>$loc</span> <span class='c'>$carried</span>")
            children.get(node).foreach { cs =>
                sb.append("<ul>")
                cs.sortBy(-_._2).foreach((ch, c) => renderNode(ch, s"· $c $unit"))
                sb.append("</ul>")
            }
            sb.append("</li>\n")
        }
        sb.append("<ul class='hottree'>\n")
        renderNode(entry, s"(entry, self ${selfOf.getOrElse(entry, 0L)} $unit)")
        sb.append("</ul>\n")
        Some(sb.toString)
    }

    private def csvRow(
        section: String,
        key: String,
        detail: String,
        count: Long,
        mem: Long,
        cpu: Long,
        fee: Option[Long]
    ): String =
        s"${csvEscape(section)},${csvEscape(key)},${csvEscape(detail)},$count,$mem,$cpu${fee
                .map(f => s",$f")
                .getOrElse("")}\n"

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

    /** Deterministic, injection-safe HTML anchor id for a `(file, line)`, computed identically in
      * the By Source Location table and the Annotated Source view so one can link to the other.
      *
      * Output is `loc_<hex>_<line>` — only `[a-z0-9_]`, so it is safe to embed unescaped in both an
      * `id` attribute and the `onclick` JS string literal; the unescaped call sites rely on that.
      * Uses a hash of the full path (not a sanitized path) so distinct paths like `a/b.scala` and
      * `a.b.scala` don't collapse to the same anchor.
      */
    private def locAnchor(file: String, line: Int): String =
        "loc_" + Integer.toHexString(file.hashCode) + "_" + line

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
.subtitle { font-size: 1.25em; font-weight: 600; color: #2a5b8c; margin: -6px 0 8px; }
.hottree, .hottree ul { list-style: none; padding-left: 0; }
.hottree ul { margin-left: 12px; border-left: 1px solid #ddd; padding-left: 10px; }
.hottree li { margin: 2px 0; }
.hottree .loc { font-weight: 600; }
.hottree .c { color: #b5651d; margin-left: 6px; }
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
.treeleaf { color: #999; }
a.goto { cursor: pointer; color: #4a90d9; text-decoration: none; margin-right: 2px; }
a.goto:hover { color: #1c5fa8; }
table.src tr.hl { background: #fff3cd !important; outline: 2px solid #f0ad4e; }"""

    private val htmlScript: String =
        """function scalusFilter(q){q=(q||'').toLowerCase();document.querySelectorAll('table.sortable tbody tr').forEach(function(tr){tr.style.display=tr.textContent.toLowerCase().indexOf(q)>=0?'':'none';});}
function scalusBuildChildren(container,id,path){
  var ins=(typeof SCALUS_IN!=='undefined'&&SCALUS_IN[id])||[];
  if(!ins.length){container.innerHTML='<div class="treeleaf">(no incoming)</div>';return;}
  var h='';
  var feeUnit=(typeof SCALUS_FEE!=='undefined'&&SCALUS_FEE)?' lov':' cpu';
  for(var i=0;i<ins.length;i++){
    var cid=ins[i][0],cnt=ins[i][1],cost=ins[i].length>2?ins[i][2]:0,lbl=(typeof SCALUS_LABEL!=='undefined'&&SCALUS_LABEL[cid])||('#'+cid);
    var costStr=cost?(' · '+cost+feeUnit):'';
    if(path.indexOf(cid)>=0){
      h+='<div class="treeleaf">↺ '+lbl+' ×'+cnt+costStr+' (loop)</div>';
    }else{
      h+='<div class="treerow"><span class="treenode" data-id="'+cid+'" data-path="'+path.join(',')+'"><span class="mark">▶</span> '+lbl+' ×'+cnt+costStr+'</span><div class="subtree" hidden></div></div>';
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
function scalusGoto(anchor){
  var el=document.getElementById(anchor);
  if(!el)return;
  scalusTab('annotated');
  el.scrollIntoView({block:'center'});el.classList.add('hl');setTimeout(function(){el.classList.remove('hl');},1800);
}
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
