package scalus.uplc.eval

object ProfileFormatter {

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
        val sb = new StringBuilder

        sb.append("=== Profile by Source Location ===\n")
        if data.bySourceLocation.isEmpty then sb.append("  (no source locations recorded)\n")
        else
            val rows = data.bySourceLocation.take(maxRows)
            val locWidth = math.max(8, rows.map(e => s"${e.file}:${e.line}".length).max)
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
                    s"${e.file}:${e.line}",
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
            if data.bySourceLocation.size > maxRows then
                sb.append(s"  ... and ${data.bySourceLocation.size - maxRows} more\n")

        sb.append('\n')
        sb.append("=== Profile by Function ===\n")
        if data.byFunction.isEmpty then sb.append("  (no functions recorded)\n")
        else
            val rows = data.byFunction.take(maxRows)
            val nameWidth = math.max(8, rows.map(_.name.length).max)
            val countWidth = math.max(5, rows.map(_.count.toString.length).max)
            val memWidth = math.max(3, rows.map(_.memory.toString.length).max)
            val cpuWidth = math.max(3, rows.map(_.cpu.toString.length).max)

            sb.append(
              formatRow(
                "function",
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
            if data.byFunction.size > maxRows then
                sb.append(s"  ... and ${data.byFunction.size - maxRows} more\n")

        sb.append(s"\nTotal: mem=${data.totalBudget.memory} cpu=${data.totalBudget.steps}")
        sb.toString
    }

    /** Renders profiling data as a self-contained HTML page. */
    def toHtml(data: ProfilingData): String = {
        val sb = new StringBuilder
        sb.append("""<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Scalus Profile</title>
<style>
body { font-family: monospace; margin: 20px; background: #fafafa; }
h2 { color: #333; }
table { border-collapse: collapse; margin-bottom: 20px; }
th, td { padding: 4px 12px; text-align: right; border: 1px solid #ddd; }
th { background: #f0f0f0; }
td:first-child, th:first-child { text-align: left; }
tr:hover { background: #e8f4fd; }
.bar { display: inline-block; height: 12px; background: #4a90d9; vertical-align: middle; }
.summary { font-size: 1.1em; margin: 10px 0; }
</style></head><body>
<h1>Scalus CEK Machine Profile</h1>
""")
        sb.append(
          s"""<p class="summary">Total budget: mem=${data.totalBudget.memory}, cpu=${data.totalBudget.steps}</p>\n"""
        )

        // By Source Location
        sb.append("<h2>By Source Location</h2>\n")
        if data.bySourceLocation.isEmpty then sb.append("<p>No source locations recorded.</p>\n")
        else
            val maxMem =
                math.max(1L, data.bySourceLocation.headOption.map(_.memory).getOrElse(1L))
            sb.append(
              "<table><tr><th>Location</th><th>Count</th><th>Memory</th><th>CPU</th><th></th></tr>\n"
            )
            data.bySourceLocation.foreach { e =>
                val barWidth = (e.memory * 100 / maxMem).toInt
                sb.append(
                  s"""<tr><td>${escapeHtml(
                        e.file
                      )}:${e.line}</td><td>${e.count}</td><td>${e.memory}</td><td>${e.cpu}</td><td><span class="bar" style="width:${barWidth}px"></span></td></tr>\n"""
                )
            }
            sb.append("</table>\n")

        // By Function
        sb.append("<h2>By Function</h2>\n")
        if data.byFunction.isEmpty then sb.append("<p>No functions recorded.</p>\n")
        else
            val maxMem = math.max(1L, data.byFunction.headOption.map(_.memory).getOrElse(1L))
            sb.append(
              "<table><tr><th>Function</th><th>Count</th><th>Memory</th><th>CPU</th><th></th></tr>\n"
            )
            data.byFunction.foreach { e =>
                val barWidth = (e.memory * 100 / maxMem).toInt
                sb.append(
                  s"""<tr><td>${escapeHtml(
                        e.name
                      )}</td><td>${e.count}</td><td>${e.memory}</td><td>${e.cpu}</td><td><span class="bar" style="width:${barWidth}px"></span></td></tr>\n"""
                )
            }
            sb.append("</table>\n")

        sb.append("</body></html>")
        sb.toString
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

    private def escapeHtml(s: String): String =
        s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}
