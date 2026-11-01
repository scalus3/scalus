package scalus.testing.budget

import scalus.testing.budget.BudgetTable.Entry

/** Pure text functions of the budget pins: the failure message (`[BP-7]`), the merge of an update
  * run into a table (`[BP-8a]`), the generated table source and the update report (`[BP-9]`).
  *
  * No I/O here, so the logic is tested on every platform. `BudgetFiles` does the writing.
  */
object BudgetText {

    /** `1101` -> `"1_101"`, `-2298799` -> `"-2_298_799"`. The same grouping is used in the
      * generated tables and in the messages.
      */
    def group(n: Long): String = {
        val digits = math.abs(n).toString
        val grouped = digits.reverse.grouped(3).mkString("_").reverse
        if n < 0 then "-" + grouped else grouped
    }

    /** The change from `expected` to `actual`, in percent of `expected`, with a sign and one
      * decimal: `"-1.5 %"`, `"+0.0 %"`.
      */
    def percent(expected: Long, actual: Long): String =
        if expected == 0 then if actual == 0 then "+0.0 %" else "new"
        else
            val p = (actual - expected).toDouble * 100 / expected.toDouble
            val tenths = math.round(math.abs(p) * 10)
            val sign = if actual < expected then "-" else "+"
            s"$sign${tenths / 10}.${tenths % 10} %"

    private def fields(pin: Pin): Seq[(String, Long)] = pin match
        case Pin.Eval(memory, steps, size, fee) =>
            Seq("memory" -> memory, "steps" -> steps, "size" -> size.toLong, "fee" -> fee)
        case Pin.Tx(memory, steps, size, fee) =>
            Seq("memory" -> memory, "steps" -> steps, "tx size" -> size.toLong, "tx fee" -> fee)
        case Pin.Size(bytes) => Seq("bytes" -> bytes.toLong)

    /** One line per pinned quantity: `memory 1_101 -> 1_085 (-1.5 %)`. For an evaluation, one more
      * line splits the fee into its execution and reference-script parts.
      */
    def changes(expected: Pin, actual: Pin): Seq[String] =
        if expected.ordinal != actual.ordinal then Seq(s"$expected -> $actual")
        else
            val lines = fields(expected).zip(fields(actual)).map { case ((name, e), (_, a)) =>
                s"$name ${group(e)} -> ${group(a)} (${percent(e, a)})"
            }
            (expected, actual) match
                case (e: Pin.Eval, a: Pin.Eval) => lines :+ feeParts(e, a)
                case (e: Pin.Tx, a: Pin.Tx)     => lines :+ txFeeParts(e, a)
                case _                          => lines

    /** `tx fee = size 180_000 -> 179_800 + execution 40_000 -> 39_000 + rest 5_000 -> 5_000`. The
      * rest is the reference-script fee and any margin the balancer added.
      */
    private def txFeeParts(e: Pin.Tx, a: Pin.Tx): String =
        def parts(p: Pin.Tx) =
            val size = Pin.sizeFee(p.size)
            val execution = Pin.executionFee(p.memory, p.steps)
            (size, execution, p.fee - size - execution)
        val (es, ee, er) = parts(e)
        val (as, ae, ar) = parts(a)
        s"tx fee = size ${group(es)} -> ${group(as)} + execution ${group(ee)} -> ${group(ae)} + " +
            s"rest ${group(er)} -> ${group(ar)}"

    /** `fee = execution 151 -> 160 + reference script 1_305 -> 1_230` */
    private def feeParts(e: Pin.Eval, a: Pin.Eval): String =
        val (ee, ae) = (Pin.executionFee(e.memory, e.steps), Pin.executionFee(a.memory, a.steps))
        val (er, ar) = (Pin.referenceScriptFee(e.size), Pin.referenceScriptFee(a.size))
        s"fee = execution ${group(ee)} -> ${group(ae)} + reference script ${group(er)} -> ${group(ar)}"

    /** The failure message of a pin that does not match (`[BP-7]`). */
    def mismatch(
        suite: String,
        key: String,
        generation: Generation,
        expected: Pin,
        actual: Pin
    ): String =
        (s"$suite: budget pin mismatch, key \"$key\", generation $generation" +:
            changes(expected, actual).map("  " + _) :+
            "Run `sbtn updateBudgets` to accept the new values.").mkString("\n")

    /** The failure message of a key that has no pin. */
    def missing(suite: String, key: String, generation: Generation, actual: Pin): String =
        s"$suite: no budget pin for key \"$key\", generation $generation (actual: ${render(actual)}). " +
            "Run `sbtn updateBudgets`."

    /** Merges the actual values of one update run into the table's entries.
      *
      *   - A key with an equal value keeps its row.
      *   - A plain row with a different value splits: the old value becomes the row of every other
      *     generation, the new value the row of `generation`.
      *   - Rows of one key that become equal for all generations join into one plain row.
      *   - New keys follow, in the order of first use.
      *   - Keys with no actual value are dropped only when `dropUnused` (every test of the suite
      *     ran); otherwise they stay.
      */
    def merge(
        entries: Seq[(String, Entry)],
        generation: Generation,
        actuals: Seq[(String, Pin)],
        dropUnused: Boolean
    ): Seq[(String, Entry)] = {
        val actual = actuals.toMap
        val existing = entries.map(_._1).toSet
        val updated = entries.flatMap { case (key, entry) =>
            actual.get(key) match
                case None => if dropUnused then None else Some(key -> entry)
                case Some(value) =>
                    val rows = entry match
                        case Entry.Plain(old) if old == value => None
                        case Entry.Plain(old) =>
                            Some(
                              Generation.values.map(g => g -> old).toMap.updated(generation, value)
                            )
                        case Entry.PerGeneration(rows) => Some(rows.updated(generation, value))
                    Some(key -> rows.fold(entry)(joined))
        }
        val added = actuals.filterNot((key, _) => existing.contains(key)).map { (key, value) =>
            key -> Entry.Plain(value)
        }
        updated ++ added
    }

    private def joined(rows: Map[Generation, Pin]): Entry =
        val values = Generation.values.toSeq.flatMap(rows.get)
        if values.size == Generation.values.length && values.distinct.size == 1 then
            Entry.Plain(values.head)
        else Entry.PerGeneration(rows)

    /** `Pin.Eval(memory = 2_097, steps = 410_169, size = 20, fee = 451)` */
    def render(pin: Pin): String = pin match
        case Pin.Eval(m, s, z, f) =>
            s"Pin.Eval(memory = ${long(m)}, steps = ${long(s)}, size = ${group(z)}, fee = ${long(f)})"
        case Pin.Tx(m, s, z, f) =>
            s"Pin.Tx(memory = ${long(m)}, steps = ${long(s)}, size = ${group(z)}, fee = ${long(f)})"
        case Pin.Size(b) => s"Pin.Size(bytes = ${group(b)})"

    /** A Long literal: `group(n)`, with an `L` beyond the Int range. scalac adapts a large literal
      * to a Long parameter, but scalafmt's parser rejects it.
      */
    private def long(n: Long): String =
        if n.isValidInt then group(n) else group(n) + "L"

    private def literal(s: String): String =
        "\"" + s.flatMap {
            case '"'  => "\\\""
            case '\\' => "\\\\"
            case '\n' => "\\n"
            case c    => c.toString
        } + "\""

    /** The source of a generated table. scalafmt runs after the writer, so long rows may wrap. */
    def source(
        packageName: String,
        tableName: String,
        suite: String,
        entries: Seq[(String, Entry)]
    ): String = {
        val rows = entries.flatMap {
            case (key, Entry.Plain(value)) => Seq(s"pin(${literal(key)}, ${render(value)})")
            case (key, Entry.PerGeneration(rows)) =>
                rows.toSeq.sortBy(_._1.ordinal).map { (g, value) =>
                    s"pin(${literal(key)}, Generation.$g, ${render(value)})"
                }
        }
        s"""package $packageName
           |
           |import scalus.testing.budget.{BudgetTable, Generation, Pin}
           |
           |/** Budget pins of [[$suite]]. Generated by `sbtn updateBudgets`. Do not edit by hand. */
           |object $tableName extends BudgetTable {
           |    override val generation: Generation = Generation.current
           |${rows.map("    " + _).mkString("\n")}
           |}
           |""".stripMargin
    }

    /** The update report of one suite: what changed, for the commit message (`[BP-9]`). */
    def report(
        suite: String,
        generation: Generation,
        before: BudgetTable,
        actuals: Seq[(String, Pin)],
        removed: Seq[String]
    ): Seq[String] = {
        val updated = actuals.flatMap { (key, value) =>
            before
                .get(key)
                .filter(_ != value)
                .map(old => s"  $key: ${changes(old, value).mkString(", ")}")
        }
        val added = actuals.collect { case (key, _) if !before.keys.contains(key) => key }
        val summary =
            s"$suite ($generation): ${updated.size} updated, ${added.size} added, ${removed.size} removed"
        Seq(summary) ++ updated ++
            added.map(k => s"  added: $k") ++ removed.map(k => s"  removed: $k")
    }
}
