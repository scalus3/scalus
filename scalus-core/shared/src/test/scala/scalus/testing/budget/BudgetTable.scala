package scalus.testing.budget

import scala.collection.mutable

/** The budget pins of one test suite, keyed by test name (plus a label).
  *
  * A suite's table is a generated, committed Scala object next to the suite, for example
  * `MathTestBudgets.scala`. `sbtn updateBudgets` rewrites it; do not edit it by hand.
  *
  * {{{
  * object MathTestBudgets extends BudgetTable {
  *     override val generation: Generation = Generation.current
  *     pin("min / min @ (1, 2)", Pin.Eval(memory = 2_097, steps = 410_169, size = 20, fee = 451))
  *     pin("gcd / gcd @ (12, 18)", Generation.Scala33, Pin.Eval(memory = 9_872, steps = 2_243_780, size = 63, fee = 1_677))
  *     pin("gcd / gcd @ (12, 18)", Generation.Scala38, Pin.Eval(memory = 9_700, steps = 2_190_000, size = 61, fee = 1_623))
  * }
  * }}}
  *
  * A key has either one plain row, valid for every generation, or one row per generation.
  *
  * The constructor captures the path of the file with the `extends BudgetTable` clause, so the
  * table knows its own file: the update run rewrites exactly that file.
  */
abstract class BudgetTable(using source: BudgetTable.SourcePath) {
    import BudgetTable.Entry

    /** The absolute path of the source file that defines this table. */
    final val sourcePath: String = source.path

    /** The generation this table was compiled with. A generated table sets it to
      * `Generation.current`, which expands with the suite's compiler.
      */
    def generation: Generation

    private val entries = mutable.LinkedHashMap.empty[String, Entry]

    /** Adds a row that holds for every generation. */
    protected final def pin(key: String, value: Pin): Unit = {
        require(!entries.contains(key), s"duplicate budget pin key \"$key\"")
        entries(key) = Entry.Plain(value)
    }

    /** Adds the row of one generation. */
    protected final def pin(key: String, generation: Generation, value: Pin): Unit = {
        entries.get(key) match
            case None => entries(key) = Entry.PerGeneration(Map(generation -> value))
            case Some(Entry.PerGeneration(rows)) if !rows.contains(generation) =>
                entries(key) = Entry.PerGeneration(rows.updated(generation, value))
            case Some(_) =>
                throw IllegalArgumentException(
                  s"duplicate budget pin key \"$key\" for generation $generation"
                )
    }

    /** The pin of `key` for this table's generation. */
    final def get(key: String): Option[Pin] = entries.get(key).flatMap {
        case Entry.Plain(value)        => Some(value)
        case Entry.PerGeneration(rows) => rows.get(generation)
    }

    /** All keys, in table order. */
    final def keys: Seq[String] = entries.keys.toSeq

    /** All entries, in table order. */
    final def all: Seq[(String, Entry)] = entries.toSeq

    /** True only for [[BudgetTable.unpinned]]. */
    def isUnpinned: Boolean = false
}

object BudgetTable {

    /** The path of the source file where it is summoned, captured at compile time.
      *
      * Not `org.scalactic.source.Position`: its `filePathname` is empty unless the environment
      * variable `SCALACTIC_FILL_FILE_PATHNAMES=yes` was set for the compiler.
      */
    final class SourcePath(val path: String)

    object SourcePath {

        /** Expands where a `SourcePath` is summoned, for example at `object FooBudgets extends
          * BudgetTable`. Generated code: `BudgetTable.SourcePath("/abs/path/FooBudgets.scala")`.
          */
        inline given here: SourcePath = ${ SourcePathMacro.hereImpl }
    }

    /** One key's rows. */
    enum Entry {
        case Plain(value: Pin)
        case PerGeneration(rows: Map[Generation, Pin])
    }

    /** The default table of a suite that does not pin yet: every pin call is a no-op.
      *
      * It exists for the migration only: a suite moves to a real table when its `budget-pin` unit
      * runs. The last such unit removes this object.
      */
    object unpinned extends BudgetTable(using SourcePath("")) {
        val generation: Generation = Generation.Scala33 // never read: the pins skip this table
        override def isUnpinned: Boolean = true
    }
}
