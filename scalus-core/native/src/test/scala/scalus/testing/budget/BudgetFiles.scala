package scalus.testing.budget

/** File access of the budget update run. Not on this platform: the update runs on the JVM only
  * (`[BP-8b]`); here the pins are only checked.
  */
object BudgetFiles {
    def writeTable(path: String, source: String): Unit =
        throw UnsupportedOperationException("budget update runs on the JVM only")

    def appendReport(lines: Seq[String]): Unit =
        throw UnsupportedOperationException("budget update runs on the JVM only")
}
