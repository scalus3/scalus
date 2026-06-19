package scalus

/** Selects test baselines that legitimately differ between Scala compiler versions.
  *
  * Budgets and compiled-script sizes shift between Scala 3.3.x (LTS) and 3.8.x because the host
  * compiler desugars some constructs more leanly since 3.8.x — e.g. a tuple-datum pattern match no
  * longer destructures-then-reconstructs the tuple, so the Scalus plugin sees a smaller tree. These
  * differences are correctness-neutral (the contracts evaluate identically); only the exact
  * budget/size numbers change. The 3.3.x LTS keeps the older, larger baselines; 3.8.x and the next
  * LTS (which inherits the leaner desugaring) use the smaller ones.
  *
  * Lives in `scalus-testkit` (not the example test sources) so that standalone projects ejected via
  * `eject-examples` — which bundle the example tests but depend only on published artifacts — can
  * still resolve `scalus.ScalaCompilerVersion`.
  *
  * The active compiler version is injected by build.sbt (`-Dscalus.test.scalaVersion`). It can't be
  * read from `scala.util.Properties.versionNumberString`, which reports the 2.13 standard-library
  * version (`2.13.x`) on the Scala 3.3 LTS rather than the compiler version. When the property is
  * absent (e.g. an ejected project, which pins the 3.3.x LTS) it defaults to `"3.3.7"`.
  */
object ScalaCompilerVersion {

    /** The compiler version building the tests, e.g. `"3.3.7"` or `"3.8.4"`. */
    val version: String = sys.props.getOrElse("scalus.test.scalaVersion", "3.3.7")

    private val (major, minor) = {
        val parts = version.split("[.\\-]").iterator.flatMap(_.toIntOption)
        (parts.nextOption().getOrElse(3), parts.nextOption().getOrElse(3))
    }

    /** True for compilers with the leaner desugaring: Scala 3.8.x and later (incl. the next LTS).
      */
    val hasLeanDesugaring: Boolean = major > 3 || (major == 3 && minor >= 8)

    /** The baseline for the active compiler: `pre38` on the 3.3.x LTS, `since38` on 3.8.x+. */
    def baseline[T](pre38: T, since38: T): T = if hasLeanDesugaring then since38 else pre38
}
