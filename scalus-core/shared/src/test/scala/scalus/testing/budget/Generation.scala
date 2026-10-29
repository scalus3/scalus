package scalus.testing.budget

import scala.quoted.{Expr, Quotes}
import scala.util.control.NonFatal

/** A compiler generation: a range of Scala compiler versions that produce the same UPLC for the
  * same source, and so the same budgets.
  *
  * The 3.3.x LTS desugars some constructs into larger trees than 3.8.x and later (for example a
  * tuple pattern match). The Scalus plugin sees those trees, so budgets differ between the two
  * generations. A budget table holds one row per generation only where the values differ
  * (`[BP-6]`).
  */
enum Generation(val minMinor: Int) {

    /** Scala 3.3.x LTS. */
    case Scala33 extends Generation(3)

    /** Scala 3.8.x, 3.9.x and later: the leaner desugaring. */
    case Scala38 extends Generation(8)
}

object Generation {

    /** The generation of a compiler version such as `"3.3.8"`, `"3.9.0"` or `"3.10.0-RC1"`: the
      * newest generation whose first minor version is at most the version's minor.
      */
    def of(version: String): Generation = {
        val parts = version.split("[.\\-]").iterator.flatMap(_.toIntOption)
        val major = parts.nextOption().getOrElse(3)
        val minor = parts.nextOption().getOrElse(3)
        if major > 3 then Generation.values.last
        else Generation.values.filter(_.minMinor <= minor).lastOption.getOrElse(Scala33)
    }

    /** The generation of the compiler that compiles the call site.
      *
      * It is `inline`, so it expands where it is called, not once in this file. The only intended
      * call is the first line of a generated `<Suite>Budgets` table, which the suite's own project
      * compiles with the suite's own compiler. So JVM, JS and Native tests of one build all see the
      * generation that actually produced their UPLC, without any build setting.
      *
      * The expansion is a constant, for example `Generation.of("3.9.0")`.
      */
    transparent inline def current: Generation = ${ GenerationMacro.currentImpl }

    /** The version string that [[current]] used, for diagnostics and tests. */
    transparent inline def currentCompilerVersion: String = ${ GenerationMacro.versionImpl }
}

/** Macro implementations for [[Generation.current]].
  *
  * Stage: compile time. This code runs inside the Scala compiler JVM while it compiles the call
  * site, never in the test JVM, the JS VM or the Native binary. It only produces a literal.
  */
private[budget] object GenerationMacro {

    /** Generated code at the call site:
      * {{{
      * Generation.of("3.9.0")
      * }}}
      */
    def currentImpl(using Quotes): Expr[Generation] = {
        val version = Expr(compilerVersion)
        '{ Generation.of($version) }
    }

    /** Generated code at the call site: the literal `"3.9.0"`. */
    def versionImpl(using Quotes): Expr[String] = Expr(compilerVersion)

    /** The version of the running compiler.
      *
      * The quotes API does not expose it, so it is read by reflection from the compiler's own
      * `dotty.tools.dotc.config.Properties.versionNumberString`. That class is always loadable
      * here, because a macro runs in a class loader whose parent holds the compiler. Reflection
      * avoids a compile-time dependency on `scala3-compiler` in every test project.
      *
      * `scala.util.Properties.versionNumberString` is not an option: on 3.3.x it reports the Scala
      * 2.13 standard library version.
      *
      * Falls back to `"3.3.8"`. `BudgetPinsTest` compares the result with `BuildInfo.scalaVersion`,
      * so a fallback fails on every platform.
      */
    private def compilerVersion: String =
        try
            val properties = Class.forName("dotty.tools.dotc.config.Properties$")
            val module = properties.getField("MODULE$").get(null)
            properties.getMethod("versionNumberString").invoke(module).asInstanceOf[String]
        catch case NonFatal(_) => "3.3.8"
}
