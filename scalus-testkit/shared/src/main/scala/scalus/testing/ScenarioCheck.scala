package scalus.testing

import scala.quoted.*

/** Source location captured at compile time. */
case class SourceLocation(file: String, line: Int) {
    override def toString: String = s"$file:$line"
}

/** Error thrown when a Scenario.check fails. */
case class CheckFailure(
    predicate: String,
    message: String,
    location: SourceLocation
) extends Exception(
      if message.isEmpty then s"Check failed: $predicate at $location"
      else s"Check failed: $predicate ($message) at $location"
    )

object ScenarioCheck {

    /** Check a condition, failing the scenario if false.
      *
      * This is an inline macro that captures:
      *   - The predicate expression as a string
      *   - Optional message
      *   - Source file and line
      *
      * Usage:
      * {{{
      * async[Scenario] {
      *     Scenario.check(balance >= 0).await
      *     Scenario.check(owner != null, "owner must be set").await
      * }
      * }}}
      */
    inline def check(inline condition: Boolean, inline message: String = ""): Scenario[Unit] =
        ${ checkImpl('condition, 'message) }

    private def checkImpl(condition: Expr[Boolean], message: Expr[String])(using
        Quotes
    ): Expr[Scenario[Unit]] = {
        import quotes.reflect.*

        val pos = Position.ofMacroExpansion
        val file = Expr(pos.sourceFile.name)
        val line = Expr(pos.startLine + 1)

        val predicateStr = Expr(condition.show)

        '{
            if $condition then Scenario.scenarioLogicMonad.pure(())
            else
                Scenario.error(
                  CheckFailure(
                    $predicateStr,
                    $message,
                    SourceLocation($file, $line)
                  )
                )
        }
    }
}
