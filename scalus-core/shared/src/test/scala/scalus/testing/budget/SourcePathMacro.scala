package scalus.testing.budget

import java.nio.file.Path
import scala.quoted.{Expr, Quotes}

/** Stage: compile time. Runs inside the compiler at the summoning site, produces a literal. */
private[budget] object SourcePathMacro {
    def hereImpl(using q: Quotes): Expr[BudgetTable.SourcePath] = {
        import q.reflect.Position
        val file = Position.ofMacroExpansion.sourceFile
        // `getJPath` is deprecated since 3.9.0. `path` is the path the compiler was given; sbt
        // passes absolute paths, and `toAbsolutePath` covers a relative one.
        val path = Path.of(file.path).toAbsolutePath.toString
        '{ BudgetTable.SourcePath(${ Expr(path) }) }
    }
}
