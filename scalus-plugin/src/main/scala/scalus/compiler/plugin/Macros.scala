package scalus.compiler.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import scalus.compiler.sir.{SIR, SIRBuiltins}
import scalus.uplc.DefaultFun

object Macros {
    import scala.quoted.*

    /** Generate a map of builtins from the [[DefaultFun]] enum
      *
      * {{{
      *   Map(
      *      requiredModule("scalus.builtin.Builtins").requiredMethod("addInteger") -> SIRBuiltins.addInteger,
      *      // ...
      *      )
      * }}}
      */
    inline def generateBuiltinsMap(ctx: Context): Map[Symbols.Symbol, SIR.Builtin] = ${
        generateBuiltinsMapImpl('ctx)
    }
    private def generateBuiltinsMapImpl(
        ctx: Expr[Context]
    )(using Quotes): Expr[Map[Symbols.Symbol, SIR.Builtin]] = {
        import quotes.reflect.*

        // Generate map entries expressions
        val entries: Seq[Expr[(Symbols.Symbol, SIR.Builtin)]] = DefaultFun.values.toSeq.map {
            name =>
                val methodName = lowerFirst(name.toString)
                '{
                    given Context = $ctx
                    (
                      Symbols
                          .requiredClass("scalus.builtin.Builtins")
                          .requiredMethod(${
                              Expr(methodName)
                          }),
                      ${ Select.unique('{ SIRBuiltins }.asTerm, methodName).asExprOf[SIR.Builtin] }
                    )
                }
        }

        // Combine all entries into a Map expression
        val mapExpr = entries.foldLeft[Expr[Map[Symbols.Symbol, SIR.Builtin]]]('{
            Map.empty[Symbols.Symbol, SIR.Builtin]
        }) { (acc, entry) =>
            '{ $acc + $entry }
        }

        mapExpr
    }

    private def lowerFirst(s: String): String =
        if s == null || s.isEmpty || !s.charAt(0).isUpper then s
        else s.updated(0, s.charAt(0).toLower)
}
