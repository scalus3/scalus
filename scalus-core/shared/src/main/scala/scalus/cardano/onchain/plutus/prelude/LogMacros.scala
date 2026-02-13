package scalus.cardano.onchain.plutus.prelude

import scalus.uplc.builtin.{Builtins, Data, ToData}

import scala.quoted.*

object LogMacros {

    /** Macro implementation for the variadic `log` function.
      *
      * Expands `log(a1, a2, ..., an)` into a single `trace` call with all arguments concatenated
      * via `appendString`, separated by spaces.
      *
      * String literals are passed through as-is (no quoting), serving as labels. String expressions
      * and all other types are converted via their `Show` instance (which adds quotes for strings),
      * or via `ToData` then `Show[Data]` as a fallback.
      *
      * @param args
      *   the varargs sequence expression
      * @return
      *   a quoted `Unit` expression containing the trace call
      */
    def logMacro(using Quotes)(args: Expr[Seq[Any]]): Expr[Unit] = {
        import quotes.reflect.*

        def isStringLiteral(arg: Expr[Any]): Boolean =
            arg.asTerm match
                case Inlined(_, _, Literal(StringConstant(_))) => true
                case Literal(StringConstant(_))                => true
                case _                                         => false

        args match {
            case Varargs(argExprs) =>
                if argExprs.isEmpty then '{ () }
                else
                    val showExprs: Seq[Expr[String]] = argExprs.map { arg =>
                        if isStringLiteral(arg) then arg.asExprOf[String]
                        else
                            arg.asTerm.tpe.widen.asType match {
                                case '[t] =>
                                    Expr.summon[Show[t]] match {
                                        case Some(showInst) =>
                                            '{ $showInst.apply(${ arg.asExprOf[t] }) }
                                        case None =>
                                            Expr.summon[ToData[t]] match {
                                                case Some(toDataInst) =>
                                                    val showData =
                                                        Expr.summon[Show[Data]].get
                                                    '{
                                                        $showData.apply(
                                                          $toDataInst.apply(${
                                                              arg.asExprOf[t]
                                                          })
                                                        )
                                                    }
                                                case None =>
                                                    report.errorAndAbort(
                                                      s"No Show or ToData instance found for type ${Type.show[t]} in log() call"
                                                    )
                                            }
                                    }
                            }
                    }

                    val concatenated = showExprs.reduceLeft { (acc, next) =>
                        '{
                            Builtins.appendString(
                              $acc,
                              Builtins.appendString(" ", $next)
                            )
                        }
                    }

                    '{ Builtins.trace($concatenated)(()) }
            case _ =>
                report.errorAndAbort("log() requires literal arguments, not a Seq variable")
        }
    }
}
