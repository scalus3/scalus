package scalus.verify

import scala.quoted.*

/** Names the method behind a function literal, for [[FunctionDef.apply]]. */
object FunctionMacro {

    /** The fully-qualified name of the method `f` eta-expands, as the Scalus plugin names it in SIR
      * (`symbol.fullName`): `scalus.cardano.onchain.plutus.prelude.Math$.clamp`.
      */
    inline def qualifiedName(inline f: Any): String = ${ qualifiedNameImpl('f) }

    /** `f` must be exactly an eta-expansion: its body calls one method with the lambda's own
      * parameters, in order. Anything else is a new function, and naming it after the method it
      * happens to call would misstate what a statement is about.
      */
    private def qualifiedNameImpl(f: Expr[Any])(using Quotes): Expr[String] = {
        import quotes.reflect.*

        def strip(t: Term): Term = t match
            case Inlined(_, Nil, e) => strip(e)
            case Block(Nil, e)      => strip(e)
            case Typed(e, _)        => strip(e)
            case _                  => t

        // the method a call chain applies, and its arguments, in order
        def callee(t: Term, args: List[Term]): Option[(Symbol, List[Term])] =
            strip(t) match
                case Apply(fun, more)                => callee(fun, more ++ args)
                case TypeApply(fun, _)               => callee(fun, args)
                case ref: Ref if ref.symbol.isDefDef => Some((ref.symbol, args))
                case _                               => None

        def notAReference(detail: String): Nothing =
            report.errorAndAbort(
              s"FunctionDef(...) takes a plain method reference, such as FunctionDef(Math.clamp); " +
                  s"$detail. Give any other function a synthetic name with FunctionDef.named.",
              f
            )

        strip(f.asTerm) match
            case Lambda(params, body) =>
                callee(body, Nil) match
                    case Some((method, args)) =>
                        val passed = args.map(strip).map {
                            case id: Ident => id.symbol
                            case _         => Symbol.noSymbol
                        }
                        if passed != params.map(_.symbol) then
                            notAReference(
                              s"this function calls ${method.name} with other arguments"
                            )
                        Expr(method.fullName)
                    case None =>
                        notAReference(
                          "the method behind it is not visible (an inline def is expanded first)"
                        )
            case _ => notAReference("this is not a function literal")
    }
}
