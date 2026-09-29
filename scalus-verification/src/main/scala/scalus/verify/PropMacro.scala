package scalus.verify

import scala.quoted.*

/** Compiles expression leaves into SIR for the runtime [[Prop]] object. */
private[verify] object PropMacro {

    def forAll[A: Type](body: Expr[A => Boolean])(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        val pos = body.asTerm.pos
        val id = (pos.start.toLong << 32) | (pos.end.toLong & 0xffffffffL)
        '{ Props.compiledForAll[A](scalus.compiler.compile($body), ${ Expr(id) }) }
    }

    def forAll2[A: Type, B: Type](body: Expr[(A, B) => Boolean])(using Quotes): Expr[Prop] = {
        val ids = parameterIds(body, 2)
        '{ Props.compiledForAll(scalus.compiler.compile($body), ${ Expr(ids) }) }
    }

    def forAll3[A: Type, B: Type, C: Type](body: Expr[(A, B, C) => Boolean])(using
        Quotes
    ): Expr[Prop] = {
        val ids = parameterIds(body, 3)
        '{ Props.compiledForAll(scalus.compiler.compile($body), ${ Expr(ids) }) }
    }

    /** One binder id per parameter of a lambda literal, from the parameter's source position. */
    private def parameterIds(body: Expr[Any], arity: Int)(using Quotes): List[Long] = {
        import quotes.reflect.*
        def strip(term: Term): Term = term match
            case Inlined(_, Nil, inner) => strip(inner)
            case Typed(inner, _)        => strip(inner)
            case Block(Nil, inner)      => strip(inner)
            case other                  => other
        strip(body.asTerm) match
            case Lambda(params, _) if params.size == arity =>
                params.map(param =>
                    (param.pos.start.toLong << 32) | (param.pos.end.toLong & 0xffffffffL)
                )
            case other =>
                report.errorAndAbort(
                  s"forAll over $arity values requires a lambda literal of $arity parameters",
                  other.pos
                )
    }

    def exists[A: Type](body: Expr[A => Boolean])(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        val pos = body.asTerm.pos
        val id = (pos.start.toLong << 32) | (pos.end.toLong & 0xffffffffL)
        '{ Props.compiledExists[A](scalus.compiler.compile($body), ${ Expr(id) }, None) }
    }

    def existsLet[A: Type](witness: Expr[A], body: Expr[A => Boolean])(using
        Quotes
    ): Expr[Prop] = {
        import quotes.reflect.*
        val pos = body.asTerm.pos
        val id = (pos.start.toLong << 32) | (pos.end.toLong & 0xffffffffL)
        '{
            Props.compiledExists[A](
              scalus.compiler.compile($body),
              ${ Expr(id) },
              Some(PropExpr.SIRExpr[A](scalus.compiler.compile($witness)))
            )
        }
    }

    def call[A: Type, R: Type](
        fn: Expr[FunctionRef[A, R]],
        arg: Expr[A],
        body: Expr[R => Boolean],
        total: Boolean
    )(using Quotes): Expr[Prop] = {
        import quotes.reflect.*
        val pos = body.asTerm.pos
        val id = (pos.start.toLong << 32) | (pos.end.toLong & 0xffffffffL)
        '{
            Props.compiledCall[A, R](
              $fn,
              PropExpr.SIRExpr[A](scalus.compiler.compile($arg)),
              scalus.compiler.compile($body),
              ${ Expr(id) },
              ${ Expr(total) }
            )
        }
    }

    def callDef[A: Type, R: Type](
        fn: Expr[FunctionDef[A, R]],
        arg: Expr[A],
        body: Expr[R => Boolean],
        total: Boolean
    )(using Quotes): Expr[Prop] = call('{ $fn.ref }, arg, body, total)

    def test(b: Expr[Boolean])(using Quotes): Expr[Prop] =
        '{ Prop.Bool(PropExpr.SIRExpr[Boolean](scalus.compiler.compile($b))) }

    def denotes[A: Type](e: Expr[A])(using Quotes): Expr[Prop] =
        '{ Prop.Denotes(PropExpr.SIRExpr[A](scalus.compiler.compile($e))) }

    def equal[A: Type](a: Expr[A], b: Expr[A])(using Quotes): Expr[Prop] = {
        '{
            Prop.Equal(
              PropExpr.SIRExpr[A](scalus.compiler.compile($a)),
              PropExpr.SIRExpr[A](scalus.compiler.compile($b))
            )
        }
    }

}
