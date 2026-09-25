package scalus.verify

import scala.quoted.*

/** Builds the leaves of a [[Prop]] from Scala expressions, naming each after its source text. */
private[verify] object PropMacro {

    def atom(b: Expr[Boolean])(using Quotes): Expr[Prop] =
        '{ Prop.Atom(${ Expr(debugName(b)) }, () => $b) }

    def denotes[A: Type](e: Expr[A])(using Quotes): Expr[Prop] =
        '{ Prop.Denotes(${ Expr(debugName(e)) }, () => $e) }

    def equal[A: Type](a: Expr[A], b: Expr[A])(using Quotes): Expr[Prop] = {
        val name = s"${sourceText(a)} equals ${sourceText(b)} (${position(a)})"
        '{ Prop.Equal(${ Expr(name) }, () => $a, () => $b) }
    }

    /** The expression's source text and position, e.g. `Math.abs(x) > 0 (PropTest.scala:19)`. */
    private def debugName(e: Expr[Any])(using Quotes): String =
        s"${sourceText(e)} (${position(e)})"

    private def sourceText(e: Expr[Any])(using Quotes): String = {
        import quotes.reflect.*
        val term = e.asTerm
        term.pos.sourceCode.getOrElse(term.show).replaceAll("\\s+", " ").trim
    }

    private def position(e: Expr[Any])(using Quotes): String = {
        import quotes.reflect.*
        val pos = e.asTerm.pos
        s"${pos.sourceFile.name}:${pos.startLine + 1}"
    }
}
