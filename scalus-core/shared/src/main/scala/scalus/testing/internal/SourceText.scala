package scalus.testing.internal

import scala.quoted.{Expr, Quotes}

/** The source text of an expression, for automatic budget pin labels in tests (D2).
  *
  * Test tooling, in main code on purpose: a macro called from inline methods of the same project
  * crashes the Scala 3.3.8 compiler (phase sbt-api, finding F-91), so the macro must come
  * precompiled. Internal: no compatibility promise.
  *
  * {{{
  * assertEvalEq(min(BigInt(1), BigInt(2)), BigInt(1))   // label: "min(BigInt(1), BigInt(2))"
  * }}}
  *
  * Runs of whitespace collapse to one space, so a reformat by scalafmt does not change a key.
  */
object SourceText {

    /** The source text of `code` as written at the call site. Inside an inline method, pass the
      * method's `inline` parameter: the macro then sees the caller's tree.
      */
    inline def of(inline code: Any): String = ${ SourceTextMacro.ofImpl('code) }
}

/** Stage: compile time. Runs inside the compiler at the call site, produces a string literal.
  *
  * Generated code at the call site: the literal `"min(BigInt(1), BigInt(2))"`.
  */
object SourceTextMacro {
    def ofImpl(code: Expr[Any])(using q: Quotes): Expr[String] = {
        import q.reflect.*
        // An argument that passed through inline methods arrives wrapped in `Inlined(None, ...)`
        // nodes, one per inline parameter it passed. Their span is the parameter's name in the
        // inline method's body, so they are unwrapped. An `Inlined(Some(call), ...)` node is an
        // inline call the user wrote, such as `BigInt(0).absolute`: its span is the user's text,
        // so the unwrapping stops there. Without the stop, the label is the inline method's body.
        //   Inlined(None) > Inlined(None) > Inlined(Some(BigInt(0).absolute)) > ... body ...
        def unwrap(t: Term): Term = t match
            case Inlined(None, _, inner) => unwrap(inner)
            case Typed(inner, _)         => unwrap(inner)
            case _                       => t
        val term = unwrap(code.asTerm)
        // A tree without source (generated code) falls back to its printed form.
        val text = term.pos.sourceCode.getOrElse(term.show)
        Expr(text.split("\\s+").iterator.filter(_.nonEmpty).mkString(" "))
    }
}
