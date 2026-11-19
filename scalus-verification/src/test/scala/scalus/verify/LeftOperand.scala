package scalus.verify

import scala.quoted.*

/** What Scala does with the left operand of a right-associative call, as a macro given the call
  * sees it, for the tests of `==>:` and `==>?:`. A right-associative operator is a method of its
  * right operand, and Scala evaluates the left operand first: once it has typed the call, and
  * converted its argument, it binds an argument that is not pure, as an identifier or a literal is,
  * to a local value, `{ val x$1 = a; b.op:(x$1) }`. It binds none for an inline parameter, which
  * takes the argument's code in place.
  */
object LeftOperand {

    /** The type of the local value Scala binds the left operand of the right-associative call `e`
      * to, or `"not bound"` where it binds none.
      */
    inline def boundAs(inline e: Any): String = ${ boundAsOf('e) }

    /** The local value Scala binds the left operand of the right-associative call `e` to, as
      * `val name: Type = value`, with an inline call shown as written, before its expansion; or
      * `"not bound"`.
      */
    inline def binding(inline e: Any): String = ${ bindingOf('e) }

    private def boundAsOf(e: Expr[Any])(using Quotes): Expr[String] = {
        import quotes.reflect.*
        Expr(boundValue(e.asTerm).fold("not bound")(_.tpt.tpe.widen.typeSymbol.name))
    }

    private def bindingOf(e: Expr[Any])(using Quotes): Expr[String] = {
        import quotes.reflect.*
        Expr(boundValue(e.asTerm).fold("not bound") { value =>
            val shown = value.rhs match
                case Some(Inlined(Some(call), _, _)) => call.show
                case Some(rhs)                       => rhs.show
                case None                            => ""
            s"val ${value.name}: ${value.tpt.tpe.widen.typeSymbol.name} = $shown"
        })
    }

    /** The local value the call `term` binds its left operand to, where it binds one: the call is
      * then a block of that one value, in the wrappers of the inline expansions it is in.
      */
    private def boundValue(using q: Quotes)(term: q.reflect.Term): Option[q.reflect.ValDef] = {
        import q.reflect.*
        term match
            case Inlined(_, Nil, inner)        => boundValue(inner)
            case Typed(inner, _)               => boundValue(inner)
            case Block(List(value: ValDef), _) => Some(value)
            case _                             => None
    }
}
