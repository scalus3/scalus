package scalus.compiler.sir.linking

import scalus.compiler.sir.{AnnotationsDecl, Binding, DataDecl, SIR}

/** What is around an expression, outermost first: data declarations and `let`s.
  *
  * Linking gives a program this shape: the data declarations it uses, inside them the definitions
  * it uses, each in a `let`, and inside those its expression. What reads a linked program takes the
  * expression out, and puts what it makes of it back inside the same wrappers.
  */
final case class Wrappers(layers: List[Wrappers.Layer]) {

    /** The data declarations, outermost first. */
    def declarations: List[DataDecl] = layers.collect { case Wrappers.Decl(data) => data }

    /** The bindings of the `let`s, outermost first. */
    def bindings: List[Binding] = layers.flatMap {
        case Wrappers.Let(bindings, _, _) => bindings
        case _: Wrappers.Decl             => Nil
    }

    /** `inner` inside the wrappers. */
    def apply(inner: SIR): SIR = layers.foldRight(inner)((layer, body) => layer(body))
}

object Wrappers {

    /** One wrapper: a `Decl` or a `Let` without the term it is around. */
    sealed trait Layer {

        /** `inner` inside this wrapper. */
        def apply(inner: SIR): SIR
    }

    final case class Decl(data: DataDecl) extends Layer {
        def apply(inner: SIR): SIR = SIR.Decl(data, inner)
    }

    final case class Let(bindings: List[Binding], flags: SIR.LetFlags, anns: AnnotationsDecl)
        extends Layer {
        def apply(inner: SIR): SIR = SIR.Let(bindings, inner, flags, anns)
    }

    /** Every data declaration and `let` that `sir` starts with, and what they are around. A `let`
      * of the expression's own is taken like a definition's: this is for an expression whose `let`s
      * are to stay around a part of it, as around the body of a function.
      */
    def of(sir: SIR): (Wrappers, SIR) = take(sir, declarations = true, _ => true)

    /** The definitions linking put around `sir`, and what they are around: the `let`s that `sir`
      * starts with and that bind definitions only ([[SIRLinker.isDefinition]]). A `let` of the
      * expression's own is part of the expression, and so is a data declaration: the ones linking
      * put around a program are outside its definitions.
      */
    def definitions(sir: SIR): (Wrappers, SIR) =
        take(sir, declarations = false, _.forall(SIRLinker.isDefinition))

    private def take(
        sir: SIR,
        declarations: Boolean,
        lets: List[Binding] => Boolean
    ): (Wrappers, SIR) = {
        @annotation.tailrec
        def loop(current: SIR, layers: List[Layer]): (Wrappers, SIR) = current match
            case SIR.Decl(data, term) if declarations => loop(term, Decl(data) :: layers)
            case SIR.Let(bindings, body, flags, anns) if lets(bindings) =>
                loop(body, Let(bindings, flags, anns) :: layers)
            case expression => Wrappers(layers.reverse) -> expression
        loop(sir, Nil)
    }
}
