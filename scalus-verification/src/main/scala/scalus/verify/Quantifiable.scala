package scalus.verify

import scalus.uplc.builtin.{ByteString, Data}

import scala.compiletime.summonAll
import scala.deriving.Mirror

/** Marks a type that can be quantified over in a [[Prop]].
  *
  * Reification obtains the SIR type, Lean type and UPLC representation from the static type of the
  * binder. This marker does not generate or enumerate values.
  */
trait Quantifiable[A]

object Quantifiable {
    given Quantifiable[BigInt] = new Quantifiable[BigInt] {}
    given Quantifiable[Boolean] = new Quantifiable[Boolean] {}
    given Quantifiable[ByteString] = new Quantifiable[ByteString] {}
    given Quantifiable[Data] = new Quantifiable[Data] {}

    /** A case class whose fields can be quantified over. Its values are those of its constructor,
      * with every field in its own domain (prop-semantics.md §1).
      */
    inline given product[A](using mirror: Mirror.ProductOf[A]): Quantifiable[A] = {
        summonAll[Tuple.Map[mirror.MirroredElemTypes, Quantifiable]]
        marker[A]
    }

    /** An instance for a type the caller has checked. */
    def marker[A]: Quantifiable[A] = new Quantifiable[A] {}
}
