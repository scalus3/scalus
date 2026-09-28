package scalus.verify

import scalus.uplc.builtin.{ByteString, Data}

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
}
