package scalus.cardano.onchain.plutus.prelude

import scalus.compiler.{UplcRepr, UplcRepresentation}

/** Represents a variable number of arguments as a list.
  * @param toList
  * @tparam T
  */
@UplcRepr(UplcRepresentation.ProductCaseOneElement)
case class Varargs[T](list: scalus.cardano.onchain.plutus.prelude.List[T])

extension [T](seq: scala.collection.immutable.Seq[T])

    def list: scalus.cardano.onchain.plutus.prelude.List[T] =
        scalus.cardano.onchain.plutus.prelude.List.from(seq)
