package scalus.uplc.builtin

import scalus.cardano.onchain.plutus.prelude.List as PList
import ByteString.given

import scala.collection.immutable.SortedMap

/** API trait for BuiltinValue providing the internal representation and operations.
  *
  * This trait is extended by the BuiltinValue companion object. The scalus-core version provides
  * the real SortedMap-based implementation, while the scalus-plugin version provides stubs.
  */
private[builtin] trait BuiltinValueApi {

    /** Internal representation type - SortedMap for efficient sorted access */
    type InnerType = SortedMap[ByteString, SortedMap[ByteString, BigInt]]

    /** The empty value containing no tokens */
    val empty: BuiltinValue = new BuiltinValue(SortedMap.empty)

    /** Create a BuiltinValue from the internal representation */
    private[scalus] def unsafeFromInner(inner: InnerType): BuiltinValue =
        new BuiltinValue(inner)

    /** Equality check for BuiltinValue */
    private[builtin] def valueEquals(self: BuiltinValue, that: Any): Boolean = that match {
        case that: BuiltinValue => self.inner == that.inner
        case _                  => false
    }

    /** Hash code for BuiltinValue */
    private[builtin] def valueHashCode(self: BuiltinValue): Int = self.inner.hashCode()

    /** String representation for BuiltinValue */
    private[builtin] def valueToString(self: BuiltinValue): String = {
        val entries = self.inner.toList.flatMap { case (currency, tokens) =>
            tokens.toList.map { case (token, amount) =>
                s"(${currency.toHex}, ${token.toHex}) -> $amount"
            }
        }
        s"BuiltinValue(${entries.mkString(", ")})"
    }

    /** Convert BuiltinValue to Data representation.
      *
      * Data encoding: Map ByteString (Map ByteString Integer)
      */
    def toData(value: BuiltinValue): Data = {
        val entries = value.inner.toList.map { case (currency, tokens) =>
            val tokenEntries = tokens.toList.map { case (token, amount) =>
                (Data.B(token), Data.I(amount))
            }
            (Data.B(currency), Data.Map(PList.from(tokenEntries)))
        }
        Data.Map(PList.from(entries))
    }

    /** Convert Data to BuiltinValue.
      *
      * @throws IllegalArgumentException
      *   if data is not in the expected format
      */
    def fromData(data: Data): BuiltinValue = {
        data match {
            case Data.Map(entries) =>
                val result = entries.toScalaList.foldLeft(
                  SortedMap.empty[ByteString, SortedMap[ByteString, BigInt]]
                ) { case (acc, (keyData, valueData)) =>
                    val currency = keyData match {
                        case Data.B(bs) => bs
                        case _ =>
                            throw new IllegalArgumentException(
                              "Expected ByteString for currency symbol"
                            )
                    }

                    val tokens = valueData match {
                        case Data.Map(tokenEntries) =>
                            tokenEntries.toScalaList.foldLeft(SortedMap.empty[ByteString, BigInt]) {
                                case (tokenAcc, (tokenKeyData, tokenValueData)) =>
                                    val token = tokenKeyData match {
                                        case Data.B(bs) => bs
                                        case _ =>
                                            throw new IllegalArgumentException(
                                              "Expected ByteString for token name"
                                            )
                                    }
                                    val amount = tokenValueData match {
                                        case Data.I(i) => i
                                        case _ =>
                                            throw new IllegalArgumentException(
                                              "Expected Integer for amount"
                                            )
                                    }
                                    // Skip zero amounts (maintain invariant)
                                    if amount != BigInt(0) then tokenAcc.updated(token, amount)
                                    else tokenAcc
                            }
                        case _ =>
                            throw new IllegalArgumentException("Expected Map for token map")
                    }
                    // Skip empty token maps (maintain invariant)
                    if tokens.nonEmpty then acc.updated(currency, tokens)
                    else acc
                }
                new BuiltinValue(result)
            case _ => throw new IllegalArgumentException("Expected Map for BuiltinValue")
        }
    }
}
