package scalus.cardano.ledger.value

/** The [[Coin]] object can be used to work with quantities of cardano assets, including lovelace
  * (ada) and native assets. With Cardano, quantities of assets have various restrictions in
  * different contexts; see the NOTES section below for a description.
  *
  * The object exposes safe arithmetic operations on three underlying types:
  *
  *   - A [[Coin.Coin]] type, which is an opaque newtype wrapper around a non-negative, bounded
  *     (64-bit) amount of coins suitable for use in a [[`TransactionBody`]]'s outputs field.
  *   - An unbounded [[Coin.Unbounded]] type that can be used in intermediate calculations where the
  *     total amount may exceed the capacity of a `Word64`.
  *   - An unbounded [[Coin.Fractional]] type that can be used, e.g., for exchange rates.
  *
  * Functions to convert safely between these three types are provided. "Safety" in this case means:
  *   - Detecting overflow/underflow when converting from bounded to unbounded types
  *   - Tested laws-compliance for most algebraic operations (ordering, vector space, commutative
  *     module)
  *     - Safe projection/injection and other type-changing round-trips where applicable. For
  *       example, if we have
  *       - c : Coin.Coin
  *       - i = SafeLong then `(c.scaleIntegral(i).scaleFractional(Rational(1, i))) == Right(c)`.
  *
  * NOTES: In the haskell `cardano-ledger`, `Coin` is represented as an (unbounded) `Integer`, but
  * the CBOR serialization instances convert directly from a (signed) `Long`. This is contrary to
  * the plutus-core spec, which defines an alternative CBOR encoding for integers larger than 64
  * bits. It is also contrary to the conway CDDL spec, which defines the coin in a transaction
  * output as a Word64 (i.e., an _unsigned_ Long).
  *
  * We do our best here to support the lowest common denominator, which is just using `Long` for the
  * bounded representation.
  */
package object coin {}
