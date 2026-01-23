package scalus.cardano.onchain.plutus.v1

import scalus.{Compile, Ignore}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.ledger
import scalus.cardano.ledger.{AssetName, Coin, Hash, MultiAsset}
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.{List, Option, SortedMap}
import scalus.cardano.onchain.plutus.prelude.{Eq, Ord, Order}
import scalus.cardano.onchain.plutus.prelude.{!==, <=>, ===}

import scala.annotation.tailrec

case class Value private (toSortedMap: SortedMap[PolicyId, SortedMap[TokenName, BigInt]]) {
    override def toString: String = {
        import Value.{adaPolicyId, adaTokenName}
        val lovelace = toSortedMap
            .get(adaPolicyId)
            .flatMap(_.get(adaTokenName))
            .getOrElse(BigInt(0))
        val adaStr = s"${lovelace / 1000000}.${"%06d".format((lovelace % 1000000).toLong)} ADA"
        val hasAssets = toSortedMap.toList.asScala.exists { case (cs, tokens) =>
            cs != adaPolicyId || tokens.toList.asScala.exists(_._1 != adaTokenName)
        }
        if hasAssets then s"Value($adaStr, ${Value.debugToString(this)})"
        else s"Value($adaStr)"
    }
}

@Compile
object Value extends ValueOffchainOps {

    /** A value representing zero units of any policy id or token.
      *
      * @example
      *   {{{
      *   Value.zero.isZero === true
      *   Value.zero.quantityOf(ByteString.fromString("policyId"), ByteString.fromString("tokenName")) === BigInt(0)
      *   Value.zero.getLovelace === BigInt(0)
      *   }}}
      */
    val zero: Value = Value(SortedMap.empty)

    /** Creates a `Value` containing the specified amount of a specific policy id and token. If the
      * amount is zero, it returns `Value.zero`.
      *
      * @param cs
      *   The policy id
      * @param tn
      *   The token name
      * @param v
      *   The amount of the token
      * @return
      *   A new `Value` containing the specified amount of the token, or `Value.zero` if the amount
      *   is zero
      * @example
      *   {{{
      *   Value(Value.adaPolicyId, Value.adaTokenName, BigInt(1000000)) === Value.lovelace(BigInt(1000000))
      *
      *   val policyId: PolicyId = ByteString.fromString("policyId")
      *   val tokenName: TokenName = ByteString.fromString("tokenName")
      *   val value = Value(policyId, tokenName, BigInt(100))
      *   value.quantityOf(policyId, tokenName) === BigInt(100)
      *   value.getLovelace === BigInt(0)
      *   value.isZero === false
      *
      *   Value(policyId, tokenName, BigInt(0)) === Value.zero
      *   }}}
      */
    def apply(cs: PolicyId, tn: TokenName, v: BigInt): Value =
        if v !== BigInt(0) then Value(SortedMap.singleton(cs, SortedMap.singleton(tn, v))) else zero

    /** Creates a `Value` representing a specific amount of ADA in lovelace.
      *
      * This is a convenience method for creating a `Value` with the ADA policy id and token name,
      * where the amount is specified in lovelace (1 ADA = 1,000,000 lovelace).
      *
      * @param v
      *   The amount of Lovelace
      * @return
      *   A new `Value` containing only the specified amount of Lovelace
      * @example
      *   {{{
      *   Value.lovelace(BigInt(1000000)) === Value(Value.adaPolicyId, Value.adaTokenName, BigInt(1000000))
      *   Value.lovelace(BigInt(1000000)).getLovelace === BigInt(1000000)
      *   Value.lovelace(BigInt(1000000)).quantityOf(ByteString.fromString("policyId"), ByteString.fromString("tokenName")) === BigInt(0)
      *   Value.lovelace(BigInt(0)) === Value.zero
      *   }}}
      */
    def lovelace(v: BigInt): Value = apply(adaPolicyId, adaTokenName, v)

    /** Creates a `Value` from a list of policy ids paired with their token amounts, without
      * validation.
      *
      * This method directly constructs a `Value` from the input list without checking for zero
      * amounts or empty token lists. Use with caution as it may create invalid states.
      *
      * @param list
      *   A list of tuples containing policy ids and their associated token amounts
      * @return
      *   A `Value` constructed directly from the input list
      * @example
      *   {{{
      *   val tokens = List.Cons(
      *     (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   Value.unsafeFromList(tokens)
      *   }}}
      * @see
      *   [[fromList]] or [[fromStrictlyAscendingListWithNonZeroAmounts]] for safe versions
      */
    def unsafeFromList(
        list: List[(PolicyId, List[(TokenName, BigInt)])]
    ): Value = Value(
      SortedMap.unsafeFromList(
        list.map { pair => (pair._1, SortedMap.unsafeFromList(pair._2)) }
      )
    )

    /** Creates a `Value` from a sorted map of policy ids to their token amounts, without
      * validation.
      *
      * This method directly constructs a `Value` from the input sorted map without checking for
      * zero amounts or empty token lists. Use with caution as it may create invalid states.
      *
      * @param sm
      *   A sorted map of policy ids to their associated token amounts
      * @return
      *   A `Value` constructed directly from the input sorted map
      * @example
      *   {{{
      *   val tokens = SortedMap.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, SortedMap.fromList(List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil))),
      *       Cons(
      *         (ByteString.fromString("ff"), SortedMap.fromList(List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil))),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.unsafeFromSortedMap(tokens)
      *   }}}
      * @see
      *   [[fromList]] or [[fromStrictlyAscendingListWithNonZeroAmounts]] for safe versions
      */
    def unsafeFromSortedMap(
        sm: SortedMap[PolicyId, SortedMap[TokenName, BigInt]]
    ): Value = Value(sm)

    /** Creates a `Value` from a list of policy ids paired with their token amounts, filtering out
      * zero amounts.
      *
      * This method safely constructs a `Value` by:
      *   - Removing all tokens with zero amounts
      *   - Removing policy ids that have no remaining tokens after filtering
      *
      * @param list
      *   A list of tuples containing policy ids and their associated token amounts
      * @return
      *   A `Value` with zero amounts filtered out
      * @example
      *   {{{
      *   val tokens = List.Cons(
      *     (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(0)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   // The second entry will be filtered out due to zero amount
      *   Value.fromList(tokens) === Value.lovelace(BigInt(1000000))
      *   }}}
      * @see
      *   [[unsafeFromList]] for an unfiltered unsafe version or
      *   [[fromStrictlyAscendingListWithNonZeroAmounts]] for a faster stricter version
      */
    def fromList(
        list: List[(PolicyId, List[(TokenName, BigInt)])]
    ): Value = Value(
      SortedMap.fromList(
        list.filterMap { pair =>
            val tokens = pair._2.filter { _._2 !== BigInt(0) }

            if tokens.nonEmpty then Option.Some((pair._1, SortedMap.fromList(tokens)))
            else Option.None
        }
      )
    )

    /** Creates a `Value` from a strictly ascending list of policy ids and token amounts, requiring
      * non-zero amounts.
      *
      * This method enforces stricter requirements than [[fromList]]:
      *   - The input list must be strictly ascending by policy id
      *   - Each token list must be strictly ascending by token name
      *   - All token amounts must be non-zero
      *   - Token lists cannot be empty
      *
      * @param list
      *   A strictly ascending list of tuples containing policy ids and their associated token
      *   amounts
      * @return
      *   A `Value` constructed from the strictly ascending lists
      * @throws scalus.cardano.onchain.RequirementError
      *   If any token amount is zero or any token list is empty
      * @example
      *   {{{
      *   // Successful case - ascending order and non-zero amounts
      *   val validTokens = List.Cons(
      *     (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *     Cons(
      *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *   Value.fromStrictlyAscendingListWithNonZeroAmounts(validTokens) // Succeeds
      *
      *   // Error case - contains zero amount
      *   val invalidTokens = List.Cons(
      *     (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(0)), List.Nil)),
      *     List.Nil
      *   )
      *   Value.fromStrictlyAscendingListWithNonZeroAmounts(invalidTokens) // Throws RequirementError
      *   }}}
      * @see
      *   [[unsafeFromList]] for an unsafe fast version or [[fromList]] for a more permissive slow
      *   version that filters invalid entries
      */
    def fromStrictlyAscendingListWithNonZeroAmounts(
        list: List[(PolicyId, List[(TokenName, BigInt)])]
    ): Value = Value(
      SortedMap.fromStrictlyAscendingList(
        list.map { case (policyId, tokens) =>
            scalus.cardano.onchain.plutus.prelude.require(
              tokens.nonEmpty && tokens.forall { case (_, v) => v !== BigInt(0) },
              "Token amounts must be non-zero and token lists must not be empty"
            )

            (policyId, SortedMap.fromStrictlyAscendingList(tokens))
        }
      )
    )

    /** The policy id for ADA, represented as an empty `ByteString`.
      *
      * @example
      *   {{{
      *   Value.adaPolicyId === ByteString.empty
      *   }}}
      */
    val adaPolicyId: PolicyId = ByteString.empty

    /** The token name for ADA, represented as an empty `ByteString`.
      *
      * @example
      *   {{{
      *   Value.adaTokenName === ByteString.empty
      *   }}}
      */
    val adaTokenName: TokenName = ByteString.empty

    /** Tests if two token asset maps contain exactly the same tokens with equal amounts.
      *
      * Compares two maps of token names to amounts, treating absent tokens as having zero amount.
      * The comparison checks if each token has equal amounts in both maps.
      *
      * @param a
      *   First map of token names to amounts
      * @param b
      *   Second map of token names to amounts
      * @return
      *   `true` if both maps contain the same tokens with equal amounts, `false` otherwise
      * @example
      *   {{{
      *   val assets1 = SortedMap.fromList(
      *     List.Cons(
      *       (ByteString.fromString("TOKEN1"), BigInt(100)),
      *       List.Cons(
      *         (ByteString.fromString("TOKEN2"), BigInt(200)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   val assets2 = SortedMap.fromList(
      *     List.Cons(
      *       (ByteString.fromString("TOKEN1"), BigInt(100)),
      *       List.Cons(
      *         (ByteString.fromString("TOKEN2"), BigInt(200)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.equalsAssets(assets1, assets2) === true
      *   }}}
      */
    def equalsAssets(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt]
    ): Boolean = {
        @tailrec
        def go(
            lhs: List[(TokenName, BigInt)],
            rhs: List[(TokenName, BigInt)]
        ): Boolean = {
            lhs match
                case List.Nil => rhs.forall(_._2 === BigInt(0))
                case List.Cons(lhsHead, lhsTail) =>
                    rhs match
                        case List.Nil => lhs.forall(_._2 === BigInt(0))
                        case List.Cons(rhsHead, rhsTail) =>
                            (lhsHead === rhsHead) && go(lhsTail, rhsTail)
        }

        go(a.toList, b.toList)
    }

    /** Tests if two `Value` instances are equal.
      *
      * Compares two `Value` instances by checking if they contain the same policy ids and tokens
      * with equal amounts, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   `true` if both `Value` instances are equal, `false` otherwise
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(1000000))
      *   Value.eq(value1, value2) === true
      *
      *   val value3 = Value.lovelace(BigInt(500000))
      *   Value.eq(value1, value3) === false
      *   }}}
      */
    def eq(a: Value, b: Value): Boolean = a.toSortedMap === b.toSortedMap

    /** Tests if two `Value` instances are not equal.
      *
      * Compares two `Value` instances by checking if they contain different policy ids or tokens
      * with unequal amounts, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   `true` if the `Value` instances are not equal, `false` otherwise
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(1000000))
      *   Value.nonEq(value1, value2) === false
      *
      *   val value3 = Value.lovelace(BigInt(500000))
      *   Value.nonEq(value1, value3) === true
      *   }}}
      */
    def nonEq(a: Value, b: Value): Boolean = !eq(a, b)

    /** Negates all token amounts in a `Value`, effectively creating the additive inverse.
      *
      * This method traverses the `Value` and negates each token amount by subtracting it from zero.
      *
      * @param v
      *   The `Value` to negate
      * @return
      *   A new `Value` with all token amounts negated
      * @example
      *   {{{
      *   val value1 = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   val value2 = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(-1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(-100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.negate(value1) === value2
      *   }}}
      */
    def negate(v: Value): Value = Value(v.toSortedMap.mapValues {
        _.mapValues { subtractInteger(0, _) }
    })

    /** Adds two `Value` instances together, combining their token amounts.
      *
      * This method performs an element-wise addition of the token amounts in both `Value`
      * instances, treating absent tokens as having zero amount.
      *
      * @param a
      *   First `Value` instance
      * @param b
      *   Second `Value` instance
      * @return
      *   A new `Value` containing the combined token amounts
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.plus(value1, value2) === Value.lovelace(BigInt(1500000))
      *   }}}
      */
    def plus(a: Value, b: Value): Value = binaryOpValues(a, b, addInteger)

    /** Subtracts the second `Value` from the first, effectively performing element-wise subtraction
      * of token amounts.
      *
      * This method subtracts the token amounts in the second `Value` from those in the first,
      * treating absent tokens as having zero amount.
      *
      * @param a
      *   The `Value` to subtract from
      * @param b
      *   The `Value` to subtract
      * @return
      *   A new `Value` containing the result of the subtraction
      * @example
      *   {{{
      *   val value1 = Value.lovelace(BigInt(1000000))
      *   val value2 = Value.lovelace(BigInt(500000))
      *   Value.minus(value1, value2) === Value.lovelace(BigInt(500000))
      *   }}}
      */
    def minus(a: Value, b: Value): Value = binaryOpValues(a, b, subtractInteger)

    /** Multiplies all token amounts in a `Value` by a specified factor.
      *
      * This method scales each token amount in the `Value` by the given factor, effectively
      * multiplying all amounts by the same integer.
      *
      * @param v
      *   The `Value` to multiply
      * @param factor
      *   The factor to multiply each token amount by
      * @return
      *   A new `Value` with all token amounts multiplied by the factor, or `Value.zero` if the
      *   factor is zero
      * @example
      *   {{{
      *   val value = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.multiply(value, BigInt(2)) === Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(2000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(200)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   Value.multiply(value, BigInt(0)) === Value.zero
      *   }}}
      */
    def multiply(v: Value, factor: BigInt): Value =
        if factor !== BigInt(0) then
            Value(
              v.toSortedMap.mapValues { _.mapValues { _ * factor } }
            )
        else zero

    /** Converts a `Value` to a debug string representation.
      *
      * Formats the `Value` as a string showing policy IDs and token amounts in a human-readable
      * format. Each policy ID and its associated tokens are displayed with their hex
      * representations and amounts.
      *
      * @param v
      *   The `Value` to convert to string
      * @return
      *   A formatted string representation of the `Value`
      * @example
      *   {{{
      *   val value = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Cons(
      *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
      *         List.Nil
      *       )
      *     )
      *   )
      *
      *   // Prints: { policy# -> { #: 1000000 }, policy#6666 -> { #544f4b454e: 100 } }
      *   println(Value.debugToString(value))
      *   }}}
      */
    @Ignore
    def debugToString(v: Value): String = {
        val pairs = v.toSortedMap.toList.asScala.map { case (cs, tokens) =>
            val tokenPairs = tokens.toList.asScala.map { case (tn, amount) =>
                s"#${tn.toHex}: $amount"
            }
            s"policy#${cs.toHex} -> { ${tokenPairs.mkString(", ")} }"
        }
        s"{ ${pairs.mkString(", ")} }"
    }

    /** Implementation of the [[scalus.cardano.onchain.plutus.prelude.Eq]] type class for `Value`.
      *
      * Provides equality comparison between two `Value` instances by delegating to [[Value.eq]].
      * Two `Value` instances are considered equal if they contain exactly the same policy ids and
      * tokens with equal amounts.
      *
      * @example
      *   {{{
      *   val value1 = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   val value2 = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   value1 === value2 // true, using derived Eq instance
      *   }}}
      */
    given valueEq: Eq[Value] = (a, b) => eq(a, b)

    /** Implementation of the [[scalus.cardano.onchain.plutus.prelude.Ord]] type class for `Value`.
      * Only makes sense as a key for collections.
      *
      * Provides total ordering for `Value` instances by comparing their underlying sorted maps. The
      * ordering is determined by:
      *   1. First comparing policy ids
      *   2. For equal policy ids, comparing their token maps
      *   3. For equal token names, comparing their amounts
      *
      * @example
      *   {{{
      * val value1 = Value.lovelace(BigInt(1000000))
      * val value2 = Value.lovelace(BigInt(500000))
      *
      * value1 <=> value2 // Returns Order.Greater
      *   }}}
      */
    val valueOrd: Ord[Value] = (x: Value, y: Value) => x.toSortedMap <=> y.toSortedMap

    /** Implementation of the [[scalus.uplc.builtin.ToData]] type class for `Value`.
      *
      * Converts a `Value` to a `Data` representation by converting its sorted map structure.
      *
      * @example
      *   {{{
      *   val value = Value.fromList(
      *     List.Cons(
      *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
      *       List.Nil
      *     )
      *   )
      *
      *   val data = value.toData // Converts to Data representation
      *   }}}
      */
    given valueToData: ToData[Value] = _.toSortedMap.toData

    /** Implementation of the [[scalus.uplc.builtin.FromData]] type class for `Value`.
      *
      * Converts a `Data` representation back into a `Value`. This method assumes that the input
      * data is well-formed and does not perform any validation on the token amounts or structure.
      *
      * @example
      *   {{{
      *   val data: Data = ... // Some Data representation of a Value
      *   val value = Value.fromData(data) // Converts from Data to Value
      *   }}}
      */
    given valueFromData: FromData[Value] =
        (data: Data) => {
            Value(
              fromData[SortedMap[PolicyId, SortedMap[TokenName, BigInt]]](data)
            )
        }

    /** Implementation of the [[scalus.uplc.builtin.FromData]] type class for `Value` with
      * validation. Validates that:
      *   - All token amounts are non-zero
      *   - No currency symbol has an empty token list
      *
      * This method ensures that the `Value` is well-formed and meets the expected structure.
      *
      * @example
      *   {{{
      *   val data: Data = ... // Some Data representation of a Value
      *   val value = Value.valueFromDataWithValidation(data) // Converts from Data to Value with validation
      *   }}}
      */
    def valueFromDataWithValidation: FromData[Value] =
        (data: Data) => {
            given [A: FromData: Ord, B: FromData]: FromData[SortedMap[A, B]] =
                SortedMap.sortedMapFromDataWithValidation

            val payload = fromData[SortedMap[PolicyId, SortedMap[TokenName, BigInt]]](data)

            scalus.cardano.onchain.plutus.prelude.require(
              payload.forall { case (_, tokens) =>
                  tokens.nonEmpty && tokens.forall { case (_, v) => v !== BigInt(0) }
              },
              "Token amounts must be non-zero and token lists must not be empty"
            )

            Value(payload)
        }

    extension (v: Value)
        /** Extension alias for [[Value.negate]]. */
        inline def unary_- : Value = negate(v)

        /** Extension alias for [[Value.plus]]. */
        inline def +(other: Value): Value = plus(v, other)

        /** Extension alias for [[Value.minus]]. */
        inline def -(other: Value): Value = minus(v, other)

        /** Extension alias for [[Value.multiply]]. */
        inline def *(factor: BigInt): Value = multiply(v, factor)

        /** Extension alias for [[Value.debugToString]]. */
        @Ignore
        inline def showDebug: String = debugToString(v)

        /** Returns the amount of Lovelace in this `Value`.
          *
          * If the `Value` contains no Lovelace, it returns zero.
          *
          * @return
          *   The amount of Lovelace in this `Value`
          * @example
          *   {{{
          *   val value = Value.lovelace(BigInt(1000000))
          *   value.getLovelace === BigInt(1000000)
          *
          *   val emptyValue = Value.zero
          *   emptyValue.getLovelace === BigInt(0)
          *   }}}
          */
        def getLovelace: BigInt = quantityOf(adaPolicyId, adaTokenName)

        /** Checks if this `Value` is zero, meaning it contains no tokens or currency symbols.
          *
          * @return
          *   `true` if the `Value` is empty, `false` otherwise
          * @example
          *   {{{
          *   val value = Value.zero
          *   value.isZero === true
          *
          *   val nonZeroValue = Value.lovelace(BigInt(1000000))
          *   nonZeroValue.isZero === false
          *   }}}
          */
        inline def isZero: Boolean = v.toSortedMap.isEmpty

        /** Checks if this `Value` is non-zero, meaning it contains at least one token or currency
          * symbol with a non-zero amount.
          *
          * @return
          *   `true` if the `Value` is non-empty, `false` otherwise
          * @example
          *   {{{
          *   val value = Value.zero
          *   value.nonZero === false
          *
          *   val nonZeroValue = Value.lovelace(BigInt(1000000))
          *   nonZeroValue.nonZero === true
          *   }}}
          */
        inline def nonZero: Boolean = !v.isZero

        /** Checks if this `Value` is non-zero and positive, meaning it contains at least one token
          * or currency symbol with a non-zero positive amount and all amounts are positive.
          *
          * @return
          *   `true` if the `Value` is non-empty and has all positive amounts, `false` otherwise
          */
        inline def isPositive: Boolean =
            nonZero && v.toSortedMap.forall(_._2.forall(_._2 > BigInt(0)))

        /** Gets the amount of a specific token in a policy id from a `Value`.
          *
          * Returns the token amount for the given policy id and token name pair. If either the
          * policy id or token name is not found, returns zero.
          *
          * @param cs
          *   The policy id to look up
          * @param tn
          *   The token name to look up within that policy id
          * @return
          *   The amount of the specified token, or zero if not found
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.quantityOf(Value.adaPolicyId, Value.adaTokenName) === BigInt(1000000)
          *   value.quantityOf(ByteString.fromString("ff"), ByteString.fromString("TOKEN")) === BigInt(100)
          *   value.quantityOf(ByteString.fromString("missing"), ByteString.fromString("TOKEN")) === BigInt(0)
          *   }}}
          */
        def quantityOf(
            cs: PolicyId,
            tn: TokenName
        ): BigInt = v.toSortedMap.get(cs) match
            case Option.Some(tokens) => tokens.get(tn).getOrElse(0)
            case Option.None         => 0

        /** Get all tokens associated with a given policy.
          *
          * Returns the token `SortedMap` for the given policy id. If the policy id is not found,
          * returns an empty `SortedMap`.
          *
          * @param cs
          *   The policy id to look up
          * @return
          *   The `SortedMap` of the specified token, or an empty `SortedMap` if not found
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (utf8"ff", List.Cons((utf8"TOKEN", BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.quantityOf(Value.adaPolicyId) === SortedMap.singleton(Value.adaTokenName, BigInt(1000000))
          *   value.quantityOf(utf8"ff") === SortedMap.singleton(utf8"TOKEN", BigInt(100))
          *   value.quantityOf(utf8"missing") === SortedMap.empty
          *   }}}
          */
        def tokens(cs: PolicyId): SortedMap[TokenName, BigInt] =
            v.toSortedMap.get(cs).getOrElse(SortedMap.empty)

        /** Returns a new `Value` with all ADA/Lovelace tokens removed.
          *
          * This method creates a copy of the value with the ADA policy id removed, effectively
          * removing all Lovelace tokens while preserving other tokens.
          *
          * @return
          *   A new `Value` without any Lovelace tokens
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   val withoutAda = Value.fromList(
          *     List.Cons(
          *       (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *       List.Nil
          *     )
          *   )
          *
          *   value.withoutLovelace === withoutAda
          *   }}}
          */
        def withoutLovelace: Value = Value(v.toSortedMap.delete(adaPolicyId))

        /** Flattens the `Value` into a list of policy id, token name, and amount triples.
          *
          * Converts the nested map structure into a flat list representation where each element
          * contains the policy id, token name, and corresponding amount.
          *
          * @return
          *   A flattened list of tuples containing (policyId, tokenName, amount)
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.flatten ===
          *   List.Cons(
          *      (Value.adaPolicyId, Value.adaTokenName, BigInt(1000000)),
          *      List.Cons(
          *        (ByteString.fromString("ff"), ByteString.fromString("TOKEN"), BigInt(100)),
          *        List.Nil
          *      )
          *   )
          *   }}}
          */
        def flatten: List[(PolicyId, TokenName, BigInt)] =
            v.toSortedMap.foldRight(List.empty) { case (pair1, acc1) =>
                pair1._2.foldRight(acc1) { case (pair2, acc2) =>
                    List.Cons((pair1._1, pair2._1, pair2._2), acc2)
                }
            }

        /** A list of all policy ids in that [[scalus.cardano.onchain.plutus.v1.Value]] with
          * non-zero tokens.
          *
          * @return
          *   A list of sorted [[scalus.cardano.onchain.plutus.v1.PolicyId]]
          * @example
          *   {{{
          *   val value = Value.fromList(
          *     List.Cons(
          *       (Value.adaPolicyId, List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)),
          *       List.Cons(
          *         (ByteString.fromString("ff"), List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)),
          *         List.Nil
          *       )
          *     )
          *   )
          *
          *   value.policyIds ===
          *   List.Cons(
          *      Value.adaPolicyId,
          *      List.Cons(
          *        ByteString.fromString("ff"),
          *        List.Nil
          *      )
          *   )
          *   }}}
          */
        def policyIds: List[PolicyId] = v.toSortedMap.keys

    private def binaryOpTokens(
        a: SortedMap[TokenName, BigInt],
        b: SortedMap[TokenName, BigInt],
        op: (BigInt, BigInt) => BigInt
    ): List[(TokenName, BigInt)] = {
        def go(
            lhs: List[(TokenName, BigInt)],
            rhs: List[(TokenName, BigInt)]
        ): List[(TokenName, BigInt)] = {
            lhs match
                case List.Nil => rhs.map { case (tn, v) => (tn, op(0, v)) }
                case List.Cons(lhsPair, lhsTail) =>
                    rhs match
                        case List.Nil => lhs.map { case (tn, v) => (tn, op(v, 0)) }
                        case List.Cons(rhsPair, rhsTail) =>
                            lhsPair match
                                case (lhsToken, lhsValue) =>
                                    rhsPair match
                                        case (rhsToken, rhsValue) =>
                                            lhsToken <=> rhsToken match
                                                case Order.Less =>
                                                    List.Cons(
                                                      (lhsToken, op(lhsValue, 0)),
                                                      go(lhsTail, rhs)
                                                    )
                                                case Order.Greater =>
                                                    List.Cons(
                                                      (rhsToken, op(0, rhsValue)),
                                                      go(lhs, rhsTail)
                                                    )
                                                case Order.Equal =>
                                                    val result = op(lhsValue, rhsValue)

                                                    if result !== BigInt(0) then
                                                        List.Cons(
                                                          (lhsToken, result),
                                                          go(lhsTail, rhsTail)
                                                        )
                                                    else go(lhsTail, rhsTail) // Skip zero result
        }

        go(a.toList, b.toList)
    }

    private def binaryOpValues(
        a: Value,
        b: Value,
        op: (BigInt, BigInt) => BigInt
    ): Value = {
        def go(
            lhs: List[(PolicyId, SortedMap[TokenName, BigInt])],
            rhs: List[(PolicyId, SortedMap[TokenName, BigInt])]
        ): List[(PolicyId, List[(TokenName, BigInt)])] = {
            lhs match
                case List.Nil =>
                    rhs.map { case (cs, tokens) =>
                        (cs, binaryOpTokens(SortedMap.empty, tokens, op))
                    }
                case List.Cons(lhsPair, lhsTail) =>
                    rhs match
                        case List.Nil =>
                            lhs.map { case (cs, tokens) =>
                                (cs, binaryOpTokens(tokens, SortedMap.empty, op))
                            }
                        case List.Cons(rhsPair, rhsTail) =>
                            lhsPair match
                                case (lhsCs, lhsTokens) =>
                                    rhsPair match
                                        case (rhsCs, rhsTokens) =>
                                            lhsCs <=> rhsCs match
                                                case Order.Less =>
                                                    List.Cons(
                                                      (
                                                        lhsCs,
                                                        binaryOpTokens(
                                                          lhsTokens,
                                                          SortedMap.empty,
                                                          op
                                                        )
                                                      ),
                                                      go(lhsTail, rhs)
                                                    )
                                                case Order.Greater =>
                                                    List.Cons(
                                                      (
                                                        rhsCs,
                                                        binaryOpTokens(
                                                          SortedMap.empty,
                                                          rhsTokens,
                                                          op
                                                        )
                                                      ),
                                                      go(lhs, rhsTail)
                                                    )
                                                case Order.Equal =>
                                                    val result =
                                                        binaryOpTokens(lhsTokens, rhsTokens, op)

                                                    if result.nonEmpty then
                                                        List.Cons(
                                                          (lhsCs, result),
                                                          go(lhsTail, rhsTail)
                                                        )
                                                    else go(lhsTail, rhsTail) // Skip empty result
        }

        Value.unsafeFromList(go(a.toSortedMap.toList, b.toSortedMap.toList))
    }
}

private trait ValueOffchainOps { self: Value.type =>
    extension (v: Value) {

        /** Converts this `Value` to a [[scalus.cardano.ledger.Value]]
          *
          * @throws IllegalArgumentException
          *   if lovelace amount exceeds Long range
          * @throws IllegalArgumentException
          *   if any asset amount exceeds Long range
          * @throws IllegalArgumentException
          *   if PolicyId is not exactly 28 bytes (thrown by Hash.scriptHash)
          * @throws IllegalArgumentException
          *   if AssetName exceeds 32 bytes (thrown by AssetName constructor)
          */
        def toLedgerValue: ledger.Value = {
            import scala.collection.immutable

            val lovelace = v.getLovelace
            require(lovelace.isValidLong, s"Lovelace amount $lovelace exceeds Long range")
            val coins = Coin(lovelace.toLong)

            val assets = v.toSortedMap.toList.asScala.collect {
                case (cs, tokens) if cs != adaPolicyId => // Skip ADA policy id
                    val tokenMap = tokens.toList.asScala.map { (tn, amount) =>
                        require(
                          amount.isValidLong,
                          s"Asset amount $amount for token ${tn.toHex} in policy ${cs.toHex} exceeds Long range"
                        )
                        (AssetName(tn), amount.toLong)
                    }
                    // Hash.scriptHash validates PolicyId is exactly 28 bytes
                    Hash.scriptHash(cs) -> immutable.SortedMap.from(tokenMap)
            }

            val ma = MultiAsset(immutable.SortedMap.from(assets))
            ledger.Value(coin = coins, assets = ma)
        }
    }
}
