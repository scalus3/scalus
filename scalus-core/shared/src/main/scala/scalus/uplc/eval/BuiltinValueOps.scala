package scalus.uplc.eval

import scalus.uplc.builtin.{BuiltinValue, ByteString, Data}
import scalus.uplc.builtin.ByteString.given

import scala.collection.immutable.SortedMap

/** Internal helpers for BuiltinValue operations in CEK machine.
  *
  * CIP-0153 MaryEraValue implementation.
  */
object BuiltinValueOps:

    // Validation constants
    val maxKeyLen: Int = 32
    val quantityMin: BigInt = -BigInt(2).pow(127)
    val quantityMax: BigInt = BigInt(2).pow(127) - 1

    def isValidKey(bs: ByteString): Boolean = bs.size <= maxKeyLen
    def isValidQuantity(i: BigInt): Boolean = i >= quantityMin && i <= quantityMax

    val empty: BuiltinValue = BuiltinValue.unsafeFromInner(SortedMap.empty)

    def unsafeFromMap(m: SortedMap[ByteString, SortedMap[ByteString, BigInt]]): BuiltinValue =
        BuiltinValue.unsafeFromInner(m)

    // Costing helpers
    def totalSize(v: BuiltinValue): Int = v.inner.values.map(_.size).sum
    def outerSize(v: BuiltinValue): Int = v.inner.size
    def maxInnerSize(v: BuiltinValue): Int =
        if v.inner.isEmpty then 0 else v.inner.values.map(_.size).max
    def negativeCount(v: BuiltinValue): Int = v.inner.values.flatMap(_.values).count(_ < 0)

    /** Memory usage based on log of outer and max inner map sizes.
      *
      * Matches Plutus ValueMaxDepth: log2(outerSize) + 1 + log2(maxInnerSize) + 1
      */
    def valueMaxDepth(v: BuiltinValue): CostingInteger = {
        val outer = outerSize(v)
        val inner = maxInnerSize(v)
        val logOuter = if outer > 0 then 32 - Integer.numberOfLeadingZeros(outer) else 0
        val logInner = if inner > 0 then 32 - Integer.numberOfLeadingZeros(inner) else 0
        CostingInteger((logOuter + logInner).toLong)
    }

    /** Insert or update a token amount in a value.
      *
      * If amount is 0, the token is removed. If the inner map becomes empty, the currency is
      * removed.
      *
      * @throws BuiltinException
      *   if currency or token name exceeds 32 bytes, or quantity overflows 128-bit range
      */
    def insertCoin(
        currency: ByteString,
        token: ByteString,
        amount: BigInt,
        value: BuiltinValue
    ): BuiltinValue = {
        if !isValidQuantity(amount) then
            throw new BuiltinException(s"Quantity $amount out of 128-bit signed integer range")

        if amount == BigInt(0) then
            // Remove token - long keys are allowed when amount is 0
            value.inner.get(currency) match {
                case None => value
                case Some(tokens) =>
                    val newTokens = tokens - token
                    if newTokens.isEmpty then BuiltinValue.unsafeFromInner(value.inner - currency)
                    else BuiltinValue.unsafeFromInner(value.inner.updated(currency, newTokens))
            }
        else
            // Insert/update token - validate keys only for non-zero amounts
            if !isValidKey(currency) then
                throw new BuiltinException(
                  s"Currency symbol exceeds max length of $maxKeyLen bytes"
                )
            if !isValidKey(token) then
                throw new BuiltinException(s"Token name exceeds max length of $maxKeyLen bytes")
            val tokens = value.inner.getOrElse(currency, SortedMap.empty[ByteString, BigInt])
            val newTokens = tokens.updated(token, amount)
            BuiltinValue.unsafeFromInner(value.inner.updated(currency, newTokens))
    }

    /** Lookup a token amount in a value.
      *
      * Returns 0 if the token is not found.
      */
    def lookupCoin(currency: ByteString, token: ByteString, value: BuiltinValue): BigInt = {
        value.inner.get(currency) match {
            case None         => BigInt(0)
            case Some(tokens) => tokens.getOrElse(token, BigInt(0))
        }
    }

    /** Merge two values, adding amounts for matching tokens.
      *
      * Zero-sum tokens are removed. Empty inner maps are removed.
      *
      * @throws BuiltinException
      *   if any resulting quantity overflows 128-bit range
      */
    def unionValue(v1: BuiltinValue, v2: BuiltinValue): BuiltinValue = {
        val allCurrencies = v1.inner.keySet ++ v2.inner.keySet
        val result =
            allCurrencies.foldLeft(SortedMap.empty[ByteString, SortedMap[ByteString, BigInt]]) {
                (acc, currency) =>
                    val tokens1 = v1.inner.getOrElse(currency, SortedMap.empty[ByteString, BigInt])
                    val tokens2 = v2.inner.getOrElse(currency, SortedMap.empty[ByteString, BigInt])
                    val allTokens = tokens1.keySet ++ tokens2.keySet
                    val mergedTokens = allTokens.foldLeft(SortedMap.empty[ByteString, BigInt]) {
                        (tokenAcc, token) =>
                            val amount1 = tokens1.getOrElse(token, BigInt(0))
                            val amount2 = tokens2.getOrElse(token, BigInt(0))
                            val sum = amount1 + amount2
                            if !isValidQuantity(sum) then
                                throw new BuiltinException(
                                  s"Overflow: sum $sum out of 128-bit signed integer range"
                                )
                            if sum != BigInt(0) then tokenAcc.updated(token, sum)
                            else tokenAcc
                    }
                    if mergedTokens.nonEmpty then acc.updated(currency, mergedTokens)
                    else acc
            }
        BuiltinValue.unsafeFromInner(result)
    }

    /** Check if v1 contains at least the amounts in v2.
      *
      * For each token in v2, check that v1 has at least that amount. Both values must not contain
      * negative amounts.
      *
      * @throws BuiltinException
      *   if either value contains negative amounts
      */
    def valueContains(v1: BuiltinValue, v2: BuiltinValue): Boolean = {
        if negativeCount(v1) > 0 then
            throw new BuiltinException("valueContains: first value contains negative amounts")
        if negativeCount(v2) > 0 then
            throw new BuiltinException("valueContains: second value contains negative amounts")
        if totalSize(v1) < totalSize(v2) then false
        else
            v2.inner.forall { case (currency, tokens2) =>
                v1.inner.get(currency) match {
                    case None => false
                    case Some(tokens1) =>
                        tokens2.forall { case (token, amount2) =>
                            val amount1 = tokens1.getOrElse(token, BigInt(0))
                            amount1 >= amount2
                        }
                }
            }
    }

    /** Multiply all amounts in a value by a scalar.
      *
      * If scalar is 0, returns empty value.
      *
      * @throws BuiltinException
      *   if any resulting quantity overflows 128-bit range
      */
    def scaleValue(scalar: BigInt, value: BuiltinValue): BuiltinValue = {
        if scalar == BigInt(0) then return empty
        if scalar == BigInt(1) then return value

        val result =
            value.inner.foldLeft(SortedMap.empty[ByteString, SortedMap[ByteString, BigInt]]) {
                case (acc, (currency, tokens)) =>
                    val scaledTokens = tokens.foldLeft(SortedMap.empty[ByteString, BigInt]) {
                        case (tokenAcc, (token, amount)) =>
                            val scaled = amount * scalar
                            if !isValidQuantity(scaled) then
                                throw new BuiltinException(
                                  s"Overflow: scaled amount $scaled out of 128-bit signed integer range"
                                )
                            // Maintain invariant: no zero quantities
                            if scaled != BigInt(0) then tokenAcc.updated(token, scaled)
                            else tokenAcc
                    }
                    if scaledTokens.nonEmpty then acc.updated(currency, scaledTokens)
                    else acc
            }
        BuiltinValue.unsafeFromInner(result)
    }

    /** Convert BuiltinValue to Data representation.
      *
      * Data encoding: Map ByteString (Map ByteString Integer)
      *
      * Delegates to BuiltinValue.toData.
      */
    def toData(value: BuiltinValue): Data = BuiltinValue.toData(value)

    /** Convert Data to BuiltinValue.
      *
      * @throws BuiltinException
      *   if data is not in the expected format, or validation fails
      */
    def fromData(data: Data): BuiltinValue = {
        // Plutus `unValueData` requires the Data to be in canonical form and FAILS otherwise (it does
        // not normalize): currency symbols and, within each currency, token names must be strictly
        // ascending (so no duplicate or unordered keys), inner token maps must be non-empty, and no
        // quantity may be zero. Mirrors PlutusCore.Value.buildValueWith.
        val keyOrdering = summon[Ordering[ByteString]]
        data match {
            case Data.Map(entries) =>
                var result = SortedMap.empty[ByteString, SortedMap[ByteString, BigInt]]
                var prevCurrency: Option[ByteString] = None
                val it = entries.toScalaList.iterator
                while it.hasNext do
                    val (keyData, valueData) = it.next()
                    val currency = keyData match {
                        case Data.B(bs) => bs
                        case _ =>
                            throw new BuiltinException("Expected ByteString for currency symbol")
                    }
                    if !isValidKey(currency) then
                        throw new BuiltinException(
                          s"Currency symbol exceeds max length of $maxKeyLen bytes"
                        )
                    prevCurrency match
                        case Some(p) if !keyOrdering.lt(p, currency) =>
                            throw new BuiltinException(
                              "unValueData: currency symbols not strictly ascending"
                            )
                        case _ =>
                    prevCurrency = Some(currency)

                    val tokens = valueData match {
                        case Data.Map(tokenEntries) =>
                            var tokenAcc = SortedMap.empty[ByteString, BigInt]
                            var prevToken: Option[ByteString] = None
                            val tit = tokenEntries.toScalaList.iterator
                            while tit.hasNext do
                                val (tokenKeyData, tokenValueData) = tit.next()
                                val token = tokenKeyData match {
                                    case Data.B(bs) => bs
                                    case _ =>
                                        throw new BuiltinException(
                                          "Expected ByteString for token name"
                                        )
                                }
                                if !isValidKey(token) then
                                    throw new BuiltinException(
                                      s"Token name exceeds max length of $maxKeyLen bytes"
                                    )
                                prevToken match
                                    case Some(p) if !keyOrdering.lt(p, token) =>
                                        throw new BuiltinException(
                                          "unValueData: token names not strictly ascending"
                                        )
                                    case _ =>
                                prevToken = Some(token)
                                val amount = tokenValueData match {
                                    case Data.I(i) => i
                                    case _ =>
                                        throw new BuiltinException("Expected Integer for amount")
                                }
                                if !isValidQuantity(amount) then
                                    throw new BuiltinException(
                                      s"Quantity $amount out of 128-bit signed integer range"
                                    )
                                if amount == BigInt(0) then
                                    throw new BuiltinException("unValueData: zero quantity")
                                tokenAcc = tokenAcc.updated(token, amount)
                            tokenAcc
                        case _ => throw new BuiltinException("Expected Map for token map")
                    }
                    if tokens.isEmpty then
                        throw new BuiltinException("unValueData: empty inner map")
                    result = result.updated(currency, tokens)
                BuiltinValue.unsafeFromInner(result)
            case _ => throw new BuiltinException("Expected Map for BuiltinValue")
        }
    }
