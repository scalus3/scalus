package scalus.cardano.ledger
package utils

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/** A [[Value]] whose amounts have no bound, for the sums that decide whether a transaction is
  * valid.
  *
  * The ledger adds values up with Haskell's `Integer` (`MaryValue`), so its sums never overflow. A
  * [[Value]] holds `Long` amounts, and a transaction can push a sum past `Long.MaxValue`: every
  * output may hold up to that many lovelace or tokens, and a certificate or a proposal declares its
  * own deposit. Such a sum is taken here and compared exactly. Zero amounts are dropped, so two
  * values are equal exactly when all their amounts are.
  */
private[cardano] final case class ExactValue private (
    coin: BigInt,
    assets: SortedMap[PolicyId, SortedMap[AssetName, BigInt]]
) {

    def +(other: ExactValue): ExactValue = ExactValue.combine(this, other, sign = 1)

    def -(other: ExactValue): ExactValue = ExactValue.combine(this, other, sign = -1)

    def +(value: Value): ExactValue = this + ExactValue(value)

    /** The same value as a [[Value]]; throws `ArithmeticException` when an amount does not fit in a
      * `Long`.
      */
    def toValue: Value = convert(amount => Coin.exact(amount).value)

    /** The same value as a [[Value]], with every amount clamped to the `Long` range, for an error
      * report.
      */
    def toClampedValue: Value = convert(ExactValue.clampedLong)

    private def convert(toLong: BigInt => Long): Value = {
        val amounts = assets.map { (policy, names) =>
            policy -> names.map((name, amount) => name -> toLong(amount))
        }
        Value(Coin(toLong(coin)), MultiAsset(amounts))
    }
}

private[cardano] object ExactValue {

    val zero: ExactValue = new ExactValue(BigInt(0), SortedMap.empty)

    def apply(value: Value): ExactValue = sum(Iterator.single(value))

    def coin(amount: BigInt): ExactValue = new ExactValue(amount, SortedMap.empty)

    def sum(values: IterableOnce[Value]): ExactValue = {
        val total = Accumulator()
        values.iterator.foreach(value => total.add(value))
        total.result
    }

    private def combine(left: ExactValue, right: ExactValue, sign: Int): ExactValue = {
        val total = Accumulator()
        total.add(left, 1)
        total.add(right, sign)
        total.result
    }

    private def clampedLong(amount: BigInt): Long = {
        if amount > Long.MaxValue then Long.MaxValue
        else if amount < Long.MinValue then Long.MinValue
        else amount.toLong
    }

    /** Adds amounts up in place, so that a sum of many values builds its maps once. */
    private final class Accumulator {
        private var coin = BigInt(0)
        private val assets = mutable.HashMap.empty[PolicyId, mutable.HashMap[AssetName, BigInt]]

        def add(value: Value): Unit = {
            coin += value.coin.value
            value.assets.assets.foreach { (policy, amounts) =>
                amounts.foreach((name, amount) => add(policy, name, BigInt(amount)))
            }
        }

        def add(value: ExactValue, sign: Int): Unit = {
            coin += value.coin * sign
            value.assets.foreach { (policy, amounts) =>
                amounts.foreach((name, amount) => add(policy, name, amount * sign))
            }
        }

        private def add(policy: PolicyId, name: AssetName, amount: BigInt): Unit = {
            val amounts = assets.getOrElseUpdate(policy, mutable.HashMap.empty)
            amounts.update(name, amounts.getOrElse(name, BigInt(0)) + amount)
        }

        /** The sum, without zero amounts or policies left empty. */
        def result: ExactValue = {
            val nonZero = assets.iterator.flatMap { (policy, amounts) =>
                val kept = SortedMap.from(amounts.iterator.filter((_, amount) => amount != 0))
                Option.when(kept.nonEmpty)(policy -> kept)
            }
            new ExactValue(coin, SortedMap.from(nonZero))
        }
    }
}
