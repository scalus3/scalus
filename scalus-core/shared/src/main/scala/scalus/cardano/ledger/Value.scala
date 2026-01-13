package scalus.cardano.ledger

import cats.kernel.CommutativeGroup
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import org.typelevel.paiges.Doc
import scalus.utils.{Pretty, Style}

import scala.annotation.targetName

/** Represents a value in Cardano, which can be either pure ADA or ADA with multi-assets */
case class Value(coin: Coin, assets: MultiAsset = MultiAsset.empty) {
    @targetName("plus")
    infix def +(rhs: Value): Value = binOp(_ + _)(rhs)

    @targetName("minus")
    infix def -(rhs: Value): Value = binOp(_ - _)(rhs)

    @targetName("negate")
    def unary_- : Value = Value(-coin, -assets)

    def isPositive: Boolean = coin.value > 0 && assets.isPositive

    def isNegative: Boolean = coin.value < 0 && assets.isNegative

    def isZeroAda: Boolean = coin.isZero
    def nonZeroAda: Boolean = coin.nonZero
    def isEmptyAssets: Boolean = assets.isEmpty
    def nonEmptyAssets: Boolean = assets.nonEmpty
    def isOnlyAda: Boolean = coin.nonZero && assets.isEmpty
    def isOnlyAssets: Boolean = coin.isZero && assets.nonEmpty
    def isZero: Boolean = isZeroAda && isEmptyAssets
    def nonZero: Boolean = nonZeroAda || nonEmptyAssets

    private def binOp(op: (Long, Long) => Long)(rhs: Value): Value = (this, rhs) match
        case (Value(coin1, assets1), Value(coin2, assets2)) =>
            Value(Coin(op(coin1.value, coin2.value)), MultiAsset.binOp(op)(assets1, assets2))

    override def toString: String = {
        val adaStr = s"${coin.value / 1000000}.${"%06d".format(coin.value % 1000000)} ADA"
        if assets.isEmpty then s"Value($adaStr)"
        else s"Value($adaStr, $assets)"
    }
}

object Value:
    /** Zero value (0 ADA, no assets) */
    val zero: Value = Value(Coin.zero)

    /** Create a pure lovelace value */
    def lovelace(amount: Long): Value = Value(Coin(amount))

    /** Create a pure ADA value */
    def ada(amount: Long): Value = Value(Coin.ada(amount))

    /** Create a Value with a single asset and ADA */
    def asset(
        policyId: PolicyId,
        assetName: AssetName,
        amount: Long,
        lovelace: Coin = Coin.zero
    ): Value =
        Value(
          lovelace,
          MultiAsset(
            scala.collection.immutable.TreeMap(
              policyId -> scala.collection.immutable.TreeMap(assetName -> amount)
            )
          )
        )

    /** Create a Value from a map of assets with ADA */
    def assets(assets: Map[PolicyId, Map[AssetName, Long]], lovelace: Coin = Coin.zero): Value = {
        assets
            .foldLeft(Value.zero) { case (acc, (policyId, assetMap)) =>
                assetMap.foldLeft(acc) { case (acc2, (assetName, amount)) =>
                    acc2 + asset(policyId, assetName, amount)
                }
            }
            .copy(coin = lovelace)
    }

    /** Create a Value from assets under a single policy with ADA */
    def fromPolicy(
        policyId: PolicyId,
        assets: Map[AssetName, Long],
        lovelace: Coin = Coin.zero
    ): Value =
        Value(
          lovelace,
          MultiAsset(
            scala.collection.immutable.TreeMap(
              policyId -> scala.collection.immutable.TreeMap(assets.toSeq*)
            )
          )
        )

    /** Combine multiple values into one */
    def combine(values: IterableOnce[Value]): Value =
        values.iterator.foldLeft(Value.zero)(_ + _)

    /** Combine multiple values into one (varargs) */
    def combine(values: Value*): Value =
        combine(values)

    /** CBOR encoder for Value */
    given Encoder[Value] with
        def write(w: Writer, value: Value): Writer =
            if value.assets.isEmpty then
                // Pure ADA value
                w.write(value.coin)
            else
                // Multi-asset value
                w.writeArrayHeader(2) // 2 elements: coin and multi-asset
                w.write(value.coin)
                w.write(value.assets)

    /** CBOR decoder for Value */
    given Decoder[Value] with
        def read(r: Reader): Value =
            if r.hasArrayHeader then
                val size = r.readArrayHeader()
                if size != 2 then
                    r.validationFailure(s"Expected 2 elements for MultiAssetValue, got $size")

                val coin = r.read[Coin]()
                val multiAsset = r.read[MultiAsset]()
                new Value(coin, multiAsset)
            else
                // Single coin value
                val coin = r.read[Coin]()
                Value(coin)

    given CommutativeGroup[Value] with
        def combine(x: Value, y: Value): Value = x + y
        def empty: Value = Value.zero
        def inverse(x: Value): Value = -x

    import Doc.*

    /** Pretty prints Value in Lucid flat style:
      *   - ADA-only: `{ ada: X.XXXXXX }`
      *   - With assets: vertically indented entries with ada first
      */
    given Pretty[Value] with
        def pretty(a: Value, style: Style): Doc =
            val adaEntry =
                Pretty.field("ada", Pretty.lit(text(Pretty.formatAda(a.coin.value)), style), style)
            Pretty.braceList(adaEntry :: MultiAsset.formatAssetEntries(a.assets, style))
