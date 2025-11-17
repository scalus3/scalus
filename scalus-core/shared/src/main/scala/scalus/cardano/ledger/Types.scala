package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.Address
import scalus.serialization.cbor.Cbor
import scalus.utils.Hex.toHex
import upickle.default.ReadWriter as UpickleReadWriter
import cats.kernel.CommutativeGroup
import monocle.*
import scalus.uplc.eval.ExCPU

import java.util
import scala.annotation.{targetName, threadUnsafe}
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.compiletime.asMatchable

enum Era(val value: Int) extends Enumeration {
    case Babbage extends Era(6)
    case Conway extends Era(7)
}

// FIXME: make sure we validate the Coin is non-negative in ledger rules
/** Represents an amount of Cardano's native currency (ADA)
  */
final case class Coin(value: Long) derives Codec {

    /** Add another coin amount */
    @targetName("plus")
    infix def +(other: Coin): Coin = Coin(value + other.value)

    /** Subtract another coin amount */
    @targetName("minus")
    infix def -(other: Coin): Coin = Coin(value - other.value)

    @targetName("negate")
    def unary_- : Coin = Coin(-value)

    infix def >(other: Coin): Boolean = value > other.value
    infix def >=(other: Coin): Boolean = value >= other.value
    infix def <(other: Coin): Boolean = value < other.value
    infix def <=(other: Coin): Boolean = value <= other.value
}

object Coin {

    /** Zero coin value */
    val zero: Coin = Coin(0)

    /** Create lovelace amount from ADA amount, e.g.  ada(2) = 2_000_000 lovelace */
    def ada(amount: Long): Coin = Coin(amount * 1_000_000L)

    given CommutativeGroup[Coin] with
        def combine(x: Coin, y: Coin): Coin = x + y
        def empty: Coin = Coin.zero
        def inverse(x: Coin): Coin = -x
}

/** Minting MultiAsset. Can't contain zeros, can't be empty */
opaque type Mint <: MultiAsset = MultiAsset
object Mint {
    def apply(ma: MultiAsset): Mint = {
        require(!ma.isEmpty, "Mint cannot be empty")
        require(
          ma.assets.forall { case (_, assets) => assets.forall { case (_, value) => value != 0 } },
          "Mint cannot contain zero values"
        )
        ma
    }

    given Encoder[Mint] = MultiAsset.given_Encoder_MultiAsset
    given Decoder[Mint] = MultiAsset.given_Decoder_MultiAsset.map(Mint.apply)
}

case class MultiAsset private (assets: SortedMap[PolicyId, SortedMap[AssetName, Long]]) {
    def isEmpty: Boolean = assets.isEmpty

    def isPositive: Boolean = assets.values.forall(_.values.forall(_ > 0))
    def isNegative: Boolean = assets.values.forall(_.values.forall(_ < 0))

    @targetName("plus")
    infix def +(other: MultiAsset): MultiAsset = MultiAsset.binOp(_ + _)(this, other)

    @targetName("minus")
    infix def -(other: MultiAsset): MultiAsset = MultiAsset.binOp(_ - _)(this, other)

    @targetName("negate")
    def unary_- : MultiAsset = new MultiAsset(
      assets.view
          .mapValues(_.view.mapValues(v => -v).to(SortedMap))
          .to(SortedMap)
    )
}

object MultiAsset {
    val zero: MultiAsset = new MultiAsset(SortedMap.empty)
    val empty: MultiAsset = zero

    /** Create a MultiAsset with a single asset */
    def asset(policyId: PolicyId, assetName: AssetName, amount: Long): MultiAsset = {
        if amount == 0 then MultiAsset.zero
        else
            new MultiAsset(
              SortedMap(policyId -> SortedMap(assetName -> amount))
            )
    }

    /** Create a MultiAsset from a single policy with multiple assets */
    def fromPolicy(policyId: PolicyId, assets: Iterable[(AssetName, Long)]): MultiAsset = {
        val filteredAssets = assets.filter(_._2 != 0).to(SortedMap)

        if filteredAssets.isEmpty then MultiAsset.zero
        else new MultiAsset(SortedMap(policyId -> filteredAssets))
    }

    /** Create a MultiAsset from a map of policies to asset maps */
    def fromAssets(
        assets: collection.Map[PolicyId, collection.Map[AssetName, Long]]
    ): MultiAsset = {
        val filteredAssets = SortedMap.from(
          assets.view
              .mapValues(_.filter(_._2 != 0).to(SortedMap))
              .filter(_._2.nonEmpty)
        )

        if filteredAssets.isEmpty then MultiAsset.zero
        else new MultiAsset(filteredAssets)
    }

    /** Create a MultiAsset from a sequence of (PolicyId, AssetName, Amount) tuples */
    def from(assets: Iterable[(PolicyId, AssetName, Long)]): MultiAsset = {
        val groupedAssets = SortedMap.from(
          assets.view
              .filter(_._3 != 0) // Filter out zero amounts
              .groupBy(_._1) // Group by PolicyId
              .view
              .mapValues(_.map(asset => asset._2 -> asset._3).to(SortedMap))
              .filter(_._2.nonEmpty) // Filter out empty policies
        )(using summon[Ordering[PolicyId]])

        if groupedAssets.isEmpty then MultiAsset.zero
        else new MultiAsset(groupedAssets)
    }

    /** Create a MultiAsset from varargs of (PolicyId, AssetName, Amount) tuples */
    def from(assets: (PolicyId, AssetName, Long)*): MultiAsset = {
        from(assets)
    }

    /** Safely create a MultiAsset that filters out zero amounts and empty policies */
    def apply(
        assets: SortedMap[PolicyId, SortedMap[AssetName, Long]] = SortedMap.empty
    ): MultiAsset = {
        if assets.isEmpty then return MultiAsset.zero

        val filteredAssets = SortedMap.from(
          assets.view
              .mapValues(_.filter(_._2 != 0)) // Remove zero amounts
              .filter(_._2.nonEmpty) // Remove empty policies
        )

        if filteredAssets.isEmpty then MultiAsset.zero
        else new MultiAsset(filteredAssets)
    }

    private[ledger] def binOp(
        op: (Long, Long) => Long
    )(self: MultiAsset, other: MultiAsset): MultiAsset = {
        val assets: SortedMap[PolicyId, SortedMap[AssetName, Long]] =
            (self.assets.keySet ++ other.assets.keySet).view
                .flatMap { policyId =>
                    val selfAssets =
                        self.assets.getOrElse(policyId, SortedMap.empty[AssetName, Long])
                    val otherAssets =
                        other.assets.getOrElse(policyId, SortedMap.empty[AssetName, Long])

                    val mergedAssets: SortedMap[AssetName, Long] =
                        (selfAssets.keySet ++ otherAssets.keySet).view
                            .flatMap { assetName =>
                                val combinedValue =
                                    op(
                                      selfAssets.getOrElse(assetName, 0L),
                                      otherAssets.getOrElse(assetName, 0L)
                                    )
                                if combinedValue != 0 then Some(assetName -> combinedValue)
                                else None
                            }
                            .to(SortedMap)
                    if mergedAssets.nonEmpty then Some(policyId -> mergedAssets) else None
                }
                .to(SortedMap)
        new MultiAsset(assets)
    }

    given Encoder[MultiAsset] =
        Encoder.forMap[PolicyId, SortedMap[AssetName, Long], SortedMap].contramap(_.assets)

    given Decoder[MultiAsset] = Decoder { r =>
        given Decoder[TreeMap[AssetName, Long]] = Decoder.forTreeMap[AssetName, Long]
        Decoder.forTreeMap[PolicyId, TreeMap[AssetName, Long]].map(MultiAsset.apply).read(r)
    }

    given CommutativeGroup[MultiAsset] with
        def combine(x: MultiAsset, y: MultiAsset): MultiAsset = x + y
        def empty: MultiAsset = MultiAsset.zero
        def inverse(x: MultiAsset): MultiAsset = -x

}

/** Represents an asset name in Cardano's multi-asset framework
  *
  * Asset names can be between 0 and 32 bytes long.
  */
final case class AssetName(bytes: ByteString) derives Codec {

    /** Ensures the asset name is at most 32 bytes */
    require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")

    /** Convert to ASCII string if possible, otherwise returns hex representation */
    override def toString: String = {
        if bytes.bytes.forall(b => b >= 32 && b < 127) then {
            new String(bytes.bytes, "ASCII")
        } else {
            bytes.toHex
        }
    }
}

object AssetName {

    given Ordering[AssetName] = (x: AssetName, y: AssetName) => {
        Ordering[ByteString].compare(x.bytes, y.bytes)
    }

    /** Empty asset name */
    val empty: AssetName = AssetName(ByteString.empty)

    /** Create an AssetName from a hex string */
    def fromHex(hex: String): AssetName = {
        val bytes = ByteString.fromHex(hex)
        require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")
        AssetName(bytes)
    }

    /** Create an AssetName from a UTF-8 string */
    def fromString(str: String): AssetName = {
        val bytes = ByteString.fromString(str)
        require(bytes.size <= 32, s"AssetName must be at most 32 bytes, got ${bytes.size}")
        AssetName(bytes)
    }
}

/** Represents the supported scripting languages in Cardano */
enum Language extends java.lang.Enum[Language] {

    /** Plutus V1, first version of Plutus */
    case PlutusV1

    /** Plutus V2, introduced in Vasil hard fork */
    case PlutusV2

    /** Plutus V3, introduced in Conway hard fork */
    case PlutusV3

    def languageId: Int = this.ordinal
}

object Language {

    /** Gets the language from an ID */
    def fromId(id: Int): Language = fromOrdinal(id)

    extension (lang: Language) {
        def show: String = lang match {
            case Language.PlutusV1 => "v1"
            case Language.PlutusV2 => "v2"
            case Language.PlutusV3 => "v3"
        }
    }

    extension (lang: Language) {
        def introducedInVersion: MajorProtocolVersion = lang match {
            case Language.PlutusV1 => MajorProtocolVersion.alonzoPV
            case Language.PlutusV2 => MajorProtocolVersion.vasilPV
            case Language.PlutusV3 => MajorProtocolVersion.changPV
        }
    }

    /** CBOR encoder for Language */
    given Encoder[Language] = Encoder { (w, language) =>
        w.writeInt(language.languageId)
    }

    /** CBOR decoder for Language */
    given Decoder[Language] = Decoder { r =>
        fromId(r.readInt())
    }

    given Ordering[Language] = (x: Language, y: Language) => {
        x.languageId - y.languageId
    }
}

/** Represents a Cardano address bytes */
// TODO: consider using Array[Byte] instead of ByteString
opaque type AddressBytes <: ByteString = ByteString

object AddressBytes {
    inline def apply(bytes: ByteString): AddressBytes = bytes

    /** Create an Address from a hex string */
    def fromHex(hex: String): AddressBytes = ByteString.fromHex(hex)

    /** Create an Address from bech32 string */
    def fromBech32(bech32: String): AddressBytes = {
        ByteString.unsafeFromArray(Bech32.decode(bech32).data)
    }

    extension (self: AddressBytes) {
        def toAddress: Address = Address.fromByteString(self)
    }

    given Encoder[AddressBytes] = Encoder { (w, address) =>
        // here we explicitly pass the ByteString encoder to avoid StackOverflowError
        // because here Encoder[Address] is resolved as Encoder[ByteString]
        w.write[ByteString](address)(using ByteString.given_Encoder_ByteString)
    }

    given Decoder[AddressBytes] = Decoder { r =>
        // here we explicitly pass the ByteString decoder to avoid StackOverflowError
        // because here Decoder[Address] is resolved as Decoder[ByteString]
        val bytes = r.read[ByteString]()(using ByteString.given_Decoder_ByteString)
        AddressBytes(bytes)
    }
}

/** Represents a slot number in Cardano
  *
  * A slot is a fixed period of time in the Cardano blockchain. Slots are where blocks can be added
  * to the chain.
  */
final case class Slot(slot: Long) derives Codec {

    /** Ensures the slot number is non-negative */
    require(slot >= 0, s"Slot number must be non-negative, got $slot")

    /** Compare slots for ordering */
    def <=(other: Slot): Boolean = slot <= other.slot

    /** Compare slots for ordering */
    def >=(other: Slot): Boolean = slot >= other.slot

    /** Compare slots for ordering */
    def <(other: Slot): Boolean = slot < other.slot

    /** Compare slots for ordering */
    def >(other: Slot): Boolean = slot > other.slot
}

/** Represents execution units for Plutus scripts in Cardano */
case class ExUnits(memory: Long, steps: Long) derives UpickleReadWriter {

    /** Returns CPU steps (same as steps)
      *
      * @deprecated
      *   Use .steps directly
      */
    @deprecated("Use .steps directly", "0.13.0")
    def cpu: ExCPU = ExCPU(steps)

    /** Formats execution units as JSON string */
    def showJson: String =
        val memoryFormatted = String.format("%.6f", memory / 1000000d)
        val cpuFormatted = String.format("%.6f", steps / 1000000d)
        s"{ mem: $memoryFormatted, cpu: $cpuFormatted }"
    def +(other: ExUnits): ExUnits =
        ExUnits(memory + other.memory, steps + other.steps)

    /** Calculate fee for the execution units given the prices */
    def fee(prices: ExUnitPrices): Coin = {
        Coin(
          (prices.priceMemory * memory + prices.priceSteps * steps).ceil
        )
    }
}

object ExUnits {
    val zero: ExUnits = ExUnits(0, 0)

    /** CBOR encoder for ExUnits */
    given Encoder[ExUnits] = deriveEncoder

    /** CBOR decoder for ExUnits with validation for non-negative values */
    given Decoder[ExUnits] = deriveDecoder[ExUnits].mapWithReader { (r, exUnits) =>
        if exUnits.memory < 0 then
            throw Borer.Error.Unsupported(
              r.position,
              s"ExUnits memory must be non-negative, got ${exUnits.memory}"
            )
        if exUnits.steps < 0 then
            throw Borer.Error.Unsupported(
              r.position,
              s"ExUnits steps must be non-negative, got ${exUnits.steps}"
            )
        exUnits
    }

    given Ordering[ExUnits] = (x: ExUnits, y: ExUnits) => {
        if x.memory != y.memory then x.memory.compareTo(y.memory)
        else x.steps.compareTo(y.steps)
    }

    /** Cats Group instance for ExUnits, allowing algebraic operations including negative values */
    given cats.kernel.Group[ExUnits] with
        def combine(x: ExUnits, y: ExUnits): ExUnits =
            ExUnits(x.memory + y.memory, x.steps + y.steps)
        def empty: ExUnits = ExUnits.zero
        def inverse(x: ExUnits): ExUnits = ExUnits(-x.memory, -x.steps)
}

/** Represents execution unit prices in the Cardano blockchain.
  *
  * ExUnitPrices define the cost of execution units in terms of the protocol's currency, with
  * separate prices for memory usage and CPU steps.
  *
  * @param priceMemory
  *   Price per memory unit
  * @param priceSteps
  *   Price per step unit
  */
case class ExUnitPrices(
    priceMemory: NonNegativeInterval,
    priceSteps: NonNegativeInterval
) derives Codec,
      UpickleReadWriter

/** Represents cost models for script languages in the Cardano blockchain.
  *
  * Cost models define the execution costs of operations in different script languages. The keys are
  * language identifiers (0 for Plutus V1, 1 for Plutus V2, 2 for Plutus V3), and the values are
  * lists of integers representing operation costs.
  *
  * @param models
  *   Map from language identifiers to lists of operation costs
  */
case class CostModels(models: Map[Int, IndexedSeq[Long]]) derives Codec {

    /** This encoder is used to generate the language view encoding for script integrity hash
      * computation.
      *
      * The encoding follows the same logic as Cardano's getLanguageView function. For PlutusV1,
      * double-bagged encoding is used for backward compatibility. For PlutusV2/V3, standard
      * encoding is used.
      *
      * The most important part is that the languages must be sorted by their language ID in
      * ascending order.
      *
      * The most stupid part is that the languages map is sorted by its keys, and the Plutus V1 key
      * is double-encoded to a bytestring instead of an UInt. So it's always `bigger` than Plutus V2
      * and V3, and thus always comes last in the map, even though its language ID is 0.
      *
      * Plutus V1 encoded key is: 41 00 # bytes(1), 00
      *
      * Plutus V2 encoded key is: 01 # uint(1)
      */
    private given LanguageViewEncoder: Encoder[CostModels] with

        private val languageIdOrdering = new Ordering[Int] {
            def compare(a: Int, b: Int): Int = {
                if a == 0 then
                    if b == 0 then 0 else 1 // PlutusV1 is "greater" than any other language
                else if b == 0 then -1
                else a.compareTo(b) // both are non-zero, compare normally
            }
        }

        def write(w: Writer, costModels: CostModels): Writer = {
            val size = costModels.models.size
            w.writeMapHeader(size)
            val sortedModels = costModels.models.toArray.sortBy(_._1)(using languageIdOrdering)
            for (langId, costModel) <- sortedModels do
                langId match
                    case 0 =>
                        // For PlutusV1 (language id 0), the language view is the following:
                        //   * the value of costmdls map at key 0 is encoded as an indefinite length
                        //     list and the result is encoded as a bytestring. (our apologies)
                        //   * the language ID tag is also encoded twice. first as a uint then as
                        //     a bytestring.
                        // PlutusV1: Double-bagged encoding for cost model as well
                        // here we must use indefinite CBOR map encoding for backward compatibility
                        val encodedModel = Cbor.encode(costModel.toList)
                        w.writeBytes(Array(0.toByte))
                        w.writeBytes(encodedModel)
                    case 1 | 2 => // PlutusV2 - uses standard encoding
                        w.writeInt(langId)
                        w.write(costModel)
                    case _ =>
                        throw new IllegalArgumentException(s"Unknown language ID: $langId")
            end for
            w
        }

    /** Generate language view encoding for script integrity hash computation.
      *
      * This implements the same logic as Cardano's getLanguageView function. For PlutusV1, it uses
      * double-bagged encoding for backward compatibility. For PlutusV2/V3, it uses standard
      * encoding.
      */
    def getLanguageViewEncoding: Array[Byte] = {
        Cbor.encode(this)(using LanguageViewEncoder)
    }
}

object CostModels {
    import upickle.default.*

    /** ReadWriter for Cardano CLI JSON format that uses string keys like "PlutusV1", "PlutusV2",
      * "PlutusV3"
      */
    val cardanoCliReadWriter: ReadWriter[CostModels] =
        readwriter[ujson.Value].bimap[CostModels](
          costModels =>
              ujson.Obj.from(
                costModels.models.map { case (langId, costs) =>
                    Language.fromId(langId).toString -> ujson.Arr.from(
                      costs.map(v => ujson.Num(v.toDouble))
                    )
                }
              ),
          json =>
              CostModels(
                json.obj.map { case (k, v) =>
                    Language.valueOf(k).languageId -> v.arr.map(_.num.toLong).toIndexedSeq
                }.toMap
              )
        )
}

/** Represents a constitution in the Cardano blockchain governance system.
  *
  * A constitution consists of an anchor (pointing to the constitution text) and an optional script
  * hash for the constitution script.
  *
  * @param anchor
  *   The anchor pointing to the constitution text
  * @param scriptHash
  *   Optional script hash for the constitution script
  */
case class Constitution(
    anchor: Anchor,
    scriptHash: Option[ScriptHash]
) derives Codec

opaque type OriginalCborByteArray <: Array[Byte] = Array[Byte]
object OriginalCborByteArray {

    /** Create an OriginalCborByteArray from a byte array */
    def apply(bytes: Array[Byte]): OriginalCborByteArray = bytes
}

case class KeepRaw[A] private (val value: A, rawBytes: () => Array[Byte]) {
    @threadUnsafe lazy val raw: Array[Byte] = rawBytes()
    override def hashCode: Int =
        util.Arrays.hashCode(Array(value.hashCode(), util.Arrays.hashCode(raw)))

    override def equals(obj: Any): Boolean = obj.asMatchable match {
        case that: KeepRaw[?] =>
            this.value == that.value && java.util.Arrays.equals(this.raw, that.raw)
        case _ => false
    }
    override def toString: String = s"KeepRaw(value=$value, raw=${raw.toHex})"
}

object KeepRaw {
    def lens[A: Encoder](): Lens[KeepRaw[A], A] = {
        val get: KeepRaw[A] => A = kr => kr.value
        val replace: A => KeepRaw[A] => KeepRaw[A] = a => kr => KeepRaw(a)
        Lens[KeepRaw[A], A](get)(replace)
    }

    /** Create a KeepRaw instance from a value and its raw CBOR bytes
      *
      * @note
      *   This method creates a `KeepRaw` instance that may be CBOR encoded differently from the
      *   original `raw` bytes. Use it only if you know what you are doing.
      */
    def unsafe[A](value: A, raw: => Array[Byte]): KeepRaw[A] =
        new KeepRaw(value, () => raw)

    /** Create a KeepRaw instance from a value, encoding it to CBOR to get the raw bytes */
    def apply[A: Encoder](value: A): KeepRaw[A] = new KeepRaw(value, () => Cbor.encode(value))

    given [A: Decoder](using OriginalCborByteArray): Decoder[KeepRaw[A]] =
        Decoder { r =>
            // Here we need to call `dataItem()` to ensure the cursor is updated
            // see https://github.com/sirthias/borer/issues/761#issuecomment-2919035884
            r.dataItem()
            val start = r.cursor
            val value = r.read[A]()
            val di = r.dataItem()
            val end = if di == DataItem.EndOfInput then r.input.cursor else r.cursor
            val raw = summon[OriginalCborByteArray].slice(start.toInt, end.toInt)
            new KeepRaw(value, () => raw)
        }

    given [A: Encoder]: Encoder[KeepRaw[A]] = (w: Writer, value: KeepRaw[A]) => {
        // FIXME: use w.writeValueAsRawBytes instead of re-encoding when it's supported:
        // https://github.com/sirthias/borer/issues/764
//        summon[Encoder[A]].write(w, value.value)
        w.output.writeBytes(value.raw)
        w
    }
}

extension (self: KeepRaw[Data]) {
    def dataHash: DataHash = {
        // We need to calculate the hash of the raw bytes, not the decoded data
        Hash(platform.blake2b_256(ByteString.unsafeFromArray(self.raw)))
    }
}

case class Sized[A] private (value: A, size: Int) {
    override def toString: String = s"Sized(value=$value, size=$size)"
}

object Sized {
    def lens[A: Encoder](): Lens[Sized[A], A] = {
        val get: Sized[A] => A = kr => kr.value
        val replace: A => Sized[A] => Sized[A] = a => sz => Sized(a)
        Lens[Sized[A], A](get)(replace)
    }

    def apply[A: Encoder](value: A): Sized[A] =
        new Sized(value, Cbor.encode(value).length)

    given [A: Decoder](using OriginalCborByteArray): Decoder[Sized[A]] =
        Decoder { r =>
            // Here we need to call `dataItem()` to ensure the cursor is updated
            // see https://github.com/sirthias/borer/issues/761#issuecomment-2919035884
            r.dataItem()
            val start = r.cursor
            val value = r.read[A]()
            val di = r.dataItem()
            val end = if di == DataItem.EndOfInput then r.input.cursor else r.cursor
            Sized(value, end.toInt - start.toInt)
        }

    given [A: Encoder]: Encoder[Sized[A]] = (w: Writer, value: Sized[A]) => {
        summon[Encoder[A]].write(w, value.value)
    }
}
