package scalus.cardano.address

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import org.typelevel.paiges.Doc
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString
import scalus.utils.{Pretty, Style}

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Implementation of Cardano CIP-19 address format following Rust Pallas implementation structure.
  * Handles the binary structure of Cardano addresses including Shelley, Stake, and Byron addresses.
  * Uses strongly typed Hash28 classes for payment, stake and script hashes.
  */

object VarUInt {

    /** Encode positive integer using variable-length encoding as specified in CIP-19 Uses
      * continuation bits to handle arbitrarily large values efficiently
      *
      * @param value
      *   The positive integer to encode (must be >= 0)
      * @return
      *   Encoded bytes with continuation bit protocol
      */
    def encodeVariableLengthUInt(value: Long): Array[Byte] = {
        require(value >= 0, "Value must be non-negative")

        val buffer = mutable.ArrayBuffer.empty[Byte]
        var remaining = value

        while {
            // Take lower 7 bits for this byte
            var currentByte = (remaining & 0x7f).toByte
            remaining >>>= 7

            // Set continuation bit if more bytes follow
            if remaining != 0 then currentByte = (currentByte | 0x80).toByte

            buffer += currentByte
            remaining != 0
        } do ()

        buffer.toArray
    }

    /** Decode variable-length integer as specified in CIP-19 Handles continuation bit protocol to
      * reconstruct original value
      *
      * @param bytes
      *   Source byte array
      * @param startIndex
      *   Index to start decoding from
      * @return
      *   Tuple of (decoded value, number of bytes consumed)
      */
    def decodeVariableLengthUInt(
        bytes: Array[Byte],
        startIndex: Int
    ): (Long, Int) = {
        require(startIndex < bytes.length, "Start index out of bounds")

        val firstByte = bytes(startIndex)
        val hasMoreBytes = (firstByte & 0x80) != 0

        if !hasMoreBytes then {
            // Simple case: single byte with MSB=0
            (firstByte & 0x7f, 1)
        } else {
            // Complex case: multiple bytes with continuation
            val valueInThisByte = firstByte & 0x7f
            val (recursiveValue, bytesUsed) = decodeVariableLengthUInt(bytes, startIndex + 1)
            // Shift previous value and combine with current byte
            ((recursiveValue << 7) | valueInThisByte, bytesUsed + 1)
        }
    }
}

import scalus.cardano.address.VarUInt.*

/** Network identification for Cardano addresses */
sealed trait Network {

    /** Check if this is mainnet */
    def isMainnet: Boolean = this == Network.Mainnet

    @deprecated("Use networkId instead", "0.13.0")
    def value: Byte = networkId

    /** Get the numeric value for this network */
    def networkId: Byte
}

object Network {
    case object Testnet extends Network {
        override def toString: String = "Testnet"
        def networkId: Byte = 0
    }

    case object Mainnet extends Network {
        override def toString: String = "Mainnet"
        def networkId: Byte = 1
    }

    case class Other(v: Byte) extends Network {
        require(v >= 2 && v <= 15, s"Invalid network byte: $v, must be in range 2-15")

        override def toString: String = s"Other($v)"
        def networkId: Byte = v
    }

    @deprecated("Use fromNetworkId instead", "0.13.0")
    def fromByte(value: Byte): Network = fromNetworkId(value)

    /** Creates Network from network id byte value */
    def fromNetworkId(value: Byte): Network = value match
        case 0x00 => Testnet
        case 0x01 => Mainnet
        case v    => Other(v)

    given Ordering[Network] with
        def compare(x: Network, y: Network): Int = x.networkId - y.networkId

}

/** Type aliases for clarity - matching Rust implementation */
type TxIdx = Long
type CertIdx = Long

/** Represents a pointer to a stake registration certificate on the blockchain Used in pointer
  * addresses to reference stake credentials indirectly
  *
  * @param slot
  *   Transaction slot number where the stake registration occurred
  * @param txIdx
  *   Transaction index within the slot
  * @param certIdx
  *   Certificate index within the transaction
  */
case class Pointer(slot: Slot, txIdx: TxIdx, certIdx: CertIdx) {

    /** Serialize pointer to variable-length encoded bytes following CIP-19 specification */
    def toBytes: Array[Byte] =
        VarUInt.encodeVariableLengthUInt(slot.slot) ++
            VarUInt.encodeVariableLengthUInt(txIdx) ++
            VarUInt.encodeVariableLengthUInt(certIdx)

    /** Convert to hex string for debugging */
    def toHex: String = toBytes.map("%02x".format(_)).mkString
}

object Pointer {

    /** Parse pointer from bytes starting at given index
      *
      * @param bytes
      *   Source byte array
      * @param startIndex
      *   Index to start parsing from
      * @return
      *   Tuple of (parsed pointer, bytes consumed)
      */
    def parseFrom(bytes: Array[Byte], startIndex: Int): Try[(Pointer, Int)] = Try {
        val (slot, used1) = decodeVariableLengthUInt(bytes, startIndex)
        val (txIdx, used2) = decodeVariableLengthUInt(bytes, startIndex + used1)
        val (certIdx, used3) = decodeVariableLengthUInt(bytes, startIndex + used1 + used2)

        (Pointer(Slot(slot), txIdx, certIdx), used1 + used2 + used3)
    }

    /** Parse pointer from complete byte array */
    def fromBytes(bytes: Array[Byte]): Try[Pointer] =
        parseFrom(bytes, 0).map(_._1)
}

/** The payment part of a Shelley address - can be either a key hash or script hash */
enum ShelleyPaymentPart {
    case Key(hash: AddrKeyHash)
    case Script(hash: ScriptHash)

    /** Get the underlying hash regardless of type */
    def asHash: Hash28 = this match
        case Key(h)    => h
        case Script(h) => h

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case Key(_)    => false

    /** Convert to bytes */
    def toBytes: ByteString = asHash

    /** Convert to hex string */
    def toHex: String = asHash.toString // Assuming Hash28 has proper toString
}

object ShelleyPaymentPart {

    /** Create from key hash */
    @deprecated("Use ShelleyPaymentPart.Key(hash) instead", "0.14.2")
    def keyHash(hash: AddrKeyHash): ShelleyPaymentPart = Key(hash)

    /** Create from script hash */
    @deprecated("Use ShelleyPaymentPart.Script(hash) instead", "0.14.2")
    def scriptHash(hash: ScriptHash): ShelleyPaymentPart = Script(hash)
}

/** The delegation part of a Shelley address - various ways to specify stake credentials */
enum ShelleyDelegationPart {
    case Key(hash: StakeKeyHash)
    case Script(hash: ScriptHash)
    case Pointer(pointer: scalus.cardano.address.Pointer)
    case Null // Enterprise addresses have no delegation part

    /** Get hash if this delegation part contains one */
    def asHash: Option[Hash28] = this match
        case Key(h)                                  => Some(h)
        case Script(h)                               => Some(h)
        case ShelleyDelegationPart.Pointer(_) | Null => None

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case _         => false

    /** Convert to bytes */
    def toBytes: ByteString = this match
        case Key(h)                           => h
        case Script(h)                        => h
        case ShelleyDelegationPart.Pointer(p) => ByteString.fromArray(p.toBytes)
        case Null                             => ByteString.empty

    /** Convert to hex string */
    def toHex: String = toBytes.toString
}

object ShelleyDelegationPart {

    /** Create from key hash */
    def keyHash(hash: StakeKeyHash): ShelleyDelegationPart = Key(hash)

    /** Create from script hash */
    def scriptHash(hash: ScriptHash): ShelleyDelegationPart = Script(hash)

    /** Create from pointer bytes */
    def fromPointer(bytes: Array[Byte]): Try[ShelleyDelegationPart] =
        scalus.cardano.address.Pointer.fromBytes(bytes).map(Pointer.apply)
}

/** The payload of a Stake address - either stake key or script */
enum StakePayload {
    case Stake(hash: StakeKeyHash)
    case Script(hash: ScriptHash)

    /** Get the underlying hash */
    def asHash: Hash28 = this match
        case Stake(h)  => h
        case Script(h) => h

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case Stake(_)  => false

    /** Convert to bytes */
    def toBytes: ByteString = asHash

    /** Convert to hex string */
    def toHex: String = asHash.toHex
}

object StakePayload {

    /** Parse from bytes - assumes 28-byte hash */
    def fromBytes(bytes: Array[Byte], isScript: Boolean): Try[StakePayload] = Try {
        require(bytes.length == 28, s"Invalid hash size: ${bytes.length}, expected 28")
        val hash = ByteString.fromArray(bytes)
        if isScript then Script(Hash.scriptHash(hash)) else Stake(Hash.stakeKeyHash(hash))
    }
}

/** A decoded Shelley address containing network, payment and delegation parts */
case class ShelleyAddress(
    network: Network,
    payment: ShelleyPaymentPart,
    delegation: ShelleyDelegationPart
) extends Address {

    /** Get numeric type ID for this address following CIP-19 specification */
    def typeId: Byte = (payment, delegation) match
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Key(_))        => 0x00
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Key(_))     => 0x01
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Script(_))     => 0x02
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Script(_))  => 0x03
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Pointer(_))    => 0x04
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Pointer(_)) => 0x05
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Null)          => 0x06
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Null)       => 0x07

    /** Build header byte combining type ID and network */
    def toHeader: Byte = ((typeId << 4) | (network.networkId & 0x0f)).toByte

    /** Get human-readable prefix for bech32 encoding */
    def hrp: Try[String] = network match
        case Network.Testnet  => Success("addr_test")
        case Network.Mainnet  => Success("addr")
        case Network.Other(x) => Failure(new IllegalArgumentException(s"Unknown network: $x"))

    /** Serialize address to bytes */
    def toBytes: ByteString = {
        val header = ByteString(toHeader)
        val paymentBytes = payment.toBytes
        val delegationBytes = delegation.toBytes
        header ++ paymentBytes ++ delegationBytes
    }

    /** Convert to hex string */
    def toHex: String = toBytes.toString

    /** Encode to bech32 string */
    def toBech32: Try[String] = for {
        prefix <- hrp
        bytes = toBytes.bytes
        encoded <- Try(Bech32.encodeFrom5Bit(prefix, Bech32.to5Bit(bytes)))
    } yield encoded

    /** Check if address contains any script hashes */
    def hasScript: Boolean = payment.isScript || delegation.isScript

    /** Check if this is an enterprise address (no delegation) */
    def isEnterprise: Boolean = delegation == ShelleyDelegationPart.Null

    def encode: Try[String] = toBech32

    def keyHashOption: Option[AddrKeyHash | StakeKeyHash] = payment match
        case ShelleyPaymentPart.Key(hash) => Some(hash)
        case _                            => None

    def scriptHashOption: Option[ScriptHash] = payment match
        case ShelleyPaymentPart.Script(hash) => Some(hash)
        case _                               => None

    /** Convert Shelley address to Stake address if it has delegation */
    def toStakeAddress: Try[StakeAddress] =
        delegation match
            case ShelleyDelegationPart.Key(hash) =>
                Success(StakeAddress(network, StakePayload.Stake(hash)))
            case ShelleyDelegationPart.Script(hash) =>
                Success(StakeAddress(network, StakePayload.Script(hash)))
            case _ =>
                Failure(
                  new IllegalArgumentException(
                    "Cannot convert address without delegation to stake address"
                  )
                )

    inline override def getNetwork: Option[Network] = Some(network)
}

object ShelleyAddress {
    import Doc.*
    import Pretty.inParens

    given Encoder[ShelleyAddress] with
        def write(w: Writer, value: ShelleyAddress): Writer = {
            w.write(value.toBytes)
            w
        }

    /** Pretty prints ShelleyAddress
      *   - Concise: bech32 encoding
      *   - Detailed: structured with network, payment, delegation
      */
    given Pretty[ShelleyAddress] = Pretty.instanceWithDetailed(
      concise = (a, _) => text(a.toBech32.getOrElse(a.toHex)),
      detailed = (a, _) =>
          val paymentDoc = a.payment match
              case ShelleyPaymentPart.Key(hash) =>
                  text("KeyHash") + inParens(text(hash.toHex))
              case ShelleyPaymentPart.Script(hash) =>
                  text("ScriptHash") + inParens(text(hash.toHex))
          val delegationDoc = a.delegation match
              case ShelleyDelegationPart.Key(hash) =>
                  text("KeyHash") + inParens(text(hash.toHex))
              case ShelleyDelegationPart.Script(hash) =>
                  text("ScriptHash") + inParens(text(hash.toHex))
              case ShelleyDelegationPart.Pointer(ptr) =>
                  text("Pointer") + inParens(
                    text(s"${ptr.slot.slot}, ${ptr.txIdx}, ${ptr.certIdx}")
                  )
              case ShelleyDelegationPart.Null => text("Null")
          text("ShelleyAddress") /
              (text("network:") & text(a.network.toString)).nested(2) /
              (text("payment:") & paymentDoc).nested(2) /
              (text("delegation:") & delegationDoc).nested(2)
    )
}

/** A decoded Stake address for delegation purposes */
case class StakeAddress(network: Network, payload: StakePayload) extends Address {

    /** Get numeric type ID */
    def typeId: Byte = payload match
        case StakePayload.Stake(_)  => 0x0e
        case StakePayload.Script(_) => 0x0f

    /** Build header byte */
    def toHeader: Byte = ((typeId << 4) | (network.networkId & 0x0f)).toByte

    /** Get human-readable prefix for bech32 encoding */
    def hrp: Try[String] = network match
        case Network.Testnet  => Success("stake_test")
        case Network.Mainnet  => Success("stake")
        case Network.Other(x) => Failure(new IllegalArgumentException(s"Unknown network: $x"))

    /** Serialize to bytes */
    def toBytes: ByteString = ByteString(toHeader) ++ payload.toBytes

    /** Convert to hex string */
    def toHex: String = toBytes.toString

    /** Encode to bech32 string */
    def toBech32: Try[String] = for {
        prefix <- hrp
        bytes = toBytes.bytes
        encoded <- Try(Bech32.encodeFrom5Bit(prefix, Bech32.to5Bit(bytes)))
    } yield encoded

    def hasScript: Boolean = payload.isScript

    def isEnterprise: Boolean = false // Stake addresses are not enterprise addresses

    def encode: Try[String] = toBech32

    def keyHashOption: Option[AddrKeyHash | StakeKeyHash] = payload match
        case StakePayload.Stake(hash) => Some(hash)
        case _                        => None

    def scriptHashOption: Option[ScriptHash] = payload match
        case StakePayload.Script(hash) => Some(hash)
        case _                         => None

    /** Convert the stake address payload to a Credential.
      *
      * Note: StakeKeyHash and AddrKeyHash are both Blake2b_224 hashes of Ed25519 public keys. They
      * differ only in their phantom type parameter for type safety purposes.
      */
    def credential: Credential = payload match
        case StakePayload.Stake(hash)  => Credential.KeyHash(AddrKeyHash(hash: ByteString))
        case StakePayload.Script(hash) => Credential.ScriptHash(hash)

    inline override def getNetwork: Option[Network] = Some(network)
}

object StakeAddress {
    import Doc.*
    import Pretty.inParens

    given Encoder[StakeAddress] with
        def write(w: Writer, value: StakeAddress): Writer = {
            w.write(value.toBytes)
            w
        }

    /** Pretty prints StakeAddress
      *   - Concise: bech32 encoding
      *   - Detailed: structured with network, payload
      */
    given Pretty[StakeAddress] = Pretty.instanceWithDetailed(
      concise = (a, style) => text(a.toBech32.getOrElse(a.toHex)),
      detailed = (a, style) =>
          val payloadDoc = a.payload match
              case StakePayload.Stake(hash)  => text("Stake") + inParens(text(hash.toHex))
              case StakePayload.Script(hash) => text("Script") + inParens(text(hash.toHex))
          (text("StakeAddress") /
              (text("network:") & text(a.network.toString)).nested(2) /
              (text("payload:") & payloadDoc).nested(2)).grouped
    )
}

/** Parsed Byron address components */
case class ParsedByronAddress(
    addrRoot: ByteString,
    addrType: Int,
    derivationPath: Option[ByteString],
    networkMagic: Option[Long],
    crc32: Long,
    computedCrc32: Long
) {

    /** Check if the CRC32 checksum is valid */
    def isValid: Boolean = crc32 == computedCrc32
}

object ParsedByronAddress {

    /** CRC32 lookup table for polynomial 0xEDB88320 (IEEE 802.3 / ISO 3309) */
    private val crc32Table: Array[Long] = {
        val table = new Array[Long](256)
        for i <- 0 until 256 do {
            var crc = i.toLong
            for _ <- 0 until 8 do {
                crc =
                    if (crc & 1) != 0 then 0xedb88320L ^ (crc >>> 1)
                    else crc >>> 1
            }
            table(i) = crc
        }
        table
    }

    /** Parse Byron address CBOR structure using direct byte manipulation.
      *
      * Byron address structure:
      *   - Outer: `[tag24(payload_bytes), crc32_checksum]`
      *   - Inner payload: `[addrRoot (28 bytes), addrAttributes (map), addrType (uint)]`
      *
      * This implementation avoids borer DOM parsing which has issues with some Byron addresses.
      *
      * @param addressBytes
      *   The raw CBOR bytes of the Byron address
      * @return
      *   Parsed Byron address components
      */
    private[address] def parse(addressBytes: ByteString): ParsedByronAddress = {
        val bytes = addressBytes.bytes
        try {
            var pos = 0

            // Parse outer array header - must be array of 2
            val outerHeader = bytes(pos) & 0xff
            if (outerHeader >> 5) != 4 then
                throw new IllegalArgumentException(
                  s"Expected array, got major type ${outerHeader >> 5}"
                )
            val arrayLen = outerHeader & 0x1f
            if arrayLen != 2 then
                throw new IllegalArgumentException(
                  s"Expected array of 2, got array of $arrayLen"
                )
            pos += 1

            // Parse element 0: tag24(bytestring)
            val tagHeader = bytes(pos) & 0xff
            if (tagHeader >> 5) != 6 then
                throw new IllegalArgumentException(
                  s"Expected tag, got major type ${tagHeader >> 5}"
                )
            val tagAdditional = tagHeader & 0x1f
            val (tagValue, tagHeaderLen) =
                if tagAdditional < 24 then (tagAdditional, 1)
                else if tagAdditional == 24 then ((bytes(pos + 1) & 0xff), 2)
                else
                    throw new IllegalArgumentException(
                      s"Unsupported tag encoding: $tagAdditional"
                    )

            if tagValue != 24 then
                throw new IllegalArgumentException(s"Expected tag24, got tag$tagValue")
            pos += tagHeaderLen

            // Parse the bytestring (payload)
            val bsHeader = bytes(pos) & 0xff
            if (bsHeader >> 5) != 2 then
                throw new IllegalArgumentException(
                  s"Expected bytestring, got major type ${bsHeader >> 5}"
                )
            val bsAdditional = bsHeader & 0x1f
            val (payloadLen, bsHeaderLen) =
                if bsAdditional < 24 then (bsAdditional, 1)
                else if bsAdditional == 24 then ((bytes(pos + 1) & 0xff), 2)
                else if bsAdditional == 25 then
                    (((bytes(pos + 1) & 0xff) << 8) | (bytes(pos + 2) & 0xff), 3)
                else
                    throw new IllegalArgumentException(
                      s"Unsupported bytestring length encoding: $bsAdditional"
                    )
            pos += bsHeaderLen

            val payloadBytes = bytes.slice(pos, pos + payloadLen)
            pos += payloadLen

            // Parse element 1: CRC32 (unsigned integer)
            val crcHeader = bytes(pos) & 0xff
            val crcMajorType = crcHeader >> 5
            if crcMajorType != 0 then
                throw new IllegalArgumentException(
                  s"Expected unsigned integer for CRC32, got major type $crcMajorType"
                )
            val crcAdditional = crcHeader & 0x1f
            val crc32: Long =
                if crcAdditional < 24 then crcAdditional.toLong
                else if crcAdditional == 24 then (bytes(pos + 1) & 0xff).toLong
                else if crcAdditional == 25 then
                    (((bytes(pos + 1) & 0xff) << 8) | (bytes(pos + 2) & 0xff)).toLong
                else if crcAdditional == 26 then
                    ((bytes(pos + 1) & 0xffL) << 24) | ((bytes(pos + 2) & 0xffL) << 16) |
                        ((bytes(pos + 3) & 0xffL) << 8) | (bytes(pos + 4) & 0xffL)
                else
                    throw new IllegalArgumentException(
                      s"Unsupported CRC32 encoding: $crcAdditional"
                    )

            // Compute CRC32 of payload for validation
            val computedCrc32 = computeCrc32(payloadBytes)

            // Parse inner payload: [addrRoot, addrAttributes, addrType]
            val (addrRoot, addrType, derivationPath, networkMagic) =
                parseInnerPayload(payloadBytes)

            ParsedByronAddress(
              addrRoot,
              addrType,
              derivationPath,
              networkMagic,
              crc32,
              computedCrc32
            )
        } catch {
            case e: IllegalArgumentException => throw e
            case e: ArrayIndexOutOfBoundsException =>
                throw new IllegalArgumentException("Byron address too short", e)
            case e: Exception =>
                throw new IllegalArgumentException(
                  s"Failed to parse Byron address: ${e.getMessage}",
                  e
                )
        }
    }

    /** Parse inner payload using direct byte manipulation.
      *
      * @return
      *   Tuple of (addrRoot, addrType, derivationPath, networkMagic)
      */
    private def parseInnerPayload(
        payload: Array[Byte]
    ): (ByteString, Int, Option[ByteString], Option[Long]) = {
        var pos = 0

        // Parse array header - must be array of 3
        val arrayHeader = payload(pos) & 0xff
        if (arrayHeader >> 5) != 4 then
            throw new IllegalArgumentException(
              s"Inner payload: expected array, got major type ${arrayHeader >> 5}"
            )
        val arrayLen = arrayHeader & 0x1f
        if arrayLen < 3 then
            throw new IllegalArgumentException(
              s"Inner payload: expected array of at least 3, got $arrayLen"
            )
        pos += 1

        // Parse addrRoot (28-byte bytestring)
        val rootHeader = payload(pos) & 0xff
        if (rootHeader >> 5) != 2 then
            throw new IllegalArgumentException(
              s"addrRoot: expected bytestring, got major type ${rootHeader >> 5}"
            )
        val rootAdditional = rootHeader & 0x1f
        val (rootLen, rootHeaderLen) =
            if rootAdditional < 24 then (rootAdditional, 1)
            else if rootAdditional == 24 then ((payload(pos + 1) & 0xff), 2)
            else
                throw new IllegalArgumentException(
                  s"addrRoot: unsupported length encoding $rootAdditional"
                )
        pos += rootHeaderLen

        if rootLen != 28 then
            throw new IllegalArgumentException(
              s"addrRoot: expected 28 bytes, got $rootLen"
            )
        val addrRoot = ByteString.unsafeFromArray(payload.slice(pos, pos + 28))
        pos += 28

        // Parse addrAttributes (map)
        val (derivationPath, networkMagic, attrBytesConsumed) =
            parseAttributesDirect(payload, pos)
        pos += attrBytesConsumed

        // Parse addrType (unsigned integer)
        val typeHeader = payload(pos) & 0xff
        if (typeHeader >> 5) != 0 then
            throw new IllegalArgumentException(
              s"addrType: expected unsigned integer, got major type ${typeHeader >> 5}"
            )
        val addrType = typeHeader & 0x1f

        (addrRoot, addrType, derivationPath, networkMagic)
    }

    /** Parse address attributes map directly from bytes.
      *
      * @return
      *   Tuple of (derivationPath, networkMagic, bytesConsumed)
      */
    private def parseAttributesDirect(
        data: Array[Byte],
        startPos: Int
    ): (Option[ByteString], Option[Long], Int) = {
        var pos = startPos
        var derivationPath: Option[ByteString] = None
        var networkMagic: Option[Long] = None

        val mapHeader = data(pos) & 0xff
        val majorType = mapHeader >> 5

        if majorType != 5 then
            throw new IllegalArgumentException(
              s"addrAttributes: expected map, got major type $majorType"
            )

        val mapLen = mapHeader & 0x1f
        pos += 1

        // Parse map entries
        var i = 0
        while i < mapLen do {
            // Parse key (unsigned integer)
            val keyHeader = data(pos) & 0xff
            if (keyHeader >> 5) != 0 then
                throw new IllegalArgumentException("Map key must be unsigned integer")
            val key = keyHeader & 0x1f
            pos += 1

            // Parse value (bytestring)
            val valHeader = data(pos) & 0xff
            if (valHeader >> 5) != 2 then
                throw new IllegalArgumentException("Map value must be bytestring")
            val valAdditional = valHeader & 0x1f
            val (valLen, valHeaderLen) =
                if valAdditional < 24 then (valAdditional, 1)
                else if valAdditional == 24 then ((data(pos + 1) & 0xff), 2)
                else if valAdditional == 25 then
                    (((data(pos + 1) & 0xff) << 8) | (data(pos + 2) & 0xff), 3)
                else
                    throw new IllegalArgumentException(
                      s"Unsupported value length encoding: $valAdditional"
                    )
            pos += valHeaderLen

            val valueBytes = data.slice(pos, pos + valLen)
            pos += valLen

            key match {
                case 1 => // Derivation path (HD address payload)
                    derivationPath = Some(ByteString.unsafeFromArray(valueBytes))
                case 2 => // Network magic (CBOR-encoded Word32)
                    // The network magic is CBOR-encoded inside the bytestring
                    networkMagic = parseNetworkMagic(valueBytes)
                case _ => // Unknown attribute, skip
            }
            i += 1
        }

        (derivationPath, networkMagic, pos - startPos)
    }

    /** Parse network magic from CBOR-encoded bytes. */
    private def parseNetworkMagic(cborBytes: Array[Byte]): Option[Long] = {
        if cborBytes.isEmpty then return None
        try {
            val header = cborBytes(0) & 0xff
            val majorType = header >> 5
            if majorType != 0 then return None // Not an unsigned integer

            val additional = header & 0x1f
            val value: Long =
                if additional < 24 then additional.toLong
                else if additional == 24 then (cborBytes(1) & 0xff).toLong
                else if additional == 25 then
                    (((cborBytes(1) & 0xff) << 8) | (cborBytes(2) & 0xff)).toLong
                else if additional == 26 then
                    ((cborBytes(1) & 0xffL) << 24) | ((cborBytes(2) & 0xffL) << 16) |
                        ((cborBytes(3) & 0xffL) << 8) | (cborBytes(4) & 0xffL)
                else return None

            Some(value)
        } catch {
            case _: Exception => None
        }
    }

    /** Compute CRC32 checksum of byte array (cross-platform implementation) */
    private[address] def computeCrc32(data: Array[Byte]): Long = {
        var crc = 0xffffffffL
        for b <- data do {
            crc = crc32Table(((crc ^ (b & 0xff)) & 0xff).toInt) ^ (crc >>> 8)
        }
        crc ^ 0xffffffffL
    }
}

/** Byron address - legacy Cardano address format using Base58 encoding.
  *
  * Byron addresses have the following CBOR structure:
  *   - Outer: `[tag24(payload_bytes), crc32_checksum]`
  *   - Inner payload: `[addrRoot (28 bytes), addrAttributes (map), addrType (uint)]`
  *
  * Address attributes (map):
  *   - Key 1: Optional derivation path (HD address payload)
  *   - Key 2: Optional network magic (CBOR-encoded Word32)
  *
  * Address types:
  *   - 0: Verification key address (ATVerKey)
  *   - 2: Redeem address (ATRedeem)
  */
case class ByronAddress(bytes: ByteString) extends Address {
    def typeId: Byte = 0x08
    def toBytes: ByteString = bytes
    def toHex: String = bytes.toString

    /** Lazily parsed Byron address structure */
    @transient lazy val parsed: ParsedByronAddress = ParsedByronAddress.parse(bytes)

    /** Encode to Base58 string (Byron addresses use Base58, not Bech32) */
    def toBase58: String = Base58.encode(bytes.bytes)

    /** HRP is not applicable for Byron addresses (they use Base58) */
    def hrp: Try[String] = Failure(
      new UnsupportedOperationException("Byron addresses don't use bech32")
    )

    /** Get network from address attributes.
      *
      * Byron addresses encode network magic in attribute key 2. If absent or equals mainnet magic
      * (764824073), returns Mainnet. Otherwise returns Testnet.
      */
    inline override def getNetwork: Option[Network] = parsed.networkMagic match {
        case None             => Some(Network.Mainnet) // No magic = mainnet
        case Some(764824073L) => Some(Network.Mainnet) // Explicit mainnet magic
        case Some(_)          => Some(Network.Testnet) // Any other magic = testnet
    }

    def hasScript: Boolean = false // Byron addresses don't have scripts
    def isEnterprise: Boolean = false // Byron addresses are not enterprise addresses

    /** Encode to human-readable format (Base58 for Byron addresses) */
    def encode: Try[String] = Success(toBase58)

    /** Get the address type (0 = VerKey, 2 = Redeem) */
    def byronAddrType: Int = parsed.addrType

    /** Get the derivation path if present (HD wallet addresses) */
    def derivationPath: Option[ByteString] = parsed.derivationPath

    /** Get the network magic if present */
    def networkMagic: Option[Long] = parsed.networkMagic

    /** Extract payment key hash from Byron address.
      *
      * Byron addresses contain an `addrRoot` which is the payment key hash. This matches Haskell's
      * bootstrapKeyHash.
      */
    def keyHashOption: Option[AddrKeyHash | StakeKeyHash] = {
        Some(AddrKeyHash(parsed.addrRoot))
    }

    def scriptHashOption: Option[ScriptHash] =
        None // Byron addresses don't have script hashes

    /** Calculate the size of Byron address attributes.
      *
      * This matches Haskell's bootstrapAddressAttrsSize which calculates: derivationPathLength +
      * unknownAttributesLength
      *
      * Known attributes are:
      *   - Key 1: Derivation path (HD address payload)
      *   - Key 2: Network magic
      *
      * Only derivation path (key 1) and unknown attributes (keys other than 1 and 2) are counted in
      * the size. Network magic (key 2) is NOT counted.
      *
      * @return
      *   The total size of relevant attributes in bytes
      */
    def attributesSize: Int = {
        // Use the already-parsed derivation path from ParsedByronAddress
        // The derivation path size is the primary component of attributesSize
        // Network magic (key 2) is NOT counted per Haskell's bootstrapAddressAttrsSize
        parsed.derivationPath.map(_.size).getOrElse(0)
    }
}

object ByronAddress {
    import Doc.*
    import Pretty.inParens

    given Encoder[ByronAddress] with
        def write(w: Writer, value: ByronAddress): Writer = {
            w.write(value.toBytes)
            w
        }

    /** Pretty prints ByronAddress as Base58 */
    given Pretty[ByronAddress] with
        def pretty(a: ByronAddress, style: Style): Doc =
            text("ByronAddress") + inParens(text(a.toBase58))

    /** Parse a Byron address from a Base58-encoded string.
      *
      * @param base58
      *   The Base58 encoded Byron address
      * @return
      *   Success with ByronAddress if valid, Failure otherwise
      */
    def fromBase58(base58: String): Try[ByronAddress] = Try {
        val decoded = Base58.decode(base58)
        val addr = ByronAddress(ByteString.fromArray(decoded))
        if !addr.parsed.isValid then {
            throw new IllegalArgumentException(
              s"Invalid Byron address CRC32 checksum: expected ${addr.parsed.crc32}, got ${addr.parsed.computedCrc32}"
            )
        }
        addr
    }

    /** Create a Byron address with proper CBOR structure.
      *
      * This is useful for testing - it creates a valid Byron address with CRC32 checksum.
      *
      * @param addrRoot
      *   The 28-byte address root (payment key hash)
      * @param addrType
      *   Address type: 0 for VerKey, 2 for Redeem
      * @param networkMagic
      *   Optional network magic (None for mainnet)
      * @return
      *   A valid ByronAddress
      */
    def create(
        addrRoot: ByteString,
        addrType: Int = 0,
        networkMagic: Option[Long] = None
    ): ByronAddress = {
        import io.bullet.borer.{Cbor, Encoder, Writer}

        require(addrRoot.size == 28, s"addrRoot must be 28 bytes, got ${addrRoot.size}")
        require(addrType == 0 || addrType == 2, s"addrType must be 0 or 2, got $addrType")

        // Build attributes map
        val attributes: Map[Int, Array[Byte]] = networkMagic match {
            case Some(magic) =>
                // Network magic is CBOR-encoded inside the bytestring
                val magicBytes = Cbor.encode(magic).toByteArray
                Map(2 -> magicBytes)
            case None => Map.empty
        }

        // Build inner payload: [addrRoot, addrAttributes, addrType]
        val payloadBytes = Cbor
            .encode(
              (
                addrRoot.bytes,
                attributes,
                addrType
              )
            )
            .toByteArray

        // Compute CRC32 of payload
        val crc32 = ParsedByronAddress.computeCrc32(payloadBytes)

        // Build outer structure: [tag24(payload), crc32]
        // tag24 (0xd818) wraps a bytestring containing CBOR
        // Use custom encoder to write tag24 + bytestring
        given Encoder[(Array[Byte], Long)] with
            def write(w: Writer, value: (Array[Byte], Long)): Writer = {
                w.writeArrayOpen(2)
                // Write tag24 (Embedded CBOR) followed by bytestring
                w.writeTag(io.bullet.borer.Tag.EmbeddedCBOR)
                w.writeBytes(value._1)
                w.writeLong(value._2)
                w.writeArrayClose()
                w
            }

        val outerBytes = Cbor.encode((payloadBytes, crc32)).toByteArray

        ByronAddress(ByteString.fromArray(outerBytes))
    }
}

/** Base trait for all Cardano addresses
  *
  * Provides common functionality and properties shared across different address types
  */
sealed trait Address {

    /** Get type ID */
    def typeId: Byte

    /** Get human-readable prefix if available */
    def hrp: Try[String]

    /** Check if address contains scripts */
    def hasScript: Boolean

    /** Check if this is an enterprise address */
    def isEnterprise: Boolean

    /** Serialize to bytes */
    def toBytes: ByteString

    /** Convert to hex string */
    def toHex: String

    /** Encode to appropriate string format */
    def encode: Try[String]

    /** Get key hash if available */
    def keyHashOption: Option[AddrKeyHash | StakeKeyHash]

    /** Get script hash if available */
    def scriptHashOption: Option[ScriptHash]

    def getNetwork: Option[Network] = None

}

// Conversion utilities between address types
object Address {

    /** Pretty prints any Address - delegates to specific type instances */
    given Pretty[Address] = Pretty.instanceWithDetailed(
      concise = (a, style) =>
          a match
              case s: ShelleyAddress => summon[Pretty[ShelleyAddress]].pretty(s, style)
              case s: StakeAddress   => summon[Pretty[StakeAddress]].pretty(s, style)
              case b: ByronAddress   => summon[Pretty[ByronAddress]].pretty(b, style),
      detailed = (a, style) =>
          a match
              case s: ShelleyAddress => summon[Pretty[ShelleyAddress]].prettyDetailed(s, style)
              case s: StakeAddress   => summon[Pretty[StakeAddress]].prettyDetailed(s, style)
              case b: ByronAddress   => summon[Pretty[ByronAddress]].prettyDetailed(b, style)
    )

    /** Create a Shelley address from payment and stake credentials */
    def apply(network: Network, payment: Credential, delegation: Credential): Address = {
        val paymentPart = payment match
            case Credential.KeyHash(hash)    => ShelleyPaymentPart.Key(hash)
            case Credential.ScriptHash(hash) => ShelleyPaymentPart.Script(hash)

        val delegationPart = delegation match
            case Credential.KeyHash(hash) =>
                // AddrKeyHash → StakeKeyHash: both are Blake2b_224 hashes, differ only in phantom type
                ShelleyDelegationPart.Key(StakeKeyHash.fromByteString(hash))
            case Credential.ScriptHash(hash) => ShelleyDelegationPart.Script(hash)

        ShelleyAddress(network, paymentPart, delegationPart)
    }

    /** Create a Shelley-era address from a payment credential with no delegation
      *
      * This is an enterprise address.
      */
    def apply(network: Network, payment: Credential): Address = {
        val paymentPart = payment match
            case Credential.KeyHash(hash)    => ShelleyPaymentPart.Key(hash)
            case Credential.ScriptHash(hash) => ShelleyPaymentPart.Script(hash)
        ShelleyAddress(network, paymentPart, ShelleyDelegationPart.Null)
    }

    /** CBOR encoder for Address */
    given Encoder[Address] with
        def write(w: Writer, value: Address): Writer = {
            w.write(value.toBytes)
            w
        }

    /** CBOR decoder for Address */
    given Decoder[Address] with
        def read(r: Reader): Address = {
            val addressBytes = r.read[AddressBytes]()
            try Address.fromByteString(addressBytes)
            catch case NonFatal(exception) => r.validationFailure(exception.getMessage)
        }

    // Internal helper functions for variable-length encoding per CIP-19

    // Address parsing and construction functions

    /** Parse address from [[scalus.builtin.ByteString]]
      *
      * @param bs
      *   Raw address bytes
      * @return
      *   Parsed address
      * @throws java.lang.IllegalArgumentException
      *   If the byte string is empty or does not match any known address format
      */
    def fromByteString(bs: ByteString): Address = fromBytes(bs.bytes)

    /** Parse address from raw bytes
      *
      * @param bytes
      *   Raw address bytes including header
      * @return
      *   Parsed address or failure with descriptive error
      */
    def fromBytes(bytes: Array[Byte]): Address = {
        require(bytes.nonEmpty, "Address bytes cannot be empty")

        val header = bytes.head
        val payload = bytes.tail

        // Extract type from upper 4 bits of header
        val addressType = (header & 0xf0) >> 4

        addressType match
            case 0x00 => parseType0(header, payload)
            case 0x01 => parseType1(header, payload)
            case 0x02 => parseType2(header, payload)
            case 0x03 => parseType3(header, payload)
            case 0x04 => parseType4(header, payload)
            case 0x05 => parseType5(header, payload)
            case 0x06 => parseType6(header, payload)
            case 0x07 => parseType7(header, payload)
            case 0x08 => parseType8(header, payload)
            case 0x0e => parseType14(header, payload)
            case 0x0f => parseType15(header, payload)
            case _ =>
                throw new IllegalArgumentException(f"Unsupported address type: 0x$addressType%02x")
    }

    /** Parse address from bech32 string */
    def fromBech32(bech32: String): Address = {
        fromBytes(Bech32.decode(bech32).data)
    }

    /** Parse address from any string format (bech32 or base58).
      *
      * Tries Bech32 first (for Shelley-era addresses), then Base58 (for Byron addresses).
      *
      * @param str
      *   The address string to parse
      * @return
      *   The parsed Address
      * @throws IllegalArgumentException
      *   if the string is not a valid address in any supported format
      */
    def fromString(str: String): Address = {
        // Try bech32 first (most common for modern addresses)
        Try(fromBech32(str))
            .orElse(ByronAddress.fromBase58(str)) // Try Base58 for Byron addresses
            .get
    }

    // Address type parsers - each handles specific CIP-19 address format

    /** Parse Type 0: Payment Key Hash + Stake Key Hash */
    private def parseType0(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-0 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Key(stakeHash)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 1: Script Hash + Stake Key Hash */
    private def parseType1(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-1 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Key(stakeHash)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 2: Payment Key Hash + Script Hash */
    private def parseType2(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-2 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Script(scriptHash)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 3: Script Hash + Script Hash */
    private def parseType3(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-3 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val scriptHash1 = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val scriptHash2 = Hash.scriptHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Script(scriptHash1)
        val delegation = ShelleyDelegationPart.Script(scriptHash2)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 4: Payment Key Hash + Pointer */
    private def parseType4(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length > 28,
          s"Invalid Type-4 address length: ${payload.length}, expected > 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val pointerBytes = payload.slice(28, payload.length)

        val pointer = Pointer
            .fromBytes(pointerBytes)
            .getOrElse(
              throw new IllegalArgumentException("Invalid pointer data")
            )

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Pointer(pointer)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 5: Script Hash + Pointer */
    private def parseType5(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length > 28,
          s"Invalid Type-5 address length: ${payload.length}, expected > 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val pointerBytes = payload.slice(28, payload.length)

        val pointer = Pointer
            .fromBytes(pointerBytes)
            .getOrElse(
              throw new IllegalArgumentException("Invalid pointer data")
            )

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Pointer(pointer)

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 6: Payment Key Hash Only (Enterprise) */
    private def parseType6(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-6 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Null

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 7: Script Hash Only (Enterprise) */
    private def parseType7(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-7 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload))

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Null

        ShelleyAddress(network, payment, delegation)
    }

    /** Parse Type 8: Byron Address (Legacy) */
    private def parseType8(header: Byte, payload: Array[Byte]): Address = {
        // Byron addresses have complex CBOR structure - simplified here
        val fullBytes = header +: payload
        ByronAddress(ByteString.fromArray(fullBytes))
    }

    /** Parse Type 14: Stake Key Hash */
    private def parseType14(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-14 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload))

        val stakePayload = StakePayload.Stake(stakeHash)
        StakeAddress(network, stakePayload)
    }

    /** Parse Type 15: Stake Script Hash */
    private def parseType15(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-15 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromNetworkId((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload))

        val stakePayload = StakePayload.Script(scriptHash)
        StakeAddress(network, stakePayload)
    }

    /** String interpolator for parsing bech32 Cardano addresses.
      *
      * @example
      *   {{{
      * import scalus.cardano.address.Address.addr
      *
      * val mainnetAddr = addr"addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer..."
      * val testnetAddr = addr"addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer..."
      * val stakeAddr = addr"stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      *   }}}
      */
    extension (sc: StringContext)
        def addr(args: Any*): Address =
            Address.fromBech32(sc.s(args*))

    /** String interpolator for parsing bech32 Cardano stake addresses.
      *
      * @example
      *   {{{
      * import scalus.cardano.address.Address.stake
      *
      * val mainnetStake = stake"stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      * val testnetStake = stake"stake_test1uqfu74w3wh4gfzu8m6e7j987h4lq9r3t7ef5gaw497uu85qsqfy27"
      *   }}}
      */
    extension (sc: StringContext)
        def stake(args: Any*): StakeAddress =
            Address.fromBech32(sc.s(args*)) match
                case sa: StakeAddress => sa
                case other =>
                    throw new IllegalArgumentException(
                      s"Expected stake address but got ${other.getClass.getSimpleName}"
                    )
}
