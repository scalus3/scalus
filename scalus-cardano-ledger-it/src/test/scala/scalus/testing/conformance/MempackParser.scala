package scalus.testing.conformance

import io.bullet.borer.Cbor
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import java.nio.ByteBuffer

/** Mempack parser for Cardano ledger types
  *
  * Based on Haskell's Data.MemPack library used by cardano-ledger.
  *
  * The mempack format uses variable-length encoding for lengths. The encoding works as follows:
  *   - If the size is 0-127: encoded as a single byte
  *   - If the size is >= 128: use variable-length encoding with continuation bit
  *
  * For ShortByteString (used for addresses), the format is:
  *   - Variable-length size (number of bytes)
  *   - The actual bytes
  */
object MempackParser {

    def parseTransactionInput(bytes: Array[Byte]): TransactionInput =
        TransactionInput(
          TransactionHash.fromArray(bytes.slice(0, 32)),
          bytes(32) | (bytes(33) << 8)
        )

    /** Parse a mempack-encoded TransactionOutput
      *
      * Supports Shelley, Alonzo, and Babbage TxOut formats based on the tag:
      *   - Tag 0: ShelleyTxOut (CompactAddr + CompactCoin)
      *   - Tag 1: AlonzoTxOut with DataHash (CompactAddr + CompactCoin + DataHash)
      *   - Tag 2: AlonzoTxOut AddrHash28 ADA-only (optimized)
      *   - Tag 3: AlonzoTxOut AddrHash28 ADA-only with DataHash32
      *   - Tag 4: BabbageTxOut with inline datum (CompactAddr + CompactValue + BinaryData)
      *   - Tag 5: BabbageTxOut with ref script (CompactAddr + CompactValue + Datum + Script)
      *
      * Based on Haskell cardano-ledger implementation.
      */
    def parseTransactionOutput(bytes: Array[Byte]): TransactionOutput = {
        var offset = 0

        // Read tag byte
        val tag = bytes(offset) & 0xff
        offset += 1

        tag match {
            case 0 => parseShelleyTxOut(bytes, offset)
            case 1 => parseAlonzoTxOutWithDataHash(bytes, offset)
            case 2 => parseAlonzoAddrHash28AdaOnly(bytes, offset)
            case 3 => parseAlonzoAddrHash28AdaOnlyWithDataHash32(bytes, offset)
            case 4 => parseBabbageTxOutWithInlineDatum(bytes, offset)
            case 5 => parseBabbageTxOutWithRefScript(bytes, offset)
            case _ => throw new IllegalArgumentException(s"Unsupported TxOut tag: $tag")
        }
    }

    /** Parse Shelley TxOut: CompactAddr + CompactCoin
      *
      * Format: CompactAddr (ShortByteString) + CompactCoin (Tag + VarLen)
      *   - CompactAddr = length byte + address bytes
      *   - CompactCoin = tag byte (0) + VarLen Word64
      *
      * Based on: instance (Era era, MemPack (CompactForm (Value era))) => MemPack (ShelleyTxOut
      * era) where packM (TxOutCompact cAddr cValue) = packTagM 0 >> packM cAddr >> packM cValue
      */
    private def parseShelleyTxOut(bytes: Array[Byte], startOffset: Int): TransactionOutput = {
        var offset = startOffset

        // Parse CompactAddr (ShortByteString with length prefix)
        // Length is encoded as a single byte for typical address lengths (29, 57 bytes)
        val addrLen = bytes(offset) & 0xff
        offset += 1

        // Read address bytes
        val addrBytes = bytes.slice(offset, offset + addrLen)
        offset += addrLen

        // Parse address
        val address = Address.fromBytes(addrBytes)

        // Parse CompactCoin: Tag(0) + VarLen Word64
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Shelley(address, value, None)
    }

    /** Parse Alonzo TxOut with DataHash (tag 1)
      *
      * Format: CompactAddr + CompactCoin + DataHash
      *   - CompactAddr = length byte + address bytes
      *   - CompactCoin = tag byte (0) + VarLen Word64
      *   - DataHash = 32 bytes (Blake2b-256 hash)
      *
      * Based on: packTagM 1 >> packM cAddr >> packM cValue >> packM dataHash
      */
    private def parseAlonzoTxOutWithDataHash(
        bytes: Array[Byte],
        startOffset: Int
    ): TransactionOutput = {
        var offset = startOffset

        // Parse CompactAddr (ShortByteString with length prefix)
        val addrLen = bytes(offset) & 0xff
        offset += 1

        // Read address bytes
        val addrBytes = bytes.slice(offset, offset + addrLen)
        offset += addrLen

        // Parse address
        val address = Address.fromBytes(addrBytes)

        // Parse CompactCoin: Tag(0) + VarLen Word64
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        // Update offset to skip the VarLen-encoded coin
        offset += varLenEncodedLength(coinBytes)

        // Parse DataHash (32 bytes)
        val dataHashBytes = bytes.slice(offset, offset + 32)
        val dataHash = DataHash.fromArray(dataHashBytes)

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Shelley(address, value, Some(dataHash))
    }

    /** Parse Alonzo TxOut_AddrHash28_AdaOnly (tag 2)
      *
      * Format: Credential + Addr28Extra + CompactCoin
      *   - Credential: staking credential (KeyHash or ScriptHash)
      *   - Addr28Extra: 32 bytes (4 x Word64) containing payment credential + network info
      *   - CompactCoin: Tag(0) + VarLen(Word64)
      */
    private def parseAlonzoAddrHash28AdaOnly(
        bytes: Array[Byte],
        startOffset: Int
    ): TransactionOutput = {
        var offset = startOffset

        // Parse staking Credential
        // Format: 1 byte discriminator + 28 bytes hash
        val stakingCred = parseCredential(bytes, offset)
        offset += 29

        // Parse Addr28Extra: 32 bytes containing packed address info
        val addr28Bytes = bytes.slice(offset, offset + 32)
        offset += 32

        // Decode Addr28Extra to extract payment credential and network
        val (network, paymentCred) = decodeAddr28Extra(addr28Bytes)

        // Parse CompactCoin: Tag(0) + VarLen encoding
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        // Construct the Address
        val address = Address.apply(network, paymentCred, stakingCred)

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Shelley(address, value, None)
    }

    /** Parse Alonzo TxOut_AddrHash28_AdaOnly_DataHash32 (tag 3)
      *
      * Format: Credential + Addr28Extra + CompactCoin + DataHash32
      *   - Credential: staking credential (KeyHash or ScriptHash)
      *   - Addr28Extra: 32 bytes (4 x Word64) containing payment credential + network info
      *   - CompactCoin: Tag(0) + VarLen(Word64)
      *   - DataHash32: 32 bytes (4 x Word64) Blake2b-256 hash
      *
      * Based on: packTagM 3 >> packM cred >> packM addr28 >> packM cCoin >> packM dataHash32
      */
    private def parseAlonzoAddrHash28AdaOnlyWithDataHash32(
        bytes: Array[Byte],
        startOffset: Int
    ): TransactionOutput = {
        var offset = startOffset

        // Parse staking Credential
        // Format: 1 byte discriminator + 28 bytes hash
        val stakingCred = parseCredential(bytes, offset)
        offset += 29

        // Parse Addr28Extra: 32 bytes containing packed address info
        val addr28Bytes = bytes.slice(offset, offset + 32)
        offset += 32

        // Decode Addr28Extra to extract payment credential and network
        val (network, paymentCred) = decodeAddr28Extra(addr28Bytes)

        // Parse CompactCoin: Tag(0) + VarLen encoding
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        // Update offset to skip the VarLen-encoded coin
        offset += varLenEncodedLength(coinBytes)

        // Parse DataHash32 (32 bytes)
        val dataHashBytes = bytes.slice(offset, offset + 32)
        val dataHash = DataHash.fromArray(dataHashBytes)

        // Construct the Address
        val address = Address.apply(network, paymentCred, stakingCred)

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Shelley(address, value, Some(dataHash))
    }

    /** Parse a Credential (staking or payment)
      *
      * Format: 1 byte discriminator + 28 bytes hash
      *   - 0x00 = KeyHash
      *   - 0x01 = ScriptHash
      */
    private def parseCredential(bytes: Array[Byte], offset: Int): Credential = {
        val discriminator = bytes(offset) & 0xff
        val hashBytes = bytes.slice(offset + 1, offset + 29)

        discriminator match {
            case 0x00 =>
                Credential.KeyHash(AddrKeyHash.fromArray(hashBytes))
            case 0x01 =>
                Credential.ScriptHash(ScriptHash.fromArray(hashBytes))
            case _ =>
                throw new IllegalArgumentException(
                  s"Invalid credential discriminator: 0x${"%02x".format(discriminator)}"
                )
        }
    }

    /** Decode Addr28Extra to extract payment credential and network
      *
      * Addr28Extra is 32 bytes (4 x Word64) encoded in LITTLE-ENDIAN (native machine byte order
      * on x86/x64) as follows:
      *   - First 3 Word64s + high 32 bits of 4th Word64 = 28-byte payment credential hash
      *   - Low 32 bits of 4th Word64 contains flags in the lowest 2 bits:
      *     - Bit 1: network (1=Mainnet, 0=Testnet)
      *     - Bit 0: credential type (1=KeyHash, 0=ScriptHash)
      *
      * IMPORTANT: The Haskell mempack library uses native byte order (little-endian on x86/x64),
      * NOT big-endian. To reconstruct the 28-byte hash:
      * 1. Read each Word64 in little-endian order
      * 2. Write them in big-endian order to get the correct hash bytes
      *
      * Based on Cardano.Ledger.Alonzo.TxOut.decodeAddress28
      */
    private def decodeAddr28Extra(
        addr28Bytes: Array[Byte]
    ): (scalus.cardano.address.Network, Credential) = {
        require(
          addr28Bytes.length == 32,
          s"Addr28Extra must be 32 bytes, got ${addr28Bytes.length}"
        )

        // mempack uses LITTLE-ENDIAN (native machine byte order on x86/x64)
        val readBuffer = ByteBuffer.wrap(addr28Bytes).order(java.nio.ByteOrder.LITTLE_ENDIAN)

        // Read 4 Word64 values in little-endian order
        val w0 = readBuffer.getLong()
        val w1 = readBuffer.getLong()
        val w2 = readBuffer.getLong()
        val w3 = readBuffer.getLong()

        // Extract flags from lowest 2 bits of w3
        val isMainnet = (w3 & 0x2) != 0
        val isKeyHash = (w3 & 0x1) != 0

        val network =
            if isMainnet then scalus.cardano.address.Network.Mainnet
            else scalus.cardano.address.Network.Testnet

        // Extract 28-byte payment credential hash
        // Write the Word64 values in big-endian to reconstruct the original hash bytes
        val paymentHashBytes = new Array[Byte](28)
        val hashBuffer = ByteBuffer.wrap(paymentHashBytes).order(java.nio.ByteOrder.BIG_ENDIAN)
        hashBuffer.putLong(w0)
        hashBuffer.putLong(w1)
        hashBuffer.putLong(w2)
        // The last 4 bytes come from the high 32 bits of w3 (after the LE read, these are in bits 32-63)
        hashBuffer.putInt((w3 >>> 32).toInt)

        // Create payment credential
        val paymentCred = if isKeyHash then {
            Credential.KeyHash(AddrKeyHash.fromArray(paymentHashBytes))
        } else {
            Credential.ScriptHash(ScriptHash.fromArray(paymentHashBytes))
        }

        (network, paymentCred)
    }

    /** Parse Babbage TxOut with inline datum (tag 4)
      *
      * Format: CompactAddr + CompactValue + BinaryData
      *   - CompactAddr = length byte + address bytes
      *   - CompactValue = CompactCoin (tag 0 + VarLen Word64) - ADA only for now
      *   - BinaryData = ShortByteString (length prefix + inline datum bytes)
      *
      * Based on: packTagM 4 >> packM cAddr >> packM cValue >> packM datum
      */
    private def parseBabbageTxOutWithInlineDatum(
        bytes: Array[Byte],
        startOffset: Int
    ): TransactionOutput = {
        var offset = startOffset

        // Parse CompactAddr (ShortByteString with length prefix)
        val addrLen = bytes(offset) & 0xff
        offset += 1

        // Read address bytes
        val addrBytes = bytes.slice(offset, offset + addrLen)
        offset += addrLen

        // Parse address
        val address = Address.fromBytes(addrBytes)

        // Parse CompactCoin: Tag(0) + VarLen Word64
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        // Update offset to skip the VarLen-encoded coin
        offset += varLenEncodedLength(coinBytes)

        // Parse BinaryData (ShortByteString with length prefix)
        val datumLen = bytes(offset) & 0xff
        offset += 1

        val datumBytes = bytes.slice(offset, offset + datumLen)
        val data = Cbor.decode(datumBytes).to[Data].value

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Babbage(
          address,
          value,
          Some(DatumOption.Inline(data)),
          None
        )
    }

    /** Parse Babbage TxOut with reference script (tag 5)
      *
      * Format: CompactAddr + CompactValue + Datum + Script
      *   - CompactAddr = length byte + address bytes
      *   - CompactValue = CompactCoin (tag 0 + VarLen Word64) - ADA only for now
      *   - Datum = Tag(0/1/2) + optional data (NoDatum/DatumHash/BinaryData)
      *   - Script = Tag(0/1) + script bytes (NativeScript/PlutusScript)
      *
      * Based on: packTagM 5 >> packM cAddr >> packM cValue >> packM datum >> packM script
      */
    private def parseBabbageTxOutWithRefScript(
        bytes: Array[Byte],
        startOffset: Int
    ): TransactionOutput = {
        var offset = startOffset

        // Parse CompactAddr (ShortByteString with length prefix)
        val addrLen = bytes(offset) & 0xff
        offset += 1

        // Read address bytes
        val addrBytes = bytes.slice(offset, offset + addrLen)
        offset += addrLen

        // Parse address
        val address = Address.fromBytes(addrBytes)

        // Parse CompactCoin: Tag(0) + VarLen Word64
        val coinTag = bytes(offset) & 0xff
        offset += 1
        require(coinTag == 0, s"Expected coin tag 0, got $coinTag")

        // VarLen Word64 parsing
        val coinBytes = bytes.slice(offset, bytes.length)
        val coin = parseVarLenWord64(coinBytes)

        // Update offset to skip the VarLen-encoded coin
        offset += varLenEncodedLength(coinBytes)

        // Parse Datum (tag + optional data)
        val datumTag = bytes(offset) & 0xff
        offset += 1

        val datumOption: Option[DatumOption] = datumTag match {
            case 0 => None // NoDatum
            case 1 =>
                // DatumHash
                val dataHashBytes = bytes.slice(offset, offset + 32)
                offset += 32
                Some(DatumOption.Hash(DataHash.fromArray(dataHashBytes)))
            case 2 =>
                // Inline BinaryData
                val datumLen = bytes(offset) & 0xff
                offset += 1
                val datumBytes = bytes.slice(offset, offset + datumLen)
                offset += datumLen
                val data = Cbor.decode(datumBytes).to[Data].value
                Some(DatumOption.Inline(data))
            case _ => throw new IllegalArgumentException(s"Unsupported Datum tag: $datumTag")
        }

        // Parse Script (tag + script bytes)
        val scriptTag = bytes(offset) & 0xff
        offset += 1

        val scriptRef: Option[ScriptRef] = scriptTag match {
            case 0 =>
                // NativeScript - ShortByteString with length prefix
                val scriptLen = bytes(offset) & 0xff
                offset += 1
                val scriptBytes = bytes.slice(offset, offset + scriptLen)
                // TODO: Parse actual native script (Timelock)
                // For now, skip the script ref parsing
                None
            case 1 =>
                // PlutusScript - ShortByteString with length prefix
                val scriptLen = bytes(offset) & 0xff
                offset += 1
                val scriptBytes = bytes.slice(offset, offset + scriptLen)
                // Assume PlutusV3 as default (latest version)
                val script = Script.PlutusV3(scalus.builtin.ByteString.fromArray(scriptBytes))
                Some(ScriptRef(script))
            case _ => throw new IllegalArgumentException(s"Unsupported Script tag: $scriptTag")
        }

        val value = Value(Coin(coin), MultiAsset.empty)
        TransactionOutput.Babbage(address, value, datumOption, scriptRef)
    }

    /** Parse variable-length Word64 encoding used by mempack
      *
      * Uses 7-bit variable-length encoding where:
      *   - Each byte uses bit 7 as a continuation bit (1 = more bytes, 0 = last byte)
      *   - Lower 7 bits of each byte contain data
      *   - Values are reconstructed in big-endian order (shift left by 7 bits for each byte)
      *
      * Based on Cardano.Ledger.Address.decode7BitVarLength
      */
    private def parseVarLenWord64(bytes: Array[Byte]): Long = {
        var offset = 0
        var acc = 0L

        while offset < bytes.length do {
            val byte = bytes(offset) & 0xff
            val hasContinuation = (byte & 0x80) != 0 // Test bit 7
            val dataBits = byte & 0x7f // Clear bit 7 to get lower 7 bits

            acc = (acc << 7) | dataBits

            offset += 1

            if !hasContinuation then {
                // This is the last byte
                return acc
            }
        }

        // If we get here, we ran out of bytes with continuation bit still set
        throw new IllegalArgumentException("VarLen encoding ended prematurely")
    }

    /** Calculate the number of bytes consumed by a VarLen-encoded value
      *
      * Scans the bytes to find where the continuation bit is not set (bit 7 = 0), indicating the
      * last byte of the encoded value.
      */
    private def varLenEncodedLength(bytes: Array[Byte]): Int = {
        var offset = 0
        while offset < bytes.length do {
            val byte = bytes(offset) & 0xff
            val hasContinuation = (byte & 0x80) != 0
            offset += 1
            if !hasContinuation then {
                return offset
            }
        }
        throw new IllegalArgumentException("VarLen encoding ended prematurely")
    }
}
