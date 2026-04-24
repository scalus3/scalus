package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import io.bullet.borer.Cbor
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{
    AddrKeyHash,
    AssetName,
    Coin,
    Credential,
    DataHash,
    DatumOption,
    MultiAsset,
    PolicyId,
    Script,
    ScriptHash,
    ScriptRef,
    Timelock,
    TransactionHash,
    TransactionInput,
    TransactionOutput,
    Value
}
import scalus.uplc.builtin.{ByteString, Data}

import scala.collection.mutable

/** Era-specific MemPack readers for `cardano-ledger` types that appear in the UTxO set stored in
  * a UTxO-HD V2 InMemory snapshot's `tables/tvar` file.
  *
  * Each reader mirrors a concrete Haskell `instance MemPack …`. For every reader we note the
  * source module + line of the Haskell instance we port so future readers can cross-check.
  *
  * ==Fixed-size vs default Word64 endianness==
  *
  * MemPack's default `Word64` instance writes native-endian bytes — little-endian on the target
  * platforms. `PackedBytes N` (used for blake2b hashes via `Hash`) writes big-endian Word64/32
  * groups explicitly, which happens to be identical to "the hash bytes in their natural order"
  * as handed out by `hashToBytes` — so for our purposes the 32- and 28-byte hashes on disk are
  * just raw bytes with no length prefix.
  *
  * `Addr28Extra` and `DataHash32` (used in the `AlonzoTxOut` stake-compressed variants) are
  * NOT `PackedBytes`; they're ad-hoc tuples of 4 Word64s packed with the default instance, i.e.
  * little-endian. We keep those two cases distinct from `readHash`.
  */
object MemPackReaders {

    /** Fixed-size 32-byte hash (e.g. `TransactionHash`, `DataHash`, `BlockHash`). No length prefix;
      * `PackedBytes32` writes the 32 hash bytes directly in natural order
      * (`cardano-crypto-class/.../PackedBytes/Internal.hs` — `packM` via `writeWord64BE` × 4 is
      * byte-for-byte equivalent to the underlying `ShortByteString`).
      */
    def readHash32(r: MemPack.Reader): Array[Byte] = r.readBytes(32)

    /** Fixed-size 28-byte hash (e.g. `AddrKeyHash`, `ScriptHash`, `PoolKeyHash`). Same reasoning
      * as `readHash32`; `PackedBytes28` = 3×Word64BE + 1×Word32BE = 28 raw bytes in order.
      */
    def readHash28(r: MemPack.Reader): Array[Byte] = r.readBytes(28)

    /** `TxIn` MemPack: hash (32 raw bytes, via `SafeHash` → `Hash HASH` → `PackedBytes 32`)
      * followed by `TxIx` which is `Word16` (2 bytes, native-endian little-endian).
      *
      * Source: `cardano-ledger-core/.../Ledger/TxIn.hs:80` — `TxIn <$> unpackM <*> unpackM`.
      */
    def readTxIn(r: MemPack.Reader): TransactionInput = {
        val txIdBytes = readHash32(r)
        val txIx = r.readWord16() // 0..65535
        TransactionInput(
          TransactionHash.fromArray(txIdBytes),
          txIx
        )
    }

    /** Read the raw bytes of a `CompactAddr`.
      *
      * `CompactAddr` is a `newtype UnsafeCompactAddr ShortByteString` deriving `MemPack` as a
      * newtype, so its wire format is inherited from `MemPack ShortByteString`: a `Length`
      * (VarLen Word) followed by that many raw bytes. The raw bytes are the standard on-wire
      * Cardano address encoding (CIP-19) — first byte is the address-type header, rest is the
      * payment + optional stake credential, or a Byron encoding with CBOR attributes.
      *
      * Source: `cardano-ledger-core/.../Ledger/Address.hs:415-417` — `newtype CompactAddr = ...
      * deriving newtype (…, MemPack)`.
      */
    def readCompactAddrBytes(r: MemPack.Reader): Array[Byte] = r.readLengthPrefixedBytes()

    /** Parse a `CompactAddr` MemPack payload into a Scalus [[Address]] via
      * [[Address.fromByteString]]. Fails with [[MemPack.DecodeError]] if the inner bytes don't
      * match any known address format.
      */
    def readCompactAddr(r: MemPack.Reader): Address = {
        val bytes = readCompactAddrBytes(r)
        try Address.fromByteString(ByteString.fromArray(bytes))
        catch
            case ex: IllegalArgumentException =>
                throw new MemPack.DecodeError(
                  s"CompactAddr: malformed address bytes (${bytes.length} byte payload): ${ex.getMessage}"
                )
    }

    /** Number of bytes in a policy id (blake2b-224, `ADDRHASH`). */
    private val PolicyIdSize: Int = 28

    /** Read a packed-`Coin`: a bare `VarLen Word64` of lovelace.
      *
      * Reproduces `packCompactCoinM` / `unpackCompactCoinM` in the `CompactValue` MemPack
      * instance at `mary/impl/src/Cardano/Ledger/Mary/Value.hs:443, 452`. The top-level
      * `CompactForm Coin` instance is different (it is tag-prefixed for binary compatibility
      * with `CompactValueAdaOnly`), so do NOT use this function for standalone `CompactForm
      * Coin` decoding — it's only appropriate inside `CompactValue`.
      */
    def readPackedCoinInsideCompactValue(r: MemPack.Reader): Coin = Coin(r.readVarLenWord())

    /** Read a `CompactValue`:
      *   - tag 0 = [[Value]] with coin-only, no assets — a `VarLen Word64` of lovelace follows.
      *   - tag 1 = multi-asset: `VarLen Word64` coin + `VarLen Word32` numMA +
      *     length-prefixed rep bytes. The rep is the flat five-region representation documented
      *     in `eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs` (regions A/B/C/D/E).
      *
      * Source: `mary/impl/src/Cardano/Ledger/Mary/Value.hs:424-454` (`instance MemPack
      * CompactValue`) and lines 722-778 (`from`, the decoder we mirror).
      */
    def readCompactValue(r: MemPack.Reader): Value = {
        val tag = r.readTag()
        tag match {
            case 0 =>
                Value(readPackedCoinInsideCompactValue(r), MultiAsset.empty)
            case 1 =>
                val coin = readPackedCoinInsideCompactValue(r)
                val numMA = r.readVarLenWord()
                if numMA < 0 || numMA > Int.MaxValue then
                    throw new MemPack.DecodeError(
                      s"CompactValue multi-asset: numAssets $numMA out of Int range"
                    )
                val rep = r.readLengthPrefixedBytes()
                val ma = decodeMultiAssetRep(rep, numMA.toInt)
                Value(coin, ma)
            case t =>
                MemPack.unknownTag("CompactValue", t)
        }
    }

    /** Parse the flat `ShortByteString` "rep" of a `CompactValueMultiAsset` into a Scalus
      * [[MultiAsset]]. See the byte-layout ASCII diagram in
      * `eras/mary/impl/src/Cardano/Ledger/Mary/Value.hs:485-527` — regions A (8 × n-byte
      * Word64LE amounts), B (2 × n-byte Word16LE policy-id offsets), C (2 × n-byte Word16LE
      * asset-name offsets), D (concatenated 28-byte policy ids), E (concatenated asset names).
      *
      * Asset-name length is the delta to the next distinct C offset, or `rep.length - offset`
      * if it's the last distinct offset. Duplicate offsets (same asset name used under more
      * than one policy) share a length.
      */
    private def decodeMultiAssetRep(rep: Array[Byte], numMA: Int): MultiAsset = {
        if numMA == 0 then return MultiAsset.empty

        val minSize = 12 * numMA
        if rep.length < minSize then
            throw new MemPack.DecodeError(
              s"CompactValue multi-asset rep: need at least $minSize bytes for $numMA assets, got ${rep.length}"
            )

        val readerA = MemPack.Reader(rep, 0, 8 * numMA)
        val amounts = new Array[Long](numMA)
        var i = 0
        while i < numMA do { amounts(i) = readerA.readWord64(); i += 1 }

        val readerB = MemPack.Reader(rep, 8 * numMA, 2 * numMA)
        val policyOffsets = new Array[Int](numMA)
        i = 0
        while i < numMA do { policyOffsets(i) = readerB.readWord16(); i += 1 }

        val readerC = MemPack.Reader(rep, 10 * numMA, 2 * numMA)
        val assetOffsets = new Array[Int](numMA)
        i = 0
        while i < numMA do { assetOffsets(i) = readerC.readWord16(); i += 1 }

        // Compute asset-name lengths from the sorted-distinct offset sequence.
        // (The Haskell side stores C sorted ascending; we do not assume the input order here,
        // we compute from the actual distinct values so a malformed rep that misorders C fails
        // with a decode error rather than silently producing garbage.)
        val distinctSorted = assetOffsets.distinct.sorted
        val assetLen = mutable.Map.empty[Int, Int]
        var j = 0
        while j < distinctSorted.length do {
            val off = distinctSorted(j)
            val nextOff = if j + 1 < distinctSorted.length then distinctSorted(j + 1) else rep.length
            if off < 0 || off > rep.length || nextOff < off then
                throw new MemPack.DecodeError(
                  s"CompactValue multi-asset rep: invalid asset-name offset $off (rep=${rep.length})"
                )
            assetLen(off) = nextOff - off
            j += 1
        }

        val triples = new Array[(PolicyId, AssetName, Long)](numMA)
        i = 0
        while i < numMA do {
            val pidOff = policyOffsets(i)
            val anOff = assetOffsets(i)
            if pidOff < 0 || pidOff + PolicyIdSize > rep.length then
                throw new MemPack.DecodeError(
                  s"CompactValue multi-asset rep: policyId offset $pidOff out of bounds (rep=${rep.length})"
                )
            val policyBytes = new Array[Byte](PolicyIdSize)
            System.arraycopy(rep, pidOff, policyBytes, 0, PolicyIdSize)
            val anLen = assetLen(anOff)
            val nameBytes = new Array[Byte](anLen)
            if anLen > 0 then System.arraycopy(rep, anOff, nameBytes, 0, anLen)
            val policyId: PolicyId = ScriptHash.fromArray(policyBytes)
            val assetName = AssetName(ByteString.fromArray(nameBytes))
            triples(i) = (policyId, assetName, amounts(i))
            i += 1
        }

        MultiAsset.from(triples.toIndexedSeq)
    }

    /** Read a MemPack `Credential kr`: 1 tag byte (0 = ScriptHash, 1 = KeyHash) followed by 28
      * raw hash bytes.
      *
      * Source: `libs/cardano-ledger-core/.../Ledger/Credential.hs:96-109`.
      */
    def readCredential(r: MemPack.Reader): Credential = {
        val tag = r.readTag()
        tag match {
            case 0 => Credential.ScriptHash(ScriptHash.fromArray(readHash28(r)))
            case 1 => Credential.KeyHash(AddrKeyHash.fromArray(readHash28(r)))
            case t => MemPack.unknownTag("Credential", t)
        }
    }

    /** Read an `Addr28Extra`: 32 bytes as 4 little-endian Word64s.
      *
      * Returns the reconstructed 28-byte payment-key hash plus a two-bit "payment bits" value:
      * bit 0 = `isKeyHash` (0 = ScriptHash, 1 = KeyHash), bit 1 = `isMainnet` (0 = Testnet, 1 =
      * Mainnet). This mirrors `decodeAddress28` at `eras/alonzo/impl/.../Alonzo/TxOut.hs:129-143`.
      *
      * On-disk encoding: `(a, b, c, d)` are 4 Word64s in little-endian; the 28-byte hash is
      * `big-endian(a) ++ big-endian(b) ++ big-endian(c) ++ big-endian(Word32((d shiftR 32)))`.
      * Bits 0/1 of `d` are the payment-cred / network selector.
      */
    private def readAddr28ExtraAsHashAndBits(
        r: MemPack.Reader
    ): (Array[Byte], Int) = {
        val buf = java.nio.ByteBuffer.allocate(32).order(java.nio.ByteOrder.BIG_ENDIAN)
        buf.putLong(r.readWord64())
        buf.putLong(r.readWord64())
        buf.putLong(r.readWord64())
        val w3 = r.readWord64()
        // Last 4 bytes of the hash come from the HIGH 32 bits of w3, big-endian.
        buf.putInt((w3 >>> 32).toInt)
        val hash = java.util.Arrays.copyOf(buf.array(), 28)
        val paymentBits = (w3 & 0x3L).toInt
        (hash, paymentBits)
    }

    /** Read a `DataHash32`: 32 bytes as 4 little-endian Word64s that, when each Word64 is
      * re-written as big-endian, give the canonical 32-byte hash. Used by the Alonzo
      * "stake-compressed + data hash" TxOut variant.
      *
      * Source: `eras/alonzo/impl/.../Alonzo/TxOut.hs:114-127`.
      */
    private def readDataHash32ReversedGroups(r: MemPack.Reader): Array[Byte] =
        readReversedGroupBytes(r, 4)

    /** Read `n` little-endian Word64s and return the 8n bytes as big-endian groups — i.e. the
      * per-word byte order is reversed. Used to reconstruct cardano-ledger hashes that pack
      * their blake2b bytes into Word64 tuples via the default (little-endian) `MemPack` but
      * decode via `PackedBytes` (big-endian). See `readAddr28ExtraAsHashAndBits` and
      * `readDataHash32ReversedGroups` for the two call sites.
      */
    private def readReversedGroupBytes(r: MemPack.Reader, numWords: Int): Array[Byte] = {
        val buf = java.nio.ByteBuffer.allocate(numWords * 8).order(java.nio.ByteOrder.BIG_ENDIAN)
        var i = 0
        while i < numWords do { buf.putLong(r.readWord64()); i += 1 }
        buf.array()
    }

    /** Read a `CompactForm Coin`: tag 0 + `VarLen Word64`. This is the tag-prefixed version
      * used standalone (e.g. in TxOut tags 2 / 3); DO NOT use inside `CompactValue`, which uses
      * the tag-less [[readPackedCoinInsideCompactValue]].
      *
      * Source: `libs/cardano-ledger-core/.../Ledger/Coin.hs:151-161`.
      */
    def readCompactCoinStandalone(r: MemPack.Reader): Coin = {
        val tag = r.readTag()
        if tag != 0 then MemPack.unknownTag("CompactForm Coin", tag)
        Coin(r.readVarLenWord())
    }

    /** Read a Babbage-/Conway-/Dijkstra-era `BabbageTxOut` (all three eras share the same
      * MemPack layout — Conway and Dijkstra reuse `BabbageTxOut` as their `TxOut`).
      *
      *   - Tag 0: `TxOutCompact'` — CompactAddr + CompactValue.
      *   - Tag 1: `TxOutCompactDH'` — CompactAddr + CompactValue + DataHash (32 raw bytes).
      *   - Tag 2: `TxOut_AddrHash28_AdaOnly` — Credential + Addr28Extra + CompactForm Coin.
      *   - Tag 3: `TxOut_AddrHash28_AdaOnly_DataHash32` — + DataHash32 (reversed-groups 32 bytes).
      *   - Tag 4: `TxOutCompactDatum` — CompactAddr + CompactValue + Datum (inline datum).
      *   - Tag 5: `TxOutCompactRefScript` — + Script (era-specific). **Not yet implemented** —
      *     decoding throws [[UnsupportedTxOutVariant]]. See M10b.next RefScript follow-up.
      *
      * Source: `eras/babbage/impl/.../Babbage/TxOut.hs:130-212`.
      */
    def readTxOut(r: MemPack.Reader): TransactionOutput = {
        val tag = r.readTag()
        tag match {
            case 0 =>
                val address = readCompactAddr(r)
                val value = readCompactValue(r)
                TransactionOutput.Shelley(address, value, datumHash = None)
            case 1 =>
                val address = readCompactAddr(r)
                val value = readCompactValue(r)
                val dh = DataHash.fromArray(readHash32(r))
                TransactionOutput.Shelley(address, value, datumHash = Some(dh))
            case 2 =>
                val stakeCred = readCredential(r)
                val (paymentHash, bits) = readAddr28ExtraAsHashAndBits(r)
                val coin = readCompactCoinStandalone(r)
                val address = reconstructCompactAddress(paymentHash, bits, stakeCred)
                TransactionOutput.Shelley(address, Value(coin, MultiAsset.empty), datumHash = None)
            case 3 =>
                val stakeCred = readCredential(r)
                val (paymentHash, bits) = readAddr28ExtraAsHashAndBits(r)
                val coin = readCompactCoinStandalone(r)
                val dhBytes = readDataHash32ReversedGroups(r)
                val address = reconstructCompactAddress(paymentHash, bits, stakeCred)
                TransactionOutput.Shelley(
                  address,
                  Value(coin, MultiAsset.empty),
                  datumHash = Some(DataHash.fromArray(dhBytes))
                )
            case 4 =>
                // Note: per `eras/babbage/impl/.../Babbage/TxOut.hs:138-141`,
                // TxOutCompactDatum's third field is `BinaryData era` — **not** `Datum era`.
                // BinaryData = newtype around ShortByteString, whose MemPack is
                // length-prefixed raw bytes (no 0/1/2 tag). Always an inline datum.
                val address = readCompactAddr(r)
                val value = readCompactValue(r)
                val inline = readBinaryDataAsInlineDatum(r)
                TransactionOutput.Babbage(
                  address,
                  value,
                  datumOption = Some(inline),
                  scriptRef = None
                )
            case 5 =>
                val address = readCompactAddr(r)
                val value = readCompactValue(r)
                val datum = readDatumOption(r)
                val script = readScript(r)
                TransactionOutput.Babbage(
                  address,
                  value,
                  datumOption = datum,
                  scriptRef = Some(ScriptRef(script))
                )
            case t =>
                MemPack.unknownTag("BabbageTxOut", t)
        }
    }

    /** Thrown when a TxOut MemPack variant or sub-structure we don't yet handle is hit — keeps
      * restore behaviour "fail loud" rather than silently dropping the entry from the UTxO set.
      */
    final class UnsupportedTxOutVariant(msg: String) extends MemPack.DecodeError(msg)

    /** Read an `AlonzoScript era` MemPack: tag 0 = native script, tag 1 = plutus script. Used
      * inside the tag-5 TxOut variant.
      *
      * Source: `eras/alonzo/impl/.../Alonzo/Scripts.hs:423-447`.
      */
    def readScript(r: MemPack.Reader): Script = {
        val tag = r.readTag()
        tag match {
            case 0 => Script.Native(readTimelock(r))
            case 1 => readPlutusScript(r)
            case t => MemPack.unknownTag("AlonzoScript", t)
        }
    }

    /** Read a `Timelock era` MemPack value. The MemPack instance wraps a `MemoBytes`, so the
      * on-disk encoding is `packMemoBytesM . mbBytes`, i.e. the ShortByteString MemPack —
      * `Length` (VarLen) + raw bytes. The raw bytes are the original CBOR encoding of the
      * timelock, which we hand to Scalus's Borer `Decoder[Timelock]`.
      *
      * Source: `eras/allegra/impl/.../Allegra/Scripts.hs:233-236` +
      * `libs/cardano-ledger-core/.../MemoBytes/Internal.hs:133-141`.
      */
    def readTimelock(r: MemPack.Reader): Timelock = {
        val bytes = r.readLengthPrefixedBytes()
        try Cbor.decode(bytes).to[Timelock].value
        catch
            case ex: RuntimeException =>
                throw new MemPack.DecodeError(
                  s"Timelock: failed to decode CBOR (${bytes.length} bytes): ${ex.getMessage}"
                )
    }

    /** Read a `PlutusScript era` MemPack value using **Conway-era tag conventions** (V1=0,
      * V2=1, V3=2). Tag 3 (Dijkstra's V4) is rejected with [[UnsupportedTxOutVariant]] until
      * Scalus gains a `Script.PlutusV4` case.
      *
      * Babbage-era snapshots only carry tags 0/1. Conway/Dijkstra snapshots can carry 0/1/2.
      * This reader is appropriate for any snapshot whose tip era is Conway or later; Babbage
      * snapshots also work (they simply won't emit tag 2).
      *
      * Source: `eras/babbage/impl/.../Babbage/Scripts.hs:134-147`,
      * `eras/conway/impl/.../Conway/Scripts.hs:184-200`,
      * `eras/dijkstra/impl/.../Dijkstra/Scripts.hs:328-347`.
      */
    def readPlutusScript(r: MemPack.Reader): Script = {
        val tag = r.readTag()
        val raw = r.readLengthPrefixedBytes()
        val bs = ByteString.fromArray(raw)
        tag match {
            case 0 => Script.PlutusV1(bs)
            case 1 => Script.PlutusV2(bs)
            case 2 => Script.PlutusV3(bs)
            case 3 =>
                throw new UnsupportedTxOutVariant(
                  s"PlutusScript tag 3 (Dijkstra PlutusV4) — not yet modelled in Scalus Script; " +
                      s"encountered a ${raw.length}-byte script in a ref-script TxOut."
                )
            case t => MemPack.unknownTag("PlutusScript", t)
        }
    }

    private def reconstructCompactAddress(
        paymentHashBytes: Array[Byte],
        paymentBits: Int,
        stakeCredential: Credential
    ): Address = {
        val network: Network =
            if (paymentBits & 0x2) != 0 then Network.Mainnet else Network.Testnet
        val paymentCred: Credential =
            if (paymentBits & 0x1) != 0 then
                Credential.KeyHash(AddrKeyHash.fromArray(paymentHashBytes))
            else Credential.ScriptHash(ScriptHash.fromArray(paymentHashBytes))
        Address.apply(network, paymentCred, stakeCredential)
    }

    /** Read a raw `BinaryData era` MemPack value (no tag byte — just a length-prefixed byte
      * string whose bytes are CBOR-encoded Plutus Data). Produces a [[DatumOption.Inline]].
      *
      * Used exclusively for the TxOut tag-4 variant (`TxOutCompactDatum`), which stores a
      * bare `BinaryData` rather than a `Datum` — tag 4 outputs always carry an inline datum.
      *
      * Source: `eras/babbage/impl/.../Babbage/TxOut.hs:138-141` (`TxOutCompactDatum … !BinaryData era`)
      * and `libs/cardano-ledger-core/.../Plutus/Data.hs:145-146` (`newtype BinaryData = BinaryData
      * ShortByteString deriving newtype MemPack`).
      */
    def readBinaryDataAsInlineDatum(r: MemPack.Reader): DatumOption = {
        val bytes = r.readLengthPrefixedBytes()
        val data =
            try Cbor.decode(bytes).to[Data].value
            catch
                case ex: RuntimeException =>
                    throw new MemPack.DecodeError(
                      s"BinaryData inline datum: failed to decode Plutus Data CBOR " +
                          s"(${bytes.length} bytes): ${ex.getMessage}"
                    )
        DatumOption.Inline(data)
    }

    /** Read a `Datum era` MemPack value: tag 0 = NoDatum, tag 1 = DatumHash + 32-byte hash,
      * tag 2 = Datum + BinaryData (length-prefixed CBOR-Plutus-Data bytes).
      *
      * Returns `None` for `NoDatum`. Tag 2 parses the BinaryData's raw bytes as a Plutus
      * `Data` via Scalus's Borer decoder; bad data surfaces as [[MemPack.DecodeError]].
      *
      * Source: `libs/cardano-ledger-core/.../Plutus/Data.hs:212-229`.
      */
    def readDatumOption(r: MemPack.Reader): Option[DatumOption] = {
        val tag = r.readTag()
        tag match {
            case 0 => None
            case 1 =>
                Some(DatumOption.Hash(DataHash.fromArray(readHash32(r))))
            case 2 =>
                val bytes = r.readLengthPrefixedBytes()
                val data =
                    try Cbor.decode(bytes).to[Data].value
                    catch
                        case ex: RuntimeException =>
                            throw new MemPack.DecodeError(
                              s"Datum inline: failed to decode Plutus Data CBOR (${bytes.length} bytes): ${ex.getMessage}"
                            )
                Some(DatumOption.Inline(data))
            case t =>
                MemPack.unknownTag("Datum", t)
        }
    }
}
