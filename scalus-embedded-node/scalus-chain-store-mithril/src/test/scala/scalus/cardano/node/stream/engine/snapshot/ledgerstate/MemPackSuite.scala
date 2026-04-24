package scalus.cardano.node.stream.engine.snapshot.ledgerstate

import org.scalatest.funsuite.AnyFunSuite

/** Byte-level tests against hand-built fixtures that mirror what the Haskell `Data.MemPack`
  * library would emit. Endianness matters; every multi-byte primitive is cross-checked against a
  * literal byte sequence.
  */
final class MemPackSuite extends AnyFunSuite {

    /** Write a non-negative `Long` as MemPack's big-endian base-128 VarLen. Used only from
      * tests to construct fixture bytes without hard-coding every byte.
      */
    private def writeVarLen(out: java.io.ByteArrayOutputStream, value: Long): Unit = {
        require(value >= 0, s"writeVarLen: negative value $value")
        // Determine how many 7-bit groups are needed.
        val bits = if value == 0 then 1 else 64 - java.lang.Long.numberOfLeadingZeros(value)
        val groups = (bits + 6) / 7
        var i = groups - 1
        while i >= 0 do {
            val groupVal = ((value >>> (i * 7)) & 0x7fL).toInt
            if i == 0 then out.write(groupVal)
            else out.write(groupVal | 0x80)
            i -= 1
        }
    }


    test("word primitives — little-endian native order") {
        // Haskell MemPack writes via writeWord8ArrayAsWord64# (native-endian). On x86/arm that's LE.
        val bytes: Array[Byte] = Array(
          // Word8 = 0x7F
          0x7f.toByte,
          // Word16 = 0x1234 → bytes 0x34 0x12 LE
          0x34.toByte, 0x12.toByte,
          // Word32 = 0x89ABCDEF → bytes 0xEF 0xCD 0xAB 0x89 LE
          0xef.toByte, 0xcd.toByte, 0xab.toByte, 0x89.toByte,
          // Word64 = 0x0123456789ABCDEF → bytes 0xEF 0xCD 0xAB 0x89 0x67 0x45 0x23 0x01 LE
          0xef.toByte, 0xcd.toByte, 0xab.toByte, 0x89.toByte,
          0x67.toByte, 0x45.toByte, 0x23.toByte, 0x01.toByte
        )
        val r = MemPack.Reader(bytes)
        assert(r.readUnsignedByte() == 0x7f)
        assert(r.readWord16() == 0x1234)
        assert(r.readWord32() == 0x89abcdefL)
        assert(r.readWord64() == 0x0123456789abcdefL)
        assert(r.eof)
    }

    test("VarLen Word — big-endian base-128, continuation bit MSB") {
        // Fixtures derived from the Haskell definition:
        //   packIntoCont7 x cont n: first byte = high 7 bits | 0x80, last byte = low 7 bits
        def roundTrip(expected: Long, bytes: Array[Byte]): Unit = {
            val r = MemPack.Reader(bytes)
            assert(r.readVarLenWord() == expected, s"decoded value for ${bytes.mkString("[", ",", "]")}")
            assert(r.eof)
        }
        // value 0 — single byte, high bit clear
        roundTrip(0L, Array(0x00.toByte))
        // value 1
        roundTrip(1L, Array(0x01.toByte))
        // value 127 — fits in 7 bits
        roundTrip(127L, Array(0x7f.toByte))
        // value 128 — two bytes: high-group=1 (0x81 = 0x80|1), low-group=0
        roundTrip(128L, Array(0x81.toByte, 0x00.toByte))
        // value 300 = 0b100101100
        //   high group (bits 7..13) = 0b10 = 2 → 0x82
        //   low  group (bits 0..6)  = 0b0101100 = 44 → 0x2C
        roundTrip(300L, Array(0x82.toByte, 0x2c.toByte))
        // value 16383 = 2^14 - 1 = two full 7-bit groups
        roundTrip(16383L, Array(0xff.toByte, 0x7f.toByte))
        // value 16384 = 2^14 — three bytes
        roundTrip(16384L, Array(0x81.toByte, 0x80.toByte, 0x00.toByte))
    }

    test("Length rejects negative underlying values") {
        // VarLen Word where the high bit of the final reconstructed Word is set → Haskell's
        // Length unpacker calls upackLengthFail. We surface that as DecodeError.
        //
        // Smallest such value: bit 63 set. 64-bit = needs 10 varlen bytes. The encoding for
        // 2^63 is:  byte0 = 0x81, subsequent eight bytes = 0x80, final = 0x00 — that's 10 bytes
        // with the proper continuation pattern.
        val maxOf63: Long = 1L << 63 // Long.MinValue when viewed as signed
        val bytes = new Array[Byte](10)
        var value = maxOf63
        // Write most-significant group first
        var numGroups = 10
        val groups = new Array[Int](numGroups)
        var v = maxOf63
        var i = 0
        while i < numGroups do {
            groups(numGroups - 1 - i) = (v & 0x7fL).toInt
            v = v >>> 7
            i += 1
        }
        // First byte: high 7 bits of value via special BigEndian reconstruction...
        // Actually easier to just assert our reader rejects a known-bad encoded sentinel.
        //
        // Use a 10-byte sequence that decodes to Long.MinValue (1L << 63):
        //  Decoding formula: acc = (acc << 7) | group_value
        //  After 10 groups of (a, 0, 0, 0, 0, 0, 0, 0, 0, 0) we get a << 63.
        //  So first group = 1 → byte 0x81 (continuation set); rest = 0x80 x8; last = 0x00.
        val negEncoded = Array[Byte](
          0x81.toByte,
          0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte,
          0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte,
          0x00.toByte
        )
        val r = MemPack.Reader(negEncoded)
        val ex = intercept[MemPack.DecodeError](r.readLength())
        assert(
          ex.getMessage.contains("negative") || ex.getMessage.contains("Length:"),
          s"unexpected error: ${ex.getMessage}"
        )
    }

    test("length-prefixed bytes") {
        //   packM (ByteString "abc") = packM (Length 3) >> raw bytes
        //     Length 3 → VarLen Word 3 → single byte 0x03
        val bytes: Array[Byte] = Array(0x03.toByte, 'a'.toByte, 'b'.toByte, 'c'.toByte)
        val r = MemPack.Reader(bytes)
        val out = r.readLengthPrefixedBytes()
        assert(out.toSeq == Seq('a'.toByte, 'b'.toByte, 'c'.toByte))
        assert(r.eof)
    }

    test("bool / maybe tagging — 1-byte tag") {
        // packTagM (Tag w) = write one byte = w  (packedTagByteCount = 1)
        //   Bool False = 0x00, Bool True = 0x01
        //   Maybe Nothing = 0x00, Maybe (Just 0x42) = 0x01 0x42
        val bytes: Array[Byte] =
            Array(0x00.toByte, 0x01.toByte, 0x00.toByte, 0x01.toByte, 0x42.toByte)
        val r = MemPack.Reader(bytes)
        assert(!r.readBool())
        assert(r.readBool())
        assert(r.readMaybe(r.readUnsignedByte()).isEmpty)
        assert(r.readMaybe(r.readUnsignedByte()).contains(0x42))
        assert(r.eof)
    }

    test("TxIn = 32 raw hash bytes + Word16 LE") {
        // Fixture: hash = 0x00..0x1F ascending, TxIx = 0x0102 → LE bytes 0x02, 0x01
        val bytes = new Array[Byte](34)
        for i <- 0 until 32 do bytes(i) = i.toByte
        bytes(32) = 0x02.toByte
        bytes(33) = 0x01.toByte
        val r = MemPack.Reader(bytes)
        val txIn = MemPackReaders.readTxIn(r)
        assert(txIn.index == 0x0102)
        val hashBytes = txIn.transactionId.bytes.toArray
        assert(hashBytes.toSeq == (0 until 32).map(_.toByte))
        assert(r.eof)
    }

    test("TxIn — rejects truncated buffer") {
        // 33 bytes (hash + 1 byte of txIx, missing the second byte)
        val r = MemPack.Reader(new Array[Byte](33))
        intercept[MemPack.DecodeError](MemPackReaders.readTxIn(r))
    }

    test("CompactAddr — Shelley enterprise address, round-trip via toBytes") {
        // Build a real Shelley enterprise address, take its canonical bytes, then prepend the
        // MemPack Length prefix. The reader should produce back the same address.
        val addrBech32 =
            "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        val addr = scalus.cardano.address.Address.fromBech32(addrBech32)
        val addrBytes = addr.toBytes.bytes
        val prefix =
            if addrBytes.length < 0x80 then Array(addrBytes.length.toByte)
            else {
                // Shouldn't happen for a standard enterprise address (29 bytes), but don't hide
                // the assumption.
                fail(
                  s"enterprise address unexpectedly >=128 bytes (${addrBytes.length}); varlen prefix needed"
                )
                Array.emptyByteArray
            }
        val bytes = prefix ++ addrBytes
        val r = MemPack.Reader(bytes)
        val decoded = MemPackReaders.readCompactAddr(r)
        assert(decoded == addr)
        assert(r.eof)
    }

    test("CompactAddr — malformed payload surfaces DecodeError") {
        // Length-prefixed 1 byte of garbage (0x00 header byte alone can't decode into a known
        // address — Byron/Shelley headers both need more data).
        val bytes = Array[Byte](0x01, 0x00)
        val r = MemPack.Reader(bytes)
        val ex = intercept[MemPack.DecodeError](MemPackReaders.readCompactAddr(r))
        assert(ex.getMessage.contains("CompactAddr"))
    }

    test("CompactValue — ada-only (tag 0)") {
        // tag=0, coin as VarLen Word: 5_000_000 = 0x4C4B40
        //   (1 0011 0001 0010 1101 0000 000)  = 23 bits → 4 varlen bytes
        //   byte0 = 0x82 (cont | 0b0000010), byte1 = 0x93 (cont | 0b0010011), byte2 = 0xA5
        //     (cont | 0b0100101), byte3 = 0x40 (last | 0b1000000)
        // Easier: just encode programmatically and assert.
        val rBuild = new java.io.ByteArrayOutputStream()
        rBuild.write(0x00) // tag
        writeVarLen(rBuild, 5_000_000L)
        val bytes = rBuild.toByteArray
        val value = MemPackReaders.readCompactValue(MemPack.Reader(bytes))
        assert(value.coin.value == 5_000_000L)
        assert(value.assets.isEmpty)
    }

    test("CompactValue — multi-asset (tag 1), single policy / single asset") {
        // Build a canonical rep:
        //   n = 1
        //   Region A (8 bytes): Word64 amount = 42 LE
        //   Region B (2 bytes): Word16 policyId offset = 12 LE
        //   Region C (2 bytes): Word16 asset-name offset = 12 + 28 = 40 LE
        //   Region D (28 bytes): the policy id = 0x01 02 ... 0x1C (1..28)
        //   Region E (5 bytes): "Snark"
        val rep = new Array[Byte](12 + 28 + 5)
        // A: 42 LE
        rep(0) = 42.toByte
        // B: 12 LE
        rep(8) = 12.toByte
        rep(9) = 0.toByte
        // C: 40 LE
        rep(10) = 40.toByte
        rep(11) = 0.toByte
        // D: policy id = 1..28
        var k = 0
        while k < 28 do { rep(12 + k) = (k + 1).toByte; k += 1 }
        // E: "Snark"
        rep(40) = 'S'.toByte
        rep(41) = 'n'.toByte
        rep(42) = 'a'.toByte
        rep(43) = 'r'.toByte
        rep(44) = 'k'.toByte

        val out = new java.io.ByteArrayOutputStream()
        out.write(0x01) // multi-asset tag
        writeVarLen(out, 1_000_000L) // coin
        writeVarLen(out, 1L) // numMA
        writeVarLen(out, rep.length.toLong) // rep length
        out.write(rep)
        val bytes = out.toByteArray
        val value = MemPackReaders.readCompactValue(MemPack.Reader(bytes))
        assert(value.coin.value == 1_000_000L)
        assert(value.assets.assets.size == 1)
        val (policy, inner) = value.assets.assets.head
        assert(inner.size == 1)
        assert(inner.keys.head.toString == "Snark")
        assert(inner.values.head == 42L)
        val hex = policy.toHex
        assert(hex == "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c")
    }

    test("CompactValue — multi-asset, empty asset name (length 0)") {
        // n=1, amount=7, policy offset=12, asset offset=40 (past end of E, since len 0)
        // No region E bytes at all.
        val rep = new Array[Byte](12 + 28)
        rep(0) = 7.toByte
        rep(8) = 12.toByte
        rep(9) = 0.toByte
        rep(10) = 40.toByte // offset = end of rep; length = 0
        rep(11) = 0.toByte
        var k = 0
        while k < 28 do { rep(12 + k) = 0xaa.toByte; k += 1 }
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x01)
        writeVarLen(out, 3L)
        writeVarLen(out, 1L)
        writeVarLen(out, rep.length.toLong)
        out.write(rep)
        val value = MemPackReaders.readCompactValue(MemPack.Reader(out.toByteArray))
        assert(value.assets.assets.size == 1)
        val (_, inner) = value.assets.assets.head
        assert(inner.keys.head.bytes.isEmpty)
        assert(inner.values.head == 7L)
    }

    test("CompactValue — unknown tag") {
        val bytes = Array[Byte](0x02)
        val ex = intercept[MemPack.DecodeError](
          MemPackReaders.readCompactValue(MemPack.Reader(bytes))
        )
        assert(ex.getMessage.contains("CompactValue"))
    }

    test("TxOut tag 0 — CompactAddr + ada-only CompactValue → Shelley(no datum hash)") {
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x00) // TxOut tag 0
        // CompactAddr: Length + raw bytes
        writeVarLen(out, addrBytes.length.toLong)
        out.write(addrBytes)
        // CompactValue tag 0 (ada-only) + varlen coin
        out.write(0x00)
        writeVarLen(out, 2_000_000L)
        val txOut = MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Shelley(a, v, dh) =>
                assert(a == addr)
                assert(v.coin.value == 2_000_000L && v.assets.isEmpty)
                assert(dh.isEmpty)
            case other => fail(s"expected Shelley output, got $other")
        }
    }

    test("TxOut tag 1 — Shelley output with 32-byte datum hash") {
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes
        val dh = new Array[Byte](32)
        var i = 0
        while i < 32 do { dh(i) = (i + 0x80).toByte; i += 1 }
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x01) // TxOut tag 1
        writeVarLen(out, addrBytes.length.toLong)
        out.write(addrBytes)
        out.write(0x00) // CompactValue ada-only
        writeVarLen(out, 1_500_000L)
        out.write(dh) // 32-byte raw hash (PackedBytes32 = raw)
        val txOut = MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Shelley(_, v, Some(gotDh)) =>
                assert(v.coin.value == 1_500_000L)
                assert(gotDh.bytes.toArray.toSeq == dh.toSeq)
            case other => fail(s"expected Shelley with datum hash, got $other")
        }
    }

    test("TxOut tag 2 — stake-compressed ada-only (keyhash payment on testnet)") {
        // Build a synthetic base address and round-trip via the tag-2 encoding.
        // 28-byte stake key hash = 0x01 .. 0x1C
        val stakeKeyHash = new Array[Byte](28)
        var i = 0
        while i < 28 do { stakeKeyHash(i) = (i + 1).toByte; i += 1 }
        // 28-byte payment key hash = 0x40 .. 0x5B
        val paymentKeyHash = new Array[Byte](28)
        i = 0
        while i < 28 do { paymentKeyHash(i) = (i + 0x40).toByte; i += 1 }

        // Encode Addr28Extra: the hash bytes split across 4 Word64s, groups big-endian inside
        // each Word64 → but on-disk we write Word64s little-endian. So we need the LE bytes of
        // each group.
        def readWord64BE(bytes: Array[Byte], at: Int): Long = {
            var acc = 0L; var j = 0
            while j < 8 do { acc = (acc << 8) | (bytes(at + j) & 0xffL); j += 1 }
            acc
        }
        val w0 = readWord64BE(paymentKeyHash, 0)
        val w1 = readWord64BE(paymentKeyHash, 8)
        val w2 = readWord64BE(paymentKeyHash, 16)
        // Bytes 24..27 of the hash become the HIGH 32 bits of w3.
        val hashTail: Long =
            ((paymentKeyHash(24) & 0xffL) << 24) |
                ((paymentKeyHash(25) & 0xffL) << 16) |
                ((paymentKeyHash(26) & 0xffL) << 8) |
                (paymentKeyHash(27) & 0xffL)
        // Low 32 bits of w3: bits 0..1 encode (isKeyHash, isMainnet). For keyhash + testnet: 0x01.
        val paymentBits = 0x01L // bit 0 = key (pubkey), bit 1 = 0 (testnet)
        val w3 = (hashTail << 32) | paymentBits

        def writeWord64LE(out: java.io.ByteArrayOutputStream, w: Long): Unit = {
            var j = 0
            while j < 8 do { out.write(((w >>> (8 * j)) & 0xffL).toInt); j += 1 }
        }
        val outBuf = new java.io.ByteArrayOutputStream()
        outBuf.write(0x02) // TxOut tag 2
        // Credential (stake) — tag 1 (KeyHash) + 28 raw bytes
        outBuf.write(0x01)
        outBuf.write(stakeKeyHash)
        // Addr28Extra — 4 LE Word64s
        writeWord64LE(outBuf, w0)
        writeWord64LE(outBuf, w1)
        writeWord64LE(outBuf, w2)
        writeWord64LE(outBuf, w3)
        // CompactForm Coin standalone — tag 0 + VarLen
        outBuf.write(0x00)
        writeVarLen(outBuf, 7_000_000L)

        val txOut = MemPackReaders.readTxOut(MemPack.Reader(outBuf.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Shelley(a, v, dh) =>
                val addrBytes = a.toBytes.bytes
                // Shelley base address, testnet, payment keyhash + stake keyhash:
                //   header byte = 0x00 (type 0, network = 0 / testnet)
                //   payload = payment-hash (28) ++ stake-hash (28) = 56 bytes total
                // Total = 57 bytes
                assert(addrBytes.length == 57, s"addr length was ${addrBytes.length}")
                assert(addrBytes(0) == 0x00.toByte, f"header byte ${addrBytes(0)}%02x")
                assert(addrBytes.slice(1, 29).toSeq == paymentKeyHash.toSeq)
                assert(addrBytes.slice(29, 57).toSeq == stakeKeyHash.toSeq)
                assert(v.coin.value == 7_000_000L && v.assets.isEmpty)
                assert(dh.isEmpty)
            case other => fail(s"expected Shelley output, got $other")
        }
    }

    test("TxOut tag 3 — same as tag 2 + DataHash32 (reversed 8-byte groups)") {
        // Minimal reuse: encode a tag-2-style block, then append the 32 byte reversed-group hash.
        // A hash of 0x10..0x2F (32 ascending bytes) split into 4 big-endian Word64s:
        //   group 0 = 0x10111213_14151617 → LE on-disk: 17 16 15 14 13 12 11 10
        val hash = new Array[Byte](32)
        var i = 0
        while i < 32 do { hash(i) = (i + 0x10).toByte; i += 1 }
        def reverse8(src: Array[Byte], at: Int): Array[Byte] = {
            val out = new Array[Byte](8)
            var j = 0
            while j < 8 do { out(j) = src(at + 7 - j); j += 1 }
            out
        }
        val hashReversedGroups =
            reverse8(hash, 0) ++ reverse8(hash, 8) ++ reverse8(hash, 16) ++ reverse8(hash, 24)

        val stakeKeyHash = new Array[Byte](28)
        i = 0
        while i < 28 do { stakeKeyHash(i) = (i + 1).toByte; i += 1 }
        val paymentKeyHash = new Array[Byte](28)
        i = 0
        while i < 28 do { paymentKeyHash(i) = (i + 0x40).toByte; i += 1 }
        def readWord64BE(bytes: Array[Byte], at: Int): Long = {
            var acc = 0L; var j = 0
            while j < 8 do { acc = (acc << 8) | (bytes(at + j) & 0xffL); j += 1 }
            acc
        }
        val w0 = readWord64BE(paymentKeyHash, 0)
        val w1 = readWord64BE(paymentKeyHash, 8)
        val w2 = readWord64BE(paymentKeyHash, 16)
        val hashTail: Long =
            ((paymentKeyHash(24) & 0xffL) << 24) |
                ((paymentKeyHash(25) & 0xffL) << 16) |
                ((paymentKeyHash(26) & 0xffL) << 8) |
                (paymentKeyHash(27) & 0xffL)
        val w3 = (hashTail << 32) | 0x03L // key + mainnet
        def writeWord64LE(out: java.io.ByteArrayOutputStream, w: Long): Unit = {
            var j = 0
            while j < 8 do { out.write(((w >>> (8 * j)) & 0xffL).toInt); j += 1 }
        }
        val outBuf = new java.io.ByteArrayOutputStream()
        outBuf.write(0x03) // TxOut tag 3
        outBuf.write(0x01); outBuf.write(stakeKeyHash)
        writeWord64LE(outBuf, w0); writeWord64LE(outBuf, w1)
        writeWord64LE(outBuf, w2); writeWord64LE(outBuf, w3)
        outBuf.write(0x00); writeVarLen(outBuf, 3_210_000L)
        outBuf.write(hashReversedGroups)

        val txOut = MemPackReaders.readTxOut(MemPack.Reader(outBuf.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Shelley(_, _, Some(dh)) =>
                assert(dh.bytes.toArray.toSeq == hash.toSeq, "hash groups must be de-reversed")
            case other => fail(s"expected Shelley with datum hash, got $other")
        }
    }

    test("TxOut tag 5 — Babbage output with Plutus V2 reference script") {
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes
        val plutusScriptBytes = Array[Byte](0x41, 0x01) // minimal dummy Plutus blob
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x05) // TxOut tag 5
        writeVarLen(out, addrBytes.length.toLong); out.write(addrBytes)
        out.write(0x00); writeVarLen(out, 1_200_000L) // CompactValue ada-only
        out.write(0x00) // Datum tag 0 = NoDatum
        out.write(0x01) // AlonzoScript tag 1 = PlutusScript
        out.write(0x01) // PlutusScript tag 1 = PlutusV2
        writeVarLen(out, plutusScriptBytes.length.toLong)
        out.write(plutusScriptBytes)
        val txOut = MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Babbage(a, _, None, Some(ref)) =>
                assert(a == addr)
                ref.script match {
                    case scalus.cardano.ledger.Script.PlutusV2(bs) =>
                        assert(bs.bytes.toSeq == plutusScriptBytes.toSeq)
                    case other => fail(s"expected PlutusV2, got $other")
                }
            case other => fail(s"expected Babbage with ref script, got $other")
        }
    }

    test("TxOut tag 5 — PlutusV4 (Dijkstra, tag 3) is unsupported until Scalus models it") {
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x05)
        writeVarLen(out, addrBytes.length.toLong); out.write(addrBytes)
        out.write(0x00); writeVarLen(out, 500_000L)
        out.write(0x00)
        out.write(0x01) // PlutusScript
        out.write(0x03) // PlutusScript tag 3 = V4 (Dijkstra)
        writeVarLen(out, 1L); out.write(0x00)
        val ex = intercept[MemPackReaders.UnsupportedTxOutVariant](
          MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        )
        assert(ex.getMessage.contains("PlutusV4") || ex.getMessage.contains("tag 3"))
    }

    test("TxOut tag 5 — native-script (Timelock) ref script") {
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes

        // Build a simple Timelock: TimeExpire at slot 12345. Serialize via Scalus's Borer
        // encoder to get canonical bytes.
        val timelock = scalus.cardano.ledger.Timelock.TimeExpire(12345L)
        val timelockCbor = io.bullet.borer.Cbor.encode(timelock).toByteArray

        val out = new java.io.ByteArrayOutputStream()
        out.write(0x05) // TxOut tag 5
        writeVarLen(out, addrBytes.length.toLong); out.write(addrBytes)
        out.write(0x00); writeVarLen(out, 1L) // CompactValue ada-only, 1 lovelace
        out.write(0x00) // Datum NoDatum
        out.write(0x00) // AlonzoScript tag 0 = NativeScript
        writeVarLen(out, timelockCbor.length.toLong)
        out.write(timelockCbor)
        val txOut = MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Babbage(_, _, None, Some(ref)) =>
                ref.script match {
                    case scalus.cardano.ledger.Script.Native(decoded) =>
                        assert(decoded == timelock)
                    case other => fail(s"expected Native, got $other")
                }
            case other => fail(s"expected Babbage with native ref script, got $other")
        }
    }

    test("TxOut tag 4 — Babbage output with inline BinaryData datum (no Datum tag)") {
        // Per cardano-ledger eras/babbage/impl/.../Babbage/TxOut.hs:138-141,
        // TxOutCompactDatum's third field is `BinaryData era`, not `Datum era`.
        // BinaryData's MemPack is a bare Length + raw bytes (no 0/1/2 discriminator).
        val addr = scalus.cardano.address.Address.fromBech32(
          "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
        )
        val addrBytes = addr.toBytes.bytes
        val plutusDataCbor = Array[Byte](0x00.toByte) // Data.I(0)
        val out = new java.io.ByteArrayOutputStream()
        out.write(0x04) // TxOut tag 4
        writeVarLen(out, addrBytes.length.toLong); out.write(addrBytes)
        out.write(0x00); writeVarLen(out, 1_000_000L) // CompactValue ada-only
        writeVarLen(out, plutusDataCbor.length.toLong) // BinaryData Length prefix
        out.write(plutusDataCbor)
        val txOut = MemPackReaders.readTxOut(MemPack.Reader(out.toByteArray))
        txOut match {
            case scalus.cardano.ledger.TransactionOutput.Babbage(_, _, Some(d), None) =>
                d match {
                    case scalus.cardano.ledger.DatumOption.Inline(data) =>
                        assert(data == scalus.uplc.builtin.Data.I(BigInt(0)))
                    case other => fail(s"expected Inline datum, got $other")
                }
            case other => fail(s"expected Babbage with datum, got $other")
        }
    }

    test("slice reader consumes exactly N bytes, sub-reader starts fresh") {
        val bytes: Array[Byte] = Array(0x01.toByte, 0x02.toByte, 0x03.toByte, 0x04.toByte)
        val r = MemPack.Reader(bytes)
        val sub = r.sliceReader(3)
        assert(sub.remaining == 3)
        assert(sub.readUnsignedByte() == 1)
        assert(sub.readUnsignedByte() == 2)
        assert(sub.readUnsignedByte() == 3)
        assert(sub.eof)
        assert(r.readUnsignedByte() == 4)
        assert(r.eof)
    }
}
