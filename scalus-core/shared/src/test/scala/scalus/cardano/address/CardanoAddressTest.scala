package scalus.cardano.address

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.{Hash, *}

import scala.util.Success

/** Test suite for CardanoAddress implementation Tests cover address encoding/decoding for different
  * address types based on CIP-19 test vectors
  */
class CardanoAddressTest extends AnyFunSuite {

    // Test vectors from CIP-19 and real Cardano addresses
    private val testVectors = List(
      // Format: (address_string, expected_type_id, description)
        // format: off
        ("addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x", 0, "Payment Key + Stake Key"),
        ("addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh", 1, "Script + Stake Key"),
        ("addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shs2z78ve", 2, "Payment Key + Script"),
        ("addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g", 3, "Script + Script"),
        ("addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k", 4, "Payment Key + Pointer"),
        ("addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu", 5, "Script + Pointer"),
        ("addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8", 6, "Payment Key Only"),
        ("addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx", 7, "Script Only"),
        ("stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw", 14, "Stake Key"),
        ("stake178phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcccycj5", 15, "Stake Script")
    )
    // format: on

    // Sample hash values for testing
    private val samplePaymentHash = AddrKeyHash(
      ByteString.fromHex("c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f")
    )
    private val sampleStakeHash = Hash.stakeKeyHash(
      ByteString.fromHex("337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251")
    )
    private val sampleScriptHash = Hash.scriptHash(
      ByteString.fromHex("1234567890abcdef1234567890abcdef12345678901234567890abcd")
    )

    test("Original test vectors should parse with bech32") {
        testVectors.foreach { case (addressStr, expectedTypeId, description) =>
            val parseResult = Address.fromBech32(addressStr)
            assert(parseResult.typeId == expectedTypeId, s"Wrong type ID for $description")
        }
    }

    test("Network enum should handle all cases correctly") {
        assert(Network.fromNetworkId(0x00) == Network.Testnet)
        assert(Network.fromNetworkId(0x01) == Network.Mainnet)
        assert(Network.fromNetworkId(0x05) == Network.Other(0x05))

        assert(Network.Testnet.networkId == 0x00)
        assert(Network.Mainnet.networkId == 0x01)
        assert(Network.Other(0x05).networkId == 0x05)

        assert(Network.Mainnet.isMainnet == true)
        assert(Network.Testnet.isMainnet == false)
    }

    test("Pointer should encode and decode correctly") {
        val pointer = Pointer(Slot(2498243), 27, 3)
        val encoded = pointer.toBytes
        // the pointer of the CIP-19 test vectors, most significant 7-bit group first
        assert(pointer.toHex == "8198bd431b03")

        // Test round-trip encoding/decoding
        val decoded = Pointer.fromBytes(encoded)
        assert(decoded == Success(pointer))
        // Test parsing from array with offset
        val Success(parsedPointer, bytesUsed) = Pointer.parseFrom(encoded, 0): @unchecked
        assert(parsedPointer == pointer)
        assert(bytesUsed == encoded.length)
    }

    test("Variable length encoding should work correctly") {
        // (value, bytes) at both sides of every length from 1 to 9 bytes, as the ledger's
        // putVariableLengthWord64 writes them: a round trip alone cannot tell the group order
        val testCases = List(
          0L -> "00",
          127L -> "7f",
          128L -> "8100",
          16383L -> "ff7f",
          16384L -> "818000",
          (1L << 21) - 1 -> "ffff7f",
          (1L << 21) -> "81808000",
          (1L << 28) - 1 -> "ffffff7f",
          (1L << 28) -> "8180808000",
          Pointer.MaxSlot -> "8fffffff7f",
          (1L << 35) - 1 -> "ffffffff7f",
          (1L << 35) -> "818080808000",
          (1L << 42) -> "81808080808000",
          (1L << 49) -> "8180808080808000",
          (1L << 56) -> "818080808080808000",
          Long.MaxValue -> "ffffffffffffffff7f"
        )

        testCases.foreach { (value, expectedHex) =>
            val encoded = VarLen.encode(value)
            assert(
              ByteString.fromArray(encoded).toHex == expectedHex,
              s"Encoding failed for $value"
            )
            assert(VarLen.size(value) == encoded.length)

            val (decoded, bytesUsed) = VarLen.decode(encoded, 0)
            assert(decoded == value)
            assert(bytesUsed == encoded.length)
        }
    }

    test("ShelleyPaymentPart should handle key and script hashes") {
        val keyPart = ShelleyPaymentPart.Key(samplePaymentHash)
        val scriptPart = ShelleyPaymentPart.Script(sampleScriptHash)

        assert(keyPart.asHash == samplePaymentHash)
        assert(scriptPart.asHash == sampleScriptHash)

        assert(keyPart.isScript == false)
        assert(scriptPart.isScript == true)

        assert(keyPart.toBytes == samplePaymentHash)
        assert(scriptPart.toBytes == sampleScriptHash)
    }

    test("ShelleyDelegationPart should handle all delegation types") {
        val keyDelegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val scriptDelegation = ShelleyDelegationPart.scriptHash(sampleScriptHash)
        val pointerDelegation = ShelleyDelegationPart.Pointer(Pointer(Slot(100), 5, 2))
        val nullDelegation = ShelleyDelegationPart.Null

        assert(keyDelegation.asHash == Some(sampleStakeHash))
        assert(scriptDelegation.asHash == Some(sampleScriptHash))
        assert(pointerDelegation.asHash == None)
        assert(nullDelegation.asHash == None)

        assert(keyDelegation.isScript == false)
        assert(scriptDelegation.isScript == true)
        assert(pointerDelegation.isScript == false)
        assert(nullDelegation.isScript == false)
    }

    test("StakePayload should handle stake and script types") {
        val stakePayload = StakePayload.Stake(sampleStakeHash)
        val scriptPayload = StakePayload.Script(sampleScriptHash)

        assert(stakePayload.asHash == sampleStakeHash)
        assert(scriptPayload.asHash == sampleScriptHash)

        assert(stakePayload.isScript == false)
        assert(scriptPayload.isScript == true)

        // Test parsing from bytes
        val stakeFromBytes = StakePayload.fromBytes(sampleStakeHash.bytes, false)
        assert(stakeFromBytes == Success(StakePayload.Stake(sampleStakeHash)))

        val scriptFromBytes = StakePayload.fromBytes(sampleScriptHash.bytes, true)
        assert(scriptFromBytes == Success(StakePayload.Script(sampleScriptHash)))
    }

    test("ShelleyAddress should calculate correct type IDs") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val addr = ShelleyAddress(Network.Mainnet, payment, delegation)
        assert(addr.typeId == 0x00)

        val scriptAddr = ShelleyAddress(
          Network.Mainnet,
          ShelleyPaymentPart.Script(sampleScriptHash),
          ShelleyDelegationPart.keyHash(sampleStakeHash)
        )
        assert(scriptAddr.typeId == 0x01)

        val enterpriseAddr = Address(
          Network.Mainnet,
          Credential.KeyHash(samplePaymentHash)
        )
        assert(enterpriseAddr.typeId == 0x06)
        assert(enterpriseAddr.isEnterprise == true)
    }

    test("ShelleyAddress should build correct headers") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val mainnetAddr = ShelleyAddress(Network.Mainnet, payment, delegation)
        val testnetAddr = ShelleyAddress(Network.Testnet, payment, delegation)

        assert(mainnetAddr.toHeader == 0x01) // type 0 (0000) + mainnet (0001)
        assert(testnetAddr.toHeader == 0x00) // type 0 (0000) + testnet (0000)
    }

    test("StakeAddress should calculate correct type IDs and headers") {
        val stakeAddr = StakeAddress(Network.Mainnet, StakePayload.Stake(sampleStakeHash))
        val scriptStakeAddr = StakeAddress(Network.Mainnet, StakePayload.Script(sampleScriptHash))

        assert(stakeAddr.typeId == 0x0e)
        assert(scriptStakeAddr.typeId == 0x0f)

        assert(stakeAddr.toHeader == 0xe1.toByte) // type 14 (1110) + mainnet (0001)
        assert(scriptStakeAddr.toHeader == 0xf1.toByte) // type 15 (1111) + mainnet (0001)

        assert(stakeAddr.hasScript == false)
        assert(scriptStakeAddr.hasScript == true)
    }

    test("Address serialization should produce correct bytes") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val shelleyAddr = ShelleyAddress(Network.Mainnet, payment, delegation)

        val bytes = shelleyAddr.toBytes
        assert(bytes.length == 57) // 1 header + 28 payment + 28 delegation
        assert(bytes.bytes(0) == 0x01) // header

        val stakeAddr = StakeAddress(Network.Mainnet, StakePayload.Stake(sampleStakeHash))
        val stakeBytes = stakeAddr.toBytes
        assert(stakeBytes.length == 29) // 1 header + 28 hash
        assert(stakeBytes.bytes(0) == 0xe1.toByte)
    }

    test("Address parsing should handle all address types") {
        // Test each address type parser with manually constructed bytes

        // Type 0: Payment Key + Stake Key
        val type0Header: Byte = 0x01 // type 0 + mainnet
        val type0Payload = samplePaymentHash.bytes ++ sampleStakeHash.bytes
        val type0Bytes = type0Header +: type0Payload

        val parsedType0 = Address.fromBytes(type0Bytes)
        parsedType0 match {
            case _: ShelleyAddress => // Expected
            case _                 => fail("Expected Shelley address")
        }
        assert(parsedType0.typeId == 0)

        // Type 14: Stake Key
        val type14Header: Byte = 0xe1.toByte // type 14 + mainnet
        val type14Bytes = type14Header +: sampleStakeHash.bytes

        val parsedType14 = Address.fromBytes(type14Bytes)

        parsedType14 match {
            case _: StakeAddress => // Expected
            case _               => fail("Expected Stake address")
        }
        assert(parsedType14.typeId == 14)
    }

    test("Address parsing should handle invalid inputs gracefully") {
        // Empty bytes
        assertThrows[IllegalArgumentException] {
            Address.fromBytes(Array.empty)
        }

        // Invalid header type
        val invalidHeader: Byte = 0x91.toByte // type 9 doesn't exist
        val invalidBytes = invalidHeader +: samplePaymentHash.bytes
        assertThrows[IllegalArgumentException] {
            Address.fromBytes(invalidBytes)
        }

        // Wrong payload length
        val shortPayload = Array[Byte](0x01, 0x02, 0x03) // too short
        assertThrows[IllegalArgumentException] {
            Address.fromBytes(shortPayload)
        }
    }

    test("Address round-trip should preserve data") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val originalAddr = ShelleyAddress(Network.Mainnet, payment, delegation)

        // Serialize and parse back
        val bytes = originalAddr.toBytes.bytes
        val parsedAddr = Address.fromBytes(bytes)

        assert(parsedAddr == originalAddr)
        assert(parsedAddr.typeId == originalAddr.typeId)
    }

    test("Address utility methods should work correctly") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val scriptPayment = ShelleyPaymentPart.Script(sampleScriptHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val normalAddr = ShelleyAddress(Network.Mainnet, payment, delegation)
        val scriptAddr = ShelleyAddress(Network.Mainnet, scriptPayment, delegation)
        val enterpriseAddr = Address(Network.Mainnet, Credential.KeyHash(samplePaymentHash))
        val stakeAddr = StakeAddress(Network.Mainnet, StakePayload.Stake(sampleStakeHash))

        assert(normalAddr.hasScript == false)
        assert(scriptAddr.hasScript == true)

        assert(normalAddr.isEnterprise == false)
        assert(enterpriseAddr.isEnterprise == true)
        assert(stakeAddr.isEnterprise == false)

//        assert(normalAddr.network == Some(Network.Mainnet))
//        assert(stakeAddr.network == Some(Network.Mainnet))
    }

    test("Address conversion between Shelley and Stake should work") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val shelleyAddr = ShelleyAddress(Network.Mainnet, payment, delegation)

        val stakeAddr = shelleyAddr.toStakeAddress
        assert(stakeAddr.isSuccess)

        val convertedStake = stakeAddr.get
        assert(convertedStake.network == shelleyAddr.network)
        assert(convertedStake.payload.asHash == delegation.asHash.get)

        // Test conversion failure for enterprise address
        val enterpriseAddr = ShelleyAddress(Network.Mainnet, payment, ShelleyDelegationPart.Null)
        val enterpriseConversion = enterpriseAddr.toStakeAddress
        assert(enterpriseConversion.isFailure)
    }

    test("Edge cases should be handled correctly") {
        // The largest pointer the ledger holds: a 32-bit slot and 16-bit indexes
        val maxPointer = Pointer(Slot(Pointer.MaxSlot), Pointer.MaxIndex, Pointer.MaxIndex)
        val encoded = maxPointer.toBytes
        // 5 bytes for the slot, 3 for each index
        assert(maxPointer.toHex == "8fffffff7f" + "83ff7f" + "83ff7f")

        val decoded = Pointer.fromBytes(encoded)
        assert(decoded == Success(maxPointer))

        // Test zero values
        val minPointer = Pointer(Slot(0), 0, 0)
        val minEncoded = minPointer.toBytes
        assert(minEncoded.sameElements(Array[Byte](0x00, 0x00, 0x00)))

        val minDecoded = Pointer.fromBytes(minEncoded)
        assert(minDecoded == Success(minPointer))
    }

    test("CIP-19 pointer addresses decode to their pointer and re-encode unchanged") {
        val pointer = Pointer(Slot(2498243), 27, 3)
        val vectors = List(
          "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k",
          "addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu"
        )
        vectors.foreach { bech32 =>
            Address.fromBech32(bech32) match
                case address @ ShelleyAddress(_, _, ShelleyDelegationPart.Pointer(decoded)) =>
                    assert(decoded == pointer, bech32)
                    assert(address.toBech32.get == bech32)
                case other => fail(s"expected a pointer address, got $other")
        }
    }

    private def hex(bytes: String): Array[Byte] = ByteString.fromHex(bytes).bytes

    /** The numbers written as `Pointer.toBytes` writes them, whether or not they fit a pointer. */
    private def pointerBytes(slot: Long, txIdx: Long, certIdx: Long): Array[Byte] =
        Seq(slot, txIdx, certIdx).flatMap(v => VarLen.encode(v).toSeq).toArray

    /** Pointers whose slot or index does not fit the ledger's 32 and 16 bits. */
    private val outOfRange = Seq(
      pointerBytes(Pointer.MaxSlot + 1, 1, 2),
      pointerBytes(5, Pointer.MaxIndex + 1, 2),
      pointerBytes(5, 1, Pointer.MaxIndex + 1),
      pointerBytes(Long.MaxValue, 0, 0)
    )

    test("Before protocol version 9 a pointer that does not fit is read as the zero pointer") {
        // As the ledger's decodePtrLenient and mkPtrNormalized: a slot of 2^32, or an index of
        // 2^16, makes the whole pointer Ptr 0 0 0
        val zero = Success(Pointer(Slot(0), 0, 0))
        outOfRange.foreach { bytes =>
            assert(Pointer.fromBytes(bytes, ProtocolVersion.valentinePV) == zero)
        }
    }

    test("From protocol version 9 a pointer that does not fit fails") {
        // As the ledger's decodePtr
        outOfRange.foreach { bytes =>
            assert(Pointer.fromBytes(bytes).isFailure)
            assert(Pointer.fromBytes(bytes, ProtocolVersion.plominPV).isFailure)
        }
    }

    test("From protocol version 9 a number takes at most the bytes its width needs") {
        // As the ledger's decodeVariableLengthWord32 and decodeVariableLengthWord16: zero groups
        // in front are allowed, up to 5 bytes for the slot and 3 for an index
        assert(Pointer.fromBytes(hex("8080808005" + "0000")) == Success(Pointer(Slot(5), 0, 0)))
        assert(Pointer.fromBytes(hex("00" + "808005" + "00")) == Success(Pointer(Slot(0), 5, 0)))
        assert(Pointer.fromBytes(hex("808080808005" + "0000")).isFailure) // 6 bytes for the slot
        assert(Pointer.fromBytes(hex("00" + "80808005" + "00")).isFailure) // 4 bytes for an index
        assert(Pointer.fromBytes(hex("9080808000" + "0000")).isFailure) // the slot 2^32
        assert(Pointer.fromBytes(hex("00" + "848000" + "00")).isFailure) // the index 2^16
        // Before version 9 each number takes any number of bytes
        assert(
          Pointer.fromBytes(hex("808080808005" + "00" + "80808005"), ProtocolVersion.valentinePV)
              == Success(Pointer(Slot(5), 0, 5))
        )
    }

    test("Bytes after a pointer fail from protocol version 7, and are dropped before it") {
        // As the ledger's ensureBufIsConsumed, which it applies from version 7
        val leftOver = hex("050102" + "ff00")
        assert(Pointer.fromBytes(leftOver).isFailure)
        assert(Pointer.fromBytes(leftOver, ProtocolVersion.vasilPV).isFailure)
        assert(
          Pointer.fromBytes(leftOver, ProtocolVersion.alonzoPV) == Success(Pointer(Slot(5), 1, 2))
        )
    }

    /** The bytes of a mainnet address with the sample payment key hash and `pointer`. */
    private def address(pointer: String) = hex("41") ++ samplePaymentHash.bytes ++ hex(pointer)

    private def withPointer(pointer: Pointer) = ShelleyAddress(
      Network.Mainnet,
      ShelleyPaymentPart.Key(samplePaymentHash),
      ShelleyDelegationPart.Pointer(pointer)
    )

    test("A pointer address reads as at the protocol version it is given") {
        val outOfRange = address("9080808000" + "0102") // the slot 2^32
        val tooWide = intercept[IllegalArgumentException](Address.fromBytes(outOfRange))
        assert(tooWide.getMessage.contains("more than 32 bits"))
        assert(
          Address.fromBytes(outOfRange, ProtocolVersion.valentinePV)
              == withPointer(Pointer(Slot(0), 0, 0))
        )

        val leftOver = address("050102" + "ff00")
        val error = intercept[IllegalArgumentException](
          Address.fromBytes(leftOver, ProtocolVersion.vasilPV)
        )
        assert(error.getMessage.contains("2 bytes left after the pointer"))
        assertThrows[IllegalArgumentException](Address.fromBytes(leftOver))
        assert(
          Address.fromBytes(leftOver, ProtocolVersion.alonzoPV)
              == withPointer(Pointer(Slot(5), 1, 2))
        )
    }

    test("A lenient read takes a pointer address as a node holds it") {
        // As the ledger's decompactAddr: the pointer normalized, the bytes after it dropped
        val outOfRange = address("9080808000" + "0102") // the slot 2^32
        val leftOver = address("050102" + "ff00")
        assert(Address.fromBytesLenient(outOfRange) == withPointer(Pointer(Slot(0), 0, 0)))
        assert(Address.fromBytesLenient(leftOver) == withPointer(Pointer(Slot(5), 1, 2)))
        val bech32 = Bech32.encode("addr", outOfRange)
        assert(Address.fromBech32Lenient(bech32) == withPointer(Pointer(Slot(0), 0, 0)))
        assertThrows[IllegalArgumentException](Address.fromBech32(bech32))
    }

    test("A pointer number with zero groups in front is written back in its shortest encoding") {
        val overlong = address("8005" + "0102") // the slot 5 in two bytes
        val parsed = Address.fromBytes(overlong)
        assert(parsed == withPointer(Pointer(Slot(5), 1, 2)))
        assert(parsed.toBytes.bytes.sameElements(address("05" + "0102")))
        // parseFrom counts the bytes it read, not the bytes the pointer writes
        val Success(pointer, bytesUsed) = Pointer.parseFrom(hex("8005" + "0102"), 0): @unchecked
        assert(bytesUsed == 4 && pointer.toBytes.length == 3)
    }

    test("Generated pointers take every encoded length") {
        // So that the property tests over addresses see a slot of 1 to 5 bytes and an index of
        // 1 to 3, not almost only 5-byte slots
        import ArbitraryInstances.given Arbitrary[Pointer]
        val gen = summon[Arbitrary[Pointer]].arbitrary
        val pointers = Seq.fill(1000)(gen.sample.get)
        assert(pointers.map(p => VarLen.size(p.slot.slot)).toSet == (1 to 5).toSet)
        assert(pointers.map(p => VarLen.size(p.txIdx)).toSet == (1 to 3).toSet)
        assert(pointers.map(p => VarLen.size(p.certIdx)).toSet == (1 to 3).toSet)
    }

    test("A pointer beyond the ledger's widths cannot be built") {
        assertThrows[IllegalArgumentException](Pointer(Slot(Pointer.MaxSlot + 1), 0, 0))
        assertThrows[IllegalArgumentException](Pointer(Slot(0), Pointer.MaxIndex + 1, 0))
        assertThrows[IllegalArgumentException](Pointer(Slot(0), 0, -1))
    }

    test("A variable-length number keeps its low 64 bits, as the ledger's Word64 decoder") {
        // 2^64 + 5 in ten 7-bit groups: 2, eight zeros, 5
        val wrapping = Array(0x82.toByte) ++ Array.fill(8)(0x80.toByte) ++ Array[Byte](0x05)
        assert(VarLen.decode(wrapping, 0) == (5L, 10))
        assert(
          Pointer.fromBytes(wrapping ++ Array[Byte](0, 0), ProtocolVersion.valentinePV)
              == Success(Pointer(Slot(5), 0, 0))
        )
    }

    test("A long run of continuation bytes decodes without recursion") {
        val longZero = Array.fill(100_000)(0x80.toByte) ++ Array[Byte](0x00)
        assert(VarLen.decode(longZero, 0) == (0L, 100_001))
        assert(
          Pointer.fromBytes(longZero ++ Array[Byte](0, 0), ProtocolVersion.valentinePV)
              == Success(Pointer(Slot(0), 0, 0))
        )
        assert(Pointer.fromBytes(longZero ++ Array[Byte](0, 0)).isFailure)
    }

    test("A truncated variable-length number fails") {
        for pv <- Seq(ProtocolVersion.conwayPV, ProtocolVersion.valentinePV) do
            assert(Pointer.fromBytes(Array(0x81.toByte), pv).isFailure)
            assert(Pointer.fromBytes(Array[Byte](0x01, 0x02), pv).isFailure)
    }

    test("Human readable prefixes should be correct") {
        val payment = ShelleyPaymentPart.Key(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val mainnetShelley = ShelleyAddress(Network.Mainnet, payment, delegation)
        val testnetShelley = ShelleyAddress(Network.Testnet, payment, delegation)

        assert(mainnetShelley.hrp == Success("addr"))
        assert(testnetShelley.hrp == Success("addr_test"))

        val mainnetStake = StakeAddress(Network.Mainnet, StakePayload.Stake(sampleStakeHash))
        val testnetStake = StakeAddress(Network.Testnet, StakePayload.Stake(sampleStakeHash))

        assert(mainnetStake.hrp == Success("stake"))
        assert(testnetStake.hrp == Success("stake_test"))

        // Test unknown network
        val unknownShelley = ShelleyAddress(Network.Other(0x05), payment, delegation)
        assert(unknownShelley.hrp.isFailure)
    }

    test("addr string interpolator should parse addresses correctly") {
        import scalus.cardano.address.Address.addr

        // Test mainnet Shelley address
        val mainnetAddress =
            addr"addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
        assert(mainnetAddress.isInstanceOf[ShelleyAddress])
        assert(mainnetAddress.asInstanceOf[ShelleyAddress].network == Network.Mainnet)

        // Test enterprise address (payment key only)
        val enterpriseAddr = addr"addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8"
        assert(enterpriseAddr.isInstanceOf[ShelleyAddress])
        assert(enterpriseAddr.asInstanceOf[ShelleyAddress].isEnterprise)

        // Test stake address via addr interpolator
        val stakeAddr = addr"stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
        assert(stakeAddr.isInstanceOf[StakeAddress])

        // Test invalid address throws
        intercept[Exception] {
            addr"invalid_address"
        }
    }

    test("stake string interpolator should parse stake addresses correctly") {
        import scalus.cardano.address.Address.stake

        // Test mainnet stake address
        val mainnetStake = stake"stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
        assert(mainnetStake.network == Network.Mainnet)
        assert(!mainnetStake.hasScript)

        // Test stake script address
        val stakeScript = stake"stake178phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcccycj5"
        assert(stakeScript.hasScript)

        // Test that non-stake address throws
        intercept[IllegalArgumentException] {
            stake"addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
        }
    }

    test("addr interpolator should support string interpolation") {
        import scalus.cardano.address.Address.addr

        val prefix = "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer"
        val suffix = "3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
        val interpolated = addr"$prefix$suffix"

        assert(interpolated.isInstanceOf[ShelleyAddress])
        assert(interpolated.asInstanceOf[ShelleyAddress].network == Network.Mainnet)
    }

    // Byron address test vectors from Cardano ledger golden tests
    // These are verified valid addresses with correct CRC32 checksums

    // Icarus-style Byron addresses - no HD derivation path (empty attributes)
    // From cardano-ledger Address1 golden test
    private val byronIcarusAddresses = List(
      "Ae2tdPwUPEZDoUnyXuAgqzhkjNXNJeiZ5nqwprg9sArZmRNjySfJ5uz4FjB"
    )

    // Daedalus-style Byron addresses - include HD derivation path in attributes
    // From cardano-ledger Address0 golden test (type ATVerKey with HDAddressPayload)
    private val byronDaedalusAddresses = List(
      "2RhQhCGqYPDpFgTsnBTbnvPvCwpqAkjwLqQkWpkqXbLRmNxd4xNd262nGsr8JiynyKRUeMLSJ9Ntho9i76uvBTrVXdJJG5yiNLb8frmUe5qX7E"
    )

    // ATRedeem Byron addresses - type 2 for redeem keys
    // From cardano-ledger Address2 golden test
    private val byronRedeemAddresses = List(
      "2RhQhCGqYPDpeh7hWZC5PsveBT6mdsT5TGN8RyWBGFECyke59YY6XfQPWK1wrPZVErkpS73P52oQG17DN7H72tgiwh8kzxcAzaqoANgRfWk7eX"
    )

    // Combined list for tests that apply to all types
    private val byronMainnetAddresses =
        byronIcarusAddresses ++ byronDaedalusAddresses ++ byronRedeemAddresses

    test("Byron address should decode from Base58") {
        for base58 <- byronMainnetAddresses do {
            val result = ByronAddress.fromBase58(base58)
            assert(result.isSuccess, s"Failed to decode: $base58")
            val addr = result.get
            assert(addr.typeId == 0x08, "Byron addresses should have type ID 0x08")
        }
    }

    test("Byron address should encode to Base58") {
        for base58 <- byronMainnetAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(addr.toBase58 == base58, s"Round-trip failed for: $base58")
        }
    }

    test("Byron address should round-trip through encode") {
        for base58 <- byronMainnetAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(addr.encode == Success(base58), s"encode() should return Base58 for: $base58")
        }
    }

    test("Byron address should detect mainnet") {
        for base58 <- byronMainnetAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(
              addr.getNetwork == Some(Network.Mainnet),
              s"Should detect mainnet for: $base58, got: ${addr.getNetwork}"
            )
        }
    }

    test("Byron address should extract addrRoot (keyHashOption)") {
        for base58 <- byronMainnetAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(addr.keyHashOption.isDefined, s"keyHashOption should be defined for: $base58")
            val keyHash = addr.keyHashOption.get
            assert(keyHash.size == 28, s"Key hash should be 28 bytes for: $base58")
        }
    }

    test("Byron address should have correct properties") {
        val addr = ByronAddress.fromBase58(byronMainnetAddresses.head).get
        assert(!addr.hasScript, "Byron addresses don't have scripts")
        assert(!addr.isEnterprise, "Byron addresses are not enterprise addresses")
        assert(addr.scriptHashOption.isEmpty, "Byron addresses don't have script hashes")
        assert(addr.hrp.isFailure, "Byron addresses don't use bech32")
    }

    test("Byron address CRC32 validation should reject invalid addresses") {
        val validBase58 = byronMainnetAddresses.head
        // Corrupt the last character
        val corruptedBase58 =
            validBase58.dropRight(1) + (if validBase58.last == 'i' then "j" else "i")

        val result = ByronAddress.fromBase58(corruptedBase58)
        assert(result.isFailure, "Should reject address with invalid CRC32")
    }

    test("Byron address should parse via Address.fromString") {
        for base58 <- byronMainnetAddresses do {
            val addr = Address.fromString(base58)
            assert(addr.isInstanceOf[ByronAddress], s"Should parse as ByronAddress: $base58")
        }
    }

    test("Byron address parsed components should be accessible") {
        val addr = ByronAddress.fromBase58(byronMainnetAddresses.head).get
        val parsed = addr.parsed

        // Check parsed structure
        assert(parsed.addrRoot.size == 28, "addrRoot should be 28 bytes")
        assert(parsed.isValid, "CRC32 should be valid")
        assert(
          parsed.addrType == 0 || parsed.addrType == 2,
          "addrType should be 0 (VerKey) or 2 (Redeem)"
        )
    }

    test("Byron address attributesSize should be calculated") {
        for base58 <- byronMainnetAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            // attributesSize should return non-negative value
            assert(addr.attributesSize >= 0, s"attributesSize should be >= 0 for: $base58")
        }
    }

    test("Daedalus Byron addresses should have derivation path") {
        for base58 <- byronDaedalusAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(
              addr.derivationPath.isDefined,
              s"Daedalus address should have derivation path: $base58"
            )
            // Daedalus derivation paths are encrypted, so we just check they exist and have data
            val path = addr.derivationPath.get
            assert(path.size > 0, s"Derivation path should not be empty for: $base58")
            // Daedalus addresses should have larger attributesSize due to derivation path
            assert(addr.attributesSize > 0, s"Daedalus should have attributesSize > 0 for: $base58")
        }
    }

    test("Icarus Byron addresses should not have derivation path") {
        for base58 <- byronIcarusAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(
              addr.derivationPath.isEmpty,
              s"Icarus address should not have derivation path: $base58, got: ${addr.derivationPath}"
            )
            // Icarus addresses typically have attributesSize = 0 (no derivation path, no unknown attrs)
            assert(
              addr.attributesSize == 0,
              s"Icarus should have attributesSize = 0 for: $base58, got: ${addr.attributesSize}"
            )
        }
    }

    test("Byron address types should be correctly identified") {
        // ATVerKey (type 0) addresses - Icarus and Daedalus
        for base58 <- byronIcarusAddresses ++ byronDaedalusAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(
              addr.byronAddrType == 0,
              s"Address should be ATVerKey (type 0): $base58, got: ${addr.byronAddrType}"
            )
        }
        // ATRedeem (type 2) addresses
        for base58 <- byronRedeemAddresses do {
            val addr = ByronAddress.fromBase58(base58).get
            assert(
              addr.byronAddrType == 2,
              s"Address should be ATRedeem (type 2): $base58, got: ${addr.byronAddrType}"
            )
        }
    }

    test("Daedalus and Icarus addresses should have different lengths") {
        // Icarus addresses are shorter (no derivation path)
        val icarusLen = byronIcarusAddresses.head.length
        // Daedalus addresses are longer (include encrypted derivation path)
        val daedalusLen = byronDaedalusAddresses.head.length

        assert(
          daedalusLen > icarusLen,
          s"Daedalus ($daedalusLen) should be longer than Icarus ($icarusLen)"
        )
    }

    test("Byron address round-trip preserves all data") {
        for base58 <- byronMainnetAddresses do {
            val addr1 = ByronAddress.fromBase58(base58).get
            val encoded = addr1.toBase58
            val addr2 = ByronAddress.fromBase58(encoded).get

            assert(addr1.toBytes == addr2.toBytes, s"Bytes should match for: $base58")
            assert(
              addr1.parsed.addrRoot == addr2.parsed.addrRoot,
              s"addrRoot should match for: $base58"
            )
            assert(
              addr1.derivationPath == addr2.derivationPath,
              s"derivationPath should match for: $base58"
            )
            assert(addr1.byronAddrType == addr2.byronAddrType, s"byronAddrType should match")
        }
    }
}
