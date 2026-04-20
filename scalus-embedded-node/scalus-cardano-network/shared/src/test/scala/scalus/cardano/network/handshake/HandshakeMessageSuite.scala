package scalus.cardano.network.handshake

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.network.NetworkMagic
import scalus.cardano.network.handshake.HandshakeMessage.*
import scalus.utils.Hex.*

/** CBOR round-trip + golden-vector tests for the handshake message codec. Goldens were produced by
  * hand from the `ouroboros-network` CDDL — each one documents exactly which message shape it
  * exercises.
  */
class HandshakeMessageSuite extends AnyFunSuite {

    private def roundTrip(m: HandshakeMessage): Unit = {
        val bytes = Cbor.encode(m).toByteArray
        val decoded = Cbor.decode(bytes).to[HandshakeMessage].value
        assert(decoded == m, s"round-trip mismatch for $m\nbytes=${bytes.toHex}")
    }

    private def v14Data(magic: Long = NetworkMagic.Preview.value): NodeToNodeVersionData.V14 =
        NodeToNodeVersionData.V14(
          networkMagic = NetworkMagic(magic),
          initiatorOnlyDiffusionMode = true,
          peerSharing = 0,
          query = false
        )

    private def v16Data(magic: Long = NetworkMagic.Preview.value): NodeToNodeVersionData.V16 =
        NodeToNodeVersionData.V16(
          networkMagic = NetworkMagic(magic),
          initiatorOnlyDiffusionMode = true,
          peerSharing = 0,
          query = false,
          perasSupport = false
        )

    test("round-trip MsgProposeVersions with v14 only") {
        roundTrip(MsgProposeVersions(VersionTable(VersionNumber.V14 -> v14Data())))
    }

    test("round-trip MsgProposeVersions with v14 + v16 mixed shapes") {
        roundTrip(
          MsgProposeVersions(
            VersionTable(
              VersionNumber.V14 -> v14Data(),
              VersionNumber.V16 -> v16Data()
            )
          )
        )
    }

    test("round-trip MsgAcceptVersion at v14") {
        roundTrip(MsgAcceptVersion(VersionNumber.V14, v14Data(magic = NetworkMagic.Mainnet.value)))
    }

    test("round-trip MsgAcceptVersion at v16 with perasSupport=true") {
        val data = NodeToNodeVersionData.V16(
          networkMagic = NetworkMagic.Mainnet,
          initiatorOnlyDiffusionMode = false,
          peerSharing = 1,
          query = true,
          perasSupport = true
        )
        roundTrip(MsgAcceptVersion(VersionNumber.V16, data))
    }

    test("round-trip MsgRefuse/VersionMismatch") {
        roundTrip(MsgRefuse(RefuseReason.VersionMismatch(List(14, 16))))
    }

    test("round-trip MsgRefuse/VersionMismatch with empty list") {
        roundTrip(MsgRefuse(RefuseReason.VersionMismatch(Nil)))
    }

    test("round-trip MsgRefuse/HandshakeDecodeError") {
        roundTrip(MsgRefuse(RefuseReason.HandshakeDecodeError(14, "bad cbor")))
    }

    test("round-trip MsgRefuse/Refused") {
        roundTrip(
          MsgRefuse(RefuseReason.Refused(16, "magic mismatch: expected 764824073, got 42"))
        )
    }

    test("round-trip MsgQueryReply") {
        roundTrip(MsgQueryReply(VersionTable(VersionNumber.V16 -> v16Data())))
    }

    // ----------------------------------------------------------------------------------------
    // Golden vectors — hand-crafted from the CDDL; the assertion on the wire bytes guards
    // against silent codec regressions.
    // ----------------------------------------------------------------------------------------

    test("golden: MsgProposeVersions(v14 only, Preview magic)") {
        // [0, {14: [2, true, 0, false]}]
        //
        // 82       array(2)
        //   00     uint 0                          ; tag for MsgProposeVersions
        //   A1     map(1)
        //     0E   uint 14                         ; key: version 14
        //     84   array(4)
        //       02 uint 2                          ; networkMagic Preview
        //       F5 true                            ; initiatorOnly
        //       00 uint 0                          ; peerSharing
        //       F4 false                           ; query
        val expected = "82 00 a1 0e 84 02 f5 00 f4".filter(_ != ' ')
        val m: HandshakeMessage = MsgProposeVersions(VersionTable(VersionNumber.V14 -> v14Data()))
        val bytes = Cbor.encode[HandshakeMessage](m).toByteArray
        assert(
          bytes.toHex == expected,
          s"expected=$expected\n     got=${bytes.toHex}"
        )
        // And the reverse direction, so a hand-edit to the bytes surfaces here too.
        val decoded = Cbor.decode(expected.hexToBytes).to[HandshakeMessage].value
        assert(decoded == m)
    }

    test("golden: MsgAcceptVersion at v16 (Mainnet)") {
        // [1, 16, [764824073, true, 0, false, false]]
        //
        // 83          array(3)
        //   01        uint 1                       ; tag for MsgAcceptVersion
        //   10        uint 16                      ; versionNumber
        //   85        array(5)
        //     1A 2D96 4A09  uint 764824073         ; Mainnet magic (0x2D964A09)
        //     F5        true
        //     00        uint 0
        //     F4        false
        //     F4        false
        val expected = "83 01 10 85 1a 2d 96 4a 09 f5 00 f4 f4".filter(_ != ' ')
        val m: HandshakeMessage =
            MsgAcceptVersion(VersionNumber.V16, v16Data(magic = NetworkMagic.Mainnet.value))
        val bytes = Cbor.encode[HandshakeMessage](m).toByteArray
        assert(
          bytes.toHex == expected,
          s"expected=$expected\n     got=${bytes.toHex}"
        )
    }

    test("golden: MsgRefuse/VersionMismatch([14, 16])") {
        // [2, [0, [14, 16]]]
        //
        // 82             array(2)
        //   02           uint 2                    ; tag for MsgRefuse
        //   82           array(2)                  ; refuseReasonVersionMismatch
        //     00         uint 0                    ; tag for VersionMismatch
        //     82         array(2)
        //       0E       uint 14
        //       10       uint 16
        val expected = "82 02 82 00 82 0e 10".filter(_ != ' ')
        val m: HandshakeMessage = MsgRefuse(RefuseReason.VersionMismatch(List(14, 16)))
        val bytes = Cbor.encode[HandshakeMessage](m).toByteArray
        assert(bytes.toHex == expected)
    }

    // ----------------------------------------------------------------------------------------
    // Error paths — verify malformed input is rejected.
    // ----------------------------------------------------------------------------------------

    test("decode rejects unknown tag") {
        // [7, ...] — tag 7 is not a valid handshake message tag
        val bytes = Array[Byte](0x82.toByte, 0x07, 0x00)
        val ex = intercept[Exception] { Cbor.decode(bytes).to[HandshakeMessage].value }
        assert(ex.getMessage.contains("tag=7") || ex.getMessage.toLowerCase.contains("handshake"))
    }

    test("decode rejects wrong-shape versionData for version (5 fields at v14)") {
        // MsgAcceptVersion with version=14 but 5-element data (v16 shape).
        // 83 01 0E 85 02 F5 00 F4 F4
        val bytes = Array[Byte](
          0x83.toByte,
          0x01,
          0x0e,
          0x85.toByte,
          0x02,
          0xf5.toByte,
          0x00,
          0xf4.toByte,
          0xf4.toByte
        )
        val ex = intercept[Exception] { Cbor.decode(bytes).to[HandshakeMessage].value }
        assert(ex.getMessage.toLowerCase.contains("versiondata"))
    }
}
