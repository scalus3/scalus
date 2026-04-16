package scalus.cardano.n2n

import org.scalatest.funsuite.AnyFunSuite

class MiniProtocolIdSuite extends AnyFunSuite {

    test("wire numbers match the ouroboros-network protocol table") {
        assert(MiniProtocolId.Handshake.wire == 0)
        assert(MiniProtocolId.ChainSync.wire == 2)
        assert(MiniProtocolId.BlockFetch.wire == 3)
        assert(MiniProtocolId.TxSubmission.wire == 4)
        assert(MiniProtocolId.KeepAlive.wire == 8)
        assert(MiniProtocolId.PeerSharing.wire == 10)
    }

    test("fromWire round-trips every enum variant") {
        for v <- MiniProtocolId.values do
            assert(MiniProtocolId.fromWire(v.wire).contains(v), s"$v did not round-trip")
    }

    test("fromWire returns None for unassigned wire numbers") {
        assert(MiniProtocolId.fromWire(1).isEmpty) // unassigned
        assert(MiniProtocolId.fromWire(5).isEmpty)
        assert(MiniProtocolId.fromWire(999).isEmpty)
    }
}
