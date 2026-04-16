package scalus.cardano.n2n

/** Ouroboros Node-to-Node mini-protocol identifiers, carried in the low 15 bits of the mux SDU
  * header's protocol field. The enum variants correspond 1:1 to the wire numbers defined in
  * ouroboros-network's `network-mux` / `cardano-diffusion` protocol tables.
  *
  * M4 implements [[Handshake]] and [[KeepAlive]] end-to-end. The other variants exist so the mux
  * can recognise and route frames for them even before their state machines land — makes wire-level
  * debugging easier and lets M5+ plug in without an enum change.
  */
enum MiniProtocolId(val wire: Int) {
    case Handshake extends MiniProtocolId(0)
    case ChainSync extends MiniProtocolId(2)
    case BlockFetch extends MiniProtocolId(3)
    case TxSubmission extends MiniProtocolId(4)
    case KeepAlive extends MiniProtocolId(8)
    case PeerSharing extends MiniProtocolId(10)
}

object MiniProtocolId {

    /** Lookup by wire number. Returns `None` for unknown protocols — the mux treats an unknown
      * inbound protocol-ID as a wire-level failure and fires the connection-root cancel.
      */
    def fromWire(wire: Int): Option[MiniProtocolId] = values.find(_.wire == wire)
}
