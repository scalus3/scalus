package scalus.cardano.network.infra

/** Direction bit on the mux SDU header. `Initiator` frames flow from the peer that originally
  * opened the mini-protocol (we, as the client); `Responder` frames flow the other way. The bit is
  * the high bit of the 16-bit protocol-id field.
  */
enum Direction(val bit: Int) {
    case Initiator extends Direction(0)
    case Responder extends Direction(1)
}

object Direction {

    /** Decode from the direction bit. Any non-zero bit is treated as [[Responder]]. */
    def fromBit(bit: Int): Direction = if (bit & 1) == 0 then Initiator else Responder
}
