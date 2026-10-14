package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Reader}

/** Reads a CBOR map as the ledger's `decodeMap` and `decodeMapByKey` read it
  * (cardano-ledger-binary, Decoding/Decoder.hs): from protocol version 9 a repeated key fails
  * decoding; before it, the last value for a key wins, as Haskell's `Map.fromList` keeps it.
  * borer's own `Map` decoder always keeps the last value.
  */
private[ledger] object LedgerMap {

    /** Reads a map and fails at the first repeated key. For types that exist only from protocol
      * version 9, such as governance actions and votes.
      */
    def readDistinct[K: Decoder, V: Decoder](r: Reader): Map[K, V] = {
        var map = Map.empty[K, V]
        def readEntry(): Unit = {
            val key = r.read[K]()
            if map.contains(key) then r.validationFailure(s"Repeated map key $key")
            map = map.updated(key, r.read[V]())
        }
        if r.hasMapHeader then
            val size = r.readMapHeader()
            var i = 0L
            while i < size do
                readEntry()
                i += 1
        else if r.hasMapStart then
            r.readMapStart()
            while !r.tryReadBreak() do readEntry()
        else r.unexpectedDataItem(expected = "Map")
        map
    }

    /** [[readDistinct]] from protocol version 9; before it, a repeated key keeps its last value. */
    def read[K: Decoder, V: Decoder](r: Reader)(using pv: ProtocolVersion): Map[K, V] =
        if pv >= ProtocolVersion.conwayPV then readDistinct[K, V](r) else r.read[Map[K, V]]()
}
