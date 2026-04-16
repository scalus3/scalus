package scalus.cardano

/** Ouroboros-network Node-to-Node transport.
  *
  * Milestone M4 — see `docs/local/claude/indexer/n2n-transport.md`. Delivers the SDU
  * multiplexer, handshake (v14/v16), and keep-alive mini-protocol; chain-sync, block-fetch and
  * `TxSubmission2` are M5+. Platform-neutral types live here; JVM-specific impls (NIO2 byte
  * channel, scheduled-executor timer) live under `scalus.cardano.n2n.jvm` in the jvm/ source
  * set.
  */
package object n2n
