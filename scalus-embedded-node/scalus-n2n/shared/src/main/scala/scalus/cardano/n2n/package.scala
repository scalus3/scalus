package scalus.cardano

/** Ouroboros-network Node-to-Node transport.
  *
  * Milestone M4 — see `docs/local/claude/indexer/n2n-transport.md`. Delivers the SDU multiplexer,
  * handshake (v14/v16), and keep-alive mini-protocol; chain-sync, block-fetch and `TxSubmission2`
  * are M5+. Platform-neutral types live here; JVM-specific byte-channel impl (NIO2) lives under
  * `scalus.cardano.n2n.jvm` in the jvm/ source set. Cancellation primitives and the platform
  * `Timer` are in [[scalus.cardano.infra]] (scalus-streaming-core).
  */
package object n2n
