package scalus.cardano.network

import scalus.cardano.network.chainsync.Tip
import scalus.cardano.node.stream.ChainPoint
import scalus.uplc.builtin.ByteString

/** Typed failures that can surface from the chain-sync / block-fetch state machines or the chain
  * applier. Sealed so pattern matches are exhaustive at every handling site.
  *
  * All variants are subtypes of `RuntimeException` so they can be carried through
  * `CancelSource.cancel(cause)` and surfaced on subscriber mailboxes via `Mailbox.fail`.
  */
sealed abstract class ChainSyncError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object ChainSyncError {

    /** The peer accepted no point in our `MsgFindIntersect` candidate list. The peer's current tip
      * is carried so diagnostics can show how far apart we are.
      *
      * Surfaces at `ChainSyncDriver.findIntersect`; in M5 this fails the provider construction
      * future (we don't attempt recovery).
      */
    final case class NoIntersection(peerTip: Tip)
        extends ChainSyncError(s"peer found no intersection; peer tip = $peerTip")

    /** BlockFetch returned [[scalus.cardano.network.blockfetch.BlockFetchMessage.MsgNoBlocks]] for
      * a point the peer just advertised on chain-sync. Indicates a peer-side rollback raced our
      * fetch. The applier may choose to continue (next chain-sync event will be the rollback) or
      * escalate, per the *Unrecoverable-failure flow* section of the design doc.
      */
    final case class MissingBlock(point: ChainPoint)
        extends ChainSyncError(s"peer returned MsgNoBlocks for $point")

    /** Envelope decoded as Byron (era 0) but M5 has no Shelley-style decoder for it. The block
      * cannot be projected into an `AppliedBlock`; connection-fatal by default. Documented
      * behaviour — see the M5 design doc's *Header & block decoding* section.
      */
    final case class ByronEra(slot: Long)
        extends ChainSyncError(s"Byron-era block encountered at slot $slot; decoder not available")

    /** CBOR decode failure on a chain-sync or block-fetch message, or on the contents of the
      * era-tagged envelope. Carries the inner CBOR bytes (truncated) to help triage.
      */
    final case class Decode(where: String, cause: Throwable, sampleBytes: Option[ByteString] = None)
        extends ChainSyncError(s"CBOR decode failure in $where", cause)

    /** The applier advertised a block via chain-sync but BlockFetch returned a different slot /
      * hash than expected. Usually a peer protocol bug; fatal.
      */
    final case class BlockMismatch(expected: ChainPoint, actual: ChainPoint)
        extends ChainSyncError(s"block-fetch delivered $actual for expected $expected")
}
