package scalus.cardano.network

import io.bullet.borer.Cbor
import scalus.cardano.ledger.{Block, BlockHeader, KeepRaw, OriginalCborByteArray}
import scalus.uplc.builtin.ByteString

/** Cardano HardFork-combinator era tag carried by chain-sync headers and block-fetch blocks.
  *
  * Era indices match `ouroboros-consensus-cardano`'s `CardanoEras` ordering: Byron first, then
  * the Shelley family in release order. M5 decodes only Shelley+ (indices 1–6). Later eras are
  * accepted as `Unknown(idx)` so a new era beyond Conway doesn't break the driver immediately —
  * the applier still fails with a typed error because it has no ledger type for the new era.
  */
enum Era(val wire: Int):
    case Byron extends Era(0)
    case Shelley extends Era(1)
    case Allegra extends Era(2)
    case Mary extends Era(3)
    case Alonzo extends Era(4)
    case Babbage extends Era(5)
    case Conway extends Era(6)
    case Unknown(idx: Int) extends Era(idx)

object Era {

    /** Decode a wire era index. Returns the matching variant; unknown indices yield
      * [[Era.Unknown]] rather than throwing so callers can surface a typed error.
      */
    def fromWire(wire: Int): Era = wire match {
        case 0     => Byron
        case 1     => Shelley
        case 2     => Allegra
        case 3     => Mary
        case 4     => Alonzo
        case 5     => Babbage
        case 6     => Conway
        case other => Unknown(other)
    }
}

/** Era-aware decoder for the inner payload of the HardFork envelope.
  *
  * The transport layer ([[chainsync.ChainSyncMessage]], [[blockfetch.BlockFetchMessage]]) already
  * strips the envelope and hands us `(era, bytes)` pairs where `bytes` is the CBOR of the
  * era-specific header or block. This object dispatches on the era tag to run
  * `scalus-cardano-ledger`'s `BlockHeader` / `Block` decoder, returning a
  * [[scalus.cardano.ledger.KeepRaw]] so callers retain the original bytes (needed for computing
  * the header hash via Blake2b-256).
  *
  * Shelley+ eras (1–6) share a single `BlockHeader` / `Block` decoder in `scalus-core`.
  * Intra-era differences (Mary assets, Alonzo Plutus, Babbage inline datums, Conway governance)
  * live inside the transaction body shape handled by the existing decoders. Byron (era 0) has a
  * distinct CBOR shape not modelled in `scalus-core`; M5 fails fast with
  * [[ChainSyncError.ByronEra]]. Unknown eras surface as [[ChainSyncError.Decode]].
  */
object BlockEnvelope {

    /** Decode the era-specific header bytes. The returned [[KeepRaw]] preserves the original
      * bytes for hash computation — the caller's `ChainApplier` hashes `.raw` with Blake2b-256
      * to obtain the [[scalus.cardano.ledger.BlockHash]] used by `ChainPoint`.
      */
    def decodeHeader(
        era: Era,
        headerBytes: ByteString
    ): Either[ChainSyncError, KeepRaw[BlockHeader]] = era match {
        case Era.Byron =>
            // Byron uses a different CBOR shape. We can't extract a slot without parsing, so
            // report slot = -1 in the error; the driver can enrich with a known slot if it
            // correlates with the tip.
            Left(ChainSyncError.ByronEra(-1L))
        case Era.Unknown(idx) =>
            Left(
              ChainSyncError.Decode(
                s"unknown era index $idx in header envelope",
                cause = null,
                sampleBytes = Some(truncate(headerBytes))
              )
            )
        case _ =>
            given OriginalCborByteArray = OriginalCborByteArray(headerBytes.bytes)
            try Right(Cbor.decode(headerBytes.bytes).to[KeepRaw[BlockHeader]].value)
            catch {
                case t: Throwable =>
                    Left(
                      ChainSyncError.Decode(
                        "BlockHeader (era-specific payload)",
                        cause = t,
                        sampleBytes = Some(truncate(headerBytes))
                      )
                    )
            }
    }

    /** Decode the era-specific block bytes. Same era-dispatch policy as [[decodeHeader]]. */
    def decodeBlock(
        era: Era,
        blockBytes: ByteString
    ): Either[ChainSyncError, KeepRaw[Block]] = era match {
        case Era.Byron =>
            Left(ChainSyncError.ByronEra(-1L))
        case Era.Unknown(idx) =>
            Left(
              ChainSyncError.Decode(
                s"unknown era index $idx in block envelope",
                cause = null,
                sampleBytes = Some(truncate(blockBytes))
              )
            )
        case _ =>
            given OriginalCborByteArray = OriginalCborByteArray(blockBytes.bytes)
            try Right(Cbor.decode(blockBytes.bytes).to[KeepRaw[Block]].value)
            catch {
                case t: Throwable =>
                    Left(
                      ChainSyncError.Decode(
                        "Block (era-specific payload)",
                        cause = t,
                        sampleBytes = Some(truncate(blockBytes))
                      )
                    )
            }
    }

    private def truncate(bs: ByteString, max: Int = 64): ByteString =
        if bs.size <= max then bs
        else ByteString.fromArray(bs.bytes.slice(0, max))
}
