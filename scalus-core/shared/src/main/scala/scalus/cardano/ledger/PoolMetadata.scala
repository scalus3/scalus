package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import scalus.uplc.builtin.ByteString

/** Represents metadata for a stake pool in the Cardano blockchain.
  *
  * Pool metadata provides information about the pool such as name, description, ticker symbol, etc.
  * It's stored off-chain, and only the URL and hash are included in the blockchain.
  *
  * @param url
  *   URL where the metadata can be found
  * @param metadataHash
  *   Hash of the metadata for verification. Conventionally a 32-byte Blake2b-256 digest, but the
  *   ledger does not constrain it: the CDDL says `pool_metadata = [url, bytes]` (conway.cddl:494,
  *   unchanged since Allegra) and Haskell holds it as `pmHash :: !ByteArray` with a decoder that
  *   checks no length (State/StakePool.hs:293-296, :522-524). Hence a plain `ByteString` rather
  *   than a fixed-width `MetadataHash`.
  */
case class PoolMetadata(url: String, metadataHash: ByteString) derives Codec {
    // Validate URL length
    LedgerBounds.requireTextBytes("URL", url, 128)
}
