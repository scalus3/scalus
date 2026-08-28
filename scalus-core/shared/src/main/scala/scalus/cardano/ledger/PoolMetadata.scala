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
  *   checks no length (State/StakePool.hs:293-296, :522-524). It is therefore a plain `ByteString`,
  *   not a fixed-width `MetadataHash`; typing it as the latter made us reject pool registrations
  *   the chain accepts.
  */
case class PoolMetadata(url: String, metadataHash: ByteString) derives Codec {
    // Validate URL length
    // The ledger bounds these by UTF-8 BYTE length, not character count: `textSizeN` uses
    // `lengthWord8` and the CDDL says `text .size (0 .. 128)`
    // (BaseTypes.hs:643-657, conway.cddl:489/496). Scala's String.length counts UTF-16 units,
    // which is <= the UTF-8 byte length for any non-ASCII text, so checking it accepted values
    // the chain rejects. (128 applies from decoder version 9; earlier versions used 64, which
    // is moot since we support protocol version 10 and above.)
    require(
      url.getBytes(java.nio.charset.StandardCharsets.UTF_8).length <= 128,
      s"URL must be at most 128 UTF-8 bytes, got ${url.getBytes(java.nio.charset.StandardCharsets.UTF_8).length}"
    )
}
