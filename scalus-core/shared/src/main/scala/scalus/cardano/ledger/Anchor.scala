package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents an anchor in the Cardano blockchain.
  *
  * An anchor contains a URL and a data hash that can be used to verify the data retrieved from the
  * URL.
  *
  * @param url
  *   The URL where the data can be retrieved
  * @param dataHash
  *   A 32-byte hash of the data
  */
case class Anchor(url: String, dataHash: DataHash) derives Codec:
    /** Validate the URL length */
    require(url.length <= 128, s"Anchor URL must be at most 128 characters, got ${url.length}")

    override def toString: String = s"Anchor($url, ${dataHash.toHex})"

object Anchor:
    /** Ordering matches Haskell's derived Ord: compare url first, then dataHash */
    given Ordering[Anchor] = Ordering.by(a => (a.url, a.dataHash))
