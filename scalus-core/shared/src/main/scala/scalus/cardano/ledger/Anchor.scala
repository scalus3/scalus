package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import org.typelevel.paiges.Doc
import scalus.utils.{Pretty, Style}

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
    // The ledger bounds these by UTF-8 BYTE length, not character count: `textSizeN` uses
    // `lengthWord8` and the CDDL says `text .size (0 .. 128)`
    // (BaseTypes.hs:643-657, conway.cddl:489/496). Scala's String.length counts UTF-16 units,
    // which is <= the UTF-8 byte length for any non-ASCII text, so checking it accepted values
    // the chain rejects. (128 applies from decoder version 9; earlier versions used 64, which
    // is moot since we support protocol version 10 and above.)
    require(
      url.getBytes(java.nio.charset.StandardCharsets.UTF_8).length <= 128,
      s"Anchor URL must be at most 128 UTF-8 bytes, got ${url.getBytes(java.nio.charset.StandardCharsets.UTF_8).length}"
    )

    override def toString: String = s"Anchor($url, ${dataHash.toHex})"

object Anchor:
    import Doc.*
    import Pretty.inParens

    /** Ordering matches Haskell's derived Ord: compare url first, then dataHash */
    given Ordering[Anchor] = Ordering.by(a => (a.url, a.dataHash))

    /** Pretty prints Anchor as url + hash */
    given Pretty[Anchor] with
        def pretty(a: Anchor, style: Style): Doc =
            text("Anchor") + inParens(text(a.url) + text(", ") + text(a.dataHash.toHex))
