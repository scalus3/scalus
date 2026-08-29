package scalus.cardano.ledger

/** Shared bounds checks for ledger types.
  */
private[ledger] object LedgerBounds {

    /** The ledger bounds `url` and `dns_name` by UTF-8 BYTE length, not character count:
      * `textSizeN` uses `lengthWord8` and is documented as "text with byte-length bounds"
      * (`libs/cardano-ledger-core/src/Cardano/Ledger/BaseTypes.hs:643-657`), and the CDDL says
      * `text .size (0 .. 128)` (`conway.cddl:489`, `:496`). `String.length` counts UTF-16 units,
      * which is never larger, so it must not be used for this.
      *
      * The bound is 128 from decoder version 9; earlier versions used 64, which is moot since we
      * support protocol version 10 and above.
      */
    def requireTextBytes(what: String, text: String, maxBytes: Int): Unit = {
        val size = text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
        require(size <= maxBytes, s"$what must be at most $maxBytes UTF-8 bytes, got $size")
    }
}
