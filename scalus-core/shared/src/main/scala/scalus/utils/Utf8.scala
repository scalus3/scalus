package scalus.utils

/** UTF-8 as the Plutus reference reads it. */
private[scalus] object Utf8 {

    /** Whether `bytes` are well-formed UTF-8 in the sense of Table 3-7 of the Unicode standard: no
      * overlong forms, no surrogates (U+D800 to U+DFFF), nothing above U+10FFFF and no truncated
      * sequence. This is what `decodeUtf8'` of the Haskell `text` package accepts.
      *
      * It is written out so that JVM, Scala.js and Scala Native decide alike, without depending on
      * how each platform's `CharsetDecoder` treats malformed input. The flat decoder of `string`
      * constants and the `decodeUtf8` builtin both use it.
      */
    def isWellFormed(bytes: Array[Byte]): Boolean = {
        def continuation(at: Int, low: Int = 0x80, high: Int = 0xbf): Boolean =
            at < bytes.length && (bytes(at) & 0xff) >= low && (bytes(at) & 0xff) <= high

        var i = 0
        var ok = true
        while ok && i < bytes.length do
            val b = bytes(i) & 0xff
            if b <= 0x7f then i += 1
            else if b >= 0xc2 && b <= 0xdf then
                ok = continuation(i + 1)
                i += 2
            else if b >= 0xe0 && b <= 0xef then
                // E0 needs A0..BF behind it (else overlong), ED needs 80..9F (else a surrogate)
                val low = if b == 0xe0 then 0xa0 else 0x80
                val high = if b == 0xed then 0x9f else 0xbf
                ok = continuation(i + 1, low, high) && continuation(i + 2)
                i += 3
            else if b >= 0xf0 && b <= 0xf4 then
                // F0 needs 90..BF behind it (else overlong), F4 needs 80..8F (else above U+10FFFF)
                val low = if b == 0xf0 then 0x90 else 0x80
                val high = if b == 0xf4 then 0x8f else 0xbf
                ok = continuation(i + 1, low, high) && continuation(i + 2) && continuation(i + 3)
                i += 4
            // C0, C1 (overlong) and F5..FF (above U+10FFFF) never start a sequence, and 80..BF
            // only continue one
            else ok = false
        ok
    }
}
