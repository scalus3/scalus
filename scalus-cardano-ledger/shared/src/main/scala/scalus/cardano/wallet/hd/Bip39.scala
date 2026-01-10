package scalus.cardano.wallet.hd

import scalus.builtin.{platform, ByteString}
import scalus.crypto.Pbkdf2

/** BIP-39 mnemonic sentence handling for Cardano wallets.
  *
  * Implements mnemonic validation and seed derivation per BIP-39. Uses English wordlist (2048
  * words).
  *
  * @see
  *   https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
  */
object Bip39 {

    /** Validate a mnemonic sentence.
      *
      * @param mnemonic
      *   space-separated words
      * @return
      *   true if valid (correct word count, all words in wordlist, valid checksum)
      */
    def isValidMnemonic(mnemonic: String): Boolean = {
        val words = normalizeWords(mnemonic)
        val wordCount = words.length

        // Valid word counts: 12, 15, 18, 21, 24
        if !Set(12, 15, 18, 21, 24).contains(wordCount) then return false

        // Check all words are in wordlist
        val indices = words.map(w => Bip39Wordlist.indexOf(w))
        if indices.exists(_ < 0) then return false

        // Verify checksum
        verifyChecksum(indices, wordCount)
    }

    /** Convert mnemonic to entropy bytes.
      *
      * @param mnemonic
      *   validated mnemonic sentence
      * @return
      *   entropy bytes (16-32 bytes depending on word count)
      * @throws IllegalArgumentException
      *   if mnemonic is invalid
      */
    def mnemonicToEntropy(mnemonic: String): Array[Byte] = {
        val words = normalizeWords(mnemonic)
        val wordCount = words.length

        require(
          Set(12, 15, 18, 21, 24).contains(wordCount),
          s"Invalid word count: $wordCount. Must be 12, 15, 18, 21, or 24."
        )

        val indices = words.map(w => Bip39Wordlist.indexOf(w))
        require(!indices.exists(_ < 0), "Mnemonic contains invalid word(s)")
        require(verifyChecksum(indices, wordCount), "Invalid mnemonic checksum")

        // Convert indices to bits
        // Each word = 11 bits, total bits = wordCount * 11
        // Entropy bits = total bits - checksum bits
        // Checksum bits = entropy bits / 32
        // So: entropy bits = wordCount * 11 * 32 / 33
        val totalBits = wordCount * 11
        val checksumBits = wordCount / 3 // 4 for 12 words, 8 for 24 words, etc.
        val entropyBits = totalBits - checksumBits
        val entropyBytes = entropyBits / 8

        // Pack indices into bits
        val allBits = new Array[Boolean](totalBits)
        var bitIndex = 0
        for idx <- indices do
            var i = 10
            while i >= 0 do
                allBits(bitIndex) = ((idx >> i) & 1) == 1
                bitIndex += 1
                i -= 1

        // Extract entropy bytes
        val entropy = new Array[Byte](entropyBytes)
        var byteIndex = 0
        while byteIndex < entropyBytes do
            var byte = 0
            var bit = 0
            while bit < 8 do
                if allBits(byteIndex * 8 + bit) then byte |= (1 << (7 - bit))
                bit += 1
            entropy(byteIndex) = byte.toByte
            byteIndex += 1

        entropy
    }

    /** Convert mnemonic + passphrase to 64-byte seed.
      *
      * Uses PBKDF2-HMAC-SHA512 with 2048 iterations.
      *
      * @param mnemonic
      *   the mnemonic sentence (must be valid per BIP-39)
      * @param passphrase
      *   optional passphrase (empty string if none)
      * @return
      *   64-byte seed
      * @throws IllegalArgumentException
      *   if mnemonic is invalid (wrong word count, unknown words, or bad checksum)
      */
    def mnemonicToSeed(mnemonic: String, passphrase: String = ""): Array[Byte] = {
        require(
          isValidMnemonic(mnemonic),
          "Invalid mnemonic: checksum verification failed or contains invalid words"
        )
        val normalizedMnemonic = normalizeMnemonic(mnemonic)
        val salt = ("mnemonic" + passphrase).getBytes("UTF-8")
        Pbkdf2.deriveKey(normalizedMnemonic.getBytes("UTF-8"), salt, 2048, 64)
    }

    /** Generate mnemonic from entropy.
      *
      * @param entropy
      *   entropy bytes (16, 20, 24, 28, or 32 bytes)
      * @return
      *   mnemonic sentence
      */
    def entropyToMnemonic(entropy: Array[Byte]): String = {
        val entropyBits = entropy.length * 8
        require(
          Set(128, 160, 192, 224, 256).contains(entropyBits),
          s"Invalid entropy size: ${entropy.length} bytes. Must be 16, 20, 24, 28, or 32."
        )

        // Calculate checksum (BIP-39 uses SHA-256)
        val checksum = sha256Checksum(entropy)
        val checksumBits = entropyBits / 32

        // Combine entropy and checksum bits
        val totalBits = entropyBits + checksumBits
        val allBits = new Array[Boolean](totalBits)

        // Copy entropy bits
        var bitIndex = 0
        for b <- entropy do
            var i = 7
            while i >= 0 do
                allBits(bitIndex) = ((b >> i) & 1) == 1
                bitIndex += 1
                i -= 1

        // Copy checksum bits
        var checksumBitIndex = 0
        while checksumBitIndex < checksumBits do
            val byteIndex = checksumBitIndex / 8
            val bitInByte = 7 - (checksumBitIndex % 8)
            allBits(bitIndex) = ((checksum(byteIndex) >> bitInByte) & 1) == 1
            bitIndex += 1
            checksumBitIndex += 1

        // Convert to word indices (11 bits each)
        val wordCount = totalBits / 11
        val words = new Array[String](wordCount)
        var wordIndex = 0
        bitIndex = 0
        while wordIndex < wordCount do
            var idx = 0
            var bit = 0
            while bit < 11 do
                if allBits(bitIndex) then idx |= (1 << (10 - bit))
                bitIndex += 1
                bit += 1
            words(wordIndex) = Bip39Wordlist.word(idx)
            wordIndex += 1

        words.mkString(" ")
    }

    private def normalizeWords(mnemonic: String): Array[String] =
        mnemonic.trim.toLowerCase.split("\\s+")

    private def normalizeMnemonic(mnemonic: String): String =
        normalizeWords(mnemonic).mkString(" ")

    private def verifyChecksum(indices: Array[Int], wordCount: Int): Boolean = {
        // Convert indices back to bits and verify checksum
        val totalBits = wordCount * 11
        val checksumBits = wordCount / 3
        val entropyBits = totalBits - checksumBits
        val entropyBytes = entropyBits / 8

        // Pack indices into bits
        val allBits = new Array[Boolean](totalBits)
        var bitIndex = 0
        for idx <- indices do
            var i = 10
            while i >= 0 do
                allBits(bitIndex) = ((idx >> i) & 1) == 1
                bitIndex += 1
                i -= 1

        // Extract entropy bytes
        val entropy = new Array[Byte](entropyBytes)
        var byteIndex = 0
        while byteIndex < entropyBytes do
            var byte = 0
            var bit = 0
            while bit < 8 do
                if allBits(byteIndex * 8 + bit) then byte |= (1 << (7 - bit))
                bit += 1
            entropy(byteIndex) = byte.toByte
            byteIndex += 1

        // Calculate expected checksum
        val hash = sha256Checksum(entropy)

        // Extract provided checksum bits
        var providedChecksum = 0
        bitIndex = entropyBits
        var i = 0
        while i < checksumBits do
            if allBits(bitIndex) then providedChecksum |= (1 << (checksumBits - 1 - i))
            bitIndex += 1
            i += 1

        // Extract expected checksum bits from hash
        var expectedChecksum = 0
        i = 0
        while i < checksumBits do
            val byteIdx = i / 8
            val bitInByte = 7 - (i % 8)
            if ((hash(byteIdx) >> bitInByte) & 1) == 1 then
                expectedChecksum |= (1 << (checksumBits - 1 - i))
            i += 1

        providedChecksum == expectedChecksum
    }

    /** Compute SHA-256 checksum for BIP-39. */
    private def sha256Checksum(data: Array[Byte]): Array[Byte] =
        platform.sha2_256(ByteString.unsafeFromArray(data)).bytes
}
