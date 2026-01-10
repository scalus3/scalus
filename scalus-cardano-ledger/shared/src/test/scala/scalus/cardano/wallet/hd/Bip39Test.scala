package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex
import scalus.utils.Hex.toHex

/** BIP-39 test vectors from the BIP-39 specification.
  *
  * @see
  *   https://github.com/trezor/python-mnemonic/blob/master/vectors.json
  */
class Bip39Test extends AnyFunSuite {

    test("Bip39Wordlist has 2048 words") {
        assert(Bip39Wordlist.size == 2048)
    }

    test("Bip39Wordlist first and last words") {
        assert(Bip39Wordlist.word(0) == "abandon")
        assert(Bip39Wordlist.word(2047) == "zoo")
    }

    test("Bip39Wordlist indexOf") {
        assert(Bip39Wordlist.indexOf("abandon") == 0)
        assert(Bip39Wordlist.indexOf("zoo") == 2047)
        assert(Bip39Wordlist.indexOf("ability") == 1)
        assert(Bip39Wordlist.indexOf("notaword") == -1)
    }

    // Test vectors from https://github.com/trezor/python-mnemonic/blob/master/vectors.json
    // Format: (entropy_hex, mnemonic, seed_hex with passphrase "TREZOR")

    test("BIP-39 vector: 128-bit entropy (12 words)") {
        val entropyHex = "00000000000000000000000000000000"
        val expectedMnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val expectedSeedHex =
            "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"

        val entropy = Hex.hexToBytes(entropyHex)
        val mnemonic = Bip39.entropyToMnemonic(entropy)
        assert(mnemonic == expectedMnemonic)

        assert(Bip39.isValidMnemonic(mnemonic))

        val extractedEntropy = Bip39.mnemonicToEntropy(mnemonic)
        assert(extractedEntropy.toHex == entropyHex)

        val seed = Bip39.mnemonicToSeed(mnemonic, "TREZOR")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 vector: 128-bit entropy variant") {
        val entropyHex = "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"
        val expectedMnemonic =
            "legal winner thank year wave sausage worth useful legal winner thank yellow"
        val expectedSeedHex =
            "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c80937a1c1069be3a3a5bd381ee6260e8d9739fce1f607"

        val entropy = Hex.hexToBytes(entropyHex)
        val mnemonic = Bip39.entropyToMnemonic(entropy)
        assert(mnemonic == expectedMnemonic)

        assert(Bip39.isValidMnemonic(mnemonic))

        val seed = Bip39.mnemonicToSeed(mnemonic, "TREZOR")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 vector: 256-bit entropy (24 words)") {
        val entropyHex = "0000000000000000000000000000000000000000000000000000000000000000"
        val expectedMnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
        val expectedSeedHex =
            "bda85446c68413707090a52022edd26a1c9462295029f2e60cd7c4f2bbd3097170af7a4d73245cafa9c3cca8d561a7c3de6f5d4a10be8ed2a5e608d68f92fcc8"

        val entropy = Hex.hexToBytes(entropyHex)
        val mnemonic = Bip39.entropyToMnemonic(entropy)
        assert(mnemonic == expectedMnemonic)

        assert(Bip39.isValidMnemonic(mnemonic))

        val extractedEntropy = Bip39.mnemonicToEntropy(mnemonic)
        assert(extractedEntropy.toHex == entropyHex)

        val seed = Bip39.mnemonicToSeed(mnemonic, "TREZOR")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 vector: 256-bit entropy variant") {
        val entropyHex = "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"
        val expectedMnemonic =
            "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title"
        val expectedSeedHex =
            "bc09fca1804f7e69da93c2f2028eb238c227f2e9dda30cd63699232578480a4021b146ad717fbb7e451ce9eb835f43620bf5c514db0f8add49f5d121449d3e87"

        val entropy = Hex.hexToBytes(entropyHex)
        val mnemonic = Bip39.entropyToMnemonic(entropy)
        assert(mnemonic == expectedMnemonic)

        assert(Bip39.isValidMnemonic(mnemonic))

        val seed = Bip39.mnemonicToSeed(mnemonic, "TREZOR")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 vector: random entropy") {
        val entropyHex = "68a79eaca2324873eacc50cb9c6eca8cc68ea5d936f98787c60c7ebc74e6ce7c"
        val expectedMnemonic =
            "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel tennis maple dilemma loan word shrug inflict delay length"
        val expectedSeedHex =
            "64c87cde7e12ecf6704ab95bb1408bef047c22db4cc7491c4271d170a1b213d20b385bc1588d9c7b38f1b39d415665b8a9030c9ec653d75e65f847d8fc1fc440"

        val entropy = Hex.hexToBytes(entropyHex)
        val mnemonic = Bip39.entropyToMnemonic(entropy)
        assert(mnemonic == expectedMnemonic)

        assert(Bip39.isValidMnemonic(mnemonic))

        val seed = Bip39.mnemonicToSeed(mnemonic, "TREZOR")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 invalid mnemonic - wrong word count") {
        val invalidMnemonic = "abandon abandon abandon"
        assert(!Bip39.isValidMnemonic(invalidMnemonic))
    }

    test("BIP-39 invalid mnemonic - invalid word") {
        val invalidMnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon notaword"
        assert(!Bip39.isValidMnemonic(invalidMnemonic))
    }

    test("BIP-39 invalid mnemonic - wrong checksum") {
        val invalidMnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon"
        assert(!Bip39.isValidMnemonic(invalidMnemonic))
    }

    test("BIP-39 mnemonic to seed with empty passphrase") {
        val mnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val expectedSeedHex =
            "5eb00bbddcf069084889a8ab9155568165f5c453ccb85e70811aaed6f6da5fc19a5ac40b389cd370d086206dec8aa6c43daea6690f20ad3d8d48b2d2ce9e38e4"

        val seed = Bip39.mnemonicToSeed(mnemonic, "")
        assert(seed.toHex == expectedSeedHex)
    }

    test("BIP-39 case insensitive") {
        val lowerMnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val upperMnemonic = lowerMnemonic.toUpperCase
        val mixedMnemonic =
            "Abandon ABANDON abandon ABANDON abandon ABANDON abandon ABANDON abandon ABANDON abandon ABOUT"

        assert(Bip39.isValidMnemonic(lowerMnemonic))
        assert(Bip39.isValidMnemonic(upperMnemonic))
        assert(Bip39.isValidMnemonic(mixedMnemonic))

        val seed1 = Bip39.mnemonicToSeed(lowerMnemonic, "")
        val seed2 = Bip39.mnemonicToSeed(upperMnemonic, "")
        val seed3 = Bip39.mnemonicToSeed(mixedMnemonic, "")

        assert(seed1.sameElements(seed2))
        assert(seed2.sameElements(seed3))
    }

    test("BIP-39 entropy round-trip for all valid sizes") {
        val sizes = Seq(16, 20, 24, 28, 32) // 128, 160, 192, 224, 256 bits

        for size <- sizes do
            val entropy = Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte)
            val mnemonic = Bip39.entropyToMnemonic(entropy)
            assert(Bip39.isValidMnemonic(mnemonic), s"Mnemonic should be valid for $size bytes")
            val recovered = Bip39.mnemonicToEntropy(mnemonic)
            assert(
              recovered.sameElements(entropy),
              s"Entropy round-trip failed for $size bytes"
            )
    }
}
