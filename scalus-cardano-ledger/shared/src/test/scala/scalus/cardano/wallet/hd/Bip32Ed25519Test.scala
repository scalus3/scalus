package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex
import scalus.utils.Hex.toHex

/** BIP32-Ed25519 test vectors from cardano-addresses.
  *
  * Test vectors sourced from: https://github.com/IntersectMBO/cardano-addresses
  *
  * These tests verify compatibility with standard Cardano wallets.
  */
class Bip32Ed25519Test extends AnyFunSuite {

    // Test vector 1 from cardano-addresses README
    // Mnemonic: "nothing heart matrix fly sleep slogan tomato pulse what roof rail since plastic false enlist"
    // Derived at path m/1852'/1815'/0'/0/0
    val testMnemonic1 =
        "nothing heart matrix fly sleep slogan tomato pulse what roof rail since plastic false enlist"

    // Expected values at path m/1852'/1815'/0'/0/0
    val expectedSigningKey1 = Hex.hexToBytes(
      "b0bf46232c7f0f58ad333030e43ffbea7c2bb6f8135bd05fb0d343ade8453c5eacc7ac09f77e16b635832522107eaa9f56db88c615f537aa6025e6c23da98ae8"
    )
    val expectedVerificationKey1 = Hex.hexToBytes(
      "fbbbf6410e24532f35e9279febb085d2cc05b3b2ada1df77ea1951eb694f3834"
    )
    val expectedChainCode1 = Hex.hexToBytes(
      "b0be1868d1c36ef9089b3b094f5fe1d783e4d5fea14e2034c0397bee50e65a1a"
    )

    // Test vector 2 from cardano-addresses README
    // Mnemonic: "exercise club noble adult miracle awkward problem olympic puppy private goddess piano fatal fashion vacuum"
    val testMnemonic2 =
        "exercise club noble adult miracle awkward problem olympic puppy private goddess piano fatal fashion vacuum"

    test("masterKeyFromMnemonic produces valid extended key") {
        val master = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)

        assert(master.kL.length == 32, "kL should be 32 bytes")
        assert(master.kR.length == 32, "kR should be 32 bytes")
        assert(master.chainCode.length == 32, "chainCode should be 32 bytes")

        // Verify clamping of master key
        assert((master.kL(0) & 0x07) == 0, "lowest 3 bits of kL[0] should be 0")
        assert((master.kL(31) & 0x80) == 0, "highest bit of kL[31] should be 0")
        assert((master.kL(31) & 0x40) != 0, "second highest bit of kL[31] should be 1")
    }

    test("deriveFromPath matches cardano-addresses output for m/1852'/1815'/0'/0/0") {
        val derived = Bip32Ed25519.deriveFromPath(testMnemonic1, "", "m/1852'/1815'/0'/0/0")

        val actualSigningKey = derived.extendedSecretKey
        val actualChainCode = derived.chainCode

        assert(
          java.util.Arrays.equals(actualSigningKey, expectedSigningKey1),
          s"Signing key mismatch.\nExpected: ${expectedSigningKey1.toHex}\nActual:   ${actualSigningKey.toHex}"
        )
        assert(
          java.util.Arrays.equals(actualChainCode, expectedChainCode1),
          s"Chain code mismatch.\nExpected: ${expectedChainCode1.toHex}\nActual:   ${actualChainCode.toHex}"
        )
    }

    test("public key derivation matches expected verification key") {
        val derived = Bip32Ed25519.deriveFromPath(testMnemonic1, "", "m/1852'/1815'/0'/0/0")

        // Derive public key from kL
        val actualVerificationKey = scalus.crypto.ed25519.Ed25519Math.scalarMultiplyBase(derived.kL)

        assert(
          java.util.Arrays.equals(actualVerificationKey, expectedVerificationKey1),
          s"Verification key mismatch.\nExpected: ${expectedVerificationKey1.toHex}\nActual:   ${actualVerificationKey.toHex}"
        )
    }

    test("hardened derivation works correctly") {
        val master = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)

        // Derive hardened child at index 1852'
        val child = master.deriveHardened(1852)

        assert(child.kL.length == 32)
        assert(child.kR.length == 32)
        assert(child.chainCode.length == 32)

        // Child key should be different from master
        assert(!java.util.Arrays.equals(child.kL, master.kL))
        assert(!java.util.Arrays.equals(child.chainCode, master.chainCode))
    }

    test("non-hardened derivation works correctly") {
        val master = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)

        // First derive to account level (hardened)
        val account = master
            .deriveHardened(1852)
            .deriveHardened(1815)
            .deriveHardened(0)

        // Then derive non-hardened for role/index
        val payment = account.deriveNormal(0).deriveNormal(0)

        assert(payment.kL.length == 32)
        assert(payment.kR.length == 32)
        assert(payment.chainCode.length == 32)
    }

    test("step-by-step derivation matches deriveFromPath") {
        // Derive using path string
        val fromPath = Bip32Ed25519.deriveFromPath(testMnemonic1, "", "m/1852'/1815'/0'/0/0")

        // Derive step by step
        val master = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)
        val stepByStep = master
            .deriveHardened(1852)
            .deriveHardened(1815)
            .deriveHardened(0)
            .deriveNormal(0)
            .deriveNormal(0)

        assert(
          java.util.Arrays.equals(fromPath.kL, stepByStep.kL),
          "kL should match between path and step-by-step derivation"
        )
        assert(
          java.util.Arrays.equals(fromPath.kR, stepByStep.kR),
          "kR should match between path and step-by-step derivation"
        )
        assert(
          java.util.Arrays.equals(fromPath.chainCode, stepByStep.chainCode),
          "chainCode should match between path and step-by-step derivation"
        )
    }

    test("different mnemonics produce different keys") {
        val master1 = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)
        val master2 = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic2)

        assert(!java.util.Arrays.equals(master1.kL, master2.kL))
        assert(!java.util.Arrays.equals(master1.kR, master2.kR))
        assert(!java.util.Arrays.equals(master1.chainCode, master2.chainCode))
    }

    test("passphrase affects key derivation") {
        val noPassphrase = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1, "")
        val withPassphrase = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1, "secret")

        assert(!java.util.Arrays.equals(noPassphrase.kL, withPassphrase.kL))
        assert(!java.util.Arrays.equals(noPassphrase.chainCode, withPassphrase.chainCode))
    }

    test("parsePath handles various formats") {
        // Standard format with apostrophe
        assert(Bip32Ed25519.parsePath("m/1852'/1815'/0'/0/0").length == 5)

        // With H suffix
        assert(Bip32Ed25519.parsePath("m/1852H/1815H/0H/0/0").length == 5)

        // Mixed
        assert(Bip32Ed25519.parsePath("m/1852'/1815H/0h/0/0").length == 5)
    }

    test("deriveChild with hardened index") {
        val master = Bip32Ed25519.masterKeyFromMnemonic(testMnemonic1)

        // Using deriveChild directly with hardened flag
        val child1 = master.deriveChild(1852 | 0x80000000)
        val child2 = master.deriveHardened(1852)

        assert(java.util.Arrays.equals(child1.kL, child2.kL))
        assert(java.util.Arrays.equals(child1.kR, child2.kR))
        assert(java.util.Arrays.equals(child1.chainCode, child2.chainCode))
    }

    test("deriveNormal rejects hardened indices") {
        val master = Bip32Ed25519
            .masterKeyFromMnemonic(testMnemonic1)
            .deriveHardened(1852)
            .deriveHardened(1815)
            .deriveHardened(0)

        // This should throw for negative index (which would be >= 2^31 as unsigned)
        intercept[IllegalArgumentException] {
            master.deriveNormal(-1) // -1 has high bit set
        }
    }

    test("deterministic key derivation") {
        // Same inputs should always produce same outputs
        val key1 = Bip32Ed25519.deriveFromPath(testMnemonic1, "", "m/1852'/1815'/0'/0/0")
        val key2 = Bip32Ed25519.deriveFromPath(testMnemonic1, "", "m/1852'/1815'/0'/0/0")

        assert(java.util.Arrays.equals(key1.kL, key2.kL))
        assert(java.util.Arrays.equals(key1.kR, key2.kR))
        assert(java.util.Arrays.equals(key1.chainCode, key2.chainCode))
    }

    test("parsePath rejects empty path") {
        val ex = intercept[IllegalArgumentException] {
            Bip32Ed25519.parsePath("")
        }
        assert(ex.getMessage.contains("cannot be empty"))
    }

    test("parsePath rejects path without m/ prefix") {
        val ex = intercept[IllegalArgumentException] {
            Bip32Ed25519.parsePath("1852'/1815'/0'")
        }
        assert(ex.getMessage.contains("must start with"))
    }

    test("parsePath rejects path with invalid index") {
        val ex = intercept[IllegalArgumentException] {
            Bip32Ed25519.parsePath("m/abc/1815'/0'")
        }
        assert(ex.getMessage.contains("Invalid index"))
    }

    test("parsePath rejects path with empty segment") {
        val ex = intercept[IllegalArgumentException] {
            Bip32Ed25519.parsePath("m/1852'//0'")
        }
        assert(ex.getMessage.contains("Empty segment"))
    }

    test("parsePath accepts uppercase M prefix") {
        val indices = Bip32Ed25519.parsePath("M/1852'/1815'/0'")
        assert(indices.length == 3)
    }
}
