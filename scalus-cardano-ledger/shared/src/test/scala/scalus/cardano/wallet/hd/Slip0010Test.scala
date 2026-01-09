package scalus.cardano.wallet.hd

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex
import scalus.utils.Hex.toHex

/** SLIP-0010 test vectors.
  *
  * @see
  *   https://github.com/satoshilabs/slips/blob/master/slip-0010.md
  */
class Slip0010Test extends AnyFunSuite {

    // Test vector 1 from SLIP-0010 specification for Ed25519
    test("SLIP-0010 Ed25519 test vector 1 - master key") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)

        assert(
          master.chainCode.toHex == "90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb"
        )
        assert(
          master.privateKeyBytes.toHex == "2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7"
        )
    }

    test("SLIP-0010 Ed25519 test vector 1 - m/0'") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master.deriveHardened(0)

        assert(
          child.chainCode.toHex == "8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69"
        )
        assert(
          child.privateKeyBytes.toHex == "68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3"
        )
    }

    test("SLIP-0010 Ed25519 test vector 1 - m/0'/1'") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master.deriveHardened(0).deriveHardened(1)

        assert(
          child.chainCode.toHex == "a320425f77d1b5c2505a6b1b27382b37368ee640e3557c315416801243552f14"
        )
        assert(
          child.privateKeyBytes.toHex == "b1d0bad404bf35da785a64ca1ac54b2617211d2777696fbffaf208f746ae84f2"
        )
    }

    test("SLIP-0010 Ed25519 test vector 1 - m/0'/1'/2'") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master.deriveHardened(0).deriveHardened(1).deriveHardened(2)

        assert(
          child.chainCode.toHex == "2e69929e00b5ab250f49c3fb1c12f252de4fed2c1db88387094a0f8c4c9ccd6c"
        )
        assert(
          child.privateKeyBytes.toHex == "92a5b23c0b8a99e37d07df3fb9966917f5d06e02ddbd909c7e184371463e9fc9"
        )
    }

    test("SLIP-0010 Ed25519 test vector 1 - m/0'/1'/2'/2'") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master.deriveHardened(0).deriveHardened(1).deriveHardened(2).deriveHardened(2)

        assert(
          child.chainCode.toHex == "8f6d87f93d750e0efccda017d662a1b31a266e4a6f5993b15f5c1f07f74dd5cc"
        )
        assert(
          child.privateKeyBytes.toHex == "30d1dc7e5fc04c31219ab25a27ae00b50f6fd66622f6e9c913253d6511d1e662"
        )
    }

    test("SLIP-0010 Ed25519 test vector 1 - m/0'/1'/2'/2'/1000000000'") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master
            .deriveHardened(0)
            .deriveHardened(1)
            .deriveHardened(2)
            .deriveHardened(2)
            .deriveHardened(1000000000)

        assert(
          child.chainCode.toHex == "68789923a0cac2cd5a29172a475fe9e0fb14cd6adb5ad98a3fa70333e7afa230"
        )
        assert(
          child.privateKeyBytes.toHex == "8f94d394a8e8fd6b1bc2f3f49f5c47e385281d5c17e65324b0f62483e37e8793"
        )
    }

    // Test vector 2 from SLIP-0010 specification
    test("SLIP-0010 Ed25519 test vector 2 - master key") {
        val seed = Hex.hexToBytes(
          "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"
        )
        val master = Slip0010.masterKeyFromSeed(seed)

        assert(
          master.chainCode.toHex == "ef70a74db9c3a5af931b5fe73ed8e1a53464133654fd55e7a66f8570b8e33c3b"
        )
        assert(
          master.privateKeyBytes.toHex == "171cb88b1b3c1db25add599712e36245d75bc65a1a5c9e18d76f9f2b1eab4012"
        )
    }

    test("SLIP-0010 Ed25519 test vector 2 - m/0'") {
        val seed = Hex.hexToBytes(
          "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"
        )
        val master = Slip0010.masterKeyFromSeed(seed)
        val child = master.deriveHardened(0)

        assert(
          child.chainCode.toHex == "0b78a3226f915c082bf118f83618a618ab6dec793752624cbeb622acb562862d"
        )
        assert(
          child.privateKeyBytes.toHex == "1559eb2bbec5790b0c65d8693e4d0875b1747f4970ae8b650486ed7470845635"
        )
    }

    test("parsePath - standard paths") {
        assert(Slip0010.parsePath("m") == Seq.empty)
        assert(Slip0010.parsePath("m/") == Seq.empty)

        val path1 = Slip0010.parsePath("m/44'")
        assert(path1.length == 1)
        assert(path1.head == (44 + Slip0010.HardenedOffset).toInt)

        val path2 = Slip0010.parsePath("m/44'/1815'/0'")
        assert(path2.length == 3)
        assert(path2(0) == (44 + Slip0010.HardenedOffset).toInt)
        assert(path2(1) == (1815 + Slip0010.HardenedOffset).toInt)
        assert(path2(2) == (0 + Slip0010.HardenedOffset).toInt)
    }

    test("parsePath - alternative hardened notation") {
        val path1 = Slip0010.parsePath("m/44H/1815H/0H")
        assert(path1.length == 3)
        assert(path1(0) == (44 + Slip0010.HardenedOffset).toInt)
        assert(path1(1) == (1815 + Slip0010.HardenedOffset).toInt)
        assert(path1(2) == (0 + Slip0010.HardenedOffset).toInt)
    }

    test("deriveFromPath") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")

        val key1 = Slip0010.deriveFromPath(seed, "m/0'")
        val key2 = Slip0010.masterKeyFromSeed(seed).deriveHardened(0)

        assert(key1.privateKeyBytes.sameElements(key2.privateKeyBytes))
        assert(key1.chainCode.sameElements(key2.chainCode))
    }

    test("derivePath convenience method") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val master = Slip0010.masterKeyFromSeed(seed)

        val key1 = master.derivePath(Seq(0, 1, 2))
        val key2 = master.deriveHardened(0).deriveHardened(1).deriveHardened(2)

        assert(key1.privateKeyBytes.sameElements(key2.privateKeyBytes))
        assert(key1.chainCode.sameElements(key2.chainCode))
    }

    test("ExtendedKey equality") {
        val seed = Hex.hexToBytes("000102030405060708090a0b0c0d0e0f")
        val key1 = Slip0010.masterKeyFromSeed(seed)
        val key2 = Slip0010.masterKeyFromSeed(seed)

        assert(key1 == key2)
        assert(key1.hashCode() == key2.hashCode())
    }

    test("integration with BIP-39") {
        // Use a known BIP-39 mnemonic to derive a seed, then derive Ed25519 keys
        val mnemonic =
            "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        val seed = Bip39.mnemonicToSeed(mnemonic, "")

        // Derive Cardano account key path: m/1852'/1815'/0'
        val accountKey = Slip0010.deriveFromPath(seed, "m/1852'/1815'/0'")

        assert(accountKey.privateKeyBytes.length == 32)
        assert(accountKey.chainCode.length == 32)
    }
}
