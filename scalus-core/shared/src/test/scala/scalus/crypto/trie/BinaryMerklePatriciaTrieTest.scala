package scalus.crypto.trie

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.Merkling
import scalus.cardano.onchain.plutus.crypto.trie.BinaryMerklePatriciaTrie as OnChainBinary
import scalus.uplc.builtin.ByteString

class BinaryMerklePatriciaTrieTest extends AnyFunSuite {

    // The fruit dataset from the on-chain BinaryMerklePatriciaTrieTest
    private val fruitEntries: Seq[(String, String)] = Seq(
      "apple[uid: 58]" -> "🍎",
      "apricot[uid: 0]" -> "🤷",
      "banana[uid: 218]" -> "🍌",
      "blueberry[uid: 0]" -> "🫐",
      "cherry[uid: 0]" -> "🍒",
      "coconut[uid: 0]" -> "🥥",
      "cranberry[uid: 0]" -> "🤷",
      "fig[uid: 68267]" -> "🤷",
      "grapefruit[uid: 0]" -> "🤷",
      "grapes[uid: 0]" -> "🍇",
      "guava[uid: 344]" -> "🤷",
      "kiwi[uid: 0]" -> "🥝",
      "kumquat[uid: 0]" -> "🤷",
      "lemon[uid: 0]" -> "🍋",
      "lime[uid: 0]" -> "🤷",
      "mango[uid: 0]" -> "🥭",
      "orange[uid: 0]" -> "🍊",
      "papaya[uid: 0]" -> "🤷",
      "passionfruit[uid: 0]" -> "🤷",
      "peach[uid: 0]" -> "🍑",
      "pear[uid: 0]" -> "🍐",
      "pineapple[uid: 12577]" -> "🍍",
      "plum[uid: 15492]" -> "🤷",
      "pomegranate[uid: 0]" -> "🤷",
      "raspberry[uid: 0]" -> "🤷",
      "strawberry[uid: 2532]" -> "🍓",
      "tangerine[uid: 11]" -> "🍊",
      "tomato[uid: 83468]" -> "🍅",
      "watermelon[uid: 0]" -> "🍉",
      "yuzu[uid: 0]" -> "🤷"
    )

    private val fruitBs: Seq[(ByteString, ByteString)] = fruitEntries.map { case (k, v) =>
        (ByteString.fromString(k), ByteString.fromString(v))
    }

    private val fullTrie = BinaryMerklePatriciaTrie.fromList(fruitBs)
    private val expectedRoot = fullTrie.rootHash

    test("empty trie has null hash") {
        val trie = BinaryMerklePatriciaTrie.empty
        assert(trie.rootHash == Merkling.NullHash)
        assert(trie.size == 0)
        assert(trie.isEmpty)
    }

    test("single element trie") {
        val trie = BinaryMerklePatriciaTrie.empty
            .insert(ByteString.fromString("hello"), ByteString.fromString("world"))
        assert(trie.size == 1)
        assert(!trie.isEmpty)
        assert(trie.get(ByteString.fromString("hello")).contains(ByteString.fromString("world")))
        assert(trie.get(ByteString.fromString("other")).isEmpty)
    }

    test("fruit trie root hash matches expected") {
        assert(fullTrie.rootHash == expectedRoot, s"got ${fullTrie.rootHash.toHex}")
    }

    test("fruit trie has correct size") {
        assert(fullTrie.size == 30)
    }

    test("get all fruits") {
        for (key, value) <- fruitBs do
            assert(
              fullTrie.get(key).contains(value),
              s"get failed for ${key.toHex}"
            )
    }

    test("get missing key returns None") {
        assert(fullTrie.get(ByteString.fromString("nonexistent")).isEmpty)
    }

    test("insert duplicate throws") {
        val (key, value) = fruitBs.head
        assertThrows[IllegalArgumentException] {
            fullTrie.insert(key, value)
        }
    }

    test("delete all fruits one by one") {
        var trie = fullTrie
        for (key, _) <- fruitBs do trie = trie.delete(key)
        assert(trie.isEmpty)
        assert(trie.rootHash == Merkling.NullHash)
    }

    test("delete missing key throws") {
        assertThrows[NoSuchElementException] {
            fullTrie.delete(ByteString.fromString("nonexistent"))
        }
    }

    test("delete and reinsert preserves root hash") {
        for (key, value) <- fruitBs do
            val without = fullTrie.delete(key)
            val restored = without.insert(key, value)
            assert(
              restored.rootHash == expectedRoot,
              s"delete+reinsert failed for ${key.toHex}"
            )
    }

    test("insertion order does not affect root hash") {
        val shuffled = scala.util.Random(42).shuffle(fruitBs)
        val trie = BinaryMerklePatriciaTrie.fromList(shuffled)
        assert(trie.rootHash == expectedRoot)
    }

    // --- Binary proof tests (mpfb) ---

    test("binary proofs verify membership via mpfb on-chain") {
        val onChain = OnChainBinary(fullTrie.rootHash)
        for (key, value) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            assert(
              onChain.has(key, value, proof),
              s"binary on-chain has() failed for ${key.toHex}"
            )
    }

    test("binary proofs support on-chain delete via mpfb") {
        val onChain = OnChainBinary(fullTrie.rootHash)
        for (key, value) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            val without = fullTrie.delete(key)
            val deleted = onChain.delete(key, value, proof)
            assert(
              deleted.root == without.rootHash,
              s"binary on-chain delete root mismatch for ${key.toHex}"
            )
    }

    test("binary proofs support on-chain insert via mpfb") {
        for (key, value) <- fruitBs do
            val without = fullTrie.delete(key)
            val proof = without.proveNonMembership(key)
            val onChainWithout = OnChainBinary(without.rootHash)
            val inserted = onChainWithout.insert(key, value, proof)
            assert(
              inserted.root == expectedRoot,
              s"binary on-chain insert root mismatch for ${key.toHex}"
            )
    }

    test("binary proofs support on-chain update via mpfb") {
        val onChain = OnChainBinary(fullTrie.rootHash)
        val newValue = ByteString.fromString("updated")
        for (key, oldValue) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            val updated = onChain.update(key, proof, oldValue, newValue)
            assert(updated.has(key, newValue, proof))
    }

    test("binary proveNonMembership for absent key via mpfb") {
        val key = ByteString.fromString("nonexistent")
        val value = ByteString.fromString("somevalue")
        val proof = fullTrie.proveNonMembership(key)
        val onChain = OnChainBinary(fullTrie.rootHash)
        val inserted = onChain.insert(key, value, proof)
        val offChainInserted = fullTrie.insert(key, value)
        assert(inserted.root == offChainInserted.rootHash)
    }

    test("binary proof is compact") {
        for (key, _) <- fruitBs.take(5) do
            val binaryProof = fullTrie.proveMembership(key)
            // Binary proofs should be non-empty
            assert(binaryProof.length > 0, s"empty proof for ${key.toHex}")
            // Each step is at most 130 bytes (Branch), with at most ~16 steps for a 30-element trie
            assert(
              binaryProof.length < 2000,
              s"unexpectedly large proof (${binaryProof.length}B) for ${key.toHex}"
            )
    }

    test("two elements with long shared prefix") {
        val k1 = ByteString.fromString("aaaa")
        val k2 = ByteString.fromString("aaab")
        val v1 = ByteString.fromString("v1")
        val v2 = ByteString.fromString("v2")

        val trie = BinaryMerklePatriciaTrie.empty.insert(k1, v1).insert(k2, v2)
        assert(trie.size == 2)
        assert(trie.get(k1).contains(v1))
        assert(trie.get(k2).contains(v2))

        val onChain = OnChainBinary(trie.rootHash)
        val proof1 = trie.proveMembership(k1)
        val proof2 = trie.proveMembership(k2)
        assert(onChain.has(k1, v1, proof1))
        assert(onChain.has(k2, v2, proof2))

        val trie1 = trie.delete(k1)
        assert(trie1.size == 1)
        assert(trie1.get(k2).contains(v2))
    }
}
