package scalus.crypto.trie

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.Merkling
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaTrie as OnChainMpfo
import scalus.uplc.builtin.ByteString

class MerklePatriciaTrieTest extends AnyFunSuite {

    private val fruitEntries: Seq[(String, String)] = Seq(
      "apple[uid: 58]" -> "\uD83C\uDF4E",
      "apricot[uid: 0]" -> "\uD83E\uDD37",
      "banana[uid: 218]" -> "\uD83C\uDF4C",
      "blueberry[uid: 0]" -> "\uD83E\uDED0",
      "cherry[uid: 0]" -> "\uD83C\uDF52",
      "coconut[uid: 0]" -> "\uD83E\uDD65",
      "cranberry[uid: 0]" -> "\uD83E\uDD37",
      "fig[uid: 68267]" -> "\uD83E\uDD37",
      "grapefruit[uid: 0]" -> "\uD83E\uDD37",
      "grapes[uid: 0]" -> "\uD83C\uDF47",
      "guava[uid: 344]" -> "\uD83E\uDD37",
      "kiwi[uid: 0]" -> "\uD83E\uDD5D",
      "kumquat[uid: 0]" -> "\uD83E\uDD37",
      "lemon[uid: 0]" -> "\uD83C\uDF4B",
      "lime[uid: 0]" -> "\uD83E\uDD37",
      "mango[uid: 0]" -> "\uD83E\uDD6D",
      "orange[uid: 0]" -> "\uD83C\uDF4A",
      "papaya[uid: 0]" -> "\uD83E\uDD37",
      "passionfruit[uid: 0]" -> "\uD83E\uDD37",
      "peach[uid: 0]" -> "\uD83C\uDF51",
      "pear[uid: 0]" -> "\uD83C\uDF50",
      "pineapple[uid: 12577]" -> "\uD83C\uDF4D",
      "plum[uid: 15492]" -> "\uD83E\uDD37",
      "pomegranate[uid: 0]" -> "\uD83E\uDD37",
      "raspberry[uid: 0]" -> "\uD83E\uDD37",
      "strawberry[uid: 2532]" -> "\uD83C\uDF53",
      "tangerine[uid: 11]" -> "\uD83C\uDF4A",
      "tomato[uid: 83468]" -> "\uD83C\uDF45",
      "watermelon[uid: 0]" -> "\uD83C\uDF49",
      "yuzu[uid: 0]" -> "\uD83E\uDD37"
    )

    private val fruitBs: Seq[(ByteString, ByteString)] = fruitEntries.map { case (k, v) =>
        (ByteString.fromString(k), ByteString.fromString(v))
    }

    private val fullTrie = MerklePatriciaTrie.fromList(fruitBs)
    private val expectedRoot = fullTrie.rootHash

    test("empty trie has null hash") {
        val trie = MerklePatriciaTrie.empty
        assert(trie.rootHash == Merkling.NullHash)
        assert(trie.size == 0)
        assert(trie.isEmpty)
    }

    test("single element trie") {
        val trie = MerklePatriciaTrie.empty
            .insert(ByteString.fromString("hello"), ByteString.fromString("world"))
        assert(trie.size == 1)
        assert(!trie.isEmpty)
        assert(trie.get(ByteString.fromString("hello")).contains(ByteString.fromString("world")))
        assert(trie.get(ByteString.fromString("other")).isEmpty)
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
        val trie = MerklePatriciaTrie.fromList(shuffled)
        assert(trie.rootHash == expectedRoot)
    }

    // --- Proof tests (mpfo) ---

    test("proofs verify membership via mpfo on-chain") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        for (key, value) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            assert(
              onChain.has(key, value, proof),
              s"on-chain has() failed for ${key.toHex}"
            )
    }

    test("proofs support on-chain delete via mpfo") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        for (key, value) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            val without = fullTrie.delete(key)
            val deleted = onChain.delete(key, value, proof)
            assert(
              deleted.root == without.rootHash,
              s"on-chain delete root mismatch for ${key.toHex}"
            )
    }

    test("proofs support on-chain insert via mpfo") {
        for (key, value) <- fruitBs do
            val without = fullTrie.delete(key)
            val proof = without.proveNonMembership(key)
            val onChainWithout = OnChainMpfo(without.rootHash)
            val inserted = onChainWithout.insert(key, value, proof)
            assert(
              inserted.root == expectedRoot,
              s"on-chain insert root mismatch for ${key.toHex}"
            )
    }

    test("proofs support on-chain update via mpfo") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        val newValue = ByteString.fromString("updated")
        for (key, oldValue) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            val updated = onChain.update(key, proof, oldValue, newValue)
            assert(updated.has(key, newValue, proof))
    }

    test("proveNonMembership for absent key via mpfo") {
        val key = ByteString.fromString("nonexistent")
        val value = ByteString.fromString("somevalue")
        val proof = fullTrie.proveNonMembership(key)
        val onChain = OnChainMpfo(fullTrie.rootHash)
        val inserted = onChain.insert(key, value, proof)
        val offChainInserted = fullTrie.insert(key, value)
        assert(inserted.root == offChainInserted.rootHash)
    }

    test("verifyMembership succeeds for present key") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        for (key, value) <- fruitBs do
            val proof = fullTrie.proveMembership(key)
            onChain.verifyMembership(key, value, proof)
    }

    test("verifyMembership fails for wrong value") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        val (key, _) = fruitBs.head
        val proof = fullTrie.proveMembership(key)
        assertThrows[Exception] {
            onChain.verifyMembership(key, ByteString.fromString("wrong"), proof)
        }
    }

    test("verifyNonMembership succeeds for absent key") {
        val key = ByteString.fromString("nonexistent")
        val proof = fullTrie.proveNonMembership(key)
        val onChain = OnChainMpfo(fullTrie.rootHash)
        onChain.verifyNonMembership(key, proof)
    }

    test("verifyNonMembership fails for present key") {
        val onChain = OnChainMpfo(fullTrie.rootHash)
        val (key, _) = fruitBs.head
        val proof = fullTrie.proveMembership(key)
        assertThrows[Exception] {
            onChain.verifyNonMembership(key, proof)
        }
    }

    test("two elements with long shared prefix") {
        val k1 = ByteString.fromString("aaaa")
        val k2 = ByteString.fromString("aaab")
        val v1 = ByteString.fromString("v1")
        val v2 = ByteString.fromString("v2")

        val trie = MerklePatriciaTrie.empty.insert(k1, v1).insert(k2, v2)
        assert(trie.size == 2)
        assert(trie.get(k1).contains(v1))
        assert(trie.get(k2).contains(v2))

        val onChain = OnChainMpfo(trie.rootHash)
        val proof1 = trie.proveMembership(k1)
        val proof2 = trie.proveMembership(k2)
        assert(onChain.has(k1, v1, proof1))
        assert(onChain.has(k2, v2, proof2))

        val trie1 = trie.delete(k1)
        assert(trie1.size == 1)
        assert(trie1.get(k2).contains(v2))
    }
}
