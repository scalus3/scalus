package scalus.cardano.onchain.plutus.mpq

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.offchain.mpq.MerklePatriciaQuad as OffChainMpq
import scalus.cardano.onchain.RequirementError
import scalus.uplc.builtin.ByteString

import scala.language.implicitConversions

/** On-chain MPQ verifier tests using dynamically generated proofs from the off-chain MPQ. */
class MerklePatriciaQuadTest extends AnyFunSuite {

    private implicit inline def toByteString(inline s: String): ByteString =
        ByteString.fromString(s)

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

    private val offChainTrie = OffChainMpq.fromList(fruitBs)
    private val trie = offChainTrie.toOnChain
    private val expectedRoot = offChainTrie.rootHash

    test("has all fruits") {
        for (key, value) <- fruitBs do
            val proof = offChainTrie.proveExists(key)
            assert(trie.has(key, value, proof), s"has() failed for ${key.toHex}")
    }

    test("insert all fruits") {
        for (key, value) <- fruitBs do
            val without = offChainTrie.delete(key)
            val proof = without.proveMissing(key)
            val inserted = without.toOnChain.insert(key, value, proof)
            assert(inserted == trie, s"insert() failed for ${key.toHex}")
    }

    test("delete all fruits") {
        for (key, value) <- fruitBs do
            val proof = offChainTrie.proveExists(key)
            val without = offChainTrie.delete(key)
            val deleted = trie.delete(key, value, proof)
            assert(deleted.root == without.rootHash, s"delete() failed for ${key.toHex}")
    }

    test("update banana to eggplant") {
        val bananaKey: ByteString = "banana[uid: 218]"
        val bananaProof = offChainTrie.proveExists(bananaKey)
        val updated = trie.update(bananaKey, bananaProof, "🍌", "🍆")
        assert(updated.has(bananaKey, "🍆", bananaProof))
    }

    test("idempotent update") {
        val bananaKey: ByteString = "banana[uid: 218]"
        val bananaProof = offChainTrie.proveExists(bananaKey)
        assert(
          trie.update(bananaKey, bananaProof, "🍌", "🍌") == trie,
          "Self update should be idempotent"
        )
    }

    test("fail fake update") {
        val bananaKey: ByteString = "banana[uid: 218]"
        val without = offChainTrie.delete(bananaKey)
        val bananaProof = offChainTrie.proveExists(bananaKey)
        assertThrows[RequirementError] {
            without.toOnChain.update(bananaKey, bananaProof, "🍌", "🍆")
        }
    }

    test("insert whatever succeeds with different value") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kiwiWithout = offChainTrie.delete(kiwiKey)
        val kiwiProof = kiwiWithout.proveMissing(kiwiKey)
        assert(kiwiWithout.toOnChain.insert(kiwiKey, "foo", kiwiProof) != trie)
    }

    test("fail inserting already present") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kiwiProof = offChainTrie.proveExists(kiwiKey)
        assertThrows[RequirementError] {
            trie.insert(kiwiKey, "🥝", kiwiProof)
        }
    }

    test("fail delete with different value") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kiwiProof = offChainTrie.proveExists(kiwiKey)
        assertThrows[RequirementError] {
            trie.delete(kiwiKey, "🤷", kiwiProof)
        }
    }

    test("fail insert already present with different value") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kiwiProof = offChainTrie.proveExists(kiwiKey)
        assertThrows[RequirementError] {
            trie.insert(kiwiKey, "foo", kiwiProof)
        }
    }

    test("fail insert nearby with wrong proof") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val guavaKey: ByteString = "guava[uid: 344]"
        val kiwiWithout = offChainTrie.delete(kiwiKey)
        val kiwiProof = kiwiWithout.proveMissing(kiwiKey)
        assertThrows[RequirementError] {
            kiwiWithout.toOnChain.insert(guavaKey, "🤷", kiwiProof)
        }
    }

    test("fail insert higher with wrong proof") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kumquatKey: ByteString = "kumquat[uid: 0]"
        val kiwiWithout = offChainTrie.delete(kiwiKey)
        val kiwiProof = kiwiWithout.proveMissing(kiwiKey)
        assertThrows[RequirementError] {
            kiwiWithout.toOnChain.insert(kumquatKey, "🤷", kiwiProof)
        }
    }

    test("fail delete nearby with wrong proof") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val guavaKey: ByteString = "guava[uid: 344]"
        val kiwiProof = offChainTrie.proveExists(kiwiKey)
        assertThrows[RequirementError] {
            trie.delete(guavaKey, "🤷", kiwiProof)
        }
    }

    test("fail delete higher with wrong proof") {
        val kiwiKey: ByteString = "kiwi[uid: 0]"
        val kumquatKey: ByteString = "kumquat[uid: 0]"
        val kiwiProof = offChainTrie.proveExists(kiwiKey)
        assertThrows[RequirementError] {
            trie.delete(kumquatKey, "🤷", kiwiProof)
        }
    }

    test("two elements with shared prefix exercising Fork") {
        val k1 = ByteString.fromString("aaaa")
        val k2 = ByteString.fromString("aaab")
        val v1 = ByteString.fromString("v1")
        val v2 = ByteString.fromString("v2")

        val smallTrie = OffChainMpq.empty.insert(k1, v1).insert(k2, v2)
        val onChain = smallTrie.toOnChain

        val proof1 = smallTrie.proveExists(k1)
        val proof2 = smallTrie.proveExists(k2)
        assert(onChain.has(k1, v1, proof1))
        assert(onChain.has(k2, v2, proof2))

        // insert
        val without1 = smallTrie.delete(k1)
        val insertProof = without1.proveMissing(k1)
        val inserted = without1.toOnChain.insert(k1, v1, insertProof)
        assert(inserted.root == smallTrie.rootHash)

        // delete
        val deleted = onChain.delete(k1, v1, proof1)
        assert(deleted.root == without1.rootHash)
    }

    test("three elements exercising Branch step") {
        val k1 = ByteString.fromString("aa")
        val k2 = ByteString.fromString("bb")
        val k3 = ByteString.fromString("cc")
        val v1 = ByteString.fromString("v1")
        val v2 = ByteString.fromString("v2")
        val v3 = ByteString.fromString("v3")

        val smallTrie = OffChainMpq.empty.insert(k1, v1).insert(k2, v2).insert(k3, v3)
        val onChain = smallTrie.toOnChain

        for (k, v) <- Seq((k1, v1), (k2, v2), (k3, v3)) do
            val proof = smallTrie.proveExists(k)
            assert(onChain.has(k, v, proof), s"has failed for ${k.toHex}")

            val without = smallTrie.delete(k)
            val insertProof = without.proveMissing(k)
            val inserted = without.toOnChain.insert(k, v, insertProof)
            assert(inserted.root == smallTrie.rootHash, s"insert failed for ${k.toHex}")

            val deleted = onChain.delete(k, v, proof)
            assert(deleted.root == without.rootHash, s"delete failed for ${k.toHex}")
    }
}
