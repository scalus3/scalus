package scalus.cardano.onchain.plutus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.compiler.Options
import scalus.crypto.tree.PositionalMerkleTree as OffChain
import scalus.crypto.tree.PositionalMerkleTreeTestSupport.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.Data.toData

class PositionalMerkleTreeTest extends AnyFunSuite {
    import PositionalMerkleTree.*

    private def scalaSingle(input: Single): ByteString =
        verifyAndEmpty(input.root, input.depth, input.position, input.element, input.proof)
    private def scalaMembership(input: Single): ByteString =
        verifyMembership(input.root, input.depth, input.position, input.element, input.proof)
    private def scalaMulti(input: Multi): ByteString =
        verifyAndEmptyMany(input.root, input.depth, input.leaves, input.proof)

    private def rejects(input: Single): Unit = {
        intercept[RequirementError](scalaSingle(input))
        intercept[RequirementError](scalaMembership(input))
        assert(singleResult(input).isFailure)
        assert(membershipResult(input).isFailure)
    }
    private def rejects(input: Multi): Unit = {
        intercept[RequirementError](scalaMulti(input))
        assert(multiResult(input).isFailure)
    }

    private val data = Vector.tabulate(16)(uniqueElement)
    private lazy val tree = OffChain.fromElements(data, 4)
    private def single(position: Int): Single =
        Single(tree.rootHash, 4, BigInt(position), data(position), tree.proveMembership(position))

    test("Ethereum BLAKE2b vectors agree as Scala and compiled Plutus V3, including empty slots") {
        for v <- vectors if v.algorithm == "blake2b256" do
            val root = ByteString.fromHex(v.root)
            for s <- v.singles do
                val input = Single(
                  root,
                  BigInt(v.depth),
                  BigInt(s.position),
                  element(s.position),
                  ByteString.fromHex(s.proof)
                )
                if s.occupied then
                    val expected = ByteString.fromHex(s.emptiedRoot)
                    assert(scalaSingle(input) == expected)
                    assert(returned(singleResult(input)) == expected)
                    assert(scalaMembership(input) == root)
                    assert(returned(membershipResult(input)) == root)
                else rejects(input)
            for m <- v.multis do
                val input = Multi(
                  root,
                  BigInt(v.depth),
                  SList.from(m.positions.map(p => Leaf(BigInt(p), element(p)))),
                  ByteString.fromHex(m.proof)
                )
                val expected = ByteString.fromHex(m.emptiedRoot)
                assert(scalaMulti(input) == expected)
                assert(returned(multiResult(input)) == expected)
    }

    test(
      "single proof: every proof byte, element byte, position bit and root mutation fails in Scala and UPLC"
    ) {
        for position <- Seq(0, 5, 15) do
            val input = single(position)
            assert(returned(singleResult(input)) == tree.emptied(Vector(position)).rootHash)
            for offset <- input.proof.bytes.indices do
                rejects(input.copy(proof = flip(input.proof, offset)))
            for offset <- input.element.bytes.indices do
                rejects(input.copy(element = flip(input.element, offset)))
            for bit <- 0 to 5 do rejects(input.copy(position = input.position.flipBit(bit)))
            for offset <- input.root.bytes.indices do
                rejects(input.copy(root = flip(input.root, offset)))
    }

    test(
      "multiproof: every helper byte, query position and element mutation fails in Scala and UPLC"
    ) {
        val positions = Vector(1, 4, 10, 15)
        val input = query(tree, data, positions)
        assert(returned(multiResult(input)) == tree.emptied(positions).rootHash)
        for offset <- input.proof.bytes.indices do
            rejects(input.copy(proof = flip(input.proof, offset)))
        val leaves = input.leaves.toScalaList.toVector
        for i <- leaves.indices do
            for bit <- 0 to 5 do
                val mutated =
                    leaves.updated(i, leaves(i).copy(position = leaves(i).position.flipBit(bit)))
                rejects(input.copy(leaves = SList.from(mutated)))
            for offset <- leaves(i).element.bytes.indices do
                val mutated =
                    leaves.updated(i, leaves(i).copy(element = flip(leaves(i).element, offset)))
                rejects(input.copy(leaves = SList.from(mutated)))
        for offset <- input.root.bytes.indices do
            rejects(input.copy(root = flip(input.root, offset)))
    }

    test(
      "CVE-2012-2459: zero padding distinguishes six leaves from eight with repeated last leaves"
    ) {
        val six = Vector.tabulate(6)(uniqueElement)
        val eight = six ++ six.takeRight(2)
        assert(OffChain.fromElements(six, 3).rootHash != OffChain.fromElements(eight, 3).rootHash)
        // Control: duplicate-last padding really has the ambiguous root this regression targets.
        def duplicateLastRoot(elements: Vector[ByteString]): ByteString = {
            var layer = elements.map(blake2b_256)
            while layer.size > 1 do
                if layer.size % 2 != 0 then layer :+= layer.last
                layer =
                    layer.grouped(2).map(p => blake2b_256(appendByteString(p(0), p(1)))).toVector
            layer.head
        }
        assert(duplicateLastRoot(six) == duplicateLastRoot(eight))
    }

    test(
      "CVE-2017-12842: exact paths, internal-node-as-leaf and negative, overflowing or above-depth positions"
    ) {
        val input = single(0)
        rejects(input.copy(proof = input.proof.drop(32)))
        rejects(input.copy(proof = concat(Vector(input.proof, EmptyLeafHash))))
        rejects(input.copy(proof = input.proof.take(input.proof.length - 1)))
        rejects(
          input.copy(proof = concat(Vector(input.proof, ByteString.fromArray(Array[Byte](1)))))
        )
        for position <- Seq(BigInt(-1), BigInt(16), BigInt(1) << 32, BigInt(1) << 256) do
            rejects(input.copy(position = position))
        rejects(single(1).copy(position = -1))
        // Hashing these 64 bytes gives a genuine internal node at depth three.
        val internal = appendByteString(blake2b_256(data(0)), blake2b_256(data(1)))
        val shortened = input.copy(element = internal, proof = input.proof.drop(32))
        rejects(shortened)
        rejects(shortened.copy(proof = input.proof))
        assert(scalaMembership(shortened.copy(depth = 3)) == tree.rootHash)
        assert(returned(membershipResult(shortened.copy(depth = 3))) == tree.rootHash)
    }

    test("tBTC GHSA-wg2x-rv86-mmpx: exact authenticated depth even when a shorter root matches") {
        val input = single(0)
        val leafRoot = blake2b_256(input.element)
        rejects(input.copy(root = leafRoot, proof = ByteString.empty))
        assert(
          scalaMembership(
            input.copy(root = leafRoot, depth = 0, proof = ByteString.empty)
          ) == leafRoot
        )
        assert(
          returned(
            membershipResult(input.copy(root = leafRoot, depth = 0, proof = ByteString.empty))
          ) == leafRoot
        )
    }

    test(
      "CVE-2023-34459: helper underflow, unused, extra and trailing helpers, and an empty query"
    ) {
        val input = query(tree, data, Vector(1, 4, 10))
        for proof <- Seq(
              ByteString.empty,
              input.proof.drop(32),
              input.proof.take(input.proof.length - 1)
            )
        do
            val bad = input.copy(proof = proof)
            val error = intercept[RequirementError](scalaMulti(bad))
            assert(error.getMessage.contains("missing helper"))
            assert(multiResult(bad).isFailure)
        for suffix <- Seq(EmptyLeafHash, ByteString.fromArray(Array[Byte](1))) do
            rejects(input.copy(proof = concat(Vector(input.proof, suffix))))
        val emptyQuery = input.copy(leaves = SList.empty)
        val error = intercept[RequirementError](scalaMulti(emptyQuery))
        assert(error.getMessage.contains("empty query"))
        assert(multiResult(emptyQuery).isFailure)
        val all = query(tree, data, data.indices.toVector)
        assert(all.proof == ByteString.empty)
        assert(returned(multiResult(all)) == OffChain.fromElements(Vector.empty, 4).rootHash)
        rejects(all.copy(proof = EmptyLeafHash))
    }

    test("CVE-2023-34459: zero-valued helpers are valid hashes, never uninitialized nodes") {
        val partial = OffChain.fromElements(Vector(data.head), 4)
        val input = query(partial, Vector(data.head), Vector(0))
        assert(input.proof.take(32) == EmptyLeafHash)
        assert(scalaMulti(input) == OffChain.fromElements(Vector.empty, 4).rootHash)
        assert(returned(multiResult(input)) == OffChain.fromElements(Vector.empty, 4).rootHash)
        rejects(input.copy(leaves = SList(Leaf(0, data(1)))))
        rejects(
          input.copy(proof =
              concat(Vector(EmptyLeafHash, EmptyLeafHash, EmptyLeafHash, EmptyLeafHash))
          )
        )
    }

    test("CVE-2018-17144 analogue: the same position twice and unsorted queries fail") {
        val input = query(tree, data, Vector(1, 4))
        rejects(input.copy(leaves = SList(Leaf(1, data(1)), Leaf(1, data(1)))))
        rejects(input.copy(leaves = input.leaves.reverse))
        // At depth zero duplicate queries would otherwise leave two candidate root nodes.
        val one = blake2b_256(data.head)
        rejects(Multi(one, 0, SList(Leaf(0, data.head), Leaf(0, data.head)), ByteString.empty))
    }

    test("multiproof rejects negative, overflowing and above-depth positions and invalid depths") {
        val input = query(tree, data, Vector(0))
        for position <- Seq(BigInt(-1), BigInt(16), BigInt(1) << 32, BigInt(1) << 256) do
            rejects(input.copy(leaves = SList(Leaf(position, data.head))))
        rejects(query(tree, data, Vector(1)).copy(leaves = SList(Leaf(-1, data(1)))))
        for depth <- Seq(BigInt(-1), BigInt(257)) do
            rejects(single(0).copy(depth = depth))
            rejects(input.copy(depth = depth))
        rejects(Multi(blake2b_256(data.head), -1, SList(Leaf(0, data.head)), ByteString.empty))
    }

    test("configured depth ceiling rejects structurally matching 257-level proofs") {
        var hash = blake2b_256(data.head)
        for _ <- 0 until 257 do hash = blake2b_256(appendByteString(hash, EmptyLeafHash))
        val proof = concat(Vector.fill(257)(EmptyLeafHash))
        rejects(Single(hash, 257, 0, data.head, proof))
        rejects(Multi(hash, 257, SList(Leaf(0, data.head)), proof))
    }

    test("maximum depth 256 accepts a real leaf at the last position") {
        var zero = EmptyLeafHash
        var root = blake2b_256(data.head)
        val siblings = Vector.newBuilder[ByteString]
        for _ <- 0 until 256 do
            siblings += zero
            root = blake2b_256(appendByteString(zero, root))
            zero = blake2b_256(appendByteString(zero, zero))
        val position = (BigInt(1) << 256) - 1
        val proof = concat(siblings.result())
        val input = Single(root, 256, position, data.head, proof)
        assert(scalaSingle(input) == zero)
        assert(returned(singleResult(input)) == zero)
        assert(scalaMembership(input) == root)
        assert(returned(membershipResult(input)) == root)
        val multi = Multi(root, 256, SList(Leaf(position, data.head)), proof)
        assert(scalaMulti(multi) == zero)
        assert(returned(multiResult(multi)) == zero)
    }

    test(
      "an unused or emptied slot cannot be proved or emptied, including the known-preimage controls"
    ) {
        val partial = OffChain.fromElements(Vector(data.head), 4)
        for value <- Vector(ByteString.empty, EmptyLeafHash, data.head) do
            rejects(Single(partial.rootHash, 4, 1, value, partial.proveMembership(1)))
            rejects(
              Multi(partial.rootHash, 4, SList(Leaf(1, value)), partial.proveMulti(Vector(1)))
            )
        val after = tree.emptied(Vector(0))
        rejects(single(0).copy(root = after.rootHash, proof = after.proveMembership(0)))
        rejects(query(after, data, Vector(0)))
        rejects(single(0).copy(root = after.rootHash))
        rejects(query(tree, data, Vector(0)).copy(root = after.rootHash))
    }

    test("the reserved empty digest is rejected directly, without needing a BLAKE2b preimage") {
        val error = intercept[RequirementError](nonEmptyLeaf(EmptyLeafHash))
        assert(error.getMessage.contains("reserved empty leaf"))
        given Options = Options.release
        val program = PlutusV3.compile { (data: Data) => nonEmptyLeaf(data.to[ByteString]) }.program
        assert(evaluate(program, EmptyLeafHash.toData).isFailure)
        assert(nonEmptyLeaf(blake2b_256(data.head)) == blake2b_256(data.head))
    }

    test(
      "k identical elements at k positions are each emptied exactly once, sequentially and together"
    ) {
        for k <- Seq(1, 2, 4, 8, 16) do
            val equal = Vector.fill(k)(data.head)
            val original = OffChain.fromElements(equal, 4)
            val positions = equal.indices.toVector
            val all = query(original, equal, positions)
            assert(returned(multiResult(all)) == original.emptied(positions).rootHash)
            var current = original
            for position <- positions do
                val input = Single(
                  current.rootHash,
                  4,
                  BigInt(position),
                  data.head,
                  current.proveMembership(position)
                )
                val next = current.emptied(Vector(position))
                assert(scalaSingle(input) == next.rootHash)
                assert(returned(singleResult(input)) == next.rootHash)
                rejects(input.copy(root = next.rootHash, proof = next.proveMembership(position)))
                current = next
            assert(current.rootHash == OffChain.fromElements(Vector.empty, 4).rootHash)
    }
}
