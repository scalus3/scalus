package scalus.crypto.tree

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.crypto.tree.PositionalMerkleTree as OnChain
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Builtins.{blake2b_256, sha2_256}

class PositionalMerkleTreeTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    import PositionalMerkleTreeTestSupport.*

    test("Ethereum v1.5.0 reference: SHA-256 and BLAKE2b roots, paths, helpers and emptied roots") {
        assert(vectors.size == 42)
        assert(vectors.map(_.depth).toSet == Set(1, 2, 3, 4, 5, 6, 15))
        for v <- vectors do
            val hash = if v.algorithm == "sha256" then sha2_256 else blake2b_256
            val data = elements(v.count)
            val tree = PositionalMerkleTree.fromElements(data, v.depth, hash)
            val model = naive(data, v.depth)
            assert(
              tree.rootHash == ByteString.fromHex(v.root),
              s"${v.algorithm}/${v.depth}/${v.shape}"
            )
            assert(
              PositionalMerkleTree.rootFromElements(data.iterator, v.depth, hash) == tree.rootHash
            )
            for single <- v.singles do
                assert(tree.proveMembership(single.position) == ByteString.fromHex(single.proof))
                if single.occupied then
                    assert(
                      tree.emptied(Vector(single.position)).rootHash == ByteString.fromHex(
                        single.emptiedRoot
                      )
                    )
            for multi <- v.multis do
                assert(model.helpers(multi.positions).map(_._1) == multi.helperIndices)
                assert(tree.proveMulti(multi.positions) == ByteString.fromHex(multi.proof))
                assert(
                  tree.emptied(multi.positions).rootHash == ByteString.fromHex(multi.emptiedRoot)
                )
    }

    private def compare(data: Vector[ByteString], depth: Int, positions: Vector[Int]): Unit = {
        val tree = PositionalMerkleTree.fromElements(data, depth)
        val model = naive(data, depth)
        assert(tree.rootHash == model.root)
        assert(PositionalMerkleTree.rootFromElements(data.iterator, depth) == model.root)
        for position <- data.indices do
            assert(tree.proveMembership(position) == model.proof(position))
        assert(tree.proveMulti(positions) == model.multiProof(positions))
        assert(tree.emptied(positions).rootHash == model.emptied(positions).root)
        val input = query(tree, data, positions)
        assert(
          OnChain.verifyAndEmptyMany(input.root, input.depth, input.leaves, input.proof) == model
              .emptied(positions)
              .root
        )
        assert(returned(multiResult(input)) == model.emptied(positions).root)
    }

    for depth <- 1 to 6 do
        test(s"naive rebuild property at depth $depth, with partial trees and duplicate elements") {
            val capacity = 1 << depth
            forAll(Gen.choose(1, capacity), Gen.listOfN(capacity, Gen.choose(0, 4))) {
                (count, values) =>
                    val data =
                        values.take(count).map(i => ByteString.fromString(s"duplicate-$i")).toVector
                    val positions = data.indices.filter(i => values(i) % 2 == 0).toVector match
                        case empty if empty.isEmpty => Vector(0)
                        case selected               => selected
                    compare(data, depth, positions)
            }
        }

    for depth <- 1 to 3 do
        test(s"exhaustive nonempty position subsets at depth $depth with duplicate elements") {
            val capacity = 1 << depth
            val data = Vector.tabulate(capacity)(i => element(i % 2))
            for mask <- 1 until (1 << capacity) do
                compare(data, depth, data.indices.filter(i => (mask & (1 << i)) != 0).toVector)
        }

    test("streaming frontier agrees at every carry and padding boundary") {
        for depth <- 0 to 6; count <- 0 to (1 << depth) do
            val data = elements(count)
            assert(
              PositionalMerkleTree.rootFromElements(data.iterator, depth) == naive(data, depth).root
            )
        assert(PositionalMerkleTree.rootFromElements(Iterator.empty, 256).length == 32)
    }

    test("depth zero has one slot and empty single and multiproofs") {
        val data = elements(1)
        val tree = PositionalMerkleTree.fromElements(data, 0)
        assert(tree.rootHash == blake2b_256(data.head))
        assert(tree.proveMembership(0) == ByteString.empty)
        compare(data, 0, Vector(0))
        assert(
          returned(
            singleResult(Single(tree.rootHash, 0, 0, data.head, ByteString.empty))
          ) == PositionalMerkleTree.EmptyLeafHash
        )
    }

    test(
      "off-chain builders reject capacity overflow, bad depths, indices, ordering and repeat emptying"
    ) {
        for depth <- Seq(-1, 30) do
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.fromElements(elements(1), depth)
            )
        for depth <- Seq(-1, 257) do
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.rootFromElements(elements(1), depth)
            )
        assertThrows[IllegalArgumentException](PositionalMerkleTree.fromElements(elements(3), 1))
        assertThrows[IllegalArgumentException](
          PositionalMerkleTree.rootFromElements(elements(3).iterator, 1)
        )
        val tree = PositionalMerkleTree.fromElements(elements(3), 2)
        for position <- Seq(-1, 4, Int.MaxValue) do
            assertThrows[IllegalArgumentException](tree.proveMembership(position))
        for positions <- Seq(Vector.empty, Vector(1, 1), Vector(2, 1), Vector(-1), Vector(4)) do
            assertThrows[IllegalArgumentException](tree.proveMulti(positions))
            assertThrows[IllegalArgumentException](tree.emptied(positions))
        assertThrows[IllegalArgumentException](tree.emptied(Vector(3)))
        assertThrows[IllegalArgumentException](tree.emptied(Vector(1)).emptied(Vector(1)))
    }
}
