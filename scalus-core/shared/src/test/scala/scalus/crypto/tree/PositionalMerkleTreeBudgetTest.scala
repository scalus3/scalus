package scalus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.cardano.onchain.plutus.crypto.tree.PositionalMerkleTree as OnChain
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.compiler.Options
import scalus.crypto.tree.PositionalMerkleTreeTestSupport.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData

object PositionalMerkleTreeBudgetTest {
    case class Removal(position: BigInt, element: ByteString, proof: ByteString)
        derives FromData,
          ToData
    case class Sequential(root: ByteString, depth: BigInt, removals: SList[Removal])
        derives FromData,
          ToData

    private given Options = Options.release
    lazy val sequentialProgram = PlutusV3.compile { (data: Data) =>
        val input = data.to[Sequential]
        input.removals.foldLeft(input.root) { (root, removal) =>
            OnChain.verifyAndEmpty(
              root,
              input.depth,
              removal.position,
              removal.element,
              removal.proof
            )
        }
    }.program
}

class PositionalMerkleTreeBudgetTest extends AnyFunSuite {
    import PositionalMerkleTreeBudgetTest.*

    private val depth = 15
    private val data = Vector.tabulate(1 << depth) { i =>
        val bytes = new Array[Byte](32)
        bytes(28) = (i >>> 24).toByte
        bytes(29) = (i >>> 16).toByte
        bytes(30) = (i >>> 8).toByte
        bytes(31) = i.toByte
        ByteString.fromArray(bytes)
    }

    test("budget: depth 15 single, sequential single proofs and SSZ multiproofs") {
        val tree = PositionalMerkleTree.fromElements(data, depth)
        val input = Single(tree.rootHash, BigInt(depth), 0, data.head, tree.proveMembership(0))
        val single = singleResult(input)
        assert(returned(single) == tree.emptied(Vector(0)).rootHash)
        assert(single.budget == ExUnits(memory = 161238, steps = 46_503274))
        println(
          "positional-merkle depth=15 elements=32768 element-bytes=32 cost-model=CardanoInfo.mainnet/PlutusV3 Options.release"
        )
        println(
          "pattern,k,single-proof-bytes,single-cpu,single-memory,multi-proof-bytes,multi-cpu,multi-memory"
        )
        println(
          s"single,1,${input.proof.size},${single.budget.steps},${single.budget.memory},,,"
        )
        assert(input.proof.size == 480)

        for pattern <- Seq("contiguous", "scattered"); k <- Seq(2, 4, 8, 16, 32) do
            val positions =
                if pattern == "contiguous" then (0 until k).toVector
                else (0 until k).map(i => (i * 7919) % tree.capacity).sorted.toVector
            var current = tree
            val removals = positions.map { position =>
                val removal =
                    Removal(BigInt(position), data(position), current.proveMembership(position))
                current = current.emptied(Vector(position))
                removal
            }
            val sequential = evaluate(
              sequentialProgram,
              Sequential(tree.rootHash, BigInt(depth), SList.from(removals)).toData
            )
            val multiInput = query(tree, data, positions)
            val multi = multiResult(multiInput)
            assert(returned(sequential) == current.rootHash)
            assert(returned(multi) == current.rootHash)
            assert(multiInput.proof.size < k * depth * 32)
            if k == 32 then
                val expected =
                    if pattern == "contiguous" then ExUnits(memory = 1275114, steps = 364_828100)
                    else ExUnits(memory = 7253570, steps = 2049_031158)
                assert(multi.budget == expected)
            println(
              s"$pattern,$k,${removals.map(_.proof.size).sum},${sequential.budget.steps},${sequential.budget.memory},${multiInput.proof.size},${multi.budget.steps},${multi.budget.memory}"
            )
    }

    test("measurement: streaming and materialized builders for 32768 leaves") {
        val expected = PositionalMerkleTree.fromElements(data, depth).rootHash
        for _ <- 0 until 3 do
            assert(PositionalMerkleTree.rootFromElements(data.iterator, depth) == expected)
            assert(PositionalMerkleTree.fromElements(data, depth).rootHash == expected)
        def time(root: => ByteString): Double = {
            val start = System.nanoTime()
            val result = root
            val elapsed = (System.nanoTime() - start) / 1e6
            assert(result == expected)
            elapsed
        }
        val streaming =
            Vector.fill(5)(time(PositionalMerkleTree.rootFromElements(data.iterator, depth)))
        val materialized =
            Vector.fill(5)(time(PositionalMerkleTree.fromElements(data, depth).rootHash))
        println(
          s"positional-merkle builder 32768x32 bytes, depth=15, 3 warmups, 5 samples, input preparation excluded: streaming-ms=${streaming.mkString(",")}; materialized-ms=${materialized.mkString(",")}"
        )
    }
}
