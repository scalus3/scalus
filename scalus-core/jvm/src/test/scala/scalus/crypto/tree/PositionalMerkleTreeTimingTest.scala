package scalus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** Wall-clock samples of the two off-chain builders. JVM only: it builds a 32,768-leaf tree 17
  * times, which the Scala.js and Native test runs have no use for.
  */
class PositionalMerkleTreeTimingTest extends AnyFunSuite {
    private val depth = 15
    private val data = Vector.tabulate(1 << depth) { i =>
        val bytes = new Array[Byte](32)
        bytes(28) = (i >>> 24).toByte
        bytes(29) = (i >>> 16).toByte
        bytes(30) = (i >>> 8).toByte
        bytes(31) = i.toByte
        ByteString.fromArray(bytes)
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
