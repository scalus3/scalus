package scalus.crypto.tree

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, JsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.cardano.onchain.plutus.crypto.tree.PositionalMerkleTree as OnChain
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.compiler.Options
import scalus.uplc.{Constant, PlutusV3, Program, Term}
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.{platform, ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}

object PositionalMerkleTreeTestSupport {
    case class Single(
        root: ByteString,
        depth: BigInt,
        position: BigInt,
        element: ByteString,
        proof: ByteString
    ) derives FromData,
          ToData

    case class Multi(
        root: ByteString,
        depth: BigInt,
        leaves: SList[OnChain.Leaf],
        proof: ByteString
    ) derives FromData,
          ToData

    case class SingleVector(position: Int, occupied: Boolean, proof: String, emptiedRoot: String)
    case class MultiVector(
        positions: Vector[Int],
        helperIndices: Vector[Int],
        proof: String,
        emptiedRoot: String
    )
    case class ReferenceVector(
        algorithm: String,
        depth: Int,
        shape: String,
        count: Int,
        root: String,
        singles: Vector[SingleVector],
        multis: Vector[MultiVector]
    )

    private given JsonValueCodec[Vector[ReferenceVector]] = JsonCodecMaker.make
    val referenceDepths: Vector[Int] = Vector(1, 2, 3, 4, 5, 6, 15)
    val referenceHashes: Vector[String] = Vector("sha256", "blake2b256")
    lazy val vectors: Vector[ReferenceVector] = readFromArray(
      platform.readFile(
        "scalus-core/shared/src/test/resources/positional-merkle-tree/ssz-v0.1.0.json"
      )
    )
    lazy val legacyVectors: Vector[ReferenceVector] = readFromArray(
      platform.readFile(
        "scalus-core/shared/src/test/resources/positional-merkle-tree/ssz-v1.5.0.json"
      )
    )

    def referenceCases(
        corpus: Vector[ReferenceVector],
        algorithm: String,
        depth: Int
    ): Vector[ReferenceVector] = {
        val cases = corpus.filter(v => v.algorithm == algorithm && v.depth == depth)
        val capacity = 1 << depth
        val expectedCounts = Set(0, math.max(1, capacity * 3 / 5), capacity) ++
            Vector(1, 3, 5, capacity - 1).filter(_ <= capacity)
        assert(cases.nonEmpty, s"No $algorithm/depth=$depth reference cases")
        assert(cases.map(_.count).toSet == expectedCounts)
        assert(cases.size == expectedCounts.size)
        assert(cases.forall(_.singles.nonEmpty))
        assert(cases.forall(v => v.singles.forall(s => s.occupied == (s.position < v.count))))
        assert(cases.filter(_.count > 0).forall(_.multis.nonEmpty))
        cases
    }

    def assertReferenceHash(actual: ByteString, expectedHex: String): Unit =
        org.scalatest.Assertions.assert(actual == ByteString.fromHex(expectedHex))

    def element(index: Int): ByteString = ByteString.fromString(s"element-${index % 7}")
    def uniqueElement(index: Int): ByteString = ByteString.fromString(s"unique-element-$index")
    def elements(count: Int): Vector[ByteString] = Vector.tabulate(count)(element)
    def concat(hashes: Iterable[ByteString]): ByteString =
        ByteString.fromArray(hashes.iterator.flatMap(_.bytes).toArray)
    def flip(bytes: ByteString, offset: Int): ByteString = {
        val mutated = bytes.bytes.clone()
        mutated(offset) = (mutated(offset) ^ 1).toByte
        ByteString.fromArray(mutated)
    }

    /** Full independent rebuilding, deliberately without frontier or path-update logic. */
    final class Naive(
        val chunks: Vector[ByteString],
        nodeHash: ByteString => ByteString = blake2b_256
    ) {
        val depth: Int = Integer.numberOfTrailingZeros(chunks.size)
        private val levels: Vector[Vector[ByteString]] = {
            var layers = Vector(chunks)
            while layers.last.size > 1 do
                layers :+= layers.last
                    .grouped(2)
                    .map { pair =>
                        nodeHash(appendByteString(pair(0), pair(1)))
                    }
                    .toVector
            layers
        }
        def root: ByteString = levels.last.head
        def proof(position: Int): ByteString =
            concat((0 until depth).map(level => levels(level)((position >> level) ^ 1)))

        def helpers(positions: Vector[Int]): Vector[(Int, ByteString)] = {
            def collect(
                index: Int,
                start: Int,
                size: Int,
                selected: Vector[Int]
            ): Vector[(Int, ByteString)] =
                if selected.isEmpty then
                    val height = Integer.numberOfTrailingZeros(size)
                    Vector(index -> levels(height)(start / size))
                else if size == 1 then Vector.empty
                else
                    val half = size / 2
                    collect(index * 2, start, half, selected.filter(_ < start + half)) ++
                        collect(
                          index * 2 + 1,
                          start + half,
                          half,
                          selected.filter(_ >= start + half)
                        )
            collect(1, 0, chunks.size, positions).sortBy(-_._1)
        }
        def multiProof(positions: Vector[Int]): ByteString = concat(helpers(positions).map(_._2))
        def emptied(positions: Iterable[Int]): Naive =
            new Naive(
              chunks.indices.map { i =>
                  if positions.toSet.contains(i) then PositionalMerkleTree.EmptyLeafHash
                  else chunks(i)
              }.toVector,
              nodeHash
            )
    }

    def naive(
        data: Vector[ByteString],
        depth: Int,
        nodeHash: ByteString => ByteString = blake2b_256
    ): Naive =
        new Naive(
          data.map(blake2b_256).padTo(1 << depth, PositionalMerkleTree.EmptyLeafHash),
          nodeHash
        )

    def query(tree: PositionalMerkleTree, data: Vector[ByteString], positions: Vector[Int]): Multi =
        Multi(
          tree.rootHash,
          BigInt(tree.depth),
          SList.from(positions.map(p => OnChain.Leaf(BigInt(p), data(p)))),
          tree.proveMulti(positions)
        )

    private given Options = Options.release
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    lazy val singleProgram: Program = PlutusV3.compile { (data: Data) =>
        val input = data.to[Single]
        OnChain.verifyAndEmpty(input.root, input.depth, input.position, input.element, input.proof)
    }.program

    lazy val membershipProgram: Program = PlutusV3.compile { (data: Data) =>
        val input = data.to[Single]
        OnChain.verifyMembership(
          input.root,
          input.depth,
          input.position,
          input.element,
          input.proof
        )
    }.program

    lazy val multiProgram: Program = PlutusV3.compile { (data: Data) =>
        val input = data.to[Multi]
        OnChain.verifyAndEmptyMany(input.root, input.depth, input.leaves, input.proof)
    }.program

    def evaluate(program: Program, input: Data): Result =
        (program.term $ input.asTerm).evaluateDebug

    def singleResult(input: Single): Result = evaluate(singleProgram, input.toData)
    def membershipResult(input: Single): Result = evaluate(membershipProgram, input.toData)
    def multiResult(input: Multi): Result = evaluate(multiProgram, input.toData)

    def returned(result: Result): ByteString = result match
        case success: Result.Success =>
            success.term match
                case Term.Const(Constant.ByteString(bytes), _) => bytes
                case other => throw new AssertionError(s"Expected a byte string, got $other")
        case failure: Result.Failure => throw new AssertionError(failure.toString)
}
