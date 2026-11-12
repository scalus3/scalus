package scalus.crypto.tree

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.exceptions.TestFailedException
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.crypto.tree.PositionalMerkleTree as OnChain
import scalus.cardano.onchain.plutus.prelude.{require as onChainRequire, List as SList}
import scalus.compiler.{Compile, Options}
import scalus.crypto.tree.PositionalMerkleTreeTestSupport.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Builtins.{appendByteString, blake2b_256, sha2_256}
import scalus.uplc.builtin.Data.toData

/** Test adapter: the authenticated state and actual payout must bind the proof's root and slot. */
@Compile
object PositionalPayoutBinding {
    case class Request(currentRoot: ByteString, paidPosition: BigInt, membership: Single)
        derives FromData,
          ToData

    def verify(request: Request): ByteString = {
        val claim = request.membership
        onChainRequire(claim.root == request.currentRoot, "Payout: current root required")
        onChainRequire(claim.position == request.paidPosition, "Payout: position mismatch")
        OnChain.verifyAndEmpty(
          claim.root,
          claim.depth,
          claim.position,
          claim.element,
          claim.proof
        )
    }
}

class PositionalMerkleTreeRegressionTest extends AnyFunSuite {
    private val data = Vector.tabulate(8)(uniqueElement)
    private lazy val tree = PositionalMerkleTree.fromElements(data, 3)
    private lazy val model = naive(data, 3)
    private given Options = Options.release
    private lazy val payoutProgram = PlutusV3.compile { (data: Data) =>
        PositionalPayoutBinding.verify(data.to[PositionalPayoutBinding.Request])
    }.program

    private def single(t: PositionalMerkleTree, element: ByteString, position: Int): Single =
        Single(t.rootHash, BigInt(t.depth), BigInt(position), element, t.proveMembership(position))

    private def accepts(input: Single, expected: ByteString): Unit = {
        assert(
          OnChain.verifyAndEmpty(
            input.root,
            input.depth,
            input.position,
            input.element,
            input.proof
          ) == expected
        )
        assert(returned(singleResult(input)) == expected)
        assert(
          OnChain.verifyMembership(
            input.root,
            input.depth,
            input.position,
            input.element,
            input.proof
          ) == input.root
        )
        assert(returned(membershipResult(input)) == input.root)
    }

    private def accepts(input: Multi, expected: ByteString): Unit = {
        assert(
          OnChain.verifyAndEmptyMany(input.root, input.depth, input.leaves, input.proof) == expected
        )
        assert(returned(multiResult(input)) == expected)
    }

    private def rejects(input: Single): Unit = {
        intercept[RequirementError](
          OnChain.verifyAndEmpty(
            input.root,
            input.depth,
            input.position,
            input.element,
            input.proof
          )
        )
        intercept[RequirementError](
          OnChain.verifyMembership(
            input.root,
            input.depth,
            input.position,
            input.element,
            input.proof
          )
        )
        assert(singleResult(input).isFailure)
        assert(membershipResult(input).isFailure)
    }

    private def rejects(input: Multi): Unit = {
        intercept[RequirementError](
          OnChain.verifyAndEmptyMany(input.root, input.depth, input.leaves, input.proof)
        )
        assert(multiResult(input).isFailure)
    }

    private def zeros(
        depth: Int,
        hash: ByteString => ByteString = blake2b_256
    ): Vector[ByteString] =
        Vector.iterate(PositionalMerkleTree.EmptyLeafHash, depth + 1)(h =>
            hash(appendByteString(h, h))
        )

    private def siblings(proof: ByteString): Vector[ByteString] =
        proof.bytes.grouped(32).map(ByteString.fromArray).toVector

    test("every claim is authenticated, including extra out-of-range and negative claims") {
        val one = query(tree, data, Vector(0))
        for position <- Vector(BigInt(8), BigInt(-1)) do
            rejects(
              one.copy(leaves =
                  SList(OnChain.Leaf(0, data.head), OnChain.Leaf(position, uniqueElement(99)))
              )
            )
            assertThrows[IllegalArgumentException](tree.proveMulti(Vector(0, position.toInt)))
            assertThrows[IllegalArgumentException](tree.emptied(Vector(0, position.toInt)))
        val input = query(tree, data, Vector(0, 3, 7))
        accepts(input, model.emptied(Vector(0, 3, 7)).root)
        val leaves = input.leaves.toScalaList.toVector
        for i <- leaves.indices do
            rejects(
              input.copy(leaves =
                  SList.from(leaves.updated(i, leaves(i).copy(element = uniqueElement(99))))
              )
            )
    }

    test("repeated positions cannot spend twice with equal, conflicting or nonadjacent claims") {
        val one = query(tree, data, Vector(0))
        for leaves <- Vector(
              Vector(OnChain.Leaf(0, data.head), OnChain.Leaf(0, data.head)),
              Vector(OnChain.Leaf(0, uniqueElement(99)), OnChain.Leaf(0, data.head)),
              Vector(
                OnChain.Leaf(0, data.head),
                OnChain.Leaf(1, data(1)),
                OnChain.Leaf(0, data.head)
              )
            )
        do
            rejects(one.copy(leaves = SList.from(leaves)))
            val positions = leaves.map(_.position.toInt)
            assertThrows[IllegalArgumentException](tree.proveMulti(positions))
            assertThrows[IllegalArgumentException](tree.emptied(positions))
        val equal = Vector.fill(8)(data.head)
        val t = PositionalMerkleTree.fromElements(equal, 3)
        accepts(query(t, equal, Vector(0, 1)), naive(equal, 3).emptied(Vector(0, 1)).root)
        for first <- Vector(data.head, uniqueElement(99)) do
            rejects(
              Multi(
                blake2b_256(data.head),
                0,
                SList(OnChain.Leaf(0, first), OnChain.Leaf(0, data.head)),
                ByteString.empty
              )
            )
    }

    test(
      "the current root is required after emptying, including stale helpers for untouched leaves"
    ) {
        val old0 = single(tree, data.head, 0)
        val old1 = single(tree, data(1), 1)
        val next = tree.emptied(Vector(0))
        assert(next.rootHash == model.emptied(Vector(0)).root)
        accepts(old0, next.rootHash)
        rejects(old0.copy(root = next.rootHash))
        rejects(old1.copy(root = next.rootHash))
        rejects(query(tree, data, Vector(1, 6)).copy(root = next.rootHash))
        accepts(single(next, data(1), 1), model.emptied(Vector(0, 1)).root)
        accepts(query(next, data, Vector(1, 6)), model.emptied(Vector(0, 1, 6)).root)
        // Pure verification against an old root remains valid; the caller supplies current state.
        accepts(old0, next.rootHash)
        assert(tree.rootHash == old0.root)
    }

    test("shared ancestors use all replacements and all-leaf emptying needs zero helpers") {
        val positions = Vector(0, 1, 6)
        val expected = model.emptied(positions).root
        accepts(query(tree, data, positions), expected)
        val emptied = tree.emptied(positions)
        var sequential = tree
        for position <- positions do
            val next = sequential.emptied(Vector(position))
            accepts(single(sequential, data(position), position), next.rootHash)
            sequential = next
        assert(sequential.rootHash == expected && emptied.rootHash == expected)
        for position <- data.indices if !positions.contains(position) do
            val input = single(emptied, data(position), position)
            accepts(input, model.emptied(positions :+ position).root)
        val all = query(tree, data, data.indices.toVector)
        assert(all.proof == ByteString.empty)
        accepts(all, zeros(3).last)
    }

    test("helpers exclude every reconstructible ancestor, including the SSZ 1423 example") {
        val positions = Vector(0, 1, 2, 3, 4)
        val helpers = model.helpers(positions)
        assert(helpers.map(_._1) == Vector(13, 7))
        val input = query(tree, data, positions)
        assert(input.proof == concat(helpers.map(_._2)))
        accepts(input, model.emptied(positions).root)
        val ancestor3 = model.helpers(Vector(0)).find(_._1 == 3).get._2
        rejects(input.copy(proof = concat(Vector(input.proof, ancestor3))))
    }

    test("descending helpers stay paired with ascending positions and their elements") {
        val positions = Vector(0, 1, 6)
        assert(model.helpers(positions).map(_._1) == Vector(15, 6, 5))
        val input = query(tree, data, positions)
        val hashes = siblings(input.proof)
        assert(hashes.distinct.size == 3)
        accepts(input, model.emptied(positions).root)
        rejects(input.copy(proof = concat(hashes.updated(0, hashes(1)).updated(1, hashes(0)))))
        rejects(input.copy(proof = concat(hashes.reverse)))
        rejects(
          input.copy(leaves =
              SList(OnChain.Leaf(0, data(1)), OnChain.Leaf(1, data.head), OnChain.Leaf(6, data(6)))
          )
        )
        for p <- data.indices do
            assert(tree.proveMulti(Vector(p)) == tree.proveMembership(p))
            accepts(query(tree, data, Vector(p)), model.emptied(Vector(p)).root)
    }

    test(
      "computed parents remain scheduled for dense and scattered sibling groups at depths 1 to 6"
    ) {
        for depth <- 1 to 6 do
            val capacity = 1 << depth
            val values = Vector.tabulate(capacity)(uniqueElement)
            val t = PositionalMerkleTree.fromElements(values, depth)
            val reference = naive(values, depth)
            for selected <- Vector(
                  Vector(0, 1, 2, 3, 4, capacity - 1).filter(_ < capacity).distinct.sorted,
                  Vector(0, 1, capacity / 2, capacity / 2 + 1, capacity - 1)
                      .filter(_ < capacity)
                      .distinct
                      .sorted,
                  values.indices.toVector
                )
            do
                val input = query(t, values, selected)
                assert(input.proof == reference.multiProof(selected))
                accepts(input, reference.emptied(selected).root)
    }

    test("empty queries differ from nonempty queries with zero helpers") {
        for proof <- Vector(ByteString.empty, tree.rootHash) do
            rejects(Multi(tree.rootHash, 3, SList.empty, proof))
        assertThrows[IllegalArgumentException](tree.proveMulti(Vector.empty))
        assertThrows[IllegalArgumentException](tree.emptied(Vector.empty))
        accepts(query(tree, data, data.indices.toVector), zeros(3).last)
        val input = query(tree, data, Vector(0, 1, 6))
        for proof <- Vector(
              input.proof.drop(32),
              input.proof.drop(1),
              concat(Vector(input.proof, zeros(0).head)),
              concat(Vector(input.proof, ByteString.fromArray(Array[Byte](1))))
            )
        do rejects(input.copy(proof = proof))
        val one = PositionalMerkleTree.fromElements(Vector(data.head), 0)
        accepts(single(one, data.head, 0), zeros(0).head)
        accepts(query(one, Vector(data.head), Vector(0)), zeros(0).head)
    }

    test("zero subtrees have the exact height and hash for partial trees and rebuilt prefixes") {
        for depth <- Vector(3, 15); count <- Vector(0, 1, 3, 5, (1 << depth) - 1) do
            val values = elements(count)
            for algorithm <- referenceHashes do
                val hash = if algorithm == "sha256" then sha2_256 else blake2b_256
                val t = PositionalMerkleTree.fromElements(values, depth, hash)
                val reference = naive(values, depth, hash)
                assert(t.rootHash == reference.root)
                assert(
                  PositionalMerkleTree.rootFromElements(
                    values.iterator,
                    depth,
                    hash
                  ) == reference.root
                )
                if count > 0 then
                    val p = count - 1
                    val rebuilt = PositionalMerkleTree.fromElements(values.take(p), depth, hash)
                    assert(t.emptied(Vector(p)).rootHash == rebuilt.rootHash)
                    assert(rebuilt.rootHash == reference.emptied(Vector(p)).root)
                    if algorithm == "blake2b256" then
                        accepts(single(t, values(p), p), rebuilt.rootHash)
                        accepts(query(t, values, Vector(p)), rebuilt.rootHash)
                else if algorithm == "blake2b256" then rejects(single(t, data.head, 0))
            val t = PositionalMerkleTree.fromElements(Vector(data.head), depth)
            val input = single(t, data.head, 0)
            val hashes = siblings(input.proof)
            assert(hashes(1) == zeros(1).last)
            assert(zeros(1).last != zeros(1, sha2_256).last)
            for bad <- Vector(zeros(0).head, zeros(1, sha2_256).last) do
                rejects(input.copy(proof = concat(hashes.updated(1, bad))))
                rejects(
                  query(t, Vector(data.head), Vector(0))
                      .copy(proof = concat(hashes.updated(1, bad)))
                )
    }

    test("equal subtrees identify different positions and immutable histories retain their roots") {
        val values = Vector.tabulate(8)(i => data(i % 2))
        val original = PositionalMerkleTree.fromElements(values, 3)
        assert(original.proveMembership(0).drop(64) == original.proveMembership(4).drop(64))
        val reference = naive(values, 3)
        var current = original
        var cleared = Vector.empty[Int]
        val snapshots = collection.mutable.ArrayBuffer.empty[(PositionalMerkleTree, ByteString)]
        for p <- Vector(0, 4, 2, 6, 1, 5, 3, 7) do
            snapshots += current -> current.rootHash
            val input = single(current, values(p), p)
            cleared :+= p
            val next = current.emptied(Vector(p))
            accepts(input, reference.emptied(cleared).root)
            assert(next.rootHash == reference.emptied(cleared).root)
            rejects(single(next, values(p), p))
            current = next
        for (snapshot, root) <- snapshots do assert(snapshot.rootHash == root)
        assert(original.rootHash == reference.root)
        assert(current.rootHash == zeros(3).last)
    }

    test("generalized index arithmetic never wraps or rounds at any supported depth") {
        val zero = zeros(256)
        val element = data.head
        var rightmost = blake2b_256(element)
        for depth <- 0 to 256 do
            if depth > 0 then rightmost = blake2b_256(appendByteString(zero(depth - 1), rightmost))
            val capacity = BigInt(1) << depth
            val p = capacity - 1
            val gindex = capacity + p
            assert(gindex.bitLength - 1 == depth)
            val input = Single(rightmost, BigInt(depth), p, element, concat(zero.take(depth)))
            accepts(input, zero(depth))
            accepts(
              Multi(rightmost, BigInt(depth), SList(OnChain.Leaf(p, element)), input.proof),
              zero(depth)
            )
            for invalid <- Vector(capacity, BigInt(-1)) do
                rejects(input.copy(position = invalid))
                rejects(
                  Multi(
                    rightmost,
                    BigInt(depth),
                    SList(OnChain.Leaf(invalid, element)),
                    input.proof
                  )
                )
            assert(PositionalMerkleTree.rootFromElements(Iterator.empty, depth) == zero(depth))
        for depth <- 0 to 29 do
            val capacity = PositionalMerkleTree.capacityAtDepth(depth)
            val last = capacity - 1
            assert(
              BigInt(PositionalMerkleTree.leafIndex(depth, last)) == (BigInt(1) << (depth + 1)) - 1
            )
            assertThrows[IllegalArgumentException](PositionalMerkleTree.leafIndex(depth, capacity))
            assertThrows[IllegalArgumentException](PositionalMerkleTree.leafIndex(depth, -1))
        for depth <- Vector(-1, 30, 31, 32, 63, 64, 257) do
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.fromElements(Vector(data.head), depth)
            )
        for depth <- Vector(-1, 257, Int.MaxValue) do
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.rootFromElements(Vector(data.head), depth)
            )
        assert(((BigInt(1) << 53) - 1).bitLength - 1 == 52)
    }

    test("node widths cannot change concatenation boundaries or truncate custom digests") {
        for width <- Vector(0, 31, 33, 64) do
            val bad: ByteString => ByteString = _ => ByteString.fromArray(new Array[Byte](width))
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.fromElements(Vector(data.head), 1, bad)
            )
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.rootFromElements(Vector(data.head), 1, bad)
            )
        val failure = new IllegalStateException("digest failed")
        val broken: ByteString => ByteString = _ => throw failure
        assert(
          intercept[IllegalStateException](
            PositionalMerkleTree.fromElements(Vector(data.head), 1, broken)
          ) eq failure
        )
        assert(
          intercept[IllegalStateException](
            PositionalMerkleTree.rootFromElements(Vector(data.head), 1, broken)
          ) eq failure
        )
        var invalid = false
        val digest: ByteString => ByteString = bytes =>
            if invalid then ByteString.fromArray(new Array[Byte](31)) else blake2b_256(bytes)
        val t = PositionalMerkleTree.fromElements(data.take(2), 1, digest)
        val root = t.rootHash
        invalid = true
        assertThrows[IllegalArgumentException](t.emptied(Vector(0)))
        assert(t.rootHash == root)
        val input = single(tree, data.head, 0)
        for proof <- Vector(
              input.proof.take(input.proof.size - 1),
              concat(Vector(input.proof, ByteString.fromArray(Array[Byte](0))))
            )
        do rejects(input.copy(proof = proof))
        for root <- Vector(
              input.root.take(31),
              concat(Vector(input.root, ByteString.fromArray(Array[Byte](0))))
            )
        do rejects(input.copy(root = root))
        val two = PositionalMerkleTree.fromElements(data.take(4), 2)
        val canonical = single(two, data.head, 0)
        val short = canonical.proof.take(31)
        val long = concat(
          Vector(
            ByteString.fromArray(Array((canonical.proof(31) ^ 1).toByte)),
            canonical.proof.drop(32)
          )
        )
        assert(short.size == 31 && long.size == 33)
        rejects(canonical.copy(proof = concat(Vector(short, long))))
        // A flat proof has no claimed field boundaries: identical wire bytes remain canonical.
        val repartitioned = concat(Vector(canonical.proof.take(31), canonical.proof.drop(31)))
        assert(repartitioned == canonical.proof)
        accepts(
          canonical.copy(proof = repartitioned),
          naive(data.take(4), 2).emptied(Vector(0)).root
        )
        val emptyProof = Single(blake2b_256(data.head), 0, 0, data.head, ByteString.empty)
        accepts(emptyProof, zeros(0).head)
        rejects(emptyProof.copy(proof = ByteString.fromArray(Array[Byte](0))))
    }

    test("the proof position is the only updated position and must match the actual payout") {
        val claim = single(tree, data.head, 0)
        val request = PositionalPayoutBinding.Request(tree.rootHash, 0, claim)
        val expected = model.emptied(Vector(0)).root
        assert(PositionalPayoutBinding.verify(request) == expected)
        assert(returned(evaluate(payoutProgram, request.toData)) == expected)
        for bad <- Vector(
              request.copy(paidPosition = 1),
              request.copy(paidPosition = 8, membership = claim.copy(position = 8)),
              request.copy(currentRoot = tree.emptied(Vector(0)).rootHash)
            )
        do
            intercept[RequirementError](PositionalPayoutBinding.verify(bad))
            assert(evaluate(payoutProgram, bad.toData).isFailure)
        assert(expected != model.emptied(Vector(1)).root)
    }

    test("capacity and configured depth are authenticated independently of proof length") {
        for depth <- 1 to 6 do
            val capacity = 1 << depth
            val values = Vector.tabulate(capacity + 1)(uniqueElement)
            assertThrows[IllegalArgumentException](PositionalMerkleTree.fromElements(values, depth))
            assertThrows[IllegalArgumentException](
              PositionalMerkleTree.rootFromElements(values.iterator, depth)
            )
            val t = PositionalMerkleTree.fromElements(values.take(capacity), depth)
            val input = single(t, values.head, 0)
            accepts(input, naive(values.take(capacity), depth).emptied(Vector(0)).root)
            rejects(input.copy(depth = depth - 1))
            rejects(input.copy(depth = depth + 1))
            rejects(input.copy(proof = input.proof.drop(32)))
            rejects(input.copy(proof = concat(Vector(input.proof, zeros(0).head))))
            rejects(input.copy(root = blake2b_256(values.head), proof = ByteString.empty))
    }

    test(
      "differential cases execute every depth, hash and applicable backend and detect corruption"
    ) {
        assert(zeros(3).last != zeros(3, sha2_256).last)
        for algorithm <- referenceHashes; depth <- referenceDepths do
            val cases = referenceCases(vectors, algorithm, depth)
            assertThrows[AssertionError](
              referenceCases(
                vectors.filterNot(v => v.algorithm == algorithm && v.depth == depth),
                algorithm,
                depth
              )
            )
            assertThrows[AssertionError](
              referenceCases(vectors.filterNot(_ == cases.head), algorithm, depth)
            )
            val v = cases.find(_.shape == "full").get
            val hash = if algorithm == "sha256" then sha2_256 else blake2b_256
            val t = PositionalMerkleTree.fromElements(elements(v.count), depth, hash)
            val expected = ByteString.fromHex(v.root)
            val corrupt = v.copy(root = flip(expected, 0).toHex)
            assertReferenceHash(t.rootHash, v.root)
            assertThrows[TestFailedException](
              assertReferenceHash(t.rootHash, corrupt.root)
            )
            val streamed =
                PositionalMerkleTree.rootFromElements(elements(v.count).iterator, depth, hash)
            assertReferenceHash(streamed, v.root)
            assertThrows[TestFailedException](assertReferenceHash(streamed, corrupt.root))
            if algorithm == "blake2b256" then
                val s = v.singles.find(_.occupied).get
                val input = Single(
                  expected,
                  BigInt(depth),
                  BigInt(s.position),
                  element(s.position),
                  ByteString.fromHex(s.proof)
                )
                val wanted = ByteString.fromHex(s.emptiedRoot)
                val bad = s.copy(emptiedRoot = flip(wanted, 0).toHex)
                val scalaRoot = OnChain.verifyAndEmpty(
                  input.root,
                  input.depth,
                  input.position,
                  input.element,
                  input.proof
                )
                val uplcRoot = returned(singleResult(input))
                assertReferenceHash(scalaRoot, s.emptiedRoot)
                assertReferenceHash(uplcRoot, s.emptiedRoot)
                assertThrows[TestFailedException](assertReferenceHash(scalaRoot, bad.emptiedRoot))
                assertThrows[TestFailedException](assertReferenceHash(uplcRoot, bad.emptiedRoot))
    }
}
