package scalus.cardano.onchain.plutus.crypto.tree

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}
import scalus.uplc.builtin.ByteString.hex

/** Positional membership and verify-and-empty for an SSZ chunk tree with BLAKE2b-256.
  *
  * The caller must authenticate both root and depth (0..256) in a datum or script parameter.
  * Positions are zero-based integers below 2^depth. Leaves hash the element once; empty leaves are
  * zero32; parents hash `left ++ right`, without tags or length mixing. Exact depth prevents a leaf
  * from being interpreted as an internal node. Empty leaves have no known element preimage.
  *
  * Verification authenticates membership against the root it receives. The caller must bind that
  * root to current state and every verified position to the actual payout or changed slot. Old
  * proofs still verify against their old roots; this library does not establish root freshness.
  */
@Compile
object PositionalMerkleTree {

    /** Reserved empty chunk, shared by unused and emptied slots. */
    val EmptyLeafHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** A position and its unhashed element. Multiproof queries must be strictly ascending. */
    case class Leaf(position: BigInt, element: ByteString) derives FromData, ToData

    private case class Node(position: BigInt, oldHash: ByteString, newHash: ByteString)

    /** Verify a positional proof and return the root with that occupied slot emptied.
      *
      * `proof` is exactly `depth` sibling hashes (32 bytes each), bottom-up. Directions come from
      * the position's bits, least significant first. Malformed proofs or empty slots fail. Both
      * roots are recomputed together in one walk.
      */
    def verifyAndEmpty(
        root: ByteString,
        depth: BigInt,
        position: BigInt,
        element: ByteString,
        proof: ByteString
    ): ByteString = {
        checkSingle(depth, position, proof)
        val leaf = nonEmptyLeaf(blake2b_256(element))
        emptyUp(root, position, leaf, EmptyLeafHash, proof, 0, depth * 32)
    }

    /** Verify membership and return the unchanged root, failing on invalid proofs or empty slots.
      * Uses the same position and proof encoding as [[verifyAndEmpty]].
      */
    def verifyMembership(
        root: ByteString,
        depth: BigInt,
        position: BigInt,
        element: ByteString,
        proof: ByteString
    ): ByteString = {
        checkSingle(depth, position, proof)
        val computed =
            membershipUp(position, nonEmptyLeaf(blake2b_256(element)), proof, 0, depth * 32)
        require(computed == root, "PositionalMerkleTree: root mismatch")
        root
    }

    /** Verify an SSZ multiproof and empty all selected occupied slots in one topology walk.
      *
      * `leaves` must be nonempty with strictly ascending, distinct, in-range positions. The proof
      * contains exactly the helpers from SSZ `get_helper_indices(2^depth + positions)`, in
      * descending generalized-index order, concatenated as 32-byte hashes. Every helper is
      * consumed; missing, extra and trailing bytes fail. Equal elements at different positions are
      * allowed.
      */
    def verifyAndEmptyMany(
        root: ByteString,
        depth: BigInt,
        leaves: List[Leaf],
        proof: ByteString
    ): ByteString = {
        val capacity = capacityAtDepth(depth)
        require(!leaves.isEmpty, "PositionalMerkleTree: empty query")
        val nodes = leafNodes(leaves, -1, capacity, Nil)
        emptyLevels(root, nodes, depth, proof, 0)
    }

    private def capacityAtDepth(depth: BigInt): BigInt = {
        require(depth >= 0 && depth <= 256, "PositionalMerkleTree: depth out of range")
        powerOfTwo(depth, 1)
    }

    private def powerOfTwo(depth: BigInt, result: BigInt): BigInt =
        if depth <= 0 then result else powerOfTwo(depth - 1, result * 2)

    private def checkSingle(depth: BigInt, position: BigInt, proof: ByteString): Unit = {
        val capacity = capacityAtDepth(depth)
        require(position >= 0 && position < capacity, "PositionalMerkleTree: position out of range")
        require(lengthOfByteString(proof) == depth * 32, "PositionalMerkleTree: proof length")
    }

    // Package visibility lets tests exercise the reserved digest without finding a hash preimage.
    private[tree] def nonEmptyLeaf(hash: ByteString): ByteString = {
        require(hash != EmptyLeafHash, "PositionalMerkleTree: reserved empty leaf")
        hash
    }

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    private def emptyUp(
        root: ByteString,
        position: BigInt,
        oldHash: ByteString,
        newHash: ByteString,
        proof: ByteString,
        offset: BigInt,
        end: BigInt
    ): ByteString =
        if offset == end then
            require(oldHash == root, "PositionalMerkleTree: root mismatch")
            newHash
        else
            val sibling = sliceByteString(offset, 32, proof)
            val parent = quotientInteger(position, 2)
            if modInteger(position, 2) == BigInt(0) then
                emptyUp(
                  root,
                  parent,
                  combine(oldHash, sibling),
                  combine(newHash, sibling),
                  proof,
                  offset + 32,
                  end
                )
            else
                emptyUp(
                  root,
                  parent,
                  combine(sibling, oldHash),
                  combine(sibling, newHash),
                  proof,
                  offset + 32,
                  end
                )

    private def membershipUp(
        position: BigInt,
        hash: ByteString,
        proof: ByteString,
        offset: BigInt,
        end: BigInt
    ): ByteString =
        if offset == end then hash
        else
            val sibling = sliceByteString(offset, 32, proof)
            val parentHash =
                if modInteger(position, 2) == BigInt(0) then combine(hash, sibling)
                else combine(sibling, hash)
            membershipUp(quotientInteger(position, 2), parentHash, proof, offset + 32, end)

    private def leafNodes(
        leaves: List[Leaf],
        previous: BigInt,
        capacity: BigInt,
        acc: List[Node]
    ): List[Node] = leaves match
        case Nil => acc
        case Cons(leaf, rest) =>
            require(
              leaf.position > previous,
              "PositionalMerkleTree: positions must be strictly ascending"
            )
            require(leaf.position < capacity, "PositionalMerkleTree: position out of range")
            val hash = nonEmptyLeaf(blake2b_256(leaf.element))
            leafNodes(
              rest,
              leaf.position,
              capacity,
              Cons(Node(leaf.position, hash, EmptyLeafHash), acc)
            )

    private def emptyLevels(
        root: ByteString,
        nodes: List[Node],
        remaining: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if remaining == BigInt(0) then
            require(offset == lengthOfByteString(proof), "PositionalMerkleTree: unused helpers")
            // Sorted, bounded leaves give exactly one node here, even at depth zero.
            val node = nodes.head
            require(node.oldHash == root, "PositionalMerkleTree: root mismatch")
            node.newHash
        else emptyLevel(root, nodes, Nil, remaining, proof, offset)

    // Known nodes and helpers are consumed right-to-left, level-by-level. Helpers therefore have
    // descending generalized indices, exactly SSZ's ordering. Parents share both root calculations.
    private def emptyLevel(
        root: ByteString,
        nodes: List[Node],
        parents: List[Node],
        remaining: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString = nodes match
        case Nil => emptyLevels(root, parents.reverse, remaining - 1, proof, offset)
        case Cons(node, rest) =>
            val parent = quotientInteger(node.position, 2)
            val isRight = modInteger(node.position, 2) == BigInt(1)
            if isRight && !rest.isEmpty && rest.head.position == node.position - 1 then
                val left = rest.head
                val merged = Node(
                  parent,
                  combine(left.oldHash, node.oldHash),
                  combine(left.newHash, node.newHash)
                )
                emptyLevel(root, rest.tail, Cons(merged, parents), remaining, proof, offset)
            else
                require(
                  offset + 32 <= lengthOfByteString(proof),
                  "PositionalMerkleTree: missing helper"
                )
                val helper = sliceByteString(offset, 32, proof)
                val merged =
                    if isRight then
                        Node(parent, combine(helper, node.oldHash), combine(helper, node.newHash))
                    else Node(parent, combine(node.oldHash, helper), combine(node.newHash, helper))
                emptyLevel(root, rest, Cons(merged, parents), remaining, proof, offset + 32)
}
