package scalus.cardano.onchain.plutus.crypto.tree

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.cardano.onchain.plutus.prelude.List.{Cons, Nil}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}
import scalus.uplc.builtin.ByteString.hex

/** Positional membership and deletion for an SSZ chunk tree with BLAKE2b-256.
  *
  * The caller must authenticate both root and depth (0..256) in a datum or script parameter.
  * Positions are zero-based integers below 2^depth. Leaves hash the element once; unused and
  * deleted leaves are [[NullHash]]; parents hash `left ++ right`, without tags or length mixing.
  * Exact depth prevents a leaf from being interpreted as an internal node. [[NullHash]] has no
  * known element preimage.
  *
  * Verification authenticates membership against the root it receives. The caller must bind that
  * root to current state and every verified position to the actual payout or changed slot. Old
  * proofs still verify against their old roots; this library does not establish root freshness.
  */
@Compile
object PositionalMerkleTree {

    /** 32 zero bytes: the reserved chunk of unused and deleted slots. */
    val NullHash: ByteString =
        hex"0000000000000000000000000000000000000000000000000000000000000000"

    /** A position and its unhashed element. Multiproof queries must be strictly ascending. */
    case class Leaf(position: BigInt, element: ByteString) derives FromData, ToData

    private case class Node(position: BigInt, oldHash: ByteString, newHash: ByteString)

    /** Verify that `element` occupies `position` and return the root with that slot set to
      * [[NullHash]].
      *
      * `proof` is exactly `depth` sibling hashes (32 bytes each), bottom-up. Directions come from
      * the position's bits, least significant first. Malformed proofs or empty slots fail. Both
      * roots are recomputed together in one walk.
      */
    def delete(
        root: ByteString,
        depth: BigInt,
        position: BigInt,
        element: ByteString,
        proof: ByteString
    ): ByteString = {
        checkSingle(depth, position, proof)
        val leaf = nonEmptyLeaf(blake2b_256(element))
        deleteUp(root, position, leaf, NullHash, proof, 0, depth * 32)
    }

    /** Verify that `element` occupies `position`, failing on invalid proofs or empty slots. Uses
      * the same position and proof encoding as [[delete]].
      */
    def verifyMembership(
        root: ByteString,
        depth: BigInt,
        position: BigInt,
        element: ByteString,
        proof: ByteString
    ): Unit = {
        checkSingle(depth, position, proof)
        val computed =
            membershipUp(position, nonEmptyLeaf(blake2b_256(element)), proof, 0, depth * 32)
        require(computed == root, "PositionalMerkleTree: root mismatch")
    }

    /** Verify an SSZ multiproof and delete all selected occupied slots in one topology walk.
      *
      * `leaves` must be nonempty with strictly ascending, distinct, in-range positions. The proof
      * contains exactly the helpers from SSZ `get_helper_indices(2^depth + positions)`, in
      * descending generalized-index order, concatenated as 32-byte hashes. Every helper is
      * consumed; missing, extra and trailing bytes fail. Equal elements at different positions are
      * allowed.
      */
    def deleteMany(
        root: ByteString,
        depth: BigInt,
        leaves: List[Leaf],
        proof: ByteString
    ): ByteString = {
        checkDepth(depth)
        require(!leaves.isEmpty, "PositionalMerkleTree: empty query")
        val nodes = leafNodes(leaves, -1, Nil)
        deleteLevels(root, nodes, depth, proof, 0)
    }

    // Positions are not compared with 2^depth up front, which would cost a loop of depth steps.
    // Each walk halves the position once per level and requires 0 at the root instead: a
    // nonnegative position reaches 0 after `depth` halvings exactly when it is below 2^depth.
    private def checkDepth(depth: BigInt): Unit =
        require(depth >= 0 && depth <= 256, "PositionalMerkleTree: depth out of range")

    private def checkSingle(depth: BigInt, position: BigInt, proof: ByteString): Unit = {
        checkDepth(depth)
        require(position >= 0, "PositionalMerkleTree: position out of range")
        require(lengthOfByteString(proof) == depth * 32, "PositionalMerkleTree: proof length")
    }

    // Package visibility lets tests exercise the reserved digest without finding a hash preimage.
    private[tree] def nonEmptyLeaf(hash: ByteString): ByteString = {
        require(hash != NullHash, "PositionalMerkleTree: reserved empty leaf")
        hash
    }

    private def combine(left: ByteString, right: ByteString): ByteString =
        blake2b_256(appendByteString(left, right))

    private def deleteUp(
        root: ByteString,
        position: BigInt,
        oldHash: ByteString,
        newHash: ByteString,
        proof: ByteString,
        offset: BigInt,
        end: BigInt
    ): ByteString =
        if offset == end then
            require(position == BigInt(0), "PositionalMerkleTree: position out of range")
            require(oldHash == root, "PositionalMerkleTree: root mismatch")
            newHash
        else
            val sibling = sliceByteString(offset, 32, proof)
            val parent = quotientInteger(position, 2)
            if modInteger(position, 2) == BigInt(0) then
                deleteUp(
                  root,
                  parent,
                  combine(oldHash, sibling),
                  combine(newHash, sibling),
                  proof,
                  offset + 32,
                  end
                )
            else
                deleteUp(
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
        if offset == end then
            require(position == BigInt(0), "PositionalMerkleTree: position out of range")
            hash
        else
            val sibling = sliceByteString(offset, 32, proof)
            val parentHash =
                if modInteger(position, 2) == BigInt(0) then combine(hash, sibling)
                else combine(sibling, hash)
            membershipUp(quotientInteger(position, 2), parentHash, proof, offset + 32, end)

    // Returns the nodes in descending position order. The first position must be above -1.
    private def leafNodes(
        leaves: List[Leaf],
        previous: BigInt,
        acc: List[Node]
    ): List[Node] = leaves match
        case Nil => acc
        case Cons(leaf, rest) =>
            require(
              leaf.position > previous,
              "PositionalMerkleTree: positions must be strictly ascending"
            )
            val hash = nonEmptyLeaf(blake2b_256(leaf.element))
            leafNodes(rest, leaf.position, Cons(Node(leaf.position, hash, NullHash), acc))

    private def deleteLevels(
        root: ByteString,
        nodes: List[Node],
        remaining: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString =
        if remaining == BigInt(0) then
            require(offset == lengthOfByteString(proof), "PositionalMerkleTree: unused helpers")
            // Every level keeps distinct positions in descending order, so the head holds the
            // largest. Positions below 2^depth all reach 0 and leave exactly one node, even at
            // depth zero; any larger position leaves a head above 0.
            val node = nodes.head
            require(node.position == BigInt(0), "PositionalMerkleTree: position out of range")
            require(node.oldHash == root, "PositionalMerkleTree: root mismatch")
            node.newHash
        else deleteLevel(root, nodes, Nil, remaining, proof, offset)

    // Known nodes and helpers are consumed right-to-left, level-by-level. Helpers therefore have
    // descending generalized indices, exactly SSZ's ordering. Parents share both root calculations.
    private def deleteLevel(
        root: ByteString,
        nodes: List[Node],
        parents: List[Node],
        remaining: BigInt,
        proof: ByteString,
        offset: BigInt
    ): ByteString = nodes match
        case Nil => deleteLevels(root, parents.reverse, remaining - 1, proof, offset)
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
                deleteLevel(root, rest.tail, Cons(merged, parents), remaining, proof, offset)
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
                deleteLevel(root, rest, Cons(merged, parents), remaining, proof, offset + 32)
}
