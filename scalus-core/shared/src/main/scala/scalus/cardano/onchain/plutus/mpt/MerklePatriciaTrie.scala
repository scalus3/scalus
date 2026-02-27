package scalus.cardano.onchain.plutus.mpt

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** A binary Merkle Patricia Trie (Radix-2 MPT). Each branch has exactly 2 children, so proofs need
  * only 1 sibling hash per step. Uses PlutusV3 `readBit` builtin for bit extraction.
  */
case class MerklePatriciaTrie(root: ByteString)

@Compile
object MerklePatriciaTrie:
    import BinaryMerkling.*

    private val Blake2b256DigestSize: BigInt = 32

    /** Proof step types. Only 2 variants (vs 3 in Radix-16 MPF) since each branch has exactly 2
      * children.
      */
    enum ProofStep derives FromData, ToData:
        /** Sibling is a branch node. */
        case Fork(skip: BigInt, neighborSkipLen: BigInt, neighborRoot: ByteString)

        /** Sibling is a leaf node. */
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    type Proof = List[ProofStep]

    extension (self: MerklePatriciaTrie)
        def isEmpty: Boolean = self.root == NullHash

        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaTrie.including(key, value, proof) == self.root

        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaTrie =
            require(
              MerklePatriciaTrie.excluding(key, proof) == self.root,
              "Invalid proof or element exists"
            )
            MerklePatriciaTrie(MerklePatriciaTrie.including(key, value, proof))

        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaTrie =
            require(
              MerklePatriciaTrie.including(key, value, proof) == self.root,
              "Invalid proof or element missing"
            )
            MerklePatriciaTrie(MerklePatriciaTrie.excluding(key, proof))

        def update(
            key: ByteString,
            proof: Proof,
            oldValue: ByteString,
            newValue: ByteString
        ): MerklePatriciaTrie =
            require(
              MerklePatriciaTrie.including(key, oldValue, proof) == self.root,
              "Invalid proof or old value missing"
            )
            MerklePatriciaTrie(MerklePatriciaTrie.including(key, newValue, proof))

    def empty: MerklePatriciaTrie = MerklePatriciaTrie(NullHash)

    def apply(root: ByteString): MerklePatriciaTrie =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaTrie(root)

    /** Compute root hash assuming element IS present. */
    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            combine(suffixBit(path, cursor), value)

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Fork(skip, neighborSkipLen, neighborRoot) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    doFork(path, cursor, nextCursor, root, neighborSkipLen, neighborRoot)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    doLeaf(path, cursor, nextCursor, root, key, neighborValue)

    /** Compute root hash assuming element is NOT present. */
    private def excluding(key: ByteString, proof: Proof): ByteString =
        doExcluding(blake2b_256(key), 0, proof)

    private def doExcluding(
        path: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil => NullHash

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Fork(skip, neighborSkipLen, neighborRoot) =>
                    steps match
                        case List.Nil =>
                            // Terminal: branch collapses, sibling absorbs prefix
                            combine(
                              consByteString(skip + 1 + neighborSkipLen, ByteString.empty),
                              neighborRoot
                            )
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            doFork(path, cursor, nextCursor, root, neighborSkipLen, neighborRoot)

                case ProofStep.Leaf(skip, key, value) =>
                    steps match
                        case List.Nil =>
                            // Terminal: sibling leaf absorbs prefix
                            combine(suffixBit(key, cursor), value)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            doLeaf(path, cursor, nextCursor, root, key, value)

    /** Reconstruct hash at a Fork proof step (sibling is a branch). */
    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborSkipLen: BigInt,
        neighborRoot: ByteString
    ): ByteString =
        val branchBit = bit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val siblingHash = combine(consByteString(neighborSkipLen, ByteString.empty), neighborRoot)
        if branchBit == BigInt(0) then combine(prefix, combine(root, siblingHash))
        else combine(prefix, combine(siblingHash, root))

    /** Reconstruct hash at a Leaf proof step (sibling is a leaf). */
    private def doLeaf(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighborKey: ByteString,
        neighborValue: ByteString
    ): ByteString =
        val branchBit = bit(path, nextCursor - 1)
        val prefix = consByteString(nextCursor - 1 - cursor, ByteString.empty)
        val siblingHash = combine(suffixBit(neighborKey, nextCursor), neighborValue)
        if branchBit == BigInt(0) then combine(prefix, combine(root, siblingHash))
        else combine(prefix, combine(siblingHash, root))
