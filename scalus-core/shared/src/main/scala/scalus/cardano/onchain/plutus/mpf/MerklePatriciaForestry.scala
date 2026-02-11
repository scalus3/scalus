package scalus.cardano.onchain.plutus.mpf

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString

/** A Merkle Patricia Forestry (MPF) is a key:value structure which stores elements in a radix trie
  * following a key, and where nodes also contains a cryptographic hash digest of the sub-trie or
  * value they hold.
  *
  * This library enforces (through hashing) that we use trie of radix 16 (hexadecimal alphabet).
  * This means that each level in the trie has up to 16 branches.
  *
  * An MPF allows for checking membership, insertion and deletion in the trie using only root hashes
  * and a succinct proofs. They are quite efficient in both cpu and mem units. And they also provide
  * proofs that are a / lot smaller than traditional Merkle Patricia Trie; proofs remain however the
  * / main limiting factor.
  */
case class MerklePatriciaForestry(root: ByteString)

@Compile
object MerklePatriciaForestry:
    import Merkling.*

    // Constants
    private val Blake2b256DigestSize: BigInt = 32

    /** A neighbor node used in a proof */
    case class Neighbor(
        nibble: BigInt,
        prefix: ByteString,
        root: ByteString
    )

    /** Types of proof steps that can occur in a proof Each step contains a skip value indicating
      * length of common prefix at that level
      */
    enum ProofStep:
        case Branch(skip: BigInt, neighbors: ByteString)
        case Fork(skip: BigInt, neighbor: Neighbor)
        case Leaf(skip: BigInt, key: ByteString, value: ByteString)

    /** A proof is a list of steps processed left to right, corresponding to neighbor nodes along
      * the path to the element being proved.
      */
    type Proof = List[ProofStep]

    /** Main Merkle Patricia Forestry class representing a key-value trie with cryptographic hash
      * digests
      */
    extension (self: MerklePatriciaForestry)
        /** Check if this trie is empty */
        def isEmpty: Boolean = self.root == NullHash

        /** Check if an element exists with a specific value */
        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry.including(key, value, proof) == self.root

        /** Insert an element with proof Fails if the proof is invalid or element already exists
          */
        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.excluding(key, proof) == self.root,
              "Invalid proof or element exists"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.including(key, value, proof))

        /** Delete an element with proof Fails if the proof is invalid or element doesn't exist
          */
        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.including(key, value, proof) == self.root,
              "Invalid proof or element missing"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.excluding(key, proof))

        /** Update an element's value with proof More efficient than separate delete+insert
          */
        def update(
            key: ByteString,
            proof: Proof,
            oldValue: ByteString,
            newValue: ByteString
        ): MerklePatriciaForestry =
            require(
              MerklePatriciaForestry.including(key, oldValue, proof) == self.root,
              "Invalid proof or old value missing"
            )
            MerklePatriciaForestry(MerklePatriciaForestry.including(key, newValue, proof))

    /** Create empty trie */
    def empty: MerklePatriciaForestry = MerklePatriciaForestry(NullHash)

    /** Create trie from root hash Root must be 32 bytes
      */
    def apply(root: ByteString): MerklePatriciaForestry =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaForestry(root)

    /** Compute root hash from proof with value */
    private def including(key: ByteString, value: ByteString, proof: Proof): ByteString =
        doIncluding(blake2b_256(key), blake2b_256(value), 0, proof)

    private def doIncluding(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            combine(suffix(path, cursor), value)

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    doBranch(path, cursor, nextCursor, root, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    doFork(path, cursor, nextCursor, root, neighbor)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doIncluding(path, value, nextCursor, steps)
                    val neighbor = Neighbor(
                      nibble = nibble(key, nextCursor - 1),
                      prefix = suffix(key, nextCursor),
                      root = neighborValue
                    )
                    doFork(path, cursor, nextCursor, root, neighbor)

    /** Compute root hash excluding element */
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
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val root = doExcluding(path, nextCursor, steps)
                    doBranch(path, cursor, nextCursor, root, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    steps match
                        case List.Nil =>
                            val prefix = consByteString(neighbor.nibble, neighbor.prefix)
                            combine(prefix, neighbor.root)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            doFork(path, cursor, nextCursor, root, neighbor)

                case ProofStep.Leaf(skip, key, value) =>
                    steps match
                        case List.Nil => combine(suffix(key, cursor), value)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val root = doExcluding(path, nextCursor, steps)
                            val neighbor = Neighbor(
                              nibble = nibble(key, cursor),
                              prefix = suffix(key, nextCursor),
                              root = value
                            )
                            doFork(path, cursor, nextCursor, root, neighbor)

    private def doBranch(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbors: ByteString
    ): ByteString =
        val branch = nibble(path, nextCursor - 1)
        val prefix = nibbles(path, cursor, nextCursor - 1)

        combine(
          prefix,
          merkle16(
            branch,
            root,
            sliceByteString(0, Blake2b256DigestSize, neighbors),
            sliceByteString(32, Blake2b256DigestSize, neighbors),
            sliceByteString(64, Blake2b256DigestSize, neighbors),
            sliceByteString(96, Blake2b256DigestSize, neighbors)
          )
        )

    private def doFork(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        root: ByteString,
        neighbor: Neighbor
    ): ByteString =
        val branch = nibble(path, nextCursor - 1)
        val prefix = nibbles(path, cursor, nextCursor - 1)

        require(branch != neighbor.nibble)

        combine(
          prefix,
          sparseMerkle16(
            branch,
            root,
            neighbor.nibble,
            combine(neighbor.prefix, neighbor.root)
          )
        )
