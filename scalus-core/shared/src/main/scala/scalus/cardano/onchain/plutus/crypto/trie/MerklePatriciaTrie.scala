package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** Merkle Patricia Trie (radix-16) with full nibble prefix encoding.
  *
  * This is the Aiken-compatible version that encodes the full nibble values in branch prefix
  * hashes.
  */
case class MerklePatriciaTrie(root: ByteString)

@Compile
object MerklePatriciaTrie:
    import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*

    // Constants
    private val Blake2b256DigestSize: BigInt = 32

    /** A neighbor node used in a proof */
    case class Neighbor(
        nibble: BigInt,
        prefix: ByteString,
        root: ByteString
    ) derives FromData,
          ToData

    /** Types of proof steps that can occur in a proof Each step contains a skip value indicating
      * length of common prefix at that level
      */
    enum ProofStep derives FromData, ToData:
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
    extension (self: MerklePatriciaTrie)
        /** Check if this trie is empty */
        def isEmpty: Boolean = self.root == NullHash

        /** Check if an element exists with a specific value */
        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaTrie.including(key, value, proof) == self.root

        /** Insert an element with proof Fails if the proof is invalid or element already exists
          */
        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaTrie =
            require(
              MerklePatriciaTrie.excluding(key, proof) == self.root,
              "Invalid proof or element exists"
            )
            MerklePatriciaTrie(MerklePatriciaTrie.including(key, value, proof))

        /** Delete an element with proof Fails if the proof is invalid or element doesn't exist
          */
        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaTrie =
            require(
              MerklePatriciaTrie.including(key, value, proof) == self.root,
              "Invalid proof or element missing"
            )
            MerklePatriciaTrie(MerklePatriciaTrie.excluding(key, proof))

        /** Update an element's value with proof More efficient than separate delete+insert
          */
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

    /** Create empty trie */
    def empty: MerklePatriciaTrie = MerklePatriciaTrie(NullHash)

    /** Create trie from root hash Root must be 32 bytes
      */
    def apply(root: ByteString): MerklePatriciaTrie =
        require(lengthOfByteString(root) == Blake2b256DigestSize, "Root must be 32 bytes")
        new MerklePatriciaTrie(root)

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
                            if skip == BigInt(0) then
                                val prefix = consByteString(neighbor.nibble, neighbor.prefix)
                                combine(prefix, neighbor.root)
                            else
                                val originalPrefix = appendByteString(
                                  nibbles(path, cursor, cursor + skip),
                                  consByteString(neighbor.nibble, neighbor.prefix)
                                )
                                combine(originalPrefix, neighbor.root)
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
                              nibble = nibble(key, nextCursor - 1),
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
