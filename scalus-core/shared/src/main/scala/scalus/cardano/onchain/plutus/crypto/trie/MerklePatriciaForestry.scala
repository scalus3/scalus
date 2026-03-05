package scalus.cardano.onchain.plutus.crypto.trie

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{require, List}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{ByteString, FromData, ToData}

/** Merkle Patricia Forestry (radix-16) with full nibble prefix encoding.
  *
  * This is the Aiken-compatible version that encodes the full nibble values in branch prefix
  * hashes.
  */
case class MerklePatriciaForestry(root: ByteString)

@Compile
object MerklePatriciaForestry:
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
    extension (self: MerklePatriciaForestry)
        /** Check if this trie is empty */
        def isEmpty: Boolean = self.root == NullHash

        /** Check if an element exists with a specific value */
        def has(key: ByteString, value: ByteString, proof: Proof): Boolean =
            MerklePatriciaForestry.including(key, value, proof) == self.root

        /** Verify membership of a key-value pair, throwing if the proof is invalid */
        def verifyMembership(key: ByteString, value: ByteString, proof: Proof): Unit =
            require(
              MerklePatriciaForestry.including(key, value, proof) == self.root,
              "Membership verification failed"
            )

        /** Verify non-membership of a key, throwing if the proof is invalid */
        def verifyNonMembership(key: ByteString, proof: Proof): Unit =
            val path = blake2b_256(key)
            val both = MerklePatriciaForestry.doCombined(path, NullHash, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Non-membership verification failed")

        /** Insert an element with proof using single-pass: parse proof once, compute both excluding
          * (verify absent) and including (new root) simultaneously.
          */
        def insert(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(exclRoot == self.root, "Invalid proof or element exists")
            MerklePatriciaForestry(inclRoot)

        /** Delete an element with proof using single-pass: parse proof once, compute both including
          * (verify present) and excluding (new root) simultaneously.
          */
        def delete(key: ByteString, value: ByteString, proof: Proof): MerklePatriciaForestry =
            val path = blake2b_256(key)
            val hValue = blake2b_256(value)
            val both = MerklePatriciaForestry.doCombined(path, hValue, 0, proof)
            val exclRoot = sliceByteString(0, Blake2b256DigestSize, both)
            val inclRoot = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
            require(inclRoot == self.root, "Invalid proof or element missing")
            MerklePatriciaForestry(exclRoot)

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

    // --- Combined single-pass: compute excluding and including simultaneously ---
    // Returns a 64-byte ByteString: excl[32] || incl[32]

    /** Combined proof traversal: parse each proof step once, compute both excluding and including
      * hashes simultaneously. Saves one full proof traversal (proof parsing, recursion, path
      * computations) and one blake2b_256(key) call.
      */
    private def doCombined(
        path: ByteString,
        value: ByteString,
        cursor: BigInt,
        proof: Proof
    ): ByteString = proof match
        case List.Nil =>
            appendByteString(NullHash, combine(suffix(path, cursor), value))

        case List.Cons(step, steps) =>
            step match
                case ProofStep.Branch(skip, neighbors) =>
                    val nextCursor = cursor + 1 + skip
                    val both = doCombined(path, value, nextCursor, steps)
                    doBranch2(path, cursor, nextCursor, both, neighbors)

                case ProofStep.Fork(skip, neighbor) =>
                    steps match
                        case List.Nil =>
                            // Last step: excluding has special prefix reconstruction
                            val nextCursor = cursor + 1 + skip
                            val exclResult =
                                if skip == BigInt(0) then
                                    combine(
                                      consByteString(neighbor.nibble, neighbor.prefix),
                                      neighbor.root
                                    )
                                else
                                    combine(
                                      appendByteString(
                                        nibbles(path, cursor, cursor + skip),
                                        consByteString(neighbor.nibble, neighbor.prefix)
                                      ),
                                      neighbor.root
                                    )
                            val inclChild = combine(suffix(path, nextCursor), value)
                            val inclResult =
                                doFork(path, cursor, nextCursor, inclChild, neighbor)
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val both = doCombined(path, value, nextCursor, steps)
                            doFork2(path, cursor, nextCursor, both, neighbor)

                case ProofStep.Leaf(skip, key, neighborValue) =>
                    steps match
                        case List.Nil =>
                            // Last step: excluding returns neighbor leaf promoted up
                            val exclResult = combine(suffix(key, cursor), neighborValue)
                            val nextCursor = cursor + 1 + skip
                            val inclChild = combine(suffix(path, nextCursor), value)
                            val neighbor = Neighbor(
                              nibble = nibble(key, nextCursor - 1),
                              prefix = suffix(key, nextCursor),
                              root = neighborValue
                            )
                            val inclResult =
                                doFork(path, cursor, nextCursor, inclChild, neighbor)
                            appendByteString(exclResult, inclResult)
                        case _ =>
                            val nextCursor = cursor + 1 + skip
                            val neighbor = Neighbor(
                              nibble = nibble(key, nextCursor - 1),
                              prefix = suffix(key, nextCursor),
                              root = neighborValue
                            )
                            val both = doCombined(path, value, nextCursor, steps)
                            doFork2(path, cursor, nextCursor, both, neighbor)

    /** Branch step for combined pass: shares branch, prefix, and neighbor slices. */
    private def doBranch2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighbors: ByteString
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = nibble(path, nextCursor - 1)
        val prefix = nibbles(path, cursor, nextCursor - 1)
        val n8 = sliceByteString(0, Blake2b256DigestSize, neighbors)
        val n4 = sliceByteString(32, Blake2b256DigestSize, neighbors)
        val n2 = sliceByteString(64, Blake2b256DigestSize, neighbors)
        val n1 = sliceByteString(96, Blake2b256DigestSize, neighbors)
        appendByteString(
          combine(prefix, merkle16(branch, exclChild, n8, n4, n2, n1)),
          combine(prefix, merkle16(branch, inclChild, n8, n4, n2, n1))
        )

    /** Fork step for combined pass: shares branch, prefix, neighborHash. */
    private def doFork2(
        path: ByteString,
        cursor: BigInt,
        nextCursor: BigInt,
        both: ByteString,
        neighbor: Neighbor
    ): ByteString =
        val exclChild = sliceByteString(0, Blake2b256DigestSize, both)
        val inclChild = sliceByteString(Blake2b256DigestSize, Blake2b256DigestSize, both)
        val branch = nibble(path, nextCursor - 1)
        val prefix = nibbles(path, cursor, nextCursor - 1)
        require(branch != neighbor.nibble)
        val neighborHash = combine(neighbor.prefix, neighbor.root)
        appendByteString(
          combine(prefix, sparseMerkle16(branch, exclChild, neighbor.nibble, neighborHash)),
          combine(prefix, sparseMerkle16(branch, inclChild, neighbor.nibble, neighborHash))
        )
