package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import scalus.serialization.cbor.Cbor as ScalusCbor
import scalus.uplc.builtin.{platform, ByteString}

import scala.collection.immutable.TreeMap

/** Represents a complete block in the Cardano blockchain
  *
  * Valid blocks must also satisfy the following constraints:
  *   1. The length of transaction_bodies and transaction_witness_sets must be the same 2. Every
  *      transaction_index must be strictly smaller than the length of transaction_bodies
  */
case class Block(
    /** Block header */
    header: BlockHeader,

    /** Transaction bodies in this block */
    transactionBodies: IndexedSeq[KeepRaw[TransactionBody]],

    /** Transaction witness sets for each transaction */
    transactionWitnessSets: IndexedSeq[KeepRaw[TransactionWitnessSet]],

    /** Auxiliary data associated with transactions by index */
    auxiliaryDataSet: Map[Int, KeepRaw[AuxiliaryData]],

    /** List of invalid transaction indices */
    invalidTransactions: IndexedSeq[Int]
):
    require(
      transactionBodies.size == transactionWitnessSets.size,
      s"Number of transaction bodies (${transactionBodies.size}) must match number of witness sets (${transactionWitnessSets.size})"
    )

    require(
      auxiliaryDataSet.keys.forall(idx => idx >= 0 && idx < transactionBodies.size),
      "Transaction index in auxiliary data set must be valid"
    )

    require(
      invalidTransactions.forall(idx => idx >= 0 && idx < transactionBodies.size),
      "Invalid transaction index must be valid"
    )

    /** Get the block number */
    def blockNumber: Long = header.blockNumber

    /** Get the slot number */
    def slot: Long = header.slot

    /** The block hash, see [[BlockHeader.hash]]. The body hash is a different value, in the header.
      */
    def hash: BlockHash = header.hash

    /** Get the number of transactions in the block */
    def txCount: Int = transactionBodies.size

    /** Get the number of invalid transactions */
    def invalidTxCount: Int = invalidTransactions.size

    /** Get the number of valid transactions */
    def validTxCount: Int = txCount - invalidTxCount

    /** Check if the block is empty (has no transactions) */
    def isEmpty: Boolean = transactionBodies.isEmpty

    /** Reconstruct complete transactions from bodies, witness sets, and auxiliary data */
    def transactions(using OriginalCborByteArray): Seq[Transaction] =
        transactionBodies.zipWithIndex.map { case (body, idx) =>
            val witnessSetRaw = transactionWitnessSets(idx)
            val auxData = auxiliaryDataSet.get(idx)
            val isValid = !invalidTransactions.contains(idx)

            Transaction(body, witnessSetRaw, isValid, auxData)
        }

object Block:

    /** The block body hash of the Alonzo and later eras: the hash of the four segment hashes.
      *
      * Each segment is hashed as the block encodes it: the tx bodies, the witness sets, the
      * auxiliary data map with tx indices in ascending order, and the invalid tx indices.
      */
    def bodyHash(
        transactionBodies: IndexedSeq[KeepRaw[TransactionBody]],
        transactionWitnessSets: IndexedSeq[KeepRaw[TransactionWitnessSet]],
        auxiliaryDataSet: Map[Int, KeepRaw[AuxiliaryData]],
        invalidTransactions: IndexedSeq[Int]
    ): BlockHash = {
        def segment[A: Encoder](a: A): Array[Byte] =
            platform.blake2b_256(ScalusCbor.encodeToByteString(a)).bytes
        val segments = segment(transactionBodies) ++ segment(transactionWitnessSets) ++
            segment(TreeMap.from(auxiliaryDataSet)) ++ segment(invalidTransactions)
        BlockHash.fromByteString(platform.blake2b_256(ByteString.unsafeFromArray(segments)))
    }

    /** CBOR encoder for Block */
    given Encoder[Block] = Encoder.derived
    given decoder(using OriginalCborByteArray): Decoder[Block] =
        // Decoder.derived[Block]
        r =>
            r.readArrayHeader()
            val blockHeader = r.read[BlockHeader]()
            given ProtocolVersion = blockHeader.headerBody.protocolVersion
            Block(
              blockHeader,
              r.read[IndexedSeq[KeepRaw[TransactionBody]]](),
              r.read[IndexedSeq[KeepRaw[TransactionWitnessSet]]](),
              r.read[Map[Int, KeepRaw[AuxiliaryData]]](),
              r.read[IndexedSeq[Int]]()
            )

case class BlockFile(era: Int, block: Block)
object BlockFile:
    /** CBOR encoder for BlockFile */
    given Encoder[BlockFile] = Encoder.derived
    given decoder(using OriginalCborByteArray): Decoder[BlockFile] =
        Decoder.derived[BlockFile]

    def fromCborArray(bytes: Array[Byte]): BlockFile =
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        scalus.serialization.cbor.Cbor.decode[BlockFile](bytes)
