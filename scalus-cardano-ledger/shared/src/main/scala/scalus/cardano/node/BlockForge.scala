package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString

import scala.collection.immutable.TreeMap

/** Builds the Conway block the Emulator forges, spec section 4.
  *
  * Nothing verifies the consensus fields, so they hold fixed bytes of the right size, spec [BH-8].
  * Everything else is what a real block of these txs would carry, so other tools can decode it.
  */
private[node] object BlockForge {

    private def zeros(size: Int): ByteString = ByteString.fromArray(new Array[Byte](size))

    private val vrfCert = VrfCert(output = zeros(64), proof = zeros(80))
    private val operationalCert = OperationalCert(zeros(32), 0L, 0L, zeros(64))
    private val bodySignature = zeros(448)

    /** The block of `txs`, in order, at `slot` and `height` on `parent`.
      *
      * @param parent
      *   the id of the parent block, or `None` when the parent is genesis, spec [BH-6]
      * @param height
      *   the height of the new block, its `blockNumber`, spec [BH-5]
      * @param protocolVersion
      *   the protocol version of the current protocol parameters, spec [BH-7]
      */
    def forge(
        parent: Option[BlockHash],
        height: Long,
        slot: SlotNo,
        txs: IndexedSeq[Transaction],
        protocolVersion: ProtocolVersion
    ): Block = {
        val body = Segments.of(txs)
        val headerBody = BlockHeaderBody(
          blockNumber = height,
          slot = slot,
          prevHash = parent,
          issuerVkey = zeros(32),
          vrfVkey = zeros(32),
          vrfResult = vrfCert,
          blockBodySize = body.size,
          // spec [BH-3]
          blockBodyHash =
              Block.bodyHash(body.bodies, body.witnessSets, body.auxiliaryData, body.invalid),
          operationalCert = operationalCert,
          protocolVersion = protocolVersion
        )
        Block(
          BlockHeader(headerBody, bodySignature),
          body.bodies,
          body.witnessSets,
          body.auxiliaryData,
          body.invalid
        )
    }

    /** The byte size of the body of a block of `txs`, spec [BH-4]. */
    def bodySize(txs: IndexedSeq[Transaction]): Long = Segments.of(txs).size

    /** Whether a block of `txs` fits `maxBodySize` and `maxExUnits`: the body size, the memory
      * units and the CPU steps, each on its own, spec [FG-5]. The execution units are those the
      * redeemers declare.
      */
    def fits(txs: IndexedSeq[Transaction], maxBodySize: Long, maxExUnits: ExUnits): Boolean =
        !txs.foldLeft(ExUnits.zero)(_ + exUnitsOf(_)).exceeds(maxExUnits) &&
            bodySize(txs) <= maxBodySize

    /** The length of the longest prefix of `txs` that [[fits]] the block limits, spec [FG-4]. The
      * first tx is always taken: the ledger's `maxTxSize` and `maxTxExecutionUnits` are below the
      * block limits, so a valid tx fits an empty block, and a forge from a non-empty mempool makes
      * progress.
      */
    def prefixThatFits(
        txs: IndexedSeq[Transaction],
        maxBodySize: Long,
        maxExUnits: ExUnits
    ): Int =
        // ceiling: encodes each longer prefix again, quadratic in its length. A mempool holds
        // about two blocks of txs; measure each tx once if a mempool grows large.
        if txs.isEmpty then 0
        else
            (2 to txs.size)
                .takeWhile(n => fits(txs.take(n), maxBodySize, maxExUnits))
                .lastOption
                .getOrElse(1)

    /** The execution units `tx` declares in its redeemers. */
    private def exUnitsOf(tx: Transaction): ExUnits =
        tx.witnessSet.redeemers.fold(ExUnits.zero)(_.value.totalExUnits)

    /** The four body segments of a block, in block order. */
    private final case class Segments(
        bodies: IndexedSeq[KeepRaw[TransactionBody]],
        witnessSets: IndexedSeq[KeepRaw[TransactionWitnessSet]],
        auxiliaryData: TreeMap[Int, KeepRaw[AuxiliaryData]],
        invalid: IndexedSeq[Int]
    ) {

        /** spec [BH-4]: the body is the four segments, encoded one after another. */
        def size: Long =
            (Cbor.encode(bodies).length + Cbor.encode(witnessSets).length +
                Cbor.encode(auxiliaryData).length + Cbor.encode(invalid).length).toLong
    }

    private object Segments {
        def of(txs: IndexedSeq[Transaction]): Segments = {
            val auxiliaryData = TreeMap.from(txs.zipWithIndex.flatMap { (tx, i) =>
                tx.auxiliaryData.map(i -> _)
            })
            // spec [BH-9]
            val invalid = txs.zipWithIndex.collect { case (tx, i) if !tx.isValid => i }
            Segments(txs.map(_.body), txs.map(_.witnessSetRaw), auxiliaryData, invalid)
        }
    }
}
