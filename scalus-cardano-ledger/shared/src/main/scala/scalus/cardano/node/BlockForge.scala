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
        val bodies = txs.map(_.body)
        val witnessSets = txs.map(_.witnessSetRaw)
        val auxiliaryData = TreeMap.from(txs.zipWithIndex.flatMap { (tx, i) =>
            tx.auxiliaryData.map(i -> _)
        })
        // spec [BH-9]
        val invalid = txs.zipWithIndex.collect { case (tx, i) if !tx.isValid => i }
        // spec [BH-4]: the body is the four segments, encoded one after another
        val bodySize = Cbor.encode(bodies).length + Cbor.encode(witnessSets).length +
            Cbor.encode(auxiliaryData).length + Cbor.encode(invalid).length
        val headerBody = BlockHeaderBody(
          blockNumber = height,
          slot = slot,
          prevHash = parent,
          issuerVkey = zeros(32),
          vrfVkey = zeros(32),
          vrfResult = vrfCert,
          blockBodySize = bodySize.toLong,
          // spec [BH-3]
          blockBodyHash = Block.bodyHash(bodies, witnessSets, auxiliaryData, invalid),
          operationalCert = operationalCert,
          protocolVersion = protocolVersion
        )
        Block(BlockHeader(headerBody, bodySignature), bodies, witnessSets, auxiliaryData, invalid)
    }
}
