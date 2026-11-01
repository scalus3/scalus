package scalus.cardano.node.emulator

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
        fittingPrefix(txs, maxBodySize, maxExUnits) == txs.size

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
        if txs.isEmpty then 0 else math.max(1, fittingPrefix(txs, maxBodySize, maxExUnits))

    /** The length of the longest prefix of `txs` whose block fits the limits, or 0.
      *
      * Each tx is measured once, from the bytes it already keeps, so the cost is linear. The body
      * size of a prefix is what [[bodySize]] encodes, spec [BH-4]: the four segment heads, then the
      * items of each tx: its body, its witness set, its aux data under its index, and its index
      * when it is invalid, spec [BH-9].
      */
    private def fittingPrefix(
        txs: IndexedSeq[Transaction],
        maxBodySize: Long,
        maxExUnits: ExUnits
    ): Int = {
        var units = ExUnits.zero
        var itemBytes = 0L
        var auxCount = 0
        var invalidCount = 0
        var n = 0
        var fitting = true
        while fitting && n < txs.size do
            val tx = txs(n)
            units = units + exUnitsOf(tx)
            itemBytes += tx.body.raw.length + tx.witnessSetRaw.raw.length
            tx.auxiliaryData.foreach { aux =>
                itemBytes += headSize(n) + aux.raw.length
                auxCount += 1
            }
            if !tx.isValid then
                itemBytes += headSize(n)
                invalidCount += 1
            val heads = 2 * headSize(n + 1) + headSize(auxCount) + headSize(invalidCount)
            fitting = !units.exceeds(maxExUnits) && heads + itemBytes <= maxBodySize
            if fitting then n += 1
        n
    }

    /** The byte size of the CBOR head of the unsigned int, array length or map size `n`. */
    private def headSize(n: Long): Int =
        if n < 24 then 1
        else if n < 0x100 then 2
        else if n < 0x10000 then 3
        else if n < 0x100000000L then 5
        else 9

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
