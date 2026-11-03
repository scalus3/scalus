package scalus.cardano.node.emulator

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.TxRefScriptsSizeValidator
import scalus.cardano.ledger.utils.ProducedUtxos
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
      * redeemers declare. The mempool capacity, spec [MP-4], is measured this way.
      */
    def fits(txs: IndexedSeq[Transaction], maxBodySize: Long, maxExUnits: ExUnits): Boolean =
        fittingPrefix(txs, maxBodySize, maxExUnits, Iterator.continually(0)) == txs.size

    /** Whether a block of `txs` on the UTxO set `utxo` fits the block limits of `params`: those of
      * [[fits]], and the total reference-script size, spec [FG-5], [BB-1].
      */
    def fitsBlock(txs: IndexedSeq[Transaction], params: ProtocolParams, utxo: Utxos): Boolean =
        prefixWithinBlockLimits(txs, params, utxo) == txs.size

    /** The length of the longest prefix of `txs` that [[fitsBlock]] on `utxo`, spec [FG-4], [BB-1].
      * The first tx is always taken: the ledger's `maxTxSize` and `maxTxExecutionUnits` are below
      * the block limits, and `submit` holds every mempool tx, `isValid` or not, to the per-tx
      * reference-script limit, as the node's mempool measure does; so a mempool tx fits an empty
      * block, and a forge from a non-empty mempool makes progress.
      */
    def prefixThatFits(txs: IndexedSeq[Transaction], params: ProtocolParams, utxo: Utxos): Int =
        if txs.isEmpty then 0 else math.max(1, prefixWithinBlockLimits(txs, params, utxo))

    /** The length of the longest prefix of `txs` whose block fits the block limits of `params` on
      * `utxo`, or 0: the one measure behind [[fitsBlock]] and [[prefixThatFits]], so the forge
      * never picks a prefix that `produceBlock(on, …)` rejects.
      */
    private def prefixWithinBlockLimits(
        txs: IndexedSeq[Transaction],
        params: ProtocolParams,
        utxo: Utxos
    ): Int =
        fittingPrefix(
          txs,
          params.maxBlockBodySize,
          params.maxBlockExecutionUnits,
          refScriptSizes(txs, utxo, params.protocolVersion)
        )

    /** The length of the longest prefix of `txs` whose block fits the limits, or 0.
      *
      * Each tx is measured once, from the bytes it already keeps, so the cost is linear. The body
      * size of a prefix is what [[bodySize]] encodes, spec [BH-4]: the four segment heads, then the
      * items of each tx: its body, its witness set, its aux data under its index, and its index
      * when it is invalid, spec [BH-9]. `refScriptSizes` gives the reference-script size of each tx
      * in order; their total must stay within [[maxRefScriptSizePerBlock]], spec [BB-1].
      */
    private def fittingPrefix(
        txs: IndexedSeq[Transaction],
        maxBodySize: Long,
        maxExUnits: ExUnits,
        refScriptSizes: Iterator[Int]
    ): Int = {
        var units = ExUnits.zero
        var itemBytes = 0L
        var refScriptBytes = 0L
        var auxCount = 0
        var invalidCount = 0
        var n = 0
        var fitting = true
        while fitting && n < txs.size do
            val tx = txs(n)
            units = units + exUnitsOf(tx)
            itemBytes += tx.body.raw.length + tx.witnessSetRaw.raw.length
            refScriptBytes += refScriptSizes.next()
            tx.auxiliaryData.foreach { aux =>
                itemBytes += headSize(n) + aux.raw.length
                auxCount += 1
            }
            if !tx.isValid then
                itemBytes += headSize(n)
                invalidCount += 1
            val heads = 2 * headSize(n + 1) + headSize(auxCount) + headSize(invalidCount)
            fitting = !units.exceeds(maxExUnits) && heads + itemBytes <= maxBodySize &&
                refScriptBytes <= maxRefScriptSizePerBlock
            if fitting then n += 1
        n
    }

    /** `ppMaxRefScriptSizePerBlockG` in cardano-ledger, a constant in Conway (C/PParams.hs:981),
      * spec [BB-1].
      */
    val maxRefScriptSizePerBlock: Int = 1024 * 1024

    /** The total reference-script size of a block of `txs` on the UTxO set `utxo` at the block
      * start, spec [BB-3], [BB-4]. It's `totalRefScriptSizeInBlock` in cardano-ledger
      * (C/Rules/Bbody.hs:369-382).
      */
    def refScriptsSize(
        txs: IndexedSeq[Transaction],
        utxo: Utxos,
        protocolVersion: ProtocolVersion
    ): Long =
        refScriptSizes(txs, utxo, protocolVersion).map(_.toLong).sum

    /** The reference-script size of each tx of a block, in order, as `txNonDistinctRefScriptsSize`
      * (C/UTxO.hs:171-175). Up to PV 10 each tx resolves its inputs on `utxo`, the UTxO set at the
      * block start. From PV 11 a tx also sees the outputs of the earlier txs of the block: the
      * outputs of a valid tx, only the collateral return of a tx with `isValid = false`, spec
      * [BB-3], [BB-4]. An input that an earlier tx spends stays in the lookup.
      *
      * Lazy, so a forge that stops at the first tx that does not fit measures no more.
      */
    private def refScriptSizes(
        txs: IndexedSeq[Transaction],
        utxo: Utxos,
        protocolVersion: ProtocolVersion
    ): Iterator[Int] =
        if protocolVersion.major <= 10 then
            txs.iterator.map(TxRefScriptsSizeValidator.txNonDistinctRefScriptsSize(_, utxo))
        else
            txs.iterator
                .scanLeft((utxo, 0)) { case ((lookup, _), tx) =>
                    (
                      lookup ++ ProducedUtxos.of(tx),
                      TxRefScriptsSizeValidator.txNonDistinctRefScriptsSize(tx, lookup)
                    )
                }
                .drop(1)
                .map(_._2)

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
