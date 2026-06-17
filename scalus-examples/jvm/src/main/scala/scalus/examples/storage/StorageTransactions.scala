package scalus.examples.storage

import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.patterns.Element
import scalus.examples.linkedlist.{LinkedListContract, LinkedListOffchain}
import scalus.cardano.onchain.plutus.prelude.Option as OnchainOption

/** Transaction creator for uncapped on-chain data storage.
  *
  * Data larger than [[chunkSize]] bytes is split across multiple linked-list nodes, each submitted
  * as a separate transaction. All nodes live at the same script address; read them back in order
  * with [[readData]].
  */
case class StorageTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    rootKey: ByteString,
    prefix: ByteString,
    chunkSize: Int
):
    private val ll = LinkedListOffchain(
      env = env,
      evaluator = evaluator,
      mintingContract = LinkedListContract,
      rootKey = rootKey,
      prefix = prefix
    )

    val policyId: PolicyId = ll.policyId
    val scriptAddress: Address = ll.scriptAddress

    /** Build all transactions needed to store `data`.
      *
      * Transactions must be submitted sequentially in the returned order.
      * @return
      *   `[initTx]` for single-chunk data, or `[initTx, appendTx1, ...]` for multi-chunk.
      */
    def storeData(
        data: ByteString,
        userUtxos: Utxos,
        sponsor: Address,
        signer: TransactionSigner
    ): List[Transaction] = {
        val chunks = splitIntoChunks(data)

        val initTx = ll.init(
          utxos = userUtxos,
          rootData = Data.B(chunks.head),
          sponsor = sponsor,
          signer = signer
        )

        if chunks.length == 1 then List(initTx)
        else
            val appendTxs = buildAppendTransactions(chunks.tail, initTx, userUtxos, sponsor, signer)
            initTx :: appendTxs
    }

    /** Reconstruct data from storage UTxOs by following the linked-list chain. */
    def readData(utxos: Iterable[Utxo]): ByteString =
        ll.readAll(utxos)
            .map { case (_, d) =>
                d match
                    case Data.B(bytes) => bytes
                    case _ =>
                        throw new IllegalStateException("Expected ByteString data in storage node")
            }
            .foldLeft(ByteString.empty)(_ ++ _)

    private def splitIntoChunks(data: ByteString): List[ByteString] =
        val bytes = data.bytes
        if bytes.isEmpty then List(ByteString.empty)
        else bytes.grouped(chunkSize).map(ByteString.fromArray).toList

    private def buildAppendTransactions(
        chunks: List[ByteString],
        previousTx: Transaction,
        userUtxos: Utxos,
        sponsor: Address,
        signer: TransactionSigner
    ): List[Transaction] =
        chunks.zipWithIndex
            .foldLeft((List.empty[Transaction], previousTx)) {
                case ((txs, prevTx), (chunk, index)) =>
                    val availableUtxos = userUtxos ++ prevTx.utxos
                    val tailUtxo = findTailUtxo(prevTx)
                    // Key: minimal big-endian encoding of the 1-based index (BigInt.toByteArray is
                    // minimal-width, not fixed 4 bytes), unique per chunk
                    val chunkKey = ByteString.fromArray(BigInt(index + 1).toByteArray)
                    val newTx = ll.appendUnordered(
                      utxos = availableUtxos,
                      anchorUtxo = tailUtxo,
                      newKey = chunkKey,
                      nodeData = Data.B(chunk),
                      sponsor = sponsor,
                      signer = signer
                    )
                    (txs :+ newTx, newTx)
            }
            ._1

    private def findTailUtxo(tx: Transaction): Utxo =
        tx.utxos
            .find { case (_, output) =>
                output.value.assets.assets.exists { case (cs, _) => cs == policyId } &&
                output.inlineDatum.exists(datum => datum.to[Element].link == OnchainOption.None)
            }
            .map(Utxo.apply)
            .getOrElse(throw new IllegalStateException("Tail UTxO not found in transaction"))
