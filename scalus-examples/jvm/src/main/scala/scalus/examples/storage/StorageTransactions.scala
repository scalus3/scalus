package scalus.examples.storage

import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.examples.UnorderedNodeAction
import scalus.patterns.{Config, Cons}
import scalus.patterns.UnorderedLinkedList as LinkedList
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data.toData

/** Transaction creator for operations that allow uncapped data storage on the blockchain.
  *
  * If the data doesn't fit into a single transaction (configured via [[chunkSize]], the actual max
  * tx size is 16KB), it's chunked into arrays of max size, and each chunk becomes a [[LinkedList]],
  * node.
  */
case class StorageTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit],
    config: Config,
    chunkSize: Int
):
    def script: Script.PlutusV3 = contract.script

    /** Build all transactions needed to store data.
      *
      * The result transactions should be submitted sequentially in the specified order.
      * @param validTo
      *   Transaction validity upper bound. Must be before the LinkedList deadline.
      * @return
      *   List of transactions: [initTx] or [initTx, appendTx1, appendTx2, ...]
      */
    def storeData(
        data: ByteString,
        userUtxos: Utxos,
        userPkh: AddrKeyHash,
        userAddress: Address,
        changeAddress: Address,
        userSigner: TransactionSigner,
        validTo: java.time.Instant
    ): List[Transaction] = {
        val chunks = splitIntoChunks(data)

        val initTx = buildInitTransaction(
          chunks.head,
          userUtxos,
          userAddress,
          changeAddress,
          userSigner
        )

        // in this case, we don't even need the linked list, could remove later as a simplification.
        if chunks.length == 1 then List(initTx)
        else
            val appendTxs = buildAppendTransactions(
              chunks.tail,
              initTx,
              userUtxos,
              userPkh,
              userAddress,
              changeAddress,
              userSigner,
              validTo
            )
            initTx :: appendTxs

    }
    private def splitIntoChunks(data: ByteString): List[ByteString] =
        val bytes = data.bytes
        if bytes.isEmpty then List(ByteString.empty)
        else
            bytes
                .grouped(chunkSize)
                .map(ByteString.fromArray)
                .toList

    /** Build initialization transaction that creates head node with first chunk. */
    private def buildInitTransaction(
        firstChunk: ByteString,
        utxos: Utxos,
        userAddress: Address,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction =
        val headDatum = Cons.head(data = Data.B(firstChunk))
        val headTokenName = LinkedList.nodeToken()

        val init = config.init

        val initIn = TransactionInput(TransactionHash.fromByteString(init.id.hash), init.idx.toInt)
        val initOut = utxos(initIn)

        // nft for the output, roughly min ada for simplification
        val headValue = Value.asset(
          script.scriptHash,
          AssetName(headTokenName),
          1,
          lovelace = Coin.ada(2)
        )

        TxBuilder(env, evaluator)
            .spend(Utxo(initIn -> initOut))
            .mint(
              script,
              Map(AssetName(headTokenName) -> 1L),
              _ => UnorderedNodeAction.Init.toData
            )
            .payTo(userAddress, headValue, headDatum)
            .complete(utxos, changeAddress)
            .sign(signer)
            .transaction

    /** Build append transactions for remaining chunks. */
    private def buildAppendTransactions(
        chunks: List[ByteString],
        previousTx: Transaction,
        userUtxos: Utxos,
        userPkh: AddrKeyHash,
        userAddress: Address,
        changeAddress: Address,
        signer: TransactionSigner,
        validTo: java.time.Instant
    ): List[Transaction] =
        // track spend inputs to avoid double spend attempts
        val initSpentInputs = Set(
          TransactionInput(
            TransactionHash.fromByteString(config.init.id.hash),
            config.init.idx.toInt
          )
        )

        chunks.zipWithIndex
            .foldLeft((List.empty[Transaction], previousTx, initSpentInputs)) {
                case ((txs, prevTx, allSpentInputs), (chunk, index)) =>
                    val newTx = buildAppendTransaction(
                      chunk,
                      index,
                      prevTx,
                      userUtxos,
                      userPkh,
                      userAddress,
                      changeAddress,
                      signer,
                      allSpentInputs,
                      validTo
                    )

                    val newSpentInputs = allSpentInputs ++ newTx.body.value.inputs.toSeq
                        .map(i => TransactionInput(i.transactionId, i.index))
                    (txs :+ newTx, newTx, newSpentInputs)
            }
            ._1

    /** Build a single append transaction.
      *
      * All chunks use the same key (userPkh) for signing and node identification. Chunks are
      * differentiated by their position in the linked list (ref chain).
      */
    private def buildAppendTransaction(
        chunk: ByteString,
        index: Int,
        previousTx: Transaction,
        userUtxos: Utxos,
        userPkh: AddrKeyHash,
        userAddress: Address,
        changeAddress: Address,
        signer: TransactionSigner,
        allSpentInputs: Set[TransactionInput],
        validTo: java.time.Instant
    ): Transaction =
        // tail utxo from the previous tx to append a new node to
        val tailUtxo = findTailUtxo(previousTx)

        val tailDatum = tailUtxo.output.inlineDatum
            .map(_.to[Cons])
            .getOrElse(throw new Exception("Tail UTxO has no inline datum"))

        // nft key, index ensures uniqueness, pkh prefix ensures authorization
        val chunkIndex = ByteString((index + 1).toByte)
        val chunkKey = userPkh ++ chunkIndex

        val newNodeDatum = Cons.cons(chunkKey, data = Data.B(chunk))

        // we're updating the tail to point to a new utxo with the current chunk
        val updatedTailDatum =
            tailDatum.copy(ref = scalus.cardano.onchain.plutus.prelude.Option.Some(chunkKey))

        // make sure to not double spend
        val unspentUserUtxos = userUtxos.filterNot { case (input, _) =>
            allSpentInputs.contains(input)
        }
        val availableUtxos = unspentUserUtxos ++ previousTx.utxos

        val nodeTokenName = LinkedList.nodeToken(chunkKey)

        // Value for new node output: ADA + minted NFT
        val newNodeValue = Value.asset(
          script.scriptHash,
          AssetName(nodeTokenName),
          1,
          lovelace = Coin.ada(2)
        )

        // The insertKey must be the chunk's key for unique NFT minting
        // We sign with userPkh, validator will check that chunkKey starts with userPkh
        val redeemer =
            UnorderedNodeAction
                .Append(scalus.cardano.onchain.plutus.v1.PubKeyHash(chunkKey), covering = tailDatum)
                .toData

        TxBuilder(env, evaluator)
            .spend(tailUtxo)
            .mint(
              script,
              Map(AssetName(nodeTokenName) -> 1L),
              redeemer,
              Set(userPkh)
            )
            .payTo(userAddress, tailUtxo.output.value, updatedTailDatum)
            .payTo(userAddress, newNodeValue, newNodeDatum)
            .validTo(validTo)
            .complete(availableUtxos, changeAddress)
            .sign(signer)
            .transaction

    /** Find the tail UTxO (node with ref = None) from a transaction's outputs.
      *
      * @param tx
      *   Transaction to search
      * @return
      *   Tail UTxO
      */
    private def findTailUtxo(tx: Transaction): Utxo =
        tx.utxos
            .find { case (_, output) =>
                // Find UTxO with LinkedList NFT and ref = None (tail node)
                output.value.assets.assets.exists { case (cs, tokens) =>
                    cs == script.scriptHash && tokens.keys.exists(tn =>
                        tn.bytes.toHex
                            .startsWith(ByteString.fromArray(LinkedList.nodeToken().bytes).toHex)
                    )
                } &&
                output.inlineDatum.exists { datum =>
                    val cons = datum.to[Cons]
                    cons.ref.isEmpty
                }
            }
            .map(Utxo.apply)
            .getOrElse(throw new Exception("Tail UTxO not found in transaction"))
