package scalus.examples.linkedlist

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.ByteString.given
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.Builtins.appendByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.*
import scalus.uplc.PlutusV3
import scalus.patterns.{Element, ElementData, NodeKey}
import scalus.cardano.onchain.plutus.prelude.Option as OnchainOption
import scalus.cardano.onchain.plutus.prelude.Option.{None as OnchainNone, Some as OnchainSome}
import scalus.examples.linkedlist.ListConfig

import scala.concurrent.Future

/** Off-chain transaction builder for the linked-list example.
  *
  * A single [[PlutusV3]] script serves as both minting policy and spending validator. `policyId`
  * and `scriptAddress` are derived from the compiled, applied script.
  *
  * @param rootKey
  *   Asset name of the root NFT (max 32 bytes).
  * @param prefix
  *   Prefix prepended to every node asset name; its length is derived automatically.
  */
case class LinkedListOffchain(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    mintingContract: PlutusV3[Data => Data => Unit],
    rootKey: ByteString,
    prefix: ByteString
) {
    private val prefixLen: Int = prefix.size

    private val cfg = ListConfig(
      rootKey = rootKey,
      prefix = prefix,
      prefixLen = BigInt(prefixLen)
    )

    private val appliedScript: PlutusV3[Data => Unit] =
        mintingContract.apply(cfg.toData)

    val script: Script.PlutusV3 = appliedScript.script
    val policyId: PolicyId = appliedScript.script.scriptHash
    val scriptAddress: Address = appliedScript.address(env.network)

    /** Decodes the [[Element]] datum from a UTxO's inline datum. */
    def readElement(utxo: Utxo): Element =
        utxo.output.inlineDatum
            .getOrElse(throw new Exception(s"UTxO has no inline datum: $utxo"))
            .to[Element]

    /** Finds the root UTxO (the one holding the `rootKey` NFT). */
    def findRoot(utxos: Iterable[Utxo]): Utxo =
        utxos
            .find { u =>
                u.output.value.assets.assets
                    .get(policyId)
                    .exists(_.contains(AssetName(rootKey)))
            }
            .getOrElse(throw new Exception("Root UTxO not found"))

    /** Finds the node UTxO whose asset name is `prefix ++ key`. */
    def findNode(utxos: Iterable[Utxo], key: NodeKey): Utxo = {
        val assetName = AssetName(appendByteString(prefix, key))
        utxos
            .find { u =>
                u.output.value.assets.assets
                    .get(policyId)
                    .exists(_.contains(assetName))
            }
            .getOrElse(throw new Exception(s"Node UTxO not found for key: $key"))
    }

    /** Returns `(key, utxo)` pairs for every element in list order (root first). */
    private def readAllWithUtxos(utxos: Iterable[Utxo]): Seq[(NodeKey, Utxo)] = {
        @annotation.tailrec
        def loop(link: OnchainOption[NodeKey], acc: Vector[(NodeKey, Utxo)]): Seq[(NodeKey, Utxo)] =
            link match
                case OnchainNone => acc
                case OnchainSome(nextKey) =>
                    val utxo = findNode(utxos, nextKey)
                    loop(readElement(utxo).link, acc :+ (nextKey, utxo))

        val rootUtxo = findRoot(utxos)
        loop(readElement(rootUtxo).link, Vector((rootKey, rootUtxo)))
    }

    /** Returns `(key, data)` for every element in list order (root first). */
    def readAll(utxos: Iterable[Utxo]): Seq[(NodeKey, Data)] =
        readAllWithUtxos(utxos).map { (key, utxo) =>
            val payload = readElement(utxo).data match
                case ElementData.Root(d) => d
                case ElementData.Node(d) => d
            (key, payload)
        }

    /** Returns the correct anchor UTxO for inserting or removing `key`.
      *
      * Walks the list and returns the last node whose asset name is strictly less than
      * `prefix ++ key`, or the root if no such node exists.
      */
    def findAnchorFor(utxos: Iterable[Utxo], key: NodeKey): Utxo = {
        val ord = summon[Ordering[ByteString]]
        val newAssetName = appendByteString(prefix, key)
        // The root is always a valid anchor (it precedes all nodes regardless of byte ordering
        // between rootKey and prefix ++ key). Among the nodes that follow, we advance the anchor
        // as long as the node's asset name is strictly less than the new asset name. The last
        // such node (or the root if none qualify) is the insertion point.
        val all = readAllWithUtxos(utxos)
        val rootUtxo = all.head._2
        val nodes = all.tail
        nodes
            .takeWhile { (k, _) => ord.compare(appendByteString(prefix, k), newAssetName) < 0 }
            .lastOption
            .map(_._2)
            .getOrElse(rootUtxo)
    }

    /** Convenience overload of [[readAll(Iterable[Utxo])]] that fetches UTxOs via `reader`. */
    def readAll(reader: BlockchainReader): Future[Seq[(NodeKey, Data)]] = {
        given scala.concurrent.ExecutionContext = reader.executionContext
        reader.findUtxos(scriptAddress).map {
            case Right(utxos) => readAll(utxos.map(Utxo.apply).toSeq)
            case Left(err)    => throw new Exception(s"Failed to query UTxOs: $err")
        }
    }

    /** The first (and only expected) AssetName under `policyId` in this UTxO. */
    private def listAssetName(utxo: Utxo): AssetName =
        utxo.output.value.assets.assets
            .getOrElse(policyId, Map.empty)
            .keys
            .headOption
            .getOrElse(throw new Exception(s"UTxO has no list asset: $utxo"))

    /** Index of a UTxO in a transaction's inputs. */
    private def inputIndex(tx: Transaction, utxo: Utxo): Int =
        tx.body.value.inputs.toSeq.indexWhere { i =>
            i.transactionId == utxo.input.transactionId && i.index == utxo.input.index
        }

    /** Index of an output at `scriptAddress` holding the given `asset`. */
    private def outputIndex(tx: Transaction, asset: AssetName): Int =
        tx.body.value.outputs.indexWhere { o =>
            o.value.address == scriptAddress && o.value.value.hasAsset(policyId, asset)
        }

    /** Initializes an empty list: mints the root NFT and sends it to the script address. */
    def init(
        utxos: Utxos,
        rootData: Data,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val rootAssetName = AssetName(rootKey)
        val rootDatum = Element(ElementData.Root(rootData), OnchainOption.None)

        def buildRedeemer(tx: Transaction): Data =
            ListAction.Init(BigInt(outputIndex(tx, rootAssetName))).toData

        TxBuilder(env, evaluator)
            .mint(script, Map(rootAssetName -> 1L), buildRedeemer)
            .payTo(scriptAddress, Value.asset(policyId, rootAssetName, 1), rootDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Destroys an empty list: burns the root NFT. */
    def deinit(
        utxos: Utxos,
        rootUtxo: Utxo,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val rootAssetName = AssetName(rootKey)

        def buildRedeemer(tx: Transaction): Data =
            ListAction.Deinit(BigInt(inputIndex(tx, rootUtxo))).toData

        TxBuilder(env, evaluator)
            .spend(rootUtxo, buildRedeemer, script)
            .mint(script, Map(rootAssetName -> -1L), buildRedeemer)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Inserts a node in ascending order after `anchorUtxo`. */
    def insert(
        utxos: Utxos,
        anchorUtxo: Utxo,
        newKey: ByteString,
        nodeData: Data,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newAssetName = AssetName(appendByteString(prefix, newKey))
        val anchorDatum = readElement(anchorUtxo)
        val anchorAssetName = listAssetName(anchorUtxo)

        val contAnchorDatum = anchorDatum.copy(link = OnchainOption.Some(newKey))
        val newElemDatum = Element(ElementData.Node(nodeData), anchorDatum.link)

        def buildRedeemer(tx: Transaction): Data =
            ListAction
                .Insert(
                  BigInt(inputIndex(tx, anchorUtxo)),
                  BigInt(outputIndex(tx, anchorAssetName)),
                  BigInt(outputIndex(tx, newAssetName))
                )
                .toData

        TxBuilder(env, evaluator)
            .spend(anchorUtxo, buildRedeemer, script)
            .mint(script, Map(newAssetName -> 1L), buildRedeemer)
            .payTo(scriptAddress, anchorUtxo.output.value, contAnchorDatum)
            .payTo(scriptAddress, Value.asset(policyId, newAssetName, 1), newElemDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Appends a node at the tail of an unordered list (`anchorUtxo` must have no link). */
    def appendUnordered(
        utxos: Utxos,
        anchorUtxo: Utxo,
        newKey: ByteString,
        nodeData: Data,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newAssetName = AssetName(appendByteString(prefix, newKey))
        val anchorDatum = readElement(anchorUtxo)
        val anchorAssetName = listAssetName(anchorUtxo)

        val contAnchorDatum = anchorDatum.copy(link = OnchainOption.Some(newKey))
        val newElemDatum = Element(ElementData.Node(nodeData), OnchainOption.None)

        def buildRedeemer(tx: Transaction): Data =
            ListAction
                .AppendUnordered(
                  BigInt(inputIndex(tx, anchorUtxo)),
                  BigInt(outputIndex(tx, anchorAssetName)),
                  BigInt(outputIndex(tx, newAssetName))
                )
                .toData

        TxBuilder(env, evaluator)
            .spend(anchorUtxo, buildRedeemer, script)
            .mint(script, Map(newAssetName -> 1L), buildRedeemer)
            .payTo(scriptAddress, anchorUtxo.output.value, contAnchorDatum)
            .payTo(scriptAddress, Value.asset(policyId, newAssetName, 1), newElemDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Prepends a node directly after the root (unordered lists). */
    def prependUnordered(
        utxos: Utxos,
        rootUtxo: Utxo,
        newKey: ByteString,
        nodeData: Data,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newAssetName = AssetName(appendByteString(prefix, newKey))
        val rootDatum = readElement(rootUtxo)

        val contRootDatum = rootDatum.copy(link = OnchainOption.Some(newKey))
        val newElemDatum = Element(ElementData.Node(nodeData), rootDatum.link)

        def buildRedeemer(tx: Transaction): Data =
            ListAction
                .PrependUnordered(
                  BigInt(inputIndex(tx, rootUtxo)),
                  BigInt(outputIndex(tx, AssetName(rootKey))),
                  BigInt(outputIndex(tx, newAssetName))
                )
                .toData

        TxBuilder(env, evaluator)
            .spend(rootUtxo, buildRedeemer, script)
            .mint(script, Map(newAssetName -> 1L), buildRedeemer)
            .payTo(scriptAddress, rootUtxo.output.value, contRootDatum)
            .payTo(scriptAddress, Value.asset(policyId, newAssetName, 1), newElemDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Removes the head node; `newRootData` is written into the root datum (may be unchanged). */
    def removeHead(
        utxos: Utxos,
        rootUtxo: Utxo,
        headUtxo: Utxo,
        newRootData: Data,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val rootDatum = readElement(rootUtxo)
        val headDatum = readElement(headUtxo)
        val headAssetName = listAssetName(headUtxo)
        val rootAssetName = listAssetName(rootUtxo)

        val contRootDatum = rootDatum.copy(
          data = scalus.patterns.ElementData.Root(newRootData),
          link = headDatum.link
        )

        def buildRedeemer(tx: Transaction): Data =
            ListAction
                .RemoveHead(
                  BigInt(inputIndex(tx, rootUtxo)),
                  BigInt(inputIndex(tx, headUtxo)),
                  BigInt(outputIndex(tx, rootAssetName))
                )
                .toData

        TxBuilder(env, evaluator)
            .spend(rootUtxo, buildRedeemer, script)
            .spend(headUtxo, _ => ListAction.Spend.toData, script)
            .mint(script, Map(headAssetName -> -1L), buildRedeemer)
            .payTo(scriptAddress, rootUtxo.output.value, contRootDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Removes a node; `anchorUtxo` must be the element immediately before it. */
    def remove(
        utxos: Utxos,
        anchorUtxo: Utxo,
        nodeUtxo: Utxo,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val anchorDatum = readElement(anchorUtxo)
        val nodeDatum = readElement(nodeUtxo)
        val nodeAssetName = listAssetName(nodeUtxo)
        val anchorAssetName = listAssetName(anchorUtxo)

        val contAnchorDatum = anchorDatum.copy(link = nodeDatum.link)

        def buildRedeemer(tx: Transaction): Data =
            ListAction
                .Remove(
                  BigInt(inputIndex(tx, anchorUtxo)),
                  BigInt(inputIndex(tx, nodeUtxo)),
                  BigInt(outputIndex(tx, anchorAssetName))
                )
                .toData

        TxBuilder(env, evaluator)
            .spend(anchorUtxo, buildRedeemer, script)
            .spend(nodeUtxo, _ => ListAction.Spend.toData, script)
            .mint(script, Map(nodeAssetName -> -1L), buildRedeemer)
            .payTo(scriptAddress, anchorUtxo.output.value, contAnchorDatum)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
