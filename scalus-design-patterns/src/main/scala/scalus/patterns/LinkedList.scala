package scalus.patterns

import scalus.compiler.Compile

import scalus.*
import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.given
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*

// Aliases consistent with the aiken implementation

/** Asset name of the NFT held in the root element. */
type RootKey = TokenName

/** Raw key for a linked-list node (asset name with the prefix stripped). */
type NodeKey = ByteString

/** Prefix prepended to every node's asset name. */
type NodeKeyPrefix = ByteString

/** Length of the node key prefix passed around to avoid length recalculation. */
type NodeKeyPrefixLength = BigInt

/** Next element pointer stored inside an [[Element]] datum. */
type Link = Option[NodeKey]

/** Datum variant for linked-list UTxOs.
  *
  * `Root` marks the start of the list (not a data node). `Node` is every other element.
  */
enum ElementData derives FromData, ToData:
    case Root(data: Data)
    case Node(data: Data)

@Compile
object ElementData:
    given Eq[ElementData] = (left, right) =>
        left match {
            case ElementData.Root(lData) =>
                right match {
                    case ElementData.Root(rData) => lData === rData
                    case ElementData.Node(_)     => false
                }
            case ElementData.Node(lData) =>
                right match {
                    case ElementData.Root(_)     => false
                    case ElementData.Node(rData) => lData === rData
                }
        }

/** Full datum of every linked-list UTxO. */
case class Element(data: ElementData, link: Link) derives FromData, ToData

@Compile
object Element

/** On-chain library for a UTxO-based singly linked list on Cardano.
  *
  * Each UTxO in the list holds exactly one NFT under `policyId` (the asset name identifies the
  * node) and an inline [[Element]] datum with a payload and a `link` to the next node.
  *
  * Every list has one root UTxO whose NFT asset name is `rootKey`. Node asset names are
  * `prefix ++ key`, which lets the validator distinguish root from node tokens cheaply.
  *
  * Every mutating operation consumes an **anchor** — the node immediately before the affected
  * position — and reproduces it with an updated `link`. The anchor's NFT and payload are preserved.
  */
@Compile
object LinkedList {

    /** Validates initializing an empty list.
      *
      * Mints exactly one `rootKey` token and produces a `Root` datum with `link = None`.
      */
    def init(
        rootOut: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey
    ): Unit = {
        val (_, assetName, elemData, link) =
            authenticateElementUtxoAndGetInfo(rootOut, policyId)
        elemData match
            case ElementData.Root(_) => ()
            case ElementData.Node(_) => fail("init: produced element must be Root")
        val mintQty = txMint.quantityOf(policyId, assetName)
        require(assetName === rootKey, "init: asset name must be rootKey")
        require(mintQty == BigInt(1), "init: rootKey token must be minted")
        require(link.isEmpty, "init: root link must be empty")
    }

    /** Validates destroying an empty list.
      *
      * The root's `link` must be `None` and exactly one `rootKey` token must be burned.
      */
    def deinit(
        rootInput: TxInInfo,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey
    ): Unit = {
        val (_, assetName, elemData, link) =
            authenticateElementUtxoAndGetInfo(rootInput.resolved, policyId)
        elemData match
            case ElementData.Root(_) => ()
            case ElementData.Node(_) => fail("deinit: spent element must be Root")
        val mintQty = txMint.quantityOf(policyId, assetName)
        require(mintQty == BigInt(-1), "deinit: rootKey token must be burned")
        require(assetName === rootKey, "deinit: asset name must be rootKey")
        require(link.isEmpty, "deinit: list must be empty")
    }

    /** Validates inserting a node in ascending order.
      *
      * Enforces `anchorAssetName < newAssetName` and, when the anchor has a successor,
      * `newKey < linkKey`. The strict inequalities also prove the key was absent (a duplicate would
      * fail them).
      *
      * @param anchorInput
      *   The node immediately before the insertion point.
      * @param contAnchorOutput
      *   The anchor reproduced with `link` pointing to the new node.
      * @param newElementOutput
      *   The new node UTxO with the minted token and initial datum.
      */
    def insert(
        anchorInput: TxInInfo,
        contAnchorOutput: TxOut,
        newElementOutput: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        val (
          anchorAssetName,
          anchorData,
          anchorLink,
          contAnchorLink,
          newElemAssetName,
          newElemData,
          newElemLink
        ) =
            validateThreeElements(
              policyId,
              anchorInput.resolved,
              contAnchorOutput,
              newElementOutput
            )

        val newKey = extractKey(newElemAssetName, prefixLen)

        newElemData match
            case ElementData.Node(_) => ()
            case ElementData.Root(_) => fail("insert: new element must be Node")

        val mintQty = txMint.quantityOf(policyId, newElemAssetName)

        require(mintQty == BigInt(1), "New node NFT must be minted")
        require(contAnchorLink === Some(newKey), "Continued anchor must point to new node")
        require(newElemLink === anchorLink, "New element must inherit anchor's old link")
        require(
          hasPrefix(newElemAssetName, prefix, prefixLen),
          "New node asset name must start with the prefix"
        )

        validateAnchorAssetName(anchorAssetName, anchorData, rootKey, prefix, prefixLen)

        anchorData match
            case ElementData.Node(_) =>
                require(
                  Builtins.lessThanByteString(anchorAssetName, newElemAssetName),
                  "New asset name must be greater than anchor (ascending order)"
                )
            case ElementData.Root(_) => ()

        anchorLink match
            case None => ()
            case Some(linkKey) =>
                require(
                  Builtins.lessThanByteString(newKey, linkKey),
                  "New key must be less than link key (ascending order)"
                )
    }

    /** Validates appending a node at the tail of the list.
      *
      * The anchor must be the last element (`link = None`). No ordering constraint on the key.
      *
      * @param anchorInput
      *   The last element (`link` must be `None`).
      * @param contAnchorOutput
      *   The anchor reproduced with `link` pointing to the new node.
      * @param newElementOutput
      *   The new node UTxO with the minted token and initial datum.
      */
    def appendUnordered(
        anchorInput: TxInInfo,
        contAnchorOutput: TxOut,
        newElementOutput: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        val (
          anchorAssetName,
          anchorData,
          anchorLink,
          contAnchorLink,
          newElemAssetName,
          newElemData,
          newElemLink
        ) =
            validateThreeElements(
              policyId,
              anchorInput.resolved,
              contAnchorOutput,
              newElementOutput
            )

        val newKey = extractKey(newElemAssetName, prefixLen)

        newElemData match
            case ElementData.Node(_) => ()
            case ElementData.Root(_) => fail("appendUnordered: new element must be Node")

        val mintQty = txMint.quantityOf(policyId, newElemAssetName)

        require(mintQty == BigInt(1), "New node NFT must be minted")
        require(anchorLink === None, "Anchor must be the last element (no link)")
        require(contAnchorLink === Some(newKey), "Continued anchor must point to new node")
        require(newElemLink === None, "New element must point to nothing")
        require(
          hasPrefix(newElemAssetName, prefix, prefixLen),
          "New node asset name must start with the prefix"
        )

        validateAnchorAssetName(anchorAssetName, anchorData, rootKey, prefix, prefixLen)
    }

    /** Validates prepending a node immediately after the root.
      *
      * The root is the only valid anchor. The new node inherits the root's old `link`. No ordering
      * constraint on the key.
      *
      * @param rootInput
      *   The root UTxO (must carry a `Root` datum).
      * @param contRootOutput
      *   The root reproduced with `link` pointing to the new node.
      * @param newElementOutput
      *   The new node UTxO with the minted token and initial datum.
      */
    def prependUnordered(
        rootInput: TxInInfo,
        contRootOutput: TxOut,
        newElementOutput: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        val (
          rootAssetName,
          rootElemData,
          rootLink,
          contRootLink,
          newElemAssetName,
          newElemData,
          newElemLink
        ) =
            validateThreeElements(policyId, rootInput.resolved, contRootOutput, newElementOutput)

        val newKey = extractKey(newElemAssetName, prefixLen)

        rootElemData match
            case ElementData.Root(_) => ()
            case ElementData.Node(_) => fail("prependUnordered: anchor must be Root")

        newElemData match
            case ElementData.Node(_) => ()
            case ElementData.Root(_) => fail("prependUnordered: new element must be Node")

        val mintQty = txMint.quantityOf(policyId, newElemAssetName)

        require(mintQty == BigInt(1), "New node NFT must be minted")
        require(contRootLink === Some(newKey), "Continued root must point to new node")
        require(newElemLink === rootLink, "New element must inherit root's old link")
        require(
          hasPrefix(newElemAssetName, prefix, prefixLen),
          "New node asset name must start with the prefix"
        )
        require(rootAssetName === rootKey, "Anchor asset name must be rootKey")
    }

    /** Validates removing a node.
      *
      * The anchor's `link` must point to the node being removed. The anchor inherits the removed
      * node's `link` and the removed node's NFT is burned.
      *
      * @param anchorInput
      *   The node immediately before the one to remove.
      * @param removingNodeInput
      *   The node to remove.
      * @param contAnchorOutput
      *   The anchor reproduced with `link` skipping the removed node.
      */
    def remove(
        anchorInput: TxInInfo,
        removingNodeInput: TxInInfo,
        contAnchorOutput: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        val (
          anchorAssetName,
          anchorData,
          anchorLink,
          contAnchorLink,
          removingAssetName,
          removingData,
          removingLink
        ) =
            validateThreeElements(
              policyId,
              anchorInput.resolved,
              contAnchorOutput,
              removingNodeInput.resolved
            )

        val removingKey = extractKey(removingAssetName, prefixLen)

        removingData match
            case ElementData.Node(_) => ()
            case ElementData.Root(_) => fail("remove: removing element must be Node")

        val mintQty = txMint.quantityOf(policyId, removingAssetName)

        require(mintQty == BigInt(-1), "Removing node NFT must be burnt")
        require(anchorLink === Some(removingKey), "Anchor must point to the node being removed")
        require(
          contAnchorLink === removingLink,
          "Continued anchor must inherit removed node's link"
        )
        require(
          hasPrefix(removingAssetName, prefix, prefixLen),
          "Removing node must have the prefix"
        )

        validateAnchorAssetName(anchorAssetName, anchorData, rootKey, prefix, prefixLen)
    }

    /** Validates removing the head node.
      *
      * The root acts as the anchor and its `link` is advanced past the removed node. Unlike
      * [[remove]], the root's `data` is unconstrained — the caller may update it freely (e.g. to
      * accumulate the removed node's payload).
      *
      * @param rootInput
      *   The root UTxO (`link` must point to the head node).
      * @param headNodeInput
      *   The head node to remove.
      * @param contRootOutput
      *   The root reproduced with `link` advanced; `data` may change freely.
      */
    def removeHead(
        rootInput: TxInInfo,
        headNodeInput: TxInInfo,
        contRootOutput: TxOut,
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        val (rootAddr, rootAssetName, rootElemData, rootLink) =
            authenticateElementUtxoAndGetInfo(rootInput.resolved, policyId)
        rootElemData match
            case ElementData.Root(_) => ()
            case ElementData.Node(_) => fail("removeHead: anchor must be Root")

        val (contRootAddr, contRootAssetName, contRootElemData, contRootLink) =
            authenticateElementUtxoAndGetInfo(contRootOutput, policyId)
        contRootElemData match
            case ElementData.Root(_) => ()
            case ElementData.Node(_) => fail("removeHead: continued root must be Root")

        val (headNodeAddr, headNodeAssetName, headNodeElemData, headNodeLink) =
            authenticateElementUtxoAndGetInfo(headNodeInput.resolved, policyId)
        headNodeElemData match
            case ElementData.Node(_) => ()
            case ElementData.Root(_) => fail("removeHead: head element must be Node")

        val headNodeKey = extractKey(headNodeAssetName, prefixLen)
        val headMintQty = txMint.quantityOf(policyId, headNodeAssetName)

        require(headMintQty == BigInt(-1), "Head node NFT must be burned")
        require(rootAddr === contRootAddr, "Root must be reproduced at the same address")
        require(rootAssetName === contRootAssetName, "Root must preserve its NFT asset name")
        require(rootLink === Some(headNodeKey), "Root must point to the head node")
        require(rootAddr === headNodeAddr, "Head node must be at the same address as root")
        require(contRootLink === headNodeLink, "Continued root link must advance past head node")
        require(rootAssetName === rootKey, "Anchor asset name must be rootKey")
        require(hasPrefix(headNodeAssetName, prefix, prefixLen), "Head node must have the prefix")
    }

    /** Spending check for nodes being added or removed.
      *
      * Requires at least one token under `policyId` to be minted or burned, delegating structural
      * validation to the minting policy (coupling pattern).
      */
    def requireListTokensMintedOrBurned(policyId: PolicyId, txMint: Value): Unit =
        txMint.toSortedMap.get(policyId) match
            case None => fail("No list tokens minted or burned in this transaction")
            case Some(m) =>
                require(!m.isEmpty, "No list tokens minted or burned in this transaction")

    /** Validates updating a node's payload without structural changes.
      *
      * The element is reproduced at the same address with the same NFT and `link`; only `data` may
      * change. No list tokens are minted or burned. Explicit indices prevent double-satisfaction.
      *
      * @param elementInputIndex
      *   Index of the element in `txInputs`.
      * @param contElementOutputIndex
      *   Index of the reproduced element in `txOutputs`.
      * @param elementInputOutref
      *   Cross-checked against `txInputs` to guard against index manipulation.
      */
    def validateElementUpdate(
        elementInputIndex: BigInt,
        contElementOutputIndex: BigInt,
        elementInputOutref: TxOutRef,
        txInputs: List[TxInInfo],
        txOutputs: List[TxOut],
        txMint: Value,
        policyId: PolicyId,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit = {
        txMint.toSortedMap.get(policyId) match
            case None => ()
            case Some(m) =>
                require(m.isEmpty, "No list tokens may be minted or burned during update")

        val elemIn = txInputs.at(elementInputIndex)
        require(
          elemIn.outRef === elementInputOutref,
          "Input index does not match elementInputOutref"
        )
        val elemOut = txOutputs.at(contElementOutputIndex)

        val (elemAddr, elemAssetName, elemData, elemLink) =
            authenticateElementUtxoAndGetInfo(elemIn.resolved, policyId)
        val (contElemAddr, contElemAssetName, contElemData, contElemLink) =
            authenticateElementUtxoAndGetInfo(elemOut, policyId)

        require(elemAddr === contElemAddr, "Element must be reproduced at the same address")
        require(elemAssetName === contElemAssetName, "Element must preserve its NFT")
        require(elemLink === contElemLink, "Element's link must remain unchanged")

        elemData match
            case ElementData.Root(_) =>
                contElemData match
                    case ElementData.Root(_) => ()
                    case ElementData.Node(_) => fail("Element variant must remain Root")
                require(elemAssetName === rootKey, "Root element must have rootKey asset name")
            case ElementData.Node(_) =>
                contElemData match
                    case ElementData.Node(_) => ()
                    case ElementData.Root(_) => fail("Element variant must remain Node")
                require(hasPrefix(elemAssetName, prefix, prefixLen), "Node must have the prefix")
    }

    private def validateAnchorAssetName(
        anchorAssetName: TokenName,
        anchorData: ElementData,
        rootKey: RootKey,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Unit =
        anchorData match
            case ElementData.Root(_) =>
                require(anchorAssetName === rootKey, "Root anchor asset name must match rootKey")
            case ElementData.Node(_) =>
                require(
                  hasPrefix(anchorAssetName, prefix, prefixLen),
                  "Anchor node must have the prefix"
                )

    private def extractKey(assetName: TokenName, prefixLen: NodeKeyPrefixLength): NodeKey =
        Builtins.sliceByteString(prefixLen, assetName.length - prefixLen, assetName)

    private def hasPrefix(
        assetName: TokenName,
        prefix: NodeKeyPrefix,
        prefixLen: NodeKeyPrefixLength
    ): Boolean =
        Builtins.sliceByteString(BigInt(0), prefixLen, assetName) === prefix

    private def authenticateElementUtxoAndGetInfo(
        output: TxOut,
        policyId: PolicyId
    ): (Address, TokenName, ElementData, Link) = {
        val datum = output.datum match
            case OutputDatum.OutputDatum(d) => d
            case _                          => fail("Element UTxO must have inline datum")
        require(output.referenceScript === None, "Element UTxO must not have a reference script")
        val (assetName, qty) = output.value.toSortedMap.get(policyId) match
            case None => fail("Element UTxO must contain a list NFT")
            case Some(tokens) =>
                tokens.toList match
                    case List.Cons((assetName, qty), List.Nil) => (assetName, qty)
                    case _ => fail("Element UTxO must contain exactly one list NFT")
        require(qty == BigInt(1), "NFT quantity must be exactly 1")
        val element = datum.to[Element]
        (output.address, assetName, element.data, element.link)
    }

    private def validateThreeElements(
        policyId: PolicyId,
        anchorOut: TxOut,
        contAnchorOut: TxOut,
        listElement: TxOut
    ): (TokenName, ElementData, Link, Link, TokenName, ElementData, Link) = {
        val (anchorAddr, anchorKey, anchorData, anchorLink) =
            authenticateElementUtxoAndGetInfo(anchorOut, policyId)
        val (anchorContAddr, contAnchorKey, contAnchorData, contAnchorLink) =
            authenticateElementUtxoAndGetInfo(contAnchorOut, policyId)
        val (listElementAddr, listElementKey, listElementData, listElementLink) =
            authenticateElementUtxoAndGetInfo(listElement, policyId)

        require(anchorAddr === anchorContAddr, "Anchor must be reproduced at the same address")
        require(
          anchorAddr.credential === listElementAddr.credential,
          "New/removed element must share payment credential with anchor"
        )
        require(anchorKey === contAnchorKey, "Anchor must preserve its NFT asset name")
        require(anchorData === contAnchorData, "Anchor underlying data must remain unchanged")

        (
          anchorKey,
          anchorData,
          anchorLink,
          contAnchorLink,
          listElementKey,
          listElementData,
          listElementLink
        )
    }

}
