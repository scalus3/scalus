package scalus.examples.linkedlist

import scalus.compiler.Compile
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{Credential, PolicyId}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.patterns.{LinkedList, NodeKeyPrefix, NodeKeyPrefixLength, RootKey}

// Redeemer
enum ListAction derives FromData, ToData {
    case Init(producedOutputIndex: BigInt)
    case Deinit(rootInputIndex: BigInt)
    case Insert(
        anchorInputIndex: BigInt,
        contAnchorOutputIndex: BigInt,
        newElemOutputIndex: BigInt
    )
    case AppendUnordered(
        anchorInputIndex: BigInt,
        contAnchorOutputIndex: BigInt,
        newElemOutputIndex: BigInt
    )
    case PrependUnordered(
        rootInputIndex: BigInt,
        contRootOutputIndex: BigInt,
        newElemOutputIndex: BigInt
    )
    case Remove(anchorInputIndex: BigInt, removingInputIndex: BigInt, contAnchorOutputIndex: BigInt)
    case RemoveHead(
        rootInputIndex: BigInt,
        headInputIndex: BigInt,
        contRootOutputIndex: BigInt
    )
    case SpendForUpdate(elemInputIndex: BigInt, contElemOutputIndex: BigInt)
    case Spend
}

// Param
case class ListConfig(
    rootKey: RootKey,
    prefix: NodeKeyPrefix,
    prefixLen: NodeKeyPrefixLength
) derives FromData,
      ToData

/** A validator for a singly linked list.
  *
  * The script is parameterized by (rootKey, prefix, prefixLen)`. The minting policy ID is derived
  * at runtime from the `ScriptContext`; it does **not** need to be stored in the configuration.
  *
  * The minting policy checks:
  *   - `Init` / `Deinit` structural invariants via [[LinkedList.init]] / [[LinkedList.deinit]].
  *   - All insert/remove/fold operations via the respective [[LinkedList]] helpers.
  *
  * The spending script delegates to the minting policy by checking that list NFTs are being
  * minted/burnt (coupling pattern) – except for `SpendForUpdate` which verifies only data changes.
  */
@Compile
object LinkedListValidator extends DataParameterizedValidator {

    inline def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val cfg = param.to[ListConfig]
        val action = redeemer.to[ListAction]

        action match {
            case ListAction.Init(producedIdx) =>
                val producedOutput = tx.outputs.at(producedIdx)
                LinkedList.init(
                  rootOut = producedOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey
                )

            case ListAction.Deinit(rootInputIdx) =>
                val rootInput = tx.inputs.at(rootInputIdx)
                LinkedList.deinit(
                  rootInput = rootInput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey
                )

            case ListAction.Insert(anchorIdx, contAnchorIdx, newElemIdx) =>
                val anchorInput = tx.inputs.at(anchorIdx)
                val contAnchorOutput = tx.outputs.at(contAnchorIdx)
                val newElemOutput = tx.outputs.at(newElemIdx)
                LinkedList.insert(
                  anchorInput = anchorInput,
                  contAnchorOutput = contAnchorOutput,
                  newElementOutput = newElemOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case ListAction.AppendUnordered(anchorIdx, contAnchorIdx, newElemIdx) =>
                val anchorInput = tx.inputs.at(anchorIdx)
                val contAnchorOutput = tx.outputs.at(contAnchorIdx)
                val newElemOutput = tx.outputs.at(newElemIdx)
                LinkedList.appendUnordered(
                  anchorInput = anchorInput,
                  contAnchorOutput = contAnchorOutput,
                  newElementOutput = newElemOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case ListAction.PrependUnordered(rootIdx, contRootIdx, newElemIdx) =>
                val rootInput = tx.inputs.at(rootIdx)
                val contRootOutput = tx.outputs.at(contRootIdx)
                val newElemOutput = tx.outputs.at(newElemIdx)
                LinkedList.prependUnordered(
                  rootInput = rootInput,
                  contRootOutput = contRootOutput,
                  newElementOutput = newElemOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case ListAction.Remove(anchorIdx, removingIdx, contAnchorIdx) =>
                val anchorInput = tx.inputs.at(anchorIdx)
                val removingInput = tx.inputs.at(removingIdx)
                val contAnchorOutput = tx.outputs.at(contAnchorIdx)
                LinkedList.remove(
                  anchorInput = anchorInput,
                  removingNodeInput = removingInput,
                  contAnchorOutput = contAnchorOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case ListAction.RemoveHead(rootIdx, headIdx, contRootIdx) =>
                val rootInput = tx.inputs.at(rootIdx)
                val headInput = tx.inputs.at(headIdx)
                val contRootOutput = tx.outputs.at(contRootIdx)
                LinkedList.removeHead(
                  rootInput = rootInput,
                  headNodeInput = headInput,
                  contRootOutput = contRootOutput,
                  txMint = tx.mint,
                  policyId = policyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case ListAction.SpendForUpdate(_, _) =>
                fail("SpendForUpdate is a spending-only action")

            case ListAction.Spend =>
                fail("Spend is a spending-only action")
        }
    }

    inline def spend(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val cfg = param.to[ListConfig]
        val action = redeemer.to[ListAction]
        val ownInput = tx.findOwnInputOrFail(ownRef, "Own input not found")
        val nftPolicyId = ownInput.resolved.value.toSortedMap.toList match
            case List.Cons((_, _), List.Cons((nftPol, _), List.Nil)) => nftPol
            case _ => fail("Cannot find NFT policy in own UTxO")

        action match {
            case ListAction.SpendForUpdate(elemInputIdx, contElemOutputIdx) =>
                val elemInput = tx.inputs.at(elemInputIdx)
                require(elemInput.outRef === ownRef, "Spend: input outref mismatch")
                LinkedList.validateElementUpdate(
                  elementInputIndex = elemInputIdx,
                  contElementOutputIndex = contElemOutputIdx,
                  elementInputOutref = ownRef,
                  txInputs = tx.inputs,
                  txOutputs = tx.outputs,
                  txMint = tx.mint,
                  policyId = nftPolicyId,
                  rootKey = cfg.rootKey,
                  prefix = cfg.prefix,
                  prefixLen = cfg.prefixLen
                )

            case _ =>
                LinkedList.requireListTokensMintedOrBurned(nftPolicyId, tx.mint)
        }
    }
}
