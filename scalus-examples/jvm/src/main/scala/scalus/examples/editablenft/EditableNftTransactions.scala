package scalus.examples.editablenft

import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v3.TxOutRef
import scalus.uplc.PlutusV3
import scalus.ledger.api.v3.TxId

/** Transaction creator for CIP-68 style editable NFTs.
  *
  * Design:
  *   - No owner field in datum - ownership = holding the user token
  *   - To edit: include user token in tx inputs (proves ownership)
  *   - To transfer: just send the user token (no validator needed)
  *   - Uses indexed UTxO pattern for O(1) lookups
  */
case class EditableNftTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Data => Unit],
    seed: Utxo
) {
    private val parameterizedScript = {
        val txOutRef = TxOutRef(TxId(seed.input.transactionId), BigInt(seed.input.index))
        contract.apply(txOutRef.toData)
    }
    val scriptAddr: Address = parameterizedScript.address(env.network)
    val policyId: PolicyId = parameterizedScript.script.scriptHash

    // ===== Helper methods =====

    /** Find user NFT UTXO in the given set of UTXOs */
    private def findUserNftUtxo(utxos: Utxos, userTokenName: ByteString): Utxo =
        utxos
            .find { case (_, out) =>
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
                }
            }
            .map(Utxo.apply)
            .getOrElse(throw new Exception("User NFT not found in UTXOs"))

    /** Find input index for a given UTXO in a transaction */
    private def findInputIndex(tx: Transaction, utxo: Utxo): Int =
        tx.body.value.inputs.toSeq.indexWhere { input =>
            input.transactionId == utxo.input.transactionId &&
            input.index == utxo.input.index
        }

    /** Find output index by address and asset */
    private def findOutputIndex(tx: Transaction, address: Address, asset: ByteString): Int =
        tx.body.value.outputs.toSeq.indexWhere { output =>
            output.value.address == address &&
            output.value.value.assets.assets.exists { case (cs, tokens) =>
                cs == policyId && tokens.get(AssetName(asset)).exists(_ > 0)
            }
        }

    /** Build a spend redeemer that continues the reference NFT at script address */
    private def buildContinuationSpendRedeemer(
        userNftUtxo: Utxo,
        refAsset: ByteString
    )(tx: Transaction): Data = {
        val userNftInputIndex = findInputIndex(tx, userNftUtxo)
        val refNftOutputIndex = findOutputIndex(tx, scriptAddr, refAsset)
        SpendRedeemer.Spend(BigInt(userNftInputIndex), BigInt(refNftOutputIndex)).toData
    }

    // ===== Public API =====

    /** Mint both reference NFT and user NFT.
      *
      * Creates:
      *   - Reference NFT (100 ++ tokenId) at script address with inline datum
      *   - User NFT (222 ++ tokenId) sent to holder's address
      */
    def mint(
        utxos: Utxos,
        tokenId: ByteString,
        initialData: ByteString,
        holderAddress: Address,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val refDatum = ReferenceNftDatum(
          tokenId = tokenId,
          data = initialData,
          isSealed = false
        )
        val refAsset = EditableNftValidator.refNftName(tokenId)
        val userAsset = EditableNftValidator.userNftName(tokenId)

        def buildMintRedeemer(tx: Transaction): Data = {
            val seedIndex = findInputIndex(tx, seed)
            val refNftOutIndex = findOutputIndex(tx, scriptAddr, refAsset)
            MintRedeemer.Mint(BigInt(seedIndex), BigInt(refNftOutIndex)).toData
        }

        TxBuilder(env, evaluator)
            .spend(seed)
            .mint(
              parameterizedScript.script,
              Map(AssetName(refAsset) -> 1L, AssetName(userAsset) -> 1L),
              buildMintRedeemer
            )
            .payTo(scriptAddr, Value.asset(policyId, AssetName(refAsset), 1), refDatum)
            .payTo(holderAddress, Value.asset(policyId, AssetName(userAsset), 1))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Edit the NFT data. The user token must be in the provided UTXOs. */
    def edit(
        utxos: Utxos,
        refNftUtxo: Utxo,
        newData: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val oldDatum = refNftUtxo.output.inlineDatum.get.to[ReferenceNftDatum]
        val newDatum = oldDatum.copy(data = newData)
        val userNftUtxo = findUserNftUtxo(utxos, oldDatum.userNftName)

        TxBuilder(env, evaluator)
            .spend(
              refNftUtxo,
              buildContinuationSpendRedeemer(userNftUtxo, oldDatum.refNftName),
              parameterizedScript.script,
              Set.empty
            )
            .spend(userNftUtxo)
            .payTo(scriptAddr, refNftUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Seal the NFT, making it immutable. The user token must be in the provided UTXOs. */
    def seal(
        utxos: Utxos,
        refNftUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val oldDatum = refNftUtxo.output.inlineDatum.get.to[ReferenceNftDatum]
        val newDatum = oldDatum.copy(isSealed = true)
        val userNftUtxo = findUserNftUtxo(utxos, oldDatum.userNftName)

        TxBuilder(env, evaluator)
            .spend(
              refNftUtxo,
              buildContinuationSpendRedeemer(userNftUtxo, oldDatum.refNftName),
              parameterizedScript.script,
              Set.empty
            )
            .spend(userNftUtxo)
            .payTo(scriptAddr, refNftUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Burn both reference NFT and user NFT. The user token must be in the provided UTXOs. */
    def burn(
        utxos: Utxos,
        refNftUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = refNftUtxo.output.inlineDatum.get.to[ReferenceNftDatum]
        val refAsset = AssetName(datum.refNftName)
        val userAsset = AssetName(datum.userNftName)
        val userNftUtxo = findUserNftUtxo(utxos, datum.userNftName)

        def buildBurnSpendRedeemer(tx: Transaction): Data = {
            val userNftInputIndex = findInputIndex(tx, userNftUtxo)
            SpendRedeemer.Burn(BigInt(userNftInputIndex)).toData
        }

        TxBuilder(env, evaluator)
            .spend(refNftUtxo, buildBurnSpendRedeemer, parameterizedScript.script, Set.empty)
            .spend(userNftUtxo)
            .mint(
              parameterizedScript.script,
              Map(refAsset -> -1L, userAsset -> -1L),
              _ => MintRedeemer.Burn.toData
            )
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }
}
