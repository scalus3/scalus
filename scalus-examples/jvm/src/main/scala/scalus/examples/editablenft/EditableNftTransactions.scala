package scalus.examples.editablenft

import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.uplc.PlutusV3
import EditableNftValidator.REFERENCE_NFT_LABEL
import EditableNftValidator.USER_NFT_LABEL

/** Transaction creator for CIP-68 style editable NFTs.
  *
  * Simplified design:
  *   - No owner field in datum - ownership = holding the user token
  *   - To edit: include user token in tx inputs (proves ownership)
  *   - To transfer: just send the user token (no validator needed)
  */
case class EditableNftTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit]
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)
    val policyId: PolicyId = script.scriptHash

    /** Mint both reference NFT and user NFT.
      *
      * Creates:
      *   - Reference NFT (100 ++ tokenId) at script address with inline datum
      *   - User NFT (222 ++ tokenId) sent to holder's address
      *
      * @param utxos
      *   UTXOs to fund the transaction
      * @param tokenId
      *   Base token name (without CIP-68 label prefix)
      * @param initialData
      *   Initial editable data
      * @param holderAddress
      *   Address to send the user NFT to
      * @param changeAddress
      *   Change address
      * @param signer
      *   Transaction signer
      */
    def mint(
        utxos: Utxos,
        tokenId: ByteString,
        initialData: ByteString,
        holderAddress: Address,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val refTokenName = REFERENCE_NFT_LABEL ++ tokenId
        val userTokenName = USER_NFT_LABEL ++ tokenId

        val refDatum = ReferenceNftDatum(
          tokenId = tokenId,
          data = initialData,
          isSealed = false
        )

        val refAsset = AssetName(refTokenName)
        val userAsset = AssetName(userTokenName)

        // Mint redeemer wraps the tokenId
        val mintRedeemer = MintRedeemer(tokenId)
        TxBuilder(env, evaluator)
            .mint(script, Map(refAsset -> 1L, userAsset -> 1L), mintRedeemer, Set.empty)
            .payTo(scriptAddress, Value.asset(policyId, refAsset, 1), refDatum)
            .payTo(holderAddress, Value.asset(policyId, userAsset, 1))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Edit the NFT data.
      *
      * The user token must be in the provided UTXOs - this proves ownership.
      *
      * @param utxos
      *   UTXOs to fund the transaction (must include user NFT)
      * @param refNftUtxo
      *   The reference NFT UTXO at script address
      * @param newData
      *   New data to set
      * @param changeAddress
      *   Change address
      * @param signer
      *   Transaction signer
      */
    def edit(
        utxos: Utxos,
        refNftUtxo: Utxo,
        newData: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val oldDatum = refNftUtxo.output.inlineDatum.get.to[ReferenceNftDatum]
        val newDatum = oldDatum.copy(data = newData)

        // Find user NFT in utxos and spend it (to prove ownership)
        val userTokenName = USER_NFT_LABEL ++ oldDatum.tokenId
        val userNftUtxo = utxos
            .find { case (_, out) =>
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
                }
            }
            .map(Utxo.apply)
            .getOrElse(throw new Exception("User NFT not found in UTXOs"))

        TxBuilder(env, evaluator)
            .spend(refNftUtxo, redeemer = Data.unit, script, Set.empty)
            .spend(userNftUtxo) // Include user token to prove ownership
            .payTo(scriptAddress, refNftUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Seal the NFT, making it immutable.
      *
      * The user token must be in the provided UTXOs - this proves ownership.
      *
      * @param utxos
      *   UTXOs to fund the transaction (must include user NFT)
      * @param refNftUtxo
      *   The reference NFT UTXO at script address
      * @param changeAddress
      *   Change address
      * @param signer
      *   Transaction signer
      */
    def seal(
        utxos: Utxos,
        refNftUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val oldDatum = refNftUtxo.output.inlineDatum.get.to[ReferenceNftDatum]
        val newDatum = oldDatum.copy(isSealed = true)

        // Find user NFT in utxos and spend it (to prove ownership)
        val userTokenName = USER_NFT_LABEL ++ oldDatum.tokenId
        val userNftUtxo = utxos
            .find { case (_, out) =>
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
                }
            }
            .map(Utxo.apply)
            .getOrElse(throw new Exception("User NFT not found in UTXOs"))

        TxBuilder(env, evaluator)
            .spend(refNftUtxo, redeemer = Data.unit, script, Set.empty)
            .spend(userNftUtxo) // Include user token to prove ownership
            .payTo(scriptAddress, refNftUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }
}
