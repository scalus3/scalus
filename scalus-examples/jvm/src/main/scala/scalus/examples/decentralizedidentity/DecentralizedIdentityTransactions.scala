package scalus.examples.decentralizedidentity

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.PlutusV3

import scalus.cardano.node.{BlockchainProvider, NetworkSubmitError, NodeSubmitError, SubmitError}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Transaction builder for Decentralized Identity operations.
  *
  * Creates and manages identity NFTs, delegation tokens, and attribute tokens.
  */
case class DecentralizedIdentityTransactions(
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

    /** The unique identity token name, derived from seed UTXO. */
    val identityTokenName: ByteString = {
        val uniqueId = blake2b_224(
          seed.input.transactionId ++ ByteString.fromArray(
            BigInt(seed.input.index).toByteArray
          )
        )
        DecentralizedIdentityValidator.identityTokenName(uniqueId)
    }

    // ===== Helper methods =====

    /** Find input index for a given UTXO in a transaction */
    private def findInputIndex(tx: Transaction, utxo: Utxo): Int = {
        val idx = tx.body.value.inputs.toSeq.indexWhere { input =>
            input.transactionId == utxo.input.transactionId &&
            input.index == utxo.input.index
        }
        require(idx >= 0, s"Input not found in transaction: ${utxo.input}")
        idx
    }

    /** Find reference input index for a given UTXO in a transaction */
    private def findRefInputIndex(tx: Transaction, utxo: Utxo): Int = {
        val idx = tx.body.value.referenceInputs.toSeq.indexWhere { input =>
            input.transactionId == utxo.input.transactionId &&
            input.index == utxo.input.index
        }
        require(idx >= 0, s"Reference input not found in transaction: ${utxo.input}")
        idx
    }

    /** Find output index by address and asset */
    private def findOutputIndex(tx: Transaction, address: Address, asset: ByteString): Int = {
        val idx = tx.body.value.outputs.toSeq.indexWhere { output =>
            output.value.address == address &&
            output.value.value.assets.assets.exists { case (cs, tokens) =>
                cs == policyId && tokens.get(AssetName(asset)).exists(_ > 0)
            }
        }
        require(idx >= 0, s"Output not found for address $address with asset")
        idx
    }

    // ===== Public API =====

    /** Create a new identity.
      *
      * Mints an identity NFT at the script address with the owner's PubKeyHash in the datum.
      */
    def createIdentity(
        utxos: Utxos,
        ownerPkh: AddrKeyHash,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = IdentityDatum(PubKeyHash(ownerPkh))
        val idAsset = AssetName(identityTokenName)

        def buildMintRedeemer(tx: Transaction): Data = {
            val seedIndex = findInputIndex(tx, seed)
            val identityOutIndex = findOutputIndex(tx, scriptAddr, identityTokenName)
            MintAction.CreateIdentity(BigInt(seedIndex), BigInt(identityOutIndex)).toData
        }

        TxBuilder(env, evaluator)
            .spend(seed)
            .mint(
              parameterizedScript,
              Map(idAsset -> 1L),
              buildMintRedeemer
            )
            .payTo(scriptAddr, Value.asset(policyId, idAsset, 1), datum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Transfer identity ownership.
      *
      * Spends the identity UTXO and re-creates it with a new owner. Both current and new owner must
      * sign.
      */
    def transferOwnership(
        utxos: Utxos,
        identityUtxo: Utxo,
        newOwnerPkh: AddrKeyHash,
        changeAddress: Address,
        ownerSigner: TransactionSigner,
        newOwnerSigner: TransactionSigner
    ): Transaction = {
        val newDatum = IdentityDatum(PubKeyHash(newOwnerPkh))

        def buildSpendRedeemer(tx: Transaction): Data = {
            val identityOutIndex = findOutputIndex(tx, scriptAddr, identityTokenName)
            SpendAction.TransferOwnership(PubKeyHash(newOwnerPkh), BigInt(identityOutIndex)).toData
        }

        val oldOwnerPkh =
            identityUtxo.output.inlineDatum.get.to[IdentityDatum].ownerPkh.hash

        TxBuilder(env, evaluator)
            .spend(
              identityUtxo,
              buildSpendRedeemer,
              parameterizedScript,
              Set(AddrKeyHash(oldOwnerPkh), newOwnerPkh)
            )
            .payTo(scriptAddr, identityUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(ownerSigner)
            .sign(newOwnerSigner)
            .transaction
    }

    /** Add a delegate.
      *
      * Identity owner mints a delegation token at the script address. The identity UTXO is used as
      * a reference input.
      */
    def addDelegate(
        utxos: Utxos,
        identityUtxo: Utxo,
        delegatePkh: AddrKeyHash,
        validFrom: Instant,
        validUntil: Instant,
        delegateType: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val ownerPkh =
            identityUtxo.output.inlineDatum.get.to[IdentityDatum].ownerPkh.hash

        val delegTn = DecentralizedIdentityValidator.delegationTokenName(
          identityTokenName,
          PubKeyHash(delegatePkh)
        )
        val delegAsset = AssetName(delegTn)

        val datum = DelegationDatum(
          identityTokenName = identityTokenName,
          delegatePkh = PubKeyHash(delegatePkh),
          validFrom = BigInt(validFrom.toEpochMilli),
          validUntil = BigInt(validUntil.toEpochMilli),
          delegateType = delegateType
        )

        def buildMintRedeemer(tx: Transaction): Data = {
            val identityRefInputIndex = findRefInputIndex(tx, identityUtxo)
            val delegationOutIndex = findOutputIndex(tx, scriptAddr, delegTn)
            MintAction.AddDelegate(BigInt(identityRefInputIndex), BigInt(delegationOutIndex)).toData
        }

        TxBuilder(env, evaluator)
            .references(identityUtxo)
            .mint(
              parameterizedScript,
              Map(delegAsset -> 1L),
              buildMintRedeemer,
              Set(AddrKeyHash(ownerPkh))
            )
            .payTo(scriptAddr, Value.asset(policyId, delegAsset, 1), datum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Revoke a delegate.
      *
      * Identity owner burns a delegation token. The identity UTXO is used as a reference input to
      * verify ownership.
      */
    def revokeDelegate(
        utxos: Utxos,
        identityUtxo: Utxo,
        delegationUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val ownerPkh =
            identityUtxo.output.inlineDatum.get.to[IdentityDatum].ownerPkh.hash
        val delegDatum = delegationUtxo.output.inlineDatum.get.to[DelegationDatum]
        val delegTn = DecentralizedIdentityValidator.delegationTokenName(
          delegDatum.identityTokenName,
          delegDatum.delegatePkh
        )
        val delegAsset = AssetName(delegTn)

        TxBuilder(env, evaluator)
            .references(identityUtxo)
            .spend(
              delegationUtxo,
              SpendAction.RevokeDelegate,
              parameterizedScript,
              Set(AddrKeyHash(ownerPkh))
            )
            .mint(parameterizedScript, Map(delegAsset -> -1L), MintAction.Burn)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Publish an attribute.
      *
      * A delegate publishes an attribute on behalf of the identity. The delegation UTXO is used as
      * a reference input to verify the delegate's authority.
      */
    def publishAttribute(
        utxos: Utxos,
        delegationUtxo: Utxo,
        key: ByteString,
        value: ByteString,
        validFrom: Instant,
        validUntil: Instant,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val delegDatum = delegationUtxo.output.inlineDatum.get.to[DelegationDatum]
        val attrTn = DecentralizedIdentityValidator.attributeTokenName(
          delegDatum.identityTokenName,
          key
        )
        val attrAsset = AssetName(attrTn)

        val datum = AttributeDatum(
          identityTokenName = delegDatum.identityTokenName,
          key = key,
          value = value
        )

        def buildMintRedeemer(tx: Transaction): Data = {
            val delegationRefInputIndex = findRefInputIndex(tx, delegationUtxo)
            val attributeOutIndex = findOutputIndex(tx, scriptAddr, attrTn)
            MintAction
                .PublishAttribute(BigInt(delegationRefInputIndex), BigInt(attributeOutIndex))
                .toData
        }

        TxBuilder(env, evaluator)
            .references(delegationUtxo)
            .mint(
              parameterizedScript,
              Map(attrAsset -> 1L),
              buildMintRedeemer,
              Set(AddrKeyHash(delegDatum.delegatePkh.hash))
            )
            .validFrom(validFrom)
            .validTo(validUntil)
            .payTo(scriptAddr, Value.asset(policyId, attrAsset, 1), datum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Revoke an attribute.
      *
      * Identity owner burns an attribute token. The identity UTXO is used as a reference input to
      * verify ownership.
      */
    def revokeAttribute(
        utxos: Utxos,
        identityUtxo: Utxo,
        attributeUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val ownerPkh =
            identityUtxo.output.inlineDatum.get.to[IdentityDatum].ownerPkh.hash
        val attrDatum = attributeUtxo.output.inlineDatum.get.to[AttributeDatum]
        val attrTn = DecentralizedIdentityValidator.attributeTokenName(
          attrDatum.identityTokenName,
          attrDatum.key
        )
        val attrAsset = AssetName(attrTn)

        TxBuilder(env, evaluator)
            .references(identityUtxo)
            .spend(
              attributeUtxo,
              SpendAction.RevokeAttribute,
              parameterizedScript,
              Set(AddrKeyHash(ownerPkh))
            )
            .mint(parameterizedScript, Map(attrAsset -> -1L), MintAction.Burn)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

}

object DecentralizedIdentityTransactions {

    /** Submit a transaction with retry logic for TOCTOU race conditions.
      *
      * When a UTXO is consumed between query time and submission time, the transaction fails with
      * `UtxoNotAvailable`. This method re-queries UTXOs and rebuilds the transaction on retryable
      * errors.
      *
      * Note: uses `Thread.sleep` for delay, which blocks a thread pool thread. This is
      * intentionally simple for example code; production code should use a scheduler-based delay.
      *
      * @param provider
      *   blockchain provider for querying UTXOs and submitting transactions
      * @param queryAddress
      *   address to re-query UTXOs from on retry
      * @param maxRetries
      *   maximum number of retry attempts (default 3)
      * @param delayMs
      *   delay in milliseconds between retries (default 1000)
      * @param buildTx
      *   function that builds a transaction from fresh UTXOs
      * @return
      *   either a submit error or the transaction hash
      */
    def submitWithRetry(
        provider: BlockchainProvider,
        queryAddress: Address,
        maxRetries: Int = 3,
        delayMs: Long = 1000
    )(
        buildTx: Utxos => Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        def attempt(retriesLeft: Int): Future[Either[SubmitError, TransactionHash]] =
            provider.findUtxos(queryAddress).flatMap {
                case Left(err) =>
                    Future.successful(
                      Left(NetworkSubmitError.InternalError(s"Failed to query UTXOs: ${err}"))
                    )
                case Right(utxos) =>
                    Try(buildTx(utxos)) match
                        case Failure(ex) =>
                            Future.successful(
                              Left(
                                NetworkSubmitError.InternalError(
                                  s"Failed to build transaction: ${ex.getMessage}",
                                  Some(ex)
                                )
                              )
                            )
                        case Success(tx) =>
                            provider.submit(tx).flatMap {
                                case right @ Right(_) => Future.successful(right)
                                case left @ Left(err) =>
                                    if retriesLeft > 0 && isRetryable(err) then
                                        Future {
                                            Thread.sleep(delayMs)
                                        }.flatMap(_ => attempt(retriesLeft - 1))
                                    else Future.successful(left)
                            }
            }

        attempt(maxRetries)
    }

    private def isRetryable(error: SubmitError): Boolean = error match
        case _: NetworkSubmitError.ConnectionError => true
        case _: NetworkSubmitError.InternalError   => true
        case _: NetworkSubmitError.MempoolFull     => true
        case _: NodeSubmitError.UtxoNotAvailable   => true
        case _                                     => false
}
