package scalus.examples.anonymousdata

import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.PlutusV3
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.node.{BlockchainProvider, NetworkSubmitError, NodeSubmitError, SubmitError}
import scalus.cardano.onchain.plutus.prelude.AssocMap
import scalus.crypto.tree.MerkleTree

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Transaction builder for anonymous data contract operations.
  *
  * @param env
  *   Cardano environment (network, protocol params)
  * @param evaluator
  *   Plutus script evaluator
  * @param contract
  *   compiled anonymous data contract
  * @param adminPkh
  *   admin public key hash (baked into the parameterized script)
  */
case class AnonymousDataTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[ByteString => Data => Unit],
    adminPkh: AddrKeyHash
) {
    private val parameterizedScript = contract.apply(adminPkh: ByteString)
    val scriptAddr: Address = parameterizedScript.address(env.network)
    val policyId: PolicyId = parameterizedScript.script.scriptHash
    private val beaconAsset = AssetName(AnonymousDataValidator.beaconTokenName)

    /** Initialize the shared UTXO with a beacon token and empty data map. */
    def initialize(
        utxos: Utxos,
        participantsRoot: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = AnonymousDataDatum(
          participantsRoot = participantsRoot,
          dataMap = AssocMap.empty[ByteString, ByteString]
        )
        val redeemer = AnonymousDataMintRedeemer.MintBeacon.toData

        TxBuilder(env, evaluator)
            .mint(
              parameterizedScript,
              Map(beaconAsset -> 1L),
              _ => redeemer
            )
            .requireSignatures(Set(adminPkh))
            .payTo(
              scriptAddr,
              Value.lovelace(2_000_000L) + Value.asset(policyId, beaconAsset, 1),
              datum
            )
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Store a new data entry in the shared UTXO. */
    def storeData(
        utxos: Utxos,
        sharedUtxo: Utxo,
        tree: MerkleTree,
        signerPkh: AddrKeyHash,
        dataKey: ByteString,
        encryptedData: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val proof = tree.proveMembership(signerPkh: ByteString)
        val redeemer =
            AnonymousDataSpendRedeemer.StoreData(proof, dataKey, encryptedData).toData

        val currentDatum = sharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        val newDatum = AnonymousDataDatum(
          participantsRoot = currentDatum.participantsRoot,
          dataMap = currentDatum.dataMap.insert(dataKey, encryptedData)
        )

        TxBuilder(env, evaluator)
            .spend(sharedUtxo, redeemer, parameterizedScript.script)
            .requireSignatures(Set(signerPkh))
            .payTo(scriptAddr, sharedUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Update an existing data entry in the shared UTXO. */
    def updateData(
        utxos: Utxos,
        sharedUtxo: Utxo,
        tree: MerkleTree,
        signerPkh: AddrKeyHash,
        dataKey: ByteString,
        newEncryptedData: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val proof = tree.proveMembership(signerPkh: ByteString)
        val redeemer =
            AnonymousDataSpendRedeemer.UpdateData(proof, dataKey, newEncryptedData).toData

        val currentDatum = sharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        val newDatum = AnonymousDataDatum(
          participantsRoot = currentDatum.participantsRoot,
          dataMap = currentDatum.dataMap.insert(dataKey, newEncryptedData)
        )

        TxBuilder(env, evaluator)
            .spend(sharedUtxo, redeemer, parameterizedScript.script)
            .requireSignatures(Set(signerPkh))
            .payTo(scriptAddr, sharedUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Delete a data entry from the shared UTXO. */
    def deleteData(
        utxos: Utxos,
        sharedUtxo: Utxo,
        tree: MerkleTree,
        signerPkh: AddrKeyHash,
        dataKey: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val proof = tree.proveMembership(signerPkh: ByteString)
        val redeemer =
            AnonymousDataSpendRedeemer.DeleteData(proof, dataKey).toData

        val currentDatum = sharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        val newDatum = AnonymousDataDatum(
          participantsRoot = currentDatum.participantsRoot,
          dataMap = currentDatum.dataMap.delete(dataKey)
        )

        TxBuilder(env, evaluator)
            .spend(sharedUtxo, redeemer, parameterizedScript.script)
            .requireSignatures(Set(signerPkh))
            .payTo(scriptAddr, sharedUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Update the participants MerkleTree root (admin only). */
    def updateParticipants(
        utxos: Utxos,
        sharedUtxo: Utxo,
        newParticipantsRoot: ByteString,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer =
            AnonymousDataSpendRedeemer.UpdateParticipants(newParticipantsRoot).toData

        val currentDatum = sharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        val newDatum = AnonymousDataDatum(
          participantsRoot = newParticipantsRoot,
          dataMap = currentDatum.dataMap
        )

        TxBuilder(env, evaluator)
            .spend(sharedUtxo, redeemer, parameterizedScript.script)
            .requireSignatures(Set(adminPkh))
            .payTo(scriptAddr, sharedUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Burn the beacon token (admin only). */
    def burn(
        utxos: Utxos,
        sharedUtxo: Utxo,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val burnRedeemer = AnonymousDataMintRedeemer.BurnBeacon.toData

        TxBuilder(env, evaluator)
            .spend(sharedUtxo)
            .mint(
              parameterizedScript,
              Map(beaconAsset -> -1L),
              _ => burnRedeemer
            )
            .requireSignatures(Set(adminPkh))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Create a gate: lock funds at the gate script with an expected data hash.
      *
      * The gate can be unlocked by proving that a specific decrypted entry in the anonymous data
      * store hashes to `expectedDataHash`.
      *
      * @param gateContract
      *   compiled gate contract (parameterized by anonymous data policyId)
      * @param expectedDataHash
      *   blake2b_256 hash of the expected decrypted data
      * @param lockedValue
      *   value to lock at the gate script
      */
    def createGate(
        gateContract: PlutusV3[ByteString => Data => Unit],
        expectedDataHash: ByteString,
        creatorPkh: AddrKeyHash,
        lockedValue: Value,
        utxos: Utxos,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val gateScript = gateContract.apply(policyId: ByteString)
        val gateAddr = gateScript.address(env.network)
        val datum = GateDatum(expectedDataHash, creatorPkh)

        TxBuilder(env, evaluator)
            .payTo(gateAddr, lockedValue, datum)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Unlock a gate by proving anonymous data via reference input.
      *
      * @param gateContract
      *   compiled gate contract (parameterized by anonymous data policyId)
      * @param gateUtxo
      *   the locked gate UTXO to spend
      * @param sharedUtxo
      *   the anonymous data shared UTXO (added as reference input)
      * @param dataKey
      *   the map key in the anonymous data store
      * @param decKey
      *   the decryption key (blake2b_256(nonce || "enc"))
      */
    def unlockGate(
        gateContract: PlutusV3[ByteString => Data => Unit],
        gateUtxo: Utxo,
        sharedUtxo: Utxo,
        dataKey: ByteString,
        decKey: ByteString,
        utxos: Utxos,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val gateScript = gateContract.apply(policyId: ByteString)
        val redeemer = GateRedeemer.Unlock(dataKey, decKey).toData

        TxBuilder(env, evaluator)
            .references(sharedUtxo)
            .spend(gateUtxo, redeemer, gateScript.script)
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Refund a gate to its creator (e.g., if the data entry was deleted).
      *
      * @param gateContract
      *   compiled gate contract (parameterized by anonymous data policyId)
      * @param gateUtxo
      *   the locked gate UTXO to spend
      * @param signerPkh
      *   the signer's pubkeyhash (must match creatorPkh in the gate datum)
      */
    def refundGate(
        gateContract: PlutusV3[ByteString => Data => Unit],
        gateUtxo: Utxo,
        signerPkh: AddrKeyHash,
        utxos: Utxos,
        changeAddress: Address,
        signer: TransactionSigner
    ): Transaction = {
        val gateScript = gateContract.apply(policyId: ByteString)
        val redeemer = GateRedeemer.Refund.toData

        TxBuilder(env, evaluator)
            .spend(gateUtxo, redeemer, gateScript.script)
            .requireSignatures(Set(signerPkh))
            .complete(availableUtxos = utxos, changeAddress)
            .sign(signer)
            .transaction
    }

    /** Find the shared UTXO (beacon token) at the script address. */
    def findSharedUtxo(provider: BlockchainProvider)(using ExecutionContext): Future[Option[Utxo]] =
        provider.findUtxos(scriptAddr).map {
            case Left(_) => None
            case Right(utxos) =>
                utxos
                    .find { case (_, txOut) =>
                        txOut.value.assets.assets.exists { case (cs, tokens) =>
                            cs == policyId && tokens.get(beaconAsset).exists(_ > 0)
                        }
                    }
                    .map(Utxo(_))
        }
}

object AnonymousDataTransactions {

    /** Submit a transaction with retry logic for TOCTOU race conditions on the shared UTXO.
      *
      * When the shared UTXO is consumed between query and submission (another participant modified
      * it), the transaction fails with `UtxoNotAvailable`. This method re-queries the shared UTXO
      * and rebuilds the transaction.
      *
      * @param provider
      *   blockchain provider for querying UTXOs and submitting transactions
      * @param maxRetries
      *   maximum number of retry attempts (default 3)
      * @param delayMs
      *   delay in milliseconds between retries (default 1000)
      * @param buildTx
      *   function that builds a transaction from fresh shared UTXO and available UTXOs
      * @return
      *   either a submit error or the transaction hash
      */
    def submitWithRetry(
        provider: BlockchainProvider,
        txCreator: AnonymousDataTransactions,
        payerAddress: Address,
        maxRetries: Int = 3,
        delayMs: Long = 1000
    )(
        buildTx: (Utxo, Utxos) => Transaction
    )(using ExecutionContext): Future[Either[SubmitError, TransactionHash]] = {
        def attempt(retriesLeft: Int): Future[Either[SubmitError, TransactionHash]] =
            for {
                sharedUtxoOpt <- txCreator.findSharedUtxo(provider)
                utxosResult <- provider.findUtxos(payerAddress)
                result <- (sharedUtxoOpt, utxosResult) match
                    case (None, _) =>
                        Future.successful(
                          Left(
                            NetworkSubmitError.InternalError("Shared UTXO not found")
                          )
                        )
                    case (_, Left(err)) =>
                        Future.successful(
                          Left(
                            NetworkSubmitError.InternalError(s"Failed to query UTXOs: $err")
                          )
                        )
                    case (Some(sharedUtxo), Right(utxos)) =>
                        Try(buildTx(sharedUtxo, utxos)) match
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
            } yield result

        attempt(maxRetries)
    }

    private def isRetryable(error: SubmitError): Boolean = error match
        case _: NetworkSubmitError.ConnectionError => true
        case _: NetworkSubmitError.InternalError   => true
        case _: NetworkSubmitError.MempoolFull     => true
        case _: NodeSubmitError.UtxoNotAvailable   => true
        case _                                     => false
}
