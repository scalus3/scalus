package scalus.cardano.txbuilder

import scalus.builtin.Builtins.{blake2b_256, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{Data, ToData}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.utils.{CollateralSufficient, MinCoinSizedTransactionOutput}
import scalus.cardano.node.{toAsync, AsyncProvider, Provider}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** A high-level, fluent API for building Cardano transactions.
  *
  * TxBuilder provides a convenient way to construct transactions by chaining method calls. It
  * handles input selection, output creation, script attachment, minting, and transaction
  * finalization including fee calculation and change handling.
  *
  * TxBuilder is purely functional; each method returns a new instance with the updated state. The
  * only methods that does effects is `complete`, others are pure.
  *
  * ==Usage==
  * {{{
  * val tx = TxBuilder(env)
  *   .spend(utxo)
  *   .payTo(recipientAddress, Value.ada(10))
  *   .build(changeTo = changeAddress)
  *   .sign(signer)
  *   .transaction
  * }}}
  *
  * @param env
  *   the environment containing protocol parameters, network info, and slot configuration
  * @param context
  *   the current transaction builder context
  * @param evaluator
  *   the Plutus script evaluator used for script validation
  * @param diffHandlerOpt
  *   optional handler for managing transaction balance differences (change)
  * @param steps
  *   accumulated transaction building steps
  * @param attachedData
  *   datum values to be included in the transaction witness set
  * @param validators
  *   ledger rules to run against the built transaction for additional validations
  */
case class TxBuilder(
    env: Environment,
    context: TransactionBuilder.Context,
    evaluator: PlutusScriptEvaluator,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedData: Map[DataHash, Data] = Map.empty,
    validators: Seq[Validator] = Seq.empty
) {

    /** Spends a UTXO with an explicit witness.
      *
      * This is the most flexible spend method, allowing full control over the witness
      * configuration. Use this when you need custom witness settings not covered by the convenience
      * overloads.
      *
      * @param utxo
      *   the UTXO to spend
      * @param witness
      *   the witness specifying how to authorize the spend (PubKeyWitness, NativeScriptWitness, or
      *   ThreeArgumentPlutusScriptWitness)
      */
    def spend(utxo: Utxo, witness: SpendWitness): TxBuilder =
        addSteps(TransactionBuilderStep.Spend(utxo, witness))

    /** Adds the specified pubkey utxo to the list of inputs, thus spending it.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to the `changeTo` or `diffHandler` parameter of [[build]].
      * @param utxo
      *   utxo to spend
      * @note
      *   use [[spend]] with utxo and redeemer to spend script-protected utxos. Otherwise, [[build]]
      *   throws.
      */
    def spend(utxo: Utxo): TxBuilder =
        addSteps(TransactionBuilderStep.Spend(utxo, PubKeyWitness))

    /** Adds the specified pubkey utxos to the list of inputs, thus spending them.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to the `changeTo` or `diffHandler` parameter of [[build]].
      * @param utxos
      *   utxos to spend
      * @note
      *   use [[spend]] with utxo and redeemer to spend script-protected utxos. Otherwise, [[build]]
      *   throws.
      */
    def spend(utxos: Utxos): TxBuilder = {
        utxos.foldLeft(this) { case (builder, utxo) => builder.spend(Utxo(utxo)) }
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set.
      *
      * The script must be present as a reference input (via [[references]]) for this to work. If
      * the script that protects the `utxo` fails with the specified `redeemer`, [[build]] is going
      * to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to the `changeTo` or `diffHandler` parameter of [[build]].
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptAttached,
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = requiredSigners.map(ExpectedSigner.apply)
        )
        spend(utxo, witness)
    }

    /** Adds the specified script-protected utxo with a delayed redeemer that is computed from the
      * built transaction.
      *
      * Use this method when the redeemer depends on the final transaction structure (e.g., for
      * self-referential scripts). The redeemer is computed after the transaction is assembled but
      * before script evaluation.
      *
      * The script must be present as a reference input (via [[references]]) for this to work.
      *
      * @param utxo
      *   utxo to spend
      * @param redeemerBuilder
      *   function that computes the redeemer from the assembled transaction
      */
    def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptAttached,
          redeemerBuilder = redeemerBuilder,
          datum = datum,
          additionalSigners = Set.empty
        )
        spend(utxo, witness)
    }

    /** Spends a script-protected UTXO with a delayed redeemer computed from the built transaction.
      *
      * Use this method when the redeemer depends on the final transaction structure (e.g., for
      * self-referential scripts that need to know input/output indices). The redeemer is computed
      * after the transaction is assembled but before script evaluation.
      *
      * @param utxo
      *   the UTXO to spend
      * @param redeemerBuilder
      *   function that computes the redeemer from the assembled transaction
      * @param script
      *   the Plutus script that protects the UTXO
      */
    def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data,
        script: PlutusScript
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemerBuilder = redeemerBuilder,
          datum = datum,
          additionalSigners = Set.empty
        )
        spend(utxo, witness)
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set.
      *
      * If the specified `script` fails with the specified redeemer, [[build]] is going to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to the `changeTo` or `diffHandler` parameter of [[build]].
      *
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param script
      *   script that protects the `utxo`
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        script: PlutusScript
    ): TxBuilder = {
        spend(utxo, redeemer, script, Set.empty)
    }

    /** Adds the specified script-protected utxo to the list of inputs and the specified redeemer to
      * the witness set, with additional required signers.
      *
      * Use this method when the validator script requires specific signatures beyond the spender.
      * The public key hashes in `additionalSigners` will be added to the transaction's required
      * signers field.
      *
      * If the specified `script` fails with the specified redeemer, [[build]] is going to throw.
      *
      * If the sum of outputs exceeds the sum of spent inputs, the change is going to be handled
      * according to the `changeTo` or `diffHandler` parameter of [[build]].
      *
      * @param utxo
      *   utxo to spend
      * @param redeemer
      *   redeemer to pass to the script to unlock the inputs
      * @param script
      *   script that protects the `utxo`
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      */
    def spend[T: ToData](
        utxo: Utxo,
        redeemer: T,
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash]
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemer = redeemer.toData,
          datum = datum,
          additionalSigners = requiredSigners.map(ExpectedSigner.apply)
        )
        addSteps(TransactionBuilderStep.Spend(utxo, witness))
    }

    /** Adds the specified utxos to the list of reference inputs.
      *
      * Reference inputs allow scripts to read UTXOs without consuming them.
      */
    def references(utxo: Utxo, rest: Utxo*): TxBuilder =
        addSteps((utxo :: rest.toList).map(TransactionBuilderStep.ReferenceOutput.apply)*)

    /** Adds the specified utxos to the list of collateral inputs.
      *
      * Collateral inputs are used to cover transaction fees if script execution fails. They are
      * only consumed if a script fails validation.
      */
    def collaterals(utxo: Utxo, rest: Utxo*): TxBuilder =
        addSteps((utxo :: rest.toList).map(TransactionBuilderStep.AddCollateral.apply)*)

    /** Adds the specified utxos to the list of collateral inputs.
      *
      * Collateral inputs are used to cover transaction fees if script execution fails. They are
      * only consumed if a script fails validation.
      */
    def collaterals(utxos: Utxos): TxBuilder =
        addSteps(
          utxos.view.map(utxo => TransactionBuilderStep.AddCollateral(Utxo(utxo))).toSeq*
        )

    /** Adds the specified output to the list of transaction outputs.
      *
      * Use this method for fine-grained control over output construction. For simpler cases, use
      * [[payTo]].
      *
      * @param output
      *   the transaction output to add
      */
    def output(output: TransactionOutput): TxBuilder =
        addSteps(TransactionBuilderStep.Send(output))

    /** Sends the specified value to the given address without a datum.
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      */
    def payTo(address: Address, value: Value): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, None, None)
          )
        )

    /** Sends the specified value to the given address with an inline datum.
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      * @param datum
      *   inline datum to attach to the output
      */
    def payTo[T: ToData](address: Address, value: Value, datum: T): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Inline(datum.toData)), None)
          )
        )

    /** Sends the specified value to the given address with a datum hash.
      *
      * Make sure to call [[attach]] with the corresponding datum data before calling [[build]].
      *
      * @param address
      *   recipient address
      * @param value
      *   amount to send
      * @param datumHash
      *   hash of the datum (the actual datum must be attached via [[attach]])
      */
    def payTo(address: Address, value: Value, datumHash: DataHash): TxBuilder =
        addSteps(
          TransactionBuilderStep.Send(
            TransactionOutput(address, value, Some(DatumOption.Hash(datumHash)), None)
          )
        )

    /** Attaches a datum to the transaction witness set.
      *
      * Use this method when spending UTXOs that have datum hashes instead of inline datums. The
      * datum hash is computed automatically and the datum is stored for inclusion in the witness
      * set.
      *
      * @param datum
      *   the datum data to attach
      */
    def attach(datum: Data): TxBuilder = {
        val dataHash = DataHash.fromByteString(blake2b_256(serialiseData(datum)))
        copy(attachedData = attachedData + (dataHash -> datum))
    }

    /** Adds transaction metadata (auxiliary data).
      *
      * Metadata is optional data attached to a transaction that does not affect validation but can
      * be used for off-chain purposes.
      *
      * @param auxiliaryData
      *   the auxiliary data to attach
      */
    def metadata(auxiliaryData: AuxiliaryData): TxBuilder =
        addSteps(TransactionBuilderStep.ModifyAuxiliaryData(_ => Some(auxiliaryData)))

    /** Mints or burns native tokens under a minting policy.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens. The minting policy
      * script must be present as a reference input (via [[references]]) for this to work.
      *
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param policyId
      *   the policy ID (script hash) of the minting policy
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        redeemer: T,
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {
        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptAttached,
                redeemer = redeemer.toData,
                additionalSigners = requiredSigners.map(ExpectedSigner.apply)
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    /** Mints or burns native tokens with the script provided inline.
      *
      * This is a convenience method for when you have the minting policy script available directly.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens.
      *
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param script
      *   the minting policy script
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mintAndAttach[T: ToData](
        redeemer: T,
        assets: collection.Map[AssetName, Long],
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash] = Set.empty
    ): TxBuilder = {

        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = script.scriptHash,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptValue(script),
                redeemer = redeemer.toData,
                additionalSigners = requiredSigners.map(ExpectedSigner.apply)
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    /** Registers a stake key with the network.
      *
      * Note that the deposit is still going to be deducted, and it's going to be calculated
      * according to the protocol parameters from [[env]]/
      */
    def registerStake(stakeAddress: StakeAddress): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        // Use None for Shelley-era registration (deposit from protocol params)
        val cert = Certificate.RegCert(credential, None)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a stake key with an explicit deposit amount. */
    def registerStake(stakeAddress: StakeAddress, deposit: Coin): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.RegCert(credential, Some(deposit))
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Deregisters a stake key from the network. */
    def deregisterStake(stakeAddress: StakeAddress): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        // Use None for Shelley-era deregistration
        val cert = Certificate.UnregCert(credential, None)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Deregisters a stake key with an explicit deposit refund amount. */
    def deregisterStake(stakeAddress: StakeAddress, refund: Coin): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.UnregCert(credential, Some(refund))
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Delegates a stake key to the specified stake pool. */
    def delegateTo(stakeAddress: StakeAddress, poolId: PoolKeyHash): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.StakeDelegation(credential, poolId)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a stake key and delegates to a stake pool in a single transaction. */
    def stakeAndDelegate(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        deposit: Coin
    ): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.StakeRegDelegCert(credential, poolId, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Withdraws staking rewards. */
    def withdrawRewards(stakeAddress: StakeAddress, amount: Coin): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        addSteps(
          TransactionBuilderStep.WithdrawRewards(
            StakeCredential(credential),
            amount,
            PubKeyWitness
          )
        )
    }

    /** Delegates voting power to a DRep. */
    def delegateVoteToDRep(stakeAddress: StakeAddress, drep: DRep): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.VoteDelegCert(credential, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers stake address and delegates voting power to a DRep in one transaction. */
    def registerAndDelegateVoteToDRep(
        stakeAddress: StakeAddress,
        drep: DRep,
        deposit: Coin
    ): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.VoteRegDelegCert(credential, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Delegates to both a stake pool and a DRep. */
    def delegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep
    ): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.StakeVoteDelegCert(credential, poolId, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers stake address and delegates to both stake pool and DRep in one transaction. */
    def registerAndDelegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep,
        deposit: Coin
    ): TxBuilder = {
        val credential = stakeAddressToCredential(stakeAddress)
        val cert = Certificate.StakeVoteRegDelegCert(credential, poolId, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers as a DRep. */
    def registerDRep(
        drepCredential: Credential,
        deposit: Coin,
        anchor: Option[Anchor]
    ): TxBuilder = {
        val cert = Certificate.RegDRepCert(drepCredential, deposit, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Unregisters as a DRep. */
    def unregisterDRep(drepCredential: Credential, deposit: Coin): TxBuilder = {
        val cert = Certificate.UnregDRepCert(drepCredential, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Updates DRep metadata anchor. */
    def updateDRep(drepCredential: Credential, anchor: Option[Anchor]): TxBuilder = {
        val cert = Certificate.UpdateDRepCert(drepCredential, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Sets a minimum fee for the transaction.
      *
      * This overrides the automatically calculated fee. Use with caution; the actual fee may need
      * to be higher to cover script execution costs.
      *
      * @param minFee
      *   the minimum fee in lovelace
      */
    def minFee(minFee: Coin): TxBuilder =
        addSteps(TransactionBuilderStep.Fee(minFee))

    /** Sets the transaction validity interval using slot numbers.
      *
      * The transaction is only valid for inclusion in a block within the specified slot range. This
      * is important for time-sensitive contracts.
      *
      * @param interval
      *   the validity interval specifying the valid slot range
      */
    def validDuring(interval: ValidityInterval): TxBuilder = {
        val steps = Seq(
          interval.invalidBefore.map(TransactionBuilderStep.ValidityStartSlot.apply),
          interval.invalidHereafter.map(TransactionBuilderStep.ValidityEndSlot.apply)
        ).flatten
        addSteps(steps*)
    }

    /** Sets the earliest time from which the transaction is valid.
      *
      * The timestamp is converted to a slot number using the environment's slot configuration.
      *
      * @param from
      *   the earliest valid time for the transaction
      */
    def validFrom(from: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(from.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityStartSlot(slot))
    }

    /** Sets the latest time until which the transaction is valid.
      *
      * The timestamp is converted to a slot number using the environment's slot configuration. The
      * transaction becomes invalid after this time.
      *
      * @param to
      *   the latest valid time for the transaction (exclusive)
      */
    def validTo(to: Instant): TxBuilder = {
        val slot = env.slotConfig.timeToSlot(to.toEpochMilli)
        addSteps(TransactionBuilderStep.ValidityEndSlot(slot))
    }

    /** Builds and finalizes the transaction.
      *
      * This method assembles the transaction from all the accumulated steps, calculates fees,
      * handles change, validates and runs all Plutus scripts, and produces a ready-to-sign
      * transaction.
      *
      * @param diffHandler
      *   the handler for managing transaction balance differences (change)
      * @return
      *   a new TxBuilder with the finalized transaction
      * @throws RuntimeException
      *   if script execution fails or if the transaction cannot be balanced
      */
    def build(diffHandler: DiffHandler): TxBuilder = {
        val network = env.network
        val params = env.protocolParams
        // Could be a good idea to immediately `modify` on every step, maybe not tho.
        val finalizedContext = for {
            built <- TransactionBuilder.modify(context, steps)
            withAttachments = addAttachmentsToContext(
              built
            ) // TODO: remove after fixes with attachments
            finalized <- withAttachments.finalizeContext(
              params,
              diffHandler,
              evaluator,
              validators
            )
        } yield finalized

        finalizedContext match {
            case Right(finalized) =>
                copy(context = finalized)
            case Left(error) =>
                throw new RuntimeException(error.reason)
        }
    }

    /** Builds and finalizes the transaction, sending any remaining value to the specified change
      * address.
      *
      * This is a convenience method that uses the default change handling strategy. Any difference
      * between inputs and outputs (minus fees) will be sent to the provided address.
      *
      * @param changeTo
      *   the address to receive any remaining value (change)
      * @return
      *   a new TxBuilder with the finalized transaction
      * @throws RuntimeException
      *   if script execution fails or if the transaction cannot be balanced
      */
    def build(changeTo: Address): TxBuilder = {
        build(diffHandler =
            (diff, tx) => Change.handleChange(diff, tx, changeTo, env.protocolParams)
        )
    }

    /** Signs the transaction with the provided signer.
      *
      * This method should be called after [[build]] to add signatures to the transaction. Multiple
      * signers can be applied by chaining sign calls.
      *
      * @param signer
      *   the transaction signer
      * @return
      *   a new TxBuilder with the signed transaction
      */
    def sign(signer: TransactionSigner): TxBuilder = {
        val tx = context.transaction
        val signedTx = signer.sign(transaction)
        copy(context = context.copy(transaction = signedTx))
    }

    /** Returns the current transaction.
      *
      * Call this after [[build]] and optionally [[sign]] to get the final transaction ready for
      * submission.
      */
    def transaction: Transaction = context.transaction

    /** Automatically completes the transaction by selecting UTXOs and collateral.
      *
      * This method queries the provider for available UTXOs at the sponsor address, selects inputs
      * to cover all outputs and fees, selects collateral if needed (for script transactions), and
      * sets up change handling to the sponsor address.
      *
      * After calling this method, you should call [[build]] to finalize the transaction.
      *
      * @param provider
      *   the provider to query for UTXOs
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      * @return
      *   a new TxBuilder with additional spend and collateral steps
      * @throws RuntimeException
      *   if insufficient UTXOs are available to cover the transaction requirements
      */
    def complete(provider: Provider, sponsor: Address): TxBuilder = {
        import scala.concurrent.ExecutionContext.Implicits.global
        Await.result(completeAsync(provider.toAsync, sponsor), Duration.Inf)
    }

    /** Asynchronously completes the transaction by selecting UTXOs, adding collateral, and
      * balancing.
      *
      * This is the async version of [[complete]]. Use this when working with async providers or
      * when you need non-blocking behavior.
      */
    def completeAsync(provider: AsyncProvider, sponsor: Address)(using
        ExecutionContext
    ): Future[TxBuilder] = {
        // Pre-build the context to get initial UTXOs
        val initialContext = TransactionBuilder
            .build(env.network, steps)
            .getOrElse(throw new RuntimeException("Failed to build initial context"))
            .ensureMinAdaAll(env.protocolParams)

        // Keep track of all known UTXOs (initial + selected during balancing)
        val selectedUtxos = mutable.Map.from(initialContext.resolvedUtxos.utxos)

        // Count expected signers: initial context + 1 for sponsor (if pubkey address)
        val sponsorExpectedSigner = sponsor match {
            case sa: ShelleyAddress =>
                sa.payment match {
                    case ShelleyPaymentPart.Key(hash) => Some(ExpectedSigner(hash))
                    case _                            => None
                }
            case _ => None
        }
        val allExpectedSigners = initialContext.expectedSigners ++ sponsorExpectedSigner.toSet
        val expectedSignerCount = allExpectedSigners.size

        // The entire diff handler is launched in a loop, and every return Right() is just an end of an iteration,
        // not necessarily the end of the algorithm.
        def handleDiffByQueryingMore(
            diff: Value,
            tx: Transaction
        ): Either[TxBalancingError, Transaction] = {
            // First, ensure collateral is balanced (happens every iteration since fee changes)
            val collateralResult = Await.result(
              asyncEnsureCollateralBalanced(
                tx,
                provider,
                sponsor,
                selectedUtxos
              ),
              Duration.Inf
            )

            collateralResult match {
                case Right(balancedTx) if balancedTx != tx =>
                    // Collateral was adjusted, iterate further
                    return Right(balancedTx)
                case Left(error) =>
                    return Left(error)
                case Right(_) => // Collateral is balanced, continue with value diff handling
            }

            // Collateral balanced -- now we can compute and process the Value diff
            val Value(adaDiff, tokenDiff) = diff
            (adaDiff, tokenDiff) match {
                // We don't have enough ADA
                case (NegativeAda(_), _) =>
                    // We don't have enough ADA, but we may have added a change output on past iterations.
                    // If so, try to take ADA from it to achieve balance.
                    val changeIndex = Change.findChangeOutput(tx, sponsor)
                    if changeIndex >= 0 then {
                        Change.handleChange(diff, tx, sponsor, env.protocolParams) match {
                            case Right(updatedTx) =>
                                // Successfully took from change
                                Right(updatedTx)
                            case Left(_) =>
                                // Can't take from change (would go below minAda), need to query for more UTXOs
                                val additionalUtxos = Await.result(
                                  asyncSelectAdditionalUtxos(
                                    provider,
                                    sponsor,
                                    diff,
                                    excludeInputs = selectedUtxos.inputSet
                                  ),
                                  Duration.Inf
                                )
                                selectedUtxos.addAll(additionalUtxos)
                                val withMoreInputs = addInputs(tx, additionalUtxos)
                                Right(withMoreInputs)
                        }
                    } else {
                        // There's no change output, the only way to get more ADA is to query the provider.
                        val additionalUtxos = Await.result(
                          asyncSelectAdditionalUtxos(
                            provider,
                            sponsor,
                            diff,
                            excludeInputs = selectedUtxos.inputSet
                          ),
                          Duration.Inf
                        )
                        // Remember the utxos to not select them again on further iterations.
                        selectedUtxos.addAll(additionalUtxos)
                        val withMoreInputs = addInputs(tx, additionalUtxos)
                        Right(withMoreInputs)
                    }

                case (ZeroAda(), ZeroTokens()) =>
                    // Balanced perfectly, we're done
                    Right(tx)

                case (ZeroAda(), MissingTokens()) =>
                    // We are missing tokens -- need to query more
                    val additionalUtxos = Await.result(
                      asyncSelectAdditionalUtxos(
                        provider,
                        sponsor,
                        diff,
                        excludeInputs = selectedUtxos.inputSet
                      ),
                      Duration.Inf
                    )
                    // Remember the utxos to not select them again on further iterations.
                    selectedUtxos.addAll(additionalUtxos)
                    val withMoreInputs = addInputs(tx, additionalUtxos)
                    Right(withMoreInputs)

                case (ZeroAda(), _) =>
                    // We are not missing any ADA and we have a token surplus -- send the surplus tokens back as change.
                    Change.handleChange(diff, tx, sponsor, env.protocolParams)

                case (PositiveAda(_), MissingTokens()) =>
                    // Despite having surplus ADA, we still need to query for more tokens, and handle the surplus as change
                    // on further iterations
                    val additionalUtxos = Await.result(
                      asyncSelectAdditionalUtxos(
                        provider,
                        sponsor,
                        diff,
                        excludeInputs = selectedUtxos.inputSet
                      ),
                      Duration.Inf
                    )
                    // Remember the utxos to not select them again on further iterations.
                    selectedUtxos.addAll(additionalUtxos)
                    val withMoreInputs = addInputs(tx, additionalUtxos)
                    Right(withMoreInputs)

                case (PositiveAda(_), _) =>
                    // We either have more tokens than necessary, or exactly the right amount of tokens.
                    // In any case -- send the ADA and potentially some tokens back as change.
                    Change.handleChange(diff, tx, sponsor, env.protocolParams)
            }
        }

        // Build the transaction with balancing, then update context with all selected UTXOs
        // Add dummy signatures for fee calculation (similar to finalizeContext)
        val txWithDummySigs = addDummySignatures(
          expectedSignerCount,
          addAttachmentsToContext(initialContext).transaction
        )
        val balancedTx = LowLevelTxBuilder.balanceFeeAndChangeWithTokens(
          txWithDummySigs,
          handleDiffByQueryingMore,
          env.protocolParams,
          selectedUtxos.utxos,
          evaluator
        )

        Future.successful {
            balancedTx match {
                case Right(tx) =>
                    // Remove dummy signatures before returning
                    val txWithoutDummySigs = removeDummySignatures(expectedSignerCount, tx)
                    // Update context with the balanced transaction and all selected UTXOs
                    val updatedContext = initialContext.copy(
                      transaction = txWithoutDummySigs,
                      resolvedUtxos = ResolvedUtxos(selectedUtxos.utxos),
                      expectedSigners = allExpectedSigners
                    )
                    copy(context = updatedContext)
                case Left(error) =>
                    throw new RuntimeException(s"Failed to balance transaction: $error")
            }
        }
    }

    // FIXME: see `completeAsync`
    private def asyncEnsureCollateralBalanced(
        tx: Transaction,
        provider: AsyncProvider,
        sponsor: Address,
        selectedUtxos: mutable.Map[TransactionInput, TransactionOutput]
    )(using ExecutionContext): Future[Either[Nothing, Transaction]] = {
        val resolvedUtxos = selectedUtxos.utxos
        val utxosToExclude = selectedUtxos.utxos
        // For transactions that don't need collateral -- return early
        val needsCollat = tx.witnessSet.redeemers.fold(false)(_.value.toSeq.nonEmpty)
        if !needsCollat then return Future.successful(Right(tx))

        // If we need collateral, first calculate how much we need vs how much is present
        val currentCollateralAmount = totalCollateralValue(tx, resolvedUtxos)
        val requiredCollateralAmount = CollateralSufficient.calculateRequiredCollateral(
          tx.body.value.fee,
          env.protocolParams.collateralPercentage
        )
        val gap = currentCollateralAmount - Value(requiredCollateralAmount)

        // Zero means no ADA _and_ no tokens -- balanced, we're done
        if gap.isZero then return Future.successful(Right(tx))

        // Note that token gap is always positive -- collateral tokens must always be returned
        val Value(adaGap, tokenGap) = gap

        // If adaGap > min ADA, meaning the amount we need to return is greater than min ADA, we can
        // create a single output = Value(adaGap, tokenGap).
        // Otherwise, we need to query more ADA for collateral input to make sure that the return output is at least min ADA.
        // We also need to make sure that we don't have more than 3 collateral inputs.

        val minAda = MinCoinSizedTransactionOutput.computeMinAda(
          Sized(TransactionOutput(sponsor, gap)),
          env.protocolParams
        )
        if adaGap < minAda then {
            // Need more collateral to meet minAda requirement for return output
            val additionalNeeded = minAda - adaGap
            asyncSelectCollateral(
              provider,
              sponsor,
              additionalNeeded,
              excludeUtxos = utxosToExclude.keySet
            ).map { additionalCollateral =>
                // Remember the utxos to not select them again in the `complete`.
                selectedUtxos.addAll(additionalCollateral)
                val txWithMoreCollateral = addCollaterals(tx, additionalCollateral.keySet)
                Right(txWithMoreCollateral)
            }
        } else {
            val returnOutput = TransactionOutput(sponsor, gap)
            val withReturnOutput = addCollateralReturnOutput(tx, returnOutput, resolvedUtxos)
            Future.successful(Right(withReturnOutput))
        }
    }

    // FIXME: see `completeAsync`
    private def asyncSelectAdditionalUtxos(
        provider: AsyncProvider,
        address: Address,
        gap: Value,
        excludeInputs: Set[TransactionInput]
    )(using ExecutionContext): Future[Utxos] = {
        val selectedUtxos = mutable.Map.empty[TransactionInput, TransactionOutput]

        // Fulfill the token requirement first
        val tokensFuture: Future[Unit] = if gap.assets.assets.nonEmpty then {
            val requiredTokens = MultiAsset(
              gap.assets.assets.map { case (policyId, assets) =>
                  policyId -> SortedMap.from(assets.map { case (assetName, amount) =>
                      assetName -> Math.abs(amount)
                  })
              }
            )
            asyncSelectUtxosWithTokens(
              provider,
              address,
              requiredTokens,
              excludeInputs ++ selectedUtxos.keySet
            ).map { tokenUtxos =>
                selectedUtxos.addAll(tokenUtxos)
            }
        } else {
            Future.successful(())
        }

        // Then cover the insufficient ADA
        tokensFuture.flatMap { _ =>
            if gap.coin.value < 0 then {
                val alreadySelectedAda = selectedUtxos.values.map(_.value.coin.value).sum
                val totalAdaNeeded = -gap.coin.value
                val remainingAdaNeeded = totalAdaNeeded - alreadySelectedAda

                if remainingAdaNeeded > 0 then {
                    provider
                        .findUtxos(address = address)
                        .map { result =>
                            val allUtxos = result
                                .getOrElse(Map.empty)
                                .filterNot { case (input, _) =>
                                    excludeInputs.contains(input) || selectedUtxos.contains(input)
                                }

                            var accumulatedAda = 0L
                            val adaUtxos = allUtxos.takeWhile { case (_, output) =>
                                val shouldTake = accumulatedAda < remainingAdaNeeded
                                if shouldTake then accumulatedAda += output.value.coin.value
                                shouldTake
                            }

                            if accumulatedAda < remainingAdaNeeded then
                                throw new RuntimeException(
                                  s"Insufficient ADA: need $remainingAdaNeeded more lovelace, found only $accumulatedAda"
                                )

                            selectedUtxos.addAll(adaUtxos)
                            selectedUtxos.toMap
                        }
                } else {
                    Future.successful(selectedUtxos.toMap)
                }
            } else {
                Future.successful(selectedUtxos.toMap)
            }
        }
    }

    // FIXME: see `completeAsync`
    private def asyncSelectUtxosWithTokens(
        provider: AsyncProvider,
        address: Address,
        requiredAssets: MultiAsset,
        excludeInputs: Set[TransactionInput]
    )(using ExecutionContext): Future[Map[TransactionInput, TransactionOutput]] = {
        provider
            .findUtxos(address = address)
            .map { result =>
                val allUtxos = result
                    .getOrElse(Map.empty)
                    .filterNot { case (input, _) => excludeInputs.contains(input) }

                var selectedUtxos = Map.empty[TransactionInput, TransactionOutput]

                // For each required token, find UTXOs that contain it
                requiredAssets.assets.foreach { case (policyId, assets) =>
                    assets.foreach { case (assetName, requiredAmount) =>
                        if requiredAmount > 0 then {
                            var collected = 0L

                            // First, count tokens from already-selected UTXOs
                            selectedUtxos.foreach { case (_, output) =>
                                output.value.assets.assets
                                    .get(policyId)
                                    .flatMap(_.get(assetName))
                                    .foreach { amount => collected += amount }
                            }

                            // Then select additional UTXOs if needed
                            if collected < requiredAmount then {
                                allUtxos.foreach { case (input, output) =>
                                    if collected < requiredAmount && !selectedUtxos.contains(input)
                                    then {
                                        output.value.assets.assets
                                            .get(policyId)
                                            .flatMap(_.get(assetName))
                                            .foreach { amount =>
                                                selectedUtxos = selectedUtxos + (input -> output)
                                                collected += amount
                                            }
                                    }
                                }
                            }

                            if collected < requiredAmount then {
                                throw new RuntimeException(
                                  s"Insufficient tokens: need $requiredAmount of ${policyId.toHex}.${assetName.toString}, found only $collected"
                                )
                            }
                        }
                    }
                }

                selectedUtxos
            }
    }

    // FIXME: see `completeAsync`
    private def asyncSelectCollateral(
        provider: AsyncProvider,
        address: Address,
        requiredAmount: Coin,
        excludeUtxos: Set[TransactionInput]
    )(using ExecutionContext): Future[Utxos] = {
        provider
            .findUtxos(
              address = address,
              minRequiredTotalAmount = Some(requiredAmount)
            )
            .map { result =>
                val utxos = result.map(_ -- excludeUtxos)
                utxos match {
                    case Right(u) if u.nonEmpty => u
                    case Right(u) => throw new RuntimeException("Could not find collateral uxtos")
                    case Left(e) => throw new RuntimeException("Could not find collateral utxos", e)
                }
            }
    }

    private def ensureCollateralBalanced(
        tx: Transaction,
        provider: Provider,
        sponsor: Address,
        selectedUtxos: mutable.Map[TransactionInput, TransactionOutput]
    ): Either[Nothing, Transaction] = {
        val resolvedUtxos = selectedUtxos.utxos
        val utxosToExclude = selectedUtxos.utxos
        // For transactions that don't need collateral -- return early
        val needsCollat = tx.witnessSet.redeemers.fold(false)(_.value.toSeq.nonEmpty)
        if !needsCollat then return Right(tx)

        // If we need collateral, first calculate how much we need vs how much is present
        val currentCollateralAmount = totalCollateralValue(tx, resolvedUtxos)
        val requiredCollateralAmount = CollateralSufficient.calculateRequiredCollateral(
          tx.body.value.fee,
          env.protocolParams.collateralPercentage
        )
        val gap = currentCollateralAmount - Value(requiredCollateralAmount)

        // Zero means no ADA _and_ no tokens -- balanced, we're done
        if gap.isZero then return Right(tx)

        // Note that token gap is always positive -- collateral tokens must always be returned
        val Value(adaGap, tokenGap) = gap

        // If adaGap > min ADA, meaning the amount we need to return is greater than min ADA, we can
        // create a single output = Value(adaGap, tokenGap).
        // Otherwise, we need to query more ADA for collateral input to make sure that the return output is at least min ADA.
        // We also need to make sure that we don't have more than 3 collateral inputs.

        val minAda = MinCoinSizedTransactionOutput.ensureMinAda(
          Sized(TransactionOutput(sponsor, gap)),
          env.protocolParams
        )
        if adaGap < minAda then {
            // Need more collateral to meet minAda requirement for return output
            val additionalNeeded = minAda - adaGap
            val additionalCollateral = selectCollateral(
              provider,
              sponsor,
              additionalNeeded,
              excludeUtxos = utxosToExclude.keySet
            )
            // Remember the utxos to not select them again in the `complete`.
            selectedUtxos.addAll(additionalCollateral)
            val txWithMoreCollateral = addCollaterals(tx, additionalCollateral.keySet)
            Right(txWithMoreCollateral)
        } else {
            val returnOutput = TransactionOutput(sponsor, gap)
            val withReturnOutput = addCollateralReturnOutput(tx, returnOutput, resolvedUtxos)
            Right(withReturnOutput)
        }
    }

    private def totalCollateralValue(tx: Transaction, utxos: Utxos): Value = {
        tx.body.value.collateralInputs.toSet.foldLeft(Value.zero) { case (acc, input) =>
            utxos.get(input).fold(acc)(output => acc + output.value)
        }
    }

    /** Adds a collateral return output to the transaction and sets the totalCollateral field.
      *
      * @param tx
      *   the transaction to modify
      * @param returnOutput
      *   the collateral return output to add
      * @param resolvedUtxos
      *   resolved UTXOs to calculate total collateral amount
      * @return
      *   modified transaction with collateral return output and totalCollateral set
      */
    private def addCollateralReturnOutput(
        tx: Transaction,
        returnOutput: TransactionOutput,
        resolvedUtxos: Utxos
    ): Transaction = {
        // Calculate total collateral = sum of collateral inputs - return output value
        val totalInputValue = totalCollateralValue(tx, resolvedUtxos)
        val totalCollateralAmount = totalInputValue.coin - returnOutput.value.coin

        modifyBody(
          tx,
          _.copy(
            collateralReturnOutput = Some(Sized(returnOutput)),
            totalCollateral = Some(totalCollateralAmount)
          )
        )
    }

    private object ZeroAda {
        def unapply(coin: Coin): Boolean = coin == Coin.zero
    }

    private object NegativeAda {
        def unapply(coin: Coin): Option[Coin] = Option.when(coin < Coin.zero)(coin)
    }

    private object PositiveAda {
        def unapply(coin: Coin): Option[Coin] = Option.when(coin > Coin.zero)(coin)
    }

    private object ZeroTokens {
        def unapply(ma: MultiAsset): Boolean = ma.isEmpty
    }

    private object MissingTokens {
        def unapply(ma: MultiAsset): Boolean = ma.exists((_, amount) => amount < 0)
    }

    extension (mutableMap: mutable.Map[TransactionInput, TransactionOutput]) {
        def utxos: Utxos = mutableMap.toMap
        private def inputSet: Set[TransactionInput] = mutableMap.toMap.keySet
    }

    extension (ma: MultiAsset) {
        def exists(f: ((AssetName, Long)) => Boolean): Boolean = {
            ma.assets.values.flatMap(_.toList).exists(f)
        }
    }

    private def selectAdditionalUtxos(
        provider: Provider,
        address: Address,
        gap: Value,
        excludeInputs: Set[TransactionInput]
    ): Utxos = {
        val selectedUtxos = mutable.Map.empty[TransactionInput, TransactionOutput]

        // Fulfill the token requirement first
        // we `abs` all the tokens to represent the _amount_ of tokens that we're querying for.
        if gap.assets.assets.nonEmpty then {
            val requiredTokens = MultiAsset(
              gap.assets.assets.map { case (policyId, assets) =>
                  policyId -> SortedMap.from(assets.map { case (assetName, amount) =>
                      assetName -> Math.abs(amount)
                  })
              }
            )
            val tokenUtxos =
                selectUtxosWithTokens(
                  provider,
                  address,
                  requiredTokens,
                  excludeInputs ++ selectedUtxos.keySet
                )
            selectedUtxos.addAll(tokenUtxos)
        }

        // Then cover the insufficient ADA, which is now less than it initially was because the token utxos
        // have ADA too.
        if gap.coin.value < 0 then {
            val alreadySelectedAda = selectedUtxos.values.map(_.value.coin.value).sum
            // gap.coin.value is negative (deficit), so negate it to get the amount needed
            val totalAdaNeeded = -gap.coin.value
            val remainingAdaNeeded = totalAdaNeeded - alreadySelectedAda

            if remainingAdaNeeded > 0 then {
                // Here, we get all of them, which is bad.
                // Ideally the provider would allow us to exclude utxos.
                val allUtxos = provider
                    .findUtxos(address = address)
                    .getOrElse(Map.empty)
                    .filterNot { case (input, _) =>
                        excludeInputs.contains(input) || selectedUtxos.contains(input)
                    }

                var accumulatedAda = 0L
                val adaUtxos = allUtxos.takeWhile { case (_, output) =>
                    val shouldTake = accumulatedAda < remainingAdaNeeded
                    if shouldTake then accumulatedAda += output.value.coin.value
                    shouldTake
                }

                if accumulatedAda < remainingAdaNeeded then
                    throw new RuntimeException(
                      s"Insufficient ADA: need $remainingAdaNeeded more lovelace, found only $accumulatedAda"
                    )

                selectedUtxos.addAll(adaUtxos)
            }
        }

        selectedUtxos.toMap
    }

    /** Selects UTXOs that contain the required native tokens */
    private def selectUtxosWithTokens(
        provider: Provider,
        address: Address,
        requiredAssets: MultiAsset,
        excludeInputs: Set[TransactionInput]
    ): Map[TransactionInput, TransactionOutput] = {
        // Query all UTXOs: might be expensive, think about a better query
        val allUtxos = provider
            .findUtxos(address = address)
            .getOrElse(Map.empty)
            .filterNot { case (input, _) => excludeInputs.contains(input) }

        var selectedUtxos = Map.empty[TransactionInput, TransactionOutput]

        // For each required token, find UTXOs that contain it
        requiredAssets.assets.foreach { case (policyId, assets) =>
            assets.foreach { case (assetName, requiredAmount) =>
                if requiredAmount > 0 then {
                    var collected = 0L

                    // First, count tokens from already-selected UTXOs
                    selectedUtxos.foreach { case (_, output) =>
                        output.value.assets.assets
                            .get(policyId)
                            .flatMap(_.get(assetName))
                            .foreach { amount => collected += amount }
                    }

                    // Then select additional UTXOs if needed
                    if collected < requiredAmount then {
                        allUtxos.foreach { case (input, output) =>
                            if collected < requiredAmount && !selectedUtxos.contains(input) then {
                                output.value.assets.assets
                                    .get(policyId)
                                    .flatMap(_.get(assetName))
                                    .foreach { amount =>
                                        selectedUtxos = selectedUtxos + (input -> output)
                                        collected += amount
                                    }
                            }
                        }
                    }

                    if collected < requiredAmount then {
                        throw new RuntimeException(
                          s"Insufficient tokens: need $requiredAmount of ${policyId.toHex}.${assetName.toString}, found only $collected"
                        )
                    }
                }
            }
        }

        selectedUtxos
    }

    private def selectCollateral(
        provider: Provider,
        address: Address,
        requiredAmount: Coin,
        excludeUtxos: Set[TransactionInput]
    ): Utxos = {
        val utxos = provider
            .findUtxos(
              address = address,
              minRequiredTotalAmount = Some(requiredAmount)
            )
            .map(_ -- excludeUtxos)
        utxos match {
            case Right(u) if u.nonEmpty => u
            case Right(u) => throw new RuntimeException("Could not find collateral uxtos")
            case Left(e)  => throw new RuntimeException("Could not find collateral utxos", e)
        }

    }

    /** Adds collateral inputs to a transaction. */
    private def addCollaterals(tx: Transaction, collaterals: Set[TransactionInput]): Transaction = {
        val currentCollaterals = tx.body.value.collateralInputs.toSet
        val newCollaterals = currentCollaterals ++ collaterals
        modifyBody(
          tx,
          _.copy(collateralInputs = TaggedSortedSet.from(newCollaterals))
        )
    }

    /** Adds inputs to a transaction. */
    private def addInputs(tx: Transaction, inputs: Utxos): Transaction = {
        val currentInputs = tx.body.value.inputs.toSet
        val newInputs = currentInputs ++ inputs.keySet
        modifyBody(
          tx,
          _.copy(inputs = TaggedSortedSet.from(newInputs))
        )
    }

    private def buildDatumWitness(utxo: Utxo): Datum = {
        utxo.output.datumOption match {
            case None                        => Datum.DatumInlined
            case Some(DatumOption.Inline(_)) => Datum.DatumInlined
            case Some(DatumOption.Hash(datumHash)) =>
                attachedData
                    .get(datumHash)
                    .map(Datum.DatumValue.apply)
                    .getOrElse(Datum.DatumInlined)
        }
    }

    private def addAttachmentsToContext(
        ctx: TransactionBuilder.Context
    ): TransactionBuilder.Context = {
        if attachedData.isEmpty then return ctx

        var updatedTx = ctx.transaction
        val currentData = updatedTx.witnessSet.plutusData.value.toMap.values.toSeq
        val allData = currentData ++ attachedData.values.map(KeepRaw(_))
        updatedTx = updatedTx.copy(
          witnessSet = updatedTx.witnessSet.copy(
            plutusData = KeepRaw(TaggedSortedMap(allData*))
          )
        )
        ctx.copy(transaction = updatedTx)
    }

    /** Appends transaction building steps to this builder.
      *
      * This is the low-level method for adding steps. Prefer using the high-level methods like
      * [[spend]], [[payTo]], [[mint]], etc. unless you need direct step control.
      *
      * @param s
      *   the steps to append
      */
    def addSteps(s: TransactionBuilderStep*): TxBuilder = copy(steps = steps ++ s)

    private def stakeAddressToCredential(stakeAddress: StakeAddress): Credential = {
        stakeAddress.payload match {
            case StakePayload.Stake(stakeKeyHash) =>
                Credential.KeyHash(stakeKeyHash.asInstanceOf[AddrKeyHash])
            case StakePayload.Script(scriptHash) => Credential.ScriptHash(scriptHash)
        }
    }
}

/** Factory methods for creating TxBuilder instances. */
object TxBuilder {

    /** Creates a TxBuilder with a custom Plutus script evaluator.
      *
      * Use this method when you need fine-grained control over script evaluation behavior.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      * @param evaluator
      *   the custom Plutus script evaluator
      */
    def apply(
        env: Environment,
        evaluator: PlutusScriptEvaluator
    ): TxBuilder = {
        val context = TransactionBuilder.Context.empty(env.network)
        TxBuilder(env, context, evaluator)
    }

    /** Creates a TxBuilder with the default Plutus script evaluator.
      *
      * The evaluator will both validate scripts and compute execution costs for fee calculation.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      */
    def apply(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator(
          CardanoInfo(env.protocolParams, env.network, env.slotConfig),
          EvaluatorMode.EvaluateAndComputeCost
        )
        apply(env, evaluator)
    }

    /** Creates a TxBuilder with an evaluator that uses constant maximum budget.
      *
      * This is useful for testing or when you want to skip actual script execution cost
      * calculation. The evaluator will still validate scripts but use a fixed cost.
      *
      * @param env
      *   the environment containing protocol parameters, network info, and slot configuration
      */
    def withConstMaxBudgetEvaluator(env: Environment): TxBuilder = {
        val evaluator = PlutusScriptEvaluator.constMaxBudget(env)
        apply(env, evaluator)
    }
}
