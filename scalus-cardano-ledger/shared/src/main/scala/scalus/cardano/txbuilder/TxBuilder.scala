package scalus.cardano.txbuilder

import scalus.uplc.builtin.Builtins.{blake2b_256, serialiseData}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{Data, ToData}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.SomeBuildError

import java.time.Instant
import scala.concurrent.Future

// -----------------------------------------------------------------------------
// TxBuilderException Hierarchy
// -----------------------------------------------------------------------------

/** Base exception for all TxBuilder errors.
  *
  * This sealed hierarchy provides typed exceptions for different failure modes in TxBuilder,
  * enabling programmatic error handling while remaining backwards compatible with RuntimeException.
  */
sealed abstract class TxBuilderException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

object TxBuilderException {

    // -------------------------------------------------------------------------
    // Build Phase Errors - wraps SomeBuildError from TransactionBuilder
    // -------------------------------------------------------------------------

    /** Error during transaction building step processing. */
    final case class BuildStepException(
        stepError: StepError,
        context: TransactionBuilder.Context
    ) extends TxBuilderException(s"Build step error: ${stepError.explain}") {
        def step: TransactionBuilderStep = stepError.step
    }

    /** Error during redeemer indexing. */
    final case class RedeemerIndexingException(
        error: RedeemerIndexingInternalError,
        context: TransactionBuilder.Context
    ) extends TxBuilderException(s"Redeemer indexing error: $error")

    /** Error during transaction balancing. */
    final case class BalancingException(
        error: TxBalancingError,
        context: TransactionBuilder.Context
    ) extends TxBuilderException(
          error match {
              case TxBalancingError.EvaluationFailed(cause) =>
                  s"Plutus script evaluation failed: ${cause.getMessage}"
              case TxBalancingError.Failed(cause) =>
                  s"Balancing failed: ${cause.getMessage}"
              case TxBalancingError.BalanceDidNotConverge(iterations) =>
                  s"Balancing did not converge after $iterations iterations"
              case TxBalancingError.InsufficientFunds(valueDiff, minRequired) =>
                  val tokenInfo =
                      if valueDiff.assets.nonEmpty then s", tokens=${valueDiff.assets}" else ""
                  s"Insufficient funds: need $minRequired more lovelace, adaDiff=${valueDiff.coin.value}$tokenInfo"
              case TxBalancingError.InsufficientCollateralForReturn(totalAda, required, minAda) =>
                  s"Collateral contains tokens but insufficient ADA for return output. " +
                      s"Total: ${totalAda.value}, required: ${required.value}, minAda: ${minAda.value}"
          },
          error match {
              case TxBalancingError.EvaluationFailed(cause) => cause
              case TxBalancingError.Failed(cause)           => cause
              case _                                        => null
          }
        ) {

        /** Returns true if this error was caused by a Plutus script failure. */
        def isScriptFailure: Boolean = error match {
            case TxBalancingError.EvaluationFailed(_) => true
            case _                                    => false
        }

        /** Returns script execution logs if this was a script failure. */
        def scriptLogs: Option[Seq[String]] = error match {
            case TxBalancingError.EvaluationFailed(cause) => Some(cause.logs.toSeq)
            case _                                        => None
        }
    }

    /** Ledger validation error. */
    final case class LedgerValidationException(
        error: TransactionException,
        context: TransactionBuilder.Context
    ) extends TxBuilderException(s"Validation error: ${error.explain}", error)

    // -------------------------------------------------------------------------
    // Selection Phase Errors - TxBuilder-specific errors
    // -------------------------------------------------------------------------

    /** Insufficient ADA for transaction completion. */
    final case class InsufficientAdaException(
        required: Coin,
        available: Coin,
        sponsorAddress: Address
    ) extends TxBuilderException(
          s"Insufficient ADA at $sponsorAddress: need ${required.value} lovelace, found only ${available.value}"
        )

    /** Insufficient tokens for transaction completion. */
    final case class InsufficientTokensException(
        policyId: PolicyId,
        assetName: AssetName,
        required: Long,
        available: Long,
        sponsorAddress: Address
    ) extends TxBuilderException(
          s"Insufficient tokens at $sponsorAddress: need $required of ${policyId.toHex}.${assetName.toString}, found only $available"
        )

    /** Converts a SomeBuildError to the appropriate TxBuilderException. */
    def fromBuildError(error: SomeBuildError): TxBuilderException = error match {
        case SomeBuildError.SomeStepError(e, ctx)             => BuildStepException(e, ctx)
        case SomeBuildError.SomeRedeemerIndexingError(e, ctx) => RedeemerIndexingException(e, ctx)
        case SomeBuildError.BalancingError(e, ctx)            => BalancingException(e, ctx)
        case SomeBuildError.ValidationError(e, ctx)           => LedgerValidationException(e, ctx)
    }
}

/** A high-level, fluent API for building Cardano transactions.
  *
  * TxBuilder provides a convenient way to construct transactions by chaining method calls. It
  * handles input selection, output creation, script attachment, minting, and transaction
  * finalization including fee calculation and change handling.
  *
  * TxBuilder is purely functional; each method returns a new instance with the updated state. The
  * `complete` method is the only one that performs effects (querying UTXOs from a provider).
  *
  * ==Platform Support==
  *
  * TxBuilder supports both JVM and JavaScript platforms:
  *   - `complete(provider, sponsor)`: Returns `Future[TxBuilder]`. Available on all platforms.
  *   - `complete(provider, sponsor, timeout)`: JVM-only blocking version with timeout parameter.
  *
  * On JVM, you can also use the `await` extension method on Future:
  * {{{
  * import scalus.cardano.txbuilder.await
  * builder.complete(provider, sponsor).await()
  * }}}
  *
  * ==Usage==
  * {{{
  * // Cross-platform - async (returns Future)
  * val txFuture = TxBuilder(env)
  *   .payTo(recipientAddress, Value.ada(10))
  *   .complete(provider, sponsorAddress)
  *   .map(_.sign(signer).transaction)
  *
  * // JVM-only - blocking with timeout
  * import scala.concurrent.duration.*
  * val tx = TxBuilder(env)
  *   .payTo(recipientAddress, Value.ada(10))
  *   .complete(provider, sponsorAddress, 30.seconds)
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
  * @param steps
  *   accumulated transaction building steps
  * @param attachedData
  *   datum values to be included in the transaction witness set
  * @param changeOutputIndex
  *   index of the explicit change output (set by [[changeTo]]), or None for implicit change
  */
case class TxBuilder(
    env: CardanoInfo,
    context: TransactionBuilder.Context,
    evaluator: PlutusScriptEvaluator,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    attachedData: Map[DataHash, Data] = Map.empty,
    changeOutputIndex: Option[Int] = None
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
        spend(utxo, redeemerBuilder, script, Set.empty)
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
      * @param requiredSigners
      *   * set of public key hashes that must sign the transaction
      */
    def spend(
        utxo: Utxo,
        redeemerBuilder: Transaction => Data,
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash]
    ): TxBuilder = {
        val datum = buildDatumWitness(utxo)

        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(script),
          redeemerBuilder = redeemerBuilder,
          datum = datum,
          additionalSigners = requiredSigners.map(ExpectedSigner.apply)
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
            TransactionOutput(address, value, datum.toData)
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

    /** Specifies an output that will receive change during transaction balancing.
      *
      * The output specifies the MINIMUM amount it must contain. During balancing, any excess ADA
      * and tokens will be added to this output. The final output will always have at least the
      * specified value, plus any change from the transaction.
      *
      * This method can only be called once per transaction. If not called, a new change output will
      * be created at the sponsor address during [[complete]].
      *
      * The change output must meet the minimum ADA requirement (minUTxO). If the transaction
      * doesn't have enough excess to cover this minimum, additional UTXOs will be selected
      * automatically.
      *
      * @param output
      *   the output that will receive change (specifies minimum amounts)
      * @throws IllegalArgumentException
      *   if called more than once
      */
    def changeTo(output: TransactionOutput): TxBuilder = {
        require(
          changeOutputIndex.isEmpty,
          s"changeTo can only be called once (already set at index ${changeOutputIndex.get})"
        )
        copy(changeOutputIndex = Some(sendStepCount))
            .addSteps(TransactionBuilderStep.Send(output))
    }

    /** Counts the number of Send steps, which corresponds to the output index. */
    private def sendStepCount: Int = steps.count(_.isInstanceOf[TransactionBuilderStep.Send])

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

    /** Mints or burns native tokens under a minting policy (reference script).
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens. The minting policy
      * script must be present as a reference input (via [[references]]) for this to work.
      *
      * @param policyId
      *   the policy ID (script hash) of the minting policy
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        redeemer: T
    ): TxBuilder = mint(policyId, assets, redeemer, Set.empty[AddrKeyHash])

    /** Mints or burns native tokens under a minting policy with required signers (reference
      * script).
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens. The minting policy
      * script must be present as a reference input (via [[references]]) for this to work.
      *
      * @param policyId
      *   the policy ID (script hash) of the minting policy
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        redeemer: T,
        requiredSigners: Set[AddrKeyHash]
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

    // -------------------------------------------------------------------------
    // Minting API - Attached script variants (script first, policyId derived)
    // -------------------------------------------------------------------------

    /** Mints or burns native tokens with the script provided inline.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens.
      *
      * @param script
      *   the minting policy script
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        script: PlutusScript,
        assets: collection.Map[AssetName, Long],
        redeemer: T
    ): TxBuilder = mint(script, assets, redeemer, Set.empty[AddrKeyHash])

    /** Mints or burns native tokens with the script provided inline and required signers.
      *
      * Use positive amounts to mint tokens and negative amounts to burn tokens.
      *
      * @param script
      *   the minting policy script
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemer
      *   redeemer to pass to the minting policy script
      * @param requiredSigners
      *   set of public key hashes that must sign the transaction
      * @tparam T
      *   type of the redeemer (must have a ToData instance)
      */
    def mint[T: ToData](
        script: PlutusScript,
        assets: collection.Map[AssetName, Long],
        redeemer: T,
        requiredSigners: Set[AddrKeyHash]
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

    /** Mints or burns native tokens with a delayed redeemer and attached script.
      *
      * Use this method when the redeemer depends on the final transaction structure (e.g., for
      * self-referential scripts). The redeemer is computed after the transaction is assembled but
      * before script evaluation.
      *
      * @param script
      *   the minting policy script
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemerBuilder
      *   function that computes the redeemer from the assembled transaction
      */
    def mint(
        script: PlutusScript,
        assets: collection.Map[AssetName, Long],
        redeemerBuilder: Transaction => Data
    ): TxBuilder = {
        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = script.scriptHash,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptValue(script),
                redeemerBuilder = redeemerBuilder,
                additionalSigners = Set.empty
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    // -------------------------------------------------------------------------
    // Minting API - Reference script variants (delayed redeemer)
    // -------------------------------------------------------------------------

    /** Mints or burns native tokens with a delayed redeemer computed from the built transaction.
      *
      * Use this method when the redeemer depends on the final transaction structure (e.g., for
      * self-referential scripts). The redeemer is computed after the transaction is assembled but
      * before script evaluation.
      *
      * The minting policy script must be present as a reference input (via [[references]]).
      *
      * @param policyId
      *   the policy ID (script hash) of the minting policy
      * @param assets
      *   map of asset names to amounts (positive for minting, negative for burning)
      * @param redeemerBuilder
      *   function that computes the redeemer from the assembled transaction
      */
    def mint(
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        redeemerBuilder: Transaction => Data
    ): TxBuilder = {
        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = amount,
              witness = TwoArgumentPlutusScriptWitness(
                scriptSource = ScriptSource.PlutusScriptAttached,
                redeemerBuilder = redeemerBuilder,
                additionalSigners = Set.empty
              )
            )
        }.toSeq

        addSteps(mintSteps*)
    }

    // -------------------------------------------------------------------------
    // Deprecated Minting API
    // -------------------------------------------------------------------------

    /** @deprecated Use mint(script, assets, redeemer) instead */
    @deprecated("Use mint(script, assets, redeemer) instead", "0.13.0")
    def mintAndAttach[T: ToData](
        redeemer: T,
        assets: collection.Map[AssetName, Long],
        script: PlutusScript
    ): TxBuilder = mint(script, assets, redeemer, Set.empty[AddrKeyHash])

    /** @deprecated Use mint(script, assets, redeemer, requiredSigners) instead */
    @deprecated("Use mint(script, assets, redeemer, requiredSigners) instead", "0.13.0")
    def mintAndAttach[T: ToData](
        redeemer: T,
        assets: collection.Map[AssetName, Long],
        script: PlutusScript,
        requiredSigners: Set[AddrKeyHash]
    ): TxBuilder = mint(script, assets, redeemer, requiredSigners)

    /** Mints or burns native tokens using a script witness.
      *
      * This is the unified API for minting operations. Use the factory methods to create the
      * witness:
      * {{{
      * // Plutus script with attached script (included in transaction)
      * import TwoArgumentPlutusScriptWitness.*
      * builder.mint(policyId, assets, attached(script, redeemer))
      *
      * // Plutus script with reference script (must be in a reference input)
      * builder.references(scriptRefUtxo)
      *        .mint(policyId, assets, reference(redeemer))
      *
      * // Plutus script with delayed redeemer (computed from final transaction)
      * builder.mint(policyId, assets, attached(script, tx => computeRedeemer(tx)))
      *
      * // Native script
      * import NativeScriptWitness.*
      * builder.mint(policyId, assets, attached(nativeScript))
      * }}}
      *
      * @param policyId
      *   the minting policy ID (script hash)
      * @param assets
      *   map of asset names to amounts (positive to mint, negative to burn)
      * @param witness
      *   the script witness authorizing the mint (NativeScriptWitness or
      *   TwoArgumentPlutusScriptWitness)
      */
    def mint(
        policyId: PolicyId,
        assets: collection.Map[AssetName, Long],
        witness: ScriptWitness
    ): TxBuilder = {
        val mintSteps = assets.map { case (assetName, amount) =>
            TransactionBuilderStep.Mint(
              scriptHash = policyId,
              assetName = assetName,
              amount = amount,
              witness = witness
            )
        }
        addSteps(mintSteps.toSeq*)
    }

    /** Registers a stake key with the network.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      */
    def registerStake(stakeAddress: StakeAddress): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.RegCert(credential, None)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a script-based stake key with the network.
      *
      * Post-Conway, script-based registrations require authorization. Use the factory methods to
      * create the witness:
      * {{{
      * import TwoArgumentPlutusScriptWitness.*
      *
      * // With attached Plutus script
      * builder.registerStake(scriptStakeAddr, attached(script, redeemer))
      *
      * // With reference Plutus script
      * builder.references(scriptRefUtxo)
      *        .registerStake(scriptStakeAddr, reference(redeemer))
      *
      * // With native script
      * import NativeScriptWitness.*
      * builder.registerStake(scriptStakeAddr, attached(nativeScript))
      * }}}
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to register
      * @param witness
      *   the script witness authorizing the registration
      */
    def registerStake(stakeAddress: StakeAddress, witness: ScriptWitness): TxBuilder = {
        val credential = stakeAddress.credential
        // Script-based registrations must use Conway-style certificate with explicit deposit
        // so that the ledger recognizes it as needing script execution.
        // Shelley-style (coin=None) doesn't trigger script execution per Certificate.scriptHashOption.
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.RegCert(credential, Some(deposit))
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Deregisters a stake key from the network.
      *
      * The refund amount is taken from protocol parameters (stakeAddressDeposit).
      */
    def deregisterStake(stakeAddress: StakeAddress): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.UnregCert(credential, None)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Deregisters a stake key with an explicit refund amount.
      *
      * @param refund
      *   the amount originally deposited during registration (must match ledger state)
      */
    def deregisterStake(stakeAddress: StakeAddress, refund: Coin): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.UnregCert(credential, Some(refund))
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Deregisters a script-based stake key from the network.
      *
      * Use the factory methods to create the witness:
      * {{{
      * import TwoArgumentPlutusScriptWitness.*
      *
      * // With attached Plutus script
      * builder.deregisterStake(scriptStakeAddr, attached(script, redeemer))
      *
      * // With explicit refund amount
      * builder.deregisterStake(scriptStakeAddr, Some(Coin.ada(2)), attached(script, redeemer))
      * }}}
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to deregister
      * @param witness
      *   the script witness authorizing the deregistration
      */
    def deregisterStake(stakeAddress: StakeAddress, witness: ScriptWitness): TxBuilder =
        deregisterStake(stakeAddress, None, witness)

    /** Deregisters a script-based stake key from the network with explicit refund.
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to deregister
      * @param refund
      *   optional refund amount (must match ledger state for explicit refund)
      * @param witness
      *   the script witness authorizing the deregistration
      */
    def deregisterStake(
        stakeAddress: StakeAddress,
        refund: Option[Coin],
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.UnregCert(credential, refund)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Delegates a stake key to the specified stake pool. */
    def delegateTo(stakeAddress: StakeAddress, poolId: PoolKeyHash): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.StakeDelegation(credential, poolId)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Delegates a script-based stake key to the specified stake pool.
      *
      * Use the factory methods to create the witness:
      * {{{
      * import TwoArgumentPlutusScriptWitness.*
      * builder.delegateTo(scriptStakeAddr, poolId, attached(script, redeemer))
      * }}}
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to delegate
      * @param poolId
      *   the pool key hash to delegate to
      * @param witness
      *   the script witness authorizing the delegation
      */
    def delegateTo(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.StakeDelegation(credential, poolId)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Registers a stake key and delegates to a stake pool in a single transaction.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      */
    def stakeAndDelegate(stakeAddress: StakeAddress, poolId: PoolKeyHash): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.StakeRegDelegCert(credential, poolId, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a script-based stake key and delegates to a stake pool.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to register and delegate
      * @param poolId
      *   the pool key hash to delegate to
      * @param witness
      *   the script witness authorizing the registration and delegation
      */
    def stakeAndDelegate(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.StakeRegDelegCert(credential, poolId, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Withdraws staking rewards. */
    def withdrawRewards(stakeAddress: StakeAddress, amount: Coin): TxBuilder = {
        val credential = stakeAddress.credential
        addSteps(
          TransactionBuilderStep.WithdrawRewards(
            credential,
            amount,
            PubKeyWitness
          )
        )
    }

    /** Withdraws staking rewards from a script-based stake address.
      *
      * Use the factory methods to create the witness:
      * {{{
      * import TwoArgumentPlutusScriptWitness.*
      * builder.withdrawRewards(scriptAddr, amount, attached(script, redeemer))
      * builder.withdrawRewards(scriptAddr, amount, reference(redeemer))
      * }}}
      *
      * @param stakeAddress
      *   stake address to withdraw from (must have script credential)
      * @param amount
      *   withdrawal amount
      * @param witness
      *   the script witness authorizing the withdrawal
      */
    def withdrawRewards(
        stakeAddress: StakeAddress,
        amount: Coin,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        addSteps(
          TransactionBuilderStep.WithdrawRewards(
            credential,
            amount,
            witness
          )
        )
    }

    /** Delegates voting power to a DRep. */
    def delegateVoteToDRep(stakeAddress: StakeAddress, drep: DRep): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.VoteDelegCert(credential, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Delegates voting power from a script-based stake address to a DRep.
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address
      * @param drep
      *   the DRep to delegate voting power to
      * @param witness
      *   the script witness authorizing the delegation
      */
    def delegateVoteToDRep(
        stakeAddress: StakeAddress,
        drep: DRep,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.VoteDelegCert(credential, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Registers stake address and delegates voting power to a DRep in one transaction.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      */
    def registerAndDelegateVoteToDRep(stakeAddress: StakeAddress, drep: DRep): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.VoteRegDelegCert(credential, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a script-based stake address and delegates voting power to a DRep.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to register
      * @param drep
      *   the DRep to delegate voting power to
      * @param witness
      *   the script witness authorizing the registration and delegation
      */
    def registerAndDelegateVoteToDRep(
        stakeAddress: StakeAddress,
        drep: DRep,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.VoteRegDelegCert(credential, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Delegates to both a stake pool and a DRep. */
    def delegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.StakeVoteDelegCert(credential, poolId, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Delegates a script-based stake address to both a stake pool and a DRep.
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address
      * @param poolId
      *   the pool key hash to delegate to
      * @param drep
      *   the DRep to delegate voting power to
      * @param witness
      *   the script witness authorizing the delegation
      */
    def delegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val cert = Certificate.StakeVoteDelegCert(credential, poolId, drep)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Registers stake address and delegates to both stake pool and DRep in one transaction.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      */
    def registerAndDelegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.StakeVoteRegDelegCert(credential, poolId, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a script-based stake address and delegates to both stake pool and DRep.
      *
      * The deposit amount is taken from protocol parameters (stakeAddressDeposit).
      *
      * @note
      *   The stake address must have a script credential. Using a key-based credential with a
      *   script witness will cause validation errors.
      * @param stakeAddress
      *   the script-based stake address to register
      * @param poolId
      *   the pool key hash to delegate to
      * @param drep
      *   the DRep to delegate voting power to
      * @param witness
      *   the script witness authorizing the registration and delegation
      */
    def registerAndDelegateToPoolAndDRep(
        stakeAddress: StakeAddress,
        poolId: PoolKeyHash,
        drep: DRep,
        witness: ScriptWitness
    ): TxBuilder = {
        val credential = stakeAddress.credential
        val deposit = Coin(env.protocolParams.stakeAddressDeposit)
        val cert = Certificate.StakeVoteRegDelegCert(credential, poolId, drep, deposit)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Registers as a DRep.
      *
      * The deposit amount is taken from protocol parameters (dRepDeposit).
      */
    def registerDRep(drepCredential: Credential, anchor: Option[Anchor]): TxBuilder = {
        val deposit = Coin(env.protocolParams.dRepDeposit)
        val cert = Certificate.RegDRepCert(drepCredential, deposit, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Registers a script-based DRep.
      *
      * The deposit amount is taken from protocol parameters (dRepDeposit). Post-Conway,
      * script-based DRep registrations require authorization.
      *
      * {{{
      * import TwoArgumentPlutusScriptWitness.*
      * builder.registerDRep(scriptCredential, Some(anchor), attached(script, redeemer))
      * }}}
      *
      * @note
      *   The credential must be a script credential (Credential.ScriptHash). Using a key-based
      *   credential with a script witness will cause validation errors.
      * @param drepCredential
      *   the script-based DRep credential
      * @param anchor
      *   optional metadata anchor for the DRep
      * @param witness
      *   the script witness authorizing the registration
      */
    def registerDRep(
        drepCredential: Credential,
        anchor: Option[Anchor],
        witness: ScriptWitness
    ): TxBuilder = {
        val deposit = Coin(env.protocolParams.dRepDeposit)
        val cert = Certificate.RegDRepCert(drepCredential, deposit, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Unregisters as a DRep.
      *
      * @param refund
      *   the amount originally deposited during registration (must match ledger state)
      */
    def unregisterDRep(drepCredential: Credential, refund: Coin): TxBuilder = {
        val cert = Certificate.UnregDRepCert(drepCredential, refund)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Unregisters a script-based DRep.
      *
      * @note
      *   The credential must be a script credential (Credential.ScriptHash). Using a key-based
      *   credential with a script witness will cause validation errors.
      * @param drepCredential
      *   the script-based DRep credential
      * @param refund
      *   the amount originally deposited during registration (must match ledger state)
      * @param witness
      *   the script witness authorizing the unregistration
      */
    def unregisterDRep(
        drepCredential: Credential,
        refund: Coin,
        witness: ScriptWitness
    ): TxBuilder = {
        val cert = Certificate.UnregDRepCert(drepCredential, refund)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
    }

    /** Updates DRep metadata anchor. */
    def updateDRep(drepCredential: Credential, anchor: Option[Anchor]): TxBuilder = {
        val cert = Certificate.UpdateDRepCert(drepCredential, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, PubKeyWitness))
    }

    /** Updates a script-based DRep metadata anchor.
      *
      * @note
      *   The credential must be a script credential (Credential.ScriptHash). Using a key-based
      *   credential with a script witness will cause validation errors.
      * @param drepCredential
      *   the script-based DRep credential
      * @param anchor
      *   optional new metadata anchor
      * @param witness
      *   the script witness authorizing the update
      */
    def updateDRep(
        drepCredential: Credential,
        anchor: Option[Anchor],
        witness: ScriptWitness
    ): TxBuilder = {
        val cert = Certificate.UpdateDRepCert(drepCredential, anchor)
        addSteps(TransactionBuilderStep.IssueCertificate(cert, witness))
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
        val slot = env.slotConfig.instantToSlot(from).toLong
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
        val slot = env.slotConfig.instantToSlot(to).toLong
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
        val params = env.protocolParams
        // Could be a good idea to immediately `modify` on every step, maybe not tho.
        val balancedContext = for {
            built <- TransactionBuilder.modify(context, steps)
            withAttachments = addAttachmentsToContext(built)
            balanced <- withAttachments.balanceContext(params, diffHandler, evaluator)
        } yield balanced

        balancedContext match {
            case Right(balanced) =>
                copy(context = balanced)
            case Left(error) =>
                throw TxBuilderException.fromBuildError(error)
        }
    }

    /** Builds and finalizes the transaction, sending any remaining value to the specified change
      * address.
      *
      * This is a convenience method that uses the default change handling strategy. Any difference
      * between inputs and outputs (minus fees) will be sent to the provided address as a new
      * output.
      *
      * Note: If [[changeTo(TransactionOutput)]] was called, that output will receive the change
      * instead of creating a new output at this address.
      *
      * @param changeTo
      *   the address to receive any remaining value (change)
      * @return
      *   a new TxBuilder with the finalized transaction
      * @throws RuntimeException
      *   if script execution fails or if the transaction cannot be balanced
      */
    def build(changeTo: Address): TxBuilder = {
        // Determine change output index and add implicit change output if needed
        val (buildSteps, changeIdx) = changeOutputIndex match {
            case Some(idx) =>
                // Explicit change output already added via changeTo - use its index
                (steps, idx)

            case None =>
                // No explicit change output - add implicit one at change address
                val implicitChangeOutput = TransactionOutput(changeTo, Value.zero)
                val implicitChangeStep = TransactionBuilderStep.Send(implicitChangeOutput)
                (steps :+ implicitChangeStep, sendStepCount)
        }

        // Build with the modified steps and use changeOutputDiffHandler
        copy(steps = buildSteps).build(diffHandler =
            (diff, tx) => Change.changeOutputDiffHandler(diff, tx, env.protocolParams, changeIdx)
        )
    }

    /** Assembles the transaction without balancing or fee calculation.
      *
      * This method builds a transaction from the accumulated steps but skips the finalization phase
      * (fee calculation, change handling, script evaluation). Useful for:
      *   - Testing: Creating transactions for ScriptContext derivation
      *   - Inspection: Previewing transaction structure before committing to balancing
      *   - Advanced use cases: When manual control over balancing is needed
      *
      * @return
      *   the unbalanced transaction
      * @throws TxBuilderException
      *   if step processing fails
      */
    def draft: Transaction = {
        TransactionBuilder
            .modify(context, steps)
            .map(addAttachmentsToContext)
            .match
                case Right(ctx)  => ctx.transaction
                case Left(error) => throw TxBuilderException.fromBuildError(error)
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
        val signedTx = signer.sign(transaction)
        copy(context = context.copy(transaction = signedTx))
    }

    /** Returns the current transaction.
      *
      * Call this after [[build]] and optionally [[sign]] to get the final transaction ready for
      * submission.
      */
    def transaction: Transaction = context.transaction

    /** Completes the transaction by selecting UTXOs, adding collateral, and balancing.
      *
      * This method queries the reader for available UTXOs at the sponsor address, selects inputs to
      * cover all outputs and fees, selects collateral if needed (for script transactions), and sets
      * up change handling to the sponsor address.
      *
      * This is the primary cross-platform method available on both JVM and JavaScript platforms.
      *
      * On JVM, you can use the blocking overload with a `timeout` parameter, or use the `await`
      * extension method:
      * {{{
      * import scalus.cardano.txbuilder.await
      * builder.complete(reader, sponsor).await(30.seconds)
      * }}}
      *
      * @param reader
      *   the async reader to query for UTXOs
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      * @return
      *   a Future containing a new TxBuilder with the transaction completed
      */
    def complete(reader: BlockchainReader, sponsor: Address): Future[TxBuilder] = {
        // Build initial context FIRST (fail-fast before async call)
        val initialBuildResult = TransactionBuilder.build(env.network, steps)

        initialBuildResult match {
            case Left(error) =>
                Future.failed(TxBuilderException.fromBuildError(error))

            case Right(initialCtx) =>
                // Only fetch UTXOs if initial build succeeds
                // Use reader's ExecutionContext for the map operation
                given scala.concurrent.ExecutionContext = reader.executionContext
                reader.findUtxos(address = sponsor).map { utxosResult =>
                    val allAvailableUtxos = utxosResult.getOrElse(Map.empty)
                    completeWithUtxos(allAvailableUtxos, sponsor, initialCtx)
                }
        }
    }

    /** Completes the transaction using pre-fetched UTXOs.
      *
      * This synchronous variant is useful when UTXOs are already available in memory, avoiding the
      * need for async provider queries. Otherwise identical to the provider-based variant.
      *
      * @param availableUtxos
      *   the UTXOs available at the sponsor address for input/collateral selection
      * @param sponsor
      *   the address to use for input selection, collateral, and change
      * @return
      *   a new TxBuilder with the transaction completed
      */
    def complete(availableUtxos: Utxos, sponsor: Address): TxBuilder = {
        val initialBuildResult = TransactionBuilder.build(env.network, steps)

        initialBuildResult match {
            case Left(error) =>
                throw TxBuilderException.fromBuildError(error)
            case Right(initialCtx) =>
                completeWithUtxos(availableUtxos, sponsor, initialCtx)
        }
    }

    /** Internal implementation shared by both complete variants. Takes the pre-computed initial
      * context to preserve fail-fast behavior in async version.
      */
    private def completeWithUtxos(
        allAvailableUtxos: Utxos,
        sponsor: Address,
        initialCtx: TransactionBuilder.Context
    ): TxBuilder = {
        // Exclude UTXOs already used in initial context (from user's steps)
        val alreadyUsedInputs = initialCtx.resolvedUtxos.utxos.keySet
        val availableUtxos = allAvailableUtxos.filterNot { case (input, _) =>
            alreadyUsedInputs.contains(input)
        }

        // Determine if we need collateral (transaction has scripts)
        val needsCollateral = initialCtx.redeemers.nonEmpty

        // Create UTXO pool and select initial collateral if needed.
        // Initial collateral of 5 ADA is a conservative estimate that covers most script
        // transactions. The actual required collateral is protocol_params.collateralPercentage
        // of the transaction fee, which is finalized during balancing.
        val emptyPool = UtxoPool(availableUtxos)
        val pool =
            if needsCollateral then {
                val initialCollateral =
                    emptyPool.selectForCollateral(Coin.ada(5), env.protocolParams)
                emptyPool.withCollateral(initialCollateral)
            } else emptyPool

        // Iteratively build and finalize until balanced
        completeLoop(
          pool = pool,
          sponsor = sponsor,
          maxIterations = TxBuilder.MaxCompleteIterations
        )
    }

    /** Iterative loop that adds UTXOs until balanceContext succeeds. */
    private def completeLoop(
        pool: UtxoPool,
        sponsor: Address,
        maxIterations: Int
    ): TxBuilder = {
        if maxIterations <= 0 then {
            throw new RuntimeException(
              "Max iterations exceeded while trying to balance transaction"
            )
        }

        // Build steps: base steps + selected inputs + selected collateral
        val spendSteps = pool.inputs.map { case (input, output) =>
            TransactionBuilderStep.Spend(Utxo(input, output), PubKeyWitness)
        }.toSeq

        val collateralSteps = pool.collateral.map { case (input, output) =>
            TransactionBuilderStep.AddCollateral(Utxo(input, output))
        }.toSeq

        // Determine change output index and add implicit change output if needed
        val (allSteps, changeIdx) = changeOutputIndex match {
            case Some(idx) =>
                // Explicit change output already added via changeTo - use its index
                (steps ++ spendSteps ++ collateralSteps, idx)

            case None =>
                // No explicit change output - add implicit one at sponsor address
                val implicitChangeOutput = TransactionOutput(sponsor, Value.zero)
                val implicitChangeStep = TransactionBuilderStep.Send(implicitChangeOutput)
                (steps ++ spendSteps ++ collateralSteps :+ implicitChangeStep, sendStepCount)
        }

        TransactionBuilder.build(env.network, allSteps) match {
            case Left(error) =>
                throw TxBuilderException.fromBuildError(error)

            case Right(ctx) =>
                val ctxWithAttachments = addAttachmentsToContext(ctx)
                val diffHandler: DiffHandler = (diff, tx) =>
                    Change.changeOutputDiffHandler(diff, tx, env.protocolParams, changeIdx)

                ctxWithAttachments.balanceContext(
                  env.protocolParams,
                  diffHandler,
                  evaluator
                ) match {
                    case Right(balancedCtx) =>
                        if balancedCtx.transaction.body.value.inputs.toSeq.isEmpty && pool.remainingForInputs.nonEmpty
                        then {
                            // Force selection of at least one input and retry
                            // This is necessary because every tx must have at least 1 input.
                            val minimalInput = pool.selectForValue(Value.lovelace(1))
                            completeLoop(pool.withInputs(minimalInput), sponsor, maxIterations - 1)
                        } else {
                            // Success! Add sponsor to expected signers
                            val sponsorSigner = extractSponsorSigner(sponsor)
                            val ctxWithSigner = balancedCtx.copy(
                              expectedSigners = balancedCtx.expectedSigners ++ sponsorSigner.toSet
                            )
                            copy(context = ctxWithSigner)
                        }

                    case Left(SomeBuildError.BalancingError(balancingError, errorCtx)) =>
                        // Handle balancing errors by adding more UTXOs
                        handleBalancingError(
                          balancingError,
                          errorCtx,
                          pool,
                          sponsor,
                          maxIterations
                        )

                    case Left(error) =>
                        throw TxBuilderException.fromBuildError(error)
                }
        }
    }

    /** Handle balancing errors by selecting more UTXOs and retrying. */
    private def handleBalancingError(
        error: TxBalancingError,
        errorCtx: TransactionBuilder.Context,
        pool: UtxoPool,
        sponsor: Address,
        maxIterations: Int
    ): TxBuilder = {
        error match {
            case TxBalancingError.InsufficientFunds(valueDiff, minRequired) =>
                // valueDiff contains both ADA and tokens - handle both uniformly
                // When we need more (deficit), valueDiff is negative, so:
                // - Positive values mean excess (change)
                // - Negative values mean deficit (need more)
                val tokensWeNeed =
                    -valueDiff.assets.negativeAssets // Negate negatives to get positive amounts
                val adaWeNeed =
                    math.max(-valueDiff.coin.value, minRequired) // Negate to get positive amount
                val additionalNeeded = Value(Coin(adaWeNeed), tokensWeNeed)
                val additionalUtxos = pool.selectForValue(additionalNeeded)

                // Check if we found enough UTXOs - for tokens, check if all needed tokens are covered
                val tokensCoveredBySelection = additionalUtxos.values
                    .foldLeft(MultiAsset.zero)((acc, o) => acc + o.value.assets)
                val remainingTokens = (tokensWeNeed - tokensCoveredBySelection).onlyPositive

                if remainingTokens.nonEmpty then {
                    // Found UTXOs but they don't cover required tokens - throw token error
                    val (policyId, assets) = remainingTokens.assets.head
                    val (assetName, requiredAmount) = assets.head
                    throw TxBuilderException.InsufficientTokensException(
                      policyId,
                      assetName,
                      requiredAmount + tokensCoveredBySelection.assets
                          .get(policyId)
                          .flatMap(_.get(assetName))
                          .getOrElse(0L),
                      pool.totalAvailableTokens(policyId, assetName),
                      sponsor
                    )
                }

                if additionalUtxos.isEmpty then {
                    // No UTXOs at all - insufficient ADA
                    throw TxBuilderException.InsufficientAdaException(
                      Coin(minRequired),
                      pool.totalAvailableAda,
                      sponsor
                    )
                }

                completeLoop(
                  pool.withInputs(additionalUtxos),
                  sponsor,
                  maxIterations - 1
                )

            case TxBalancingError.InsufficientCollateralForReturn(totalAda, required, minAda) =>
                // Need more collateral ADA
                val neededMore = Coin(required.value - totalAda.value + minAda.value)
                val additionalCollateral = pool.selectForCollateral(neededMore, env.protocolParams)

                if additionalCollateral.isEmpty || additionalCollateral == pool.collateral then {
                    throw TxBuilderException.BalancingException(error, errorCtx)
                }

                completeLoop(
                  pool.withCollateral(additionalCollateral),
                  sponsor,
                  maxIterations - 1
                )

            case _ =>
                // Other balancing errors - cannot recover
                throw TxBuilderException.BalancingException(error, errorCtx)
        }
    }

    /** Extract sponsor's expected signer from address if it's a pubkey address. */
    private def extractSponsorSigner(sponsor: Address): Option[ExpectedSigner] = {
        sponsor match {
            case sa: ShelleyAddress =>
                sa.payment match {
                    case ShelleyPaymentPart.Key(hash) => Some(ExpectedSigner(hash))
                    case _                            => None
                }
            case _ => None
        }
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
        val currentData = updatedTx.witnessSet.plutusData.value.toSortedMap
        val allData = currentData ++ attachedData.view.map((k, v) => k -> KeepRaw(v))
        updatedTx = updatedTx.withWitness(_.copy(plutusData = KeepRaw(TaggedSortedMap(allData))))
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
}

/** Factory methods for creating TxBuilder instances. */
object TxBuilder {

    /** Maximum number of iterations for the complete loop.
      *
      * The complete loop adds UTXOs from the pool until the transaction can be finalized. Typical
      * transactions complete in 1-2 iterations. If this limit is exceeded, it usually indicates
      * insufficient funds or an issue with the transaction structure.
      */
    private val MaxCompleteIterations = 10

    // -------------------------------------------------------------------------
    // Factory methods
    // -------------------------------------------------------------------------

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
        env: CardanoInfo,
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
    def apply(env: CardanoInfo): TxBuilder = {
        val evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
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
    def withConstMaxBudgetEvaluator(env: CardanoInfo): TxBuilder = {
        val evaluator = PlutusScriptEvaluator.constMaxBudget(env)
        apply(env, evaluator)
    }
}

/** Creates a TxBuilder using a context CardanoInfo.
  *
  * Example:
  * {{{
  * given CardanoInfo = CardanoInfo.mainnet
  * val builder = txBuilder  // Uses the given CardanoInfo
  * }}}
  */
def txBuilder(using env: CardanoInfo): TxBuilder = TxBuilder(env)
