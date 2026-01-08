package scalus.patterns

import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v1.{Address, Credential, ValidatorHash}
import scalus.ledger.api.v3.{Redeemer, ScriptPurpose, TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v2.TxOut
import scalus.prelude.*

import scala.annotation.tailrec

/** Indexer pattern for matching inputs to outputs by index. Indices are computed off-chain using
  * SpendWithDelayedRedeemer after the transaction is built.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-design-patterns/tree/main/lib/aiken-design-patterns]]
  */
@Compile
object UtxoIndexer {

    /** Validates a single input at the specified index. */
    def validateInput(
        ownRef: TxOutRef,
        inputIdx: BigInt,
        tx: TxInfo,
        validator: TxInInfo => Boolean
    ): Unit = {
        val input = tx.inputs.at(inputIdx)
        require(input.outRef === ownRef, InputIndexMismatch)
        require(validator(input), ValidatorFailed)
    }

    /** Validates a one-to-one relationship between an input and output at specified indices. */
    def oneToOne(
        ownRef: TxOutRef,
        inputIdx: BigInt,
        outputIdx: BigInt,
        tx: TxInfo,
        validator: (TxInInfo, TxOut) => Boolean
    ): Unit = {
        val input = tx.inputs.at(inputIdx)
        require(input.outRef === ownRef, InputIndexMismatch)

        val output = tx.outputs.at(outputIdx)
        require(validator(input, output), ValidatorFailed)
    }

    /** Validates a one-to-many relationship between an input and multiple outputs.
      *
      * This function validates one input against multiple outputs, executing both per-output and
      * collective validation.
      */
    def oneToMany(
        ownRef: TxOutRef,
        inputIdx: BigInt,
        outputIndices: List[BigInt],
        tx: TxInfo,
        perOutputValidator: (TxInInfo, BigInt, TxOut) => Boolean,
        collectiveValidator: (TxInInfo, List[TxOut]) => Boolean
    ): Unit = {
        val input = tx.inputs.at(inputIdx)
        require(input.outRef === ownRef, InputIndexMismatch)

        val allOutputs =
            validateAndCollectOutputs(outputIndices, tx.outputs, input, perOutputValidator)

        require(collectiveValidator(input, allOutputs), CollectiveValidatorFailed)
    }

    /** Validates multiple script inputs without redeemers, each with a corresponding output.
      *
      * This function processes multiple UTxOs from the same script, validating each input-output
      * pair. The inputs are only validated if they contain the specified script hash.
      */
    def multiOneToOneNoRedeemer(
        indexPairs: List[(BigInt, BigInt)],
        scriptHash: ValidatorHash,
        tx: TxInfo,
        validator: (BigInt, TxInInfo, BigInt, TxOut) => Boolean
    ): Unit = {
        val scriptCredential = Credential.ScriptCredential(scriptHash)

        val remainingPairs =
            processMultipleInputs(tx.inputs, indexPairs, tx.outputs, scriptCredential, validator)

        require(remainingPairs.isEmpty, UnprocessedIndexPairs)
    }

    /** Validates multiple script inputs with redeemers using the withdraw-zero trick.
      *
      * Processes multiple UTxOs from the same script where each has different redeemer data. Uses a
      * staking script (withdraw-0 validator) as a coupling mechanism - the transaction must include
      * a zero-amount withdrawal from the stake address. Each spend redeemer must embed the staking
      * credential which is extracted and validated by this function.
      */
    def multiOneToOneWithRedeemer[A](
        indexPairs: List[(BigInt, BigInt)],
        spendingScriptHash: ValidatorHash,
        stakeScriptHash: ValidatorHash,
        tx: TxInfo,
        redeemerCoercerAndStakeExtractor: Data => (A, Credential),
        validator: (BigInt, TxInInfo, A, BigInt, TxOut) => Boolean
    ): Unit = {
        val spendingCredential = Credential.ScriptCredential(spendingScriptHash)
        val stakeCredential = Credential.ScriptCredential(stakeScriptHash)

        // Filter and coerce redeemers from Spend purposes that match our stake credential
        val scriptRedeemers = filterAndCoerceRedeemers(
          tx.redeemers,
          stakeCredential,
          redeemerCoercerAndStakeExtractor
        )

        val remainingRedeemers = processMultipleInputsWithRedeemers(
          indexPairs,
          scriptRedeemers,
          tx.inputs,
          tx.outputs,
          spendingCredential,
          validator
        )

        require(remainingRedeemers.isEmpty, UnprocessedRedeemers)
    }

    /** Helper function to validate and collect outputs. */
    @tailrec
    private def validateAndCollectOutputs(
        indices: List[BigInt],
        outputs: List[TxOut],
        input: TxInInfo,
        validator: (TxInInfo, BigInt, TxOut) => Boolean,
        acc: List[TxOut] = List.Nil
    ): List[TxOut] = {
        indices match
            case List.Nil => acc
            case List.Cons(currIdx, rest) =>
                val output = outputs.at(currIdx)
                require(validator(input, currIdx, output), PerOutputValidatorFailed)
                validateAndCollectOutputs(rest, outputs, input, validator, List.Cons(output, acc))
    }

    /** Helper function to process multiple inputs and validate against outputs. */
    @tailrec
    private def processMultipleInputs(
        inputs: List[TxInInfo],
        indexPairs: List[(BigInt, BigInt)],
        outputs: List[TxOut],
        scriptCredential: Credential,
        validator: (BigInt, TxInInfo, BigInt, TxOut) => Boolean,
        currentIdx: BigInt = BigInt(0)
    ): List[(BigInt, BigInt)] = {
        inputs match
            // return the unprocessed pairs, if any
            case List.Nil => indexPairs
            case List.Cons(input, restInputs) =>
                val nextIdx = currentIdx + BigInt(1)
                // if the input has the specified hash -- process it, else recurse further
                if input.resolved.address.credential === scriptCredential then
                    indexPairs match
                        case List.Nil =>
                            fail(MoreScriptUtxosSpentThanSpecified)
                        case List.Cons((inIdx, outIdx), restPairs) =>
                            require(currentIdx === inIdx, InputIndexMismatch)
                            val output = outputs.at(outIdx)
                            require(
                              validator(inIdx, input, outIdx, output),
                              MultiValidationFailed
                            )
                            processMultipleInputs(
                              restInputs,
                              restPairs,
                              outputs,
                              scriptCredential,
                              validator,
                              nextIdx
                            )
                else
                    processMultipleInputs(
                      restInputs,
                      indexPairs,
                      outputs,
                      scriptCredential,
                      validator,
                      nextIdx
                    )
    }

    /** Helper to filter redeemers by Spend purpose and matching stake credential. */
    private def filterAndCoerceRedeemers[A](
        redeemers: SortedMap[ScriptPurpose, Redeemer],
        stakeCredential: Credential,
        coercerAndExtractor: Data => (A, Credential)
    ): List[A] = {
        redeemers.foldRight(List.empty[A]) { (purposeAndRedeemer, acc) =>
            purposeAndRedeemer match
                case (ScriptPurpose.Spending(_), redeemer) =>
                    val (coercedRedeemer, extractedStakeCred) =
                        coercerAndExtractor(redeemer)
                    if extractedStakeCred === stakeCredential then List.Cons(coercedRedeemer, acc)
                    else acc
                case _ => acc
        }
    }

    /** Helper to process multiple inputs with their redeemers. */
    @tailrec
    private def processMultipleInputsWithRedeemers[A](
        indexPairs: List[(BigInt, BigInt)],
        redeemers: List[A],
        inputs: List[TxInInfo],
        outputs: List[TxOut],
        spendingCredential: Credential,
        validator: (BigInt, TxInInfo, A, BigInt, TxOut) => Boolean
    ): List[A] = {
        indexPairs match
            case List.Nil => redeemers
            case List.Cons((inIdx, outIdx), restPairs) =>
                redeemers match
                    case List.Nil =>
                        fail(TooManyIndicesSpecified)
                    case List.Cons(redeemer, restRedeemers) =>
                        val input = inputs.at(inIdx)
                        require(
                          input.resolved.address.credential === spendingCredential,
                          InputNotFromSpendingScript
                        )
                        val output = outputs.at(outIdx)
                        require(
                          validator(inIdx, input, redeemer, outIdx, output),
                          MultiValidationFailed
                        )
                        processMultipleInputsWithRedeemers(
                          restPairs,
                          restRedeemers,
                          inputs,
                          outputs,
                          spendingCredential,
                          validator
                        )
    }

    inline val InputIndexMismatch = "Input index does not match ownRef"
    inline val ValidatorFailed = "Validator failed for input-output pair"
    inline val PerOutputValidatorFailed = "Per-output validator failed"
    inline val CollectiveValidatorFailed = "Collective validator failed"
    inline val MoreScriptUtxosSpentThanSpecified =
        "More UTxOs of the script are spent than specified"
    inline val UnprocessedIndexPairs = "All index pairs must be processed"
    inline val MultiValidationFailed = "Validation failed"
    inline val TooManyIndicesSpecified = "Too many indices specified"
    inline val InputNotFromSpendingScript = "Input not from spending script"
    inline val UnprocessedRedeemers = "All redeemers must be processed"
}
