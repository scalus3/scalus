package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*

import java.nio.file.Path

/** Base class for ledger rules validation tests
  *
  * Uses BlocksTestUtils for common infrastructure like loading blocks from SCALUS_IT_DATA_PATH
  * and creating UTxO resolver.
  */
abstract class LedgerRulesValidationTestBase extends AnyFunSuite {

  // All pure validators
  protected lazy val pureValidators: List[STS.Validator] = List(
    AllInputsMustBeInUtxoValidator,
    EmptyInputsValidator,
    ExactSetOfRedeemersValidator,
    ExUnitsTooBigValidator,
    FeesOkValidator,
    InputsAndReferenceInputsDisjointValidator,
    MetadataValidator,
    MissingKeyHashesValidator,
    MissingOrExtraScriptHashesValidator,
    MissingRequiredDatumsValidator,
    NativeScriptsValidator,
    OutsideForecastValidator,
    OutsideValidityIntervalValidator,
    OutputBootAddrAttrsSizeValidator,
    OutputsHaveNotEnoughCoinsValidator,
    OutputsHaveTooBigValueStorageSizeValidator,
    ProtocolParamsViewHashesMatchValidator,
    ScriptsWellFormedValidator,
    TooManyCollateralInputsValidator,
    TransactionSizeValidator,
    ValueNotConservedUTxOValidator,
    VerifiedSignaturesInWitnessesValidator,
    WrongNetworkInTxBodyValidator,
    WrongNetworkValidator,
    WrongNetworkWithdrawalValidator
  )

  // All mutators
  protected lazy val mutators: List[STS.Mutator] = List(
    AddOutputsToUtxoMutator,
    FeeMutator,
    PlutusScriptsTransactionMutator,
    RemoveInputsFromUtxoMutator
  )

  // Default context for validation
  protected def defaultContext: Context = Context.testMainnet()

  // Default initial state
  protected def defaultState: State = State()

  // Data class for validation failure
  case class ValidationFailure(
      blockPath: Path,
      txIndex: Int,
      txId: TransactionHash,
      validatorName: String,
      error: TransactionException
  )

  // Data class for validation summary
  case class ValidationSummary(
      totalBlocks: Int,
      totalTransactions: Int,
      totalValidations: Int,
      failures: List[ValidationFailure]
  ) {
    def failureCount: Int = failures.size
    def successCount: Int = totalValidations - failureCount
    def successRate: Double =
      if totalValidations > 0 then successCount.toDouble / totalValidations else 0.0

    def report: String = {
      val sb = new StringBuilder
      sb.append(s"\n=== Validation Summary ===\n")
      sb.append(s"Blocks processed: $totalBlocks\n")
      sb.append(s"Transactions validated: $totalTransactions\n")
      sb.append(s"Total validations: $totalValidations\n")
      sb.append(s"Successes: $successCount\n")
      sb.append(s"Failures: $failureCount\n")
      sb.append(f"Success rate: ${successRate * 100}%.2f%%\n")

      if failures.nonEmpty then
        sb.append(s"\n=== Failures by Validator ===\n")
        val byValidator = failures.groupBy(_.validatorName)
        for (validator, fails) <- byValidator.toSeq.sortBy(-_._2.size) do
          sb.append(s"  $validator: ${fails.size} failures\n")

        sb.append(s"\n=== First 10 Detailed Failures ===\n")
        for failure <- failures.take(10) do
          sb.append(s"  Block: ${failure.blockPath.getFileName}\n")
          sb.append(s"  TX Index: ${failure.txIndex}\n")
          sb.append(s"  TX ID: ${failure.txId}\n")
          sb.append(s"  Validator: ${failure.validatorName}\n")
          sb.append(s"  Error: ${failure.error.getMessage}\n")
          sb.append(s"  ---\n")

      sb.toString
    }
  }

  // Run validators as composed STS chain
  protected def validateWithComposedChain(
      validators: List[STS.Validator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    val composedValidator = STS.Validator(validators, "ComposedValidators")
    var failures = List.empty[ValidationFailure]
    var totalTxs = 0
    var consecutiveMissingUtxos = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0

    for (blockPath, blockIdx) <- blockPaths.zipWithIndex
        if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
    do
      val (blockFile, blockBytes) = loadBlock(blockPath)
      given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
      val block = blockFile.block
      var state = initialState
      processedBlocks += 1

      for (tx, txIdx) <- block.transactions.zipWithIndex do
        totalTxs += 1
        try
          val utxos = scalusUtxoResolver.resolveUtxos(tx)
          val txState = state.copy(utxos = state.utxos ++ utxos)
          consecutiveMissingUtxos = 0 // Reset on success

          composedValidator(context, txState, tx) match {
            case Left(error) =>
              failures = ValidationFailure(
                blockPath,
                txIdx,
                tx.id,
                composedValidator.name,
                error
              ) :: failures
            case Right(_) => // success
          }
        catch
          case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
            // Skip transactions with missing UTxOs
            consecutiveMissingUtxos += 1
            if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
              println(
                s"⚠ Stopping validation: ${consecutiveMissingUtxos} consecutive missing UTxO errors"
              )

    ValidationSummary(
      processedBlocks,
      totalTxs,
      totalTxs,
      failures.reverse
    )
  }

  // Run validators sequentially in a single pass
  protected def validateWithSinglePass(
      validators: List[STS.Validator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    var failures = List.empty[ValidationFailure]
    var totalTxs = 0
    var totalValidations = 0
    var consecutiveMissingUtxos = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0

    for (blockPath, blockIdx) <- blockPaths.zipWithIndex
        if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
    do
      val (blockFile, blockBytes) = loadBlock(blockPath)
      given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
      val block = blockFile.block
      var state = initialState
      processedBlocks += 1

      for (tx, txIdx) <- block.transactions.zipWithIndex do
        totalTxs += 1
        try
          val utxos = scalusUtxoResolver.resolveUtxos(tx)
          val txState = state.copy(utxos = state.utxos ++ utxos)
          consecutiveMissingUtxos = 0 // Reset on success

          // Run all validators sequentially
          for validator <- validators do
            totalValidations += 1
            validator(context, txState, tx) match {
              case Left(error) =>
                failures =
                  ValidationFailure(blockPath, txIdx, tx.id, validator.name, error) :: failures
              case Right(_) => // success
            }
        catch
          case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
            // Skip transactions with missing UTxOs
            consecutiveMissingUtxos += 1
            if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
              println(
                s"⚠ Stopping validation: ${consecutiveMissingUtxos} consecutive missing UTxO errors"
              )

    ValidationSummary(
      processedBlocks,
      totalTxs,
      totalValidations,
      failures.reverse
    )
  }

  // Run validators separately and collect failures from each
  protected def validateWithSeparateCollections(
      validators: List[STS.Validator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    var allFailures = List.empty[ValidationFailure]
    var totalTxs = 0
    var totalValidations = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0
    var stoppedValidators = List.empty[String]

    // Run each validator separately
    for validator <- validators do
      var consecutiveMissingUtxos = 0

      for (blockPath, blockIdx) <- blockPaths.zipWithIndex
          if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
      do
        val (blockFile, blockBytes) = loadBlock(blockPath)
        given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
        val block = blockFile.block
        var state = initialState
        if blockIdx == 0 then processedBlocks += 1

        for (tx, txIdx) <- block.transactions.zipWithIndex do
          if blockIdx == 0 then totalTxs += 1
          totalValidations += 1

          try
            val utxos = scalusUtxoResolver.resolveUtxos(tx)
            val txState = state.copy(utxos = state.utxos ++ utxos)
            consecutiveMissingUtxos = 0 // Reset on success

            validator(context, txState, tx) match {
              case Left(error) =>
                allFailures =
                  ValidationFailure(blockPath, txIdx, tx.id, validator.name, error) :: allFailures
              case Right(_) => // success
            }
          catch
            case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
              // Skip transactions with missing UTxOs
              consecutiveMissingUtxos += 1
              if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
                stoppedValidators = validator.name :: stoppedValidators

    if stoppedValidators.nonEmpty then
      println(s"⚠ Stopped ${stoppedValidators.size} validator(s) due to consecutive missing UTxO errors")

    ValidationSummary(
      processedBlocks,
      totalTxs / validators.size, // Adjust for multiple passes
      totalValidations,
      allFailures.reverse
    )
  }

  // Run mutators with composed chain
  protected def validateMutatorsWithComposedChain(
      mutators: List[STS.Mutator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    val composedMutator = STS.Mutator(mutators, "ComposedMutators")
    var failures = List.empty[ValidationFailure]
    var totalTxs = 0
    var consecutiveMissingUtxos = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0

    for (blockPath, blockIdx) <- blockPaths.zipWithIndex
        if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
    do
      val (blockFile, blockBytes) = loadBlock(blockPath)
      given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
      val block = blockFile.block
      var state = initialState
      processedBlocks += 1

      for (tx, txIdx) <- block.transactions.zipWithIndex do
        totalTxs += 1
        try
          val utxos = scalusUtxoResolver.resolveUtxos(tx)
          val txState = state.copy(utxos = state.utxos ++ utxos)
          consecutiveMissingUtxos = 0 // Reset on success

          composedMutator(context, txState, tx) match {
            case Left(error) =>
              failures =
                ValidationFailure(blockPath, txIdx, tx.id, composedMutator.name, error) :: failures
            case Right(newState) =>
              state = newState.asInstanceOf[State]
          }
        catch
          case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
            // Skip transactions with missing UTxOs
            consecutiveMissingUtxos += 1
            if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
              println(
                s"⚠ Stopping validation: ${consecutiveMissingUtxos} consecutive missing UTxO errors"
              )

    ValidationSummary(
      processedBlocks,
      totalTxs,
      totalTxs,
      failures.reverse
    )
  }

  protected def validateMutatorsWithSinglePass(
      mutators: List[STS.Mutator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    var failures = List.empty[ValidationFailure]
    var totalTxs = 0
    var totalValidations = 0
    var consecutiveMissingUtxos = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0

    for (blockPath, blockIdx) <- blockPaths.zipWithIndex
        if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
    do
      val (blockFile, blockBytes) = loadBlock(blockPath)
      given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
      val block = blockFile.block
      var state = initialState
      processedBlocks += 1

      for (tx, txIdx) <- block.transactions.zipWithIndex do
        totalTxs += 1
        try
          val utxos = scalusUtxoResolver.resolveUtxos(tx)
          val txState = state.copy(utxos = state.utxos ++ utxos)
          consecutiveMissingUtxos = 0 // Reset on success

          // Run all mutators sequentially
          var currentState = txState
          for mutator <- mutators do
            totalValidations += 1
            mutator(context, currentState, tx) match {
              case Left(error) =>
                failures =
                  ValidationFailure(blockPath, txIdx, tx.id, mutator.name, error) :: failures
              case Right(newState) =>
                currentState = newState
            }
          state = currentState
        catch
          case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
            // Skip transactions with missing UTxOs
            consecutiveMissingUtxos += 1
            if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
              println(
                s"⚠ Stopping validation: ${consecutiveMissingUtxos} consecutive missing UTxO errors"
              )

    ValidationSummary(
      processedBlocks,
      totalTxs,
      totalValidations,
      failures.reverse
    )
  }

  protected def validateMutatorsWithSeparateCollections(
      mutators: List[STS.Mutator],
      context: Context = defaultContext,
      initialState: State = defaultState
  ): ValidationSummary = {
    var allFailures = List.empty[ValidationFailure]
    var totalTxs = 0
    var totalValidations = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0
    var stoppedMutators = List.empty[String]

    // Run each mutator separately
    for mutator <- mutators do
      var consecutiveMissingUtxos = 0

      for (blockPath, blockIdx) <- blockPaths.zipWithIndex
          if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
      do
        val (blockFile, blockBytes) = loadBlock(blockPath)
        given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
        val block = blockFile.block
        var state = initialState
        if blockIdx == 0 then processedBlocks += 1

        for (tx, txIdx) <- block.transactions.zipWithIndex do
          if blockIdx == 0 then totalTxs += 1
          totalValidations += 1

          try
            val utxos = scalusUtxoResolver.resolveUtxos(tx)
            val txState = state.copy(utxos = state.utxos ++ utxos)
            consecutiveMissingUtxos = 0 // Reset on success

            mutator(context, txState, tx) match {
              case Left(error) =>
                allFailures =
                  ValidationFailure(blockPath, txIdx, tx.id, mutator.name, error) :: allFailures
              case Right(newState) =>
                state = newState
            }
          catch
            case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
              // Skip transactions with missing UTxOs
              consecutiveMissingUtxos += 1
              if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
                stoppedMutators = mutator.name :: stoppedMutators

    if stoppedMutators.nonEmpty then
      println(s"⚠ Stopped ${stoppedMutators.size} mutator(s) due to consecutive missing UTxO errors")

    ValidationSummary(
      processedBlocks,
      totalTxs / mutators.size, // Adjust for multiple passes
      totalValidations,
      allFailures.reverse
    )
  }
}

/** Test suite for pure validators only */
class LedgerRulesPureValidatorsTest extends LedgerRulesValidationTestBase {

  test("validate transactions with composed STS chain - pure validators") {
    val summary = validateWithComposedChain(pureValidators)
    println(summary.report)
    // assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }

  test("validate transactions in single pass - pure validators") {
    val summary = validateWithSinglePass(pureValidators)
    println(summary.report)
    //assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }

  test("validate transactions with separate collections - pure validators") {
    val summary = validateWithSeparateCollections(pureValidators)
    println(summary.report)
    // assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }
}

/** Test suite for mutators only */
class LedgerRulesMutatorsTest extends LedgerRulesValidationTestBase {

  test("validate transactions with composed STS chain - mutators") {
    val summary = validateMutatorsWithComposedChain(mutators)
    println(summary.report)
    assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }

  test("validate transactions in single pass - mutators") {
    val summary = validateMutatorsWithSinglePass(mutators)
    println(summary.report)
    assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }

  test("validate transactions with separate collections - mutators") {
    val summary = validateMutatorsWithSeparateCollections(mutators)
    println(summary.report)
    assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }
}

/** Test suite for all validators (pure + mutators) */
class LedgerRulesAllValidatorsTest extends LedgerRulesValidationTestBase {

  private lazy val allValidators: List[STS] = pureValidators ++ mutators

  test("validate transactions with all validators - mixed approach") {
    // For all validators (pure + mutators), we need a mixed approach
    // Run pure validators first, then mutators
    var failures = List.empty[ValidationFailure]
    var totalTxs = 0
    var totalValidations = 0
    var consecutiveMissingUtxos = 0
    val maxConsecutiveMissingUtxos = 10

    val blockPaths = getAllBlocksPaths()
    var processedBlocks = 0

    for (blockPath, blockIdx) <- blockPaths.zipWithIndex
        if consecutiveMissingUtxos < maxConsecutiveMissingUtxos
    do
      val (blockFile, blockBytes) = loadBlock(blockPath)
      given OriginalCborByteArray = OriginalCborByteArray(blockBytes)
      val block = blockFile.block
      var state = defaultState
      processedBlocks += 1

      for (tx, txIdx) <- block.transactions.zipWithIndex do
        totalTxs += 1
        try
          val utxos = scalusUtxoResolver.resolveUtxos(tx)
          val txState = state.copy(utxos = state.utxos ++ utxos)
          consecutiveMissingUtxos = 0 // Reset on success

          // Run pure validators
          for validator <- pureValidators do
            totalValidations += 1
            validator(defaultContext, txState, tx) match {
              case Left(error) =>
                failures =
                  ValidationFailure(blockPath, txIdx, tx.id, validator.name, error) :: failures
              case Right(_) => // success
            }

          // Run mutators
          var currentState = txState
          for mutator <- mutators do
            totalValidations += 1
            mutator(defaultContext, currentState, tx) match {
              case Left(error) =>
                failures =
                  ValidationFailure(blockPath, txIdx, tx.id, mutator.name, error) :: failures
              case Right(newState) =>
                currentState = newState
            }
          state = currentState

        catch
          case e: IllegalStateException if e.getMessage.contains("UTXO not found") =>
            // Skip transactions with missing UTxOs
            consecutiveMissingUtxos += 1
            if consecutiveMissingUtxos >= maxConsecutiveMissingUtxos then
              println(
                s"⚠ Stopping validation: ${consecutiveMissingUtxos} consecutive missing UTxO errors"
              )

    val summary = ValidationSummary(
      processedBlocks,
      totalTxs,
      totalValidations,
      failures.reverse
    )
    println(summary.report)
    // assert(summary.failureCount == 0, s"Found ${summary.failureCount} failures")
  }
}
