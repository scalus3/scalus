package scalus.testing

import org.scalacheck.Gen
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.Data

import scala.concurrent.{ExecutionContext, Future}

/** Standard transaction variation patterns for property-based and exhaustive testing.
  *
  * This object provides factory methods for creating common transaction variation patterns. Each
  * method returns a `TxVariations[S]` or `TxSamplingVariations[S]` that can be combined with other
  * variations using the `++` operator.
  *
  * Variations are parameterized by a state type `S` and use lambdas to extract state-dependent
  * values. This makes them generic and reusable across different contract scenarios.
  *
  * ==Example Usage==
  * {{{
  * case class AuctionState(utxo: Utxo, currentBid: Value)
  *
  * val stealVariation = StandardTxVariations.removeContractOutput[AuctionState](
  *   extractUtxo = _.utxo,
  *   redeemer = _ => BidRedeemer.toData,
  *   script = auctionScript
  * )
  *
  * // Combine with other variations
  * val allVariations = stealVariation ++ corruptDatumVariation
  *
  * // Use in testing
  * stealVariation.enumerate(provider, state, sponsor, signer)
  * }}}
  */
object StandardTxVariations {

    // === Default Combined Variations ===

    /** Default combined variations for testing common attack vectors.
      *
      * This method creates a combined `TxVariations` that includes the most common attack patterns
      * that every spending validator should protect against:
      *   - `removeContractOutput`: Steal attack - spend without returning funds
      *   - `duplicateOutput`: Double output attack - split value into two outputs
      *   - `stealPartialValue`: Partial theft - return less value than expected
      *
      * These variations require minimal configuration and cover the most critical vulnerabilities.
      * The output address is automatically taken from the UTXO being spent.
      *
      * ==Example Usage==
      * {{{
      * case class ContractState(utxo: Utxo)
      *
      * val variations = StandardTxVariations.default[ContractState](
      *   extractUtxo = _.utxo,
      *   extractDatum = s => s.utxo.output.requireInlineDatum,
      *   redeemer = _ => MyRedeemer.toData,
      *   script = myScript
      * )
      *
      * // Enumerate all default attack variations
      * variations.enumerate(provider, state, sponsor, signer)
      * }}}
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param extractDatum
      *   function to extract the datum for the output from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a combined TxVariations with all default attack patterns
      */
    def default[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S] = {
        removeContractOutput(extractUtxo, redeemer, script) ++
            duplicateOutput(extractUtxo, extractDatum, redeemer, script) ++
            stealPartialValue(
              extractUtxo,
              s => extractUtxo(s).output.value,
              extractDatum,
              redeemer,
              script
            )
    }

    /** Extended default variations with corrupted datum testing.
      *
      * This extends `default` with additional variations:
      *   - All variations from `default`
      *   - `corruptDatum`: Return correct value but with corrupted datum
      *   - `wrongOutputAddress`: Send to wrong recipient address
      *
      * Use this when you have generators for invalid datums and alternative addresses. The output
      * address is automatically taken from the UTXO being spent.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param extractDatum
      *   function to extract the datum for the output from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @param corruptedDatums
      *   function generating invalid datum values from state
      * @param alternativeAddresses
      *   function generating alternative addresses from state
      * @return
      *   a combined TxVariations with extended attack patterns
      */
    def defaultExtended[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript,
        corruptedDatums: S => Gen[Data],
        alternativeAddresses: S => Gen[Address]
    ): TxVariations[S] = {
        default(extractUtxo, extractDatum, redeemer, script) ++
            corruptDatum(extractUtxo, corruptedDatums, redeemer, script) ++
            wrongOutputAddress(extractUtxo, alternativeAddresses, extractDatum, redeemer, script)
    }

    // === Output Variations ===

    /** Remove expected output - funds go to change (steal attack).
      *
      * This variation spends the contract UTXO but doesn't create the expected output, effectively
      * stealing the funds to the sponsor's change address.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxVariations that builds the steal transaction
      */
    def removeContractOutput[S](
        extractUtxo: S => Utxo,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo = extractUtxo(state)
            val red = redeemer(state)
            val env = provider.cardanoInfo

            TxBuilder(env)
                .spend(utxo, red, script)
                .complete(provider, sponsor)
                .map(b => Seq(b.sign(signer).transaction))
        }
    }

    /** Return partial value - less than expected.
      *
      * This variation returns only a portion of the expected value back to the contract, attempting
      * to steal the difference. The output address is taken from the UTXO being spent.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param extractExpectedValue
      *   function to extract the expected return value from state
      * @param extractDatum
      *   function to extract the datum for the output from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxSamplingVariations that generates partial value transactions
      */
    def stealPartialValue[S](
        extractUtxo: S => Utxo,
        extractExpectedValue: S => Value,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val expectedValue = extractExpectedValue(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val env = provider.cardanoInfo

            // Generate partial amounts (1% to 99% of expected)
            val percentageGen = Gen.choose(1, 99)

            percentageGen.map { percentage =>
                val partialCoin = Coin((expectedValue.coin.value * percentage) / 100)
                val partialValue = Value(partialCoin, expectedValue.assets)

                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(utxo.output.address, partialValue, datum)
                    .complete(provider, sponsor)
                    .map(_.sign(signer).transaction)(using provider.executionContext)
            }
        }
    }

    /** Corrupt datum - return correct value but wrong datum.
      *
      * This variation returns the expected value to the contract but with a corrupted or invalid
      * datum, testing datum validation. The output address is taken from the UTXO being spent.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param corruptedDatums
      *   function generating invalid datum values from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxSamplingVariations that generates corrupted datum transactions
      */
    def corruptDatum[S](
        extractUtxo: S => Utxo,
        corruptedDatums: S => Gen[Data],
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val red = redeemer(state)
            val datumGen = corruptedDatums(state)
            val env = provider.cardanoInfo

            datumGen.map { badDatum =>
                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(utxo.output.address, utxo.output.value, badDatum)
                    .complete(provider, sponsor)
                    .map(_.sign(signer).transaction)(using provider.executionContext)
            }
        }
    }

    /** Wrong output address - send to wrong recipient.
      *
      * This variation sends the contract output to a different address than expected, testing
      * address validation.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param addresses
      *   function generating alternative addresses from state
      * @param extractDatum
      *   function to extract the datum for the output from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxSamplingVariations that generates wrong address transactions
      */
    def wrongOutputAddress[S](
        extractUtxo: S => Utxo,
        addresses: S => Gen[Address],
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val addressGen = addresses(state)
            val env = provider.cardanoInfo

            addressGen.map { wrongAddress =>
                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(wrongAddress, utxo.output.value, datum)
                    .complete(provider, sponsor)
                    .map(_.sign(signer).transaction)(using provider.executionContext)
            }
        }
    }

    /** Duplicate output - split value into two outputs.
      *
      * This variation creates two outputs instead of one, splitting the value between them. Tests
      * that the contract properly validates output uniqueness. The output address is taken from the
      * UTXO being spent.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param extractDatum
      *   function to extract the datum for the outputs from state
      * @param redeemer
      *   function to extract the redeemer data from state
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxVariations that builds the duplicate output transaction
      */
    def duplicateOutput[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo = extractUtxo(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val env = provider.cardanoInfo

            // Split the value in half for two outputs
            val halfCoin = Coin(utxo.output.value.coin.value / 2)
            val halfValue = Value(halfCoin, utxo.output.value.assets)

            TxBuilder(env)
                .spend(utxo, red, script)
                .payTo(utxo.output.address, halfValue, datum)
                .payTo(utxo.output.address, halfValue, datum)
                .complete(provider, sponsor)
                .map(b => Seq(b.sign(signer).transaction))
        }
    }

    // === Minting Variations ===

    /** Unauthorized mint - mint without proper authorization.
      *
      * This variation attempts to mint tokens without satisfying the minting policy requirements.
      *
      * @param baseTxBuilder
      *   function to build the base transaction from provider, state, and sponsor
      * @param policyId
      *   the minting policy ID
      * @param assetName
      *   the name of the asset to mint
      * @param mintAmounts
      *   generator for mint amounts to try
      * @param redeemer
      *   the redeemer for the minting policy
      * @param script
      *   the minting policy script
      * @return
      *   a TxSamplingVariations that generates unauthorized mint transactions
      */
    def unauthorizedMint[S](
        baseTxBuilder: (BlockchainProvider, S, Address) => Future[TxBuilder],
        policyId: PolicyId,
        assetName: AssetName,
        mintAmounts: Gen[Long],
        redeemer: Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            implicit val ec: ExecutionContext = provider.executionContext

            mintAmounts.map { amount =>
                baseTxBuilder(provider, state, sponsor).flatMap { builder =>
                    builder
                        .mint(script, Map(assetName -> amount), redeemer)
                        .complete(provider, sponsor)
                        .map(_.sign(signer).transaction)
                }
            }
        }
    }

    /** Mint extra tokens beyond allowed.
      *
      * This variation mints more tokens than the policy should allow, testing quantity validation.
      *
      * @param baseTxBuilder
      *   function to build the base transaction from provider, state, and sponsor
      * @param policyId
      *   the minting policy ID
      * @param assetName
      *   the name of the asset to mint
      * @param expectedAmount
      *   function to extract the expected mint amount from state
      * @param extraAmounts
      *   generator for extra amounts to add
      * @param redeemer
      *   the redeemer for the minting policy
      * @param script
      *   the minting policy script
      * @return
      *   a TxSamplingVariations that generates extra mint transactions
      */
    def mintExtra[S](
        baseTxBuilder: (BlockchainProvider, S, Address) => Future[TxBuilder],
        policyId: PolicyId,
        assetName: AssetName,
        expectedAmount: S => Long,
        extraAmounts: Gen[Long],
        redeemer: Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val expected = expectedAmount(state)
            implicit val ec: ExecutionContext = provider.executionContext

            extraAmounts.map { extra =>
                baseTxBuilder(provider, state, sponsor).flatMap { builder =>
                    builder
                        .mint(script, Map(assetName -> (expected + extra)), redeemer)
                        .complete(provider, sponsor)
                        .map(_.sign(signer).transaction)
                }
            }
        }
    }

    // === Timing Variations ===

    /** Test around a deadline boundary.
      *
      * This variation generates transactions with validity intervals that cross the deadline
      * boundary, testing time-sensitive contract logic.
      *
      * @param extractDeadline
      *   function to extract the deadline slot from state
      * @param baseTxBuilder
      *   function to build the base transaction with a specific validity slot
      * @return
      *   a TxSamplingVariations that generates deadline boundary transactions
      */
    def aroundDeadline[S](
        extractDeadline: S => Long,
        baseTxBuilder: (BlockchainProvider, S, Address, Long) => Future[TxBuilder]
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val deadline = extractDeadline(state)
            implicit val ec: ExecutionContext = provider.executionContext

            slotsAround(deadline).map { slot =>
                baseTxBuilder(provider, state, sponsor, slot).flatMap { builder =>
                    builder
                        .complete(provider, sponsor)
                        .map(_.sign(signer).transaction)
                }
            }
        }
    }

    /** No validity range - missing time constraints.
      *
      * This variation builds a transaction without setting validity bounds, testing that contracts
      * properly require time constraints.
      *
      * @param baseTxBuilder
      *   function to build the base transaction from provider, state, and sponsor
      * @return
      *   a TxVariations that builds transactions without validity ranges
      */
    def noValidityRange[S](
        baseTxBuilder: (BlockchainProvider, S, Address) => Future[TxBuilder]
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            baseTxBuilder(provider, state, sponsor).flatMap { builder =>
                // Build without setting any validity interval
                builder
                    .complete(provider, sponsor)
                    .map(b => Seq(b.sign(signer).transaction))
            }
        }
    }

    /** Wide validity range - overly permissive time bounds.
      *
      * This variation sets a very wide validity interval (1 year), testing that contracts properly
      * constrain time bounds.
      *
      * @param baseTxBuilder
      *   function to build the base transaction from provider, state, and sponsor
      * @return
      *   a TxVariations that builds transactions with wide validity ranges
      */
    def wideValidityRange[S](
        baseTxBuilder: (BlockchainProvider, S, Address) => Future[TxBuilder]
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val env = provider.cardanoInfo
            val currentSlot = env.slotConfig.instantToSlot(java.time.Instant.now()).toLong
            // Set validity range to 1 year (approximately 31536000 seconds / slot length)
            val oneYearInSlots = 31536000L // roughly 1 year in slots at 1 second/slot

            baseTxBuilder(provider, state, sponsor).flatMap { builder =>
                builder
                    .validDuring(
                      ValidityInterval(Some(currentSlot - 1000), Some(currentSlot + oneYearInSlots))
                    )
                    .complete(provider, sponsor)
                    .map(b => Seq(b.sign(signer).transaction))
            }
        }
    }

    // === Redeemer Variations ===

    /** Wrong redeemer - use incorrect action/data.
      *
      * This variation uses an incorrect redeemer, testing that the contract properly validates
      * redeemer data.
      *
      * @param extractUtxo
      *   function to extract the UTXO to spend from state
      * @param redeemers
      *   function generating alternative redeemer values from state
      * @param outputBuilder
      *   function to add outputs to the transaction builder
      * @param script
      *   the Plutus script protecting the UTXO
      * @return
      *   a TxSamplingVariations that generates wrong redeemer transactions
      */
    def wrongRedeemer[S](
        extractUtxo: S => Utxo,
        redeemers: S => Gen[Data],
        outputBuilder: (BlockchainProvider, S, Address) => TxBuilder => TxBuilder,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val redeemerGen = redeemers(state)
            val env = provider.cardanoInfo
            implicit val ec: ExecutionContext = provider.executionContext

            redeemerGen.map { wrongRedeemer =>
                val builder = TxBuilder(env)
                    .spend(utxo, wrongRedeemer, script)

                val withOutputs = outputBuilder(provider, state, sponsor)(builder)

                withOutputs
                    .complete(provider, sponsor)
                    .map(_.sign(signer).transaction)
            }
        }
    }

    // === Value Boundary Variations ===

    /** Test around a value threshold.
      *
      * This variation generates transactions with values near a threshold, testing boundary
      * conditions in value validation.
      *
      * @param extractThreshold
      *   function to extract the threshold value from state
      * @param txWithAmount
      *   function to build a transaction with a specific amount
      * @return
      *   a TxSamplingVariations that generates threshold boundary transactions
      */
    def aroundThreshold[S](
        extractThreshold: S => Coin,
        txWithAmount: (BlockchainProvider, S, Address, Coin) => Future[TxBuilder]
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val threshold = extractThreshold(state)
            implicit val ec: ExecutionContext = provider.executionContext

            valuesAround(threshold).map { amount =>
                txWithAmount(provider, state, sponsor, amount).flatMap { builder =>
                    builder
                        .complete(provider, sponsor)
                        .map(_.sign(signer).transaction)
                }
            }
        }
    }

    // === Reference Input Variations ===

    /** Remove required reference input.
      *
      * This variation builds a transaction without a required reference input, testing that the
      * contract properly validates reference inputs.
      *
      * @param baseTxWithoutRef
      *   function to build the transaction without the reference input
      * @return
      *   a TxVariations that builds transactions missing reference inputs
      */
    def removeReferenceInput[S](
        baseTxWithoutRef: (BlockchainProvider, S, Address) => Future[TxBuilder]
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            baseTxWithoutRef(provider, state, sponsor).flatMap { builder =>
                builder
                    .complete(provider, sponsor)
                    .map(b => Seq(b.sign(signer).transaction))
            }
        }
    }

    /** Use wrong/stale reference input.
      *
      * This variation uses an incorrect or stale reference input, testing that the contract
      * properly validates reference input data.
      *
      * @param baseTxBuilder
      *   function to build the base transaction from provider, state, and sponsor
      * @param wrongRefs
      *   function generating alternative reference inputs from state
      * @return
      *   a TxSamplingVariations that generates wrong reference input transactions
      */
    def wrongReferenceInput[S](
        baseTxBuilder: (BlockchainProvider, S, Address) => Future[TxBuilder],
        wrongRefs: S => Gen[Utxo]
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        ): Gen[Future[Transaction]] = {
            val refGen = wrongRefs(state)
            implicit val ec: ExecutionContext = provider.executionContext

            refGen.map { wrongRef =>
                baseTxBuilder(provider, state, sponsor).flatMap { builder =>
                    builder
                        .references(wrongRef)
                        .complete(provider, sponsor)
                        .map(_.sign(signer).transaction)
                }
            }
        }
    }

    // === Double Satisfaction ===

    /** Double satisfaction attack - satisfy one validator, steal from another.
      *
      * This variation attempts a double satisfaction attack by spending two UTXOs protected by the
      * same script but only providing the correct output for one of them. The output address is
      * taken from the first UTXO being spent.
      *
      * @param extractFirstUtxo
      *   function to extract the first UTXO to spend from state
      * @param extractSecondUtxo
      *   function to extract the second UTXO to spend from state
      * @param firstRedeemer
      *   function to extract the redeemer for the first UTXO from state
      * @param secondRedeemer
      *   function to extract the redeemer for the second UTXO from state
      * @param outputValue
      *   function to extract the single output value (for one UTXO only)
      * @param outputDatum
      *   function to extract the output datum from state
      * @param script
      *   the Plutus script protecting the UTXOs
      * @return
      *   a TxVariations that builds double satisfaction attack transactions
      */
    def doubleSatisfaction[S](
        extractFirstUtxo: S => Utxo,
        extractSecondUtxo: S => Utxo,
        firstRedeemer: S => Data,
        secondRedeemer: S => Data,
        outputValue: S => Value,
        outputDatum: S => Data,
        script: PlutusScript
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            provider: BlockchainProvider,
            state: S,
            sponsor: Address,
            signer: TransactionSigner
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo1 = extractFirstUtxo(state)
            val utxo2 = extractSecondUtxo(state)
            val red1 = firstRedeemer(state)
            val red2 = secondRedeemer(state)
            val value = outputValue(state)
            val datum = outputDatum(state)
            val env = provider.cardanoInfo

            TxBuilder(env)
                .spend(utxo1, red1, script)
                .spend(utxo2, red2, script)
                // Only provide output for one UTXO, stealing the other
                .payTo(utxo1.output.address, value, datum)
                .complete(provider, sponsor)
                .map(b => Seq(b.sign(signer).transaction))
        }
    }

    // === Boundary Generators (helpers) ===

    /** Generate values around a threshold.
      *
      * Generates Coin values at and near the threshold: exactly at threshold, 1 lovelace below, 1
      * lovelace above, 10% below, and 10% above.
      *
      * @param threshold
      *   the threshold value to generate around
      * @return
      *   a Gen that produces Coin values near the threshold
      */
    def valuesAround(threshold: Coin): Gen[Coin] = {
        val below = math.max(0L, threshold.value - 1)
        val above = threshold.value + 1
        val tenPercentBelow = math.max(0L, (threshold.value * 9) / 10)
        val tenPercentAbove = (threshold.value * 11) / 10

        Gen.oneOf(
          Coin(threshold.value), // exactly at threshold
          Coin(below), // 1 lovelace below
          Coin(above), // 1 lovelace above
          Coin(tenPercentBelow), // 10% below
          Coin(tenPercentAbove) // 10% above
        )
    }

    /** Generate slots around a deadline.
      *
      * Generates slot numbers at and near the deadline: exactly at deadline, 1 slot before, 1 slot
      * after, 10 slots before, and 10 slots after.
      *
      * @param deadline
      *   the deadline slot to generate around
      * @return
      *   a Gen that produces slot numbers near the deadline
      */
    def slotsAround(deadline: Long): Gen[Long] = {
        val before = math.max(0L, deadline - 1)
        val after = deadline + 1
        val tenBefore = math.max(0L, deadline - 10)
        val tenAfter = deadline + 10

        Gen.oneOf(
          deadline, // exactly at deadline
          before, // 1 slot before
          after, // 1 slot after
          tenBefore, // 10 slots before
          tenAfter // 10 slots after
        )
    }
}
