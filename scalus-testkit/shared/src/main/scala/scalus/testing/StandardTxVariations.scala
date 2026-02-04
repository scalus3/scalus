package scalus.testing

import org.scalacheck.Gen
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainReader
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.Data

import scala.concurrent.{ExecutionContext, Future}

/** Standard transaction variation patterns for property-based and exhaustive testing.
  *
  * This object provides factory methods for creating common transaction variation patterns. Each
  * method returns a `TxVariations[S]` or `TxSamplingVariations[S]` that can be combined with other
  * variations using the `++` operator.
  *
  * Variations receive a `TxTemplate` containing the base transaction builder, sponsor, and signer.
  * They modify the builder or create new transactions using the template's sponsor/signer.
  *
  * ==Example Usage==
  * {{{
  * val stealVariation = StandardTxVariations.removeContractOutput[AuctionState](
  *   extractUtxo = _.utxo,
  *   redeemer = _ => BidRedeemer.toData,
  *   script = auctionScript
  * )
  *
  * // Combine with other variations
  * val allVariations = stealVariation ++ corruptDatumVariation
  *
  * // Use in testing - txTemplate provides base tx, sponsor, signer
  * stealVariation.enumerate(provider, state, txTemplate)
  * }}}
  */
object StandardTxVariations {

    // === Default Combined Variations ===

    /** Default combined variations for testing common attack vectors.
      *
      * Includes:
      *   - `removeContractOutput`: Steal attack - spend without returning funds
      *   - `duplicateOutput`: Double output attack - split value into two outputs
      *   - `stealPartialValue`: Partial theft - return less value than expected
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

    /** Extended default variations with corrupted datum and wrong address testing. */
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

    /** Remove expected output - funds go to change (steal attack). */
    def removeContractOutput[S](
        extractUtxo: S => Utxo,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo = extractUtxo(state)
            val red = redeemer(state)
            val env = reader.cardanoInfo

            TxBuilder(env)
                .spend(utxo, red, script)
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    /** Return partial value - less than expected. */
    def stealPartialValue[S](
        extractUtxo: S => Utxo,
        extractExpectedValue: S => Value,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val expectedValue = extractExpectedValue(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val env = reader.cardanoInfo

            Gen.choose(1, 99).map { percentage =>
                val partialCoin = Coin((expectedValue.coin.value * percentage) / 100)
                val partialValue = Value(partialCoin, expectedValue.assets)

                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(utxo.output.address, partialValue, datum)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)(using reader.executionContext)
            }
        }
    }

    /** Corrupt datum - return correct value but wrong datum. */
    def corruptDatum[S](
        extractUtxo: S => Utxo,
        corruptedDatums: S => Gen[Data],
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val red = redeemer(state)
            val datumGen = corruptedDatums(state)
            val env = reader.cardanoInfo

            datumGen.map { badDatum =>
                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(utxo.output.address, utxo.output.value, badDatum)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)(using reader.executionContext)
            }
        }
    }

    /** Wrong output address - send to wrong recipient. */
    def wrongOutputAddress[S](
        extractUtxo: S => Utxo,
        addresses: S => Gen[Address],
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val addressGen = addresses(state)
            val env = reader.cardanoInfo

            addressGen.map { wrongAddress =>
                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(wrongAddress, utxo.output.value, datum)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)(using reader.executionContext)
            }
        }
    }

    /** Duplicate output - split value into two outputs. */
    def duplicateOutput[S](
        extractUtxo: S => Utxo,
        extractDatum: S => Data,
        redeemer: S => Data,
        script: PlutusScript
    ): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo = extractUtxo(state)
            val datum = extractDatum(state)
            val red = redeemer(state)
            val env = reader.cardanoInfo

            val halfCoin = Coin(utxo.output.value.coin.value / 2)
            val halfValue = Value(halfCoin, utxo.output.value.assets)

            TxBuilder(env)
                .spend(utxo, red, script)
                .payTo(utxo.output.address, halfValue, datum)
                .payTo(utxo.output.address, halfValue, datum)
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    // === Minting Variations ===

    /** Unauthorized mint - add minting to base transaction. */
    def unauthorizedMint[S](
        assetName: AssetName,
        mintAmounts: Gen[Long],
        redeemer: Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            implicit val ec: ExecutionContext = reader.executionContext

            mintAmounts.map { amount =>
                txTemplate.builder
                    .mint(script, Map(assetName -> amount), redeemer)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    /** Mint extra tokens beyond allowed. */
    def mintExtra[S](
        assetName: AssetName,
        expectedAmount: S => Long,
        extraAmounts: Gen[Long],
        redeemer: Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val expected = expectedAmount(state)
            implicit val ec: ExecutionContext = reader.executionContext

            extraAmounts.map { extra =>
                txTemplate.builder
                    .mint(script, Map(assetName -> (expected + extra)), redeemer)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    // === Timing Variations ===

    /** Test around a deadline boundary - modifies validity interval of base tx. */
    def aroundDeadline[S](
        extractDeadline: S => Long
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val deadline = extractDeadline(state)
            implicit val ec: ExecutionContext = reader.executionContext

            slotsAround(deadline).map { slot =>
                txTemplate.builder
                    .validDuring(ValidityInterval(Some(slot - 100), Some(slot + 100)))
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    /** No validity range - removes time constraints from base tx. */
    def noValidityRange[S](): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            // Use base builder without setting validity interval
            txTemplate.builder
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    /** Wide validity range - sets overly permissive time bounds on base tx. */
    def wideValidityRange[S](): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val env = reader.cardanoInfo
            val currentSlot = env.slotConfig.instantToSlot(java.time.Instant.now()).toLong
            val oneYearInSlots = 31536000L

            txTemplate.builder
                .validDuring(
                  ValidityInterval(Some(currentSlot - 1000), Some(currentSlot + oneYearInSlots))
                )
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    // === Redeemer Variations ===

    /** Wrong redeemer - use incorrect redeemer with base tx outputs. */
    def wrongRedeemer[S](
        extractUtxo: S => Utxo,
        redeemers: S => Gen[Data],
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val utxo = extractUtxo(state)
            val redeemerGen = redeemers(state)
            val env = reader.cardanoInfo
            implicit val ec: ExecutionContext = reader.executionContext

            redeemerGen.map { wrongRed =>
                // Replace the spend with wrong redeemer, keep other parts from template
                TxBuilder(env)
                    .spend(utxo, wrongRed, script)
                    .payTo(utxo.output.address, utxo.output.value)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    // === Value Boundary Variations ===

    /** Test around a value threshold - modifies output value in base tx. */
    def aroundThreshold[S](
        extractThreshold: S => Coin,
        extractUtxo: S => Utxo,
        redeemer: S => Data,
        script: PlutusScript
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val threshold = extractThreshold(state)
            val utxo = extractUtxo(state)
            val red = redeemer(state)
            val env = reader.cardanoInfo
            implicit val ec: ExecutionContext = reader.executionContext

            valuesAround(threshold).map { amount =>
                TxBuilder(env)
                    .spend(utxo, red, script)
                    .payTo(utxo.output.address, Value(amount))
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    // === Reference Input Variations ===

    /** Remove required reference input from base tx. */
    def removeReferenceInput[S](): TxVariations[S] = new TxVariations[S] {
        override def enumerate(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            // Use base builder - assumes it was built without the required reference
            txTemplate.builder
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    /** Add wrong/stale reference input to base tx. */
    def wrongReferenceInput[S](
        wrongRefs: S => Gen[Utxo]
    ): TxSamplingVariations[S] = new TxSamplingVariations[S] {
        override def gen(
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        ): Gen[Future[Transaction]] = {
            val refGen = wrongRefs(state)
            implicit val ec: ExecutionContext = reader.executionContext

            refGen.map { wrongRef =>
                txTemplate.builder
                    .references(wrongRef)
                    .complete(reader, txTemplate.sponsor)
                    .map(_.sign(txTemplate.signer).transaction)
            }
        }
    }

    // === Double Satisfaction ===

    /** Double satisfaction attack - spend two UTXOs, only satisfy one. */
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
            reader: BlockchainReader,
            state: S,
            txTemplate: TxTemplate
        )(using ExecutionContext): Future[Seq[Transaction]] = {
            val utxo1 = extractFirstUtxo(state)
            val utxo2 = extractSecondUtxo(state)
            val red1 = firstRedeemer(state)
            val red2 = secondRedeemer(state)
            val value = outputValue(state)
            val datum = outputDatum(state)
            val env = reader.cardanoInfo

            TxBuilder(env)
                .spend(utxo1, red1, script)
                .spend(utxo2, red2, script)
                .payTo(utxo1.output.address, value, datum)
                .complete(reader, txTemplate.sponsor)
                .map(b => Seq(b.sign(txTemplate.signer).transaction))
        }
    }

    // === Boundary Generators ===

    /** Generate values around a threshold. */
    def valuesAround(threshold: Coin): Gen[Coin] = {
        val below = math.max(0L, threshold.value - 1)
        val above = threshold.value + 1
        val tenPercentBelow = math.max(0L, (threshold.value * 9) / 10)
        val tenPercentAbove = (threshold.value * 11) / 10

        Gen.oneOf(
          Coin(threshold.value),
          Coin(below),
          Coin(above),
          Coin(tenPercentBelow),
          Coin(tenPercentAbove)
        )
    }

    /** Generate slots around a deadline. */
    def slotsAround(deadline: Long): Gen[Long] = {
        val before = math.max(0L, deadline - 1)
        val after = deadline + 1
        val tenBefore = math.max(0L, deadline - 10)
        val tenAfter = deadline + 10

        Gen.oneOf(deadline, before, after, tenBefore, tenAfter)
    }
}
