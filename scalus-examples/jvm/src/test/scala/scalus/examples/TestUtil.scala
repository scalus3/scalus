package scalus.examples

import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{Environment, PubKeyWitness, TransactionUnspentOutput, Wallet as WalletTrait, Witness}
import scalus.ledger.api.v3
import scalus.uplc.Program
import scalus.uplc.eval.ExBudget
import scalus.cardano.node.Provider
import scalus.testing.kit.ScalusTest

object TestUtil extends ScalusTest {

    val testProtocolParams: ProtocolParams = CardanoInfo.mainnet.protocolParams

    val testEnvironmentWithoutEvaluator: Environment = Environment(
      cardanoInfo = CardanoInfo.mainnet,
      evaluator = (_: Transaction, _: Map[TransactionInput, TransactionOutput]) => Seq.empty,
    )

    val testEnvironmentWithEvaluator: Environment = Environment(
      cardanoInfo = CardanoInfo.mainnet,
      evaluator = PlutusScriptEvaluator(
        slotConfig = CardanoInfo.mainnet.slotConfig,
        initialBudget = ExBudget.enormous,
        protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
        costModels = testProtocolParams.costModels
      ),
    )

    def createTestAddress(keyHash: String): ShelleyAddress = {
        ShelleyAddress(
          network = CardanoInfo.mainnet.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(ByteString.fromHex(keyHash))),
          delegation = ShelleyDelegationPart.Null
        )
    }

    def createTestWallet(address: Address, ada: BigInt): WalletTrait = new WalletTrait {
        private val testInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("0" * 64)),
          0
        )
        private val testOutput = TransactionOutput.Babbage(
          address = address,
          value = Value(Coin(ada.toLong)),
          datumOption = None,
          scriptRef = None
        )
        private val txUnspentOutput = TransactionUnspentOutput(testInput, testOutput)

        override def owner: Address = address
        override def utxo: Utxos = Map((testInput, testOutput))
        override def collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq(
          (txUnspentOutput, PubKeyWitness)
        )
        override def selectInputs(
            required: Value
        ): Option[Seq[(TransactionUnspentOutput, Witness)]] = {
            val available = testOutput.value
            if available.coin.value >= required.coin.value then {
                Some(Seq((txUnspentOutput, PubKeyWitness)))
            } else {
                None
            }
        }
    }

    def createTestWallet(provider: Provider, address: Address): WalletTrait =
        new WalletTrait {
            override val owner: Address = address
            override lazy val utxo: Utxos = provider.findUtxos(address).toOption.get
            override val collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq(
              (TransactionUnspentOutput(provider.findUtxo(address).toOption.get), PubKeyWitness)
            )

            override def selectInputs(
                required: Value
            ): Option[Seq[(TransactionUnspentOutput, Witness)]] = {
                val available = provider.findUtxos(address, minRequiredAmount = Some(required.coin))
                available match
                    case Left(_) => None
                    case Right(utxos) =>
                        Some(
                          utxos.view.map { entry =>
                              (TransactionUnspentOutput(entry), PubKeyWitness)
                          }.toSeq
                        )
            }
        }

    def getScriptUtxo(tx: Transaction): (TransactionInput, TransactionOutput) = {
        val (transactionOutput, index) = tx.body.value.outputs.view
            .map(_.value)
            .zipWithIndex
            .find { (transactionOutput, _) => transactionOutput.address.hasScript }
            .getOrElse(throw new Exception("No script output found in transaction"))

        (TransactionInput(tx.id, index), transactionOutput)
    }

    def findUtxoByAddressAndDatum(
        tx: Transaction,
        address: Address,
        datum: Option[DatumOption] = None
    ): Option[(TransactionInput, TransactionOutput)] = {
        tx.body.value.outputs.view
            .map(_.value)
            .zipWithIndex
            .find { (transactionOutput, _) =>
                address == transactionOutput.address && (
                  (datum, transactionOutput.datumOption) match
                      case (Some(d1), Some(d2)) => d1.contentEquals(d2)
                      case (None, None)         => true
                      case _                    => false
                )
            }
            .map { (transactionOutput, index) =>
                (TransactionInput(tx.id, index), transactionOutput)
            }
    }

    def extractDatumFromOutput(
        tx: Transaction,
        output: TransactionOutput
    ): Option[Data] = {
        def getDatum(dataHash: DataHash) =
            tx.witnessSet.plutusData.value.toMap
                .get(dataHash)
                .map(_.value)

        output match
            case TransactionOutput.Shelley(_, _, Some(datumHash)) =>
                getDatum(datumHash)
            case TransactionOutput.Babbage(_, _, datumOption, _) =>
                datumOption match
                    case Some(DatumOption.Hash(hash))   => getDatum(hash)
                    case Some(DatumOption.Inline(data)) => Some(data)
                    case None                           => None
            case _ => None
    }

    def getScriptContextV3(
        tx: Transaction,
        utxos: Utxos,
        input: TransactionInput,
        redeemerTag: RedeemerTag = RedeemerTag.Spend,
        environment: Environment = testEnvironmentWithoutEvaluator
    ): v3.ScriptContext = {
        val inputs = tx.body.value.inputs
        // assume 1 script input
        val inputIdx = inputs.toSeq.indexWhere(_ == input)

        val redeemersMap = tx.witnessSet.redeemers.get.value.toMap
        val (data, exUnits) = redeemersMap.getOrElse(
          (redeemerTag, inputIdx),
          throw new Exception(s"No redeemer found for $redeemerTag input at index $inputIdx")
        )
        val redeemer = scalus.cardano.ledger.Redeemer(redeemerTag, inputIdx, data, exUnits)

        val spentOutput = utxos.getOrElse(
          input,
          throw new Exception(s"$redeemerTag output not found in UTxO set: $input")
        )
        val datum = extractDatumFromOutput(tx, spentOutput)

        LedgerToPlutusTranslation.getScriptContextV3(
          redeemer,
          datum,
          tx,
          utxos,
          environment.slotConfig,
          environment.protocolParams.protocolVersion.toMajor
        )
    }

    def runValidator(
        validatorProgram: Program,
        tx: Transaction,
        utxo: Utxos,
        wallet: WalletTrait,
        scriptInput: TransactionInput,
        redeemerTag: RedeemerTag = RedeemerTag.Spend,
        environment: Environment = testEnvironmentWithoutEvaluator
    ) = {
        val scriptContext =
            TestUtil.getScriptContextV3(tx, utxo, scriptInput, redeemerTag, environment)
        validatorProgram.runWithDebug(scriptContext)
    }
}
