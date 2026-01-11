package scalus.testing.kit

import scalus.builtin.Builtins.{appendByteString, blake2b_224, blake2b_256}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.{AllNeededScriptHashes, AllResolvedScripts}
import scalus.cardano.txbuilder.{PubKeyWitness, RedeemerManagement, RedeemerPurpose, Wallet as WalletTrait, Witness}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.{TxId, TxOutRef, ValidatorHash}
import scalus.ledger.api.{v1, v2, v3, ScriptContext}
import scalus.testing.kit.ScalusTest
import scalus.uplc.Program

object TestUtil extends ScalusTest {
    import scalus.builtin.ByteString.*

    // Mock data generation constants and methods
    val rootKeyHash: ByteString =
        hex"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"

    val rootTxHash: ByteString =
        hex"5a077cbcdffb88b104f292aacb9687ce93e2191e103a30a0cc5505c18b719f98"

    private def mockHash(
        variation: BigInt,
        root: ByteString,
        hash: ByteString => ByteString
    ): ByteString = hash:
        appendByteString(ByteString.fromArray(variation.toByteArray), root)

    private def mockKeyHash(variation: BigInt): ByteString =
        mockHash(variation, rootKeyHash, blake2b_224)

    private def mockTxHash(variation: BigInt): TxId =
        TxId(mockHash(variation, rootTxHash, blake2b_256))

    def mockPubKeyHash(variation: BigInt): PubKeyHash = PubKeyHash(mockKeyHash(variation))

    def mockScriptHash(variation: BigInt): ValidatorHash =
        mockKeyHash(variation + 200)

    def mockTxOutRef(variation: BigInt, idx: BigInt): TxOutRef =
        TxOutRef(mockTxHash(variation), idx)

    def mockTxInput(variation: BigInt, idx: BigInt): TransactionInput =
        val TxOutRef(id, index) = mockTxOutRef(variation, idx)
        TransactionInput(TransactionHash.fromByteString(id.hash), index.toInt)

    val testEnvironment: CardanoInfo = CardanoInfo.mainnet

    val testEvaluator: PlutusScriptEvaluator =
        PlutusScriptEvaluator(testEnvironment, EvaluatorMode.EvaluateAndComputeCost)

    def createTestAddress(keyHash: String): ShelleyAddress = {
        ShelleyAddress(
          network = CardanoInfo.mainnet.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(ByteString.fromHex(keyHash))),
          delegation = ShelleyDelegationPart.Null
        )
    }

    def createTestAddress(addrKeyHash: AddrKeyHash): ShelleyAddress = {
        ShelleyAddress(
          network = CardanoInfo.mainnet.network,
          payment = ShelleyPaymentPart.Key(addrKeyHash),
          delegation = ShelleyDelegationPart.Null
        )
    }

    @deprecated("Will be removed", "0.13.0")
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
        private val txUnspentOutput = Utxo(testInput, testOutput)

        override def owner: Address = address
        override def utxo: Utxos = Map((testInput, testOutput))
        override def collateralInputs: Seq[(Utxo, Witness)] = Seq(
          (txUnspentOutput, PubKeyWitness)
        )
        override def selectInputs(
            required: Value
        ): Option[Seq[(Utxo, Witness)]] = {
            val available = testOutput.value
            if available.coin.value >= required.coin.value then {
                Some(Seq((txUnspentOutput, PubKeyWitness)))
            } else {
                None
            }
        }
    }

    @deprecated("will be removed", "0.14.2")
    def getScriptUtxo(tx: Transaction): (TransactionInput, TransactionOutput) = {
        val (transactionOutput, index) = tx.body.value.outputs.view
            .map(_.value)
            .zipWithIndex
            .find { (transactionOutput, _) => transactionOutput.address.hasScript }
            .getOrElse(throw new Exception("No script output found in transaction"))

        (TransactionInput(tx.id, index), transactionOutput)
    }

    @deprecated("will be removed", "0.14.2")
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

    @deprecated("Will be removed", "0.13.0")
    def runValidator(
        validatorProgram: Program,
        tx: Transaction,
        utxo: Utxos,
        wallet: WalletTrait,
        scriptInput: TransactionInput,
        redeemerTag: RedeemerTag = RedeemerTag.Spend,
        environment: CardanoInfo = testEnvironment
    ) = {
        given CardanoInfo = environment
        val scriptContext = tx.getScriptContextV3(utxo, RedeemerPurpose.ForSpend(scriptInput))
        validatorProgram.runWithDebug(scriptContext)
    }

    extension (tx: Transaction)
        /** Get all script contexts for all Plutus scripts in the transaction.
          *
          * @param utxos
          *   The UTxO set for resolving spent outputs
          * @param env
          *   The CardanoInfo containing protocol parameters and slot configuration
          * @return
          *   Map from Redeemer to ScriptContext (union type: v1 | v2 | v3)
          */
        def scriptContexts(utxos: Utxos)(using env: CardanoInfo): Map[Redeemer, ScriptContext] =
            // 1. Resolve all scripts
            val scriptsMap = AllResolvedScripts.allResolvedScriptsMap(tx, utxos) match
                case Right(map)  => map
                case Left(error) => throw error

            // 2. Get needed script data
            val neededScripts = AllNeededScriptHashes.allNeededScriptData(tx, utxos) match
                case Right(data) => data
                case Left(error) => throw error

            // 3. Get redeemers map
            val redeemersMap = tx.witnessSet.redeemers.map(_.value.toMap).getOrElse(Map.empty)

            // 4. Build contexts for each Plutus script redeemer
            neededScripts.flatMap { case (tag, index, hash, outputOpt) =>
                for
                    script <- scriptsMap.get(hash)
                    plutusScript <- script match
                        case ps: PlutusScript => Some(ps)
                        case _                => None // Skip native scripts
                    (data, exUnits) <- redeemersMap.get((tag, index))
                yield
                    val redeemer = Redeemer(tag, index, data, exUnits)
                    val datum = outputOpt.flatMap(extractDatumFromOutput(tx, _))
                    val context: ScriptContext = plutusScript match
                        case _: Script.PlutusV1 =>
                            LedgerToPlutusTranslation.getScriptContextV1(
                              redeemer,
                              tx,
                              utxos,
                              env.slotConfig,
                              env.majorProtocolVersion
                            )
                        case _: Script.PlutusV2 =>
                            LedgerToPlutusTranslation.getScriptContextV2(
                              redeemer,
                              tx,
                              utxos,
                              env.slotConfig,
                              env.majorProtocolVersion
                            )
                        case _: Script.PlutusV3 =>
                            LedgerToPlutusTranslation.getScriptContextV3(
                              redeemer,
                              datum,
                              tx,
                              utxos,
                              env.slotConfig,
                              env.majorProtocolVersion
                            )
                    redeemer -> context
            }.toMap

        /** Get only V1 script contexts */
        def scriptContextsV1(utxos: Utxos)(using CardanoInfo): Map[Redeemer, v1.ScriptContext] =
            scriptContexts(utxos).collect { case (r, sc: v1.ScriptContext) => r -> sc }

        /** Get only V2 script contexts */
        def scriptContextsV2(utxos: Utxos)(using CardanoInfo): Map[Redeemer, v2.ScriptContext] =
            scriptContexts(utxos).collect { case (r, sc: v2.ScriptContext) => r -> sc }

        /** Get only V3 script contexts */
        def scriptContextsV3(utxos: Utxos)(using CardanoInfo): Map[Redeemer, v3.ScriptContext] =
            scriptContexts(utxos).collect { case (r, sc: v3.ScriptContext) => r -> sc }

        /** Get a single V1 script context for a specific redeemer purpose.
          *
          * @param utxos
          *   The UTxO set for resolving spent outputs
          * @param purpose
          *   The redeemer purpose (ForSpend, ForMint, ForCert, ForReward, ForVote, ForPropose)
          * @return
          *   The V1 script context
          * @throws Exception
          *   if no V1 script context is found for the given purpose
          */
        def getScriptContextV1(
            utxos: Utxos,
            purpose: RedeemerPurpose
        )(using CardanoInfo): v1.ScriptContext = {
            val tag = purpose.redeemerTag
            val index = RedeemerManagement.indexFor(tx, purpose)
            tx.scriptContextsV1(utxos)
                .find { case (redeemer, _) =>
                    redeemer.tag == tag && redeemer.index == index
                }
                .map(_._2)
                .getOrElse(
                  throw new Exception(s"No V1 script context found for $purpose")
                )
        }

        /** Get a single V2 script context for a specific redeemer purpose.
          *
          * @param utxos
          *   The UTxO set for resolving spent outputs
          * @param purpose
          *   The redeemer purpose (ForSpend, ForMint, ForCert, ForReward, ForVote, ForPropose)
          * @return
          *   The V2 script context
          * @throws Exception
          *   if no V2 script context is found for the given purpose
          */
        def getScriptContextV2(
            utxos: Utxos,
            purpose: RedeemerPurpose
        )(using CardanoInfo): v2.ScriptContext = {
            val tag = purpose.redeemerTag
            val index = RedeemerManagement.indexFor(tx, purpose)
            tx.scriptContextsV2(utxos)
                .find { case (redeemer, _) =>
                    redeemer.tag == tag && redeemer.index == index
                }
                .map(_._2)
                .getOrElse(
                  throw new Exception(s"No V2 script context found for $purpose")
                )
        }

        /** Get a single V3 script context for a specific redeemer purpose.
          *
          * @param utxos
          *   The UTxO set for resolving spent outputs
          * @param purpose
          *   The redeemer purpose (ForSpend, ForMint, ForCert, ForReward, ForVote, ForPropose)
          * @return
          *   The V3 script context
          * @throws Exception
          *   if no V3 script context is found for the given purpose
          */
        def getScriptContextV3(
            utxos: Utxos,
            purpose: RedeemerPurpose
        )(using CardanoInfo): v3.ScriptContext = {
            val tag = purpose.redeemerTag
            val index = RedeemerManagement.indexFor(tx, purpose)
            tx.scriptContextsV3(utxos)
                .find { case (redeemer, _) =>
                    redeemer.tag == tag && redeemer.index == index
                }
                .map(_._2)
                .getOrElse(
                  throw new Exception(s"No V3 script context found for $purpose")
                )
        }
}
