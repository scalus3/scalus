package scalus.examples.crowdfunding

import cps.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.cardano.node.{BlockchainReader, Emulator}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.testing.*
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.testing.kit.TestUtil.genesisHash
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

import java.time.Instant
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/** Scenario exploration test for crowdfunding contract.
  *
  * Uses non-deterministic branching to explore different action sequences (donate, wait, withdraw,
  * reclaim) and verify invariants and access control hold across all paths.
  */
class CrowdfundingScenarioTest extends AnyFunSuite {
    import CrowdfundingScenarioTest.*
    import Scenario.futureToScenarioConversion

    test("explore: campaign invariants hold under non-deterministic actions") {
        val emulator = createEmulator()
        val scenario = async[Scenario] {
            val campaignId = createCampaignS().await

            Scenario
                .explore(maxDepth = 3) { _ =>
                    async[Scenario] {
                        val action = Scenario
                            .choices(
                              "donate_bob",
                              "donate_charles",
                              "wait",
                              "withdraw",
                              "reclaim"
                            )
                            .await
                        action match
                            case "donate_bob" =>
                                tryDonateS(campaignId, donor1Addr, 3_000_000L, donor1).await
                            case "donate_charles" =>
                                tryDonateS(campaignId, donor2Addr, 4_000_000L, donor2).await
                            case "wait" =>
                                Scenario.sleep(20).await
                            case "withdraw" =>
                                tryWithdrawS(campaignId).await
                            case "reclaim" =>
                                tryReclaimS(campaignId).await

                        // Check datum invariants
                        val reader = Scenario.snapshotReader.await
                        val maybeDatum = tryGetCampaignDatum(reader, campaignId).await
                        maybeDatum match
                            case Some(datum) =>
                                Scenario
                                    .check(
                                      datum.totalSum >= 0,
                                      "totalSum must be non-negative"
                                    )
                                    .await
                                Scenario
                                    .check(
                                      datum.goal == BigInt(goal),
                                      "goal must not change"
                                    )
                                    .await
                                Scenario
                                    .check(
                                      datum.withdrawn <= datum.totalSum,
                                      "withdrawn must not exceed totalSum"
                                    )
                                    .await
                            case None => () // campaign fully consumed is ok
                    }
                }
                .await
        }

        val results = Await.result(Scenario.runAll(emulator)(scenario), Duration(120, "s"))
        val violations = results.flatMap(_._2)
        assert(
          violations.isEmpty,
          s"Found violations: ${violations.map(v => s"${v.message} at ${v.location}")}"
        )
    }
}

object CrowdfundingScenarioTest {
    import Scenario.futureToScenarioConversion

    private val crowdfundingContract = CrowdfundingContract.withErrorTraces
    private val donationMintingContract = DonationMintingContract.withErrorTraces
    private val crowdfundingScript = crowdfundingContract.script
    private val crowdfundingPolicyId = crowdfundingScript.scriptHash

    private val network = Network.Mainnet
    private val recipient = Alice
    private val donor1 = Bob
    private val donor2 = Charles

    private val recipientAddr = recipient.address(network)
    private val donor1Addr = donor1.address(network)
    private val donor2Addr = donor2.address(network)

    private val goal = 10_000_000L
    private val deadlineSlot: Long = 100L
    private val beforeDeadlineSlot: Long = deadlineSlot - 10

    private val scriptAddress = crowdfundingContract.address(network)

    private def createEmulator(): Emulator = {
        val emulator = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) -> TransactionOutput.Babbage(
              address = recipientAddr,
              value = Value.lovelace(50_000_000L)
            ),
            Input(genesisHash, 1) -> TransactionOutput.Babbage(
              address = recipientAddr,
              value = Value.lovelace(50_000_000L)
            ),
            Input(genesisHash, 2) -> TransactionOutput.Babbage(
              address = donor1Addr,
              value = Value.lovelace(50_000_000L)
            ),
            Input(genesisHash, 3) -> TransactionOutput.Babbage(
              address = donor1Addr,
              value = Value.lovelace(50_000_000L)
            ),
            Input(genesisHash, 4) -> TransactionOutput.Babbage(
              address = donor2Addr,
              value = Value.lovelace(50_000_000L)
            ),
            Input(genesisHash, 5) -> TransactionOutput.Babbage(
              address = donor2Addr,
              value = Value.lovelace(50_000_000L)
            )
          ),
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
        emulator.setSlot(beforeDeadlineSlot)
        emulator
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private def extractPkh(address: ShelleyAddress): PubKeyHash =
        address.payment match
            case ShelleyPaymentPart.Key(hash) => PubKeyHash(hash)
            case _ => throw IllegalArgumentException("Expected key payment credential")

    private def addressFromPkh(pkh: PubKeyHash): ShelleyAddress =
        ShelleyAddress(
          network,
          ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(pkh.hash)),
          ShelleyDelegationPart.Null
        )

    private def getDonationScript(campaignId: ByteString): Script.PlutusV3 = {
        val appliedProgram = donationMintingContract.program $ campaignId.toData
        Script.PlutusV3(appliedProgram.cborByteString)
    }

    private def computeDonationPolicyId(campaignId: ByteString): ByteString =
        getDonationScript(campaignId).scriptHash

    private def findCampaignUtxo(
        reader: BlockchainReader,
        campaignId: ByteString
    ): Future[Option[Utxo]] = {
        given scala.concurrent.ExecutionContext = reader.executionContext
        val nftAsset = AssetName(campaignId)
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos
                .find { case (_, output) =>
                    output.value.assets.assets
                        .get(crowdfundingPolicyId)
                        .exists(_.get(nftAsset).exists(_ > 0))
                }
                .map { case (input, output) => Utxo(input, output) }
        }
    }

    private def findDonationUtxos(
        reader: BlockchainReader,
        campaignId: ByteString,
        donationPolicyId: ScriptHash
    ): Future[Seq[Utxo]] = {
        given scala.concurrent.ExecutionContext = reader.executionContext
        val nftAsset = AssetName(campaignId)
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos
                .filterNot { case (_, output) =>
                    output.value.assets.assets
                        .get(crowdfundingPolicyId)
                        .exists(_.get(nftAsset).exists(_ > 0))
                }
                .filter { case (_, output) =>
                    output.value.assets.assets
                        .get(donationPolicyId)
                        .exists(_.nonEmpty)
                }
                .map { case (input, output) => Utxo(input, output) }
                .toSeq
        }
    }

    private def tryGetCampaignDatum(
        reader: BlockchainReader,
        campaignId: ByteString
    ): Future[Option[CampaignDatum]] = {
        given scala.concurrent.ExecutionContext = reader.executionContext
        findCampaignUtxo(reader, campaignId).map(
          _.flatMap(u => u.output.inlineDatum.map(_.to[CampaignDatum]))
        )
    }

    // =========================================================================
    // Base actions
    // =========================================================================

    private def createCampaignS(): Scenario[ByteString] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        val deadline = reader.cardanoInfo.slotConfig.slotToTime(deadlineSlot)
        val recipientPkh = extractPkh(recipientAddr)
        val recipientKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)

        given scala.concurrent.ExecutionContext = reader.executionContext

        val utxos = reader.findUtxos(recipientAddr).await.getOrElse(Map.empty)
        if utxos.isEmpty then throw RuntimeException("No UTxOs at recipient address")

        val firstUtxo = utxos.head
        val seedUtxo = Utxo(firstUtxo._1, firstUtxo._2)

        val txOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
          scalus.cardano.onchain.plutus.v3.TxId(firstUtxo._1.transactionId),
          firstUtxo._1.index
        )
        val campaignId = scalus.uplc.builtin.Builtins.blake2b_256(
          scalus.uplc.builtin.Builtins.serialiseData(txOutRef.toData)
        )

        val donationPolicyId = computeDonationPolicyId(campaignId)
        val datum = CampaignDatum(
          totalSum = BigInt(0),
          goal = BigInt(goal),
          recipient = recipientPkh,
          deadline = BigInt(deadline),
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )
        val redeemer = Action.Create(
          goal = BigInt(goal),
          recipient = recipientPkh,
          deadline = BigInt(deadline)
        )

        val nftAsset = AssetName(campaignId)
        val mintedValue = Value.asset(crowdfundingPolicyId, nftAsset, 1L)

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(seedUtxo)
            .mint(crowdfundingScript, Map(nftAsset -> 1L), redeemer, Set(recipientKeyHash))
            .payTo(scriptAddress, Value(Coin(2_000_000L)) + mintedValue, datum)
            .validTo(Instant.ofEpochMilli(deadline - 1000))
            .complete(reader, recipientAddr)
            .await
            .sign(recipient.signer)
            .transaction

        val result = Scenario.submit(tx).await
        result match
            case Right(_)  => campaignId
            case Left(err) => throw RuntimeException(s"Failed to create campaign: $err")
    }

    private def donateS(
        campaignId: ByteString,
        donorAddress: ShelleyAddress,
        amount: Long,
        donor: Party
    ): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val campaignUtxo = findCampaignUtxo(reader, campaignId).await
            .getOrElse(throw RuntimeException("Campaign not found"))
        val currentDatum = campaignUtxo.output.requireInlineDatum.to[CampaignDatum]

        val donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
        val donationScript = getDonationScript(campaignId)

        val newDatum = CampaignDatum(
          totalSum = currentDatum.totalSum + BigInt(amount),
          goal = currentDatum.goal,
          recipient = currentDatum.recipient,
          deadline = currentDatum.deadline,
          withdrawn = currentDatum.withdrawn,
          donationPolicyId = currentDatum.donationPolicyId
        )

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val donationTokenValue = Value.asset(donationPolicyId, donationAsset, 1L)
        val nftAsset = AssetName(campaignId)
        val nftValue = Value.asset(crowdfundingPolicyId, nftAsset, 1L)
        val newCampaignValue =
            Value.lovelace(campaignUtxo.output.value.coin.value + amount) + nftValue
        val donationUtxoValue = Value.lovelace(amount) + donationTokenValue

        val donorPkh = extractPkh(donorAddress)
        val donationDatum = DonationDatum(donorPkh, BigInt(amount))

        val donateRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.address == scriptAddress &&
                sized.value.value.assets.assets
                    .get(crowdfundingPolicyId)
                    .exists(_.get(nftAsset).exists(_ > 0))
            }
            val donationOutputIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.address == scriptAddress &&
                !sized.value.value.assets.assets
                    .get(crowdfundingPolicyId)
                    .exists(_.get(nftAsset).exists(_ > 0))
            }
            Action
                .Donate(
                  BigInt(amount),
                  BigInt(inputIdx),
                  BigInt(campaignOutputIdx),
                  BigInt(donationOutputIdx)
                )
                .toData
        }

        val tx = TxBuilder(reader.cardanoInfo)
            .spend(campaignUtxo, donateRedeemer, crowdfundingScript, Set.empty)
            .mint(donationScript, Map(donationAsset -> 1L), donateRedeemer)
            .payTo(scriptAddress, newCampaignValue, newDatum)
            .payTo(scriptAddress, donationUtxoValue, donationDatum)
            .validTo(Instant.ofEpochMilli(currentDatum.deadline.toLong - 1000))
            .complete(reader, donorAddress)
            .await
            .sign(donor.signer)
            .transaction

        val result = Scenario.submit(tx).await
        result match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"Failed to donate: $err")
    }

    private def withdrawS(campaignId: ByteString): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val campaignUtxo = findCampaignUtxo(reader, campaignId).await
            .getOrElse(throw RuntimeException("Campaign not found"))
        val currentDatum = campaignUtxo.output.requireInlineDatum.to[CampaignDatum]

        val recipientPkh = currentDatum.recipient
        val recipientKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)
        val recipientAddress = addressFromPkh(recipientPkh)

        val donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
        val donationScript = getDonationScript(campaignId)
        val donationUtxos = findDonationUtxos(reader, campaignId, donationPolicyId).await
        if donationUtxos.isEmpty then throw RuntimeException("No donations to withdraw")

        val totalWithdrawAmount: BigInt = donationUtxos
            .map(_.output.requireInlineDatum.to[DonationDatum].amount)
            .sum

        val newWithdrawn = currentDatum.withdrawn + totalWithdrawAmount
        val isFullWithdrawal = newWithdrawn == currentDatum.totalSum

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val burnMap = Map(donationAsset -> -donationUtxos.size.toLong)
        val nftAsset = AssetName(campaignId)

        val withdrawRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx =
                if isFullWithdrawal then -1
                else
                    tx.body.value.outputs.indexWhere { sized =>
                        sized.value.address == scriptAddress &&
                        sized.value.value.assets.assets
                            .get(crowdfundingPolicyId)
                            .exists(_.get(nftAsset).exists(_ > 0))
                    }
            val recipientOutputIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.address == recipientAddress
            }
            val donationInputIndices = donationUtxos
                .map(u => BigInt(tx.body.value.inputs.toSeq.indexOf(u.input)))
                .sorted
            Action
                .Withdraw(
                  BigInt(inputIdx),
                  BigInt(campaignOutputIdx),
                  BigInt(recipientOutputIdx),
                  scalus.cardano.onchain.plutus.prelude.List.from(donationInputIndices)
                )
                .toData
        }

        val withDonations = donationUtxos.foldLeft(
          TxBuilder(reader.cardanoInfo)
              .spend(campaignUtxo, withdrawRedeemer, crowdfundingScript, Set(recipientKeyHash))
        ) { (b, utxo) =>
            b.spend(utxo, withdrawRedeemer, crowdfundingScript, Set.empty)
        }

        val withRecipient = withDonations
            .mint(donationScript, burnMap, withdrawRedeemer)
            .payTo(recipientAddress, Value.lovelace(totalWithdrawAmount.toLong))

        val finalBuilder =
            if isFullWithdrawal then withRecipient
            else
                val nftVal = Value.asset(crowdfundingPolicyId, nftAsset, 1L)
                val newDatum = CampaignDatum(
                  totalSum = currentDatum.totalSum,
                  goal = currentDatum.goal,
                  recipient = currentDatum.recipient,
                  deadline = currentDatum.deadline,
                  withdrawn = newWithdrawn,
                  donationPolicyId = currentDatum.donationPolicyId
                )
                withRecipient.payTo(
                  scriptAddress,
                  Value.lovelace(2_000_000L) + nftVal,
                  newDatum
                )

        val tx = finalBuilder
            .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
            .complete(reader, recipientAddress)
            .await
            .sign(recipient.signer)
            .transaction

        val result = Scenario.submit(tx).await
        result match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"Failed to withdraw: $err")
    }

    private def reclaimS(campaignId: ByteString): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        given scala.concurrent.ExecutionContext = reader.executionContext

        val campaignUtxo = findCampaignUtxo(reader, campaignId).await
            .getOrElse(throw RuntimeException("Campaign not found"))
        val currentDatum = campaignUtxo.output.requireInlineDatum.to[CampaignDatum]

        val donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
        val donationScript = getDonationScript(campaignId)
        val donationUtxos = findDonationUtxos(reader, campaignId, donationPolicyId).await
        if donationUtxos.isEmpty then throw RuntimeException("No donations to reclaim")

        val donorInfos = donationUtxos.map { utxo =>
            val dd = utxo.output.requireInlineDatum.to[DonationDatum]
            val addr = addressFromPkh(dd.donor)
            (addr, dd.donor, dd.amount, utxo.output.value.coin.value)
        }

        val totalReclaimAmount: BigInt = donorInfos.map(_._3).sum
        val newWithdrawn = currentDatum.withdrawn + totalReclaimAmount
        val isFullReclaim = newWithdrawn == currentDatum.totalSum

        val donorKeyHashes = donorInfos.map { case (_, pkh, _, _) =>
            AddrKeyHash.fromByteString(pkh.hash)
        }.toSet

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val burnMap = Map(donationAsset -> -donationUtxos.size.toLong)
        val nftAsset = AssetName(campaignId)

        val reclaimRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx =
                if isFullReclaim then -1
                else
                    tx.body.value.outputs.indexWhere { sized =>
                        sized.value.address == scriptAddress &&
                        sized.value.value.assets.assets
                            .get(crowdfundingPolicyId)
                            .exists(_.get(nftAsset).exists(_ > 0))
                    }
            val sortedPairs = donationUtxos
                .zip(donorInfos)
                .map { case (utxo, (donorAddr, _, _, _)) =>
                    val donationIdx =
                        BigInt(tx.body.value.inputs.toSeq.indexOf(utxo.input))
                    val reclaimerOutIdx =
                        BigInt(tx.body.value.outputs.indexWhere(_.value.address == donorAddr))
                    (donationIdx, reclaimerOutIdx)
                }
                .sortBy(_._1)
            Action
                .Reclaim(
                  BigInt(inputIdx),
                  BigInt(campaignOutputIdx),
                  scalus.cardano.onchain.plutus.prelude.List.from(sortedPairs.map(_._1)),
                  scalus.cardano.onchain.plutus.prelude.List.from(sortedPairs.map(_._2))
                )
                .toData
        }

        val withDonations = donationUtxos.foldLeft(
          TxBuilder(reader.cardanoInfo)
              .spend(campaignUtxo, reclaimRedeemer, crowdfundingScript, donorKeyHashes)
        ) { (b, utxo) =>
            b.spend(utxo, reclaimRedeemer, crowdfundingScript, Set.empty)
        }

        val withPayments = donorInfos.foldLeft(
          withDonations.mint(donationScript, burnMap, reclaimRedeemer)
        ) { case (b, (donorAddr, _, _, utxoLovelace)) =>
            b.payTo(donorAddr, Value.lovelace(utxoLovelace))
        }

        val finalBuilder =
            if isFullReclaim then withPayments
            else
                val nftVal = Value.asset(crowdfundingPolicyId, nftAsset, 1L)
                val newDatum = CampaignDatum(
                  totalSum = currentDatum.totalSum,
                  goal = currentDatum.goal,
                  recipient = currentDatum.recipient,
                  deadline = currentDatum.deadline,
                  withdrawn = newWithdrawn,
                  donationPolicyId = currentDatum.donationPolicyId
                )
                withPayments.payTo(
                  scriptAddress,
                  Value.lovelace(2_000_000L) + nftVal,
                  newDatum
                )

        val feePayerAddr = donorInfos.head._1
        val allSigners = new TransactionSigner(
          Set(donor1.account.paymentKeyPair, donor2.account.paymentKeyPair)
        )

        val tx = finalBuilder
            .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
            .complete(reader, feePayerAddr)
            .await
            .sign(allSigners)
            .transaction

        val result = Scenario.submit(tx).await
        result match
            case Right(_)  => ()
            case Left(err) => throw RuntimeException(s"Failed to reclaim: $err")
    }

    // =========================================================================
    // Try actions with precondition/postcondition checks
    // =========================================================================

    private def tryDonateS(
        campaignId: ByteString,
        donorAddress: ShelleyAddress,
        amount: Long,
        donor: Party
    ): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        val maybeDatum = tryGetCampaignDatum(reader, campaignId).await
        val slotTime = reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
        val shouldSucceed = maybeDatum.exists(d => slotTime < d.deadline.toLong)

        Scenario.scenarioLogicMonad
            .flatMapTry(donateS(campaignId, donorAddress, amount, donor)) {
                case Success(_) =>
                    Scenario.check(shouldSucceed, "donate succeeded but deadline has passed")
                case Failure(_) =>
                    Scenario.check(!shouldSucceed, "donate failed but should have succeeded")
            }
            .await
    }

    private def tryWithdrawS(campaignId: ByteString): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        val maybeDatum = tryGetCampaignDatum(reader, campaignId).await
        val slotTime = reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
        val shouldSucceed = maybeDatum match
            case Some(d) =>
                val dpId = ScriptHash.fromByteString(d.donationPolicyId)
                val hasDonations = findDonationUtxos(reader, campaignId, dpId).await.nonEmpty
                slotTime > d.deadline.toLong && d.totalSum >= d.goal && hasDonations
            case None => false

        Scenario.scenarioLogicMonad
            .flatMapTry(withdrawS(campaignId)) {
                case Success(_) =>
                    Scenario.check(shouldSucceed, "withdraw succeeded but preconditions not met")
                case Failure(_) =>
                    Scenario.check(!shouldSucceed, "withdraw failed but preconditions were met")
            }
            .await
    }

    private def tryReclaimS(campaignId: ByteString): Scenario[Unit] = async[Scenario] {
        val reader = Scenario.snapshotReader.await
        val maybeDatum = tryGetCampaignDatum(reader, campaignId).await
        val slotTime = reader.cardanoInfo.slotConfig.slotToTime(reader.currentSlot.await)
        val hasDonations = maybeDatum match
            case Some(d) =>
                val dpId = ScriptHash.fromByteString(d.donationPolicyId)
                findDonationUtxos(reader, campaignId, dpId).await.nonEmpty
            case None => false
        val shouldSucceed = maybeDatum match
            case Some(d) =>
                slotTime > d.deadline.toLong && d.totalSum < d.goal && hasDonations
            case None => false

        Scenario.scenarioLogicMonad
            .flatMapTry(reclaimS(campaignId)) {
                case Success(_) =>
                    Scenario.check(shouldSucceed, "reclaim succeeded but preconditions not met")
                case Failure(_) =>
                    Scenario.check(!shouldSucceed, "reclaim failed but preconditions were met")
            }
            .await
    }
}
