package scalus.examples.crowdfunding

import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainReader, Emulator}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.{HdAccount, HdKeyPair}
import scalus.crypto.ed25519.given
import scalus.testing.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}

import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** ScalaCheck Commands property-based test for crowdfunding contract with many participants.
  *
  * Creates 200 participants (1 recipient + 199 donors) and uses ContractScalaCheckCommands to
  * generate random sequences of actions (donate, wait, withdraw, reclaim), verifying invariants hold
  * after each successful transaction.
  */
class CrowdfundingScalaCheckCommandTest extends AnyFunSuite {
    import CrowdfundingScalaCheckCommandTest.*

    private given ExecutionContext = ExecutionContext.global

    test("crowdfunding: invariants hold under random action sequences with many participants") {
        val (emulator, campaignId) = createEmulatorWithCampaign()
        val step = new CrowdfundingStep(campaignId)

        val commands = ContractScalaCheckCommands(emulator, step) { (reader, state) =>
            Future.successful {
                state.datum match
                    case Some(d) =>
                        Prop(d.totalSum >= 0) :| "totalSum non-negative" &&
                        Prop(d.goal == BigInt(goal)) :| "goal unchanged" &&
                        Prop(d.withdrawn >= 0) :| "withdrawn non-negative" &&
                        Prop(d.withdrawn <= d.totalSum) :| "withdrawn <= totalSum"
                    case None =>
                        Prop.passed // campaign fully consumed
            }
        }

        val result = org.scalacheck.Test.check(
            org.scalacheck.Test.Parameters.default
                .withMinSuccessfulTests(10)
                .withMaxDiscardRatio(20),
            commands.property()
        )

        assert(result.passed, s"Property test failed: $result")
    }
}

object CrowdfundingScalaCheckCommandTest {
    private val crowdfundingContract = CrowdfundingContract.withErrorTraces
    private val donationMintingContract = DonationMintingContract.withErrorTraces
    private val crowdfundingScript = crowdfundingContract.script
    private val crowdfundingPolicyId = crowdfundingScript.scriptHash

    private val network = Network.Mainnet
    private val goal = 5_000_000L
    private val donationAmount = 200_000L
    private val deadlineSlot: Long = 100L
    private val beforeDeadlineSlot: Long = deadlineSlot - 10
    private val donorsPerStep = 5
    private val maxDonationsPerBatch = 10

    private val scriptAddress = crowdfundingContract.address(network)

    // =========================================================================
    // Participants (200 total: 1 recipient + 199 donors)
    // =========================================================================

    case class Participant(index: Int, account: HdAccount) {
        lazy val addrKeyHash: AddrKeyHash = account.paymentKeyHash

        val address: ShelleyAddress =
            ShelleyAddress(
              network,
              ShelleyPaymentPart.Key(account.paymentKeyHash),
              ShelleyDelegationPart.Null
            )

        lazy val signer: TransactionSigner = new TransactionSigner(Set(account.paymentKeyPair))
    }

    private val mnemonic: String =
        "test test test test test test test test test test test test " +
            "test test test test test test test test test test test sauce"

    private val numDonors = 199

    // Derive master key once, then derive all accounts efficiently
    private val participants: IndexedSeq[Participant] = {
        val masterKey = HdKeyPair.masterFromMnemonic(mnemonic, "")
        val purposeKey = masterKey.deriveHardened(1852)
        val coinTypeKey = purposeKey.deriveHardened(1815)

        (0 to numDonors).map { i =>
            val accountKey = coinTypeKey.deriveHardened(i)
            Participant(i, new HdAccount(i, accountKey))
        }
    }

    private val recipientP = participants(0)
    private val donors: IndexedSeq[Participant] = participants.drop(1)

    // Lookup: PubKeyHash.hash -> Participant (for finding signing keys during reclaim)
    private val participantByPkhHash: Map[ByteString, Participant] =
        participants.map { p =>
            extractPkh(p.address).hash -> p
        }.toMap

    // =========================================================================
    // State
    // =========================================================================

    case class CrowdfundingState(
        campaignId: ByteString,
        campaignUtxo: Option[Utxo],
        datum: Option[CampaignDatum],
        donationUtxos: Seq[Utxo]
    )

    // =========================================================================
    // Step
    // =========================================================================

    class CrowdfundingStep(campaignId: ByteString)
        extends ContractStepVariations[CrowdfundingState] {

        override def extractState(reader: BlockchainReader)(using
            ExecutionContext
        ): Future[CrowdfundingState] =
            for
                campaignUtxo <- findCampaignUtxo(reader, campaignId)
                datum = campaignUtxo.flatMap(_.output.inlineDatum.map(_.to[CampaignDatum]))
                donationUtxos <- datum match
                    case Some(d) =>
                        findDonationUtxos(
                          reader,
                          campaignId,
                          ScriptHash.fromByteString(d.donationPolicyId)
                        )
                    case None => Future.successful(Seq.empty)
            yield CrowdfundingState(campaignId, campaignUtxo, datum, donationUtxos)

        // Not used directly since allVariations is overridden
        override def makeBaseTx(reader: BlockchainReader, state: CrowdfundingState)(using
            ExecutionContext
        ): Future[TxTemplate] =
            Future.successful(
              TxTemplate(TxBuilder(reader.cardanoInfo), donors.head.address, donors.head.signer)
            )

        override def variations: TxVariations[CrowdfundingState] = TxVariations.empty

        override def allVariations(
            reader: BlockchainReader,
            state: CrowdfundingState
        )(using ExecutionContext): Future[Seq[Transaction]] =
            state.datum match
                case None => Future.successful(Seq.empty)
                case Some(d) =>
                    reader.currentSlot.flatMap { currentSlot =>
                        val slotTime = reader.cardanoInfo.slotConfig.slotToTime(currentSlot)
                        val beforeDeadline = slotTime < d.deadline.toLong
                        val afterDeadline = slotTime > d.deadline.toLong
                        val goalReached = d.totalSum >= d.goal
                        val hasDonations = state.donationUtxos.nonEmpty

                        val txFutures = Seq.newBuilder[Future[Option[Transaction]]]

                        // Donate: select a rotating subset of donors
                        if beforeDeadline && state.campaignUtxo.isDefined then
                            val offset = state.donationUtxos.size
                            val selected = (0 until donorsPerStep).map { i =>
                                donors((offset + i * 37) % donors.size)
                            }
                            selected.foreach { donor =>
                                txFutures += buildDonateTx(reader, state, donor)
                                    .map(Some(_))
                                    .recover { case _ => None }
                            }

                        // Withdraw (batch)
                        if afterDeadline && goalReached && hasDonations then
                            txFutures += buildWithdrawTx(reader, state)
                                .map(Some(_))
                                .recover { case _ => None }

                        // Reclaim (batch)
                        if afterDeadline && !goalReached && hasDonations then
                            txFutures += buildReclaimTx(reader, state)
                                .map(Some(_))
                                .recover { case _ => None }

                        val futures = txFutures.result()
                        if futures.isEmpty then Future.successful(Seq.empty)
                        else Future.sequence(futures).map(_.flatten)
                    }

        override def slotDelays(state: CrowdfundingState): Seq[Long] = Seq(20L, 50L)
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

    private def hasCampaignNft(output: TransactionOutput, nftAsset: AssetName): Boolean =
        output.value.assets.assets
            .get(crowdfundingPolicyId)
            .exists(_.get(nftAsset).exists(_ > 0))

    private def findCampaignOutputIdx(tx: Transaction, nftAsset: AssetName): Int =
        tx.body.value.outputs.indexWhere(sized => hasCampaignNft(sized.value, nftAsset))

    private def findCampaignUtxo(
        reader: BlockchainReader,
        campaignId: ByteString
    )(using ExecutionContext): Future[Option[Utxo]] = {
        val nftAsset = AssetName(campaignId)
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos
                .find((_, output) => hasCampaignNft(output, nftAsset))
                .map((input, output) => Utxo(input, output))
        }
    }

    private def findDonationUtxos(
        reader: BlockchainReader,
        campaignId: ByteString,
        donationPolicyId: ScriptHash
    )(using ExecutionContext): Future[Seq[Utxo]] = {
        val nftAsset = AssetName(campaignId)
        reader.findUtxos(scriptAddress).map(_.getOrElse(Map.empty)).map { utxos =>
            utxos
                .filterNot((_, output) => hasCampaignNft(output, nftAsset))
                .filter { case (_, output) =>
                    output.value.assets.assets
                        .get(donationPolicyId)
                        .exists(_.nonEmpty)
                }
                .map((input, output) => Utxo(input, output))
                .toSeq
        }
    }

    // =========================================================================
    // Transaction builders
    // =========================================================================

    private def buildDonateTx(
        reader: BlockchainReader,
        state: CrowdfundingState,
        donor: Participant
    )(using ExecutionContext): Future[Transaction] = {
        val campaignUtxo = state.campaignUtxo.getOrElse(throw RuntimeException("No campaign"))
        val currentDatum = state.datum.getOrElse(throw RuntimeException("No datum"))
        val donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
        val donationScript = getDonationScript(state.campaignId)

        val newDatum = CampaignDatum(
          totalSum = currentDatum.totalSum + BigInt(donationAmount),
          goal = currentDatum.goal,
          recipient = currentDatum.recipient,
          deadline = currentDatum.deadline,
          withdrawn = currentDatum.withdrawn,
          donationPolicyId = currentDatum.donationPolicyId
        )

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val donationTokenValue = Value.asset(donationPolicyId, donationAsset, 1L)
        val nftAsset = AssetName(state.campaignId)
        val nftValue = Value.asset(crowdfundingPolicyId, nftAsset, 1L)
        val newCampaignValue =
            Value.lovelace(campaignUtxo.output.value.coin.value + donationAmount) + nftValue
        val donationUtxoValue = Value.lovelace(donationAmount) + donationTokenValue

        val donorPkh = extractPkh(donor.address)
        val donationDatum = DonationDatum(donorPkh, BigInt(donationAmount))

        val donateRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx = findCampaignOutputIdx(tx, nftAsset)
            val donationOutputIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.address == scriptAddress && !hasCampaignNft(sized.value, nftAsset)
            }
            Action
                .Donate(
                  BigInt(donationAmount),
                  BigInt(inputIdx),
                  BigInt(campaignOutputIdx),
                  BigInt(donationOutputIdx)
                )
                .toData
        }

        TxBuilder(reader.cardanoInfo)
            .spend(campaignUtxo, donateRedeemer, crowdfundingScript, Set.empty)
            .mint(donationScript, Map(donationAsset -> 1L), donateRedeemer)
            .payTo(scriptAddress, newCampaignValue, newDatum)
            .payTo(scriptAddress, donationUtxoValue, donationDatum)
            .validTo(Instant.ofEpochMilli(currentDatum.deadline.toLong - 1000))
            .complete(reader, donor.address)
            .map(_.sign(donor.signer).transaction)
    }

    private def buildWithdrawTx(
        reader: BlockchainReader,
        state: CrowdfundingState
    )(using ExecutionContext): Future[Transaction] = {
        val campaignUtxo = state.campaignUtxo.getOrElse(throw RuntimeException("No campaign"))
        val currentDatum = state.datum.getOrElse(throw RuntimeException("No datum"))
        val donationUtxos = state.donationUtxos.take(maxDonationsPerBatch)

        val recipientPkh = currentDatum.recipient
        val recipientKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)
        val recipientAddress = addressFromPkh(recipientPkh)

        val donationScript = getDonationScript(state.campaignId)

        val totalWithdrawAmount: BigInt = donationUtxos
            .map(_.output.requireInlineDatum.to[DonationDatum].amount)
            .sum

        val newWithdrawn = currentDatum.withdrawn + totalWithdrawAmount
        val isFullWithdrawal = newWithdrawn == currentDatum.totalSum

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val burnMap = Map(donationAsset -> -donationUtxos.size.toLong)
        val nftAsset = AssetName(state.campaignId)

        val withdrawRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx =
                if isFullWithdrawal then -1
                else findCampaignOutputIdx(tx, nftAsset)
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

        val withRecipient = donationUtxos
            .foldLeft(
              TxBuilder(reader.cardanoInfo)
                  .spend(
                    campaignUtxo,
                    withdrawRedeemer,
                    crowdfundingScript,
                    Set(recipientKeyHash)
                  )
            ) { (b, utxo) =>
                b.spend(utxo, withdrawRedeemer, crowdfundingScript, Set.empty)
            }
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

        finalBuilder
            .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
            .complete(reader, recipientAddress)
            .map(_.sign(recipientP.signer).transaction)
    }

    private def buildReclaimTx(
        reader: BlockchainReader,
        state: CrowdfundingState
    )(using ExecutionContext): Future[Transaction] = {
        val campaignUtxo = state.campaignUtxo.getOrElse(throw RuntimeException("No campaign"))
        val currentDatum = state.datum.getOrElse(throw RuntimeException("No datum"))
        val donationUtxos = state.donationUtxos.take(maxDonationsPerBatch)

        val donationScript = getDonationScript(state.campaignId)

        val donorInfos = donationUtxos.map { utxo =>
            val dd = utxo.output.requireInlineDatum.to[DonationDatum]
            val addr = addressFromPkh(dd.donor)
            val participant = participantByPkhHash(dd.donor.hash)
            (addr, dd.donor, dd.amount, utxo.output.value.coin.value, participant)
        }

        val totalReclaimAmount: BigInt = donorInfos.map(_._3).sum
        val newWithdrawn = currentDatum.withdrawn + totalReclaimAmount
        val isFullReclaim = newWithdrawn == currentDatum.totalSum

        val donorKeyHashes = donorInfos.map { case (_, pkh, _, _, _) =>
            AddrKeyHash.fromByteString(pkh.hash)
        }.toSet

        val donationAsset = AssetName(DonationMintingPolicy.donationTokenName)
        val burnMap = Map(donationAsset -> -donationUtxos.size.toLong)
        val nftAsset = AssetName(state.campaignId)

        val reclaimRedeemer: Transaction => Data = { (tx: Transaction) =>
            val inputIdx = tx.body.value.inputs.toSeq.indexOf(campaignUtxo.input)
            val campaignOutputIdx =
                if isFullReclaim then -1
                else findCampaignOutputIdx(tx, nftAsset)
            val sortedPairs = donationUtxos
                .zip(donorInfos)
                .map { case (utxo, (donorAddr, _, _, _, _)) =>
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

        val withPayments = donorInfos.foldLeft(
          donationUtxos
              .foldLeft(
                TxBuilder(reader.cardanoInfo)
                    .spend(campaignUtxo, reclaimRedeemer, crowdfundingScript, donorKeyHashes)
              ) { (b, utxo) =>
                  b.spend(utxo, reclaimRedeemer, crowdfundingScript, Set.empty)
              }
              .mint(donationScript, burnMap, reclaimRedeemer)
        ) { case (b, (donorAddr, _, _, utxoLovelace, _)) =>
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
          donorInfos.map(_._5.account.paymentKeyPair).toSet
        )

        finalBuilder
            .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
            .complete(reader, feePayerAddr)
            .map(_.sign(allSigners).transaction)
    }

    // =========================================================================
    // Setup
    // =========================================================================

    private def createEmulatorWithCampaign(): (Emulator, ByteString) = {
        given ExecutionContext = ExecutionContext.global

        // 2 UTxOs per participant for spending + collateral
        val addresses = participants.flatMap(p => Seq(p.address, p.address))
        val emulator = Emulator.withAddresses(addresses, Value.lovelace(50_000_000L))
        emulator.setSlot(beforeDeadlineSlot)

        val deadline = emulator.cardanoInfo.slotConfig.slotToTime(deadlineSlot)
        val recipientPkh = extractPkh(recipientP.address)
        val recipientKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)

        val utxos = Await.result(
          emulator.findUtxos(recipientP.address).map(_.getOrElse(Map.empty)),
          Duration.Inf
        )
        val firstUtxo = utxos.head
        val seedUtxo = Utxo(firstUtxo._1, firstUtxo._2)

        val txOutRef = scalus.cardano.onchain.plutus.v3.TxOutRef(
          scalus.cardano.onchain.plutus.v3.TxId(firstUtxo._1.transactionId),
          firstUtxo._1.index
        )
        val campaignId = scalus.uplc.builtin.Builtins.blake2b_256(
          scalus.uplc.builtin.Builtins.serialiseData(txOutRef.toData)
        )

        val donationPolicyId = getDonationScript(campaignId).scriptHash
        val datum = CampaignDatum(
          totalSum = BigInt(0),
          goal = BigInt(goal),
          recipient = recipientPkh,
          deadline = BigInt(deadline),
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )
        val redeemer: Action = Action.Create(
          goal = BigInt(goal),
          recipient = recipientPkh,
          deadline = BigInt(deadline)
        )

        val nftAsset = AssetName(campaignId)
        val mintedValue = Value.asset(crowdfundingPolicyId, nftAsset, 1L)

        val tx = Await.result(
          TxBuilder(emulator.cardanoInfo)
              .spend(seedUtxo)
              .mint(crowdfundingScript, Map(nftAsset -> 1L), redeemer, Set(recipientKeyHash))
              .payTo(scriptAddress, Value(Coin(2_000_000L)) + mintedValue, datum)
              .validTo(Instant.ofEpochMilli(deadline - 1000))
              .complete(emulator, recipientP.address)
              .map(_.sign(recipientP.signer).transaction),
          Duration.Inf
        )

        val submitResult = Await.result(emulator.submit(tx), Duration.Inf)
        assert(submitResult.isRight, s"Campaign creation failed: $submitResult")

        (emulator, campaignId)
    }
}
