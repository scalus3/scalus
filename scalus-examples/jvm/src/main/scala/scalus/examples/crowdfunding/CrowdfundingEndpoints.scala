package scalus.examples.crowdfunding

import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address as CardanoAddress, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, AssetName, CardanoInfo, Coin, DatumOption, Script, ScriptHash, Transaction, Utxo, Value as LedgerValue}
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.ledger.api.v1.PubKeyHash
import scalus.uplc.PlutusV3

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/** Endpoints for building crowdfunding transactions.
  *
  * Handles off-chain transaction construction using the delayed redeemer pattern for computing UTxO
  * indices.
  *
  * @param env
  *   Cardano network information
  * @param provider
  *   Node provider for querying UTxOs and submitting transactions
  * @param crowdfundingContract
  *   Compiled crowdfunding validator
  * @param donationMintingContract
  *   Compiled donation minting policy (parameterized)
  */
class CrowdfundingEndpoints(
    env: CardanoInfo,
    provider: Provider,
    crowdfundingContract: PlutusV3[Data => Unit],
    donationMintingContract: PlutusV3[Data => Data => Unit]
) {
    private val crowdfundingScript = crowdfundingContract.script
    private val crowdfundingPolicyId = crowdfundingScript.scriptHash
    val scriptAddress: CardanoAddress = crowdfundingContract.address(env.network)

    /** Extract PubKeyHash from a ShelleyAddress */
    private def extractPkh(address: ShelleyAddress): PubKeyHash =
        address.payment match
            case ShelleyPaymentPart.Key(hash) => PubKeyHash(hash)
            case _ => throw IllegalArgumentException("Address must have key payment credential")

    /** Create a ShelleyAddress from a PubKeyHash */
    private def addressFromPkh(pkh: PubKeyHash): ShelleyAddress =
        ShelleyAddress(
          env.network,
          ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(pkh.hash)),
          ShelleyDelegationPart.Null
        )

    /** Apply campaign ID to donation minting policy and get the script.
      *
      * The donation minting policy is parameterized by campaignId, resulting in a unique policyId
      * per campaign.
      */
    private def getDonationScript(campaignId: ByteString): Script.PlutusV3 =
        val appliedProgram = donationMintingContract.program $ campaignId.toData
        Script.PlutusV3(appliedProgram.cborByteString)

    /** Compute the donation policy ID for a given campaign ID. */
    def computeDonationPolicyId(campaignId: ByteString): ByteString =
        getDonationScript(campaignId).scriptHash

    /** Creates a new crowdfunding campaign.
      *
      * Mints a campaign NFT and creates the initial campaign UTxO with the specified parameters.
      *
      * @param recipientAddress
      *   Address of the campaign recipient (receives funds if goal is reached)
      * @param goal
      *   Funding goal in lovelace
      * @param deadline
      *   POSIX timestamp when the campaign ends
      * @param initialValue
      *   Initial ADA locked with the campaign (for min UTxO requirements)
      * @param signer
      *   Transaction signer with recipient's keys
      * @return
      *   The submitted transaction and the campaign ID
      */
    def createCampaign(
        recipientAddress: ShelleyAddress,
        goal: Long,
        deadline: Long,
        initialValue: Coin,
        signer: TransactionSigner
    )(using ExecutionContext): Future[(Transaction, ByteString)] =
        val recipientPkh = extractPkh(recipientAddress)
        val recipientAddrKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)

        for
            // Get UTxOs to find one for deriving campaign ID
            utxos <- provider
                .findUtxos(recipientAddress)
                .map(_.getOrElse(Map.empty))

            _ = if utxos.isEmpty then throw RuntimeException("No UTxOs found at recipient address")

            // Use first UTxO to derive campaign ID (same as validator does)
            // Hash the serialized TxOutRef to get a 32-byte campaign ID (AssetName limit)
            firstUtxo = utxos.head
            txOutRef = scalus.ledger.api.v3.TxOutRef(
              scalus.ledger.api.v3.TxId(firstUtxo._1.transactionId),
              firstUtxo._1.index
            )
            campaignId = scalus.builtin.Builtins.blake2b_256(
              scalus.builtin.Builtins.serialiseData(txOutRef.toData)
            )

            // Compute donation policy ID for this campaign
            donationPolicyId = computeDonationPolicyId(campaignId)

            datum = CampaignDatum(
              totalSum = BigInt(0),
              goal = BigInt(goal),
              recipient = recipientPkh,
              deadline = BigInt(deadline),
              withdrawn = BigInt(0),
              donationPolicyId = donationPolicyId
            )

            redeemer = Action.Create(
              goal = BigInt(goal),
              recipient = recipientPkh,
              deadline = BigInt(deadline)
            )

            nftAsset = AssetName(campaignId)
            mintedValue = LedgerValue.asset(crowdfundingPolicyId, nftAsset, 1L)

            // Create UTxO object from the first utxo (which is used to derive campaignId)
            seedUtxo = Utxo(firstUtxo._1, firstUtxo._2)

            tx <- TxBuilder(env)
                .spend(
                  seedUtxo
                ) // Must spend this UTxO - validator derives campaignId from first input
                .mint(crowdfundingScript, Map(nftAsset -> 1L), redeemer, Set(recipientAddrKeyHash))
                .payTo(scriptAddress, LedgerValue(initialValue) + mintedValue, datum)
                .validTo(Instant.ofEpochMilli(deadline - 1000))
                .complete(provider, recipientAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield (tx, campaignId)

    /** Donates to a campaign.
      *
      * Creates a donation by:
      *   - Spending the campaign UTxO and updating its totalSum
      *   - Minting a donation token (token name = encoded amount)
      *   - Creating a donation value UTxO at the script address
      *   - Sending the donation token to the donor
      *
      * @param campaignId
      *   The campaign identifier (token name of campaign NFT)
      * @param donorAddress
      *   Address of the donor
      * @param amount
      *   Donation amount in lovelace
      * @param signer
      *   Transaction signer with donor's keys
      * @return
      *   The submitted transaction
      */
    def donate(
        campaignId: ByteString,
        donorAddress: ShelleyAddress,
        amount: Long,
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
        for
            campaignUtxo <- findCampaignUtxo(campaignId).map(
              _.getOrElse(throw RuntimeException(s"No campaign found for id: $campaignId"))
            )
            currentDatum = extractCampaignDatum(campaignUtxo)

            // Compute donation policy and script for this campaign
            donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
            donationScript = getDonationScript(campaignId)

            // Create updated campaign datum
            newDatum = currentDatum.copy(
              totalSum = currentDatum.totalSum + BigInt(amount)
            )

            // Donation token: name = encoded amount
            tokenName = DonationMintingPolicy.encodeAmount(BigInt(amount))
            donationAsset = AssetName(tokenName)
            donationTokenValue = LedgerValue.asset(donationPolicyId, donationAsset, 1L)

            // Campaign NFT must be preserved
            nftAsset = AssetName(campaignId)
            nftValue = LedgerValue.asset(crowdfundingPolicyId, nftAsset, 1L)

            // New campaign value = current + donation amount
            newCampaignValue = LedgerValue.lovelace(
              campaignUtxo.output.value.coin.value + amount
            ) + nftValue

            // Unified donation UTxO: ADA + donation token (at script address)
            donationUtxoValue = LedgerValue.lovelace(amount) + donationTokenValue

            // DonationDatum identifies the original donor (for reclaim authorization)
            donorPkh = extractPkh(donorAddress)
            donationDatum = DonationDatum(donorPkh)

            // Build transaction with delayed redeemer
            tx <- TxBuilder(env)
                .spend(
                  campaignUtxo,
                  redeemerBuilder = (tx: Transaction) => {
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
                  },
                  crowdfundingScript,
                  Set.empty
                )
                .mint(
                  donationScript,
                  Map(donationAsset -> 1L),
                  (tx: Transaction) => {
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
                )
                .payTo(scriptAddress, newCampaignValue, newDatum) // Updated campaign UTxO
                // Unified donation UTxO: ADA + donation token + DonationDatum (at script address)
                .payTo(scriptAddress, donationUtxoValue, donationDatum)
                .validTo(Instant.ofEpochMilli(currentDatum.deadline.toLong - 1000))
                .complete(provider, donorAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Withdraws funds from a successful campaign.
      *
      * After the deadline, if the goal is reached, the recipient can withdraw donated funds by
      * burning the corresponding donation tokens.
      *
      * @param campaignId
      *   The campaign identifier
      * @param recipientAddress
      *   Address of the campaign recipient
      * @param donationUtxos
      *   Donation UTxOs at script address (contain both tokens and ADA)
      * @param signer
      *   Transaction signer with recipient's keys (no donor signatures needed - tokens are at
      *   script address)
      * @return
      *   The submitted transaction
      */
    def withdraw(
        campaignId: ByteString,
        recipientAddress: ShelleyAddress,
        donationUtxos: Seq[Utxo],
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
        val recipientPkh = extractPkh(recipientAddress)
        val recipientAddrKeyHash = AddrKeyHash.fromByteString(recipientPkh.hash)

        for
            campaignUtxo <- findCampaignUtxo(campaignId).map(
              _.getOrElse(throw RuntimeException(s"No campaign found for id: $campaignId"))
            )
            currentDatum = extractCampaignDatum(campaignUtxo)

            // Verify recipient matches
            _ = if currentDatum.recipient != recipientPkh then
                throw RuntimeException("Only campaign recipient can withdraw")

            donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
            donationScript = getDonationScript(campaignId)

            // Calculate total amount being withdrawn from donation UTxOs (get amount from tokens)
            tokenAmounts: Seq[BigInt] = donationUtxos.map { utxo =>
                val tokens = utxo.output.value.assets.assets.getOrElse(donationPolicyId, Map.empty)
                tokens.foldLeft(BigInt(0)) { case (acc, (name, qty)) =>
                    acc + DonationMintingPolicy.decodeAmount(name.bytes) * qty.toLong
                }
            }
            totalWithdrawAmount: BigInt = tokenAmounts.foldLeft(BigInt(0))(_ + _)

            // Calculate if this is full or partial withdrawal
            newWithdrawn = currentDatum.withdrawn + totalWithdrawAmount
            isFullWithdrawal = newWithdrawn == currentDatum.totalSum

            // Build burn map for donation tokens
            burnMap = donationUtxos
                .flatMap { utxo =>
                    utxo.output.value.assets.assets.getOrElse(donationPolicyId, Map.empty).map {
                        case (name, qty) => (name, -qty.toLong)
                    }
                }
                .groupBy(_._1)
                .map { case (name, pairs) => (name, pairs.map(_._2).sum) }

            nftAsset = AssetName(campaignId)

            // Helper to build the Withdraw redeemer
            withdrawRedeemer = (tx: Transaction) => {
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
                val donationInputIndices = donationUtxos.map { utxo =>
                    BigInt(tx.body.value.inputs.toSeq.indexOf(utxo.input))
                }
                Action
                    .Withdraw(
                      BigInt(inputIdx),
                      BigInt(campaignOutputIdx),
                      BigInt(recipientOutputIdx),
                      scalus.prelude.List.from(donationInputIndices)
                    )
                    .toData
            }

            // Build transaction: spend campaign UTxO
            builderWithCampaign = TxBuilder(env).spend(
              campaignUtxo,
              redeemerBuilder = withdrawRedeemer,
              crowdfundingScript,
              Set(recipientAddrKeyHash)
            )

            // Spend donation UTxOs (at script address - no donor signatures needed)
            builderWithDonations = donationUtxos.foldLeft(builderWithCampaign) { (builder, utxo) =>
                builder.spend(utxo, withdrawRedeemer, crowdfundingScript, Set.empty)
            }

            // Burn donation tokens
            builderWithBurn = builderWithDonations.mint(donationScript, burnMap, withdrawRedeemer)

            // Pay recipient
            builderWithRecipient =
                builderWithBurn.payTo(
                  recipientAddress,
                  LedgerValue.lovelace(totalWithdrawAmount.toLong)
                )

            // Update campaign UTxO if partial withdrawal
            builderWithCampaignOutput =
                if isFullWithdrawal then builderWithRecipient
                else
                    val nftValue = LedgerValue.asset(crowdfundingPolicyId, nftAsset, 1L)
                    val newDatum = currentDatum.copy(withdrawn = newWithdrawn)
                    builderWithRecipient.payTo(
                      scriptAddress,
                      LedgerValue.lovelace(2_000_000L) + nftValue,
                      newDatum
                    )

            tx <- builderWithCampaignOutput
                .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
                .complete(provider, recipientAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Reclaims funds from a failed campaign.
      *
      * After the deadline, if the goal is NOT reached, original donors can reclaim their donations.
      * Funds go back to the original donor identified in DonationDatum.
      *
      * @param campaignId
      *   The campaign identifier
      * @param donationUtxos
      *   Donation UTxOs to reclaim (at script address, contain tokens + ADA + DonationDatum)
      * @param signer
      *   Transaction signer (for fee payment)
      * @return
      *   The submitted transaction
      */
    def reclaim(
        campaignId: ByteString,
        donationUtxos: Seq[Utxo],
        signer: TransactionSigner
    )(using ExecutionContext): Future[Transaction] =
        for
            campaignUtxo <- findCampaignUtxo(campaignId).map(
              _.getOrElse(throw RuntimeException(s"No campaign found for id: $campaignId"))
            )
            currentDatum = extractCampaignDatum(campaignUtxo)

            donationPolicyId = ScriptHash.fromByteString(currentDatum.donationPolicyId)
            donationScript = getDonationScript(campaignId)

            // Extract donor info from DonationDatum and calculate amounts from tokens
            donorInfos: Seq[(ShelleyAddress, PubKeyHash, BigInt)] = donationUtxos.map { utxo =>
                val donorPkh = extractDonationDatum(utxo).donor
                val donorAddress = addressFromPkh(donorPkh)
                val tokens = utxo.output.value.assets.assets.getOrElse(donationPolicyId, Map.empty)
                val amount = tokens.foldLeft(BigInt(0)) { case (acc, (name, qty)) =>
                    acc + DonationMintingPolicy.decodeAmount(name.bytes) * qty.toLong
                }
                (donorAddress, donorPkh, amount)
            }
            totalReclaimAmount: BigInt = donorInfos.map(_._3).foldLeft(BigInt(0))(_ + _)

            // Collect all unique donor key hashes for required signers
            donorKeyHashes: Set[AddrKeyHash] = donorInfos.map { case (_, pkh, _) =>
                AddrKeyHash.fromByteString(pkh.hash)
            }.toSet

            // Calculate if this is full or partial reclaim
            newWithdrawn = currentDatum.withdrawn + totalReclaimAmount
            isFullReclaim = newWithdrawn == currentDatum.totalSum

            // Build burn map
            burnMap = donationUtxos
                .flatMap { utxo =>
                    utxo.output.value.assets.assets.getOrElse(donationPolicyId, Map.empty).map {
                        case (name, qty) => (name, -qty.toLong)
                    }
                }
                .groupBy(_._1)
                .map { case (name, pairs) => (name, pairs.map(_._2).sum) }

            nftAsset = AssetName(campaignId)

            // Helper to build Reclaim redeemer
            reclaimRedeemer = (tx: Transaction) => {
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
                val donationInputIndices = donationUtxos.map { utxo =>
                    BigInt(tx.body.value.inputs.toSeq.indexOf(utxo.input))
                }
                // Find output index for each donor
                val reclaimerOutputIndices = donorInfos.map { case (donorAddr, _, _) =>
                    BigInt(tx.body.value.outputs.indexWhere(_.value.address == donorAddr))
                }
                Action
                    .Reclaim(
                      BigInt(inputIdx),
                      BigInt(campaignOutputIdx),
                      scalus.prelude.List.from(donationInputIndices),
                      scalus.prelude.List.from(reclaimerOutputIndices)
                    )
                    .toData
            }

            // Build transaction: spend campaign UTxO
            builderWithCampaign = TxBuilder(env).spend(
              campaignUtxo,
              redeemerBuilder = reclaimRedeemer,
              crowdfundingScript,
              donorKeyHashes // Include donor key hashes for fee estimation
            )

            // Spend donation UTxOs (at script address)
            builderWithDonations = donationUtxos.foldLeft(builderWithCampaign) { (builder, utxo) =>
                builder.spend(utxo, reclaimRedeemer, crowdfundingScript, Set.empty)
            }

            // Burn donation tokens
            builderWithBurn = builderWithDonations.mint(donationScript, burnMap, reclaimRedeemer)

            // Pay each donor their reclaimed amount
            builderWithPayments = donorInfos.foldLeft(builderWithBurn) {
                case (builder, (donorAddr, _, amount)) =>
                    builder.payTo(donorAddr, LedgerValue.lovelace(amount.toLong))
            }

            // Update campaign UTxO if partial reclaim
            builderWithCampaignOutput =
                if isFullReclaim then builderWithPayments
                else
                    val nftValue = LedgerValue.asset(crowdfundingPolicyId, nftAsset, 1L)
                    val newDatum = currentDatum.copy(withdrawn = newWithdrawn)
                    builderWithPayments.payTo(
                      scriptAddress,
                      LedgerValue.lovelace(2_000_000L) + nftValue,
                      newDatum
                    )

            // Use first donor address for fee payment
            feePayerAddress = donorInfos.headOption
                .map { case (addr, _, _) => addr }
                .getOrElse(throw RuntimeException("No donation UTxOs provided"))

            tx <- builderWithCampaignOutput
                .validFrom(Instant.ofEpochMilli(currentDatum.deadline.toLong + 1000))
                .complete(provider, feePayerAddress)
                .map(_.sign(signer).transaction)

            _ <- provider.submit(tx).map {
                case Right(_)    => ()
                case Left(error) => throw RuntimeException(s"Failed to submit: $error")
            }
        yield tx

    /** Finds the campaign UTxO containing the campaign NFT with the given ID. */
    def findCampaignUtxo(
        campaignId: ByteString
    )(using ExecutionContext): Future[scala.Option[Utxo]] =
        for utxos <- provider
                .findUtxos(scriptAddress)
                .map(_.getOrElse(Map.empty))
        yield
            val nftAsset = AssetName(campaignId)
            utxos
                .find { case (_, output) =>
                    output.value.assets.assets
                        .get(crowdfundingPolicyId)
                        .exists(_.get(nftAsset).exists(_ > 0))
                }
                .map { case (input, output) =>
                    Utxo(input, output)
                }

    /** Finds all donation UTxOs at the script address for a campaign.
      *
      * Donation UTxOs are identified by:
      *   - Being at script address
      *   - NOT containing the campaign NFT
      *   - Containing a donation token (from the campaign's donation policy)
      */
    def findDonationUtxos(campaignId: ByteString)(using ExecutionContext): Future[Seq[Utxo]] =
        for
            utxos <- provider.findUtxos(scriptAddress).map(_.getOrElse(Map.empty))
            campaignUtxo <- findCampaignUtxo(campaignId)
            currentDatum = campaignUtxo.map(extractCampaignDatum)
        yield
            val nftAsset = AssetName(campaignId)
            val donationPolicyId = currentDatum
                .map(d => ScriptHash.fromByteString(d.donationPolicyId))
                .getOrElse(throw RuntimeException("Campaign not found"))

            utxos
                .filterNot { case (_, output) =>
                    // Exclude campaign UTxO (has NFT)
                    output.value.assets.assets
                        .get(crowdfundingPolicyId)
                        .exists(_.get(nftAsset).exists(_ > 0))
                }
                .filter { case (_, output) =>
                    // Include only UTxOs with donation tokens
                    output.value.assets.assets
                        .get(donationPolicyId)
                        .exists(_.nonEmpty)
                }
                .map { case (input, output) => Utxo(input, output) }
                .toSeq

    /** Extracts the CampaignDatum from a campaign UTxO. */
    private def extractCampaignDatum(utxo: Utxo): CampaignDatum =
        utxo.output.datumOption match
            case Some(DatumOption.Inline(data)) =>
                scalus.builtin.Data.fromData[CampaignDatum](data)
            case _ =>
                throw IllegalStateException("Expected inline datum in campaign UTxO")

    /** Extracts the DonationDatum from a donation UTxO. */
    private def extractDonationDatum(utxo: Utxo): DonationDatum =
        utxo.output.datumOption match
            case Some(DatumOption.Inline(data)) =>
                scalus.builtin.Data.fromData[DonationDatum](data)
            case _ =>
                throw IllegalStateException("Expected inline DonationDatum in donation UTxO")
}
