package scalus.examples.crowdfunding

import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.compiler.Options
import scalus.ledger.api.v1.{Address, Credential, PubKeyHash}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.PlutusV3
import scalus.Compile

// ============================================================================
// DATA MODELS
// ============================================================================

/** Campaign datum representing the state of a crowdfunding campaign
  *
  * @param totalSum
  *   Total amount collected in lovelace
  * @param goal
  *   Funding goal in lovelace
  * @param recipient
  *   Public key hash of the campaign recipient
  * @param deadline
  *   POSIX time when the campaign ends
  * @param withdrawn
  *   Total amount already withdrawn/reclaimed (for incremental operations)
  * @param donationPolicyId
  *   Policy ID of the donation tokens for this campaign
  */
case class CampaignDatum(
    totalSum: BigInt,
    goal: BigInt,
    recipient: PubKeyHash,
    deadline: PosixTime,
    withdrawn: BigInt,
    donationPolicyId: PolicyId
) derives Data.FromData,
      Data.ToData

@Compile
object CampaignDatum {
    given Eq[CampaignDatum] = Eq.derived
}

/** Datum for donation UTxOs at the script address.
  *
  * Each donation UTxO at the script address contains:
  *   - Donated ADA (may include extra for min UTxO)
  *   - Donation token (proves donation exists)
  *   - This datum (identifies donor and stores actual donation amount)
  *
  * @param donor
  *   Public key hash of the original donor (for reclaim authorization)
  * @param amount
  *   The actual donation amount in lovelace
  */
case class DonationDatum(
    donor: PubKeyHash,
    amount: BigInt
) derives Data.FromData,
      Data.ToData

@Compile
object DonationDatum {
    given Eq[DonationDatum] = Eq.derived
}

/** Actions that can be performed on the crowdfunding contract
  *
  * All actions use indexed UTxO pattern for O(1) lookups. Indices are computed off-chain using
  * delayed redeemer pattern.
  */
enum Action derives Data.FromData, Data.ToData:
    /** Create a new campaign by minting campaign NFT */
    case Create(goal: BigInt, recipient: PubKeyHash, deadline: PosixTime)

    /** Donate to a campaign
      * @param amount
      *   Donation amount in lovelace
      * @param campaignInputIdx
      *   Index of campaign input in txInfo.inputs
      * @param campaignOutputIdx
      *   Index of campaign output in txInfo.outputs
      * @param donationOutputIdx
      *   Index of donation value output in txInfo.outputs
      */
    case Donate(
        amount: BigInt,
        campaignInputIdx: BigInt,
        campaignOutputIdx: BigInt,
        donationOutputIdx: BigInt
    )

    /** Withdraw funds (recipient claims after successful campaign)
      * @param campaignInputIdx
      *   Index of campaign input
      * @param campaignOutputIdx
      *   Index of campaign output (-1 if fully withdrawn)
      * @param recipientOutputIdx
      *   Index of recipient's payout output
      * @param donationInputIndices
      *   Indices of donation UTxOs being consumed
      */
    case Withdraw(
        campaignInputIdx: BigInt,
        campaignOutputIdx: BigInt,
        recipientOutputIdx: BigInt,
        donationInputIndices: List[BigInt]
    )

    /** Reclaim funds (token holders reclaim after failed campaign)
      * @param campaignInputIdx
      *   Index of campaign input
      * @param campaignOutputIdx
      *   Index of campaign output (-1 if fully reclaimed)
      * @param donationInputIndices
      *   Indices of donation UTxOs being consumed
      * @param reclaimerOutputIndices
      *   Indices of outputs returning funds to token holders
      */
    case Reclaim(
        campaignInputIdx: BigInt,
        campaignOutputIdx: BigInt,
        donationInputIndices: List[BigInt],
        reclaimerOutputIndices: List[BigInt]
    )

@Compile
object Action

// ============================================================================
// DONATION MINTING POLICY (Parameterized by campaignId)
// ============================================================================

/** Minting policy for donation tokens, parameterized by campaign ID.
  *
  * Token name = donation amount encoded as ByteString. This makes tokens transferable and fungible
  * by amount - whoever holds the token can withdraw/reclaim that amount.
  *
  * Security: Burning requires the campaign UTxO to be spent in the same transaction. The campaign
  * spending is validated by CrowdfundingValidator which enforces deadline and goal conditions.
  */
@Compile
object DonationMintingPolicy {

    /** Fixed token name for all donation tokens.
      *
      * The donation amount is stored in the UTxO's lovelace value, not encoded in the token name.
      * This simplifies the design and avoids integer encoding/overflow issues.
      */
    val donationTokenName: ByteString = ByteString.empty

    inline def validate(param: Data)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                val campaignId = param.to[ByteString]
                val action = sc.redeemer.to[Action]

                action match
                    case Action.Donate(amount, campaignInputIdx, _, _) =>
                        handleDonateMint(campaignId, policyId, sc.txInfo, amount, campaignInputIdx)
                    case Action.Withdraw(campaignInputIdx, _, _, _) =>
                        handleBurn(campaignId, policyId, sc.txInfo, campaignInputIdx)
                    case Action.Reclaim(campaignInputIdx, _, _, _) =>
                        handleBurn(campaignId, policyId, sc.txInfo, campaignInputIdx)
                    case Action.Create(_, _, _) =>
                        fail("Create action does not mint donation tokens")
            case _ => fail("Unsupported script purpose")
    }

    private inline def handleDonateMint(
        campaignId: ByteString,
        policyId: PolicyId,
        txInfo: TxInfo,
        amount: BigInt,
        campaignInputIdx: BigInt
    ): Unit =
        // 1. Verify amount is positive
        require(amount > BigInt(0), "Donation amount must be positive")

        // 2. Find campaign input and verify it has the campaign NFT
        val campaignInput = txInfo.inputs.at(campaignInputIdx)
        val campaignDatum = campaignInput.resolved.datum match
            case OutputDatum.OutputDatum(d) => d.to[CampaignDatum]
            case _                          => fail("Campaign must have inline datum")

        // 3. Verify this donation policy matches the campaign's expected policy
        require(
          campaignDatum.donationPolicyId === policyId,
          "Donation policy must match campaign's expected policy"
        )

        // 4. Verify we're before deadline
        require(
          txInfo.validRange.isEntirelyBefore(campaignDatum.deadline),
          "Donations must be before deadline"
        )

        // 5. Verify exactly one donation token is minted
        require(
          txInfo.mint.quantityOf(policyId, donationTokenName) === BigInt(1),
          "Exactly one donation token must be minted"
        )

        // 6. Verify no other tokens are minted under this policy (V011 protection)
        val allMintedUnderPolicy = txInfo.mint.flatten.filter { case (pid, _, _) =>
            pid === policyId
        }
        require(
          allMintedUnderPolicy.length === BigInt(1),
          "Only one token type may be minted under donation policy"
        )

    /** Handle burning of donation tokens.
      *
      * Security: Verifies that the campaign UTxO is being spent in this transaction. The campaign
      * spending goes through CrowdfundingValidator which validates all conditions (deadline, goal,
      * signatures).
      */
    private inline def handleBurn(
        campaignId: ByteString,
        policyId: PolicyId,
        txInfo: TxInfo,
        campaignInputIdx: BigInt
    ): Unit =
        // 1. Verify campaign UTxO is being spent (this triggers CrowdfundingValidator)
        val campaignInput = txInfo.inputs.at(campaignInputIdx)
        val campaignDatum = campaignInput.resolved.datum match
            case OutputDatum.OutputDatum(d) => d.to[CampaignDatum]
            case _                          => fail("Campaign must have inline datum")

        // 2. Verify this is the correct campaign by checking donation policy matches
        require(
          campaignDatum.donationPolicyId === policyId,
          "Campaign donation policy must match this policy"
        )

        // 3. All tokens of this policy must be burned (negative quantity)
        val mintedTokens = txInfo.mint.tokens(policyId)
        require(
          mintedTokens.forall { case (_, amount) => amount < BigInt(0) },
          "Only burning allowed during withdraw/reclaim"
        )
}

// ============================================================================
// CROWDFUNDING VALIDATOR (Main Script)
// ============================================================================

/** Main crowdfunding validator handling campaign NFT minting and UTxO spending. */
@Compile
object CrowdfundingValidator extends Validator {

    inline override def spend(
        @annotation.unused datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        redeemer.to[Action] match
            case Action.Donate(amount, campaignInputIdx, campaignOutputIdx, donationOutputIdx) =>
                val input = txInfo.inputs.at(campaignInputIdx)
                require(input.outRef === txOutRef, "Input index does not match txOutRef")

                val (scriptHash, currentDatum) = input.resolved match
                    case TxOut(
                          Address(Credential.ScriptCredential(sh), _),
                          _,
                          OutputDatum.OutputDatum(inlineDatum),
                          _
                        ) =>
                        (sh, inlineDatum.to[CampaignDatum])
                    case _ => fail("Campaign input must have script credential and inline datum")

                handleDonateSpend(
                  txInfo,
                  scriptHash,
                  currentDatum,
                  amount,
                  campaignOutputIdx,
                  donationOutputIdx
                )

            case Action.Withdraw(
                  campaignInputIdx,
                  campaignOutputIdx,
                  recipientOutputIdx,
                  donationInputIndices
                ) =>
                val campaignInput = txInfo.inputs.at(campaignInputIdx)

                // Check if this is the campaign UTxO or a donation value UTxO
                if campaignInput.outRef === txOutRef then
                    // This is the campaign UTxO - do full validation
                    val (scriptHash, currentDatum) = campaignInput.resolved match
                        case TxOut(
                              Address(Credential.ScriptCredential(sh), _),
                              value,
                              OutputDatum.OutputDatum(inlineDatum),
                              _
                            ) =>
                            // Verify campaign NFT exists (policyId = scriptHash)
                            verifyCampaignNftPresent(value, sh)
                            (sh, inlineDatum.to[CampaignDatum])
                        case _ =>
                            fail("Campaign input must have script credential and inline datum")

                    handleWithdrawSpend(
                      txInfo,
                      scriptHash,
                      currentDatum,
                      campaignOutputIdx,
                      recipientOutputIdx,
                      donationInputIndices
                    )
                else
                    // This is a donation value UTxO - just verify it's in the list
                    val isInDonationList = donationInputIndices.exists { idx =>
                        txInfo.inputs.at(idx).outRef === txOutRef
                    }
                    require(isInDonationList, "Donation UTxO must be in donationInputIndices")

            case Action.Reclaim(
                  campaignInputIdx,
                  campaignOutputIdx,
                  donationInputIndices,
                  reclaimerOutputIndices
                ) =>
                val campaignInput = txInfo.inputs.at(campaignInputIdx)

                // Check if this is the campaign UTxO or a donation value UTxO
                if campaignInput.outRef === txOutRef then
                    // This is the campaign UTxO - do full validation
                    val (scriptHash, currentDatum) = campaignInput.resolved match
                        case TxOut(
                              Address(Credential.ScriptCredential(sh), _),
                              value,
                              OutputDatum.OutputDatum(inlineDatum),
                              _
                            ) =>
                            // Verify campaign NFT exists (policyId = scriptHash)
                            verifyCampaignNftPresent(value, sh)
                            (sh, inlineDatum.to[CampaignDatum])
                        case _ =>
                            fail("Campaign input must have script credential and inline datum")

                    handleReclaimSpend(
                      txInfo,
                      scriptHash,
                      currentDatum,
                      campaignOutputIdx,
                      donationInputIndices,
                      reclaimerOutputIndices
                    )
                else
                    // This is a donation value UTxO - just verify it's in the list
                    val isInDonationList = donationInputIndices.exists { idx =>
                        txInfo.inputs.at(idx).outRef === txOutRef
                    }
                    require(isInDonationList, "Donation UTxO must be in donationInputIndices")

            case Action.Create(_, _, _) =>
                fail("Create action is only valid for minting")

    /** Handle donation spend - validates campaign UTxO update */
    private inline def handleDonateSpend(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        currentDatum: CampaignDatum,
        amount: BigInt,
        campaignOutputIdx: BigInt,
        donationOutputIdx: BigInt
    ): Unit =
        // 1. Time validation: must be before deadline
        require(
          txInfo.validRange.isEntirelyBefore(currentDatum.deadline),
          "Donation must be before deadline"
        )

        // 2. Amount must be positive
        require(amount > BigInt(0), "Donation amount must be positive")

        // 3. Verify continuing campaign output
        val campaignOutput = txInfo.outputs.at(campaignOutputIdx)
        val newDatum = campaignOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[CampaignDatum]
            case _                          => fail("Campaign output must have inline datum")

        // 4. Verify datum update - only totalSum should change
        val expectedDatum = CampaignDatum(
          totalSum = currentDatum.totalSum + amount,
          goal = currentDatum.goal,
          recipient = currentDatum.recipient,
          deadline = currentDatum.deadline,
          withdrawn = currentDatum.withdrawn,
          donationPolicyId = currentDatum.donationPolicyId
        )
        require(newDatum === expectedDatum, "Updated datum must reflect donation")

        // 5. Verify donation UTxO is created at script address with token + ADA + DonationDatum
        val donationOutput = txInfo.outputs.at(donationOutputIdx)
        require(
          donationOutput.address === Address.fromScriptHash(scriptHash),
          "Donation output must go to script address"
        )
        require(
          donationOutput.value.getLovelace >= amount,
          "Donation output must contain at least the donation amount"
        )

        // 6. Verify donation token is minted and goes to donation UTxO
        // 6. Verify donation token is minted and goes to donation UTxO
        val tokenName = DonationMintingPolicy.donationTokenName
        require(
          txInfo.mint.quantityOf(currentDatum.donationPolicyId, tokenName) === BigInt(1),
          "Donation token must be minted"
        )
        require(
          donationOutput.value.quantityOf(currentDatum.donationPolicyId, tokenName) === BigInt(1),
          "Donation token must be in donation UTxO"
        )

        // 7. Verify donation UTxO has DonationDatum with correct amount
        donationOutput.datum match
            case OutputDatum.OutputDatum(d) =>
                val donationDatum = d.to[DonationDatum]
                require(
                  donationDatum.amount === amount,
                  "DonationDatum must contain correct amount"
                )
            case _ => fail("Donation output must have inline DonationDatum")

    /** Handle withdraw spend - validates fund transfer to recipient */
    private inline def handleWithdrawSpend(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        currentDatum: CampaignDatum,
        campaignOutputIdx: BigInt,
        recipientOutputIdx: BigInt,
        donationInputIndices: List[BigInt]
    ): Unit =
        // 1. Time validation: must be after deadline
        require(
          txInfo.validRange.isEntirelyAfter(currentDatum.deadline),
          "Withdraw only allowed after deadline"
        )

        // 2. Goal must be reached
        require(
          currentDatum.totalSum >= currentDatum.goal,
          "Goal must be reached for withdrawal"
        )

        // 3. Recipient must sign
        require(
          txInfo.isSignedBy(currentDatum.recipient),
          "Recipient must sign withdrawal"
        )

        // 4. Verify donation indices are unique (prevents double-spend attack)
        requireStrictlyAscending(donationInputIndices)

        // 5. Calculate total being withdrawn from donation inputs
        val totalWithdrawn = donationInputIndices.foldLeft(BigInt(0)) { (sum, idx) =>
            val donationInput = txInfo.inputs.at(idx)
            sum + donationInput.resolved.value.getLovelace
        }

        // 6. Verify recipient receives the funds
        val recipientOutput = txInfo.outputs.at(recipientOutputIdx)
        require(
          recipientOutput.address === Address.fromPubKeyHash(currentDatum.recipient),
          "Funds must go to recipient"
        )
        require(
          recipientOutput.value.getLovelace >= totalWithdrawn,
          "Recipient must receive withdrawn amount"
        )

        // 7. Verify donation tokens are burned
        verifyDonationsBurned(txInfo, currentDatum.donationPolicyId, donationInputIndices)

        // 8. Verify campaign output or removal
        val newWithdrawn = currentDatum.withdrawn + totalWithdrawn
        if newWithdrawn === currentDatum.totalSum then
            // Full withdrawal - campaign is complete
            ()
        else
            // Partial withdrawal - verify updated campaign datum
            val campaignOutput = txInfo.outputs.at(campaignOutputIdx)
            val newDatum = campaignOutput.datum match
                case OutputDatum.OutputDatum(d) => d.to[CampaignDatum]
                case _                          => fail("Campaign output must have inline datum")
            // Verify all immutable fields remain unchanged, only withdrawn updates (V015 protection)
            val expectedDatum = CampaignDatum(
              totalSum = currentDatum.totalSum,
              goal = currentDatum.goal,
              recipient = currentDatum.recipient,
              deadline = currentDatum.deadline,
              withdrawn = newWithdrawn,
              donationPolicyId = currentDatum.donationPolicyId
            )
            require(newDatum === expectedDatum, "Only withdrawn field may change")
            // Verify campaign NFT is preserved in output
            verifyCampaignNftPresent(campaignOutput.value, scriptHash)

    /** Handle reclaim spend - validates fund return to token holders */
    private inline def handleReclaimSpend(
        txInfo: TxInfo,
        scriptHash: ValidatorHash,
        currentDatum: CampaignDatum,
        campaignOutputIdx: BigInt,
        donationInputIndices: List[BigInt],
        reclaimerOutputIndices: List[BigInt]
    ): Unit =
        // 1. Time validation: must be after deadline
        require(
          txInfo.validRange.isEntirelyAfter(currentDatum.deadline),
          "Reclaim only allowed after deadline"
        )

        // 2. Goal must NOT be reached
        require(
          currentDatum.totalSum < currentDatum.goal,
          "Cannot reclaim if goal was reached"
        )

        // 3. Verify donation indices are unique (prevents double-spend attack)
        requireStrictlyAscending(donationInputIndices)

        // 4. Verify each donation is returned to the original donor (from DonationDatum)
        val totalReclaimed =
            donationInputIndices.zip(reclaimerOutputIndices).foldLeft(BigInt(0)) {
                case (sum, (donationIdx, reclaimerOutIdx)) =>
                    val donationInput = txInfo.inputs.at(donationIdx)

                    // Get donor and amount from DonationDatum
                    val donationDatum = donationInput.resolved.datum match
                        case OutputDatum.OutputDatum(d) => d.to[DonationDatum]
                        case _ => fail("Donation input must have inline DonationDatum")
                    val donorPkh = donationDatum.donor
                    val donationAmount = donationDatum.amount

                    // Verify funds go to the original donor
                    val reclaimerOutput = txInfo.outputs.at(reclaimerOutIdx)
                    require(
                      reclaimerOutput.address === Address.fromPubKeyHash(donorPkh),
                      "Funds must return to original donor"
                    )
                    require(
                      reclaimerOutput.value.getLovelace >= donationAmount,
                      "Donor must receive at least their donation amount"
                    )

                    sum + donationAmount
            }

        // 5. Verify donation tokens are burned
        verifyDonationsBurned(txInfo, currentDatum.donationPolicyId, donationInputIndices)

        // 6. Verify campaign output or removal
        val newWithdrawn = currentDatum.withdrawn + totalReclaimed
        if newWithdrawn === currentDatum.totalSum then
            // Full reclaim - campaign is complete
            ()
        else
            // Partial reclaim - verify updated campaign datum
            val campaignOutput = txInfo.outputs.at(campaignOutputIdx)
            val newDatum = campaignOutput.datum match
                case OutputDatum.OutputDatum(d) => d.to[CampaignDatum]
                case _                          => fail("Campaign output must have inline datum")
            // Verify all immutable fields remain unchanged, only withdrawn updates (V015 protection)
            val expectedDatum = CampaignDatum(
              totalSum = currentDatum.totalSum,
              goal = currentDatum.goal,
              recipient = currentDatum.recipient,
              deadline = currentDatum.deadline,
              withdrawn = newWithdrawn,
              donationPolicyId = currentDatum.donationPolicyId
            )
            require(newDatum === expectedDatum, "Only withdrawn field may change")
            // Verify campaign NFT is preserved in output
            verifyCampaignNftPresent(campaignOutput.value, scriptHash)

    /** Verify that the campaign UTxO contains exactly one campaign NFT.
      *
      * This prevents attacks using fake campaign UTxOs without the NFT. The campaign NFT has
      * policyId = scriptHash, so we check for exactly one token from that policy.
      */
    def verifyCampaignNftPresent(value: Value, scriptHash: ValidatorHash): Unit =
        val nftTokens = value.tokens(scriptHash)
        // Must have exactly one token type with quantity 1
        val hasExactlyOneNft =
            nftTokens.size === BigInt(1) &&
                nftTokens.forall { case (_, qty) => qty === BigInt(1) }
        require(hasExactlyOneNft, "Campaign input must contain exactly one campaign NFT")

    /** Verify that indices are strictly ascending (which guarantees uniqueness).
      *
      * This prevents double-spending attacks where the same donation UTxO index is referenced
      * multiple times in the redeemer.
      */
    def requireStrictlyAscending(indices: List[BigInt]): Unit =
        // Use fold to check consecutive pairs: track previous value, verify each is greater
        // Start with minimum possible value so first element always passes
        indices.foldLeft(BigInt(-1)) { (prev, curr) =>
            require(prev < curr, "Donation indices must be strictly ascending (no duplicates)")
            curr
        }
        ()

    /** Verify that donation tokens are burned for the given donation inputs.
      *
      * Gets the token name from the donation UTxO's tokens (not from lovelace amount, which may
      * include extra for min UTxO requirements).
      */
    private inline def verifyDonationsBurned(
        txInfo: TxInfo,
        donationPolicyId: PolicyId,
        donationInputIndices: List[BigInt]
    ): Unit =
        val tokenName = DonationMintingPolicy.donationTokenName
        // Count donation tokens and verify each input has exactly 1
        val tokenCount = donationInputIndices.foldLeft(BigInt(0)) { (count, idx) =>
            val donationInput = txInfo.inputs.at(idx)
            val tokens = donationInput.resolved.value.tokens(donationPolicyId)
            val hasOneToken = tokens.get(tokenName) match
                case Option.Some(qty) => tokens.size === BigInt(1) && qty === BigInt(1)
                case Option.None      => false
            require(hasOneToken, "Donation input must have exactly 1 donation token")
            count + BigInt(1)
        }
        // Verify exact number of tokens are burned
        require(
          txInfo.mint.quantityOf(donationPolicyId, tokenName) === -tokenCount,
          "All donation tokens must be burned"
        )

    inline override def mint(
        redeemer: Data,
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit =
        redeemer.to[Action] match
            case Action.Create(goal, recipient, deadline) =>
                handleCreateMint(policyId, txInfo, goal, recipient, deadline)
            case _ =>
                // Burning campaign NFT is allowed at end
                handleBurn(policyId, txInfo)

    private inline def handleCreateMint(
        policyId: PolicyId,
        txInfo: TxInfo,
        goal: BigInt,
        recipient: PubKeyHash,
        deadline: PosixTime
    ): Unit =
        // 1. Recipient must sign
        require(
          txInfo.isSignedBy(recipient),
          "Recipient must sign campaign creation"
        )

        // 2. Goal must be positive
        require(goal > BigInt(0), "Goal must be positive")

        // 3. Deadline must be in the future
        require(
          txInfo.validRange.isEntirelyBefore(deadline),
          "Deadline must be in the future"
        )

        // 4. Find a consumed UTxO to derive unique campaign ID
        val consumedUtxo = txInfo.inputs.match
            case List.Cons(first, _) => first.outRef
            case List.Nil            => fail("Must consume at least one UTxO")

        // Hash the serialized TxOutRef to get a 32-byte campaign ID (AssetName limit)
        val campaignId = scalus.builtin.Builtins.blake2b_256(
          scalus.builtin.Builtins.serialiseData(consumedUtxo.toData)
        )

        // 5. Verify exactly one campaign NFT is minted
        require(
          txInfo.mint.quantityOf(policyId, campaignId) === BigInt(1),
          "Exactly one campaign NFT must be minted"
        )

        // 5a. Verify no other tokens are minted under this policy (V011 protection)
        val allMintedUnderPolicy = txInfo.mint.flatten.filter { case (pid, _, _) =>
            pid === policyId
        }
        require(
          allMintedUnderPolicy.length === BigInt(1),
          "Only one token type may be minted under campaign policy"
        )

        // 6. Find the output going to the script address
        val campaignOutput = txInfo.outputs.filter { out =>
            out.address === Address.fromScriptHash(policyId)
        }.match
            case List.Cons(out, List.Nil) => out
            case _ => fail("There must be exactly one output to the campaign script")

        // 7. Verify the output contains the minted NFT
        require(
          campaignOutput.value.quantityOf(policyId, campaignId) === BigInt(1),
          "Campaign output must contain the minted NFT"
        )

        // 8. Get the donation policy ID from datum (computed off-chain)
        val donationPolicyId = campaignOutput.datum match
            case OutputDatum.OutputDatum(d) => d.to[CampaignDatum].donationPolicyId
            case _                          => fail("Campaign output must have inline datum")

        // 9. Verify the datum is correct
        val expectedDatum = CampaignDatum(
          totalSum = BigInt(0),
          goal = goal,
          recipient = recipient,
          deadline = deadline,
          withdrawn = BigInt(0),
          donationPolicyId = donationPolicyId
        )
        campaignOutput.datum match
            case OutputDatum.OutputDatum(datumData) =>
                require(
                  datumData.to[CampaignDatum] === expectedDatum,
                  "Initial campaign datum must be correct"
                )
            case _ => fail("Campaign output must have inline datum")

    private inline def handleBurn(
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit =
        val mintedTokens = txInfo.mint.tokens(policyId)
        require(
          mintedTokens.forall { case (_, amount) => amount < BigInt(0) },
          "Only burning is allowed"
        )
}

// ============================================================================
// COMPILATION
// ============================================================================

private given Options = Options.release

lazy val CrowdfundingContract = PlutusV3.compile(CrowdfundingValidator.validate)
lazy val DonationMintingContract = PlutusV3.compile(DonationMintingPolicy.validate)
