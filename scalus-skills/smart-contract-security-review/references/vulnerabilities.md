# Cardano Smart Contract Vulnerability Patterns

Detailed patterns for Scalus/Cardano smart contract security review with code examples.
Based on Cardano Developer Portal security guidelines and Scalus-specific patterns.

## Table of Contents

### Critical
1. [V001: Redirect Attack](#v001-redirect-attack)
2. [V002: Token/NFT Not Verified](#v002-tokennft-not-verified)
3. [V003: Inexact Burn/Mint Validation](#v003-inexact-burnmint-validation)
4. [V004: Integer Overflow](#v004-integer-overflow)
5. [V005: Double Satisfaction](#v005-double-satisfaction)

### High
6. [V006: Index Validation Missing](#v006-index-validation-missing)
7. [V007: Self-Dealing / Shill Bidding](#v007-self-dealing--shill-bidding)
8. [V008: Double Spend via Index Reuse](#v008-double-spend-via-index-reuse)
9. [V009: Inexact Refund Amount](#v009-inexact-refund-amount)
10. [V010: Other Redeemer Attack](#v010-other-redeemer-attack)
11. [V011: Other Token Name Attack](#v011-other-token-name-attack)
12. [V012: Missing UTxO Authentication](#v012-missing-utxo-authentication)

### Medium
13. [V013: Time Handling](#v013-time-handling)
14. [V014: Missing Signature Validation](#v014-missing-signature-validation)
15. [V015: Datum Mutation Not Validated](#v015-datum-mutation-not-validated)
16. [V016: Insufficient Staking Control](#v016-insufficient-staking-control)
17. [V017: Arbitrary Datum](#v017-arbitrary-datum)

### Low / Design Issues
18. [V018: Unbounded Value](#v018-unbounded-value)
19. [V019: Unbounded Datum](#v019-unbounded-datum)
20. [V020: Unbounded Inputs](#v020-unbounded-inputs)
21. [V021: UTxO Contention](#v021-utxo-contention)
22. [V022: Cheap Spam / Dust Attack](#v022-cheap-spam--dust-attack)
23. [V023: Locked Value](#v023-locked-value)

---

## V001: Redirect Attack

**Severity**: Critical

### Description
On Cardano, UTxOs are location-agnostic - they can be sent to any address. If a validator doesn't explicitly verify that continuing outputs go to its own script address, an attacker can redirect funds to a malicious contract.

### Attack Scenario
1. Attacker deploys a malicious script with same interface
2. Constructs transaction pointing continuing output to malicious script
3. Original validator accepts (it only checks datum/value, not destination)
4. Funds are now controlled by attacker's script

### Vulnerable Pattern
```scala
def handleBid(/* ... */) = {
  val continuingOutput = txInfo.outputs.at(outputIdx)
  // Only checks value and datum, NOT address
  require(
    continuingOutput.value.getLovelace >= newBidAmount,
    "Insufficient bid"
  )
  require(
    continuingOutput.datum.to[AuctionDatum] === expectedDatum,
    "Invalid datum"
  )
}
```

### Secure Pattern (from AuctionValidator)
```scala
def handleBid(/* ... */) = {
  val continuingOutput = txInfo.outputs.at(outputIdx)

  // CRITICAL: Verify address first
  require(
    continuingOutput.address === Address.fromScriptHash(scriptHash),
    "Continuing output must go to auction script address"
  )

  // Then check value and datum
  require(
    continuingOutput.value.getLovelace >= newBidAmount,
    "Insufficient bid"
  )
}
```

### Detection Patterns
Search for:
- `outputs.at(` without nearby `address ===`
- `txInfo.outputs` filtering without address validation
- Continuing output handling in spend validators

---

## V002: Token/NFT Not Verified

**Severity**: Critical

### Description
Contracts using NFTs or state tokens to identify instances must verify the token is present in outputs. Missing verification allows attackers to remove the token and break contract invariants.

### Attack Scenario
1. Contract uses NFT to identify campaign/auction
2. Attacker creates transaction that sends output without the NFT
3. Contract accepts because it doesn't verify token presence
4. Original NFT can be used to create duplicate "official" instances

### Vulnerable Pattern
```scala
def handleWithdraw(/* ... */) = {
  val continuingOutput = txInfo.outputs.at(outputIdx)
  // Checks amount but not NFT presence
  require(
    continuingOutput.value.getLovelace >= remainingAmount,
    "Insufficient remaining"
  )
}
```

### Secure Pattern (from CrowdfundingValidator)
```scala
def verifyCampaignNftPresent(output: TxOut, campaignPolicyId: PolicyId): Unit = {
  require(
    output.value.quantityOf(campaignPolicyId, campaignTokenName) === BigInt(1),
    "Campaign NFT must be present in output"
  )
}

def handleWithdraw(/* ... */) = {
  val continuingOutput = txInfo.outputs.at(outputIdx)
  verifyCampaignNftPresent(continuingOutput, campaignPolicyId)
  // ... rest of validation
}
```

### Detection Patterns
Search for:
- Contracts using `policyId` or `tokenName` without `quantityOf` checks
- State machine patterns without token verification on transitions
- NFT-based contracts without presence checks in continuing outputs

---

## V003: Inexact Burn/Mint Validation

**Severity**: Critical

### Description
Using `>=` instead of `===` for token mint/burn quantities allows attackers to mint extra tokens or bypass burn validation.

### Attack Scenario (Minting)
1. Contract allows minting with `>= 1` check
2. Attacker mints 1000 tokens in single transaction
3. All pass the `>= 1` validation

### Attack Scenario (Burning)
1. Contract requires burning tokens with `<= -tokenCount` check
2. Attacker burns only partial amount
3. Contract accepts because it doesn't verify exact count

### Vulnerable Pattern
```scala
def mint(/* ... */) = {
  // BAD: Allows minting unlimited tokens
  require(
    txInfo.mint.quantityOf(policyId, tokenName) >= BigInt(1),
    "Must mint at least one token"
  )
}

def burn(/* ... */) = {
  // BAD: Allows partial burns
  require(
    txInfo.mint.quantityOf(policyId, tokenName) <= -donationCount,
    "Must burn donation tokens"
  )
}
```

### Secure Pattern (from CrowdfundingValidator)
```scala
def mint(/* ... */) = {
  // GOOD: Exact minting validation
  require(
    txInfo.mint.quantityOf(policyId, tokenName) === BigInt(1),
    "Must mint exactly one token"
  )
}

def burn(/* ... */) = {
  // GOOD: Exact burn validation
  require(
    txInfo.mint.quantityOf(policyId, donationTokenName) === -tokenCount,
    "Must burn exactly the specified token count"
  )
}
```

### Detection Patterns
Search for:
- `mint.quantityOf` with `>=` or `<=` instead of `===`
- Flexible burn validation allowing partial burns
- Token minting without upper bound

---

## V004: Integer Overflow

**Severity**: Critical

### Description
Custom encoding schemes or arithmetic operations without bounds checking can overflow, leading to fund loss or validation bypass.

### Vulnerable Pattern
```scala
// Custom encoding without bounds checking
def encodeAmount(amount: BigInt, multiplier: BigInt): BigInt = {
  amount * multiplier  // Can overflow!
}

def decodeAmount(encoded: BigInt, multiplier: BigInt): BigInt = {
  encoded / multiplier  // May lose precision
}
```

### Secure Pattern
Remove custom encoding; store values directly in datum:
```scala
case class DonationDatum(
  donor: PubKeyHash,
  amount: BigInt,  // Store directly, no encoding
  campaignId: ByteString
)
```

If encoding is necessary, add bounds checking:
```scala
def encodeAmount(amount: BigInt): BigInt = {
  require(amount >= BigInt(0), "Amount must be non-negative")
  require(amount < BigInt(2).pow(64), "Amount too large")
  amount
}
```

---

## V005: Index Validation Missing

**Severity**: High

### Description
Using `indexOf` without checking for -1 or `at()` without bounds validation can lead to unexpected behavior.

### Vulnerable Pattern
```scala
def findDonation(donations: List[Donation], donor: PubKeyHash): Donation = {
  val idx = donations.indexOf(d => d.donor === donor)
  donations.at(idx)  // BAD: idx might be -1
}
```

### Secure Pattern (from CrowdfundingValidator)
```scala
def requireFound(idx: BigInt, message: String): Unit = {
  require(idx >= BigInt(0), message)
}

def findDonation(donations: List[Donation], donor: PubKeyHash): Donation = {
  val idx = donations.indexOf(d => d.donor === donor)
  requireFound(idx, s"Donation not found for donor")
  donations.at(idx)
}
```

### Detection Patterns
Search for:
- `.indexOf(` without subsequent `>= 0` check
- `.at(idx)` where `idx` comes from user input or `indexOf`
- List operations without bounds validation

---

## V006: Self-Dealing / Shill Bidding

**Severity**: High

### Description
In auction or betting contracts, if sellers can bid on their own items, they can manipulate prices or win their own auctions risk-free.

### Attack Scenario
1. Seller creates auction
2. Seller bids on own item using different address
3. If outbid, seller gets their money back
4. If wins, seller keeps both item and highest bid

### Secure Pattern (from AuctionValidator)
```scala
def handleBid(seller: PubKeyHash, bidder: PubKeyHash, /* ... */) = {
  // Prevent shill bidding
  require(
    !(bidder === seller),
    "Seller cannot bid on their own auction"
  )
}

def handleEnd(seller: PubKeyHash, winner: PubKeyHash, /* ... */) = {
  // Defense in depth: verify at settlement too
  require(
    !(winner === seller),
    "Seller cannot be the winner"
  )
}
```

### Detection Patterns
Search for:
- Auction/betting contracts without seller/bidder comparison
- Role-based contracts without role separation checks
- Two-party contracts allowing same address for both parties

---

## V007: Double Spend via Index Reuse

**Severity**: High

### Description
When processing multiple inputs/outputs via index arrays, lack of uniqueness validation allows double-spending.

### Vulnerable Pattern
```scala
def processMultipleDonations(indices: List[BigInt], /* ... */) = {
  // BAD: Same index can appear multiple times
  indices.foreach { idx =>
    processDonation(donations.at(idx))
  }
}
```

### Secure Pattern (from CrowdfundingValidator)
```scala
def requireStrictlyAscending(indices: List[BigInt]): Unit = {
  indices.foldLeft(BigInt(-1)) { (prev, curr) =>
    require(prev < curr, "Indices must be strictly ascending (no duplicates)")
    curr
  }
}

def processMultipleDonations(indices: List[BigInt], /* ... */) = {
  requireStrictlyAscending(indices)  // Ensures uniqueness
  indices.foreach { idx =>
    processDonation(donations.at(idx))
  }
}
```

### Detection Patterns
Search for:
- `List[BigInt]` indices in redeemers
- Batch processing without uniqueness checks
- Multiple UTxO consumption without ascending order validation

---

## V008: Inexact Refund Amount

**Severity**: High

### Description
Using `>=` for refund validation allows manipulation - excess funds can be redirected.

### Vulnerable Pattern
```scala
def handleBid(currentHighestBid: BigInt, /* ... */) = {
  val refundOutput = txInfo.outputs.at(refundIdx)
  // BAD: Allows overpaying, enabling fund manipulation
  require(
    refundOutput.value.getLovelace >= currentHighestBid,
    "Refund too small"
  )
}
```

### Secure Pattern (from AuctionValidator)
```scala
def handleBid(currentHighestBid: BigInt, /* ... */) = {
  val refundOutput = txInfo.outputs.at(refundIdx)
  // GOOD: Exact refund prevents manipulation
  require(
    refundOutput.value.getLovelace === currentHighestBid,
    "Refund must be exact amount"
  )
}
```

---

## V009: Time Boundary Edge Cases

**Severity**: Medium

### Description
Incorrect time boundary semantics (inclusive vs exclusive) can cause transactions at exact deadlines to fail unexpectedly.

### Best Practice
```scala
// Use clear boundary methods
require(
  validRange.isEntirelyBefore(deadline),  // Clear: tx must complete before deadline
  "Transaction must be before deadline"
)

require(
  validRange.isEntirelyAfter(startTime),  // Clear: tx must start after startTime
  "Transaction must be after start time"
)
```

### Detection Patterns
Search for:
- Time comparisons with `<` vs `<=`
- Deadline checks without clear inclusive/exclusive semantics
- POSIXTime comparisons without using built-in methods

---

## V010: Missing Signature Validation

**Severity**: Medium

### Description
Actions that should require authorization may be missing signature checks.

### Secure Pattern
```scala
def handleCancel(seller: PubKeyHash, /* ... */) = {
  require(
    txInfo.isSignedBy(seller),
    "Seller must sign cancellation"
  )
}

def handleClaim(beneficiary: PubKeyHash, /* ... */) = {
  require(
    txInfo.isSignedBy(beneficiary),
    "Beneficiary must sign claim"
  )
}
```

### Detection Patterns
Search for:
- Action handlers without `isSignedBy` checks
- State transitions without authorization
- Withdrawal/cancel operations without signature validation

---

## V011: Datum Mutation Not Validated

**Severity**: Medium

### Description
State transitions should verify that immutable fields remain unchanged.

### Secure Pattern
```scala
def validateDatumTransition(oldDatum: CampaignDatum, newDatum: CampaignDatum) = {
  // Immutable fields must not change
  require(oldDatum.owner === newDatum.owner, "Owner cannot change")
  require(oldDatum.targetAmount === newDatum.targetAmount, "Target cannot change")
  require(oldDatum.deadline === newDatum.deadline, "Deadline cannot change")

  // Only mutable fields can differ (e.g., currentAmount)
}
```

### Detection Patterns
Search for:
- Datum updates without field comparison
- State machines without transition validation
- Contracts accepting any datum without checking consistency

---

## V005: Double Satisfaction

**Severity**: Critical

### Description
When multiple UTxOs are consumed in a single transaction, each validator sees the same transaction outputs. A single output can satisfy validation requirements for multiple inputs, allowing attackers to pay once for multiple claims.

### Attack Scenario
1. Contract A requires output to address X with value V
2. Contract B (separate UTxO) requires output to address X with value V
3. Attacker creates ONE output satisfying both requirements
4. Both validators pass, but only one payment is made

### Vulnerable Pattern
```scala
def spend(/* ... */) = {
  // Checks that some output pays to beneficiary
  val paymentExists = txInfo.outputs.exists { out =>
    out.address === beneficiaryAddress &&
    out.value.getLovelace >= requiredAmount
  }
  require(paymentExists, "Payment required")
}
```

### Secure Pattern
Use a unique identifier (e.g., own UTxO reference) to link inputs to specific outputs:
```scala
def spend(/* ... */, ownRef: TxOutRef) = {
  // Use output index from redeemer or derive from input
  val outputIdx = redeemer.to[BigInt]
  val output = txInfo.outputs.at(outputIdx)

  // Verify this output is specifically for this input (e.g., via datum)
  require(
    output.datum.to[PaymentDatum].sourceUtxo === ownRef,
    "Output must reference this specific UTxO"
  )
}
```

Alternatively, use NFT-based linking:
```scala
// Mint a unique token for this input, require it in the output
val uniqueToken = deriveTokenName(ownRef)
require(
  output.value.quantityOf(policyId, uniqueToken) === BigInt(1),
  "Output must contain unique linking token"
)
```

### Detection Patterns
Search for:
- `outputs.exists` or `outputs.find` without unique linking
- Multiple inputs expecting payment to same address
- Missing correlation between inputs and outputs

---

## V010: Other Redeemer Attack

**Severity**: High

### Description
When a script can be invoked multiple ways in the same transaction (e.g., as spend and mint), attackers may bypass validation by using a different redeemer on another invocation.

### Attack Scenario
1. Script has spend logic requiring signature
2. Same script has mint logic with different requirements
3. Attacker uses mint redeemer to bypass spend validation
4. Both executions pass but security is bypassed

### Vulnerable Pattern
```scala
// Spend assumes validation happened
def spend(datum: Data, redeemer: Data, tx: TxInfo, ownRef: TxOutRef) = {
  val action = redeemer.to[SpendAction]
  action match {
    case SpendAction.Claim => claimLogic(tx)
  }
}

// Mint has weaker requirements
def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo) = {
  // No signature check here!
  val action = redeemer.to[MintAction]
  action match {
    case MintAction.Create => createLogic(tx)
  }
}
```

### Secure Pattern
Ensure each entry point validates independently:
```scala
def spend(datum: Data, redeemer: Data, tx: TxInfo, ownRef: TxOutRef) = {
  // Always validate authorization in spend
  require(tx.isSignedBy(owner), "Owner signature required")
  // ... rest of logic
}

def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo) = {
  // Also validate authorization in mint
  require(tx.isSignedBy(owner), "Owner signature required")
  // ... rest of logic
}
```

---

## V011: Other Token Name Attack

**Severity**: High

### Description
Minting policies that only check for specific token names allow attackers to mint arbitrary additional tokens under the same policy ID.

### Attack Scenario
1. Policy validates minting of "CampaignNFT"
2. Attacker includes additional tokens: "FakeNFT", "AttackerToken"
3. Policy only checks "CampaignNFT", ignores others
4. Attacker now has valid tokens under legitimate policy ID

### Vulnerable Pattern
```scala
def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo) = {
  // Only checks the expected token
  require(
    tx.mint.quantityOf(policyId, expectedTokenName) === BigInt(1),
    "Must mint campaign token"
  )
  // Other tokens under same policyId are ignored!
}
```

### Secure Pattern
Validate ALL tokens minted under this policy:
```scala
def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo) = {
  // Get all tokens minted under this policy
  val mintedTokens = tx.mint.filter(_.policyId === policyId)

  // Ensure only expected tokens are minted
  require(mintedTokens.size === 1, "Only one token type allowed")
  require(
    mintedTokens.head.tokenName === expectedTokenName,
    "Unexpected token name"
  )
  require(
    mintedTokens.head.quantity === BigInt(1),
    "Must mint exactly one"
  )
}
```

---

## V012: Missing UTxO Authentication

**Severity**: High

### Description
Without proper authentication, anyone can create UTxOs at a script address with arbitrary datums, potentially corrupting contract state.

### Attack Scenario
1. Contract expects UTxOs to be created through proper minting
2. Attacker directly sends UTxO to script address with fake datum
3. Contract processes fake UTxO as legitimate
4. State corruption or theft occurs

### Secure Pattern
Use authenticating tokens:
```scala
// Only process UTxOs containing the auth token
def spend(datum: Data, redeemer: Data, tx: TxInfo, ownRef: TxOutRef) = {
  val ownInput = tx.inputs.find(_.outRef === ownRef).get.resolved

  // Verify this UTxO has the authentication token
  require(
    ownInput.value.quantityOf(authPolicyId, authTokenName) >= BigInt(1),
    "UTxO must contain authentication token"
  )

  // Now safe to process
}
```

---

## V013: Time Handling

**Severity**: Medium

### Description
Validators only see time intervals (validity ranges), not exact timestamps. Incorrect handling of interval bounds can enable time manipulation attacks.

### Vulnerable Pattern
```scala
def spend(/* ... */) = {
  // BAD: Only checks lower bound
  require(
    tx.validRange.from >= deadline,
    "Must be after deadline"
  )
}
```

### Secure Pattern
```scala
def spend(/* ... */) = {
  // GOOD: Entire validity range must be after deadline
  require(
    tx.validRange.isEntirelyAfter(deadline),
    "Transaction must be entirely after deadline"
  )

  // Or for before deadline:
  require(
    tx.validRange.isEntirelyBefore(deadline),
    "Transaction must be entirely before deadline"
  )
}
```

---

## V016: Insufficient Staking Control

**Severity**: Medium

### Description
Contracts that don't verify staking credentials allow attackers to redirect staking rewards.

### Attack Scenario
1. Contract locks funds without specifying staking credential
2. Attacker registers their staking key to the script address
3. All staking rewards go to attacker

### Secure Pattern
```scala
// Verify staking credential in output address
def spend(/* ... */) = {
  val continuingOutput = tx.outputs.at(outputIdx)

  // Check both payment and staking credentials
  require(
    continuingOutput.address.stakingCredential === expectedStakingCred,
    "Invalid staking credential"
  )
}
```

---

## V017: Arbitrary Datum

**Severity**: Medium

### Description
Insufficient datum validation when locking funds can cause UTxOs to become unspendable if the datum doesn't match expected format.

### Secure Pattern
```scala
// Validate datum structure before accepting
def mint(/* ... */) = {
  val output = tx.outputs.at(outputIdx)

  // Ensure datum is present and valid
  val datum = output.datum match {
    case OutputDatum.InlineDatum(d) => d.to[ExpectedDatum]
    case _ => fail("Inline datum required")
  }

  // Validate datum fields
  require(datum.owner.length === 28, "Invalid owner hash")
  require(datum.amount > BigInt(0), "Amount must be positive")
}
```

---

## V018: Unbounded Value

**Severity**: Low (Design Issue)

### Description
UTxOs with unlimited tokens can exceed size/execution limits, making funds unspendable.

### Mitigation
Set maximum token limits:
```scala
require(
  output.value.size <= maxTokenTypes,
  "Too many token types in output"
)
```

---

## V019: Unbounded Datum

**Severity**: Low (Design Issue)

### Description
Unbounded datum growth can exceed resource constraints, locking funds.

### Mitigation
Limit datum size:
```scala
require(
  datum.participants.length <= maxParticipants,
  "Too many participants"
)
```

---

## V020: Unbounded Inputs

**Severity**: Low (Design Issue)

### Description
Operations requiring many UTxOs may exceed transaction limits.

### Mitigation
Design for bounded input requirements; use batching patterns.

---

## V021: UTxO Contention

**Severity**: Low (Design Issue)

### Description
Shared global state (single UTxO) creates bottlenecks when multiple users access simultaneously.

### Mitigation
Use per-user UTxOs or batching patterns; avoid global state where possible.

---

## V022: Cheap Spam / Dust Attack

**Severity**: Low

### Description
Low-cost malicious transactions can obstruct legitimate operations.

### Mitigation
```scala
require(
  inputValue.getLovelace >= minDonationAmount,
  "Donation below minimum"
)
```

---

## V023: Locked Value

**Severity**: Low (Design Issue)

### Description
Design flaws can make funds permanently inaccessible.

### Mitigation
- Include emergency withdrawal mechanisms
- Verify all state transitions have valid exits
- Consider timeout-based fallbacks

---

## Scalus-Specific Notes

### Validator Trait Methods
All validators implement these methods (check each for vulnerabilities):
- `spend(datum, redeemer, tx, ownRef)` - Spending validator
- `mint(redeemer, policyId, tx)` - Minting policy
- `reward(redeemer, stakingKey, tx)` - Staking validator
- `certify(redeemer, cert, tx)` - Certificate validator
- `vote(redeemer, voter, tx)` - Voting validator
- `propose(procedure, tx)` - Proposal validator

### Common Scalus Patterns
- Use `Address.fromScriptHash(scriptHash)` for address construction
- Use `txInfo.outputs.at(idx)` for indexed access
- Use `value.quantityOf(policyId, tokenName)` for token amounts
- Use `txInfo.isSignedBy(pubKeyHash)` for signature checks
- Use `validRange.isEntirelyBefore/After` for time validation

### Finding Script Hash
```scala
// In spend validator, get own script hash from ownRef
val ownInput = txInfo.inputs.find(_.outRef === ownRef).get
val scriptHash = ownInput.resolved.address.credential match {
  case Credential.ScriptCredential(hash) => hash
  case _ => fail("Not a script")
}
```
