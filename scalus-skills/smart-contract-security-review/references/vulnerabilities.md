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
13. [V025: Oracle Data Validation](#v025-oracle-data-validation)

### Medium
14. [V013: Time Handling](#v013-time-handling)
15. [V014: Missing Signature Validation](#v014-missing-signature-validation)
16. [V015: Datum Mutation Not Validated](#v015-datum-mutation-not-validated)
17. [V016: Insufficient Staking Control](#v016-insufficient-staking-control)
18. [V017: Arbitrary Datum](#v017-arbitrary-datum)
19. [V024: Parameterization Verification](#v024-parameterization-verification)

### Low / Design Issues
20. [V018: Unbounded Value](#v018-unbounded-value)
21. [V019: Unbounded Datum](#v019-unbounded-datum)
22. [V020: Unbounded Inputs](#v020-unbounded-inputs)
23. [V021: UTxO Contention / EUTXO Concurrency DoS](#v021-utxo-contention--eutxo-concurrency-dos)
24. [V022: Cheap Spam / Dust Attack](#v022-cheap-spam--dust-attack)
25. [V023: Locked Value](#v023-locked-value)

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

### Verification: Attack Transaction Tracing (REQUIRED)

#### Step 1: Construct Attack Transaction

```
Attack scenario: Redirect continuing output to attacker's script

Inputs:
  - UTxO_A: 100 ADA at legitimate script, datum={owner=O, state=Active}

Outputs:
  - 100 ADA to ATTACKER's script address (not the original script!)

Redeemers:
  - UTxO_A → SomeAction(outputIdx=0)

Signatories: [attacker]
```

#### Step 2: Trace Validator Execution

```scala
// Trace the code that handles continuing output:

val continuingOutput = txInfo.outputs.at(outputIdx)  // → attacker's output

// Is there an address check? Trace to find out:
// LOOK FOR: require(continuingOutput.address === ???)

// If NO address check exists → VULNERABLE
// If address check uses script's own hash → SAFE
// If address uses datum field (user-specified) → Intentional, SAFE
```

#### Step 3: Key Questions

| Question | If Yes |
|----------|--------|
| Does code check `output.address === Address.fromScriptHash(ownHash)`? | SAFE |
| Does code use `findOwnOutputsByCredential(scriptCredential)`? | SAFE (filters by address) |
| Does output go to datum-specified address (e.g., `datum.beneficiary`)? | Intentional design, SAFE |
| Is this a close/burn action with no continuing output? | V001 N/A |
| Can attacker control the address through redeemer? | VULNERABLE |

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

### Verification: Attack Transaction Tracing (REQUIRED)

#### Step 1: Construct Attack Transaction

```
Attack scenario: Remove NFT from continuing output

Inputs:
  - UTxO_A: 50 ADA + NFT(policyId, "state") at script, datum={state=Active}

Outputs:
  - 50 ADA to script (WITHOUT the NFT!)
  - NFT to attacker's wallet

Redeemers:
  - UTxO_A → UpdateState
```

#### Step 2: Trace Validator Execution

```scala
// Trace the continuing output validation:

val continuingOutput = txInfo.outputs.at(outputIdx)

// Is there a token check? Look for:
// require(continuingOutput.value.quantityOf(policyId, tokenName) === 1)

// If NO token check → VULNERABLE (attacker keeps NFT)
// If token check exists → SAFE
```

#### Step 3: Key Questions

| Question | If Yes |
|----------|--------|
| Does code check `output.value.quantityOf(policyId, tokenName) === 1`? | SAFE |
| Is this a close/burn action that burns the token? | V002 N/A |
| Does contract use address-based ID instead of tokens? | Different design, not V002 |
| Is token verification in a helper function that gets called? | SAFE |

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

### False Positive Indicators
- `>=` is intentional for "mint at least N" scenarios (check if this is the design)
- Exact check done elsewhere in the same transaction flow
- The flexibility is constrained by other validation (e.g., must match datum count)
- Burning partial amount is intentional for incremental withdrawal patterns

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

### False Positive Indicators
- Index bounds check in a helper function (e.g., `requireFound`, `requireValidIndex`)
- Index is guaranteed valid by construction (e.g., always 0 for single-element list)
- Index comes from validated redeemer structure that enforces bounds
- The `at()` call is on `txInfo.inputs` or `txInfo.outputs` where index is from script's own UTxO lookup

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

### False Positive Indicators
- Authorization done via NFT/token ownership instead of signature
- The action is public by design (e.g., anyone can trigger liquidation if conditions met)
- Signature check exists in a helper function or parent method
- Authorization comes from spending a specific UTxO (implicit signature via spending)
- Multi-sig or DAO-based authorization via separate validator

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

### Verification: Attack Transaction Tracing (REQUIRED)

**Do NOT report based on pattern matching. You MUST construct and trace an attack transaction.**

#### Step 1: Construct Attack Transaction

```
Attack scenario: Pay once, satisfy two escrows

Inputs:
  - UTxO_A: 12 ADA at script, datum={seller=S, amount=10, init=2}
  - UTxO_B: 12 ADA at script, datum={seller=S, amount=10, init=2}

Outputs:
  - 12 ADA to seller S  (attacker pays ONCE instead of twice)
  - 1 ADA to buyer B

Redeemers:
  - UTxO_A → Pay
  - UTxO_B → Pay

Signatories: [B]
```

#### Step 2: Trace Validator Execution

Execute the validator code line-by-line with your attack transaction.
Track what each variable evaluates to. Check if each `require()` passes.

```scala
// For UTxO_A, trace through handlePay:

// What does contractBalance evaluate to?
val contractInputs = txInfo.findOwnInputsByCredential(credential)
// → If this returns [UTxO_A, UTxO_B], then:
val contractBalance = getAdaFromInputs(contractInputs)  // → 24 ADA

// Does this require pass?
require(contractBalance === escrowAmount + initAmount)
// → require(24 === 12) → FAILS ❌

// Attack blocked! This is a FALSE POSITIVE.
```

#### Step 3: Write Test if Uncertain

```scala
test("V005: Double satisfaction attack") {
  val utxoA = escrowUtxo(seller = S, amount = 10.ada)
  val utxoB = escrowUtxo(seller = S, amount = 10.ada)

  val attackTx = tx(
    inputs = List(utxoA, utxoB),
    outputs = List(output(S, 12.ada)),  // Pay once!
    redeemers = Map(utxoA -> Pay, utxoB -> Pay)
  )

  // Does this pass or fail?
  evaluate(EscrowValidator, utxoA, attackTx) shouldBe ???
}
```

### Example Trace: EscrowValidator (FALSE POSITIVE)

**Suspicious code**:
```scala
val sellerOutputs = txInfo.findOwnOutputsByCredential(PubKeyCredential(seller))
require(Utils.getAdaFromOutputs(sellerOutputs) === escrowAmount + initAmount)
```

**Attack transaction**: Spend UTxO_A and UTxO_B (both 12 ADA), create single 12 ADA output to seller.

**Trace execution**:
```
Line 57: contractInputs = findOwnInputsByCredential(scriptCredential)
         → [UTxO_A, UTxO_B]

Line 58: contractBalance = getAdaFromInputs(contractInputs)
         → 12 + 12 = 24 ADA

Line 118-120: require(contractBalance === 10 + 2)
              → require(24 === 12)
              → FAILS ❌
```

**Conclusion**: Attack fails at line 118. The `contractBalance` check implicitly prevents spending multiple UTxOs. **FALSE POSITIVE**.

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

### False Positive Indicators
- Policy uses `mint.filter(_.policyId === policyId)` and validates all returned tokens
- Token name is derived deterministically from transaction data (e.g., hash of input ref)
- Separate redeemer cases each validate their tokens AND check total mint count
- Policy explicitly validates mint map size for this policy ID

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

## V021: UTxO Contention / EUTXO Concurrency DoS

**Severity**: Low-Medium (Design Issue)

### Description
Shared global state (single UTxO) creates bottlenecks when multiple users access simultaneously. In the worst case, attackers can intentionally block protocol operations by repeatedly spending critical UTxOs with trivial transactions.

### Attack Scenario (EUTXO Concurrency DoS)
1. Protocol has a "global" UTxO that must be consumed for key operations
2. Attacker monitors mempool for legitimate transactions targeting this UTxO
3. Attacker submits competing transaction with higher fee, spending the same UTxO
4. Attacker's transaction wins, legitimate transaction fails
5. Attacker repeats, effectively blocking the protocol

### Vulnerable Pattern
```scala
// Single global state UTxO - easy target for DoS
@Compile
object GlobalStateValidator extends Validator {
  def spend(datum: Data, redeemer: Data, ctx: TxInfo) = {
    // All protocol operations go through this single UTxO
    val globalState = datum.to[GlobalState]
    // ... update state ...
  }
}
```

### Mitigation Strategies

**1. Per-user UTxOs**
```scala
// Each user has their own UTxO - no contention
case class UserState(owner: PubKeyHash, balance: BigInt)

def spend(datum: Data, redeemer: Data, ctx: TxInfo, ownRef: TxOutRef) = {
  val state = datum.to[UserState]
  require(ctx.isSignedBy(state.owner), "Owner must sign")
  // User only affects their own UTxO
}
```

**2. Batching with multiple UTxOs**
```scala
// Multiple identical state UTxOs - reduces contention
// Off-chain: round-robin or random selection of which UTxO to use
```

**3. Time-locked operations**
```scala
// Require minimum time between state changes
case class GlobalState(lastUpdate: POSIXTime, data: Data)

def spend(datum: Data, redeemer: Data, ctx: TxInfo) = {
  val state = datum.to[GlobalState]
  val minInterval = POSIXTime(60000)  // 1 minute

  require(
    ctx.validRange.from > state.lastUpdate + minInterval,
    "Must wait between updates"
  )
}
```

**4. Stake-based access**
```scala
// Require stake to interact, making DoS expensive
require(
  ctx.inputs.exists(i => i.value.getLovelace >= minStake),
  "Minimum stake required"
)
```

### Assessment Questions

| # | Question | Risk Level |
|---|----------|------------|
| 1 | Does protocol have single "global" UTxO for critical operations? | If Yes → Medium |
| 2 | Can anyone submit transactions affecting this UTxO? | If Yes → Higher risk |
| 3 | Is there rate limiting or stake requirement? | If No → Higher risk |
| 4 | Can protocol function if UTxO is temporarily blocked? | If No → Higher risk |

### Detection Patterns
Search for:
- Single validator handling all protocol state
- No per-user state separation
- Missing rate limiting or stake requirements
- Critical operations dependent on single UTxO availability

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

## V024: Parameterization Verification

**Severity**: Varies (assess per case)

### Description
Parameterized validators (using `ParameterizedValidator` or `DataParameterizedValidator`) include parameters that affect the script hash. On-chain code cannot cryptographically verify that parameters were correctly applied. Depending on what parameters control and what verification mechanisms exist, this may or may not be a security concern.

### Assessment Process

Answer these questions to determine severity:

| # | Question | Impact |
|---|----------|--------|
| 1 | Do parameters control authorization/ownership (owner pubkey, admin key)? | If No → likely safe |
| 2 | Is there an authentication token (NFT) verifying legitimate instances? | If Yes → mitigated |
| 3 | Is the script hash published/verifiable through trusted channels? | If Yes → mitigated |
| 4 | Can users independently verify correct parameters before interacting? | If Yes → mitigated |

### Severity Matrix

| Scenario | Severity |
|----------|----------|
| Parameters are only configuration (fees, thresholds, non-auth data) | Informational |
| Auth params + authentication token present | Low |
| Auth params + script hash verified off-chain (registry, verified UI) | Low |
| Auth params + no token + no verification mechanism | Medium-High |

### Attack Scenario (when vulnerable)
1. Protocol uses parameterized validator with `owner: PubKeyHash` parameter
2. Attacker deploys same validator with their own `owner` parameter
3. Both scripts have different hashes but identical interface
4. Users interact with attacker's version thinking it's legitimate
5. Attacker controls the "owner" operations

### Example: Low Risk (configuration only)
```scala
// Parameters don't control authorization - just configuration
case class FeeConfig(feePercent: BigInt, minFee: BigInt)

@Compile
object SwapValidator extends ParameterizedValidator[FeeConfig] {
  def spend(config: FeeConfig)(datum: Data, redeemer: Data, ctx: TxInfo) = {
    // Fee config doesn't grant special privileges
    val fee = calculateFee(amount, config.feePercent, config.minFee)
    // ... validation logic
  }
}
```

### Example: Mitigated with Auth Token
```scala
case class ProtocolParams(owner: PubKeyHash, authPolicyId: PolicyId)

@Compile
object ProtocolValidator extends ParameterizedValidator[ProtocolParams] {
  def spend(params: ProtocolParams)(datum: Data, redeemer: Data, ctx: TxInfo, ownRef: TxOutRef) = {
    val ownInput = ctx.inputs.find(_.outRef === ownRef).get.resolved

    // Auth token verifies this is a legitimate instance
    require(
      ownInput.value.quantityOf(params.authPolicyId, authTokenName) === BigInt(1),
      "Authentication NFT required"
    )
    // Now safe to use params.owner
  }
}
```

### Example: Vulnerable (auth params, no verification)
```scala
case class VulnerableParams(owner: PubKeyHash, treasury: Address)

@Compile
object VulnerableValidator extends ParameterizedValidator[VulnerableParams] {
  def spend(params: VulnerableParams)(datum: Data, redeemer: Data, ctx: TxInfo) = {
    // RISKY: owner controls withdrawal but nothing verifies correct params
    require(ctx.isSignedBy(params.owner), "Owner must sign")
    // Attacker can deploy with their own owner and trick users
  }
}
```

### Secure Alternatives

**Option 1: Add authentication token**
Mint an NFT when creating the protocol instance, require it in all operations.

**Option 2: Use datum for mutable auth data**
```scala
case class ValidatorDatum(owner: PubKeyHash, config: Config)

@Compile
object SafeValidator extends Validator {
  def spend(datum: Data, redeemer: Data, ctx: TxInfo) = {
    val d = datum.to[ValidatorDatum]
    // Owner is visible in datum, users can verify before interacting
  }
}
```

**Option 3: Publish script hash registry**
Maintain on-chain or verified off-chain registry of legitimate script hashes.

### Detection Patterns
Search for:
- Classes extending `ParameterizedValidator` or `DataParameterizedValidator`
- Parameters containing `PubKeyHash`, `Address`, or authorization-related fields
- Missing authentication tokens for parameterized validators with auth params

---

## V025: Oracle Data Validation

**Severity**: High (when applicable)

### Description
Contracts relying on external oracle data (prices, exchange rates, external state) must properly validate authenticity and freshness. Without proper validation, attackers can provide manipulated or stale data.

### Applicability
This vulnerability applies to contracts that:
- Use external price feeds for liquidations, swaps, or collateral calculations
- Rely on off-chain data signed by oracles
- Make decisions based on external market conditions

### Attack Vectors

**1. Missing Signature Verification**
Oracle data accepted without verifying it was signed by trusted oracle.

**2. Stale Data Attack**
Using outdated oracle data that no longer reflects current conditions.

**3. Price Manipulation**
Attacker manipulates oracle's data source (e.g., low-liquidity DEX) to report incorrect prices.

**4. Single Oracle Dependency**
Relying on one oracle creates single point of failure.

### Vulnerable Pattern
```scala
def spend(datum: Data, redeemer: Data, ctx: TxInfo) = {
  val oracleData = redeemer.to[OracleData]

  // BAD: No signature verification
  // BAD: No timestamp/freshness check
  val price = oracleData.price

  // Using unverified price for liquidation
  if (collateralValue / price < liquidationThreshold) {
    // Allow liquidation - attacker can trigger with fake low price
  }
}
```

### Secure Pattern
```scala
case class OracleData(
  price: BigInt,
  timestamp: POSIXTime,
  signature: ByteString
)

def spend(datum: Data, redeemer: Data, ctx: TxInfo) = {
  val oracleData = redeemer.to[OracleData]
  val maxOracleAge = POSIXTime(300000)  // 5 minutes

  // 1. Verify oracle signature
  val message = serializeForSigning(oracleData.price, oracleData.timestamp)
  require(
    verifyEd25519Signature(trustedOraclePubKey, message, oracleData.signature),
    "Invalid oracle signature"
  )

  // 2. Check data freshness - oracle timestamp must be recent
  require(
    ctx.validRange.from - oracleData.timestamp < maxOracleAge,
    "Oracle data too stale"
  )

  // 3. Sanity bounds on values
  require(
    oracleData.price > minReasonablePrice && oracleData.price < maxReasonablePrice,
    "Price outside reasonable bounds"
  )

  // Now safe to use price
}
```

### Multi-Oracle Pattern (for high-value protocols)
```scala
case class MultiOracleData(
  oracleReadings: List[(PubKeyHash, BigInt, ByteString)],  // (oracle, price, sig)
  timestamp: POSIXTime
)

def validateMultiOracle(
  data: MultiOracleData,
  trustedOracles: List[PubKeyHash],
  minOracles: BigInt
): BigInt = {
  val validPrices = data.oracleReadings.filter { case (oracle, price, sig) =>
    trustedOracles.contains(oracle) &&
    verifySignature(oracle, (price, data.timestamp), sig)
  }.map(_._2)

  require(
    BigInt(validPrices.length) >= minOracles,
    "Insufficient valid oracle signatures"
  )

  // Use median to resist manipulation
  median(validPrices)
}
```

### Assessment Questions

| # | Question | Risk Level |
|---|----------|------------|
| 1 | Does contract use external price/data feeds? | If No → N/A |
| 2 | Is oracle signature verified on-chain? | If No → High |
| 3 | Is data freshness checked? | If No → Medium |
| 4 | Are there sanity bounds on values? | If No → Medium |
| 5 | Is there multi-oracle redundancy? | If No → consider for high-value |

### Detection Patterns
Search for:
- Redeemer fields containing `price`, `rate`, `oracle` without signature verification
- External data used without `verifyEd25519Signature` or similar
- Price-based logic (liquidation, swap rates) without freshness checks
- Single trusted pubkey for critical price data

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

---

## Off-Chain Vulnerabilities

These vulnerabilities exist in transaction builder code, not the on-chain validators. Review these when off-chain code (transaction builders, endpoint classes) exists alongside smart contracts.

### OC001: UTXO Discovery Confusion / Token Name Collision

**Severity**: Medium-High

**Description**: When off-chain code queries UTXOs using shared identifiers (policyId + tokenName), and multiple UTXOs can share the same identifier, the query may return the wrong UTXO.

**Attack Scenario**:
1. Alice starts auction with itemId="rare-painting"
2. Attacker also starts auction with itemId="rare-painting"
3. Buyer queries findActiveUtxo("rare-painting")
4. Query returns attacker's UTXO (non-deterministic ordering)
5. Buyer interacts with attacker's fake auction

**Vulnerable Pattern**:
```scala
def findActiveUtxo(itemId: ByteString): Future[Option[Utxo]] =
    provider.queryUtxos { u =>
        u.output.address == scriptAddress &&
        u.output.value.hasAsset(policyId, AssetName(itemId))
    }
    .limit(1)  // Returns arbitrary one if multiple exist!
    .execute()
```

**Secure Patterns**:
1. **Verify datum fields**: Check seller/owner in datum matches expected
2. **Use unique identifiers**: itemId = hash(seller ++ item ++ utxo_ref)
3. **Return all matches**: Let caller verify the correct one
4. **Include seller in query**: Filter by expected seller address or pubkeyhash

**Example secure pattern**:
```scala
def findActiveUtxo(itemId: ByteString, expectedSeller: PubKeyHash): Future[Option[Utxo]] =
    provider.queryUtxos { u =>
        u.output.address == scriptAddress &&
        u.output.value.hasAsset(policyId, AssetName(itemId))
    }
    .execute()
    .map { utxos =>
        utxos.find { utxo =>
            val datum = utxo.inlineDatum.map(_.to[AuctionDatum])
            datum.exists(_.seller == expectedSeller)
        }
    }
```

**Detection**: Search for UTXO queries with `.limit(1)` or that return arbitrary matches without verifying datum ownership fields.

---

### OC002: TOCTOU Race Condition

**Severity**: Medium

**Description**: Time-of-check to time-of-use. The UTXO state when queried may differ from state when transaction is submitted.

**Attack Scenario**:
1. Off-chain code queries UTXO and builds transaction
2. Between query and submit, attacker submits competing transaction
3. Original transaction fails or interacts with unexpected state

**Vulnerable Pattern**:
```scala
def placeBid(itemId: ByteString, bidAmount: BigInt): Future[Transaction] = {
    for {
        utxo <- findActiveUtxo(itemId)  // State at time T1
        tx <- buildBidTransaction(utxo, bidAmount)
        result <- submitTransaction(tx)  // State may have changed by T2
    } yield result
    // No error handling - fails silently or with cryptic error
}
```

**Secure Pattern**:
```scala
def placeBid(itemId: ByteString, bidAmount: BigInt): Future[Transaction] = {
    def attempt(retries: Int): Future[Transaction] = {
        for {
            utxo <- findActiveUtxo(itemId)
            tx <- buildBidTransaction(utxo, bidAmount)
            result <- submitTransaction(tx).recoverWith {
                case e: UtxoConsumedError if retries > 0 =>
                    // UTXO was spent, retry with fresh state
                    attempt(retries - 1)
            }
        } yield result
    }
    attempt(maxRetries = 3)
}
```

**Mitigation**:
- Handle transaction failures gracefully with retry logic
- Use reference inputs where applicable (read-only, no consumption)
- Implement optimistic concurrency with exponential backoff
- Log and surface UTXO contention to users

**Detection**: Search for transaction building code without error handling or retry logic around submission.

---

### OC003: Missing Datum Validation in Queries

**Severity**: Medium

**Description**: Off-chain code trusts UTXO data without validating datum structure or ownership fields. Attackers can create fake UTXOs with malicious datums.

**Attack Scenario**:
1. Contract allows anyone to create UTXOs at script address
2. Attacker creates UTXO with malicious datum (e.g., attacker as beneficiary)
3. Off-chain code queries and uses this UTXO without validation
4. Legitimate user's transaction interacts with attacker's UTXO

**Vulnerable Pattern**:
```scala
def findAuction(itemId: ByteString): Future[AuctionDatum] = {
    for {
        utxo <- findActiveUtxo(itemId)
        // BAD: Assumes datum is valid, doesn't verify ownership
        datum = utxo.inlineDatum.get.to[AuctionDatum]
    } yield datum
}
```

**Secure Pattern**:
```scala
def findAuction(itemId: ByteString, expectedSeller: PubKeyHash): Future[AuctionDatum] = {
    for {
        utxo <- findActiveUtxo(itemId)
        datum <- utxo.inlineDatum match {
            case Some(d) => Future.successful(d.to[AuctionDatum])
            case None => Future.failed(new IllegalStateException("Missing inline datum"))
        }
        // Validate datum fields match expectations
        _ <- if (datum.seller == expectedSeller) Future.unit
             else Future.failed(new IllegalStateException(s"Wrong seller: expected $expectedSeller, got ${datum.seller}"))
    } yield datum
}
```

**Detection Patterns**:
- UTXO queries without subsequent datum field validation
- Datum deserialization without ownership/authorization checks
- Methods that return datum directly without validation

---

### Off-Chain Detection Commands

```bash
# Find off-chain transaction builder code
grep -rn "queryUtxos\|TxBuilder\|Provider" --include="*.scala" $PATH

# Find methods returning Future[Transaction]
grep -rn "Future\[Transaction\]" --include="*.scala" $PATH

# Find endpoint/action methods (common naming patterns)
grep -rn "def.*Auction\|def.*bid\|def.*end\|def.*start\|def.*claim" --include="*.scala" $PATH

# Find .limit(1) patterns in queries
grep -rn "\.limit(1)" --include="*.scala" $PATH

# Find queryUtxos without datum validation nearby
grep -rn -A 10 "queryUtxos" --include="*.scala" $PATH | grep -v "inlineDatum\|datum"
```
