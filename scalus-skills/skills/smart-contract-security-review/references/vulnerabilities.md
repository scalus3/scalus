# Cardano Smart Contract Vulnerability Patterns

Detailed patterns for Scalus/Cardano smart contract security review with code examples.
Based on Cardano Developer Portal security guidelines and Scalus-specific patterns.

## Table of Contents

Every entry carries a taxonomy ID in parentheses (family-number, e.g. `VP-2`: DS double
satisfaction, VP value preservation, AU authentication, MI minting, TI time, DT datum, IX
index/ordering, PU script purpose, EV evaluation, AR arithmetic, RS resources, DE design) and a
**Fixed by** line naming the Scalus operation or idiom that closes it.

### Critical
1. [V001: Redirect Attack (VP-3)](#v001-redirect-attack-vp-3)
2. [V002: Token/NFT Not Verified (AU-1)](#v002-tokennft-not-verified-au-1)
3. [V003: Inexact Burn/Mint Validation (MI-1 / MI-3)](#v003-inexact-burnmint-validation-mi-1--mi-3)
4. [V004: Rounding Direction (AR-1)](#v004-rounding-direction-ar-1)
5. [V005: Double Satisfaction (DS-1 / DS-2)](#v005-double-satisfaction-ds-1--ds-2)
6. [V026: Value Not Preserved on Continuing Output (VP-1)](#v026-value-not-preserved-on-continuing-output-vp-1)
7. [V027: ADA-Only Value Comparison (VP-2)](#v027-ada-only-value-comparison-vp-2)
8. [V028: One-Shot Seed Not Bound (MI-2)](#v028-one-shot-seed-not-bound-mi-2)
9. [V029: Missed Input (IX-2)](#v029-missed-input-ix-2)

### High
10. [V006: Index Validation Missing (IX-1)](#v006-index-validation-missing-ix-1)
11. [V007: Self-Dealing / Shill Bidding (DE-3)](#v007-self-dealing--shill-bidding-de-3)
12. [V008: Double Spend via Index Reuse (IX-1)](#v008-double-spend-via-index-reuse-ix-1)
13. [V009: Inexact Refund Amount (VP-4)](#v009-inexact-refund-amount-vp-4)
14. [V010: Other Redeemer Attack (PU-1)](#v010-other-redeemer-attack-pu-1)
15. [V011: Other Token Name Attack (MI-1)](#v011-other-token-name-attack-mi-1)
16. [V012: Missing UTxO Authentication (AU-1 / AU-5)](#v012-missing-utxo-authentication-au-1--au-5)
17. [V025: Oracle Data Validation (DE-2)](#v025-oracle-data-validation-de-2)
18. [V030: Evaluation-Order Trap (EV-1)](#v030-evaluation-order-trap-ev-1)
19. [V031: Signature Domain Separation (AU-7)](#v031-signature-domain-separation-au-7)
20. [V032: Certificate Purposes Unguarded (PU-3)](#v032-certificate-purposes-unguarded-pu-3)

### Medium
21. [V013: Time Handling (TI-1 / TI-2)](#v013-time-handling-ti-1--ti-2)
22. [V014: Missing Signature Validation (AU-2)](#v014-missing-signature-validation-au-2)
23. [V015: Datum Mutation Not Validated (DT-1)](#v015-datum-mutation-not-validated-dt-1)
24. [V016: Insufficient Staking Control (AU-4)](#v016-insufficient-staking-control-au-4)
25. [V017: Arbitrary Datum (DT-3)](#v017-arbitrary-datum-dt-3)
26. [V024: Parameterization Verification (AU-6)](#v024-parameterization-verification-au-6)
27. [V033: Voting / Proposing Purposes Unguarded (PU-4)](#v033-voting--proposing-purposes-unguarded-pu-4)
28. [V034: Value-Map Normalisation (VP-5)](#v034-value-map-normalisation-vp-5)
29. [V035: Min-ADA Griefing on Forced Outputs (VP-6)](#v035-min-ada-griefing-on-forced-outputs-vp-6)
30. [V036: Hash Grinding (DE-4)](#v036-hash-grinding-de-4)

### Low / Design Issues
31. [V018: Unbounded Value (RS-1)](#v018-unbounded-value-rs-1)
32. [V019: Unbounded Datum (RS-2)](#v019-unbounded-datum-rs-2)
33. [V020: Unbounded Inputs (RS-3)](#v020-unbounded-inputs-rs-3)
34. [V021: UTxO Contention / EUTXO Concurrency DoS (RS-5)](#v021-utxo-contention--eutxo-concurrency-dos-rs-5)
35. [V022: Cheap Spam / Dust Attack (RS-6)](#v022-cheap-spam--dust-attack-rs-6)
36. [V023: Locked Value (DE-1)](#v023-locked-value-de-1)
37. [V037: Reference-Script Size (RS-7)](#v037-reference-script-size-rs-7)

---

## V001: Redirect Attack (VP-3)

**Severity**: Critical
**Fixed by**: `tx.findContinuingOutputOrFail(ownInput, msg)` (exactly one output to the whole own address)

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
  require(continuingOutput.hasInlineDatum(expectedDatum), "Invalid datum")
}
```

### Secure Pattern
```scala
def handleBid(ownInput: TxInInfo, /* ... */) = {
  // Exactly one output to the WHOLE own address (payment AND staking credential).
  // Fails when there is none or more than one.
  val continuingOutput =
    txInfo.findContinuingOutputOrFail(ownInput, "Expected exactly one continuing output")

  // Tokens exact, ADA at least: the state NFT cannot leave, min-ADA top-ups are allowed.
  require(
    continuingOutput.value.hasSameTokensAndAtLeastAda(expectedValue),
    "Continuing output value"
  )
  require(continuingOutput.hasInlineDatum(expectedDatum), "Invalid datum")
}
```

Not a fix (see V016 / AU-4):
- `out.address === Address.fromScriptHash(ownHash)`: the constructed address has no staking
  credential, so it never equals a real output address that carries one, and it accepts a
  franken address if the script address itself has none.
- `findOutputsByScriptHash(h)` / `findOutputsByCredential(c)` for the continuing output: they
  match the payment credential only; the attacker keeps the payment part and swaps the staking part.

### Detection Patterns
Search for:
- `outputs.at(` for a continuing output without a whole-address check
- `address.credential ===` or `Address.fromScriptHash` on a continuing output
- `findOutputsByScriptHash` / `findOutputsByCredential` used to locate the continuing output

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
// LOOK FOR: findContinuingOutputOrFail(ownInput, ...) or
//           require(continuingOutput.address === ownInput.resolved.address)

// If NO address check exists → VULNERABLE
// If the check compares the WHOLE address of the own input → SAFE
// If the check compares only the payment credential → V016 (AU-4), not SAFE
// If address uses datum field (user-specified) → Intentional, SAFE
```

#### Step 3: Key Questions

| Question | If Yes |
|----------|--------|
| Does code obtain the continuing output with `findContinuingOutputOrFail(ownInput, msg)`? | SAFE |
| Does code check `output.address === ownInput.resolved.address` (whole address)? | SAFE |
| Does code check `output.address === Address.fromScriptHash(ownHash)`? | NOT SAFE: no staking credential in the constructed address (AU-4, V016) |
| Does code use `findOutputsByScriptHash` / `findOutputsByCredential` or `address.credential ===`? | NOT SAFE for the continuing output: payment credential only; staking part can be swapped (AU-4). Acceptable only with a following `require(out.address === ownInput.resolved.address)` |
| Does output go to datum-specified address (e.g., `datum.beneficiary`)? | Intentional design, SAFE |
| Is this a close/burn action with no continuing output? | V001 N/A |
| Can attacker control the address through redeemer? | VULNERABLE |

---

## V002: Token/NFT Not Verified (AU-1)

**Severity**: Critical
**Fixed by**: `out.value.hasNft(policy, name)` on the continuing output (`hasOnly(policy, name, 1)` when nothing else may ride along under the policy)

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

### Secure Pattern (from Auction)
```scala
def handleWithdraw(ownInput: TxInInfo, /* ... */) = {
  val continuingOutput =
    txInfo.findContinuingOutputOrFail(ownInput, "Expected exactly one continuing output")
  // Exactly one unit of the state NFT; `> 0` is a different predicate for a non-NFT asset.
  require(
    continuingOutput.value.hasNft(campaignPolicyId, campaignTokenName),
    "Campaign NFT must be present in output"
  )
  // ... rest of validation
}
```

### Detection Patterns
Search for:
- Contracts using `policyId` or `tokenName` without `hasNft` / `hasOnly` / `quantityOf` checks
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
// require(continuingOutput.value.hasNft(policyId, tokenName))

// If NO token check → VULNERABLE (attacker keeps NFT)
// If token check exists → SAFE
```

#### Step 3: Key Questions

| Question | If Yes |
|----------|--------|
| Does code check `output.value.hasNft(policyId, tokenName)` (or `quantityOf(...) === BigInt(1)`)? | SAFE |
| Is this a close/burn action that burns the token? | V002 N/A |
| Does contract use address-based ID instead of tokens? | Different design, not V002 |
| Is token verification in a helper function that gets called? | SAFE |

---

## V003: Inexact Burn/Mint Validation (MI-1 / MI-3)

**Severity**: Critical
**Fixed by**: `tx.mint.hasOnly(policy, name, signedQty)` (whole policy sub-map, quantity signed); `tx.onlyBurnsUnder(policy)` for "burn only, mint nothing"

### Description
Using `>=` instead of `===` for token mint/burn quantities allows attackers to mint extra tokens or bypass burn validation. The sharper form of the same class: checking one `quantityOf` under the policy says nothing about the other token names minted under it (V011, MI-1). `hasOnly` compares the whole sub-map of the policy in one `equalsData`.

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

### Secure Pattern (from Auction)
```scala
def mint(/* ... */) = {
  // GOOD: exactly one token under this policy, and nothing else under it.
  require(
    txInfo.mint.hasOnly(policyId, tokenName, 1),
    "Must mint exactly one token"
  )
}

def burn(/* ... */) = {
  // GOOD: the quantity is signed; -tokenCount pins an exact burn.
  require(
    txInfo.mint.hasOnly(policyId, donationTokenName, -tokenCount),
    "Must burn exactly the specified token count"
  )
}

def close(/* ... */) = {
  // GOOD when the name is not known: at least one entry under the policy and every quantity
  // negative. `tokens(policyId).forall(_._2 < 0)` alone is vacuously true on an empty map and
  // passes a transaction that burns nothing (the NFT survives for replay).
  require(txInfo.onlyBurnsUnder(policyId), "Only burning is allowed")
}
```

### Detection Patterns
Search for:
- `mint.quantityOf` with `>=` or `<=` instead of `===`
- `mint.quantityOf(...) === ...` without a check on the rest of the policy sub-map (`hasOnly` covers both)
- `tokens(policy).forall(_._2 < 0)` without a `nonEmpty` guard (vacuous truth)
- Flexible burn validation allowing partial burns
- Token minting without upper bound

### False Positive Indicators
- `>=` is intentional for "mint at least N" scenarios (check if this is the design)
- Exact check done elsewhere in the same transaction flow
- The flexibility is constrained by other validation (e.g., must match datum count)
- Burning partial amount is intentional for incremental withdrawal patterns

---

## V004: Rounding Direction (AR-1)

**Severity**: Critical (scales with how finely the attacker can split one action into many)
**Fixed by**: `a divCeil b` / `a divFloor b` (infix on `BigInt`); `require(amount > 0)` at the decoding boundary (AR-2)

### Description
On-chain `Integer` is unbounded: there is no integer overflow in Plutus arithmetic, and this
entry was previously mis-framed as one. The real arithmetic class is **rounding direction**. Every
fee or share computed as `amount * rate / denominator` leaves a remainder, and the direction of
rounding decides who keeps it. An attacker who can split one large action into many small ones
harvests one rounding unit per action.

Two refinements belong to the same family:
- **AR-2, sign**: datum and redeemer amounts are attacker-supplied. A `BigInt` the code assumes
  positive can be negative and inverts every comparison built on it (`balance - amount >= 0` passes
  when `amount` is negative). Refine at the boundary: `require(amount > 0, msg)`.
- **AR-3, ledger bound**: the ledger stores an output's token quantity as a bounded integer
  (int64) although the script's arithmetic is unbounded. A check can pass on-chain for an amount no
  valid transaction can carry, which is a liveness failure, not theft.

### Direction rule
Round **against** the party that benefits from the remainder:
- Fees and payouts **owed by the contract** round **down** (`divFloor`).
- Amounts **owed to the contract** (fees it collects, installments it withholds) round **up** (`divCeil`).

### Vulnerable Pattern
```scala
// `/` on BigInt lowers to divideInteger, which floors. For a fee the protocol collects, flooring
// hands the remainder to the user on every swap; 1 000 swaps of 1 000 lovelace each pay zero fee.
val fee = amountIn * feeNumerator / feeDenominator
```

### Secure Pattern (from LinearVestingValidator)
```scala
// The quantity the contract KEEPS rounds up at every step, so the beneficiary can never withdraw
// a rounding unit early:
val timeBetween = (d.vestingPeriodEnd - d.vestingPeriodStart) divCeil d.totalInstallments
val futureInstallments = (d.vestingPeriodEnd - currentTime) divCeil timeBetween
val expectedRemaining = (futureInstallments * d.totalVestingQty) divCeil d.totalInstallments
```
```scala
// The amount the contract PAYS OUT rounds down (from VestingValidator):
vestingDatum.initialAmount * elapsed divFloor vestingDatum.duration
```
Alphanumeric infix operators have the lowest precedence: `a divCeil n * fee` parses as
`a divCeil (n * fee)`. Parenthesize operand expressions.

Sign refinement at the boundary (from VestingValidator):
```scala
val Action(requestedAmount) = redeemer.to[Action]
require(requestedAmount > 0, NonPositiveAmount)
```

### Detection Patterns
Search for:
- `/` on protocol quantities (fees, shares, installments) without a stated rounding direction
- `divFloor` on an amount the contract collects, `divCeil` on an amount it pays out
- Redeemer or datum `BigInt` amounts used in comparisons without a `> 0` / `>= 0` boundary check
- A minimum fee missing where the per-action fee can round to zero

### False Positive Indicators
- The rounded quantity is bounded by a whole-value equality elsewhere (`out.value === ...`)
- The operation cannot be split (one-shot, fixed-size)
- A `require(fee >= minFee)` or equivalent floor exists

---

## V006: Index Validation Missing (IX-1)

**Severity**: High
**Fixed by**: `tx.findInputOrFail(ref, msg)`, `list.findUniqueOrFail(p, msg)`, `list.singleOrFail(msg)` instead of index plumbing

### Description
`indexOf` returns -1 when nothing matches, and a redeemer-supplied index is attacker-chosen.
`List.at` fails on any out-of-range index, -1 included, so a bad index is a liveness failure
rather than theft; the real hazard is that an index says nothing about WHAT it points at. An
index that resolves to the wrong element passes every check written against "the element at idx".
The index plumbing itself is the smell: prefer the lookup that states the property.

### Vulnerable Pattern
```scala
def findDonation(donations: List[Donation], donor: PubKeyHash): Donation = {
  val idx = donations.indexOf(d => d.donor === donor)
  donations.at(idx)  // BAD: fails on -1; and nothing pins WHICH element idx names
}

// BAD: the redeemer picks the input; nothing checks it is the seed / own input
val seedInput = tx.inputs.at(redeemer.seedIndex)
```

### Secure Pattern
```scala
// Exactly one donation for this donor; zero or two is a failure.
def findDonation(donations: List[Donation], donor: PubKeyHash): Donation =
  donations.findUniqueOrFail(_.donor === donor, "Expected exactly one donation for donor")

// Locate by the property, not by position.
val ownInput = tx.findInputOrFail(ownRef, "Own input not found")

// When an index is kept for cost reasons, pin what it points at (from EditableNftValidator):
require(tx.inputs.at(seedIndex).outRef === seed, MustSpendSeed)
```

### Detection Patterns
Search for:
- `.indexOf(` without subsequent `>= 0` check
- `.at(idx)` where `idx` comes from the redeemer, without a check on the element it returns
- `filter(...).head` or `find(...).get` where "exactly one" is the intent (`findUniqueOrFail`)

### False Positive Indicators
- The element returned by `at(idx)` is immediately pinned (`outRef === ownRef`, address check, token check)
- Index is guaranteed valid by construction (e.g., always 0 for single-element list)
- The `at()` call is on `txInfo.inputs` or `txInfo.outputs` where index is from script's own UTxO lookup

---

## V007: Self-Dealing / Shill Bidding (DE-3)

**Severity**: High
**Fixed by**: no operation; a role-separation `require` on every branch (design-level, DETECT)

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

## V008: Double Spend via Index Reuse (IX-1)

**Severity**: High
**Fixed by**: strictly ascending index lists; `UtxoIndexer.multiOneToOneNoRedeemer` (walks the inputs, not the indices; see V029)

### Description
When processing multiple inputs/outputs via index arrays, lack of uniqueness validation allows double-spending. The same family covers length mismatch between two index lists and `zip` truncation (the shorter list silently drops the tail of the longer one).

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
- `zip` of two redeemer lists without a length check

---

## V009: Inexact Refund Amount (VP-4)

**Severity**: High
**Fixed by**: `===` on the whole `Value` (`refundOutput.value === Value.lovelace(amount)`); `tx.hasPaidTagged(addr, value, tag)` under batching

### Description
Using `>=` for refund validation allows manipulation - excess funds can be redirected. `>=` on a
payout is also the enabling condition for V005: an output that satisfies "at least X" can
satisfy two "at least X" obligations at once.

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

### Secure Pattern
```scala
def handleBid(currentHighestBid: BigInt, /* ... */) = {
  val refundOutput = txInfo.outputs.at(refundIdx)
  // GOOD: exact, and on the whole Value (lovelace-only `===` still lets tokens ride along)
  require(
    refundOutput.value === Value.lovelace(currentHighestBid),
    "Refund must be exact amount"
  )
}
```
For a payee that is a key and is paid in lovelace by design, summing the whole value and
projecting afterwards is the accepted form (from EscrowValidator):
```scala
require(
  sellerOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace ===
      escrowDatum.escrowAmount + escrowDatum.initializationAmount,
  "Seller must receive exactly escrow amount plus initialization amount"
)
```

### Detection Patterns
Search for:
- `>=` on a refund or payout amount
- `getLovelace >=` / `getLovelace ===` on an output that may carry tokens (see V027)

---

## V014: Missing Signature Validation (AU-2)

**Severity**: Medium
**Fixed by**: `tx.isSignedBy(pkh)` / `tx.isSignedByAny(keys)` for key authorities; for a script authority, prove the script ran (see below). Design-level (DETECT): no operation can add an absent check

### Description
Actions that should require authorization may be missing signature checks. A branch of the
redeemer enum simply forgets its check; there is nothing structurally wrong with the code.

**Signature is not authorization when the authority is a script.** A script-credential owner
(a multisig script, a DAO, a stake validator) cannot sign a transaction. `isSignedBy` on a
script hash is always false, and a check that skips it "because the owner is a script" is an
open door. For a script authority, check that the script actually ran in this transaction:
- an input at that credential is being spent (`tx.inputs.exists(_.resolved.address.credential === cred)`), or
- a withdrawal keyed by that credential exists (`tx.withdrawals.getOrFail(cred, msg)`, the
  withdraw-zero forwarding idiom; pin the redeemer with `StakeValidator.spend`, never `spendMinimal` alone, see V010).

### Vulnerable Pattern
```scala
redeemer.to[Action] match
  case Action.Cancel => require(txInfo.isSignedBy(datum.seller), "Seller must sign")
  case Action.Finalize => payout(datum)   // BAD: nobody has to sign
```

### Secure Pattern
```scala
def handleCancel(seller: PubKeyHash, /* ... */) = {
  require(txInfo.isSignedBy(seller), "Seller must sign cancellation")
}

def handleClaim(beneficiary: PubKeyHash, /* ... */) = {
  require(txInfo.isSignedBy(beneficiary), "Beneficiary must sign claim")
}

// Any of several keys:
require(txInfo.isSignedByAny(datum.admins), "An admin must sign")

// A script authority: prove the authority script ran in this transaction.
def requireOwnerAuthorized(owner: Credential, txInfo: TxInfo): Unit = owner match
  case Credential.PubKeyCredential(pkh) => require(txInfo.isSignedBy(pkh), "Owner must sign")
  case Credential.ScriptCredential(_) =>
    txInfo.withdrawals.getOrFail(owner, "Owner script must run (withdrawal)")
```

### Detection Patterns
Search for:
- Action handlers without `isSignedBy` / `isSignedByAny` checks
- State transitions without authorization
- Withdrawal/cancel operations without signature validation
- An owner or admin typed as `Credential` or `Address` whose script case is not handled
- `isSignedBy` applied to a hash that can be a script hash

### False Positive Indicators
- Authorization done via NFT/token ownership instead of signature
- The action is public by design (e.g., anyone can trigger liquidation if conditions met)
- Signature check exists in a helper function or parent method
- Authorization comes from spending a specific UTxO (implicit signature via spending)
- Multi-sig or DAO-based authorization via separate validator, AND the code proves that validator ran

---

## V015: Datum Mutation Not Validated (DT-1)

**Severity**: Medium
**Fixed by**: build the expected datum from the old one and compare the whole thing: `out.hasInlineDatum(oldDatum.copy(currentAmount = newAmount))`

### Description
State transitions should verify that immutable fields remain unchanged. Comparing field by field
invites the omission of one; comparing the whole expected datum cannot omit a field.

### Secure Pattern
```scala
// Whole-datum comparison: one `equalsData`, no decoding, no field can be forgotten.
val expectedDatum = oldDatum.copy(currentAmount = oldDatum.currentAmount + contribution)
require(continuingOutput.hasInlineDatum(expectedDatum), "Datum transition")
```
Use `inlineOrFail[T]` only when the new datum's fields are needed, then pin the immutable ones:
```scala
val newDatum = continuingOutput.datum.inlineOrFail[CampaignDatum]("Inline datum required")
require(newDatum.owner === oldDatum.owner, "Owner cannot change")
require(newDatum.deadline === oldDatum.deadline, "Deadline cannot change")
```

### Detection Patterns
Search for:
- Datum updates without field comparison
- State machines without transition validation
- Contracts accepting any datum without checking consistency
- `inlineOrFail` followed by checks on a subset of the fields

---

## V005: Double Satisfaction (DS-1 / DS-2)

**Severity**: Critical
**Fixed by**: `tx.inputs.findUniqueOrFail(_.resolved.address.credential === ownCred, msg)` (single own input per transaction) or `tx.hasPaidTagged(addr, value, tag)` with a `TxOutRef`-derived tag (also closes the cross-script variant DS-2)

### Description
When multiple UTxOs are consumed in a single transaction, each validator sees the same transaction outputs. A single output can satisfy validation requirements for multiple inputs, allowing attackers to pay once for multiple claims.

DS-2 is the cross-instance / cross-script variant: two different scripts (distinct hashes) that
both require "an output of V to address A" are satisfied by the same output. A single-own-input
guard does not see the other script's input; only a tagged output does.

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
Option 1, the single-own-input guard (from VestingValidator). Exactly one input at the own
credential per transaction, so no output can serve two instances. Measured cheaper than
`inputs.count(p) === BigInt(1)`:
```scala
val ownInputInfo = txInfo.findInputOrFail(txOutRef)
val ownCredential = ownInputInfo.resolved.address.credential
txInfo.inputs.findUniqueOrFail(
  _.resolved.address.credential === ownCredential,
  MultipleVestingInputs
)
```

Option 2, the tagged output. The payout carries a tag derived from the spent `TxOutRef`, which is
globally unique, so one output cannot satisfy two instances of this script or of any other
script (DS-2). All three comparisons are exact; a `>=` on the value reopens the hole:
```scala
val tag = OutputDatum.OutputDatum(ownRef.deriveTokenName.toData)
require(
  txInfo.hasPaidTagged(datum.beneficiary, Value.lovelace(datum.amount), tag),
  "Payout must be tagged with this UTxO's reference"
)
```

### Detection Patterns
Search for:
- `outputs.exists` or `outputs.find` without unique linking
- Multiple inputs expecting payment to same address
- Missing correlation between inputs and outputs
- Payout checks with `>=` (V009): the enabling condition for one output satisfying two obligations

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
// For UTxO_A, trace through spend:

// Line 58-61: the single-own-input guard runs before any handler
txInfo.inputs.findUniqueOrFail(
  _.resolved.address.credential === contractAddress.credential,
  "Exactly one escrow input may be spent"
)
// → inputs at the script credential: [UTxO_A, UTxO_B] → two matches → FAILS ❌

// Attack blocked before handlePay is reached. V005 is a FALSE POSITIVE.
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

### Example Trace: EscrowValidator (V005 FALSE POSITIVE, V027 FINDING)

**Suspicious code** (`handlePay`):
```scala
val contractBalance = txInfo.valueSpentFrom(contractAddress).getLovelace
// ...
val sellerOutputs =
    txInfo.findOutputsByCredential(Credential.PubKeyCredential(escrowDatum.seller))
require(
  sellerOutputs.foldLeft(Value.zero)(_ + _.value).getLovelace ===
      escrowDatum.escrowAmount + escrowDatum.initializationAmount,
  "Seller must receive exactly escrow amount plus initialization amount"
)
```

**Attack transaction 1 (V005)**: Spend UTxO_A and UTxO_B (both 12 ADA), create single 12 ADA output to seller.

**Trace execution**:
```
Line 58-61: inputs.findUniqueOrFail(_.resolved.address.credential === contractAddress.credential)
            → [UTxO_A, UTxO_B] both match → FAILS ❌
```

**Conclusion 1**: the attack fails at the single-own-input guard. V005 is a **FALSE POSITIVE**.

**Attack transaction 2 (V027, ADA-only comparison)**: the seller payout and `contractBalance`
are compared on `.getLovelace` only. Suppose the escrow UTxO holds 12 ADA + 500 USDM:
```
Inputs:   UTxO_A: 12 ADA + 500 USDM at script, datum={seller=S, amount=10, init=2}
Outputs:  12 ADA to seller S           ← getLovelace sum === 12, passes
          500 USDM to buyer B          ← tokens stripped, nothing checks them
Signatories: [B]
```
```
contractBalance = valueSpentFrom(contractAddress).getLovelace → 12 → passes
seller sum .getLovelace === 12 → passes
```
**Conclusion 2**: the lovelace-only comparisons pass while native tokens leave. This is a
**V027 finding** UNLESS the contract proves the UTxO is ADA-only at the boundary. EscrowValidator
does: `handleDeposit` requires `txInfo.valuePaidTo(contractAddress) === Value.lovelace(escrowAmount + initializationAmount)`
(whole value, exact), so no token can enter the escrow through `Deposit`. Trace the boundary
before reporting; report when no such proof exists.

---

## V010: Other Redeemer Attack (PU-1)

**Severity**: High
**Fixed by**: the compiler plugin's default `fail` for every unimplemented purpose (see below); for cross-script dependencies, `StakeValidator.spend` with a redeemer validator (pins WHICH redeemer ran), never `spendMinimal` alone

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

### False Positive: Scalus Validator Framework
The purpose methods of `Validator`, `ParameterizedValidator` and `DataParameterizedValidator`
(`spend`, `mint`, `reward`, `certify`, `vote`, `propose`) are declared `inline` and abstract,
with no body in the trait. The Scalus compiler plugin (`SIRPreprocessor`) completes every
`object` that extends one of these traits: for each purpose method the object does not define, it
synthesizes an inline override whose body throws (`"abstract method in Validator"`), which
compiles to a UPLC `error`. The dispatcher in `validate` therefore reaches a failing body for
every purpose the author did not write.

This is the reason V010 is a false positive, not a convention: a validator that only implements
`spend` cannot be invoked as a minting, rewarding, certifying, voting or proposing script, so
there is no weaker entry point to reach. The same property closes V032 (PU-3) and V033 (PU-4)
by default. **V010 is a false positive for a single-purpose Scalus validator object**; report it
only when the validator implements several purposes (or several redeemer branches) with
inconsistent authorization, or when a cross-script dependency checks that another script RAN
without checking WHICH redeemer it ran with (`StakeValidator.spendMinimal`).

---

## V011: Other Token Name Attack (MI-1)

**Severity**: High
**Fixed by**: `tx.mint.hasOnly(policy, name, qty)`; for a fixed set of names, `tx.mint.tokens(policyId) === expected.tokens(policyId)`

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
Validate the WHOLE sub-map of this policy:
```scala
def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo) = {
  // Exactly {expectedTokenName -> 1} under policyId, nothing else under it.
  require(tx.mint.hasOnly(policyId, expectedTokenName, 1), "Must mint exactly one token")
}
```
For a fixed set of names (from EditableNftValidator, the CIP-68 reference/user pair):
```scala
val expectedMint =
    Value(policyId, refTokenName, 1) + Value(policyId, userTokenName, 1)
require(
  tx.mint.tokens(policyId) === expectedMint.tokens(policyId),
  MustMintExactlyNftPair
)
```

### False Positive Indicators
- Policy uses `mint.hasOnly(...)` or compares `mint.tokens(policyId)` against a whole expected map
- Token name is derived deterministically from transaction data (`ownRef.deriveTokenName`) AND the sub-map is pinned
- Separate redeemer cases each validate their tokens AND check the whole sub-map

---

## V012: Missing UTxO Authentication (AU-1 / AU-5)

**Severity**: High
**Fixed by**: `ownInput.resolved.value.hasNft(authPolicy, authName)` on the own input and on every reference input read; the NFT itself comes from a one-shot policy (V028)

### Description
Without proper authentication, anyone can create UTxOs at a script address with arbitrary datums, potentially corrupting contract state. The same applies to **reference inputs**: a datum read from a reference input is attacker-planted unless the input carries the protocol's NFT. AU-5 is the state-machine form: a step that accepts a UTxO with no valid ancestry ("trust no UTxO").

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
  val ownInput = tx.findInputOrFail(ownRef, "Own input not found").resolved

  // Exactly one unit of the authentication NFT
  require(
    ownInput.value.hasNft(authPolicyId, authTokenName),
    "UTxO must contain authentication token"
  )

  // A datum read from a reference input needs the same proof
  val oracleInput = tx.referenceInputs.findUniqueOrFail(
    _.resolved.value.hasNft(oraclePolicyId, oracleTokenName),
    "Expected exactly one oracle reference input"
  )
  // Now safe to process
}
```

### Detection Patterns
Search for:
- `referenceInputs` read by address or datum shape without a token check
- Own input processed without `hasNft` / `hasOnly` when the protocol has a state token
- Minting policies for the auth token that are not one-shot (V028)

---

## V013: Time Handling (TI-1 / TI-2)

**Severity**: Medium (Critical when a time lock is the only guard)
**Fixed by**: `tx.validFromOrFail(msg)` / `tx.validToOrFail(msg)` (fail on an unbounded bound); `tx.validRange.isEntirelyAfter(t)` / `isEntirelyBefore(t)` (closure-aware, fail closed on an infinite bound)

### Description
Validators only see time intervals (validity ranges), not exact timestamps, and the
**transaction author chooses the interval**. Two classes:

- **TI-1, unbounded range read as "now"**. A bound may be infinite. Any helper that projects the
  lower bound to a number must invent a value for the infinite case. The deprecated
  `TxInfo.getValidityStartTime` returned `0` for an unbounded range, so a transaction with NO
  lower bound was treated as happening at the Unix epoch and every "has the deadline passed?"
  comparison flipped. In the Scalus examples, Vesting and DecentralizedIdentity both read the
  lower bound through it; the migration replaced both with `validFromOrFail`, which fails the
  script when the bound is not finite. There is no defaulting variant.
- **TI-2, inclusivity**. The ledger builds the lower bound closed and the upper bound open:
  `validFromOrFail` is the earliest inclusion time (inclusive), `validToOrFail` is exclusive.
  `IntervalBound.finite(default)` and `finiteOrFail` drop the closure flag; the two `TxInfo`
  accessors state it in their scaladoc.

### Vulnerable Pattern
```scala
def spend(/* ... */) = {
  // BAD (TI-1): 0 when the range has no lower bound; a backdated or absent validFrom passes
  val now = tx.getValidityStartTime
  require(now >= datum.unlockAt, "Too early")

  // BAD (TI-2): the raw bound is an IntervalBound, its closure flag is ignored
  val to = tx.validRange.to.finiteOrFail("no upper bound")
  require(to <= deadline, "Too late")
}
```

### Secure Pattern (from HtlcValidator)
```scala
redeemer.to[Action] match
  case Action.Timeout =>
    val validFrom = tx.validFromOrFail(ValidRangeMustBeBound)
    // validFrom is inclusive, hence 10 <= 10 is correct
    require(config.timeout <= validFrom, InvalidCommitterTimePoint)
  case Action.Reveal(preimage) =>
    val validTo = tx.validToOrFail(ValidRangeMustBeBound)
    // validTo is exclusive, hence 10 <= 10 is correct
    require(validTo <= config.timeout, InvalidReceiverTimePoint)
```
Interval predicates when no number is needed (from Auction); both return `false` on an infinite bound:
```scala
require(txInfo.validRange.isEntirelyBefore(auctionEndTime), "Auction has ended")
require(txInfo.validRange.isEntirelyAfter(auctionEndTime), "Auction still running")
```
Bounding the attacker's window: `validToOrFail(m) - validFromOrFail(m) <= maxWidth` fails closed for free.

### Detection Patterns
Search for:
- `getValidityStartTime` (deprecated; returns 0 on an unbounded range)
- `IntervalBound.finite(` with a default value
- `validRange.from` / `validRange.to` compared as raw bounds
- A datum timestamp written from `validToOrFail` and later treated as an exact posting time (it can be late, never early)

---

## V016: Insufficient Staking Control (AU-4)

**Severity**: Medium
**Fixed by**: `tx.findContinuingOutputOrFail(ownInput, msg)` (whole address); or `findUniqueOrFail` by credential followed by `require(out.address === ownInput.resolved.address)`

### Description
An address is a pair: payment credential and optional staking credential. A validator that
compares only the payment credential accepts an output at `(myScriptHash, ATTACKER_stake_key)`
("franken" / "mangled" address). The attacker cannot spend the funds but collects all staking
rewards on the protocol's TVL and can delegate the stake. A second consequence: a "only one input
at this credential" guard keeps working, but the protocol's funds now sit at many distinct
addresses. In the Scalus examples, PaymentSplitter carried the payout-side variant (payee
outputs matched on the payment credential only).

### Attack Scenario
1. Contract checks `out.address.credential === own.address.credential`
2. Attacker builds the continuing output at the same payment credential and their own staking key
3. All staking rewards go to attacker

### Vulnerable Pattern
```scala
require(out.address.credential === ownInput.resolved.address.credential, "must return to script")
require(out.address === Address.fromScriptHash(ownHash), "...")   // no staking credential at all
val outs = tx.findOutputsByScriptHash(ownHash)                     // payment credential only
```

### Secure Pattern (from EscrowValidator)
```scala
// Unique output to the WHOLE own address, staking part included.
val contractOutput =
    txInfo.findContinuingOutputOrFail(ownInput, "Expected exactly one contract output")
```
When a credential-only finder is kept for another reason, pin the whole address afterwards:
```scala
val out = tx.outputs.findUniqueOrFail(_.address.credential === ownCred, "one own output")
require(out.address === ownInput.resolved.address, "Must return to the same full address")
```

### Detection Patterns
Search for:
- `address.credential ===` on a continuing output or a payout
- `Address.fromScriptHash` / `Address.fromPubKeyHash` compared against an output address
- `findOutputsByScriptHash` / `findOutputsByCredential` used for the continuing output

---

## V017: Arbitrary Datum (DT-3)

**Severity**: Medium
**Fixed by**: `out.datum.inlineOrFail[T](msg)` when fields are needed; `out.hasInlineDatum(expected)` when equality is the check. Both reject a missing datum and a datum hash

### Description
Insufficient datum validation when locking funds can cause UTxOs to become unspendable if the datum doesn't match expected format. The datum-hash form of the same class: a continuing output that carries only a datum HASH whose preimage is never published bricks the UTxO (the next spend cannot supply the datum).

### Secure Pattern
```scala
// Validate datum structure before accepting
def mint(/* ... */) = {
  val output = tx.outputs.at(outputIdx)

  // Inline datum required; a hash or no datum fails here
  val datum = output.datum.inlineOrFail[ExpectedDatum]("Inline datum required")

  // Validate datum fields
  require(datum.owner.length === BigInt(28), "Invalid owner hash")
  require(datum.amount > 0, "Amount must be positive")
}
```

### Detection Patterns
Search for:
- `datum.to[T]` on an `OutputDatum` match arm that accepts `OutputDatumHash` or `NoOutputDatum`
- Continuing outputs whose datum is never inspected

---

## V018: Unbounded Value (RS-1)

**Severity**: Low (Design Issue)
**Fixed by**: `value.withoutLovelace.isZero` for an ADA-only protocol; `out.value.hasSameTokensAndAtLeastAda(expected)` pins the token set of a continuing output (dust cannot be added)

### Description
UTxOs with unlimited tokens can exceed size/execution limits, making funds unspendable. Token dust deposited into a protocol UTxO is also the lever for V035 (min-ADA griefing).

### Mitigation
Declare the token set at the boundary (from PaymentSplitterValidator):
```scala
// Only ADA is split. A contract UTxO holding native tokens would let the fee payer pocket
// those tokens for free, so reject non-ADA contract inputs outright.
require(
  input.resolved.value.withoutLovelace.isZero,
  "Contract input must contain only ADA"
)
```
On a continuing output, `hasSameTokensAndAtLeastAda(expected)` makes the non-ADA part exactly `expected`'s, so no new token type can enter.

---

## V019: Unbounded Datum (RS-2)

**Severity**: Low (Design Issue)
**Fixed by**: no operation; bound every list in the datum (DETECT)

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

## V020: Unbounded Inputs (RS-3)

**Severity**: Low (Design Issue)
**Fixed by**: no operation; design for bounded inputs (DOCUMENT)

### Description
Operations requiring many UTxOs may exceed transaction limits.

### Mitigation
Design for bounded input requirements; use batching patterns.

---

## V021: UTxO Contention / EUTXO Concurrency DoS (RS-5)

**Severity**: Low-Medium (Design Issue)
**Fixed by**: no operation; per-user UTxOs, batching, rate limits (DOCUMENT)

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
    ctx.validFromOrFail("Lower bound required") > state.lastUpdate + minInterval,
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

## V022: Cheap Spam / Dust Attack (RS-6)

**Severity**: Low
**Fixed by**: no operation; a minimum-amount `require` at the entry point (DOCUMENT)

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

## V023: Locked Value (DE-1)

**Severity**: Low (Design Issue; Critical when it is reachable)
**Fixed by**: no operation; every state needs an exit path (DETECT). Related: V017 datum-hash bricking, V035 min-ADA griefing

### Description
Design flaws can make funds permanently inaccessible.

### Mitigation
- Include emergency withdrawal mechanisms
- Verify all state transitions have valid exits
- Consider timeout-based fallbacks

---

## V024: Parameterization Verification (AU-6)

**Severity**: Varies (assess per case)
**Fixed by**: an authentication NFT checked with `hasNft` (V012), minted by a one-shot policy (V028); otherwise a published script hash (DOCUMENT + pattern)

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
    val ownInput = ctx.findInputOrFail(ownRef, "Own input not found").resolved

    // Auth token verifies this is a legitimate instance
    require(
      ownInput.value.hasNft(params.authPolicyId, authTokenName),
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

## V025: Oracle Data Validation (DE-2)

**Severity**: High (when applicable)
**Fixed by**: no operation; `verifyEd25519Signature` over a domain-separated payload (V031), freshness against `validToOrFail`, an NFT-authenticated reference input (V012) (DOCUMENT)

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

  // 1. Verify oracle signature over a DOMAIN-SEPARATED payload (see V031): the payload must
  //    commit to this script instance and to a nonce, or the signature is replayable.
  val message = serializeForSigning(oracleData.price, oracleData.timestamp)
  require(
    verifyEd25519Signature(trustedOraclePubKey, message, oracleData.signature),
    "Invalid oracle signature"
  )

  // 2. Check data freshness. The transaction runs before its upper bound, so the data's age at
  //    execution is at most validTo - timestamp. Fails when the range is unbounded.
  require(
    ctx.validToOrFail("Upper bound required") - oracleData.timestamp <= maxOracleAge,
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

## V026: Value Not Preserved on Continuing Output (VP-1)

**Severity**: Critical
**Fixed by**: `out.value.hasSameTokensAndAtLeastAda(expected)` (tokens exact, ADA open above) or `out.value === expected` (exact); tie every datum balance to `out.value.quantityOf(asset)`
**In the Scalus examples**: AMM (datum reserves never tied to the pool output value; pool fully drainable), Lottery, Vesting, PaymentSplitter

### Description
A spending validator that checks the DATUM transition but not the VALUE lets the attacker keep
the difference. Three variants: (a) no value check at all; (b) `>=` where `===` was meant; (c) the
datum records a balance that is never tied to the actual `Value` of the output. Variant (c) is the
worst because the code looks thorough.

### Attack Scenario
```
Input:   pool @ script, value = 1_000_000 tokenX + 1_000_000 tokenY, datum = Reserves(1e6, 1e6)
Outputs: pool @ script, value = 1 tokenX + 1 tokenY, datum = Reserves(1e6, 1e6)   <- datum lies
         999_999 tokenX + 999_999 tokenY -> attacker
```

### Vulnerable Pattern
```scala
// Only the datum transition is checked; value never mentioned
val out = tx.findContinuingOutputOrFail(ownInput, "one pool output")
require(out.hasInlineDatum(expectedReserves), "reserves")
```

### Secure Pattern (from AmmValidator)
```scala
// Bind the datum reserves to the tokens actually held by the continuing pool output.
require(
  poolOutput.value.quantityOf(params.t0._1, params.t0._2) === newDatum.r0,
  ReserveT0Mismatch
)
require(
  poolOutput.value.quantityOf(params.t1._1, params.t1._2) === newDatum.r1,
  ReserveT1Mismatch
)
require(poolOutput.value.hasNft(poolPolicyId, poolNftName), "Pool output must retain the pool NFT")
```
Whole-value preservation minus an authorized withdrawal (from VestingValidator):
```scala
require(
  contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
  ContinuingValueMismatch
)
```
When the builder may add lovelace for min-ADA, use `hasSameTokensAndAtLeastAda(expected)`:
`===` rejects a valid top-up, and a whole-value `>=` lets one output satisfy two "at least"
obligations (V005).

### Detection Patterns
Search for:
- A continuing output whose `.value` is never read
- A datum field named like a balance (`reserves`, `amount`, `balance`) with no `quantityOf` binding it to the output
- `getLovelace >=` on a continuing output (V027 and V009 at once)

### False Positive Indicators
- The ledger's own balance rule is enough because the script holds nothing but the state NFT (verify with `hasOnly`)
- Value is pinned by a whole-`Value` equality elsewhere on the same output

---

## V027: ADA-Only Value Comparison (VP-2)

**Severity**: Critical
**Fixed by**: compare the whole `Value` (`out.value === expected`, `hasSameTokensAndAtLeastAda`, `tx.valuePaidTo(addr) === v`); or prove the UTxO is ADA-only at the boundary: `value.withoutLovelace.isZero`
**In the Scalus examples**: Vesting (continuing-output check was lovelace-only; native tokens could be stripped), PaymentSplitter (both validators reconciled `getLovelace` only; tokens in a contract UTxO could be skimmed by the fee payer)

### Description
`Value` is a two-level map, but the ergonomic accessor is `getLovelace`. A check written in
lovelace passes while every native token in the UTxO is redirected to the attacker. The
deprecated `Utils.getAdaFromOutputs` / `getAdaFromInputs` were lovelace-only by construction;
`valuePaidTo` / `valueSpentFrom` sum the whole `Value` so a caller who wants only ADA projects it
afterwards, and a caller who forgets gets the safe answer.

### Attack Scenario
```
Input:   vault @ script, value = 10 ADA + 500 USDM
Outputs: vault @ script, value = 10 ADA          <- passes the getLovelace check
         500 USDM -> attacker
```

### Vulnerable Pattern
```scala
require(out.value.getLovelace === own.value.getLovelace, "value preserved")   // WRONG
require(tx.valuePaidTo(contractAddress).getLovelace === expected, "...")      // still lovelace-only
```

### Secure Pattern (from EscrowValidator)
```scala
// Whole-value check: the continuing output carries exactly the escrow amount plus the
// initialization amount, and nothing else.
require(
  txInfo.valuePaidTo(contractAddress) ===
      Value.lovelace(escrowDatum.escrowAmount + escrowDatum.initializationAmount),
  "Contract output must contain exactly escrow amount plus initialization amount"
)
```
If the protocol is genuinely ADA-only, prove it at the boundary (from PaymentSplitterValidator):
```scala
require(input.resolved.value.withoutLovelace.isZero, "Contract input must contain only ADA")
```
A `.getLovelace` projection is acceptable ONLY for a key payee paid in ADA by design (the
seller of an escrow, the beneficiary of a vesting), never for a script UTxO.

### Detection Patterns
Search for:
- `getLovelace` compared on a script's continuing output or on a value sum of script inputs/outputs
- `getAdaFromOutputs` / `getAdaFromInputs` (deprecated, lovelace-only)
- `valuePaidTo(...).getLovelace` / `valueSpentFrom(...).getLovelace` on a script address

### False Positive Indicators
- The UTxO is proven ADA-only at deposit (`withoutLovelace.isZero`, or `valuePaidTo(addr) === Value.lovelace(n)`)
- The payee is a key, and the tokens have no owner other than the payer

---

## V028: One-Shot Seed Not Bound (MI-2)

**Severity**: Critical
**Fixed by**: `tx.findInputOrFail(seed, "seed must be spent")` as a statement, or `require(tx.inputs.at(seedIndex).outRef === seed, msg)`; derive the name with `seed.deriveTokenName`; pin the mint with `tx.mint.hasOnly(policyId, name, 1)`
**In the Scalus examples**: EditableNft (checked that SOME input existed at the seed index, never compared it to the parameter; one-shot mint defeated, NFT uniqueness broken; fixed)

### Description
A one-shot policy is parameterized by a `TxOutRef` and must require that THAT exact reference is
consumed in the minting transaction. A UTxO can be spent once, so the policy can fire once. If the
check weakens to "some input exists at index i" or "an input with the right index number",
uniqueness collapses and the NFT is mintable forever. Every authentication built on that NFT
(V012, V024) collapses with it.

### Vulnerable Pattern
```scala
require(tx.inputs.get(seedIndex).isDefined, "seed present")   // never compares to the seed
```

### Secure Pattern (from EditableNftValidator)
```scala
val seed = param.to[TxOutRef]
// Bind the seed: the input at seedIndex must be the exact parameterized seed UTxO, not merely
// some input that exists. A wrong index simply fails the check (fails closed).
require(tx.inputs.at(seedIndex).outRef === seed, MustSpendSeed)
// Pin the whole mint under this policy
require(tx.mint.hasOnly(policyId, seed.deriveTokenName, 1), "Must mint exactly one")
```
Without an index (one scan, no `Option`):
```scala
tx.findInputOrFail(seed, "Seed UTxO must be consumed")
```
The off-chain builder must pick the seed and the policy parameter from the same value.

### Detection Patterns
Search for:
- A `TxOutRef` parameter that is never compared with `===` against an input's `outRef`
- `inputs.get(idx).isDefined` / `inputs.at(idx)` in a minting policy with no `outRef ===`
- A token name that is not derived from the seed (`deriveTokenName`) in a policy that claims uniqueness

---

## V029: Missed Input (IX-2)

**Severity**: Critical
**Fixed by**: iterate the INPUT list and require every input at the protocol credential to be covered; `UtxoIndexer.multiOneToOneNoRedeemer` does this in both directions

### Description
The UTxO-indexer pattern (and the withdraw-zero "global validator" pattern) delegates per-input
validation to a single pass driven by redeemer-supplied indices. If the global validator checks
only the inputs the indices NAME, an attacker adds one more script input that no index names, and
that input is spent with no validation at all. Note that the singular indexer patterns
(`oneToOne`, `oneToMany`) solve missed-input, NOT double satisfaction (V005).

### Attack Scenario
```
Inputs:  scriptUtxo0 (indexed), scriptUtxo1 (indexed), scriptUtxo2 (NOT indexed)
Redeemer (global): pairs = [(0,0), (1,1)]
Outputs: correct continuations for 0 and 1; utxo2's value -> attacker
```

### Vulnerable Pattern
```scala
// Iterates the index list; a script input that no pair names is never looked at
indexPairs.foreach { case (inIdx, outIdx) =>
  validate(tx.inputs.at(inIdx), tx.outputs.at(outIdx))
}
```

### Secure Pattern (from UtxoIndexer.multiOneToOneNoRedeemer)
The walk is over `tx.inputs`: when it meets a script-credential input with no pair left it fails
with `MoreScriptUtxosSpentThanSpecified`; at the end it requires `remainingPairs.isEmpty`
(`UnprocessedIndexPairs`). Both directions are covered.
```scala
UtxoIndexer.multiOneToOneNoRedeemer(
  indexPairs,
  scriptHash,
  tx,
  (inIdx, input, outIdx, output) => {
    require(output.address === input.resolved.address, "Must return to the same address")
    require(output.value.hasSameTokensAndAtLeastAda(input.resolved.value), "Value preserved")
  }
)
```
Every pattern callback returns `Unit`; put each obligation in its own `require` (V030).

### Detection Patterns
Search for:
- A loop over redeemer indices in a global / stake validator without a walk over `tx.inputs`
- No `count` / `findUniqueOrFail` / walk that bounds how many script inputs the transaction has
- `oneToOne` / `oneToMany` used as the only defence against V005

---

## V030: Evaluation-Order Trap (EV-1)

**Severity**: High
**Fixed by**: one obligation per `require`; pattern callbacks return `Unit` (a `Boolean` result is discarded silently; wrap it in `require`)

### Description
UPLC control flow is lazy. `&&` and `||` short-circuit, `if` / `match` evaluate only the taken
branch, and `fail` only fires when forced. A security-relevant predicate placed on the right of
`||`, or inside an untaken branch, silently never runs.

Migration hazard: the validator callbacks of `UtxoIndexer`, `StakeValidator` and
`TransactionLevelMinterValidator` take `=> Unit`. A lambda that still ends in a `Boolean`
expression compiles (the value is discarded) and validates nothing.

### Attack Scenario
A transaction that satisfies the cheap left-hand disjunct, so the expensive right-hand check (the
one that protects the funds) never evaluates.

### Vulnerable Pattern
```scala
require(isEmergency || (tx.isSignedBy(owner) && valuePreserved(out)), "...")
// when isEmergency is true, NOTHING else is checked

UtxoIndexer.oneToOne(ownRef, inIdx, outIdx, tx, (input, output) =>
  output.address === input.resolved.address   // Boolean, discarded: a silent no-op
)
```

### Secure Pattern
```scala
require(isEmergency || tx.isSignedBy(owner), "authorized")
require(valuePreserved(out), "value preserved")     // unconditional

UtxoIndexer.oneToOne(ownRef, inIdx, outIdx, tx, (input, output) =>
  require(output.address === input.resolved.address, "Must return to the same address")
)
```

### Detection Patterns
Search for:
- `||` whose right operand contains a `require`-worthy check
- A `&&` chain inside one `require` where a later conjunct is the security check
- A pattern callback (`validator = ...`, `perOutputValidator = ...`) whose last expression is a `Boolean`
- Checks placed only in one arm of an `if` / `match` that the redeemer selects

---

## V031: Signature Domain Separation (AU-7)

**Severity**: High (when applicable: any `verifyEd25519Signature` over an application payload)
**Fixed by**: no operation; the signed payload must commit to a protocol tag, this script hash / policy id, a nonce (a `TxOutRef` spent in this transaction), and every security-relevant field

### Description
A signature verified on-chain over an application-defined payload is replayable across script
instances, across protocols, across networks, and repeatedly within one protocol, unless the
payload carries a domain separator.

### Attack Scenario
Attacker takes a valid oracle signature over `(price, timestamp)` published for protocol A and
replays it into protocol B, or into instance 2 of protocol A, or twice into the same instance.

### Vulnerable Pattern
```scala
val msg = serialiseData(OracleData(price, timestamp).toData)
require(verifyEd25519Signature(oracleKey, msg, sig), "bad signature")
```

### Secure Pattern
```scala
case class SignedPayload(
    domain: ByteString,      // protocol tag, e.g. "myprotocol.oracle.v1"
    instance: ScriptHash,    // THIS script instance
    nonce: TxOutRef,         // spent in this transaction, so the payload is single-use
    price: BigInt,
    timestamp: PosixTime
) derives ToData

val payload = SignedPayload(domainTag, ownScriptHash, ownRef, price, timestamp)
tx.findInputOrFail(payload.nonce, "Nonce must be spent")
require(verifyEd25519Signature(oracleKey, serialiseData(payload.toData), sig), "bad signature")
```

### Detection Patterns
Search for:
- `verifyEd25519Signature` / `verifyEcdsaSecp256k1Signature` / `verifySchnorrSecp256k1Signature`
  whose message omits the script hash or a spent `TxOutRef`
- Signed payloads that are pure application data (`price`, `amount`, `recipient`)

---

## V032: Certificate Purposes Unguarded (PU-3)

**Severity**: High
**Fixed by**: the plugin's default `fail` for an unimplemented `certify` (V010); when `certify` is implemented, an explicit `TxCert` match with a failing default arm

### Description
A stake credential controlled by a script is gated by that script for EVERY certificate action,
not just withdrawals. A `certify` that approves anything lets an unrelated party submit a
deregistration certificate: the credential is deregistered (every withdraw-zero forwarding spend
now fails until re-registration) and the key deposit is refunded to the attacker's chosen account.
The mirror attack is an unsolicited registration or delegation.

### Attack Scenario
```
Certificates: [ UnRegStaking(scriptCredential, refund) ]
Redeemer:     Certifying(0, ...) -> whatever the script accepts
Outputs:      deposit refund -> attacker
```

### Vulnerable Pattern
```scala
inline override def certify(redeemer: Data, cert: TxCert, tx: TxInfo): Unit = ()   // permissive
```

### Secure Pattern (from OptimizedPaymentSplitterValidator)
```scala
inline override def certify(payeesData: Data, redeemer: Data, cert: TxCert, tx: TxInfo): Unit = {
  cert match
    case TxCert.RegStaking(_, _)   => () // Allow registration
    case TxCert.UnRegStaking(_, _) => () // Allow de-registration
    case _ => fail("Only stake registration/de-registration allowed")
}
```
Decide deliberately whether third parties may deregister; deny it unless the protocol intends it.
Not implementing `certify` at all is safe: the plugin's default body fails.

### Detection Patterns
Search for:
- `override def certify` with a body of `()` or a match with a permissive `case _ => ()`
- A hand-written dispatcher on `ScriptInfo` with a `case _ => ()` fallthrough
- `reward`-only validators whose `certify` allows `UnRegStaking` without authorization

---

## V033: Voting / Proposing Purposes Unguarded (PU-4)

**Severity**: Medium
**Fixed by**: the plugin's default `fail` for unimplemented `vote` / `propose` (V010); when implemented, an explicit match with a failing default arm, and the index-checked certificate / procedure

### Description
Plutus V3 added `VotingScript(Voter)` and `ProposingScript(index, ProposalProcedure)`. A DRep or
constitution script that dispatches on `ScriptInfo` without handling these silently approves
governance actions. The `ProposingScript` / `CertifyingScript` purposes also carry a 0-based
index into `tx.proposalProcedures` / `tx.certificates`, so index confusion (V006) applies here.

### Vulnerable Pattern
```scala
// Hand-written dispatcher
sc.scriptInfo match
  case ScriptInfo.SpendingScript(ref, datum) => spend(...)
  case _ => ()   // approves every vote and proposal
```

### Secure Pattern
Extend `Validator` and implement only the purposes the script has; leave `vote` / `propose`
unimplemented so the plugin's default body fails. When a governance purpose is intended:
```scala
inline override def vote(redeemer: Data, voter: Voter, tx: TxInfo): Unit = {
  require(tx.isSignedByAny(drepCommittee), "Committee must sign the vote")
}
```

### Detection Patterns
Search for:
- Hand-written `ScriptInfo` dispatchers with a permissive default arm
- `override def vote` / `override def propose` with no authorization

References: CIP-69 (script purposes in Plutus V3): https://cips.cardano.org/cip/CIP-0069

---

## V034: Value-Map Normalisation (VP-5)

**Severity**: Medium
**Fixed by**: decode attacker-supplied `Value`s with `Value.valueFromDataWithValidation` (rejects zero amounts and non-ascending keys); compare ledger-provided values freely, they are canonical

### Description
A `Value` is a nested map serialised as `Data`. Two representations can denote the same value:
one with a zero-quantity entry and one without, or with keys in a different order. `Value`'s
`===` compares the sorted maps structurally, and `hasOnly` compares the policy sub-map with one
`equalsData`; both assume canonical form. Every ledger-provided value (`out.value`,
`input.resolved.value`, `tx.mint`) IS canonical (strictly ascending keys, no zero amounts, no
empty inner maps), so comparisons among them are safe. A `Value` decoded from a redeemer or datum
with a plain `to[Value]` is not validated: an attacker-supplied zero entry makes a legitimate
comparison fail (locking the UTxO), or a non-canonical value compares unequal to an equal one.

### Attack Scenario
Attacker stores `(policyX, nameY, 0)` in a datum field of type `Value`. The validator compares it
against the ledger value with `===`; the comparison fails on every legitimate spend; protocol halted.

### Vulnerable Pattern
```scala
val expected = datum.lockedValue                 // decoded with the plain FromData[Value]
require(out.value === expected, "value preserved")
```

### Secure Pattern
```scala
// At the decoding boundary, once. The given must be in scope where LockDatum's FromData is
// derived (its companion), not at the `.to[LockDatum]` call site.
@Compile
object LockDatum {
  given FromData[Value] = Value.valueFromDataWithValidation
  given FromData[LockDatum] = FromData.derived
}

val expected = datum.to[LockDatum].lockedValue    // canonical or the script fails
require(out.value === expected, "value preserved")
```
Or avoid the attacker-supplied `Value` altogether: check named quantities with `quantityOf` /
`hasNft` / `hasOnly` against ledger values.

### Detection Patterns
Search for:
- A `Value` field in a datum or redeemer type
- `===` between a ledger value and a decoded `Value` without `valueFromDataWithValidation`

### False Positive Indicators
- Both operands are ledger-provided or built from ledger values with `+` / `-` / `Value.lovelace`

---

## V035: Min-ADA Griefing on Forced Outputs (VP-6)

**Severity**: Medium
**Fixed by**: bound the token set at the DEPOSIT boundary (`withoutLovelace.isZero`, or `hasSameTokensAndAtLeastAda` against a fixed expected value); keep an ADA headroom invariant

### Description
Every output must carry a minimum ADA proportional to its size. A validator that forces an output
of a fixed non-ADA value, or forces a datum, can be pushed below min-ADA by an attacker who
inflates the output's size (more token types, a bigger datum). The transaction then cannot be
built at all and the UTxO is stuck (V023).

### Attack Scenario
Attacker deposits 40 distinct dust tokens into the vault UTxO. The "return the same value to the
script" rule now forces an output whose min-ADA exceeds the ADA actually in the UTxO. No valid
spending transaction exists.

### Vulnerable Pattern
```scala
// Any "preserve the value" rule on a UTxO whose token set is attacker-controlled
require(out.value === ownInput.resolved.value, "value preserved")
```

### Secure Pattern
```scala
// Deposit boundary: only the protocol's own assets may enter
require(
  depositOutput.value.hasSameTokensAndAtLeastAda(Value.lovelace(minDeposit) + stateNft),
  "Deposit may carry only ADA and the state NFT"
)
```
`hasSameTokensAndAtLeastAda` leaves ADA open above, so a min-ADA top-up is never rejected.

### Detection Patterns
Search for:
- A whole-value preservation rule on a UTxO that anyone can pay into
- Datum lists that grow with user input on a UTxO with fixed ADA (V019)

---

## V036: Hash Grinding (DE-4)

**Severity**: Medium
**Fixed by**: no operation; commit-reveal (hash a secret in one transaction, reveal it in a later one), never a hash of transaction data as randomness (DOCUMENT)

### Description
Validation is deterministic and the transaction author controls the transaction's contents, so
any outcome derived from a hash of transaction data can be ground: the author retries until the
hash is favourable. "Which bucket does this land in", "who wins the raffle", "which input comes
first" (the ledger sorts inputs by `(txId, idx)`, so grinding a `txId` also grinds input
position) are all grindable.

### Vulnerable Pattern
```scala
// The author retries the transaction until the hash picks the winner they want
val roll = byteStringToInteger(true, blake2b_256(serialiseData(tx.id.toData)))
val winner = players.at(roll % players.length)
```

### Secure Pattern (from LotteryValidator)
Each player commits `sha2_256(preimage)` in an earlier transaction; the outcome is a function of
both revealed preimages, so neither player can grind alone:
```scala
case Action.RevealPlayerOne(preimage) =>
  require(sha2_256(preimage) === state.playerOneSecret, "Fraudulent attempt")
```

### Detection Patterns
Search for:
- `tx.id`, an output reference, or a hash of them used to select a winner, a slot, or an index
- Randomness derived from anything the transaction author controls

---

## V037: Reference-Script Size (RS-7)

**Severity**: Low (Design Issue)
**Fixed by**: no operation; keep the compiled script small and report its size in review (DOCUMENT)

### Description
Reference scripts let a transaction point at an on-chain script instead of embedding it. Their
size is fee-priced under Conway (a tiered per-byte fee with a per-transaction cap) after a 2024
mainnet incident in which many reward-purpose reference scripts were run per transaction for a
negligible fee, degrading node performance. Large scripts also raise every user's fees and can
exceed the per-transaction reference-script size cap, which makes a design that needs several
large reference scripts in one transaction unbuildable.

### Mitigation
- Report the compiled script size (bytes) in the review; treat growth as a regression
- Split rarely-used logic into a separate script rather than one large validator
- Note the `MerkelizedValidator` pattern's size warning when many scripts run per transaction

References: https://github.com/IntersectMBO/cardano-ledger/issues/3952

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

Any of these that a validator `object` does not define is completed by the compiler plugin with a
body that fails (see V010). Check only the ones the object defines.

### Safe API (prefer these; the deprecated spelling is in brackets)
- Own input: `tx.findInputOrFail(ownRef, msg)` [`findOwnInputOrFail`]
- Continuing output: `tx.findContinuingOutputOrFail(ownInput, msg)` (whole address); never a
  credential-only finder or `=== Address.fromScriptHash(h)` for it
- Exactly one: `list.findUniqueOrFail(p, msg)`, `list.singleOrFail(msg)`; never `filter(...).head`
- Value sums: `tx.valuePaidTo(addr)`, `tx.valueSpentFrom(addr)` (whole `Value`; project with
  `.getLovelace` only for a key payee paid in ADA by design) [`getAdaFromOutputs` / `getAdaFromInputs`]
- Continuing value: `out.value.hasSameTokensAndAtLeastAda(expected)`; state token: `out.value.hasNft(p, n)`
- Mint: `tx.mint.hasOnly(policy, name, signedQty)`; burn-only: `tx.onlyBurnsUnder(policy)`
- Datum: `out.hasInlineDatum(expected)` for equality, `out.datum.inlineOrFail[T](msg)` for fields
- Time: `tx.validFromOrFail(msg)` (inclusive), `tx.validToOrFail(msg)` (exclusive) [`getValidityStartTime`];
  `validRange.isEntirelyBefore/After` when no number is needed
- Signatures: `tx.isSignedBy(pkh)`, `tx.isSignedByAny(keys)`; a script authority cannot sign (V014)
- Credentials: `cred.scriptHashOrFail(msg)`, `cred.pubKeyHashOrFail(msg)`
- Division: `a divCeil b`, `a divFloor b` (state the rounding direction)
- Double satisfaction: single-own-input `findUniqueOrFail`, or `tx.hasPaidTagged(addr, value, tag)`
  with `ownRef.deriveTokenName`
- Equality: derive `Eq` and use `===`; it lowers to the same `equalsData` as `a.toData == b.toData`

### Finding Script Hash
```scala
// In spend validator, get own script hash from ownRef
val ownInput = txInfo.findInputOrFail(ownRef, "Own input not found")
val scriptHash = ownInput.resolved.address.credential.scriptHashOrFail("Not a script")
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
