# Scalus Smart Contract Testing Strategy

## Part 1: Testing Platforms & What They Provide

### Platform Comparison

| Platform        | What it tests               | Speed     | Budget | Ledger Rules | Network |
|-----------------|-----------------------------|-----------|--------|--------------|---------|
| **JVM**         | Scala logic correctness     | Very fast | No     | No           | No      |
| **PlutusVM**    | UPLC execution + budget     | Fast      | Yes    | No           | No      |
| **Emulator**    | Full tx validation + ledger | Medium    | Yes    | Yes          | No      |
| **Yaci DevKit** | Real network behavior       | Slow      | Yes    | Yes          | Yes     |
| **Testnet**     | Production-like             | Very slow | Yes    | Yes          | Yes     |

### What Each Platform Uniquely Provides

**JVM Only:**

- Scala debugger (breakpoints, stepping)
- Fast iteration (1000s of tests in seconds)
- Exception stack traces with line numbers

**PlutusVM Only:**

- Accurate execution budget (memory + CPU steps)
- UPLC-specific behavior (e.g., integer overflow semantics)
- Error traces matching on-chain behavior

**Emulator Only:**

- UTxO set state management
- Full Cardano ledger rules (phase 1 + phase 2)
- Multi-transaction protocol testing
- Time/slot progression
- No network latency

**Yaci DevKit Only:**

- Block production
- Transaction finality
- Real Cardano node behavior
- Concurrent transaction handling
- Network serialization

---

## Part 2: Test Categories - Required vs Optional

### REQUIRED Tests (Must Pass Before Deployment)

| Test Type                         | Platform       | What It Verifies                                |
|-----------------------------------|----------------|-------------------------------------------------|
| **Logic correctness**             | JVM + PlutusVM | Validator accepts valid inputs, rejects invalid |
| **Budget within limits**          | PlutusVM       | Script doesn't exceed protocol limits           |
| **Transaction building**          | Emulator       | Full tx validates, correct fees, balanced       |
| **Security: unauthorized access** | Emulator       | Only authorized parties can spend               |
| **Security: time manipulation**   | Emulator       | Time-locked contracts enforce bounds            |

### OPTIONAL Tests (Recommended but Not Blocking)

| Test Type                         | Platform               | When Needed                    |
|-----------------------------------|------------------------|--------------------------------|
| **Property tests**                | JVM (sampled PlutusVM) | Complex logic, many edge cases |
| **Budget regression**             | PlutusVM               | Optimizing contract size       |
| **Protocol flow**                 | Emulator               | Multi-step protocols           |
| **Integration**                   | Yaci DevKit            | Before mainnet deployment      |
| **Security: double satisfaction** | Emulator               | Multi-script protocols only    |
| **Security: datum fuzzing**       | JVM                    | User-provided data contracts   |

### REDUNDANT Tests (Don't Do Both)

| Redundant Pair                  | Keep     | Drop                 | Why                                  |
|---------------------------------|----------|----------------------|--------------------------------------|
| JVM-only logic + PlutusVM logic | Both     | Neither              | Different purposes (debug vs budget) |
| Emulator success + Yaci success | Emulator | Yaci (for same case) | Same ledger rules, Emulator faster   |
| Property test on PlutusVM       | Sampled  | Full                 | 1000× PlutusVM is too slow           |

---

## Part 3: Platform Selection Matrix

### Which Platform for Which Test?

```
┌─────────────────────────────────────────────────────────────────┐
│                     TEST TYPE → PLATFORM                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  "Does my logic work?"                                          │
│      → JVM (fast, debuggable)                                   │
│      → PlutusVM (verify same result)                            │
│                                                                 │
│  "What's my budget?"                                            │
│      → PlutusVM only (JVM doesn't track budget)                 │
│                                                                 │
│  "Does my transaction validate?"                                │
│      → Emulator (full ledger rules, fast)                       │
│                                                                 │
│  "Does my protocol work end-to-end?"                            │
│      → Emulator (multi-tx, state tracking)                      │
│      → Yaci DevKit (final verification before mainnet)          │
│                                                                 │
│  "Is my contract secure?"                                       │
│      → Emulator (construct attack transactions)                 │
│                                                                 │
│  "Will it work on mainnet?"                                     │
│      → Yaci DevKit (real node, block production)                │
│      → Testnet (production-like, before mainnet)                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Part 4: Multi-Platform Test Execution

### Tests That SHOULD Run on Multiple Platforms

| Test                | JVM         | PlutusVM  | Emulator | Yaci    |
|---------------------|-------------|-----------|----------|---------|
| **Validator logic** | ✓ Debug     | ✓ Verify  | -        | -       |
| **Happy path tx**   | -           | -         | ✓ Fast   | ✓ Final |
| **Error cases**     | ✓ Fast      | ✓ Logs    | ✓ Ledger | -       |
| **Property tests**  | ✓ All cases | ✓ Sampled | -        | -       |
| **Protocol flow**   | -           | -         | ✓ Dev    | ✓ Final |

### How to Structure Multi-Platform Tests

```scala
// PATTERN 1: Validator logic - JVM + PlutusVM
// Tests the same logic on both platforms, compares results

class HtlcValidatorTest extends ValidatorTestSuite {
  // Automatically runs on JVM and PlutusVM
  test("valid preimage reveals") {
    validator.test(validPreimage, ctx).assertSuccess()
    // JVM: runs compiled.code(data)
    // PlutusVM: runs compiled.program
    // Asserts: same success/failure outcome
  }
}

// PATTERN 2: Transaction flow - Emulator first, Yaci for final
// Same test code, different Provider

trait HtlcProtocolTest {
  def provider: Provider // Abstract

  test("lock → reveal flow") {
    val lockedUtxo = lock(provider, alice, bob, amount)
    val revealTx = reveal(provider, bob, preimage, lockedUtxo)
    assertSuccess(provider, revealTx)
  }
}

class HtlcEmulatorTest extends HtlcProtocolTest {
  val provider = Emulator.withAddresses(...) // Fast
}

class HtlcYaciTest extends HtlcProtocolTest with YaciDevKit {
  val provider = createTestContext().provider // Real network
}
```

---

## Part 5: Testing Strategy for a Single Contract

### Example: HTLC Contract

```
HTLC Contract Test Strategy
===========================

PHASE 1: Validator Logic (JVM + PlutusVM)
─────────────────────────────────────────
Required:
  ✓ Reveal with valid preimage succeeds
  ✓ Reveal with wrong preimage fails
  ✓ Timeout after deadline succeeds
  ✓ Timeout before deadline fails
  ✓ Wrong receiver signature fails
  ✓ Wrong committer signature fails

Property tests (JVM, sampled on PlutusVM):
  ✓ Any valid preimage + correct context → success
  ✓ Any invalid preimage → failure
  ✓ Random data doesn't crash (robustness)

PHASE 2: Budget Verification (PlutusVM only)
────────────────────────────────────────────
Required:
  ✓ Reveal budget < protocol max
  ✓ Timeout budget < protocol max

Optional:
  ○ Budget equals expected (regression)
  ○ Budget comparison across backends

PHASE 3: Transaction Validation (Emulator)
──────────────────────────────────────────
Required:
  ✓ Lock tx creates correct UTxO
  ✓ Reveal tx spends correctly
  ✓ Timeout tx spends correctly
  ✓ Fees are reasonable

Security (Emulator):
  ✓ Eve cannot reveal with valid preimage (wrong receiver)
  ✓ Eve cannot timeout before deadline
  ✓ Alice cannot reveal without preimage
  ✓ Bob cannot timeout (wrong committer)

PHASE 4: Integration (Yaci DevKit)
──────────────────────────────────
Optional (before mainnet):
  ○ Full lock → reveal flow
  ○ Full lock → timeout flow
  ○ Concurrent transactions
```

---

## Part 6: Testing Strategy for a Protocol

### Example: DEX Protocol (Multiple Validators)

```
DEX Protocol Test Strategy
==========================

Validators:
  - PoolValidator (liquidity pool)
  - SwapValidator (token swaps)
  - LPTokenPolicy (minting policy)

PHASE 1: Individual Validator Logic (JVM + PlutusVM)
────────────────────────────────────────────────────
For each validator:
  ✓ All valid operations succeed
  ✓ All invalid operations fail
  ✓ Property tests for edge cases

PHASE 2: Cross-Validator Interaction (Emulator)
───────────────────────────────────────────────
Required:
  ✓ Swap correctly updates pool state
  ✓ Add liquidity mints correct LP tokens
  ✓ Remove liquidity burns LP tokens correctly
  ✓ Pool invariants maintained (k = x * y)

Security:
  ✓ Double satisfaction: swap can't satisfy pool without payment
  ✓ LP token minting requires pool validator
  ✓ Price manipulation attacks fail
  ✓ Sandwich attack mitigation works

PHASE 3: Protocol State Machine (Emulator)
──────────────────────────────────────────
Required:
  ✓ Create pool → add liquidity → swap → remove liquidity
  ✓ Multiple swaps maintain invariants
  ✓ Concurrent users don't corrupt state

Optional:
  ○ Stress test with many transactions
  ○ Fee accumulation over time

PHASE 4: Integration (Yaci DevKit)
──────────────────────────────────
Required (before mainnet):
  ✓ Full protocol flow with real blocks
  ✓ Multiple users interacting
  ✓ Transaction ordering behavior

PHASE 5: Economic Testing (Simulation)
─────────────────────────────────────
Optional:
  ○ Arbitrage scenarios
  ○ Liquidity provider returns
  ○ Fee optimization
```

---

## Part 7: Test Pyramid for Smart Contracts

```
                    ┌───────────┐
                    │  Testnet  │  ← Final verification
                    │  (rare)   │     before mainnet
                    └─────┬─────┘
                          │
                  ┌───────┴───────┐
                  │  Yaci DevKit  │  ← Integration tests
                  │  (few tests)  │     real network behavior
                  └───────┬───────┘
                          │
          ┌───────────────┴───────────────┐
          │           Emulator            │  ← Protocol tests
          │    (many transaction tests)   │     security tests
          └───────────────┬───────────────┘
                          │
  ┌───────────────────────┴───────────────────────┐
  │              JVM + PlutusVM                   │  ← Unit tests
  │  (many validator tests, property tests)      │     logic + budget
  └───────────────────────────────────────────────┘
```

**Test Count Distribution:**

- JVM/PlutusVM: ~70% of tests (fast, thorough)
- Emulator: ~25% of tests (transactions, security)
- Yaci DevKit: ~5% of tests (integration, final check)
- Testnet: Manual verification before mainnet

---

## Part 8: ScriptContext for Unit Tests

### The Question: Where Does ScriptContext Come From?

Two approaches for unit testing validators:

### Approach A: Construct Manually

Build ScriptContext by hand with mock data.

```scala
test("reveal with valid preimage succeeds") {
  val ctx = ScriptContext(
    txInfo = TxInfo(
      inputs = List(mockInput),
      outputs = List(mockOutput),
      signatories = List(Bob.addrKeyHash),
      validRange = POSIXTimeRange.before(timeout),
      // ... all other fields
    ),
    redeemer = Action.Reveal(preimage).toData,
    scriptInfo = SpendingScript(txOutRef, Some(datum))
  )
  validator.test(ctx.toData).assertSuccess()
}
```

**Pros:**

- Fast - no transaction building overhead
- Precise control - test exact edge cases
- Isolated - tests only validator logic
- Simple dependency - no TxBuilder needed

**Cons:**

- Verbose - many fields to set manually
- Error-prone - easy to create invalid contexts
- Maintenance - must update when ScriptContext changes
- Unrealistic - may test contexts that can't occur in practice

### Approach B: Derive from Transaction

Build a real transaction, then extract ScriptContext.

```scala
test("reveal with valid preimage succeeds") {
  val tx = txCreator.reveal(
    utxos = utxos,
    lockedUtxo = lockedUtxo,
    preimage = validPreimage,
    // ...
  )

  // Extract context from built transaction
  val ctx = tx.getScriptContextV3(utxos, SpendingScript(txOutRef))

  validator.test(ctx.toData).assertSuccess()
}
```

**Pros:**

- Realistic - context matches what on-chain would see
- Less error-prone - TxBuilder validates structure
- Automatic - fields filled correctly by builder
- Reusable - same tx builder for Emulator tests

**Cons:**

- Slower - transaction building overhead
- More setup - needs UTxOs, TxBuilder
- Less precise - harder to test specific edge cases
- Dependency - requires cardano-ledger module

### Comparison

| Aspect                | Manual             | From Transaction |
|-----------------------|--------------------|------------------|
| **Speed**             | Very fast          | Medium           |
| **Setup complexity**  | Low                | Medium           |
| **Error risk**        | High (invalid ctx) | Low              |
| **Edge case testing** | Easy               | Harder           |
| **Realism**           | Low                | High             |
| **Maintenance**       | High               | Low              |

### **RECOMMENDATION: Hybrid Approach**

Use **both** depending on test type:

```scala
// For logic edge cases: Manual context (fast, precise)
class HtlcLogicTest extends ValidatorTestSuite {
  test("empty preimage fails") {
    val ctx = makeMinimalContext(
      redeemer = Action.Reveal(ByteString.empty),
      signatories = List(Bob.addrKeyHash)
    )
    validator.test(ctx.toData).assertFailureWith("InvalidPreimage")
  }
}

// For realistic scenarios: Derive from transaction
class HtlcTransactionTest extends ScalusTest {
  test("reveal with valid preimage succeeds") {
    val tx = txCreator.reveal(validPreimage, ...)
    val ctx = tx.scriptContextsV3.head
    validator.test(ctx.toData).assertSuccess()

    // Then also verify full transaction
    assertTxSuccess(provider, tx)
  }
}
```

### Helper Utilities Needed

```scala
// ScalusTest should provide:
trait ScalusTest {
  // Minimal context builders for edge case testing
  def makeSpendingContext(
                           datum: Data,
                           redeemer: Data,
                           signatories: List[PubKeyHash] = Nil,
                           validRange: POSIXTimeRange = POSIXTimeRange.always,
                           inputs: List[TxInInfo] = Nil,
                           outputs: List[TxOut] = Nil
                         ): ScriptContext

  // For minting policy tests
  def makeMintingContext(
                          redeemer: Data,
                          mint: Value,
                          signatories: List[PubKeyHash] = Nil
                        ): ScriptContext
}

// TestUtil already provides (extend):
extension (tx: Transaction)
  def scriptContextsV3(utxos: Utxos): Map[Redeemer, ScriptContext]
  def getScriptContextV3(utxos: Utxos, purpose: RedeemerPurpose): ScriptContext
```

### When to Use Each

| Test Type                  | Approach | Why                          |
|----------------------------|----------|------------------------------|
| **Logic edge cases**       | Manual   | Precise control over context |
| **Valid input variations** | From Tx  | Realistic, less error-prone  |
| **Property tests**         | Manual   | Fast, many iterations        |
| **Transaction tests**      | From Tx  | Already building tx anyway   |
| **Security tests**         | From Tx  | Must be realistic            |

---

## Part 8b: Testing Minting Policies

### Key Differences from Spending Validators

| Aspect                 | Spending Validator                | Minting Policy                  |
|------------------------|-----------------------------------|---------------------------------|
| **Datum**              | Has datum                         | No datum                        |
| **ScriptInfo**         | `SpendingScript(txOutRef, datum)` | `MintingScript(currencySymbol)` |
| **What to check**      | UTxO being spent                  | `mint` field in TxInfo          |
| **Address derivation** | Script hash → address             | Script hash → CurrencySymbol    |

### Minting Policy Structure

```scala
@Compile
object LPTokenPolicy {
  inline def validate(ctx: Data): Unit = {
    val scriptCtx = ctx.to[ScriptContext]
    scriptCtx.scriptInfo match
      case ScriptInfo.MintingScript(currencySymbol) =>
        val txInfo = scriptCtx.txInfo
        val mintedAmount = txInfo.mint.get(currencySymbol)
        // Validate minting conditions
        require(mintedAmount > 0, "Must mint positive amount")
        require(txInfo.signatories.contains(adminPkh), "Admin must sign")
      case _ => fail("Must be minting")
  }
}
```

### Unit Testing Minting Policies

**Approach A: Manual Context**

```scala
test("minting with admin signature succeeds") {
  val currencySymbol = LPTokenPolicy.compiled.script.hash

  val ctx = makeMintingContext(
    currencySymbol = currencySymbol,
    redeemer = MintAction.Mint.toData,
    mint = Value.fromAssets(currencySymbol, "LP", 1000),
    signatories = List(adminPkh)
  )

  policy.test(ctx.toData).assertSuccess()
}

test("minting without admin signature fails") {
  val ctx = makeMintingContext(
    currencySymbol = currencySymbol,
    redeemer = MintAction.Mint.toData,
    mint = Value.fromAssets(currencySymbol, "LP", 1000),
    signatories = List.empty // No admin!
  )

  policy.test(ctx.toData).assertFailureWith("Admin must sign")
}
```

**Approach B: From Transaction**

```scala
test("mint LP tokens in real transaction") {
  val mintTx = TxBuilder(env)
    .mintAsset(policy, "LP", 1000, MintAction.Mint.toData)
    .payTo(recipientAddress, Value.fromAssets(currencySymbol, "LP", 1000))
    .sign(adminSigner)
    .complete(provider, sponsorAddress)
    .await()
    .transaction

  // Extract minting context
  val ctx = mintTx.getScriptContextV3(utxos, MintingScript(currencySymbol))
  policy.test(ctx.toData).assertSuccess()

  // Also verify full transaction
  assertTxSuccess(provider, mintTx)
}
```

### Helper Utility for Minting Contexts

```scala
trait ScalusTest {
  def makeMintingContext(
                          currencySymbol: CurrencySymbol,
                          redeemer: Data,
                          mint: Value,
                          signatories: List[PubKeyHash] = Nil,
                          validRange: POSIXTimeRange = POSIXTimeRange.always,
                          inputs: List[TxInInfo] = Nil, // For checking pool UTxO
                          outputs: List[TxOut] = Nil // For checking minted tokens destination
                        ): ScriptContext = {
    ScriptContext(
      txInfo = TxInfo(
        inputs = inputs,
        referenceInputs = Nil,
        outputs = outputs,
        fee = 0,
        mint = mint,
        certificates = Nil,
        withdrawals = AssocMap.empty,
        validRange = validRange,
        signatories = signatories,
        redeemers = AssocMap.empty,
        data = AssocMap.empty,
        id = TxId(ByteString.fill(32)(0))
      ),
      redeemer = redeemer,
      scriptInfo = ScriptInfo.MintingScript(currencySymbol)
    )
  }
}
```

### Common Minting Policy Test Cases

```
Minting Policy Test Checklist
=============================

Authorization:
  ✓ Authorized party can mint
  ✓ Unauthorized party cannot mint
  ✓ Multi-sig requirements enforced (if applicable)

Amount validation:
  ✓ Positive amounts allowed
  ✓ Zero amount rejected (usually)
  ✓ Negative amounts (burning) - depends on policy

Token name validation:
  ✓ Valid token names accepted
  ✓ Invalid token names rejected (if applicable)

Context validation:
  ✓ Correct currency symbol checked
  ✓ Required inputs present (e.g., pool UTxO for LP tokens)
  ✓ Required outputs present (e.g., tokens go to correct address)

Burning:
  ✓ Authorized burning succeeds
  ✓ Unauthorized burning fails
  ✓ Burn amount matches requirements
```

### Minting Policy + Validator Interaction

For protocols where minting is coupled with a validator (e.g., LP tokens):

```scala
test("LP tokens can only be minted when adding liquidity") {
  // Must spend pool UTxO AND mint LP tokens in same transaction
  val tx = TxBuilder(env)
    .spendScript(poolUtxo, AddLiquidity(amount).toData, poolValidator)
    .mintAsset(lpPolicy, "LP", lpAmount, MintAction.Mint.toData)
    .payToScript(poolAddress, newPoolDatum, poolValue + amount)
    .payTo(userAddress, Value.fromAssets(lpCurrencySymbol, "LP", lpAmount))
    .complete(provider, userAddress)
    .await()
    .transaction

  assertTxSuccess(provider, tx)
}

test("SECURITY: cannot mint LP tokens without pool interaction") {
  assertTxFail("Pool UTxO not spent") {
    TxBuilder(env)
      .mintAsset(lpPolicy, "LP", 1000, MintAction.Mint.toData)
      // Missing: .spendScript(poolUtxo, ...)
      .complete(provider, userAddress)
      .await()
      .transaction
  }
}
```

---

## Part 8c: Testing Stake Validators

### Stake Validator Purposes

Stake validators handle three distinct purposes:

| Purpose        | ScriptInfo                      | When Triggered                           |
|----------------|---------------------------------|------------------------------------------|
| **Rewarding**  | `RewardingScript(credential)`   | Withdrawing staking rewards              |
| **Certifying** | `CertifyingScript(index, cert)` | Registration, delegation, deregistration |
| **Voting**     | `VotingScript(voter)`           | Governance voting (Conway era)           |

### Stake Validator Structure

```scala
@Compile
object StakeValidator {
  inline def validate(ctx: Data): Unit = {
    val scriptCtx = ctx.to[ScriptContext]
    scriptCtx.scriptInfo match
      case ScriptInfo.RewardingScript(credential) =>
        // Validate reward withdrawal
        val withdrawnAmount = scriptCtx.txInfo.withdrawals.get(credential)
        require(withdrawnAmount > 0, "Must withdraw positive")
        require(scriptCtx.txInfo.signatories.contains(adminPkh), "Admin must sign")

      case ScriptInfo.CertifyingScript(index, cert) =>
        cert match
          case StakingCredential.RegDRepCert(_, _) =>
          // Validate DRep registration
          case StakingCredential.UnRegDRepCert(_, _) =>
          // Validate DRep deregistration
          case _ => fail("Unsupported certificate")

      case ScriptInfo.VotingScript(voter) =>
        // Validate governance vote
        require(scriptCtx.txInfo.signatories.contains(voterPkh), "Voter must sign")

      case _ => fail("Invalid script purpose")
  }
}
```

### Testing Reward Withdrawals

```scala
test("authorized withdrawal succeeds") {
  val credential = StakingCredential.ScriptCredential(stakeValidator.script.hash)

  val ctx = makeRewardingContext(
    credential = credential,
    redeemer = WithdrawAction.Withdraw.toData,
    withdrawals = Map(credential -> 1000000), // 1 ADA
    signatories = List(adminPkh)
  )

  stakeValidator.test(ctx.toData).assertSuccess()
}

test("unauthorized withdrawal fails") {
  val ctx = makeRewardingContext(
    credential = credential,
    redeemer = WithdrawAction.Withdraw.toData,
    withdrawals = Map(credential -> 1000000),
    signatories = List.empty // No admin!
  )

  stakeValidator.test(ctx.toData).assertFailureWith("Admin must sign")
}
```

### Testing Certificates

```scala
test("DRep registration succeeds") {
  val cert = StakingCredential.RegDRepCert(drepCredential, deposit)

  val ctx = makeCertifyingContext(
    certIndex = 0,
    certificate = cert,
    redeemer = CertAction.Register.toData,
    signatories = List(drepPkh)
  )

  stakeValidator.test(ctx.toData).assertSuccess()
}
```

### Testing with Emulator

```scala
test("withdraw rewards via Emulator") {
  val withdrawTx = TxBuilder(env)
    .withdrawRewards(stakeAddress, rewardAmount, WithdrawAction.Withdraw.toData, stakeValidator)
    .sign(adminSigner)
    .complete(provider, sponsorAddress)
    .await()
    .transaction

  assertTxSuccess(provider, withdrawTx)
}
```

### Stake Validator Test Checklist

```
Stake Validator Test Checklist
==============================

Reward Withdrawals:
  ✓ Authorized withdrawal succeeds
  ✓ Unauthorized withdrawal fails
  ✓ Partial withdrawal allowed (if applicable)
  ✓ Zero withdrawal rejected

Certificates:
  ✓ Registration with correct deposit succeeds
  ✓ Delegation to valid pool succeeds
  ✓ Deregistration returns deposit
  ✓ Invalid certificate type rejected

Governance (Conway):
  ✓ Authorized vote succeeds
  ✓ Unauthorized vote fails
  ✓ Vote within valid governance action
```

---

## Part 8d: Error Message Matching

### Why Error Messages Matter

1. **Debugging** - Know exactly why validation failed
2. **User feedback** - Show meaningful errors to users
3. **Test precision** - Verify correct failure path, not just "failed"

### Error Trace Compilation

Scalus compiles error messages into UPLC traces:

```scala
// In validator code
require(preimageHash == expectedHash, "InvalidPreimage")
// Compiles to: if !(preimageHash == expectedHash) then error("InvalidPreimage")

// Or using fail()
if !condition then fail("InvalidPreimage")
```

### Enabling Error Traces

```scala
// Development: include error traces (larger script)
val devContract = HtlcContract.compiled.withErrorTraces

// Production: minimal script (no traces)
val prodContract = HtlcContract.compiled
```

### Assertion Patterns

**Pattern 1: Exact Match**

```scala
test("wrong preimage fails with InvalidPreimage") {
  assertTxFail("InvalidPreimage") {
    txCreator.reveal(wrongPreimage, ...)
  }
}
```

**Pattern 2: Contains Match**

```scala
test("timeout before deadline fails") {
  assertTxFail(error => error.contains("TimePoint")) {
    txCreator.timeout(beforeDeadline, ...)
  }
}
```

**Pattern 3: Regex Match**

```scala
test("signature check fails") {
  assertTxFailMatching("""Unsigned.*Transaction""".r) {
    txCreator.reveal(validPreimage, wrongSigner)
  }
}
```

**Pattern 4: Multiple Possible Errors**

```scala
test("invalid input fails") {
  assertTxFailOneOf("InvalidPreimage", "ExpiredTimeout", "MissingSignature") {
    txCreator.reveal(invalidInput, ...)
  }
}
```

### Implementation in ScalusTest

```scala
trait ScalusTest {
  // Exact match
  def assertTxFail(expectedError: String)(buildTx: => Transaction): Unit

  // Predicate match
  def assertTxFail(predicate: String => Boolean)(buildTx: => Transaction): Unit

  // Regex match
  def assertTxFailMatching(pattern: Regex)(buildTx: => Transaction): Unit

  // One of multiple errors
  def assertTxFailOneOf(errors: String*)(buildTx: => Transaction): Unit

  // For unit tests (not transaction)
  def assertFailureWith(expectedError: String, result: VmResult): Unit

  def assertFailureMatching(predicate: String => Boolean, result: VmResult): Unit
}
```

### Error Message Best Practices

```scala
// GOOD: Specific, actionable error names
object HtlcValidator {
  val InvalidReceiverPreimage = "InvalidReceiverPreimage"
  val InvalidReceiverTimePoint = "InvalidReceiverTimePoint"
  val UnsignedReceiverTransaction = "UnsignedReceiverTransaction"
  val InvalidCommitterTimePoint = "InvalidCommitterTimePoint"
  val UnsignedCommitterTransaction = "UnsignedCommitterTransaction"
  val MustBeSpending = "MustBeSpending"
}

// BAD: Generic errors
require(condition, "Error") // Which error?
require(condition, "Failed") // Why failed?
```

### Error Categories

```scala
// Suggested naming convention
object ErrorMessages {
  // Format: {Action}{Reason}

  // Authorization errors
  val UnsignedTransaction = "UnsignedTransaction"
  val UnauthorizedSigner = "UnauthorizedSigner"
  val MissingMultiSig = "MissingMultiSig"

  // Validation errors
  val InvalidDatum = "InvalidDatum"
  val InvalidRedeemer = "InvalidRedeemer"
  val InvalidPreimage = "InvalidPreimage"

  // Time errors
  val ExpiredDeadline = "ExpiredDeadline"
  val NotYetValid = "NotYetValid"
  val InvalidTimeRange = "InvalidTimeRange"

  // Amount errors
  val InsufficientFunds = "InsufficientFunds"
  val InvalidAmount = "InvalidAmount"
  val ZeroAmount = "ZeroAmount"

  // Script purpose errors
  val MustBeSpending = "MustBeSpending"
  val MustBeMinting = "MustBeMinting"
  val MustBeRewarding = "MustBeRewarding"
}
```

---

## Part 8e: Test Organization

### Directory Structure

```
scalus-examples/
├── shared/src/main/scala/scalus/examples/
│   └── htlc/
│       ├── HtlcValidator.scala      # Validator logic
│       ├── HtlcContract.scala       # Compilation wrapper
│       ├── HtlcTransactions.scala   # Transaction builders
│       └── HtlcTypes.scala          # Datum, Redeemer types
│
└── jvm/src/test/scala/scalus/examples/
    └── htlc/
        ├── HtlcValidatorTest.scala  # Unit tests (JVM + PlutusVM)
        ├── HtlcBudgetTest.scala     # Budget verification
        ├── HtlcEmulatorTest.scala   # Transaction tests
        ├── HtlcSecurityTest.scala   # Security tests
        └── HtlcIntegrationTest.scala # Yaci DevKit tests
```

### Test File Naming Convention

| File Name                | Purpose                    | Platform               |
|--------------------------|----------------------------|------------------------|
| `*ValidatorTest.scala`   | Validator logic unit tests | JVM + PlutusVM         |
| `*BudgetTest.scala`      | Budget verification        | PlutusVM               |
| `*EmulatorTest.scala`    | Transaction validation     | Emulator               |
| `*SecurityTest.scala`    | Attack scenario tests      | Emulator               |
| `*IntegrationTest.scala` | End-to-end tests           | Yaci DevKit            |
| `*PropertyTest.scala`    | Property-based tests       | JVM (sampled PlutusVM) |

### Test Class Organization

```scala
// Pattern 1: Single file with nested describes
class HtlcTest extends AnyFunSuite, ScalusTest {

  describe("Validator Logic") {
    test("reveal with valid preimage succeeds") {
    ...
    }
    test("reveal with wrong preimage fails") {
    ...
    }
  }

  describe("Transaction Validation") {
    test("lock creates correct UTxO") {
    ...
    }
    test("reveal spends correctly") {
    ...
    }
  }

  describe("Security") {
    test("Eve cannot steal funds") {
    ...
    }
    test("time manipulation fails") {
    ...
    }
  }
}

// Pattern 2: Separate files per concern
class HtlcValidatorTest extends ValidatorTestSuite {
...
}

class HtlcEmulatorTest extends EmulatorTestSuite {
...
}

class HtlcSecurityTest extends SecurityTestSuite {
...
}
```

### Test Naming Convention

```scala
// Format: "{subject} {condition} {expected outcome}"

// Good examples
test("reveal with valid preimage succeeds")
test("reveal with wrong preimage fails with InvalidPreimage")
test("timeout after deadline succeeds")
test("Eve cannot reveal with valid preimage")

// Bad examples
test("test1")
test("reveal test")
test("should work")
```

### Test Traits Composition

```scala
// Base traits provide reusable functionality
trait HtlcTestSetup {
  given env: CardanoInfo = TestUtil.testEnvironment

  val contract = HtlcContract.compiled.withErrorTraces
  val txCreator = HtlcTransactions(env, evaluator, contract)

  val validPreimage = genByteStringOfN(32).sample.get
  val wrongPreimage = genByteStringOfN(12).sample.get
  val timeout = Instant.now().plusSeconds(3600)
}

// Compose for different test types
class HtlcValidatorTest
  extends AnyFunSuite
    with ScalusTest
    with HtlcTestSetup {
  // Unit tests
}

class HtlcEmulatorTest
  extends AnyFunSuite
    with ScalusTest
    with HtlcTestSetup {
  // Transaction tests
}

class HtlcSecurityTest
  extends AnyFunSuite
    with SecurityTestKit
    with HtlcTestSetup {
  // Security tests
}
```

### Test Tags for Filtering

```scala
import org.scalatest.Tag

object Security extends Tag("Security")

object Budget extends Tag("Budget")

object Integration extends Tag("Integration")

object Slow extends Tag("Slow")

class HtlcTest extends AnyFunSuite {
  test("Eve cannot steal funds", Security) {
  ...
  }
  test("reveal budget within limits", Budget) {
  ...
  }
  test("full flow on Yaci", Integration, Slow) {
  ...
  }
}

// Run specific tags:
// sbtn "testOnly * -- -n Security"
// sbtn "testOnly * -- -l Slow"  // exclude slow
```

### Shared Test Data

```scala
// In test resources or companion object
object HtlcTestData {
  val scenarios = Table(
    ("name", "preimage", "expectedResult"),
    ("valid 32-byte", validPreimage, Success),
    ("empty", ByteString.empty, Failure("InvalidPreimage")),
    ("too short", shortPreimage, Failure("InvalidPreimage")),
    ("wrong hash", wrongPreimage, Failure("InvalidPreimage"))
  )
}

// Use in property-like tests
class HtlcValidatorTest extends AnyFunSuite with TableDrivenPropertyChecks {
  forAll(HtlcTestData.scenarios) { (name, preimage, expected) =>
    test(s"reveal with $name preimage") {
      val result = validator.reveal(preimage, ctx)
      expected match
        case Success => result.assertSuccess()
        case Failure(msg) => result.assertFailureWith(msg)
    }
  }
}
```

---

## Part 8f: Property Testing Details

### ScalaCheck Integration

Scalus provides `ArbitraryInstances` for common Plutus types:

```scala
import scalus.testing.kit.ArbitraryInstances

trait ArbitraryInstances {
  // ByteString generators
  given Arbitrary[ByteString]

  def genByteStringOfN(n: Int): Gen[ByteString]

  // Plutus types
  given Arbitrary[PubKeyHash]

  given Arbitrary[ValidatorHash]

  given Arbitrary[CurrencySymbol]

  given Arbitrary[TokenName]

  given Arbitrary[Value]

  given Arbitrary[POSIXTime]

  // Cardano types
  given Arbitrary[TransactionInput]

  given Arbitrary[TransactionOutput]
}
```

### Custom Generators for Your Contract

```scala
object HtlcGenerators {
  // Generate valid Config datums
  val genConfig: Gen[Config] = for {
    committer <- arbitrary[PubKeyHash]
    receiver <- arbitrary[PubKeyHash]
    image <- genByteStringOfN(32) // SHA3-256 hash
    timeout <- Gen.posNum[Long].map(POSIXTime(_))
  } yield Config(committer, receiver, image, timeout)

  // Generate valid preimages (32 bytes for SHA3-256)
  val genValidPreimage: Gen[ByteString] = genByteStringOfN(32)

  // Generate invalid preimages
  val genInvalidPreimage: Gen[ByteString] = Gen.oneOf(
    Gen.const(ByteString.empty), // Empty
    genByteStringOfN(16), // Too short
    genByteStringOfN(64), // Too long
    Gen.alphaNumStr.map(_.getBytes.toByteString) // Random
  )

  // Generate actions
  val genAction: Gen[Action] = Gen.oneOf(
    genValidPreimage.map(Action.Reveal(_)),
    Gen.const(Action.Timeout)
  )
}
```

### Property Test Patterns

**Pattern 1: Invariant Properties**

```scala
property("valid preimage always reveals before timeout") {
  forAll(genConfig, genValidPreimage) { (config, preimage) =>
    val image = sha3_256(preimage)
    val configWithImage = config.copy(image = image)
    val ctx = makeContext(configWithImage, Action.Reveal(preimage), beforeTimeout)

    validator.test(ctx.toData).assertSuccess()
  }
}
```

**Pattern 2: Negative Properties**

```scala
property("invalid preimage always fails") {
  forAll(genConfig, genInvalidPreimage) { (config, invalidPreimage) =>
    whenever(sha3_256(invalidPreimage) != config.image) {
      val ctx = makeContext(config, Action.Reveal(invalidPreimage), beforeTimeout)

      validator.test(ctx.toData).assertFailureWith("InvalidPreimage")
    }
  }
}
```

**Pattern 3: Round-Trip Properties**

```scala
property("datum serialization round-trips") {
  forAll(genConfig) { config =>
    val data = config.toData
    val decoded = data.to[Config]
    decoded shouldBe config
  }
}
```

### Shrinking for Better Failure Reports

```scala
// Custom shrinker for ByteString
implicit val shrinkByteString: Shrink[ByteString] = Shrink { bs =>
  // Shrink by removing bytes from the end
  (1 until bs.size).map(n => bs.take(n)).toStream
}

// Custom shrinker for Config
implicit val shrinkConfig: Shrink[Config] = Shrink { config =>
  for {
    committer <- shrink(config.committer)
    receiver <- shrink(config.receiver)
    image <- shrink(config.image)
    timeout <- shrink(config.timeout)
  } yield Config(committer, receiver, image, timeout)
}
```

### Sampling Strategies for PlutusVM

```scala
// Strategy 1: Fixed sampling rate
property("valid preimage reveals") {
  forAll(genPreimage) { preimage =>
    validator.test(preimage, ctx)
      .withPlutusVMSampling(rate = 0.1) // 10% on PlutusVM
      .assertSuccess()
  }
}

// Strategy 2: First N on PlutusVM
property("valid preimage reveals") {
  forAll(genPreimage) { preimage =>
    validator.test(preimage, ctx)
      .withPlutusVMFirst(n = 10) // First 10 on PlutusVM
      .assertSuccess()
  }
}

// Strategy 3: Environment-controlled
// SCALUS_PLUTUSVM_SAMPLE_RATE=0.1 (default)
// SCALUS_PLUTUSVM_SAMPLE_RATE=1.0 (CI full verification)
property("valid preimage reveals") {
  forAll(genPreimage) { preimage =>
    validator.test(preimage, ctx)
      .assertSuccess() // Uses env var
  }
}
```

### Property Test Configuration

```scala
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait PropertyTestConfig extends ScalaCheckPropertyChecks {
  // Reduce iterations for faster local testing
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = 100, // Default: 100
      maxDiscardedFactor = 5.0, // Allow 5x discards
      minSize = 0,
      sizeRange = 100
    )
}

// For CI: more iterations
trait CIPropertyTestConfig extends PropertyTestConfig {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = 1000, // 10x more
      maxDiscardedFactor = 10.0
    )
}
```

---

## Part 8g: CI/CD Integration

### Test Suite Categories

```yaml
# .github/workflows/test.yml
jobs:
  fast-tests:
    name: Fast Tests (JVM + PlutusVM)
    steps:
      - run: sbtn "testOnly * -- -l Integration -l Slow"
    # Runs: Validator tests, property tests, emulator tests
    # Excludes: Integration (Yaci), Slow tests

  integration-tests:
    name: Integration Tests (Yaci DevKit)
    needs: fast-tests
    steps:
      - run: sbtn "testOnly * -- -n Integration"
    # Runs only tests tagged with Integration

  full-verification:
    name: Full PlutusVM Verification
    needs: fast-tests
    env:
      SCALUS_PLUTUSVM_SAMPLE_RATE: "1.0"  # 100% on PlutusVM
    steps:
      - run: sbtn "testOnly *PropertyTest"
```

### Parallel Test Execution

```scala
// build.sbt
Test / testOptions += Tests.Argument(
  "-oD", // Show durations
  "-u", "target/test-reports" // JUnit XML reports
)

// Parallel execution by test class
Test / parallelExecution := true
Test / testForkedParallel := true
```

### Test Reports and Artifacts

```yaml
# Upload test reports
- uses: actions/upload-artifact@v3
  if: always()
  with:
    name: test-reports
    path: |
      **/target/test-reports/*.xml
      **/target/test-reports/*.html

# Budget tracking artifact
- uses: actions/upload-artifact@v3
  with:
    name: budget-reports
    path: target/budget-*.json
```

### Budget Regression Detection

```scala
// In BudgetTest
test("reveal budget") {
  val result = validator.test(preimage, ctx).assertSuccess()

  // Record budget for CI tracking
  BudgetTracker.record("htlc-reveal", result.budget)

  // Assert within known limits
  result.assertBudgetWithin(memory = 100000, steps = 50000000)
}

// BudgetTracker writes JSON
// target/budget-htlc.json
// { "htlc-reveal": { "memory": 85432, "steps": 42567890 } }
```

### CI Pipeline Stages

```
┌─────────────────────────────────────────────────────────────┐
│                      CI Pipeline                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Stage 1: Compile & Format (2 min)                          │
│    sbtn "clean; scalafmtCheck; compile; Test/compile"       │
│                                                             │
│  Stage 2: Fast Tests (5 min)                                │
│    sbtn "testOnly * -- -l Integration -l Slow"              │
│    - Validator unit tests                                   │
│    - Property tests (sampled)                               │
│    - Emulator tests                                         │
│                                                             │
│  Stage 3: Integration Tests (10 min) [parallel]             │
│    sbtn scalusCardanoLedgerIt/test                          │
│    - Yaci DevKit tests                                      │
│                                                             │
│  Stage 4: Full Verification (15 min) [parallel]             │
│    SCALUS_PLUTUSVM_SAMPLE_RATE=1.0                          │
│    sbtn "testOnly *PropertyTest"                            │
│    - All property tests on PlutusVM                         │
│                                                             │
│  Stage 5: Security Audit (optional, manual trigger)         │
│    sbtn "testOnly *SecurityTest"                            │
│    - All security tests                                     │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### GitHub Actions Example

```yaml
name: Scalus Tests

on: [ push, pull_request ]

jobs:
  compile:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          java-version: '21'
          distribution: 'temurin'
      - run: sbtn "scalafmtCheck; compile; Test/compile"

  fast-tests:
    needs: compile
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: sbtn "testOnly * -- -l Integration -l Slow"

  integration:
    needs: fast-tests
    runs-on: ubuntu-latest
    services:
      docker:
        image: docker:dind
    steps:
      - uses: actions/checkout@v4
      - run: sbtn scalusCardanoLedgerIt/test

  full-verification:
    needs: fast-tests
    runs-on: ubuntu-latest
    env:
      SCALUS_PLUTUSVM_SAMPLE_RATE: "1.0"
    steps:
      - uses: actions/checkout@v4
      - run: sbtn "testOnly *PropertyTest"
```

---

## Part 8h: Debugging Workflows

### Debugging on JVM

**Advantage:** Full Scala debugger support

```scala
// Step 1: Use debugOnJvm to isolate JVM execution
test("debug: reveal logic") {
  validator.debugOnJvm.test(preimage, ctx)
    .assertSuccess()
}

// Step 2: Set breakpoints in validator code
@Compile
object HtlcValidator {
  inline def validate(ctx: Data): Unit = {
    val scriptCtx = ctx.to[ScriptContext]
    // Set breakpoint here ↓
    val preimage = scriptCtx.redeemer.to[Action] match
      case Action.Reveal(p) => p
      case _ => fail("Expected Reveal")

    // Inspect variables in debugger
    val hash = sha3_256(preimage)
    // ...
  }
}

// Step 3: Run test in debug mode (IntelliJ/VS Code)
// Breakpoints will hit, can inspect all variables
```

### Debugging on PlutusVM

**Challenge:** No debugger, only error traces

```scala
// Step 1: Enable error traces
val contract = HtlcContract.compiled.withErrorTraces

// Step 2: Add strategic log() calls
@Compile
object HtlcValidator {
  inline def validate(ctx: Data): Unit = {
    log("Entering validate")
    val scriptCtx = ctx.to[ScriptContext]
    log(s"Got script info: ${scriptCtx.scriptInfo}")

    scriptCtx.scriptInfo match
      case ScriptInfo.SpendingScript(_, datum) =>
        log("Is spending script")
        val config = datum.get.to[Config]
        log(s"Config timeout: ${config.timeout}")
    // ...
  }
}

// Step 3: Check logs in test result
test("debug PlutusVM behavior") {
  val result = validator.debugOnPlutusVM.test(preimage, ctx)
  println(s"Logs: ${result.logs.mkString("\n")}")
  println(s"Budget: ${result.budget}")
}
```

### Debugging JVM vs PlutusVM Discrepancies

When JVM succeeds but PlutusVM fails (or vice versa):

```scala
test("investigate discrepancy") {
  val jvmResult = validator.debugOnJvm.test(preimage, ctx)
  val vmResult = validator.debugOnPlutusVM.test(preimage, ctx)

  println(s"JVM: ${jvmResult.outcome}")
  println(s"PlutusVM: ${vmResult.outcome}")
  println(s"PlutusVM logs: ${vmResult.logs}")

  // Common causes:
  // 1. Integer overflow (Scala Int vs UPLC BigInt)
  // 2. String encoding differences
  // 3. Lazy evaluation differences
  // 4. Error message format differences
}
```

### Debugging Transaction Failures

```scala
test("debug transaction failure") {
  try {
    val tx = txCreator.reveal(preimage, ...)
    println(s"Transaction built successfully")
    println(s"Redeemers: ${tx.witnessSet.redeemers}")
    println(s"ExUnits: ${tx.witnessSet.redeemers.map(_.value.totalExUnits)}")
  } catch {
    case e: TxBuilderException.BalancingException =>
      println(s"Script evaluation failed!")
      println(s"Script logs: ${e.scriptLogs.getOrElse(Nil).mkString("\n")}")
      println(s"Exception: ${e.getMessage}")

    case e: TxBuilderException =>
      println(s"Transaction building failed: ${e.getMessage}")
  }
}
```

### Debugging Emulator Submission

```scala
test("debug emulator submission") {
  val provider = createProvider()
  val tx = txCreator.reveal(preimage, ...)

  // Get detailed submission result
  val result = provider.submit(tx).await()
  result match {
    case Right(txHash) =>
      println(s"Submitted: $txHash")
      println(s"New UTxO set: ${provider.utxos}")

    case Left(SubmitError.NodeError(msg, ex)) =>
      println(s"Ledger validation failed: $msg")
      ex.foreach(_.printStackTrace())

    case Left(SubmitError.NetworkError(msg, _)) =>
      println(s"Network error: $msg")
  }
}
```

### Debugging Budget Issues

```scala
test("debug budget") {
  val result = validator.debugOnPlutusVM.test(preimage, ctx)

  println(s"Memory: ${result.budget.memory}")
  println(s"CPU steps: ${result.budget.steps}")

  // Compare with protocol limits
  val limits = env.protocolParams.maxTxExecutionUnits
  println(s"Max memory: ${limits.memory}")
  println(s"Max steps: ${limits.steps}")
  println(s"Memory %: ${100.0 * result.budget.memory / limits.memory}%")
  println(s"Steps %: ${100.0 * result.budget.steps / limits.steps}%")

  // If over budget, identify expensive operations
  // by adding log() before/after operations
}
```

### Debug Helper Utilities

```scala
trait DebugHelpers {
  // Pretty print Data values
  def debugData(data: Data): String = pprint(data).plainText

  // Pretty print ScriptContext
  def debugContext(ctx: ScriptContext): Unit = {
    println(s"TxInfo inputs: ${ctx.txInfo.inputs.length}")
    println(s"TxInfo outputs: ${ctx.txInfo.outputs.length}")
    println(s"Signatories: ${ctx.txInfo.signatories}")
    println(s"Valid range: ${ctx.txInfo.validRange}")
    println(s"Redeemer: ${debugData(ctx.redeemer)}")
    println(s"Script info: ${ctx.scriptInfo}")
  }

  // Diff two results
  def diffResults(jvm: JvmResult, vm: VmResult): Unit = {
    if jvm.succeeded != vm.succeeded then
      println(s"DISCREPANCY: JVM ${if jvm.succeeded then "succeeded" else "failed"}")
      println(s"           PlutusVM ${if vm.succeeded then "succeeded" else "failed"}")
    if !vm.succeeded then
      println(s"PlutusVM error: ${vm.logs.lastOption.getOrElse("unknown")}")
  }
}
```

### Debugging Checklist

```
When a test fails:
==================

1. [ ] Is it JVM or PlutusVM failure?
       → Run debugOnJvm and debugOnPlutusVM separately

2. [ ] Is the ScriptContext correct?
       → Print and inspect all fields

3. [ ] Is the datum/redeemer serialized correctly?
       → Check round-trip: data.to[Type].toData == data

4. [ ] Is it a script error or ledger error?
       → Check if TxBuilder fails vs Emulator fails

5. [ ] Is it a budget issue?
       → Check budget vs protocol limits

6. [ ] Is there a semantic difference?
       → Compare JVM vs PlutusVM behavior for same input

7. [ ] Is the error message helpful?
       → Enable .withErrorTraces if not

8. [ ] Can you reproduce with minimal input?
       → Use ScalaCheck shrinking to find smallest failing case
```

---

## Part 9: The Scalus Advantage - Dual Execution

### Key Insight: Same Code, Multiple Execution Environments

Scalus uniquely enables **the same validator logic** to run on:

1. **JVM** - via `compiled.code(args)` - Scala execution
2. **PlutusVM** - via `compiled.program` - UPLC execution

This is powerful because:

```scala
// This Scala code
@Compile
object HtlcValidator {
  inline def validate(ctx: ScriptContext): Unit = {
    // Your logic here
  }
}

// Can be executed as:
compiled.code(ctx.toData) // Runs Scala on JVM (debuggable)
compiled.program.evaluate // Runs UPLC on PlutusVM (realistic)
```

### Why Both Matter

| Aspect               | JVM                 | PlutusVM           |
|----------------------|---------------------|--------------------|
| **Speed**            | Very fast           | Fast               |
| **Debugging**        | Full Scala debugger | Error traces only  |
| **Budget**           | Not tracked         | Accurate           |
| **Semantics**        | Scala               | UPLC (may differ!) |
| **Production match** | No                  | Yes                |

### Semantic Differences to Watch For

```scala
// Integer overflow
val x: BigInt = BigInt(2).pow(64) // JVM: works
// PlutusVM: works (arbitrary precision)

// But with bounded integers:
val y: Int = Int.MaxValue + 1 // JVM: wraps to negative
// PlutusVM: N/A (uses BigInt)

// String operations
val s = "hello".take(3) // JVM: works
// PlutusVM: ByteString operations, not String
```

### Recommended Dual Execution Strategy

```
┌─────────────────────────────────────────────────────────┐
│              ALWAYS RUN ON BOTH                         │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. Run on JVM first (fast feedback)                    │
│  2. Run on PlutusVM (verify same result)                │
│  3. Assert: both succeed OR both fail                   │
│  4. Record budget from PlutusVM                         │
│                                                         │
│  For property tests:                                    │
│  - Run all cases on JVM                                 │
│  - Sample 10% on PlutusVM                               │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

## Part 10: Security Testing Patterns

### Pattern 1: Unauthorized Access (CRITICAL)

**Every spending path must verify authorization.**

```scala
test("SECURITY: Eve cannot reveal with valid preimage") {
  assertTxFail(HtlcValidator.UnsignedReceiverTransaction) {
    txCreator.reveal(
      receiverPkh = Eve.addrKeyHash, // Wrong receiver
      signer = Eve.signer
    )
  }
}
```

### Pattern 2: Time Manipulation (HIGH)

**Time-locked contracts must enforce bounds in both directions.**

```scala
test("SECURITY: receiver cannot extend deadline") {
  assertTxFail(HtlcValidator.InvalidReceiverTimePoint) {
    txCreator.reveal(validTo = afterTimeout) // Past deadline
  }
}

test("SECURITY: committer cannot claim early") {
  assertTxFail(HtlcValidator.InvalidCommitterTimePoint) {
    txCreator.timeout(validFrom = beforeTimeout) // Before deadline
  }
}
```

### Pattern 3: Double Satisfaction (HIGH for multi-script)

**Prevent one redeemer satisfying multiple scripts.**

```scala
test("SECURITY: swap can't satisfy pool without payment") {
  val tx = buildTxWith(
    spendPool(poolRedeemer),
    spendSwap(reusingPoolRedeemer) // Try to reuse
  )
  assertTxFail("swap not satisfied")(tx)
}
```

### Pattern 4: Datum Manipulation (MEDIUM)

**Fuzz test datum handling for robustness.**

```scala
property("SECURITY: random datum doesn't crash") {
  forAll(genArbitraryData) { data =>
    val result = Try(validator.validate(data))
    // Should succeed OR fail gracefully (not crash)
    result.isSuccess || result.failed.get.isInstanceOf[ValidationError]
  }
}
```

---

## Summary: Test Strategy Checklist

### For a Single Contract

- [ ] **Logic tests** (JVM + PlutusVM): all valid/invalid cases
- [ ] **Budget check** (PlutusVM): within protocol limits
- [ ] **Transaction tests** (Emulator): full tx builds and validates
- [ ] **Security: unauthorized access** (Emulator): all spending paths
- [ ] **Security: time attacks** (Emulator): if time-sensitive
- [ ] **Integration** (Yaci): before mainnet deployment

### For a Protocol (Multiple Validators)

- [ ] All single contract checks for each validator
- [ ] **Cross-validator tests** (Emulator): interactions work correctly
- [ ] **Security: double satisfaction** (Emulator): scripts can't be tricked
- [ ] **State machine tests** (Emulator): full protocol flows
- [ ] **Integration** (Yaci): end-to-end with real blocks
