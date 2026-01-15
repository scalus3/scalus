---
name: smart-contract-security-review
description: Security review for Scalus/Cardano smart contracts. Analyzes @Compile annotated validators for vulnerabilities like redirect attacks, inexact value validation, missing token verification, integer overflow, and self-dealing. Use when reviewing on-chain code, before deploying validators, or when /security-review is invoked. Requires explicit path argument.
---

# Smart Contract Security Review

Analyze Scalus/Cardano smart contracts for security vulnerabilities.

## Target Code Identification

Find on-chain code by searching for:
1. Objects/classes with `@Compile` annotation
2. Objects extending `Validator`, `DataParameterizedValidator`, or `ParameterizedValidator`
3. Objects compiled with `PlutusV3.compile()`, `PlutusV2.compile()`, or `PlutusV1.compile()`

Search patterns:
```
grep -rn "@Compile" --include="*.scala" <path>
grep -rn "extends Validator" --include="*.scala" <path>
grep -rn "extends DataParameterizedValidator" --include="*.scala" <path>
```

## Workflow

1. **Discovery**: Find all `@Compile` annotated code in specified path
2. **Classification**: Identify validator type (spend/mint/reward/certify/vote/propose)
3. **Analysis**: Check each validator against vulnerability checklist
4. **False Positive Verification**: For each potential issue, verify it's not a false positive
5. **Reporting**: Generate structured report with severity levels (only verified issues)
6. **Remediation**: Use TodoWrite to track issues, fix one-by-one with user confirmation

## Vulnerability Checklist

Based on Cardano Developer Portal security guidelines and Scalus-specific patterns.
For detailed patterns and code examples, see `references/vulnerabilities.md`.

### Critical Severity

| ID | Name | Risk | Detection |
|----|------|------|-----------|
| V001 | Redirect Attack | Funds stolen via output redirection | `outputs.at(idx)` without address validation |
| V002 | Token/NFT Not Verified | State token excluded | Missing `quantityOf` on outputs |
| V003 | Inexact Burn/Mint | Extra tokens minted | `>=` instead of `===` for quantities |
| V004 | Integer Overflow | Arithmetic overflow | Custom encoding without bounds |
| V005 | Double Satisfaction | Pay once, satisfy many | `outputs.exists` without unique linking |

### High Severity

| ID | Name | Risk | Detection |
|----|------|------|-----------|
| V006 | Index Validation Missing | Invalid index access | `.at(idx)` without bounds check |
| V007 | Self-Dealing/Shill Bidding | Price manipulation | No seller/bidder separation |
| V008 | Double Spend via Index | Same UTxO processed twice | Index lists without uniqueness |
| V009 | Inexact Refund Amount | Fund manipulation | `>=` for refunds instead of `===` |
| V010 | Other Redeemer Attack | Bypass via different redeemer | Multiple script purposes |
| V011 | Other Token Name Attack | Unauthorized token minting | Policy doesn't check all tokens |
| V012 | Missing UTxO Authentication | Fake UTxO injection | No auth token verification |
| V025 | Oracle Data Validation | Price manipulation, stale data | Oracle data without signature/freshness |

### Medium Severity

| ID | Name | Risk | Detection |
|----|------|------|-----------|
| V013 | Time Handling | Time manipulation | Incorrect interval bounds |
| V014 | Missing Signature | Unauthorized actions | No `isSignedBy` checks |
| V015 | Datum Mutation | Unauthorized state change | No field comparison |
| V016 | Insufficient Staking Control | Reward redirection | No staking credential check |
| V017 | Arbitrary Datum | Unspendable UTxOs | No datum validation |
| V024 | Parameterization Verification | Script substitution (varies) | ParameterizedValidator with auth params, no token |

### Low Severity / Design Issues

| ID | Name | Risk | Detection |
|----|------|------|-----------|
| V018 | Unbounded Value | UTxO size limit | Unlimited tokens in output |
| V019 | Unbounded Datum | Resource exhaustion | Growing datum size |
| V020 | Unbounded Inputs | TX limit exceeded | Many required UTxOs |
| V021 | UTxO Contention / Concurrency DoS | Bottleneck, DoS | Shared global state, no rate limit |
| V022 | Cheap Spam/Dust | Operation obstruction | No minimum amounts |
| V023 | Locked Value | Permanent lock | Missing exit paths |

## False Positive Verification

**CRITICAL**: Before reporting ANY vulnerability, you MUST verify exploitability by tracing code execution with a concrete attack transaction.

### Verification Method: Attack Transaction Tracing

For each potential vulnerability:

1. **Construct a concrete attack transaction**
   - Define specific inputs (UTxOs with concrete values/datums)
   - Define the redeemer values
   - Define the outputs the attacker would create
   - Define signatories

2. **Execute the validator logic mentally with this transaction**
   - Go line-by-line through the validator code
   - Track what each variable evaluates to with your attack tx
   - Check EVERY `require()` statement - does it pass or fail?

3. **If ANY require fails, the attack fails → False Positive**

4. **Only report if ALL requires pass with the attack transaction**

### Example: V005 Double Satisfaction Verification

**Potential vulnerability detected**: `handlePay` uses `getAdaFromOutputs(sellerOutputs)` without unique linking.

**Construct attack transaction**:
```
Inputs:
  - EscrowA: 12 ADA, datum={seller=S, buyer=B, escrowAmount=10, initAmount=2}
  - EscrowB: 12 ADA, datum={seller=S, buyer=B, escrowAmount=10, initAmount=2}
Outputs:
  - 12 ADA to seller S (single output for both!)
  - 1 ADA to buyer B
Signatories: [B]
```

**Trace execution for EscrowA**:
```scala
// Line 57-58:
val contractInputs = txInfo.findOwnInputsByCredential(contractAddress.credential)
// → Returns [EscrowA, EscrowB] (both have same script credential)

val contractBalance = Utils.getAdaFromInputs(contractInputs)
// → 12 + 12 = 24 ADA

// Line 118-120 in handlePay:
require(contractBalance === escrowDatum.escrowAmount + escrowDatum.initializationAmount)
// → require(24 === 10 + 2)
// → require(24 === 12)
// → FAILS! ❌
```

**Result**: Attack transaction fails at line 118-120. This is a **FALSE POSITIVE**.

### When to Write a Test

If the attack trace is complex or you're uncertain, write an actual test:

```scala
test("V005: Double satisfaction attack should fail") {
  // Setup: Create two escrow UTxOs with same seller
  val escrowA = createEscrowUtxo(seller = S, buyer = B, amount = 10.ada)
  val escrowB = createEscrowUtxo(seller = S, buyer = B, amount = 10.ada)

  // Attack: Try to spend both with single output to seller
  val attackTx = Transaction(
    inputs = List(escrowA, escrowB),
    outputs = List(TxOut(sellerAddress, 12.ada)),  // Only pay once!
    redeemers = Map(escrowA -> Pay, escrowB -> Pay)
  )

  // Verify: Should this pass or fail?
  // If it passes → Real vulnerability
  // If it fails → False positive
  evaluateValidator(EscrowValidator, escrowA, attackTx) shouldBe failure
}
```

### Verification Checklist

Before reporting, answer these questions:

| Question | Answer Required |
|----------|-----------------|
| What is the specific attack transaction? | Inputs, outputs, redeemers, signatories |
| Which line would the attacker exploit? | File:line reference |
| Did you trace through EVERY require in the code path? | Yes/No |
| Does the attack pass ALL requires? | Yes (report) / No (false positive) |
| What value does the attacker gain? | Concrete amount/asset |

### Do NOT Report If

- You only found a pattern match without tracing execution
- You haven't constructed a specific attack transaction
- Any `require()` in the code path would fail the attack
- You're unsure whether the attack works (investigate more or write a test)

## Output Format

Use clickable `file_path:line_number` format for all code locations.

### Finding Format

For each vulnerability found, output in this format:

```
### [SEVERITY] ID: Vulnerability Name

**Location:** `full/path/to/File.scala:LINE`
**Method:** methodName

**Issue:** Brief description of what's wrong

**Vulnerable code** (`full/path/to/File.scala:LINE-LINE`):
```scala
// actual code from file
```

**Fix:**
```scala
// proposed fix
```

---
```

### Summary Table

At the end, provide a summary with clickable locations:

```
## Summary

| ID | Severity | Location | Issue | Status |
|----|----------|----------|-------|--------|
| C-01 | Critical | `path/File.scala:123` | Missing mint validation | Fixed |
| H-01 | High | `path/File.scala:87` | Token not in output | Declined |
| M-01 | Medium | `path/File.scala:200` | Missing signature | False Positive |

## False Positives

| ID | Location | Reason |
|----|----------|--------|
| M-01 | `path/File.scala:200` | Authorization is done via NFT ownership in `verifyAuth` helper |

**Security Grade:** A/B/C/D/F
```

### Location Format Rules

1. Always use full path from project root: `scalus-examples/jvm/src/.../File.scala:123`
2. For ranges use: `File.scala:123-145`
3. For method references: `File.scala:123` (methodName)
4. Make locations clickable by using backticks

## Interactive Workflow

For each finding:
1. Display issue with location and proposed fix
2. Prompt: "Apply fix? [y/n/s/d/f]"
   - y: Apply fix, mark completed, verify with `sbtn compile`
   - n: Skip, log as "declined"
   - s: Skip without logging
   - d: Show more details (attack scenario)
   - f: Mark as false positive (prompts for reason, logged to summary)
3. After all findings: run `sbtn quick` to verify fixes
4. Generate summary report including:
   - Fixed issues
   - Declined issues
   - False positives with reasons

## Reference

For detailed vulnerability patterns and code examples, see:
- `references/vulnerabilities.md` - Full pattern documentation with Scalus-specific examples
