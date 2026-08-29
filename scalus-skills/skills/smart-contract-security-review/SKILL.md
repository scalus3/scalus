---
name: smart-contract-security-review
description: Security review for Scalus/Cardano smart contracts. Analyzes @Compile annotated validators for vulnerabilities like redirect attacks, inexact value validation, missing token verification, ADA-only comparisons, rounding direction, and self-dealing. Use when reviewing on-chain code, before deploying validators, or when /security-review is invoked. Requires explicit path argument.
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
Taxonomy IDs are family-number (DS double satisfaction, VP value preservation, AU
authentication, MI minting, TI time, DT datum, IX index, PU purpose, EV evaluation, AR
arithmetic, RS resources, DE design). "Fixed by" names the Scalus operation or idiom that
closes the class; "design" means no operation can, review the design.

### Critical Severity

| ID | Taxonomy | Name | Risk | Detection | Fixed by |
|----|----------|------|------|-----------|----------|
| V001 | VP-3 | Redirect Attack | Funds stolen via output redirection | `outputs.at(idx)` without a whole-address check | `findContinuingOutputOrFail(ownInput, msg)` |
| V002 | AU-1 | Token/NFT Not Verified | State token excluded | No `hasNft` / `hasOnly` on the continuing output | `out.value.hasNft(policy, name)` |
| V003 | MI-1 / MI-3 | Inexact Burn/Mint | Extra tokens minted, partial burn | `>=` instead of `===`; `quantityOf` alone; `forall(_._2 < 0)` on a possibly empty map | `tx.mint.hasOnly(policy, name, signedQty)`; `tx.onlyBurnsUnder(policy)` |
| V004 | AR-1 | Rounding Direction (on-chain `Integer` is unbounded; there is no overflow) | Remainder harvested per split action; negative amounts | `/` on fees or shares without a stated direction; redeemer amounts without `> 0` | `a divCeil b` / `a divFloor b` (round against the party that benefits: contract payouts down, amounts owed to the contract up) |
| V005 | DS-1 / DS-2 | Double Satisfaction | Pay once, satisfy many | `outputs.exists` without unique linking; `>=` payouts | `inputs.findUniqueOrFail(_.resolved.address.credential === ownCred, msg)` or `hasPaidTagged(addr, value, tag)` |
| V026 | VP-1 | Value Not Preserved on Continuing Output | Continuing UTxO drained while the datum looks right | Datum transition checked, `.value` never read; datum balance not tied to `quantityOf` | `out.value.hasSameTokensAndAtLeastAda(expected)` or `out.value === expected`; bind datum balances with `quantityOf` |
| V027 | VP-2 | ADA-Only Value Comparison | Native tokens stripped while lovelace matches | `getLovelace` compared on a script UTxO; `getAdaFromOutputs` / `getAdaFromInputs` | whole `Value`: `valuePaidTo(addr)`, `valueSpentFrom(addr)`, `===`; or `withoutLovelace.isZero` at the boundary |
| V028 | MI-2 | One-Shot Seed Not Bound | "Unique" NFT mintable forever | `TxOutRef` parameter never compared with an input's `outRef` | `findInputOrFail(seed, msg)` or `inputs.at(i).outRef === seed`; `seed.deriveTokenName`; `mint.hasOnly` |
| V029 | IX-2 | Missed Input | A script input no index names is spent unchecked | Loop over redeemer indices, no walk over `tx.inputs` | walk `tx.inputs`; `UtxoIndexer.multiOneToOneNoRedeemer` |

### High Severity

| ID | Taxonomy | Name | Risk | Detection | Fixed by |
|----|----------|------|------|-----------|----------|
| V006 | IX-1 | Index Validation Missing | Wrong element behind an index | `.at(idx)` from the redeemer with no check on the element | `findInputOrFail`, `findUniqueOrFail`, `singleOrFail` |
| V007 | DE-3 | Self-Dealing/Shill Bidding | Price manipulation | No seller/bidder separation | design: `require(!(bidder === seller))` |
| V008 | IX-1 | Double Spend via Index | Same UTxO processed twice | Index lists without uniqueness; `zip` truncation | strictly ascending indices; `UtxoIndexer` |
| V009 | VP-4 | Inexact Refund Amount | Fund manipulation, enables V005 | `>=` for refunds instead of `===` | `out.value === Value.lovelace(n)` (whole value); `hasPaidTagged` |
| V010 | PU-1 | Other Redeemer Attack | Bypass via different redeemer or purpose | Several purposes/branches with weaker checks; `StakeValidator.spendMinimal` alone | plugin default `fail` for unimplemented purposes (false positive for single-purpose objects, see below); `StakeValidator.spend` with a redeemer validator |
| V011 | MI-1 | Other Token Name Attack | Unauthorized token minting | `quantityOf` on one name, rest of the policy unchecked | `tx.mint.hasOnly(policy, name, qty)`; `mint.tokens(policy) === expected.tokens(policy)` |
| V012 | AU-1 / AU-5 | Missing UTxO Authentication | Fake UTxO or planted reference-input datum | No auth token on own input or reference input | `ownInput.resolved.value.hasNft(authPolicy, name)`; one-shot policy (V028) |
| V025 | DE-2 | Oracle Data Validation | Price manipulation, stale data | Oracle data without signature/freshness | design: `verifyEd25519Signature` + domain separation (V031), freshness vs `validToOrFail` |
| V030 | EV-1 | Evaluation-Order Trap | Security check never evaluated | Check on the right of `\|\|` or in an untaken branch; pattern callback ending in a `Boolean` | one obligation per `require`; callbacks return `Unit` |
| V031 | AU-7 | Signature Domain Separation | Off-chain signature replayed across instances | `verifyEd25519Signature` over a payload without script hash and nonce | design: payload commits to domain tag, own script hash, spent `TxOutRef` |
| V032 | PU-3 | Certificate Purposes Unguarded | Deregistration griefing, deposit theft | `certify` body `()` or permissive `case _ => ()` | plugin default `fail`; explicit `TxCert` match with failing default |

### Medium Severity

| ID | Taxonomy | Name | Risk | Detection | Fixed by |
|----|----------|------|------|-----------|----------|
| V013 | TI-1 / TI-2 | Time Handling | Unbounded range read as time 0; wrong inclusivity | `getValidityStartTime` (deprecated, returns 0 when unbounded); raw bound compared | `validFromOrFail(msg)` (inclusive) / `validToOrFail(msg)` (exclusive); `isEntirelyAfter/Before` |
| V014 | AU-2 | Missing Signature | Unauthorized actions; script owner cannot sign | No `isSignedBy` on a branch; `isSignedBy` on a script hash | `isSignedBy` / `isSignedByAny` for keys; for a script authority prove it ran (withdrawal or spent input) |
| V015 | DT-1 | Datum Mutation | Unauthorized state change | Field-by-field comparison with a field missing | `out.hasInlineDatum(old.copy(...))` |
| V016 | AU-4 | Insufficient Staking Control | Reward redirection (franken address) | `address.credential ===`, `=== Address.fromScriptHash(h)`, credential-only finders on the continuing output | `findContinuingOutputOrFail`; or `require(out.address === ownInput.resolved.address)` |
| V017 | DT-3 | Arbitrary Datum | Unspendable UTxOs, datum-hash bricking | No datum validation; hash-only datum accepted | `out.datum.inlineOrFail[T](msg)`; `out.hasInlineDatum(x)` |
| V024 | AU-6 | Parameterization Verification | Script substitution (varies) | ParameterizedValidator with auth params, no token | auth NFT via `hasNft` + one-shot policy; design |
| V033 | PU-4 | Voting / Proposing Purposes Unguarded | Governance actions approved silently | Hand-written `ScriptInfo` dispatcher with `case _ => ()` | plugin default `fail`; explicit `vote` / `propose` with authorization |
| V034 | VP-5 | Value-Map Normalisation | Locked UTxO or false equality on a non-canonical `Value` | `Value` field in datum/redeemer compared with `===` | `Value.valueFromDataWithValidation` at the boundary; ledger values are canonical |
| V035 | VP-6 | Min-ADA Griefing | Forced output pushed below min-ADA, UTxO stuck | Whole-value preservation on a UTxO anyone can pay into | bound the token set at deposit: `withoutLovelace.isZero`, `hasSameTokensAndAtLeastAda` |
| V036 | DE-4 | Hash Grinding | Attacker grinds a hash-derived outcome | `tx.id` / out-ref hash used as randomness | design: commit-reveal |

### Low Severity / Design Issues

| ID | Taxonomy | Name | Risk | Detection | Fixed by |
|----|----------|------|------|-----------|----------|
| V018 | RS-1 | Unbounded Value | UTxO size limit, min-ADA lever | Unlimited tokens in output | `withoutLovelace.isZero`; `hasSameTokensAndAtLeastAda` pins the token set |
| V019 | RS-2 | Unbounded Datum | Resource exhaustion | Growing datum size | design: bound every list |
| V020 | RS-3 | Unbounded Inputs | TX limit exceeded | Many required UTxOs | design: batching |
| V021 | RS-5 | UTxO Contention / Concurrency DoS | Bottleneck, DoS | Shared global state, no rate limit | design: per-user UTxOs |
| V022 | RS-6 | Cheap Spam/Dust | Operation obstruction | No minimum amounts | design: minimum `require` |
| V023 | DE-1 | Locked Value | Permanent lock | Missing exit paths | design: exit path per state |
| V037 | RS-7 | Reference-Script Size | Fee blow-up, size cap exceeded | Large compiled scripts, many per transaction | design: report script size, split logic |

Checklist rules:
1. V010, V032, V033: a purpose the validator `object` does not define is completed by the compiler
   plugin with a body that fails, so an undefined purpose is never an entry point. Report only
   purposes the object defines.
2. V001 and V016 together: a credential-only finder or `Address.fromScriptHash` never proves the
   continuing output; only the whole address does.
3. V027 before V005: when a value check reads `.getLovelace`, ask whether tokens can be in the
   UTxO before asking whether one output can serve two inputs.
4. V013: any `getValidityStartTime` is a finding; a `validFromOrFail` / `validToOrFail` is not.

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

**Potential vulnerability detected**: `handlePay` sums the seller's outputs by credential without unique linking.

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
// Line 58-61, before any handler:
txInfo.inputs.findUniqueOrFail(
  _.resolved.address.credential === contractAddress.credential,
  "Exactly one escrow input may be spent"
)
// → inputs at the script credential: [EscrowA, EscrowB] → two matches → FAILS ❌

// Line 63: never reached
val contractBalance = txInfo.valueSpentFrom(contractAddress).getLovelace
```

**Result**: Attack transaction fails at line 58-61. V005 is a **FALSE POSITIVE**.

**Second look at the same lines (V027)**: `contractBalance` and the seller sum are compared on
`.getLovelace` only. Attack transaction 2: EscrowA holds 12 ADA + 500 USDM; outputs 12 ADA to S
and 500 USDM to B. Both lovelace checks pass, the tokens leave. This is a **V027 finding** unless
the contract proves the UTxO is ADA-only at the boundary; EscrowValidator does
(`handleDeposit`: `txInfo.valuePaidTo(contractAddress) === Value.lovelace(escrowAmount + initializationAmount)`),
so here it is not reported. Without that proof, report it.

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
