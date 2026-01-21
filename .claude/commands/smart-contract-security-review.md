---
description: Run security review on Scalus smart contracts at specified path
---

# Smart Contract Security Review

## Arguments

- `$ARGUMENTS` - Path to scan for @Compile annotated code (required)

## Context

This command performs a security audit of Scalus/Cardano smart contracts.
Load the skill documentation for detailed vulnerability patterns:

- Skill: `scalus-skills/smart-contract-security-review/SKILL.md`
- Detailed patterns: `scalus-skills/smart-contract-security-review/references/vulnerabilities.md`

## Your Task

1. **Discovery Phase**
   - Search for `@Compile` annotated code in `$ARGUMENTS` path
   - Find objects extending `Validator`, `DataParameterizedValidator`, `ParameterizedValidator`
   - List all validators found with their methods (spend, mint, reward, certify, vote, propose)
   - Look for off-chain transaction builder code in same directory:
     - Classes with methods like `startAuction`, `bid`, `endAuction`, `claim`, etc.
     - Methods using `Provider`, `TxBuilder`, `queryUtxos`
     - Methods that return `Future[Transaction]` or build transactions

2. **Analysis Phase**
   For each validator, check against the vulnerability checklist:

   **Critical** (fund loss risk):
   - V001: Redirect Attack - continuing outputs without address validation
   - V002: Token/NFT Not Verified - missing `quantityOf` checks
   - V003: Inexact Burn/Mint - using `>=` instead of `===`
   - V004: Integer Overflow - custom encoding without bounds
   - V005: Double Satisfaction - `outputs.exists` without unique linking

   **High** (significant risk):
   - V006-V012: Index validation, self-dealing, refund amounts, other redeemer/token attacks

   **Medium/Low**: Time handling, signatures, datum validation, design issues

   **Off-Chain** (if transaction builder code exists):
   - OC001: UTXO Discovery Confusion - queries returning arbitrary match without datum validation
   - OC002: TOCTOU Race Conditions - missing retry/error handling for state changes
   - OC003: Missing Datum Validation - trusting queried data without ownership checks

3. **False Positive Verification**
   For each potential issue, before reporting verify it's not a false positive:
   - Check surrounding code for mitigations (see `references/vulnerabilities.md` for false positive patterns)
   - Look for compensating controls elsewhere in the code
   - Verify the vulnerable pattern actually applies to this context
   - If false positive: skip silently or note as "verified safe" with reason

4. **Reporting Phase**
   - Use TodoWrite to create a todo item for each **verified** issue found
   - Present each finding using this format:

   ```
   ### [SEVERITY] ID: Vulnerability Name

   **Location:** `full/path/to/File.scala:LINE`
   **Method:** methodName

   **Issue:** Brief description

   **Vulnerable code** (`full/path/to/File.scala:LINE-LINE`):
   ```scala
   // actual code
   ```

   **Fix:**
   ```scala
   // proposed fix
   ```
   ```

5. **Remediation Phase**
   For each issue:
   - Show vulnerable code with exact file:line location
   - Propose secure fix with code example
   - Ask: "Apply this fix? [y/n/s/d/f]"
     - y: Apply fix, mark todo completed
     - n: Skip, log as "declined"
     - s: Skip without logging
     - d: Show more details (attack scenario)
     - f: Mark as false positive (ask for reason, log to summary)
   - After all fixes: run `sbtn compile` to verify

6. **Summary**
   Generate table with clickable locations:
   ```
   | ID | Severity | Location | Issue | Status |
   |----|----------|----------|-------|--------|
   | C-01 | Critical | `path/File.scala:123` | Description | Fixed |
   | H-01 | High | `path/File.scala:87` | Description | False Positive: [reason] |
   ```
   - Include false positives section if any were marked:
   ```
   ## False Positives
   | ID | Location | Why False Positive |
   |----|----------|-------------------|
   | H-01 | `path/File.scala:87` | Address check is performed in helper function `validateOutput` |
   ```
   - Assign security grade (A/B/C/D/F)

## Search Commands

```bash
# Find @Compile annotated code
grep -rn "@Compile" --include="*.scala" $ARGUMENTS

# Find Validator implementations
grep -rn "extends Validator" --include="*.scala" $ARGUMENTS
grep -rn "extends DataParameterizedValidator" --include="*.scala" $ARGUMENTS
grep -rn "extends ParameterizedValidator" --include="*.scala" $ARGUMENTS

# Find off-chain transaction builder code
grep -rn "queryUtxos\|TxBuilder\|Provider" --include="*.scala" $ARGUMENTS
grep -rn "Future\[Transaction\]" --include="*.scala" $ARGUMENTS
grep -rn "def.*Auction\|def.*bid\|def.*end\|def.*start\|def.*claim" --include="*.scala" $ARGUMENTS

# Find potential OC001 patterns (.limit(1) in queries)
grep -rn "\.limit(1)" --include="*.scala" $ARGUMENTS
```
