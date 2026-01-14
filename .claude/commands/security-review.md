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

3. **Reporting Phase**
   - Use TodoWrite to create a todo item for each issue found
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

4. **Remediation Phase**
   For each issue:
   - Show vulnerable code with exact file:line location
   - Propose secure fix with code example
   - Ask: "Apply this fix? [y/n/s/d]"
   - If yes: apply fix, mark todo completed
   - After all fixes: run `sbtn compile` to verify

5. **Summary**
   Generate table with clickable locations:
   ```
   | ID | Severity | Location | Issue |
   |----|----------|----------|-------|
   | C-01 | Critical | `path/File.scala:123` | Description |
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
```
