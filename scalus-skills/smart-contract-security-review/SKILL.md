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
4. **Reporting**: Generate structured report with severity levels
5. **Remediation**: Use TodoWrite to track issues, fix one-by-one with user confirmation

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

| ID | Severity | Location | Issue |
|----|----------|----------|-------|
| C-01 | Critical | `path/File.scala:123` | Missing mint validation |
| H-01 | High | `path/File.scala:87` | Token not in output |

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
2. Prompt: "Apply fix? [y/n/s/d]"
   - y: Apply fix, mark completed, verify with `sbtn compile`
   - n: Skip, log as "declined"
   - s: Skip without logging
   - d: Show more details (attack scenario)
3. After all findings: run `sbtn quick` to verify fixes
4. Generate summary report

## Reference

For detailed vulnerability patterns and code examples, see:
- `references/vulnerabilities.md` - Full pattern documentation with Scalus-specific examples
