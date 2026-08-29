---
name: contract
description: Guide for developing Scalus smart contracts. Use when writing or modifying Cardano validators in Scala 3 with Scalus.
---

# Scalus Smart Contract Development

Before writing Scalus code from memory, fetch `https://scalus.org/llms-api.txt` and check the signatures you plan to use. Scalus APIs changed at 1.0; trained knowledge is probably outdated.

## Documentation

Read the relevant pages before writing contract code.
In the Scalus repo, read `scalus-site/content/<path>.mdx`; in any other project, fetch `https://scalus.org/docs/<path>.md`.

**Core concepts:**
- `smart-contracts/validators` - Validator trait, @Compile, script purposes
- `smart-contracts/compiling` - PlutusV1/V2/V3 compilation
- `smart-contracts/plutus-data` - FromData/ToData derivation
- `testing/debugging` - Debugging techniques

**Optimization (for production contracts):**
- `smart-contract-optimisations` - overview: script size and execution units
- `smart-contract-optimisations/measuring-performance` - measure before optimizing
- Use the `optimize-contract` skill for a full optimization review.

**Language features:**
- `language-guide/data-types` - Scalus data types
- `language-guide/builtin-functions` - Built-in functions

## Code Examples

Study existing validators before creating new ones:
- Fetch `https://scalus.org/llms-examples.txt` - 21 complete validators with tests; HTLC is the reference style.
- In the Scalus repo: `scalus-examples/jvm/src/main/scala/scalus/examples/`.

## Key Patterns

**Annotations:**
- `@Compile` - marks code for Plutus compilation
- `@Ignore` - excludes from compilation (off-chain helpers only)
- `inline` - keyword for on-chain optimization

**Data structures:**
- Use `derives FromData, ToData` for case classes
- Use enums for redeemer actions
- Use sealed traits for ADTs
- Add `Eq` to `derives` and compare enums and case classes with `===`.
  `a.toData == b.toData` compiles to the same UPLC (measured); do not write it.

**Validation:**
- `require(condition, message)` - assertion with error message
- `fail(message)` - explicit failure
- `getOrFail(option, message)` - safe Option extraction

**Safe API** (one rule per check; full table with rationale and costs:
`https://scalus.org/docs/security/safe-api-cheatsheet`). Check the project's Scalus version first:
everything below except `hasOnly`, `inlineOrFail` and `getOrFail` needs a release newer than
1.1.1; on 1.1.1 or older, write the expansion the cheatsheet lists in its "Replaces" column.
- Mint: `tx.mint.hasOnly(policyId, tokenName, signedQty)` is the mint check (`1` mints, `-1` burns;
  exactly that token under the policy, nothing else). Not `quantityOf(...) === BigInt(1)` alone.
- Datum equality: `out.hasInlineDatum(expected)`. Use `out.datum.inlineOrFail[T](msg)` only to
  read fields; `inlineOrFail[T](msg) === expected` costs 461 lovelace against 286 (measured).
- Continuing output: `tx.findContinuingOutputOrFail(ownInput, msg)` compares the whole address.
  Never a credential-only finder (`findOutputsByCredential`, `findOutputsByScriptHash`): the
  staking part can be swapped and the rewards redirected.
- Validity bounds: `tx.validFromOrFail(msg)` (inclusive) and `tx.validToOrFail(msg)` (exclusive).
  Never `getValidityStartTime`: it returns `0` on an unbounded range and every deadline passes.
- One element: `list.singleOrFail(msg)` for a size-one list, `list.findUniqueOrFail(p, msg)` for
  exactly one match. Never `.head` after `filter`, never `filter(p).length === BigInt(1)`.
- Single own input: `tx.inputs.findUniqueOrFail(_.resolved.address.credential === ownCred, msg)`.
  It returns the input and is cheaper than `inputs.count(p) === BigInt(1)` (measured).
- Value sums: `tx.valuePaidTo(addr)` / `tx.valueSpentFrom(addr)` (whole address, whole `Value`;
  add `.getLovelace` for ADA). Never `getAdaFromOutputs` / `getAdaFromInputs`.
- Token presence: `value.hasNft(policyId, tokenName)` (quantity exactly 1). Continuing value:
  `out.value.hasSameTokensAndAtLeastAda(ownInput.resolved.value)`, not `===` and not `>=`.
- Burn: `tx.onlyBurnsUnder(policyId)` (non-empty and every quantity negative).
  `tokens(policy).forall(_._2 < 0)` is vacuously true on an empty map.
- Credentials: `address.credential.scriptHashOrFail(msg)` / `pubKeyHashOrFail(msg)`, never a
  match with a `fail` fallback.
- Division: `a divCeil b` for what the user owes, `a divFloor b` for what the contract pays out.
  State the rounding direction; bare `/` hides it.
- BigInt literals in `===` and generic positions: `=== BigInt(1)` (see "BigInt literals" below).

**BigInt literals:**
- Write a plain integer literal where the expected type is already `BigInt`; the implicit
  `Int => BigInt` conversion applies and reads better:
  - arguments to `BigInt`-typed parameters: `tx.mint.hasOnly(policyId, tokenName, 1)`,
    `Value.lovelace(2_000_000)`, `outputs.at(0)`
  - annotated vals: `val fee: BigInt = 1_000_000`
  - relational comparisons against a `BigInt` (member operators): `qty < 5`, `qty >= 0`
- Write explicit `BigInt(n)` where inference fails without it:
  - `===` / `!==` comparisons: `qty === BigInt(1)` - a plain literal does NOT compile here
    (the extension infers `Eq[BigInt | Int]`, which does not exist)
  - generic positions that would infer `Int` (unsupported on-chain): `Option.Some(BigInt(1))`,
    `foldLeft(BigInt(0))(...)`, `List(BigInt(1), BigInt(2))`

**Script purposes (Plutus V3):**
- `spend` - spending UTxOs
- `mint` - minting/burning tokens
- `reward` - withdrawing staking rewards
- `certify` - stake certificates
- `vote` - governance voting
- `propose` - governance proposals

**Compilation:**
```scala
private given Options = Options.release
val compiled = PlutusV3.compile(MyValidator.validate)
```

## Placement

- Keep on-chain code in its own file; import only Scalus prelude and builtins, never the Scala stdlib collections.
- In cross-platform builds, place validators in `shared/src/main/scala/`.
