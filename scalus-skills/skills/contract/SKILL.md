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
- On-chain, compare enums and case classes with `a.toData == b.toData`

**Validation:**
- `require(condition, message)` - assertion with error message
- `fail(message)` - explicit failure
- `getOrFail(option, message)` - safe Option extraction
- `output.datum.inlineOrFail[MyDatum](message)` - extract and decode an inline datum,
  fail the script otherwise. Use it instead of matching on `OutputDatum.OutputDatum` with
  a `fail` fallback; the no-message overload fails with "Expected inline datum".
- `tx.mint.hasOnly(policyId, tokenName, 1)` - exact single-token mint check: exactly
  `{tokenName -> amount}` under the policy, nothing else, other policies unconstrained.
  Use it instead of `quantityOf(...) === BigInt(1)` plus a separate only-token check.

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
