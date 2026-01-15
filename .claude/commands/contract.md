---
description: Guide for developing Scalus smart contracts
---

When helping with smart contract development, read the relevant documentation first.

## Documentation Paths

Read these files to understand patterns before writing contract code:

**Core concepts:**
- `scalus-site/content/smart-contract/validators.mdx` - Validator trait, @Compile, script purposes
- `scalus-site/content/smart-contract/compiling.mdx` - PlutusV1/V2/V3 compilation
- `scalus-site/content/smart-contract/from-data.mdx` - FromData/ToData derivation
- `scalus-site/content/smart-contract/debugging.mdx` - Debugging techniques

**Optimization (for production contracts):**
- `scalus-site/content/smart-contract/optimisations/script-context.mdx` - ScriptContext optimization
- `scalus-site/content/smart-contract/optimisations/macros.mdx` - Macro optimizations
- `scalus-site/content/smart-contract/optimisations/sir.mdx` - SIR-level optimizations

**Language features:**
- `scalus-site/content/language-guide/data-types.mdx` - Scalus data types
- `scalus-site/content/language-guide/builtin-functions.mdx` - Built-in functions

## Code Examples

Study existing validators before creating new ones:
- `scalus-examples/shared/src/main/scala/scalus/examples/`

## Key Patterns

**Annotations:**
- `@Compile` - marks code for Plutus compilation
- `@Ignore` - excludes from compilation (off-chain helpers only)
- `inline` - keyword for on-chain optimization

**Data structures:**
- Use `derives FromData, ToData` for case classes
- Use enums for redeemer actions
- Use sealed traits for ADTs

**Validation:**
- `require(condition, message)` - assertion with error message
- `fail(message)` - explicit failure
- `getOrFail(option, message)` - safe Option extraction

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

- Place validators in `shared/src/main/scala/` for cross-platform support
- Use `scalus-examples` module for new contract examples
