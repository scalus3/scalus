Source: https://scalus.org/docs/security/datum-validation

# Datum Shape Validation

Scalus decodes on-chain data lazily: `fromData[T]` compiles to nothing, and fields are read from
the underlying `Data` only when your code touches them. There is no constructor-tag or
field-count check at the validator boundary. This page explains why that is the default, where
the residual risk actually lives, and what to do about it.

## Lazy by Default, on Purpose

Toolchains differ here. Aiken's `expect` decodes a typed datum strictly at the boundary
(tag, exact field count, deep). PlutusTx decodes the declared fields but accepts extra trailing
ones. Scalus, like plu-ts, trusts the datum and projects fields lazily.

Strict-by-default sounds safer, but it adds a catalogued vulnerability: **arbitrary-datum
bricking**. If a validator rejects any output whose datum does not decode to the expected type,
then an attacker (or a buggy wallet) that sends a UTxO to the script address with a
wrong-shaped datum creates an output that can never be spent, even though the validator would
never have looked at that datum's content. Lazy projection is immune: a datum the validator
never reads can never fail to decode.

The trade is two different exposures:

- Strict boundary decoding buys shape guarantees at the cost of robustness against
  arbitrary-datum bricking.
- Lazy projection keeps that robustness; the residual risk is shape smuggling into your own
  protocol state (below).

## Why "Huge Datum" Attacks Are Weaker Than They Look

- All Plutus `Data` accessors are constant-cost; datum size is irrelevant to a validator that
  does not traverse it. `equalsData` is charged on the smaller operand, so a giant datum cannot
  even make a comparison expensive. Only explicit iteration (and `serialiseData`) scale with
  attacker-controlled size.
- Inline datums are self-limiting: min-ada charges roughly 4.4 ADA per KiB, so a 16 KB inline
  datum permanently sinks about 70 ADA.
- The dangerous variant is the **datum hash**: it costs about 1 ADA regardless of preimage size,
  and an output carrying 32 random bytes as its datum hash is unspendable forever (no preimage
  can ever be supplied). Never accept datum-hash outputs as protocol state.

## Where the Real Risk Is

Shape validation cannot detect a well-formed fake datum; authenticating the UTxO (state-thread
or one-shot NFT) is what protects you there. The actual exposure is the **continuing output's
datum** when a validator checks it field by field: an attacker appends extra trailing fields,
they become protocol state, every later spend that traverses the datum pays more, and the state
UTxO can eventually exceed the execution budget. Byte-different datums that decode to the same
typed value also silently fork anything keyed on datum bytes or hashes.

Whole-datum equality already closes this hole: `===` on datums compiles to a byte-exact
`equalsData`, and it is cheap.

```scala
// Field-wise check: extra trailing fields are smuggled through
require(newDatum.owner === datum.owner && newDatum.deadline === datum.deadline)

// Whole-datum check: byte-exact, no smuggling possible
require(newDatum === expectedDatum, "unexpected continuing datum")
```

## Guidance

In priority order:

1. **Authenticate protocol UTxOs with a state-thread or one-shot NFT.** Shape validation cannot
   detect a well-formed fake; this is the defence that matters most.
2. **Check continuing-output datums with whole-datum `===`**, not field by field.
3. **Require inline datums on protocol outputs.** Never accept a datum-hash output as protocol
   state.
4. **Handle the PlutusV3 no-datum branch explicitly** (on V3 the script runs with `None`; on
   V1/V2 a missing datum is a phase-1 rejection).
5. **Bound anything you iterate**: datum-carried lists and token counts in a value. Real UTxOs
   have been bricked by exceeding the memory budget with 150+ assets, well under the size limit.
6. **Validate `ByteString` lengths used as credentials.** Nothing forces a key hash to be 28
   bytes; a zero-length or over-long value can make a required output impossible to build.

## Opt-In Validation

When your logic depends on a decoded structure being well-formed, validate it explicitly at the
point of use:

- `SortedMap` decoded from a datum is not checked for key order by the default `FromData`
  instance; a deliberately mis-ordered map can make lookups miss entries. Decode with
  `sortedMapFromDataWithValidation` when the order matters (see
  [Collections](/docs/language-guide/collections#sortedmap)).
- At PV11, `Value` operations lowered to CIP-153 builtins reject non-canonical values for free
  (see [Value Builtins](/docs/smart-contract-optimisations/value-builtins)).

  A general opt-in deep validator with Aiken-equivalent semantics (an `expect`-style combinator
  that checks constructor tags, exact field counts and nested shapes) is planned. Until then,
  whole-datum `===` plus the guidance above covers the known attack surface.

## See Also

- **[Common Vulnerabilities](/docs/security/common-vulnerabilities)** – Known vulnerability patterns and mitigations
- **[Plutus Data](/docs/smart-contracts/plutus-data)** – How `toData`/`fromData` work
- **[Design Patterns](/docs/design-patterns)** – State-thread NFTs and other structural defences
