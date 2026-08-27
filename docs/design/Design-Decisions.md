# Scalus Design Decisions

A register of cross-cutting design decisions: what we chose, what we rejected, and why.
Each entry is self-contained and dated. Append new decisions at the end.

---

## DD-1. Datum shape validation: lazy by default, strict by opt-in

**Date:** 2026-08-04
**Status:** Decided. Implementation of the opt-in half is tracked as
[T9 in `CODEGEN_IMPROVEMENT_PLAN.md`](../internal/CODEGEN_IMPROVEMENT_PLAN.md).
**Related:** finding R3 of the internal UPLC correctness audit (WONTFIX, Aiken parity).

### Decision

Scalus keeps its lazy, no-op `fromData` as the default on-chain data representation.
We do **not** adopt Aiken's strict-decode-at-the-boundary model as the default.

We will add an **opt-in** deep validation combinator with Aiken-equivalent semantics
(`expect` / `softCast`), and document the two places where a contract author actually
needs it.

### The question

Since the default on-chain representation became `Data`, with `toData`/`fromData`
compiled to nothing, any datum can be cast to any type and its fields read without a
check that the datum really has that shape. Aiken instead verifies, on
`expect s: SomeType = datum`, that the constructor tag matches and that the field
count is **exactly** as declared, rejecting extra trailing fields.

The motivating worry: an attacker builds an output whose datum looks like `SomeType`
but carries extra fields, bloating the datum until the UTxO becomes unspendable.

### What Scalus does today

| Behaviour | Where |
| --- | --- |
| User sealed traits/enums default to `SumCaseClassRepresentation.DataConstr` | `SirTypeUplcGenerator.scala:369`, `DataConstrEmitter.scala:31-46` |
| `fromData[T](d)` is a pure retag, emitting no code | `Lowering.scala:1042-1065` |
| The linker erases every `FromData` application, including hand-written validating instances | `SIRLinker.scala:138-159` |
| Field access is `sndPair(unConstrData d)` plus list drops. Tag discarded, no arity check | `ProdDataListOps.scala:103-195` |
| Enum match on PV11 uses `Case`, so out-of-range tags **do** fail | `DataConstrEmitter.scala:315-319` |
| Pre-PV11 the last constructor is an unconditioned `else`, so out-of-range tags are absorbed | `DataConstrEmitter.scala:320-364` |
| `===` lowers to whole-tree `equalsData`, so it is byte-exact | `LoweringEq.scala:358-375` |
| The validator boundary retags the whole `ScriptContext` | `Validator.scala:11-13` |
| Off-chain derived `FromData` also skips the arity check | `FromDataMacros.scala:95-101` |

Net: extra trailing fields are silently ignored by field projection, but they **do**
break `===`, because `equalsData` compares the full `Constr(tag, args)`.

### How other toolchains compare

| Toolchain | Tag check | Exact arity | Deep | Default at boundary |
| --- | --- | --- | --- | --- |
| Aiken `expect` | yes | yes | yes, `Data` fields stop recursion | implicit `expect` on typed datum and redeemer; `ScriptContext` trusted |
| Plutarch `PTryFrom` | yes | yes | yes, `PData` stops recursion | opt-in |
| PlutusTx derived `IsData` | yes | **no** | no | strict decode of declared fields |
| plu-ts `pmatch` | multi-ctor only | no | no | trust the datum |
| **Scalus** | PV11 match only | **no** | no | trust the datum, project lazily |

Aiken's exact-arity check is real and deliberate: `list_access_to_uplc` emits
`chooseList (tailList fields) then error` for the last declared field, and zero-field
constructors go through `Air::FieldsEmpty`. Its motivation is a language-level
contract, that after `expect` the value *is* a valid `T` and round-trips with
`ToData`. Aiken has repeatedly closed shallow-check holes rather than relaxing them.

Notably, PlutusTx accepts extra trailing fields in **both** its safe and unsafe derived
decoders, and its source carries the comment
`-- TODO: safe match for the whole thing? not needed atm`. Scalus is in the PlutusTx
camp here, not the Aiken camp.

### Why the described attack is weaker than it looks

Three facts from the Plutus cost model and the ledger rules bound it.

**1. Inline datums do not re-enter the spending transaction.** A hashed datum must be
supplied in full in the spender's witness set, and counts against the 16,384-byte
transaction limit. An inline datum is read straight from the UTxO and costs the
spender zero bytes.

**2. All `Data` accessors are constant-cost.** From `builtinCostModelC.json`:

```
unConstrData 24588   unListData 25933   headList 83150   tailList 81663
sndPair     141992   chooseData 94375   unIData  20744   nullList  74433   -- all constant_cost
equalsData   {intercept 898148, slope 27279}   -- min_size
serialiseData {intercept 955506, slope 213312} -- linear_in_x
```

Datum size is therefore irrelevant to a validator that does not traverse it.
`equalsData` is charged on the **smaller** operand (`minCostStream` drains the shorter
side), so a giant datum cannot even make a comparison expensive. Only `serialiseData`
and explicit iteration scale with attacker-controlled size.

**3. CBOR decoding is never charged to the script.** `mkTermToEvaluate` injects the
datum, redeemer and context as pre-decoded UPLC constants; the decode happened in
ledger phase 1.

**Conclusion:** the budget attack is always a validator that *iterates* a
datum-carried list, never the datum's raw size.

### The economics, and where they fail

Verified against mainnet epoch 647 (PV11): `max_tx_size` 16384,
`max_tx_ex_mem` 16,500,000, `max_tx_ex_steps` 10,000,000,000,
`coins_per_utxo_size` 4310, `max_val_size` 5000. There is **no datum-size protocol
parameter**; `max_val_size` covers the value only.

Min-ada is `(160 + serialisedSize(TxOut)) * coinsPerUTxOByte`, so:

- **Inline datum: about 4.41 ADA per KiB.** A 16 KB inline datum sinks roughly 70 ADA
  permanently. Self-limiting.
- **Datum hash: about 1 ADA regardless of preimage size**, because
  `datum_option = [0, hash32]` is about 35 bytes. Min-ada does not bound this variant
  at all. An attacker can plant a UTxO for under 2 ADA whose spender must carry a
  ~16 KB preimage, consuming nearly the whole transaction budget.

For scale, a script running at the full execution budget costs about 1.67 ADA.

### Bricking primitives worth knowing

Both read directly from `cardano-ledger`; neither appears to be written up publicly.

1. **Datum-hash bricking still works on Plutus V3.** CIP-69 relaxed only the `NoDatum`
   case; the `DatumHash` branch of `getInputDataHashesTxBody` is not language-gated.
   An output carrying 32 random bytes as a datum hash can never satisfy
   `inputHashes ⊆ dom(txdats)` and is unspendable forever. Still live in the Dijkstra
   era.
2. **Missing datum on V1/V2 is a phase-1 rejection**, so the script never runs and no
   collateral is taken. On V3 the script runs with `None`, which validators must
   handle deliberately.

### The decisive argument: strict-by-default would add a vulnerability

MLabs' catalogued **arbitrary-datum** vulnerability states that a mismatched datum type
causes future consumption to fail *"even if the validator wouldn't have examined the
datum content"*, with impacts listed as unspendable outputs and protocol halting.

That bricking class exists **because** PlutusTx and Aiken decode strictly at the
boundary. Scalus's lazy retag is immune to it: a wrong-typed datum that the validator
never reads simply never fails. Aiken's implicit boundary `expect` is therefore *more*
exposed to arbitrary-datum bricking than Scalus is.

So the trade is not "Aiken safe, Scalus unsafe". It is two different exposures:

- Strict boundary decoding buys shape guarantees at the cost of robustness against
  arbitrary-datum bricking.
- Lazy projection keeps that robustness, leaving shape smuggling into one's own state
  as the residual risk.

Making validation the default would trade a real, catalogued vulnerability class for a
hypothetical one that has **no published exploit, CVE, or audit finding anywhere** in
the ecosystem.

### Where the residual risk actually is

Not the input datum. Shape validation cannot detect a well-formed fake; that is what
state-thread NFTs are for. The exposure is the **continuing output's datum** when a
validator checks it field by field rather than comparing the whole value.

An attacker who can drive a transaction appends extra fields; those fields become
protocol state, and every later spend that traverses the datum pays more, until the
state UTxO can no longer be spent within the execution budget. A second effect is that
two byte-different datums decode to the same typed value, so anything keyed on datum
bytes or hashes sees a silent state fork.

Whole-datum `===` already closes this, since it is byte-exact `equalsData` and is
cheap. Field-wise output checks are the hole.

### Approaches considered

| # | Approach | Cost | Verdict |
| --- | --- | --- | --- |
| A | Status quo plus documentation | zero | necessary, not sufficient |
| B | Opt-in derived `expect[T]` / `softCast[T]`, deep and arity-exact | pay per use | **chosen** |
| C | Shallow check only (tag plus top-level arity) | cheaper | possible later, if profiling demands it |
| D | Strict by default at the validator boundary | every datum and redeemer, every run | **rejected**, reintroduces arbitrary-datum bricking |
| E | Off-chain guards: TxBuilder/Emulator datum-size warnings, blueprint schema checks | free on-chain | worth doing alongside |
| F | Security-review rules and documentation | zero | worth doing alongside |

### Actions

1. Implement T9 as `Data.expect[T]` (fails) and `Data.softCast[T]: Option[T]`
   (recoverable, matching Aiken's `if/is`). Semantics: tag dispatch via PV11 `Case`,
   recursive field validation, exact arity via `chooseList` on the tail including
   zero-field constructors, `Data`-typed fields stop the recursion, one hoisted cached
   decoder per type.
2. Add the same arity check to `FromDataMacros.scala:95`, so off-chain decoding and
   on-chain `expect` agree on what a valid `T` is.
3. Add two entries to
   `scalus-skills/skills/smart-contract-security-review/references/vulnerabilities.md`,
   which currently has V017 arbitrary datum, V019 unbounded datum and V022 cheap spam,
   but nothing on extra trailing fields or on the datum-hash bricking primitive.
4. Optionally emit an Emulator/TxBuilder warning when an output datum exceeds a
   configurable size.

### Guidance for contract authors

In priority order:

1. **Authenticate UTxOs with a state-thread or one-shot NFT.** Shape validation cannot
   detect a well-formed fake. This is the ecosystem-standard defence and the one that
   matters most.
2. **Check continuing-output datums with whole-datum `===`, or with `expect`.**
   Field-by-field checks are where extra fields get smuggled into protocol state.
3. **Require inline datums on protocol outputs.** Never accept a datum-hash output as
   protocol state.
4. **Handle the V3 no-datum branch explicitly.**
5. **Bound anything you iterate**: datum-carried lists, and token counts in a value.
   `cardano-node#3360` bricked a real UTxO with 150+ assets across 20+ policies by
   exceeding the memory limit, not the size limit.
6. **Validate `ByteString` lengths used as credentials.** Nothing forces a key hash to
   be 28 bytes; a zero-length or over-long value can make a required output impossible
   to build, locking the UTxO.

### References

- Aiken: `gen_uplc/builder.rs` (`list_access_to_uplc`), `ast.rs`
  (`into_script_context_handler`), <https://aiken-lang.org/language-tour/control-flow>
- Plutarch `PTryFrom`:
  <https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/src/Typeclasses/PTryFrom.md>
- PlutusTx `plutus-tx/src/PlutusTx/IsData/TH.hs`
- MLabs vulnerability guide: <https://mlabs.city/blog/common-plutus-security-vulnerabilities>
- Plutonomicon state-thread pattern: <https://plutonomicon.github.io/plutonomicon/statethread>
- Vacuumlabs "Trust No UTxO":
  <https://medium.com/@vacuumlabs_auditing/cardano-vulnerabilities-3-trust-no-utxo-b252650ac2b9>
- CIP-32 (inline datums), CIP-69 (optional datum), CIP-117 (V3 return value)
- `cardano-node#3360` (ExUnits brick), `cardano-cli#501`, `cardano-db-sync#1076`
