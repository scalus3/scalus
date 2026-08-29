# Single-constructor products: unchecked constructor tag, and silent tag rewrite

**Date:** 2026-08-28
**Scope:** `SirToUplcV3Lowering` (the default backend), product (single-constructor case class)
lowering: `ProductCaseEmitter`, `ProdDataListOps`, `DataConstrEmitter`, plus the off-chain
`FromDataMacros` counterpart.
**Method:** code read + end-to-end repro. Every claim marked **confirmed (executed)** was produced
by compiling through the real plugin + lowering and evaluating on the CEK machine.
**Repro:** a scratch suite of 21 tests (`scalus.compiler.tagaudit.ProdConstrTagAuditTest`, modelled
on `PatternMatchAuditTest.scala`) was run green and then discarded rather than committed. Every
lettered test below states its program, input and observed output, so the suite can be rebuilt from
this document alone.

---

## Verdict

The behaviour is real, on the default pipeline. It is two separate defects.

| # | Defect | Status in existing docs | Severity |
|---|--------|-------------------------|----------|
| **P1** | A single-constructor product reads its fields with `sndPair(unConstrData d)`. The constructor tag is never compared. `Constr(k, args)` is accepted for any `k`. | **Documented and decided** – `docs/design/Design-Decisions.md` DD-1 (2026-08-04), "lazy by default, strict by opt-in". | low on its own; it is the enabler for P2 |
| **P2** | Re-encoding that same value emits `constrData(staticIndex, args)`, so the runtime tag is **replaced** by the tag the type declares. `Constr(1, args)` in, `Constr(0, args)` out. | **Partly documented.** DD-1 says the tag is *discarded on read* and never that it is *reinvented on write*; the site's Equality vs Field Reads section (added independently in `1f09ca557`) documents one instance, `inlineOrFail[T] === x`. | **not independently exploitable** – it can only fire on input P1 already accepted. Major *as an amplifier of P1*; see "Is P2 exploitable on its own?" |

P2 is the undocumented part, but it is **not** a standalone vulnerability. Test R shows it never
touches a well-formed value, so it can only fire on input that P1 already let through. The practical
consequence is that **P1 is the thing worth fixing**; fixing P1 removes P2 for free.

---

## What the code does

```
ProductCaseEmitter.scala:15-46    defaultRepresentation(product) = ProdDataList   // List[Data], no tag
ProductCaseEmitter.scala:48-51    defaultDataRepresentation(product) = ProdDataConstr  // packed Data
ProductCaseEmitter.scala:367-381  ProdDataConstr -> ProdDataList : sndPair(unConstrData x)   // tag dropped
ProductCaseEmitter.scala:277-286  ProdDataList -> ProdDataConstr : constrData(retrieveConstrIndex(tp), x)
ProductCaseEmitter.scala:216-250  retrieveConstrIndex: 0 for a standalone case class; the index in the
                                  parent's constructor list for a member of a sealed hierarchy
ProdDataListOps.scala:103-186     genSelect: converts the scrutinee to ProdDataList, then dropList/tailList + headList
ProdDataListOps.scala:203-256     genMatch: a single-case match also converts to ProdDataList first
```

The tag is lost at `ProdDataConstr -> ProdDataList` and reinvented, from the static type, at
`ProdDataList -> ProdDataConstr`. Nothing connects the two.

Crucially, **`ProdDataList` is the default representation for products**, not `ProdDataConstr`
(`ProductCaseEmitter.scala:15-46`). That default is what a non-`inline` function parameter of
product type gets. So the round trip happens exactly at function boundaries.

Sums are different and correct. `DataConstrEmitter.scala:315-319` dispatches on the tag with PV11
`Case`-on-integer, which errors on an out-of-range index, and each branch re-encodes with the tag it
matched. **Confirmed (executed):** a one-case `enum` rejects `Constr(1, …)` with
`Case index 1 out of bounds for 1 branches`, while the equivalent `case class` accepts it.

## What the CEK machine actually does – confirmed (executed)

`case class State(owner: ByteString, counter: BigInt)`, PV11, `Options.default`.

| Test | Program | Input | Output |
|---|---|---|---|
| A | `d.to[State].counter` | `Constr(0/1/99, [B, I 42])` | `42` in all three cases – tag never checked |
| I | `d.to[State] match { case State(_, c) => c }` | `Constr(1/99, …)` | `42` – `match` does not check either |
| J | `d.to[State].counter` | `Data.I(7)` | fails; `unConstrData` is the only guard |
| D | `val s = d.to[State]; s.toData` | `Constr(1, …)` | `Constr(1, …)` – tag **preserved** |
| B | `helper(d.to[State])`, `def helper(s: State) = s.toData` | `Constr(1, …)` | **`Constr(0, …)`** – tag **rewritten** |
| B | same | `Constr(99, [B, I 42])` | `Constr(0, [B, I 42])` |
| B | same | `Constr(1, [B, I 42, I 999])` | `Constr(0, [B, I 42, I 999])` – the extra field **survives**; only the tag is normalised |
| C | `State(s.owner, s.counter).toData` | `Constr(1, …)` | `Constr(0, …)` (expected – explicit rebuild) |
| E | `helper(d.to[State]) == d` | `Constr(0,…)` gives `True`; `Constr(1,…)` gives **`False`** |
| G | the same two programs under `Options.releaseUntagged` (`optimizeUplc = true`) | `Constr(1, …)` | direct `Constr(1,…)`, via helper `Constr(0,…)` – the divergence is **not** an artefact of disabled optimisation |
| F | `enum Shape { case Circle(r); case Square(side) }`, `helper(d.to[Shape.Circle])` | `Constr(1, [I 5])` (a `Square`) | **`Constr(0, [I 5])` – a well-formed `Circle`** |
| K | `d.to[Shape] match { … }` (sum) | `Constr(7, …)` | fails, PV11 `Case` out of bounds; matched values re-encode with their own tag |
| K2 | the same at PV10 (`Options.plomin`) | `Constr(7, …)` | silently decodes as the last constructor – audit finding **R3**, WONTFIX by design (Aiken parity) |

Two rows matter most.

1. **D vs B.** The same source expression `s.toData` emits different bytes depending on whether the
   value crossed a function boundary. Extracting a helper function – a refactor with no semantic
   content – changes what the script writes on-chain. There is no diagnostic.
2. **F.** A `Square` cast to `Circle` does not merely read wrong; it comes back out as a *valid,
   canonical* `Circle`. The laundering turns an ill-typed input into a well-typed one.

## Exactly which source shapes launder – confirmed (executed)

Test O tried the plausible intra-expression round trips. **None of them launder.** With
`Constr(1, [B, I 42])` in:

| Shape | Result |
|---|---|
| `if cond then State(…) else d.to[State]`, then `.toData` | `Constr(1, …)` – both branches stay `ProdDataConstr` |
| `d.to[State] match { case s @ State(_,_) => s.toData }` | `Constr(1, …)` – lowers to the identity `(lam s s)` |
| `List.single(d.to[State]).toData` | `List [Constr(1, …)]` |
| `Wrapper(d.to[State]).toData` | `Constr(0, [Constr(1, …)])` – outer tag correct, inner preserved |
| `val s = d.to[State]; s.toData` (test D) | `Constr(1, …)` |
| **`helper(d.to[State])` for a non-`inline` `def helper(s: State)`** (test B) | **`Constr(0, …)`** |

The emitted UPLC for test B shows why:

```
[(lam reencode (lam d [reencode [sndPair [unConstrData d]]]))   -- caller unpacks
 (lam s      [constrData (con integer 0) s])]                   -- callee rewraps with a literal 0
```

The unpack is in the caller, the rewrap is in the callee, and a lambda separates them. So:

- The laundering trigger is **precisely one thing**: a product value derived from `Data` that is
  passed to (or returned from) a non-`inline` function and then re-encoded.
- A local peephole cannot see across that lambda. This is an important negative result for the fix
  options below.

## Is P2 exploitable on its own? No. That decides the fix.

**R – legitimate values are never corrupted (confirmed, executed).** A real `Shape.Square` (runtime
tag 1), obtained by matching on `Shape` and handed to `def reencodeSquare(sq: Shape.Square)`, comes
back as `Constr(1, [I 5])`. `retrieveConstrIndex` reads the index from the parent's constructor list
and gets 1; the emitted UPLC literally contains `constrData (con integer 1)`.

So the rewrite is only ever observable when the incoming tag differs from the declared one, which
means P1 accepted a wrong tag first. **P2 is never an entry point.** There is no attack that starts
with P2.

### What the amplification does buy Eve

1. **A valid forgery that crosses a trust boundary (test F, executed).** Requires the contract to
   read a concrete variant of a sealed hierarchy, e.g. `d.to[Order.Buy]` instead of matching on
   `Order`. Eve supplies `Constr(1, [I 100])` (a `Sell`). The contract reads it as `Buy(100)`, and a
   helper re-encodes it as `Constr(0, [I 100])` – a *canonical* `Buy`. Any downstream consumer that
   is written correctly (a `match` on `Order`, another script, an off-chain decoder) now accepts the
   forgery. **Without P2, the value stays `Constr(1, …)` and every strict consumer rejects it, so the
   damage stops at the one buggy contract.** This is the whole security value of fixing P2.

2. **An equality collision (test S, executed).** `def sameState(a: State, b: State) = a.toData == b.toData`
   returns `True` for `Constr(0, [B, I 42])` vs `Constr(1, [B, I 42])`, while raw `a == b` on the
   same two values returns `False`. A "the datum did not change" check written over laundered values
   accepts a byte-changed datum. Exploitable only where a third party keys on the bytes (datum
   hashes, indexers, a partner script).

### What it does not buy her

- **No entry.** Eve still needs P1, which is a decided, documented design.
- **No fund drain by itself.** For a standalone case class (no hierarchy) the rewrite maps garbage
  onto the canonical form. Inside one contract that is harmless or mildly corrective.
- **No corruption of honest traffic** (R).
- **Nothing at all** in a contract that never uses `to[ConcreteVariant]` and compares whole datums
  against the raw incoming `Data` (test E is still byte-exact).

### The honest bottom line

If we keep DD-1's lazy read (P1) as the default, P2 is worth fixing for exactly one reason: it stops
our own compiler from *minting* well-formed values out of ill-formed input, which is what lets a
single buggy contract poison correctly written neighbours. If we fix P1 instead (S2), P2 vanishes as
a side effect and this argument is moot.

The non-security reasons stand on their own and are arguably the stronger case:
`s.toData` emitting different bytes depending on whether the value crossed a function boundary
(D vs B) means the emitted `Data` is not a function of the source program. That is an audit and
refactoring hazard regardless of Eve.

## Why it is like this

- **P1 is a decided design.** DD-1 ("Datum shape validation: lazy by default, strict by opt-in",
  2026-08-04) records it explicitly: *"Field access is `sndPair(unConstrData d)` plus list drops.
  Tag discarded, no arity check."* The rationale is that strict boundary decoding reintroduces the
  catalogued **arbitrary-datum bricking** vulnerability, and that shape validation cannot detect a
  well-formed fake anyway (state-thread NFTs are the real defence). Option D (strict by default) was
  rejected; option B (opt-in `Data.expect[T]` / `softCast[T]`, tracked as T9) was chosen; option C
  (shallow tag + arity check) was left open, *"possible later, if profiling demands it"*.
- **P2 was never designed, only observed.** No commit message, scaladoc or design note explains the
  rewrite. It falls out of the representation design: `ProdDataList` is `List[Data]` and
  structurally cannot carry a tag, so the inverse conversion has to invent one, and the only thing
  in scope is the static type. This reads as an oversight, not a decision. Commit `1f09ca557`
  independently hit one instance of it while measuring `hasInlineDatum` vs `inlineOrFail`, and
  documented that case on the site; this analysis generalises it to every product round trip.
- **UPLC constrains the alternatives.** There is no builtin that builds a `pair(integer, list(data))`
  – only `mkPairData :: Data -> Data -> pair(data,data)`. So the existing `PairIntDataList`
  representation can only ever arise as a transient from `unConstrData`; it cannot be a general
  tag-carrying representation for constructed values. **The only tag-carrying carrier for a product
  is `ProdDataConstr` itself.**
- Audit finding **R3** covers the neighbouring sum-side issue (out-of-range tags absorbed by the
  pre-PV11 `else` chain) and is WONTFIX for Aiken parity. Note that the audit's action list (item 6:
  "**DONE** … strict Constr-tag decode on all protocol-version targets") contradicts R3's own status
  block; K2 shows R3's lenient behaviour is still live on the `plomin` target. Those two lines
  should be reconciled.
- Off-chain agrees with on-chain: `FromDataMacros.scala:56` derives `unConstrData(d).snd`, no tag
  check, no arity check. **Confirmed (executed):** the JVM decoder reads `Constr(1, …)` and
  `Constr(1, [.., extra])` as a valid `State`, and `ToData` re-encodes as `Constr(0, …)`. A TxBuilder
  that decodes a datum and rebuilds it launders the tag off-chain too.

## Security consequences

### What is *not* at risk

- **Non-`Constr` data.** `unConstrData` still fails on `I`/`B`/`List`/`Map`, so this is confined to
  `Constr`-shaped inputs.
- **Sums on the default (PV11) target.** Tag-checked by `Case`, and re-encoded faithfully.
- **The `ScriptContext`.** Ledger-produced, always well-formed.
- **Whole-datum `===` against the raw incoming datum.** Still byte-exact (test E). DD-1's primary
  recommended defence is intact when the comparison operand is the untouched `Data`.
- **Direct theft.** Laundering *normalises* attacker garbage. Inside a single contract it is usually
  harmless or even corrective. It is not by itself a fund-drain primitive.

### Where the real exposure is

1. **Type confusion inside a sealed hierarchy (F) – the sharpest case.** `d.to[ConcreteVariant]`
   compiles and runs for any variant's bytes, and re-encodes as the *declared* variant. A validator
   that reads a specific variant directly instead of matching on the parent accepts another
   variant's payload and then hands downstream code a value that is now genuinely, canonically that
   variant. Any later `match` on the parent – in this script, in a sibling script, or off-chain –
   agrees with the forgery. This defeats DD-1's "shape validation cannot detect a well-formed fake"
   argument in an uncomfortable way: here the fake is *manufactured by our own compiler*.
   **Contract-author rule:** never `to[]` a concrete variant of a sealed hierarchy; always `match`
   on the parent type.

2. **Byte-identity contexts.** Two byte-different datums (`Constr(0,…)`, `Constr(1,…)`) denote the
   same typed state on-chain, but hash and serialise differently. Anything keyed on datum bytes –
   `serialiseData`, datum hashes, off-chain indexer keys, a state-fork detector – sees two states
   where the validator sees one. DD-1 already flags this class ("a silent state fork") for extra
   trailing fields; the tag adds a second, cheaper axis, with no size cost at all.

3. **Ecosystem divergence.** Scalus's accepted state space is wider than the CIP-57 blueprint it
   publishes. PlutusTx, Aiken, Plutarch, `cardano-cli` and blueprint-driven JS decoders reject a
   non-zero tag where the schema says constructor 0. An attacker plants a `Constr(1, …)` UTxO at the
   script address for min-ada; the Scalus validator is unbothered, but strict off-chain
   infrastructure built from the blueprint crashes or refuses to build a spend. A cheap
   liveness/griefing vector against everything *around* the contract.

4. **Refactor-instability of on-chain output (D vs B, stable under `release` per G).** Extracting a
   helper changes the bytes a script writes. If those bytes feed a hash, an equality check against a
   partner script, or an indexer, behaviour changes with no source change and no warning. This is
   the part that should worry us most as maintainers: the emitted `Data` is not a function of the
   source program alone.

**Economics.** No new cost vector. A tag is 1–3 bytes and, unlike the trailing-fields variant, has
no min-ada penalty, so planting `Constr(k, …)` costs exactly what any dust UTxO costs.

## How other toolchains handle this

Verified against sources, not documentation: Plinth/PlutusTx `1.63.0.0`, Aiken `v1.1.23` (the current
release), Plutarch `master` (read via the GitHub API; the local clone is 1.2.0 and only used for the
older `punDataSum`), Pebble `main` (`HarmonicLabs/pebble`, 0.3.x).

Two separate questions per toolchain. **(1)** Is the constructor tag checked when a
single-constructor product is read from untrusted `Data`? **(2)** When the value is written back,
does the emitted tag come from the runtime value or from the static type?

| Toolchain | (1) tag checked on read | (2) tag on re-encode | Can it launder? |
|---|---|---|---|
| **Plinth**, default `makeIsDataIndexed` | **Yes**, always, even for one constructor | static | **No** – decode already forced runtime == static |
| **Plinth**, `asData` products | **No** | static | **Yes**, same shape as Scalus |
| **Aiken** | **Yes** at the `Data` boundary (`expect`) | n/a | **No**, structurally impossible |
| **Plutarch** | **No** for a single variant | static | **Yes**, but only on an explicit `pcon` |
| **Pebble** | **No** – and it *warns* that the check is redundant | explicit `toData`/`fromData` IR nodes | mitigated by a deliberately one-directional peephole |
| **Scalus** | **No** | static | **Yes, implicitly**, at non-`inline` function boundaries |

### Plinth (PlutusTx) – checks by default, skips it in `asData`, and says why

`plutus-tx/src/PlutusTx/IsData/TH.hs:49-64` puts an index equality test into *every* generated
decode pattern:

```haskell
ixMatchPat = [p|((PlutusTx.==) (conIx :: Integer) -> True)|]
```

`unsafeFromDataClause` (`:260-306`) dispatches with `caseInteger index kases args` and falls through
to `traceError reconstructCaseError`. So a one-constructor Plinth type still rejects `Constr(1, …)`.
Encoding writes the static index (`mkConstrCreateExpr conIx`), which is sound precisely *because*
decode already pinned it.

The exception is `asData`, where the same file omits the check with an explicit rationale
(`:83-85`):

> *"If generating pattern synonyms for a product type declared with `asData`, we can avoid the index
> match, as we know that the type only has one constructor."*

That is Scalus's P1 verbatim, in the reference implementation. **Ground: performance.** `asData`
exists to avoid decoding at all, and the index match is the first thing dropped. Note also the
neighbouring `-- TODO: safe match for the whole thing? not needed atm`, which is why Plinth accepts
extra trailing fields (already cited in DD-1).

### Aiken – checks once at the boundary, and cannot launder even if it did not

Aiken emits the index clause only when the cast is a real cast from `Data`
(`gen_uplc.rs:385`):

```rust
full_check: !tipo.is_data() && value.tipo().is_data() && kind.is_expect(),
```

and `gen_uplc.rs:1752-1786` gates the tag `when` on
`data_type.constructors.len() > 1 || props.full_check`. So:

- destructuring an already-typed one-constructor value: **no** check (it was validated on entry);
- `expect`ing one out of `Data`: **yes**, the index clause is emitted even for a single constructor.

The second half is the more interesting one for us. `builder.rs:475-520`,
`known_data_to_type`, returns the term **unchanged** for a user data type:

```rust
Some(UplcType::Data) | None => { ... term }
```

An Aiken value of a user type *is* its `Data`. There is no unpacked "list of fields" form for a
function parameter, so there is nothing to rewrap and no static tag to reinvent. **Ground:** the
language-level contract that after `expect` the value *is* a valid `T` and round-trips with
`ToData`; the representation choice then makes the round trip free.

**This is exactly solution S1.** Aiken is the existence proof that the per-value `ProdDataConstr`
representation is workable, and it is why Aiken pays the tag check once instead of never.

### Plutarch – same omission as Scalus, but the rewrite is never implicit

`Plutarch/Repr/Data.hs`, `pmatchDataStruct` (master):

```haskell
case handlers (pmatch (pasConstr # x) $ \(PBuiltinPair _ y) -> y) of
  [(_, h)] -> pure h        -- one constructor: only `snd` is used, index discarded
  _        -> ... groupHandlers (handlers ds) idx
```

and `pconDataStruct` writes the static index:

```haskell
idx = pconstant $ toInteger $ SOP.hindex xs
punsafeCoerce $ pconstrBuiltin # idx #$ builtinList
```

The older `Plutarch/DataRepr/Internal.hs` states the ground outright:

> `-- | If there is only a single variant, then we can safely extract it.`
> `punDataSum = plam $ \t -> punsafeCoerce $ psndBuiltin # (pasConstr #$ pforgetData $ pdata t)`

which is character-for-character our `sndPair(unConstrData d)`.

The difference from Scalus is **not** the encoding, it is **who inserts the round trip**. Plutarch's
`PIsData (PDataSum defs)` has `pfromDataImpl = punsafeCoerce` and `pdataImpl = punsafeCoerce` – both
identity – so a value that is merely passed around never gets rewritten. A tag rewrite requires the
programmer to write `pcon`, an explicit act of construction. Checked accessors exist and are opt-in:
`ptryIndexDataSum` (*"Try getting the nth variant. Errs if it's another variant"*) and `PTryFrom`.

**Ground:** Plutarch is deliberately explicit; every representation conversion is written by hand, so
an implicit one cannot appear.

### Pebble – skips the check, warns you for writing it, and documents the exact asymmetry

`_compileIsExpr.ts` emits a diagnostic when you test the constructor of a one-constructor type:

> `This_check_is_redundant_Struct_0_has_only_one_possible_constructor`

So Pebble is the most explicit of all: not only is the tag not checked, writing the check is a
warning.

More relevant to P2, Pebble carries a data round-trip eliminator
(`IR/toUPLC/subRoutines/eliminateDataRoundTripsAndReturnRoot.ts`) whose header states the rule we
are missing:

> *"Only the ALWAYS-SAFE direction is rewritten – decoding a value that was just encoded (the encoder
> is total, so the decoder cannot fail and is the identity) … The reverse (encode-after-decode, e.g.
> `iData( unIData( d ) )`) is **NOT** rewritten on its own: removing it would drop the decoder's
> validation of untrusted data."*

That is third-party corroboration of the finding, from the opposite direction. A decode/encode round
trip on untrusted data is only benign when the decode validates. Scalus performs
`constrData(0, sndPair(unConstrData d))` where the decode half validates **nothing**, so the round
trip is a normalisation rather than a validated identity.

Pebble's model is SOP internally (`IRConstr`/`IRCase`) with explicit `TirToDataExpr` /
`TirFromDataExpr` at the boundaries plus this peephole – again, conversions are explicit nodes, not
an implicit representation lattice.

### What this means for Scalus

- **On P1 (unchecked read) Scalus is not an outlier.** Plinth-`asData`, Plutarch and Pebble all skip
  the single-constructor tag check, and all three give the same ground: with one constructor the
  check looks redundant. Only Aiken and default Plinth pay for it. DD-1's position is defensible and
  has company.
- **On P2 (the rewrite) Scalus is alone.** Every other toolchain that writes a static index either
  validated the tag first (Plinth, Aiken) or only writes it where the programmer explicitly
  constructs a value (Plutarch, Pebble). Scalus is the only one that inserts the decode/encode round
  trip **implicitly**, as a side effect of parameter-representation selection, on code the author
  never wrote.
- The two toolchains that avoid the problem do it by the two routes already identified: Aiken =
  **S1** (the value is its `Data`; no unpacked parameter form), Plinth/Aiken = **S2** (validate the
  tag, then the static write is sound). Pebble's peephole is **S0** done in an architecture where the
  conversions are explicit IR nodes and therefore visible to it.

## Measurements

All PV11, `Options.default`, `ExUnits(mem, steps)`. Lovelace uses mainnet `priceSteps` 7.21e-5,
`priceMemory` 5.77e-2.

| # | Program | ExUnits |
|---|---|---|
| M | `d.to[State].counter` (product, no tag check) | `(2060, 640237)` |
| M | `d.to[OneCase] match { case Only(_, c) => c }` (1-case enum, PV11 `Case`, tag-checked) | `(3292, 974132)` |
| L | hand-written `fstPair` + `equalsInteger` + `ifThenElse` guard, **marginal cost** | **`(+1033, +354228)`, about 85 lovelace** |
| P | `passThrough(s: State): State = s` then `.toData` | `(2496, 556831)`, emits **`Constr 0`** |
| P | `passThroughData(d: Data): Data = d.to[State].toData` | `(1400, 208100)`, emits **`Constr 1`** |
| P | `counterOf(s: State)` vs `counterOfData(d: Data)` (field read through a helper) | **identical**, `(2660, 736237)` both |
| Q | locally built `State(…)` fed to `counterOf(s: State)` | `(4056, 1172111)` |
| Q | locally built `State(…)` fed to `counterOfData(d: Data)` | `(5452, 1568842)` |

Reading of P and Q, which is the whole trade-off in two lines:

- For a **datum-derived** value, the packed `ProdDataConstr` form is **both correct and cheaper**:
  it saves `(1096, 348731)`, about 88 lovelace, on pass-through, and costs exactly nothing on field
  reads.
- For a **locally constructed** value, `ProdDataList` wins by `(1396, 396731)`, about 109 lovelace,
  because `ProdDataConstr` would force a `constrData` + `unConstrData` + `sndPair` round trip.

So `ProdDataList` is the right default *for values the program builds*, and the wrong default *for
values that arrive as `Data`*. Today one per-type default serves both.

## Viable solutions

### S0 – Peephole on the inverse conversion. **Rejected on evidence.**

The obvious idea is to recognise a `ProdDataList` whose provenance is `sndPair(unConstrData x)` in
`ProductCaseEmitter.emitConvert` and return `x` for the inverse conversion. Test O shows every
intra-expression shape already avoids the round trip, and the emitted UPLC for test B shows the
unpack and the rewrap sit on opposite sides of a lambda. **The peephole would never fire on any
observed case.** Recorded here so it does not get proposed again.

### S1 – Choose the product representation per value, not per type *(fixes P2, no runtime check; this is what Aiken does)*

Make a product value that originates from `Data` keep `ProdDataConstr` across function boundaries,
and keep `ProdDataList` for values the program constructs. `.toData` on the first kind is then the
identity, so the tag is preserved by construction.

Concretely: at the call site, a product argument already in `ProdDataConstr` should not be forced to
the callee's per-type default. That needs either a second lowering of the callee specialised on the
argument representation, or the parameter representation as part of the function's lowering key. The
machinery for representation-keyed lowering already exists (`InOutRepresentationPair`,
`chooseCommonRepresentation`, `RepresentationProxyLoweredValue`).

- Fixes the whole of P2, including the cross-lambda case.
- Costs nothing on field reads and **saves** about 88 lovelace per datum-derived pass-through (P).
- Does not fix P1: `Constr(1, …)` is still *read* as a `State`; it is just written back as
  `Constr(1, …)`. Test F would then yield a `Square` again instead of a forged `Circle`, which is
  strictly better, because the value stops lying about itself.
- Risk: representation-keyed specialisation can duplicate code for functions called with both kinds.
  Needs a size and budget check on `scalus-examples` before landing.

### S2 – Shallow tag check on the read path *(fixes P1 and P2 together; DD-1 option C)*

Emit the tag check once, where `ProdDataConstr -> ProdDataList` happens
(`ProductCaseEmitter.scala:367`). After the check the runtime tag provably equals the static index,
so the rewrap at `:277` becomes correct for free. One change closes both defects.

This is **not** DD-1's rejected option D. Option D validates at the boundary, including datums the
validator never reads, which is exactly what reintroduces arbitrary-datum bricking. A check at the
*projection* site preserves DD-1's key property, *"a wrong-typed datum that the validator never
reads simply never fails"*, because a datum that is never projected is never checked. On that axis
it is strictly safer than Aiken's implicit boundary `expect`.

- Cost: `(+1033, +354228)`, about 85 lovelace per unpacked product value, measured (L). The lowering
  already caches the unpacked list per scope, so it is one check per value, not one per field.
- Implementation is small: reuse `lvCaseInteger` with a single branch on PV11. The one-case-enum
  path already emits exactly this and already produces the right error (`Case index 1 out of bounds
  for 1 branches`). Fall back to `fstPair` + `equalsInteger` + `lvIfThenElse` on PV10.
- Changes script hashes and budgets for every product-reading contract. Needs a release note and
  re-pinned `ExUnits` baselines, on both compiler generations where dual baselines are pinned.
- Should land as a compiler `Option` (`checkProductTags`), default off for one release, then on.

### S3 – Opt-in only: ship T9 `Data.expect[T]` / `softCast[T]` and document

Already the decided plan in DD-1. Necessary regardless, but on its own it leaves P2 live for every
contract that does not call `expect`, and P2 is a codegen-faithfulness issue rather than a
validation-policy issue. **Check when implementing T9:** its spec is written around sum dispatch; it
must also check `tag == 0` for single-constructor *products*, which is exactly the case that has no
dispatch today.

### S4 – Documentation and tooling, alongside whichever of the above

- Add a DD-1 row and paragraph for the rewrite: the tag is discarded on read **and reinvented on
  write** when the value crosses a non-`inline` function boundary.
- `scalus-skills/skills/smart-contract-security-review/references/vulnerabilities.md` has V017
  (arbitrary datum), V019 (unbounded datum), V022 (cheap spam) but nothing on tags. Add: never
  `to[]` a concrete variant of a sealed hierarchy; do not key anything on datum bytes you did not
  receive verbatim.
- Emit a compiler warning at `ProductCaseEmitter.scala:277` when the rewrap target is a literal tag
  and the value's provenance is a `Data` boundary. Cheap, and it names the exact site.

## Decision (2026-08-29): document, do not fix

**Status: WONTFIX for now, documented.** P2 is not independently exploitable (test R), it never
touches well-formed values, and reaching it requires a contract-authoring mistake that the docs now
name explicitly. The comparison above shows Scalus is not an outlier on P1 either. Fixing P2 would
change script hashes for every product-reading contract, which is not worth it for a conditional
risk.

What shipped instead:

- `scalus-site/content/security/datum-validation.mdx` gains a **The Constructor Tag** section: the
  tag is not checked on read, `match` does not check it either, sums do check it at PV11, the
  re-encode uses the declared tag when a value crosses a non-`inline` function boundary, and
  well-formed values are unaffected. It states the two rules (never `to[]` a concrete variant; do
  not key on datum bytes you did not receive verbatim) and the manual guard
  `require(datum.toConstr.fst === BigInt(0), ...)`, measured at about 165 lovelace (test T).
- The same page's toolchain paragraph is corrected with the verified Plinth / Aiken / Plutarch /
  Pebble behaviour.
- `scalus-site/content/smart-contracts/plutus-data.mdx` gains a warning callout where `to[T]` is
  first introduced, linking to that section.
- Guidance list item 3 is the sealed-hierarchy rule.

If this is revisited, the options below stand as analysed; **S2** remains the only one that closes
test F, and **S1** is the Aiken design.

### Options, if revisited

1. **S4 (partly done).** The docs half shipped. The unshipped half is a plugin *warning* on
   `to[ConcreteVariant]`, which would close test F at compile time for zero on-chain cost. This is
   the cheapest remaining action and the natural next step if the item is reopened.
2. **S2 behind an `Options` flag** (`checkProductTags`), about 85 lovelace per unpacked value.
   Fixes P1 and P2 together.
3. **S1**, the per-value representation choice. Removes the forgery-minting capability and is
   *cheaper* than today on pass-through. This is what Aiken does.
4. **S3**, T9 `expect` / `softCast`, with the product case explicitly in its spec.
5. Do not pursue **S0**; test O and the test-B UPLC show it cannot fire.
