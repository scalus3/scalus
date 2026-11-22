---
area: gen
---

# D5 – Shared generators

The owner approved the recommendations on 2026-11-21 ("agree with recommended"), and the `Table`
form of `checkEval` the same day ("ok, start the pilot"). Each rule has *Implementation status:
planned* until the pilot or the rollout builds it. The pilot built `[gen.quality.genstats-classes]`,
`[gen.output.genstats-forms]`, `[gen.runs.checkeval-table]` to `[gen.runs.checkeval-row-report]`,
`[gen.magnitude.range]` to `[gen.magnitude.bit-length]`, `[gen.value.*]`, `[gen.cover.pooled-keys]`
to `[gen.cover.random-members]`, `[gen.family.given-calls-gen]` for `Coin`, `v1.Value` and
`v2.TxOut`, and `[gen.quality.*]` for the four rows of pilot step 8. Rollout unit 1 (`uplc.test`)
built `[gen.cover.three-parts]` and `[gen.cover.encoding-edges]` for `ByteString`, `Data.I` and
the `Constr` tag, `[gen.family.given-calls-gen]` for `ByteString`, `I`, `Data`, `Term` and
`Program`, `[gen.family.program-versions]`, `[gen.family.program-v100-terms]`,
`[gen.runs.edge-table]` for the two `Data` CBOR properties, and three rows of
the threshold test. String constants stay off: the pretty printer does not escape `"`, `\` or
control characters. Rollout unit 2 (`v1` to `v3`) built `[gen.magnitude.users]` for `genAmount`,
`[gen.cover.three-parts]` for `PosixTime` and `Interval`, `[gen.size.caps]` and
`[gen.size.default-caps]` for the inputs, outputs and signatories of `TxInfo`,
`[gen.cover.optional-empty]` and `[gen.cover.no-gen-option]` for its reference inputs,
certificates, withdrawals, redeemers, datums and treasury amounts, and two rows of the threshold
test. Votes and proposal procedures stay empty: no generator of the governance types exists.

This note is step D5 of `[P3-11a]` in `docs/superpowers/specs/2026-10-20-test-refactoring-design.md`.
It amends H-35 and H-36 in `helpers.md`. The measurements and the research are in
`d5-evidence.md`. The implementing session reads these two files only; there is no separate plan.

Every rule ID starts with `gen`. Find a rule with `rg -nF '[gen.value.positive]' .`

## 1. Overview

*Informative.* Property tests in Scalus draw their inputs from shared ScalaCheck generators.
Today one `given Arbitrary[T]` per type serves every purpose, so the inputs often do not suit the
test. For example, 69 % of on-chain `Value` samples hold a negative amount, two `Value` samples
never share a policy, and `Coin` never generates 0.

In this design the one `given` per type generates well-formed values with realistic weights.
Every other purpose gets a named `Gen` with a fixed prefix. Each scalar generator mixes a
realistic core, the listed edges of its type, and a wide part by bit length. Transaction-level
generators cap their size by construction, and they read protocol limits from the parameter
snapshot. Most suites keep 10 runs, so each law also tests its edges as fixed cases. A seeded
test in CI checks each generator against a predicate and minimum shares. A generator change is
proved by a sample diff, three seeds and one mutation. A pilot on `Coin`, `Value`, `TxOut` and
`ValueTest` comes before the rollout.

## 2. Goals and non-goals

*Informative.*

Goals:

- A test gets the inputs that its purpose needs, and the call site names the purpose.
- Ten runs reach the edges and the encoding boundaries of a type.
- A generator change is proved safe before it lands.
- CI time does not grow. This is goal 1 of the test refactoring program.

Non-goals, each with the place where it goes:

| Item | Where it goes |
|---|---|
| A stateful generator of valid transactions (`genValidTx(utxo, params)`, in the style of cardano-ledger `genTx`) | D6, ledger rules kits |
| Seed or print the 665 `.sample.get` values | D6 |
| Malformed and non-canonical CBOR: `twiddle`-style re-encodings that must decode to the same value, and mutated bytes that must be rejected without a crash | `fill-gap` unit after the rollout; the names `genMalformed<T>Bytes` and `twiddle` are reserved |
| Mainnet blocks and ledger vectors as calibration of the realistic core and as seeds for one-field mutations | `fill-gap` unit after the rollout |
| CDDL validation of generated values, as `CddlSpec.hs` does | Open; needs a JVM CDDL validator |
| Owners and signers of generated transactions drawn from `Party` | D6, in `genValidTx`; see the `Party` decision of section 4 |
| F-35: Scalus accepts a stake address as an output address | Main-code fix, apart from D5; the generator fix in the rollout stops `CborSerializationTest` pinning it |

## 3. Definitions

| Term | Meaning |
|---|---|
| Well-formed value | A value that the Scala type and the protocol format allow. It may still break a ledger rule. |
| Well-formed `given` | The one `given Arbitrary[T]` of a shared type. It generates well-formed values. |
| Named generator | A `def`, `lazy val` or `val` of type `Gen[T]`, with a name from the family table of section 6.1. |
| Family | A purpose of generated inputs: well-formed, role, any, invalid, bytes, or evaluating terms. |
| Realistic core | The part of a scalar generator that draws values as they occur on chain. |
| Edge | A listed value of a type where code changes its behaviour, for example an encoding boundary. |
| Wide part | The part of a scalar generator that covers the whole allowed range. |
| Pooled key | A policy id or an asset name drawn from a fixed pool of 10, so that two samples share keys. |
| Transaction-level generator | A generator of a transaction, a transaction body, or a script context (`TxInfo`, `ScriptContext`). |
| Off-chain property | A property whose body runs as Scala code on the JVM, JS or Native. |
| On-chain property | A property run through `checkEval`, which compiles its body to UPLC and evaluates it. |
| Threshold test | `ArbitraryInstancesQualityTest`, the seeded CI test of section 6.7. |

## 4. Approaches

*Informative.* One decision block per decision. The evidence for each `Context:` line is in
`d5-evidence.md`.

### Decision: One given per type for well-formed values, a named generator for every other purpose

Status: Accepted, 2026-11-21
Area: gen
Context: A test needs one of four kinds of input. A round-trip or a law needs well-formed values. A rule test needs values that a ledger rule accepts. A decoder test needs values or bytes that the protocol rejects. An evaluation test needs terms that evaluate. Today one `given` per type serves all four: the 34 % of stake addresses in `TransactionOutput` suit a decoder test and spoil every rule test.
Options:
- A: one `given` per type; a test picks every other family with an explicit `forAll(genX)`. The call site names the family.
- B: one `given` per family, picked by import (`import valid.given`). A changed import changes the values silently, and a mixed import is ambiguous.
- C: wrapper types such as QuickCheck's `Positive`: `forAll((v: Any[Value]) => …)`. Every test unwraps, and the published testkit gains types.
Chosen: A, because the call site shows the family and the published testkit stays small.
Consequences: Adds the rules of section 6.1.

### Decision: Positive amounts in the Value given, signed amounts in named generators

Status: Accepted, 2026-11-21
Area: gen
Context: 69 % of on-chain `Value` samples hold a negative amount, and 60 % of `v2.TxOut` samples hold lovelace ≤ 0. A real output never does. cardano-ledger generates positive `MaryValue` amounts and a signed `MultiAsset` for the mint.
Options:
- A: a positive `given`; signed amounts only in `genMintValue`, `genAnyValue` and `genValueOver`. The negation, subtraction and scaling laws of `ValueTest` must move to `genAnyValue`.
- B: keep the signed `given`. Every rule test and script context gets values that no transaction holds.
Chosen: A, as cardano-ledger does. The survey finds no property that needs a negative amount to pass.
Consequences: Adds the rules of section 6.2.

### Decision: Three parts per scalar generator, with the wide part by bit length

Status: Accepted, 2026-11-21
Area: gen
Context: The D5 draft asked for 80 % realistic values, 10 % edges and 10 % full range. A uniform full range puts almost every sample at the top bit length: a uniform `Coin` in [0, 2^53] has its median at 4.5 × 10^15 lovelace and never generates 0. With random 28-byte policy ids, two values never share a policy (0.00 %). Every `TransactionBody` holds certificates and collateral.
Options:
- Weights: 50 % core, 25 % edges, 25 % wide; or the draft's 80/10/10.
- Wide part: uniform over the range; or a bit length first, as Plutus does.
- Pooled keys: only for the `Value` key generators; or also for hashes, credentials and `TxOutRef`.
Chosen: 50/25/25, the wide part by bit length, and pooled keys for `Value` only. With 10 runs an edge needs weight to be seen. Pooled hashes would make about 1 % of sample pairs collide, and a rules test that assumes distinct hashes would turn flaky until D6 seeds `.sample.get`.
Consequences: Adds the rules of section 6.3 and the helper `genMagnitude`.

### Decision: Caps by construction, with a 10 % tail up to 16 items

Status: Accepted, 2026-11-21
Area: gen
Context: Bounding by protocol parameters is feasible: the limits are sizes and counts that a generator can compute. It is useful only where a ledger rule, the Emulator or an evaluator consumes the value, because an oversized value fails the size rule first and the test passes without reaching the rule under test. It is harmful in codec tests: an encoder must handle every well-formed value, and governance changes the limits. Today `TxInfo` has a median of 18 inputs, and 25 % of samples exceed 16 384 bytes. `Transaction` is discarded at 100 % of sizes 0 to 2.
Options:
- A: caps with a 10 % tail up to 16 items.
- B: the draft's hard cap of 4, plus `genLarge` generators tagged `Slow`.
Chosen: A, because lookups in a list of 10 or more items still get inputs.
Consequences: Adds the rules of section 6.4.

### Decision: Keep 10 runs and test the listed edges as fixed cases

Status: Accepted, 2026-11-21. The `Table` form of `checkEval` (`[gen.runs.checkeval-table]`) replaces the fixed examples of on-chain laws.
Area: gen
Context: Most suites run ScalaTest's default of 10 cases. An edge of weight w is seen in one run with chance 1 − (1 − w)^10: 10 % for w = 1 %, 40 % for w = 5 %, 65 % for w = 10 %. Over many CI runs each edge is reached, but a bug then shows as a one-off red build. CI time is goal 1, so fewer samples is not an option.
Options:
- A: keep 10. A law over a type with listed edges first runs `forAll(Table(edges))`, then `forAll(gen)`. One table per law, no new framework.
- B: raise the default to 100 in `EvalTestKit` and `ScalusTest`. About 10 times the property time, because many property bodies compile and evaluate UPLC. Not measured.
- C: 100 for off-chain properties, 10 for on-chain ones. Two defaults to explain.
- D: ScalaTest's native `Generator`, which runs edges first. A second property framework beside ScalaCheck.
Chosen: A. It makes the edges deterministic, as jqwik's `EdgeCasesMode.FIRST` does, with the `Table` that ScalaTest already has.
Consequences: Adds the rules of section 6.5.

### Decision: The property guards the bounds that shrinking can break

Status: Accepted, 2026-11-21
Area: gen
Context: ScalaCheck 1.19 shrinks by type. A failure of `Gen.choose(1000, 2000)` reports `0`, and `Gen.listOfN(3, Gen.choose(5, 9))` reports `List(0)`. Case classes and `autoDerived` types do not shrink at all. ScalaTest prints the original value and the `Init Seed`, and `testOnly <Suite> -- -S <seed>` repeats the run.
Options: None weighed. ScalaCheck 1.19 has no integrated shrinking, as Hedgehog and Hypothesis have.
Chosen: The property checks its bounds with `whenever`, or a `Shrink` keeps them. The repository already does both: `PositionalMerkleTreeTest` guards with `whenever`, and `FlatTest` turns shrinking off with `noShrink`.
Consequences: Adds the rules of section 6.6.

### Decision: A seeded threshold test in CI, with GenStats as the full report

Status: Accepted, 2026-11-21
Area: gen
Context: The draft put a GenStats gate in a nightly job. With fixed seeds, a test result changes only when a generator changes. `ArbitraryInstancesBoundsTest` already works this way for node counts.
Options:
- A: a seeded threshold test in CI. It cannot be flaky.
- B: GenStats thresholds in a nightly job, as the D5 draft said.
Chosen: A. GenStats stays the full report.
Consequences: Adds the rules of sections 6.7 and 6.8.

### Decision: Each given that D5 adds or changes calls a named generator gen\<T\>

Status: Accepted, 2026-11-21
Area: gen
Context: `arbitrary[T]` already gives a `Gen[T]`, but only where the `given` is in scope. Both chains define `given Arbitrary[Value]` and `given Arbitrary[Credential]`, so one scope cannot import the givens of both chains. Role generators such as `genOutputValue` build on the default generator of their type. The six generator files hold 117 givens, and 25 of them are `autoDerived`. Today 2 givens delegate to a named generator (`Timelock`, `Relay`); the `gen…` vals of `cardano.address` are local to the given bodies. `genMultiAsset(...)` and `genMint(...)` already exist with default parameters, and their givens pass other bounds: 0 to 4 policies, not 1 to 8.
Options:
- A: each `given` that D5 adds or changes calls a named `gen<T>`; any other given gets one on demand. About 10 givens in the pilot and the rollout.
- B: every `given` calls a named `gen<T>`. 117 new members, most of which no test calls.
- C: keep the givens as they are; tests call `arbitrary[T]`. No call without the givens in scope.
Chosen: A. A test can then call `scalus.ledger.api.v1.ArbitraryInstances.genValue` with no given in scope, and a role generator composes the default generator by name. The testkit has no MiMa check, so the new members cost no compatibility work.
Consequences: Adds `[gen.family.given-calls-gen]`, `[gen.family.default-name]`, `[gen.family.existing-gen-name]`, `[gen.family.lazy-gen]` and `[gen.family.gen-on-demand]`, and a row of the family table.

### Decision: The TransactionOutput given covers every output form, and the threshold test checks each form

Status: Accepted, 2026-11-21
Area: gen
Context: The family table said "payment address only, coin ≥ 0" for `Arbitrary[TransactionOutput]`. It meant "no stake address", but a reader can take it as "key outputs only". Today the generators produce every form: in GenStats, 25 % of ledger outputs hold a script reference, 12.6 % an inline datum and 12.2 % a datum hash. Of `v2.TxOut` samples, 33.4 % hold a datum hash and 33.1 % an inline datum. No check keeps these shares: pilot step 6 replaces the value part of `v2.TxOut`, and its threshold row checks tokens only.
Options:
- A: the default `given` keeps every form; the threshold test checks a minimum share per form; a role generator serves a test that needs one form.
- B: one role generator per form, and a default `given` with key outputs only. Every round-trip must then combine the role generators, or it loses the script forms.
Chosen: A, because a round-trip or a law must see every form, and a test that needs one form names it.
Consequences: Adds the rules of section 6.9. Changes the example of the family table, and the `v2.TxOut` row of pilot step 8.

### Decision: Party stays out of the shared generators; D6 uses it for owners and signers

Status: Accepted, 2026-11-21
Area: gen
Context: `Party` is an enum of 20 test parties with HD keys from one test mnemonic, and it has `genParty` and a `given Arbitrary[Party]`. It is in the test sources of `scalus-cardano-ledger`, copied into the testkit, and it needs `HdAccount` from `scalus-cardano-ledger` main. The shared generators are in the test sources of `scalus-core`, which cannot depend on that module. Codec and law tests need key hashes, not signing keys. Rule tests that spend generated outputs need the signing key of each owner.
Options:
- A: keep `Party` out of D5. The D6 `genValidTx` draws the owners and the signers from `Party`.
- B: write the 20 key hashes of `Party` as a credential pool in `scalus-core`, with a ledger test that checks they equal `Party.addrKeyHash`. Gives collisions of credentials; reopens the pooled-keys question, and `.sample.get` rules tests can turn flaky until D6 seeds them.
- C: move `Party` into `scalus-core`. Needs `HdAccount` and the wallet code in `scalus-core`.
- D: generators that use `Party` in the test sources of `scalus-cardano-ledger`, where `genParty` already lives. Possible today; it is where A puts them.
Chosen: A. Inside D5 a pool of key hashes without keys adds only collisions, and the owner chose pooled keys for `Value` only. In D6 the keys let the generated transaction carry real witnesses.
Consequences: Adds a non-goal row for D6. No rule of area `gen` changes.

## 5. Assumptions

- ScalaCheck's `Seed` and `Gen` give the same values on JVM, JS and Native for the same seed and
  size. The generators use no platform code except BLS `hashToGroup`. ScalaCheck makes it true;
  nobody has measured it. `[gen.assume.platform-seeds]`
- The protocol parameters of `blockfrost-params-epoch-645.json` are the limits that matter. The
  maintainer who updates the snapshot after a parameter change makes it true, and the generators
  follow by `[gen.size.params-source]`. `[gen.assume.param-snapshot]`

## 6. Rules

### 6.1 Families and naming

*Informative.* A test picks its family by name. The default `given` serves round-trips and laws;
a named generator serves every other purpose.

| Family | Name | Contents | Example |
|---|---|---|---|
| Well-formed (default) | `given Arbitrary[T]` | Every value that the protocol format allows for `T`, with realistic weights. | `Arbitrary[TransactionOutput]`: every output form (section 6.9), never a stake address (F-35). |
| Default | `gen<T>` | The generator behind the `given`, by `[gen.family.given-calls-gen]`. | `genCoin`, `genValue` |
| Role | `gen<Role><T>` | A sub-domain or a shape that a test names. | `genMintValue`, `genLovelaceValue`, `genSmallInterval` |
| Any | `genAny<T>` | Every value that the Scala type can hold, including values that the protocol rejects. | `genAnyValue`: signed amounts for the group laws of `ValueTest` |
| Invalid | `genInvalid<T>` | Values that break one named rule. | `genInvalidTxOut` with a stake address (F-35) |
| Bytes | `genMalformed<T>Bytes` | Encodings that a decoder must reject. A non-goal of D5. | – |
| Evaluating terms | `TermGen.gen…` | Closed terms with a known result. `TermGen.arithTree` is the first one. | – |

- A shared type MUST have at most one `given Arbitrary`. `[gen.family.one-given]`
- Every other shared generator MUST be a named generator with a prefix from the family table. `[gen.family.named-gen]`
- Existing names that do not follow the table, such as `TermGen.arithTree`, `genPolicyId` and
  `genAmount`, MUST stay. `[gen.family.keep-old-names]`
- A `genInvalid<T>` generator MUST be added only when a negative test needs it. `[gen.family.invalid-on-demand]`
- A test MAY shadow a `given` inside one test body with a named generator:
  `given Arbitrary[Value] = Arbitrary(genAnyValue)`. `[gen.family.local-shadow]`
- A well-formed `given` MUST pass a check written from the specification, not from Scalus
  production code. `[gen.family.spec-check]`
- `Arbitrary[Term]` and `Arbitrary[Program]` stay the syntax family, and the programs MUST
  include the versions 1.0.0 and 1.1.0. `[gen.family.program-versions]`
- A program of version 1.0.0 MUST hold only terms without `Constr` and `Case`. `[gen.family.program-v100-terms]`
- Each `given Arbitrary[T]` that D5 adds or changes MUST delegate to a named generator:
  `given Arbitrary[Coin] = Arbitrary(genCoin)`. `[gen.family.given-calls-gen]`
- The named generator behind a `given` MUST be `gen` plus the simple name of the type,
  with no role word. `[gen.family.default-name]`
- A named generator in a trait MUST be a `def` or a `lazy val`. `[gen.family.lazy-gen]`
- Where a member named `gen<T>` already exists, the `given` MUST keep calling it with
  its own arguments, and a second member of that name MUST NOT be added. `[gen.family.existing-gen-name]`
- Any other `given` MUST get a named generator only when a role generator or a test
  needs one. `[gen.family.gen-on-demand]`

**Why:** `[gen.family.local-shadow]` is the only way to feed a named generator to `checkEval`,
which takes an implicit `Arbitrary` and no `Gen`. `ValueTest` already shadows `BigInt` this way in
`* properties`. `[gen.family.spec-check]` follows constrained-generators: the generator and the
check come from one specification, so they cannot drift. The check must not call Scalus code,
because F-35 shows that Scalus accepts a stake address where the node does not. The threshold test
runs the check, by `[gen.quality.predicate]`. `[gen.family.one-given]` keeps the published testkit
small: a user who mixes in `ScalusTest` sees one instance per type. `[gen.family.program-v100-terms]`: the parser rejects `Constr` and `Case` below version 1.1.0. `[gen.family.lazy-gen]`: a strict `val` that another strict `val` reads during the initialisation
of the trait is still `null`. `Arbitrary.apply` takes its `Gen` by name, so the `given` alone is
safe. `[gen.family.existing-gen-name]`: `genMultiAsset()` gives up to 8 policies and `arbitrary[MultiAsset]` up to 4, and a second `genMultiAsset` with no parameters is ambiguous with the defaults. A change to a `given` that only moves its body into `gen<T>` keeps the values, so `[gen.proof.refactor-md5]` proves it.

*Before and after, illustrative:*

```scala
// before: the negation law gets negative amounts from the only Value generator
checkEval { (value: Value) => -(-value) === value }

// after: the law shadows the given with the family that it needs ([gen.family.local-shadow])
given Arbitrary[Value] = Arbitrary(genAnyValue)
checkEval { (value: Value) => -(-value) === value }

// a rule-shaped test takes the default, which is non-negative
forAll { (out: TxOut) => assert(out.value.getLovelace >= 1) }
```

### 6.2 `Value` amounts

- `Arbitrary[v1.Value]` MUST generate positive amounts only, lovelace included. `[gen.value.positive]`
- The positive amounts MUST be at most 2^64, the bound of `genAmount`. `[gen.value.amount-max]`
- `Arbitrary[v2.TxOut]` MUST always hold a lovelace entry of at least 1. `[gen.value.output-lovelace]`
- A role generator `genOutputValue` MUST build the value of `Arbitrary[v2.TxOut]`. `[gen.value.output-gen]`
- `Arbitrary[v1.Value]` MUST keep the lovelace entry optional. `[gen.value.lovelace-optional]`
- `genMintValue` MUST generate non-zero signed amounts and no lovelace entry. `[gen.value.mint-signed]`
- `genAnyValue` MUST generate signed amounts in the range of `genAmount`, ±2^64. `[gen.value.any-signed]`
- `genValueOver(policies, tokens)` MUST generate signed values whose keys come only from the given
  lists. `[gen.value.over-keys]`
- The two-operand laws of `ValueTest` MUST use `genValueOver` with 2 policies and 2 token names. `[gen.value.two-operand-laws]`
- `Arbitrary[TxInfo]` of `v3` MUST draw `mint` from `genMintValue`. `[gen.value.txinfo-mint]`

**Why:** A canonical `Value` holds no zero amount, so "non-negative" means positive entries. `[gen.value.output-lovelace]`: every real output carries the min-UTxO lovelace, and the v3 script
context of a real transaction always holds its lovelace entry. `[gen.value.two-operand-laws]`: the
`+` law of `ValueTest` iterates over the entries of the sum only. With the 30 % pool of `[gen.cover.pooled-keys]`, a shared (policy, token) pair occurs in about 0.2 % of pairs (the
reviewer's model), too rare for 10 runs. With 2 policies and 2 tokens, two independent draws share
a key in most cases. The ledger `Value` and `MultiAsset` generators are already non-negative, and
the ledger `Mint` is signed.

### 6.3 Coverage of the value space

*Informative.* Each scalar generator mixes three parts. The wide part picks a bit length first,
then a value of that length, so that small and large magnitudes are equally likely. Keyed values
draw part of their keys from a pool, so that two samples share keys. Optional parts are often
absent, as in swarm testing.

- Each scalar generator MUST mix three parts: a realistic core, the listed edges of its type, and
  a wide part over the whole allowed range. `[gen.cover.three-parts]`
- The weights SHOULD be 50 % core, 25 % edges and 25 % wide. `[gen.cover.weights]`
- A generator MAY deviate from these weights when its type has no realistic core, such as a hash,
  or no wide part, such as a `Boolean`. `[gen.cover.weights-exception]`
- The wide part MUST choose a bit length uniformly, then a value of that length. `[gen.cover.wide-by-bits]`
- The edges MUST include each encoding boundary of the type. `[gen.cover.encoding-edges]`
- The policy id and asset name generators of the `Value` `given` MUST draw 30 % of their keys
  from a fixed pool of 10. `[gen.cover.pooled-keys]`
- The pool MUST hold the policy ids `ByteString.fromArray(Array.fill(28)(i.toByte))` and the asset
  names `ByteString.fromString(i.toString)`, for `i` from 0 to 9. `[gen.cover.pool-contents]`
- New members `genPooledPolicyId` and `genPooledAssetName` MUST draw the pooled keys. `[gen.cover.pooled-members]`
- `genPolicyId` and `genAssetName` MUST stay unchanged. `[gen.cover.random-members]`
- A transaction-level generator MUST leave each optional field and each optional collection empty
  in at least 50 % of samples. `[gen.cover.optional-empty]`
- These optional fields MUST use `Gen.frequency`, not `Gen.option`. `[gen.cover.no-gen-option]`
- A generator of a type with cross-field rules, such as `ttl` ≥ validity start, MUST respect them
  in its realistic core. `[gen.cover.cross-field]`

**Why:** `[gen.cover.pooled-keys]`: with random 28-byte ids two values never share a policy, so
the merge in `Value.plus` and every "same key" branch run only in hand-written examples. `[gen.cover.no-gen-option]`: ScalaCheck 1.19 `Gen.option` gives `None` in only 10 % of samples. `[gen.cover.weights]`: see section 6.5. With 10 runs, one of the 6 `Coin` edges has weight
25 % / 6 ≈ 4.2 %, and a run sees it with chance 1 − (1 − 0.042)^10 ≈ 35 %.

*Example, illustrative, edges per type for `[gen.cover.encoding-edges]`:*

| Type | Edges |
|---|---|
| `Data.I` | 0, ±1, ±23, ±24, ±255, ±256, ±2^16, ±2^32, 2^63 − 1, 2^64 − 1, 2^64, −2^64, −2^64 − 1 |
| `Data.Constr` tag | 0, 6, 7, 127, 128, 2^64 − 1 |
| `Data.B`, `ByteString` | 0, 1, 28, 32, 64, 65 bytes |
| `Coin` | 0, 1, 999 999, 1 000 000, 2^53, 4.5 × 10^16 |
| `TxOutRef` index | 0, 1, 255, 256, 65 535 |

*Before and after, illustrative, for `[gen.cover.wide-by-bits]`:*

```scala
// before: median 4.5 × 10^15 lovelace, never 0
given Arbitrary[Coin] = Arbitrary(Gen.choose(0L, pow(2L, 53).toLong).map(Coin.apply))

// after
val MaxLovelace = 45_000_000_000_000_000L // max supply, 45 × 10^9 ADA
given Arbitrary[Coin] = Arbitrary(
  Gen.frequency(
    2 -> Gen.choose(1_000_000L, 100_000_000L),                          // realistic: 1 to 100 ADA
    1 -> Gen.oneOf(0L, 1L, 999_999L, 1_000_000L, 1L << 53, MaxLovelace), // edges
    1 -> genMagnitude(0L, MaxLovelace)                                    // wide, by bit length
  ).map(Coin.apply)
)
```

`MaxLovelace` is a constant of the protocol, not a protocol parameter, so `[gen.size.params-source]` does not apply to it.

The helper `genMagnitude` is the one new helper of this section:

```scala
def genMagnitude(min: BigInt, max: BigInt): Gen[BigInt]
def genMagnitude(min: Long, max: Long): Gen[Long]
```

- `genMagnitude` MUST require `min <= max`. `[gen.magnitude.range]`
- If the range holds both signs, `genMagnitude` MUST pick the negative or the non-negative part
  with equal chance, then work on the absolute values of that part. `[gen.magnitude.sign-split]`
- `genMagnitude` MUST pick a bit length uniformly from the bit lengths of the part, then a value
  uniformly from that bit length, clipped to the part. `[gen.magnitude.bit-length]`
- `genMagnitude` MUST replace the wide branch of `iArb`, `genAmount`, `Coin`, `Slot` and
  `ExUnits`. `[gen.magnitude.users]`

*Example, illustrative:* `genMagnitude(0L, 1000L)` gives each of the bit lengths 0 to 10 with
chance 1/11: 0, 1, 2 to 3, 4 to 7, …, 512 to 1 000. Bit length 0 is the value 0.

**Why:** `Gen.chooseNum` gives the edges but not the bands. The `Pointer` generator of
`cardano.address` already uses bands for its own type.

### 6.4 Size and protocol limits

*Informative.* The bound of a generator depends on the code that consumes its values. The current
limits in the snapshot are `maxTxSize` 16 384, `maxValueSize` 5 000, `maxTxExecutionUnits`
16.5 × 10^6 memory and 10^10 steps, and `maxCollateralInputs` 3.

| Consumer | Bound | How |
|---|---|---|
| CBOR, Flat, `toData` round-trips | Format only (Word16 index, 28/32-byte hashes) | In the `given` |
| Ledger rules, Emulator, `TxBuilder`, script contexts | Protocol limits | Caps by construction, then one rescale and retry, as `genMultiAsset` does |
| A test of the size rule itself | Above the limit | A named `genOversized<T>` |
| A rule test whose rule is not a size rule | Fit the parameters to the value | As `genUtxoExecContext` does |

- A generator for codec round-trips MUST apply format bounds only, not protocol limits. `[gen.size.codec-unbounded]`
- A transaction-level generator MUST cap its collections by construction. `[gen.size.caps]`
- The default caps SHOULD be 1 to 4 inputs, outputs and signatories in 90 % of samples, and 5 to
  16 in 10 %. `[gen.size.default-caps]`
- A generator that applies a protocol limit MUST read it from
  `CardanoInfo.mainnet.protocolParams`, not from a literal. `[gen.size.params-source]`
- A generator MUST NOT use `suchThat` for a size limit. `[gen.size.no-such-that]`
- A sum type derived with `autoDerived` MUST define a `Fallback`, so that sizes 0 to 4 give a
  value. `[gen.size.derived-fallback]`
- A generator for more items than the default caps MUST be a named `genLarge<T>`. `[gen.size.large-named]`
- A suite that uses a `genLarge<T>` generator MUST be tagged `Slow`. `[gen.size.large-slow]`

**Why:** `[gen.size.codec-unbounded]`: an encoder must handle every well-formed value, and the
limits are protocol policy. `[gen.size.default-caps]` keeps a tail instead of a hard cap of 4.
Today `TxInfo` costs 15.4 s per 10 000 samples. `[gen.size.derived-fallback]` is the H-36 finding:
`ArbitraryDerivation` falls back to `Gen.fail`.

### 6.5 Run count and edges

- The default run count MUST stay 10. `[gen.runs.default-ten]`
- A property over a type with listed edges MUST first run its edges as a ScalaTest `Table`, then
  the random cases. `[gen.runs.edge-table]`
- `EvalTestKit` MUST provide a `checkEval` form that takes a `Table` of edges, for 1, 2
  and 3 arguments. `[gen.runs.checkeval-table]`
- This form MUST compile the property once, evaluate it on every row, then run the
  random cases. `[gen.runs.checkeval-once]`
- When a row fails, this form MUST report the row and its arguments. `[gen.runs.checkeval-row-report]`

**Why:** `[gen.runs.edge-table]` makes the edges deterministic: each run tests each edge, whatever
its weight. `[gen.runs.checkeval-once]`: `checkEval` already compiles its body once and evaluates
each argument with no budget pin, so a row costs one evaluation. A fixed example through
`assertEvalEq` instead adds a budget pin per row. The existing fixed examples of `ValueTest` stay;
they test results and budgets, not laws. A helper `forAllWithEdges` comes only if 5 or more
off-chain suites repeat the two calls of `[gen.runs.edge-table]`.

*Example, illustrative, the `+` law of `ValueTest` with edges:*

```scala
val sumEdges = Table(
  ("value1", "value2"),
  (Value.zero, Value.zero),                                   // both empty
  (Value.lovelace(1), Value.lovelace(-1)),                    // amounts cancel
  (Value(policyA, tokenA, 1), Value(policyA, tokenB, 1)),     // same policy, other token
  (Value(policyA, tokenA, BigInt(2).pow(64)), Value(policyA, tokenA, 1)) // past 2^64
)
checkEval(sumEdges) { (value1: Value, value2: Value) => sumHoldsEveryKey(value1, value2) }
```

The pilot names the form. If an overload of `checkEval` breaks the type inference of the lambda,
the form gets its own name, such as `checkEvalWithEdges`.

### 6.6 Shrinking and reproducibility

- A property that depends on a bound of a scalar or collection generator MUST check the bound with
  `whenever`, or use a `Shrink` that keeps the bound. `[gen.shrink.guard-bounds]`
- A shared `Shrink` MUST keep the well-formed invariants of its type. `[gen.shrink.keep-invariants]`
- A generator MUST produce the same values for the same seed and size on every platform. `[gen.shrink.same-values]`

**Why:** ScalaCheck 1.19 has no integrated shrinking, so the property guards its own
preconditions. `[gen.shrink.same-values]` relies on `[gen.assume.platform-seeds]`. The unseeded
`.sample.get` calls are a D6 item.

### 6.7 Threshold test

- A seeded test `ArbitraryInstancesQualityTest` MUST run in CI on the JVM. `[gen.quality.ci-test]`
- The threshold test MUST live in `scalus-core/jvm/src/test/scala/scalus/testing/`, outside the
  testkit copy list. `[gen.quality.location]`
- The threshold test MUST sample 5 000 values per generator at size 100, from `Seed(42)` with
  `seed.slide`, as `ArbitraryInstancesBoundsTest` does. `[gen.quality.sampling]`
- For each well-formed `given`, the threshold test MUST check the predicate of `[gen.family.spec-check]` on every sample. `[gen.quality.predicate]`
- The threshold test MUST check a minimum share for each listed edge class. `[gen.quality.edge-shares]`
- The minimum share MUST be half of the expected share. `[gen.quality.edge-share-min]`
- The threshold test MUST fail when a generator discards more than 5 % of samples at sizes 5 to
  100. `[gen.quality.discard-limit]`
- `GenStatsTest` MUST report the new edge classes: `Constr` tag 0 and ≥ 128, the 2^64 boundary,
  shared keys between two samples, and CBOR size against the protocol limits. `[gen.quality.genstats-classes]`

**Why:** with fixed seeds the result changes only when a generator changes, so the test cannot be
flaky. QuickCheck's `checkCoverage` and Hypothesis's health checks make the same checks part of a
property.

### 6.8 Proof of a generator change

- A change that keeps the values MUST show an identical md5 of 1 000 seeded samples before and
  after, the method of F-16 and F-29. `[gen.proof.refactor-md5]`
- A change of distribution MUST show three things. `[gen.proof.new-distribution]`
  1. The GenStats diff, without the `time` field, shows only the intended rows.
  2. `jvm/testOnly * -- -S <seed>` passes for 3 seeds.
  3. One mutation of main code that the old generator missed now fails a property with the default
     run count on 3 of 3 seeds. The mutation is then reverted.

*Example, illustrative, a candidate that pilot step 9 verifies:* the mutation makes `Value.plus`
keep the left token map when both operands hold the same non-ADA policy. The two-operand laws of
`ValueTest` (`+ properties`, `- properties`) draw independent values, which today never share such
a policy, so they cannot fail. No `ValueTest` law uses one value twice. With `genValueOver`
(`[gen.value.two-operand-laws]`) the two-operand `+` law is expected to fail on every seed.

### 6.9 Output forms

*Informative.* An output is a key output or a script output, it carries no datum, a
datum hash or an inline datum, and it may carry a script reference. A round-trip or a law must see
every form, so the default `given` keeps them all, and the threshold test keeps their shares.

- `Arbitrary[TransactionOutput]` MUST generate key and script payment credentials,
  the Shelley and the Babbage form, no datum, a datum hash, an inline datum, and outputs with and
  without a script reference. `[gen.output.ledger-forms]`
- `Arbitrary[v2.TxOut]` MUST generate key and script payment credentials, each case of
  `OutputDatum`, and outputs with and without a reference script. `[gen.output.onchain-forms]`
- `GenStatsTest` MUST report each form of `[gen.output.ledger-forms]` and `[gen.output.onchain-forms]` as its own class, with the payment credential apart from the staking credential. `[gen.output.genstats-forms]`
- The threshold test MUST check a minimum share for each form, equal to half of its
  share in the "before" GenStats of pilot step 1. `[gen.output.form-shares]`
- A test that needs one form MUST take a role generator, such as `genScriptOutput`. `[gen.output.role-gen]`
- A role generator of one form MUST be added only when a test needs it. `[gen.output.role-on-demand]`

**Why:** `[gen.output.form-shares]` takes today's shares as the floor, because no mainnet
measurement of the forms exists yet. The calibration from mainnet is a non-goal of D5. The forms
of the Shelley output are fewer by type: it holds no inline datum and no script reference.

## 7. Pilot

*Informative.* The pilot covers `Coin`, `v1.Value`, `v2.TxOut`, the `uplc.test` companion (F-8)
and `ValueTest`. Order matters: step 1 records "before".

1. Extend `GenStatsTest` per `[gen.quality.genstats-classes]` and `[gen.output.genstats-forms]`. Run it with
   `scalusJVM/testOnly scalus.testing.GenStatsTest -- -n scalus.testing.Benchmark` and commit
   `gen-stats.md` as "before".
2. Add `object ArbitraryInstances extends ArbitraryInstances` to `uplc.test` (F-8).
3. Add the `Table` form of `checkEval` to `EvalTestKit` per `[gen.runs.checkeval-table]`,
   with a test that a failing row reports its arguments. Add `genMagnitude` to `uplc.test.ArbitraryInstances`. The pilot does not change `iArb`,
   `genAmount`, `Slot` or `ExUnits`; the rollout does.
4. `Coin` (ledger): three parts per `[gen.cover.three-parts]`, as the example of section 6.3.
   The body moves into `genCoin` per `[gen.family.given-calls-gen]`; the same holds
   for the `given` of steps 5 and 6 (`genValue`, `genTxOut`).
5. `v1.Value`: non-negative `given`, `genMintValue`, `genAnyValue`; pooled policy ids and asset
   names per `[gen.cover.pooled-keys]`. `v3.TxInfo` mint from `genMintValue`. The published members
   `genAmount`, `genToken`, `genAsset` and `genLovelace` stay as they are (signed); the new `given`
   uses new non-negative parts, so no member changes meaning.
6. `v2.TxOut`: value from `genOutputValue` per `[gen.value.output-gen]`; the address stays
   `autoDerived`.
7. `ValueTest`:
   - `unary_ properties`, `* properties` and the one-operand laws of `+ properties` and
     `- properties` shadow the `given` with `genAnyValue` per `[gen.family.local-shadow]`.
   - The two-operand laws of `+ properties` and `- properties` shadow it with
     `genValueOver(2 policies, 2 tokens)` per `[gen.value.two-operand-laws]`.
   - The two-operand laws also check that every key of `value1` and `value2` appears in the sum
     unless the amounts cancel.
   - Each law first runs an edge table per `[gen.runs.edge-table]`, through the
     `Table` form of `[gen.runs.checkeval-table]`, as the example of section 6.5.
8. `ArbitraryInstancesQualityTest` with the rows below. Import each chain with a scoped
   `import ….ArbitraryInstances.given`, as `GenStatsTest` does: both chains define
   `given Arbitrary[Credential]` and `given Arbitrary[Value]`, so a mix-in of both is ambiguous.

   | Generator | Predicate on every sample | Edge class: minimum share |
   |---|---|---|
   | `Coin` | 0 ≤ coin ≤ `MaxLovelace` | coin = 0: 2 %; coin = `MaxLovelace`: 2 %; 1 to 100 ADA: 25 % |
   | `v1.Value` | canonical (ascending keys, no zero amount, no empty token map); policy id 0 or 28 bytes; token name at most 32 bytes; 1 ≤ amount ≤ 2^64 | empty value: 2 %; lovelace only: 10 %; 2 or more policies: 10 %; two consecutive samples share a non-ADA policy: 1 % |
   | `v2.TxOut` | the `v1.Value` predicate; lovelace ≥ 1 | a token besides lovelace: 25 %; script payment credential, each `OutputDatum` case, and a reference script, each at half of its share in the "before" GenStats (`[gen.output.form-shares]`) |
   | `genMintValue` | canonical; no lovelace entry; no zero amount | a negative amount: 25 % |

   The shares are first estimates. Step 8 measures them. If a share is below its minimum, the
   generator is wrong or the estimate is, and the commit message says which.
9. Run the mutation of `[gen.proof.new-distribution]`, record the result in the commit message,
   and revert it.

Acceptance:

- GenStats diff, without the `time` field: only rows whose type contains `Coin` or `v1.Value`
  change. Today these are, in the ledger group, `Coin`, `Value`, `TransactionOutput`,
  `Certificate`, `GovAction`, `TransactionBody`, `Transaction`, and, in the on-chain group,
  `v1.Value`, `v2.TxOut`, `TxInInfo`, `TxInfo`. Confirm the list with a grep of the generator
  files before the run.
- `jvm/testOnly * -- -S <seed>` passes for 3 seeds. This covers the properties only: `-S` does not
  reach the 665 `.sample.get` calls in 43 files, which draw their own random seed.
- For the `.sample.get` calls: edit the `Coin` given temporarily to `Gen.const(Coin(0))`, run
  `jvm/testFull`, then repeat with `Gen.const(Coin(MaxLovelace))`. Revert the edit.
  - A test that fails gets a local named generator at its call site, for example a positive
    `Coin`. Its assertion does not change.
  - If more than 10 tests fail in one run, stop and report to the owner before fixing.
- `ValueTest` passes on JVM, JS and Native.
- All new members are in files of the testkit copy list; the list does not change.
- CHANGELOG, `### Changed`, one entry: the testkit generators changed their distribution.
  `Arbitrary[Value]` (on-chain) no longer generates negative amounts (use `genAnyValue` or
  `genMintValue`); `Arbitrary[TxOut]` always holds lovelace ≥ 1; the `mint` of `Arbitrary[TxInfo]`
  is signed and holds no lovelace; `Arbitrary[Coin]` now generates 0 and values up to the max
  supply. Not an `### Upgrading` item: no build or binary breaks, but a user test that samples
  `Coin` can now see 0.

Rollout after the pilot, one unit per file, in this order: `uplc.test` (`Data` tags and edges,
`ByteString`, `Program` versions, String/BLS constants), `v1` to `v3` (`Interval`, `TxInfo` caps
and fields), `cardano.address`, `cardano.ledger` (`ExUnits`, `TransactionOutput` per F-35,
`TransactionBody` per `[gen.cover.optional-empty]` and `[gen.cover.cross-field]`, `Fallback` per `[gen.size.derived-fallback]`).

## 8. Open questions

*Informative.* The owner answered each question of the D5 draft on 2026-11-21 with the
recommendation.

| Question | Decided by |
|---|---|
| Is the well-formed family the right default for the `given`? | `[gen.family.one-given]` |
| Which option for the run count? | `[gen.runs.default-ten]`, `[gen.runs.edge-table]` |
| A threshold test in CI, or GenStats thresholds in a nightly job? | `[gen.quality.ci-test]` |
| Caps with a 10 % tail up to 16, or a hard cap of 4 plus `genLarge` tagged `Slow`? | `[gen.size.default-caps]` |
| Weights 50/25/25, or 80/10/10? | `[gen.cover.weights]` |
| Pooled keys only for the `Value` key generators, or also for hashes, credentials and `TxOutRef`? | `[gen.cover.pooled-keys]` |
| CDDL validation of generated values | Still open; needs a JVM CDDL validator |
| Does each `given` call a named `gen<T>`? | `[gen.family.given-calls-gen]`, `[gen.family.gen-on-demand]` |
| Does `Arbitrary[TransactionOutput]` miss script outputs, datums and script references? | No, today it generates them; `[gen.output.ledger-forms]` and `[gen.output.form-shares]` keep them |
| Do the shared generators use `Party`? | No, D6 does; see the `Party` decision of section 4 |
| Does `checkEval` get a `Table` form? | Yes, `[gen.runs.checkeval-table]` |
