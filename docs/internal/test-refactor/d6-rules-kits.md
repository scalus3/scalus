---
area: kit
---

# D6 – Ledger rules test kits

Status: accepted, 2026-11-24. The owner accepted every recommendation: Q1 to Q5 as recommended in
section 9. Q6 came from the pilot; the owner accepted its sum bound, with a `RedeemerUnits` given in place of the member override. Every rule has *Implementation status: planned* until its pilot step or rollout unit
lands.

This note is step D6 of `[P3-11a]` in `docs/superpowers/specs/2026-10-20-test-refactoring-design.md`.
It amends H-23, H-24, H-25, H-38 and H-50 in `helpers.md`, and it takes the items that the D5 note
(`d5-generators.md`) gave to D6. There is no separate plan.

Every rule ID starts with `kit`. Find a rule with `rg -nF '[kit.context.one-builder]' .`

## 1. Overview

*Informative.* The ledger rule tests live in `scalus.cardano.ledger.rules`: 39 suites in
`scalus-cardano-ledger/shared` and 2 in `jvm`, 13 432 lines. Two styles exist side by side.

The certificate, withdrawal and governance suites use fixed values. Each of 13 suites builds its
own 9-line `Context(UtxoEnv(...))` on preprod, its own empty transaction with certificates, and
often its own pool registration. The script and witness suites mix in `ValidatorRulesTestKit`.
They use `Context()` on mainnet, a random transaction from `randomTransactionWithIsValidField`,
random keys from `generateKeyPair()`, and 531 unseeded `.sample.get` calls. A failure of such a
test cannot be reproduced, because each call draws its own seed.

In this design the certificate suites share one `CertsTestKit`, and the script suites get
builders for a spending transaction and a script UTxO in `ValidatorRulesTestKit`. Rule tests use
fixed values and `Party` keys. A test that claims a law over all values becomes a `forAll`
property, which ScalaTest reports with its seed. `ValidatorRulesTestKit` bounds `ExUnits` to the
protocol limit, as a guard for law tests. A pilot on two certificate suites and one script suite
comes before the rollout.

## 2. Goals and non-goals

*Informative.* The rule suites take 7.3 s of CI in total (`timings.csv`), so CI time is not the
goal here. The goals are, in program order: reproducible failures (coverage), one way to build a
rule test (uniformity), and fewer copies of the context and transaction setup (duplication).

Non-goals, each with the place where it goes:

| Item | Where it goes |
|---|---|
| A stateful generator of valid transactions, `genValidTx(utxo, params)`, that D5 gave to D6 | The `to-property` step of `[P3-8]`; owner question Q2 |
| The 82 `.sample.get` calls in `scalus.cardano.txbuilder` (jvm tests) | The `adopt-helper` unit of that package |
| H-38 `StreamTestKit` (stream and hub suites, not rule suites) | D8, module-local kits |
| The `ArbitraryDerivation.split` fix at size 0 | Owner question Q3; no D6 rule depends on it |
| One fee-overflow policy | Owner question Q4; no D6 rule depends on it |
| F-35: an output address that is a stake address (`MissingKeyHashesValidatorTest` builds such outputs today) | Main-code fix, apart from D6 |

## 3. Definitions

| Term | Meaning |
|---|---|
| Rule suite | A test suite in the package `scalus.cardano.ledger.rules`. |
| Certificate suite | A rule suite that builds a `Context` with a `CertState`: the 13 suites of section 4, decision 1. |
| Script suite | A rule suite that mixes in `ValidatorRulesTestKit`: 21 suites today. |
| Example test | A test whose name states one case, such as "withdrawal fails when reward account is not registered". |
| Law test | A test whose name or body claims a property of every value, such as "any transaction without redeemers passes". |
| Random transaction | A value of `randomTransactionWithIsValidField`: `Arbitrary[Transaction]` with `isValid = true`. |

## 4. Approaches

*Informative.* One decision block per decision. The counts come from `grep` over
`scalus-cardano-ledger/{shared,jvm}/src/test/scala/scalus/cardano/ledger/rules/` on 2026-11-23.
Several counts in `helpers.md` are out of date; this note gives today's numbers.

### Decision: A separate CertsTestKit trait, without generators

Status: Accepted
Area: kit
Context: 13 certificate suites each define a context: `CertsValidatorTest`, `CertsMutatorTest`, `CertsMutatorBookkeepingTest`, `CertsMutatorCommitteeTest`, `CertsMutatorDRepExpiryTest`, `CertsMutatorVrfKeyHashesTest`, `StakeCertificatesValidatorTest`, `StakeCertificatesMutatorTest`, `StakePoolCertificatesValidatorTest`, `AccountRegistrationTest`, `WithdrawalsTest`, `WithdrawalsDelegatedValidatorTest`, `GovMutatorTest`. `TreasuryValueMismatchValidatorTest` and `ScriptsWellFormedValidatorTest` have the same 9 lines. H-24 counted 9. None of them mixes in `ValidatorRulesTestKit`, and none calls `.sample.get`. `Certificate.PoolRegistration(...)` is written out 9 times in 8 files.
Options:
- A: H-24 as proposed: a trait `CertsTestKit` next to `ValidatorRulesTestKit`, with no `ArbitraryInstances`.
- B: put the builders into `ValidatorRulesTestKit`. The certificate suites then inherit the givens of the `ArbitraryInstances` chain (89 in the ledger file alone) that they never use, and any local given can turn ambiguous.
- C: a Scala `object` that suites import. Same content as A; a trait lets a suite override `params`.
Chosen: A.
Consequences: Adds `[kit.certs.trait]` and the rules of section 6.2.

### Decision: One context builder that takes a slot

Status: Accepted
Area: kit
Context: The 15 copies differ in five things only. 3 suites use the first slot of an epoch (`firstSlotOfEpoch`) and 12 use slot 0. 5 change the protocol major, 2 change another parameter (`dRepActivity`, `govActionLifetime`), 2 use their own slot configuration with epochs of 100 slots, 1 sets the treasury. All start from `CardanoInfo.preprod` parameters and use `Network.Testnet`. On preprod, epoch 0 is not slot 0: `zeroSlot` is 86 400 and `zeroEpoch` is 4. H-24 proposed `context(certState, epoch, protocolMajor)`, which would move the 12 suites at slot 0 to another slot.
Options:
- A: `context(certState, slot, protocolMajor, treasury)` with defaults (`CertState.empty`, 0, the major of `params`, `Coin.zero`), plus `epochStart(epoch): SlotNo`. `params` and `slotConfig` are trait members that a suite may override. A pure refactor: no test gets another context.
- B: H-24 as written, by epoch. Changes the slot of 11 suites. Probably harmless, but it is not a pure refactor, and the coverage gate cannot tell.
Chosen: A. Default parameters are allowed here: the interop rule against them covers the published API, not test sources.
Consequences: Adds `[kit.context.one-builder]` to `[kit.context.overridable]`.

### Decision: ValidatorRulesTestKit bounds the generated ExUnits to the protocol limit

Status: Accepted
Area: kit
Context: The D5 rollout gave `Arbitrary[ExUnits]` a wide part up to `Long.MaxValue`, and added `genScriptExUnits`, bounded by `maxTxExecutionUnits` of the parameter snapshot. `FeesOkValidatorTest` calls `genScriptExUnits` by name. The 13 direct `arbitrary[ExUnits]` calls in the rule suites are all in `ExactSetOfRedeemersValidatorTest`, which must keep the wide generator. So a new given that only serves direct calls reaches no call site. The given matters through the redeemers of `Arbitrary[Transaction]` (`ArbitraryInstances.scala:589`, `:597`), which the 71 random transactions draw. Those calls resolve inside the trait `ArbitraryInstances`, so a given that a suite declares reaches them only if the generators take it as a `using` parameter. The pilot found that this given cannot be an `Arbitrary[ExUnits]` (see `[kit.exunits.bounded-given]`). The D5 reason ("units above the limit make `ExUnits.fee` throw") no longer holds: commit `05a84b24e` made the fee exact. The reason today is a guard, not an observed failure: a generated transaction whose redeemers exceed `maxTxExecutionUnits` fails `ExUnitsTooBigValidator` in any test that runs that rule, so a law test would fail for a rule it does not test. No current test shows this: the random transactions of `FeesOkValidatorTest` and `ExUnitsTooBigValidatorTest` set their redeemers explicitly.
Options:
- A: an override in `ValidatorRulesTestKit` that delegates to `genScriptExUnits`. Every `ExUnits` that a script suite draws, also inside a generated transaction, stays within the limit. `ExactSetOfRedeemersValidatorTest` draws from `genExUnits` by name, which needs no implicit resolution.
- B: a second given with its own name. Reaches the direct calls only, which is no useful call site. A second anonymous given may clash with the synthesized name of the inherited one.
- C: no given; each test that needs bounded units calls `genScriptExUnits` by name. No hidden change, but the random transactions keep their wide redeemers.
Chosen: A, in the form of Q6: a `given RedeemerUnits` in the kit that the redeemer generators take as a `using` parameter, with each value up to a third of the limit. An `override given Arbitrary[ExUnits]` changed implicit resolution, see `[kit.exunits.bounded-given]`.
Consequences: Adds `[kit.exunits.bounded-given]` and `[kit.exunits.wide-by-name]`. Until the rollout removes the random transactions, A bounds their redeemers. After it, A has no consumer until a law test draws a transaction, for example in the `to-property` step (Q2). The given stays as that guard.

### Decision: Fixed values for example tests, forAll for law tests

Status: Accepted
Area: kit
Context: The rule suites hold 531 unseeded `.sample.get` calls in 19 files (467 shared, 64 jvm). `MissingOrExtraScriptHashesValidatorTest` has 105, `ExactSetOfRedeemersValidatorTest` 94, `MissingKeyHashesValidatorTest` 74, `PlutusScriptsTransactionMutatorTest` 64, `FeesOkValidatorTest` 53. Each call draws its own seed, so ScalaTest's `-S <seed>` does not reach it, and a red build cannot be repeated. By name, every test of the five largest suites is an example test ("... rule success", "... failure with missing Mint in witnesses"): it draws an input or a hash only to have one, then sets the fields that the rule reads. No rule suite calls `forAll` today. H-25 already proposed fixed transactions for the 13 users it counted; today `randomTransactionWithIsValidField` has 71 call sites in 14 files.
Options:
- A: an example test takes fixed values from kit builders (`input(n)`, `keyHash(n)`, `Party`); a law test becomes a `forAll` over a named generator. ScalaTest prints the seed of a failing `forAll`.
- B: keep the calls and seed them: a helper `sampleSeeded(gen)` with a seed from the suite and test name, printed on failure. Few edits. A fixed seed gives a fixed value, so the test is an example test with its value hidden.
- C: turn every test into `forAll`. 10 runs of tests that do not claim a law, and each property needs bounds that the example never needed.
Chosen: A. The value that a test depends on is then visible at the call site, and the law tests get real coverage with a reported seed.
Consequences: Adds `[kit.values.fixed-example]` to `[kit.values.no-random-tx]`, and retires `randomTransactionWithIsValidField` (H-25). Cost: a random transaction reaches main-code statements by chance, for example in the CBOR encoder of a field that the rule never reads. A fixed transaction can lose them, and `[CG-3]` then blocks the unit. The unit adds a fixed field or a law test for each lost statement, or names the other suite that covers it.

### Decision: Party keys replace generateKeyPair

Status: Accepted
Area: kit
Context: `generateKeyPair()` has 74 call sites in 7 rule suites (24 in `MissingKeyHashesValidatorTest`, 17 in `NativeScriptsValidatorTest`, 16 in `FeesOkValidatorTest`, 12 in `VerifiedSignaturesInWitnessesValidatorTest`). Each call gives a new random key pair, and the tests sign with `platform.signEd25519(privateKey, tx.id)`. `Party` (H-18) has 20 parties with HD keys from one test mnemonic, `addrKeyHash` and a `signer`. Its `account.paymentKeyPair.sign(message)` signs any message, so a test can still sign a body other than its own. The bootstrap-witness tests need a chain code and Byron attributes; the pilot checks whether an HD key serves them. The D5 decision "Party stays out of the shared generators" gave `Party` owners and signers to D6.
Options:
- A: `Party` for every key in a rule test. Deterministic keys, readable names (`Alice.addrKeyHash`). 20 keys per test at most.
- B: keep `generateKeyPair()`. Random keys keep the failures unrepeatable, against the previous decision.
Chosen: A. A test that needs more than 20 keys keeps `KeyPairGenerator` and says why in a comment.
Consequences: Adds `[kit.keys.party]` and retires `generateKeyPair` (H-25).

### Decision: H-50 stays in D6, H-38 moves to D8

Status: Accepted
Area: kit
Context: `helpers.md` unit 4 groups H-23, H-24, H-25, H-38 and H-50. H-50 (`CardanoLedgerVectors.checkVector`) runs the ledger rules against the cardano-ledger vectors. Since `helpers.md` was written, the JVM suite `CardanoLedgerConformanceTest` gained a map of expected failures and the spec tests `[CV-1]` and `[CV-2]`; the IT copy still runs its own loop without that map. H-38 (`StreamTestKit`) serves the stream and hub suites in `scalus.cardano.node.stream`, which run no ledger rule.
Options:
- A: H-50 in D6 as its own rollout unit, re-scoped to today's JVM suite; H-38 to D8.
- B: both in D6, as `helpers.md` groups them.
Chosen: A. D6 then holds only the code that tests the ledger rules.
Consequences: Adds `[kit.vectors.check-vector]`.

## 5. Assumptions

- The preprod parameters of `CardanoInfo.preprod` are the parameters that the certificate suites
  need. The maintainer who updates the snapshot keeps them current. `[kit.assume.preprod-params]`
- `Party` derives the same 20 keys on the JVM and on JS, the two platforms of
  `scalus-cardano-ledger`. The HD wallet code makes it true; no test checks it yet. `[kit.assume.party-keys]`

## 6. Rules

### 6.1 Kits and their place

- `CertsTestKit` MUST be a trait in
  `scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/ledger/rules/`. `[kit.certs.trait]`
- `CertsTestKit` MUST NOT extend `ArbitraryInstances`. `[kit.certs.no-generators]`
- `ValidatorRulesTestKit` MUST stay a trait that extends `ArbitraryInstances`. `[kit.rules.keeps-generators]`
- A rule suite MUST NOT define its own context builder, transaction builder or pool registration
  when a kit member builds the same value. `[kit.rules.no-local-copy]`

**Why:** `[kit.certs.no-generators]`: the certificate suites use fixed values only, and the
inherited givens of the `ArbitraryInstances` chain (89 in the ledger file alone) would make each
local given a candidate for ambiguity.

### 6.2 Context and transaction builders

*Informative.* `CertsTestKit` holds what the certificate suites copy today: the parameters, the
deposits, a context, a transaction with certificates and withdrawals, a pool registration and an
account state. `ValidatorRulesTestKit` gains the builders of H-23 for the script suites.

- `CertsTestKit` MUST provide `context(certState, slot, protocolMajor, treasury): Context` on
  `Network.Testnet`, from its members `params` and `slotConfig`. `[kit.context.one-builder]`
- The members `params` and `slotConfig` MUST default to `CardanoInfo.preprod`. `[kit.context.preprod]`
- A certificate suite MAY override `params` and `slotConfig`. `[kit.context.overridable]`
- The defaults of `context` MUST be `CertState.empty`, slot 0, the protocol major of `params`, and
  `Coin.zero`. `[kit.context.defaults]`
- `CertsTestKit` MUST provide `epochStart(epoch: Long): SlotNo` from its `slotConfig`. `[kit.context.epoch-start]`
- `CertsTestKit` MUST provide `certTx(certs: Certificate*)` and
  `certTx(withdrawals: Map[RewardAccount, Coin], certs: Certificate*)`, each with no inputs, no
  outputs and fee 0. `[kit.certs.cert-tx]`
- `certTx(certs*)` MUST set no withdrawals, and `certTx(withdrawals, certs*)` MUST set
  `Some(Withdrawals(...))`, also for an empty map, as the copies did. `[kit.certs.cert-tx-withdrawals]`
- `CertsTestKit` MUST provide `poolRegistration(operator, rewardAccount, cost, pledge, owners,
  vrfKeyHash)`, with margin 1, no relays and no metadata. `[kit.certs.pool-registration]`
- The defaults of `poolRegistration` MUST be the minimum pool cost of `params`, a pledge of 1 ada,
  no owners and the VRF key hash `"a" * 64`. `[kit.certs.pool-defaults]`
- `CertsTestKit` MUST provide `accountState(balance, pool, drep): ConwayAccountState` with the key
  deposit of `params`. `[kit.certs.account-state]`
- `CertsTestKit` MUST provide `keyDeposit` and `certState(accounts, pools, dreps)`. `[kit.certs.state]`
- `ValidatorRulesTestKit` MUST provide `scriptAddress(hash)` and `scriptUtxo(hash, datum, scriptRef)`
  as H-23 lists them. `[kit.rules.script-utxo]`
- `ValidatorRulesTestKit` MUST provide `txSpending(inputs*)(witnessSet)` as H-23 lists it. `[kit.rules.tx-spending]`
- `ValidatorRulesTestKit` MUST provide `validate(validator, utxos, tx)`, which runs the validator on
  `Context()` and `State(utxos)`. `[kit.rules.validate]`
- A suite whose tests need another context, such as `OutsideValidityIntervalValidatorTest` with its
  7 slots, MUST call the validator directly with that context. `[kit.rules.own-context]`
- `RuleTestValues` MUST provide `input(n: Int): TransactionInput`, distinct for each `n`. `[kit.values.input]`
- `RuleTestValues`, a trait without generators that both kits mix in, MUST provide `input(n)`,
  `addrKeyHash(n)`, `scriptHash(n)`, `poolKeyHash(n)`, `stakeKeyHash(n)`, `govActionId(n)`,
  `keyAddress(n, network)`, `rewardAccount(n, network)`, `scriptRewardAccount(hash)`,
  `scriptAddress(hash)`, `emptyBody`, `anchor`, `proposal(action, account, deposit)` and
  `mintOne(policy)`. Each `n` gives a distinct value; the default network is `Network.Testnet`,
  the network of `Context()`. `[kit.values.fixed]`
- `CertsTestKit` MUST provide `govTx(proposals, votes, certs)`. `[kit.certs.gov-tx]`
- A suite MUST NOT define a member with the name of a kit member that has default arguments:
  Scala rejects two overloads with defaults. `[kit.values.no-default-overload]`

  *Implementation status (simplify pass, 2026-11-24):* the value builders first lived in
  `ValidatorRulesTestKit`, so the certificate suites copied them. `TxBuilderCompleteTest` and
  `TxBuilderPerformanceTest` mixed in that kit only for `ArbitraryInstances`; they now mix in
  `ArbitraryInstances`, and their own `input` and `scriptUtxo` no longer clash. `scriptUtxo` takes
  the input and returns a pair, not the `Utxo` of H-23, so that `Map(scriptUtxo(...))` gives the
  `Utxos` of `validate`.

**Why:** `[kit.context.defaults]` makes the refactor pure: each of the 15 copies maps to one call
with the same slot, parameters and treasury. `[kit.context.epoch-start]` keeps the 3 epoch-based
suites (`StakePoolCertificatesValidatorTest`, `CertsMutatorDRepExpiryTest`, `GovMutatorTest`) on
the slot they use today. `[kit.context.overridable]` serves `CertsMutatorDRepExpiryTest` and
`GovMutatorTest`, which use epochs of 100 slots and changed parameters. H-24 named the type `AccountState`; the code type is
`ConwayAccountState`.

*Before and after, illustrative,* `CertsValidatorTest`:

```scala
// before: 11 lines of context and 18 lines of transaction in the suite
private def mkContext(certState: CertState): Context =
    new Context(
      env = UtxoEnv(
        slot = 0, params = protocolParams, certState = certState,
        network = Network.Testnet, treasury = Coin.zero
      ),
      slotConfig = cardanoInfo.slotConfig
    )
private def runValidator(withdrawals: SortedMap[RewardAccount, Coin], certs: Seq[Certificate] = Seq.empty,
    certState: CertState = CertState.empty) = { /* TransactionBody(...) */ }

// after: the suite mixes in CertsTestKit
class CertsValidatorTest extends AnyFunSuite, CertsTestKit {
    private def run(state: CertState, withdrawals: Map[RewardAccount, Coin], certs: Certificate*) =
        CertsValidator.validate(context(state), State(certState = state), certTx(withdrawals, certs*))
```

### 6.3 Generated values in rule tests

- `ValidatorRulesTestKit` MUST declare `given RedeemerUnits = RedeemerUnits.perTx(3)`, so that the
  redeemers of one generated transaction fit in `maxTxExecutionUnits` together. `[kit.exunits.bounded-given]`

  *Implementation status (pilot, 2026-11-24):* the first form, an `override given` in
  `ValidatorRulesTestKit`, broke 10 calls in `MissingKeyHashesValidatorTest`. A given in a
  subtrait outranks every inherited given, so `genMapOfSizeFromArbitrary(0, 4)` inferred
  `Map[ExUnits, ExUnits]`. The second form overrode a `protected def exUnitsGen` and bounded each
  value, not the sum (Q6). The owner chose a given at the use site instead. `RedeemerUnits` is a
  type of its own, so no call infers it. Its default, `genExUnits`, is in its companion, so a
  suite's given wins. The 6 generators from `Redeemer` to `BlockFile` take it as a `using`
  parameter. `Arbitrary[ExUnits]` stays wide.
- A rule suite MUST NOT override a given of `ArbitraryInstances`. `[kit.exunits.no-given-override]`
- `ExactSetOfRedeemersValidatorTest` MUST draw its units from `genExUnits` by name. `[kit.exunits.wide-by-name]`
- Rollout unit 6 MUST turn that draw into a `forAll(genExUnits)` per test: the verdict of the
  set-of-redeemers rule does not depend on the units, which is a law. `[kit.exunits.wide-forall]`
- An example test in a rule suite MUST use fixed values, not `.sample.get`. `[kit.values.fixed-example]`
- A law test in a rule suite MUST be a `forAll` over a named generator or a `given`. `[kit.values.law-forall]`
- A rule suite MUST NOT call `randomTransactionWithIsValidField`. `[kit.values.no-random-tx]`
- A rule test MUST take each signing key from `Party`. `[kit.keys.party]`
- A test that needs more than 20 keys MAY use `KeyPairGenerator`, with a comment that says
  why. `[kit.keys.over-twenty]`

**Why:** `[kit.exunits.bounded-given]`: a transaction under test must fail only the rule under test;
see the decision of section 4. `[kit.exunits.wide-by-name]`: the set-of-redeemers rule ignores the
units, so the wide part costs nothing and keeps the full range covered. `[kit.values.fixed-example]`
and `[kit.values.law-forall]`: every failure in a rule suite then has either a visible value or a
printed seed.

*Before and after, illustrative,* `ExUnitsTooBigValidatorTest`:

```scala
// before: a random transaction with one field replaced, a context on mainnet
val tx = randomTransactionWithIsValidField.withWitness(
  TransactionWitnessSet(redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))))
assert(ExUnitsTooBigValidator.validate(Context(), State(), tx).isRight)

// after: a fixed spending transaction; the redeemer is the only field the rule reads
val tx = txSpending(input(0))(
  TransactionWitnessSet(redeemers = Some(KeepRaw(Redeemers(Redeemer(Spend, 0, Data.unit, exUnits))))))
assert(validate(ExUnitsTooBigValidator, Map.empty, tx).isRight)
```

### 6.4 Retirements and vectors

- The unit that removes the last caller of `withNetwork`, `randomTransactionWithIsValidField` or
  `generateKeyPair` MUST delete that member from `ValidatorRulesTestKit`. `[kit.rules.retire-h25]`
- `CardanoLedgerVectors` MUST provide `checkVector(name, mode): List[String]`. `[kit.vectors.check-vector]`
- The JVM and the IT conformance suites MUST both call `checkVector`. `[kit.vectors.both-suites]`
- The IT suite MUST keep its `EvaluatorMode.Validate`. `[kit.vectors.it-mode]`

**Why:** `[kit.vectors.check-vector]`: the IT copy fails with a `pprint` of every case, the JVM
suite with a list of mismatches; one function gives both the same report.

## 7. Pilot

*Informative.* `[P3-11a]` names 2 certificate suites. These have no `.sample.get`, no `Context()`
and no `ValidatorRulesTestKit`, so they pilot `CertsTestKit` only. Owner question Q1 asks whether
to add one script suite, so that the pilot also covers H-23, the `ExUnits` override, fixed values
and `Party`. `VerifiedSignaturesInWitnessesValidatorTest` has all four: 144 lines, 12
`.sample.get`, 4 random transactions, 4 `Context()` and 12 key pairs. The steps below assume
yes; without it, step 3 drops out.

1. `CertsTestKit` per section 6.2, with `CertsValidatorTest` and
   `StakePoolCertificatesValidatorTest` moved onto it. `StakePoolCertificatesValidatorTest` covers
   `epochStart` and `poolRegistration`.
2. The `ExUnits` override `[kit.exunits.bounded-given]` and `[kit.exunits.wide-by-name]`.
3. `VerifiedSignaturesInWitnessesValidatorTest` on `txSpending`, `input(n)`, `validate` and
   `Party`. Its two bootstrap-witness tests show whether an HD key of `Party` can make a Byron
   witness. If not, they keep `KeyPairGenerator` with a comment that says why, and this note
   gains a rule for that exception.

Acceptance:

- Each pilot suite has the same test names and the same assertions as before (`[AG-8]`).
- The coverage gate `[CG-1]` to `[CG-3]` shows no lost statement in `scalus.cardano.ledger.rules`.
- A test checks that `Arbitrary[ExUnits]` inside a suite that mixes in `ValidatorRulesTestKit`
  never exceeds `maxTxExecutionUnits`, and that `arbitrary[Transaction]` there draws redeemers
  within it. The test compiles and passes on Scala 3.3.9, 3.9.0 and 3.10.1.
- `scalusCardanoLedgerJS/Test/compile` passes, because the kits are in `shared/`. The module has
  no Native build.
- The line count of the three suites drops; the commit message gives the numbers.

*Pilot result, 2026-11-24 (commit "test: D6 pilot").* Every acceptance item holds.

| Item | Result |
|---|---|
| Same test names and assertions | Yes, in all 3 suites |
| Tests | 42 pilot tests pass; 505 tests of every suite that mixes in a kit pass |
| Coverage gate | The 3 suites lose 0 statements in the ledger module and 685 in `scalus-core`, all from the random transactions (CBOR of `ProtocolParamUpdate`, `Address`, `Data`). The full `scalusJVM` and `scalusCardanoLedgerJVM` run still covers all 685, so no statement is lost. |
| `ExUnits` bound | Passes on 3.3.9; the bound moved to `given RedeemerUnits`, see `[kit.exunits.bounded-given]`. The test sums the redeemers of 300 transactions with fixed seeds |
| Over-limit paths after the bound | Still covered in the full run: the throw of `Coin.exact`, the failure branch of `ExUnitsTooBigValidator`, both saturation branches of `CostingInteger.satPlus` |
| `scalusCardanoLedgerJS/Test/compile` | Passes |
| Line counts | 100 to 55, 179 to 141, 144 to 88 |
| Bootstrap witnesses with `Party` | Work: the rule checks only the Ed25519 signature, so no `KeyPairGenerator` exception is needed |

Two suites in `scalus.cardano.txbuilder` (jvm) also mix in `ValidatorRulesTestKit` and define
their own `input(index)`. They now mark it `override` and keep their values.

The coverage gate ran as a comparison, not with `coverage-run.sh`: that script and `coverage.py`
look for `*/target/scala-*/scoverage-report`, but sbt 2 writes
`target/out/jvm/scala-<v>/<project>/scoverage-report`. The baseline also predates many main-code
changes, so its positions no longer match.

## 8. Rollout

*Informative.* One unit per group of files, each at most about 800 changed lines (`[P4-3]`), in
this order. The `.sample.get` units are the largest; they split by file. Three suites need no
unit: `ProposalDepositValidatorTest` and `TxRefScriptsSizeValidatorTest` already build fixed
values with no copied context, and `VerifiedSignaturesInWitnessesPlatformTest` has no setup to
share.

| # | Unit | Suites |
|---|---|---|
| 1 | Certificate suites onto `CertsTestKit` | the other 11 certificate suites, plus `TreasuryValueMismatchValidatorTest` and `ScriptsWellFormedValidatorTest` for `context` |
| 2 | Small script suites: fixed values, `Party`, H-23 builders | `EmptyInputs…`, `ExUnitsTooBig…`, `InputsAndReferenceInputsDisjoint…`, `TooManyCollateralInputs…`, `TransactionSize…`, `WrongNetwork*`, `OutputsHave*`, `OutputBootAddrAttrsSize…`, `OutsideValidityInterval…` (own contexts, `[kit.rules.own-context]`), `AllInputsMustBeInUtxo…`, `Metadata…` |
| 3 | `MissingKeyHashesValidatorTest` | 74 `.sample.get`, 24 key pairs |
| 4 | `MissingRequiredDatumsValidatorTest`, `NativeScriptsValidatorTest`, `ValueNotConservedUTxOValidatorTest` | 68 `.sample.get` |
| 5 | `FeesOkValidatorTest` | 53 `.sample.get`, 16 key pairs |
| 6 | `ExactSetOfRedeemersValidatorTest` | 94 `.sample.get` |
| 7 | `MissingOrExtraScriptHashesValidatorTest` | 105 `.sample.get`; may split in two |
| 8 | `PlutusScriptsTransactionMutatorTest` (jvm) | 64 `.sample.get` |
| 9 | H-25 deletions per `[kit.rules.retire-h25]`, if units 2 to 8 left callers to it | `ValidatorRulesTestKit` |
| 10 | H-50 `checkVector` | the JVM and IT conformance suites |

*Implementation status, unit 1 (2026-11-24): done.* The 13 suites mix in `CertsTestKit`; 2899 lines
became 2606. The 443 rule tests keep their names and pass. Kept local:
the transaction builders of `GovMutatorTest` and `TreasuryValueMismatchValidatorTest` (votes,
proposals, treasury value), certificate states with committee keys, dormant epochs or a zero
deposit. Three suites added proposals with `certTx(...).withBody(...)`; `govTx` replaced them. A suite's own `params`,
`keyDeposit`, `context` or `poolRegistration` clashes with the kit member, so it goes or gets
another name.

*Implementation status, units 2 to 10 (2026-11-24): done.* Each unit is one commit. The rule
suites hold no `.sample.get`, no `randomTransactionWithIsValidField` and no `generateKeyPair`; the
only random draws left are the 13 `forAll(genExUnits)` law tests of unit 6. The 443 rule tests keep
their names and pass. What the fixed values changed:

- Unit 3: `RegCert` and `UnregCert` always carry a deposit. A `RegCert` without one needs no
  witness, so a random `None` made the success test weaker.
- Unit 4: the native-script tests sign the id of the transaction they submit. The fixed asset names
  remove the flake of "mint and burn same asset", whose two random names could collide.
- Unit 5: four failure tests had a total collateral that did not match the net collateral, so they
  also failed a second check that `isLeft` could not tell apart. Their totals now match.
- Unit 8: the input-order test asserts that a `HashSet` of its fixed inputs iterates in another
  order than the sorted one.
- Unit 9: `withNetwork`, `randomTransactionWithIsValidField` and `generateKeyPair` are deleted.
  `mintTx` keeps 3 callers outside the rule suites, so it stays, with fixed values.
- Unit 10: `CardanoLedgerVectors.checkVector` holds the check and `expectedFailures`; both
  conformance suites call it. The IT suite now also compares the new ledger state, and 1 of its 45
  vectors fails that check: in "UTXOW.Fails with PPViewHashesDontMatch before PV 11", case 0,
  one UTxO of the new state has another datum hash than the ledger's. The old IT loop checked only
  the verdict. The JVM copy of the vector passes. The exclusion list accepts only an entry that a
  spec step removes, so the failure stays visible until someone finds its cause.
- Fixed collateral and output addresses are key addresses. The random ones could also be stake or
  Byron addresses, which no rule under test reads.

The coverage gate `[CG-3]` ran once for units 2 to 8 together, not per unit: the full `scalusJVM`
and `scalusCardanoLedgerJVM` run, compared with the full run of the pilot. The ledger module lost 0
statements, `scalus-core` lost 7:

- `Voter.scriptHashOption` for a `ConstitutionalCommitteeHotScript` voter, 2 statements: the
  voting procedures of `MissingOrExtraScriptHashesValidatorTest` now hold that voter too.
- `Network.Other.toString`, 2 statements: `CardanoAddressTest` now asserts it.
- `CommonContextExtraction`, 3 statements in the `Case` scrutinee decomposition: reached or not by
  the unseeded `forAll` tests of `CommonContextExtractionTest`, which D6 does not touch.

## 9. Open questions

*Informative.* Each question needs the owner's answer. Each has a recommendation.

**Q1. Pilot scope.** *Answered 2026-11-24: as recommended.* Pilot on the 2 certificate suites of `[P3-11a]` only, or add one script
suite? *Recommended:* add `VerifiedSignaturesInWitnessesValidatorTest`. The certificate suites have no
generated values, so without a script suite the riskiest parts (the `ExUnits` override, fixed
values, `Party` signatures, H-23) reach the rollout unpiloted.

**Q2. Where does `genValidTx` go?** *Answered: as recommended.* D5 gave a stateful generator of valid transactions, with
`Party` owners and signers, to D6. H-25 goes the other way: it replaces random transactions with
fixed ones. *Recommended:* D6 builds no `genValidTx`. It moves to the `to-property` step of
`[P3-8]`, where the first law test that needs a valid transaction defines its interface. D6 keeps
`Party` for keys (`[kit.keys.party]`), so the later generator can draw from it.

**Q3. Fix `ArbitraryDerivation.split` at size 0?** *Answered: as recommended.* Today `split` returns `Fallback[T]` at size 0,
and that is always `Gen.fail`: the `summon` runs inside a generic method, so it never sees a
`given Fallback[T]` at the call site. Two workarounds exist: `withFallback` (3 derived sums:
`AuxiliaryData`, `GovAction`, `Certificate`) and `given arbitraryOption` (every `Option` field).
A fix, for example a uniform choice of a subtype at size 0 with a recursion guard, would retire
both. It changes the values of every derived sum type (27 `autoDerived` sites), so it needs the
full proof of `[gen.proof.new-distribution]` over all of them. *Recommended:* not now. Keep the
two workarounds, and fix `split` once D6 has replaced the unseeded `.sample.get` calls of the rule
suites: then a value change shows as a reproducible failure, not as a one-off red build. Reopen
earlier if a fourth `withFallback` becomes necessary.

**Q4. One fee-overflow policy?** *Answered: as recommended.* Today the code has three, plus one saturation:

| Code | Policy past the `Long` range |
|---|---|
| `ExUnits.fee` (`scalus-core/.../ledger/Types.scala:542`) and `MinTransactionFee.computeMinFee` | Throw `ArithmeticException`, via `Coin.exact` (`Types.scala:71`) |
| `MinTransactionFee.computeExactMinFee` | Exact `BigInt`, as cardano-ledger `Integer` |
| `FeesOkValidator` (`rules/FeesOkValidator.scala:55`) | Compare exactly, report the minimum clamped to `Long.MaxValue` |
| `Redeemers.totalExUnits` (`scalus-core/.../ledger/Redeemer.scala:101`) | Saturate at `Long.MaxValue`; read by `ExUnitsTooBigValidator` and `BlockForge` |

*Recommended:* keep exact `BigInt` inside the rules (cardano-ledger semantics) and throw at the
public `Coin` boundary (`ExUnits.fee`, `computeMinFee`), as today. Make the error report of
`FeesOkValidator` carry the exact `BigInt`, so that no path clamps. Leave the saturation of
`totalExUnits`: a saturated sum still exceeds `maxTxExecutionUnits`, so `ExUnitsTooBigValidator`
gives the ledger's verdict. This does not unify the paths: it keeps three behaviours (exact inside,
throw at `Coin`, saturate the unit sum) and removes only the clamp, because each one matches its
caller. The change touches the public error type of `FeesOkValidator`, so it is a main-code `fix:`
commit outside D6, with its own failing test (`[AG-6]`). Option B, one policy everywhere (exact
`BigInt` on the public API too), changes the return type of `ExUnits.fee` and needs a
deprecation cycle.

**Q5. Approve the recommendations of section 4?** *Answered: yes.* The six decisions: a separate `CertsTestKit`,
a context by slot, the `ExUnits` override, fixed values with `forAll` for laws, `Party` keys, and
H-50 in D6 with H-38 in D8.

**Q6. Bound the sum of the units, not each value?** *Answered: yes, 2026-11-24, by a `given RedeemerUnits` at the use site, not by a member override.* *Found in the pilot.*
`[kit.exunits.bounded-given]` bounds each `ExUnits` value. `Arbitrary[Redeemers]` draws 1 to 3
redeemers, so their sum can reach 3 times `maxTxExecutionUnits`. `ExUnitsTooBigValidator` checks
that sum (`Redeemers.totalExUnits`), so a generated transaction can still fail it. *Recommended:*
in the kit, draw each value up to a third of `maxTxExecutionUnits`, so that any 3 redeemers stay
within it. The pilot test then asserts the sum, not each value.
