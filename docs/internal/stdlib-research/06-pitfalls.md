# Cardano Validator Pitfalls – An Actionable Taxonomy for a Correct-by-Construction Stdlib

**Status:** research input for the Scalus high-level smart-contract standard library design.
**Date:** 2026-08-26.
**Sibling docs:** `docs/internal/stdlib-research/` (this file is `06-pitfalls.md`).

---

## 0. How to read this document

The headline goal of the new stdlib is: **the high-level functions must be correct by
construction and make the common pitfalls impossible or loud.** Every entry below is
therefore written as a five-column argument:

| Column | Question it answers |
|---|---|
| **Mechanism** | Why does the eUTxO model allow this at all? |
| **Attack tx** | What does the attacker's transaction literally look like? |
| **Naive code** | What does the vulnerable validator look like? |
| **Correct check** | What is the minimal correct predicate? |
| **API-level fix** | **The important one.** What signature + default behaviour removes the footgun? |

Each entry carries a disposition:

| Marker | Meaning |
|---|---|
| **ELIMINATE** | A stdlib API can make the bug *unrepresentable*: the wrong code does not typecheck, or the safe behaviour is the only behaviour the API offers. |
| **LOUD** | The stdlib cannot choose the policy for the user, but it can make the unsafe path an explicit, named, greppable opt-out (`…Unchecked`, `allowMultipleOwnInputs = true`) and fail with a specific message otherwise. |
| **DETECT** | Only a linter / compiler-plugin warning / adversarial test generator can catch it. API shape alone cannot. |
| **DOCUMENT** | Inherent to the protocol design space. The stdlib can supply vocabulary, patterns and examples only. |

### Evidence base

The ranking in §2 is `frequency × severity`, with **frequency** grounded in three
independent corpora and **severity** in the published audit literature:

1. **In-house corpus** – the 21 Scalus blueprint examples reviewed in
   `docs/internal/EXAMPLES_REVIEW.md`: 20 real bugs found in code written by people who
   knew the model well. This is the best available signal for "what a competent developer
   gets wrong with *today's* Scalus API".
2. **The MLabs vulnerability register** – 11 named classes, each with a *property
   statement*; the closest thing Cardano has to a CWE list.
   <http://mlabs.city/blog/common-plutus-security-vulnerabilities>
3. **The Cardano Developer Portal security curriculum** – now the largest structured
   catalogue, with identifiers (`arbitrary-datum`, `other-redeemer`, `other-token-name`,
   `missed-input`, `signature-domain-separation`, `evaluation-order`, `hash-grinding`,
   `unbounded-*`, `cheap-spam`, `utxo-contention`, `insufficient-staking-control`).
   <https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/overview/>
4. **Audit-firm corpora** – Tweag reports **276 findings across 20+ audits**
   (30 critical / 35 high / 66 medium / 95 low / 50 lowest), of which 44 are classified as
   directly exploitable vulnerabilities.
   <https://www.tweag.io/blog/2026-07-23-cardano-audits-retrospective/>

---

## 1. Source map

| Source | What it contributes | URL |
|---|---|---|
| Plutus/Plinth "Common weaknesses" | The original *double satisfaction* article; only ever one article in that section | <https://plutusvn.readthedocs.io/en/latest/reference/common-weaknesses/double-satisfaction.html> |
| Cardano Developer Portal – security curriculum | Largest structured catalogue with stable identifiers | <https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/overview/> |
| MLabs – Common Plutus Security Vulnerabilities | 11 classes with property statements + Haskell repros | <http://mlabs.city/blog/common-plutus-security-vulnerabilities> |
| Plutonomicon `vulnerabilities.md` | 12 classes, incl. UTxO value-size spam, infinite mint, parameterization | <https://plutonomicon.github.io/plutonomicon/vulnerabilities> |
| Plutonomicon patterns | State thread token, forwarding policy, CnG proof, assoc list, distributed map | <https://github.com/Plutonomicon/plutonomicon> |
| Vacuumlabs / Invariant0 "Cardano Vulnerabilities" series | Double satisfaction ×2, *Trust No UTxO*, Token Security ×2 | <https://medium.com/@vacuumlabs_auditing/cardano-vulnerabilities-1-double-satisfaction-219f1bc9665e> |
| Vacuumlabs Cardano CTF | Intentionally-vulnerable contracts as a training corpus | <https://github.com/vacuumlabs/cardano-ctf> |
| Tweag retrospective + Minswap post-mortem | Audit statistics; the canonical real incident | <https://www.tweag.io/blog/2022-03-25-minswap-lp-vulnerability/> |
| Aiken – Common Design Patterns | One-shot mint, receipts, tagged outputs, STT, forwarding | <https://aiken-lang.org/fundamentals/common-design-patterns> |
| Anastasia Labs design patterns | Stake validator, UTxO indexers, validity-range normalisation | <https://github.com/Anastasia-Labs/design-patterns> |
| CIP-112 (`Observe` purpose) | The principled replacement for the withdraw-zero trick | <https://cips.cardano.org/cip/CIP-0112> |
| Adamant – mangled / franken addresses | Real staking-credential hijack disclosures | <https://medium.com/adamant-security/multi-sig-concerns-mangled-addresses-and-the-dangers-of-using-stake-keys-in-your-cardano-project-94894319b1d8> |
| Scalus `EXAMPLES_REVIEW.md` | 20 real bugs in 21 in-house examples | repo-local |
| Scalus `scalus-design-patterns/` | Existing Scalus ports of the Anastasia Labs patterns | repo-local |

---

## 2. Ranked taxonomy

Score = frequency (1–5) × severity (1–5). "In-house" counts occurrences in the
21-example corpus. **Disposition** is the *best achievable* stdlib disposition, not
today's.

| Rank | ID | Pitfall | F | S | Score | In-house | Disposition |
|---|---|---|---|---|---|---|---|
| 1 | **DS-1** | Double satisfaction – two own-script inputs share one output | 5 | 5 | 25 | Vesting, Vault, Auction, UpgradeableProxy, Betting (5) | **LOUD** |
| 2 | **VP-1** | Value not preserved on the continuing output (`>=`, absent, or datum-only accounting) | 5 | 5 | 25 | AMM, Lottery, Vesting, PaymentSplitter (4) | **ELIMINATE** |
| 3 | **VP-2** | ADA-only comparison – native tokens skimmed / dumped | 4 | 5 | 20 | Vesting, PaymentSplitter (2) | **ELIMINATE** |
| 4 | **VP-3** | Missing continuing-output **address** check (redirect attack) | 4 | 5 | 20 | Lottery (1) | **ELIMINATE** |
| 5 | **AU-1** | Missing UTxO authentication – unauthenticated reference input / planted datum | 4 | 5 | 20 | PriceBet, DecentralizedIdentity (2) | **ELIMINATE** |
| 6 | **MI-1** | "Other token name" / non-exclusive mint check (**the Minswap `isUnity` bug**) | 4 | 5 | 20 | AMM, Betting (2) | **ELIMINATE** |
| 7 | **MI-2** | One-shot NFT: seed `TxOutRef` never bound | 3 | 5 | 15 | EditableNft (1) | **ELIMINATE** |
| 8 | **TI-1** | Unbounded validity range silently used as "now" | 3 | 5 | 15 | Vault (1) | **ELIMINATE** |
| 9 | **AU-2** | Missing signature / authorization on a state transition | 4 | 4 | 16 | Vault, UpgradeableProxy, Lottery (3) | **DETECT** |
| 10 | **IX-1** | Index-list handling: duplicates, length mismatch, `zip` truncation | 3 | 5 | 15 | Crowdfunding (1) | **ELIMINATE** |
| 11 | **DS-2** | Cross-instance / cross-script double satisfaction (distinct script hashes) | 3 | 5 | 15 | Auction (1) | **LOUD** |
| 12 | **DT-1** | Datum continuity: immutable fields not pinned ("datum hijacking") | 3 | 4 | 12 | – | **ELIMINATE** |
| 13 | **PU-1** | "Other redeemer" / purpose confusion – a second entry point bypasses checks | 3 | 5 | 15 | – | **ELIMINATE** (largely done) |
| 14 | **AU-3** | Own-input / `ownRef` confusion; own script hash derived wrongly | 3 | 4 | 12 | – | **ELIMINATE** |
| 15 | **VP-4** | Inexact refund / payout (`>=` where `===` was meant) | 3 | 4 | 12 | – | **LOUD** |
| 16 | **IX-2** | `missed-input`: an extra script input that no index covers | 2 | 5 | 10 | – | **ELIMINATE** |
| 17 | **EV-1** | Evaluation-order trap – a required check short-circuited away | 3 | 4 | 12 | – | **ELIMINATE** |
| 18 | **AU-4** | Missing staking-credential check (franken/mangled addresses) | 3 | 3 | 9 | PaymentSplitter (1) | **ELIMINATE** |
| 19 | **DT-2** | Untyped / lazy `Data` decoding – `.to[T]` validates nothing | 3 | 3 | 9 | – | **LOUD** |
| 20 | **MI-3** | Burn checks: sign confusion, partial burn accepted | 2 | 4 | 8 | – | **ELIMINATE** |
| 21 | **DT-3** | Datum-hash bricking / missing datum on continuing output (`arbitrary-datum`) | 2 | 4 | 8 | – | **ELIMINATE** |
| 22 | **VP-5** | Value-map normalisation: zero entries / non-canonical order defeat `==` | 2 | 4 | 8 | – | **ELIMINATE** |
| 23 | **DE-1** | Locked value – no exit path / unreachable state | 2 | 5 | 10 | Betting (1) | **DETECT** |
| 24 | **AU-5** | "Trust No UTxO" – a state-machine step accepts a UTxO with no valid ancestry | 2 | 5 | 10 | – | **LOUD** |
| 25 | **IX-3** | Ordering assumptions: inputs *are* sorted; outputs are **not** | 2 | 4 | 8 | – | **ELIMINATE** |
| 26 | **AU-6** | Parameterization unverifiable on-chain (script substitution) | 2 | 4 | 8 | – | **DOCUMENT** + pattern |
| 27 | **TI-2** | Interval bound inclusivity handled wrongly; improper ("never") intervals | 3 | 2 | 6 | HTLC (1) | **ELIMINATE** |
| 28 | **RS-1** | Token dust / unbounded value on a protocol UTxO | 2 | 4 | 8 | – | **LOUD** |
| 29 | **RS-2** | Unbounded datum growth | 2 | 4 | 8 | – | **DETECT** |
| 30 | **RS-3** | Unbounded inputs / UTxO fragmentation DoS | 2 | 4 | 8 | – | **DOCUMENT** |
| 31 | **PU-2** | Withdraw-zero forwarding pitfalls (stake-validator coupling) | 2 | 4 | 8 | – | **ELIMINATE** (pattern exists) |
| 32 | **PU-3** | Certificate purposes unguarded – deregistration griefing + deposit theft | 1 | 4 | 4 | – | **ELIMINATE** |
| 33 | **AU-7** | Signature domain separation – replayable off-chain signatures | 1 | 5 | 5 | – | **ELIMINATE** |
| 34 | **DE-2** | Oracle staleness / manipulation | 2 | 4 | 8 | PriceBet (1) | **DOCUMENT** |
| 35 | **AR-1** | Division rounding direction; fee-rounding exploitation | 2 | 3 | 6 | – | **LOUD** |
| 36 | **AR-2** | Negative quantities accepted where positive assumed | 2 | 3 | 6 | – | **ELIMINATE** |
| 37 | **DE-3** | Self-dealing / missing role separation | 2 | 3 | 6 | – | **DETECT** |
| 38 | **RS-4** | Quadratic scans / worst-case budget blow-up | 3 | 2 | 6 | PaymentSplitter-naive (1) | **DOCUMENT** + pattern |
| 39 | **RS-5** | UTxO contention / concurrency DoS | 2 | 3 | 6 | – | **DOCUMENT** |
| 40 | **VP-6** | Min-ADA griefing on forced outputs | 2 | 3 | 6 | – | **LOUD** |
| 41 | **RS-6** | Cheap spam / dust griefing | 2 | 2 | 4 | – | **DOCUMENT** |
| 42 | **DE-4** | Hash grinding – attacker-influenced hashes used as "randomness" | 1 | 4 | 4 | – | **DOCUMENT** |
| 43 | **PU-4** | Voting / proposing purposes unguarded | 1 | 3 | 3 | – | **ELIMINATE** |
| 44 | **AR-3** | Int64 boundary at ledger serialisation (Value quantities) | 1 | 3 | 3 | – | **LOUD** |
| 45 | **RS-7** | Reference-script size fees / script-size DoS | 1 | 3 | 3 | – | **DOCUMENT** |
| 46 | **EV-2** | Compiler-level JVM ↔ on-chain divergence (Scalus-specific) | 1 | 5 | 5 | – | **ELIMINATE** (test kit) |
| 47 | **DE-5** | Replay: the same state reachable twice; missing uniqueness | 1 | 4 | 4 | – | **DOCUMENT** |

### What this ranking says

* The **top six** entries (DS-1, VP-1, VP-2, VP-3, AU-1, MI-1) account for the large
  majority of real findings across all four corpora, and **every one of them is
  addressable by API shape**, not by developer discipline. That is the core argument for
  the new stdlib.
* Everything scoring ≥ 12 is an **ELIMINATE** or **LOUD** – i.e. a design task, not a
  documentation task.
* The **DETECT**-only entries (AU-2 missing signature, DE-1 locked value, RS-2 unbounded
  datum, DE-3 self-dealing) are the ones that should drive a companion *linter* and
  *adversarial test-kit* workstream, since no signature can catch them.

---

## 3. The proposed API vocabulary (referenced by every entry below)

So the "API-level fix" column stays coherent, here is the small vocabulary the rest of the
document assumes. Details and alternatives belong in the API-design doc; this is only
enough to make the fixes concrete. Types are the existing Scalus ones
(`scalus.cardano.onchain.plutus.v3`).

```scala
/** A resolved, authenticated view of the UTxO being spent.
  * Only the framework constructs it, so `ownInput` / `ownAddress` / `ownScriptHash`
  * cannot be derived wrongly by user code (kills AU-3).
  */
final case class Own[D](
    ref: TxOutRef,
    input: TxInInfo,
    address: Address,          // full address: payment AND staking part
    scriptHash: ValidatorHash,
    value: Value,
    datum: D                   // already decoded, shape-checked (see DT-2)
)

/** Every spending validator MUST state how many of its own UTxOs may be spent
  * in one transaction and how inputs bind to outputs. No default. (kills DS-1)
  */
enum OwnInputPolicy:
    case Exclusive                          // exactly one input at own payment credential
    case TaggedOutputs                      // each own input claims an output tagged with its own ref
    case Indexed                            // redeemer carries (inIdx, outIdx); coverage verified
    case Aggregated                         // validator folds over ALL own inputs and outputs
    case Unchecked(justification: String)   // explicit, greppable escape hatch

@Compile
trait SpendingValidator[D: FromData, R: FromData]:
    def ownInputPolicy: OwnInputPolicy               // abstract: must be answered
    def spend(own: Own[D], redeemer: R, tx: TxInfo): Unit

/** What the continuing output must carry, relative to the input. */
enum ValuePolicy:
    case Preserve                    // full multi-asset equality with the input value
    case PreservePlus(delta: Value)  // input + delta, exactly
    case PreserveMinus(delta: Value) // input - delta, exactly
    case Exactly(v: Value)

/** Where the continuing output must go. */
enum AddressPolicy:
    case SameAsInput                    // full address incl. staking part (kills AU-4)
    case SamePaymentCredential          // explicit opt-out of the staking check
    case To(addr: Address)

/** Locate and fully constrain the continuing output in ONE call.
  * There is no partial variant: value, datum and address are all required arguments.
  * (kills VP-1, VP-2, VP-3, DT-1, DT-3)
  */
def continuing[D, D2: ToData](
    own: Own[D],
    value: ValuePolicy,
    datum: D2,
    address: AddressPolicy,
    tx: TxInfo
): TxOut
```

Two cross-cutting rules that fall out of the entries below:

* **R1 – No `Boolean`-returning check callbacks in the public API.** Every user-supplied
  predicate is a `Unit`-returning block built from `require(cond, msg)`. A `Boolean`
  callback invites `a && b` chains, which is exactly the `evaluation-order` footgun
  (EV-1), and it throws away the failure message. Today's
  `UtxoIndexer.oneToOne(..., validator: (TxInInfo, TxOut) => Boolean)` is on the wrong
  side of this rule.
* **R2 – Every default is the safe one; unsafe is a *named* argument, never an omission.**
  `Preserve`, `SameAsInput`, `Exclusive` are what you get if you do nothing thoughtful.
  Getting the unsafe behaviour requires typing `Unchecked`, `SamePaymentCredential`,
  `atLeast = true`.

---

## 4. Detailed entries

### DS – Double satisfaction family

#### DS-1 · Double satisfaction: two own-script inputs share one output · **LOUD** · rank 1

**Mechanism.** A Cardano validator *validates*, it does not *act*. Each spent script input
runs its own validator invocation, and **every invocation sees the same `TxInfo`**. If the
validator's obligation is phrased as "*some* output pays X", then N inputs each looking for
"some output paying X" are all satisfied by the *same single* output. The attacker
therefore discharges N liabilities with one payment. The Plutus docs put it plainly: the
validator "can only ascertain whether its wishes have been carried out, which in this case
is ambiguous".
<https://plutusvn.readthedocs.io/en/latest/reference/common-weaknesses/double-satisfaction.html>

**Attack tx.**

```
Inputs:   utxoA @ script  (datum: pay 10 ADA to Alice)
          utxoB @ script  (datum: pay 10 ADA to Alice)
Outputs:  10 ADA -> Alice          <- ONE output, counted twice
          rest   -> attacker
Redeemers: utxoA -> Claim, utxoB -> Claim
```
Attacker walks away with both NFTs / both escrows for the price of one.
Vacuumlabs' worked example: two NFTs bought for 120 ADA instead of 240.
<https://medium.com/@vacuumlabs_auditing/cardano-vulnerabilities-1-double-satisfaction-219f1bc9665e>

**Naive code.**

```scala
// Scalus, vulnerable
val paid = tx.outputs.exists { o =>
    o.address === beneficiary && o.value.getLovelace >= price
}
require(paid, "must pay the beneficiary")
```
```haskell
-- PlutusTx, the classic form
valuePaidTo info beneficiary `geq` price
```
```aiken
// Aiken, same shape
list.any(self.outputs, fn(o) { o.address == beneficiary && ... })
```

**Correct check.** Pick exactly one of the five known countermeasures and enforce it:

| # | Countermeasure | Enforces | Cost / limitation |
|---|---|---|---|
| C1 | **Only one own-script input** – count inputs whose payment credential equals own, require `=== 1` | no second invocation exists | forbids batching; must compare *payment credential*, not full address, or AU-4 defeats it |
| C2 | **Uniquely tagged outputs** – the claimed output's inline datum contains this input's `TxOutRef` | 1:1 input↔output binding | the payee must accept a datum; costs a datum on every payout; min-ADA impact |
| C3 | **Redeemer-indexed pairing** – redeemer carries `(inIdx, outIdx)`; validator checks `tx.inputs.at(inIdx).outRef === ownRef` | 1:1 binding without datums | must *also* verify index coverage, else IX-2; indices are off-chain-computed |
| C4 | **Sum over all own inputs** – fold every own-credential input and every own-credential output, compare aggregates | correct under batching | O(n) per invocation → O(n²) per tx; use with PU-2 |
| C5 | **Ban other scripts entirely** – require no other script input, no mint, no withdrawal | removes cross-script variants too | breaks composability; the Plutus docs call it "restrictive" |

Note that C2/C3 **do not** by themselves stop DS-2: two *different* script hashes each
tagging "their" output can still share a payout if the tag is not instance-specific. And
the Anastasia Labs UTxO-indexer docs state outright that the singular indexer patterns
"[do] not provide protection against the double satisfaction vulnerability".
<https://github.com/Anastasia-Labs/aiken-design-patterns>

**API-level fix.** *The abstract `ownInputPolicy` member.* A spending validator does not
compile until the author has answered "how many of my UTxOs may be spent at once, and how
do inputs bind to outputs":

```scala
@Compile
object Vesting extends SpendingValidator[VestingDatum, VestingRedeemer]:
    override def ownInputPolicy = OwnInputPolicy.Exclusive     // <- no default; must be written
    def spend(own: Own[VestingDatum], r: VestingRedeemer, tx: TxInfo): Unit = ...
```

The framework injects the corresponding guard *before* `spend` runs:

| Policy | Injected guard |
|---|---|
| `Exclusive` | `require(tx.inputs.count(_.resolved.address.credential === own.address.credential) === 1, OwnInputNotExclusive)` |
| `TaggedOutputs` | every own-credential input must have a distinct output whose inline datum tag `=== ownRef`; `payout` helpers auto-attach the tag |
| `Indexed` | index pairs decoded as `IndexPairs` (strictly ascending by construction) **and** coverage of the own-credential input set verified (kills IX-2 too) |
| `Aggregated` | `spend` receives `List[Own[D]]` and `List[TxOut]` instead of a single `Own`, so writing a per-input check is not expressible |
| `Unchecked(why)` | nothing – but the string is greppable, shows in the blueprint metadata, and the security-review skill flags it |

Plus the payout helpers, which encode C2 and C4 so the user never hand-rolls them:

```scala
/** Exactly one output pays `recipient` exactly `amount`, and carries `own.ref` as its
  * inline-datum tag. Safe under any OwnInputPolicy. */
def payTagged(own: Own[?], recipient: Address, amount: Value, tx: TxInfo): Unit

/** Exactly one output pays `recipient` exactly `amount`, untagged.
  * Rejected unless ownInputPolicy is Exclusive – checked by the framework. */
def payExact(recipient: Address, amount: Value, tx: TxInfo): Unit
```

**Why LOUD and not ELIMINATE.** The stdlib cannot know whether a protocol *wants* to allow
batching, so it cannot pick the policy. What it can do is make "no policy" impossible to
express. That converts DS-1 from "a thing you must remember" to "a question you must
answer", which is the entire difference.

**Prior art.** Helios bakes C2 into its stdlib as `tx.value_sent_to_datum(addr, datum,
bool)`, used in the PicoSwap example precisely to datum-tag the payout with a nonce.
Aiken documents C2 as the "tagged outputs" pattern using the input's `OutputReference` as
the inline datum. Neither makes it the default.
<https://aiken-lang.org/fundamentals/common-design-patterns>

---

#### DS-2 · Cross-instance / cross-script double satisfaction · **LOUD** · rank 11

**Mechanism.** DS-1's C1 ("only one own input") is scoped to *this script hash*. Two
**parameterised instances** of the same validator have **different script hashes**, so each
sees exactly one input at *its own* credential and both guards pass — while sharing one
payout. Same for two genuinely different validators that happen to owe the same party.
Vacuumlabs devote a whole follow-up article to this variant.
<https://medium.com/@invariant0/cardano-vulnerabilities-2-double-satisfaction-continued-a66043d025c0>

**Attack tx.**

```
Inputs:   auctionA @ scriptHashA (one-shot param: seedA), seller = S
          auctionB @ scriptHashB (one-shot param: seedB), seller = S
Outputs:  100 ADA -> S     <- satisfies both "pay the seller >= 100" checks
```
This is exactly the in-house **Auction** finding: the "fixed" `handleEnd` counted NFTs only
under *its own* script hash, so two same-seller instances ended in one transaction were not
detected (`EXAMPLES_REVIEW.md`).

**Naive code.**

```scala
// counts only inputs at MY hash -- two instances have two hashes
val myInputs = tx.inputs.filter(_.resolved.address.credential === Credential.ScriptCredential(ownHash))
require(myInputs.length === 1, "single script input")
require(tx.outputs.exists(o => o.address === seller && o.value.getLovelace >= price))
```

**Correct check.** The payout must be tagged with something unique to *this instance*, not
to this script: the input's `TxOutRef`, or the instance's own script hash, embedded in the
payout's inline datum. The in-house Auction fix does exactly this – "the seller payout must
carry this auction's `scriptHash` as an inline datum".

**API-level fix.** `payTagged` tags with `own.ref` (the `TxOutRef`), which is globally
unique across every script and every instance, by ledger construction. There is no cheaper
tag that is also correct, so the stdlib should not offer one. Additionally,
`OwnInputPolicy.Exclusive`'s doc-comment and error message must state that it is
**instance-scoped and does not protect against DS-2**, and the security-review skill should
flag `Exclusive` + `outputs.exists` in the same validator.

---

### VP – Value preservation family

#### VP-1 · Value not preserved on the continuing output · **ELIMINATE** · rank 2

**Mechanism.** A spending validator that checks the *datum* transition but not the *value*
lets the attacker keep the difference. Variants seen in the wild:
(a) no value check at all; (b) `>=` where `===` was meant; (c) the datum records a balance
that is never tied to the actual `Value` of the output. (c) is the worst because the code
*looks* thorough. The in-house **AMM** finding is exactly (c): "Datum reserves never tied
to pool output `Value` … Pool fully drainable" – `Value` was imported but unused.

**Attack tx.**

```
Input:   pool @ script, value = 1_000_000 tokenX + 1_000_000 tokenY, datum = Reserves(1e6, 1e6)
Outputs: pool @ script, value = 1 tokenX + 1 tokenY, datum = Reserves(1e6, 1e6)   <- datum lies
         999_999 tokenX + 999_999 tokenY -> attacker
```

**Naive code.**

```scala
// vulnerable: only the datum transition is checked
val out = tx.outputs.at(outIdx)
require(out.address === ownAddress, "must continue")
require(out.datum.to[Reserves] === expectedReserves, "reserves")
// value never mentioned
```

**Correct check.**

```scala
require(out.value === own.value - withdrawn + deposited, "value must be preserved exactly")
require(out.datum.to[Reserves].x === out.value.quantityOf(policyX, nameX), "datum must match value")
```

**API-level fix.** *Make the value policy a required argument of the only continuation
helper.*

```scala
val out = continuing(
    own,
    value   = ValuePolicy.PreserveMinus(payout),   // required
    datum   = own.datum.copy(reserves = newReserves),
    address = AddressPolicy.SameAsInput,
    tx      = tx
)
```

There is no `continuing(own, datum)` overload and no `findOwnOutputs` in the high-level
surface. `ValuePolicy` has no "unchecked" case; the loosest option is `Exactly(v)`, which
still forces the author to *name* the value. If the datum carries a balance, the stdlib
offers the pairing helper that removes variant (c) entirely:

```scala
/** Asserts that a datum field and a Value entry agree. Intended for pool/vault datums. */
def requireDatumMatchesValue(claimed: BigInt, out: TxOut, asset: AssetClass): Unit
```

**Existing precedent.** The Developer Portal lists "Value-Preservation Check" as one of the
five established patterns: "explicitly verify that total value in script outputs equals the
expected value (inputs minus authorized withdrawals plus authorized deposits)".
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/>

---

#### VP-2 · ADA-only comparison – native tokens skimmed · **ELIMINATE** · rank 3

**Mechanism.** `Value` is a two-level map, but the ergonomic accessor is
`value.getLovelace`. A check written in lovelace passes while every native token in the
UTxO is redirected to the attacker. Two independent in-house findings: **Vesting**
("continuing-output check is lovelace-only → native tokens can be stripped") and
**PaymentSplitter** ("both validators reconciled only `getLovelace` → native tokens in a
contract UTxO could be skimmed by the fee payer"). The Developer Portal names the same bug
"insufficient token-type validation".
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/token-security/>

**Attack tx.**

```
Input:   vault @ script, value = 10 ADA + 500 USDM
Outputs: vault @ script, value = 10 ADA          <- passes getLovelace check
         500 USDM -> attacker
```

**Naive code.**

```scala
require(out.value.getLovelace === own.value.getLovelace, "value preserved")   // WRONG
```

**Correct check.**

```scala
require(out.value === own.value, "value preserved")   // full multi-asset equality
```
…or, if the protocol is genuinely ADA-only, prove it at the boundary:
```scala
require(own.value.withoutLovelace.isZero, "contract UTxOs must be ADA-only")
```
(the in-house PaymentSplitter fix took exactly this route).

**API-level fix.** Three moves:

1. `ValuePolicy.Preserve` compares the **whole** `Value`. Lovelace-only preservation is not
   in the enum at all.
2. Rename/deprecate the tempting accessor. `value.getLovelace` stays for arithmetic, but
   the high-level surface exposes no `requireLovelace…` comparison helper. If an ADA-only
   protocol is wanted, the author declares it once:
   ```scala
   /** Asserts this UTxO carries nothing but ADA. Makes an ADA-only protocol's
     * assumption explicit and checkable, instead of implicit in every comparison. */
   def requireAdaOnly(v: Value): Unit
   ```
3. Ban mixed comparison at the type level where possible: give `Lovelace` its own opaque
   type so `out.value.getLovelace === own.value` does not typecheck and
   `out.value === own.value.getLovelace` does not either.

---

#### VP-3 · Missing continuing-output address check (redirect attack) · **ELIMINATE** · rank 4

**Mechanism.** The validator finds "the output at index `i`" from a redeemer-supplied index
and checks its value and datum but never its address. The attacker points index `i` at
their own address, or at a look-alike script they control.

**Attack tx.**

```
Input:   100 ADA @ legitimate script, datum = {owner: O, state: Active}
Outputs: [0] 100 ADA -> ATTACKER's script, datum = {owner: O, state: Active}
Redeemer: SomeAction(outputIdx = 0)
```

**Naive code.**

```scala
val out = tx.outputs.at(redeemer.outputIdx)
require(out.value.getLovelace >= newBid, "insufficient")
require(out.datum.to[AuctionDatum] === expected, "bad datum")
// address never checked
```

**Correct check.** `require(out.address === own.address, ...)` – and it must be the **full**
address, see AU-4.

**API-level fix.** The `continuing` helper takes `address: AddressPolicy` as a **required**
argument whose safe value `SameAsInput` compares the whole `Address`. Crucially, there is
**no raw `tx.outputs.at(idx)` in the high-level surface** – the only way to obtain a
"continuing output" object is through `continuing(...)`, which has already constrained it.
Users who need arbitrary output access drop to the low-level `TxInfo` API, which is a
visible altitude change.

---

#### VP-4 · Inexact refund / payout (`>=` where `===` was meant) · **LOUD** · rank 15

**Mechanism.** `>=` on a payout looks conservative ("at least the required amount") but it
lets the attacker overshoot on one output and reclaim the excess elsewhere, and it is the
enabling condition for DS-1 (an output that satisfies "at least" can satisfy two
obligations at once). MLabs' `other-token-name` fix uses the same reasoning at the mint
level: replace `assetClassValueOf v c == q` with whole-map equality.

**Attack tx.** With `refund >= currentBid`, the attacker builds one 200-ADA output to Alice
that satisfies both a 100-ADA refund obligation from escrow A and a 100-ADA refund
obligation from escrow B.

**Naive code.**

```scala
require(refundOutput.value.getLovelace >= currentHighestBid, "refund too small")
```

**Correct check.**

```scala
require(refundOutput.value === Value.lovelace(currentHighestBid), "refund must be exact")
```

**API-level fix.** `payExact` / `payTagged` take a `Value`, not a minimum, and compare with
`===`. A minimum-payment helper exists but is named for what it is and carries the DS
warning in its signature:

```scala
/** Requires SOME output to `recipient` with AT LEAST `minimum`.
  * DANGEROUS under batching: see DS-1. Requires ownInputPolicy = Exclusive.
  */
def payAtLeastRequiresExclusive(recipient: Address, minimum: Value, tx: TxInfo): Unit
```
The name is deliberately unpleasant. Grepping for `AtLeast` finds every DS-1 candidate in
a codebase.

---

#### VP-5 · Value-map normalisation defeats structural equality · **ELIMINATE** · rank 22

**Mechanism.** A `Value` is a nested map serialised as `Data`. Two representations can
denote the same value: one with a zero-quantity entry and one without, or with token names
in a different order. If any part of the value is attacker-controlled, comparing the raw
maps for equality is exploitable in both directions: an attacker-supplied zero entry makes
a legitimate `==` fail (locking the UTxO), or a canonicalisation gap makes an illegitimate
value compare equal. The Developer Portal's guidance: "Do not compare raw value maps for
equality when any part is attacker-controlled; check specific quantities with
`quantity_of`, and normalize before comparing."
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/token-security/>

**Attack tx.** Attacker sends the continuing output with an extra `(policyX, nameY, 0)`
entry. The value is economically identical; `equalsData` on the raw `Data` says "different";
the validator rejects every legitimate spend → protocol halted.

**Naive code.**

```scala
require(out.value.toSortedMap === own.value.toSortedMap, "value preserved")  // raw compare
```

**Correct check.** Compare via a normalising comparison, or check named quantities.

**API-level fix.** Scalus already has the right primitive: `Value.eq` /
`Value.equalsAssets` compare semantically, and
`Value.fromStrictlyAscendingListWithNonZeroAmounts` / `valueFromDataWithValidation` reject
non-canonical inputs at the boundary. The stdlib rules:

1. `Value`'s `===` is **always** the normalising comparison; the raw-`Data` comparison is
   not reachable from the public surface.
2. Any `Value` that enters the validator from an attacker-controlled position (a redeemer
   field, a datum field) is decoded with `valueFromDataWithValidation`, which rejects zero
   entries and non-ascending keys, so a normalised representation is the *only* one that
   ever reaches user code.
3. `TxOut.value` from the ledger is already canonical (ledger-enforced), so the cost is
   paid only where it is needed.

---

#### VP-6 · Min-ADA griefing on forced outputs · **LOUD** · rank 40

**Mechanism.** Every output must carry a minimum ADA proportional to its size
(`coinsPerUTxOByte`). A validator that forces an output of a fixed non-ADA value, or that
forces a datum, can be pushed below min-ADA by an attacker who inflates the output's size
(more token types, a bigger datum) – the transaction then cannot be built at all, and the
UTxO is stuck.

**Attack tx.** Attacker deposits 40 distinct dust tokens into the vault UTxO. The
validator's "return the same value to the script" rule now forces an output whose min-ADA
exceeds the ADA actually in the UTxO. No valid spending transaction exists.

**Naive code.** Any `ValuePolicy.Preserve` on a UTxO whose token set is attacker-controlled.

**Correct check.** Bound the token set (RS-1) *and* keep an ADA headroom invariant.

**API-level fix.** `ValuePolicy.Preserve` is paired with a token-set policy at the
*deposit* boundary, not at the spend boundary:

```scala
/** Declares the complete set of asset classes this protocol will ever accept into
  * its own UTxOs. Enforced on every output the stdlib creates or checks.
  * `AssetPolicy.Open` is the greppable opt-out. */
enum AssetPolicy:
    case AdaOnly
    case Allowed(classes: List[AssetClass])
    case Open(justification: String)
```
The framework checks `AssetPolicy` in `continuing(...)`, so an attacker's dust cannot enter
a protocol UTxO in the first place. See RS-1.

---

### AU – Authentication and identity family

#### AU-1 · Missing UTxO authentication (unauthenticated reference input / planted datum) · **ELIMINATE** · rank 5

**Mechanism.** *Anyone* can create a UTxO at *any* address, with *any* datum. A validator
that locates protocol state by **address alone** – "the input from the oracle script", "the
delegation record at my own address" – is reading attacker-supplied data. MLabs' property
statement: "All spending and referencing of legit protocol outputs is authenticated."
The same class covers reference inputs, which feel read-only and therefore safe, and are
not.

**Attack tx.**

```
Reference inputs: fakeOracle @ oracleScriptAddress, datum = Rate(999999), no beacon NFT
Inputs:           bet @ script
Outputs:          whole pot -> attacker
```
The attacker created `fakeOracle` themselves, in an earlier transaction, for the cost of
min-ADA.

This is a **real, twice-repeated in-house bug**: **PriceBet** ("`Win` authenticated the
oracle reference input only by script credential, never by a beacon NFT") and
**DecentralizedIdentity** ("`PublishAttribute` accepted any datum-shaped delegation at the
script address without requiring the delegation token → forged delegation, also defeating
revocation").

**Naive code.**

```scala
val oracle = tx.referenceInputs
    .find(_.resolved.address.credential === Credential.ScriptCredential(oracleHash))
    .getOrFail("no oracle")
val rate = oracle.resolved.datum.to[Rate]     // attacker-controlled
```

**Correct check.** Authenticate by a **beacon / state-thread NFT** whose minting policy
guarantees uniqueness:

```scala
require(oracle.resolved.value.quantityOf(oraclePolicy, beaconName) === 1, "not the real oracle")
```

**API-level fix.** *There is no unauthenticated state reader in the stdlib.* The only way
to read foreign state is:

```scala
/** Reads protocol state from the unique UTxO carrying `beacon`.
  * Fails if zero or more than one candidate carries it.
  * There is no `readByAddress` counterpart in this API.
  */
def readAuthenticated[S: FromData](beacon: AssetClass, from: StateSource, tx: TxInfo): S

enum StateSource:
    case ReferenceInputs
    case Inputs
    case Either
```

Two supporting moves:

* `Own[D].datum` is safe *because the ledger proved this UTxO was locked by this script and
  the script vetted the datum when it was created* – but only if the script really did vet
  it, which is DT-3. So the stdlib pairs `readAuthenticated` with a creation-side rule: the
  minting policy that issues the beacon must be the one that pins the initial datum (the
  **state thread token** pattern).
  <https://plutonomicon.github.io/plutonomicon/statethread>
* A one-line `AssetClass` alias plus `Beacon` newtype makes the intent greppable and
  prevents passing a fungible policy where a one-shot NFT is required:
  ```scala
  opaque type Beacon = AssetClass   // constructed only by `oneShot(...)`, see MI-2
  ```

**Precedent.** The Developer Portal lists "State/Beacon Token" as pattern #1: "a unique NFT
in every script UTxO — prevents rogue UTXOs and solves double satisfaction".

---

#### AU-2 · Missing signature / authorization on a state transition · **DETECT** · rank 9

**Mechanism.** A branch of the redeemer enum simply forgets its authorization check. There
is nothing structurally wrong with the code; a required predicate is absent. Three in-house
occurrences: **Vault** ("`finalize` has no signature check → anyone flips Idle→Pending and
forces payout"), **UpgradeableProxy** ("`Call` needs no signature"), **Lottery**
("winning-reveal branches check only preimage hash + parity – no signature").

**Attack tx.** Any third party submits the state transition.

**Naive code.**

```scala
redeemer match
    case Cancel => require(tx.isSignedBy(datum.seller), "seller must sign")
    case Finalize => payout(datum)          // <- nobody has to sign
```

**Correct check.** `require(tx.isSignedBy(datum.owner), ...)` on every branch that is not
deliberately permissionless.

**API-level fix (partial).** Signatures alone cannot make an *absent* check present. But
two things help materially:

1. **A single authorization vocabulary that handles script credentials.** "Signed by" is
   not the same as "authorized by": if the authority is a *script*, no signature exists.
   No current Scalus helper covers this, and it is a genuine gap:
   ```scala
   /** Authorization for ANY credential:
     *  - PubKeyCredential -> the key is in `tx.signatories`
     *  - ScriptCredential -> that script actually ran in this tx, i.e. one of:
     *      an input at that credential is being spent,
     *      a withdrawal keyed by that credential exists (withdraw-zero),
     *      the credential appears in the CIP-112 observations list (when available).
     */
   def requireAuthorizedBy(cred: Credential, tx: TxInfo): Unit
   ```
   This makes multi-sig-by-script, DAO-by-script and withdraw-zero forwarding all express
   as one call, instead of each protocol reinventing it.
2. **A total, authorization-annotated transition table.** If the high-level API asks for a
   `Map[Action, Transition]` where `Transition` has an explicit
   `authority: Authority` field (`Signer(pkh)`, `Credential(c)`, `Anyone(justification)`),
   then "forgot the signature check" becomes "wrote `Anyone("…")`", which is greppable and
   reviewable. This is the DETECT→LOUD upgrade path, at the cost of a more opinionated API.

The `smart-contract-security-review` skill (V014) plus an adversarial test generator that
submits every redeemer branch from an unrelated key is the practical mitigation today.

---

#### AU-3 · Own-input / `ownRef` confusion; own script hash derived wrongly · **ELIMINATE** · rank 14

**Mechanism.** In Plutus V1/V2, `findOwnInput` returns `Maybe`, and every validator
re-derives its own address and script hash by hand. Hand-derivation goes wrong in three
ways: `.get` on a `None`; matching `Credential.ScriptCredential(h)` and silently accepting
a `PubKeyCredential`; or using `Address.fromScriptHash(h)` (which has **no staking part**)
to compare against an output that legitimately does have one — see AU-4.

**Attack tx.** Usually a *liveness* rather than a theft bug (the validator fails on valid
transactions), but the "own address without staking part" variant is a real theft enabler.

**Naive code.**

```scala
val ownInput = tx.inputs.find(_.outRef === ownRef).get              // .get
val ownHash = ownInput.resolved.address.credential match
    case Credential.ScriptCredential(h) => h
    case _ => fail("not a script")
require(out.address === Address.fromScriptHash(ownHash))            // drops staking part!
```

**Correct check.** Resolve once, keep the *whole* address, never reconstruct it.

**API-level fix.** The `Own[D]` record is built by the framework before user code runs, and
carries `address` (full), `scriptHash`, `value`, `ref` and the decoded `datum`. A spending
validator never sees a bare `ownRef` and has nothing to resolve. The escape hatch is the
low-level `TxInfo` API, where `findOwnInputOrFail` (which already exists) is the safe form
and `findOwnInput` returning `Option` should be `private[scalus]` or documented as
low-level.

---

#### AU-4 · Missing staking-credential check ("franken" / "mangled" addresses) · **ELIMINATE** · rank 18

**Mechanism.** A Cardano address is a *pair*: a payment credential (who may spend) and an
optional staking credential (who collects rewards, and who may delegate). A validator that
compares only the payment credential accepts an output at
`(myScriptHash, ATTACKER_stake_key)`. The attacker cannot spend the funds, but they
**collect all staking rewards on protocol TVL indefinitely**, and can delegate the stake.
MLabs list a second, nastier consequence: a "only one input from this address" DS-1 guard
**breaks**, because protocol funds now sit at many distinct addresses that share one
payment credential.

**Attack tx.**

```
Input:   1_000_000 ADA @ (scriptHash, protocolStakeKey)
Outputs: 1_000_000 ADA @ (scriptHash, ATTACKER_stakeKey)     <- passes a credential-only check
```

Real disclosures of exactly this shape exist against Atomic Swap and TradingTent, and
against validator *payouts* generally.
<https://medium.com/adamant-security/multi-sig-concerns-mangled-addresses-and-the-dangers-of-using-stake-keys-in-your-cardano-project-94894319b1d8>
<https://www.essentialcardano.io/article/cardano-franken-addresses>

The in-house **PaymentSplitter** carries the payout-side variant: "payee outputs matched on
payment credential only (staking-credential redirect of rewards)".

**Naive code.**

```scala
require(out.address.credential === own.address.credential, "must return to script")
```
```haskell
[(_, contVal)] = scriptOutputsAt ownValidatorHash info    -- ownHash only
```

**Correct check.**

```scala
require(out.address === own.address, "must return to the SAME full address")
```

**API-level fix.** Three moves, all of them defaults:

1. `AddressPolicy.SameAsInput` compares the **whole** `Address`. The credential-only
   comparison is `AddressPolicy.SamePaymentCredential`, which must be typed out, and whose
   scaladoc states the reward-hijack consequence.
2. `Address.fromScriptHash(h)` — which silently produces `stakingCredential = None` — is
   **removed from the high-level surface**, or renamed
   `Address.scriptNoStaking(h)` so the omission is in the name. Constructing the "expected"
   address by hand is the mechanism of this bug.
3. Payout helpers take `Address`, never `Credential`. `payExact(recipient: Address, …)`
   cannot be called with a bare payment credential.

---

#### AU-5 · "Trust No UTxO" – a state-machine step accepts a UTxO with no valid ancestry · **LOUD** · rank 24

**Mechanism.** A validator sees only *this* transaction. It cannot see how the UTxO it is
spending came to exist. In a multi-step protocol, a validator for step *k* that checks only
"the datum looks like a valid step-*k* state" accepts a UTxO the attacker fabricated
directly in step-*k* shape, skipping steps 1..k-1. Vacuumlabs' name for the class, with a
DAO-voting worked example: the attacker builds a UTxO reading "proposal already passed"
without any votes ever being cast.
<https://medium.com/@vacuumlabs_auditing/cardano-vulnerabilities-3-trust-no-utxo-b252650ac2b9>

**Attack tx.** Attacker sends `1 ADA + fabricatedDatum(Passed)` to the DAO script address,
then spends it with the `Execute` redeemer.

**Naive code.**

```scala
val d = own.datum.to[ProposalDatum]
require(d.status === Status.Passed, "not passed")
executeProposal(d)          // no proof the status was reached legitimately
```

**Correct check.** Every legitimate protocol UTxO carries a beacon NFT whose minting policy
enforces the *initial* state, and every transition preserves the beacon and is validated by
the same script. Ancestry then follows by induction.

**API-level fix.** This is the deep reason AU-1's `readAuthenticated` and MI-2's one-shot
NFT belong in the same design. Concretely:

```scala
/** A spending validator whose own UTxOs are always beacon-authenticated.
  * The framework asserts `own.value.quantityOf(beacon) === 1` before `spend` runs,
  * and asserts the beacon is preserved by `continuing(...)` unless the transition
  * declares `terminal = true` (in which case it must be burned).
  */
@Compile
trait BeaconedValidator[D, R] extends SpendingValidator[D, R]:
    def beacon: Beacon
```

Marked LOUD rather than ELIMINATE because a protocol may legitimately have no beacon (an
HTLC's UTxO is self-describing: whoever funded it chose the datum, and only they lose if it
is wrong). But a *stateful* protocol without a beacon should have to say so.

---

#### AU-6 · Parameterization unverifiable on-chain (script substitution) · **DOCUMENT + pattern** · rank 26

**Mechanism.** UPLC has no on-chain "hash of a partially-applied script" primitive, so a
script cannot verify that a *counterpart* script is the correct instantiation of a known
template with known parameters. Consequence: a user may interact with an attacker's
identically-shaped instance whose `owner` parameter is the attacker.

**Attack tx.** Attacker deploys the same validator with `owner = attacker`, publishes a UI
pointing at it. Everything works, and the attacker owns the admin path.

**Naive code.** `ParameterizedValidator[Params]` where `Params` contains a `PubKeyHash`
that grants authority, with no beacon.

**Correct check.** Off-chain: compute the expected hash, bake it into the dependent script.
On-chain: compare credentials against that baked hash. Scalus already ships this as
`ParameterValidation` / `ParameterValidationOnChain`
(`scalus-design-patterns/src/main/scala/scalus/patterns/ParameterValidation.scala`).

**API-level fix (partial).** The stdlib cannot verify instantiation on-chain, but it can:

1. Emit the computed instance hash into the **CIP-57 blueprint** automatically, so the
   published artifact is the source of truth for "the real instance".
2. Refuse to compile a `ParameterizedValidator` whose parameter type contains a
   `PubKeyHash` / `Credential` / `Address` **unless** the validator also declares a
   `beacon` (a macro-level check, since parameter types are known at compile time). This is
   the single highest-value lint in this whole document, because it converts the most
   subtle class into a compile error.

---

#### AU-7 · Signature domain separation – replayable off-chain signatures · **ELIMINATE** · rank 33

**Mechanism.** Protocols that verify an Ed25519 signature on-chain
(`verifyEd25519Signature`) over an application-defined payload must include a domain
separator. Without one, a signature is replayable across script instances, across
protocols, across networks, and repeatedly within one protocol.
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/unchecked-inputs/>

**Attack tx.** Attacker takes a valid oracle signature over `(price, timestamp)` published
for protocol A and replays it into protocol B, or into instance 2 of protocol A, or twice
into the same instance.

**Naive code.**

```scala
val msg = serialiseData(OracleData(price, timestamp).toData)
require(verifyEd25519Signature(oracleKey, msg, sig), "bad signature")
```

**Correct check.** The signed payload must commit to: signer identity, network id, script
hash / policy id of *this instance*, a nonce or consumed `TxOutRef`, and every
security-relevant field.

**API-level fix.** Provide the only signing-payload constructor, and make it impossible to
omit the domain:

```scala
/** Builds the canonical signed payload. All four domain components are required
  * positional arguments; there is no overload that omits any of them. */
def signedPayload(
    domain: ByteString,        // protocol tag, e.g. utf8"scalus.oracle.v1"
    instance: ScriptHash,      // this instance
    nonce: TxOutRef,           // one-time use, must be spent in this tx
    body: Data
): ByteString

def requireSignature(key: PubKey, payload: ByteString, sig: ByteString): Unit
```
`requireSignature` accepts only a `ByteString` produced by `signedPayload` (an opaque type),
so a bare `serialiseData(body)` cannot be passed.

---

### MI – Minting family

#### MI-1 · "Other token name" / non-exclusive mint check · **ELIMINATE** · rank 6

**Mechanism.** A minting policy that asserts *"the quantity of my expected asset is N"*
says nothing about **other token names under the same policy id**, nor about other policies.
The attacker mints the expected token *and* an arbitrary extra token that other parts of
the protocol will treat as authentic.

**This is the canonical real Cardano incident.** Minswap's uniqueness helper was
`isUnity v c = assetClassValueOf v c == 1` – it checked that one unit of the pool NFT was
present, not that *nothing else* was minted. An attacker could mint a counterfeit pool NFT
alongside the real one and then mint unlimited LP tokens for any pool, draining every pool.
~$195M TVL was at risk; found and responsibly disclosed by the competing WingRiders team in
March 2022, after a Tweag audit had missed it.
<https://www.tweag.io/blog/2022-03-25-minswap-lp-vulnerability/>
<https://medium.com/@wingriderscom/wingriders-hackers-put-on-a-white-hat-and-save-195m-tvl-on-minswap-7a5e9615876c>

The correct form named in the post-mortem:
`Map.lookup curr (getValue v) == Just (Map.fromList [(tok, 1)])`.

Two in-house instances: **AMM** ("the mint check sums all asset names under the policy – LP
name unconstrained") and **Betting** ("beacon asset name unconstrained on mint").

**Attack tx.**

```
Mint: (myPolicy, "PoolNFT", 1)          <- what the policy checks
      (myPolicy, "PoolNFT_evil", 1)     <- what it does not
```

**Naive code.**

```scala
require(tx.mint.quantityOf(policyId, expectedName) === 1, "must mint one")
```
```haskell
assetClassValueOf txInfoMint ownAssetClass == someQuantity
```

**Correct check.** Compare the **whole sub-map** for this policy:

```scala
require(tx.mint.tokens(policyId) === SortedMap.singleton(expectedName, BigInt(1)),
        "policy must mint exactly this and nothing else")
```

**API-level fix.** *The stdlib exposes no `quantityOf`-based mint assertion at all.* The
only minting assertion is whole-sub-map:

```scala
/** The ONLY mint assertion in the high-level API.
  * Compares the entire sub-map under `policyId`, so extra token names are impossible.
  * Positive quantities mint, negative burn; `SortedMap.empty` means "this policy
  * must not mint or burn anything in this transaction".
  */
def requireMintExactly(policyId: PolicyId, expected: SortedMap[TokenName, BigInt], tx: TxInfo): Unit

// sugar, all defined in terms of the above:
def requireMintOne(policyId: PolicyId, name: TokenName, tx: TxInfo): Unit  = requireMintExactly(policyId, SortedMap.singleton(name, 1), tx)
def requireBurnAll(policyId: PolicyId, name: TokenName, qty: BigInt, tx: TxInfo): Unit = requireMintExactly(policyId, SortedMap.singleton(name, -qty), tx)
def requireNoMint(policyId: PolicyId, tx: TxInfo): Unit = requireMintExactly(policyId, SortedMap.empty, tx)
```

Scalus already has the necessary primitive: `Value.tokens(cs): SortedMap[TokenName, BigInt]`
and `Value.hasOnly(cs, tn, amount)`. `hasOnly` is *exactly* the right shape and should be
promoted to the headline mint API.

**Corroboration from the Plutus team itself.** The PR adding
`Value.currencySymbolValueOf` (the "sum all token names under this symbol" helper) records
in its discussion that this function *"is perhaps responsible for more critical
vulnerabilities than any other utility function in onchain code"* – because summing across
token names conflates mints and burns, so "total for my symbol == 1" is satisfiable by
minting 2 of token A and burning 1 of token B.
<https://github.com/IntersectMBO/plutus/pull/5781>
A stdlib that ships a sum-over-names helper at all should name it for its danger.

Relatedly, Plutus V3 wraps `txInfoMint` in a dedicated `MintValue` newtype precisely because
it is the only `Value` in the script context that can hold negative quantities.
<https://github.com/IntersectMBO/plutus/issues/6445> A Scalus `MintValue` opaque type with
only whole-sub-map accessors would carry that distinction into the type system.
(ADA can never be minted or burned in any Plutus version – a ledger-wide invariant, not a
V3 change: <https://cardano-ledger.readthedocs.io/en/latest/explanations/policies.html>.)

---

#### MI-2 · One-shot NFT: seed `TxOutRef` never bound · **ELIMINATE** · rank 7

**Mechanism.** A "one-shot" policy is parameterised by a `TxOutRef` and must require that
*that exact ref* is consumed in the minting transaction. Since a UTxO can be spent only
once, the policy can then fire only once. If the check is weakened to "some input exists at
index i", or to "an input with the right index number", uniqueness collapses and the NFT is
mintable forever.

**In-house instance, Critical:** **EditableNft** – "Seed UTxO never bound:
`EditableNftValidator.scala:65` checks `tx.inputs.get(seedIndex).isDefined` but never
compares to `param.seed` → one-shot mint defeated, NFT uniqueness broken."

**Attack tx.** Attacker mints the "unique" NFT a second time using any input at all, then
uses it to impersonate the authentic instance (which feeds AU-1 and AU-5).

**Naive code.**

```scala
require(tx.inputs.get(seedIndex).isDefined, "seed present")     // never compares to param.seed
```

**Correct check.**

```scala
require(tx.inputs.exists(_.outRef === param.seed), "seed UTxO must be consumed")
```

**API-level fix.** A constructor that binds the seed and derives the name in one step, and
returns the opaque `Beacon` that `readAuthenticated` and `BeaconedValidator` demand:

```scala
/** Asserts the seed UTxO is spent in this transaction, asserts that this policy mints
  * exactly one token whose name is derived from the seed, and returns the beacon.
  * There is no way to obtain a `Beacon` without going through a one-shot proof.
  */
def oneShot(seed: TxOutRef, policyId: PolicyId, tx: TxInfo): Beacon

/** The canonical derivation, so off-chain and on-chain cannot disagree. */
def oneShotTokenName(seed: TxOutRef): TokenName =
    TokenName(blake2b_256(serialiseData(seed.toData)))
```

The `Beacon` opaque type is what makes this an ELIMINATE rather than a LOUD: every API that
consumes authentication (`readAuthenticated`, `BeaconedValidator.beacon`) demands a
`Beacon`, and the *only* constructor of a `Beacon` is `oneShot`, which contains the seed
check. Uniqueness becomes a type-system consequence.

Off-chain, the matching `TxBuilder` helper must pick the seed and set the policy parameter
from the same value, so the on-chain/off-chain mismatch (a separate common bug) is also
removed.

---

#### MI-3 · Burn checks: sign confusion, partial burn accepted · **ELIMINATE** · rank 20

**Mechanism.** Burning is a negative mint. Two errors are common: comparing with `<=`
("burn at least N") which accepts a partial burn, and getting the sign backwards so a mint
passes a burn check. A partial burn leaves live authenticator tokens in circulation, which
is AU-1 with extra steps.

**Attack tx.** Protocol requires "burn the 100 donation tokens"; check is
`mint.quantityOf(p, n) <= -100`; attacker burns 100 but also mints 100 under a different
name (MI-1), or the check is `>= -100` and the attacker burns 1.

**Naive code.**

```scala
require(tx.mint.quantityOf(policyId, name) <= -tokenCount, "must burn")
```

**Correct check.** `requireMintExactly(policyId, SortedMap.singleton(name, -tokenCount), tx)`.

**API-level fix.** Same as MI-1: there is one mint assertion and it is exact and whole-map.
`requireBurnAll` takes a **positive** `qty` and negates internally, so the caller never
writes a sign. A `Burn`/`Mint` sum type on the quantity would go further:

```scala
enum MintDelta:
    case Mint(qty: BigInt)   // require qty > 0 at construction
    case Burn(qty: BigInt)   // require qty > 0 at construction
```
so `-` never appears in user code and a sign error is unrepresentable.

---

#### MI-4 · Infinite mint via a forwarding policy's unguarded redeemer · **ELIMINATE** · rank – (see PU-1)

**Mechanism.** A forwarding minting policy authorises minting whenever a given validator
runs. If *any* redeemer of that validator does not itself constrain the mint — Plutonomicon's
example is a `WitnessMyState` redeemer meant only to allow read-only inspection — then
minting is unconstrained under that redeemer.
<https://plutonomicon.github.io/plutonomicon/vulnerabilities>

**Correct check.** Every redeemer branch of the forwarded-to validator must constrain
`tx.mint` for the forwarding policy.

**API-level fix.** This is PU-1 in a minting costume: the fix is that the framework
requires *every* branch of the redeemer enum to state its mint expectation. See PU-1.
Concretely, `SpendingValidator` can require a total function
`def mintPolicy(r: R): SortedMap[TokenName, BigInt]` when the validator declares a coupled
policy, which the framework checks with `requireMintExactly` before dispatching. A branch
that wants no mint writes `SortedMap.empty`, explicitly.

---

#### MI-5 · Missing "only my policy" scope on the *spend* side · **ELIMINATE**

**Mechanism.** The mirror image of MI-1: a spending validator that requires "an NFT is
present" without pinning the policy id accepts any NFT, including one the attacker minted
under their own policy.

**Correct check.** Always pin `(policyId, tokenName)` together; never match on token name
alone.

**API-level fix.** `AssetClass` is a single type carrying both halves, and every
quantity helper takes an `AssetClass`, never a bare `TokenName`. Scalus's
`quantityOf(cs, tn)` already takes both; the rule is that the high-level API must not offer
a name-only variant, and `Value.tokens(cs)` (which returns the whole sub-map for one policy)
is the right shape for enumerating.

---

### TI – Time family

Ledger facts every entry below depends on (verified):
`invalid_before` is **inclusive**, `invalid_hereafter` is **exclusive**
(<https://docs.cardano.org/about-cardano/explore-more/time>); a script never sees "now",
only the transaction's self-declared validity interval, which the transaction *author*
chooses; slot→POSIXTime conversion is done by consensus (era history), not by the script,
so a script cannot reconstruct wall-clock time from a slot across an era boundary.

#### TI-1 · Unbounded validity range silently used as "now" · **ELIMINATE** · rank 8

**Mechanism.** A validity interval bound may be infinite. Any helper that turns "the lower
bound" into a `BigInt` must invent a value for the infinite case. If it invents `0`, then a
transaction with **no** lower bound is treated as happening at the Unix epoch, and every
"has the deadline passed?" comparison silently flips.

**This is a live footgun in the current Scalus API.**
`TxInfo.getValidityStartTime` (`v3/Contexts.scala:1102`) returns `BigInt(0)` for
`NegInf`/`PosInf`, and `IntervalBound.finite(default)` invents a caller-supplied default.
The in-house **Vault** finding is exactly this: "deadline derived from
`getValidityStartTime` (lower bound / 0 if unbounded), so a backdated `validFrom` makes
`finalize` pass immediately".

**Attack tx.**

```
Validity: invalid_before = <absent>, invalid_hereafter = <absent>
Inputs:   vault @ script, datum = { unlockAt: 1_800_000_000 }
Outputs:  everything -> attacker
```
`getValidityStartTime` → `0`; `0 + waitPeriod < 1_800_000_000` is false, or the comparison
is written the other way and passes. Either way the time lock never binds.

**Naive code.**

```scala
val now = tx.getValidityStartTime          // 0 when unbounded
require(now >= datum.unlockAt, "too early")
```

**Correct check.** Never project the interval to a scalar. Compare *intervals*:

```scala
require(tx.validRange.isEntirelyAfter(datum.unlockAt), "not yet unlocked")
```
`isEntirelyAfter` already returns `false` for an infinite lower bound – it **fails closed**,
which is the right default and is worth preserving.

**API-level fix.** Three concrete moves:

1. **Delete `getValidityStartTime` from the high-level surface** (deprecate in the low-level
   one). A function whose contract is "returns 0 if unbounded" is a trap by construction.
2. **`now` is not a scalar.** If a protocol genuinely needs a number, it must first prove
   the bound is finite:
   ```scala
   /** The transaction's lower bound as a POSIX time.
     * FAILS the script if the lower bound is not finite.
     * There is no defaulting variant. */
   def requireLowerBound(tx: TxInfo): PosixTime

   /** Both bounds, with a maximum permitted width.
     * Forces the author to think about how wide an attacker may make the window. */
   def requireBoundedRange(tx: TxInfo, maxWidth: BigInt): (PosixTime, PosixTime)
   ```
3. **Interval-level predicates are the primary API** and read as obligations:
   ```scala
   def requireAfter(deadline: PosixTime, tx: TxInfo): Unit   // isEntirelyAfter, else fail
   def requireBefore(deadline: PosixTime, tx: TxInfo): Unit  // isEntirelyBefore, else fail
   ```

Note the deeper point: **the transaction author picks the validity range**, so a wide range
is an attacker capability, not an accident. `requireBoundedRange` is the only way to bound
that capability, and no current Cardano stdlib offers it.

---

#### TI-2 · Interval bound inclusivity mishandled; improper ("never") intervals · **ELIMINATE** · rank 27

**Mechanism.** `Interval` carries an `isInclusive` flag on each bound, so four
combinations exist per comparison and it is easy to get one wrong. Worse, an *improper*
interval (`from > to`) denotes the empty set – no slot satisfies it, so such a transaction
can never be included in a block, but a validator that pattern-matches on bounds may still
"pass" it, which matters when the validator's result is being relied on elsewhere (e.g. a
cost model or an off-chain simulation). The in-house **HTLC** finding is the mild version:
"two error-message strings have inclusive/exclusive wording swapped;
`finite`/`finiteOrFail` discard the bound's closure flag".

**Attack tx.** Depends on the protocol; the canonical case is a deadline exactly on the
boundary, where an off-by-one lets a claim land one slot after the refund window opened,
so both the claimant and the refunder have a valid path.

**Naive code.**

```scala
val to = tx.validRange.to.finiteOrFail("no upper bound")   // discards isInclusive
require(to <= deadline, "too late")
```

**Correct check.** Use the closure-aware predicates (`isEntirelyBefore` /
`isEntirelyAfter`), never the raw bound.

**API-level fix.** Adopt the **normalised interval** as the only shape user code sees.
Scalus already ships `NormalizedInterval`
(`scalus-design-patterns/src/main/scala/scalus/patterns/NormalizedInterval.scala`, ported
from the Anastasia Labs pattern): four cases (`ClosedRange`, `FromNegInf`, `ToPosInf`,
`Always`), all bounds inclusive, improper intervals rejected. Promote it out of
design-patterns into the stdlib, and have the framework normalise `tx.validRange` **once**,
before user code runs:

```scala
final case class Own[D](..., validRange: NormalizedInterval)
```
Then `isInclusive` never appears in user code, `never` intervals fail before `spend` is
entered, and `finite`/`finiteOrFail` (which discard the closure flag) become unnecessary.
<https://github.com/Anastasia-Labs/design-patterns/blob/main/validity-range-normalization/VALIDITY-RANGE-NORMALIZATION.md>

---

### DT – Datum and data family

#### DT-1 · Datum continuity: immutable fields not pinned ("datum hijacking") · **ELIMINATE** · rank 12

**Mechanism.** A state transition validates the *changed* field and forgets the rest. The
attacker rewrites `owner` while preserving `amount`, and the validator sees a well-formed
datum. The Developer Portal calls this **datum hijacking** and gives exactly this shape:
`{owner: "Alice", amount: 100}` → `{owner: "Attacker", amount: 100}`.

**Attack tx.**

```
Input:   100 ADA @ script, datum = { owner: Alice, counter: 7 }
Outputs: 100 ADA @ script, datum = { owner: ATTACKER, counter: 8 }
```
Value preserved, counter incremented correctly, ownership stolen.

**Naive code.**

```scala
val newD = out.datum.to[State].get
require(newD.counter === old.counter + 1, "counter must advance")
// owner never compared
```

**Correct check.** Compare *every* field, or compare the whole datum against a
constructed expectation.

**API-level fix.** *Make the expected datum a required, whole-value argument.* The
`continuing` helper takes `datum: D2` – the **complete** expected next datum – and compares
by `toData` equality (cheap under V3 lowering, per the repo's own guidance):

```scala
val out = continuing(
    own,
    value   = ValuePolicy.Preserve,
    datum   = own.datum.copy(counter = own.datum.counter + 1),   // everything else pinned
    address = AddressPolicy.SameAsInput,
    tx      = tx
)
```

`copy` is the mechanism: every field the author does not name is carried over *by
construction*, so "forgot to pin `owner`" is not expressible. There is deliberately **no**
`continuing(own, datumPredicate: D2 => Boolean)` overload – a predicate is exactly the shape
that lets a field go unchecked.

Where the next datum genuinely depends on transaction data (e.g. a new deadline read from
the redeemer), the author still writes a full value, so the "which fields may move" decision
is forced into one visible expression.

---

#### DT-2 · Untyped / lazy `Data` decoding – `.to[T]` validates nothing · **LOUD** · rank 19

**Mechanism.** Scalus's derived `FromData` is (deliberately) lazy and structural: `.to[T]`
does not walk the value and does not check the shape. Field access is what forces
decoding, so an attacker-supplied datum with the wrong constructor index or arity may pass
through several checks before failing – or never fail at all, if the validator only touches
the fields that happen to line up. The same is true of Aiken's `expect` on untrusted data
and of PlutusTx's `unsafeFromBuiltinData`.

The repo has already researched this and concluded: keep the lazy no-op `fromData`, add an
**opt-in** deep `expect` (see the datum-shape-validation research note). This entry is the
security argument for making that opt-in *loud* rather than obscure.

**Attack tx.** Attacker locks a UTxO at the script address with a datum that is
`Constr(3, [...])` where the validator expects `Constr(0, [pkh, amount])`. Downstream, a
field read returns whatever happens to be at that position.

**Naive code.**

```scala
val d = own.datum.to[State]     // no validation happens here
require(d.amount > 0, "positive")
```

**Correct check.** Validate the shape at the trust boundary, once, for any datum the
protocol did not itself create under its own validator.

**API-level fix.** Make the *trust status* of a datum part of its type:

```scala
/** Datum that this protocol's own validator vetted when the UTxO was created.
  * Only obtainable via `Own[D]` on a BeaconedValidator, or via `readAuthenticated`. */
opaque type Trusted[D] = D

/** Any Data from an unauthenticated source. Cannot be used until validated. */
opaque type Untrusted[D] = Data

/** The only bridge. Performs a deep structural check (the opt-in T9 `expect`). */
def validate[D: FromDataStrict](u: Untrusted[D]): D
```

Then `Own[D].datum` is `D` (already trusted, because the beacon proved ancestry), while
anything read from an arbitrary `TxOut` is `Untrusted[D]` and must go through `validate`.
The cost of the deep check is paid exactly where it is needed, and skipping it is a type
error rather than an omission.

Even without the opaque types, the minimum viable version is: **the high-level API never
exposes a raw `TxOut.datum.to[T]`.** Reading a datum from a non-own output goes through
`readAuthenticated` (AU-1), which is beacon-gated anyway.

---

#### DT-3 · Datum-hash bricking / missing datum on the continuing output (`arbitrary-datum`) · **ELIMINATE** · rank 21

**Mechanism.** Two distinct failure modes with one root cause – the datum on a script
output is not constrained:

* **Bricking by hash.** An output at a script address may carry only a *datum hash*.
  Spending it requires supplying the preimage as a witness. If the preimage is never
  published (lost, or deliberately bogus), the UTxO is **permanently unspendable**. Since
  anyone may create an output at any address, an attacker can plant such UTxOs cheaply. If
  the protocol's logic must consume "all UTxOs at the script address", the protocol halts.
  CIP-32 (inline datums) exists to remove the hash indirection.
  <https://cips.cardano.org/cip/CIP-32>
* **Bricking by shape.** MLabs' `arbitrary-datum`: the protocol locks an output without
  validating datum structure; later, a legitimate transaction fails because the datum does
  not decode. "Unspendable outputs, protocol halting."

Note that in **Plutus V3 the spending datum is `Option[Datum]`** – it may be entirely
absent. A validator that assumes a datum is present must say so.
<https://cips.cardano.org/cip/CIP-0069>

**Attack tx.** Attacker sends `min-ADA @ scriptAddress` with `datumHash = blake2b(random)`,
no preimage anywhere. Protocol's batcher, which sweeps all script UTxOs, can no longer
build a valid transaction.

**Naive code.**

```scala
val d = own.datum.get          // "None.get" on a datum-less V3 input
```
```scala
require(out.address === own.address, "continues")
// output's datum never constrained -- may be a hash, or absent
```

**Correct check.** On every output the protocol creates: require an **inline** datum with
the exact expected value. On every input the protocol consumes: reject anything without a
usable datum, and design so that planted UTxOs are ignorable (beacon-gated, AU-1/AU-5).

**API-level fix.**

1. `continuing(...)` writes and checks an **inline** datum. There is no
   `continuing(..., datumHash = …)` variant; a hash-datum output must be constructed
   through a separate, named, documented helper.
2. `Own[D].datum` is a plain `D`, not an `Option[D]`. A validator that tolerates a
   datum-less input declares it:
   ```scala
   @Compile
   trait SpendingValidator[D, R]:
       /** Default: a missing datum fails the script with a specific message. */
       def datumRequired: Boolean = true
   ```
   This removes the whole `datum.get` vs `datum.getOrFail(msg)` class (four in-house Low
   findings: SimpleTransfer, Vault, "many").
3. Protocol UTxOs are located by **beacon**, never by "everything at my address" (AU-1), so
   planted junk is invisible to the sweeper and bricking-by-hash becomes a non-event.

---

### IX – Index and ordering family

Ledger facts (verified): the ledger **re-sorts transaction inputs lexicographically** by
`(transaction_id, index)` – submission order is *not* preserved
(<https://cips.cardano.org/cip/CIP-0128>, which proposes changing this and was **not** in
the Dijkstra Phase 1 scope as of Aug 2026). **Outputs are a list and preserve submission
order.** So input indices are stable but *not* under the transaction author's direct
control, and output indices are entirely under the author's control.

#### IX-1 · Index-list handling: duplicates, length mismatch, `zip` truncation · **ELIMINATE** · rank 10

**Mechanism.** Batch validators take a list of indices in the redeemer to avoid O(n²)
scans. Three independent bugs live there: the same index appearing twice (one payout
counted for two obligations); the index list being *shorter* than the item list, with
`zip` silently truncating; and indices being out of range.

**In-house instance, Critical:** **Crowdfunding** – "`reclaimerOutputIndices`
length/uniqueness unchecked; `zip` truncation lets an attacker reclaim all donation UTxOs
while paying out only a prefix, sweeping the rest as change."

**Attack tx.**

```
Inputs:  20 donation UTxOs @ script
Redeemer: Reclaim(reclaimerOutputIndices = [0])     <- one index for twenty inputs
Outputs: [0] refund to donor #1
         [1] everything else -> attacker (as "change")
```
`donations.zip(indices)` yields one pair; nineteen refunds are never checked.

**Naive code.**

```scala
donations.zip(indices).foreach { (d, i) =>
    require(tx.outputs.at(i).address === d.donor, "refund")
}
```

**Correct check.**

```scala
require(indices.length === donations.length, "one index per donation")
requireStrictlyAscending(indices)                  // no duplicates, and deterministic
```

**API-level fix.** Encode the invariants in the *type* of the redeemer field, so decoding
enforces them:

```scala
/** A list of indices that is strictly ascending by construction.
  * Its FromData instance REJECTS non-ascending or duplicate input, so a redeemer
  * carrying duplicates never reaches user code. */
opaque type AscendingIndices = List[BigInt]

/** A one-to-one pairing of inputs to outputs, strictly ascending in the input index. */
opaque type IndexPairs = List[(BigInt, BigInt)]
```

and replace `zip` with a total, length-checked combinator:

```scala
/** Fails if the two lists have different lengths. There is no truncating zip
  * in the high-level API. */
def zipExact[A, B](xs: List[A], ys: List[B]): List[(A, B)]
```

`zip`-that-truncates is a general-purpose-language convenience that has no business in a
validator stdlib. The same argument applies to `take`, `drop` and `at` on redeemer-derived
indices: the safe form fails loudly, and the truncating form should not be reachable from
the high-level surface.

Scalus's `List.at` already fails on out-of-range, which is correct. Keep it.

---

#### IX-2 · `missed-input`: an extra script input that no index covers · **ELIMINATE** · rank 16

**Mechanism.** The UTxO-indexer pattern (and the withdraw-zero "global validator" pattern)
delegates per-input validation to a single pass driven by redeemer-supplied indices. If the
global validator checks only the inputs the indices *name*, an attacker adds one more
script input that no index names – and that input is spent with **no validation at all**.
The Developer Portal identifier is `missed-input`.
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/unchecked-inputs/>

**Attack tx.**

```
Inputs:  scriptUtxo0 (indexed), scriptUtxo1 (indexed), scriptUtxo2 (NOT indexed)
Redeemer (global): pairs = [(0,0), (1,1)]
Outputs: correct continuations for 0 and 1; utxo2's value -> attacker
```

**Naive code.** Any global validator that iterates `indexPairs` rather than iterating the
inputs.

**Correct check.** Iterate the **input list**, not the index list, and assert that every
input at the protocol's credential is covered – "no gaps, no duplicates".

**API-level fix.** Scalus's existing `UtxoIndexer.multiOneToOneNoRedeemer` **already gets
this right** and is worth calling out as the model: it walks `tx.inputs`, and when it meets
a script-credential input with no pair left it calls
`fail(MoreScriptUtxosSpentThanSpecified)`; at the end it requires `remainingPairs.isEmpty`.
Both directions are covered.

The stdlib rules that generalise it:

1. **Coverage is checked by the framework, not by user code.** Under
   `OwnInputPolicy.Indexed`, the framework performs the walk and only then calls the
   user's per-pair block. The user cannot forget it because the user does not write it.
2. **The per-pair block is `Unit`-returning** (rule R1), so a `false` cannot be swallowed.
   Today's `UtxoIndexer` takes `(BigInt, TxInInfo, BigInt, TxOut) => Boolean`, which loses
   the failure reason and invites `&&` chains – it should become a `=> Unit` block.
3. Note the Anastasia Labs caveat, which must stay in the scaladoc: the **singular**
   indexer patterns (`oneToOne`, `oneToMany`) do **not** protect against DS-1 – they solve
   `missed-input`, not double satisfaction. Today's Scalus README already says this; the
   API should say it too, in the type: `oneToOne` should require
   `ownInputPolicy = Exclusive | TaggedOutputs`.

---

#### IX-3 · Ordering assumptions: inputs *are* sorted; outputs are **not** · **ELIMINATE** · rank 25

**Mechanism.** A validator that pairs "the i-th input with the i-th output" is relying on
two different guarantees. The input side is stable but attacker-influenceable: the ledger
sorts inputs lexicographically by `(txId, index)`, so an attacker who controls one of their
own UTxO's refs (by grinding a txId, DE-4) can influence where their input lands. The
output side has *no* guarantee at all beyond "the author chose this order". Positional
matching without an explicit binding is therefore unsound in both directions.

**Attack tx.** Attacker adds a dust input whose `(txId, idx)` sorts before the protocol's,
shifting every index by one.

**Naive code.**

```scala
tx.inputs.zip(tx.outputs).foreach { (i, o) => require(o.value === i.resolved.value) }
```

**Correct check.** Bind explicitly: by `outRef` (`input.outRef === own.ref`), by beacon, or
by redeemer index *with* an `outRef` assertion.

**API-level fix.** The framework never hands the user a bare index. `OwnInputPolicy.Indexed`
asserts `tx.inputs.at(inIdx).outRef === own.ref` *for the caller* – which is precisely what
`UtxoIndexer.oneToOne` already does with `require(input.outRef === ownRef, InputIndexMismatch)`.
Document the ordering facts in one place (an `Ordering` scaladoc page) and state the rule:
**an index is only ever a hint; the binding must be by `TxOutRef` or by beacon.**

---

### PU – Script purpose family

#### PU-1 · "Other redeemer" / purpose confusion · **ELIMINATE** (largely done) · rank 13

**Mechanism.** A script that runs under several purposes (or several redeemer branches)
may have one branch whose checks are weaker. Because *all* the checks the protocol relies
on must hold under *every* path that can spend/mint, the weakest branch defines the
security level. MLabs' property: "Logic under one script redeemer that relies on the logic
enforced by another redeemer … explicitly requires the presence of the redeemer under which
the intended logic exists."

MLabs' worked example is a staking protocol where a new `AddRewards` redeemer was added to
the global validator; an attacker consumed the global state with `AddRewards` instead of
`UpdateState`, bypassing the position validation the protocol assumed had run. Plutonomicon's
**infinite mint** (MI-4) is the same shape in a minting policy.

**Attack tx.** Spend the UTxO with the weak redeemer; every other script in the transaction
believes the strong redeemer's invariants hold.

**Naive code.**

```scala
// positionsValidator: "the global validator will check everything"
def spend(...) = require(tx.inputs.exists(_.resolved.address.credential === globalCred))
// but does not require WHICH redeemer the global validator ran with
```

**Correct check.** Assert the exact redeemer constructor of the counterpart script:

```scala
val r = tx.redeemers.getOrFail(ScriptPurpose.Spending(globalRef), "global must run")
require(r.to[GlobalAction] === GlobalAction.UpdateState, "wrong global redeemer")
```

**API-level fix.** Two halves, one of which Scalus already has:

1. **Unimplemented purposes fail by default.** The Scalus compiler plugin already generates
   a default `fail` for every purpose a validator does not implement, which is why the
   security-review skill records V010 as "always a false positive for single-purpose Scalus
   validators". This is exactly the right default and should be documented as a deliberate
   safety property, not an implementation detail.
2. **Cross-script dependency is expressed as a value, not as a convention.**
   ```scala
   /** Asserts that `script` runs in this transaction under `purpose`
     * AND that its redeemer equals `expected`. Both halves in one call –
     * the "it ran" half alone is the bug. */
   def requireRanWith[R: ToData](purpose: ScriptPurpose, expected: R, tx: TxInfo): Unit
   ```
   Compare Scalus's existing `StakeValidator.spend`, which takes
   `withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean` – the right idea, but the
   `Boolean` return violates rule R1 and the redeemer check is *optional*
   (`spendMinimal` skips it). `spendMinimal` is precisely the PU-1 shape and its scaladoc
   should say so.

---

#### PU-2 · Withdraw-zero forwarding pitfalls · **ELIMINATE** (pattern exists) · rank 31

**Mechanism.** The "withdraw-zero trick": because the ledger does not filter zero-value
entries out of the withdrawals map, a transaction can withdraw exactly 0 lovelace from a
registered stake credential whose script is a Plutus script, forcing that script to run
**once for the whole transaction** instead of once per input. It turns O(n²) validation
into O(n). Scalus ships it as `scalus.patterns.StakeValidator` (a ~71% budget reduction on
the payment-splitter benchmark).

Four things go wrong:

| Pitfall | Consequence |
|---|---|
| The forwarding spend checks only "the withdrawal exists", not which redeemer it ran with | PU-1 |
| The global withdrawal validator checks aggregates, not per-input bindings | DS-1 at scale – the withdraw-zero shape is where this bug most often appears |
| The global validator iterates the index list instead of the input list | IX-2 (`missed-input`) |
| The stake credential must be **registered** on L1, and registration/deregistration is a certificate purpose the script also gates | PU-3 |

Anastasia Labs' own `STAKE-VALIDATOR.md` carries a "Double Satisfaction Security Warning"
for exactly this reason: "it is imperative to ensure that each script input is uniquely
associated with an output".
<https://github.com/Anastasia-Labs/design-patterns/blob/main/stake-validator/STAKE-VALIDATOR.md>

**API-level fix.** Make the coupling a *framework* concern rather than a pattern the user
wires up:

```scala
/** A validator whose heavy logic runs once, in the reward endpoint.
  * The framework injects the spend-side check (withdrawal present AND redeemer matches),
  * runs the reward-side logic over ALL own-credential inputs, and enforces
  * input/output coverage. The user writes only the per-pair rule. */
@Compile
trait ForwardedValidator[D, R] extends SpendingValidator[D, R]:
    def stakeCredential: Credential
    def perPair(own: Own[D], out: TxOut, tx: TxInfo): Unit    // Unit, not Boolean
```

Also: when CIP-112's `Observe` purpose ships, this trait's implementation swaps from
withdraw-zero to `Observe` with **no change to user code** – which is the strongest argument
for wrapping the trick rather than documenting it.
<https://cips.cardano.org/cip/CIP-0112>

---

#### PU-3 · Certificate purposes unguarded – deregistration griefing + deposit theft · **ELIMINATE** · rank 32

**Mechanism.** A stake credential controlled by a script is gated by that script for
**every** certificate action, not just withdrawals. A script that handles only the
rewarding purpose and approves anything else lets an unrelated party submit a
**deregistration** certificate: the credential is deregistered (so every withdraw-zero
forwarding spend now fails – protocol liveness halted until re-registration) **and the key
deposit is refunded to the attacker's chosen account**. The mirror attack is an
unsolicited registration/delegation, which makes rewards accrue and breaks off-chain code
that assumes a zero balance.
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/staking-and-certificates/>

**Attack tx.**

```
Certificates: [ StakeDeregistration(scriptCredential) ]
Redeemer:     Certifying(0, ...) -> whatever the script accepts
Outputs:      deposit refund -> attacker
```

**Naive code.** A `reward`-only validator whose `certify` is permissive – or, in a
hand-written dispatcher, a `case _ => ()` fallthrough.

**Correct check.** Handle the certifying purpose explicitly and deny deregistration unless
the protocol genuinely intends it.

**API-level fix.** Scalus's default-`fail`-for-unimplemented-purposes already does the right
thing, and this entry is the concrete reason to keep it and never soften it. Beyond that:

```scala
/** Certificate policy for a script-controlled stake credential.
  * `ForwardingOnly` (the default for ForwardedValidator) denies registration,
  * deregistration and delegation outright. */
enum CertPolicy:
    case ForwardingOnly
    case Custom(rule: (TxCert, TxInfo) => Unit)
```
`ForwardedValidator` sets `CertPolicy.ForwardingOnly` by default, so a stake credential
created purely to host the withdraw-zero trick cannot be deregistered by a third party
unless the author opts out.

---

#### PU-4 · Voting / proposing purposes unguarded · **ELIMINATE** · rank 43

**Mechanism.** Plutus V3 added `VotingScript(Voter)` and
`ProposingScript(Int, ProposalProcedure)`. A DRep or constitution script that dispatches on
`ScriptInfo` without handling these silently approves governance actions. The
`ProposingScript`/`CertifyingScript` variants also carry a **0-based index** into
`txInfoProposalProcedures` / `txInfoTxCerts`, so index-confusion (IX-3) applies here too.
<https://cips.cardano.org/cip/CIP-0069>

**API-level fix.** Same default-fail property, plus dedicated high-level traits
(`DRepValidator`, `ConstitutionValidator`) that receive the already-resolved
`ProposalProcedure` / `Voter` and the *index-checked* certificate, so the author never
indexes `tx.certificates` by hand.

---

### EV – Evaluation and toolchain family

#### EV-1 · Evaluation-order trap: a required check short-circuited away · **ELIMINATE** · rank 17

**Mechanism.** UPLC control flow is lazy. `&&` and `||` short-circuit, `if`/`match` evaluate
only the taken branch, and `error`/`fail` only fires when *forced*. A security-relevant
predicate placed on the right of `||`, or inside an untaken branch, silently never runs.
The Developer Portal identifier is `evaluation-order`: "Every check that must run to keep
the validator safe is actually forced, not placed on a branch that can be skipped."
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/evaluation-and-grinding/>

**Attack tx.** A transaction that satisfies the cheap left-hand disjunct, so the expensive
right-hand check (the one that actually protects the funds) never evaluates.

**Naive code.**

```scala
require(isEmergency || (tx.isSignedBy(owner) && valuePreserved(out)), "…")
// when isEmergency is true, NOTHING else is checked
```

**Correct check.** Every obligation is its own statement.

```scala
require(isEmergency || tx.isSignedBy(owner), "authorized")
require(valuePreserved(out), "value preserved")     // unconditional
```

**API-level fix.** This is **rule R1** from §3, and it is the single most mechanical rule in
this document: **no public API takes a `Boolean`-returning callback.** Every user-supplied
rule is a `Unit`-returning block whose only failure mode is a `require` with a message.

Concretely this changes the existing Scalus pattern library:

| Today | Proposed |
|---|---|
| `UtxoIndexer.oneToOne(…, validator: (TxInInfo, TxOut) => Boolean)` | `(TxInInfo, TxOut) => Unit` |
| `StakeValidator.spend(…, withdrawalRedeemerValidator: (Redeemer, Lovelace) => Boolean)` | `(Redeemer, Lovelace) => Unit` |
| `MerkelizedValidator` / `TransactionLevelMinterValidator` `…Validator: … => Boolean` | `… => Unit` |

The benefits compound: the failure message survives to the trace, `&&` chains become
sequential statements (each independently forced), and a "forgot to return the last
condition" bug becomes impossible because there is no return value. Aiken's own idiom
points the same way – its docs recommend explicit `and { … }` / `or { … }` blocks that list
every condition rather than operator chains.

Where a genuine predicate is needed (a filter, a search), it is a `Boolean` — but a
*validation rule* never is.

---

#### EV-2 · Compiler-level JVM ↔ on-chain divergence (Scalus-specific) · **ELIMINATE via test kit** · rank 46

**Mechanism.** Scalus validators are ordinary Scala 3, so a developer's unit tests may run
on the JVM while the deployed artifact is UPLC. Any place where the compiler's lowering
diverges from Scala semantics is a silent, undetectable-by-testing hole.
`docs/internal/UPLC_CORRECTNESS_AUDIT.md` records several that were **live and shipped**,
now fixed:

| ID | Divergence | Class |
|---|---|---|
| E1 | `BigInt./` maps to floored `divideInteger`; Scala truncates toward zero | wrong value for negative operands |
| I1 | User-defined `Eq` instances silently replaced by structural equality | wrong comparison |
| M1/M2 | Pattern-match rows misordered or dropped | wrong branch taken |
| E2 | By-name parameters compiled strict | laziness lost (interacts with EV-1) |
| E3 | `x.copy(all fields explicit)` dropped the receiver expression | a `validate(datum).copy(…)` whose validation throws silently succeeded |
| X1 | Wildcard-only match dropped an effectful scrutinee | check skipped |

E3 is the most alarming for this document: *a validation call was silently discarded by the
compiler*. That is EV-1 caused by the toolchain rather than by the author.

**API-level fix.** Not a signature — a **differential test obligation**:

```scala
/** Runs the same validator input through the JVM implementation AND the compiled UPLC,
  * asserting identical accept/reject and identical trace. The high-level stdlib's own
  * test kit runs this for every documented example. */
def assertJvmMatchesOnChain[A](f: A => Unit, input: A): Unit
```
The repo already has the ingredients (`PlutusV3.compile` gives both `CompiledPlutus.code`
on the JVM and `.program.evaluateDebug()` on-chain from one source). Making differential
testing the *default* shape of the stdlib's own test helpers is how this class stays closed.

---

### AR – Arithmetic family

#### AR-1 · Division rounding direction; fee-rounding exploitation · **LOUD** · rank 35

**Mechanism.** On-chain integers are unbounded, so classic overflow does not apply. What
*does* apply is rounding. `divideInteger` floors (rounds toward −∞) while `quotientInteger`
truncates (rounds toward zero); the two differ for negative operands. And in any protocol
that computes a fee or a share as `amount * rate / denominator`, the direction of rounding
decides who absorbs the remainder. An attacker who can split one large action into many
small ones harvests one rounding unit each time.

**Attack tx.** Instead of one 1,000,000-lovelace swap paying a 0.3% fee, the attacker
submits 1,000 swaps of 1,000 lovelace each where the fee rounds down to zero.

**Naive code.**

```scala
val fee = amount * feeNumerator / feeDenominator   // rounds toward -inf; who wins?
```

**Correct check.** Round *against* the party being protected against, explicitly, and
enforce a minimum:

```scala
val fee = ceilDiv(amount * feeNumerator, feeDenominator)
require(fee >= minFee, "below minimum fee")
```

**API-level fix.**

1. Do not expose a bare `/` on protocol quantities. Expose named, direction-explicit
   operations: `divFloor`, `divCeil`, `divTruncate`, and make the *name* mandatory.
2. Ship `ceilDiv` / `floorDiv` and a `mulDiv(a, b, c, rounding)` helper, so the common
   "proportional share" computation is one call with the rounding choice as an argument.
3. Note for the Scalus port specifically: `BigInt./` lowers to `divideInteger` (floored),
   **not** Scala's truncation. This was audit finding E1 and it is now fixed, but the
   lesson holds: on-chain arithmetic should not reuse the host language's operator when the
   semantics differ.

---

#### AR-2 · Negative quantities accepted where positive assumed · **ELIMINATE** · rank 36

**Mechanism.** Datum and redeemer fields are attacker-supplied. A `BigInt` amount that the
protocol assumes is positive can be negative, which inverts every comparison built on it
(`balance - amount > 0` passes when `amount` is negative, and the balance grows).

**Attack tx.** `Withdraw(amount = -1_000_000)`: the "remaining balance" check passes, the
payout is computed as a negative number, and the arithmetic elsewhere credits the attacker.

**Naive code.**

```scala
case class Withdraw(amount: BigInt)
require(datum.balance - r.amount >= 0, "insufficient")
```

**Correct check.** `require(r.amount > 0, "amount must be positive")` at the boundary.

**API-level fix.** Refine at the decoding boundary, once, in the type:

```scala
/** A BigInt proven > 0. Its FromData instance rejects zero and negatives,
  * so a redeemer or datum carrying a bad amount never reaches user code. */
opaque type Positive <: BigInt
opaque type NonNegative <: BigInt
```
Protocol datums then say `amount: Positive` and the check is performed by the framework at
decode time, exactly once, with a specific error message. This is strictly better than a
`require` in every branch, which is the thing people forget.

Pair with a `Quantity`/`Lovelace` opaque type so that mixing a token count with a lovelace
count is a type error.

---

#### AR-3 · Int64 boundary at ledger serialisation · **LOUD** · rank 44

**Mechanism.** Plutus `Integer` is unbounded, but the *ledger's* representation of a token
quantity in a transaction output is bounded (CBOR int64 in practice). A validator can
compute and compare an amount that no valid transaction could ever carry, so a check that
passes on-chain may correspond to a transaction that cannot be built – a liveness bug – or,
in a protocol that stores an amount in a datum and settles it later, an accounting divergence.

**API-level fix.** The `Positive`/`Quantity` opaque types (AR-2) validate against the
ledger's bound at construction, and the off-chain `TxBuilder` shares the same constant. The
`LOUD` marking reflects that the exact bound is a ledger detail worth pinning in one place
and asserting in a test rather than assuming.

---

### RS – Resource and DoS family

Current mainnet limits (Conway, PV11, sampled epoch 651):
`coinsPerUTxOByte` **4,310**; `maxValueSize` **5,000 B**; `maxTxSize` **16,384 B**;
`maxTxExecutionUnits` **16,500,000 mem / 10,000,000,000 steps**; `maxCollateralInputs` **3**;
`minFeeRefScriptCostPerByte` **15**. There is **no** `maxDatumSize` parameter; datum size is
bounded only indirectly by `maxTxSize`.

#### RS-1 · Token dust / unbounded value on a protocol UTxO · **LOUD** · rank 28

**Mechanism.** Anyone may add any native tokens to any output they create. If a protocol's
UTxO accumulates hundreds of distinct asset classes, three things break: the `Value`
approaches `maxValueSize` (5,000 B) so no valid output can be built; any validator that
iterates the value blows the ExUnits budget; and a validator that pattern-matches a
single-asset value (`let [(cs,tn,amt)] = flattenValue v`) fails outright.

**Attack tx.** Attacker mints 300 distinct one-lovelace-worth tokens and sends them into
the protocol treasury UTxO. Every subsequent legitimate spend now exceeds a limit. The
protocol halts, permanently, for the price of a few transactions.

**Naive code.** Any `ValuePolicy.Preserve` on a UTxO whose token set is not constrained at
the *deposit* boundary.

**Correct check.** Constrain the token set on every output the protocol creates:

```scala
require(out.value.policyIds.length <= maxPolicies, "too many asset classes")
// or, better:
require(out.value === Value.lovelace(ada) + Value(poolPolicy, poolName, n), "unexpected assets")
```

**API-level fix.** The `AssetPolicy` from VP-6, enforced by `continuing(...)` and by every
output-creating helper:

```scala
enum AssetPolicy:
    case AdaOnly
    case Allowed(classes: List[AssetClass])
    case Open(justification: String)
```
`AdaOnly` and `Allowed` are cheap (one `hasOnly`/`tokens` comparison), and `Open` is the
greppable opt-out. The default for `BeaconedValidator` is `Allowed(List(beacon))` plus ADA,
which is what almost every protocol actually wants.

**Prior art worth copying.** Aiken's `Value` is an opaque type that **never contains a
zero-quantity entry**, enforced by every constructor – a footgun closed by construction
after two CHANGELOG fixes (v1.3.0 "clear empty asset lists", v1.4.0 "missing null-check on
`value.add`"). Scalus's `Value` has the same normalisation primitives; the rule is to make
them the *only* reachable path.
<https://github.com/aiken-lang/stdlib/blob/main/CHANGELOG.md>

---

#### RS-2 · Unbounded datum growth · **DETECT** · rank 29

**Mechanism.** A datum containing a growable collection (`List[Participant]`,
`Map[String, PubKeyHash]`) eventually exceeds what a transaction can carry or what a
validator can traverse within budget – and then the UTxO is unspendable forever. MLabs'
property statement asks for "an upper bound … low enough to not prevent consumption of the
UTxO as an input in a future transaction".

**API-level fix (partial).** No signature prevents a developer from putting a `List` in a
datum. What the stdlib *can* do:

1. Provide bounded collection types with the cap in the type
   (`BoundedList[A, 20]`), whose `FromData` rejects longer inputs.
2. Provide the off-chain alternatives so the unbounded datum is never the easy path: the
   linked-list / distributed-map / Merkle-trie structures already in
   `scalus-design-patterns` (`LinkedList.scala`) and `scalus.crypto.trie`.
3. A compiler-plugin lint: warn when a `@Compile`d datum type contains an unbounded
   collection.

---

#### RS-3 · Unbounded inputs / UTxO fragmentation DoS · **DOCUMENT** · rank 30

**Mechanism.** A protocol that lets outputs fragment ends up needing more inputs than a
transaction can hold. MLabs' faucet example: a validator that requires change to return to
the script but does not constrain the *shape* of the change lets an attacker create
thousands of tiny script UTxOs; eventually the next claim cannot be built.

**API-level fix.** `continuing(...)` requires **exactly one** continuing output by default
(the count is part of its contract, unlike `getContinuingOutputs` which hands back a list).
A protocol that wants N continuations says `continuingMany(own, n, …)` with an explicit `n`.
That single default kills the fragmentation vector for the common case.

---

#### RS-4 · Quadratic scans / worst-case budget blow-up · **DOCUMENT + pattern** · rank 38

**Mechanism.** Every spent script input runs the validator, and each run that iterates all
inputs is O(n); the transaction as a whole is O(n²). A protocol that works with 3 inputs
fails at 15. The in-house **PaymentSplitter** pair exists precisely to demonstrate this
(naive O(n²) vs stake-validator O(n), ~71% budget reduction).

**API-level fix.** Make the O(n) shape the *documented default* for anything batch-like:
`ForwardedValidator` (PU-2). Plus a budget-regression test helper so the cost curve is
pinned:

```scala
/** Asserts the validator's ExUnits stay within budget at N inputs, for N in a range.
  * Catches "works at 3, fails at 15" before deployment. */
def assertBudgetScales(v: Validator, inputs: Range, budget: ExUnits): Unit
```

---

#### RS-5 · UTxO contention / concurrency DoS · **DOCUMENT** · rank 39

**Mechanism.** A single "global state" UTxO must be consumed by every protocol operation,
so concurrent users race and all but one fail. In the adversarial version, an attacker
spends the contended UTxO with a trivial transaction on every block, halting the protocol.
Countermeasures are architectural: per-user UTxOs, sharded state, batching via a stake
validator, time-locked or fee-gated access.

**API-level fix.** None at the signature level. The stdlib contributes vocabulary and
worked examples (the linked-list and distributed-map patterns) and, importantly, an
**Emulator scenario** that submits competing transactions so contention shows up in tests.

---

#### RS-6 · Cheap spam / dust griefing · **DOCUMENT** · rank 41

**Mechanism.** If disrupting the protocol costs less than the value of the disruption, it
will be disrupted. MLabs' lending example: thousands of tiny undercollateralised positions
crowd out the liquidations that keep the protocol solvent.

**API-level fix.** Minimum-size assertions are one-liners; the design guidance is the
deliverable. Provide `requireMinimum(value, floor)` and document the economic argument.

---

#### RS-7 · Reference-script size fees / script-size DoS · **DOCUMENT** · rank 45

**Mechanism.** Reference scripts (CIP-33) let a transaction point at an on-chain script
instead of embedding it. Before Conway, reference-script size imposed **validation work on
every node but no fee on the submitter** – a pure externality.

**This was exploited on mainnet.** On 25 June 2024 (from block 10,487,530) an attacker ran
up to **194 reward-purpose scripts per transaction for roughly 0.9 ADA**, degrading node
performance network-wide. The response was Conway's **tiered reference-script fee**:
`minFeeRefScriptCostPerByte = 15`, with the rate multiplied by **1.2× for every additional
25,600-byte tier**, plus a per-transaction reference-script size cap (reported as 200 KiB;
treat that figure as lower-confidence).
<https://cointelegraph.com/news/cardano-developers-thwart-ddos-attack-upgrade-security>
<https://github.com/IntersectMBO/cardano-ledger/issues/3952>

**API-level fix.** The stdlib should surface script size as a first-class, tested quantity –
the `MerkelizedValidator` scaladoc already carries the 200 KiB warning – and the blueprint
output should report the compiled size so a size regression is visible in review.

---

### DE – Design-level family

#### DE-1 · Locked value – no exit path / unreachable state · **DETECT** · rank 23

**Mechanism.** A state machine with a state that has no outgoing transition, or a branch
whose precondition can never be satisfied, permanently locks whatever is in that UTxO. The
in-house **Betting** finding: "No timeout/reclaim path … funds lock forever if the oracle
goes silent or nobody joins; the README documented a Timeout that wasn't implemented."

**API-level fix (partial).** If the high-level API models the protocol as an explicit
transition table rather than as a `match` on a redeemer enum, then reachability is a
*property of a value* and can be checked:

```scala
/** A declarative transition table. The macro checks at compile time that every
  * state has at least one outgoing transition, and warns on unreachable states. */
def transitions: List[Transition[S, R]]
```
This is the strongest argument in the whole document for a declarative state-machine layer
on top of the raw `spend` handler. It also gives DE-1 and AU-2 a shared solution: the table
carries an `authority` per transition (AU-2) and a totality check (DE-1). Formal work
already names this property **liquidity** ("no permanent fund lockup") alongside validity
and fidelity.
<https://drops.dagstuhl.de/entities/document/10.4230/OASIcs.FMBC.2025.6>

---

#### DE-2 · Oracle staleness / manipulation · **DOCUMENT** · rank 34

**Mechanism.** Three separate failures: the oracle datum is not authenticated (that is
AU-1, and the in-house **PriceBet** bug); the data is authentic but stale; the data is
authentic and fresh but the underlying market was manipulated.

**API-level fix.** AU-1's `readAuthenticated` handles authenticity. Freshness needs the
time API (TI-1) and a policy:

```scala
def readFreshOracle[S: FromData](beacon: Beacon, maxAge: BigInt, timestampOf: S => PosixTime, tx: TxInfo): S
```
Manipulation resistance (medians, TWAPs, multi-oracle quorums) is protocol design, and the
stdlib's contribution is worked examples, not signatures.

---

#### DE-3 · Self-dealing / missing role separation · **DETECT** · rank 37

**Mechanism.** An auction where the seller may bid, a lending market where the liquidator
may be the borrower. The code is well-formed; a domain constraint is missing.

**API-level fix.** None at the signature level. The declarative transition table (DE-1) can
carry role annotations, which at least makes the omission visible in review. Practically
this is a security-review-skill item (V007) plus a property test that instantiates both
roles with the same key.

---

#### DE-4 · Hash grinding – attacker-influenced hashes used as "randomness" · **DOCUMENT** · rank 42

**Mechanism.** Validation is deterministic and the transaction author controls the
transaction's contents, so any outcome derived from a hash of transaction data can be
*ground*: the author retries until the hash is favourable. Anything from "which bucket does
this entry land in" to "who wins the raffle" is grindable.
<https://developers.cardano.org/docs/developers/curriculum/smart-contracts/security/vulnerabilities/evaluation-and-grinding/>

Note the interaction with IX-3: because the ledger sorts inputs by `(txId, idx)`, grinding a
`txId` also grinds *input position*.

**API-level fix.** The stdlib must not ship anything that looks like `randomFrom(tx)`.
It should ship the commit-reveal primitives instead (a `Commitment` type with
`commit(nonce, value)` / `reveal`), so the safe construction is the available one.

---

#### DE-5 · Replay: the same state reachable twice · **DOCUMENT** · rank 47

**Mechanism.** The eUTxO ledger prevents *transaction* replay structurally (a spent output
cannot be spent again), which is a real advantage over account-model chains. What it does
not prevent is *application-level* replay: an off-chain signature reused (AU-7), or a
protocol that lets an identical state be recreated so a "once only" action happens twice.
The one-shot NFT (MI-2) is the standard primitive for "this can happen at most once".

**API-level fix.** `oneShot` (MI-2) + `signedPayload` (AU-7) cover the two mechanisms.
The remaining risk is design-level.

---

## 5. Known real incidents on Cardano

Cardano's public record of *exploited* validator bugs is thin – which is itself a finding:
the near-misses were caught by white hats and disclosed, not exploited. Confidence is
marked per row. Items marked **out of scope** are included only so they are not mistaken
for validator bugs.

| Date | Protocol | Class | What happened | Outcome | Confidence |
|---|---|---|---|---|---|
| **Mar 2022** | **Minswap** | **MI-1** (non-exclusive mint check) | `isUnity v c = assetClassValueOf v c == 1` checked that one pool NFT was minted, not that **nothing else** was. An attacker could mint a counterfeit pool NFT alongside the real one, then mint unlimited LP tokens for **any** pool and drain all liquidity. Correct form: `Map.lookup curr (getValue v) == Just (Map.fromList [(tok,1)])`. | **Not exploited.** Found by the *competing* WingRiders team on 21 Mar 2022 while reading Minswap's newly open-sourced code, disclosed within hours. Minswap froze orders and used the bug itself, in a controlled migration, to move all positions to a patched contract. **~$195M TVL at risk.** A Tweag audit closed 31 Jan 2022 had missed it; Tweag committed to extending their Pirouette formal tool to catch the class. | **High** |
| **~2022** | **Atomic Swap**, **TradingTent** | **AU-4** (mangled / franken addresses) | Validators compared only the payment credential, so a counterparty could supply an address pairing the correct payment key with *their* staking key, spoofing UTxO ownership for the swap's purposes and taking temporary staking control. | Responsibly disclosed by researcher "Adamant"; TradingTent patched and paid a bounty. A follow-up piece covers the same attack against funds paid *out* by validator scripts. | **Medium** (single-source, the researcher's own account) |
| **Jun 2024** | **Cardano mainnet** | **RS-7** (reference-script / withdraw-zero fee externality) | From block 10,487,530, an attacker ran up to **194 reward-purpose scripts per transaction for ~0.9 ADA**, because reference-script size imposed node validation work but **no proportional fee**. Node performance degraded network-wide; SPO block-height battles intensified. | Mitigated (an Anastasia Labs engineer countered with a deregistration attack against the attacker's own flawed contract); Conway introduced the **tiered reference-script fee** (`minFeeRefScriptCostPerByte = 15`, ×1.2 per 25,600-byte tier). | **Medium** (journalism-sourced mechanism; no official IOG post-mortem found) |
| **Dec 2024** | **Lenfi V2** | undisclosed (lending/deposits) | Lead dev found a critical bug allowing deposits to be drained. Rather than white-hat-draining immediately (which would have exposed already-repaid loans), the team ran a 60-hour window urging borrowers to repay, then secured the remainder. | **Not exploited.** Self-discovered, self-remediated, no funds lost. | **Medium** (mechanism never disclosed) |
| **ongoing** | **Minswap** (bug bounty) | **RS-1 / MI-1 variant** | `validate_factory` checked only for *presence* of the Factory NFT in outputs, not exclusivity – a "dust token" attack against the Factory UTxO. | Medium severity; found by Micah (Butane), paid **7,500 ADA**. A second minor finding by a TxPipe dev paid 2,000 ADA. | **High** |
| **Oct–Nov 2022** | **Indigo Protocol** (pre-launch audit) | **PU-1** (`other-redeemer`) | MLabs found 17 issues; the critical one: the CDP validator did not check that the Stability Pool input was spent **with the expected redeemer**, so a full liquidation could burn more iAssets from the pool than the CDP's actual debt. | Found pre-mainnet; accepted as "accounted for" via the DAO's audit-gating process. | **High** |
| Jun 2026 | SecondFi wallet | **out of scope** – client-side Ed25519 signing bug | An unaudited third-party signer derived each signature's nonce from the *public* transaction hash only, omitting the secret key – every signed transaction leaked the private key. | **Exploited.** ~16M ADA (~$2.4M) drained from ~178 wallets; up to 129M ADA at risk. **Not a validator bug.** | High |
| Nov 2025 | Cardano mainnet | **out of scope** – node deserialisation bug | A malformed delegation transaction was accepted by newer nodes and rejected by older ones, splitting the chain for hours. | Patched within ~3 hours of detection. **Not a validator bug.** | High |

**Checked and found nothing:** SundaeSwap, MuesliSwap, WingRiders (own contracts), Genius
Yield, Optim Finance, Liqwid (a May 2023 Discord compromise is social engineering, not a
contract bug), Spectrum. Absence of a public record is not proof of absence.

**Reading of the record.** Every *validator* incident above is a top-10 entry in §2:
MI-1 twice, AU-4 once, PU-1 once, RS-7 once. Nothing exotic. The taxonomy's ranking is
consistent with what actually happened.

---

## 6. Prior art: what other stdlibs enforce vs. leave to convention

This table is the design brief in miniature. Everything in the right-hand column is an
opportunity.

| Library | Helper | Enforced by construction | Left to convention (the footgun) |
|---|---|---|---|
| **PlutusTx** | `getContinuingOutputs :: ScriptContext -> [TxOut]` | filters by own address | returns a **list**; every `head`/`[out] ->` consumer silently assumes singularity. Fails with the opaque trace `"Lf"` |
| **PlutusTx** | `valuePaidTo :: TxInfo -> PubKeyHash -> Value` | – | `mconcat`s **all** outputs to a key: the canonical DS-1 enabler. Also silently ignores script addresses with the same staking part |
| **PlutusTx** | `ownCurrencySymbol :: ScriptContext -> CurrencySymbol` | – | total-looking type, partial impl: crashes with `"Lh"` outside a minting purpose |
| **PlutusTx** | `findOwnInput :: ScriptContext -> Maybe TxInInfo` | – | returns `Nothing` (not an error) for any non-spending purpose |
| **Aiken stdlib** | `assets.match(left, right: Data, assert_lovelace: fn(Lovelace, Lovelace) -> Bool)` | **exact equality on every non-ADA asset**, relational only on lovelace | caller still chooses the relation |
| **Aiken stdlib** | `Value` opaque type | **never holds a zero-quantity entry**; empty asset dicts cleared (CHANGELOG v1.3.0, v1.4.0) | – (this is the model: a footgun closed by construction after being found twice) |
| **Aiken stdlib** | `find_script_outputs -> List<Output>` | filters by script hash | same singleton assumption as `getContinuingOutputs` |
| **Aiken stdlib** | `resolve_input(...) -> Output` | – | signature reads total, doc says "**Fails** when no matching output is found" |
| **Aiken language** | `validator { … else(_) { fail } }` | **"When no fallback is explicitly specified, Aiken defaults to a validator that is always rejecting."** | – |
| **Aiken language** | `spend(datum: Option<T>, …)` | **datum is forced to `Option`**; the author must `expect Some(datum)` | – |
| **Aiken language** | `unused::variable` lint | fires on an unused `own_ref` | a warning, not an error; silenced by `_own_ref` |
| **aiken-design-patterns** | `one_to_one(…, double_satisfaction_prevented: Bool, …)` | own-input identity (`own_ref == in_ref`) | the DS flag is only `expect`-checked to be `True` – a **reminder, not an enforcement**. Callers can pass `True` without having done anything |
| **aiken-design-patterns** | `one_to_many` | output indices must be **strictly ascending** (`fail @"Output indices must be in ascending order"`) | same non-enforcing DS flag |
| **aiken-design-patterns** | `one_to_one_no_redeemer` | walks **all** inputs, requires index list fully consumed – closes `missed-input` (IX-2) | pairing correctness still the caller's |
| **aiken-design-patterns** | `validate_mint` **vs** `validate_mint_minimal` | full variant binds redeemer + token map | the `_minimal` sibling checks only that *something* was minted – **the unsafe variant sits one character away from the safe one** |
| **aiken-design-patterns** | `normalize_time_range -> NormalizedTimeRange` | 5 exhaustive inclusive-only cases; re-derives `InvalidRange` even though phase-1 should have | – (the other model to copy) |
| **Helios** | `tx.value_sent_to_datum(addr, datum, is_inline) -> Value` | **datum-tagged** value lookup – DS-1 closed *if used* | untagged siblings `value_sent_to` / `value_paid_to` sit right beside it |
| **liqwid-plutarch-extra** | `ptryOwnInput` vs `pfindOwnInput` | a **naming convention** distinguishing fail-loud from `Maybe` | no DS primitive at all |
| **OpShin, plu-ts/pebble** | – | – | no safety helper found; DS prevention entirely manual |
| **Scalus (today)** | `TxInfo.findOwnInputOrFail` | fails loud | the `Option` sibling `findOwnInput` is equally prominent |
| **Scalus (today)** | `Value.hasOnly(cs, tn, amount)` | **exactly the whole-sub-map check MI-1 needs** | not the headline mint API; `quantityOf` is more discoverable |
| **Scalus (today)** | `UtxoIndexer.multiOneToOneNoRedeemer` | walks all inputs, both-direction coverage (IX-2 closed) | `Boolean` callbacks (EV-1); `oneToOne` carries no DS guard |
| **Scalus (today)** | `TxInfo.getValidityStartTime` | – | **returns `0` for an unbounded lower bound** (TI-1) – the in-house Vault bug |
| **Scalus (today)** | plugin default-`fail` for unimplemented purposes | **PU-1 / PU-3 / PU-4 closed by default** | undocumented as a security property |

**Three lessons for the new API.**

1. **A safe helper next to an unsafe sibling with a similar name is not a safe API.**
   `validate_mint` / `validate_mint_minimal`, `value_sent_to_datum` / `value_sent_to`,
   `findOwnInputOrFail` / `findOwnInput`. The unsafe sibling must be *harder* to reach – a
   different (low-level) import, a longer name that states the danger, or absent entirely.
2. **A flag that is only checked to be `true` is documentation, not enforcement.**
   Aiken's `double_satisfaction_prevented: Bool` is an honest admission that the library
   could not enforce the property. Scalus can do better with `OwnInputPolicy`, because a
   *sum type of strategies* forces a real choice where a `Bool` only forces a keystroke.
3. **Normalisation at the boundary is the cheapest correctness win available.** Aiken's
   zero-free `Value` and Anastasia Labs' `NormalizedTimeRange` both work by making the
   awkward representation unreachable. The same move applies to intervals, values, indices
   and amounts in Scalus.

---

## 7. Top 15 – the proposed API-level mitigations, consolidated

| # | Pitfall | Proposed API | Default behaviour that removes the footgun |
|---|---|---|---|
| 1 | DS-1 double satisfaction | `trait SpendingValidator { def ownInputPolicy: OwnInputPolicy }` – abstract, no default | Cannot compile without choosing `Exclusive` / `TaggedOutputs` / `Indexed` / `Aggregated` / `Unchecked(why)`; the framework injects the matching guard |
| 2 | VP-1 value not preserved | `continuing(own, value: ValuePolicy, datum, address, tx): TxOut` | `value` is a **required** argument; no `continuing(own, datum)` overload; no `ValuePolicy.Unchecked` |
| 3 | VP-2 ADA-only comparison | `ValuePolicy.Preserve` = full multi-asset equality; `requireAdaOnly(v)`; opaque `Lovelace` | Lovelace-only preservation is not in the enum; mixing `Lovelace` and `Value` is a type error |
| 4 | VP-3 redirect attack | `address: AddressPolicy` required; **no raw `tx.outputs.at(i)`** in the high-level surface | `AddressPolicy.SameAsInput` |
| 5 | AU-1 unauthenticated UTxO | `readAuthenticated[S](beacon: Beacon, source, tx): S` – **the only foreign-state reader** | No `readByAddress` exists; `Beacon` is only obtainable from `oneShot` |
| 6 | MI-1 other-token-name | `requireMintExactly(policyId, expected: SortedMap[TokenName, BigInt], tx)` – **the only mint assertion** | Whole-sub-map comparison; `requireNoMint` = `SortedMap.empty`. No `quantityOf`-based mint helper exists |
| 7 | MI-2 one-shot NFT | `oneShot(seed: TxOutRef, policyId, tx): Beacon` | The seed-spent check is *inside* the only `Beacon` constructor, so NFT uniqueness is a type-system consequence |
| 8 | TI-1 unbounded range as "now" | Delete `getValidityStartTime`; `requireAfter/requireBefore(deadline, tx)`; `requireBoundedRange(tx, maxWidth)` | No scalar "now"; no defaulting variant; `requireLowerBound` fails on an infinite bound |
| 9 | AU-2 missing authorization | `requireAuthorizedBy(cred: Credential, tx)` handling pubkey **and script** credentials | One vocabulary for signatures, spent-input authority, withdraw-zero and CIP-112 observation |
| 10 | IX-1 index-list bugs | `opaque type AscendingIndices` / `IndexPairs` with rejecting `FromData`; `zipExact` | Duplicates and non-ascending order are rejected at **decode** time; no truncating `zip` in the high-level API |
| 11 | DT-1 datum hijacking | `continuing(…, datum: D2)` takes the **complete** expected datum, compared by `toData` | Author writes `own.datum.copy(field = …)`; every unnamed field is pinned by construction. No predicate overload |
| 12 | PU-1 other-redeemer | Plugin default-`fail` per purpose (**already shipped**) + `requireRanWith(purpose, expected, tx)` | "It ran" and "it ran with *this* redeemer" are one call, not two |
| 13 | AU-3 own-input confusion | `Own[D]` built by the framework, carrying `ref`, full `address`, `scriptHash`, `value`, decoded `datum` | Nothing to resolve, nothing to reconstruct, no `.get` |
| 14 | AU-4 staking-credential hijack | `AddressPolicy.SameAsInput` compares the **whole** address; remove/rename `Address.fromScriptHash` | Credential-only comparison must be typed out as `SamePaymentCredential`; payout helpers take `Address`, never `Credential` |
| 15 | EV-1 evaluation-order trap | **Rule R1:** no public API takes a `Boolean` callback; every rule is a `Unit` block of `require`s | `&&` chains become independently-forced statements; failure messages survive to the trace |

Two cross-cutting items that did not fit the top 15 but carry comparable weight:

* **DT-2 / DT-3** – `Own[D].datum` is a decoded, shape-checked `D` (not `Option[Data]`),
  `continuing` writes an **inline** datum, and untrusted `Data` is `Untrusted[D]` until it
  passes `validate`. This closes the `datum.get` family and `arbitrary-datum` in one move.
* **DE-1 / AU-2 / DE-3** – a declarative `transitions: List[Transition[S, R]]` layer whose
  macro checks totality (every state has an exit) and carries an `authority` per
  transition. This is the only construction that converts three **DETECT** entries into
  compile-time or review-time signals.

---

## 8. Gaps, uncertainties and open questions

Recorded honestly so the design does not build on sand.

**Verified but worth re-checking before the API freezes**

* Inputs are re-sorted lexicographically by `(txId, idx)`; **CIP-128** would change this and
  was **not** in the Dijkstra Phase 1 scope as of Aug 2026. If it lands, IX-3's guidance
  changes. <https://cips.cardano.org/cip/CIP-0128>
* **CIP-112** (`Observe` purpose) is the intended successor to withdraw-zero; implementation
  status not confirmed. `ForwardedValidator` should be designed so the swap is invisible to
  user code. <https://cips.cardano.org/cip/CIP-0112>
* Protocol parameters move at hard forks: `maxTxExecutionUnits.mem` went 14,000,000 (PV10) →
  16,500,000 (PV11). Any constant baked into the stdlib must be a test-pinned value, not a
  literal.

**Unverified / could not confirm**

* Ordering of `referenceInputs`, `mint`, `withdrawals` and `redeemers` is **inferred** from
  the underlying `set`/`Map` types, not stated in a ledger document. Do not build a
  guarantee on it; bind by `outRef` or by key lookup instead.
* The per-transaction reference-script size cap of **200 KiB** comes from a search summary,
  not a primary document.
* The exact Conway stake-registration deposit for a withdraw-zero credential.
* No dedicated **academic** survey of Cardano/Plutus vulnerabilities appears to exist. The
  MLabs register and the Developer Portal curriculum are the de facto taxonomy and should be
  cited as industry sources, not peer-reviewed ones. The relevant formal work is
  *"Validity, Liquidity, and Fidelity: Formal Verification for Smart Contracts in Cardano"*
  (Ferariu, Wadler, Melkonian, FMBC 2025)
  <https://drops.dagstuhl.de/entities/document/10.4230/OASIcs.FMBC.2025.6>
  and *"Properties of UTxO Ledgers and Programs Implemented on Them"*
  (Vinogradova, Sorokin, LSFA'24) <https://arxiv.org/abs/2506.05832>.
  No Cardano-specific security work by Bruno Gavranović or Bartoletti was found.
* The claim that Plutus V3 removed a lovelace entry from `mint` is **not** supported. ADA
  can never be minted in any version (a ledger-wide invariant); what V3 actually added is
  the `MintValue` newtype, motivated by mint/burn sign safety.
  <https://github.com/IntersectMBO/plutus/issues/6445>
* V3 does **not** require a datum on script outputs – it makes the spending datum
  `Option[Datum]`, i.e. more permissive than V1/V2, which is why DT-3 matters more in V3,
  not less.

**Design questions this document raises but does not settle**

1. Is `OwnInputPolicy` an abstract member (forces a choice, costs boilerplate) or an
   annotation (`@OwnInputs(Exclusive)`, cheaper to read, easier to forget)?
2. Do `Trusted[D]` / `Untrusted[D]` opaque types pay for themselves, or is
   "no raw `TxOut.datum.to[T]` in the high-level API" sufficient?
3. How much ExUnits does each injected guard cost, and which are cheap enough to be
   unconditional? `Exclusive` is a single fold over inputs; `TaggedOutputs` adds a datum per
   payout with a min-ADA consequence (VP-6). The budget numbers belong in the API-design
   doc, measured, not estimated.
4. Should the declarative transition table be the *primary* API (opinionated, catches DE-1
   and AU-2) or an optional layer above `SpendingValidator`?
5. What does the migration path look like for the existing `scalus-design-patterns`
   modules, whose `Boolean` callbacks violate rule R1?

---

## 9. Cross-reference: this taxonomy vs. the existing security-review skill

The `smart-contract-security-review` skill's V001–V025 map onto this document as follows.
Where the mapping is not 1:1, the skill should be updated.

| Skill ID | This doc | Note |
|---|---|---|
| V001 Redirect Attack | **VP-3** | – |
| V002 Token/NFT Not Verified | **AU-1** | – |
| V003 Inexact Burn/Mint | **MI-1 / MI-3** | Skill treats `>=` vs `===`; the sharper form is whole-sub-map |
| V004 Integer Overflow (Critical) | **AR-1 / AR-2 / AR-3** | **Mis-framed.** On-chain `Integer` is unbounded – there is no overflow. Re-title as rounding direction + sign refinement + ledger serialisation bound |
| V005 Double Satisfaction | **DS-1 / DS-2** | Skill lacks the cross-instance variant |
| V006 Index Validation Missing | **IX-1** | – |
| V007 Self-Dealing | **DE-3** | – |
| V008 Double Spend via Index | **IX-1** | – |
| V009 Inexact Refund | **VP-4** | – |
| V010 Other Redeemer | **PU-1** | Skill says "always a false positive for Scalus" – true *because* of the plugin's default-`fail`; worth stating as the reason |
| V011 Other Token Name | **MI-1** | – |
| V012 Missing UTxO Authentication | **AU-1 / AU-5** | – |
| V013 Time Handling | **TI-1 / TI-2** | Skill misses the `getValidityStartTime`-returns-0 case, which is the one that actually bit |
| V014 Missing Signature | **AU-2** | Should add: signature ≠ authorization when the authority is a script |
| V015 Datum Mutation | **DT-1** | – |
| V016 Insufficient Staking Control | **AU-4** | – |
| V017 Arbitrary Datum | **DT-3** | – |
| V018 Unbounded Value | **RS-1** | – |
| V019 Unbounded Datum | **RS-2** | – |
| V020 Unbounded Inputs | **RS-3** | – |
| V021 UTxO Contention | **RS-5** | – |
| V022 Cheap Spam | **RS-6** | – |
| V023 Locked Value | **DE-1** | – |
| V024 Parameterization | **AU-6** | – |
| V025 Oracle Data | **DE-2** | – |
| – | **VP-1, VP-2, VP-5, VP-6** | **Missing from the skill.** VP-1/VP-2 are the two most frequent in-house bugs |
| – | **MI-2** (one-shot seed not bound) | **Missing.** Critical in-house finding (EditableNft) |
| – | **IX-2** (`missed-input`) | **Missing** |
| – | **EV-1** (evaluation order) | **Missing** |
| – | **AU-7** (signature domain separation) | **Missing** |
| – | **PU-3 / PU-4** (certificate, governance purposes) | **Missing** |
| – | **DE-4** (hash grinding) | **Missing** |
| – | **RS-7** (reference-script fees) | **Missing** |

That is **twelve** classes the current review skill does not look for, four of which are
Critical-severity and two of which have already occurred in the in-house corpus.
