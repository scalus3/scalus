# Scalus examples: on-chain validation pattern catalogue

Research input for the high-level "smart contract standard library" API design.

**Method.** Every `*.scala` file under `scalus-examples/jvm/src/main`,
`scalus-examples/shared/src/main` and `scalus-examples/lottery-complete/src/main`
was read in full (12 335 LOC total, of which ~4 400 LOC is on-chain validator code).
Frequency counts are *file* counts produced by `grep -rl` over those three trees and
cross-checked by hand. All line numbers are verified against the working tree at
commit `d9bac08c5`.

Paths are repo-relative from the worktree root
`/Users/nau/projects/lantr/scalus/.claude/worktrees/stdlib-api-research`.
`SE = scalus-examples/jvm/src/main/scala/scalus/examples`.

---

## 1. The corpus

### 1.1 On-chain validators analysed (35 files)

| # | File | Script purposes | Notes |
|---|------|-----------------|-------|
| 1 | `SE/htlc/HtlcValidator.scala` | spend | hand-rolled `validate` + `scriptInfo` match |
| 2 | `SE/vesting/VestingValidator.scala` | spend | `extends Validator` |
| 3 | `SE/escrow/EscrowValidator.scala` | spend | `extends Validator` |
| 4 | `SE/auction/Auction.scala` | spend + mint | `extends DataParameterizedValidator` |
| 5 | `SE/auction/UnfixedAuction.scala` | spend + mint | deliberately vulnerable twin of #4 |
| 6 | `SE/betting/BettingValidator.scala` | spend + mint | `extends Validator` |
| 7 | `SE/crowdfunding/Crowdfunding.scala` | spend + mint (×2 objects) | `CrowdfundingValidator` + `DonationMintingPolicy` |
| 8 | `SE/lottery/LotteryValidator.scala` | spend | `extends Validator` |
| 9 | `scalus-examples/lottery-complete/src/main/scala/lottery/onchain/LotteryValidator.scala` | spend | **near-clone of #8** |
| 10 | `SE/vault/VaultValidator.scala` | spend | `extends Validator` |
| 11 | `SE/paymentsplitter/PaymentSplitterValidator.scala` | spend | naive O(N²) |
| 12 | `SE/paymentsplitter/OptimizedPaymentSplitterValidator.scala` | spend + certify + reward | stake-validator pattern |
| 13 | `SE/editablenft/EditableNftValidator.scala` | spend + mint | CIP-68 |
| 14 | `SE/decentralizedidentity/DecentralizedIdentityValidator.scala` | spend + mint | |
| 15 | `SE/upgradeableproxy/UpgradeableProxyValidator.scala` | spend | |
| 16 | `SE/simpletransfer/SimpleTransferValidator.scala` | spend | |
| 17 | `SE/pricebet/PricebetValidator.scala` | spend | `DataParameterizedValidator` |
| 18 | `SE/pricebet/OracleValidator.scala` | spend + mint | |
| 19 | `SE/amm/AmmValidator.scala` | spend + mint | |
| 20 | `SE/factory/Factory.scala` | (library of `validateCreate/Destroy/Spend`) | |
| 21 | `SE/factory/FactoryExample.scala` | spend + mint | thin wrapper over #20 |
| 22 | `SE/linkedlist/LinkedListValidator.scala` | spend + mint | delegates to `scalus.patterns.LinkedList` |
| 23 | `SE/MembershipToken.scala` | spend + mint | `ParameterizedValidator[ByteString]` |
| 24 | `SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala` | spend | **re-implements prelude helpers by hand** |
| 25 | `scalus-examples/shared/src/main/scala/scalus/examples/MintingPolicy.scala` | mint | raw-`Data` hand-written deserialiser |
| 26 | `SE/PreimageValidator.scala` | spend (3 variants) | |
| 27 | `SE/PubKeyValidator.scala` | spend (3 variants) | raw builtin list walking |
| 28 | `SE/HelloCardano.scala` | spend | |
| 29 | `SE/bilinearAccumulator/AllowlistValidator.scala` | spend | |
| 30–33 | `SE/setbench/SetBench{Imt,Acc,Mpf16b,Mpf16o}Validator.scala` | spend | four near-identical bodies |
| 34–35 | `SE/setbench/SetBenchMpf16{b,o}LightValidator.scala` | spend | proof-only, no tx checks |

### 1.2 Files with **no** transaction-context validation (excluded from counts)

`SE/Groth16.scala` (pure pairing verifier), `SE/cape/factorial/*`, `SE/cape/fibonacci/*`
(pure arithmetic CAPE submissions), `SE/atomictransactions/AtomicTransactions.scala`
(off-chain only — the "contract" is the ledger's own atomicity),
and all `*Transactions.scala` / `*Offchain.scala` / `*Endpoints.scala` / `*Contract.scala`
files (off-chain builders and blueprint wiring).

---

## 2. Pattern catalogue

Each entry: **what**, **call sites (verbatim)**, **file count**, **boilerplate → proposed API**, **hazards**.

---

### P01 — "Signed by party X"

**Files: 20.** Five mutually incompatible spellings of the same check.

`SE/htlc/HtlcValidator.scala:59-60`
```scala
require(config.timeout <= validFrom, InvalidCommitterTimePoint)
require(tx.isSignedBy(config.committer), UnsignedCommitterTransaction)
```

`SE/lottery/LotteryValidator.scala:108-111`
```scala
require(
  tx.signatories.exists(_ === pkh),
  "Must be signed by player one"
)
```

`SE/HelloCardano.scala:22-23`
```scala
val signed = tx.signatories.contains(owner)
require(signed, "Must be signed")
```

`SE/PreimageValidator.scala:43`
```scala
tx.signatories.find(_.hash == pkh).orFail("Not signed")
```

`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:170-179` — a whole hand-written loop:
```scala
def requireSignedBy(
    signatories: List[PubKeyHash],
    party: PubKeyHash,
    message: String
): Unit = {
    def go(signatories: List[PubKeyHash]): Unit = signatories match {
        case List.Nil              => fail(message)
        case List.Cons(head, tail) => if head.toData == party.toData then () else go(tail)
    }
    go(signatories)
}
```

`SE/PubKeyValidator.scala:118-124` — the same loop again over raw `BuiltinList[Data]`.

**Boilerplate → API.** Today the developer picks one of `isSignedBy` / `signatories.exists` /
`signatories.contains` / `signatories.find(...).orFail` / a bespoke loop, and separately supplies
the message. One line should do it:

```scala
tx.requireSignedBy(config.committer)                    // message auto-derived
tx.requireSignedByAny(List(player1, player2))           // replaces `isSignedBy(a) || isSignedBy(b)`
tx.requireSignedByAll(signers)                          // multisig
```

**Hazards.**
- `SE/betting/BettingValidator.scala:208-211` builds "any of" by hand:
  `require(txInfo.isSignedBy(player1) || txInfo.isSignedBy(player2), ...)` — two full list scans.
- `SE/bilinearAccumulator/AllowlistValidator.scala:57` uses `tx.signatories.head`
  with **no non-empty guard**; `SE/MembershipToken.scala:73-74` does the same but *does* guard
  (`require(txInfo.signatories.length > 0, "No signatories")`). Taking "the first signatory"
  as an identity is itself unsound — the ordering is attacker-influenceable.
  `SE/factory/Factory.scala:129` documents exactly this: the old
  `isSignedBy(signatories.head)` check "was vacuous (head is always a signatory)".
- `requireSignedBy` in #24 compares via `toData` — a full serialisation per element.

---

### P02 — "Find my own input"

**Files: 22** use `findOwnInputOrFail`; 1 more re-implements it.

`SE/vesting/VestingValidator.scala:53`
```scala
val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
```

`SE/amm/AmmValidator.scala:213`
```scala
val ownInput = tx.findOwnInputOrFail(ownRef, "Own pool input not found")
```

`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:158-165` — hand-rolled duplicate:
```scala
def findOwnInputOrFail(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo = {
    def go(inputs: List[TxInInfo]): TxInInfo = inputs match
        case List.Cons(head, tail) =>
            if head.outRef.toData == txOutRef.toData then head
            else go(tail)
        case List.Nil => fail("Own input not found")
    go(inputs)
}
```

**Indexed variant** (redeemer carries the index, validator binds it):
`SE/auction/Auction.scala:119-120`
```scala
val input = txInfo.inputs.at(inputIdx)
require(input.outRef === txOutRef, "Input index does not match txOutRef")
```
`SE/paymentsplitter/OptimizedPaymentSplitterValidator.scala:91-93`
```scala
val ownInput = tx.inputs.at(spendRedeemer.ownInputIndex)
require(ownInput.outRef === ownRef, "Own input index mismatch")
```
`SE/linkedlist/LinkedListValidator.scala:184-185`
```scala
val elemInput = tx.inputs.at(elemInputIdx)
require(elemInput.outRef === ownRef, "Spend: input outref mismatch")
```

**Boilerplate → API.** The "own input" is *always* needed and is *always* followed by
`.resolved`, `.resolved.address`, `.resolved.value` or `.resolved.datum`. The Validator trait
should just hand it over:

```scala
// in the spend signature itself, or:
val self = tx.ownInput(ownRef)          // TxInInfo, fails with a standard message
val self = tx.ownInputAt(idx, ownRef)   // O(1) indexed variant, binds outRef in one call
self.address / self.value / self.datumAs[T]
```

**Hazards.** The unindexed form is O(n) per script invocation → O(n²) across a batch
(the whole reason `OptimizedPaymentSplitterValidator` exists). The indexed form is only safe
*because* of the `require(... === ownRef)` line; that line is easy to forget and there is no
type-level enforcement today.

---

### P03 — "Parse the datum or fail"

**Files: 27** — the single most common line in the corpus.

`SE/vesting/VestingValidator.scala:47-48`
```scala
val vestingDatum = datum.getOrFail(DatumNotFound)
val vestingConfig = vestingDatum.to[Config]
```
`SE/vault/VaultValidator.scala:72`
```scala
val datum = d.getOrFail(NoDatumExists).to[State]
```
`SE/setbench/SetBenchImtValidator.scala:24`
```scala
val state = datum.getOrFail("No datum").to[ImtDatum]
```
`SE/pricebet/PricebetValidator.scala:54`
```scala
val state = datum.getOrFail("Datum must be present").to[PricebetState]
```

**Boilerplate → API.**
```scala
val cfg = datum.as[Config]                 // Option[Data] => Config, standard message
```
Better still: a typed `spend[D, R]` entry point so the datum and redeemer arrive already decoded
(`Validator` currently hands out `Option[Data]` + `Data` and every example decodes them by hand;
`redeemer.to[Action]` appears in **24** files).

**Hazards.** `SE/vesting/VestingValidator.scala:47` keeps *both* the raw `Data` and the decoded
value because the raw form is needed later for the continuing-datum comparison at line 119
(`contractOutput.datum === OutputDatum.OutputDatum(vestingDatum)`). Every example that only keeps
the decoded value has to re-encode with `.toData` to compare — see
`SE/editablenft/EditableNftValidator.scala:165` (`newDatum.toData === d.get`).

---

### P04 — "Read an inline datum or fail"

**Files: 18** use `inlineOrFail`; 2 more hand-match `OutputDatum`.

`SE/upgradeableproxy/UpgradeableProxyValidator.scala:67-68`
```scala
val continuationDatum =
    continuationOutput.datum.inlineOrFail[ProxyDatum](ContinuationMustHaveInlineDatum)
```
`SE/amm/AmmValidator.scala:118-119`
```scala
inline def readPoolDatum(out: TxOut): AmmDatum =
    out.datum.inlineOrFail[AmmDatum]("Pool output must have inline datum")
```
`lottery-complete/.../LotteryValidator.scala:79-82` — hand-matched instead:
```scala
val newState = continuationOutput.datum match {
    case v2.OutputDatum.OutputDatum(datum) => datum.to[State]
    case _ => fail("continuation out must have an inline datum")
}
```
`SE/lottery/LotteryValidator.scala:100-102` — the *same contract*, other spelling:
```scala
val newState = continuationOutput.datum.inlineOrFail[State](
  "continuation out must have an inline datum"
)
```

**Boilerplate → API.** `out.datumAs[T]` (fails on hash/none), plus `out.datumAsOpt[T]`.

**Hazards.** `inlineOrFail` silently rejects `OutputDatum.OutputDatumHash` — correct here, but the
Escrow example instead reads the datum as raw `Data` and compares
(`SE/escrow/EscrowValidator.scala:103`), which decodes a different way for the same intent.

---

### P05 — "Exactly one continuing output at my own address"

**Files: 14.** This is the pattern with the **most divergent spellings** in the whole corpus — six.

1. `findOwnOutputsByCredential` + length check — `SE/vesting/VestingValidator.scala:66,101-102`
```scala
val contractOutputs = txInfo.findOwnOutputsByCredential(contractAddress.credential)
...
require(contractOutputs.length === BigInt(1), NotExactlyOneContractOutput)
val contractOutput = contractOutputs.head
```
2. Same, but `.size ==` — `SE/vault/VaultValidator.scala:197-202`
```scala
private def getVaultOutput(tx: TxInfo, ownRef: TxOutRef): TxOut = {
    val ownInput = tx.findOwnInputOrFail(ownRef, OwnInputNotFound)
    val scriptOutputs = tx.findOwnOutputsByCredential(ownInput.resolved.address.credential)
    require(scriptOutputs.size == BigInt(1), NotExactlyOneVaultOutput)
    scriptOutputs.head
}
```
3. `outputs.filter(_.address === ...)` + length — `SE/pricebet/PricebetValidator.scala:65-72`
```scala
val continuationOutputs =
    tx.outputs.filter(out => out.address === ownInput.resolved.address)
require(
  continuationOutputs.length === BigInt(1),
  "Must have exactly one continuation output"
)
val continuationOutput = continuationOutputs.head
```
4. `filter` + pattern match on the cons cell — `SE/amm/AmmValidator.scala:122-128`
```scala
inline def findPoolOutput(outputs: List[TxOut], addr: Address): TxOut = {
    val matching = outputs.filter(_.address === addr)
    matching match
        case List.Cons(out, List.Nil) => out
        case List.Nil                 => fail("No pool output found")
        case _                        => fail("Multiple pool outputs found")
}
```
5. `filter` + inline match, discarding the "multiple" distinction — `SE/auction/Auction.scala:422-426`
```scala
val auctionOutput = txInfo.outputs.filter { out =>
    out.address === Address.fromScriptHash(policyId)
}.match
    case List.Cons(out, List.Nil) => out
    case _ => fail("There must be exactly one output to the auction script")
```
6. `headOption.getOrFail` — **does not check uniqueness at all** —
`SE/upgradeableproxy/UpgradeableProxyValidator.scala:62-65`
```scala
val continuationOutput =
    tx.outputs
        .filter(out => out.address === ownInput.resolved.address)
        .headOption
        .getOrFail(MissingContinuation)
```

Also `SE/betting/BettingValidator.scala:97-108` uses `findOwnScriptOutputs(scriptHash)` and matches
`List.Cons(TxOut(...), List.Nil)` in one destructuring step, and
`SE/setbench/SetBench{Imt,Acc,Mpf16b,Mpf16o}Validator.scala:61-63 / 46-48 / 44-46 / 43-45`
repeat spelling (1) verbatim four times.

**Boilerplate → API.**
```scala
val cont = tx.uniqueContinuingOutput(self)       // fails on 0 and on >1
val conts = tx.continuingOutputs(self)           // when N>1 is legitimate
```

**Hazards.**
- Spelling 6 accepts several continuing outputs and validates only the first → the remaining
  script UTxOs' value can be swept. The file's own comment at lines 53-58 argues double
  satisfaction is closed by the *input* count check, which does not cover this.
- Spellings 1 and 3 compare `address` vs `credential` inconsistently — see **P06**.
- `.head` after `.length === 1` is safe, but the pattern is copy-pasted 14 times and one
  mis-copied guard silently becomes an unchecked `.head`.

---

### P06 — "Continuing output goes to the right address"

**Files: 12.** Three semantics, freely mixed.

Full address (payment **and** staking part) — `SE/vesting/VestingValidator.scala:105-108`
```scala
// Pin the continuing output to the exact own input address: matching the payment
// credential alone would let the staking credential (and thus delegation rewards)
// be redirected to the attacker.
require(contractOutput.address === ownInput.address, ContinuingAddressMismatch)
```
Same reasoning, different contract — `SE/betting/BettingValidator.scala:122-126`
```scala
// V016 fix: Verify full address including staking credential
require(
  outputAddress === address,
  "Output address must match input address (including staking credential)"
)
```
Payment credential only — `SE/vault/VaultValidator.scala:194-195`
```scala
private def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String): Unit =
    require(out.address.credential === ownInput.resolved.address.credential, message)
```
Reconstructed enterprise address (**drops the staking part**) —
`SE/auction/Auction.scala:202-205`
```scala
require(
  continuingOutput.address === Address.fromScriptHash(scriptHash),
  "Continuing output must go to auction script address"
)
```
Credential-vs-policy — `SE/editablenft/EditableNftValidator.scala:81-84`
```scala
require(
  refNftOutput.address.credential === Credential.ScriptCredential(policyId),
  ReferenceNftMustBePreserved
)
```

**Boilerplate → API.** Make the safe form the default and the loose form explicit:
```scala
tx.requireSameAddress(self, cont)             // full address, the default
tx.requireSamePaymentCredential(self, cont)   // opt-in, documented as staking-unsafe
```

**Hazards.** `Address.fromScriptHash(h)` builds an address with **no staking credential**.
In `Auction`/`UnfixedAuction`/`Crowdfunding`/`Betting` (4 files) this is used as the continuing-output
target, so a script UTxO that *did* carry a staking part can only continue to the enterprise form —
functionally a staking-rights reset, and inconsistent with the Vesting/Betting comments that
explicitly call the staking part security-relevant.

---

### P07 — "Continuing output preserves the value (minus the withdrawal)"

**Files: 12.** Full-`Value` comparison in 4 files; **lovelace-only** in 8.

Full value, exact — `SE/vesting/VestingValidator.scala:110-115`
```scala
// The continuing output must preserve the entire remaining value — ADA and any
// native tokens — minus only the withdrawn lovelace. A lovelace-only check would
// let native tokens be stripped out of the locked UTxO.
require(
  contractOutput.value === ownInput.value - Value.lovelace(requestedAmount),
  ContinuingValueMismatch
)
```
Full value, unchanged — `SE/upgradeableproxy/UpgradeableProxyValidator.scala:70-73`
```scala
require(
  continuationOutput.value === ownInput.resolved.value,
  ValueMustBePreserved
)
```
Full value, delta — `SE/simpletransfer/SimpleTransferValidator.scala:73-76`
```scala
require(
  contractOutput.value === balance + amount,
  "Contract has received incorrect amount"
)
```
Lovelace only — `SE/pricebet/PricebetValidator.scala:76-79`
```scala
require(
  continuationOutput.value.getLovelace === initialBetAmount * 2,
  "Must match bet amount"
)
```
Lovelace only, `>=` — `SE/auction/Auction.scala:231-234`
```scala
require(
  continuingOutput.value.getLovelace >= bidAmount,
  "Continuing output must contain at least the bid amount"
)
```
Lovelace only via a summing helper — `SE/escrow/EscrowValidator.scala:95-100`
```scala
require(
  Utils.getAdaFromOutputs(
    contractOutputs
  ) === escrowDatum.escrowAmount + escrowDatum.initializationAmount,
  "Contract output must contain exactly escrow amount plus initialization amount"
)
```

**Boilerplate → API.**
```scala
tx.requireValuePreserved(self, cont)                     // value unchanged
tx.requireValueDelta(self, cont, -Value.lovelace(amt))   // exact delta, tokens included
tx.requireNoTokensAdded(self, cont)                      // token-set unchanged, ADA free
```

**Hazards.**
- The 8 lovelace-only sites let an attacker strip native tokens out of the continuing UTxO.
  `Escrow` (#3) is the clearest: `contractBalance` is `Utils.getAdaFromInputs(contractInputs)`
  (`EscrowValidator.scala:57`), and the seller/buyer payout checks at lines 139-143 and 177-179
  compare lovelace only — any native asset locked at the escrow address is free to take.
- `Pricebet:76` compares `=== initialBetAmount * 2` where `initialBetAmount` is the *own input's*
  lovelace; native tokens in the bet UTxO are unconstrained in the continuation.
- `Auction:232` uses `>=` for the continuing output but `===` for the refund at line 250 —
  asymmetric, and the `>=` is what makes the "seller output tag" workaround at lines 327-339
  necessary.

---

### P08 — "Continuing datum is the expected new state"

**Files: 12.** Three strategies.

(a) Whole-datum equality against a *constructed* expectation —
`SE/auction/Auction.scala:212-222`
```scala
val expectedNewDatum = Datum(
  seller = seller,
  highestBidder = Option.Some(bidder),
  highestBid = bidAmount,
  auctionEndTime = auctionEndTime,
  itemId = itemId
)
require(
  newDatum === expectedNewDatum,
  "New datum must reflect the new bid"
)
```
`SE/crowdfunding/Crowdfunding.scala:384-392`
```scala
val expectedDatum = CampaignDatum(
  totalSum = currentDatum.totalSum + amount,
  goal = currentDatum.goal,
  recipient = currentDatum.recipient,
  deadline = currentDatum.deadline,
  withdrawn = currentDatum.withdrawn,
  donationPolicyId = currentDatum.donationPolicyId
)
require(newDatum === expectedDatum, "Updated datum must reflect donation")
```

(b) Raw-`Data` identity — `SE/vesting/VestingValidator.scala:115-118`
```scala
require(
  contractOutput.datum === OutputDatum.OutputDatum(vestingDatum),
  InvalidDatum
)
```
`SE/editablenft/EditableNftValidator.scala:163-168`
```scala
if datum.isSealed then
    // check the entire datum
    require(newDatum.toData === d.get, SealedNftImmutable)
else
    // just check the token id, rest is ok to change
    require(newDatum.tokenId === datum.tokenId, TokenIdImmutable)
```

(c) Field-by-field "must not change" — `SE/lottery/LotteryValidator.scala:116-127`
```scala
require(
  newState.playerOneSecret === state.playerOneSecret,
  "Player one secret must not change"
)
require(
  newState.playerTwoSecret === state.playerTwoSecret,
  "Player two secret must not change"
)
require(
  newState.revealDeadline === state.revealDeadline,
  "Reveal deadline must not change"
)
```
…repeated **four times** in that one file (lines 116-127, 159-170) and again verbatim in
`lottery-complete/.../LotteryValidator.scala:97-108, 137-148`.
`SE/vault/VaultValidator.scala:94-102` and `SE/pricebet/PricebetValidator.scala:92-98`
do the same by hand.

**Boilerplate → API.**
```scala
tx.requireDatum(cont, expectedDatum)                       // (a)/(b) unified
tx.requireDatumUnchanged(self, cont)                       // pure state-preserving spends
tx.requireDatumChangedOnly(self, cont)(_.totalSum, _.withdrawn)  // (c), macro-checked field set
```
`requireDatumChangedOnly` is the highest-value one: strategy (c) is 5 files × ~4 `require`s each,
and it is the strategy that silently rots when a field is added to the datum.

**Hazards.**
- Strategy (c) fails **open** when a datum gains a field: `Crowdfunding` documents this as
  "V015 protection" (`Crowdfunding.scala:487`) and switched to (a) for exactly that reason.
- `Vesting` compares `OutputDatum.OutputDatum(vestingDatum)` against the output's `datum` —
  correct only because it kept the *raw* `Data` (P03); the decode/re-encode round trip in
  `EditableNft:165` is not guaranteed to be byte-identical for hand-written `ToData` instances.
- `Pricebet:94-98` cannot use `===` for `Rational` (no `Eq`) and falls back to
  `RationalEq.equals(...)` — a cross-multiplication — while every other field uses `===`.

---

### P09 — "Deadline reached / not yet reached"

**Files: 14.** Six different time primitives.

`isEntirelyBefore` (6 files) — `SE/auction/Auction.scala:175-178`
```scala
require(
  txInfo.validRange.isEntirelyBefore(auctionEndTime),
  "Bid must be placed before auction ends"
)
```
`isEntirelyAfter` (10 files) — `SE/vault/VaultValidator.scala:148`
```scala
require(tx.validRange.isEntirelyAfter(datum.finalizationDeadline), DeadlineNotPassed)
```
Negated `isEntirelyAfter` (a *third* meaning: "not provably after") —
`SE/pricebet/PricebetValidator.scala:107`
```scala
require(!tx.validRange.isEntirelyAfter(state.deadline), "Deadline passed")
```
Raw bound with a default — `SE/htlc/HtlcValidator.scala:57-59`
```scala
val validFrom = tx.validRange.from.finite(0)
// validFrom is inclusive, hence 10 <= 10 is correct
require(config.timeout <= validFrom, InvalidCommitterTimePoint)
```
Raw bound, failing — `SE/htlc/HtlcValidator.scala:62-64`
```scala
val validTo = tx.validRange.to.finiteOrFail(ValidRangeMustBeBound)
// validTo is exclusive, hence 10 <= 10 is correct
require(validTo <= config.timeout, InvalidReceiverTimePoint)
```
`getValidityStartTime` (4 files) — `SE/vesting/VestingValidator.scala:68`
```scala
val txEarliestTime = txInfo.getValidityStartTime
```
Upper bound, deliberately — `SE/vault/VaultValidator.scala:132-137`
```scala
// Derive the request time from the validity interval's *upper* bound, not the lower bound.
// The lower bound (getValidityStartTime) can be backdated arbitrarily, which would let an
// attacker set finalizationDeadline in the past and finalize immediately, defeating the
// wait. The ledger guarantees the upper bound is >= now, so deadline >= now + waitTime.
val requestTime = tx.validRange.to.finiteOrFail(NoFinalizationUpperBound)
```
Hand-matched bound — `SE/decentralizedidentity/DecentralizedIdentityValidator.scala:196-199`
```scala
val txEndTime = tx.validRange.to.boundType match
    case IntervalBoundType.Finite(t) => t
    case _ => fail("Transaction must have a finite upper validity bound")
require(txEndTime <= delegDatum.validUntil, "Delegation expired")
```

**Boilerplate → API.**
```scala
tx.requireAfter(deadline)                 // == isEntirelyAfter, fails closed on ±inf
tx.requireBefore(deadline)
tx.requireWithin(from, to)                // DID's "delegation window" check, one line
tx.latestTime                             // upper bound or fail — the *safe* "now"
tx.earliestTime                           // lower bound or fail — currently defaults to 0
```

**Hazards.**
- **`getValidityStartTime` returns `0` when the lower bound is infinite**
  (`scalus-core/.../v3/Contexts.scala:1102-1104`). In
  `SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:82-84` that value is *written into the
  datum* as `depositTime`; with an unbounded lower bound the recorded deposit time is 0, so the
  30-minute refund window at line 141 (`val deadline = escrowDatum.depositTime + deadlineSeconds`)
  is already in the past and the buyer can refund immediately. Vault's comment (above) documents
  the general shape of this bug.
- `Vault` and `DID` re-derive "the upper bound or fail" independently; `Htlc` uses `finiteOrFail`
  for the same thing. Three spellings, one concept.
- `!isEntirelyAfter` (Pricebet) is *not* the negation a reader expects: for an unbounded interval
  it is `true`, i.e. the deadline check passes.
- Inclusive/exclusive semantics are carried in *comments* (`HtlcValidator.scala:58,63`),
  not in the API.

---

### P10 — "Mint exactly one token of name N and nothing else"

**Files: 9** use `Value.hasOnly`; 3 more roll it by hand.

`SE/auction/Auction.scala:403-407`
```scala
// 3. Validate ALL tokens minted under this policy (prevents Other Token Name Attack)
require(
  txInfo.mint.hasOnly(policyId, itemId, 1),
  "Must mint exactly one auction NFT with the specified itemId and nothing else"
)
```
`SE/factory/Factory.scala:101-102`
```scala
// Check exactly 1 token minted under this policy with the correct name
require(tx.mint.hasOnly(policyId, expectedTokenName, 1), MustMintExactlyOneToken)
```
`SE/amm/AmmValidator.scala:191-196` — `hasOnly` with a *computed, possibly negative* delta:
```scala
// The tx must mint/burn exactly `lpDelta` of the LP token and NOTHING else under
// this policy: `hasOnly` pins the token name and rejects any other name in one check
// (works for negative `lpDelta`, i.e. burns, too), so the pool NFT can't be minted
// or burned on a liquidity change either.
val lpDelta = continuationDatum.lpSupply - poolDatum.lpSupply
require(tx.mint.hasOnly(policyId, lpTokenName, lpDelta), "Mint: LP delta mismatch")
```
Hand-rolled pair equality — `SE/editablenft/EditableNftValidator.scala:98-103`
```scala
val expectedMint =
    Value(policyId, refTokenName, 1) + Value(policyId, userTokenName, 1)
require(
  tx.mint.tokens(policyId) === expectedMint.tokens(policyId),
  MustMintExactlyNftPair
)
```
Hand-rolled singleton match — `SE/betting/BettingValidator.scala:268-271`
```scala
val quantity = tx.mint.tokens(policyId).toList match
    case List.Cons((_, qty), List.Nil) => qty
    case _ => fail("Must mint or burn exactly one token type under this policy")
```

**Boilerplate → API.** `hasOnly` already exists and is the model to follow. What is missing is the
multi-asset generalisation that `EditableNft` needs:
```scala
tx.requireMintExactly(policyId, Map(refName -> 1, userName -> 1))
tx.requireMintOneShot(policyId, tokenName, seedUtxo)   // fuses P10 + P13
```

**Hazards.** Nine files independently rediscovered the "Other Token Name Attack".
`SE/MembershipToken.scala:106-112` is the odd one out and does it by flattening:
```scala
val allMinted = txInfo.mint.flatten.filter { case (pid, _, _) =>
    pid === policyId
}
require(allMinted.length === BigInt(1), "Expected exactly one burn entry")
allMinted.foreach { case (_, _, qty) =>
    require(qty === BigInt(-1), "Must burn exactly 1 token")
}
```
— which does **not** pin the token name, so any name under the policy can be burned.

---

### P11 — "Burn exactly this token"

**Files: 9.**

`SE/editablenft/EditableNftValidator.scala:171-174`
```scala
val isRefNftBurned = tx.mint.quantityOf(policyId, refTokenName) === BigInt(-1)
require(isRefNftBurned, MustBurnRefNft)
val isUserNftBurned = tx.mint.quantityOf(policyId, userTokenName) === BigInt(-1)
require(isUserNftBurned, MustBurnUserNft)
```
`SE/betting/BettingValidator.scala:193-199`
```scala
// Burn the bet NFT so the bet is one-shot and cannot be re-locked into a forged bet.
require(
  txInfo.mint.quantityOf(scriptHash, betTokenName(value, scriptHash)) === BigInt(
    -1
  ),
  "The bet token must be burned when announcing the winner"
)
```
`SE/factory/Factory.scala:180-182`
```scala
// The NFT must be burned (qty = -1)
val burnQty = tx.mint.quantityOf(factoryPolicyId, tokenName)
require(burnQty === BigInt(-1), ProductNFTMustBeBurned)
```
`SE/crowdfunding/Crowdfunding.scala:662-666` — burn *count* must match input count:
```scala
// Verify exact number of tokens are burned
require(
  txInfo.mint.quantityOf(donationPolicyId, tokenName) === -tokenCount,
  "All donation tokens must be burned"
)
```

**Boilerplate → API.** `tx.requireBurn(policyId, tokenName)` / `tx.requireBurn(policyId, tokenName, n)`.

**Hazards.** `quantityOf(...) === -1` does **not** exclude other names under the same policy;
that is what `hasOnly` is for. `EditableNft` gets it right in the mint endpoint (line 117) and
uses the weaker form in the spend endpoint (lines 171,173) — a deliberate split, but the
asymmetry is invisible at the call site.

---

### P12 — "Only burning is allowed under this policy"

**Files: 5.** Character-for-character identical in three of them.

`SE/auction/Auction.scala:452-457`
```scala
// For burning, verify all tokens of this policy are burned (negative quantity)
val mintedTokens = txInfo.mint.tokens(policyId)
require(
  mintedTokens.forall { case (_, amount) => amount < 0 },
  "Only burning is allowed (all amounts must be negative)"
)
```
`SE/crowdfunding/Crowdfunding.scala:758-762`
```scala
val mintedTokens = txInfo.mint.tokens(policyId)
require(
  mintedTokens.forall { case (_, amount) => amount < 0 },
  "Only burning is allowed"
)
```
`SE/decentralizedidentity/DecentralizedIdentityValidator.scala:231-235`
```scala
// Ensure all quantities under this policy are negative (only burns allowed)
require(
  tx.mint.tokens(policyId).forall((_, qty) => qty < 0),
  "Burn action must only burn tokens"
)
```

**Boilerplate → API.** `tx.requireOnlyBurns(policyId)`.

**Hazards.** `forall` over an **empty** token map is `true`, so the check is vacuous for a
policy with no entries. In practice a V3 minting policy only executes when its `policyId` is
present in the mint field and Conway rejects zero-quantity mint entries, so the map should be
non-empty whenever this branch runs — the hole is not reachable on-chain today. It is still a
latent foot-gun (the same `forall` shape is used on *input* values, where emptiness is reachable),
so a stdlib `requireOnlyBurns` should demand at least one negative entry rather than rely on that
ledger invariant.

---

### P13 — "One-shot: the seed UTxO is consumed"

**Files: 6.**

`SE/factory/Factory.scala:95-96`
```scala
// Seed UTxO must be consumed (one-shot guarantee)
require(tx.inputs.exists(_.outRef === seedUtxo), SeedUtxoMustBeConsumed)
```
`SE/amm/AmmValidator.scala:157-162`
```scala
// Consume the one-shot seed so this policyId can only ever be initialized once.
require(
  tx.inputs.exists(_.outRef === param.to[AmmParams].seed),
  "Init: must spend the seed UTxO"
)
```
`SE/pricebet/OracleValidator.scala:53-55`
```scala
// Verify the seed UTXO is being spent
val seedUtxoIsSpent = tx.inputs.exists(_.outRef === config.seedUtxo)
require(seedUtxoIsSpent, "Must spend seed utxo to mint the beacon")
```
Indexed variant — `SE/editablenft/EditableNftValidator.scala:63-67`
```scala
// Bind the seed: the input at seedIndex must be the exact parameterized seed UTxO,
// not merely some input that exists. Otherwise the one-shot guarantee is defeated
// and the same policy can mint unlimited NFTs (uniqueness broken). A wrong index
// simply fails the check (fails closed), so it cannot be bypassed.
require(tx.inputs.at(seedIndex).outRef === seed, MustSpendSeed)
```
`SE/decentralizedidentity/DecentralizedIdentityValidator.scala:89-91`
```scala
val seedRef = param.to[TxOutRef]
val spentInput = tx.inputs.at(seedIndex)
require(spentInput.outRef === seedRef, "Must spend the parameterized seed UTxO")
```

**Boilerplate → API.** `tx.requireSpends(seedUtxo)` and `tx.requireSpendsAt(idx, seedUtxo)`.

**Hazards.** `exists` is O(n); the indexed form is O(1) but the two are not interchangeable in
review — `EditableNft`'s comment exists precisely because a reviewer had to reason about the
difference. A single named helper with an optional index argument removes the ambiguity.

---

### P14 — "Derive a unique token name from a consumed UTxO"

**Files: 2** on-chain (+2 off-chain mirrors).

`SE/factory/Factory.scala:62-63`
```scala
def computeTokenName(seedUtxo: TxOutRef): TokenName =
    Builtins.blake2b_256(Builtins.serialiseData(seedUtxo.toData))
```
`SE/crowdfunding/Crowdfunding.scala:707-710`
```scala
// Hash the serialized TxOutRef to get a 32-byte campaign ID (AssetName limit)
val campaignId = scalus.uplc.builtin.Builtins.blake2b_256(
  scalus.uplc.builtin.Builtins.serialiseData(consumedUtxo.toData)
)
```
plus the same expression in `SE/crowdfunding/CrowdfundingEndpoints.scala` (off-chain, must agree).

**Boilerplate → API.** `TokenName.fromUtxo(seedUtxo)` in the *shared* stdlib, so on-chain and
off-chain provably agree (the AMM example already argues for this style at
`SE/amm/AmmValidator.scala:55-61`).

**Hazards.** `Crowdfunding.scala:703-705` picks the seed as `inputs.head`:
```scala
val consumedUtxo = txInfo.inputs.match
    case List.Cons(first, _) => first.outRef
    case List.Nil            => fail("Must consume at least one UTxO")
```
The *first* input in a Plutus `TxInfo` is lexicographically ordered, so the minter cannot freely
choose it — but nothing binds it to the campaign either, and a second campaign in the same
transaction would derive the same id.

---

### P15 — "This UTxO holds the NFT that authenticates it"

**Files: 11.**

`SE/pricebet/PricebetValidator.scala:116-125`
```scala
// Authenticate the oracle UTxO by its beacon NFT — being at the oracle script
// address is not enough, since anyone can pay a forged datum to that address. The
// beacon is a one-shot mint under the oracle's own policy (= oracleScriptHash), so
// only the genuine oracle UTxO carries it. ...
require(
  oracleInput.resolved.value
      .quantityOf(config.oracleScriptHash, OracleBeaconName) === BigInt(1),
  OracleInputMustHaveBeacon
)
```
`SE/amm/AmmValidator.scala:295-299`
```scala
// The pool NFT must stay with the pool - it can only be burned via `Close`.
require(
  poolOutput.value.quantityOf(poolPolicyId, poolNftName) === BigInt(1),
  "Pool output must retain the pool NFT"
)
```
`SE/crowdfunding/Crowdfunding.scala:606-612`
```scala
def verifyCampaignNftPresent(value: Value, scriptHash: ValidatorHash): Unit =
    val nftTokens = value.tokens(scriptHash)
    // Must have exactly one token type with quantity 1
    val hasExactlyOneNft =
        nftTokens.size === BigInt(1) &&
            nftTokens.forall { case (_, qty) => qty === BigInt(1) }
    require(hasExactlyOneNft, "Campaign input must contain exactly one campaign NFT")
```
`SE/decentralizedidentity/DecentralizedIdentityValidator.scala:174-184`
```scala
// The delegation must actually hold its delegation token. Being at the script
// address with a datum-shaped value is not enough — anyone can pay a forged
// DelegationDatum there. ...
require(
  delegationRefInput.resolved.value.quantityOf(policyId, delegTn) === BigInt(1),
  "Delegation reference input must hold the delegation token"
)
```
`SE/betting/BettingValidator.scala:113-121`
```scala
require(
  value.policyIds.contains(scriptHash),
  "Input must contain the bet token"
)
// V002 fix: Verify bet token is preserved in output
require(
  outputValue.policyIds.contains(scriptHash),
  "Output must contain the bet token"
)
```

**Boilerplate → API.**
```scala
tx.requireAuthNft(input, policyId, tokenName)   // "this UTxO is the real one"
value.requireExactlyOneToken(policyId)          // Crowdfunding's shape
```

**Hazards.**
- `Betting:114` checks only that *some* asset under the policy is present — quantity and name
  unchecked. The file then derives the token name from the value at lines 245-248, so a UTxO
  holding two tokens of that policy fails there instead; the two checks must be read together.
- The "authenticate by NFT" idea is documented in prose in 4 files and absent in others
  (`Escrow`, `SimpleTransfer`, `Vault` have no beacon at all and rely purely on the address).

---

### P16 — "Output pays party X"

**Files: 8.**

`SE/auction/Auction.scala:244-252`
```scala
val refundOutput = txInfo.outputs.at(refundOutputIdx)
require(
  refundOutput.address === Address.fromPubKeyHash(previousBidder),
  "Refund output must go to previous bidder"
)
require(
  refundOutput.value.getLovelace === currentHighestBid,
  "Previous bidder must receive exactly their bid amount"
)
```
`SE/crowdfunding/Crowdfunding.scala:463-471`
```scala
val recipientOutput = txInfo.outputs.at(recipientOutputIdx)
require(
  recipientOutput.address === Address.fromPubKeyHash(currentDatum.recipient),
  "Funds must go to recipient"
)
require(
  recipientOutput.value.getLovelace >= totalWithdrawn,
  "Recipient must receive withdrawn amount"
)
```
Credential-match instead of address-match — `SE/lottery/LotteryValidator.scala:208-217`
```scala
val supposedWinnerOutput = tx.outputs.at(winnerOutputIdx)
supposedWinnerOutput.address.credential match {
    case v1.Credential.PubKeyCredential(hash) =>
        require(hash === playerOnePkh, "Wrong winner")
    case v1.Credential.ScriptCredential(_) => fail("Winner must be pubkey")
}
require(
  supposedWinnerOutput.value.getLovelace >= amount,
  "Insufficient payout"
)
```
Sum across all outputs — `SE/betting/BettingValidator.scala:250-255`
```scala
/** Sum the lovelace paid to a public key's (enterprise) address across all outputs. */
private inline def totalPaidTo(txInfo: TxInfo, pkh: PubKeyHash): BigInt =
    txInfo.outputs.foldLeft(BigInt(0)) { (acc, out) =>
        if out.address === Address.fromPubKeyHash(pkh) then acc + out.value.getLovelace
        else acc
    }
```

**Boilerplate → API.**
```scala
tx.requirePaid(pkh, Value.lovelace(amount))        // exact, all assets
tx.requirePaidAtLeast(pkh, Value.lovelace(amount))
tx.totalPaidTo(pkh): Value                          // sum, all assets
```

**Hazards.**
- `Address.fromPubKeyHash(pkh)` matches only the **enterprise** address. A payee whose wallet
  address has a staking part will not match. `Betting`'s helper even documents this
  ("(enterprise) address"), so a refund to a normal wallet address silently fails to count.
  `Lottery` avoids it by matching the credential — two files, opposite choices, same intent.
- Six of the eight sites compare `getLovelace` only, so a payout that is nominally correct in ADA
  can also drain the party's tokens (nothing constrains what *else* is in that output — that is
  fine — but nothing requires tokens the contract owed them either).
- `>=` vs `===` is chosen ad hoc: `Auction:250` uses `===` for refunds and `>=` for the seller
  (line 324); `Crowdfunding:469` uses `>=` for the recipient but `===` for donor reclaims
  (line 567, "Exact match required to prevent min UTxO theft (V009 protection)").

---

### P17 — "Sum lovelace over a set of inputs/outputs"

**Files: 6.**

Prelude helper — `SE/escrow/EscrowValidator.scala:56-57`
```scala
val contractInputs = txInfo.findOwnInputsByCredential(contractAddress.credential)
val contractBalance = Utils.getAdaFromInputs(contractInputs)
```
`SE/vesting/VestingValidator.scala:85-89`
```scala
val beneficiaryInputs = txInfo.findOwnInputsByCredential(beneficiaryCred)
val beneficiaryOutputs = txInfo.findOwnOutputsByCredential(beneficiaryCred)

val adaInInputs = Utils.getAdaFromInputs(beneficiaryInputs)
val adaInOutputs = Utils.getAdaFromOutputs(beneficiaryOutputs)
```
Hand-rolled fold — `SE/vault/VaultValidator.scala:158-160`
```scala
val totalToOwner =
    ownerOutputs.foldLeft(BigInt(0))((acc, out) => acc + out.value.getLovelace)
require(totalToOwner >= datum.amount, VaultAmountChanged)
```
Hand-rolled fold with `toData` comparison —
`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:115-119`
```scala
val cred = Credential.PubKeyCredential(sellerKeyHash).toData
val sellerAda = outputs.foldLeft(BigInt(0)): (sum, out) =>
    if out.address.credential.toData == cred
    then sum + out.value.lovelaceAmount
    else sum
require(sellerAda == escrowPrice, "Seller must receive exactly escrow price")
```
Fold over token quantities — `SE/auction/Auction.scala:276-280`
```scala
val totalAuctionNftsSpent = txInfo.inputs.foldLeft(BigInt(0)) { (count, input) =>
    if input.resolved.address === scriptAddress then
        count + input.resolved.value.tokens(scriptHash).values.foldLeft(BigInt(0))(_ + _)
    else count
}
```

**Boilerplate → API.**
```scala
tx.inputValueAt(credential): Value        // full Value, not just lovelace
tx.outputValueAt(credential): Value
tx.mintedUnder(policyId): SortedMap[TokenName, BigInt]
```
Note the existing `Utils.getAdaFromInputs/getAdaFromOutputs`
(`scalus-core/.../v3/Contexts.scala:1146-1183`) are **lovelace-only by construction** — they are
the direct cause of the P07 token-stripping hazards in Escrow and Vesting.

**Hazards.** Vesting's beneficiary accounting at `VestingValidator.scala:88-97` is
`adaInOutputs === requestedAmount + adaInInputs - txInfo.fee` — an ADA-only ledger identity that
breaks if the beneficiary's own inputs carry tokens, and that hard-codes the assumption that the
beneficiary pays the fee.

---

### P18 — "Exactly one own input" (double-satisfaction guard)

**Files: 5.**

`SE/vesting/VestingValidator.scala:56-61`
```scala
// Reject spending more than one vesting UTxO at once: otherwise a single continuing
// output could satisfy several script inputs (double satisfaction) and the remaining
// locked funds of the extra inputs would be siphoned off.
require(
  txInfo.findOwnInputsByCredential(contractAddress.credential).length === BigInt(1),
  MultipleVestingInputs
)
```
`SE/upgradeableproxy/UpgradeableProxyValidator.scala:53-58` — same comment, same code:
```scala
// Reject spending more than one proxy UTxO at once: otherwise a single continuation
// output could satisfy several script inputs (double satisfaction) and the value of the
// extra inputs would be swept off to the attacker.
require(
  tx.findOwnInputsByCredential(ownInput.resolved.address.credential).length === BigInt(1),
  MultipleProxyInputs
)
```
`SE/betting/BettingValidator.scala:212-218`
```scala
// Exactly one bet input — the per-player refund check below sums outputs by address,
// so batching two bets in one tx could let one refund satisfy both. One input per
// reclaim keeps the accounting sound.
require(
  txInfo.findOwnInputsByCredential(address.credential).length === BigInt(1),
  "Reclaim must spend exactly one bet input"
)
```
`SE/simpletransfer/SimpleTransferValidator.scala:56-60`
```scala
// eliminate double satisfaction by ensuring exactly one contract own input and at most one own output
require(contractInputs.size === BigInt(1), "Contract should have exactly one own input")
require(
  contractOutputs.size <= 1,
  "Contract should have at most one own output"
)
```
`SE/auction/Auction.scala:273-284` — the token-counting variant (quoted under P17).

**Boilerplate → API.** `tx.requireSingleOwnInput(self)` — and, better, make it the **default**
behaviour of a `SingleUtxoValidator` base trait, with an explicit opt-out for batch-aware scripts.

**Hazards.** The 30 validators that *don't* have this check are not all safe; `UnfixedAuction`
exists in the repo purely to demonstrate the resulting exploit
(`SE/auction/UnfixedAuction.scala:11-27`). This is the single highest-value candidate for a
"secure by default" stdlib entry point.

---

### P19 — "Indexed lookup with redeemer-supplied index"

**Files: 10** (`outputs.at`), **7** (`inputs.at`), **2** (`referenceInputs.at`).

`SE/betting/BettingValidator.scala:161-167`
```scala
// V005 fix: Use indexed lookup to prevent double satisfaction
require(
  payoutOutputIdx >= 0,
  "Payout output index must be non-negative"
)
val payoutOutput = txInfo.outputs.at(payoutOutputIdx)
val TxOut(payoutAddress, payoutValue, _, _) = payoutOutput
```
`SE/crowdfunding/Crowdfunding.scala:453-460`
```scala
// 4. Verify donation indices are unique (prevents double-spend attack)
requireStrictlyAscending(donationInputIndices)

// 5. Calculate total being withdrawn from donation inputs
val totalWithdrawn = donationInputIndices.foldLeft(BigInt(0)) { (sum, idx) =>
    val donationInput = txInfo.inputs.at(idx)
    sum + donationInput.resolved.value.getLovelace
}
```
`SE/linkedlist/LinkedListValidator.scala:86-88`
```scala
val anchorInput = tx.inputs.at(anchorIdx)
val contAnchorOutput = tx.outputs.at(contAnchorIdx)
val newElemOutput = tx.outputs.at(newElemIdx)
```
`SE/pricebet/PricebetValidator.scala:109-114`
```scala
val oracleInput: TxInInfo = tx.referenceInputs.at(index)
oracleInput.resolved.address.credential match {
    case Credential.PubKeyCredential(hash) => fail(OracleInputMustBeOracleScript)
    case Credential.ScriptCredential(hash) =>
        require(hash == config.oracleScriptHash, OracleInputMustBeOracleScript)
}
```

**Boilerplate → API.** Every safe use of `.at` is followed by a binding check. Fuse them:
```scala
tx.inputAt(idx, expectedOutRef)                  // fails if the index is not the expected UTxO
tx.outputAt(idx, expectedAddress)                // fails if the output is not at that address
tx.refInputAt(idx, policyId, tokenName)          // fails unless it carries the beacon
tx.requireDistinct(indices) / requireAscending(indices)
```

**Hazards.**
- Negative indices: `Betting:162` and `Auction:240,296` guard with `>= 0`;
  `Crowdfunding` and `LinkedList` do not. `List.at` has PV11 on-chain guards, so the failure mode
  is a script error rather than a wrong element — but the error message is unhelpful.
- Index re-use is the real attack. `Crowdfunding.scala:524-541` needed **two** independent guards
  (equal length + distinctness) and 18 lines of comment to explain why one is not enough:
  ```scala
  require(
    donationInputIndices.length === reclaimerOutputIndices.length,
    "Reclaimer output count must match donation count"
  )
  requireDistinct(reclaimerOutputIndices)
  ```

---

### P20 — "Get my own script hash / policy id"

**Files: 10.** Three spellings.

Destructure in the input match — `SE/auction/Auction.scala:122-130`
```scala
val (scriptHash, inputValue, currentDatum) = input.resolved match
    case TxOut(
          Address(Credential.ScriptCredential(sh), _),
          value,
          OutputDatum.OutputDatum(inlineDatum),
          _
        ) =>
        (sh, value, inlineDatum.to[Datum])
    case _ => fail("Auction input must have script credential and inline datum")
```
Match on the credential — `SE/editablenft/EditableNftValidator.scala:136-139`
```scala
val scriptAddress = ownInput.resolved.address
val policyId = scriptAddress.credential match
    case Credential.ScriptCredential(hash) => hash
    case _                                 => fail(ExpectedScriptCredential)
```
`scriptOption` — `SE/factory/FactoryExample.scala:60-63`
```scala
val ownInput = tx.findOwnInputOrFail(ownRef)
val factoryPolicyId =
    ownInput.resolved.address.credential.scriptOption
        .getOrFail("Own address must be Script")
```
`SE/paymentsplitter/OptimizedPaymentSplitterValidator.scala:97-99` uses `scriptOption` too;
`SE/amm/AmmValidator.scala:215-217`, `SE/decentralizedidentity/…:250-252`,
`SE/MembershipToken.scala:127-129`, `SE/pricebet/OracleValidator.scala:124-127` each repeat the
credential match.

**Boilerplate → API.** `self.ownScriptHash` / `tx.ownPolicyId` on the validator context —
zero call sites should need to pattern-match a `Credential` to learn their own hash.
The `Auction`-style mega-destructure should be replaceable by
`val (hash, value, d) = tx.ownScriptInput[Datum](ownRef)`.

**Hazards.** The mega-destructure appears **4×** in `Auction.scala` (lines 122-130, 147-155)
and `UnfixedAuction.scala` (42-50, 66-74) and **2×** in `Crowdfunding.scala` (258-266, 288-299) —
identical 9-line blocks. In `Auction` it also binds `inputValue`, which is then **never used**
(dead binding in both branches of both files).

---

### P21 — "Preimage hashes to the committed secret"

**Files: 3.**

`SE/htlc/HtlcValidator.scala:66`
```scala
require(sha3_256(preimage) == config.image, InvalidReceiverPreimage)
```
`SE/lottery/LotteryValidator.scala:88-89`
```scala
val isValid = sha2_256(preimage) === state.playerOneSecret
require(isValid, "Fraudulent attempt")
```
`SE/PreimageValidator.scala:45`
```scala
require(sha2_256(preimage) == hash, "Wrong preimage")
```

**Boilerplate → API.** `require(preimage.hashesTo(secret))` — plus a documented note about
minimum preimage length (Lottery documents 32 bytes in prose at lines 62-65 but does not enforce it).

**Hazards.** `==` (structural) vs `===` (`Eq`) is used interchangeably for `ByteString` across the
three sites; they agree today but the inconsistency is exactly what the
`scala3-rightassoc-extension-trap` class of bugs feeds on.

---

### P22 — "Read a value from a reference input (oracle)"

**Files: 2.**

`SE/pricebet/PricebetValidator.scala:109-136` (quoted in P19/P15) — address check + beacon check +
inline datum + timestamp-within-validity check.

`SE/decentralizedidentity/DecentralizedIdentityValidator.scala:352-366`
```scala
private inline def findIdentityOwner(
    tx: TxInfo,
    policyId: PolicyId,
    identityTokenName: ByteString
): PubKeyHash = {
    val identityRefInput = tx.referenceInputs
        .find { txInInfo =>
            txInInfo.resolved.value.quantityOf(policyId, identityTokenName) === BigInt(1)
        }
        .getOrFail("Identity reference input not found")

    identityRefInput.resolved.datum
        .inlineOrFail[IdentityDatum]("Identity must have inline datum")
        .ownerPkh
}
```

**Boilerplate → API.**
```scala
tx.refInputWithToken(policyId, tokenName): TxInInfo
tx.oracleDatum[T](scriptHash, beaconName): T     // address + beacon + inline datum in one call
```

**Hazards.** `DID`'s version does **not** check the reference input's address, only the token —
acceptable because the token is one-shot, but the two examples make opposite trade-offs and
neither is obviously the house style.

---

### P23 — "Withdraw-zero / forced script execution"

**Files: 2.**

`SE/paymentsplitter/OptimizedPaymentSplitterValidator.scala:100-102`
```scala
// Just check that reward endpoint was triggered (withdraw zero trick)
StakeValidator.spendMinimal(ownScriptHash, tx)
```
`SE/upgradeableproxy/UpgradeableProxyValidator.scala:80-83`
```scala
case ProxyRedeemer.Call =>
    // Ensure the logic stake validator was called
    val logicCredential = Credential.ScriptCredential(d.logicHash)
    tx.withdrawals.getOrFail(logicCredential, LogicNotInvoked)
```

**Boilerplate → API.** `tx.requireWithdrawalFrom(scriptHash)` — one name for what is currently
`scalus.patterns.StakeValidator.spendMinimal` in one file and a raw
`withdrawals.getOrFail` in the other.

---

### P24 — "Amount must be positive"

**Files: 8.**

`SE/vesting/VestingValidator.scala:51` → `require(requestedAmount > 0, NonPositiveAmount)`
`SE/crowdfunding/Crowdfunding.scala:374` → `require(amount > 0, "Donation amount must be positive")`
`SE/crowdfunding/Crowdfunding.scala:694` → `require(goal > 0, "Goal must be positive")`
`SE/amm/AmmValidator.scala:239` → `require(x0 > 0 && x1 > 0, "Deposit: amounts must be positive")`
`SE/auction/Auction.scala:416-419` → `require(startingBid > 0, "Starting bid must be positive")`
`SE/simpletransfer/SimpleTransferValidator.scala:66` → `require(amount.isPositive, "Negative amount")`

**Boilerplate → API.** Trivial, but worth a `requirePositive(x, name)` so the message is uniform;
more valuable is that `SimpleTransfer` uses `Value.isPositive` (a *whole-Value* predicate) while
the rest compare a `BigInt` — the stdlib should offer both under obviously different names.

---

### P25 — "Reject native tokens"

**Files: 3.**

`SE/vault/VaultValidator.scala:89`
```scala
require(value.withoutLovelace.isZero, CannotAddTokens)
```
`SE/paymentsplitter/PaymentSplitterValidator.scala:64-70`
```scala
// Only ADA is split. A contract UTxO holding native tokens would let the
// fee payer pocket those tokens for free (outputs reconcile lovelace only),
// so reject non-ADA contract inputs outright.
require(
  input.resolved.value.withoutLovelace.isZero,
  "Contract input must contain only ADA"
)
```
`SE/paymentsplitter/OptimizedPaymentSplitterValidator.scala:153-158` — same code, same comment.

**Boilerplate → API.** `value.requireAdaOnly()`. This is the *mitigation* for the P07/P17
lovelace-only hazard; only 3 of the 8 lovelace-only contracts apply it.

---

### P26 — "Redeemer index list is well-formed"

**Files: 1** (Crowdfunding) — but the concept generalises to every indexed-UTxO contract.

`SE/crowdfunding/Crowdfunding.scala:619-639`
```scala
def requireStrictlyAscending(indices: List[BigInt]): Unit =
    // Use fold to check consecutive pairs: track previous value, verify each is greater
    // Start with minimum possible value so first element always passes
    indices.foldLeft(BigInt(-1)) { (prev, curr) =>
        require(prev < curr, "Donation indices must be strictly ascending (no duplicates)")
        curr
    }
    ()

def requireDistinct(indices: List[BigInt]): Unit =
    indices.foldLeft(List.empty[BigInt]) { (seen, curr) =>
        require(!seen.contains(curr), "Reclaimer output indices must be distinct")
        List.Cons(curr, seen)
    }
    ()
```

**Boilerplate → API.** Promote both verbatim: `List.requireStrictlyAscending`,
`List.requireDistinct` (the latter is O(n²) — the stdlib version should say so, or sort).

---

### P27 — "Exactly one token type under a policy in this Value"

**Files: 5.**

`SE/betting/BettingValidator.scala:244-248`
```scala
/** The bet NFT's token name — the single asset under the bet's own policy in its UTxO value. */
private inline def betTokenName(value: Value, scriptHash: PolicyId): TokenName =
    value.tokens(scriptHash).toList match
        case List.Cons((name, _), List.Nil) => name
        case _                              => fail("Bet UTxO must hold exactly one bet token")
```
`SE/factory/Factory.scala:173-178`
```scala
val ownFactoryTokens =
    ownInputValue.toSortedMap.get(factoryPolicyId).getOrFail(NoFactoryToken)
val (tokenName, _) = ownFactoryTokens.toList match
    case List.Cons(pair, List.Nil) => pair
    case _                         => fail(MustHaveExactlyOneFactoryToken)
```
`SE/linkedlist/LinkedListValidator.scala:178-180`
```scala
val nftPolicyId = ownInput.resolved.value.toSortedMap.toList match
    case List.Cons((_, _), List.Cons((nftPol, _), List.Nil)) => nftPol
    case _ => fail("Cannot find NFT policy in own UTxO")
```
`SE/crowdfunding/Crowdfunding.scala:655-659`
```scala
val hasOneToken = tokens.get(tokenName) match
    case Option.Some(qty) => tokens.size === BigInt(1) && qty === BigInt(1)
    case Option.None      => false
require(hasOneToken, "Donation input must have exactly 1 donation token")
```

**Boilerplate → API.** `value.singleTokenOf(policyId): TokenName` (fails on 0 or >1) and
`value.singleAssetName: (PolicyId, TokenName)`.

**Hazards.** `LinkedList:178-180` assumes the value is *exactly* `[ada, nft]` — a second native
asset in the node UTxO breaks the list irrecoverably. `Betting:246` derives the token name from
the value it is about to burn, so an attacker who can add a token to the bet UTxO turns a
successful reclaim into a permanent failure (griefing).

---

### P28 — "Token name has a prefix / structured name"

**Files: 2.**

`SE/decentralizedidentity/DecentralizedIdentityValidator.scala:335-349`
```scala
private inline def findTokenWithPrefix(
    value: Value,
    policyId: PolicyId,
    prefix: String
): ByteString = {
    val prefixBs = ByteString.fromString(prefix)
    val tokenMap = value.tokens(policyId)
    // Find first token whose name starts with the prefix
    tokenMap
        .find { case (tn, qty) =>
            qty > 0 && tn.take(prefixBs.length) === prefixBs
        }
        .map(_._1)
        .getOrFail("Token with prefix not found")
}
```
`SE/editablenft/EditableNftValidator.scala:180-185`
```scala
// CIP-67/68 asset name labels: 100 (0x000643b0) = reference token, 222 (0x000de140) = user token.
inline def refNftName(tokenId: ByteString): ByteString = Cip68ReferenceLabel ++ tokenId
inline def userNftName(tokenId: ByteString): ByteString = Cip68UserLabel ++ tokenId

private inline def Cip68ReferenceLabel: ByteString = hex"000643b0"
private inline def Cip68UserLabel: ByteString = hex"000de140"
```

**Boilerplate → API.** `Cip68.referenceName(id)` / `Cip68.userName(id)` belong in the stdlib
(they are a published standard, not application logic).

**Hazards.** `findTokenWithPrefix` returns the **first** match. If an attacker can add a second
token with the same prefix under the same policy, which one is "the" identity token depends on
`SortedMap` order. Every `RevokeDelegate`/`RevokeAttribute`/`TransferOwnership` path depends on it
(`DecentralizedIdentityValidator.scala:261, 293, 312`).

---

### P29 — "Enum state-machine transition guard"

**Files: 6.**

`SE/lottery/LotteryValidator.scala:105-113`
```scala
newState.lotteryState match {
    case LotteryState.PlayerOneRevealed(length, pkh) =>
        require(length === preimage.length, "Length mismatch")
        require(
          tx.signatories.exists(_ === pkh),
          "Must be signed by player one"
        )
    case _ => fail("Invalid state transition")
}
```
`SE/vault/VaultValidator.scala:36-47` — hand-written predicates instead of a match:
```scala
extension (s: Status) {
    def isPending: Boolean = s match {
        case Status.Idle    => false
        case Status.Pending => true
    }

    def isIdle: Boolean = s match {
        case Status.Idle    => true
        case Status.Pending => false
    }
}
```
`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:107-109`
```scala
escrowDatum.state match
    case EscrowState.Deposited => ()
    case _                     => fail("Escrow must be in Deposited state")
```
`SE/vault/VaultValidator.scala:100-102` — comparing enum values by re-serialising:
```scala
// A deposit must not change the withdrawal state machine — otherwise anyone could flip a
// Pending withdrawal back to Idle (or vice versa) just by adding funds.
require(newDatum.status.toData == datum.status.toData, DepositMustNotChangeStatus)
```

**Boilerplate → API.** A declarative transition table would remove the most error-prone code in the
corpus:
```scala
StateMachine[State, Action]
  .transition(Idle, InitiateWithdrawal -> Pending)
  .transition(Pending, Cancel -> Idle)
```
Short of that: `require(state.is[EscrowState.Deposited], msg)` and derived `Eq` for enums so
`toData ==` is never needed.

**Hazards.** `Vault:102` uses `toData ==` because `Status` has no `Eq` instance — a full
serialisation of both sides on every deposit. `Lottery` duplicates a 25-line transition arm four
times per file across two files (8 copies of the same logic).

---

### P30 — "No / at most one own output"

**Files: 4.**

`SE/vault/VaultValidator.scala:152-153`
```scala
val scriptOutputs = tx.findOwnOutputsByCredential(ownInput.resolved.address.credential)
require(scriptOutputs.size == BigInt(0), WithdrawalsMustNotSendBackToVault)
```
`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:123-124`
```scala
val scriptOutputs = findOutputsByCredential(outputs, ownCredential)
require(scriptOutputs.isEmpty, "No funds should remain in script")
```
`SE/simpletransfer/SimpleTransferValidator.scala:83-84`
```scala
if withdraw === balance then
    // if withdrawing all, there should be no contract output
    require(contractOutputs.isEmpty, "Contract own output is not empty")
```
`SE/simpletransfer/SimpleTransferValidator.scala:58-61` — `<= 1` (quoted in P18).

**Boilerplate → API.** `tx.requireNoContinuingOutput(self)` — three spellings today
(`size == BigInt(0)`, `.isEmpty`, `.size <= 1`).

---

### P31 — "Script purpose dispatch"

**Files: 3** hand-roll it; the rest use `Validator` / `ParameterizedValidator` /
`DataParameterizedValidator`.

`SE/htlc/HtlcValidator.scala:38-44`
```scala
inline def validate(scData: Data): Unit = {
    val ctx = scData.to[ScriptContext]
    ctx.scriptInfo match
        case ScriptInfo.SpendingScript(txOutRef, datum) =>
            spend(datum, ctx.redeemer, ctx.txInfo, txOutRef)
        case _ => fail(MustBeSpending)
}
```
`SE/cape/twopartyescrow/TwoPartyEscrowValidator.scala:51-57` — identical shape, different message.
`SE/crowdfunding/Crowdfunding.scala:153-170` — the parameterised variant, matching
`ScriptInfo.MintingScript(policyId)`.

**Boilerplate → API.** These three should simply extend `Validator`/`DataParameterizedValidator`;
the fact that they don't suggests the traits are under-discovered or don't fit
(`TwoPartyEscrow` needs a fixed redeemer type `BigInt`; `Htlc` wants `inline` control).

---

## 3. Frequency-ranked table (all patterns)

Counts are **on-chain files containing the pattern** out of the 35-file corpus in §1.1.

| Rank | Pattern | Files | Distinct spellings | Existing helper? | Hazard level |
|-----:|---------|------:|-------------------:|------------------|--------------|
| 1 | P03 parse datum/redeemer or fail | 27 | 2 | `getOrFail` + `.to[T]` | low |
| 2 | P02 find own input | 23 | 3 | `findOwnInputOrFail` | med (O(n²)) |
| 3 | P01 signed by party | 20 | 5 | `isSignedBy` | **high** (`.head`) |
| 4 | P04 read inline datum or fail | 18 | 2 | `inlineOrFail` | low |
| 5 | P05 exactly one continuing output | 14 | 6 | none | **high** |
| 6 | P09 deadline before/after | 14 | 6 | `isEntirelyBefore/After` | **high** |
| 7 | P07 continuing value preserved | 12 | 4 | none | **high** (lovelace-only) |
| 8 | P08 continuing datum correct | 12 | 3 | none | **high** (field drift) |
| 9 | P06 continuing output address | 12 | 4 | none | med (staking part) |
| 10 | P15 authenticating NFT present | 11 | 4 | `quantityOf` | med |
| 11 | P19 indexed UTxO lookup | 10 | 3 | `List.at` | **high** (index reuse) |
| 12 | P20 own script hash extraction | 10 | 3 | `scriptOption` | low |
| 13 | P10 mint exactly one, nothing else | 9 | 3 | `Value.hasOnly` | med |
| 14 | P11 burn exactly this token | 9 | 2 | `quantityOf` | med |
| 15 | P16 output pays party X | 8 | 4 | `Address.fromPubKeyHash` | **high** (enterprise-only) |
| 16 | P24 amount must be positive | 8 | 2 | none | low |
| 17 | P17 sum lovelace over a set | 6 | 3 | `Utils.getAdaFrom*` | **high** (ADA-only) |
| 18 | P13 one-shot seed consumed | 6 | 2 | none | med |
| 19 | P29 enum state transition | 6 | 4 | none | med |
| 20 | P12 only burning allowed | 5 | 1 | none | med (empty-map) |
| 21 | P18 exactly one own input | 5 | 2 | none | **high** (missing in 30) |
| 22 | P27 single token under policy | 5 | 4 | none | med (griefing) |
| 23 | P30 no / ≤1 own output | 4 | 3 | none | low |
| 24 | P25 reject native tokens | 3 | 1 | `withoutLovelace.isZero` | low |
| 25 | P21 preimage hash check | 3 | 2 | builtins | low |
| 26 | P31 script-purpose dispatch | 3 | 1 | `Validator` trait | low |
| 27 | P14 token name from seed UTxO | 2 | 1 | none | med |
| 28 | P22 oracle via reference input | 2 | 2 | none | med |
| 29 | P23 withdraw-zero coupling | 2 | 2 | `StakeValidator` | low |
| 30 | P28 prefixed / CIP-68 token names | 2 | 2 | none | med |
| 31 | P26 redeemer index-list hygiene | 1 | 2 | none | **high** |

---

## 4. Near-duplicates the API must unify

| # | Concept | Site A | Site B | Divergence |
|---|---------|--------|--------|-----------|
| N1 | signed by | `HtlcValidator.scala:60` `tx.isSignedBy(pkh)` | `TwoPartyEscrowValidator.scala:170-179` hand-written `requireSignedBy` loop | one is O(n) `contains`, the other compares `toData` |
| N2 | signed by | `LotteryValidator.scala:109` `tx.signatories.exists(_ === pkh)` | `HelloCardano.scala:22` `tx.signatories.contains(owner)` | same function, two names |
| N3 | signed by | `PreimageValidator.scala:43` `signatories.find(_.hash == pkh).orFail` | `PubKeyValidator.scala:118-124` raw `BuiltinList` recursion | one returns `Option`, the other loops |
| N4 | own input | `VestingValidator.scala:53` `findOwnInputOrFail` | `TwoPartyEscrowValidator.scala:158-165` own `findOwnInputOrFail` | duplicate name, duplicate code, different equality |
| N5 | own input | `AmmValidator.scala:213` linear search | `OptimizedPaymentSplitterValidator.scala:91-93` indexed + bind | O(n) vs O(1); only the second scales |
| N6 | unique continuing output | `AmmValidator.scala:122-128` (`Cons/Nil` match, distinguishes 0 from >1) | `UpgradeableProxyValidator.scala:62-65` (`headOption`, **no uniqueness check**) | one is safe, one is not |
| N7 | unique continuing output | `VestingValidator.scala:101` `.length === BigInt(1)` | `VaultValidator.scala:200` `.size == BigInt(1)` | `length`/`size`, `===`/`==` |
| N8 | unique continuing output | `LotteryValidator.scala:94` `continuationOutputs.length == BigInt(1)` | `LotteryValidator.scala:138` `continuationOutputs.length === BigInt(1)` | **same file**, both spellings |
| N9 | continuing address | `VestingValidator.scala:108` full `address ===` | `VaultValidator.scala:195` `address.credential ===` | staking part checked vs not |
| N10 | continuing address | `Auction.scala:203` `Address.fromScriptHash(sh)` | `BettingValidator.scala:124` `outputAddress === address` | reconstructed enterprise address vs real address |
| N11 | continuing value | `VestingValidator.scala:114` full `Value` delta | `PricebetValidator.scala:77` `getLovelace === x*2` | tokens protected vs strippable |
| N12 | continuing datum | `Crowdfunding.scala:384-392` construct-and-compare | `LotteryValidator.scala:116-127` field-by-field "must not change" | one is closed, one fails open on new fields |
| N13 | datum equality | `VestingValidator.scala:116` `OutputDatum.OutputDatum(rawData)` | `EditableNftValidator.scala:165` `newDatum.toData === d.get` | raw-`Data` kept vs re-encoded |
| N14 | "now" | `VestingValidator.scala:68` `getValidityStartTime` (lower bound, defaults to 0) | `VaultValidator.scala:136` `validRange.to.finiteOrFail` (upper bound) | backdating-safe vs not; see P09 hazard |
| N15 | finite bound | `HtlcValidator.scala:62` `to.finiteOrFail(msg)` | `DecentralizedIdentityValidator.scala:196-198` hand-matched `IntervalBoundType.Finite` | helper vs open-coded |
| N16 | deadline | `AuctionValidator` `isEntirelyBefore(t)` | `PricebetValidator.scala:107` `!isEntirelyAfter(t)` | not equivalent for unbounded intervals |
| N17 | mint exactly one | `Factory.scala:102` `hasOnly` | `EditableNftValidator.scala:98-103` `tokens(p) === expected.tokens(p)` | 1-asset vs n-asset form |
| N18 | mint exactly one | `Auction.scala:405` `hasOnly` | `BettingValidator.scala:268-270` `Cons((_, qty), Nil)` match | name pinned vs name unconstrained |
| N19 | burn one | `Factory.scala:181-182` `quantityOf === -1` | `MembershipToken.scala:106-112` `mint.flatten.filter(...).length === 1` | second does not pin the token name |
| N20 | only burns | `Auction.scala:454-457` | `DecentralizedIdentityValidator.scala:232-235` | identical logic, different lambda syntax |
| N21 | sum ADA to a party | `Utils.getAdaFromOutputs` (Escrow, Vesting) | `BettingValidator.scala:251-255` `totalPaidTo` fold | prelude helper vs local copy |
| N22 | sum ADA to a party | `VaultValidator.scala:158-160` fold on `getLovelace` | `TwoPartyEscrowValidator.scala:115-119` fold on `lovelaceAmount` with `toData` compare | two accessor names for lovelace |
| N23 | pay-to-party | `Auction.scala:246` `address === Address.fromPubKeyHash(x)` | `LotteryValidator.scala:209-213` credential match | enterprise-only vs any address |
| N24 | single own input | `VestingValidator.scala:56-61` | `UpgradeableProxyValidator.scala:53-58` | identical code + near-identical comment |
| N25 | single token in value | `BettingValidator.scala:245-248` `tokens(p).toList` match | `Factory.scala:173-178` `toSortedMap.get(p)` + match | two ways into the same map |
| N26 | script hash of self | `EditableNftValidator.scala:136-139` credential match | `FactoryExample.scala:61-63` `scriptOption.getOrFail` | 4 lines vs 2 |
| N27 | own script input decode | `Auction.scala:122-130` 9-line `TxOut(Address(ScriptCredential…), …)` destructure | `Crowdfunding.scala:258-266` the same 9 lines | copy-paste ×6 across 3 files |
| N28 | two lotteries | `SE/lottery/LotteryValidator.scala` (304 L) | `lottery-complete/…/LotteryValidator.scala` (255 L) | whole-file near-clone; only `inlineOrFail` vs hand-match and line wrapping differ |
| N29 | four setbench validators | `SetBenchImtValidator.scala:61-69` | `SetBenchAccValidator.scala:46-52`, `SetBenchMpf16bValidator.scala:44-50`, `SetBenchMpf16oValidator.scala:43-49` | identical 6-line continuing-output epilogue ×4 |
| N30 | two payment splitters | `PaymentSplitterValidator.scala:56-73` | `OptimizedPaymentSplitterValidator.scala:145-160` | same fold, one in `spend`, one in `reward` |

---

## 5. Locally-defined helpers — promotion candidates

These are `def`s declared *inside* an example object. Every one is a stdlib candidate; the ones
marked ★ are duplicated across files.

| Helper | Location | Signature | Promote as |
|--------|----------|-----------|------------|
| ★ `findOwnInputOrFail` | `TwoPartyEscrowValidator.scala:158` | `def findOwnInputOrFail(inputs: List[TxInInfo], txOutRef: TxOutRef): TxInInfo` | already exists — delete the copy |
| ★ `findOutputsByCredential` | `TwoPartyEscrowValidator.scala:167` | `def findOutputsByCredential(outputs: List[TxOut], cred: Credential): List[v2.TxOut]` | already exists as `findOwnOutputsByCredential` |
| ★ `requireSignedBy` | `TwoPartyEscrowValidator.scala:170` | `def requireSignedBy(signatories: List[PubKeyHash], party: PubKeyHash, message: String): Unit` | `tx.requireSignedBy(pkh)` |
| ★ `paysAtLeast` | `SE/lottery/LotteryValidator.scala:297` **and** `lottery-complete/…:249` | `private inline def paysAtLeast(tx: TxInfo, pkh: PubKeyHash, amount: BigInt): Boolean` | `tx.paysAtLeast(pkh, Value)` |
| `totalPaidTo` | `BettingValidator.scala:251` | `private inline def totalPaidTo(txInfo: TxInfo, pkh: PubKeyHash): BigInt` | `tx.totalPaidTo(pkh): Value` |
| `betTokenName` | `BettingValidator.scala:245` | `private inline def betTokenName(value: Value, scriptHash: PolicyId): TokenName` | `value.singleTokenOf(policyId): TokenName` |
| `getVaultOutput` | `VaultValidator.scala:197` | `private def getVaultOutput(tx: TxInfo, ownRef: TxOutRef): TxOut` | `tx.uniqueContinuingOutput(ownRef)` |
| `getVaultDatum` | `VaultValidator.scala:204` | `private def getVaultDatum(vaultOutput: TxOut)` (→ `State`) | `out.datumAs[T]` |
| `requireOutputToOwnAddress` | `VaultValidator.scala:194` | `private def requireOutputToOwnAddress(ownInput: TxInInfo, out: TxOut, message: String): Unit` | `tx.requireSamePaymentCredential(self, out)` |
| `requireEntireVaultIsSpent` | `VaultValidator.scala:188` | `private def requireEntireVaultIsSpent(datum: State, output: TxOut): Unit` | app-specific; keep |
| `findPoolOutput` | `AmmValidator.scala:122` | `inline def findPoolOutput(outputs: List[TxOut], addr: Address): TxOut` | `outputs.uniqueAt(addr)` |
| `findScriptOutput` | `AmmValidator.scala:133` | `inline def findScriptOutput(outputs: List[TxOut], policyId: PolicyId): TxOut` | `outputs.uniqueAtScript(hash)` |
| `readPoolDatum` | `AmmValidator.scala:118` | `inline def readPoolDatum(out: TxOut): AmmDatum` | `out.datumAs[T]` |
| `verifyCampaignNftPresent` | `Crowdfunding.scala:606` | `def verifyCampaignNftPresent(value: Value, scriptHash: ValidatorHash): Unit` | `value.requireExactlyOneToken(policyId)` |
| ★ `requireStrictlyAscending` | `Crowdfunding.scala:619` | `def requireStrictlyAscending(indices: List[BigInt]): Unit` | `List.requireStrictlyAscending` |
| ★ `requireDistinct` | `Crowdfunding.scala:634` | `def requireDistinct(indices: List[BigInt]): Unit` | `List.requireDistinct` |
| `verifyDonationsBurned` | `Crowdfunding.scala:646` | `private inline def verifyDonationsBurned(txInfo: TxInfo, donationPolicyId: PolicyId, donationInputIndices: List[BigInt]): Unit` | app-specific, but the "burn count == input count" core is general |
| `findTokenWithPrefix` | `DecentralizedIdentityValidator.scala:335` | `private inline def findTokenWithPrefix(value: Value, policyId: PolicyId, prefix: String): ByteString` | `value.tokenWithPrefix(policyId, prefix)` — with a uniqueness guard |
| `findIdentityOwner` | `DecentralizedIdentityValidator.scala:352` | `private inline def findIdentityOwner(tx: TxInfo, policyId: PolicyId, identityTokenName: ByteString): PubKeyHash` | `tx.refInputWithToken(policyId, tn).datumAs[T]` |
| `refNftName` / `userNftName` | `EditableNftValidator.scala:181-182` | `inline def refNftName(tokenId: ByteString): ByteString` | `Cip68.referenceName / userName` |
| `computeTokenName` | `Factory.scala:62` | `def computeTokenName(seedUtxo: TxOutRef): TokenName` | `TokenName.fromUtxo(seedUtxo)` (shared on/off-chain) |
| `validateCreate/Destroy/Spend` | `Factory.scala:88,137,164` | `def validateCreate(tag, seedUtxo, policyId, spendingScriptHash, tx): Unit` etc. | a `FactoryPattern` module, mirroring `scalus.patterns.LinkedList` |
| `linearVesting` | `VestingValidator.scala:124` | `def linearVesting(vestingDatum: Config, timestamp: BigInt): BigInt` | `Vesting.linear(start, duration, total, now)` |
| `Status.isPending / isIdle` | `VaultValidator.scala:38,43` | `extension (s: Status) def isPending: Boolean` | generated by an enum-`Eq`/predicate derivation |
| `AmmMath.depositDatum / redeemDatum / swapResult` | `AmmValidator.scala:68,76,84` | `def swapResult(current: AmmDatum, feeNumerator: BigInt, feeDenominator: BigInt, t0In: Boolean, amountIn: BigInt): (BigInt, AmmDatum)` | keep app-specific, but the "shared on/off-chain math object" *idiom* is the stdlib lesson |
| `RationalEq.equals` | used at `PricebetValidator.scala:96` | cross-multiplication equality | `given Eq[Rational]` in the prelude |
| `handleBid/handleEnd/handleMint/handleBurn` | `Auction.scala:163,258,382,448`; `UnfixedAuction.scala:81,173,250,302`; `Crowdfunding.scala:359,427,501,680,754`; `Escrow.scala:69,108,146` | private per-action handlers | naming convention only; no API |

---

## 6. Top-15 API candidates (ranked by files-touched × hazard)

| # | Proposed API | Replaces | Files | Why |
|---|--------------|----------|------:|-----|
| 1 | `tx.uniqueContinuingOutput(self): TxOut` | P05 (6 spellings) | 14 | one of the six spellings is unsafe |
| 2 | `tx.requireContinuation(self, value = …, datum = …)` | P05+P06+P07+P08 fused | 12 | the four checks are always written together and one is usually weakened |
| 3 | `tx.requireSignedBy(pkh)` / `requireSignedByAny/All` | P01 | 20 | 5 spellings, `.head` misuse |
| 4 | `tx.ownInput(ownRef)` / `tx.ownInputAt(idx, ownRef)` (+ `ownScriptHash`) | P02+P20 | 23 | fuses the index-binding `require`; removes 9-line destructures |
| 5 | `datum.as[T]` / typed `spend[D, R]` entry point | P03+P04 | 27 | boilerplate on every single validator |
| 6 | `tx.requireAfter/requireBefore/requireWithin`, `tx.latestTime` | P09 | 14 | `getValidityStartTime` returning 0 is a live bug source |
| 7 | `tx.requireSingleOwnInput(self)` / `SingleUtxoValidator` base | P18 | 5 (needed in ~20) | double satisfaction; `UnfixedAuction` documents the exploit |
| 8 | `tx.requirePaid(pkh, value)` / `tx.totalPaidTo(pkh): Value` | P16+P17 | 8 + 6 | enterprise-address bug + lovelace-only bug |
| 9 | `tx.requireMintExactly(policyId, assets)` / `requireBurn` / `requireOnlyBurns` | P10+P11+P12 | 9 + 9 + 5 | `hasOnly` covers 1 asset; 4 files hand-roll the n-asset form |
| 10 | `tx.requireDatumChangedOnly(self, cont)(fields…)` | P08(c) | 5 | the only strategy that fails **open** when a datum grows |
| 11 | `tx.inputAt/outputAt/refInputAt(idx, expectation)` + `List.requireDistinct/Ascending` | P19+P26 | 10 | index reuse needed 18 lines of comment in Crowdfunding |
| 12 | `tx.requireSpends(seed)` / `requireMintOneShot(policy, name, seed)` | P13+P10+P14 | 6 | the one-shot idiom is 3 checks that must be co-present |
| 13 | `value.singleTokenOf(policyId)` / `value.requireExactlyOneToken(policyId)` / `tx.requireAuthNft(...)` | P15+P27 | 11 + 5 | beacon authentication is prose-documented, not API |
| 14 | `value.requireAdaOnly()` + Value-level (not lovelace-level) comparisons everywhere | P07+P25 | 8 vs 3 | only 3 of the 8 lovelace-only contracts mitigate |
| 15 | `Cip68.referenceName/userName`, `TokenName.fromUtxo(ref)`, `given Eq[Rational]`, `Eq` for enums | P14+P28+P29 | 2–6 each | published standards + missing `Eq` force `toData ==` round-trips |

---

## 7. Cross-cutting observations

1. **`===` vs `==` is not consistent, even inside one file.**
   `SE/lottery/LotteryValidator.scala:94` uses `==` and line 138 uses `===` for the same check;
   `SE/vault/VaultValidator.scala` uses `==` throughout (lines 94-102, 141, 153, 177-183) while
   every other validator uses `===`. Any new API should take typed arguments so the choice
   disappears from user code.

2. **Error messages are hand-managed constants.** 9 files keep a block of
   `inline val XyzError = "..."` at the bottom (e.g. `VaultValidator.scala:214-251`,
   38 constants). A `require`-with-derived-message API would delete all of them.

3. **The "indexed UTxO / delayed redeemer" pattern is now the house style** for anything with
   more than one UTxO (`Auction`, `Crowdfunding`, `Betting`, `EditableNft`, `DID`, `LinkedList`,
   `OptimizedPaymentSplitter`, `Lottery`), but nothing in the library supports it: every index is
   an untyped `BigInt` in a redeemer case class and every binding check is hand-written.
   A typed `Ix[TxIn]` / `Ix[TxOut]` wrapper plus `tx.at(ix, expectation)` would make the pattern
   safe by construction.

4. **Two whole files are near-clones** (`SE/lottery` vs `lottery-complete`, §4 N28) and four more
   share a copy-pasted epilogue (`setbench`, N29). Those are the cheapest possible regression tests
   for whatever API lands: if the API is right, both lotteries collapse to the same source.
