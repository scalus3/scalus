# SortedMap key-order correctness: analysis and proposal

Status: analysis, plus the oracle and the decoder fixes it uncovered. Branch
`worktree-sortedmap-ord-audit` off `origin/master` (b98a2d77c).

**Landed so far:** the pre-Conway interval simplification ("drop the pre-Conway validity-interval branch"), this document
("analysis of script-context key ordering, and the fixes it produced"), the golden-corpus oracle ("add the cardano-ledger golden TxInfo corpus as a translation oracle"), the three decoder fixes it forced
("accept transactions the ledger accepts", "accept transactions the ledger accepts", "accept transactions the ledger accepts"), and **four of the ordering defects**:

| Defect | Commit | Oracle evidence |
|---|---|---|
| 3 + 4 – `toString` sorts in votes | "stop re-sorting voting procedures by toString" | V3 votes 14 mismatches → **0** |
| 8 – `Ordering[RewardAccount]` | "order reward accounts by credential kind, not just hash" | tx-building path, no cell covers it |
| 7 – withdrawal helper split | "split withdrawal ordering by what each consumer needs" | V1 withdrawals 35 → **0** |
| 1 – `Ord[Credential]` → ledger order | "order Credential the ledger's way, script before key" | V3 withdrawals 14 → **0** |
| 5 – bloxbean withdrawal order | "order withdrawals the way the ledger does" (bloxbean) | no cell (corpus does not exercise bloxbean) |
| 10 – pointer addresses | "keep the staking credential of pointer addresses" | no cell (corpus has zero pointer addresses) |
| 6 – always-true `Eq` instances | fixed on master independently; we contribute "regression tests for Eq[DCert] and Eq[ScriptPurpose]" | JVM regression test (invisible on-chain) |
| **2 – `redeemers` → `AssocMap`** | "make TxInfo.redeemers an AssocMap with linear Eq lookup" | no cell (corpus is `Spending`-only); own test, off-chain and in UPLC |
| 12 + 13 – `ChangedParameters` encoding | "match the ledger's ChangedParameters encoding" | own tests |
| 15 – url/DNS byte length | "bound urls and DNS names by UTF-8 bytes, not characters" | own test (found by the sweep, Appendix C) |
| 9 – unsound `Ordering[GovAction]` | "delete Ordering[GovAction] and Ordering[ProposalProcedure]" | measured, then deleted |

**All six oracle cells now assert equality.** Each fix first made its cell *fail* with "defect
appears fixed, flip this cell", which is the ratchet working as intended. ExUnits baselines did
not move and MiMa stayed clean throughout.

**No defect from this audit remains open.** The defect-14 follow-up sweep is done; see Appendix C.

**Two findings from doing the work that improve on this analysis:**
1. `AssocMap` also carries `@UplcRepr(PackedDataMap)`, so switching `redeemers` to it leaves the
   **on-chain `Data` encoding unchanged** - only the lookup differs. The migration cost below was
   overestimated; there is no re-encoding and no context-byte change.
2. That move also fixes **V1/V2 `Certifying`**, which §1 called structurally unfixable by any
   `Ord`. It is unfixable *by an `Ord`*; abandoning the `Ord` resolves it.

## Source provenance (read this first)

- `cardano-ledger` at `/Users/nau/projects/lantr/cardano-ledger`. **The brief says this
  checkout is at `bbf00fc`; it is not.** Its working HEAD is `226b002d5`
  (`cardano-ledger-conway-1.23.0.0`). `bbf00fc84` exists as an object but is **not an
  ancestor of HEAD**, and the relevant files differ substantially between the two. All
  Haskell citations below were therefore read via `git show bbf00fc:<path>` so the line
  numbers match the brief.
- Every ordering-relevant fact was **re-verified at the mainnet-deployed tag**
  `cardano-ledger-conway-1.22.1.0` (node 11.0.1, PV11). All are identical. The only
  difference is a rename: `ConwayRewarding` (deployed) -> `ConwayWithdrawing` (bbf00fc),
  same constructor position. **So the conclusions hold for what is running on mainnet
  today**, not merely for an unreleased ledger revision.
- `plutus` at `/Users/nau/projects/iohk/plutus`, `ac40fae4de`, release `1.63.0.0`.

Legend: **READ** = quoted from source. **INFERRED** = derived from READ facts, chain stated.
**EXEC** = reproduced by running code (scratch tests, since deleted).

Commits on this branch are referenced by subject, not hash: the branch has been rebased and
every hash it once cited is dead. Hashes appear below only for commits already on `master`
(`4e5602090`, `54c4ba395`) and for the `cardano-ledger` checkout.

---

## 0. The mechanism, and why the suite is green

Two READ facts combine:

```scala
// scalus-core/.../plutus/prelude/SortedMap.scala:625-638
def get(key: A): Option[B] = {
    def go(lst: PairList[A, B]): Option[B] = lst match
        case PairNil => None
        case PairCons(pair, tail) => pair match
            case (k, v) => key <=> k match
                case Order.Less    => None      // <-- short-circuit
                case Order.Greater => go(tail)
                case Order.Equal   => Some(v)
```

```scala
// scalus-core/.../plutus/prelude/SortedMap.scala:293-296
/** ... There is no validation that the keys are in strictly ascending order */
given sortedMapFromData[A: FromData, B: FromData]: FromData[SortedMap[A, B]] = ...
```

So `Ord` must agree with the order the ledger delivers, or `get` returns `None` for a
present key.

**On-chain the decode step does not merely fail to re-sort - it does not exist.**
`SortedMap` is declared `@UplcRepr(UplcRepresentation.PackedDataMap)` (`SortedMap.scala:17`),
so under V3 lowering the value *is* the node's raw Data map and `fromData` lowers to a
no-op cast. The `sortedMapFromData` given quoted above is the **off-chain / `simple`
backend** path only. This removes the last hypothetical escape: there is no decode stage
on-chain that could reorder anything, so the node's byte order reaches `get` verbatim.

**The reason most tests pass is not that the maps are fine.** For the `fromList` fields it
is that Scalus's own off-chain translator re-sorts them with the very `Ord` under test:

```scala
// LedgerToPlutusTranslation.scala:650  (V2)
withdrawals = SortedMap.fromList(getWithdrawals(body.withdrawals)),
// LedgerToPlutusTranslation.scala:713  (V3)
withdrawals = SortedMap.fromList(withdrawals),
```

and `fromList` is an insertion sort by `Ord` (`SortedMap.scala:84-98`, READ). Any script
context Scalus builds is therefore **self-consistent by construction**, whatever `Ord`
says. `LedgerRulesValidationTest`'s 1000 mainnet blocks are re-translated by this same
code, so they never witness the node's ordering either. For these fields the divergence
exists only where the `Data` is built by the **real node** and consumed without any re-sort
– i.e. in actual on-chain execution, where per the paragraph above the map is used as-is,
and which no test in the repo exercises.

**This does not cover `redeemers`, and the distinction matters.** `redeemers` is built with
`SortedMap.unsafeFromList` from `redeemers.sorted` (`LedgerToPlutusTranslation.scala:656-661`,
`:719-724`). `Ordering[Redeemer]` is `(tag.ordinal, index)` (`Redeemer.scala:61-66`) and the
`RedeemerTag` ordinals – `Spend, Mint, Cert, Reward, Voting, Proposing` (`Redeemer.scala:19`)
– match `ConwayPlutusPurpose`'s constructor order exactly. So **the translator emits
redeemers in correct ledger order**, and it is `Ord[ScriptPurpose]` that disagrees with it.
Defect 2 is therefore reachable through **Scalus's own translator and Emulator, with no node
involved**: a spend+mint transaction yields `[Spending, Minting]`, and `get(Minting)` hits
`Order.Less` at `Spending` and returns `None`. The suite is green here for a different
reason – no test performs `redeemers.get` on a multi-tag map, and the mainnet-replay scripts
are Plutus/Aiken `AssocMap` linear lookups, never Scalus's `SortedMap.get`.
(INFERRED from two verified links: `Ordering[Redeemer]` READ, and the identical `get` miss
on exactly that key order demonstrated in EXEC `[B]`.)

This is the single most important finding: **the test suite is structurally incapable of
detecting these bugs**, and its greenness is not evidence of correctness.

The same blindness covers defect 8 by a parallel route: the transaction **builder** assigns
Reward redeemer indices from `Ordering[RewardAccount]`, and Scalus's **evaluator** resolves
them from the same ordering. Builder and evaluator agree with each other and disagree with
the node, so the Emulator cannot see it either.

### EXEC: both high-severity defects reproduced

Building the map in *ledger* order (as a node delivers it) and looking up a present key:

```
[A] get(scriptCred) = Some(1)
[A] get(keyCred)    = None    <-- present in map, expected Some(2)
[B] get(Spending) = true
[B] get(Minting)  = false     <-- present, expected true
[C] Scalus Ord[Credential].compare(key, script)       = Less    (ledger says Greater)
[C] Scalus Ord[ScriptPurpose].compare(Minting, Spending) = Less  (ledger says Greater)
```

**And in compiled UPLC, which is what actually runs on-chain.** A validator
`(d: Data) => d.to[SortedMap[Credential, BigInt]].get(keyCred).isDefined`, compiled with
`PlutusV3.compile` and applied to a two-entry withdrawals map containing the key:

```
[J] ledger order (what a real node delivers) -> Const(Bool(false))  ExUnits(8756,2127568)
[J] plutus order (what Scalus's Ord expects) -> Const(Bool(true))   ExUnits(12350,3031015)
```

The key is present in both maps; `true` is correct for both. This is the defect executing
in real UPLC, not a JVM-side artifact - note the earlier `Eq` case (defect 6) where JVM and
UPLC **disagree**, which is exactly why this had to be measured rather than argued.

---

## 1. Per-version, per-field delivered key order

**Shared spine (READ).** All three Plutus versions build redeemers through one function:

```haskell
-- eras/babbage/impl/src/Cardano/Ledger/Babbage/TxInfo.hs:222-226
transTxRedeemers proxy pv tx =
  PV2.unsafeFromList <$> mapM (transRedeemerPtr proxy pv $ tx ^. bodyTxL)
      (Map.toList $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
```

`Map.toList` fixes the order **before** any Plutus conversion, and the map is
`Map (PlutusPurpose AsIx era) (Data, ExUnits)` (`Alonzo/TxWits.hs:145`). `AsIx` keeps only
the index (`Alonzo/Scripts.hs:281-283`), so the order is **(constructor, Word32 index)**.
Conway calls it for V1, V2 *and* V3 (`Conway/TxInfo.hs:458`, `:504`).

Ledger constructor order (READ, `Conway/Scripts.hs:202-209`, identical at the mainnet tag):
`Spending < Minting < Certifying < Withdrawing(=Rewarding) < Voting < Proposing`.

| Version | Field | Scalus type | Delivered order | Scalus `Ord` correct? | Evidence |
|---|---|---|---|---|---|
| V1 | `withdrawals` | `List[(StakingCredential,BigInt)]` (`v1/Contexts.scala:641`) | Plutus order (`Alonzo/Plutus/TxInfo.hs:301-309` builds `Map PV1.StakingCredential`) | no `get`-miss risk (a `List`), but the **field content order is wrong today** – built straight from raw-hash `getOrderedWithdrawals` with no `fromList` to mask it. See defect 7. | READ |
| V1 | `data` | `List[(DatumHash,Datum)]` | `Map.toList` on `TxDats` = hash bytes | n/a (List) | READ |
| V1 | `dcert` | `List[DCert]` | submitter order | n/a (List) | READ |
| V1 | *redeemers* | **field does not exist** in `v1.TxInfo` | – | n/a | READ |
| V2 | `withdrawals` | `SortedMap[StakingCredential,BigInt]` (`v2/Contexts.scala:154`) | **Plutus order** (PubKey<Script) | **YES, correct today** | READ |
| V2 | `data` | `SortedMap[DatumHash,Datum]` | hash bytes | YES | READ |
| V2 | `redeemers` | `SortedMap[v1.ScriptPurpose,Redeemer]` | (ledger ctor, index) | **NO** – defect 2 | READ |
| V3 | `withdrawals` | `SortedMap[Credential,Lovelace]` (`v3/Contexts.scala:861`) | **ledger order** (Script<Key) via `transMap` (`Conway/TxInfo.hs:514`, `:549-551`) | **NO** – defect 1 | READ |
| V3 | `data` | `SortedMap[DatumHash,Datum]` | hash bytes (`Conway/TxInfo.hs:518`) | YES | READ |
| V3 | `redeemers` | `SortedMap[v3.ScriptPurpose,Redeemer]` | (ledger ctor, index) | **NO** – defect 2 | READ |
| V3 | `votes` | `SortedMap[Voter,SortedMap[GovernanceActionId,Vote]]` | ledger `Ord Voter`, inner `Ord GovActionId` (`Conway/TxInfo.hs:701-704`) | outer ctor order OK; **inner Credential wrong**, and translator destroys both – defects 3/4 | READ |
| V3 | `proposalProcedures` | `List[ProposalProcedure]` | submitter order | n/a (List) | READ |

**The V2/V3 withdrawals split is real and is the trap that killed the prior attempt.**
V1/V2 build a map already keyed by the *Plutus* type, so `Map.toList` yields Plutus order:

```haskell
-- eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/TxInfo.hs:301-309
transWithdrawals :: Withdrawals -> Map.Map PV1.StakingCredential Integer
transWithdrawals (Withdrawals mp) = Map.foldlWithKey' accum Map.empty mp
  where accum ans accountAddress (Coin n) =
          Map.insert (PV1.StakingHash (transAccountAddress accountAddress)) n ans
transTxBodyWithdrawals txBody = Map.toList (transWithdrawals (txBody ^. withdrawalsTxBodyL))
```

V3 instead keys by the *ledger* type and converts afterwards (`Conway/TxInfo.hs:697-699`,
`:549-551`). The two `Credential` types have **opposite** constructor order:

```haskell
-- cardano-ledger: libs/cardano-ledger-core/src/Cardano/Ledger/Credential.hs:98-101
data Credential (kr :: KeyRole) = ScriptHashObj !ScriptHash | KeyHashObj !(KeyHash kr)
  deriving (Show, Eq, Generic, Ord)
-- plutus: plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs:30-37
data Credential = PubKeyCredential PubKeyHash | ScriptCredential ScriptHash
  deriving stock (Eq, Ord, Show, Generic)
```

(`AccountAddress` orders by `(Network, AccountId)` – `Address.hs:183-190` – and the network
is constant within a real transaction, so ledger-`Credential` order dominates there.)

**Measured against the golden corpus (Appendix B), the delivered order is exactly
`(Network, ScriptHashObj < KeyHashObj, hash ascending)`.** The network is *dropped* from the
Plutus key by `transAccountAddress`, so a flat key sequence from a mixed-network transaction
reads as two concatenated Script-then-Key blocks. Every multi-key corpus instance splits into
at most two such blocks – exactly the two networks – with zero internal violations.

Two consequences. First, defect 8's fix is precisely "insert the constructor comparison
between network and hash" in `Ordering[RewardAccount]`, which already compares the network.
Second, tests should compare **flat key sequences and nothing cleverer**: once that ordering
is fixed, Scalus receives `RewardAccount`s carrying their networks and reproduces the blocked
sequence naturally. Adding block detection to a test would only hide the defect.

### Index source per redeemer purpose (task 1's second half)

`transRedeemerPtr` resolves each index via `redeemerPointerInverse`
(`Babbage/TxInfo.hs:202-207`), which is `fromIndex` into a container
(`Conway/TxBody.hs:667-679`, `Alonzo/TxBody.hs:536-544`). Container semantics from the
`Indexable` instances (`Alonzo/TxBody.hs:546-581`), all READ:

| Purpose | Index resolved against | Container order | Content-based `Ord` can track it? |
|---|---|---|---|
| `Spending` | `inputsTxBodyL` : `Set TxIn`, `Set.elemAt` | `Ord TxIn` = (TxId bytes, ix) | **Yes** – Scalus `Ord[TxOutRef]` matches |
| `Minting` | `mintedTxBodyF` : `Set PolicyID`, `Set.elemAt` | ScriptHash bytes | **Yes** |
| `Certifying` | `certsTxBodyL` : `OSet`, via `OSet.toStrictSeq` | **submitter/insertion order** | **No** |
| `Withdrawing`/`Rewarding` | `unWithdrawals` : `Map AccountAddress`, `Map.elemAt` | ledger `Credential`, Script<Key | **Yes, but only in ledger order** |
| `Voting` | `votingProceduresTxBodyL` : `Map Voter`, `Map.elemAt` | ledger `Ord Voter` (`Procedures.hs:338-342`), inner ledger `Credential` | **Yes, in ledger order** |
| `Proposing` | `proposalProceduresTxBodyL` : `OSet` | **submitter order** | **No** |

**Consequence.** V3 escapes the two submitter-ordered cases because its Plutus keys carry
the index: `Certifying(index, cert)` and `Proposing(index, procedure)`
(`v3/Contexts.scala:662`, `:664`), and Scalus already compares index first
(`v3/Contexts.scala:717-724`, `:734-737`). **V1/V2 do not**: `v1.ScriptPurpose.Certifying(cert)`
is content-only (`v1/Contexts.scala:738`) while certificates are submitter-ordered. So
**V1/V2 `Certifying` is the one cell no total content order can ever track.** Everything
else is reachable by a ledger-aligned `Ord`.

### The irreconcilable instance (what the prior attempt missed)

`Ord[StakingCredential]` is load-bearing in two V2 places that need **opposite** orders:

- `v2.TxInfo.withdrawals : SortedMap[StakingCredential, …]` needs **Plutus** order (PubKey<Script).
- `v1.ScriptPurpose.Rewarding(stakingCred)` inside `v2.TxInfo.redeemers` needs **ledger**
  order (Script<Key), because the delivered order follows the `AsIx` index, which is the
  position in `Map AccountAddress`.

A single `given Ord[StakingCredential]` cannot satisfy both.

**Resolved, as far as the fields go** ("order Credential the ledger's way, script before key"): `Ord[Credential]` now follows the ledger,
and `Ord[StakingCredential]` carries an explicit Plutus-order body that no longer delegates to
it. Each instance documents which era it serves. The V2 withdrawals oracle cell stayed green
across every commit, which is what shows the split is right rather than the V3 cell merely
having gone green.

**Resolved for `redeemers` too, by removing the question** ("make TxInfo.redeemers an AssocMap with linear Eq lookup"). `redeemers` is no
longer keyed by an `Ord` at all, so `v1.ScriptPurpose.Rewarding` delegating to Plutus order no
longer matters: the assoc-list lookup is `Eq`-driven and order-insensitive. The bespoke
ledger-order comparator that would otherwise have been needed in that one arm was never
written, which is the outcome option B was chosen for.

### Upstream will not stabilise this

[IntersectMBO/plutus#5726](https://github.com/IntersectMBO/plutus/issues/5726): "the ordering
of `ScriptPurpose` is not well-defined and left as an implementation detail". Real Plutus is
immune because `AssocMap.lookup` is a linear `Eq` scan (`plutus-tx/src/PlutusTx/AssocMap.hs:240-248`,
READ) over an order-preserving `unsafeFromList` (`:224-225`, READ); Aiken types these fields
as `Pairs`. **The exposure is specific to Scalus's short-circuiting `SortedMap.get`.**

---

## 2. Defects

Severity: **high** = silently wrong on-chain result; **medium** = wrong bytes/indices off-chain.

| # | Sev | Location | Defect | Evidence |
|---|---|---|---|---|
| 1 | high – **FIXED** ("order Credential the ledger's way, script before key") | `v1/Contexts.scala:487` `given Ord[Credential]` | PubKey<Script, but V3 delivers Script<Key. `withdrawals.get(pubKeyCred)` misses a present key when a script credential also withdrew. | EXEC `[A]`,`[C]`, and `[J]` in compiled UPLC |
| 2 | high – **FIXED** ("make TxInfo.redeemers an AssocMap with linear Eq lookup") | `v1/Contexts.scala:762` and `v3/Contexts.scala:697`, both `Ord[ScriptPurpose]` | Order `Minting<Spending<Rewarding<Certifying`; ledger is `Spending<Minting<Certifying<Withdrawing<Voting<Proposing`. `redeemers.get(Minting(..))` misses in any spend+mint tx. | EXEC `[B]`,`[C]` Fixed by moving the field to `AssocMap` (option B), not by reordering. |
| 3 | high – **FIXED** ("stop re-sorting voting procedures by toString") | `LedgerToPlutusTranslation.scala:990`, `:993` | `getVotingProcedures` re-sorts by `_._1.toString`. `vp.procedures` is **already** a correct `SortedMap` (`Voter.scala:94`). `toString` breaks both levels: `"…HotKey" < "…HotScript"`, and gov-action index 10 before 2. Wrong on its own terms, independent of `Ord`. | EXEC `[F]` |
| 4 | high – **FIXED** ("stop re-sorting voting procedures by toString") | `LedgerToPlutusTranslation.scala:834-836` | `getScriptPurposeV3`'s `RedeemerTag.Voting` resolves an index by position in the same `toString` sort, so it can name the **wrong voter**. | READ |
| 5 | medium – **FIXED** ("order withdrawals the way the ledger does" (bloxbean)) | `bloxbean-.../Interop.scala:40`, and both `getScriptPurpose` Reward arms | `Ordering[StakingCredential.StakingHash]` orders by raw hash bytes, ignoring the constructor – an order no node emits. **On fixing it a second wrong order turned up in the same file**, not recorded here originally: both Reward index resolutions sorted by `_.getBytes` over the raw reward address, and the address header packs credential kind (bit 4) *above* network (low nibble), so that sorts by kind above network and puts keys before scripts – wrong on both axes. | READ |
| 6 | medium (off-chain only – **verified**) – **FIXED on master independently**, while this branch was in progress; we contribute the regression tests | `v1/Contexts.scala:310` `Eq[DCert]`, `:743` `Eq[ScriptPurpose]` | Inner pattern binders shadow the outer ones, so every field comparison is a self-comparison (`curSymbol === curSymbol`). Both return `true` for **any** two values sharing a constructor. **The off-chain-only claim was not inherited from the brief – it was measured:** compiling one expression and reading both paths gives JVM `compiled.code = true` but on-chain `program.term.evaluate = Const(Bool(false))`. So on-chain `===` really does lower to structural comparison and bypass the body; the defect is JVM/off-chain only. | EXEC `[D]`,`[E]`,`[H]`,`[I]` |
| **7** | **high** – **FIXED** ("split withdrawal ordering by what each consumer needs") | **`LedgerToPlutusTranslation.scala:386-390`** | **NEW – not in the brief.** `getOrderedWithdrawals` has defect 5's clone **in core**. It resolves Rewarding redeemer indices at `:773` (V1) and `:820` (V3); the ledger resolves them against `Map AccountAddress` (script-first). Names the **wrong credential** in the script purpose. | READ |
| **8** | **high** – **FIXED** ("order reward accounts by credential kind, not just hash") | **`RewardAccount.scala:24-31` `Ordering[RewardAccount]`** | **NEW – not in the brief, and the worst of the three clones.** Compares `(network, payload.asHash)`, ignoring the script/key constructor. `Withdrawals` is a `SortedMap[RewardAccount,Coin]` (`Withdrawals.scala:15`) under this ordering, and `TransactionEditor.indexFor` (`:115-127`) derives **Reward redeemer indices from its key order**. Traced onto the build path: `indexFor` is called by `attachRedeemer` (`:143-149`), which stamps the index into the `Redeemer` it emits. So Scalus assigns an index the node will resolve against ledger order to a *different* withdrawal, invoking the wrong redeemer or failing validation. | EXEC `[G]` + READ |
| 9 | low (latent) – **FIXED** ("delete Ordering[GovAction] and Ordering[ProposalProcedure]") | `GovAction.scala:112`, `ProposalProcedure.scala:32` | **Correction: the earlier wording "non-total" was wrong.** `compare` is a total function; what it violates is **antisymmetry**, i.e. consistency with `equals`: `Ordering.by(_.ordinal)` returns 0 for any two actions sharing a constructor, so `compare(a, b) == 0` without `a == b`. `SortedSet`/`SortedMap` read `compare == 0` as "same key", so the failure mode is **silent data loss**, not a wrong sort order. Measured: `NoConfidence(None) != NoConfidence(Some(id))`, `compare == 0`, `SortedSet(both).size == 1`. `Ordering[ProposalProcedure]` compared `govAction` through it and inherited the flaw. **Both deleted** rather than documented: a `given` is picked up silently, so a comment helps only someone already reading the file. Neither was used - they date from `4e5602090`, which stubbed `GovAction` at the ordinal because full comparison "would require Ordering for many nested types", and `54c4ba395` then moved `proposalProcedures` to the insertion-ordered `TaggedOrderedSet`, removing the only reason they existed. | EXEC |

**Swept and found clean** (task 2, so the negative result is on record): `Ord[ByteString]`
is `lessThanByteString`, unsigned with shorter-prefix-first (`Builtins.scala:260-270`),
matching Haskell `ByteString` `Ord` – so every hash-keyed cell above is sound;
`Ord[GovernanceActionId]` (`v3:460`) is numeric on the index, so defect 3's "10 before 2"
claim is real; `Ord[Voter]` (`v3:384`) has the correct **constructor** order
(Committee<DRep<StakePool, matching `Procedures.hs:338-342`) and is wrong only through the
`Credential` it delegates to; `Ord[DCert]` (`v1:343`) is total and self-consistent;
`Ordering[GovActionId]`, `Ordering[TransactionInput]`, `Ordering[Redeemer]` are correct.

Defects **7 and 8 are the analysis's main addition** to the brief: the same
constructor-ignoring ordering exists three times, and one copy sits in transaction
building rather than script-context translation.

---

## 3. Serialisation blast radius

> **Correction.** My first sweep here was invalid: the pattern was
> `grep -rn ... $W | grep -v "\.claude"`, and because the worktree path itself contains
> `.claude`, that filter discarded **every** line. The empty result meant "nothing survived
> the filter", not "nothing exists". Redone with `git grep` inside the worktree; the
> conclusions below come from real output, and the corrected sweep found **three affected
> fields the first pass would have missed**.

Every `SortedMap` in main source keyed by an affected type (`git grep`, tests excluded):

| Location | Field | Delivered order | Affected by an `Ord[Credential]` change? |
|---|---|---|---|
| `v2/Contexts.scala:154` | `TxInfo.withdrawals` | Plutus | yes – **must stay Plutus order** |
| `v2/Contexts.scala:157` | `TxInfo.redeemers` | (ctor, AsIx) | yes |
| `v3/Contexts.scala:861` | `TxInfo.withdrawals` | ledger | yes |
| `v3/Contexts.scala:864` | `TxInfo.redeemers` | (ctor, AsIx) | yes |
| `v3/Contexts.scala:867` | `TxInfo.votes` | ledger | yes |
| **`v3/Contexts.scala:513`** | **`GovernanceAction.TreasuryWithdrawals.withdrawals`** | **ledger** – `transMap transAccountAddress` (`Conway/TxInfo.hs:671-674`, READ) | **yes – missed by the first sweep** |
| **`v3/Contexts.scala:520`** | **`GovernanceAction.UpdateCommittee.addedMembers`** | **ledger** – `transMap (ColdCommitteeCredential . transCred)` (`Conway/TxInfo.hs:675-680`, READ) | **yes – missed by the first sweep** |
| **`v3/Contexts.scala:469`** | **`Committee.members`** | ledger (same `transMap` idiom) | **yes – missed by the first sweep** |
| `VotingProcedures.scala:15` | ledger-domain `procedures` | correct already (`Voter.scala:94`) | no |
| `LedgerToPlutusTranslation.scala:385`, `:984`; `bloxbean/Interop.scala:1087` | translator internals | – | defects 3/7 |
| `scalus-design-patterns/UtxoIndexer.scala:193` | a `redeemers` **parameter** | – | consumer, not a constructor |

These three extra fields **strengthen** the recommendation rather than complicating it: all
three want **ledger** order, which is what flipping `Ord[Credential]` gives them. They are
currently wrong for the same reason V3 `withdrawals` is.

- **No user datum or redeemer type** in `scalus-examples` or `scalus-design-patterns` is
  keyed by these types – every hit above is either a Plutus context type, a translator
  internal, or a consumer. So changing `Ord` does **not** change any user datum's CBOR.
- **Script-context bytes do change.** `SortedMap.toData` emits in `Ord` order, and the
  translator's `fromList` (`:650`, `:713`) sorts by `Ord`. Changing `Ord` changes the
  `Data` handed to the CEK machine for any context with 2+ withdrawals or 2+ redeemers,
  which changes CPU/memory.
- **Therefore ExUnits-pinned tests must be re-measured**, and some pin **per compiler
  generation** (`ScalaCompilerVersion.baseline(pre38, since38)`), so those need measuring
  on **both** 3.3.x and 3.8.x. Files carrying baselines: `PreimageExampleTest`,
  `AuctionValidatorTest`, `OptimizedPaymentSplitterValidatorTest`,
  `NaivePaymentSplitterValidatorTest`, `AmmTest`, `ClausifyTest`, `KnightsDataTest`.
  Most single-purpose examples will not move (they have <2 entries); this is a
  re-measure-and-check, not an expected mass update.
- **Defect 8 changes transaction CBOR**: fixing `Ordering[RewardAccount]` reorders the
  withdrawals map in the serialised body, changing the **transaction hash** for any tx with
  2+ withdrawals whose constructor and byte orders disagree. Any test pinning a tx hash or
  fee with multiple withdrawals will need updating. A correctness fix, but a visible
  byte-level change.

---

## 4. Recommendation

### Fix unconditionally, under every option: defects 3, 4, 7, 8 — **all landed**

These are wrong on their own terms and independent of the `Ord` design question. They are
also the cheapest and highest-value work here.

- **3/4**: delete both `.sortBy(_._1.toString)` calls. `vp.procedures` is already a
  correctly ordered `SortedMap`; use `SortedMap.unsafeFromList(List.from(vp.procedures))`
  and iterate the inner map in its existing order. Resolve the Voting redeemer index
  against that same order.
- **7/8**: replace the two raw-hash orderings with the ledger's – **script before key**,
  then hash bytes – and give `RewardAccount`/`StakeAddress` one shared, documented
  ordering so the three copies become one. This is the fix that repairs transaction
  building.

*Note the trap that killed the prior attempt:* the `toString` sort in defect 3 is
**accidentally consistent** with the old PubKey-first `Ord[Credential]`. Flip `Ord` without
fixing 3 and votes break. **Fix 3 and 4 first, verify, then touch `Ord`.** That is the order
actually used – "stop re-sorting voting procedures by toString" → "order reward accounts by credential kind, not just hash" → "split withdrawal ordering by what each consumer needs" → "order Credential the ledger's way, script before key" – with the oracle
verifying each step, and the votes cell stayed green throughout.

### For the ordering question: hybrid (B for redeemers, A for the rest)

| Field | Representation | Why |
|---|---|---|
| `withdrawals` (V2) | keep `SortedMap`, keep **Plutus** order | content-keyed; ledger delivers Plutus order; **already correct, do not touch** |
| `withdrawals` (V3) | keep `SortedMap`, `Ord[Credential]` -> **ledger** order (Script<Key) | content-keyed and ledger-ordered; a corrected `Ord` tracks it exactly |
| `data` (V2/V3) | keep `SortedMap` | hash-keyed, already correct |
| `votes` (V3) | keep `SortedMap` | content-keyed; correct once `Ord[Credential]` is fixed and defect 3 is gone |
| `TreasuryWithdrawals`, `UpdateCommittee.addedMembers`, `Committee.members` (V3) | keep `SortedMap`, **ledger** order | content-keyed and ledger-ordered; fixed by the same `Ord[Credential]` flip |
| **`redeemers` (V2/V3)** | **association list + linear `Eq` lookup** | positionally keyed by `(constructor, AsIx)`; **no content order is correct in general**, and V1/V2 `Certifying` is provably untrackable |

**Why not pure A.** It cannot fix V1/V2 `Certifying` at all (READ: content-only Plutus key,
submitter-ordered container). It also pins Scalus to an ordering upstream explicitly
refuses to specify, which has **already changed once** – withdrawals moved from Plutus to
ledger keying between V2 and V3 – and it forces the bespoke `Rewarding` comparator
described in §1 to dodge the `Ord[StakingCredential]` conflict. A is a correct-looking fix
that leaves a known hole and a standing dependency on an unpromised invariant.

**Why not pure B.** Converting the content-keyed fields too would be a larger API break for
no correctness gain, and would replace today's early-exit scan with a full scan on
`withdrawals`/`data`/`votes`, which are the fields validators touch most. (Both are linear;
the sorted form just stops early on a miss.)

**Why the split is principled, not a compromise:** it follows the ledger's own distinction.
Content-keyed fields have an order derived from their keys, so a key-based structure is
right for them. `redeemers` has an order derived from *positions in other containers*, so
no key-based structure can be right for it – which is precisely why upstream Plutus and
Aiken both use association lists (`Pairs`) here.

**Cost.** `redeemers` maps are tiny (one entry per script invocation; typically 1–4,
rarely >10), so a linear `Eq` scan costs a handful of CEK steps more than the current
early exit – far below the ~13.85 lovelace/call scale of a couple of steps. Against that,
`Eq` on `ScriptPurpose` lowers to `equalsData` under V3 lowering, which is cheap. The real
cost is the **public `TxInfo` API break** on the `redeemers` field and a MiMa filter.

**Measured after the fact:** no ExUnits baseline moved. That is **not** evidence the lookup is
free – it is that no ExUnits-pinned test performs a redeemers lookup at all. The
`StakeValidator`/`MerkelizedValidator` patterns that do are not budget-pinned. The real cost
delta of linear-`Eq` versus short-circuit-`Ord` is still **unmeasured**, and would need a
deliberate benchmark.

**Migration sketch.**
1. ~~Land defects 3/4/7/8 as a standalone, no-API-change fix. Verify.~~ **Done.** Defect 6
   was not required for these and remains open; it is still a prerequisite for step 3.
2. ~~Flip `Ord[Credential]` to ledger order. Give `Ord[StakingCredential]` an explicit
   Plutus-order body that does not delegate.~~ **Done** ("order Credential the ledger's way, script before key"). ExUnits baselines were
   re-run and did not move; MiMa clean. This fixed `votes` and the gov-action maps for free,
   but **not** `Rewarding` inside `redeemers`, which is defect 2.
3. ~~Change `redeemers` to an assoc list with `lookup` by `Eq`.~~ **Done** ("make TxInfo.redeemers an AssocMap with linear Eq lookup"), using
   the existing `prelude.AssocMap`. No deprecation window was needed and the
   `scalus-design-patterns` call sites were untouched: adding `AssocMap.getOrFail` to mirror
   `SortedMap`'s made them compile unchanged. 13 MiMa filters.

**Prerequisite ordering matters:** defect 6 must be fixed **before** step 3. Assoc-list
lookup is `Eq`-driven, and the always-true `Eq[ScriptPurpose]`/`Eq[DCert]` would make a
JVM-side lookup return the first entry sharing a constructor. On-chain `===` is structural
so on-chain B is safe, but the off-chain path would be silently wrong. Defect 6 is a
**blocker for B**, not an independent cleanup.

**What each option fixes:**

| Defect | A only | Hybrid (recommended) |
|---|---|---|
| 1 (V3 withdrawals) | fixed | fixed |
| 2 V3 redeemers | fixed | fixed |
| 2 V1/V2 `Certifying` | **NOT fixable** | fixed – **and this is what landed** ("make TxInfo.redeemers an AssocMap with linear Eq lookup") |
| 2 V1/V2 `Rewarding` | fixed, via bespoke comparator | fixed |
| 3,4,7,8 | must fix separately | must fix separately |
| 6 | optional | **prerequisite** |

---

## 5. Test design

**The proposed property test is circular for the `fromList` fields, but not for all of them.**
"Every field is ascending under its `Ord`" is, for `withdrawals`/`data`/gov-action maps,
asserted against maps the translator built with `SortedMap.fromList` – sorted by the very
`Ord` being checked – so for those it **passes today with every defect in place**, and
adding "`get(k)` is defined for every delivered key" does not rescue it.

But it is **not** circular on the `unsafeFromList` paths, and there are two of them, not
one. On `redeemers` it would **fire today**: the translator emits `[Spending, Minting]`,
which is descending under an `Ord` that says `Minting < Spending`. On `votes` it would
likely fire too. So the proposed test is worth writing – it just cannot be the only test,
because it is blind on exactly the fields where the translator re-sorts.

A test that would actually have caught all of this needs an **independent oracle**. When this
section was first written the oracle was hypothetical; **it turns out one already exists as
checked-in data in cardano-ledger** – see Appendix B. The two-layer design below stands, with
Appendix B supplying layer 1 for free:

**Layer 1 – translator faithfulness (this is the layer that catches the bugs).**
**This layer is now built** – see Appendix B. The hand-transcribed-oracle risk noted below was
avoided entirely: the vectors come from the ledger's own generator, not from a reimplementation.
Assert that Scalus's translator emits keys in the order the *ledger* would, using an order
derived independently of `plutus.prelude.Ord`:
- Write a test-only `LedgerOrder` module transcribing the Haskell `Ord`s directly
  (`Credential`: Script<Key; `AccountAddress`: (Network, Credential); `Voter`; the
  `(constructor, AsIx)` purpose order). Property: for arbitrary generated `Transaction`s,
  `getTxInfoV3(tx).withdrawals.toList.map(_._1)` equals the ledger-ordered key sequence.
  This is independent of `Ord` and fails today.
- **Stronger, and the only thing that removes the hand-transcription risk:** golden
  `ScriptContext` vectors captured from a **real node**. The repo already runs a Yaci
  DevKit devnet in companion mode with full PV11 cost models
  (`scalusCardanoLedgerIt`), so it can submit a tx with a script+key withdrawal and a
  spend+mint+cert redeemer set, capture the node's `ScriptContext` `Data`, and check it
  into the repo. Then assert Scalus's translator reproduces those bytes, and that
  `get` finds every key in them. `ScriptContextComparisonTest` is the natural home – it is
  currently a debugging tool (75 `println`s, 1 assertion) and should be turned into a real
  assertion.

**Layer 2 – once layer 1 holds**, the originally proposed property becomes meaningful as a
regression guard: ascending under `Ord`, **and** `get(k)` defined for every delivered key,
**and** – the part that makes it non-circular – asserted against maps built with
`unsafeFromList` from the ledger-ordered sequence, never with `fromList`.

**Targeted unit tests** for the shapes real transactions take, which is where the money is:
spend+mint (defect 2), script+key withdrawal (defects 1, 7, 8), withdraw-zero forwarding
alongside a certifying redeemer (the `StakeValidator.spend` break), a vote with both a
hot-key and hot-script committee voter (defect 3), gov-action indices 2 and 10 (defect 3).

**What this still misses.** The oracle is hand-derived unless the vectors come from a node,
and node vectors only cover the shapes actually submitted. Neither approach detects a
*future* ledger reordering – only a conformance suite maintained upstream would, and
upstream has declined to specify the ordering at all. That residual risk is itself an
argument for option B on `redeemers`, which is immune to reordering by construction.

---

## Impact: what actually breaks on-chain

Not hypothetical. These are shipped library call sites that will fail against a real node:

- `scalus-design-patterns/StakeValidator.scala:40-41` – `redeemers.getOrFail(Rewarding(..))`
  then `withdrawals.getOrFail(scriptCred)`. The withdrawals lookup survives (script
  credential is `Ord`-greatest, so the scan walks the whole list), which is why the pattern
  works today. The **redeemers** lookup breaks as soon as the transaction also carries a
  certifying redeemer: delivered order is `Spending, Minting, Certifying, Withdrawing`, and
  Scalus's `Ord` ranks `Rewarding < Certifying`, so `get` hits `Order.Less` at `Certifying`
  and returns `None` before reaching the entry. (INFERRED from the two READ orders; the
  same short-circuit is demonstrated directly in EXEC `[B]`.)
- `MerkelizedValidator.scala:59`, `:75`, `:78`; `TransactionLevelMinterValidator.scala:36`;
  `UpgradeableProxyValidator.scala:79` – same pattern.

## Open / could not determine

- **Proven in compiled UPLC, and now measured against the ledger's own golden output.** `[J]`
  shows the miss executing in real V3-lowered UPLC on a hand-built ledger-ordered map, and the
  oracle (Appendix B) shows our translator disagreeing with ledger-produced bytes in 14/15 V3
  withdrawal cases, 14/17 vote cases and 35/39 V1 withdrawal cases. What is still not
  *observed* is a live node emitting such a context for a transaction Scalus submitted; that
  link is READ from ledger source (verified at the deployed mainnet tag) plus the golden
  corpus, but not captured from a running node. A Yaci devnet submission would close it.
- **Frequency of the triggering shapes on mainnet** is unquantified. `LedgerRulesValidationTest`'s
  1000 blocks pass, but per §0 that suite cannot witness the defect, so it bounds nothing.
  Counting real mainnet transactions with (a) ≥2 withdrawals of differing credential kinds
  or (b) a certifying redeemer alongside a rewarding one would size the exposure.

---

# Appendix A. Full review of `LedgerToPlutusTranslation.scala`

Scope: every translation in the file checked against `cardano-ledger` at `bbf00fc`, with
ordering-relevant facts re-verified at the mainnet tag `cardano-ledger-conway-1.22.1.0`.

## A.1 Collection order, per field

Container semantics READ from the Scalus side:
`TaggedSortedSet[A] = SortedSet[A]` and `.toSet` returns the `SortedSet` itself
(`TaggedSortedSet.scala:18,27`), so it iterates **sorted**;
`TaggedOrderedSet`/`TaggedOrderedStrictSet` are `ListSet.from(_).toIndexedSeq`
(`TaggedOrderedSet.scala:28`), so they iterate in **insertion** order and never sort;
`MultiAsset.assets` is a `SortedMap[PolicyId, SortedMap[AssetName, Long]]` (`Types.scala:115`).
Both the on-chain `Ord[ByteString]` (`Builtins.scala:260-270`) and the off-chain
`Ordering[ByteString]` (`ByteStringOffchainApi.scala:143-148`) are **unsigned**, matching
Haskell's `ByteString` `Ord` – so every hash-keyed ordering below is sound.

| Field | Scalus source of order | Ledger requirement | Verdict |
|---|---|---|---|
| `inputs` | `body.inputs.toSet` -> `SortedSet[TransactionInput]` | `Set.toList (inputsTxBodyL)`, `Ord TxIn` (`Conway/TxInfo.hs:410`) | **OK** |
| `referenceInputs` | `TaggedSortedSet` | `Set.toList` | **OK** |
| `outputs` | `body.outputs` `IndexedSeq` | `F.toList (outputsTxBodyL)` = submitter order (`Conway/TxInfo.hs:416`) | **OK** |
| `dcert`/`certificates` | `TaggedOrderedStrictSet.toSeq` = insertion | `F.toList (certsTxBodyL)` = OSet order (`Alonzo/Plutus/TxInfo.hs:298-299`) | **OK** |
| `signatories` | `TaggedSortedSet` | `Set.toList (reqSignerHashesTxBodyG)` (`Alonzo/Plutus/TxInfo.hs:313-314`) | **OK** |
| `data` | `plutusData.toSortedMap`, then V2/V3 `SortedMap.fromList` | `Map.toList` on `TxDats` = hash bytes (`Alonzo/Plutus/TxInfo.hs:317-318`) | **OK** (re-sort is a no-op, but it is still an unnecessary re-sort) |
| `mint` V1/V2 | ADA entry first, then `MultiAsset` `SortedMap`; `Value.unsafeFromList` | `transMintValue m = transCoinToValue zero <> transMultiAsset m` (`Alonzo/Plutus/TxInfo.hs:344-345`) – **includes** zero-ADA | **OK** (empty policy id sorts first) |
| `mint` V3 | no ADA entry | `PV3.UnsafeMintValue . PV1.getValue . transMultiAsset` (`Conway/TxInfo.hs:545-546`) – **excludes** ADA | **OK** |
| `withdrawals` V1 | `getOrderedWithdrawals`, raw-hash | Plutus order, PubKey<Script | **WRONG** – defect 7 |
| `withdrawals` V2 | `SortedMap.fromList` -> `Ord[StakingCredential]` = Plutus order | Plutus order | **OK by accident** – the `fromList` re-sort masks the broken helper |
| `withdrawals` V3 | `SortedMap.fromList` -> `Ord[Credential]` = PubKey<Script | ledger order, Script<Key | **WRONG** – defect 1 |
| `redeemers` V2/V3 | `redeemers.sorted` = `(tag.ordinal, index)`, then `unsafeFromList` | `(constructor, AsIx)` – ordinals match `ConwayPlutusPurpose` exactly | **translator OK**; the `Ord` disagrees – defect 2 |
| `votes` | `.sortBy(_._1.toString)`, both levels | ledger `Ord Voter` / `Ord GovActionId` | **WRONG** – defect 3 |
| `proposalProcedures` | `TaggedOrderedSet.toSeq` | OSet order | **OK** |
| `TreasuryWithdrawals` | `SortedMap.fromList` -> PubKey<Script | ledger order (`Conway/TxInfo.hs:671-674`) | **WRONG** |
| `UpdateCommittee.addedMembers` | `SortedMap.fromList` -> PubKey<Script | ledger order (`Conway/TxInfo.hs:675-680`) | **WRONG** |

## A.2 Redeemer index resolution

The ledger resolves each index via `fromIndex` into a specific container
(`Conway/TxBody.hs:667-679`, `Alonzo/TxBody.hs:546-581`).

| Purpose | Scalus resolves against | Ledger resolves against | Verdict |
|---|---|---|---|
| `Spend` | `body.inputs.toSeq` (sorted) | `Set TxIn`, `Set.elemAt` | **OK** |
| `Mint` | `assets.keys` (sorted `SortedMap`) | `Set PolicyID`, `Set.elemAt` | **OK** |
| `Cert` | `body.certificates.toSeq` (insertion) | `OSet` -> `toStrictSeq` | **OK** |
| `Reward` | `getOrderedWithdrawals` (raw hash) | `Map AccountAddress`, `Map.elemAt`, Script<Key | **WRONG** – defect 7, names the wrong credential |
| `Proposing` | `proposalProcedures.toSeq` | `OSet` | **OK** |
| `Voting` | `.sortBy(_._1.toString)` | `Map Voter`, `Map.elemAt` | **WRONG** – defect 4, names the wrong voter |

## A.3 Two new non-ordering defects

| # | Sev | Location | Defect |
|---|---|---|---|
| **10** | **high** – **FIXED** ("keep the staking credential of pointer addresses") | `LedgerToPlutusTranslation.scala:99-101` | **Pointer addresses lose their staking credential.** `getAddress` maps `ShelleyDelegationPart.Pointer` to `Option.None`. The ledger maps it to a real value: `transStakeReference (StakeRefPtr (Ptr slot txIx certIx)) = Just (PV1.StakingPtr ...)` (`libs/cardano-ledger-core/src/Cardano/Ledger/Plutus/TxInfo.hs:133-137`, **byte-identical at the mainnet tag**). All three versions are affected: V1/V2 via `transAddr` (`Babbage/TxInfo.hs:153`) and V3 via `Babbage.transTxOutV2` (`Conway/TxInfo.hs:454`, `:500`). Trigger: any script transaction touching a UTxO or producing an output at a pointer address. Scripts reading `output.address.stakingCredential` see `None` where the node shows `Some(StakingPtr ..)`, changing both behaviour and ExUnits. **The fix is mechanical – both types already exist**: `Pointer(slot, txIdx, certIdx)` (`Address.scala:136`) and `v1.StakingCredential.StakingPtr(a, b, c)`. The inline comment "we don't include staking credential in script context" asserts the opposite of the ledger. Covered by a targeted unit test, not the corpus, which contains zero pointer addresses. **Not fixed for the bloxbean interop**: its `getAddress` goes through CCL's `Address.getDelegationCredential`, which does not model pointers. |
| **11** | low (historical replay only) | `LedgerToPlutusTranslation.scala:510-514`, `:547-551` | **PV9 deposit omission not modelled.** `transTxCert` takes a `ProtVer` and omits the deposit when `hardforkConwayBootstrapPhase pv`, i.e. `pvMajor pv == 9` (`Conway/Era.hs:257-258`): `RegDepositTxCert`/`UnRegDepositTxCert` yield `Nothing`, not `Just deposit` (`Conway/TxInfo.hs:573-582`). The ledger comment states this was exercised on mainnet and "can never be removed for Conway era (#4863)". Scalus's `getTxCertV3` passes the deposit unconditionally and does not take a protocol version at all. Correct at PV10+ (today's default), wrong when replaying PV9 blocks. Fix: thread the `protocolVersion` already available in `getTxInfoV3` into `getTxCertV3`. |

## A.4 Verified correct (recorded so it is not re-litigated)

- **`getInterval` is correct, despite looking wrong.** The asymmetry – upper bound gated on
  `protocolVersion <= 8` in the `(None, Some)` branch but unconditionally exclusive in
  `(Some, Some)` – mirrors a real asymmetry in the ledger. Alonzo uses `PV1.to` (inclusive)
  for `(SNothing, SJust)` but `strictUpperBound` (exclusive) for `(SJust, SJust)`
  (`Alonzo/Plutus/TxInfo.hs:260-270`); Conway made **both** exclusive and is **not**
  PV-gated (`Conway/TxInfo.hs:798-810`). So Scalus's `PV<=8` test is modelling the
  *era* boundary, not a within-Conway conditional. It deserves a comment saying so.
- **V1/V2 Conway-feature guards** match `guardConwayFeaturesForPlutusV1V2`
  (`Conway/TxInfo.hs:~360-381`): voting procedures, proposal procedures, non-zero donation,
  and current treasury value each abort.
- **`getDCert`** rejecting Conway certificates matches `transTxCertV1V2`
  (`Conway/TxInfo.hs:573-580`), which accepts only `RegDepositTxCert`/`UnRegDepositTxCert`
  plus `transTxCertCommon`, and errors otherwise.
- **Byron address handling.** Scalus throws; Conway-era `transTxOutV1` returns an error
  (`Conway/TxInfo.hs:315-318`). (Alonzo-era V1 silently *skipped* them via `mapMaybe`, but
  Scalus targets Conway.)
- **`getMintValueV1V2` vs `getMintValueV3`** – the zero-ADA split is correct, see A.1.

## A.5 `ChangedParameters` encoding – audited, two divergences

Previously listed here as unverified; the field-by-field pass has now been done.

The ledger's chain is `transGovAction` (`Conway/TxInfo.hs:662-666`) ->
`toPlutusChangedParameters` (`:787`) -> `Conway/PParams.hs:199-204`:

```haskell
toPlutusData ppu = P.Map $ mapMaybe ppToData (eraPParams @era)
  where ppToData PParam {ppUpdate} = do
          PParamUpdate {ppuTag, ppuLens} <- ppUpdate
          t <- strictMaybeToMaybe $ ppu ^. ppuLens
          pure (P.I (toInteger @Word ppuTag), toPlutusData t)
```

A `P.Map` of `(I tag, value)`; absent params omitted by `mapMaybe`, never `null`; order is
`eraPParams` order, which is ascending tag 0-11, 16-33. `ChangedParameters` is a newtype with
`deriving newtype ToData`, so the map is embedded raw with no constr wrapper
(`plutus-ledger-api/src/PlutusLedgerApi/V3/Contexts.hs:321-331`), and the Plutus doc at `:302`
states the contract: "This map is non-empty, and the keys are stored in ascending order."

Scalus's `given ToData[ProtocolParamUpdate]` (`ProtocolParamUpdate.scala:264-313`) is a
hand-written `mapData` over the same fixed 0-11, 16-33 order, skipping `None`. **All 30
present keys, the tag numbering, the threshold field orders (pool 5-tuple, DRep 10-tuple),
`ExUnits` mem-first ordering, and the absent-param handling match.** Keys 12-15 are correctly
absent on both sides. Two value encodings diverge. Both were re-verified as identical between
`bbf00fc` and the mainnet tag.

| # | Sev | Location | Defect |
|---|---|---|---|
| **12** | medium – **FIXED** ("match the ledger's ChangedParameters encoding") | `UnitInterval.scala:94-103`, `NonNegativeInterval.scala:311-320` | **Rationals are not reduced.** The ledger emits `List [I num, I denom]` from a GHC `Ratio` (`libs/cardano-ledger-core/src/Cardano/Ledger/Plutus/ToPlutusData.hs:78`), and `%` always yields lowest terms with a positive denominator; the decode path builds `n % d` (`cardano-ledger-binary/.../Decoder.hs:493-501`). Scalus emits numerator and denominator **verbatim** – `NonNegativeInterval.apply` (`:210`) does no reduction. Affects keys 9, 10, 11, 33 and every rational nested in 19, 25, 26. Example: a proposal setting `poolPledgeInfluence` to `[6, 10]` gives the chain `(I 9, List[I 3, I 5])` and Scalus `(I 9, List[I 6, I 10])`. |
| **13** | medium – **FIXED** ("match the ledger's ChangedParameters encoding") | `Types.scala:752-758` `given ToData[CostModels]` | **Cost-model map order is not sorted** (key 18). The ledger emits `Map.toAscList` over a `Data.Map Word8` (`ToPlutusData.hs:58`, `:95`; `flattenCostModels` at `CostModels.hs:443-447`), so always ascending by language id. Scalus iterates `models: Map[Int, IndexedSeq[Long]]` (`Types.scala:657`) with **no sort**, so the emitted order follows the CBOR source order. Reachable because the ledger's map decoder stopped enforcing canonical order at decoder version >= 9 (`Decoder.hs:766-795`): a proposal encoding `{2:.., 0:.., 1:..}` is accepted, the ledger emits ascending, Scalus does not. **This is the same defect class as the rest of this document** – a collection whose order is assumed rather than established. |

Both need non-canonical but ledger-accepted CBOR, so they are not reachable by accident. Any
proposer willing to pay the gov-action deposit can craft a `ParameterChange` where a
Scalus-evaluated script sees different `Data` than the chain does. The ledger normalises;
Scalus preserved the input verbatim.

**Fixed in "match the ledger's ChangedParameters encoding", at the encoding boundary only.** The ledger normalises at
*construction* - its decoder builds `n % d`, which reduces - and matching it there would have
been closer to the original. It would also change re-serialised transaction bytes, and so
transaction hashes. Normalising in `ToData` fixes the observed divergence without that risk;
a test asserts the CBOR round-trip stays exact.

**Adjacent, opposite direction:** Scalus's interval CBOR *decoders* are **stricter** than the
ledger's. `UnitInterval.scala:76-82` requires tag 30 and `denominator > 0`; the ledger allows
an untagged rational (`allowTag 30`, `Decoder.hs:439-447`) and lets `%` normalise a negative
sign. Some ledger-valid transactions therefore make Scalus throw before translation runs.

## A.5b Defect 14: our decoders are stricter than the chain

Found by Appendix B: **every one of the 100 golden transactions failed Scalus's CBOR
decoder**, while the corpus UTxO field decoded 100/100. The failures were content-level, in
three fields the ledger leaves unconstrained. Each was checked against **both** the Haskell
type and the CDDL before being called a defect.

| # | Sev | Location | Defect | Unlocked |
|---|---|---|---|---|
| **14a** | medium | `BootstrapWitness.scala` | Required a 32-byte `chainCode`. CDDL: `chain_code : bytes` (`conway.cddl:778`); Haskell: `newtype ChainCode = ChainCode ByteArray` (`Keys/Bootstrap.hs:67`). Unconstrained. | 13 |
| **14b** | medium | `GovAction.scala:201`, `:149` | `update_committee` removed members read with a plain `Set` decoder, which rejects Conway's optional 258 set tag outright, and written without it. CDDL: `set<committee_cold_credential>` (`conway.cddl:749`). | 3 |
| **14c** | medium | `PoolMetadata.scala:16` | Hash typed as a 32-byte `MetadataHash`. CDDL: `pool_metadata = [url, bytes]` (`conway.cddl:494`, unchanged since Allegra); Haskell: `pmHash :: !ByteArray` with a decoder that checks no length (`State/StakePool.hs:293-296`, `:522-524`). | 67 |

`vkey` and `signature` **are** constrained (`bytes .size 32` / `.size 64`), so those checks
correctly stay.

All three are fixed in one commit, "accept transactions the ledger accepts"; decode coverage went
0 -> 83 of 100 and MiMa stayed clean, because `MetadataHash` erases to `ByteString`. The
remaining 17 instances carry values no real transaction can hold – 16 uints above 2^63 (real
`Coin` is bounded by max supply, about 2^55) and one negative protocol major version – which
we are right to reject.

**This is the same class as the `UnitInterval` strictness noted in A.5**, and both were found
by accident rather than by looking. A systematic decoder-versus-CDDL sweep is very likely to
find more, and is worth doing on its own merits: every instance is a transaction the chain
accepts and Scalus cannot read.

## A.5c Still not verified

- `checkReferenceInputsNotDisjointFromInputs` (`Conway/TxInfo.hs:816`) – a Conway-era check
  in the same ledger module; not traced to a Scalus equivalent. Appendix B found it is a PV11
  conditional on a *failure* path, so the golden corpus cannot cover it.

## A.6 Proposal: make delivered order explicit and checkable

The user's instinct is right, and this review supplies the strongest argument for it:
`getOrderedWithdrawals` **already carries a docstring** claiming it is "sorted by staking
credential for deterministic ordering as required by Cardano's validation rules"
(`:367-371`, `:378-382`) – and the order it actually produces is one **no node emits**. A
paraphrased comment did not prevent the bug; it disguised it. So the convention has to be
stronger than prose:

1. **Every translated collection gets a comment naming the ledger function and line**, e.g.
   `// ledger order: Map.toList (unWithdrawals ...) => Ord AccountAddress = (Network, Script<Key). Conway/TxInfo.hs:549-551`.
   A citation can be checked against the source; "deterministic ordering" cannot.
2. **Default to `unsafeFromList` for delivered-order fields.** If the source collection is
   already in ledger order, re-sorting is at best a no-op and at worst silent corruption.
   `data` is a live example of the harmless case (`fromList` over an already-sorted
   `SortedMap`) that still costs work and still hides intent.
3. **Treat any `fromList` / `.sorted` / `.sortBy` on a delivered collection as a review
   flag**, allowed only with a comment saying why re-ordering is required. Under this rule
   all four remaining ordering defects become visible at the call site: the two
   `sortBy(_._1.toString)` calls have no justification, and the two `fromList` withdrawals
   calls would have to state which order they are producing and why.
4. **Back the load-bearing ones with the ledger-order oracle test from §5.**

Worth noting: **`redeemers` is the one field the translator gets right, and it is already
written in exactly this style** – an explicit `.sorted` whose ordering provably matches the
ledger's, followed by `unsafeFromList` that preserves it. The pattern being asked for
already exists in the file; it just needs to be applied to the other five fields and
documented.


---

# Appendix B. An independent oracle already exists – and is now wired up

**Status: landed** ("add the cardano-ledger golden TxInfo corpus as a translation oracle", plus the three decoder fixes in A.5b). What follows records
both the survey and what happened when it was actually built.

## B.1 The find

`eras/conway/impl/golden/translations.cbor` in cardano-ledger: 5.74 MB, **100 instances**,
each a 5-field record `(ProtVer, SupportedLanguage, UTxO, Tx, VersionedTxInfo)`
(`eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/Translation/TranslationInstance.hs:55-61`).
The last field is **the expected TxInfo produced by the real Haskell ledger**. Generated by
`cabal run cardano-ledger-conway:gen-golden`, fixed seed `100000` (`Golden.hs:37-46`).

**It needs no Haskell toolchain.** The blob hash is `6f5f00fd45e6f79220b40474f1639285fcdbeafe`
at both HEAD (`226b002d5`) and the mainnet tag `cardano-ledger-conway-1.22.1.0` – identical.

Coverage: PV9/PV10/PV11 x PlutusV1/V2/V3, all 9 cells populated; all 20 Conway tx-body keys;
all 3 `Voter` and all 7 `GovernanceAction` constructors. Fixed environment
`epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)`,
`systemStart = 1684445839000` (`TranslatableGen.hs:98-102`), i.e. exactly
`SlotConfig(zeroTime = 1_684_445_839_000_000, zeroSlot = 0, slotLength = 1000)` in Scalus terms.

## B.2 It independently confirms defects 1, 3 and 7

The expected `txInfoWdrl` is stored as an order-preserving assoc list. Across **all 32
PlutusV3 instances, 32/32 with zero counterexamples**, the delivered key order is:

> network block, then `ScriptCredential` before `PubKeyCredential`, then hash ascending.

The same pattern holds for `txInfoVotes` keys. Plutus's own constructor indices are the
opposite (`PubKeyCredential`=0, `ScriptCredential`=1). **This upgrades defect 1 from "I read
the Haskell" to "the Haskell's own output agrees, in bytes."**

## B.2b What happened when it was built

Wired up as `scalus-cardano-ledger/jvm/src/test/.../GoldenTranslationVectors.scala` (loader plus
a small CBOR reader, since borer has no codec for cborg's generic encoding) and
`TxInfoTranslationGoldenTest.scala`. Route (a) below, with a flake-input variant rather than
vendoring: pinned by sha256 as a `fetchurl` in `flake.nix` and symlinked into the repo root by
the dev-shell `shellHook`, exactly like `plutus-conformance`. Zero git bytes, and the hash is
the drift signal. A missing corpus is a hard failure, never a skip.

**First run: 0 of 100 transactions decoded.** That is defect 14 (A.5b), found only because the
oracle forced real ledger bytes through our decoder. The UTxO field decoded 100/100, which is
what proved the slicing was right and the fault was ours. After the three decoder fixes: 83/100.

**The red/green matrix was written down before the first run, and every cell held:**

| Cell | Predicted | Measured | After the fix |
|---|---|---|---|
| V3 `withdrawals` | red (defect 1) | **14 mismatches / 15 compared** | 0 ("order Credential the ledger's way, script before key") |
| V3 `votes` | red (defect 3) | **14 / 17** | 0 ("stop re-sorting voting procedures by toString") |
| V1 `withdrawals` | red (defect 7) | **35 / 39** | 0 ("split withdrawal ordering by what each consumer needs") |
| V3 `data` | green | 0 / 18 | 0 |
| V2 `withdrawals` | green (Plutus order is correct today) | 0 / 19 | 0 throughout |
| V2 `data` | green | 0 / 20 | 0 |

All six now assert equality. The V2 withdrawals column is the load-bearing one for the
V1/V2-versus-V3 split: it stayed green across every commit, so the `Ord[Credential]` flip
demonstrably did not drag V2 along with it.

The green cells matter as much as the red ones: they show the probe is not simply failing
everything. Each red cell asserts the mismatch is still present, so fixing a defect *fails*
its cell and forces an explicit flip to an equality assertion – one reviewable commit per
defect. Two environment self-checks (corpus shape, slot config) run first, so "wrong corpus"
and "wrong environment" die as their own clear errors rather than as 100 ordering mismatches.

Also confirmed here: the withdrawal order is `(Network, Script < Key, hash)`, not simply
`(Script < Key, hash)`. My first check reported 27 "violations" until the network-major
grouping was accounted for. See the note in section 1.

## B.3 What it does NOT cover (measured, not assumed)

| Audit defect | Covered? | Evidence |
|---|---|---|
| `Ord` on withdrawal / vote / credential-keyed maps | **Yes** | 32/32, see B.2 |
| **Redeemer ordering (defect 2)** | **No** | `genRedeemers` emits only `ConwaySpending (AsIx 0)` or empty, deliberately (`conway/TranslatableGen.hs:99-110`). The whole corpus has 18 redeemer entries, all `Spending`. |
| **Pointer addresses (defect 10)** | **No** | 0 pointer addresses and 0 `StakingPtr` nodes in the conway corpus. |
| PV11 input/refInput disjointness | **No** | It is a *failure* path, so such instances are never stored. Measured 0/100. |
| Byron addresses | **No** | 0 in the conway corpus. |
| Pre-Conway interval closure | **No** | Corpus is PV9/10/11 only. (Moot now – that branch was removed.) |

Structural gaps: the file stores **`TxInfo`, never a `ScriptContext`**, so `getScriptInfoV3` is
outside it; and it stores Haskell records, not `Data`, so the `toData` boundary is untested.

**The Alonzo and Babbage corpora are unusable**: 94/100 instances in each contain
genesis-delegation or MIR certificates, which Scalus's Conway-only `Certificate` enum
(`Certificate.scala:12-61`) has no cases for. That is also why Alonzo's 147 pointer-address
instances cannot serve as the pointer oracle.

## B.4 Routes considered

- **(a) The golden corpus – TAKEN** (as a pinned flake `fetchurl`, not vendored; see B.2b). Tx and UTxO are standard Conway
  CBOR that Scalus already decodes. The expected TxInfo uses cborg generic encoding
  (`TranslationInstance.hs:171-177`), whose rules were reverse-engineered and verified:
  constructors as `listLen(1+arity)` then `word conIdx`; Haskell lists as indefinite arrays;
  `Maybe` as `listLen 0`/`listLen 1`; newtypes as `[0, payload]`; `AssocMap` as
  `[0, [[k,v],..]]`, order-preserving. **Tier 1** (orderings only, ~150 lines Scala, 1-3 days)
  kills the whole `Ord` bug class. **Tier 2** (full structural decode, ~40 types) is 1-2 weeks.
- **(b) One-off Haskell emitter – RECOMMENDED, supplement.** Not a CI dependency: run once
  under nix, vendor the output. Emit Plutus `Data` rather than the cborg record – the ledger
  already routes `PlutusV3Args` through `PV3.toData` (`Plutus/Language.hs:517-535`) and
  `toPlutusV3Args` builds the full `ScriptContext` incl. `ScriptInfo` (`Conway/TxInfo.hs:749-761`).
  That closes the `toData` boundary and the generator gaps in one go. ~50-80 lines against the
  public testlibs; the real cost is the nix environment (GHC 9.6.7, CHaP, three
  `source-repository-package` pins, global `-Werror`) – budget a day.
- **(c) Port generators to ScalaCheck – REJECT.** Not on effort, on principle: it yields
  *inputs* with no *oracle*, so you compare Scalus against Scalus. That is exactly the
  structural blindness in §0.
- **(d) Capture from a devnet – DEFER.** Its unique value is that it is the **only** route
  that exercises the *consumption* side, where `@UplcRepr(PackedDataMap)` means node bytes
  reach `get` verbatim. Position it as a follow-on for a few hand-picked cases once the fix
  lands, not as the oracle.

Also portable and cheap: the two value-pinning specs in
`eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/TxInfoSpec.hs` (PV9-vs-PV10 cert
deposits at `:43-79`, validity-interval closure at `:83-99`) have hand-written expected values.

## B.5 Dead ends, recorded so they are not re-explored

- `libs/cardano-ledger-conformance` conforms STS rules to the Agda spec; grep for
  `TxInfo|ScriptContext|PlutusLedgerApi` over it returns **zero hits**.
- `libs/cardano-ledger-test` writes no files; in-process QuickCheck only, no TxInfo contact.
- **The plutus repo has no ScriptContext conformance corpus.** `plutus-conformance/` is UPLC
  evaluation only. `plutus-ledger-api` has no `data-files` and no `Arbitrary` for `TxInfo`,
  `ScriptContext`, `Credential`, `TxCert` or any governance type. Only two consumable
  artifacts: one opaque 1000-byte V1 ScriptContext used for a decode-only smoke test
  (`test/Spec/ContextDecoding.hs:19-33`), and `doc/docusaurus/static/plutus.json`, a CIP-57
  blueprint of the V3 ScriptContext tree – a schema, not vectors, and not CI-guarded.
- Plutus documents **no** ordering guarantee anywhere except `ChangedParameters`
  (`V3/Contexts.hs:302`). `V2/Contexts.hs:113-120` explains there is deliberately no `Eq` for
  `TxInfo` because ordering makes equality ambiguous. The ordering truth lives only in
  cardano-ledger, which is why the golden corpus matters.

## B.6 Risks

- **Decodability.** The corpus is QuickCheck output, not realistic transactions: mint
  quantities span nearly the full Int64 range, all 20 body keys can be populated at once,
  arbitrary `ProtocolParamUpdate` inside gov actions. Mitigation: a per-instance skip list
  with an asserted minimum-decoded threshold, so silent coverage erosion fails the build.
  Each decode failure is itself a finding. **Not yet attempted in Scala.**
- **Corpus drift.** Regeneration is deterministic for a fixed seed but depends on `Arbitrary`
  instances across several packages; there is no `cabal.project.freeze` and `gen-golden`
  appears in no CI workflow. Vendor a pinned copy; treat an upstream change as a re-diff signal.
- **What no route catches:** the on-chain consumption path (only (d) touches it), and whether
  `MajorProtocolVersion` is threaded correctly at *call sites* rather than inside the translator.

---

# Appendix C. The decoder-versus-CDDL sweep

Defect 14 surfaced three decoders that were stricter than the chain, all found *by accident*
because the golden corpus would not decode. This is the deliberate version of that search.
**Its negative results are the point**: they are what stops someone re-running the same pass.

## C.1 Method

Two systematic passes, not opportunistic grepping:

1. Every size/length `require` in `scalus.cardano.ledger` and `scalus.cardano.address`,
   cross-checked against the Conway CDDL's `.size` rules.
2. Every `set<>` / `nonempty_set<>` / `oset<>` field in the CDDL, checked for tag-aware
   decoding - the shape that produced defect 14b. Note `set<a0> = #6.258([* a0]) / [* a0]`,
   so the 258 tag is **optional** and a decoder must accept both forms.

## C.2 One new defect: too lax, not too strict

| # | Sev | Location | Defect |
|---|---|---|---|
| **15** | low – **FIXED** ("bound urls and DNS names by UTF-8 bytes, not characters") | `Anchor.scala`, `PoolMetadata.scala`, `Relay.scala` | `url` and `dns_name` are bounded by **UTF-8 byte length**: `textSizeN` uses `lengthWord8` and is documented as "text with byte-length bounds" (`BaseTypes.hs:643-657`), and the CDDL says `text .size (0 .. 128)` (`conway.cddl:489`, `:496`). We checked `String.length`, which counts UTF-16 units and is never larger, so we **accepted values the chain rejects**. |

This is the **opposite direction** to defect 14: too lax rather than too strict, so the risk is
building a transaction the chain refuses rather than failing to read a valid one. (The bound is
128 bytes from decoder version 9; earlier versions used 64, which is moot now that we support
protocol version 10 and above.)

## C.3 Negative results - checked and clean

**Every remaining size constraint matches the CDDL exactly.** No further too-strict cases:

| Scalus check | CDDL rule |
|---|---|
| `VKeyWitness` vkey 32, signature 64 | `vkey = bytes .size 32`, `signature = bytes .size 64` |
| `BootstrapWitness` publicKey 32, signature 64 | same two rules |
| `OperationalCert` hotVKey 32, sigma 64 | `kes_vkey = bytes .size 32`, `signature` |
| `VrfCert` proof 80 | `vrf_cert = [bytes, bytes .size 80]` |
| `BlockHeaderBody` vrfVkey 32 | `vrf_vkey = bytes .size 32` |
| `Hashes` 28 / 32, `AddrKeyHash` 28 | `hash28`, `hash32` |
| `AssetName` <= 32 | `asset_name = bytes .size (0 .. 32)` |
| `GovActionId` index 0..65535 | `gov_action_index : uint .size 2` |
| `ShelleyAddress` payload 28, Byron `addrRoot` 28 | `hash28` |

**Every CDDL set field decodes tag-aware.** 14b was the only one:

| CDDL field | Scalus |
|---|---|
| `inputs`, `collateral`, `reference_inputs`, `required_signers` | `TaggedSortedSet` (tag-aware decoder) |
| `certificates`, `proposal_procedures` (`nonempty_oset`) | `TaggedOrderedStrictSet` / `TaggedOrderedSet` |
| `pool_owners : set<addr_keyhash>` | plain `Set`, but read through a tag-aware `readSet` (`Certificate.scala:512-519`) |
| `plutus_v1/v2/v3_script` sets | `TaggedSortedStrictMap` |
| `update_committee`'s `set<committee_cold_credential>` | **was** a plain `Set` decoder – defect 14b, fixed |

## C.4 Noted, not changed

- **Validation placement is inconsistent.** `Anchor` and `PoolMetadata` validate in the
  case-class body, so direct construction is checked; `Relay` validates **only in its decoder**
  (`Relay.scala:124`, `:131`), so `Relay.SingleHostName(...)` built in code skips the check.
  That is why defect 15's test reaches `Relay` through CBOR. Worth unifying, but it is a
  code-hygiene question rather than a divergence from the ledger.
- **Integer widths are laxer than the CDDL** in several places (`uint .size 2` fields held as
  `Int`, `uint` held as `Long`). This accepts more than the chain does, but never rejects a
  valid transaction, and the `OverLong` failures left in the golden corpus (16 of the 17
  undecodable instances) are values no real transaction can hold. Not pursued.
- `Ordering[Anchor]` (`Anchor.scala:38`) became unreferenced when defect 9's orderings were
  deleted, but it compares all fields and is antisymmetric, so it is dead code rather than a
  hazard. Left in place.

## C.5 What the sweep would still miss

It compares *declared* constraints against the CDDL. It does not cover decoders that are wrong
in structure rather than in bounds - a field read in the wrong order, an optional field treated
as required, a tag accepted where the ledger demands one. The golden corpus is the check for
that class, and it currently decodes 83 of 100; raising that number is the way to find more.
