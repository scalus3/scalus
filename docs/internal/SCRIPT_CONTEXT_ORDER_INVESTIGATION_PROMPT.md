# Prompt: script-context map key ordering in Scalus

Paste everything below the line into a fresh session. It is self-contained.

Scalus references were verified at commit `da213326d`; Haskell references at
`cardano-ledger` `bbf00fc` and `plutus` `ac40fae4de` (`1.63.0.0`), the checkouts named below. If any
of those have moved, re-verify before trusting a line number.

---

You are investigating a correctness problem in Scalus (a Scala 3 -> UPLC compiler for Cardano) and
proposing how to fix it. A previous session attempted a fix, got it wrong, and the work was deleted.
Nothing is in the tree now. Your job is to do the analysis properly and propose an approach - not to
rush to a patch.

**Read this whole brief before touching anything.** The prior session's failure mode was acting on a
partial mechanism before understanding the whole shape, four times in a row.

## Repository and sources

**Start by creating a fresh worktree off the latest `master`.** Do not work in
`.claude/worktrees/stdlib-api-research` - that branch carries an unrelated stdlib-API design effort,
and the deleted first attempt at this fix. From the primary checkout
`/Users/nau/projects/lantr/scalus`:

```bash
git fetch origin
# then create a worktree off origin/master, e.g. via your harness's worktree tool,
# or: git worktree add .claude/worktrees/<name> -b <branch> origin/master
```

One setup step is needed inside any fresh Scalus worktree, or `scalusJVM/Test/compile` fails on a
missing conformance corpus:

```bash
ln -s "$(readlink /Users/nau/projects/lantr/scalus/plutus-conformance)" <worktree>/plutus-conformance
```

Authoritative Haskell - **read it, never guess**:

- `cardano-ledger`: `/Users/nau/projects/lantr/cardano-ledger` (at `bbf00fc`). All Haskell line
  numbers in this brief refer to **this** checkout. A second copy exists at
  `/Users/nau/projects/iohk/cardano-ledger` at a different commit, so its line numbers differ - if
  you use it, re-verify.
- `plutus`: `/Users/nau/projects/iohk/plutus` (at `ac40fae4de`, release `1.63.0.0`). Contains
  `plutus-ledger-api/` and `plutus-tx/`. No cloning needed.

## The problem

`TxInfo.withdrawals`, `.redeemers` and `.votes` are decoded into Scalus's `SortedMap`. Two facts
about that type combine badly:

- `SortedMap.get` short-circuits: it returns `None` as soon as it meets a key that sorts *after* the
  one being looked up (`scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/prelude/SortedMap.scala:633`).
- `sortedMapFromData` does **not** re-sort; it trusts the incoming order
  (`.../prelude/SortedMap.scala:296`).

So `Ord` must agree with the key order the ledger actually delivers. Where it does not, `get`
silently returns `None` for a key that is present - a validator reads "no such redeemer" and fails,
or worse, takes a wrong branch.

## What is already established

Verified by reading cardano-ledger and by execution. Re-verify anything you intend to rely on; do
not treat this section as authority.

**1. V3 delivers the ledger's key order, not Plutus's.** Keys are converted *after* `Map.toList` has
fixed the order, so the ledger's own `Ord` survives into the script context:

```haskell
-- eras/conway/impl/src/Cardano/Ledger/Conway/TxInfo.hs:697-699
transMap :: (t1 -> k) -> (t2 -> v) -> Map.Map t1 t2 -> PV3.Map k v
transMap transKey transValue =
  PV3.unsafeFromList . map (\(k, v) -> (transKey k, transValue v)) . Map.toList
```

The trap: `cardano-ledger`'s `Credential` and `plutus-ledger-api`'s `Credential` are **different
types with opposite constructor order**, and the ledger's is the one that reaches the script.

```haskell
-- cardano-ledger: libs/cardano-ledger-core/src/Cardano/Ledger/Credential.hs:98-101
data Credential (kr :: KeyRole)
  = ScriptHashObj !ScriptHash
  | KeyHashObj !(KeyHash kr)
  deriving (Show, Eq, Generic, Ord)

-- plutus: plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs:75
PlutusTx.makeIsDataSchemaIndexed ''Credential [('PubKeyCredential, 0), ('ScriptCredential, 1)]
```

**2. The fields differ in kind. This is the central insight and the prior session missed it.**

- `withdrawals`, `data`, `votes` are keyed by **content**, and the ledger derives their order from
  that same content. A content-based `Ord` can track them.
- `redeemers` is keyed **positionally**. The ledger's map is
  `Map (PlutusPurpose AsIx era) (Data era, ExUnits)`
  (`eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxWits.hs:145`), and

  ```haskell
  -- eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Scripts.hs:281-283
  newtype AsIx ix it = AsIx {unAsIx :: ix}
    deriving stock (Show)
    deriving newtype (Eq, Ord, NFData, NoThunks, EncCBOR, DecCBOR, Generic)
  ```

  keeps **only** the `Word32` index - the `TxIn` / `PolicyID` / `AccountAddress` is a phantom type
  parameter. So the order is `(constructor, index)`, and **no content-based `Ord` can be correct for
  it in general.**

  V3 partly escapes because `Certifying(idx, cert)` and `Proposing(idx, _)` carry the index in the
  Plutus key. V1/V2 have no such escape: `v1.ScriptPurpose.Certifying(cert)` is content-only, while
  certificates are a submitter-ordered `IndexedSeq` (`TaggedOrderedStrictSet`, an opaque
  `IndexedSeq`). The submitter picks the order freely, so no total content order can track it.

**3. Withdrawals changed between V2 and V3; redeemers never did.** V1/V2 build a withdrawals map
*already keyed by the Plutus type*, so `Map.toList` yields Plutus order:

```haskell
-- eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/TxInfo.hs:301-305
transWithdrawals :: Withdrawals -> Map.Map PV1.StakingCredential Integer
transWithdrawals (Withdrawals mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans accountAddress (Coin n) =
      Map.insert (PV1.StakingHash (transAccountAddress accountAddress)) n ans

-- V2 reuses it: eras/babbage/impl/src/Cardano/Ledger/Babbage/TxInfo.hs:390
, PV2.txInfoWdrl = PV2.unsafeFromList $ Alonzo.transTxBodyWithdrawals txBody
```

V3 instead uses the ledger-keyed `transMap` form (`Conway/TxInfo.hs:514`, `:549`). But V1/V2
*redeemers* already iterate a ledger-keyed map (`Babbage/TxInfo.hs:222-226`), and
`AlonzoPlutusPurpose` (`Alonzo/Scripts.hs:332-337`) has the same relative constructor order as
`ConwayPlutusPurpose` (`Conway/Scripts.hs:202-209`).

**4. Real Plutus and Aiken are immune.** `PlutusTx.AssocMap.lookup` is a linear `==` scan with no
ordering assumption, and `unsafeFromList` is a pure wrap that preserves list order:

```haskell
-- plutus: plutus-tx/src/PlutusTx/AssocMap.hs:240-248
lookup :: forall k v. Eq k => k -> Map k v -> Maybe v
lookup c (Map xs) =
  let go :: [(k, v)] -> Maybe v
      go [] = Nothing
      go ((c', i) : xs') = if c' == c then Just i else go xs'
   in go xs

-- :224-225
unsafeFromList :: [(k, v)] -> Map k v
unsafeFromList = Map
```

Aiken likewise types these fields as `Pairs`, not `Dict`. The exposure is specific to Scalus's
short-circuiting `SortedMap.get`.

**5. Upstream declined to specify the ordering at all.**
[IntersectMBO/plutus#5726](https://github.com/IntersectMBO/plutus/issues/5726) (Aiken's author, Jan
2024, 44 comments): *"the ordering of `ScriptPurpose` is not well-defined and left as an
implementation detail"* and, asked why scripts would care, *"Scripts don't generally; but compiler
makers and library builders do."* So any fix that pins `Ord` to the ledger's constructor order is
depending on a promise nobody made.

## Known defects, all present in the tree right now

| # | Severity | Where | What |
|---|---|---|---|
| 1 | high | `v1/Contexts.scala:487` `given Ord[Credential]` | Sorts PubKey before Script, following the Plutus tags. V3 delivers Script first, so `withdrawals.get(pubKeyCred)` misses a present key when a script credential also withdrew. |
| 2 | high | `v1/Contexts.scala` and `v3/Contexts.scala`, both `given Ord[ScriptPurpose]` | Order `Minting < Spending < Rewarding < Certifying`. The ledger delivers `Spending < Minting < Certifying < Withdrawing(=Rewarding) < Voting < Proposing`. `redeemers.get(Minting(...))` misses a present key in any tx that also spends a script input - the common "spend and mint" shape. |
| 3 | high | `LedgerToPlutusTranslation.scala:990` and `:993` | `getVotingProcedures` re-sorts by `_._1.toString`. `vp.procedures` is **already** a correctly ordered `SortedMap` using `Ordering[Voter]` (`Voter.scala:94`, script before key). The `toString` sort destroys that: `"...HotKey" < "...HotScript"`, and inner gov-action index 10 sorts before 2. Independent of the `Ord` question - wrong on its own terms. |
| 4 | high | `LedgerToPlutusTranslation.scala:834` | `getScriptPurposeV3`'s `RedeemerTag.Voting` case uses the same `toString` sort to resolve a redeemer index by position, so it can name the **wrong voter**. |
| 5 | medium | `bloxbean-cardano-client-lib/.../Interop.scala:40` | `Ordering[StakingCredential.StakingHash]` orders by raw hash bytes and ignores the constructor, producing an order no node emits. Live: used for a `TreeMap` and for V3 withdrawals. Also skews `Reward` redeemer index lookups. |
| 6 | medium (off-chain only) | `v1/Contexts.scala:314,319,324` and `:747,751` | `Eq[DCert]` and v1 `Eq[ScriptPurpose]` shadow their outer pattern binders with the inner ones, so every field comparison is a self-comparison and both return `true` for any two values sharing a constructor. On-chain `===` lowers to structural comparison and never calls the body, so this is JVM/off-chain only. Confirmed by execution. |

## Why the existing test suite is green anyway

Understand this before proposing tests, because it is the reason the bugs survived.

- **The dominant real-world lookup happens to work.** Withdraw-zero forwarding (`StakeValidator.spend`)
  looks up `Rewarding`, and the buggy `Ord` ranks `Spending` and `Minting` *below* it, so the scan
  walks past them and succeeds. It only breaks alongside a certifying redeemer, which is rare.
- **`LedgerRulesValidationTest` validates 1000 real mainnet blocks** with full script evaluation
  (`PlutusScriptsTransactionMutator` is in `DefaultValidators.scala:47`) and passes. It bounds the
  blast radius; it does not refute the mechanism.
- **Every test builds its maps with `SortedMap.singleton` / `fromList` / `empty`**, all of which
  sort by `Ord`, so the map is self-consistent and no test ever reads a map built the way the
  translator builds one.
- **The bundled conformance vectors contain zero multi-entry maps** across 175 transactions, so any
  sweep over them witnesses nothing about ordering.
- **`ScriptContextComparisonTest` asserts nothing** - it is a `println` debugging tool.

## What the previous attempt did, and why it was deleted

It set `Ord[Credential]` to ledger order, `Ord[StakingCredential]` to Plutus order (on the theory
that V1/V2 and V3 key withdrawals by different types, so each can carry its own order), reordered
both `Ord[ScriptPurpose]` instances, and split the translator's withdrawals helpers.

Two things killed it:

1. **The premise was one distinction short.** Three orders are needed across two types, not two. V2
   `withdrawals` needs Plutus order, but `v1.ScriptPurpose.Rewarding` needs *ledger* order for the
   V1/V2 redeemer map - and both route through `Ord[StakingCredential]`.
2. **It introduced defect 3 above.** The `toString` sort was *accidentally* consistent with the old
   PubKey-first `Ord[Credential]`; flipping `Ord` broke votes.

And even setting those aside, it could not have been right: defect 2's V1/V2 `Certifying` case is
structurally unfixable by any `Ord`.

## Your task

1. **Verify the ground truth yourself.** Build a per-version, per-field table of the delivered key
   order - V1, V2, V3 x `withdrawals`, `redeemers`, `votes`, `data`, and anything else map-shaped
   you find. Mark every cell READ (you read the Haskell) or INFERRED. Include the *index source* for
   each redeemer purpose: what does `redeemerPointerInverse` resolve an index against, and does that
   container's order match Scalus's content-based `Ord` for that constructor? Check `Spending`,
   `Minting`, `Certifying`, `Rewarding`/`Withdrawing`, `Voting`, `Proposing` separately.

2. **Sweep for defects of the same class.** Every `given Ord[...]` and `given Eq[...]` under
   `scalus-core/shared/src/main/scala/scalus/cardano/onchain/plutus/**`, plus the `Ordering[...]`
   instances in `scalus-core/.../cardano/ledger/**` and `bloxbean-cardano-client-lib/**`. A regex
   found defect 6; look for what a regex misses - wrong constructor order, asymmetric comparisons,
   missing cases, `Order.Equal` where a field comparison belongs, `Ord` disagreeing with its own
   `Eq`, non-total orders.

3. **Check the serialisation blast radius.** `SortedMap.toData` emits in `Ord` order. Find every
   place a `SortedMap` keyed by these types is constructed off-chain and serialised into a
   datum/redeemer, or compared against canonical CBOR. Changing `Ord` changes those bytes.

4. **Propose an approach**, with the trade-offs stated honestly. At least these two, plus anything
   better you see:
   - **A: pin `Ord` to the ledger's constructor order.** Small, no API break. But it encodes an
     ordering upstream refuses to specify, it has already changed once (withdrawals, V2 -> V3), and
     it cannot fix V1/V2 `Certifying` at all.
   - **B: association-list fields with linear `Eq` lookup** for the positionally-keyed fields, as
     `PlutusTx.AssocMap` and Aiken's `Pairs` do. Correct by construction, immune to future
     reordering. Costs O(n) instead of an early exit, and is a public `TxInfo` API break.

   A hybrid is plausible - content-keyed fields keep `SortedMap` with a corrected `Ord`, while
   `redeemers` becomes an assoc list. If you propose it, say exactly which fields go which way and
   why. Give a migration sketch and a cost estimate for whatever you recommend, and note which
   defects each option does and does not fix.

5. **Design the test that would have caught all of this.** The suggestion on the table is a property
   test asserting every field of `getTxInfoV2` / `getTxInfoV3` is ascending under its `Ord` for
   arbitrary transactions, plus `map.get(k)` defined for every delivered key - the second assertion
   matters, since ascending-ness alone passes a map `unsafeFromList` never built. Evaluate that,
   improve it, and say what it would still miss.

## Rules

- Every load-bearing claim quotes real source with `file:line`. Distinguish sharply between what you
  READ and what you INFERRED. If a link in the chain is unverified, say so rather than bridging it
  with plausible reasoning.
- **Proving a mechanism is not proving an impact.** The prior session claimed a confirmed bug from a
  `SortedMap` property in isolation, then over-retracted when shown a passing suite. A green suite
  bounds the blast radius; it does not refute a mechanism.
- You may write and run scratch tests - preferred, in fact. Put them in your scratch dir, or under
  `scalus-core/shared/src/test/scala/scalus/` if they must compile in-repo, and **delete them when
  done**. Do not modify main source. Do not commit.
- Build: `sbtn -Dsbt.supershell=false -Dsbt.log.noformat=true "scalusJVM/testOnly ..."`. Redirect
  output to a file - piping sbt through `head` wedges the client. Never run two sbt commands at once.
- Work in your own fresh worktree off `origin/master` (see Repository and sources). Do not touch the
  `stdlib-api-research` branch.
- This is an analysis-and-proposal task. Do not land a fix.

## Deliverable

A written proposal covering: the per-version per-field table (1), defects found with severity and
`file:line` (2), serialisation findings (3), your recommended approach with migration and cost (4),
and the test design (5). Where you could not determine something, say so explicitly and state what
would settle it.
