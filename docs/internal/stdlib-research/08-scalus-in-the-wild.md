# Scalus in the wild: what contract authors outside the scalus repo hand-roll

Research input for the high-level "smart contract standard library" API design.
Companion to `01-scalus-examples.md` (in-repo examples) and `02-scalus-existing-api.md`.

**Method.** 30 local checkouts under `/Users/nau/projects/lantr/` were screened with
`grep -rl --include='*.scala' -E '@Compile|extends Validator|ScriptContext|TxInfo|scalus.ledger.api|scalus.cardano.onchain|scalus.prelude'`.
Every file that turned out to hold real on-chain code was then read **in full**
(~5 900 LOC of on-chain Scala across 6 independent projects + 3 Scalus-team templates).
Line numbers are verified against the working trees listed in §1.1.

Counts in the ranked table (§4) are **per project, not per checkout**: `hydrozoa`/`hydrozoa2`
are two checkouts of one repo (the newer, `hydrozoa`, is cited); `vela` and `stable/vela` are two
generations of one project (both cited, counted once); `cosmex/.claude/worktrees/*` is excluded.

---

## 1. The corpus

### 1.1 Projects **with** Scalus on-chain code (6 independent + 3 templates)

| Project | Checkout @ commit | On-chain files (LOC) | Scalus API vintage |
|---|---|---|---|
| **binocular** (Bitcoin↔Cardano bridge oracle) | `binocular` @ `480046a` 2026-08-26 | `oracle/BitcoinValidator.scala` (1446), `watchtower/TreasuryMovementValidator.scala` (683), `watchtower/TransactionVerifierValidator.scala` (200), `watchtower/PegOutVerifier.scala` (198), `bitcoin/BitcoinHelpers.scala` (671) | current (`scalus.cardano.onchain.plutus.*`) |
| **vela** (vUSD CDP stablecoin) | `vela` @ `d5c6fcc` 2026-08-03 **and** `stable/vela` @ `16aac47` (older gen, more validators) | `onchain/CdpValidator.scala` (410 / 755), `onchain/StabilityPoolValidator.scala` (517), `onchain/BondingCurveValidator.scala` (234/242), `onchain/OracleValidator.scala` (143), `onchain/StabilityPoolNftPolicy.scala` (48), `onchain/StrictLookups.scala` (68/47), `onchain/InterestMath.scala`, `onchain/VelaMath.scala` | current |
| **hydrozoa** (rule-based L2 head) | `hydrozoa` @ `d1029f9cf` 2026-08-24 (newer than `hydrozoa2` @ `d5456e42`) | `…/plutus/DisputeResolutionScript.scala` (671), `…/plutus/RuleBasedTreasuryScript.scala` (550), `…/state/{TreasuryState,VoteState,RegimeState}.scala`, `lib/cardano/scalus/ledger/api/{ValueExtensions,TxOutExtensions,ByteStringExtensions}.scala`, `lib/cardano/scalus/Scalar.scala` | current |
| **cosmex** (payment-channel DEX) | `cosmex` @ `46c26f6` 2026-01-07 | `cosmex/CosmexValidator.scala` (1203), `test/…/demo/SimpleMintingPolicy.scala` (40) | ~0.11 (`scalus.ledger.api.v3`, `scalus.prelude`) |
| **adastream** (file-bond + HTLC) | `adastream` @ `a7c822c` 2025-12-18 | `src/contract.scala` (246) | ~0.10 (`scalus.builtin.Data.field`, raw `BuiltinList`) |
| **proofspace-cardano-trust-registry** | `proofspace-…` @ `4e3d936` 2025-03-05 | `onchain/{MintingPolicyElements,SindleMaintainer,SubmtiWithCostMaintainerApprove,UsingVotingTokens}.scala`, `common/PreludeListData.scala` | very old (`Maybe`, `AssocMap.lookup`, `throw new Exception`) |
| *scalus-starter* (template) | `8b2a87e` 2026-08-22 | `starter/MintingPolicy.scala` (124) | current |
| *hello.g8* / *validator.g8* (templates) | — | `HelloCardano.scala` (29), `$name$.scala` (52) | current |

Templates are Scalus-team-authored: they are **weak** evidence of "users re-implement by hand"
and are footnoted rather than counted in §4.

### 1.2 Projects screened and skipped (one line each)

| Repo | Verdict |
|---|---|
| `UPLC-CAPE` | Haskell + Nix benchmark harness — no Scala. |
| `amaru-treasury`, `bodega-market-smart-contracts` | Aiken (`.ak` + `plutus.json`) — no Scala. |
| `sugar-rush-ledger` | Rust workspace — no Scala. |
| `sc-fvt` | Lean/Haskell formal-verification of Plutus — no Scala. |
| `bitvestor`, `cip113-programmable-tokens` | `src/` present but zero `.scala` files. |
| `binocular-outpoints` | only a stale `target/` dir. |
| `cosmex-scalus` | only an empty `project/` dir (abandoned split-out). |
| `treasury`, `treasury-contracts`, `treasury-proposal` | notebooks / docs / Aiken — no Scala. |
| `validator` | empty. |
| `midgard-poc` | Scala, but cats-effect "Hello Cats" only — no ledger code. |
| `midgard-mvp` | Scala off-chain services; no `@Compile`, no `ScriptContext`. |
| `scalus-cape-submissions` | `@Compile` on pure arithmetic (`factorial`, `fibonacci`) — no tx context. |
| `optspend` | a 2023-era Scalus **0.6.1** scratch file whose whole "validator" is `def list_has(list) = throw new Exception("withdrawal not found")` (`contract.scala:12-13`). Dead sketch; one datum-free finding: it was already reaching for a *withdrawal-credential lookup* helper. |
| `scalus-treasury` | Scalus **off-chain only** — `m5-withdraw/m5-withdraw.scala` builds an unsigned mainnet tx with `TxBuilder`; `treasury-publish/ContractData.scala` has no `@Compile`/`Validator`/`ScriptContext`. The on-chain vendor script is Aiken (fetched by hash from Blockfrost). |
| `shared-wallets` | Scalus is used only to **type the datums/redeemers and apply params** to an Aiken blueprint (`SharedWallets.scala:14-35`, `:41-60`). No `@Compile`. Still contributes one category-5 data point (§3.5). |

---

## 2. Per-project findings

### 2.1 hydrozoa — the closest thing to a user-written stdlib

Hydrozoa physically created a package named after the Scalus API it wished existed:
`hydrozoa/lib/cardano/scalus/ledger/api/`.

**Locally defined helpers (category 1)**

| Helper | Signature | Purpose | Site |
|---|---|---|---|
| `containsCurrencySymbol` | `extension (self: Value) def containsCurrencySymbol(cs: PolicyId): Boolean` | any token of a policy present | `ValueExtensions.scala:17-21` |
| `containsExactlyOneAsset` | `def containsExactlyOneAsset(cs, tn, amount): Boolean` | value holds *exactly* one non-ADA asset, right name, right qty, **nothing else** | `ValueExtensions.scala:29-46` |
| `onlyNonAdaAsset` | `def onlyNonAdaAsset: (PolicyId, TokenName, BigInt)` | destructure the single-NFT value or fail | `ValueExtensions.scala:51-71` |
| `unary_-` on `Value` | `def unary_- : Value = Value.zero - self` | negate a value for burn comparisons | `ValueExtensions.scala:74` |
| `inlineDatumOfType[T]` | `extension (self: TxOut) inline def inlineDatumOfType[T](using FromData[T]): T` | inline-datum decode or fail | `TxOutExtensions.scala:20-22` |
| `ByteStringExtension` | `<`,`<=`,`>`,`>=`,`at`,`take`,`slice`,`drop` on `ByteString` | ordering + slicing sugar over builtins | `ByteStringExtensions.scala:6-33` |
| `Scalar` | full BLS12-381 scalar field type (`+ * / neg`, `fromByteStringBigEndian…`) | KZG accumulator arithmetic | `lib/cardano/scalus/Scalar.scala:14-…` |
| `findRegimeReference` | `def findRegimeReference(tx: TxInfo, headMp: PolicyId): RuleBasedRegimeDatum` | locate + decode an NFT-authenticated reference input | `RuleBasedTreasuryScript.scala:145-160` |
| `maxVote` | `def maxVote(a: VoteStatus, b: VoteStatus): VoteStatus` | domain fold | `DisputeResolutionScript.scala:635-650` |
| `resolve` / `evacuate` | `extension (self: Unresolved) def resolve(...): Resolved` etc. | **single-source the legal datum transition**, used by both on-chain check and off-chain builder | `state/TreasuryState.scala:47-70` |

The `resolve`/`evacuate` pattern is worth lifting verbatim into stdlib guidance — its scaladoc
states the intent: *"single-sourcing which fields advance and which are carried forward immutably.
Validators build the expected next datum from these and check the output against it; off-chain
builders construct the next datum the same way."*

**Recurring inline idioms (category 2)**

- Own-input lookup, **4×** with a bare `.get`, **3×** with `getOrFail`, each labelled *"TODO: factor out"*:
  ```scala
  // RuleBasedTreasuryScript.scala:208-211  (also :283-286, :442-445)
  // TODO: factor out
  val treasuryInput = tx.inputs
      .find(_.outRef === ownRef)
      .getOrFail("Impossible happened: own input was not found")
      .resolved
  ```
  ```scala
  // DisputeResolutionScript.scala:170 (also :405, :553, :591)
  val voteInput = tx.inputs.find(_.outRef === ownRef).get.resolved
  ```
- "Exactly one continuing output" via `filter` + `Cons/require(tail.isEmpty)` — **3×**:
  ```scala
  // DisputeResolutionScript.scala:343-347
  val voteOutput = tx.outputs.filter(o => o.value === voteInput.value) match
      case List.Cons(o, tail) =>
          require(tail.isEmpty, VoteVoteOutputExists)
          o
      case _ => fail(VoteVoteOutputExists)
  ```
  ```scala
  // RuleBasedTreasuryScript.scala:218-223
  val treasuryOutput = tx.outputs
      .filter(e => e.address === treasuryInput.address) match
      case List.Cons(o, tail) =>
          require(tail.isEmpty, ResolveTreasuryOutputFailure)
          o
      case _ => fail(ResolveTreasuryOutputFailure)
  ```
- **Beacon-token reference-input lookup** (CIP-67 prefixed name under a policy) hand-written **4×**
  with an identical nested `toSortedMap.get(policy).getOrElse(SortedMap.empty).toList.find(...)` walk:
  `DisputeResolutionScript.scala:214-227`, `:468-481`, `:558-572`, `RuleBasedTreasuryScript.scala:146-157`.
- Value preservation as a full-`Value` equation:
  ```scala
  // RuleBasedTreasuryScript.scala:421-427
  val evacuatedValue = evacuationOutputs.foldLeft(Value.zero)((acc, o) => acc + o.value)
  val valueIsPreserved = treasuryInput.value === (treasuryOutput.value + evacuatedValue)
  ```
- Deadline checks written out as an `IntervalBoundType.Finite` match, **2×** (`DisputeResolutionScript.scala:261-265`, `:498-505`):
  ```scala
  tx.validRange.to.boundType match {
      case IntervalBoundType.Finite(toTime) =>
          require(toTime <= treasuryDatum.deadlineVoting, VoteTimeValidityCheck)
      case _ => fail(VoteTimeValidityCheck)
  }
  ```
- **No-reference-script-on-continuing-output** check, **3×** (`:353-355`, `:530-532`, `:621-623`) —
  a safety idiom absent from every other repo.
- "No other input carries this policy" exclusion, **3×** (`:178-186`, `:448-456`, `:593-601`).

**Workarounds (category 3)**

- `VoteState.scala:61-65` — explicit `given` instead of `derives`, because the linker cannot
  resolve companion-derived SIR:
  > *"Explicit givens (rather than `derives` clauses on the types) so the derived instances are
  > direct members of this `@Compile` object and their SIR is emitted for on-chain use … A
  > clause-derived instance lands in the type's companion, whose SIR the on-chain linker cannot
  > resolve (fails at script-build time, not Scala compile time)."*
- `DisputeResolutionScript.scala:276-289` — *"Temporary workaround"* + a hand-written `@tailrec`
  `verifySignatures(a, b)` because there is no `List.zipAll`/`forall2`-style multisig helper.
- `DisputeResolutionScript.scala:296-300` — *"TODO (Scalus team): the `List[Option[Signature]]`
  encoding bloats wire size when many coil peers abstain. A sparser encoding like
  `List[(CoilPeerId, Signature)]` … Worth evaluating once cost benchmarks are in place."*
- `RuleBasedTreasuryScript.scala:475-479` — *"TODO: comparing as bytestrings is more efficient,
  we want to have this constant in Scalus"*, then a 96-hex-char literal of the BLS12-381 G1
  identity inline in the validator.
- `DisputeResolutionScript.scala:404`, `:418`, `:424` — *"TODO: hide `ownInput` and `otherInput`
  so they can't be used accidentally"*, *"TODO: make a helper"* (×2, for the
  continuing/removed input swap).
- `RuleBasedTreasuryScript.scala:45` — *"TODO: inline these fields into TreasuryRedeemer to avoid
  extra data deconstruction?"*

**Pitfalls (category 4)**

- Bare `.get` on own-input lookup in 4 places (`DisputeResolutionScript.scala:170, 405, 553, 591`)
  — an unfindable own input aborts with the prelude's generic message, not a domain one.
- Positional output layout asserted by an `@unchecked` destructure:
  ```scala
  // RuleBasedTreasuryScript.scala:305-309
  //   - The change utxo is position zero
  //   - the treasury utxo in position one
  //   - the tail be evacuatees
  val List.Cons(_, List.Cons(treasuryOutput, evacuationOutputs)) = tx.outputs: @unchecked
  ```
  Any extra output shifts the whole interpretation; only the follow-up address/beacon/value checks
  keep it safe. The author's own comment at `:313-318` records that the address pin was *added
  later* because *"without this an Evacuate could redirect the beacon and the entire treasury value
  to an arbitrary address"*.
- `DisputeResolutionScript.scala:614-616` uses `find` (first match) rather than the file's own
  "exactly one" idiom for the Abstain continuing output — inconsistent with `:343-347`.

**Performance workarounds (category 5)**

- `EvacuateRedeemer.setupRefInputIdx` (`RuleBasedTreasuryScript.scala:46-53`) — reference-input
  **index passed in the redeemer**, then authenticated: `tx.referenceInputs !! setupRefInputIdx`
  (`:365`) followed by an outRef check against the regime datum (`:367-371`).
- `RuleBasedTreasuryScript.scala:420` — *"TODO: combine with iterating for poly calculation up above?"*
  (two folds over `evacuationOutputs`).
- `:193`, `:216` — *"TODO: pass vote input's outRef in the redeemer?"*, *"TODO: pass output index in redeemer?"*

### 2.2 vela — the "StrictLookups" project

**Locally defined helpers (category 1)** — `onchain/StrictLookups.scala`, imported by *every* validator:

```scala
// vela/src/main/scala/vela/onchain/StrictLookups.scala:16-46
extension [A](self: List[A]) {
    @tailrec
    def findOrFail(predicate: A => Boolean): A = self match
        case List.Nil => fail("element not found")
        case List.Cons(head, tail) =>
            if predicate(head) then head else tail.findOrFail(predicate)

    def oneOrFail(message: String): A = self match
        case List.Cons(head, List.Nil) => head
        case _                         => fail(message)
}

extension [V](self: Value) {
    def existingQuantityOf(policyId: PolicyId, tokenName: TokenName): BigInt =
        self.toSortedMap.lookupOrFail(policyId).lookupOrFail(tokenName)
}
extension [V](self: SortedMap[ByteString, V]) {
    def lookupOrFail(key: ByteString): V = {
        @tailrec
        def go(lst: PairList[ByteString, V]): V = lst match
            case PairNil => fail("key not found")
            case PairCons((k, v), tail) =>
                if key == k then v
                else if key < k then fail("key not found")   // sorted short-circuit
                else go(tail)
        go(self.toPairList)
    }
}
```

The **newer** checkout adds a second, byte-for-byte identical copy for `SortedMap[BigInt, V]`
(`vela/…/StrictLookups.scala:57-70`) because the extension is not `Ord`-generic. Scalus already
ships `SortedMap.getOrFail` / `.at` (`prelude/SortedMap.scala:659`, `:680`) — this is a
**discoverability/API-shape** finding, not a missing API: the author wanted (a) a sorted
short-circuit and (b) a `Value`-level `existingQuantityOf`.

`oneOrFail` is used **~30×** across the two checkouts (13 in `stable/vela/CdpValidator.scala`
alone) — the single most-used user-written helper in the whole corpus.

Other local helpers:
`getInlineCdpDatum` / `getInlineSpState` / `getInlineDepositDatum`
(`stable/vela/CdpValidator.scala:126-128`, `StabilityPoolValidator.scala:91-97`,
`vela/CdpValidator.scala:108-110`) — the same 3-line inline-datum decode written **5×**;
`getContinuingCdp` (`stable/vela/CdpValidator.scala:173-187`, dup in `vela/CdpValidator.scala:115-129`);
`getPriceOracleData` / `validateTimeAndOracle` (`stable/vela/CdpValidator.scala:131-168`);
`getOraclePrice` (`vela/CdpValidator.scala:72-91`);
`isAboveMCR` / `collateralRatioAtLeast` (cross-multiplied ratio, no division);
`InterestMath.accruedInterest`.

**Recurring inline idioms (category 2)**

- Own script hash from own input — **6×**:
  ```scala
  // stable/vela/CdpValidator.scala:208-210 (also StabilityPoolValidator.scala:112,
  // BondingCurveValidator.scala:66/68, OracleValidator.scala:80, vela/CdpValidator.scala:150)
  val ownInput = tx.findOwnInputOrFail(ownRef)
  val ScriptCredential(scriptHash) = ownInput.resolved.address.credential: @unchecked
  val cred = ScriptCredential(scriptHash)
  ```
  …and once from the reward purpose: `val ScriptCredential(scriptHash) = stakingKey: @unchecked`
  (`stable/vela/CdpValidator.scala:627`).
- "The unique NFT in this UTxO" — **3×**:
  ```scala
  // stable/vela/CdpValidator.scala:213-222
  val inputTokens = ownInput.resolved.value.tokens(scriptHash)
  require(inputTokens.size === BigInt(1), "Input must contain exactly one CDP NFT")
  val nftName = inputTokens.toList.head._1
  require(inputTokens.toList.head._2 === BigInt(1), "Input must contain exactly one CDP NFT")
  ```
- "The single output holding NFT (policy, name)" — **6×**, e.g.
  ```scala
  // stable/vela/CdpValidator.scala:336-341
  val output = tx.outputs
      .filter { out =>
          out.address.credential === cred &&
          out.value.quantityOf(scriptHash, nftName) === BigInt(1)
      }
      .oneOrFail("CollectInterest must leave exactly one matching CDP output")
  ```
- **Expected-datum equality through `toData`** — **9×**:
  `require(newDatum.toData === expectedDatum.toData, "Invalid CDP datum update")`
  (`stable/vela/CdpValidator.scala:241, 264, 282, 350, 674`;
  `StabilityPoolValidator.scala:205, 260, 355, 417`).
- **Cross-purpose redeemer check** (`mint` reads the `spend` redeemer of the same input) — **11×**:
  ```scala
  // stable/vela/CdpValidator.scala:474-480
  val spendAction = tx.redeemers
      .get(ScriptPurpose.Spending(cdpInput.outRef))
      .getOrFail("Missing CDP spend redeemer")
      .to[CdpAction]
  spendAction match
      case CdpAction.AdjustDebt => ()
      case _                    => fail("Mint and spend actions must match")
  ```
- **Payout-to-address sum**:
  ```scala
  // stable/vela/CdpValidator.scala:311-317 (identical shape at vela/CdpValidator.scala:226-232)
  require(
    tx.outputs
        .filter(_.address.credential === PubKeyCredential(cdp.owner))
        .foldLeft(BigInt(0))((sum, output) => sum + output.value.getLovelace) >=
        ownInput.resolved.value.getLovelace,
    "All collateral ADA must be returned to owner"
  )
  ```
- **Co-spend / token-presence-in-inputs** — **5×**:
  `tx.inputs.exists { input => input.resolved.value.quantityOf(policy, name) === BigInt(1) }`
  (`stable/vela/CdpValidator.scala:366-372`, `StabilityPoolValidator.scala:445-450`, `:487-492`, `:505-510`).
- **One-shot mint** — **4×**: `tx.inputs.exists(_.outRef === scriptParams.oneShot)`
  (`stable/vela/CdpValidator.scala:745`, `BondingCurveValidator.scala:167`,
  `vela/OracleValidator.scala:127`), and once as
  `tx.inputs.findOrFail(_.outRef.toData == oneShotTxOutRef)` (`StabilityPoolNftPolicy.scala:45`).
- **Whole-value equality against a constructed `Value`**, the strictest form seen anywhere:
  ```scala
  // stable/vela/BondingCurveValidator.scala:192-201
  val expectedTreasuryValue = Value.unsafeFromList(
    List(Value.adaPolicyId -> List(Value.adaTokenName -> treasuryAda),
         policyId          -> List(NFT_NAME -> BigInt(1))))
  require(output.value.toData === expectedTreasuryValue.toData, "NFT missing from bootstrap output")
  ```

**Pitfalls (category 4)**

- Address **credential-only** comparison is the house style
  (`out.address.credential === cred`, ~15 sites) — the **staking part is unconstrained**, so a
  continuing output may silently change its delegation. Only the Bootstrap branch pins the whole
  address: `require(output.address === Address(ScriptCredential(policyId), None), …)`
  (`stable/vela/BondingCurveValidator.scala:176-180`).
- `stable/vela/CdpValidator.scala:379-382`: `require(tx.mint.quantityOf(scriptHash, VUSD_TOKEN_NAME) <= -effectiveDebt, "Effective debt vUSD must be burned")`
  — `<=` where the sibling `Close` branch (`:298-301`) uses `===`.
- `stable/vela/CdpValidator.scala:336-341` (`CollectInterest`) locates the continuing output by
  `filter(...).oneOrFail` over **all** outputs, whereas `AdjustCollateral` uses
  `findOwnOutputsByCredential`. Two different "own output" notions in one validator.
- `stable/vela/StabilityPoolValidator.scala:305`
  `filter { _.resolved.address.credential.toData == cdpCred.toData }` — `==`/`toData` comparison
  in one branch while every neighbouring branch uses `===`.
- `vela/CdpValidator.scala:234-256` (`Liquidate`) comments *"The collateral is released to the
  liquidator (enforced off-chain by the tx builder)"* — no on-chain constraint on where the
  collateral goes.

**Performance workarounds (category 5)**

- `pow516Qt` **cached in the datum** and re-verified on every transition
  (`stable/vela/BondingCurveValidator.scala:21`, `:113-117`) — recompute-once, carry-forward.
- `stable/vela/CdpValidator.scala:161-167` — the oracle reference input is found by NFT scan, then
  `PriceOracleData.oracleExpiry` is **repurposed to carry `txStart`** to avoid a second tuple
  allocation: *"oracleExpiry is repurposed to carry txStart after validation"*.
- `StrictLookups.lookupOrFail` short-circuits on sorted order (`else if key < k then fail`).
- `StabilityPoolNftPolicy.scala:42-46` uses `existingQuantityOf` (fail-fast) instead of
  `quantityOf` (returns 0) to avoid a full map walk on the happy path.

### 2.3 binocular — heaviest on-chain compute, most defensive style

**Locally defined helpers (category 1)**

| Helper | Signature | Site |
|---|---|---|
| `of[A]` | `extension (d: OutputDatum) inline def of[A: FromData]: A` | `TreasuryMovementValidator.scala:229-233` |
| `findOracleInput` | `(refInputs: List[TxInInfo], oracleScriptHash: ByteString): TxOut` | `TreasuryMovementValidator.scala:359-371` **and** an independent second copy in `TransactionVerifierValidator.scala:86-105` |
| `getOracleState` | `(oracleOutput: TxOut): ChainState` | `TransactionVerifierValidator.scala:108-112` |
| `tmInputCount` | `(inputs, tmScriptHash): BigInt` — "exactly one own-script input" | `TreasuryMovementValidator.scala:385-388` |
| `committedRoots` | `(outs): (ByteString, ByteString)` — "exactly one match, else fail" | `TreasuryMovementValidator.scala:303-314` |
| `reverse` | `extension (a: ByteString) def reverse: ByteString` | `BitcoinValidator.scala:23` |
| `insertAscending` / `insertionSort` | `(x, sorted: List[BigInt])` | `BitcoinValidator.scala:430-439` |
| `BitcoinHelpers` (671 LOC) | varint reading, tx/witness walking, merkle proof, compact-bits | `bitcoin/BitcoinHelpers.scala` |

`of[A]`'s scaladoc is a direct statement of a Scalus limitation:
> *"`inline` so the `FromData[A]` derivation expands at the call site — a non-inline generic would
> reference the companion's `derived$FromData` module, which is not `@Compile`d for
> externally-defined types like `ConfigDatum` and `ChainState`."*

**Recurring inline idioms (category 2)**

- Own input + own script hash:
  ```scala
  // BitcoinValidator.scala:1245-1248
  val ownInput = inputs.find(_.outRef === outRef).getOrFail("Input not found").resolved
  val Credential.ScriptCredential(policyId) = ownInput.address.credential: @unchecked
  ```
  ```scala
  // TreasuryMovementValidator.scala:412-415
  val ownOut = tx.findOwnInput(ownRef).get.resolved
  val tmScriptHash = ownOut.address.credential match
      case Credential.ScriptCredential(h) => h
      case _                              => fail("TM input is not at a script address")
  ```
- **Continuing output = same address + NFT**, written twice verbatim
  (`BitcoinValidator.scala:1268-1273` and `:1335-1340`):
  ```scala
  val continuingOutput = outputs
      .find(out =>
          out.address.toData == ownInput.address.toData
              && out.value.quantityOf(policyId, ByteString.empty) == BigInt(1)
      )
      .getOrFail("No continuing output with oracle NFT found")
  ```
  Note the **full address** comparison — staking part *is* pinned here, unlike vela.
- **Value preservation split into "tokens exact / ADA monotone"**, twice
  (`BitcoinValidator.scala:1276-1285`, `:1341-1348`):
  ```scala
  require(ownInput.value.withoutLovelace.toData == continuingOutput.value.withoutLovelace.toData,
          "Non-ADA tokens must be preserved")
  require(continuingOutput.value.lovelaceAmount >= ownInput.value.lovelaceAmount,
          "ADA value can only increase")
  ```
- **Whole-datum equality** rather than field-wise, with the rationale spelled out:
  ```scala
  // TreasuryMovementValidator.scala:515-533
  // spec [CTM-27] rebuild the WHOLE expected datum and compare the whole OutputDatum. On-chain
  // FromData is an erased retag (no tag or arity check), so field-wise reads would also accept
  // `Constr 5 [root, junk]` at the singleton address …
  val exp = OutputDatum.OutputDatum(BridgeState(...).toData)
  require(exp === bssOut.datum, "TM confirm: singleton datum is not the attested state")
  ```
- **Constr-tag pinning**, because `FromData` does not check it:
  ```scala
  // TreasuryMovementValidator.scala:576-579
  require(unConstrData(rawDatum).fst == BigInt(0),
          "TM mint: NFT output datum is not an UnconfirmedTm record")
  ```
- **NFT-authenticated reference input** — 3×
  (`TreasuryMovementValidator.scala:470-476`, `:482-488`, `:599-604`):
  ```scala
  val cfgOut = tx.referenceInputs
      .find(refIn => refIn.resolved.value.quantityOf(configNftPolicy, configNftName) == BigInt(1))
      .getOrFail("TM confirm: no config reference input")
      .resolved
  ```
- **Exactly-one-token mint check** via pattern match on the flattened map:
  ```scala
  // TreasuryMovementValidator.scala:635-649
  tx.mint.tokens(ownPolicyId).toList match
      case ScalusList.Cons((nft, amount), ScalusList.Nil) if nft == ByteString.empty => …
      case _ => fail("Only singe TM NFT is allowed")
  ```
  and via `SortedMap.singleton` comparison:
  `require(tx.mint.tokens(policyId).toData == SortedMap.singleton(ByteString.empty, BigInt(-1)).toData, "Must burn oracle NFT")` (`BitcoinValidator.scala:1304-1309`, `:1410-1434`).
- **Deadline**: `require(tx.validRange.isEntirelyAfter(timeout), "TM GC: grace period has not elapsed")`
  (`TreasuryMovementValidator.scala:544-547`) — the only repo using `Interval.isEntirelyAfter`.
- **Validity-window width bound**, 2 repos:
  `require(intervalEndMs - intervalStartMs <= MaxValidityWindow, "Validity interval too wide")`
  (`BitcoinValidator.scala:1211`; vela at `stable/vela/CdpValidator.scala:153`).

**Workarounds (category 3)**

- `TransactionVerifierValidator.scala:164-175` still hand-decodes the script context by index:
  ```scala
  val sc = unConstrData(scData).snd
  val txInfoData = sc.head
  val redeemer   = sc.tail.head
  val scriptInfo = unConstrData(sc.tail.tail.head)
  if scriptInfo.fst == BigInt(1) then …
  ```
  while `TreasuryMovementValidator.scala:652-658` documents that this was *"a workaround from
  before Scalus V3 lowering made `to`/`toData` no-ops on the structural script-context types"* —
  i.e. one file was migrated and its sibling was not.
- `findOracleInput` exists twice, in two files, with **different semantics**: the TM copy also
  requires the oracle NFT (`:366-367`), the TransactionVerifier copy matches the script hash only
  (`:99-101`) — a security-relevant divergence caused by copy-paste rather than a shared helper.
- `PegOutVerifier.scala:56-59` documents deliberately *not* reusing
  `allInputOutpoints` + `allOutputs`: *"those would walk the input region twice … `scanTm` streams
  over the bytes, short-circuits, and allocates nothing."*

**Pitfalls (category 4)**

- `TreasuryMovementValidator.scala:359-371` — `findOracleInput` ends in a bare `.get`:
  ```scala
  refInputs.find { … }.get.resolved
  ```
  (same file uses `getOrFail` with messages everywhere else); likewise `:412` `tx.findOwnInput(ownRef).get`
  and `:561-563` `tx.outputs.find(…).get`.
- The author explicitly documents the `find`-vs-`filter` trap at `:304-306`:
  > *"`filter` then match, NOT `find`: `find` stops at the first commitment and would silently
  > accept a TM carrying a second one."*
  This is the single clearest in-corpus argument for a stdlib `uniqueOrFail`.
- `BitcoinValidator.scala:495-498`: `if !params.testingMode then require(hashInt <= target, …)` —
  a **parameter-controlled bypass of PoW validation** compiled into the same script.

**Performance workarounds (category 5)**

- `BlockSummary.addedTimeDelta` stores `currentTime - timestamp` *"(saves CBOR bytes vs absolute time)"*
  (`BitcoinValidator.scala:55`).
- `existsAsChild` deliberately inspects only branch heads — *"O(k) where k is the number of
  branches, not the total number of blocks"* (`BitcoinValidator.scala:619-631`).
- Chainwork is stored per `Blocks` node and only recomputed on split/partial promotion
  (`computeChainwork` scaladoc, `:529-539`).
- `insertionSort` over an 11-element list instead of a general sort (`:435-439`).
- The mint redeemer **is** an output index: `val outputIndex = redeemer.to[BigInt]; val oracleOutput = tx.outputs.at(outputIndex)` (`BitcoinValidator.scala:1417-1418`), then authenticated by NFT+address.
- `TmMintRedeemer(bridgeStateRefInputIndex: BigInt)` (`TreasuryMovementValidator.scala:177`) —
  reference-input index in the redeemer, then authenticated by NFT (`:606-610`).

### 2.4 cosmex — 1 203 LOC single-file state machine

**Locally defined helpers (category 1)**

| Helper | Signature | Site |
|---|---|---|
| `findOwnInputAndIndex` | `(inputs: List[TxInInfo], spendingTxOutRef: TxOutRef): (TxInInfo, BigInt)` | `CosmexValidator.scala:148-159` |
| `findOwnInputAndIndex` (**second copy**) | `(i: BigInt, txIns: List[TxInInfo]): (TxOut, BigInt)` — nested inside `cosmexSpending` | `CosmexValidator.scala:973-977` |
| `tryFindOwnOutput` | `(outputs, expectedAddress, clientTxOutRef): Option[TxOut]` — match own output **by datum field** | `:176-199` |
| `findOwnOutputOrByIndex` | `(outputs, expectedAddress, clientTxOutRef, fallbackIndex): TxOut` | `:206-217` |
| `expectNewState` | `(ownOutput, ownInputAddress, newState, newValue): Boolean` — datum + address + value in one | `:219-249` |
| `txSignedBy` | `(signatories: List[PubKeyHash], k: PubKeyHash): Boolean` | `:251-252` |
| `minValue` | `(a: Value, b: Value): Value` — element-wise min | `:258-267` |
| `assetClassValue` | `(assetClass: (PolicyId, TokenName), i: BigInt): Value` | `:1055-1056` |
| `lockedInOrders` | `(orders): Value` — fold of order collateral | `:330-343` |
| `validRange` | `(interval: Interval): (PosixTime, PosixTime)` — both bounds finite or fail | `:1176-1185` |
| `abs` | `(x: BigInt): BigInt` | `:1160` |

`expectNewState` is the corpus' clearest "continuing output" abstraction:

```scala
// CosmexValidator.scala:242-248
val expectedNewDatum = datum === expectedDatum
val sameAddress      = address === ownInputAddress
val preserveValue    = value === newValue
expectedNewDatum.? && sameAddress.? && preserveValue.?
```

**Recurring idioms (category 2)** — signature check via a hand-rolled `txSignedBy`
(6 call sites: `:280`, `:281`, `:318`, `:388`, `:389`, `:644`); cross-input redeemer read
(`:697-701`); own-output-by-datum-field lookup (`:176-199`); interval both-bounds destructure
(`:1176-1185`, called 4×).

**Pitfalls (category 4)**

- Two `findOwnInputAndIndex` definitions in one file with **different return types**
  (`:148` returns `TxInInfo`, `:973` returns `TxOut`); only the nested one is actually used
  (`:981`) — the top-level one is dead but exported.
- Own-output resolution silently falls back to positional indexing:
  ```scala
  // CosmexValidator.scala:212-217
  tryFindOwnOutput(outputs, expectedAddress, clientTxOutRef) match
      case Option.Some(txOut) => txOut
      case Option.None        => outputs !! fallbackIndex   // index-based fallback
  ```
- Payout branch checks only that the value is **non-zero**, with a comment admitting it:
  ```scala
  // CosmexValidator.scala:760-767
  // For full payout, verify client receives non-zero value
  // Exact amount verification is done by client before signing
  val valueNonZero = txOutValue.isPositive
  if hashMatch && valueNonZero then true
  else fail("Invalid payout: client should receive all funds")
  ```
  The sibling exchange branch *does* check `txOutValue === ownInputValue` (`:776`).
- `:504-505` `val isNewerSnapshot = oldVersion <= newSignedSnapshot.signedSnapshot.snapshotVersion`
  — non-strict `<=` on a contest "must be newer" rule.
- `:227-241` computes `blake2b_256(serialiseData(...))` hashes of actual vs expected datum purely
  to `trace` whether they match — the result (`hashMatch`) is **never used in the verdict**; only
  `datum === expectedDatum` is. Pure on-chain cost for debug output.
- ~20 `trace("…")(())` calls left in the validator body (`:228`, `:238`, `:413-441`, `:534-582`, …).

**Performance workarounds (category 5)**

- `Transfer(txOutIndex: TxOutIndex, value: Value)` (`:32`) — the **output index travels in the
  redeemer**, and other inputs' redeemers are read to sum transfers targeting it (`:692-713`).
- `PendingTxType.PendingOut(txOutIndex)` / `PendingTransfer(txOutIndex)` (`:53-56`) — same trick in
  the datum.
- `PRICE_SCALE` fixed-point integer arithmetic instead of rationals (`:146`).

### 2.5 adastream

Helpers: `verifyPreimage` (`contract.scala:109-110`), `verifyMerkleInclusionProof`
(`:88-107`, walks a raw `BuiltinList[Data]`), `verifyFraudProof` (`:112-148`),
`customXor`/`xorBytes` (`:57-80`, superseded by the builtin at `:83-85` but kept),
`integerToByteString` with a positivity guard (`:53-55`), `extension (a: Array[Byte]) def toHex` (`:11`).

**Pitfalls (category 4)** — this is the richest small file in the corpus:

- Signatory checked **by position**, twice:
  ```scala
  // contract.scala:176-179
  // get PubKeyHash as a ByteString from the first signatory
  // NOTE: we assume that the first signatory is the server
  ctxData.field[ScriptContext](_.txInfo.signatories).toList
  ```
  ```scala
  // contract.scala:196
  val verifySignature = signatures.head.toByteString == serverPubKeyHash
  ```
  ```scala
  // contract.scala:239-241
  val signaturePubKeyHashData = txInfoData.field[TxInfo](_.signatories).toList.head
  signaturePubKeyHashData == ownerPubKeyHash
  ```
  `tx.signatories` is ledger-sorted; "first signatory" is not the server.
- Duplicated conjunct:
  ```scala
  // contract.scala:242
  require(expired && expired, "HTLC is not expired")
  ```
- `bondContractValidator` (`:161-182`) validates **only** when `scriptInfo` is a spending script and
  silently succeeds otherwise (`if infoPair.fst == BigInt(1) then …` with no `else`).
- `throw new Exception("Number must be positive")` / `throw new Exception("X")` inside `@Compile`
  code (`:54`, `:79`).

### 2.6 proofspace-cardano-trust-registry

**Locally defined helpers (category 1)**

| Helper | Signature | Site |
|---|---|---|
| `verifyPkh` | `(pkh: PubKeyHash)(ctx: ScriptContext)` = `List.findOrFail(signatories)(_ === pkh)` | `MintingPolicyElements.scala:20-22` |
| `retrieveDatum` | `(txOut: TxOut, txInfo: TxInfo): Datum` — inline **or datum-hash → `txInfo.data` lookup** | `MintingPolicyElements.scala:27-35` |
| `filterMintedOutputs` | `(ctx, registryName, checkOps, checkOtherOut): List[TxOut]` | `MintingPolicyElements.scala:37-77` |
| `operationsWithPayment` | `(txOut, parsedDatum, changeCost): Boolean` — lovelace floor on an output | `SubmtiWithCostMaintainerApprove.scala:50-62` |
| `PreludeListData.{listToData,listFromData,builtinToPrelude}` | `List[T] <-> Data` | `common/PreludeListData.scala:10-31` |

`retrieveDatum` is the **only** datum-hash-resolving helper in the corpus and has no Scalus
equivalent beyond `TxInfo.findOwnDatum`:

```scala
// MintingPolicyElements.scala:27-35
def retrieveDatum(txOut: TxOut, txInfo: TxInfo): Datum = {
    txOut.datum match
      case OutputDatum.NoOutputDatum => throw new Exception("No datum in the output")
      case OutputDatum.OutputDatum(d) => d
      case OutputDatum.OutputDatumHash(datumHash) =>
        AssocMap.lookup(txInfo.data)(datumHash) match
          case Maybe.Just(d) => d
          case _ => throw new Exception("Unknown datum hash in the output")
}
```

**Recurring idioms (category 2)** — a nested `AssocMap.lookup(value)(policy)` +
`AssocMap.lookup(byNames)(name)` "quantity of" walk written out **5×**
(`UsingVotingTokens.scala:31-37`, `:42-53`, `:58-69`; `MintingPolicyElements.scala:47-50`;
`SubmtiWithCostMaintainerApprove.scala:41-46`, `:51-60`) — this repo predates `Value.quantityOf`.

**Pitfalls (category 4)**

- `UsingVotingTokens.scala:57-72` computes the "change cost" from **inputs** at
  `targetCredential`, not from outputs, so nothing is actually *paid*:
  ```scala
  val changeCostInputs = scalus.prelude.List.foldLeft(ctx.txInfo.inputs, BigInt(0)) { (acc, txIn) =>
    AssocMap.lookup(txIn.resolved.value)(ByteString.empty) match … }
  if (changeCostInputs < changeCostAda) then
    throw new Exception("Not enough ADA to propose the change")
  ```
- `UsingVotingTokens.scala:31-37` throws from **inside a `filter` predicate**
  (`throw new Exception("Minted outputs with the other name as expected")`), so an unrelated
  matching input aborts the whole script.
- `SindleMaintainer.scala:29-32` defines `verifyDatum` and never calls it; `:48` passes
  `(txOut, parsedDatum, ops) => true` as the check function — the "operations are valid" rule is
  a no-op.
- `val unused = List.findOrFail(...)` appears **4×** (`:36`, `:46`, `SubmtiWith…:38`, `:39`) —
  fail-fast-for-effect with a discarded binding, because there is no `require`-style helper.

### 2.7 Templates (Scalus-team authored — footnote only)

`scalus-starter/MintingPolicy.scala:81-97` is the canonical "exactly one token of my policy" +
deadline + signature shape:

```scala
val mintedTokens = tx.mint.tokens(ownPolicyId)
mintedTokens.toList match
    case Cons((tokName, amount), Nil) =>
        require(tokName == tokenName, "Token name not found")
        if amount > BigInt(0) then
            val to = tx.validRange.to.finiteOrFail("Must have finite upper bound")
            require(to <= mintDeadline, "Minting deadline passed")
    case _ => fail("Multiple tokens found")
require(tx.isSignedBy(adminPubKeyHash), "Not signed by admin")
```

`hello.g8/HelloCardano.scala:24-27` is datum-decode + `isSignedBy` only.
`validator.g8` is a six-purpose `fail(...)` scaffold.

`cosmex/.../demo/SimpleMintingPolicy.scala:32-38` hand-writes the one-shot check field-by-field
(`input.outRef.id.hash == utxoRef.id.hash && input.outRef.idx == utxoRef.idx`) rather than
`===`, and the same file documents a **compiler pitfall** at `:69-101`
(applying params to an already-optimised program yields bad DeBruijn indices).

---

## 3. Cross-cutting observations

### 3.1 The five things every non-trivial validator does first

Every one of the 6 projects opens `spend` with some subset of:
own input → own script hash/credential → own NFT name → datum decode → oracle/config reference input.
Only step 1 has a stdlib API (`findOwnInputOrFail`). Steps 2–5 are hand-written everywhere.

### 3.2 Address vs credential vs full address — three incompatible house styles

| Style | Staking part | Repos |
|---|---|---|
| `out.address === in.address` | preserved (any change rejected) | hydrozoa (`RuleBasedTreasuryScript.scala:315-318`, `DisputeResolutionScript.scala:349`), binocular (`BitcoinValidator.scala:1270`, `TreasuryMovementValidator.scala:510-513`), cosmex (`CosmexValidator.scala:243`) |
| `out.address.credential === cred` | **unconstrained** | vela (~15 sites), proofspace (`SubmtiWithCostMaintainerApprove.scala:28`) |
| `out.address === Address(cred, None)` | forced to *no* staking part | vela Bootstrap (`stable/vela/BondingCurveValidator.scala:176-180`), binocular mint (`BitcoinValidator.scala:1425-1429`) |

A stdlib that offers only `findOwnOutputsByCredential` nudges users toward the middle row.

### 3.3 Datum comparison: whole-datum equality has won, but for two different reasons

`toData`-level equality of the *entire* expected datum is used by vela (9×), binocular (2×),
cosmex (1×) and hydrozoa's `resolve`/`evacuate` transitions. Binocular states the security reason
(`TreasuryMovementValidator.scala:515-521`: `FromData` is an erased retag, so field-wise reads
accept wrong-arity/wrong-tag `Constr`s); hydrozoa states the maintenance reason (single-source the
legal transition). Only binocular additionally pins the Constr tag (`:576-579`).

### 3.4 Fail-fast lookups are re-invented in every repo

`oneOrFail` (vela, ~30×) ≈ `filter … case Cons(o, tail) => require(tail.isEmpty …)` (hydrozoa, 3×)
≈ `case ScalusList.Cons(only, ScalusList.Nil) => … case _ => fail(…)` (binocular, 2×)
≈ `mintedTokens.toList match { case Cons((n,a), Nil) => … case _ => fail }` (scalus-starter).
Four spellings of one operation, none of them in the stdlib.

### 3.5 Index-in-redeemer is the dominant performance workaround (5 projects)

| Project | Field | Authenticated afterwards by |
|---|---|---|
| binocular | `TmMintRedeemer(bridgeStateRefInputIndex)` (`TreasuryMovementValidator.scala:177`) | singleton NFT (`:606-610`) |
| binocular | mint redeemer *is* an output index (`BitcoinValidator.scala:1417-1418`) | NFT + address (`:1419-1429`) |
| hydrozoa | `EvacuateRedeemer.setupRefInputIdx` (`RuleBasedTreasuryScript.scala:52`) | outRef vs regime datum (`:367-371`) |
| cosmex | `Transfer(txOutIndex, value)` (`CosmexValidator.scala:32`) | script-hash check on the input (`:695-707`) |
| shared-wallets | `UpdateAdminConfigDatum(outputConfigIdx)`, `SpendAsAdminGroup(referenceConfigIdx)`, `RegisterSharedWallet(outputConfigIdx)`, … (`SharedWallets.scala:22-38`) | (Aiken side) |

The recurring shape is **"index for lookup, token for authentication"**. A stdlib helper that
bundles the two (`tx.outputAt(i).requiringToken(policy, name)`) would remove the class of bugs
where an author uses the index and forgets the authentication.

### 3.6 Compiler-shaped workarounds worth fixing at the source

- companion-derived `FromData`/`ToData` not linkable → hydrozoa `VoteState.scala:61-65`;
  binocular `TreasuryMovementValidator.scala:225-233` (`inline def of[A]`).
- pre-V3-lowering hand-decoded `ScriptContext` still shipping in
  `TransactionVerifierValidator.scala:164-175` and `adastream/contract.scala:161-182`.
- `Compile`d code containing `throw new Exception` (proofspace ×8, adastream ×2) — no
  `require`-with-message idiom existed at that vintage.

---

## 4. Ranked table — helper/idiom → projects that hand-roll it → proposed API

"Projects" counts the 6 independent projects only (adastream `A`, binocular `B`, cosmex `C`,
hydrozoa `H`, proofspace `P`, vela `V`). Templates are noted with `†`.

| # | Helper / idiom | Projects | Sites | Proposed high-level API |
|---|---|---|---|---|
| 1 | "exactly one element / else fail" on a filtered list | 4 — B,H,V,†starter | ~40 | `list.uniqueOrFail(inline message)` ; `list.findUnique(p): Option[A]` + `list.findUniqueOrFail(p)(inline message)` |
| 2 | Inline-datum decode `OutputDatum.OutputDatum(d) => d.to[T]` else fail | 5 — B,C,H,P,V | 20 | `txOut.inlineDatumOrFail[T](inline message)` ; `txOut.findInlineDatum[T]: Option[T]` (plus `txOut.datumOrFail[T]` resolving hashes via `tx.data`, see #12) |
| 3 | Own script hash / credential from own input (`: @unchecked` destructure) | 3 — B,H,V | 10 | `tx.ownScriptHashOrFail(ownRef): ScriptHash` ; `tx.ownCredential(ownRef): Credential` |
| 4 | "The single continuing output at my address holding NFT (p,n)" | 5 — B,C,H,V,†starter | 15 | `tx.findOwnContinuingOutput(ownRef): Option[TxOut]` + `…OrFail`; `tx.findOwnContinuingOutputWithToken(ownRef, policy, name)OrFail` |
| 5 | Expected-datum equality via `toData` (+ Constr-tag pinning) | 4 — B,C,H,V | 13 | `txOut.expectInlineDatum[T](expected: T)(inline message)`; `txOut.expectDatumTag(tag)` |
| 6 | Token / NFT quantity lookup that **fails** when absent | 4 — H,P,V (+B via `SortedMap` compare) | 12 | `value.quantityOfOrFail(policy, name)(inline message)` ; `value.requireNft(policy, name)` |
| 7 | Reference / co-spent input authenticated by an NFT | 3 — B,H,V | 12 | `tx.findReferenceInputWithToken(policy, name): Option[TxInInfo]` + `…OrFail`; `tx.findInputWithToken(...)` |
| 8 | Cross-purpose redeemer read (`tx.redeemers.get(Spending(ref))`) | 2 — C,V | 12 | `tx.spendRedeemerOrFail[A](ref)(inline message)`; `tx.requireSpendRedeemer[A](ref)(expected)` |
| 9 | Index-in-redeemer + authenticate-by-token | 5 — B,C,H,V,shared-wallets | 6 | `tx.outputAtWithToken(idx, policy, name)OrFail`; `tx.referenceInputAtWithToken(idx, policy, name)OrFail` |
| 10 | Signature check (`isSignedBy` / hand-rolled / `signatories.head`) | 6 — all (B,V via existing `isSignedBy`; A,C,H,P hand-rolled) | 20 | already `tx.isSignedBy`; add `tx.requireSignedBy(pkh)(inline message)` and `tx.isSignedByAll(list)` / `tx.countSignedBy(list): BigInt` (multisig quorum — hydrozoa hand-rolls this twice) |
| 11 | Deadline / interval bound destructure (`IntervalBoundType.Finite` match) | 5 — A,B,C,H,V (+†starter) | 10 | already `finiteOrFail` / `isEntirelyAfter`; add `tx.requireBefore(t)`, `tx.requireAfter(t)`, `tx.requireValidityWidthAtMost(ms)` (B and V both hand-roll the width bound) |
| 12 | Datum-hash resolution (`OutputDatumHash` → `tx.data`) | 1 — P | 1 | `txOut.datumOrFail[T](tx)(inline message)` — resolves inline **and** hashed |
| 13 | One-shot mint ("the seed UTxO is spent") | 3 — B,V,†cosmex-demo | 5 | `tx.requireSpends(outRef)(inline message)` |
| 14 | Value preservation: tokens exact + ADA monotone | 2 — B,H | 6 | `value.tokensEqual(other): Boolean` ; `txOut.preservesTokensOf(txIn)` ; `txOut.valueEquals(txIn.value + delta)` |
| 15 | "Exactly one asset in this value" / unique NFT name | 2 — H,V | 6 | `value.onlyNonAdaAssetOrFail: (PolicyId, TokenName, BigInt)` ; `value.uniqueTokenOf(policy)OrFail: (TokenName, BigInt)` |
| 16 | Payout-to-address sum (`filter(...).foldLeft(0)(_ + _.value.getLovelace)`) | 2 — V (×2 gens), C (`minValue` variant) | 3 | `tx.lovelacePaidTo(cred): BigInt` ; `tx.valuePaidTo(cred): Value` ; `tx.requirePaidAtLeast(cred, value)` |
| 17 | "No other input carries policy p" / "exactly one own-script input" | 2 — B,H | 5 | `tx.requireSingleOwnScriptInput(ownRef)(inline message)` ; `tx.countInputsWithPolicy(p): BigInt` |
| 18 | Exactly-one-token mint/burn under my policy | 4 — B,H,V,†starter | 8 | `tx.mintedUniqueTokenOrFail(policy): (TokenName, BigInt)` ; `tx.requireMint(policy, name, qty)` ; `tx.requireBurn(policy, name)` |
| 19 | `SortedMap` lookup that fails (sorted short-circuit, `Ord`-generic) | 1 — V (×2 copies) | 4 | exists as `SortedMap.getOrFail` — **discoverability gap**; make it `Ord`-generic + short-circuiting and document it |
| 20 | Whole-`Value` construction for exact comparison | 2 — B,V | 3 | `Value.of((policy, name, qty)*)` + `value === expected` (already possible; needs an ergonomic builder) |
| 21 | ByteString ordering / `take` / `drop` / `slice` sugar | 2 — B,H | many | `ByteString` extensions `<`,`<=`,`>`,`>=`,`take`,`drop`,`slice` in the prelude |
| 22 | Datum-transition constructors (`old.resolve(x): NewDatum`) shared on/off-chain | 1 — H | 2 | not an API — a **documented pattern** for the stdlib guide |
| 23 | No-reference-script-on-continuing-output | 1 — H | 3 | `txOut.requireNoReferenceScript(inline message)` |
| 24 | BLS12-381 scalar field type | 1 — H | 1 file | `scalus.prelude.bls12_381.Scalar` (hydrozoa's `Scalar.scala` is a ready-made donation) |

**Top 8 by combined breadth × site count**: #1 `uniqueOrFail`, #2 `inlineDatumOrFail`,
#4 `findOwnContinuingOutput`, #10 signature helpers, #5 `expectInlineDatum`, #3 `ownScriptHash`,
#7 `findReferenceInputWithToken`, #8 `spendRedeemerOrFail`.

Naming follows the Scalus convention already used by `findOwnInput`/`findOwnInputOrFail`:
`findX: Option[…]` plus `findXOrFail(inline message)`, with a `requireX(...)` form where the
result is only needed for its effect.

---

## 5. Bug/pitfall register (quoted, not fixed)

| # | Project | Site | Quote / issue |
|---|---|---|---|
| 1 | adastream | `contract.scala:242` | `require(expired && expired, "HTLC is not expired")` — duplicated conjunct |
| 2 | adastream | `contract.scala:196`, `:239-241`, `:176-179` | signatory identified as `signatories.head` (*"we assume that the first signatory is the server"*) |
| 3 | adastream | `contract.scala:161-182` | `if infoPair.fst == BigInt(1) then …` with no `else` — non-spend purposes pass |
| 4 | cosmex | `CosmexValidator.scala:760-767` | full payout accepts any `txOutValue.isPositive` (*"Exact amount verification is done by client before signing"*) |
| 5 | cosmex | `:504-505` | `oldVersion <= newSnapshot.snapshotVersion` for a "must be newer" contest rule |
| 6 | cosmex | `:212-217` | own-output lookup falls back to `outputs !! fallbackIndex` |
| 7 | cosmex | `:148` vs `:973` | two `findOwnInputAndIndex` with different return types in one file |
| 8 | cosmex | `:227-241` | `blake2b_256(serialiseData(...))` computed on-chain only to `trace` the result |
| 9 | binocular | `TreasuryMovementValidator.scala:369`, `:412`, `:563` | bare `.get` on `find`/`findOwnInput` in a file that otherwise always uses `getOrFail` |
| 10 | binocular | `TransactionVerifierValidator.scala:99-101` vs `TreasuryMovementValidator.scala:366-367` | two `findOracleInput` copies; only one also checks the oracle NFT |
| 11 | binocular | `BitcoinValidator.scala:495-498` | `if !params.testingMode then require(hashInt <= target …)` — parameter-gated PoW bypass |
| 12 | hydrozoa | `RuleBasedTreasuryScript.scala:305-309` | positional output layout via `@unchecked` destructure |
| 13 | hydrozoa | `DisputeResolutionScript.scala:170, 405, 553, 591` | bare `.get` on own-input lookup (×4) |
| 14 | hydrozoa | `DisputeResolutionScript.scala:614-616` | `find` (first match) for a continuing output where the same file elsewhere requires uniqueness |
| 15 | vela | ~15 sites | continuing output matched on `address.credential` only — staking part unconstrained |
| 16 | vela | `stable/vela/CdpValidator.scala:379-382` | `<=` on the liquidation burn where `Close` (`:298-301`) uses `===` |
| 17 | vela | `vela/CdpValidator.scala:234-256` | liquidation collateral destination *"enforced off-chain by the tx builder"* |
| 18 | vela | `stable/vela/StabilityPoolValidator.scala:305` | `credential.toData == cdpCred.toData` in one branch, `===` in all neighbours |
| 19 | proofspace | `UsingVotingTokens.scala:57-72` | "change cost" summed over **inputs**, so nothing is paid |
| 20 | proofspace | `UsingVotingTokens.scala:31-37` | `throw` inside a `filter` predicate |
| 21 | proofspace | `SindleMaintainer.scala:29-32`, `:48` | `verifyDatum` defined but never called; the ops check is `=> true` |

---

## 6. Take-aways for the API design

1. **The stdlib's biggest single win is `uniqueOrFail`.** Four projects, four spellings, ~40 sites,
   and binocular documents the exact bug it prevents (`TreasuryMovementValidator.scala:304-306`).
2. **Bundle the `spend` preamble.** `ownInput → ownScriptHash → ownNft → datum` is written out in
   full by every project. A single `tx.ownScriptContext(ownRef)`-style accessor would delete
   ~10 lines from the top of every validator.
3. **Make "continuing output" one concept, not three.** The address/credential/`Address(cred, None)`
   split (§3.2) is a live security divergence; whichever the stdlib names `findOwnContinuingOutput`
   becomes the default and should be the *safe* one (full address).
4. **Whole-datum equality needs a first-class API** (`expectInlineDatum[T]`) plus tag pinning —
   users converged on it for security reasons Scalus should encode rather than document.
5. **`SortedMap.getOrFail` already exists and vela wrote it twice anyway.** Discoverability, not
   coverage, was the failure mode; the stdlib needs an index of "the fail-fast form of every lookup".
6. **Fix the linker/derivation papercuts** (§3.6): they force `inline def of[A]` and explicit-given
   workarounds in the two most sophisticated codebases in the corpus.
7. **Adopt hydrozoa's donations verbatim** where possible: `ValueExtensions`, `ByteStringExtensions`,
   `Scalar`, and the `resolve`/`evacuate` datum-transition pattern.
