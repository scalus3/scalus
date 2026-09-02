# Changelog

## Unreleased

### Performance

- `EtaReduce` removes multi-argument wrappers (`\a.\b. f a b` → `f`), not only single-argument
  ones, and now traverses into `Constr` arguments and `Case` scrutinee/branches so wrappers
  nested there reduce like any other subterm. A value-arity analysis guards the rewrite:
  eta-reduction is unsound in call-by-value when the wrapper delays work, so a redex collapses
  only when the head provably accepts that many arguments without performing any

### Changed

- **Streaming is a capability of a provider, not a kind of provider.** `BlockchainStreamProvider`
  is renamed `BlockchainStreaming` and no longer extends `BlockchainProvider`; a streaming view is
  obtained from the provider it observes, `provider.streaming()`, which is cached and performs no
  I/O. Reads and submission stay on the provider the caller already holds. Whether a backend
  streams was already answered by `StreamCapabilities`, per-request and honestly, and the type
  system answered it worse — a static `BlockchainStreamProvider` never told a caller that any
  particular subscription would work, since `StreamingBlockfrostProvider.subscribeBlockQuery`
  throws unconditionally. `BlockchainStreamProviderTF`, `BlockchainStreamReader` and
  `BlockchainStreamReaderTF` are deprecated aliases; the read/write split had nothing to
  distinguish on the streaming side, which has no `submit`. **Migration:** replace
  `new StreamingEmulator(emulator)` with `emulator.streaming()` and
  `StreamingBlockfrostProvider(client)` with `client.streaming()`, and call one-shot reads and
  `submit` on the provider rather than the view
- Subscriptions return `ScalusAsyncSource` instead of a `C[_]` inferred from the expected type.
  The stream-type parameter made a call's return type depend on which adapter its file imported,
  made two imported adapters an ambiguity, and put a higher-kinded `using` parameter on six public
  entry points that Java could not call. fs2 users add `.toStream`, the extension method the
  adapter already shipped; its `Resource` helper never used the type parameter to begin with
- `EmulatorBase.onTransactionApplied` observes every applied transaction. It replaces
  `StreamingEmulator.submit` as the emulator's event source, which fixes a silent defect:
  submitting straight to an emulator that had a streaming wrapper produced no events at all.
  Notifications carry the resolved `spent` set, so block synthesis no longer looks up
  `getAppliedTx` and no longer fails after `clearAppliedTxs()`
- The Blockfrost streaming feed follows demand rather than construction. It previously began
  polling in `StreamingBlockfrostProvider.apply`, costing one request per interval — about 8,640 a
  day against a 50,000/day tier — with nothing subscribed. The first `pull` on any subscription
  starts it, so a set of subscriptions registered together shares one starting position, and the
  last cancellation suspends it. Suspending clears the cursor, so a resumed follower re-reads the
  tip rather than delivering a backlog of the quiet interval to a subscription that asked for
  events from now
- **One streaming view per provider**, so the tuning it used to take moved to where that view is
  configured from: `EmulatorBase.securityParam` and `BlockfrostProvider.pollInterval`, the latter
  reachable through new overloads of `preprod` / `preview` / `mainnet` / `create` and a fifth
  constructor parameter. A second view was never merely redundant — on the emulator two views
  number their own blocks, so one transaction appears at different heights to two sets of
  subscribers over a single ledger and `newEmptyBlock()` on one does not advance the other; on
  Blockfrost it is two poll loops against one quota. Both settings arrived as overloads and a
  secondary constructor rather than defaulted parameters, so every existing arity is preserved and
  the change costs no binary compatibility
- `scalus-design-patterns`: the validator callbacks of `UtxoIndexer`, `StakeValidator` and
  `TransactionLevelMinterValidator` now return `Unit` and are expected to `require` / `fail` with
  their own message, instead of returning `Boolean` and failing with a generic library message. A
  `Boolean` callback could only ever report "validator failed", and a stray `false` from a missed
  branch was silent. The generic `*ValidatorFailed` message constants are gone. The module has no
  MiMa baseline yet, so this is an in-place change. **Migration hazard:** an existing `Boolean`
  lambda still compiles against the `Unit` parameter (value discard) and then checks nothing;
  after upgrading, grep every callback passed to these patterns and wrap its condition in
  `require(..., message)`
- Examples migrated to the safe API (`findInputOrFail`, `hasInlineDatum`, `hasNft`,
  `validFromOrFail` / `validToOrFail`, `valuePaidTo` / `valueSpentFrom`, `onlyBurnsUnder`). Every
  pinned budget moved downward; `HtlcValidator` shrank from 322 to 318 bytes and its CAPE script
  from 582 to 548 bytes, and the CAPE two-party escrow moved from hand-navigated `Data` to the
  typed `Validator` context, shrinking from 1174 to 1079 bytes with every measured budget lower. `EscrowValidator` now compares its continuing output as a whole `Value`,
  so tokens can no longer be stripped from the escrow UTxO, and `VestingValidator`,
  `DecentralizedIdentityValidator` and `OnChainCellOps` fail on an unbounded validity range where
  they used to read it as the Unix epoch
- `TxInfo.redeemers` (Plutus V2 and V3) is an `AssocMap[ScriptPurpose, Redeemer]` instead of a
  `SortedMap`. Redeemer keys are positional: the ledger keys its map by `PlutusPurpose AsIx era`,
  which keeps only the `Word32` index, so no content-based `Ord` can reproduce their order. A
  sorted map's short-circuiting lookup therefore walked past keys that were present and returned
  `None`. `AssocMap` does a linear `Eq` scan, which is what `PlutusTx.AssocMap` and Aiken's
  `Pairs` do for the same field. The on-chain `Data` encoding is unchanged, since both types
  carry `@UplcRepr(PackedDataMap)`. **Migration hazard:** `redeemers.get(purpose)` still
  compiles, so the break is loud only where `SortedMap`-specific members were used; code written
  against the old lookup missing a present key will now find it
- `Ordering[GovAction]` and `Ordering[ProposalProcedure]` are deleted. Both compared only the
  `GovAction` constructor ordinal, so two distinct proposals compared equal and a `SortedSet`
  silently dropped one. That violates the `Ordering` contract rather than merely being a coarse
  order. Neither had a use: `proposalProcedures` is a `TaggedOrderedSet`, which preserves
  submitter order and never sorts
- `CardanoInfo.preprod` is pinned to preprod epoch 310 instead of 303, picking up the parameter
  update enacted at preprod epoch 305: `maxTxExecutionUnits.memory` 16,500,000 -> 17,500,000,
  `maxBlockExecutionUnits.memory` 72,000,000 -> 77,500,000 and `minPoolCost` 170 -> 75 ada. Cost
  models and every other parameter are unchanged. Mainnet (epoch 645) and preview (epoch 1370)
  pins already match their networks; the equivalent mainnet parameter change is still an open
  governance action

### Deprecated

- `TxInfo.findOwnInput`, `findOwnInputOrFail`, `findOwnDatum`, `findOwnScriptOutputs`,
  `findOwnInputsByCredential`, `findOwnOutputsByCredential` are renamed without the `Own`, which
  is a Plutus inheritance that is wrong for every Scalus version (all take an explicit
  argument); `findOwnInputs` / `findOwnOutputs` are `inputs.filter` / `outputs.filter`
- `TxInfo.getValidityStartTime` returned `0` for an unbounded range, so a transaction with no
  lower bound passed every "not before" deadline; use `validFromOrFail(msg)`
- `Utils.getAdaFromOutputs` / `getAdaFromInputs` sum lovelace only and let native tokens be
  stripped; use `TxInfo.valuePaidTo(addr)` / `valueSpentFrom(addr)`
- `List.single` / `PairList.single`; use `singleton`
- Scaladoc warnings on `IntervalBound.finite(default)`, `Address.fromScriptHash` /
  `fromPubKeyHash` (no staking part) and the payment-credential-only output finders

### Fixed

- `Eq[DCert]` and `Eq[ScriptPurpose]` (Plutus V1) compared every field to itself: the inner
  pattern binder shadowed the outer one, so any two values with the same constructor were equal
  off-chain. On-chain the lowering replaces `Eq` with structural `equalsData`, which hid it
- `dischargeCekValEnv` returned `Constr` and `Case` nodes untouched, so a variable inside a
  constructor argument or a case branch kept its unresolved name whenever a CEK value was
  discharged back to a term
- `TermSanitizer` could emit names the reference plutus-core parser rejects as malformed. It
  allowed hyphen-digit anywhere in a name, but the upstream textual grammar treats a `-<digits>`
  suffix as a terminal Unique-id token, so a name carrying both a `-<id>` suffix and a
  `'<counter>` disambiguation suffix (e.g. `a-91533'653`) produced unparseable UPLC. Every `-`
  now maps to `_`
- Script-context collections were ordered by an `Ord` that disagreed with the order the Cardano
  ledger delivers, and `SortedMap.get` short-circuits, so a lookup could walk past a key that was
  present and report `None`. `Ord[Credential]` (Plutus V1) now sorts script credentials before
  key credentials, matching the ledger's derived `Ord (Credential kr)` over
  `ScriptHashObj | KeyHashObj`; `Ordering[RewardAccount]` compares network, credential kind and
  hash rather than the hash alone; V1/V2 withdrawals keep Plutus order while V3 withdrawals and
  the container a `Withdrawing` redeemer index resolves against keep ledger order, which are
  deliberately opposite constructor orders and are now separate helpers; and
  `getVotingProcedures` no longer re-sorts by `toString`, which named the wrong voter for a
  `Voting` redeemer index
- Pointer addresses lost their staking credential in translation. The ledger translates them in
  every Plutus version, so the delegation part is now a `StakingPtr`
- The ledger decoders rejected transactions the node accepts: chain codes were length-checked
  although the CDDL leaves them unconstrained, and urls and DNS names were bounded by characters
  where `text .size (0..128)` counts UTF-8 bytes
- `ChangedParameters.toData` did not match the ledger's encoding of a parameter-change
  governance action
- `scalus-bloxbean-cardano-client-lib`: `Ordering[Voter]` sorted by CCL's `VoterType` ordinal,
  which declares key before script and so is the opposite of the ledger's order, resolving a
  `Voting` redeemer index to the wrong voter. A `Proposing` redeemer index was collected from the
  certificate list, which can never hold a `ProposalProcedure`, so every `Proposing` redeemer
  threw

### Added

- A translation oracle for the script context: cardano-ledger's own golden `TxInfo` corpus is
  vendored as a test resource, pinned by sha256, and compared field by field against the Scalus
  translation for V1, V2 and V3 withdrawals, votes and datums. Each cell asserts a minimum number
  of transactions compared, so the oracle cannot silently lose coverage and still report green
- On-chain safe API, nineteen operations the research corpus showed contract authors
  re-implement by hand with a measurable rate of security-relevant divergence. Prelude:
  `List.findUniqueOrFail(p, msg)` (one pass, fails on zero or two matches, where `find` accepts a
  second), `List.singleOrFail(msg)` and `SortedMap`/`AssocMap.singleOrFail(msg)` (the only
  element of a size-one collection, where `head` accepts the first of many),
  `a divFloor b` / `a divCeil b` (explicit rounding, one `divideInteger` each). `Credential`:
  `scriptHashOrFail(msg)`, `pubKeyHashOrFail(msg)`. `Value`: `hasNft(policy, name)` (exactly
  one; the strict twin is `hasOnly(policy, name, 1)`), `hasSameTokensAndAtLeastAda(expected)`
  (tokens exact, ADA open above, the continuing-output value check). `TxOut.hasInlineDatum(x)`,
  measured cheaper than `datum.inlineOrFail[T](msg) === x` (286 vs 461 lovelace) because the
  decode form unwraps and rewraps before `equalsData`. `TxInfo`: `findContinuingOutputOrFail`
  (compares the whole address, staking part included), `valuePaidTo` / `valueSpentFrom` (whole
  `Value`, not lovelace only), `isSignedByAny`, `validFromOrFail` / `validToOrFail` (lower bound
  inclusive, upper bound exclusive, as the ledger builds them), `onlyBurnsUnder` (guards the
  empty sub-map that makes `forall` vacuously true), `hasPaidTagged` (the tagged-output
  double-satisfaction defence). `TxOutRef.deriveTokenName` is `blake2b_256(serialiseData(ref))`
- `List.singleton` and `PairList.singleton`, the constructor name the map types already used
- Streaming subscriptions in `scalus.cardano.node.stream`: `BlockchainStreamProvider` /
  `BlockchainStreamReader` (and their `TF` variants over a generic effect) add rollback-aware
  event subscriptions to the existing reader/provider pair. Everything that changes over time
  now has both a one-shot read and a subscription, so `pollForConfirmation` becomes
  `subscribeTransactionStatus` with no sleep loop and no missed-update window.

  A provider declares only `StreamCapabilities`; whether a given request is served, and whether
  it is cheap, is derived from that by `SubscriptionSupport.of` — so a provider can neither
  refuse what it advertised nor accept what it did not. Subscriptions are registered
  synchronously, which makes `subscribe` followed by `submit` race-free.

  The stream type is chosen per call rather than per provider. `ScalusAsyncSource` needs nothing
  beyond the stdlib and works on JVM and JS alike; the new `scalus-streaming-fs2` module adds
  fs2 `Stream`s, and further adapters plug in through `ScalusAsyncStreamAdapter`.
  `StreamingEmulator` implements the facade over the in-memory emulator, and
  `StreamProviderConformance` in `scalus-testkit` holds any implementation to the capabilities
  it declares

- `StreamingBlockfrostProvider`, the first streaming provider over a real backend. It follows the
  chain by polling `/blocks/{hash}/next` and asks `/addresses/{a}/transactions` what each watched
  address did in each new block, so cost scales with how many addresses are subscribed rather than
  with how busy the chain is, and `pollInterval` is the one dial.

  What that costs in capability is declared rather than discovered: `ScanSupport.Unsupported` and
  `pushdown = {Address}`, no `Block` kind (it holds no blocks) and no `TransactionStatus` kind (it
  cannot follow a hash it was never asked to watch, and reporting `Pending` forever would be
  indistinguishable from a transaction that had not landed). Reorgs are detected and fail every
  subscription with `ResyncRequiredException` rather than being reconciled, which is why
  `rollbackHorizon` is `None` and `RolledBack` never arrives

- Scalus implementations of all 8 [UPLC-CAPE](https://github.com/IntersectMBO/UPLC-CAPE)
  benchmark scenarios in `scalus-examples`, each with a test pinning its script size and
  per-case execution budget, plus `scripts/cape-submit.sh` to generate, verify, measure and
  rank submissions against the published leaderboard in one command

## 1.1.1 (2026-08-25)

### Fixed

- `ByteString.toHex` is a plain `def` again. As a `@threadUnsafe lazy val` it cached a hex
  `String` twice the size of the bytes on every instance that was ever printed or encoded (about
  3x memory amplification), and that initialization is not safe under concurrent access.
  `@threadUnsafe` is gone from every remaining `lazy val` (#349, #350)
- `ShelleyPaymentPart.toHex` and `ShelleyDelegationPart.toHex` returned the hex wrapped in
  literal double quotes, because both went through `ByteString.toString`

### Performance

- `PlutusScriptEvaluator` passes the `TransactionHash` to `evalScript` instead of a pre-encoded
  hex string. The base path never reads it, so every redeemer of every fee-balancing pass paid
  for a hex `String` that only the custom `evalBudget` hook uses. The public callback type is
  unchanged
- `TypeScheme.arity` and `TypeScheme.numTypeVars` are plain eager `val`s. As `lazy val`s they
  compiled to a volatile field with a type test and an unbox on every read, on the per-node hot
  path of the UPLC optimizer (~277ns vs ~874ns per 1024 steady-state reads)

### JavaScript (npm `scalus` 1.1.1)

The npm package is published again after 0.18.1, and now carries the same version number as the
JVM libraries. What changed for JavaScript users since 0.18.1:

- **Protocol version 11 (van Rossem) is the default** for `Scalus.evaluateScript` and
  `Scalus.evalPlutusScripts`. Version 0.18.1 defaulted to PV10 (Plomin), so execution budgets
  differ. Pass `protocolMajorVersion` to `evalPlutusScripts` to select another version
- **`scalus.js` is a self-contained ES module** with the `@noble/*` crypto dependencies inlined.
  A browser loads it directly from `<script type="module">`; the old CommonJS shim no longer
  works. `require("scalus")` now fails with `ERR_PACKAGE_PATH_NOT_EXPORTED`, so CommonJS callers
  need `await import("scalus")`
- **`Emulator.withAddresses` funds each address with 10 000 ADA** when `lovelacePerAddress` is
  omitted. It funded 10 000 lovelace before, which is below min-ada, and both the README and the
  type declarations promised 10 000 ADA. The JVM `Emulator.withAddresses` already used 10 000
  ADA. This fix landed after the `v1.1.1` tag, so it reached the npm bundle only, not the Maven
  `scalus_sjs1` 1.1.1 artifact
- **`scalus.d.ts` matches the runtime again**: `ExUnits`, `Result` and `Redeemer` are declared as
  top-level exports (they were never members of `Scalus` at runtime), and the declarations gained
  the `Emulator` constructor's `initialStakeRewards` argument, the
  `submitTx(tx, debugScripts)` overload, and the `SlotConfig` fields

## 1.1.0 (2026-08-21)

### Added

- **Mutual recursion for top-level `def`s** in `@Compile` objects. Local mutual recursion inside
  a `compile {}` block now reports a clear error (`LocalMutualRecursionNotSupported`) instead of
  failing obscurely
- **UPLC source map**: full profile reports (`SCALUS_PROFILE=full`) now include
  `<scriptHash>-<tag>-<index>.uplc.json` – the pretty-printed UPLC of the script plus spans
  mapping the text back to Scala source positions and function names, indexed in
  `profile-manifest.json` as format `"uplc"`. Consumed by the Scalus Profile VS Code extension
  0.3.0 (compiled UPLC view with bidirectional cursor sync)
- Profile reports carry derived on-chain fee columns (lovelace/ADA per row)
- `plutus.v1.Value.containsAtLeast` – "does this value cover every asset of the other one", with
  the exact semantics of the CIP-153 `valueContains` builtin on canonical values, on every
  protocol version
- `plutus.v1.Value.insertCoin(cs, tn, amount)` – set one coin's amount (REPLACES, unlike `+`;
  zero deletes the coin), with the exact semantics of the CIP-153 `insertCoin` builtin, on every
  protocol version; lowers to that builtin at PV11
- `plutus.v1.Value.hasOnly(cs, tn, amount)` – "the tokens under this policy are exactly
  `{tn -> amount}`"; one `equalsData` on the policy's token map, ~35% cheaper in fee than the
  equivalent `tokens(cs) === SortedMap.singleton(...)`, portable to every protocol version
- `plutus.v2.OutputDatum.inlineOrFail[A]` (with an optional message overload) – decode an inline
  datum or fail, replacing the usual match boilerplate; `inline`, so calling it on a receiver
  statically known not to be inline is a compile error
- `ExUnits.fitsWithin` / `ExUnits.exceeds` – component-wise budget comparison matching the
  ledger's `pointWiseExUnits`
- **AI-assisted development**: scalus.org now serves llms.txt artifacts for coding agents –
  `llms.txt`, `llms-full.txt` (all docs as one file), `llms-examples.txt` (21 example
  contracts), and a version-pinned `llms-api.txt` API cheatsheet generated by the new
  `llmApiGen` sbt task. The Scalus skills (contract, contract-test, local-development,
  optimize-contract, smart-contract-security-review) ship as a Claude Code plugin:
  `/plugin install scalus@scalus`

### Performance

- **Self-application recursion** replaced the Z combinator in the recursion encoding: Knights
  benchmark -19.8% mem / -16.1% cpu, CAPE fibonacci_25 fee -23%, typical validators -4..12% cpu,
  smaller scripts
- **Static-argument transformation** (under `optimizeUplc = true`): loop arguments that never
  change are no longer re-passed; example corpus average -7.5% mem / -5.2% cpu, with recursive
  folds over constant context improving far more
- The SIR-to-UPLC lowering now runs through one unified pipeline (`sir.toUplc`, `lowerToUplc`
  and `CompiledPlutus` all route through it)
- Generated UPLC differs from 1.0.0: script hashes and pinned budgets change on upgrade.
  `Options.plomin` (PV10) still reproduces pre-van-Rossem output

### Deprecated

- `given Ordering[ExUnits]` – execution units are only partially ordered and the lexicographic
  ordering is wrong for budget checks; use `fitsWithin`/`exceeds`. Also fixed:
  `ExUnitsTooBigValidator` compared totals with that ordering and could accept a transaction
  over the CPU cap
- `SIR.toLoweredValue` and `SIR.lowerToUplc` – use `sir.toUplc` (the unified pipeline)

### Removed

- `EvaluatorReportConfig.profileThreshold` and `SCALUS_PROFILE_THRESHOLD` (never implemented)

### Changed

- **PV11 Value builtins (CIP-153).** At `targetProtocolVersion >= vanRossemPV`,
  `plutus.v1.Value` operations (`quantityOf`/`getLovelace`, `+`, `-`, `*`, `negate`, and the new
  `Value.containsAtLeast`) lower to the CIP-153 builtins – ~13-75x cheaper per operation.
  Behavior change: the builtins validate canonical form (strictly ascending keys, no zero
  amounts, no empty inner maps, keys <= 32 bytes, amounts within the signed 128-bit range) and
  fail on
  malformed values that the portable lowering tolerated; `unionValue`/`scaleValue` fail on
  128-bit overflow. Opt out with `Options.valueBuiltins = false`.
- `plutus.v1.Value.withoutLovelace` now deletes the `(adaPolicyId, adaTokenName)` coin (via
  `insertCoin(ada, ada, 0)`, lowering to the CIP-153 `insertCoin` builtin at PV11) instead of the
  whole empty-policy entry. Identical for ledger-shaped values; a non-ada token sitting under the
  empty policy now survives.
- The `scalus-testkit`-published `Value` generator (`plutus.v1.ArbitraryInstances.genAmount`) now
  draws token quantities from `+-(2^64)` instead of `+-(2^128)`, so generated values stay inside
  the CIP-153 quantity range; property tests that scale a generated `Value` must bound their
  factor to match
- **BREAKING (`scalus-sbt-plugin`)**: the sbt 1.x baseline is now **sbt 1.9.0**, up from 1.5.8.
  Consumers on sbt 1.5-1.8 must upgrade to sbt 1.9.0 or later. The plugin's shared-source shim
  moved to `sbt2-compat` 0.2.0, which itself requires sbt >= 1.9. The sbt 2.x axis is unchanged.
- Dependency refresh: BouncyCastle 1.84 -> 1.85.2 (20+ CVE fixes, incl. CVE-2026-14682),
  borer 1.16.2 -> 1.17.0, jsoniter-scala 2.38.16 -> 2.40.1, magnolia 1.3.21 -> 1.3.23,
  sttp client4 4.0.25 -> 4.0.26, dotty-cps-async 1.3.3 -> 1.3.4

### Fixed

- **AMM example**: two pools with identical parameters shared a script hash and LP policy,
  making LP tokens fungible across pools (mint cheap in one, redeem against a better reserve
  ratio in another). Each pool is now identified by a one-shot seed UTxO and a POOL NFT, with
  an explicit Init/Close lifecycle, and the LP token name is pinned in the mint check
- **EditableNft example**: the mint policy now pins the minted value to exactly the ref/user
  pair (extra tokens could ride along), requires the reference output to hold only the ref
  NFT, and burn is an exact-pair burn (burning just the user NFT orphaned the ref NFT forever)

## 1.0.0 (2026-07-30)

First stable release. `scalus-core`, `scalus-cardano-ledger` and
`scalus-bloxbean-cardano-client-lib` form the MiMa-checked stable API surface.
Migration guide: https://scalus.org/docs/get-started/migrating-to-1.0

### Fixed

- generated-name collisions in SIR lowering (prefix+counter conflation) and in the
  pattern-matching compiler (two matches on one source line) that could silently capture variables
- Transparent TypeVar conversions are checked against the registered byte shape instead of
  blindly relabeled; unknown shapes raise a positioned compile error

## 1.0.0-M2 (2026-07-30)

### Added

- `ScalusTest.runWithProfileReport` — profiling reports for direct UPLC evaluation, not just
  ledger-driven runs
- `YaciConfig.imageTag` and `YaciConfig.startupTimeoutSeconds`

### Changed

- **BREAKING**: profile rendering moved from `PlutusScriptEvaluator` internals into
  `scalus.uplc.eval.ProfileReportWriter` (removed classes were implementation-private)
- Yaci DevKit devnet defaults to `bloxbean/yaci-cli:0.12.0-beta5` in companion node mode — a
  protocol version 11 (van Rossem) node that runs PV11-compiled scripts
  (bloxbean/yaci-devkit#184, #185)
- dependency updates: yaci 0.4.5

### Fixed

- `BlockfrostProvider.localYaci` anchors slot-zero time on the chain's latest block instead of
  the Yaci admin `startTime`
- the testkit waits for the yaci-store index to reach the node tip before tests start
  (fixes "All inputs are spent" right after devnet startup)

## 1.0.0-M1 (2026-07-28)

First milestone of the 1.0 line. Protocol version 11 (van Rossem) is the default compile and
evaluation target, all previously deprecated API is removed, and `scalus-core`,
`scalus-cardano-ledger` and `scalus-bloxbean-cardano-client-lib` become the MiMa-checked stable
surface. Migration guide: https://scalus.org/docs/get-started/migrating-to-1.0

### Added

- Java-friendly Emulator API: Java collection accessors, `*OrNull` lookups, `SubmitResult`,
  `CompletableFuture`-based async methods, `EmulatorInitialState.builder()`
- `FlatCodec`/`Flats` Java facade for the flat codec with static encode/decode helpers and named
  `Flat` instances

### Changed

- **BREAKING**: `scalus.serialization.flat` package object converted to top-level definitions –
  source-compatible, but binary names changed
- **BREAKING**: `SIRVersion` bumped to 6.0 (`TypeVar` `optId` codec fix) – recompile precompiled
  SIR modules
- **BREAKING**: protocol version 11 (van Rossem) is now the default compile and evaluation
  target – every script hash changes; budgets drop ~30% and scripts shrink ~20%; use
  `Options.plomin` to reproduce pre-PV11 output
- Cardano protocol parameters updated: mainnet epoch 645, preprod 303, preview 1370
- ledger rules accept the van Rossem batch6 builtins on PlutusV1/V2/V3 at PV11

### Removed

- all previously deprecated API: `ReprTag`, top-level `scalus.ScalusDebug` and
  `scalus.CompileDerivations` aliases, `w7l`
- legacy experimental `scalus.bloxbean.TxEvaluator` – use `ScalusTransactionEvaluator` or
  `PlutusScriptEvaluator`
- the `multiIndexArray` builtin – a Scalus invention that no Plutus release implements; scripts
  using it could not decode under real Plutus tooling. Use `indexArray` per element

### Fixed

- `BlockfrostProvider.findUtxos` resolves reference scripts (previously `scriptRef` was always
  `None`, breaking reference-script fee calculation and UTxO selection); resolved scripts are
  hash-verified and cached per provider
- `PlutusVM` factories and `PlutusScriptEvaluator` now respect the target protocol version when
  selecting builtin semantics
- PlutusV1/V2 `Case`/`Constr` CEK step costs fell back to a ~300M placeholder instead of the
  Plutus reference values
- `List.at`/`!!` rejects negative and past-the-end indices on-chain, matching JVM semantics

## 0.18.2 (2026-06-19)

### Added

- CIP-57 blueprints embedded in the packaged JAR (`META-INF/scalus/blueprints/<Contract>.json`) on
  `package`, with a skip opt-out (`SCALUS_SKIP_BLUEPRINT` / `blueprint / skip`)
- `eject-examples` tool turns any bundled example into a standalone sbt project (build, compiler plugin,
  `sbt blueprint`/`deploy` wired up)
- profiling: `profile.json` in the Full report set; Scala 3 syntax highlighting in the annotated-source HTML

### Changed

- `scalus.js` published as an ES module (dropped `scalajs-bundler`), with `@noble` crypto inlined and
  minification enabled
- `ScalusBlueprintPlugin` renamed to `ScalusSbtPlugin` (old name kept as a back-compat alias);
  `scalus-sbt-plugin` cross-built for sbt 1 and sbt 2
- default LTS moved to Scala 3.3.8 (plugin still cross-builds 3.3.7/3.3.8/3.8.4); Scala Native unified on 3.8.4
- `scalus-testkit` packages the shared test helpers it exposes (e.g. `KeyPairGenerator`) so downstream
  projects compile
- dependency updates: esbuild 0.28.1, dompurify 3.4.11, magnolia, jsoniter-scala, jackson-databind

### Fixed

- security-hardening sweep across the example validators (Cardano Blueprint rosetta set) — amm drain,
  auction/lottery/vault/editablenft/crowdfunding/escrow and others
- `Eq` no longer warns on `===` for an `Eq` received as a using parameter, and JVM-module compiler warnings
  are cleared

## 0.18.1 (2026-06-15)

### Added

- Protocol Version 11 (van Rossem) evaluator support, matching `cardano-node` 11.0.1 / Plutus
  1.63.0.0. Adds builtin semantics variants `D` (PlutusV1/V2) and `E` (PlutusV3/V4) for PV11, plus
  cost models for the new builtins — `expModInteger`, `dropList`, the array operations,
  `bls12_381_G1/G2_multiScalarMul`, and the `Value` builtins (`insertCoin`, `lookupCoin`,
  `unionValue`, `valueContains`, `valueData`, `unValueData`, `scaleValue`) — wired to protocol
  parameters. PV11 semantics: UTF-8-byte-length text costing for `appendString`/`equalsString`/
  `encodeUtf8`, the `consByteString` `[0,255]` range check, `Int64`-bounded `shiftByteString`/
  `rotateByteString`, and `above_and_below_diagonal` CPU costing for `divideInteger`/`modInteger`.
  When a pre-PV11 cost model is supplied for a PV11 evaluation, the new builtins fall back to the
  vendored Plutus reference cost model instead of a placeholder cost
- CEK profiling data is now exposed through `scalus.js` (the interactive HTML report renderer stays
  JVM-side)
- UPLC text parser now accepts Haskell ASCII control-code escape mnemonics (`\NUL` … `\DEL`) and raw
  control characters inside string literals, matching Plutus's `charLiteral`

### Changed

- `scalus-core` and `scalus-cardano-ledger` build against Plutus 1.63.0.0; the bundled Plutus
  conformance suite is the 1.63 corpus, evaluated under semantics variant E
- the Plutus conformance test discovers every evaluation case in the corpus at compile time and runs
  them all by default, skipping only an explicit, documented ignore list (so new corpus cases are
  picked up automatically). Only 3 cases remain skipped — the `bls12_381_*_hashToGroup` DST-length
  cases blocked by a blst Java-binding bug (supranational/blst#232)
- MiMa compatibility baseline (`scalusStableVersion`) bumped to 0.18.0, and the now-obsolete MiMa
  problem filters removed
- dependency updates: sbt 1.12.12

### Fixed

- `bls12_381_G1/G2_multiScalarMul` rejects scalars outside the signed 4096-bit (512-byte) range, as
  Plutus does, instead of silently reducing them modulo the group order
- `unValueData` is costed by the input `Data`'s node count (matching Plutus `DataNodeCount`) rather
  than its full memory usage — it was over-charging by roughly 3× — and now rejects non-canonical
  `Value` encodings (currency symbols and token names must be strictly ascending, inner maps
  non-empty, no zero quantities) instead of silently normalizing them
- corrected a swapped `intercept`/`slope` in the `unValueData` memory cost in `builtinCostModelC.json`

## 0.18.0 (2026-06-09)

### Added

- Scala 3 cross-build: `scalus-plugin` and `scalus-core` now compile on both Scala 3.3.7 (LTS) and
  3.8.x. The plugin isolates the one diverging compiler-API call (phase registration) behind a
  version-specific `PluginCompat` trait; the remaining ~9.4k lines of dotty-internal API usage
  compile unchanged. The Native platform targets 3.8.3 while JVM/JS stay on 3.8.4, with
  version-conditional budget/size baselines for the cross-build
- V3 `==` now routes to structural equality: non-structural types (`Rational`, `AssocMap`) are
  excluded from the `Eq` system (their `given Eq` is replaced with a `compiletime.error` and the
  plugin errors on `Rational ==` / `AssocMap ==`, with `RationalEq`/`AssocMapEq` for explicit
  comparison), so every remaining `Eq` is structural; the V3 `List`/`Option` intrinsics
  (`contains`, `indexOf`, `deleteFirst`, `distinct`, `diff`, `Option.contains`) drop their dead
  `Eq` argument and compare via `equalsRepr`
- Conway certificate mutators in `scalus-cardano-ledger`: `StakePoolCertificatesMutator`
  (Shelley/Conway POOL transitions — registration, re-registration into `futureStakePoolParams`,
  retirement) and `VotingCertificatesMutator` (Conway GOVCERT DRep register/unregister/update with
  deposit validation). `SlotConfig` gains `epochLength`/`zeroEpoch` plus `epochOf`/`firstSlotOfEpoch`
  so the current epoch is derived from the slot (mainnet `zeroEpoch=208`, preprod `4`, preview
  1-day epochs) instead of treating the slot number as an epoch
- `EvaluatorReportConfig` — typed configuration for `PlutusScriptEvaluator` diagnostic output
  (output directory, artifact selection, profile level/outputs), overridable via the `SCALUS_DUMP`,
  `SCALUS_DUMP_DIR`, and `SCALUS_PROFILE*` environment variables; new `PlutusScriptEvaluator.apply`
  overload accepting it. The legacy `debugDumpFilesForTesting: Boolean` constructors are retained
  and map onto it
- profiling integrated into `PlutusScriptEvaluator`: when enabled (`SCALUS_PROFILE=summary|full`,
  `SCALUS_PROFILE_OUT=…`, or `EvaluatorReportConfig.profile`), each script's profile is rendered to
  the console or to per-script files. Output formats: a compact text summary, CSV, JSON, and a
  **self-contained interactive HTML report** — a tabbed UI with sortable/filterable tables and
  %-of-CPU bars, a "By Source Location" view with full source-position attribution and a recursive
  incoming-call tree, a call graph with inclusive (call-stack) cost and a Hot Paths tree showing
  where the budget flows from the entry point, a per-line cost-annotated source view (cross-linked
  from the location tables), and a transition-matrix heatmap with cost-weighted edges and a
  hot-edges table; the report title and per-entry fee are included
- `ProfileFormatter.toCsv` / `toJson` / `summary`, and `toHtml(data, sources)` with source
  annotation; `platform.createDirectories` and `platform.fileExists` for cross-platform file I/O

### Changed

- Evaluator debug dumps now use stable, overwriting filenames
  (`<scriptHash>-<language>-<tag>-<index>.flat`) instead of the volatile `script-<txid>-…` names,
  so fee-balancing re-evaluations overwrite rather than accumulate duplicate files; the per-builtin
  budget log is truncated once per evaluation (previously appended without bound) and renamed to
  `budget.log`; a `manifest.json` describing every dumped script is written alongside (#93)
- dependency updates: Yaci DevKit 0.4.4, cardano-client-lib/backend-blockfrost 0.7.2,
  jsoniter-scala 2.38.14, dotty-cps-async 1.3.3, jackson-databind 2.21.4, Scala Native libs 0.5.12,
  sttp client4 4.0.25, slf4j 2.0.18; security bumps for brace-expansion (CVE-2026-45149) and vitest
  (CVE-2026-47429) in `scalus-site`

### Fixed

- `EtaReduce` free-variable check had no cases for `Case`/`Constr` terms, so `λx.(f x)` was
  eta-reduced even when `f` referenced `x` inside a `Case`/`Constr` — dropping the binder and
  leaving `x` unbound; it now uses the canonical `TermAnalysis.freeVars`. Surfaced on Scala 3.8.x
  (Knights `depthSearch` lowering); latent on 3.3.x where the shape did not arise
- `TxBuilder` now accepts permissionless script stake-credential registration (registration is
  witness-free in the Conway ledger) instead of rejecting it as a credential/witness-type mismatch,
  while still honoring an explicitly attached script/native witness
- conformance vectors now run in cardano-ledger's Imp slot/epoch environment

## 0.17.0 (2026-05-05)

### Added

- `@UplcRepr` annotation system for fine-grained UPLC type representation control: opt types into
  native `UplcConstr` representation, propagate the annotation through lambda parameters/return
  types, intrinsic return types, `genSelect` field types, and `IfThenElse`/`Apply` target-type
  propagation; honored by `FromData`/`ToData` derivation
- native `UplcConstr` intrinsics: equality (`Eq` dispatch via `equalsRepr`), `Option` operations
  (`isDefined`, `isEmpty`, `get`, `getOrElse`, `map`, `flatMap`, `filter`, `exists`, `forall`),
  list `appendedAll` (eliminates per-element re-encoding when concatenating
  `@UplcRepr(UplcConstr)` lists, used by `Queue` in the Knights benchmark)
- Common Subexpression Elimination (CSE) UPLC optimizer pass — extracts repeated subterms into
  shared lets with descriptive names (also extracts `Force(Builtin)` and `Force(Force(Builtin))`)
- Common Context Extraction (CCE) UPLC optimizer pass — generalized to any single-hole position;
  extracts shared lambdas with descriptive names
- `Options.uplcOptimizers` for custom optimizer chain configuration
- CEK machine profiler: per-source-location and per-builtin-function breakdowns plus a state
  transition matrix; `optimize-contract` skill drives budget optimization workflow
- `UtxoFlow` framework for multi-transaction off-chain flows (Phase 1): branching, recursion/loop
  support for local tail-recursive functions, parameterized flows
  (`DataParameterizedFlowCellValidator`), compile-time checks for async closures and non-tail
  recursion
- `CellValidator` with full `UtxoCell` lifecycle: `CellContext`, `TxBuilder` integration,
  `transitionSpend`/`transitionMint` helpers, hardened against V011/V005/V016/V024 vulnerability
  patterns; `AuctionCell` reference example; `Factory` pattern for the Cardano UTxO model
- `scalus-blueprint-plugin` v0.1 — sbt plugin for CIP-57 blueprint generation, with `sbtn deploy`
  and `sbtn blueprint` commands
- Scalus plugin and example ejector (`#252`) — eject contract examples into standalone projects
  with cross-version sbt plugin support
- recursive auction contract example with `Bind` pattern handling fix
- agent-based testing model in `scalus-testkit` (`ContractTestActor`)
- opaque-type support in `FromData`/`ToData` derivation and the compiler plugin; top-level opaque
  types resolved via `$package` module scanning; `SIRModuleAnnotation` marker trait and
  `Module.anns` field
- variadic factory intrinsics for `Map.singleton` and `Map.empty`; `List.unboxedNil[A]` for
  unboxed empty list creation
- `Options.noWarn` to silence SIR lowering warnings
- `MiniBF`-compatible Blockfrost API endpoints; `UtxoSource.FromAsset` in `BlockfrostProvider`
- emulator preconfig support: pre-initialize from JSON files, allow custom datums, extended JS
  API (`tick`, `hasTx`, `getDelegation`, `getDatum`, `withState`)
- `assertEvalWithBudgets` and `PlutusV3.withOptions` for dual-budget testing across modes
- optional Scalus on-chain identification tag marker (spec + implementation)
- `DynJson` utility for dot-syntax JSON navigation
- gated CEK diagnostic assertions for representation/arity mismatches
  (`-Dscalus.assert.apply.data.to.uc=1`): `[APPLY-DATA-TO-NATIVE-UC]`,
  `[APPLY-VCONSTR-ARITY-MISMATCH]`, `[CASE-VCONSTR-ARITY-MISMATCH]`,
  `[CASE-DATA-ARITY-MISMATCH]`
- documentation: Advanced Optimisations chapter, Authenticated Collections (Merkle Trees, MPF,
  Bilinear Accumulators) split into static/incremental pages, Ledger Framework architecture,
  TxBuilder close-out report, unit/property/TDD testing pages, thorough Lottery and Auction
  example READMEs, Profiling page

### Changed

- **BREAKING**: removed deprecated APIs marked `@deprecated` since 0.14.x (including bloxbean
  CCL APIs); MiMa filters added for the removed surface
- `SumDataList`/`SumDataPairList` unified into parameterized `SumBuiltinList(elementRepr)`;
  `SumPairBuiltinList` separated for pair-element lists
- `PairData` and `ArrayData` aliases removed in favor of parameterized `ProdBuiltinPair(fstRepr,
  sndRepr)` and `ProdBuiltinArray(elementRepr)` carried throughout the lowering
- `PairList` intrinsic providers added; `SumDataPairList` head representation fixed
- forced builtins naming convention updated from `__builtin_Name` to `__Name`
- `cachedTopLevelHelpers` cache keys discriminate by call-site env capture and resolved `TypeVar`
  bindings (`captureFingerprint(tps)` plus `typeUnifyEnv.filledTypes`), so two callers with the
  same SIR shape but different repr environments get separate helpers
- `pendingTopLevelLetRecs` scoped per compile unit (no cross-compile leakage); only letrec
  bindings reachable from the lowering root are wrapped
- `stableKey` includes structural repr info to reduce `SumReprProxy` identity leaks across
  `cachedTopLevelHelpers` lookups
- `ProductCaseUplcConstrSirTypeGenerator.upcastOne` dispatches on `input.representation` rather
  than relabeling unconditionally — closes the unsafe-Data-as-UC relabel surface that produced
  4-arg native selectors against Data scrutinees
- `genConstrLowered` is the canonical Constr-dispatch API; `precomputedValues` removed and the
  dispatch chain pushed down into generators
- `TypeVarKind` simplified to `Transparent`/`Fixed`; `nativeTypeVarRepresentation` and
  `nativeListElements` flags removed (the `@UplcRepr` machinery keeps native representations
  end-to-end, eliminating dual lowering paths and per-test `if/else` budget snapshots)
- `typeProxy` replaced with `typeProxyRepr` for explicit representation control
- removed `MerklePatriciaForestry` radix-2/64 variants, keeping only the MPF-16 implementation;
  `Fork.neighborHash` reduced to a single slice+blake2b
- non-derived `Eq` instances now produce a warning; `Eq.keyPairEq` deprecated
- `scalus-plugin` published with `CrossVersion.full` to allow per-Scala-version publishing

### Fixed

- `ProdDataConstr` / `DataConstr` `.isCompatibleOn(_, TypeVarRepresentation(_))` was unconditionally
  permissive: any TypeVar kind matched. For `@UplcRepr(UplcConstr)` types whose default rep is
  native Constr, a Data → `TypeVarRepresentation(Unwrapped)` "compat" returned true, producing a
  pure relabel and leaving Data bytes labeled as Unwrapped. Downstream `genSelect` then emitted a
  4-arg native-UC selector against a 2-arg `Data.Constr` scrutinee, leaving a 2-arg residual
  lambda and a `MultiplyInteger Apply LamAbs` runtime crash. Fix: discriminate `TypeVarKind` —
  `Transparent`/`Fixed` stay compatible; `Unwrapped` only when
  `lctx.typeGenerator(tp).defaultRepresentation(tp) == this`
- KnightsTest:475 heisenbug: output `TypeVar` leak via wrong `eqClass` inheritance and
  non-deterministic tie-break in `chooseCommonRepresentation`
- intrinsic binding internal `TypeVar`s no longer leak across dispatches; rebound at unify time
  rather than stripped on merge-back
- `Fixed` `TypeVar` in `UplcConstr` fields falls back to default rep instead of leaking abstract
  Data marker; `Fixed` `TypeVar` no longer leaks into `ProdBuiltinPair`/`SumUplcConstr`
  `fieldReprs`
- representations propagate through `TypeVar` and `FreeUnificator` widenings so structural
  unification doesn't drop the caller's repr context
- `optTargetType` propagates through `IfThenElse` to branches; `lvApply` preserves `@UplcRepr`
  param reprs; CSE guarded against partial-builtin helpers
- lazy `Case` branch evaluation in JIT to match CEK semantics
- CEK and JIT evaluation of Plutus benchmark use-case scripts
- recursive enum derivation: allow unfilled type proxies in `AppliedType` args
- non-recursive let bindings scoping; opaque types with type alias returns filter the full inline
  proxy chain
- pair pattern variables added to scope in PV11 `genMatchPairData`
- UTxO query errors propagate as `TxBuilderException.UtxoQueryException`
- `slotToTime` uses `.toLong` for Scala.js `BigInt` compatibility
- CI JVM heap capped at 7GB to prevent OOM on GitHub Actions runners

## 0.16.0 (2026-03-06)

### Added

- compile-time partial evaluation in UPLC optimizer with occurrence analysis for smarter inlining
- source positions in UPLC Term and CEK error reporting for precise error diagnostics
- diagnostic replay for release scripts — `DebugScript` API re-evaluates stripped scripts with full
  traces for debugging
- `strip trace/log` statements for release builds
- `PairList` type to avoid `SumDataList`/`SumDataPairList` per-element conversions, used internally
  in `SortedMap` and `Value.zero` for cheaper on-chain operations
- `lovelaceAmount` on `Value` for efficient lovelace extraction via `PairList.head`
- `LinkedList` library implementation with on-chain validator and off-chain utilities
- Merkle Patricia Forestry variants (mpfb, mpfo) with on-chain `verifyMembership`/`verifyNonMembership`,
  off-chain trie operations, and Aiken cross-implementation compatibility
- `FrontierMerkleTree`, `MerkleTree`, `MembershipToken` data structures
- `IncrementalMerkleTree` (formerly AppendOnlyMerkleTree) with path-byte encoding optimization
- bilinear accumulator allowlist validator with `scalus-ethereum-kzg-ceremony` subproject
- Pippenger MSM optimization for BLS12-381 with allocation-free Montgomery NTT
- constant product AMM contract example
- multi-pool DEX example with multiple withdraw-zero scripts
- anonymous data contract with on-chain reading via reference inputs
- `DecentralizedIdentity` on-chain contract with `CompiledPlutus` mint and required signers
- upgradable proxy validator
- CAPE two-party escrow benchmark with factorial and fibonacci benchmarks
- `payTo` overloads for `CompiledPlutus` in `TxBuilder`
- `checkTransaction`, `pollForConfirmation`, and `submitAndPoll` on `BlockchainProvider`
- `ProtocolParams.diff` and `CardanoInfo.verify`
- `UtxoEnvDefaults` for inlined protocol parameters
- emulator stake state pre-initialization with `Map[Address, Coin]` and JS APIs
- `->` operator support in Scalus compiler plugin
- `assertEvalWithBudget` lambda overload to prevent constant folding in budget tests
- builtin totality annotations for improved dead code elimination
- Cardano protocol parameters updated to epoch 616
- `datumBuilder` for outputs with datums that depend on the final transaction shape

### Changed

- reorganized crypto packages — `crypto/trie`, `crypto/tree`, `crypto/accumulator`; flattened
  `bls12_381`
- moved off-chain crypto from `cardano.offchain.crypto` to `scalus.crypto`
- renamed `MerklePatriciaTrie` to `MerklePatriciaForestry`, `BinaryMerklePatriciaTrie` to
  `FusedMerklePatriciaForestry`
- merged `Compiled[A]` trait into `CompiledPlutus[A]`
- `requiredSigners` is now a transaction-wide concept; per-step `requiredSigners` APIs deprecated
- replaced `PlutusV4` with `PlutusV3` + `targetProtocolVersion`
- replaced `sourcePos` with `UplcAnnotation` in UPLC Term
- optimized `List.at` with direct tail-recursive traversal
- `List.single` and `PairList.single` are now `inline` for cheaper on-chain operations
- use `TaggedSortedSet.apply` instead of `TaggedSortedSet.from` for zero-cost wrapping of SortedSets
- removed deprecated code from 0.13.x and earlier
- removed unused `scalusPluginTests` project

### Fixed

- UPLC optimizer now uses position-ignoring equality to avoid false mismatches
- `Force(Delay)` elimination after inner optimization in Inliner
- free variable tracking in `substitute` to avoid name capture
- `Constr` with value args recognized as WHNF in `isValueForm`
- unsaturated builtin applications recognized as WHNF
- propagate `inlinedFrom` in `compileThrowException` for inline `require`
- `Math.sqrt` optimized with `log2`/`exp2` initial guess
- deterministic validator iteration order using `SortedSet`
- validate seed `TxOutRef` on-chain and harden off-chain helpers
- propagate spent budget through CEK/emulator error chain
- ignore zero-amount entries in assets map during minting
- fetch Yaci DevKit slot config from admin API instead of hardcoding

## 0.15.1 (2026-02-13)

### Added

- Merkle Patricia Forestry data structure for on-chain Merkle proofs
- variadic `log` function with auto-show for trace diagnostics
- `offsetOf[A]` compile-time macro for case class field indices
- `BuiltinList` pattern matching and optional Nil branch in `CaseList`
- `(con value ...)` syntax support in UPLC parser/printer
- `Future`/`Scenario` conversion and `EmulatorBase` API

### Changed

- simplified `Scenario` and `ContractScalaCheckCommands` APIs
- updated MaryEraValue builtin cost models to match Plutus 1.58.0.0 reference
- extracted dependency versions into build.sbt variables and bumped dependencies

### Fixed

- count native reference scripts towards minFee calculation
- use `Timelock.toCbor` for native ref script size instead of `Cbor.encode(Script)`
- account for sponsor VKey witness in fee estimation
- handle exceptions in `Scenario` monad and filter extra VKey witnesses in `TxBuilder.sign`
- filter auction UTxO by NFT asset to prevent spam UTxO attacks
- fix MaryEraValue builtins and pool retirement validation

## 0.15.0 (2026-02-05)

### Added

- all new builtins for protocol version 11 hard fork
- JavaScript Emulator support for cross-platform testing
- HD wallet implementation with BIP-39 mnemonic, BIP32-Ed25519 key derivation, and CIP-1852 account
- Conway era ledger state rules: `DELEG` and `POOL` transitions for stake delegation and pool
  registration management
- `TxBuilder.draft` method for unbalanced transaction assembly
- `txBuilder` function with `CardanoInfo` context parameter
- type-safe `UtxoQuery` DSL with parallel execution and rate limiting for UTxO queries
- `Transaction.scriptContexts` extension method for easier script context access
- `MaryEraValue` support (CIP-0153)
- `expModInteger` builtin (CIP-109) for modular exponentiation
- `bls12_381_G1_multiScalarMul` and `bls12_381_G2_multiScalarMul` builtins (CIP-0109)
- `g1` and `g2` string interpolators for BLS12-381 elements
- use `dropList` for case class field selection at index >= 2 on PlutusV4
- `Pretty` typeclass with automatic derivation for ADTs and Cardano ledger types
- `Eq.derived` for automatic equality typeclass derivation
- design patterns documentation: Merkelized Validators, Parameter Validation, OptimizedPaymentSplitter
- Crowdfunding, English Auction, Lottery, and PriceBet smart contract examples
- multi-environment integration testing infrastructure with Yaci DevKit consolidation
- comprehensive `scalus-testkit` API with `EvalTestKit` and `ScalusTest` base classes
- `Output` type alias for `TransactionOutput`
- `fetchScript` method to retrieve scripts from Blockfrost
- 5 ADA threshold for collateral return optimization

### Changed

- reorganized package structure - `scalus.builtin` types and `scalus.prelude` moved to new locations
- `Compile` and `Ignore` annotations moved to `scalus.compiler` package
- `Validator` traits moved from `prelude` to `v3` package
- `Provider` deprecated in favor of `BlockchainProvider`
- simplified `BlockfrostProvider` API with platform-specific backends
- BLS12-381 classes moved to `scalus.uplc.builtin.bls12_381` package with shorter names
- optimized `toData` for constant `ByteString`, `String`, `Boolean`, and `Integer` values
- optimized `mkNilData` builtin to constant
- optimized `Eq.derived` to use direct field access
- improved PlutusV3 match lowering to reduce script size
- renamed `__z_combinator` to `__Z`
- deprecated `Contract` trait
- removed deprecated code from 0.12.x

### Fixed

- suppress false warnings for type tests on enum variants
- handle inline accessors in compiler plugin
- ensure error messages appear in UPLC traces
- `ByteString` now extends `Serializable`
- security improvements for example validators: AuctionValidator, BettingValidator, Crowdfunding,
  VaultValidator, VestingValidator
- use Conway-style certificates for script-based stake registration
- platform-specific `modPow` for Scala.js and Scala Native compatibility

## 0.14.2 (2025-12-31)

### Added

- BLS12-381 cryptographic operations for Scala Native via FFI bindings to blst library, enabling
  full Plutus V3 support on Native platform
- pure Scala implementations of cryptographic hash functions for Native platform:
  - Keccak-256 and SHA3-256 (replaces Rust tiny_keccak_wrapper)
  - RIPEMD-160 (enables ripemd_160 builtin on Native)
- type-safe `Blueprint.plutusV3` factory methods with automatic schema derivation for CIP-57
  blueprints
- `PlutusV3.withErrorTraces` method for debug builds with error traces
- `ToData` and `FromData` instances for Conway era protocol parameters to support governance actions
  in Plutus V3 scripts
- `scalus-secp256k1-jni` project with custom secp256k1 JNI bindings (replaces bitcoin-s dependency)
- `Input` type alias for `TransactionInput` to improve code readability
- missing Plutus conformance tests for 100% coverage (108 new tests)
- documentation for builtin functions, protocol parameters, emulator, and TxBuilder

### Changed

- replaced bitcoin-s secp256k1 with scalus-secp256k1-jni using secp256k1 0.6.0
- `TransactionInput` comparison now uses efficient ByteString ordering instead of hex string
  conversion
- slot configuration variables renamed to camelCase with deprecation warnings
- deprecated old `Blueprint.apply` method in favor of type-safe `Blueprint.plutusV3` methods
- updated nixpkgs to version 25.11 and JDK to version 25

### Fixed

- collateral sufficiency check in `FeesOkException`
- error handling in `SIRCompiler`
- ECDSA signature component validation in secp256k1 on JavaScript platform
- overflow in `NonNegativeInterval` arithmetic with large denominators
- empty list handling in `Data.Map` lowering
- reward account conversion in `ProposalProcedure` translation for V3
- `ScriptDataHashGenerator.computeScriptDataHash` "no such element" exception
- native access for BLST JNI library on Java 22+
- blst-java JNI symbol conflict with nix blst library

## 0.14.1 (2025-12-22)

### Added

- CIP-0153 MaryEraValue (BuiltinValue) support with 7 builtin functions: `insertCoin`, `lookupCoin`,
  `unionValue`, `valueContains`, `valueData`, `unValueData`, `scaleValue`
- TypeScript tests for Scalus NPM package
- Yaci DevKit integration tests for TxBuilder
- native script minting integration test
- browser tests for NPM package

### Changed

- NPM bundle renamed to `scalus.js`
- removed pattern matching restrictions (guards section)
- documentation updated for new TxBuilder API

### Fixed

- include proposal deposits in `TxBalance.produced` calculation
- `BuiltinValue` type parameter support in SIR lowering
- JSON parsing for `ProtocolParams` to handle both string and number formats

## 0.14.0 (2025-12-19)

### Added

- Plutus V4 language support with Case instructions on boolean, integer, list, Data, and Pair types
- CIP-0138 Plutus Core Builtin Type Array implementation
- CIP-0156 Plutus Core Builtin Function `multiIndexArray`
- JIT compiler with 6x speedup over CEK interpreter
- `TxBuilder.complete` with iterative balancing, automatic UTxO selection, and async support
- `TxBuilder` staking and governance implementation with delegation, DRep registration, and voting
- `Data` pattern matching support
- `copy` method support for case classes in Scalus scripts
- `dropList` builtin implementation with saturating CostingInteger arithmetic
- cross-platform Ed25519 signing with type-safe opaque types (VerificationKey, Signature,
  SigningKey)
- `addr` and `stake` string interpolators for bech32 address parsing
- `ByteString.utf8` string interpolator support in SIR compiler
- `Blueprint.fromJson(InputStream)` overload for efficient parsing
- `Transaction.utxos` method for transaction chaining
- `Emulator` for transaction testing
- `StrictIf` optimization for if-then-else expressions
- `preprod` and `preview` constants in `CardanoInfo`
- SIR function application operator `$` with comprehensive test suite
- collateral with native tokens support per Babbage specification
- delayed redeemer support for minting operations
- `TermSanitizer` to sanitize variable names in UPLC terms
- field selection for Data subtypes (I, B, List, Map, Constr)
- `toLedgerValue` method for Value conversion
- `SortedMap.from` and `Value.unsafeFromSortedMap` methods
- `MultiAsset.onlyPositive` method
- Plutus conformance tests updated from 1.40.0.0 to 1.53.0.0
- comprehensive ledger rules validation tests

### Changed

- **BREAKING**: `mint` API unified - use `mint(script, assets, redeemer)` for attached scripts,
  `mint(policyId, assets, redeemer)` for reference scripts
- **BREAKING**: `Provider` consolidated to async-only, `AsyncProvider` renamed to `Provider`
- **BREAKING**: `scalus.sir` package moved to `scalus.compiler.sir`
- **BREAKING**: `builtin.Data` uses `scalus.prelude.List` instead of
  `scala.collection.immutable.List`
- **BREAKING**: `ChangeOutputDiffHandler` now accepts `Value` instead of `Coin`
- renamed `NodeEmulator` to `Emulator` and moved to scalus-cardano-ledger
- renamed `AsyncBlockfrostProvider` to `BlockfrostProvider`
- renamed `LucidEvolutionKeyPair` to `LucidKeyPair`
- `TxBuilder` async methods renamed: `completeAsync` to `complete` (returns `Future[TxBuilder]`)
- deprecated `Environment` type alias in favor of `CardanoInfo`
- deprecated `PaymentBuilder` in favor of `TxBuilder`
- deprecated `mintAndAttach` in favor of `mint(script, assets, redeemer)`
- deprecated `SpendWithDelayedRedeemer` in favor of `Spend` with delayed redeemer witness
- migrated sttp from v3 to v4
- updated jsoniter-scala-core to 2.38.6
- updated bcprov-jdk18on to 1.83
- updated pprint to 0.9.6
- optimized CEK machine with mutable HashMap for builtins initialization
- optimized list transformations using view for better performance
- JIT compiler eliminates VCon allocations for cost-calculating builtins

### Fixed

- floor division edge case on Scala.js for `Long.MinValue / -1`
- redeemer index recalculation after adding inputs in `TxBuilder.complete`
- collateral calculation precision in `calculateRequiredCollateral`
- inline datum handling for PlutusV1 scripts
- deterministic ordering in topological sort during SIR lowering
- wildcard case generation for product types in pattern matching
- `TransactionSigner` preserves existing witnesses when signing
- delayed redeemers recomputed after `complete` adds inputs
- `Interop.toScalusData` preserves order in Map
- zero-ada change output kept if it contains assets
- `SIRType.show` deterministic by removing hashcodes from type variable display
- collateral index output calculation in `PlutusScriptsTransactionMutator`
- various ledger rule validators: `ValueNotConservedUTxO`, `MissingRequiredDatums`,
  `MissingOrExtraScriptHashes`, `FeesOk`, `OutputsHaveNotEnoughCoins`
- `balanceFeeAndChange` correctly handling modifications that invalidate result
- bootstrap address attributes size validation in transactions

## 0.13.0 (2025-10-22)

### Added

- new pattern matching based on decision trees
- primitive constant pattern matching in `SimpleSirToUplcLowering` and `SirToUplc110Lowering`
  backends
- LazyVal transformation to float lazy lets closer to their usage points
- lambda barrier for let-floating optimization to prevent work duplication
- `evalPlutusScripts` to JScalus for JavaScript platform
- LinkedList pattern implementation with initialization and de-initialization
- PlutusScriptEvaluator benchmark for performance testing
- Fibonacci lookup implementation using pre-packed ByteString
- UTXO indexers using delayed redeemers
- `SortedMap.getOrFail` accepts custom error messages
- Ledger Rules Framework: TraitObjectScanner for automatic discovery of rule implementations
- `compileWithOptions` method accepting explicit compiler options

### Changed

- updated Scala version to 3.3.7
- **BREAKING**: repackaged txbuilder from `scalus.cardano.ledger.txbuilder` to
  `scalus.cardano.txbuilder`
- **BREAKING**: `ProtocolParams.costModels` type changed to `CostModels`
- **BREAKING**: removed `semanticVariant` from `MachineParams`
- moved `SlotConfig` to scalus-core
- **BREAKING**: replaced `bloxbean.SlotConfig` with `cardano.ledger.SlotConfig`
- renamed `UTxO` to `Utxos` for consistency
- updated jsoniter-scala-core to 2.38.3
- inheritance via inline override for validators
- updated builtin cost models to use CardanoInfo (reduces scalus-cardano-ledger-opt-bundle.js by ~
  100k)
- renamed `builtinsMeaning` to `getBuiltinRuntime` in Cek and PlutusVM
- pre-compute TxInfo for Plutus versions to optimize redeemer evaluation
- replaced ProtocolParams instantiation with CardanoInfo for consistency
- consolidated compile method handling in compiler plugin
- made validator helper methods public to support inheritance
- replaced `BuiltinsMeaning` with `CardanoBuiltins`
- removed deprecated methods and cleaned up code
- improved error diagnostics for wildcard case positioning
- enhanced SIRLinker debug logging and error handling
- modified base validator traits to use inline abstract methods
- disabled linker in compiler plugin (functionality moved to separate compilation phase)
- improved script witness computation and UTxO handling
- optimized ByteString handling and improved offchain method documentation
- refactored LinkedList contract to use failures instead of bools
- refactored HTLC and Vault validators for improved clarity

### Fixed

- Word64 flat format encoding
- `Term.Constr.tag` encoding as unsigned Word64 type
- pattern matching on primitive constants with default case
- `@` pattern bindings in nested positions
- duplicate wildcard cases in pattern match compilation
- incorrect typing of tail of list pattern
- bug in alphaEquality (case arguments may be variables)
- type substitution in constructors
- `Interop.getMintValue` resolves all BlocksValidation fails
- Boolean data representation compatibility with Plutus
- compilation on Windows
- bug with incorrect handling of `SIR.Const`
- non-recursive let bindings scoping bug
- performance regression in LinkedList tests
- S3LoweringDataAccess regression
- validation for non-negative integers in flat serialization
- Data CBOR encoding for large negative integers (Java adds an extra 0 byte)

## 0.12.1 (2025-10-08)

### Added

- Address constructors for Credentials
- Transaction and TransactionOutput constructors
- `NormalizedInterval` implementation for interval operations
- ledger rules: `missingRequiredDatums`, `validateOutputBootAddrAttrsTooBig`,
  `hasExactSetOfRedeemers`
- upstream tx editor and tx builder
- additional functions to StdlibTestKit

### Changed

- refactored and improved offchain MultiAsset, Coin and Value
- refactored HtlcValidator to follow scalus style
- deprecated prelude `orFail`, replaced `==` with `===` in several places
- simplified datum retrieval in Escrow, HelloCardano, and Vesting scripts
- refactored TransactionWitnessSet.apply
- moved BuildInfo to `scalus.utils`
- replaced Hex.hexToBytes calls with extension method
- rewritten patterns tests using UPLC

### Fixed

- not all tree is traversed during Apply handling in CaseConstrApply optimization pass
- plutus scripts error propagation to low level tx balancer
- wrong redeemers sorting in ScriptContext
- low level tx balancer fee change re-looping
- hardcoded change output values in transaction building
- budget calculation: wrong sorting order of redeemers

## 0.12.0 (2025-09-27)

### Added

- Cardano ledger rules implementation and validation
- experimental UPLC JIT compiler for improved performance
- `Show` typeclass implementation for better debugging in smart contracts
- new `Betting`, `Escrow` and `Simple transfer` examples
- CBOR codecs reserialize to the same bytes with `KeepRaw`
- experimental transaction builder with intention-based API
- `monocle` optics support for lens-based data manipulation
- Ed25519 compatibility for Cardano key derivation and message signing
- BLS12-381 scalar field support
- property-based testing utilities with ScalaCheck integration
- enhanced plutus script evaluation on JavaScript platform

### Changed

- major backwards compatible package reorganizations
- moved Cardano ledger types to `scalus.cardano.ledger`
- moved protocol parameters and cost models to appropriate packages
- transaction builder redesigned with cleaner intention-based API
- improved Value implementation using SortedMap
- enhanced error messages and debugging support
- updated build system and dependency management

### Fixed

- script data hash generation for PlutusV1 cost model serialization order
- various transaction building and balancing issues
- improved error handling in script evaluation
- better handling of native tokens in transaction building

## 0.11.0 (2025-07-28)

### Added

- new more efficient UPLC codegen
- CIP-57 Blueprints support
- improved Scalus standard library and tests
- `PlutusScriptEvaluator` for evaluating Plutus scripts using Scalus ledger domain types
- `ScriptDataHashGenerator` for computing script integrity hash
- added and improved Cardano Ledger rules
- Arbitrary instances for Plutus domain types
- Vesting example

### Fixed

- avoid expensive G1/G2 zero/generator constants initialization

### Changed

- Plutus `Value` uses `SortedMap` instead of `AssocMap`
- improved Cardano Ledger classes

## 0.10.4 (2025-07-16)

### Fixed

- large BigInt encoding occasionally added an extra byte to the CBOR encoding
- mint value doesn't contain 0 Ada in Plutus V3 scripts

## 0.10.3 (2025-07-16)

### Added

- updated to a new Maven Central repository

## 0.10.2 (2025-07-07)

### Added

- lazy cost model loading in `ScalusTransactionEvaluator`

### Fixed

- BLS builtins serialization
- cost parameters for `divideInteger`, `modInteger`, `quotientInteger`, `remainderInteger`

## 0.10.1 (2025-06-04)

### Added

- more Cardano ledger rules

### Changed

- moved Cardano Ledger domain types to scalus project
- refactor Hash types in ledger domain types

## 0.10.0 (2025-05-28)

### Added

- improved Scalus standard library
- Scalus Design Patterns project
- Scalus Examples: HTLC, PaymentSplitter
- Scalus benchmarks: Knights and Clausify
- new UPLC 1.1 generation `SirToUplc110Lowering` (experimental)
- Cardano Ledger domain types with CBOR codecs (experimental)
- Scalus Bilinear Accumulator implementation (experimental)

### Changed

- simplify `ToData`/`FromData` derivation allowing Scala 3 `derives`
- updated to Scala 3.3.6, Scala.js 1.19.0

### Fixed

- support for Scala argumentless functions in Scalus compiler

## 0.9.0 (2025-04-17)

### Added

- all Plutus V3 builtins from the Plomin hard fork
- improved Scalus standard library
- support for TupleN types
- support for destructuring: val (a, b) = Foo(2, “b”)
- BLS12-381 primitives work on JVM and Javascript
- Groth16 SNARK implementation works on JVM and Javascript
- Cardano native scripts support: Timelock
- advanced UPLC optimization: case/constr instead of apply
- generate better error traces for UPLC
- PaymentSplitter example
- simple Validator trait for writing validators
- Scalus Testkit: a simple test framework for testing Plutus scripts

### Changed

- better SIR code generation (remove recursivity)
- better UPLC optimizations: eta-reduce
- better error messages in the compiler
- renamed `Maybe` to `Option` to follow Scala naming conventions
- cardano-client-lib 0.6.4

### Fixed

- bug with Select fields during linking of SIR

## 0.8.5 (2025-02-16)

### Added

- Scalus Scala Native support with all Plutus V1/V2 builtins
- a C example using libscalus native library for script evaluation and budget calculation
- compile synthetic DataType.apply as a primary constructor
- better error messages during Scalus on-chain script compilation
- async-profiler support for profiling

### Changed

- dependencies updated: Scala 3.3.5, Scala.js 1.18.2
- speedup MemoryUsage calculation

### Fixed

- removed unnecessary bitcoin-s-crypto dependency
- Data.unit type is Data, not Constr

## 0.8.4 (2025-01-05)

### Added

- CIP-117: fail on non-unit results in Plutus V3 scripts

### Fixed

- OptimizingSirToUplcLowering threw an exception when generateErrorTraces was enabled

## 0.8.3 (2024-12-20)

### Added

- customizable UPLC Inliner optimization pass
- Prelude List functions: single, cons, length, map2
- experimental Groth16 SNARK implementation
- Data `toCbor` and `fromCbor` extensions
- add generated ScalaDocs to website

### Changed

- code is updated to Scala 3.5+
- use latest dependencies

### Fixed

- BLS12-381 G1/G2 compressed zero points encoding

## 0.8.2 (2024-10-11)

Nothing changed, just pushed the wrong commit under 0.8.1.

## 0.8.1 (2024-10-11)

### Fixed

- cost model for `integerToBytestring` and `bytestringToInteger`
- flat encoding of SIR `Data` was wrong for `Map` and `List` types

## 0.8.0 (2024-10-03)

### Added

- initial Plutus V3 support:
    - all builtins work on JVM
    - V3 `ScriptContext` etc for script compilation
    - V3 script evaluation and cost calculation
- Cardano Client Lib can use `ScalusTransactionEvaluator` for V3 script evaluation
- improved `deriveEnum` for `ToData` and `FromData` (see Tutorial)

### Changed

- few renames and API changes in `scalus-bloxbean-cardano-client-lib` `Interop` module

## 0.7.2 (2024-08-11)

### Added

- `VM.evaluateDebug` with useful info about the execution: budget, costs, builtins, logs
- `eval`, `evalDebug`, `show`, `showHightlighted` extensions for `Term` and `Program`
- `toUplcOptimized` extension for `SIR` to generate optimized UPLC code
- `?` tracing operator and `log` function similar to Aiken's `?` operator

### Changed

- don't remove top-level eta-expansions in `compile` method

### Fixed

- fieldAsData/field didn't work for aliaed types
- small fix in `OptimizingSirToUplcLowering` with tracing errors
- multiple inner matches in pattern matching code generation

## 0.7.1 (2024-08-04)

### Added

- implemented all Plutus V1/V2 builtins on JVM and JavaScript platforms
- passing all Plutus Conformance Tests on JVM and JavaScript platforms
- `OptimizingSirToUplcLowering` SIR to UPLC lowering with built-in optimizations
- PlutusV3 constr/case support
- `Data.to*` extension methods

### Changed

- speedup `UplcParser` by 10x

### Fixed

- use Java 11 on Github CI and release builds

## 0.7.0 (2024-05-29)

Scalus CEK and cost calculation implementation is now feature complete.

We were able to validate transactions from whole Cardano Epoch 484 using Scalus and Cardano Client
Lib.

### Added

- `ScalusTransactionEvaluator` - Cardano Client Lib (CCL) `TransactionEvaluator` implementation. You
  can now use Scalus
  to evaluate scripts and their costs off-chain during transaction building.
- SIR and UPLC optimizations: `RemoveRecursivity`, `EtaReduce`
- `evaluateScriptRestricting` mode
- Advanced documentation
- `Data` JSON codec

### Changed

- optimized From/ToData instances to generate less code
- `PlutusV1Params`, `PlutusV2Params` are now classes with Long fields
- Removed type parameter from `Interval`
- bytesToHex uses lowercase hex characters

### Fixed

- memoryUsageInteger was wrong in certain cases
- sliceByteString wrong order of parameters
- Data CBOR encoding of large integers must be chunked by 64 byte chunks
- typo in `ConstAboveDiagonal` cost calculation implementation

## 0.6.1 (2024-04-18)

### Added

- compile `==`, `!=` operators as Plutus builtins for `Boolean`, `ByteString`, `String` and `Data`
  types
- `++` operator, `size` and `length` functions for `ByteString`

### Fixed

- wrong `ConstAboveDiagonal` cost calculation used in division

## 0.6.0 (2024-04-05)

### Added

- fast debruijned CEK machine implementation
- CEK machine benchmarking, runs on par with high performance Haskell implementation
- CEK machine execution budget calculation
- cost model for CEK machine loading from Cardano CLI and Blockfrost API JSONs
- updated to Scala 3.3.3
- updated to Scala.js 1.16.0, 3x reduction in JS bundle size

### Changed

- lots of internal refactoring

## 0.5.0 (2024-02-17)

### Added

- better error messages in the compiler
- Scalus Intermediate Representation (SIR) now has a version number (requires recompilation of
  existing SIR)
- SIR and UPLC pretty printers can print normal and syntax highlighted code using XTerm colors
- UPLC pretty printer outputs a better indented code
- added Blake2b 224 builtin implementation on JVM
- updated dependencies: Scala 3.3.2
- use uplc cli from Plutus 1.15.0.1

### Changed

- moved scalus.uplc.Data to scalus.builtin.Data
- renamed scalus.builtins to scalus.builtin

### Fixed

- Plutus Data Map can have duplicate keys; fixed Data CBOR codec to handle this case.
