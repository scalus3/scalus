# Emulator ledger rules at PV11: inventory, gaps and phases – design

Status: proposed, not implemented. Date: 2026-11-02. Branch: `claude/ledger-rules-gap-25d42d`.

## 1. Purpose and scope

The Scalus Emulator behaves like a small Cardano node for blocks, the mempool and forks (spec `2026-10-21-emulator-chain-design.md`). Its ledger runs only the transaction rules, and its only epoch rule moves donations into the treasury. So a dApp test can pass in the Emulator and fail on a real node, and some realistic scenarios cannot be tested at all. Examples:

- A PlutusV3 tx that spends a UTxO and also reads it as a reference input passes in the Emulator. At PV11 the node rejects it (section 5.4, `[TX-1]`).
- A vote on a gov action that does not exist passes in the Emulator. The node rejects it with `GovActionsDoNotExist` (section 5.8).
- A stake credential delegated to a pool never earns a reward. Tests use `addRewards` instead (section 6).
- A retired pool is never removed, and its deposit is never refunded (section 6).

This spec does four things:

1. It lists every Cardano ledger rule that applies at PV11 (Conway), with its source (section 4).
2. It compares each rule and each predicate failure with Scalus (sections 5 and 6).
3. It ranks the gaps by their impact on dApp tests (section 7).
4. It gives the data model (section 8), the normative rules (sections 9 to 15) and the phases (section 16). The phases are the work plan. There is no separate plan document.

In scope: the tx rules (LEDGER and below), the block body rules (BBODY), and the epoch rules (TICK, NEWEPOCH, EPOCH, SNAP, POOLREAP, RUPD, RATIFY, ENACT, HARDFORK).

Out of scope:

- Consensus: OCERT, OVERLAY, PRTCL, TICKN, UPDN, TICKF, nonces and the leader schedule (section 17).
- PV12 (Dijkstra) rules and Plutus V4.
- An N2C socket server and a random block schedule (chain spec section 18).

## 2. Definitions

| Term | Meaning |
|---|---|
| ledger | The Haskell `cardano-ledger` at tag `cardano-ledger-conway-1.22.1.0`, the pin of mainnet node 11.0.1. |
| PV11 | Protocol version with major 11. It is the highest Conway version (`libs/cardano-ledger-core/internal/Cardano/Ledger/Internal/Definition/Era.hs:132`). |
| Emulator | `EmulatorBase` and its JVM and JS subclasses, and `ImmutableEmulator`. |
| tx rules | The Scalus validators and mutators in `R/` that `EmulatorBase.processTransaction` runs. |
| epoch | `slotConfig.epochOf(slot)`. |
| boundary n | The first tick into epoch n. "At boundary n" means "during the transition from epoch n − 1 to epoch n". |
| epoch transition | One run of NEWEPOCH for one boundary. |
| epoch state | The new per-snapshot state of section 8.2: account state, snapshots, blocks made, pending reward update, pending ratify result, previous pparams. |
| account state | Treasury and reserves, the Haskell `ChainAccountState`. |
| snapshot (stake) | One of mark, set, go: the stake per credential, the delegations and the pool params at one boundary. Not the `LedgerSnapshot` of the chain spec, which this spec calls "ledger snapshot". |
| gov state | Proposals, committee, constitution and the four enacted roots (section 8.1). |
| performance model | `PoolPerformance.Ideal` or `PoolPerformance.Measured` (section 14). |
| matters for tests | A gap that lets a dApp test pass in the Emulator and fail on a node, or that makes a realistic scenario impossible to test. |
| faithful, low value | A gap that changes only the name or the number of reported failures, or a rare edge case. |
| consensus-only | A rule that checks consensus data that the Emulator does not produce. Skip. |

Path prefixes. Ledger, relative to `../cardano-ledger`:

- `C/` = `eras/conway/impl/src/Cardano/Ledger/Conway/`
- `B/` = `eras/babbage/impl/src/Cardano/Ledger/Babbage/`
- `A/` = `eras/alonzo/impl/src/Cardano/Ledger/Alonzo/`
- `S/` = `eras/shelley/impl/src/Cardano/Ledger/Shelley/`
- `K/` = `libs/cardano-ledger-core/src/Cardano/Ledger/`

Scalus, relative to the repo root:

- `R/` = `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/rules/`
- `L/` = `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/ledger/`
- `N/` = `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/`
- `SC/` = `scalus-core/shared/src/main/scala/scalus/cardano/ledger/`

## 3. Method

- Every ledger claim was read from source at the pin. The ledger checkout is at `226b002d5`, tag `cardano-ledger-conway-1.22.1.0`.
- Every Scalus claim was read at `416d76361` (master at the time of writing).
- The full research notes, with more line references, are outside the repo in the shared notes folder of this session: `ledger-utxo-inventory.md`, `ledger-certs-gov-inventory.md`, `ledger-epoch-block-inventory.md`, `scalus-tx-rules.md`, `scalus-state-emulator.md`.

Five PV gates differ between PV10 and PV11 (`C/Era.hs:174-202`, `S/Era.hs:157-160`):

| Gate | Condition | Effect at PV11 |
|---|---|---|
| `hardforkConwayBootstrapPhase` | major = 9 | false: no bootstrap relaxations |
| `hardforkConwayDisallowUnelectedCommitteeFromVoting` | major > 10 | GOV raises `UnelectedCommitteeVoters` |
| `hardforkConwayDELEGIncorrectDepositsAndRefunds` | major > 10 | DELEG raises `DepositIncorrectDELEG`, `RefundIncorrectDELEG` |
| `hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule` | major > 10 | withdrawal checks, drain and DRep expiry updates run in LEDGER, before CERTS |
| `hardforkConwayDisallowDuplicatedVRFKeys` | major > 10 | POOL raises `VRFKeyHashAlreadyRegistered` |

Other PV11 behaviour: PlutusV3 TxInfo rejects common inputs and reference inputs (`C/TxInfo.hs:488-493`); `ppuWellFormed` rejects `nOpt = 0` (`C/PParams.hs:951-952`); BBODY counts reference scripts of earlier txs in the block (`C/Rules/Bbody.hs:369-382`); the HARDFORK rule into PV11 fills `psVRFKeyHashes` (`C/Rules/HardFork.hs:77-78`).

## 4. Rule inventory

Class: T = matters for tests, F = faithful, low value, X = consensus-only, N/A = not applicable to an Emulator.
Scalus: I = implemented, P = partial, M = missing.

| Rule | Ledger transition | Reads | Writes | Failures | Scalus | Class |
|---|---|---|---|---|---|---|
| CHAIN header checks | `eras/shelley/impl/src/Cardano/Ledger/Chain.hs:62-77` | pparams, header view | – | 3, section 5.10 | P: forge keeps body size in limit (`N/EmulatorState.scala:200-209`) | F |
| BBODY | `C/Rules/Bbody.hs:306-356`, `A/Rules/Bbody.hs:189-258` | pparams, UTxO, txs | ledger state, blocks made | 7, section 5.10 | P: ex-units and body size in forge (`N/emulator/BlockForge.scala:77-82,128-129`); no ref-script total, no blocks made | T |
| LEDGERS | `S/Rules/Ledgers.hs:193-202` | – | ledger state | `LedgerFailure` wrapper | I: `N/EmulatorState.scala:289-305` folds txs | – |
| MEMPOOL | `C/Rules/Mempool.hs:103-137` | UTxO | – | `ConwayMempoolFailure` | I: revalidation drops `InputsSpent` (chain spec section 10) | – |
| LEDGER | `C/Rules/Ledger.hs:381-470` | treasury, accounts, gov state | UTxO state, cert state, proposals | 9, section 5.1 | P: no GOV, no DRep expiry bump (`R/CardanoMutator.scala:8`) | T |
| UTXOW | `B/Rules/Utxow.hs:328-390` | UTxO, pparams | – | 20, section 5.2 | I | T |
| UTXO | `C/Rules/Utxo.hs:247-251`, `B/Rules/Utxo.hs:365-434` | UTxO, pparams | UTxO, fees, deposited, instant stake | 23, section 5.3 | P: `deposited` never written (`R/Entities.scala:31`) | T |
| UTXOS | `C/Rules/Utxos.hs:253-259` | pparams | donation | 3 + 14 context errors, section 5.4 | P: PV11 V3 overlap missing | T |
| CERTS | `C/Rules/Certs.hs:208-251` | cert state | cert state | 1 at PV11 (wrapper) | I: `R/CertsMutator.scala:14` | – |
| CERT | `C/Rules/Cert.hs:201-232` | – | – | wrappers | I | – |
| DELEG | `C/Rules/Deleg.hs:180-304` | accounts, pools, DReps | accounts, reverse delegations | 8, section 5.5 | P: `DRepState.delegates` never written (`R/CertsMutator.scala:210-214`) | T |
| POOL | `S/Rules/Pool.hs:213-328` | pools, VRF map | pools, future params, retiring, VRF map | 6, section 5.6 | P: 2 failures missing | T |
| GOVCERT | `C/Rules/GovCert.hs:173-279` | DReps, committee, proposals | DReps, committee state | 6, section 5.7 | P: no committee state (`R/CertsMutator.scala:207`) | T |
| GOV | `C/Rules/Gov.hs:456-643` | proposals, cert state, committee, constitution | proposals, votes | 20, section 5.8 | M: only the deposit (`R/ProposalDepositValidator.scala:11`) | T |
| TICK | `S/Rules/Tick.hs:260-279` | all | all | none (`Void`) | P: only donation move (`N/EmulatorBase.scala:608-621`) | T |
| NEWEPOCH | `C/Rules/NewEpoch.hs:165-192` | epoch state | epoch state | none | M | T |
| EPOCH | `C/Rules/Epoch.hs:276-379` | all | all | none | P: only step E9 donations | T |
| SNAP | `S/Rules/Snap.hs:76-103` | UTxO, accounts, pools | snapshots, fee snapshot | none | M (`L/StakeDistribution.scala` is not SNAP, section 6) | T |
| POOLREAP | `S/Rules/PoolReap.hs:132-229` | pools, accounts | pools, accounts, treasury, deposited | none | M (`R/CertsMutator.scala:144-146`) | T |
| RUPD + reward calc | `S/Rules/Rupd.hs:112-163`, `S/LedgerState/PulsingReward.hs:99-299` | go snapshot, blocks made, reserves, fees | reward update | none | M (`L/RewardsCalculation.scala` is not RUPD, section 6) | T |
| reward application | `S/LedgerState/IncrementalStake.hs:95-169` | accounts | accounts, treasury, reserves, fees | none | M | T |
| DRep pulser | `C/Governance/DRepPulser.hs:208-425`, `C/Governance.hs:460-515` | accounts, instant stake, pools, DReps, proposals | ratify state | none | M | T |
| RATIFY | `C/Rules/Ratify.hs:318-360` | ratify env | ratify state | none | M | T |
| ENACT | `C/Rules/Enact.hs:83-134` | enact state | enact state | none | M | T |
| HARDFORK | `C/Rules/HardFork.hs:70-128` | epoch state | VRF map (PV11), DRep delegs (PV10) | none | M | F |
| TICKF | `C/Rules/Tickf.hs:60-92` | – | forecast | none | N/A | X |

MIR, UPEC, NEWPP, PPUP and DELEGS are `VoidEraRule` in Conway (`C/Era.hs:58-86`). So no MIR exists, and only the reward update changes the reserves.

**Why** the epoch rules have no failures: TICK, NEWEPOCH, EPOCH, SNAP, POOLREAP, RUPD, RATIFY, ENACT and HARDFORK all have `Void` predicate failures (`S/Rules/Tick.hs:106`, `C/Rules/NewEpoch.hs:120`, `C/Rules/Epoch.hs:182`, `S/Rules/Snap.hs:63`, `S/Rules/PoolReap.hs:110`, `S/Rules/Rupd.hs:90`, `C/Rules/Ratify.hs:101`, `C/Rules/Enact.hs:75`, `C/Rules/HardFork.hs:63`). Their gaps are wrong state, not missing rejections.

## 5. Gap table: tx and block predicate failures

Status: I = implemented, P = partial, M = missing, N/A = not raised at PV11 or not applicable. Phase refers to section 16.

General difference: the ledger collects every failure (`libs/small-steps/src/Control/State/Transition/Extended.hs:703-721`). Scalus stops at the first failure and bundles several constructors into one exception (`R/Entities.scala:130-139`; `L/Entities.scala:8-515`). Pass or fail is the same. This is `[FL-1]`, phase 6.

### 5.1 LEDGER (`C/Rules/Ledger.hs:138-147`)

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `ConwayWdrlNotDelegatedToDRep` | `C/Rules/Ledger.hs:503-518` | `R/WithdrawalsDelegatedValidator.scala:14` | I | – |
| `ConwayTreasuryValueMismatch` | `C/Rules/Ledger.hs:472-484` | `R/TreasuryValueMismatchValidator.scala:11` | I; the treasury source changes in phase 3 | 3 |
| `ConwayTxRefScriptsSizeTooBig` | `C/Rules/Ledger.hs:486-501` | `R/TxRefScriptsSizeValidator.scala:20` | I | – |
| `ConwayWithdrawalsMissingAccounts` | `S/Rules/Ledger.hs:361-368` | `R/CertsValidator.scala:37` | I, bundled | – |
| `ConwayIncompleteWithdrawals` | `S/Rules/Ledger.hs:369` | `R/CertsValidator.scala:37` | I, bundled | – |
| `ConwayMempoolFailure` | `C/Rules/Mempool.hs:111-134` | mempool revalidation, chain spec `[RV-*]` | I (equivalent) | – |
| DRep expiry bump on vote, dormant bump | `C/Rules/Ledger.hs:419-420`, `C/Rules/Certs.hs:262-333` | none | M (state, not a failure) | 1 |

### 5.2 UTXOW (`C/Rules/Utxow.hs:77-131`)

All 17 raised constructors are implemented. Evidence: `R/VerifiedSignaturesInWitnessesValidator.scala:10`, `R/MissingKeyHashesValidator.scala:12`, `R/MissingOrExtraScriptHashesValidator.scala:12`, `R/NativeScriptsValidator.scala:12`, `R/ExactSetOfRedeemersValidator.scala:12`, `R/MissingRequiredDatumsValidator.scala:12`, `R/ProtocolParamsViewHashesMatchValidator.scala:15`, `R/ScriptsWellFormedValidator.scala:13`, `R/MetadataValidator.scala:12`. The 163 UTXO, UTXOW and UTXOS vector dirs pass in CI (`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/CardanoLedgerConformanceTest.scala:27-29`).

Not raised at PV11: `PPViewHashesDontMatch` (replaced by `ScriptIntegrityHashMismatch`, `A/Rules/Utxow.hs:300-302`), `MIRInsufficientGenesisSigsUTXOW` (`C/Rules/Utxow.hs:333-334`).

### 5.3 UTXO (`C/Rules/Utxo.hs:90-162`)

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `InputSetEmptyUTxO` | `S/Rules/Utxo.hs:437-438` | `R/EmptyInputsValidator.scala:9` | I | – |
| `BadInputsUTxO` | `S/Rules/Utxo.hs:466-474` | `R/AllInputsMustBeInUtxoValidator.scala:10` | I | – |
| `OutsideValidityIntervalUTxO` | `eras/allegra/impl/src/Cardano/Ledger/Allegra/Rules/Utxo.hs:247-249` | `R/OutsideValidityIntervalValidator.scala:8` | I | – |
| `OutsideForecast` | `A/Rules/Utxo.hs:393-401` | `R/OutsideForecastValidator.scala:23`, always passes | N/A: fixed `SlotConfig`, no horizon | – |
| `FeeTooSmallUTxO`, `ScriptsNotPaidUTxO`, `CollateralContainsNonADA`, `InsufficientCollateral`, `IncorrectTotalCollateralField`, `NoCollateralInputs` | `B/Rules/Utxo.hs:213-261` | `R/FeesOkValidator.scala:23` | I. A present but empty redeemer map runs the collateral checks (`R/FeesOkValidator.scala:60`), but Conway decoding rejects such a map (`A/TxWits.hs:571-572`), so no node tx reaches this case | – |
| `ValueNotConservedUTxO` | `S/Rules/Utxo.hs:518-523` | `R/ValueNotConservedUTxOValidator.scala:18` | I | – |
| `BabbageOutputTooSmallUTxO` | `B/Rules/Utxo.hs:326-346` | `R/OutputsHaveNotEnoughCoinsValidator.scala:10` | I | – |
| `OutputTooBigUTxO` (value size) | `A/Rules/Utxo.hs:428-447` | `R/OutputsHaveTooBigValueStorageSizeValidator.scala:10` | P: size not PV-dependent (TODO `:40`) | 6 |
| `OutputTooBigUTxO` (boot attrs) | `B/Rules/Utxo.hs:413-414`, over all sized outputs | `R/OutputBootAddrAttrsSizeValidator.scala:17` | I: the collateral return is checked (`[TX-4]`), and `ByronAddress.attributesSize` counts what `bootstrapAddressAttrsSize` counts: the decoded HD payload plus the unknown attributes (`libs/cardano-ledger-core/src/Cardano/Ledger/Address.hs:334-340`). Before phase 0 it counted the raw attribute-1 value, CBOR head included, and skipped unknown attributes | 0 (`[TX-4]`) |
| `WrongNetwork`, `WrongNetworkWithdrawal`, `WrongNetworkInTxBody` | `S/Rules/Utxo.hs:484-506`, `A/Rules/Utxo.hs:457-462` | `R/WrongNetwork*Validator.scala` | I | – |
| `MaxTxSizeUTxO` | `S/Rules/Utxo.hs:572-581` | `R/TransactionSizeValidator.scala:8` | I | – |
| `ExUnitsTooBigUTxO` | `A/Rules/Utxo.hs:475-481` | `R/ExUnitsTooBigValidator.scala:9` | I | – |
| `TooManyCollateralInputs` | `A/Rules/Utxo.hs:491-497` | `R/TooManyCollateralInputsValidator.scala:9` | I | – |
| `BabbageNonDisjointRefInputs` | `B/Rules/Utxo.hs:230-237`, PV 9–10 only | `R/InputsAndReferenceInputsDisjointValidator.scala:15` | N/A at PV11; gated right | – |
| `OutputBootAddrAttrsTooBig`, `OutputTooSmallUTxO`, `ExpiredUTxO`, `UpdateFailure` | never raised in Conway | – | N/A | – |

### 5.4 UTXOS (`C/Rules/Utxos.hs:72-83`)

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `ValidationTagMismatch` (FailedUnexpectedly) | `B/Rules/Utxos.hs:184-191` | `R/PlutusScriptsTransactionMutator.scala:54-64` | I | – |
| `ValidationTagMismatch` (PassedUnexpectedly) | `B/Rules/Utxos.hs:255-260` | `R/PlutusScriptsTransactionMutator.scala:46-52`, generic exception | I, generic error | 6 |
| `CollectErrors` `NoRedeemer`, `NoCostModel` | `A/Plutus/Evaluate.hs:185-188` | `L/PlutusScriptEvaluator.scala:594-617`, generic exception | I, generic error | 6 |
| `CollectErrors` V1/V2 context errors (13 constructors, `C/TxInfo.hs:142-180`) | `B/TxInfo.hs:118-322`, `C/TxInfo.hs:317-734` | `SC/LedgerToPlutusTranslation.scala`; the 25 IT vectors "Conway features fail in Plutus V1/V2" pass | I | – |
| `CollectErrors` `ReferenceInputsNotDisjointFromInputs` | `C/TxInfo.hs:488-493`, PlutusV3 at PV ≥ 11 | `SC/LedgerToPlutusTranslation.scala:698-713`, no check | **M** | 0 (`[TX-1]`) |

### 5.5 DELEG (`C/Rules/Deleg.hs:106-114`)

All 7 constructors raised at PV11 are implemented, bundled into one exception (`R/StakeCertificatesValidator.scala:152`). `IncorrectDepositDELEG` is not raised at PV11 (`C/Era.hs:184-185`). Gap: the vectors are not run (`[CV-1]`). State gap: `DRepState.delegates` is never written (`R/CertsMutator.scala:210-214`), phase 1. The pool delegator set (`spsDelegators`, `K/State/CertState.hs:281-298`) has no Scalus field and no phase: no rule reads it, the vector decoder discards it, and D5 computes the stake from the accounts (D22).

### 5.6 POOL (`S/Rules/Pool.hs:93-115`)

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `StakePoolNotRegisteredOnKeyPOOL`, `StakePoolRetirementWrongEpochPOOL`, `StakePoolCostTooLowPOOL`, `WrongNetworkPOOL` | `S/Rules/Pool.hs:235-326` | `R/StakePoolCertificatesValidator.scala:90` | I | – |
| `PoolMedataHashTooBig` | `S/Rules/Pool.hs:249-254` | none; `SC/PoolMetadata.scala:21` takes any length | **M** | 0 (`[TX-2]`) |
| `VRFKeyHashAlreadyRegistered` | `S/Rules/Pool.hs:269-271, 283-286`, PV11 | none; no VRF map in `L/Entities.scala:667` | **M** | 1 |

### 5.7 GOVCERT (`C/Rules/GovCert.hs:109-118`)

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `ConwayDRepAlreadyRegistered`, `ConwayDRepNotRegistered`, `ConwayDRepIncorrectDeposit`, `ConwayDRepIncorrectRefund` | `C/Rules/GovCert.hs:213-275` | `R/CertsMutator.scala:153` | I, inside the mutator | – |
| `ConwayCommitteeHasPreviouslyResigned` | `C/Rules/GovCert.hs:194-199` | `R/CertsMutator.scala`, `committeeCert` | I (phase 2) | 2 |
| `ConwayCommitteeIsUnknown` | `C/Rules/GovCert.hs:200-208` | same | I (phase 2) | 2 |

### 5.8 GOV (`C/Rules/Gov.hs:180-221`)

Scalus has no GOV rule. It checks only the deposit (`R/ProposalDepositValidator.scala:11`) and the voter witnesses (`L/utils/AllNeededKeyHashes.scala:77`). It stores no proposal and no vote.

| Failure | Ledger | Status | Phase |
|---|---|---|---|
| `ProposalDepositIncorrect` | `C/Rules/Gov.hs:534-541` | I | – |
| `GovActionsDoNotExist` | `:600`, `:617` | M | 2 |
| `MalformedProposal` | `:404-410`, `:513` | M | 2 |
| `ProposalProcedureNetworkIdMismatch` | `:544-546` | M | 2 |
| `TreasuryWithdrawalsNetworkIdMismatch` | `:551-555` | M | 2 |
| `DisallowedVoters` | `:375-387`, `:620` | M | 2 |
| `ConflictingCommitteeUpdate` | `:564-565` | M | 2 |
| `ExpirationEpochTooSmall` | `:567-568` | M | 2 |
| `InvalidPrevGovActionId` | `:576-578` | M | 2 |
| `VotingOnExpiredGovAction` | `:367-373`, `:619` | M | 2 |
| `ProposalCantFollow` | `:500-510`, `:685-707` | M | 2 |
| `InvalidGuardrailsScriptHash` | `:431-437`, `:558`, `:570` | M | 2 |
| `VotersDoNotExist` | `:605-616` | M | 2 |
| `ZeroTreasuryWithdrawals` | `:560-562` | M | 2 |
| `ProposalReturnAccountDoesNotExist` | `:515-519` | M | 2 |
| `TreasuryWithdrawalReturnAccountsDoNotExist` | `:520-530` | M | 2 |
| `UnelectedCommitteeVoters` | `:489-492`, `:664-677`, PV11 | M | 2 |
| `DisallowedProposalDuringBootstrap`, `DisallowedVotesDuringBootstrap` | `:389-454`, PV9 only | N/A | – |

### 5.9 Tx state effects

| Effect | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| UTxO, fees, donation, collateral path | `S/Rules/Utxo.hs:598-621`, `B/Rules/Utxo.hs:497-510`, `C/Rules/Utxos.hs:285` | `R/PlutusScriptsTransactionMutator.scala:32-109` | I | – |
| `utxosDeposited += deposits − refunds` | `S/Rules/Utxo.hs:607-615` | never written (`R/Entities.scala:31`) | M | 1 |
| Proposals and votes stored | `C/Rules/Ledger.hs:452-455` | none | M | 2 |
| Committee hot keys and resignation | `C/Rules/GovCert.hs:193-211` | none | M | 2 |
| DRep expiry with dormancy | `C/Rules/GovCert.hs:281-310` | `currentEpoch + dRepActivity` (`R/CertsMutator.scala:181-206`) | P | 1 |
| DRep expiry bump on vote | `C/Rules/Certs.hs:277-297` | none | M | 1 |
| Reverse delegations (`drepDelegs`, pool delegators) | `C/State/VState.hs:120-143`, `K/State/CertState.hs:281-298` | never written | M | 1 |
| Withdrawal drain before certs | `C/Rules/Ledger.hs:419-421` | `R/CertsMutator.scala:20-30` | I | – |

### 5.10 BBODY and CHAIN

| Failure | Ledger | Scalus | Status | Phase |
|---|---|---|---|---|
| `TooManyExUnits` | `A/Rules/Bbody.hs:245-249` | forge limit, `N/emulator/BlockForge.scala:77-82` | I | – |
| `WrongBlockBodySizeBBODY`, `InvalidBodyHashBBODY` | `A/Rules/Bbody.hs:201-223` | forge builds the header from the body (chain spec `[BH-3]`, `[BH-4]`) | I by construction | – |
| `BodyRefScriptsSizeTooBig` | `C/Rules/Bbody.hs:347-355, 369-382` | none | **M** | 0 (`[BB-1]`) |
| `HeaderProtVerTooHigh` | `C/Rules/Bbody.hs:320-346` | forge copies the PV from pparams (chain spec `[BH-7]`) | N/A | – |
| `BlockSizeTooLargeCHAIN` | `Chain.hs:73-75` | forge limit | I | – |
| `HeaderSizeTooLargeCHAIN` | `Chain.hs:70-72` | fixed-size mock header | F | 6 |
| `ObsoleteNodeCHAIN` | `Chain.hs:69` | – | X | – |

## 6. Gap table: state and data

| Data a rule needs | Ledger | Scalus today | Needed by |
|---|---|---|---|
| Reserves | `K/State/ChainAccountState` `casReserves` | none | RUPD, reward application |
| Treasury as state | `casTreasury` | `UtxoEnv.treasury` only (`R/Entities.scala:44-49`) | EPOCH, ENACT, POOLREAP |
| Stake snapshots mark, set, go, fee | `K/State/SnapShots.hs:187-465` | none; `L/StakeDistribution.scala:30` counts UTxO stake only, ignores reward balances and registration | SNAP, RUPD, RATIFY |
| Blocks made per pool | `nesBprev`, `nesBcur` | none; forged `issuerVkey` is fixed bytes (`N/emulator/BlockForge.scala:43`) | RUPD |
| Reward update | `nesRu` | none; `L/RewardsCalculation.scala` is a pro-rata split in `Double`, used only by its test | reward application |
| Previous pparams | `cgsPrevPParams` | none | RUPD |
| Exact rationals for rho, tau, a0 | `UnitInterval`, `NonNegativeInterval` | `Double` (`SC/ProtocolParams.scala:32,33,40`) | RUPD |
| Proposals with votes | `C/Governance/Proposals.hs` | `GovState = Array[Element]`, always empty (`L/Entities.scala:576`) | GOV, RATIFY, EPOCH |
| Committee, constitution, enacted roots | `C/Governance/Internal.hs:131-142` | none | GOV, GOVCERT, RATIFY, ENACT |
| Committee state | `K/State/CertState.hs:302-330` | commented out (`L/Entities.scala:630`) | GOVCERT, GOV, RATIFY |
| Dormant epochs | `vsNumDormantEpochs` | commented out (`L/Entities.scala:631`) | GOVCERT, LEDGER, EPOCH |
| VRF key map | `psVRFKeyHashes` | none | POOL, POOLREAP |
| `ProtocolParamUpdate` applied to pparams | `C/PParams.hs` `applyPPUpdates` | no `applyTo` (`SC/ProtocolParamUpdate.scala:13`) | ENACT |
| Mutable pparams | gov state | fixed at construction in `Context.env.params` | ENACT |
| Seeded DRep expiry | – | `expiry = 0L` (`N/EmulatorBase.scala:779`) | DRep expiry |

## 7. Impact on dApp tests

### 7.1 Matters for tests, ranked

| Rank | Gap | Effect on a test | Phase |
|---|---|---|---|
| 1 | No GOV rule, no gov state | Any vote and any proposal pass. Proposal deposits vanish. A governance dApp cannot test a proposal lifecycle at all. | 2, 5 |
| 2 | No rewards, no snapshots, no reserves | A staking dApp never sees a real reward. `addRewards` is the only way to fund an account. Liquid-staking and reward-sharing logic cannot be tested end to end. | 3, 4 |
| 3 | No POOLREAP, no future pool params | A retired pool stays registered. Its deposit never returns. A pool update never takes effect. Delegations to a retired pool stay. | 3 |
| 4 | PlutusV3 input and reference-input overlap at PV11 | A tx passes in the Emulator and fails on mainnet with `CollectErrors`. This affects a common pattern: spend a script UTxO that carries its own reference script. | 0 |
| 5 | No RATIFY, no ENACT | Treasury withdrawals, parameter changes, committee updates and hard forks never happen. Tests of a guardrail script or a treasury dApp stop at the proposal. | 5 |
| 6 | DRep expiry and dormancy wrong | A seeded DRep has expiry 0. Votes do not extend expiry. Ratify results would be wrong once phase 5 lands. | 1 |
| 7 | Missing tx checks: pool metadata hash size, duplicate VRF key, committee certs, block ref-script total | Txs pass that a node rejects. | 0, 1, 2 |

### 7.2 Faithful, low value

- Collect all failures and report Haskell constructor names (`[FL-1]`).
- PV-dependent value size (`R/OutputsHaveTooBigValueStorageSizeValidator.scala:40`).
- Distinct errors for `PassedUnexpectedly` and `CollectErrors`.
- `nonMyopic` pool ranking (`S/PoolRank.hs:126-175`). It changes no balance.
- Header size check.
- The pparams "point of no return" and pulsing schedules (section 12, decision D3).
- HARDFORK into PV10 (`updateDRepDelegations`). The Emulator starts at PV11.

### 7.3 Consensus-only, skip

OCERT, OVERLAY, PRTCL, TICKN, UPDN, TICKF, `ObsoleteNodeCHAIN`, nonces, leader schedule, KES. Section 17 lists them.

## 8. Data model

### 8.1 Tx-visible state (in `rules.State` and `CertState`)

These fields are read or written by tx rules, so they live in the ledger state that the tx rules thread.

- `VotingState` MUST gain `committeeState: Map[Credential, CommitteeAuthorization]`. `[DM-1]`
- `CommitteeAuthorization` MUST be one of `HotCredential(hot: Credential)` or `Resigned(anchor: Option[Anchor])`. `[DM-2]`
- `VotingState` MUST gain `numDormantEpochs: EpochNo`. `[DM-3]`
- `PoolsState` MUST gain `vrfKeyHashes: Map[VrfKeyHash, Long]`, the count of pools per VRF key. `[DM-4]`
- `rules.State.govState` MUST hold a `ConwayGovState` with the fields below. `[DM-5]`

| Field | Type | Ledger |
|---|---|---|
| `proposals` | insertion-ordered `GovActionState`s, keyed by `GovActionId` | `C/Governance/Proposals.hs:238-243`, `pProps` |
| `roots` | `GovRoots(pparamUpdate, hardFork, committee, constitution: Option[GovActionId])` | `pRoots` of `Proposals` (`C/Governance/Proposals.hs:240`); GOV reads them through `toPrevGovActionIds` (`C/Rules/Gov.hs:466`). They equal `ensPrevGovActionIds` of the enact state at each boundary |
| `committee` | `Option[Committee(members: Map[Credential, EpochNo], threshold: UnitInterval)]` | `C/Governance/Procedures.hs:544-549` |
| `constitution` | `Constitution` (exists, `SC/Types.scala:801`) | – |

- `GovActionState` MUST hold: `id`, `committeeVotes`, `drepVotes`, `spoVotes`, `procedure: ProposalProcedure`, `proposedIn`, `expiresAfter`. `[DM-6]`
- The CHANGELOG MUST list the type change of `State.govState` and `UTxOState.govState` in bold as a migration hazard. `[DM-5a]`
- The current pparams MUST stay in `Context.env.params`. `[DM-7]`

*Implementation status (phase 2, design):* `[DM-5]` the class is `GovState` in `scalus.cardano.ledger`, the Scalus name of the ledger's `ConwayGovState`, as `VotingState` is the name of `VState` (D27). It replaces the alias `type GovState = Array[Element]` under the same name, so `State.govState` and `UTxOState.govState` keep their names and only change type; `GovState.empty` has no proposal, empty roots, no committee and the constitution of `[ES-14]`. Fields, in this order: `proposals: Vector[GovActionState]`, `roots: GovRoots`, `committee: Option[Committee]`, `constitution: Constitution`. There is no per-purpose graph (D28): the ledger serializes a `Proposals` as the roots and the ordered map only (`C/Governance/Proposals.hs:385-388`) and rebuilds the graph from them (`mkProposals`, `:338-356`), so a parent check needs only the roots and the list. `GovState` has `proposal(id: GovActionId): Option[GovActionState]`, a linear scan, and the GOV operations of `[GV-6]`, `[GV-14]` and `[GV-15]` as methods; phase 3 and 5 add the removal and enactment operations to it. `GovRoots` is a case class of four `Option[GovActionId]` with `GovRoots.empty`; `GovActionPurpose` is an enum `PParamUpdate, HardFork, Committee, Constitution` with `GovActionPurpose.of(action: GovAction): Option[GovActionPurpose]` (`withGovActionParent`, `C/Governance/Procedures.hs:782-790`: NoConfidence and UpdateCommittee share `Committee`; TreasuryWithdrawals and InfoAction have none). `Committee` is `case class Committee(members: Map[Credential, EpochNo], threshold: UnitInterval)`. `[DM-6]` `GovActionState` is a case class with the seven fields in the ledger's order (`C/Governance/Procedures.hs:213-221`): `committeeVotes: Map[Credential, Vote]` keyed by the hot credential, `drepVotes: Map[Credential, Vote]`, `spoVotes: Map[PoolKeyHash, Vote]`, and `def action: GovAction = procedure.govAction`. `[DM-1]`, `[DM-2]` `committeeState` is the third field of `VotingState`, after `numDormantEpochs` (D29), and `CommitteeAuthorization` is an enum `HotCredential(hot: Credential)`, `Resigned(anchor: Option[Anchor])` (`K/State/CertState.hs:302-307`). All new types live in one file, `L/GovState.scala`, have no default parameters and have `empty` values where the ledger has a `Default`. None of them gets a production CBOR codec: nothing in production serializes a `State` (D36). MiMa: `scalus.cardano.ledger.rules` and `scalus.cardano.ledger` take per-symbol filters, with a comment, for `State` and `UTxOState` (`apply`, `copy`, the constructor, the product accessor of the field, the three default-argument methods of `State.govState`), for `GovState.empty` (result type) and for the 2-argument `VotingState.copy`; a deprecated `Array[Element]` constructor of `State` is not kept, since no body of it could mean anything.

*Implementation status (phase 2, code, gov-state-model):* `[DM-1]`, `[DM-2]`, `[DM-5]`, `[DM-6]` are in `L/GovState.scala` (`CommitteeAuthorization`, `Committee`, `GovActionState`, `GovRoots`, `GovState`) and `VotingState.committeeState`. `GovActionPurpose` and the GOV operations of `[GV-6]`, `[GV-14]`, `[GV-15]` come with GOV; no rule writes the new fields yet. `VotingState` has no deprecated 2-argument forms (D39): the 2-argument form never shipped, and MiMa reports no `VotingState` problem beyond the 1-argument `copy` filter of D17. MiMa reports 14 problems for the type change, all on `UTxOState`, `rules.State` and the companion object of the old alias (`Entities$package$GovState$`); `State.apply$default$5` is not among them. They are per-symbol filters with one comment in `build.sbt`. Tests: `ImmutableEmulatorTest` "keep every field of the ledger state" (one proposal with a vote, a committee and a hot key, phase 2 acceptance 5); the `assertSameSnapshot` helpers of `EmulatorForkTest`, `EmulatorChainTest` and `EmulatorMempoolTest` compare the snapshots with `==`, gov state included.

*Implementation status (phase 2, code, gov-conformance):* `[CV-2]`, `[CV-3]`, `[CV-7]`, `[CV-8]` are in the conformance test. `CardanoLedgerConformanceTest` runs the 132 GOV, EPOCH, RATIFY, ENACT and LEDGER dirs (a test pins the count), and all 361 conformance tests pass. `LedgerStateComparison` compares `govState.proposals` (in order; the detail names the ids, or each differing proposal), `govState.roots`, `govState.committee`, `govState.constitution` and `committeeState`; the `govState` exclusion and the two `dreps.expiry` exclusions of phase 1 are gone, and only `instantStake` stays excluded. A mutation that drops DRep key votes in `GovState.withVote` (not committed) fails 67 vectors with `govState.proposals`. `[CV-8]` is `CardanoLedgerVectors.caseEpoch`, refined by D43: a case runs at the largest of 899, the `[CV-8]` epoch of each case of its dir up to it, and the `proposedIn` of each proposal in its old state. The cases of a dir are now read in numeric order (`10` came before `2`). The verdict is strict: a pass needs `Right`, a fail needs `Left`, and a thrown exception matches neither; no case of the run dirs relied on a throw. A ledger state that does not decode gives `UndecodableState`, not a fail verdict, and its decode is inside the `Try`, so one such case no longer stops its dir. `expectedFailures` holds three entries, each with its cause: `ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal` cases 10 to 13 and `GOV.Voting.committee member can not vote on UpdateCommittee action` cases 2 and 3 (states with a `Word64` above `Long.MaxValue`, Q9), and `GOV.Voting.expired gov-actions` case 5, a vote three epochs after its proposal (`GovSpec.hs:788-798`) that leaves no trace in the dump; the ledger disables that test in its own conformance run (`disableInConformanceIt`, `S/ImpTest.hs:758-766`), and `GovMutatorTest` covers `VotingOnExpiredGovAction`. Correction to **Why** `[CV-2]`: consecutive cases do not always chain. Of 2 241 consecutive pairs in the run dirs, 151 differ (the new state, or the old one of a failed case, is not the next old state): `passEpoch` and Imp state edits between txs. Each case still carries its own old state, so no case needs a transition the harness does not run.

**Why** `[DM-7]`: every tx rule reads `env.params` today. ENACT changes pparams only at a boundary, and each boundary already builds a new env (`N/EmulatorBase.scala:608-621`). Moving pparams into the gov state would touch every validator.

### 8.2 Epoch state (in `LedgerSnapshot`, not seen by tx rules)

- `LedgerSnapshot` MUST gain `epoch: EpochState`. `[DM-8]`
- `EpochState` MUST hold the fields below. `[DM-9]`

| Field | Type | Ledger |
|---|---|---|
| `lastEpoch` | `EpochNo` | `nesEL` |
| `accountState` | `AccountState(treasury: Coin, reserves: Coin)` | `ChainAccountState` |
| `snapshots` | `SnapShots(mark, set, go: StakeSnapshot, fee: Coin)` | `K/State/SnapShots.hs` |
| `blocksMadePrev`, `blocksMadeCur` | `Map[PoolKeyHash, Long]` | `nesBprev`, `nesBcur` |
| `pendingRewards` | `Option[RewardUpdate]` | `nesRu` |
| `pendingRatify` | `Option[RatifyState]` | `cgsDRepPulsingState` |
| `prevParams` | `ProtocolParams` | `cgsPrevPParams` |

- `StakeSnapshot` MUST hold: stake per credential, pool delegation per credential, and the pool params per pool at the boundary. `[DM-10]`
- `RewardUpdate` MUST hold: `deltaT`, `deltaR`, `deltaF`, and the rewards per credential. `[DM-11]`
- `RatifyState` MUST hold: the enact state after the run, the enacted ids in order, the expired ids, and `delayed`. `[DM-12]`
- `UtxoEnv.treasury` MUST equal `epoch.accountState.treasury` in every ledger snapshot. `[DM-13]`

**Why** `[DM-13]`: `[SC-6]` of the provider spec keeps the treasury in the env for LEDGER. The account state becomes the source, and the env copies it at each boundary.

- New epoch types and functions MUST live in a new package `scalus.cardano.ledger.epoch`. `[DM-14]`
- `RewardsCalculation` and `StakeDistribution` MUST be deprecated, with a pointer to the new package. `[DM-15]`

**Why** `[DM-14]`, `[DM-15]`: `scalus.cardano.ledger.rules` is a MIXED MiMa package, so every new symbol there needs care. The old objects use `Double` and a wrong formula (section 6). Fixing them in place would change results under the same name.

### 8.3 Rationals

- The reward calculation MUST use exact rationals for `rho`, `tau` and `a0`. `[DM-16]`
- `[DM-17]` withdrawn by the owner decision on Q3 (2026-11-02). It derived rationals from `Double`.
- `ProtocolParams.monetaryExpansion` and `ProtocolParams.treasuryCut` MUST have type `UnitInterval` (`SC/UnitInterval.scala:18`). `[DM-18]`
- `ProtocolParams.poolPledgeInfluence` MUST have type `NonNegativeInterval` (`SC/NonNegativeInterval.scala:20`). `[DM-19]`
- The JSON readers of `ProtocolParams` MUST parse these three values from their decimal text, not through `Double`. `[DM-20]`

*Example, illustrative,* for `[DM-20]`: Blockfrost `"rho": 0.003` gives `UnitInterval(3, 1000)`. A `Double` path gives `0.003000000000000000062...`, which is not `3/1000`.

**Why** `[DM-18]`, `[DM-19]`: the ledger types are `UnitInterval` and `NonNegativeInterval`. The change breaks the binary API of `scalus-core`; the CHANGELOG MUST list it in bold as a migration hazard. `[DM-21]`

## 9. Tx parity rules (phase 0)

- The PlutusV3 TxInfo builder MUST fail with a collect error when `pvMajor ≥ 11` and inputs and reference inputs share an element. `[TX-1]`
- The failure of `[TX-1]` MUST name every common input. `[TX-1a]`
- The POOL check MUST reject a pool registration whose metadata hash is longer than 32 bytes. `[TX-2]`
- `[TX-3]` withdrawn. Conway decoding rejects an empty redeemer map (`A/TxWits.hs:571-572`), so the case cannot occur.
- `OutputBootAddrAttrsSizeValidator` MUST check the collateral return output too. `[TX-4]`

*Example, illustrative,* for `[TX-1]`, PlutusV3 spend of `u` with `u` also in reference inputs:

| PV | Ledger | Scalus today | Scalus after |
|---|---|---|---|
| 10 | `BabbageNonDisjointRefInputs` | reject | reject |
| 11 | `CollectErrors [BadTranslation (ReferenceInputsNotDisjointFromInputs [u])]` | accept | reject |
| 11, PlutusV2 script | accept | accept | accept |

**Why** `[TX-1]`: commit `123babebd` allowed the overlap outside PV 9 and 10, which matches the UTXO rule. The PlutusV3 context check (`C/TxInfo.hs:488-493`) was not ported. The TxBuilder now builds such txs (`SpendAndReferenceSameUtxoTest`), so a dApp test passes locally and fails on mainnet.

*Implementation status (phase 0):* `[TX-1]` `getTxInfoV3` throws an `IllegalArgumentException` that names the common inputs, sorted; the Plutus mutator turns it into a phase-1 rejection whatever `isValid` says, so no collateral is taken. The evaluator builds the `TxInfo` of every Plutus version the needed scripts use before it runs any script, as `collectPlutusScriptsWithContext` does (`eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:157-214`), so an earlier failing PlutusV1 or PlutusV2 script of a tx with `isValid = false` cannot turn the collect error into a collateral collection. The TxBuilder keeps allowing the overlap in its steps, since a PlutusV2 or native-script tx is valid with it at PV 11. For a PlutusV3 tx, `complete` evaluates the scripts and fails with a `BalancingException` that names the input. `[TX-2]` is a new `PoolMetadataHashTooBigException`, not a field of `StakePoolException`, to keep MiMa compatibility; the bundled `StakePoolException` is reported first when both apply.

*Implementation status (phase 0):* `[TX-4]` `ByronAddress.attributesSize` computes `bootstrapAddressAttrsSize`: the length of the HD payload that attribute 1 serializes as a CBOR bytestring, without its head (`eras/byron/ledger/impl/src/Cardano/Chain/Common/AddrAttributes.hs:109`, `:178-179`), plus the content bytes of every attribute whose key is not 1 or 2 (`Attributes.hs:140-142`); a repeated key keeps its last value (D13). The network magic does not count. An address that a node cannot decode has no node size; for it Scalus counts all bytes of a malformed attribute 1 and ignores a malformed attribute 2 (D14). `ByronAddress.derivationPath` still returns the raw attribute-1 value (D15).

### 9.1 Block body

- The forge MUST stop a block before the total reference-script size of its txs exceeds 1 MiB. `[BB-1]`
- `produceBlock(on, slot, txs)` MUST return `ForgeError.BlockTooBig` when the total of `[BB-1]` exceeds 1 MiB. `[BB-2]`
- At PV ≥ 11 the total MUST resolve each tx's inputs against the UTxO at the block start plus the outputs of earlier txs in the block. `[BB-3]`
- For an earlier tx with `isValid = false`, `[BB-3]` MUST add only its collateral return. `[BB-4]`

**Why** `[BB-3]`, `[BB-4]`: `C/Rules/Bbody.hs:369-382`. Spent inputs are not removed from the lookup. The per-tx size function is `C/UTxO.hs:171-175`.

*Implementation status (phase 0):* `BlockForge.refScriptsSize` ports `totalRefScriptSizeInBlock`, PV switch included: up to PV 10 every tx resolves on the UTxO at the block start. The forge limit and `[FG-18]` of `produceBlock(on, …)` check it with the body size and the execution units. `submit` holds every tx, `isValid` or not, to the per-tx reference-script limit of 200 KiB, as the node's mempool measure does (`txMeasureRefScripts`, ouroboros-consensus `Shelley/Ledger/Mempool.hs`): LEDGER skips that check when `isValid` is false, and the forge always takes the first mempool tx, so such a tx over 1 MiB would otherwise be forged into a block that breaks `[BB-1]`. The mempool capacity (chain spec `[MP-4]`) still measures only the body size and the execution units; the node's block capacity also has a reference-script dimension. The acceptance test of phase 0 item 3 uses six txs of 200 KiB, not three of 400 KiB: `ConwayTxRefScriptsSizeTooBig` caps a valid tx at 200 KiB, so three valid txs total at most 600 KiB. Five fit a block (1 024 000 bytes), the sixth forges in the next block.

### 9.2 Conformance harness

- `CardanoLedgerConformanceTest` MUST run the DELEG (24), CERTS (2) and GOVCERT (10) vector dirs. `[CV-1]`
- `CardanoLedgerConformanceTest` MUST run the GOV (56), EPOCH (13), RATIFY (46), ENACT (16) and LEDGER (1) vector dirs once phase 2 lands. `[CV-2]`
- Each phase MUST remove the `LedgerStateComparison` exclusions it makes true (`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/LedgerStateComparison.scala:39-50`). `[CV-3]`

*Implementation status (phase 0):* `[CV-1]` 33 of the 36 dirs pass. Four cases of three GOVCERT dirs need committee state (`ConwayCommitteeHasPreviouslyResigned`, `ConwayCommitteeIsUnknown`, section 5.7): "fails for.registering a resigned CC member hotkey" cases 3 and 6, "fails for.resigning a nonexistent CC member hotkey" case 1, "succeeds for.resigning a non-CC key" case 0. The test lists them as expected failures and fails when one of them starts to match, so phase 2 must remove them. The harness also failed open: the vectors are cached in the temp directory, and macOS had deleted the cached files, so every vector loaded no case and passed. The cache now records its file count, and a vector with no case fails.

**Why** `[CV-2]`: every vector is one LEDGER transition: the tx, the old ledger state and the new ledger state. Consecutive vectors chain with no gap (checked: `new(0) == old(1)` in "EPOCH.Proposals.Expired proposal deposit refunded"). So the EPOCH, RATIFY and ENACT dirs test the tx rules and the stored proposals of those scenarios. They do not test the boundary. Boundary acceptance comes from ported Imp scenarios (section 16).

| Exclusion | Removed in phase |
|---|---|
| `deposited` | 1 |
| `dreps.delegates` | 1 |
| `dreps.expiry` | 1 |
| `govState` | 2 |
| `instantStake` | 4, or never (decision D5) |

- The vector decoder MUST pass the decoded `numDormantEpochs` into `VotingState` (`LedgerState.scala:342-351` drops it today), and the decoded VRF map into `PoolsState` (the old 4-map layout has none and decodes to empty). `[CV-4]`
- The comparison MUST compare `pools.vrfKeyHashes`. `[CV-5]`

**Why** `[CV-4]`: without the counter the dormant bump of `[DR-2]` never fires in a vector, and the `dreps.expiry` exclusion cannot come off. **Why** `[CV-5]`: the vectors are PV10, where neither side writes the map (`[BK-4]`), so the comparison checks the PV gate for free (D23).

- The vector decoder MUST read the gov state of a vector as the seven-field `ConwayGovState` record (`C/Governance.hs:361-371`): the proposals as `[roots, [GovActionState…]]` (`C/Governance/Proposals.hs:385-388`), the committee and the constitution; it MUST ignore the current and previous pparams (hashes in the vectors), the future pparams and the DRep pulser. It MUST read the committee state of `VState` into `committeeState`. `[CV-6]`
- The comparison MUST compare `govState.proposals` in order, each with its id, votes, procedure and epochs, and `govState.roots`, `govState.committee`, `govState.constitution` and `committeeState`. `[CV-7]`

**Why** `[CV-6]`: checked on the vectors (2026-11-03, `GOV.Proposals.Consistency.Proposals are stored in the expected order/9`, `GOVCERT.fails for.resigning a nonexistent CC member hotkey/1`). A `StrictMaybe` in the state is `[]` or `[x]`: the four roots, the committee, the anchor of a `Resigned` committee entry (`K/State/CertState.hs:318-319`). Inside a stored `GovAction` the previous action id is `null`, as in a tx (`encodeNullStrictMaybe`), and so is an absent constitution guardrails hash (`C/Governance/Procedures.hs:936-941`), so the existing `GovAction`, `ProposalProcedure`, `GovActionId` and `Constitution` codecs read them. A `GovActionState` is a seven-element array in field order; votes are maps from `[tag, hash]` credentials or pool key hashes to `0 | 1 | 2` = No, Yes, Abstain, the `Vote` ordinals. A `Committee` is `[members, threshold]` with a tag-30 rational. A `CommitteeAuthorization` is `[0, credential]` or `[1, [] | [anchor]]`.

- The harness MUST derive the epoch of each case from its `newLedgerState`, in this order: the `proposedIn` of a proposal whose id carries the case tx's id; else, for a DRep that the tx registers, updates or votes with, its new expiry minus `dRepActivity` plus the `numDormantEpochs` of the old state; else 899. The slot is the first slot of that epoch. `[CV-8]`

**Why** `[CV-8]`: the dump carries no epoch, and the Imp tests pass epochs. Checked on the vectors (2026-11-03): 132 GOV, EPOCH, RATIFY, ENACT and LEDGER dirs hold proposals with `proposedIn` from 900 to 919 (`RATIFY.When CC expired.*` at 919, `GOV.Proposals.Consistency.Subtrees are pruned … over multiple rounds` at 906 and 913), and DRep expiries of 1000 and 1001 from votes at 901. At a fixed 899 the `govState.proposals` and `dreps.expiry` comparisons of `[CV-7]` fail in those cases, and `ExpirationEpochTooSmall` and `VotingOnExpiredGovAction` can flip a verdict. The new state is the only record of the epoch: `[GV-5]` writes it into every new proposal, and `[DR-1]`, `[DR-4]` into every DRep the tx touches; the DRep formula inverts `[DR-1]` with the counter the tx saw, which is the old counter when the tx has no proposal (a tx with a proposal takes the first branch). A case that touches neither has no observable epoch, and 899 is as good as any.

*Implementation status (phase 2, design):* the decoders are test-only, in `LedgerState.scala` of the conformance test; `LedgerState` and `UTxOState` derive decoders only, since nothing encodes a `GovState` (D36). The comparison names its fields `govState.proposals`, `govState.roots`, `govState.committee`, `govState.constitution`, `committeeState`; the `govState` exclusion goes. `[CV-8]` also retires the two `dreps.expiry` exclusions of phase 1, whose vote txs give its second branch (D38). The GOV, EPOCH, RATIFY, ENACT and LEDGER dirs of `[CV-2]` may still need an `expectedFailures` entry scoped by vector and case for a verdict the harness cannot replay (the treasury is 0 in every case), and a `LedgerStateComparison.Exclusion` scoped by vector name for a state field it cannot replay; each entry names its cause (D37).

*Implementation status (phase 2, code, gov-state-model):* `[CV-6]` is in `LedgerState.scala`: decoders for `GovState`, `GovRoots`, `GovActionState`, `Committee` and `CommitteeAuthorization`, and `VState` reads the committee state; the encoders of the test state classes are gone (D36). The gov state decoder reads the record length and skips the fields after the constitution, so a dump with full pparams in place of hashes still decodes. `GovStateDecoderTest` decodes every old and new state of every vector dir, except two whose stored proposal holds a `Word64` above `Long.MaxValue`: `ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal` (withdrawal amounts, `Coin` is a `Long`) and `GOV.Voting.committee member can not vote on UpdateCommittee action` (a threshold denominator of 10^19, `UnitInterval` is two `Long`s). A harness run of these dirs cannot decode those cases (the state, or the tx that makes the proposal) and needs a D37 `expectedFailures` entry; before this change the decoder skipped the gov state and decoded them. The comparison still compares `govState` as one field under the `govState` exclusion; `[CV-7]` splits it.

*Implementation status (phase 1, review round 1):* the comparison also covers `dreps.numDormantEpochs`. The decoder passes the counter in (`[CV-4]`), and the bump of `[DR-2]` shows in `dreps.expiry`, but the reset of `[DR-3]` shows only in the output counter: seven success vectors fire it (a valid tx with a proposal and a counter of 1 or 2), and 41 more carry a non-zero counter through unchanged. All run dirs pass with the compare.

## 10. Bookkeeping rules (phase 1)

- The tx rules MUST add the tx deposits to `State.deposited` and subtract the tx refunds, for `isValid = true`, computed against the cert state before CERTS. `[BK-1]`
- The deposits and refunds of `[BK-1]` MUST be the values that `TxBalance` uses (`L/utils/TxBalance.scala:33-109`), proposal deposits included. `[BK-2]`
- DELEG MUST keep `DRepState.delegates` equal to the set of accounts delegated to that DRep. `[BK-3]`
- POOL MUST keep `vrfKeyHashes` as `S/Rules/Pool.hs:276-310` does, and MUST NOT write it at PV < 11. `[BK-4]`
- POOL MUST reject a new pool whose VRF key is in `vrfKeyHashes` at PV ≥ 11. `[BK-5]`
- POOL MUST reject a re-registration whose VRF key differs from its own and is in `vrfKeyHashes` at PV ≥ 11. `[BK-6]`
- The Emulator MUST fill `vrfKeyHashes` from the seeded pools at genesis. `[BK-7]`
- `[BK-5]` and `[BK-6]` MUST run per certificate, in tx order, against the state the earlier certificates of the same tx produced. `[BK-8]`

**Why** `[BK-7]`: the ledger fills the map only on the hard fork into PV11 (`C/Rules/HardFork.hs:107-125`). An Emulator that starts at PV11 never sees that hard fork.

**Why** `[BK-8]`: the ledger runs one POOL step per certificate (`C/Rules/Certs.hs:249-251`). A tx that registers two new pools with one VRF key fails on the second.

*Implementation status (phase 1):* `[BK-1]` said "the UTXO mutator". That cannot work in Scalus: the ledger computes the deposit change against the cert state before CERTS (`S/Rules/Utxo.hs:607-608`; the refund of an unregistered account or DRep, and the pool deposit by `psStakePools` membership, `C/State/CertState.hs:121-123`; LEDGER passes that state to UTXOW on purpose, `C/Rules/Ledger.hs:457-466`). Scalus runs `CertsMutator` before `PlutusScriptsTransactionMutator` (`R/DefaultValidators.scala:53-55`), and `UtxoEnv.certState` is set when the `Context` is built, never refreshed per tx, and read by no rule, so it is stale in the Emulator. So `CertsMutator` writes `deposited`: it is the one mutator that holds the pre-CERTS state (D16). Details: a tx with proposals and no certificate changes `deposited` (`conwayTotalDepositsTxBody` adds `conwayProposalsDeposits`, `C/TxBody.hs:373-391`), so the update runs before the empty-certificates early return of `applyCertificates`; the change can be negative, and `Coin(Long)` has no sign check, so `deposited + deposits − refunds` in that order is exact; a tx with `isValid = false` leaves `deposited` as it is (the Babbage invalid path writes only the UTxO and the fees, `B/Rules/Utxo.hs:497-510`), which the `!event.isValid` early return of `CertsMutator` already gives.

*Implementation status (phase 1):* `[BK-4]` semantics as read from `S/Rules/Pool.hs:266-310`: a new pool does `Map.insert vrf 1`, an overwrite, not a count; every re-registration runs the update, whatever its VRF key: no future params → insert the new VRF key with 1; future params with a different VRF key → insert the new one and delete the future one, even when that is the VRF key of the current params; the same VRF key → no change; otherwise the VRF key of the current params stays in the map until POOLREAP (`S/Rules/PoolReap.hs:225-228`, phase 3). Only the hard-fork fill counts occurrences (`insertWith (+1)`, `C/Rules/HardFork.hs:107-125`), and `[BK-7]` copies that: two seeded pools with one VRF key give the count 2. At PV < 11 POOL does not touch the map (`updateVRFKeyHash = id`), so the PV10 vectors keep the map of their old state. The VRF key type is `VrfKeyHash` (`SC/Hashes.scala:126`), the type of `Certificate.PoolRegistration.vrfKeyHash`; the count is a `Long` (D18). The failure is a new `VrfKeyHashAlreadyRegisteredException`, not a field of `StakePoolException`, as the phase 0 `PoolMetadataHashTooBigException` (D19). The check runs in the POOL step of `CertsMutator`, which gains an error channel for it (`private[rules]`, no MiMa effect), gated on `context.env.params.protocolVersion.major >= 11` as `R/InputsAndReferenceInputsDisjointValidator.scala:14-15` does.

*Implementation status (phase 1, code):* `[BK-4]` to `[BK-8]` live in `CertsMutator.pool` (`R/CertsMutator.scala`); the deprecated `StakePoolCertificatesMutator` uses the same step, so its `Error` gains `VrfKeyHashAlreadyRegisteredException`. `[BK-6]` compares with the VRF key of the current params, not of the future ones (`S/Rules/Pool.hs:284-286`), so a second re-registration in one epoch with the same fresh VRF key fails: the first one put that key in the map. Tests pin this, and the delete of a future VRF key equal to the current one, in `CertsMutatorVrfKeyHashesTest`. `[BK-7]` fills the map at genesis whatever the protocol version (`EmulatorBase.buildInitialState`), counting the seeded pools only, since a genesis has no future params; at PV < 11 POOL neither reads nor writes the map, so the fill has no effect there, and a gate would be one more branch for no observable change. `ruleName` names the failure `VRFKeyHashAlreadyRegistered`, the ledger's spelling.

*Implementation status (phase 1):* `[BK-3]` writes `delegates` in the DELEG step as `C/Rules/Deleg.hs:324-373` and `C/State/VState.hs:120-143` do: a new vote delegation to a credential DRep adds the account; a changed one removes it from the old DRep and adds it to the new; `UnregCert` removes it; `AlwaysAbstain` and `AlwaysNoConfidence` have no set. The PV9 reverse-delegation bug (`processDelegationInternal (pvMajor pv < 10)`, ledger issue 4772) is not emulated (D20). `UnregDRepCert` keeps clearing the accounts by their `dRepDelegation` (`R/CertsMutator.scala:215-227`, spec `[SC-21]`), not by the `delegates` set (D21). The pool delegator set (`spsDelegators`) has no Scalus field and no phase (D22).

*Implementation status (phase 1, code):* `[BK-1]` and `[BK-2]` are `TxBalance.depositChange` (deposits from `conwayTotalDepositsTxCerts` and `producedProposalDeposits`, refunds from `conwayTotalRefundsTxCerts`, the functions of `consumedExact` and `producedExact`), added in `CertsMutator.transit` against the cert state before the withdrawal drain. The Emulator genesis starts `deposited` at the deposits of its seeded registrations (D26). `[BK-3]` is `CertsMutator.deleg`, which now works on the whole `CertState`; `applyCertificate` passes the DReps it writes on to GOVCERT, so a vote delegation and the DRep's unregistration in one tx clear both. The deprecated `StakeCertificatesMutator` writes `delegates` too, but not `deposited`. Tests: `CertsMutatorBookkeepingTest`, `EmulatorGenesisDepositedTest`; the conformance vectors cover both fields (with either update removed, 22 or more vectors fail).

### 10.1 DRep expiry

- GOVCERT MUST set the expiry of a registered or updated DRep to `currentEpoch + dRepActivity − numDormantEpochs`. `[DR-1]`
- LEDGER MUST run, for a valid tx with at least one proposal, the dormant bump of `C/Rules/Certs.hs:313-333` before CERTS. `[DR-2]`
- The dormant bump MUST add `numDormantEpochs` to the expiry of every DRep whose `expiry + numDormantEpochs ≥ currentEpoch`, then set `numDormantEpochs` to 0. `[DR-3]`
- LEDGER MUST set, after `[DR-2]` and before CERTS, the expiry of each registered DRep that votes in the tx by `[DR-1]`. `[DR-4]`
- `CertsMutator` MUST run `[DR-2]` and `[DR-4]` after the withdrawal drain and before the cert loop (`R/CertsMutator.scala:20-34`). `[DR-4a]`
- The Emulator MUST give a seeded DRep the expiry `genesisEpoch + dRepActivity`. `[DR-5]`
- At PV 9 a DRep registration MUST get the expiry `currentEpoch + dRepActivity`, without the dormant subtraction; an update subtracts at every PV. `[DR-1a]`
- The Emulator MUST seed `numDormantEpochs` with 0. `[DR-6]`

**Why** `[DR-1a]`: `computeDRepExpiryVersioned` (`C/Rules/GovCert.hs:281-309`) skips the subtraction in the bootstrap phase for `ConwayRegDRep` only; `ConwayUpdateDRep` and the vote bump call `computeDRepExpiry` directly. The PV10 vectors and the PV11 Emulator always subtract.

**Why** `[DR-6]`: the ledger genesis `VState` has `EpochNo 0` (`C/State/VState.hs:75-76`). Nothing increments the counter before the EPOCH rule of phase 3, so in the Emulator the dormant bump of `[DR-2]` is a no-op until then; the vectors and the unit tests of the example below exercise it.

*Implementation status (phase 1):* `[DR-5]` is `context.slotConfig.epochOf(context.env.slot) + context.env.params.dRepActivity` in `buildInitialState` (`N/EmulatorBase.scala:754-789`), no new API. `[DR-4]` changes only DReps that are in `dreps` (`Map.adjust`); an unregistered voter is a GOV failure of phase 2. The order of `[DR-2]`, `[DR-4]` and the drain does not matter: they write disjoint fields (`C/Rules/Ledger.hs:419-421` runs bump, vote, drain). The `numDormantEpochs` that `[DR-4]` reads is the value after `[DR-2]`, so a tx with proposals and votes uses 0.

*Implementation status (phase 1, code):* `[DM-3]` is `VotingState.numDormantEpochs`, with a deprecated 1-argument constructor and `apply`, `VotingState.empty` and one MiMa filter for the 1-argument `copy` (D17). `[DR-1]`, `[DR-1a]` are in `CertsMutator.govCert`, which takes the counter of the state it runs on; `[DR-1a]` gates on `protocolVersion.major == 9`, as `hardforkConwayBootstrapPhase` does. `[DR-2]` to `[DR-4a]` are `CertsMutator.dormantDRepExpiries` and `votingDRepExpiries`, run in `transit` before the cert loop for every valid tx; the ledger runs them in the empty-certificates base case of CERTS at PV ≤ 10 (`C/Rules/Certs.hs:228-245`), which is also before any certificate, so one code path serves both. A counter of 0 leaves the state as it is, as `updateDormantDRepExpiry` does. The expiry subtraction is not clamped at 0 (the ledger's `binOpEpochNo (-)` on `Word64`); it needs `numDormantEpochs > currentEpoch + dRepActivity`, which no state reaches. `[DR-5]`, `[DR-6]` are in `EmulatorBase.buildInitialState`. `[CV-3]`: the `dreps.expiry` exclusion is gone except for two `UTXOS.PlutusV3 Initialization.Updating CostModels` vectors, named in full (`and setting the govPolicy afterwards succeeds`, `with alwaysFails govPolicy does not validate`), whose DRep votes come after the Imp test passed epochs; the third CostModels vector (`but no PlutusV3 CostModels fails`) votes before its `passNEpochs` (`UtxosSpec.hs:516-534`) and passes with no exclusion; the dump has no epoch, the harness runs every case at epoch 899 (`CardanoLedgerVectors.impEpoch`), and the `[DR-4]` expiry is 2 to 6 epochs short. That is a harness limit, not a phase 3 gap: the exclusion is scoped by vector name and goes when the harness knows each case's epoch (`[CV-8]`, phase 2). Tests: `CertsMutatorDRepExpiryTest` (the example table, the PV 9 rule, the bump boundary, a vote together with a proposal, a registration after the bump), `EmulatorGenesisDRepExpiryTest`.

*Example, illustrative,* `dRepActivity = 20`:

| Event | Epoch | `numDormantEpochs` before | DRep expiry after |
|---|---|---|---|
| register | 100 | 0 | 120 |
| 3 epochs with no active proposal | 103 | 3 | 120 (stored) |
| vote | 103 | 3 | 103 + 20 − 3 = 120 |
| tx with a proposal | 104 | 3 | 120 + 3 = 123; dormant := 0 |

## 11. Governance tx rules (phase 2)

### 11.1 GOV

- The tx rules MUST include a GOV validator and a GOV mutator that implement `C/Rules/Gov.hs:456-643`. `[GV-1]`
- GOV MUST see the cert state after CERTS. `[GV-2]`
- The GOV checks MUST run in the mutator chain, after `CertsMutator`, as the GOVCERT DRep checks do today (`R/CertsMutator.scala:153`). `[GV-2a]`

**Why** `[GV-2a]`: `STS.Mutator.transit` runs all validators before all mutators (`R/Entities.scala:213-223`). A validator sees only the pre-tx state, so it cannot see same-tx registrations, which GOV accepts (`C/Rules/Ledger.hs:439-451`).

- GOV MUST check each proposal in tx order, with the checks of `C/Rules/Gov.hs:494-578`. `[GV-3]`
- GOV MUST give each proposal the id `(txId, index in the tx)`. `[GV-4]`
- GOV MUST set `proposedIn = currentEpoch` and `expiresAfter = currentEpoch + govActionLifetime`. `[GV-5]`
- GOV MUST accept a `prevActionId` equal to the purpose root or to a pending proposal of the same purpose, including earlier proposals of the same tx. `[GV-6]`
- GOV MUST reject a `ParameterChange` that fails `ppuWellFormed`, including `nOpt = 0` at PV ≥ 11. `[GV-7]`
- GOV MUST reject a `ParameterChange` or `TreasuryWithdrawals` whose policy hash differs from the constitution's guardrails hash. `[GV-8]`
- GOV MUST check votes only after it adds the tx's proposals. `[GV-9]`
- GOV MUST check `GovActionsDoNotExist`, `VotingOnExpiredGovAction` and `DisallowedVoters` only over known voters. `[GV-10]`
- GOV MUST use the voter permission table of `C/Governance/Internal.hs:381-540`. `[GV-11]`
- GOV MUST reject at PV ≥ 11 a committee vote whose hot credential is not authorized by a cold credential in `committee.members`. `[GV-12]`
- GOV MUST NOT use the member term in `[GV-12]`. `[GV-13]`
- GOV MUST replace an earlier vote of the same voter on the same action. `[GV-14]`
- GOV MUST remove the votes of a DRep that the tx unregisters from all proposals. `[GV-15]`
- The GOV mutator MUST NOT run for `isValid = false`. `[GV-16]`

*Example, illustrative,* for `[GV-10]`: an unknown DRep votes on an unknown action. The ledger raises only `VotersDoNotExist`, not `GovActionsDoNotExist` (`C/Rules/Gov.hs:603-614`).

*Example, illustrative,* for `[GV-13]`: a member whose term ended in epoch 50 still votes in epoch 60 without a GOV failure. RATIFY ignores the vote (`[RT-7]`).

Rules added in the phase 2 design round (agent, 2026-11-03), each read from `C/Rules/Gov.hs:456-643` at the pin:

- The checks of `[GV-3]` for one proposal MUST run in this order, and the first failure is the one Scalus reports: `ProposalCantFollow` (`[GV-17]`), `MalformedProposal` (`[GV-7]`), `ProposalReturnAccountDoesNotExist`, `TreasuryWithdrawalReturnAccountsDoNotExist`, `ProposalDepositIncorrect`, `ProposalProcedureNetworkIdMismatch`, then by action: TreasuryWithdrawals `TreasuryWithdrawalsNetworkIdMismatch`, `InvalidGuardrailsScriptHash`, `ZeroTreasuryWithdrawals`; UpdateCommittee `ConflictingCommitteeUpdate`, `ExpirationEpochTooSmall` (`[GV-18]`); ParameterChange `InvalidGuardrailsScriptHash`; then `InvalidPrevGovActionId` (`[GV-6]`). `[GV-3a]`
- The return-account checks of `[GV-3a]` MUST test registration against the accounts after CERTS, for the proposal's return account and for every withdrawal account of a TreasuryWithdrawals. `[GV-3b]`
- `ppuWellFormed` of `[GV-7]` MUST reject an update when any of these holds: `maxBlockBodySize`, `maxTxSize`, `maxBlockHeaderSize`, `maxValueSize`, `collateralPercentage`, `committeeTermLimit`, `governanceActionValidityPeriod`, `poolDeposit`, `governanceActionDeposit` or `drepDeposit` is given as 0; `adaPerUtxoByte` is given as 0 at PV ≥ 10; `nOpt` is given as 0 at PV ≥ 11; no field is given. `[GV-7a]`
- A voter MUST count as known for `[GV-10]` when: a committee hot credential is the `HotCredential` of any entry of `committeeState`, membership and term not required; a DRep credential is in `dreps` after CERTS; a pool id is in `stakePools` after CERTS. `[GV-10a]`
- GOV MUST check the votes in this order: at PV ≥ 11 `UnelectedCommitteeVoters` over every committee voter of the tx, before the proposals; then after the proposals `VotersDoNotExist`, `GovActionsDoNotExist`, `VotingOnExpiredGovAction` (`expiresAfter < currentEpoch`), `DisallowedVoters`. `[GV-10b]`
- A ParameterChange changes a security-group parameter for `[GV-11]` when the update gives any of: `minFeeA`, `minFeeB`, `maxBlockBodySize`, `maxTxSize`, `maxBlockHeaderSize`, `adaPerUtxoByte`, `maxBlockExUnits`, `maxValueSize`, `governanceActionDeposit`, `minFeeRefScriptCoinsPerByte`. `[GV-11a]`
- GOV MUST reject a HardForkInitiation whose version cannot follow its predecessor: when `prevActionId` equals the hard-fork root, or the proposed major is above the current major plus one, the predecessor is the current pparams version; otherwise it is the version of the pending HardForkInitiation that `prevActionId` names. `new` can follow `cur` when `new.major = cur.major + 1 ∧ new.minor = 0`, or `new.major = cur.major ∧ new.minor = cur.minor + 1`. `[GV-17]`
- `[GV-17]` MUST look the predecessor up in the proposals before the tx only; a HardForkInitiation earlier in the same tx is not consulted, and the proposal then passes to `[GV-6]`. `[GV-17a]`
- GOV MUST reject an UpdateCommittee that adds and removes one credential (`ConflictingCommitteeUpdate`), or adds a member whose term is at most `currentEpoch` (`ExpirationEpochTooSmall`). `[GV-18]`
- `TransactionException.ruleName` MUST return the ledger constructor name for every GOV failure: `GovActionsDoNotExist`, `MalformedProposal`, `ProposalProcedureNetworkIdMismatch`, `TreasuryWithdrawalsNetworkIdMismatch`, `DisallowedVoters`, `ConflictingCommitteeUpdate`, `ExpirationEpochTooSmall`, `InvalidPrevGovActionId`, `VotingOnExpiredGovAction`, `ProposalCantFollow`, `InvalidGuardrailsScriptHash`, `VotersDoNotExist`, `ZeroTreasuryWithdrawals`, `ProposalReturnAccountDoesNotExist`, `TreasuryWithdrawalReturnAccountsDoNotExist`, `UnelectedCommitteeVoters`. `ProposalDepositIncorrect` keeps its existing exception and name. `[GV-19]`
- GOV MUST read the committee, the constitution's guardrails hash and the roots from the gov state before the tx. `[GV-20]`

**Why** `[GV-17a]`: `preceedingHardFork` gets `st`, the state before the tx, not the accumulating proposals (`C/Rules/Gov.hs:500-510`). **Why** `[GV-10a]` ignores membership: GOV interns a committee voter against `authorizedHotCommitteeCredentials committeeState` (`C/Rules/Gov.hs:477, 607`), which `K/State/CertState.hs:334-340` builds from every non-resigned entry. **Why** `[GV-7a]`: `C/PParams.hs:934-957`; the bootstrap exemption of `adaPerUtxoByte` is PV 9 only. **Why** `[GV-11a]`: the `SecurityGroup` fields of `ConwayPParams` (`C/PParams.hs:643-708`). **Why** `[GV-20]`: LEDGER passes `govState ^. committeeGovStateL` and the constitution to `GovEnv`, and the proposals as the GOV state (`C/Rules/Ledger.hs:438-451`); only ENACT changes the committee and the constitution.

*Implementation status (phase 2, design):* `[GV-1]`, `[GV-2a]` GOV is one mutator, `GovMutator` in `R/`, the second element of `DefaultMutators.all` between `CertsMutator` and `PlutusScriptsTransactionMutator` (D30). It sees the cert state after CERTS, which `[GV-2]` needs, and the gov state before the tx, which `[GV-20]` needs: the mutator chain threads the `State`, and no earlier mutator writes `govState`. The validator part of `[GV-1]` is the existing `ProposalDepositValidator`; `GovMutator` does not repeat the deposit check, so a tx with a bad parent and a bad deposit reports the deposit first, which the ledger also reports (it collects all failures). `GovMutator` returns `success(state)` for `isValid = false` (`[GV-16]`). Its env is read from the arguments: `txId = event.id`, `currentEpoch = context.slotConfig.epochOf(context.env.slot)`, pparams and network from `context.env`, committee, guardrails hash and roots from `state.govState`, DReps, pools and accounts from `state.certState`. The hard-fork decoder limit `GovAction.MaxHardForkMajor` (`SC/GovAction.scala`) is a decoding bound, not `[GV-17]`: a PV10 vector proposes major 12 and the ledger rejects it with `ProposalCantFollow`, which only the rule gives. `[GV-19]` the failure is one `TransactionException.GovException(transactionId, failure: GovFailure)`, where `GovFailure` is an enum in `L/GovState.scala` whose cases carry the ledger constructor names and payloads (ids, voters with action ids, accounts, credentials, the two protocol versions, the two script hashes); `ruleName` returns the case name (D31). `ppuWellFormed` and the security-group test of `[GV-11a]` are private functions of `GovMutator`, not methods of `ProtocolParamUpdate` (D32). The committee votes map is keyed by the hot credential built from the `Voter`: `ConstitutionalCommitteeHotKey` to `Credential.KeyHash`, `ConstitutionalCommitteeHotScript` to `Credential.ScriptHash`; a `StakingPoolKey` becomes a `PoolKeyHash`.

*Implementation status (phase 2, code, gov-rule):* `[GV-1]` to `[GV-20]`, `[GV-2a]`, `[GV-3a]`, `[GV-3b]`, `[GV-7a]`, `[GV-10a]`, `[GV-10b]`, `[GV-11a]`, `[GV-17a]` are `R/GovMutator.scala`, the second element of `DefaultMutators.all`. `GovFailure` and `GovActionPurpose` are in `L/GovState.scala`, with the `[GV-6]`, `[GV-14]`, `[GV-15]` operations as `GovState.addProposal` (returns `None` for a bad parent), `withVote` and `withoutDRepVotes`; `GovRoots.root(purpose)` reads one root. `TransactionException.GovException(transactionId, failure)` carries the failure, and `ruleName` returns `failure.productPrefix`, the case name, which is a literal on every platform. The payloads are `List` for the ledger's `NonEmpty`, `Set` for `NonEmptySet` and `Map` for `NonEmptyMap`. The PV 9 exemptions of the ledger are kept: at PV 9 the return-account checks, `ZeroTreasuryWithdrawals` and the `adaPerUtxoByte` condition of `[GV-7a]` are skipped (D40); the bootstrap failures themselves stay N/A. `[GV-9]`, the same-tx parent of `[GV-6]` and `[GV-17a]` cannot be reached by a real tx: a proposal id holds the tx id, which is the hash of the body that names it. `GovMutatorTest` drives them through the `private[rules]` overload `GovMutator.transit(context, state, event, txId)`, which `transit` calls with `event.id`. Tests: `GovMutatorTest` (every failure, the order of `[GV-3a]` and `[GV-10b]`, the permission table of section 15, the examples above, `[GV-12]` at PV 10 and 11), `EmulatorGovTest` (phase 2 acceptance 4), `WithdrawalsTest` (the mutator order). A scratch run of the GOV, RATIFY, ENACT, EPOCH and LEDGER dirs at the fixed epoch 899 (not committed; running them is `[CV-7]`, `[CV-8]`) gave 1772 cases with 3 verdict mismatches: `GOV.Voting.expired gov-actions` case 5, an epoch case that `[CV-8]` fixes, and two `RATIFY.Voting.Active voting stake.*.Rewards contribute …` cases that fail a POOL retirement-epoch check at 899, not GOV. The two vectors with Word64 values above `Long.MaxValue` still throw in the decoder (gov-state-model report). Every negative case in those dirs fails with the failure its test name gives, for example `ProposalReturnAccountDoesNotExist` for "Fails with invalid network ID in proposal return address", which is the first of the two failures the ledger collects there (`GovSpec.hs:1044-1050`).

### 11.2 GOVCERT committee checks

- GOVCERT MUST reject `AuthCommitteeHotCert` and `ResignCommitteeColdCert` for a cold credential whose state is `Resigned`. `[GC-1]`
- GOVCERT MUST reject them for a cold credential that is not in `committee.members` and not added by a pending `UpdateCommittee`. `[GC-2]`
- GOVCERT MUST overwrite the cold credential's state with the new authorization. `[GC-3]`
- A pending `UpdateCommittee` for `[GC-2]` is one in the proposals before the tx; an `UpdateCommittee` proposed in the same tx does not count. `[GC-2a]`
- `[GC-1]` MUST be checked before `[GC-2]`, and `ruleName` MUST return `ConwayCommitteeHasPreviouslyResigned` and `ConwayCommitteeIsUnknown` for them. `[GC-4]`

**Why** `[GC-2a]`: LEDGER computes `committeeProposals` from the gov state before GOV runs (`C/Rules/Ledger.hs:398-400`) and passes it down CERTS and CERT to GOVCERT (`C/Rules/Certs.hs:222, 251`, `C/Rules/Cert.hs:232`). **Why** `[GC-4]`: `C/Rules/GovCert.hs:191-210`.

*Implementation status (phase 2, design):* the checks are in `CertsMutator.govCert`, which is `private[rules]`: it takes the whole `VotingState` instead of the `(dreps, accounts)` tuple, plus the pre-tx `GovState` for the committee and the committee-purpose proposals, and its error type widens to `TransactionException`. The failures are two new exceptions, `CommitteeHasPreviouslyResignedException(transactionId, coldCredential)` and `CommitteeIsUnknownException(transactionId, coldCredential)`, not fields of `DRepException` (D19, D31). `[CV-1]`: the four GOVCERT expected failures of phase 0 come off the list.

*Implementation status (phase 2, code, govcert-committee):* the checks are a separate step, `CertsMutator.committeeCert(txId, govState)(committeeState, cert)`, `private[rules]`, run in the CERT step after `govCert`, not a widened `govCert` (D41). `govState` is `State.govState` as CERTS sees it, which is the pre-tx gov state because `CertsMutator` runs before `GovMutator` (D30), so `[GC-2a]` needs no extra code; the pending additions are the `addedMembers` of every `UpdateCommittee` proposal in `govState.proposals`, as `proposalsWithPurpose grCommitteeL` with the `UpdateCommittee` match of `C/Rules/GovCert.hs:201-206`. The exceptions are `TransactionException.CommitteeHasPreviouslyResignedException` and `CommitteeIsUnknownException`, each with `transactionId` and `coldCredential`. The four GOVCERT expected failures are off `CardanoLedgerConformanceTest`, and the harness now compares `committeeState` (`LedgerStateComparison`), so the GOVCERT and other dirs check `[GC-3]` against the vectors. The deprecated `VotingCertificatesMutator` still ignores committee certificates.

## 12. Epoch boundary (phase 3, completed by phases 4 and 5)

### 12.1 Tick

- The Emulator MUST run one epoch transition for each boundary that a slot change crosses, in order. `[EB-1]`
- The Emulator MUST NOT run an epoch transition for a slot change that crosses no boundary. `[EB-2]`
- The ticked tip MUST stay computed, not stored, as in chain spec `[NL-2]`. `[EB-3]`

*Example, illustrative,* for `[EB-1]`: `setSlot` from epoch 10 to epoch 13 runs the transitions for boundaries 11, 12 and 13. A pool with `retiring = 12` is removed at boundary 12. A proposal reward from boundary 11 is paid at boundary 12.

### 12.2 Order of one epoch transition

For boundary n, the epoch transition MUST run these steps in this order. Each step has its own ID. `[EB-4]`

| ID | Step | Ledger | Phase |
|---|---|---|---|
| `[EB-5]` | Apply `pendingRewards` (`[RW-10]`). | `C/Rules/NewEpoch.hs:172-178` | 4 |
| `[EB-6]` | SNAP: `go := set`, `set := mark`, `mark := snapshot of now`, `fee := State.fees`. | `S/Rules/Snap.hs:95-103` | 4 |
| `[EB-7]` | POOLREAP part 1: replace the params of each pool in `futureStakePoolParams`; empty that map. | `S/Rules/PoolReap.hs:151-168` | 3 |
| `[EB-8]` | POOLREAP part 2: for each pool with `retiring = n`, refund its deposit to its reward account if registered, else to the treasury. | `S/Rules/PoolReap.hs:172-222` | 3 |
| `[EB-9]` | POOLREAP part 3: clear the pool delegation of its delegators; remove the pool from `stakePools`, `retiring`, `deposits` and `vrfKeyHashes`. | `S/Rules/PoolReap.hs:221-228` | 3 |
| `[EB-10]` | Take `pendingRatify` (`[RT-1]`). | `C/Rules/Epoch.hs:301-304` | 5 |
| `[EB-11]` | Pay enacted treasury withdrawals to registered accounts; subtract only the paid sum from the treasury. | `C/Rules/Epoch.hs:216-249` | 5 |
| `[EB-12]` | Remove expired proposals and their descendants; for each enacted proposal, make it the root and remove its siblings and their descendants. | `C/Governance/Proposals.hs:492-556` | 3 (expiry), 5 (enactment) |
| `[EB-13]` | Set committee and constitution from the enact state; `prevParams := params`; `params :=` the enacted pparams. | `C/Rules/Epoch.hs:325-332` | 5 |
| `[EB-14]` | Refund the deposit of each removed proposal to its return account if registered, else to the treasury. | `C/Rules/Epoch.hs:186-200, 334-336` | 3 |
| `[EB-15]` | `numDormantEpochs += 1` when no remaining proposal has `expiresAfter ≥ n`. | `C/Rules/Epoch.hs:204-208` | 3 |
| `[EB-16]` | Keep in `committeeState` only the cold credentials of current members. | `C/Rules/Epoch.hs:426-430` | 3 |
| `[EB-17]` | Treasury += donation + unclaimed deposits of `[EB-8]` and `[EB-14]`; donation := 0. | `C/Rules/Epoch.hs:354-357` | 3 |
| `[EB-18]` | `deposited :=` the sum of all stake, pool, DRep and proposal deposits. | `C/Rules/Epoch.hs:358-363` | 3 |
| `[EB-19]` | Run HARDFORK when the major PV changed. | `C/Rules/Epoch.hs:374-378` | 5 |
| `[EB-20]` | Compute `pendingRatify` from the state after `[EB-19]` (`[RT-2]`). | `C/Rules/Epoch.hs:379` | 5 |
| `[EB-21]` | `blocksMadePrev := blocksMadeCur`; `blocksMadeCur := ∅`; `lastEpoch := n`. | `C/Rules/NewEpoch.hs:182-192` | 4 |
| `[EB-22]` | Compute `pendingRewards` from the state after `[EB-21]` (`[RW-1]`). | `S/Rules/Rupd.hs`, `S/LedgerState/PulsingReward.hs:284-299` | 4 |

- `[EB-8]` and `[EB-14]` MUST check registration against the accounts at that step. `[EB-23]`
- A phase that has not landed MUST leave its steps as identity. `[EB-24]`

**Why** `[EB-20]` and `[EB-22]` run at the boundary, not inside the epoch: both pulsers are pure functions of state that is fixed at the boundary at PV > 6 (`S/LedgerState/PulsingReward.hs:180-181`, `S/Era.hs:154-155`; `C/Governance.hs:471-515`). One computation gives the pulsed result. Decision D3.

**Why** `[EB-13]` takes the pparams from the enact state: the ledger reads them from `cgsFuturePParams`, which TICK makes definite only at the "point of no return" (`S/Rules/Tick.hs:150-157`; `C/Governance.hs:301-325`). A node always ticks there. The Emulator may not, because `setSlot` can jump. Applying the enact state gives what a node that ticked normally gives. Decision D3.

### 12.3 Worked example, illustrative

Epoch length 100 slots. Pool P registered at genesis epoch 0. A tx in epoch 2 retires P at epoch 4. Its reward account is unregistered in epoch 3.

| Boundary | Effect |
|---|---|
| 3 | nothing for P |
| 4 | `[EB-8]`: P's account is unregistered, so the 500 ADA deposit goes to the treasury by `[EB-17]`. `[EB-9]`: P is removed; its delegators have no pool. |

## 13. Rewards (phase 4)

### 13.1 Snapshot

- SNAP MUST compute the mark snapshot from the UTxO, the accounts and the pools at the boundary. `[SN-1]`
- The stake of a credential MUST be the coin of base-address outputs with that stake credential plus its reward balance. `[SN-2]`
- SNAP MUST include only registered credentials delegated to a registered pool. `[SN-3]`
- SNAP MUST drop credentials with zero stake. `[SN-4]`
- Pointer and enterprise addresses MUST add no stake. `[SN-5]`
- The total active stake MUST be 1 when the snapshot is empty. `[SN-6]`

**Why** `[SN-2]` to `[SN-6]`: `K/State/Stake.hs:220-246`, `C/State/Stake.hs:101-121`, `K/State/SnapShots.hs:276-278, 418-430`.

### 13.2 Calculation

- The reward calculation MUST compute `pendingRewards` from: `go`, `fee`, `blocksMadePrev`, `reserves`, `prevParams`. `[RW-1]`
- The reward calculation MUST use the formulas of the table below, with `floor` where shown. `[RW-2]`

| Quantity | Formula | Ledger |
|---|---|---|
| `expectedBlocks` | `floor(f · slotsPerEpoch)` | `S/LedgerState/PulsingReward.hs:127-129` |
| `eta` | `min(1, blocksMade / expectedBlocks)` (`Measured`); `1` (`Ideal`) | `:133-138` |
| `deltaR1` | `floor(eta · rho · reserves)` | `:121-125` |
| `rPot` | `fee + deltaR1` | `:139` |
| `deltaT1` | `floor(tau · rPot)` | `:140` |
| `R` | `rPot − deltaT1` | `:141` |
| `totalStake` | `maxLovelaceSupply − reserves` | `:144, 304-306` |
| `sigma`, `sigmaA` | `poolStake / totalStake`, `poolStake / totalActiveStake` | `S/Rewards.hs:392-393` |
| `p`, `s` | `pledge / totalStake`, `ownerStake / totalStake` | `S/Rewards.hs:390-391` |
| `maxP` | `floor(R/(1+a0) · (σ' + p'·a0·(σ' − p'·(z0−σ')/z0)/z0))`, `z0 = 1/nOpt`, `σ' = min(σ, z0)`, `p' = min(p, z0)`; 0 if `pledge > ownerStake` | `K/State/SnapShots.hs:119-138`; `S/Rewards.hs:359-362` |
| `appPerf` | `(blocksN / max(1, blocksTotal)) / sigmaA` (`Measured`); `1` (`Ideal`) | `S/Rewards.hs:86-97` |
| `poolR` | `floor(appPerf · maxP)` | `S/Rewards.hs:364` |
| leader | `poolR` if `poolR ≤ cost`, else `cost + floor((poolR − cost)·(m + (1−m)·s/σ))` | `S/Rewards.hs:110-127` |
| member | 0 if `poolR ≤ cost`, else `floor((poolR − cost)·(1−m)·t/σ)`, `t = memberStake / totalStake` | `S/Rewards.hs:140-157` |
| `deltaR2` | `R − Σ rewards` | `S/LedgerState/PulsingReward.hs:242-280` |
| `deltaT`, `deltaR`, `deltaF` | `deltaT1`, `deltaR2 − deltaR1`, `−fee` | same |

- Under `Measured`, a pool with no block in `blocksMadePrev` MUST get no reward. `[RW-3]`
- Under `Ideal`, every pool in `go` with stake MUST be treated as a pool with `appPerf = 1`. `[RW-4]`
- A self-delegated owner MUST get no member reward. `[RW-5]`
- The leader reward MUST go to the pool's reward account. `[RW-6]`
- The calculation MUST sum the rewards of one credential. `[RW-7]`
- The calculation MUST omit a zero reward. `[RW-8]`
- The calculation MUST hold `deltaT + deltaR + Σ rewards + deltaF = 0`. `[RW-9]`

### 13.3 Application

- At the next boundary `[EB-5]` MUST add each reward to its account when the account is registered then. `[RW-10]`
- `[EB-5]` MUST add the rewards of unregistered accounts to the treasury. `[RW-11]`
- `[EB-5]` MUST add `deltaT` to the treasury, `deltaR` to the reserves and `deltaF` to `fees`. `[RW-12]`

### 13.4 Example, normative test vector for `[RW-2]`

Inputs: `reserves = 35 × 10^15`, `fee = 0`, `Ideal`, `rho = 3/1000`, `tau = 1/5`, `a0 = 3/10`, `nOpt = 500`. One pool: stake `10^13`, pledge 0, no owner stake, cost `170 × 10^6`, margin `1/100`. One member holds all `10^13`.

| Quantity | Value |
|---|---|
| `deltaR1` | 105 000 000 000 000 |
| `deltaT1` | 21 000 000 000 000 |
| `R` | 84 000 000 000 000 |
| `totalStake` | 10 000 000 000 000 000 |
| `maxP` = `poolR` | 64 615 384 615 |
| leader | 814 453 846 |
| member | 63 800 930 768 |
| `deltaR2` | 83 935 384 615 386 |

### 13.5 Timing, illustrative

A tx in epoch e delegates credential c to pool P.

| Boundary | Effect for c |
|---|---|
| e+1 | c is in `mark` |
| e+2 | c is in `set` |
| e+3 | c is in `go`; `[EB-22]` computes c's reward for epoch e+2 |
| e+4 | `[EB-5]` pays it |

## 14. Emulator surface

- `ChainConfig` MUST gain `poolPerformance: PoolPerformance`, with `Ideal` in `ChainConfig.instant` and `ChainConfig.manual`. `[ES-1]`
- `ChainConfig` MUST gain `activeSlotsCoeff: Rational`, with `1/20` in `ChainConfig.instant` and `ChainConfig.manual`. `[ES-2]`
- `ChainConfig` MUST gain `blockIssuer: BlockIssuer`, one of `Unattributed` or `Pool(poolId)`, with `Unattributed` in both presets. `[ES-3]`
- The forge MUST add one block to `blocksMadeCur` of the issuer pool. `[ES-4]`
- The forge MUST NOT count an `Unattributed` block. `[ES-5]`
- `EmulatorInitialState` MUST gain `reserves`, `committee`, `committeeHotKeys` and `constitution`. `[ES-6]`
- When `reserves` is not given, the Emulator MUST set it to 6 029 414 056 167 760 lovelace, mainnet at epoch 665 (Koios `/totals`). `[ES-7]`
- When the treasury is not given, the Emulator MUST set it to 1 392 896 975 262 771 lovelace, mainnet at epoch 665. `[ES-7a]`
- The Emulator MUST NOT create a UTxO for the ADA that is outside the Emulator. `[ES-7b]`

**Why** `[ES-7]` to `[ES-7b]`: owner decision on Q6 (2026-11-02). The formula takes `totalStake = maxLovelaceSupply − reserves` (`S/LedgerState/PulsingReward.hs:144, 304-306`). So with mainnet reserves, a test pool has the share of stake and the reward it would have on mainnet. A UTxO for the rest adds nothing when it is not delegated, and it outweighs every test voter when it is. Tests set other pools, stake and reserves through `EmulatorInitialState`.

*Example, illustrative,* mainnet pparams, `Ideal`, pool cost 170 ADA, margin 1 %:

| Delegated to the pool | Member reward per epoch |
|---|---|
| 10 000 ADA | 0 (pool reward 2.86 ADA, below the cost) |
| 1 000 000 ADA | 115 ADA |
| 10 000 000 ADA | 2 666 ADA |
- The Emulator MUST fill `mark`, `set` and `go` from the initial state at genesis. `[ES-8]`
- `EmulatorBase` MUST provide reads for: reserves, current epoch, current pparams, proposals, one gov action state, committee, constitution, one DRep state, the three snapshots, the pending reward update. `[ES-9]`
- Every new surface MUST follow the interop rules: no default parameters, `with<Field>` copy methods, Java and JS forms. `[ES-10]`
- Every Emulator factory MUST run the epoch rules. There MUST NOT be a flag that turns them off. `[ES-11]`
- The CHANGELOG MUST list `[ES-11]` in bold as a migration hazard: retiring pools are reaped, and delegated accounts earn rewards. `[ES-12]`
- When `EmulatorInitialState` gives no committee, the gov state MUST have no committee. `[ES-13]`
- When `EmulatorInitialState` gives no constitution, the constitution MUST have no guardrails script. `[ES-14]`

**Why** `[ES-11]`: owner decision on Q4 (2026-11-02). The Emulator is to behave like a node, and a node has no switch for epoch rules.

**Why** `[ES-13]`: owner decision on Q7 (2026-11-02). A test that needs ratification seeds a committee (`[ES-6]`). Without one, only NoConfidence and UpdateCommittee can pass (`[RT-6]`).

**Why** `[ES-1]`: in `Instant` each tx makes one block, at any slot, so `blocksMade / expectedBlocks` is close to 0. Under `Measured`, rewards would be close to 0. `Ideal` gives the rewards of a node where every pool is perfect, which is what a dApp test expects. `Measured` exists for tests of pool performance. Owner decision on Q1 (2026-11-02).

**Why** `[ES-8]`: without it, seeded stake earns its first reward at boundary 4 (section 13.5). With it, at boundary 2. Owner decision on Q2 (2026-11-02): the Emulator never starts from a real genesis, so it seeds the snapshots.

*Example, illustrative,* for `[ES-6]`: a test that wants a ParameterChange to pass seeds one committee member with a hot key and threshold `1/1`, and sets `dRepVotingThresholds` low. Without a committee the gov state is in no-confidence, and ParameterChange can never pass (`[RT-6]`).

- The Emulator MUST seed `committeeState` with a `HotCredential` entry per seeded hot key, and MUST reject a hot key whose cold credential is not a seeded committee member. `[ES-6a]`

**Why** `[ES-6a]`: GOVCERT rejects the same authorization with `ConwayCommitteeIsUnknown` (`[GC-2]`); a genesis must not reach a state a tx cannot reach.

*Implementation status (phase 2, design):* `[ES-6]` `EmulatorInitialState` gains `committee: Option[Committee]`, `committeeHotKeys: Map[Credential, Credential]` (cold to hot) and `constitution: Option[Constitution]` as fields 6 to 8, with the defaults `None`, `Map.empty`, `None` as the other five fields have; a deprecated 5-argument constructor and companion `apply` without defaults keep the 1.4 shape, and the 5-argument `copy` gets a MiMa filter (D33). A call that names only the old five fields resolves to the deprecated overload, since resolution prefers the alternative that needs no default, so `Builder.build()` and `JEmulator` pass all eight. The `Builder` is the Java form of `EmulatorInitialState` and gains `committee(Committee)`, `addCommitteeHotKey(cold: Credential, hot: Credential)` and `constitution(Constitution)`; the case class gets no `with<Field>` methods, as it has none today; `JsEmulatorOptions` gains `committee`, `committeeHotKeys` and `constitution`, credentials given as `credentialType` and `credentialHash` like `JDRepRegistration`, with the `ReversedMissingMethodProblem` filters the earlier options have; `scalus.d.ts` is regenerated. `[ES-13]`, `[ES-14]` `GovState.empty` holds `committee = None` and `Constitution(Anchor("", <32 zero bytes>), None)`; a seeded constitution replaces it whole. The genesis `State` is built in `EmulatorBase` with the gov state and `deposited`; `Emulator.withState` starts from it through `EmulatorState.initial(ledger: State, …)` and `EmulatorBase.fromState`, `buildInitialState` keeps its public tuple signature, and `EmulatorState.depositsOf` becomes `private[node]` (D34). `[ES-9]`, gov part: `EmulatorBase.govState: GovState`, `getGovState` in `EmulatorJavaApi`, and `getGovState()` in `JEmulator` as a JS object with the proposals in order (id, votes, action kind, `proposedIn`, `expiresAfter`), the committee and the constitution; a single proposal is `govState.proposal(id)`, a DRep state is `certState.vstate.dreps` (D35).

*Implementation status (phase 2, code, emulator-gov-surface):* `[ES-6]`, `[ES-6a]`, `[ES-13]`, `[ES-14]` and the gov part of `[ES-9]` are in the code as the design note above says, with these refinements. `reserves` of `[ES-6]` waits for phase 3 (`[ES-7]`). `EmulatorBase.buildInitialState` seeds `committeeState` with one `HotCredential` per hot key and throws `IllegalArgumentException` for a hot key whose cold credential is not a seeded member, also when no committee is seeded; JS `Emulator.create` turns it into a `TypeError`. `EmulatorBase.genesisLedger` builds the genesis `State`, and `EmulatorBase.fromInitialState` is the one body of `Emulator.withState` on both platforms (D34). The deprecated 5-argument forms carry the version `1.4.1`, the last tag. The Java form is `getGovState` returning the `GovState` itself, as `getProtocolParams` returns `ProtocolParams`. The JS shape is D42. `TxBuilder` does not collect the cold-key witness of `AuthCommitteeHotCert` and `ResignCommitteeColdCert` (`TransactionStepsProcessor.scala`, "not supported"), so `sign` drops the cold key's signature; an Emulator test adds the cold key with `requireSignature`. Rollback and mempool revalidation need no gov code: the snapshots carry `State.govState`, so a rollback removes a proposal and revalidation drops a vote on it with `GovActionsDoNotExist` (`EmulatorGovTest`).

## 15. Ratify and enact (phase 5)

- `[EB-20]` MUST run RATIFY over the proposals in insertion order, sorted stably by priority: NoConfidence, UpdateCommittee, NewConstitution, HardForkInitiation, ParameterChange, TreasuryWithdrawals, InfoAction. `[RT-1]`
- The ratify environment MUST use `reCurrentEpoch = n`, the new mark pool distribution, and the DRep distribution of `[RT-3]`. `[RT-2]`
- The DRep distribution MUST add, per account delegated to a DRep: its UTxO stake, its reward balance, and the deposits of proposals whose return account it is. `[RT-3]`
- The SPO distribution MUST add the proposal deposits of each delegator to its pool's stake. `[RT-4]`
- `[RT-5]` is split into `[RT-5a]` to `[RT-5e]`. RATIFY MUST enact an action only when all five hold.
- The action's parent MUST equal the root of its purpose; TreasuryWithdrawals and InfoAction have no parent. `[RT-5a]`
- For UpdateCommittee, every new member term MUST be at most `reCurrentEpoch + committeeMaxTermLength`. `[RT-5b]`
- No delaying action MUST have been enacted earlier in this run. `[RT-5c]`
- For TreasuryWithdrawals, the sum MUST be at most the treasury of the enact state. `[RT-5d]`
- The committee, the SPOs and the DReps MUST each accept by the threshold table below. `[RT-5e]`
- NoConfidence, UpdateCommittee, NewConstitution and HardForkInitiation MUST count as delaying actions. `[RT-5f]`
- RATIFY MUST treat the committee threshold as "never" when there is no committee or when the active committee is smaller than `committeeMinSize`. `[RT-6]`
- The committee ratio MUST exclude expired, resigned and unauthorized members, and abstain votes. `[RT-7]`
- The SPO ratio MUST count a non-voting pool as No for HardForkInitiation, and otherwise by its reward account's DRep delegation. `[RT-8]`
- The DRep ratio MUST ignore DReps with `reCurrentEpoch > expiry`. `[RT-9]`
- RATIFY MUST mark an action expired when `expiresAfter < reCurrentEpoch` and it is not enacted. `[RT-10]`
- ENACT MUST implement the table of `C/Rules/Enact.hs:83-134`. `[EN-1]`
- ENACT MUST apply a `ParameterChange` with `ProtocolParamUpdate.applyTo`. `[EN-2]`
- HARDFORK into PV11 MUST fill `vrfKeyHashes`. `[EN-3]`
- The Emulator MUST NOT enact a HardForkInitiation to major 12 or higher; RATIFY MUST treat it as not accepted, and the Emulator MUST log a warning. `[EN-4]`

**Why** `[EN-4]`: PV12 (Dijkstra) rules are out of scope. Running Conway rules under a PV12 label would give wrong results silently. Owner decision on Q8 (2026-11-02).

Thresholds, normative for `[RT-5e]` (`C/Governance/Internal.hs:337-540`). "never" = `NoVotingThreshold`, the action cannot pass. "auto" = `NoVotingAllowed`, the body counts as accepting.

| Action | Committee | SPO | DRep |
|---|---|---|---|
| NoConfidence | auto | `pvtMotionNoConfidence` | `dvtMotionNoConfidence` |
| UpdateCommittee | auto | `pvtCommitteeNormal`, or `pvtCommitteeNoConfidence` when there is no committee | `dvtCommitteeNormal`, or `dvtCommitteeNoConfidence` when there is no committee |
| NewConstitution | committee threshold | auto | `dvtUpdateToConstitution` |
| HardForkInitiation | committee threshold | `pvtHardForkInitiation` | `dvtHardForkInitiation` |
| ParameterChange | committee threshold | `pvtPPSecurityGroup` if a security-group param changes, else auto | maximum over the changed groups of network, economic, technical and gov thresholds (`:319-335`) |
| TreasuryWithdrawals | committee threshold | auto | `dvtTreasuryWithdrawal` |
| InfoAction | never | never | never |

- A threshold of 0 MUST count as accepted without a ratio (`C/Rules/Ratify.hs:130, 175, 234`). `[RT-11]`
- A ratio with a zero denominator MUST be 0 (`C/Rules/Ratify.hs:144, 208, 259`). `[RT-12]`
- The SPO ratio MUST be yes / (total active − abstain), weighted by pool stake with proposal deposits. `[RT-13]`
- For the SPO default vote of `[RT-8]`: AlwaysNoConfidence MUST count as Yes on NoConfidence and No otherwise; AlwaysAbstain MUST count as Abstain; any other delegation MUST count as No. `[RT-14]`
- The DRep ratio MUST count a non-voting active DRep as No, AlwaysNoConfidence as Yes on NoConfidence and No otherwise, and MUST exclude AlwaysAbstain. `[RT-15]`

Voter permission, normative for `[GV-11]` (`C/Governance/Internal.hs:381-540`):

| Action | CC | DRep | SPO |
|---|---|---|---|
| NoConfidence | no | yes | yes |
| UpdateCommittee | no | yes | yes |
| NewConstitution | yes | yes | no |
| HardForkInitiation | yes | yes | yes |
| ParameterChange | yes | yes | only if a security-group param changes |
| TreasuryWithdrawals | yes | yes | no |
| InfoAction | yes | yes | yes |

**Why** `[RT-1]` to `[RT-15]`: `C/Rules/Ratify.hs:136-360`, `C/Governance/Internal.hs:337-552`, `C/Governance/DRepPulser.hs:208-249`. Port these functions one by one; the order of checks matters.

*Example, illustrative,* lifecycle with `govActionLifetime = 6`:

| Epoch | Event |
|---|---|
| 10 | proposal submitted (`expiresAfter = 16`); votes cast |
| boundary 11 | `[EB-20]` ratifies it with `reCurrentEpoch = 11`, using the votes cast in epoch 10 |
| boundary 12 | `[EB-11]` to `[EB-14]` enact it and refund the deposit |

A vote cast in epoch 11 counts only from boundary 12. The last ratify attempt is at boundary 17; removal and refund happen at boundary 18.

## 16. Phases

Ordered by testing impact. Each phase lands as its own branch and keeps CI green.

### Phase 0 – tx parity fixes

- Scope: `[TX-1]`, `[TX-2]`, `[TX-4]`, `[BB-1]` to `[BB-4]`, `[CV-1]`.
- No state change.
- Acceptance:
  1. A failing test per fix on its parent commit (memory rule "fix needs a failing test"). For `[TX-1]`: a PlutusV3 spend with the same UTxO in inputs and reference inputs at PV11 fails; the same tx with a PlutusV2 script passes.
  2. The 36 DELEG, CERTS and GOVCERT dirs pass. Note: GOVCERT dirs with committee certs may need phase 2; list them as expected failures with a pointer to phase 2.
  3. A Manual-preset test with three 400 KiB ref-script txs forges two blocks.
- Risks: the TxBuilder may produce overlapping txs for PlutusV3 at PV11 by default (commit `123babebd`). Check `SpendAndReferenceSameUtxoTest` and change the builder if it does.

### Phase 1 – ledger bookkeeping

- Scope: `[DM-3]`, `[DM-4]`, `[BK-1]` to `[BK-8]`, `[DR-1]` to `[DR-6]`, `[DR-1a]`, `[DR-4a]`, `[CV-4]`, `[CV-5]`.
- Acceptance:
  1. `[CV-3]`: remove the `deposited`, `dreps.delegates` and `dreps.expiry` exclusions; all run dirs pass.
  2. Unit tests of the example in section 10.1.
  3. A PV11 test of a duplicate VRF key on a new pool and on a re-registration, and of two new pools with one VRF key in one tx (`[BK-8]`).
- Risks: `VotingState` and `PoolsState` are public case classes. New fields break `copy` and `apply` for MiMa. Precedent: `UtxoEnv` gained `treasury` with a deprecated old constructor, and `copy` broke (`2026-10-07-scalus-emulator-provider-design.md:345`). Decided in D17: the new primary constructors take no defaults; a deprecated constructor and a deprecated companion `apply` keep the 1.4 arity and defaults; `VotingState.empty` and `PoolsState.empty` replace the zero-argument calls; per-symbol MiMa filters with a comment for `copy` and for the synthetic default-argument methods that vanish, taken from `mimaReportBinaryIssues` output (baseline `scalusStableVersion = 1.4.0`). `committeeState` stays in phase 2 and costs a second forwarder round (D24).

### Phase 2 – governance tx rules

- Scope: `[DM-1]`, `[DM-2]`, `[DM-5]`, `[DM-5a]`, `[DM-6]`, `[GV-1]` to `[GV-20]`, `[GV-2a]`, `[GV-3a]`, `[GV-3b]`, `[GV-7a]`, `[GV-10a]`, `[GV-10b]`, `[GV-11a]`, `[GV-17a]`, `[GC-1]` to `[GC-4]`, `[GC-2a]`, `[ES-6]`, `[ES-6a]` (committee, hot keys, constitution), the gov parts of `[ES-9]`, `[ES-13]`, `[ES-14]`, `[CV-2]`, `[CV-6]`, `[CV-7]`, `[CV-8]`.
- Acceptance:
  1. The 56 GOV dirs and the tx steps of the EPOCH, RATIFY, ENACT and LEDGER dirs pass, with the harness epoch of `[CV-8]`, and with any `expectedFailures` or `Exclusion` entry scoped and explained (D37).
  2. `[CV-3]`: remove the `govState` exclusion, and the four GOVCERT expected failures of phase 0. The test decoder decodes the gov state by `[CV-6]` (`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/LedgerState.scala:10`).
  3. Unit tests for each PV11-only failure: `UnelectedCommitteeVoters`, `nOpt = 0`; and for `[GV-17a]`, `[GC-2a]` and `[GV-15]`, which the vectors cover thinly or not at all.
  4. An Emulator test: a vote by a seeded DRep on a missing action fails with `GovActionsDoNotExist`; the same vote by an unregistered DRep fails with `VotersDoNotExist` (`[GV-10b]`); a proposal and a vote on it by that DRep pass, and `govState.proposal(id)` shows the vote.
  5. `ImmutableEmulatorTest` "keep every field of the ledger state" round-trips a non-empty gov state (one proposal and a committee), since an empty one cannot fail the assertion.
- Risks: replacing `GovState = Array[Element]` changes a public type, with the filters of D27 and the CHANGELOG line of `[DM-5a]`. The vectors are PV10, so PV11-only checks have unit tests only. The harness replays every case with a treasury of 0 and, before `[CV-8]`, at epoch 899; `[CV-8]` lands first, and what it cannot derive takes D37 entries.

### Phase 3 – epoch boundary core

- Scope: `[DM-8]`, `[DM-9]` (account state, `lastEpoch`), `[DM-13]`, `[DM-14]`, `[EB-1]` to `[EB-4]`, `[EB-7]` to `[EB-9]`, `[EB-12]` (expiry only), `[EB-14]`, `[EB-15]`, `[EB-16]`, `[EB-17]`, `[EB-18]`, `[EB-23]`, `[EB-24]`, `[ES-7]`, chain-spec amendments of section 18.
- Acceptance (ported Imp scenarios, `eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/EpochSpec.hs`):
  1. "Expired proposal deposit refunded" (`:84`).
  2. "Proposals are expired and removed as expected" (`:107`).
  3. "deposit is moved to treasury when the reward address is not registered" (Treasury block, `:400`).
  4. "expiry is updated based on the number of dormant epochs" (DRep block, `:146`).
  5. Shelley Imp `EpochSpec` "Crosses epoch boundaries" (`eras/shelley/impl/testlib/Test/Cardano/Ledger/Shelley/Imp/EpochSpec.hs:39`).
  6. The example of section 12.3, and a pool re-registration that takes effect at the next boundary.
  7. The existing donation tests stay green (`EmulatorParityTest.scala:372-466`, `EmulatorForkTest.scala:427-462`).
- Risks: a jump of many epochs runs many transitions; measure a 1000-epoch `setSlot`. The tip view is ticked on every read (`N/EmulatorState.scala:96`), so after such a jump each read reruns the transitions; cache the ticked snapshot by (tip block id, slot) in `EmulatorState`. Existing tests that retire a pool and then cross its epoch change behaviour; list them in the CHANGELOG in bold as a migration hazard.

### Phase 4 – snapshots and rewards

- Scope: `[DM-9]` (snapshots, blocks, rewards, `prevParams`), `[DM-10]`, `[DM-11]`, `[DM-15]`, `[DM-16]`, `[DM-18]` to `[DM-21]`, `[EB-5]`, `[EB-6]`, `[EB-21]`, `[EB-22]`, `[SN-1]` to `[SN-6]`, `[RW-1]` to `[RW-12]`, `[ES-1]` to `[ES-5]`, `[ES-8]`, the stake parts of `[ES-9]`.
- Acceptance:
  1. The normative vector of section 13.4.
  2. A property test against a port of the reference `rewardOnePool` (`eras/shelley/test-suite/src/Test/Cardano/Ledger/Shelley/Rewards.hs:368-430`) with `d = 0`. The Shelley golden examples (`TwoPools.hs`) use `d = 0.5` (`Examples/Init.hs:51`), so they are not direct vectors.
  3. An Emulator test of section 13.5: delegate, cross four boundaries, withdraw the exact reward.
  4. A `Measured` test: an `Unattributed` chain pays no reward; a `Pool(P)` chain pays P.
  5. Optional IT test: one mainnet epoch with the it-data stake state (`StakeStateResolver`) against Koios account rewards.
- Risks: `Ideal` rewards change account balances in tests that cross epochs with delegated accounts; those that withdraw an exact seeded amount break (`[ES-11]`, `[ES-12]`). The rational pparam types break `scalus-core` binary compatibility (`[DM-21]`). Small test pools earn nothing for their members under mainnet reserves (`[ES-7]`); document the 600 000 ADA rule of thumb.

### Phase 5 – ratify and enact

- Scope: `[DM-12]`, `[EB-10]`, `[EB-11]`, `[EB-12]` (enactment), `[EB-13]`, `[EB-19]`, `[EB-20]`, `[RT-1]` to `[RT-15]`, `[EN-1]` to `[EN-4]`, mutable pparams.
- Acceptance (ported from `RatifySpec.hs` and `EnactSpec.hs`, names as in the vector dirs):
  1. ENACT "Treasury withdrawals.Modify EnactState as expected", "Withdrawals exceeding treasury submitted in several proposals within the same epoch".
  2. ENACT "Competing proposals.higher action priority wins", "proposals of same priority are enacted in order of submission".
  3. ENACT "Committee enactment.CC re-election", "NoConfidence", "Constitution", "futurePParams".
  4. RATIFY "Delaying actions.A delaying action delays all other actions even when all of them may be ratified in the same epoch".
  5. RATIFY "Expired and resigned committee members are discounted from quorum.Expired" and ".Resigned".
  6. RATIFY "Voting.Active voting stake.DRep.Proposal deposits contribute to active voting stake.Directly", "Rewards contribute to active voting stake".
  7. RATIFY "SPO default votes.After bootstrap phase.Reward account delegated to AlwaysNoConfidence".
  8. RATIFY "CommitteeMinSize affects in-flight proposals.TreasuryWithdrawal fails to ratify due to an increase in CommitteeMinSize".
  9. EPOCH "DRep.proposal is accepted after two epochs".
  10. The lifecycle example of section 15 as an Emulator test.
- Risks: the ratify rules are long and order-sensitive; port the Haskell function by function. `ProtocolParamUpdate.applyTo` covers about 30 fields; test each one.

### Phase 6 – faithful, low value

- Scope: `[FL-1]` collect all failures with Haskell names; PV-dependent value size; distinct `PassedUnexpectedly` and `CollectErrors`; header size; `nonMyopic`.
- Acceptance: the conformance test compares the failure constructors, not only pass or fail.
- Do this only on demand.

## 17. Consensus-only rules, skipped

| Rule | Ledger | Reason |
|---|---|---|
| OCERT | `libs/cardano-protocol-tpraos/src/Cardano/Protocol/TPraos/Rules/OCert.hs:31-50` | KES and op-cert counters; the forge writes fixed bytes |
| OVERLAY | `.../Rules/Overlay.hs:97-129` | VRF leader checks |
| PRTCL, TICKN, UPDN | `.../Rules/Prtcl.hs:112-218`, `Tickn.hs:65`, `Updn.hs:32` | nonces and header sequence; the chain spec keeps slot and block number invariants |
| TICKF | `C/Rules/Tickf.hs:60-92` | forecast for the leader schedule |
| `ObsoleteNodeCHAIN` | `Chain.hs:69` | node config |
| `adoptGenesisDelegs` | `S/Rules/Tick.hs:112-140` | no genesis delegation certs in Conway |

## 18. Chain-spec and provider-spec amendments

- `[EB-1]` supersedes chain spec `[NL-2]`: `tick` runs the epoch transitions, not only the donation move. `[CS-1]`
- `[EB-17]` supersedes provider spec `[SC-7a]` and `[SC-7f]`: the donation moves at each boundary; with several boundaries the later ones see 0, so the result is the same. `[CS-2]`
- `[DM-13]` keeps provider spec `[SC-6]`, with the account state as the source. `[CS-3]`
- Chain spec section 18 item 1 and item 2 are replaced by phases 3 to 5. `[CS-4]`

## 19. Decisions

| # | Decision | Rejected |
|---|---|---|
| D1 | One spec for all gaps; phases by testing impact | one spec per rule family, as chain spec section 18 planned |
| D2 | Pparams stay in `Context.env` (`[DM-7]`) | pparams in the gov state as in Haskell |
| D3 | Rewards and ratify computed at the boundary; no pulsing, no point of no return | port the pulsers and the TICK schedule |
| D4 | `Ideal` performance by default (owner, Q1) | `Measured` by default; a random block schedule |
| D5 | Stake computed from the UTxO at the boundary; no incremental instant stake | maintain `utxosInstantStake` per tx |
| D6 | New epoch code in `scalus.cardano.ledger.epoch`; old objects deprecated | fix `RewardsCalculation` in place |
| D7 | GOV before rewards | rewards first: `addRewards` is a workaround, GOV has none |
| D8 | Vectors test tx rules only; boundaries use ported Imp scenarios | extend the Haskell dumper now (Q5) |
| D9 | Rational pparam types (owner, Q3) | decimal-string conversion of `Double` |
| D10 | Epoch rules always on (owner, Q4) | an opt-in flag for one release |
| D11 | No default committee (owner, Q7) | a test committee at genesis |
| D12 | Reject a hard fork to PV12 (owner, Q8) | enact it under Conway rules |
| D13 | A repeated Byron attribute key keeps its last value, as the node's `Map` does (agent, phase 0) | sum every occurrence |
| D14 | A Byron address a node cannot decode (attribute 1 not one whole bytestring, attribute 2 not a Word32) gets a size: all bytes of attribute 1, none of attribute 2 (agent, phase 0) | throw from `attributesSize`. A node rejects such a tx at decoding, so no choice has node parity; the residual gap is that the Emulator accepts such an output when its size is 64 or less |
| D15 | `ByronAddress.derivationPath` keeps returning the raw attribute-1 value (agent, phase 0) | return the decoded HD payload, which changes a public accessor for no rule |
| D16 | `CertsMutator` writes `State.deposited`, since it is the one mutator that holds the pre-CERTS cert state (`[BK-1]`) (agent, phase1) | the UTXO mutator, which runs after CERTS and would miss the refunds of unregistered accounts and DReps; reordering `DefaultMutators.all`, against provider spec `[SC-3a]`; threading the pre-CERTS state through `Context` |
| D17 | `VotingState` and `PoolsState` gain fields with no defaults; a deprecated constructor and companion `apply` keep the 1.4 arity and defaults; `.empty` vals; per-symbol MiMa filters for `copy` and the vanished default-argument methods (agent, phase1) | a wildcard filter, which AGENTS.md forbids for `scalus.cardano.ledger`; defaults on the new primary constructor, against the interop rule |
| D18 | The VRF map is `Map[VrfKeyHash, Long]` with the existing `VrfKeyHash` of `SC/Hashes.scala:126` (agent, phase1) | a `NonZero Word64` wrapper type as in Haskell; the saturation at `maxBound` is unreachable |
| D19 | `VRFKeyHashAlreadyRegistered` is a new `VrfKeyHashAlreadyRegisteredException` (agent, phase1) | a new field of `StakePoolException`, which breaks its `apply`, `copy` and constructor |
| D20 | The PV9 reverse-delegation bug (ledger issue 4772, `processDelegationInternal (pvMajor pv < 10)`) is not emulated (agent, phase1) | port the `preserveIncorrectDelegation` branch; the Emulator runs PV11 and the vectors PV10 |
| D21 | `UnregDRepCert` keeps clearing accounts by their `dRepDelegation` scan, `[SC-21]` (agent, phase1) | clear by the `delegates` set as `C/Rules/GovCert.hs:237-257` does; equal only when the set is consistent, and `EmulatorTest.scala:867` hand-builds a state with an empty set |
| D22 | No pool delegator set (`spsDelegators`) in any phase (agent, phase1) | a `delegators` field on `PoolsState`; nothing reads it and D5 computes the stake from the accounts |
| D23 | The vector decoder passes `numDormantEpochs` and the VRF map through, and the comparison adds `pools.vrfKeyHashes` (`[CV-4]`, `[CV-5]`) (agent, phase1) | leave the map out of the comparison; the PV10 vectors then check nothing about the `[BK-4]` gate |
| D24 | `committeeState` stays in phase 2, at the cost of a second forwarder round on `VotingState` (agent, phase1) | add the field now, unexercised, against provider spec `[SC-19b]` |
| D25 | The duplicate-VRF check runs in the sequential POOL step of `CertsMutator`, which gains an `Either` result (`[BK-8]`) (agent, phase1) | a per-tx check in `StakePoolCertificatesValidator`, which misses two new pools with one VRF key in one tx |
| D26 | The Emulator genesis sets `deposited` to the sum of the deposits of the seeded accounts, pools and DReps, in the `CertState` overload of `EmulatorState.initial` (`[BK-1]`) (agent, phase1) | start at 0, which makes `deposited` negative after a refund of a seeded deposit; the `State` overload, whose caller owns `deposited` |
| D27 | The gov state class is `GovState`, the Scalus name of `ConwayGovState` as `VotingState` is of `VState`; it replaces the `Array[Element]` alias under the same name, so `State.govState` and `UTxOState.govState` keep their names and change type, covered by per-symbol MiMa filters and a bold CHANGELOG line (`[DM-5]`, `[DM-5a]`) (agent, phase2) | a `ConwayGovState` class with a deprecated `GovState` alias and companion: two names for one type; a deprecated `Array[Element]` constructor on `State`, which no body could give a meaning |
| D28 | Proposals are a `Vector[GovActionState]` in insertion order with a linear `proposal(id)`; the roots are four `Option[GovActionId]`; no per-purpose graph, the parent check is "equals the purpose root, or is an existing proposal of the same purpose" (`[GV-6]`) (agent, phase2) | a `VectorMap` keyed by id, whose order-insensitive equality hides an order bug in the comparison and the fork tests; a port of `PGraph`, which the ledger itself rebuilds from the roots and the ordered map |
| D29 | `committeeState` is the third field of `VotingState`, after `numDormantEpochs`, with a deprecated 2-argument constructor and `apply` and a filter for the 2-argument `copy` (`[DM-1]`) (agent, phase2) | the Haskell order `(dreps, committeeState, numDormantEpochs)`, which also changes the type of `_2` |
| D30 | GOV is one mutator, `GovMutator`, second in `DefaultMutators.all` between `CertsMutator` and `PlutusScriptsTransactionMutator`; `ProposalDepositValidator` stays as the validator part of `[GV-1]` and the mutator does not repeat the deposit check (agent, phase2) | a second GOV validator for the pure-tx checks, which duplicates the per-proposal loop; the checks inside `CertsMutator`, which mixes two ledger rules in one object |
| D31 | GOV failures are one `GovException(transactionId, failure: GovFailure)`, an enum whose cases are the sixteen ledger constructors with their payloads, so `ruleName` returns the case name (`[GV-19]`); the two committee GOVCERT failures are `CommitteeHasPreviouslyResignedException` and `CommitteeIsUnknownException` (`[GC-4]`) (agent, phase2) | sixteen exception classes, each needing a `ruleName` line; new fields on `DRepException`, which D19 rejected for the same reason |
| D32 | `ppuWellFormed` and the security-group test are private functions of `GovMutator` (`[GV-7a]`, `[GV-11a]`) (agent, phase2) | public methods on `ProtocolParamUpdate` in scalus-core, API surface that no caller outside GOV needs; phase 5 may lift them when ENACT needs the groups |
| D33 | `EmulatorInitialState` gains `committee`, `committeeHotKeys` and `constitution` as fields 6 to 8 with defaults, a deprecated 5-argument constructor and `apply` without defaults, a filter for the 5-argument `copy`; `Builder.build()` and `JEmulator` call the full form; a hot key for a non-member is rejected (`[ES-6]`, `[ES-6a]`) (agent, phase2) | no defaults on the primary, which breaks every Scala caller of the case-class form; accepting a hot key for a non-member, which GOVCERT `[GC-2]` rejects |
| D34 | The genesis `State` with the gov state is built in `EmulatorBase`; `Emulator.withState` starts from it through `EmulatorState.initial(ledger: State, …)` and `EmulatorBase.fromState`; `buildInitialState` keeps its signature and `depositsOf` becomes `private[node]` (agent, phase2) | a gov-state parameter on the private `Emulator` constructors and a new `EmulatorState.initial` overload |
| D35 | The gov reads of `[ES-9]` are `EmulatorBase.govState`, `getGovState` in the Java API and `getGovState()` in `JEmulator`, plus `GovState.proposal(id)`; a DRep state is read from `certState.vstate.dreps` (agent, phase2) | one Emulator method per listed read, in three languages each |
| D36 | The vector decoder reads the gov state with test-only decoders; `LedgerState` and `UTxOState` derive decoders only; no production CBOR codec for the new types (`[CV-6]`) (agent, phase2) | production codecs, which nothing in production serializes |
| D37 | The GOV, EPOCH, RATIFY, ENACT and LEDGER dirs run with an `expectedFailures` list scoped by vector and case for verdicts, as `[CV-1]` had, and `LedgerStateComparison.Exclusion` entries scoped by vector name for state fields, for what the harness cannot replay (the treasury is 0 in every case); each entry names its cause (agent, phase2) | excluding whole dirs, which hides regressions in their other cases |
| D38 | The harness derives each case's epoch from its `newLedgerState`: a proposal of the case tx, else a DRep the tx touched, else 899 (`[CV-8]`); this retires the phase-1 `dreps.expiry` exclusions (agent, phase2) | a fixed epoch 899 with exclusions on `govState.proposals` and `dreps.expiry` for every late case, which would blind the comparison in most RATIFY, EPOCH and ENACT cases; extending the Haskell dumper with the epoch, which is Q5 work on another repo |
| D39 | Refines D29: `VotingState` gets no deprecated 2-argument constructor, `apply` or `copy` filter; the deprecated 1-argument forms of D17 forward to the 3-argument form. The 2-argument form never shipped (1.4.1 has the 1-argument `VotingState`), so no binary or source caller needs it, and MiMa reports no 2-argument problem (agent, phase2) | the deprecated 2-argument forms of D29, which serve no released caller |
| D40 | GOV keeps the ledger's PV 9 exemptions: at PV 9 it skips the return-account checks and `ZeroTreasuryWithdrawals` (`unless hardforkConwayBootstrapPhase`, `C/Rules/Gov.hs:514-530, 559-562`) and the `adaPerUtxoByte` condition of `ppuWellFormed` (`C/PParams.hs:948-949`); the PV 9 bootstrap failures stay not emulated (agent, phase2) | checking them at every version, which rejects at PV 9 what the ledger accepts; emulating the bootstrap failures, which spec section 5.8 marks N/A |
| D41 | The committee checks of `[GC-1]` to `[GC-4]` are a new step `CertsMutator.committeeCert` that reads and writes only `committeeState`, not a `govCert` that takes the whole `VotingState` and widens its error type as the 11.2 design note said; `govCert` and the deprecated `VotingCertificatesMutator`, whose `Error` is `DRepException`, stay unchanged (agent, phase2) | widening `govCert`, which changes the deprecated mutator's error type for no gain |
| D42 | The JS gov surface reuses one `Committee` and one `Constitution` object shape for the `Emulator.create` options and for `getGovState()`; the threshold is a `Fraction` of two numbers; a vote is one `GovVote` row `{ voterRole, credentialType, credentialHash, vote }` in one `votes` array per proposal; `getGovState()` shows no hot keys and no proposal procedure. `ImmutableEmulator` gets no gov read: its `state.govState` is already public (`[ES-9]`, `[ES-10]`) (agent, phase2) | CBOR for the constitution and the committee, as `JDRepRegistration.anchor` takes; three vote arrays per proposal; a `govState` accessor on `ImmutableEmulator` |
| D43 | Refines `[CV-8]` and D38: the harness epoch of a case is the largest of 899, the `[CV-8]` epoch of each case of its dir up to it, and the `proposedIn` of each proposal in its old state. Time in an Imp test only moves forward, and each term is the exact epoch of an earlier case or a lower bound of this one, so the floor never overshoots; it fixes the negative cases, which have no new state, and the cases that touch neither a proposal nor a DRep (`RATIFY.Voting.Active voting stake.*.Rewards contribute …` retire a pool at 918, which needs an epoch from 913). A case whose epoch leaves no trace takes an `expectedFailures` entry (D37) (agent, phase2) | `[CV-8]` alone, which runs every negative case at 899; reading the epoch from the DRep pulser of the dump, which `[CV-6]` ignores and whose `RatifyState` would bound only expired actions |
| D44 | Keep `Coin(Long)` and `UnitInterval(Long, Long)`; a `Word64` above `Long.MaxValue` stays an accepted divergence, and the six conformance cases of the two vectors (`ENACT.Treasury withdrawals.Withdrawals exceeding maxBound Word64 submitted in a single proposal`, `GOV.Voting.committee member can not vote on UpdateCommittee action`) stay enforced expected failures (agent, phase2, resolves Q9) | change both to `BigInt`: a binary break of the most used ledger types for values no real tx carries (max supply is 4.5 × 10^16 lovelace, below 2^63 ≈ 9.2 × 10^18) |

**Why** D5: an incremental instant stake touches every tx mutator. The boundary computation is O(UTxO) once per epoch, which is cheap for an Emulator.

## 20. Open questions for the owner

Resolved 2026-11-02: Q1 `Ideal` (D4), Q2 seed the snapshots (`[ES-8]`), Q3 change the types (`[DM-18]` to `[DM-21]`), Q4 on by default (`[ES-11]`), Q6 mainnet-like reserves and treasury (`[ES-7]` to `[ES-7b]`), Q7 no committee (`[ES-13]`), Q8 reject (`[EN-4]`), Q9 keep the `Long` types (D44).



5. **Q5.** PV11 vectors. Findings (2026-11-02, research note `pv11-vectors-research.md` in the session notes):
   - Scalus's `vectors.tar.gz` is byte-identical to Cardano Blueprint `main` (`cardano-scaling/cardano-blueprint`, `src/ledger/conformance-test-vectors/`). Dingo (`blinklabs-io/ouroboros-mock`), gerolamo and the old Amaru harness use the same PV10 per-tx corpus. Amaru now vendors the scenario corpus of Blueprint PR #71 (still PV10) and checks only tx verdicts (`crates/amaru-ledger/tests/evaluate_ledger_states.rs:257, 327`).
   - No PV10-format corpus exists at PV11. Upstream cardano-ledger has a dump hook (`CONFORMANCE_CBOR_DUMP_PATH`, `libs/cardano-ledger-conformance/src/.../ExecSpecRule/Core.hs:262`) for LEDGER and NEWEPOCH, but it dumps only Haskell/Agda divergences. Its conformance Imp suite moved to PV11 in `4bc98a1c1` (2026-07-01), after the 1.22.1.0 tag.
   - The only PV11 corpus: Dugite (`michaeljfazio/dugite`) patches that hook to dump every case and publishes `ledger-rules.tar.gz` weekly (1.6 GB: 7 446 LEDGER, 533 NEWEPOCH transitions). Its pin is cardano-ledger master `a88b60bd` (conway 1.23.0.0), not the production pin, and the test names are lost.
   - No Imp-derived data tests the reward formula: Imp tests make no blocks, so eta = 0. Amaru tests rewards and pots against per-epoch stake-distribution fixtures that a Haskell extractor pinned to node 11.0.1 produces from mainnet, preprod and preview snapshots (`crates/amaru/tests/summary.rs`). Dingo compares with Koios (`internal/koiosparity/`).
   - Recommendation: (a) keep the PV10 tarball as a regression set; (b) prototype a `NewEpochState` decoder and a NEWEPOCH runner on a NEWEPOCH-only slice of Dugite's corpus (owner approved; downloaded 2026-11-02: release `conformance-corpus-v20261101-084724`, `ledger-rules.tar.gz` sha256 `018c480caad1abf311b7203998d0e905b743d17ee26213edc4575d027faab996`, 10.2 GB unpacked, of which NEWEPOCH is 533 cases in 20 MB, each with `ctx`, `env`, `sig`, `st`, `st_out` CBOR files); (c) then generate our own corpus with a small Scalus-owned Haskell dumper on the public Imp hooks `modifyImpInitPostSubmitTxHook` and `modifyImpInitPostEpochBoundaryHook` (`eras/shelley/impl/testlib/Test/Cardano/Ledger/Shelley/ImpTest.hs:127-130`), at the production tag 1.22.1.0. It runs the full Conway Imp suite at PV11 (`withEachEraVersion`), needs no Agda, keeps test names, and gives LEDGER and NEWEPOCH before and after states; (d) for rewards, use Amaru's PV11 mainnet fixtures (epochs 644 to 648) or Koios, through the it-data stake cache.