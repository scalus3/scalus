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

All 7 constructors raised at PV11 are implemented, bundled into one exception (`R/StakeCertificatesValidator.scala:152`). `IncorrectDepositDELEG` is not raised at PV11 (`C/Era.hs:184-185`). Gap: the vectors are not run (`[CV-1]`). State gap: `DRepState.delegates` and the pool delegator set are never written (`R/CertsMutator.scala:210-214`), phase 1.

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
| `ConwayCommitteeHasPreviouslyResigned` | `C/Rules/GovCert.hs:194-199` | `R/CertsMutator.scala:207`, `case _ => Right(state)` | **M** | 2 |
| `ConwayCommitteeIsUnknown` | `C/Rules/GovCert.hs:200-208` | same | **M** | 2 |

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
| `proposals` | insertion-ordered `GovActionState`s, keyed by `GovActionId` | `C/Governance/Proposals.hs` |
| `roots` | `GovRoots(pparamUpdate, hardFork, committee, constitution: Option[GovActionId])` | `ensPrevGovActionIds` |
| `committee` | `Option[Committee(members: Map[Credential, EpochNo], threshold: UnitInterval)]` | `C/Governance/Procedures.hs:544-549` |
| `constitution` | `Constitution` (exists, `SC/Types.scala:801`) | – |

- `GovActionState` MUST hold: `id`, `committeeVotes`, `drepVotes`, `spoVotes`, `procedure: ProposalProcedure`, `proposedIn`, `expiresAfter`. `[DM-6]`
- The current pparams MUST stay in `Context.env.params`. `[DM-7]`

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

## 10. Bookkeeping rules (phase 1)

- The UTXO mutator MUST add the tx deposits to `State.deposited` and subtract the tx refunds, for `isValid = true`. `[BK-1]`
- The deposits and refunds of `[BK-1]` MUST be the values that `TxBalance` uses (`L/utils/TxBalance.scala:33-109`). `[BK-2]`
- DELEG MUST keep `DRepState.delegates` equal to the set of accounts delegated to that DRep. `[BK-3]`
- POOL MUST keep `vrfKeyHashes` as `S/Rules/Pool.hs:276-310` does. `[BK-4]`
- POOL MUST reject a new pool whose VRF key is in `vrfKeyHashes` at PV ≥ 11. `[BK-5]`
- POOL MUST reject a re-registration whose VRF key differs from its own and is in `vrfKeyHashes` at PV ≥ 11. `[BK-6]`
- The Emulator MUST fill `vrfKeyHashes` from the seeded pools at genesis. `[BK-7]`

**Why** `[BK-7]`: the ledger fills the map only on the hard fork into PV11 (`C/Rules/HardFork.hs:107-125`). An Emulator that starts at PV11 never sees that hard fork.

### 10.1 DRep expiry

- GOVCERT MUST set the expiry of a registered or updated DRep to `currentEpoch + dRepActivity − numDormantEpochs`. `[DR-1]`
- LEDGER MUST run, for a valid tx with at least one proposal, the dormant bump of `C/Rules/Certs.hs:313-333` before CERTS. `[DR-2]`
- The dormant bump MUST add `numDormantEpochs` to the expiry of every DRep whose `expiry + numDormantEpochs ≥ currentEpoch`, then set `numDormantEpochs` to 0. `[DR-3]`
- LEDGER MUST set, after `[DR-2]` and before CERTS, the expiry of each registered DRep that votes in the tx by `[DR-1]`. `[DR-4]`
- `CertsMutator` MUST run `[DR-2]` and `[DR-4]` after the withdrawal drain and before the cert loop (`R/CertsMutator.scala:20-34`). `[DR-4a]`
- The Emulator MUST give a seeded DRep the expiry `genesisEpoch + dRepActivity`. `[DR-5]`

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

### 11.2 GOVCERT committee checks

- GOVCERT MUST reject `AuthCommitteeHotCert` and `ResignCommitteeColdCert` for a cold credential whose state is `Resigned`. `[GC-1]`
- GOVCERT MUST reject them for a cold credential that is not in `committee.members` and not added by a pending `UpdateCommittee`. `[GC-2]`
- GOVCERT MUST overwrite the cold credential's state with the new authorization. `[GC-3]`

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

- Scope: `[DM-3]`, `[DM-4]`, `[BK-1]` to `[BK-7]`, `[DR-1]` to `[DR-5]`, `[DR-4a]`.
- Acceptance:
  1. `[CV-3]`: remove the `deposited`, `dreps.delegates` and `dreps.expiry` exclusions; all run dirs pass.
  2. Unit tests of the example in section 10.1.
  3. A PV11 test of a duplicate VRF key on a new pool and on a re-registration.
- Risks: `VotingState` and `PoolsState` are public case classes. New fields break `copy` and `apply` for MiMa. Precedent: `UtxoEnv` gained `treasury` with a deprecated old constructor, and `copy` broke (`2026-10-07-scalus-emulator-provider-design.md:345`).

### Phase 2 – governance tx rules

- Scope: `[DM-1]`, `[DM-2]`, `[DM-5]`, `[DM-6]`, `[GV-1]` to `[GV-16]`, `[GV-2a]`, `[GC-1]` to `[GC-3]`, `[ES-6]` (committee, hot keys, constitution), the gov parts of `[ES-9]`, `[CV-2]`.
- Acceptance:
  1. The 56 GOV dirs and the tx steps of the EPOCH, RATIFY, ENACT and LEDGER dirs pass.
  2. `[CV-3]`: remove the `govState` exclusion. The test decoder MUST decode `ConwayGovState` proposals (`scalus-cardano-ledger/jvm/src/test/scala/scalus/testing/conformance/LedgerState.scala:10`).
  3. Unit tests for each PV11-only failure: `UnelectedCommitteeVoters`, `nOpt = 0`.
  4. An Emulator test: a vote on a missing action fails with `GovActionsDoNotExist`.
- Risks: replacing `GovState = Array[Element]` changes a public type. The vectors are PV10, so PV11-only checks have unit tests only.

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

**Why** D5: an incremental instant stake touches every tx mutator. The boundary computation is O(UTxO) once per epoch, which is cheap for an Emulator.

## 20. Open questions for the owner

Resolved 2026-11-02: Q1 `Ideal` (D4), Q2 seed the snapshots (`[ES-8]`), Q3 change the types (`[DM-18]` to `[DM-21]`), Q4 on by default (`[ES-11]`), Q6 mainnet-like reserves and treasury (`[ES-7]` to `[ES-7b]`), Q7 no committee (`[ES-13]`), Q8 reject (`[EN-4]`).


5. **Q5.** PV11 vectors. Findings (2026-11-02, research note `pv11-vectors-research.md` in the session notes):
   - Scalus's `vectors.tar.gz` is byte-identical to Cardano Blueprint `main` (`cardano-scaling/cardano-blueprint`, `src/ledger/conformance-test-vectors/`). Dingo (`blinklabs-io/ouroboros-mock`), gerolamo and the old Amaru harness use the same PV10 per-tx corpus. Amaru now vendors the scenario corpus of Blueprint PR #71 (still PV10) and checks only tx verdicts (`crates/amaru-ledger/tests/evaluate_ledger_states.rs:257, 327`).
   - No PV10-format corpus exists at PV11. Upstream cardano-ledger has a dump hook (`CONFORMANCE_CBOR_DUMP_PATH`, `libs/cardano-ledger-conformance/src/.../ExecSpecRule/Core.hs:262`) for LEDGER and NEWEPOCH, but it dumps only Haskell/Agda divergences. Its conformance Imp suite moved to PV11 in `4bc98a1c1` (2026-07-01), after the 1.22.1.0 tag.
   - The only PV11 corpus: Dugite (`michaeljfazio/dugite`) patches that hook to dump every case and publishes `ledger-rules.tar.gz` weekly (1.6 GB: 7 446 LEDGER, 533 NEWEPOCH transitions). Its pin is cardano-ledger master `a88b60bd` (conway 1.23.0.0), not the production pin, and the test names are lost.
   - No Imp-derived data tests the reward formula: Imp tests make no blocks, so eta = 0. Amaru tests rewards and pots against per-epoch stake-distribution fixtures that a Haskell extractor pinned to node 11.0.1 produces from mainnet, preprod and preview snapshots (`crates/amaru/tests/summary.rs`). Dingo compares with Koios (`internal/koiosparity/`).
   - Recommendation: (a) keep the PV10 tarball as a regression set; (b) prototype a `NewEpochState` decoder and a NEWEPOCH runner on a NEWEPOCH-only slice of Dugite's corpus (owner approved; downloaded 2026-11-02: release `conformance-corpus-v20261101-084724`, `ledger-rules.tar.gz` sha256 `018c480caad1abf311b7203998d0e905b743d17ee26213edc4575d027faab996`, 10.2 GB unpacked, of which NEWEPOCH is 533 cases in 20 MB, each with `ctx`, `env`, `sig`, `st`, `st_out` CBOR files); (c) then generate our own corpus with a small Scalus-owned Haskell dumper on the public Imp hooks `modifyImpInitPostSubmitTxHook` and `modifyImpInitPostEpochBoundaryHook` (`eras/shelley/impl/testlib/Test/Cardano/Ledger/Shelley/ImpTest.hs:127-130`), at the production tag 1.22.1.0. It runs the full Conway Imp suite at PV11 (`withEachEraVersion`), needs no Agda, keeps test names, and gives LEDGER and NEWEPOCH before and after states; (d) for rewards, use Amaru's PV11 mainnet fixtures (epochs 644 to 648) or Koios, through the it-data stake cache.