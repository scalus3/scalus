# Triage of the main-code defects found in P2

2026-10-22. Covers F-35 to F-82 in `findings.md` (F-33 is fixed; the owner set F-34 aside).
Most were found by reading the code, not by running it, so the first step for each fix is a test
that fails on the current code.

Severity order: a Scalus evaluator that disagrees with the node > on-chain code that users ship
or copy > off-chain code that builds wrong transactions or loses funds > compiler and tooling
defects > cosmetic or latent.

## S1 – the Scalus evaluator disagrees with the node

A script passes in Scalus (Emulator, budget estimates, tests) and fails on chain, or the reverse.

| ID | Defect |
|---|---|
| F-82 (fixed) | `equalsData` ignored `Data.Map` entry order and duplicate keys (set equality). |
| F-78 (fixed) | `MkCons` accepted a non-constant head on a `(list value)` constant; Plutus 1.63 fails. |

## S2 – on-chain code that users ship or copy

| ID | Defect |
|---|---|
| F-49 (fixed) | Lottery: double satisfaction in Lose and Timeout. |
| F-50, F-63 (fixed) | Payment splitters (example and pattern) count outputs, not distinct payees: one payee can take another's share. |
| F-64 (fixed) | Pattern payment splitter misses the native-token guard that the example splitters got in 3c5cba3c8. |
| F-53 (fixed) | Vault `deposit`: no signature check, no one-vault-input guard. |
| F-52 (fixed) | Vault `initiateWithdrawal` does not check `waitTime` and `amount` of the new datum. |
| F-47 (fixed) | Groth16 verifier ignores extra public inputs, so a valid proof passes with wrong inputs. |
| F-48 (fixed) | Betting: the mint branch does not check that the script output holds the minted token. |
| F-51 (fixed) | Oracle `Update` accepts rate 0 or denominator 0; the oracle UTxO is then stuck. |
| F-80 (fixed) | UtxoCells: no check that the continuing output keeps the beacon token. |
| F-81 (fixed) | UtxoFlow macros: free variables of chunk 0 become datum fields, so the transaction builder chooses them (e.g. `minBid`). |
| F-56 (fixed) | Stdlib `Ord[IntervalBound]` orders inclusive above exclusive at the same time, which is wrong for lower bounds. |
| F-46 (fixed) | Three example validators: `findSignatureOrFail` ignores its argument and recurses on the same list (valid transactions fail). |
| F-65 (fixed) | Stdlib `Math.log(x, 1)` never terminates (JVM loop; on chain, budget exhausted). |
| F-84 (fixed) | Lottery: a first reveal could drain the pot (found by the review of the S2 fixes). |
| F-85 | Vault example: tokens in a vault UTxO can be stripped by a deposit, or added on cancel. |
| F-86 | Oracle example: a negative rate passes `Update`, and the mint does not check the initial rate. |
| F-87 | Groth16 example: public inputs are not range-checked against the scalar field order. |
| F-88 (fixed) | Oracle example: the beacon could leave the oracle (mint placement, Update preservation). |
| F-89 | Pricebet, lottery and vault examples: double satisfaction, oracle freshness, front-running and payout checks (list in findings.md). |

## S3 – off-chain code that builds wrong transactions or keys

| ID | Defect |
|---|---|
| F-39 | Bloxbean interop: `Data.Map` goes through a Scala `HashMap`, so entry order changes and the datum hash differs. |
| F-37 | BIP-39 seed: passphrase not NFKD-normalized; a non-ASCII passphrase gives a different wallet than other tools. |
| F-74 | `editTransactionSafe` drops the whole witness set, not only the redeemers. |
| F-73 | `TxBuilder` cannot spend a datum-less PlutusV3 script UTxO. |
| F-67 | Blockfrost `FromTransaction`/`FromInputs` returns spent outputs. |
| F-68 | Blockfrost `FromAsset` silently returns partial results when one holder request fails. |
| F-54 | Vesting off-chain `withdraw` drops native tokens from the continuing output. |
| F-58, F-59 | Stake snapshot omits reward balances; reward arithmetic uses `Double` above 2^53. |
| F-35, F-38 | `Address.fromBytes` and `Cip1852.parsePath` accept input the ledger or the spec rejects. |
| F-55 | JS `Utxo` constructor truncates a non-integer `outputIndex`. |
| F-57, F-76, F-36 | JSON codecs that cannot read their own output (Timelock Blockfrost, `DataReadWriter`, blueprint `Purpose`). |
| F-72 | A PlutusV4 script file is written with envelope type `PlutusScriptV3`. |
| F-61 | `UtxoQuery` macro turns `coin > n` into `coin >= n`. |
| F-60, F-69, F-70, F-71 | Emulator and testkit: wrong submit-error rule name, `checkTransaction` always `Confirmed`, `getDatum` always `None`, preconfiguration drops a UTxO on an index collision. |

## S4 – compiler and tooling

| ID | Defect |
|---|---|
| F-83 (fixed) | `@UplcRepr(SumBuiltinList(UplcConstr))` compiled to a `(list value)` of `constr` values, which fails on chain. |
| F-40 | Scott/SoP lowering: selecting `.args` on `Data.Constr` fails (field-name mismatch). |
| F-41 | A zero-arg method call `foo()` in a `@Compile` object crashes lowering (known, test ignored). |
| F-42, F-43 | `SIR.~=~` maps names non-injectively and ignores `Let` flags, so test assertions are weak (as F-33). |
| F-44 | Error-trace abbreviation misses errors under `Cast` and `Select`. |
| F-45 | `SirDSL.lam` types each parameter with the body's type. |
| F-75 | JVM `Builtins.indexArray` truncates the index to 32 bits; the CEK does not. |
| F-77 | `satMul(-1, Long.MinValue)` does not saturate. |
| F-79 | `TermSanitizer` keeps non-ASCII letters that its doc forbids. |

## S5 – cosmetic or latent

| ID | Defect |
|---|---|
| F-66 | `List.show` prints a trailing `, `. |
| F-62 | Subscription coverage of `MintsPolicy` is inconsistent; no provider triggers it today. |
