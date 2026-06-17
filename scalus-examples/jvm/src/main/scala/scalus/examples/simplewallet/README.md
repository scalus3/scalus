# Simple Wallet

On Cardano, wallets are native — no smart contract needed. Public key addresses handle deposits and withdrawals at the
ledger level. The owner's signature is the only authorization required to spend UTxOs.

## How it works

`SimpleWalletTransactions.scala` shows that the rosetta "simple wallet" operations (deposit, create transaction,
execute transaction, withdraw) all reduce to ordinary Cardano transactions signed by the owner's key — `transfer`
pays a recipient with change back to the owner, and `withdrawAll` spends every owner UTxO to a recipient. No Plutus
validator is involved.

The same file also provides `MultiSigWallet`, an m-of-n multisig wallet using Cardano's native script system
(`Timelock.MOf`) — e.g. any two of three owners must sign — again without a Plutus validator.
