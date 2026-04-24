# Simple Wallet

On Cardano, wallets are native — no smart contract needed. Public key addresses handle deposits and withdrawals at the
ledger level. The owner's signature is the only authorization required to spend UTxOs.

## How it works

`SimpleWallet.scala` demonstrates that the rosetta "simple wallet" operations (deposit, create transaction, execute
transaction, withdraw) all reduce to ordinary Cardano transactions signed by the owner's key. No Plutus validator is
involved.

The file also shows a 2-of-3 multisig wallet using Cardano's native script system (`Timelock.MOf`), where any two of
three owners must sign to authorize a spend — again without a Plutus validator.
