# Simple Wallet

On Cardano, wallets are native -- no smart contract needed. Public key addresses handle deposits and withdrawals at the
ledger level.

This example also shows a 2-of-3 multisig wallet using Cardano's native script system, with no Plutus validator
required.

## Files

| File                 | Purpose                                     |
|----------------------|---------------------------------------------|
| `SimpleWallet.scala` | Off-chain: pubkey address + native multisig |
