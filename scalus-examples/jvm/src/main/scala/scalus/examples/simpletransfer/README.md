# Simple Transfer

The owner deposits native cryptocurrency; the recipient withdraws arbitrary fractions of the balance.

## How it works

The contract is parameterized by the owner and recipient public key hashes.

- **Deposit** — the owner adds funds. The validator requires exactly one continuation output at the contract address.
- **Withdraw** — the recipient claims any amount. On Cardano a full withdrawal would destroy the contract UTxO, so the
  recipient must leave a minimum amount to preserve it (or spend the UTxO entirely if no continuation is needed).

`SimpleTransferValidator.scala` is the on-chain spending validator.
