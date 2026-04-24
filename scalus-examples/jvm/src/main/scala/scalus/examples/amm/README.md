# AMM (Automated Market Maker)

Constant-product DEX pool. A single script serves as both the spending validator and LP token minting policy.

Users can deposit a token pair to receive LP tokens, redeem LP tokens to withdraw proportional reserves, or swap one
token for the other with a fee.

## How it works

The pool is parameterized by a token pair (`t0`, `t1`) and a fee ratio. Its datum tracks the current reserves of both
tokens and the total LP token supply.

- **Deposit** — the user sends amounts of both tokens. The contract mints LP tokens proportional to the deposit (using
  square root pricing for the first deposit).
- **Redeem** — the user burns LP tokens and receives proportional shares of both reserves.
- **Swap** — the user sends one token and receives the other. The fee is deducted from the input, and the contract
  enforces the constant-product invariant (`x * y >= k`).

`AmmValidator.scala` contains the on-chain validator that handles all three actions as both a spending and minting
policy. `AmmOffchain.scala` provides off-chain helpers for querying pool state and building transactions.
