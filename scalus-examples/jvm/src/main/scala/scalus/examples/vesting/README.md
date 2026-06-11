# Vesting

Handles the maturation of cryptocurrency for a beneficiary. Before the start time, nothing can be withdrawn. Between
start and expiration, the available amount grows proportionally with elapsed time. After expiration, the entire balance
is available.

## How it works

The contract is parameterized by the beneficiary, the start timestamp, the vesting duration, and the initial amount.
The vested fraction at any point is `elapsed / duration`, clamped to [0, 1].

- **Withdraw** — the beneficiary claims up to the currently vested portion. The remainder stays locked at the contract
  address in a continuation UTxO that preserves the full remaining value (ADA and any native tokens) and the original
  vesting address.

Only one vesting UTxO may be spent per transaction: this prevents a double-satisfaction attack where a single
continuation output is reused to satisfy several script inputs at once.

`VestingValidator.scala` is the on-chain spending validator that computes the vested amount and enforces the withdrawal
limit. `VestingTransactions.scala` builds the off-chain withdraw transaction.
