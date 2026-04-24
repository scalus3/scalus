# HTLC (Hash Time-Locked Contract)

The committer deposits cryptocurrency and commits to the SHA3-256 hash of a secret. The receiver can claim by revealing
the preimage before the deadline. After the deadline, the committer reclaims the deposit.

Used as a building block for cross-chain atomic swaps.

## How it works

The contract is parameterized by the committer, receiver, the hash commitment, and a timeout.

- **Reveal** — before the timeout, the receiver provides a preimage that hashes to the committed value and claims the
  deposit.
- **Timeout** — after the timeout, the committer reclaims the deposit.

`HtlcValidator.scala` is the on-chain spending validator. `HtlcTransactions.scala` builds the off-chain transactions
for revealing and timing out.
