# Lottery

Two players bet equal amounts of cryptocurrency. The winner is chosen fairly using a commit-reveal-punish protocol.

## How it works

Both players join in a single multisig transaction by paying their bets and committing SHA-256 hashes of secret
preimages. The contract then enters the reveal phase.

The winner is determined by `(len(preimage1) + len(preimage2)) mod 2`. If the sum is even, the revealing player wins;
if odd, they lose. Players should use preimages of at least 32 bytes to prevent brute-force guessing.

### Reveal phase

Each player reveals their preimage one at a time. The validator verifies that the preimage hashes to the committed
secret and updates the state. When the second player reveals, the fairness function determines the winner. If the
revealing player loses, they must use the `Lose` action instead, which pays the pot to the winner.

### Timeout

If a player fails to reveal before the deadline, the other player can claim the pot via the `Timeout` action.

### Cardano-specific design

- **Single-UTXO state machine** — the lottery lives in one UTXO that is consumed and re-created on each state
  transition.
- **Multisig initiation** — both players commit in one atomic transaction, unlike Solidity implementations that require
  two separate join steps.
- **Time enforcement** — the `Timeout` action uses Cardano's validity interval (`validRange.isEntirelyAfter`) rather
  than an on-chain clock.

`LotteryValidator.scala` is the on-chain state machine. `LotteryTransactions.scala` builds the off-chain transactions
for initiating, revealing, conceding, and timing out.
