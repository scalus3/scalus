# Lottery

Two players bet equal amounts of cryptocurrency. The winner is chosen fairly using a commit-reveal-punish protocol.

## How it works

Both players join in a single multisig transaction by paying their bets and committing SHA-256 hashes of secret
preimages. The contract then enters the reveal phase.

The winner is determined by `(len(preimage1) + len(preimage2)) mod 2`. If the sum is even, the revealing player wins;
if odd, they lose. Players should use preimages of at least 32 bytes to prevent brute-force guessing.

> **Note on fairness.** The length-based winner function is taken verbatim from the rosetta-smart-contracts reference
> (the Solidity version computes `(bytes(secret0).length + bytes(secret1).length) % 2`). It is cryptographically weak —
> the outcome depends only on the parity of the preimage lengths, not their 256 bits of entropy, and it tensions with
> the "use ≥32-byte preimages" advice (if both follow it with a fixed 32-byte length, the result is always even). It is
> kept as-is to stay faithful to the benchmark; a production lottery should derive the winner from the secret *values*.

### Reveal phase

Each player reveals their preimage one at a time. The validator verifies that the preimage hashes to the committed
secret and updates the state. When the second player reveals, the fairness function determines the winner. If the
revealing player loses, they must use the `Lose` action instead, which pays the pot to the winner.

### Timeout

If a player fails to reveal before the deadline, the player who *did* reveal can claim the pot via the `Timeout`
action. Because the revealer's preimage is already public on-chain by then, **anyone** can submit the `Timeout`
transaction — so the validator pins the payout to the revealer's stored public-key hash, ensuring the pot can only go
to the rightful claimant. A winning reveal must land *before* the deadline (`isEntirelyBefore`), so it cannot race a
post-deadline `Timeout`.

### Cardano-specific design

- **Single-UTXO state machine** — the lottery lives in one UTXO that is consumed and re-created on each state
  transition.
- **Multisig initiation** — both players commit in one atomic transaction, unlike Solidity implementations that require
  two separate join steps.
- **Time enforcement** — the `Timeout` action uses Cardano's validity interval (`validRange.isEntirelyAfter`) rather
  than an on-chain clock.

### Simplifications vs. the rosetta-smart-contracts reference

- **Symmetric reveal order** — either player may reveal first (the state machine is symmetric), whereas the reference
  fixes the order: player 0 reveals, then player 1.
- **Single `revealDeadline`** — one deadline for the whole reveal phase, rather than the reference's separate
  `end_join` and `end_reveal` (and `end_reveal + constant`) boundaries.
- **No join timeout** — because initiation is an atomic multisig transaction, there is no "player 2 failed to join"
  state, so the reference's `end_commit`/`end_join` refund of player 1's own bet is not needed.
- **Length-based winner** — kept verbatim from the reference (see the fairness note above).

`LotteryValidator.scala` is the on-chain state machine. `LotteryTransactions.scala` builds the off-chain transactions
for initiating, revealing, conceding, and timing out.
