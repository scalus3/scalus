# Lottery

Two players bet equal amounts of ADA. The winner is chosen fairly using a commit-reveal-punish protocol enforced
entirely on-chain, with no trusted third party and no randomness oracle.

The use case follows the
[rosetta-smart-contracts lottery](https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/lottery)
specification. It is intended as a clear reference for modeling adversarial interactions, timeouts, and fairness
guarantees in deterministic smart contracts on Cardano.

## How it works

Both players join in a single multisig transaction by paying their bets and committing SHA-256 hashes of secret
preimages. The contract then enters the reveal phase.

The winner is determined by `(len(preimage1) + len(preimage2)) mod 2`. If the sum is even, the revealing player wins;
if odd, they lose. Players should use preimages of at least 32 bytes to prevent brute-force guessing.

> **Note on fairness.** The length-based winner function is taken verbatim from the rosetta-smart-contracts reference
> (the Solidity version computes `(bytes(secret0).length + bytes(secret1).length) % 2`). It is cryptographically weak –
> the outcome depends only on the parity of the preimage lengths, not their 256 bits of entropy, and it tensions with
> the "use ≥32-byte preimages" advice (if both follow it with a fixed 32-byte length, the result is always even). It is
> kept as-is to stay faithful to the benchmark; a production lottery should derive the winner from the secret *values*.

### Protocol

1. Both players join the lottery in one transaction by paying the same bet and committing to a secret.
2. One player reveals their preimage. Either player may go first.
3. If the other player has not revealed before `revealDeadline`, the revealer claims both bets via `Timeout`.
4. Otherwise the second player reveals. The fairness function decides the winner from the two preimage lengths.
5. If the second revealer loses, they concede via `Lose`, which pays the pot to the winner.

### Reveal phase

Each player reveals their preimage one at a time. The validator verifies that the preimage hashes to the committed
secret and updates the state. When the second player reveals, the fairness function determines the winner. If the
revealing player loses, they must use the `Lose` action instead, which pays the pot to the winner.

### Timeout

If a player fails to reveal before the deadline, the player who *did* reveal can claim the pot via the `Timeout`
action. Because the revealer's preimage is already public on-chain by then, **anyone** can submit the `Timeout`
transaction – so the validator pins the payout to the revealer's stored public-key hash, ensuring the pot can only go
to the rightful claimant. A winning reveal must land *before* the deadline (`isEntirelyBefore`), so it cannot race a
post-deadline `Timeout`.

## On-chain model

The lottery lives in one state UTXO locked at the validator script address. The inline datum is a state machine:

```
State
+-- playerOneSecret : Secret          -- SHA-256 hash commitment from player 1
+-- playerTwoSecret : Secret          -- SHA-256 hash commitment from player 2
+-- revealDeadline  : PosixTime       -- deadline for the reveal phase
+-- lotteryState    : LotteryState
    +-- Empty                          -- no player has revealed yet
    +-- PlayerOneRevealed(length, pkh) -- player 1 revealed; stores preimage length and pubkey hash
    +-- PlayerTwoRevealed(length, pkh) -- player 2 revealed; stores preimage length and pubkey hash
```

### Actions (redeemer)

| Action                            | When                                            | Effect                                                         |
|-----------------------------------|-------------------------------------------------|----------------------------------------------------------------|
| `RevealPlayerOne(preimage)`       | State is `Empty` or `PlayerTwoRevealed`         | Verifies preimage hash, transitions state or determines winner |
| `RevealPlayerTwo(preimage)`       | State is `Empty` or `PlayerOneRevealed`         | Symmetric to above for player 2                                |
| `Lose(preimage, winnerOutputIdx)` | One player has revealed                         | Losing player concedes; pot paid to winner                     |
| `Timeout(preimage)`               | One player has revealed and deadline has passed | Revealing player claims pot after opponent failed to act       |

### State transitions

```
                 +---------------------------------------------+
                 |                   Empty                      |
                 +--------+------------------+-----------------+
          RevealPlayerOne |                  | RevealPlayerTwo
                          v                  v
              +-------------------+  +-------------------+
              | PlayerOneRevealed |  | PlayerTwoRevealed |
              +-------+---+---+--+  +--+---+---+--------+
                      |   |   |        |   |   |
         RevealP2 ----+   |   |        |   |   +---- RevealP1
         (win/lose)       |   |        |   |        (win/lose)
                   Lose --+   |        |   +-- Lose
                (concede)     |        |    (concede)
                    Timeout --+        +-- Timeout
                 (after deadline)     (after deadline)
```

### Validator enforcement

* **Commitment-reveal flow** – the preimage must hash to the committed secret (`sha2_256(preimage) == secret`).
* **State continuity** – on a reveal from `Empty`, exactly one continuation output must exist at the contract
  address with an inline datum; secrets and deadline must be unchanged.
* **Signature checks** – the revealing player's public-key hash must be among the transaction signatories.
* **Winner selection** – on the second reveal, `(storedLength + preimage.length) mod 2 == 0` means the revealer
  wins; if odd, the validator fails with `"Unlucky"` and the player must use `Lose` instead.
* **Payout verification** – on `Lose`, the output at `winnerOutputIdx` must pay the winner's public-key credential at
  least the full contract balance.
* **Deadline enforcement** – on `Timeout`, the transaction validity range must be entirely after `revealDeadline`.

### Cardano-specific design

- **Single-UTXO state machine** – the lottery lives in one UTXO that is consumed and re-created on each state
  transition.
- **Multisig initiation** – both players commit in one atomic transaction, unlike Solidity implementations that require
  two separate join steps.
- **Time enforcement** – the `Timeout` action uses Cardano's validity interval (`validRange.isEntirelyAfter`) rather
  than an on-chain clock.

### Simplifications vs. the rosetta-smart-contracts reference

- **Symmetric reveal order** – either player may reveal first (the state machine is symmetric), whereas the reference
  fixes the order: player 0 reveals, then player 1.
- **Single `revealDeadline`** – one deadline for the whole reveal phase, rather than the reference's separate
  `end_join` and `end_reveal` (and `end_reveal + constant`) boundaries.
- **No join timeout** – because initiation is an atomic multisig transaction, there is no "player 2 failed to join"
  state, so the reference's `end_commit`/`end_join` refund of player 1's own bet is not needed.
- **Length-based winner** – kept verbatim from the reference (see the fairness note above).

## Off-chain interface

`LotteryTransactions` builds, balances, and signs every transaction with the Scalus `TxBuilder`.

### Create lottery (commit phase)

Both players provide UTXOs, commit their secrets, and sign a single multisig transaction.

```scala
val txCreator = LotteryTransactions(env, evaluator, contract)

val tx = txCreator.initiateLottery(
  playerOneUtxos = aliceUtxos,
  playerTwoUtxos = bobUtxos,
  betAmount = Coin(10_000_000L),          // 10 ADA each
  playerOnePkh = Alice.addrKeyHash,
  playerTwoPkh = Bob.addrKeyHash,
  secret1 = sha2_256(preimage1),          // commitment
  secret2 = sha2_256(preimage2),          // commitment
  revealDeadline = deadline.toEpochMilli,
  changeAddress = Alice.address,
  playerOneSigner = Alice.signer,
  playerTwoSigner = Bob.signer
)
provider.submit(tx)
```

### Reveal

Player 1 reveals their preimage and the contract transitions to `PlayerOneRevealed`. `revealPlayerTwo` is symmetric.
On the second reveal, an even combined length wins the pot; an odd one is rejected with `"Unlucky"` and the player
must use `lose` instead.

```scala
val tx = txCreator.revealPlayerOne(
  utxos = aliceUtxos,
  lotteryUtxo = lotteryUtxo,
  preimage = preimage1,
  playerOnePkh = Alice.addrKeyHash,
  playerOneSecret = secret1,
  playerTwoSecret = secret2,
  revealDeadline = deadline.toEpochMilli,
  sponsor = Alice.address,
  validTo = deadline,
  signer = Alice.signer
)
```

### Concede (lose)

The losing player concedes, directing the pot to the winner.

```scala
val tx = txCreator.lose(
  utxos = bobUtxos,
  lotteryUtxo = lotteryUtxo,
  preimage = preimage2,
  loserPkh = Bob.addrKeyHash,
  winnerAddress = Alice.address,
  winnerOutputIdx = BigInt(0),
  sponsor = Bob.address,
  validTo = deadline,
  signer = Bob.signer
)
```

### Timeout

If the opponent fails to reveal before the deadline, the revealing player claims the pot.

```scala
val tx = txCreator.timeout(
  utxos = aliceUtxos,
  lotteryUtxo = lotteryUtxo,
  preimage = preimage1,
  claimantPkh = Alice.addrKeyHash,
  payeeAddress = Alice.address,
  sponsor = Alice.address,
  validFrom = afterDeadline,
  signer = Alice.signer
)
```

## Tests

The tests live in `scalus-examples/jvm/src/test/scala/scalus/examples/lottery/`:

* `LotteryValidatorTest` – every action from every state, positive and negative: wrong preimages, tampered
  secrets or deadline, odd-sum reveal (`"Unlucky"`), timeout before the deadline, a third party trying to steal the pot
  via `Timeout`, and the reveal-at-deadline boundary. Each case creates a fresh emulator, builds the transaction via
  `LotteryTransactions`, and checks both direct validator evaluation and full emulator submission.
* `LotteryScenarioTest` – `Scenario` exploration of 5 concurrent games with overlapping players, branching over
  reveal, lose, timeout, and wait actions and checking invariants after each step.
* `LotteryScalaCheckCommandTest` – ScalaCheck `Commands` over 10 concurrent games with overlapping participants,
  checking invariants after every random action sequence.

## Build and run

```bash
sbtn "scalusExamplesJVM/testOnly scalus.examples.lottery.*"
```

To turn this example into a standalone project that depends on a released Scalus, start from the
[scalus-starter](https://github.com/scalus3/scalus-starter) template and copy the three source files in.

## Files

| File                          | Purpose                                                                 |
|-------------------------------|-------------------------------------------------------------------------|
| `LotteryValidator.scala`      | On-chain state machine (compiled to UPLC by the Scalus compiler plugin) |
| `LotteryContract.scala`       | Compiles the validator and builds the CIP-57 blueprint                  |
| `LotteryTransactions.scala`   | Off-chain transaction builders using `TxBuilder`                        |

## Disclaimer

This code is a reference implementation and educational example. It has not been audited and must not be used with
real funds without an independent security review.
