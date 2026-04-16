# Lottery -- Reference Implementation

This project implements a two-player Lottery use case based on the
[Rosetta smart contract specifications](https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/lottery),
using [Scalus](https://github.com/nau/scalus) for both on-chain and off-chain code.

The use case implements a **fair, trust-minimized betting protocol** where two players stake an equal
amount of ADA and the winner redeems the entire pot. Fairness is achieved without relying on external
randomness or oracles, using a **commit-reveal-punish** protocol enforced entirely on-chain.

This implementation is intended as a **clear reference** for modeling adversarial interactions,
timeouts, and fairness guarantees in deterministic smart contracts on Cardano.

---

## Overview

In this lottery:

* Two players place an equal bet in ADA
* Each player commits to a secret (via its SHA-256 hash)
* Secrets are revealed sequentially
* The winner is computed as a deterministic, fair function of both secrets:
  `(len(preimage1) + len(preimage2)) mod 2`
* Dishonest behavior (e.g. refusing to reveal) is **punished** via timeout claims
* An honest player is never worse off than interacting with another honest player
* Players should use preimages of at least 32 bytes to prevent brute-force attacks

No trusted third party or randomness oracle is required.

---

## Actors

* **Player 1** -- first participant to join and reveal
* **Player 2** -- second participant to join and reveal

Both players are symmetric in stake and potential payoff.

---

## Protocol

The protocol followed by (honest) players is the following:

1. `player1` and `player2` join the lottery by paying the bet and committing to a secret
   (the bet is the same for each player);
2. `player1` reveals the first secret;
3. if `player1` has not revealed, `player2` can redeem both players' bets after a given deadline
   (`revealDeadline`);
4. once `player1` has revealed, `player2` reveals the secret;
5. if `player2` has not revealed, `player1` can redeem both players' bets after a given deadline
   (`revealDeadline`);
6. once both secrets have been revealed, the winner, who is fairly determined as a function of the
   two revealed secrets, can redeem the whole pot.

Since Cardano natively supports multiple signers per transaction, step 1 is a single multisig
transaction (no separate join steps needed).

---

## On-chain Model

The lottery is represented by a **state UTxO** locked at a validator script address.

The inline datum stores the lottery state as a state machine:

```
State
+-- playerOneSecret : ByteString      -- SHA-256 hash commitment from player 1
+-- playerTwoSecret : ByteString      -- SHA-256 hash commitment from player 2
+-- revealDeadline  : PosixTime       -- deadline for the second reveal
+-- lotteryState    : LotteryState
    +-- Empty                          -- no player has revealed yet
    +-- PlayerOneRevealed(length, pkh) -- player 1 revealed; stores preimage length and pubkey hash
    +-- PlayerTwoRevealed(length, pkh) -- player 2 revealed; stores preimage length and pubkey hash
```

### Actions (redeemer)

| Action                            | When                                             | Effect                                                      |
|-----------------------------------|--------------------------------------------------|-------------------------------------------------------------|
| `RevealPlayerOne(preimage)`       | State is `Empty` or `PlayerTwoRevealed`          | Verifies preimage hash, transitions state or determines winner |
| `RevealPlayerTwo(preimage)`       | State is `Empty` or `PlayerOneRevealed`          | Symmetric to above for player 2                             |
| `Lose(preimage, winnerOutputIdx)` | One player has revealed                          | Losing player concedes; pot paid to winner                  |
| `Timeout(preimage)`               | One player has revealed and deadline has passed   | Revealing player claims pot after opponent failed to act    |

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

The validator enforces:

* **Commitment-reveal flow** -- preimage must hash to the committed secret
  (`sha2_256(preimage) == secret`)
* **State continuity** -- on reveal from `Empty`, exactly one continuation output must exist at the
  contract address with an inline datum; secrets and deadline must be unchanged
* **Signature checks** -- the revealing player's pub key hash must be in the transaction signatories
* **Winner selection** -- on second reveal, `(storedLength + preimage.length) mod 2 == 0` determines
  the winner; if odd, the validator fails with `"Unlucky"` and the player must use `Lose` instead
* **Payout verification** -- on `Lose`, the output at `winnerOutputIdx` must pay to the winner's pub
  key credential with at least the full contract balance
* **Deadline enforcement** -- on `Timeout`, the transaction's validity range must be entirely after
  `revealDeadline`

---

## Off-chain Interface

The off-chain logic is implemented in `LotteryTransactions.scala` using the Scalus `TxBuilder`.
All transactions are built, balanced, and signed programmatically.

### Create Lottery (Commit Phase)

Both players provide UTxOs, commit their secrets, and sign a single multisig transaction.

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

### Reveal Secret -- Player 1

Player 1 reveals their preimage. The contract transitions to `PlayerOneRevealed`.

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

### Reveal Secret -- Player 2

Player 2 reveals their preimage. If the combined length is even, player 2 wins and claims the pot.
Otherwise the validator rejects with `"Unlucky"` and player 2 must use `Lose` instead.

```scala
val tx = txCreator.revealPlayerTwo(
  utxos = bobUtxos,
  lotteryUtxo = lotteryUtxo,
  preimage = preimage2,
  playerTwoPkh = Bob.addrKeyHash,
  playerOneSecret = secret1,
  playerTwoSecret = secret2,
  revealDeadline = deadline.toEpochMilli,
  sponsor = Bob.address,
  validTo = deadline,
  signer = Bob.signer
)
```

### Concede (Lose)

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

---

## Fairness and Security Properties

This implementation guarantees:

* No reliance on external randomness
* No trusted coordinator
* Dishonest players are penalized (timeout claims)
* Honest players are never disadvantaged
* Deterministic and auditable winner selection

---

## Testing

The test suite (`LotteryTest.scala`) provides comprehensive coverage:

**Positive cases:**
* Player 1 reveals valid preimage from Empty state
* Player 2 reveals valid preimage from Empty state
* Player 2 reveals with even sum (wins)
* Player 1 reveals with even sum after player 2 revealed (wins)
* Player 2 concedes after player 1 revealed
* Player 1 concedes after player 2 revealed
* Player 1 timeout after player 2 failed to reveal
* Player 2 timeout after player 1 failed to reveal
* Reveal succeeds at deadline boundary

**Negative cases:**
* Reveal with wrong preimage (player 1 and player 2)
* Attempt to tamper with opponent's secret
* Attempt to change deadline
* Second reveal with odd sum (Unlucky)
* Timeout before deadline
* Lose with wrong preimage
* Timeout with wrong preimage
* Second reveal with wrong preimage

Each test creates a fresh emulator, builds transactions via `LotteryTransactions`, and verifies
both direct Scala validator evaluation and full emulator submission.

---

## Build and Run

```bash
# Compile
sbt compile

# Run all tests
sbt test

# Generate CIP-57 blueprint (plutus.json)
sbt run
```

---

## Project Structure

| File                                    | Package            | Purpose                                                          |
|-----------------------------------------|--------------------|------------------------------------------------------------------|
| `onchain/LotteryValidator.scala`        | `lottery.onchain`  | On-chain validator logic (compiled to UPLC via the Scalus compiler plugin) |
| `onchain/LotteryContract.scala`         | `lottery.onchain`  | Compiles the validator and generates a CIP-57 blueprint          |
| `onchain/GenBlueprint.scala`            | `lottery.onchain`  | `@main` entry point to write `plutus.json`                       |
| `offchain/LotteryTransactions.scala`    | `lottery.offchain` | Off-chain transaction builders using `TxBuilder`                 |
| `LotteryTest.scala`                     | `lottery`          | Comprehensive emulator-based test suite                          |

---

## Disclaimer

This code is a **reference implementation** and **educational example**.

It has not been audited and should not be used with real funds without independent security review.
