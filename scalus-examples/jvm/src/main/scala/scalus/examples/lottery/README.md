# Lottery

## Scalus/Cardano

The Scalus implementation follows the commit-reveal-punish protocol using Cardano's eUTxO model. Both players join in a
single multisig transaction (Cardano natively supports multiple signers per transaction), so step 1 is not split.

### Fairness function

The winner is determined by `(len(preimage1) + len(preimage2)) mod 2`. If the sum is even, the revealing player wins; if
odd, they lose. Players are recommended to use preimages of at least 32 bytes to prevent brute-force guessing of the
opponent's preimage from its hash.

### On-chain state

The contract datum tracks the lottery state as a state machine:

```
State
├── playerOneSecret : ByteString      -- SHA-256 hash commitment from player 1
├── playerTwoSecret : ByteString      -- SHA-256 hash commitment from player 2
├── revealDeadline  : PosixTime       -- deadline for the second reveal
└── lotteryState    : LotteryState
    ├── Empty                          -- no player has revealed yet
    ├── PlayerOneRevealed(length, pkh) -- player 1 revealed; stores preimage length and pubkey hash
    └── PlayerTwoRevealed(length, pkh) -- player 2 revealed; stores preimage length and pubkey hash
```

### Actions (redeemer)

| Action                            | When                                            | Effect                                                                                                                                                     |
|-----------------------------------|-------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RevealPlayerOne(preimage)`       | State is `Empty` or `PlayerTwoRevealed`         | Verifies `sha2_256(preimage) == playerOneSecret`. Transitions to `PlayerOneRevealed` (if from `Empty`) or determines winner (if from `PlayerTwoRevealed`). |
| `RevealPlayerTwo(preimage)`       | State is `Empty` or `PlayerOneRevealed`         | Symmetric to above for player 2.                                                                                                                           |
| `Lose(preimage, winnerOutputIdx)` | One player has revealed                         | The losing player concedes, proving identity via preimage. Pot is paid to the winner (verified by output index).                                           |
| `Timeout(preimage)`               | One player has revealed and deadline has passed | The revealing player claims the pot after the opponent failed to act before `revealDeadline`.                                                              |

### State machine transitions

```
                 ┌─────────────────────────────────────────────┐
                 │                   Empty                      │
                 └────────┬──────────────────┬─────────────────┘
          RevealPlayerOne │                  │ RevealPlayerTwo
                          ▼                  ▼
              ┌───────────────────┐  ┌───────────────────┐
              │ PlayerOneRevealed │  │ PlayerTwoRevealed │
              └───────┬───┬───┬───┘  └───┬───┬───┬───────┘
                      │   │   │          │   │   │
         RevealP2 ────┘   │   │          │   │   └──── RevealP1
         (win/lose)       │   │          │   │        (win/lose)
                   Lose ──┘   │          │   └── Lose
                (concede)     │          │    (concede)
                    Timeout ──┘          └── Timeout
                 (after deadline)     (after deadline)
```

### Validator checks

**Reveal (from `Empty`):**

- Preimage hashes to the player's committed secret (`sha2_256(preimage) == secret`).
- Exactly one continuation output at the contract address.
- Continuation output carries an inline datum with the updated `LotteryState`.
- The new state records the preimage length and the revealing player's pub key hash.
- Both secrets and the deadline are unchanged in the continuation datum.
- The revealing player's pub key hash is in the transaction signatories.

**Reveal (second reveal, from `PlayerXRevealed`):**

- Preimage hashes to the player's committed secret.
- `(storedLength + preimage.length) mod 2 == 0` — the revealing player wins. Otherwise the validator fails with
  `"Unlucky"`, and the player must use `Lose` instead.

**Lose:**

- Preimage hashes to the losing player's committed secret (proves identity).
- The output at `winnerOutputIdx` pays to the winner's pub key credential.
- The output contains at least the full contract balance.

**Timeout:**

- Preimage hashes to the claiming player's committed secret.
- The transaction's validity range is entirely after `revealDeadline`.

### Cardano-specific design notes

- **Single-UTXO state machine.** The lottery lives in one UTXO. The `initiate` transaction creates it; `reveal` consumes
  and re-creates it with updated state; terminal actions (`reveal`-win, `lose`, `timeout`) consume it without
  continuation.
- **Multisig initiation.** Both players provide UTXOs and sign the initiate transaction. Cardano transactions natively
  support multiple inputs from different addresses and multiple required signers, so no separate join steps are needed.
- **Time enforcement.** The `Timeout` action requires `tx.validRange.isEntirelyAfter(revealDeadline)`, which leverages
  Cardano's deterministic transaction validity interval rather than on-chain clock reads.
- **No commit phase split.** Unlike Solidity/Fe implementations that split step 1 into two transactions because the
  platform lacks multisig, the Scalus implementation handles both commitments atomically.

### Project structure

| File                        | Purpose                                                                                                    |
|-----------------------------|------------------------------------------------------------------------------------------------------------|
| `LotteryValidator.scala`    | On-chain validator logic (compiled to UPLC via the Scalus compiler plugin)                                 |
| `LotteryTransactions.scala` | Off-chain transaction builders for `initiate`, `revealPlayerOne`, `revealPlayerTwo`, `lose`, and `timeout` |
| `LotteryContract.scala`     | Compiles the validator and generates a CIP-57 blueprint                                                    |

### Tests

| File                                 | Purpose                                                                                                                                                                                                                         |
|--------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `LotteryValidatorTest.scala`         | Unit tests covering all actions, boundary conditions (deadline edge), and negative cases (wrong preimage, wrong secret, early timeout, odd-sum loss). Each test runs both direct Scala invocation and full emulator submission. |
| `LotteryScenarioTest.scala`          | Non-deterministic scenario exploration of 5 concurrent lottery games with overlapping players, verifying invariants hold under interleaved actions.                                                                             |
| `LotteryScalaCheckCommandTest.scala` | Property-based testing with ScalaCheck Commands over 10 concurrent games with HD-derived participants, generating random action sequences and checking contract invariants after each step.                                     |

### Running

```bash
# Compile
sbtn scalusExamplesJVM/compile

# Run lottery tests
sbtn 'scalusExamplesJVM/testOnly scalus.examples.lottery.*'
```
