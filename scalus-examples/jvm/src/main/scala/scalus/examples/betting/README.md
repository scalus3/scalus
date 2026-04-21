# Betting

Two players and an oracle. Both players join by depositing equal bets. The oracle determines the winner, who receives
the whole pot. If the oracle does not act by the deadline, both players can reclaim their bets.

## On-chain state

```
Config (validator parameter)
├── player1    : PubKeyHash
├── player2    : PubKeyHash
├── oracle     : PubKeyHash
└── expiration : PosixTime
```

## Actions

| Action                              | When            | Effect                                 |
|-------------------------------------|-----------------|----------------------------------------|
| `Join(outputIdx)`                   | Player 2 joins  | Matches bet, beacon token minted       |
| `AnnounceWinner(winner, outputIdx)` | Oracle resolves | Winner receives pot via indexed output |

## Files

| File                        | Purpose                         |
|-----------------------------|---------------------------------|
| `BettingValidator.scala`    | On-chain validator (mint+spend) |
| `BettingContract.scala`     | PlutusV3 compilation            |
| `BettingTransactions.scala` | Off-chain transaction builders  |
