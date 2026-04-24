# Betting

Two players and an oracle. Both players join by depositing equal bets. The oracle determines the winner, who receives
the whole pot. If the oracle does not act by the deadline, both players can reclaim their bets.

## How it works

The contract is parameterized by the two players, the oracle, and an expiration time. A beacon token tracks the
contract state.

- **Join** — the second player matches the first player's bet. Both players sign the join transaction (Cardano natively
  supports multiple signers), and a beacon token is minted.
- **AnnounceWinner** — the oracle signs a transaction paying the pot to the winner via an indexed output.
- **Timeout** — if the oracle hasn't acted by the deadline, either player can reclaim their bet.

`BettingValidator.scala` is the on-chain validator handling both minting and spending. `BettingTransactions.scala`
builds the off-chain transactions for joining, announcing, and timing out.
