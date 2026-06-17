# Betting

Two players and an oracle. Both players join by depositing equal bets. The oracle determines the winner, who receives
the whole pot. If the oracle does not act by the deadline, both players can reclaim their bets.

## How it works

The contract is parameterized by the two players, the oracle, and an expiration time. A beacon token tracks the
contract state.

- **Join** — the second player matches the first player's bet. Both players sign the join transaction (Cardano natively
  supports multiple signers). The bet's beacon NFT is minted once, at creation.
- **AnnounceWinner** — the oracle signs a transaction paying the pot to the winner via an indexed output, and **burns
  the beacon NFT**.
- **Timeout** — if the oracle hasn't acted by the deadline, either player can reclaim their bet; the beacon NFT is
  burned here too.

Burning the NFT at the end makes the bet a true one-shot: a finished bet's token can't be re-locked at the script
with a forged config to sidestep the creation-time checks (the creator signs, the oracle differs from player 1, etc.).

`BettingValidator.scala` is the on-chain validator handling both minting and spending. `BettingTransactions.scala`
builds the off-chain transactions for joining, announcing, and timing out.
