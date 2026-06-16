# Price Bet

Two players bet on whether an exchange rate will be above or below a target value. An oracle publishes the rate as a
reference input. The winner claims the pot; if no opponent joins, the owner reclaims after the deadline.

## How it works

The owner creates the contract by depositing a bet and specifying a target exchange rate and a deadline. The datum
tracks both players, the deadline, and the target rate.

- **Join** — before the deadline, a second player matches the bet.
- **Win** — before the deadline, the player claims the pot if the oracle's published rate exceeds the target. The
  oracle data is read from a reference input (the oracle UTxO is not consumed). The oracle is authenticated by its
  **beacon NFT**: being at the oracle script address is not enough — anyone can pay a forged datum there — so `Win`
  requires the referenced UTxO to carry the oracle's one-shot beacon token, which only the genuine oracle holds.
- **Timeout** — after the deadline, if no opponent joined, the owner reclaims the deposit.

`PricebetValidator.scala` is the on-chain spending validator. `OracleValidator.scala` is the oracle's data feed
validator. `PricebetTransactions.scala` builds the off-chain transactions.
