# Price Bet

Two players bet on whether an exchange rate will be above or below a target value. An oracle publishes the rate as a
reference input. The winner claims the pot; if no opponent joins, the owner reclaims after the deadline.

## On-chain state

```
PricebetState
├── owner        : PubKeyHash
├── player       : PubKeyHash
├── deadline     : PosixTime
└── exchangeRate : Rational
```

## Actions

| Action    | When            | Effect                                    |
|-----------|-----------------|-------------------------------------------|
| `Join`    | Before deadline | Second player matches the bet             |
| `Win`     | After deadline  | Winner claims pot (oracle rate vs target) |
| `Timeout` | After deadline  | Owner reclaims if no opponent joined      |

## Files

| File                         | Purpose                        |
|------------------------------|--------------------------------|
| `PricebetValidator.scala`    | On-chain spending validator    |
| `OracleValidator.scala`      | Oracle data feed validator     |
| `PricebetContract.scala`     | PlutusV3 compilation           |
| `PricebetTransactions.scala` | Off-chain transaction builders |
