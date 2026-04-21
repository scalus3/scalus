# Vesting

Handles the maturation of cryptocurrency for a beneficiary. Before the start time, nothing can be withdrawn. Between
start and expiration, the available amount is proportional to elapsed time. After expiration, the entire balance is
available.

## On-chain state

```
Config (validator parameter)
├── beneficiary    : PubKeyHash
├── startTimestamp : PosixTime
├── duration       : BigInt        -- vesting period in milliseconds
└── initialAmount  : BigInt
```

## Actions

| Action             | Effect                                                     |
|--------------------|------------------------------------------------------------|
| `Withdraw(amount)` | Beneficiary claims up to vested portion; rest stays locked |

## Files

| File                        | Purpose                        |
|-----------------------------|--------------------------------|
| `VestingValidator.scala`    | On-chain spending validator    |
| `VestingContract.scala`     | PlutusV3 compilation           |
| `VestingTransactions.scala` | Off-chain transaction builders |
