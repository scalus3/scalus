# Simple Transfer

The owner deposits native cryptocurrency; the recipient withdraws arbitrary fractions of the balance.

On Cardano a full withdrawal would not preserve the contract UTxO, so the recipient must leave a minimum amount during a
withdraw.

## On-chain state

```
Parties (validator parameter)
├── owner     : PubKeyHash
└── recipient : PubKeyHash
```

## Actions

| Action     | Effect                                                    |
|------------|-----------------------------------------------------------|
| `Deposit`  | Owner adds funds; one continuation output at contract     |
| `Withdraw` | Recipient claims; zero or one continuation output allowed |

## Files

| File                            | Purpose                     |
|---------------------------------|-----------------------------|
| `SimpleTransferValidator.scala` | On-chain spending validator |
| `SimpleTransferContract.scala`  | PlutusV3 compilation        |
