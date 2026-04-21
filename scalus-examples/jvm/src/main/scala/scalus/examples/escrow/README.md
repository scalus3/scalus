# Escrow

Trusted intermediary between a buyer and a seller. The seller sets the buyer's address and the required payment amount.
The buyer deposits the payment. Then either the buyer releases the funds to the seller, or the seller refunds the buyer.

On Cardano the contract cannot have an empty balance, so the seller provides an initialization amount which is returned
during pay or refund.

## On-chain state

```
Config (validator parameter)
├── seller               : PubKeyHash
├── buyer                : PubKeyHash
├── escrowAmount         : BigInt
└── initializationAmount : BigInt
```

## Actions

| Action    | Effect                             |
|-----------|------------------------------------|
| `Deposit` | Buyer deposits the required amount |
| `Pay`     | Buyer releases funds to seller     |
| `Refund`  | Seller returns funds to buyer      |

## Files

| File                       | Purpose                        |
|----------------------------|--------------------------------|
| `EscrowValidator.scala`    | On-chain spending validator    |
| `EscrowContract.scala`     | PlutusV3 compilation           |
| `EscrowOffchain.scala`     | Off-chain utilities            |
| `EscrowTransactions.scala` | Off-chain transaction builders |
