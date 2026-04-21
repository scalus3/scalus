# Factory

Creates and manages product NFTs using the factory pattern. Each product gets a unique token name derived from
`blake2b_256(seedUtxO)`. The minting policy enforces one-shot spending; a spending validator governs product lifecycle.

## On-chain state

```
ProductDatum
├── tag     : ByteString
└── creator : PubKeyHash
```

## Actions

| Action    | Effect                                 |
|-----------|----------------------------------------|
| `Create`  | Spends seed UTxO, mints product NFT    |
| `Destroy` | Burns product NFT, reclaims locked ADA |

## Files

| File                   | Purpose                 |
|------------------------|-------------------------|
| `Factory.scala`        | On-chain validation     |
| `FactoryExample.scala` | Off-chain usage example |
