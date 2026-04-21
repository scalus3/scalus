# AMM (Automated Market Maker)

Constant-product DEX pool. A single script serves as both the spending validator and LP token minting policy.

Users can deposit a token pair to receive LP tokens, redeem LP tokens to withdraw proportional reserves, or swap one
token for the other with a fee.

## On-chain state

```
AmmParams (validator parameter)
├── t0, t1         : TradedToken
├── feeNumerator   : BigInt
└── feeDenominator : BigInt

AmmDatum
├── r0       : BigInt   -- reserve of token 0
├── r1       : BigInt   -- reserve of token 1
└── lpSupply : BigInt   -- total LP tokens outstanding
```

## Actions

| Action                         | Effect                                                   |
|--------------------------------|----------------------------------------------------------|
| `Deposit(x0, x1)`              | Adds liquidity; mints LP tokens (sqrt for first deposit) |
| `Redeem(lp)`                   | Burns LP tokens; releases proportional reserves          |
| `Swap(t0In, amountIn, minOut)` | Fee-adjusted swap; enforces x*y>=k invariant             |

## Files

| File                 | Purpose                           |
|----------------------|-----------------------------------|
| `AmmValidator.scala` | On-chain validator (spend+mint)   |
| `AmmContract.scala`  | PlutusV3 compilation              |
| `AmmOffchain.scala`  | Off-chain queries and tx builders |
