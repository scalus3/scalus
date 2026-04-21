# Upgradeable Proxy

Proxy pattern that forwards calls to a logic contract. The owner can upgrade the logic by updating the hash in the
datum.

On Cardano this is implemented via the stake validator pattern: the spending validator checks that a withdrawal from the
logic contract's stake validator is present in the transaction, forcing the logic to run.

## On-chain state

```
ProxyDatum
├── logicHash : ValidatorHash
└── owner     : PubKeyHash
```

## Actions

| Action    | Effect                                                  |
|-----------|---------------------------------------------------------|
| `Call`    | Executes via logic contract (stake withdrawal required) |
| `Upgrade` | Owner updates logicHash                                 |

## Files

| File                             | Purpose                        |
|----------------------------------|--------------------------------|
| `UpgradeableProxy.scala`         | On-chain spending validator    |
| `UpgradeableProxyOffchain.scala` | Off-chain transaction builders |
