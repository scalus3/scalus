# Upgradeable Proxy

Proxy pattern that forwards calls to a logic contract. The owner can upgrade the logic by updating the hash in the
datum.

## How it works

On Cardano this is implemented via the stake validator pattern: the spending validator checks that a withdrawal from the
logic contract's stake validator is present in the transaction, which forces the logic validator to execute.

The datum stores the current logic validator hash and the owner's public key hash.

- **Call** — to interact with the proxy, the transaction must include a withdrawal from the logic contract's stake
  address. This triggers the logic validator to run, effectively delegating validation to the current logic contract.
- **Upgrade** — the owner updates the logic hash in the datum, pointing the proxy to a new logic contract.

`UpgradeableProxy.scala` is the on-chain spending validator. `UpgradeableProxyOffchain.scala` builds the off-chain
transactions.
