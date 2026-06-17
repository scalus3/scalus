# Factory

Creates and manages product NFTs using the factory pattern. Each product gets a unique token name derived from
`blake2b_256(seedUtxO)`.

## How it works

The minting policy enforces one-shot spending of a seed UTxO to guarantee unique token names. A spending validator
governs the product lifecycle. Each product NFT is locked at the script address with a datum containing a tag string
and the creator's public key hash.

- **Create** — spends a seed UTxO, mints a product NFT, and locks it at the script address with the product datum.
  The party named as `creator` in that datum must sign (so a product can't be attributed to a non-signer).
- **Destroy** — the creator burns the product NFT and reclaims the locked ADA. Authorization comes from the spending
  validator: burning the NFT means spending the product UTxO, which requires the datum's `creator` to sign.

`Factory.scala` contains the on-chain validation logic. `FactoryExample.scala` demonstrates off-chain usage.
