# Decentralized Identity

Self-sovereign identity (SSI) system. An owner mints an identity token and can delegate attribute creation to other
parties. Delegations are time-bounded and can be revoked.

## How it works

Token name prefixes distinguish types: `"i"` for identity, `"d"` for delegation, `"a"` for attribute. Delegation and
attribute names are derived via `blake2b_224` for unlinkability.

- **Mint identity** — the owner spends a seed UTxO and mints an `"i"`-prefixed identity token.
- **Mint delegation** — the owner creates a time-bounded delegation by minting a `"d"`-prefixed token. The delegation
  datum records the delegatee's public key hash and the validity period.
- **Mint attribute** — a delegatee with a valid delegation mints an `"a"`-prefixed attribute token.
- **Revoke** — the owner burns delegation or attribute tokens to revoke them.

`DecentralizedIdentityValidator.scala` is the on-chain parameterized validator. `DecentralizedIdentityTransactions.scala`
builds the off-chain transactions for all operations.
