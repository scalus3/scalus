# Decentralized Identity

Self-sovereign identity (SSI) system. An owner mints an identity token and can delegate attribute creation to other
parties. Delegations are time-bounded and can be revoked.

## How it works

Token name prefixes distinguish types: `"i"` for identity, `"d"` for delegation, `"a"` for attribute. Delegation and
attribute names are derived via `blake2b_224` for unlinkability.

- **Mint identity** — the owner spends a seed UTxO and mints an `"i"`-prefixed identity token.
- **Mint delegation** — the owner creates a time-bounded delegation by minting a `"d"`-prefixed token. The delegation
  datum records the delegatee's public key hash and the validity period.
- **Mint attribute** — a delegatee with a valid delegation mints an `"a"`-prefixed attribute token. The delegation is
  read from a reference input and authenticated by requiring it to **hold the delegation token** — being a
  datum-shaped UTxO at the script address is not enough, since anyone can pay a forged datum there.
- **Revoke** — the owner burns delegation or attribute tokens to revoke them. Because publishing requires the token,
  burning it genuinely ends the delegate's authority.

`DecentralizedIdentityValidator.scala` is the on-chain parameterized validator. `DecentralizedIdentityTransactions.scala`
builds the off-chain transactions for all operations.
