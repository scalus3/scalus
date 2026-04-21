# Decentralized Identity

Self-sovereign identity (SSI) system. An owner mints an identity token and can delegate attribute creation to other
parties. Delegations are time-bounded and can be revoked.

Token name prefixes distinguish types: `"i"` identity, `"d"` delegation, `"a"` attribute. Delegation and attribute names
are derived via `blake2b_224` for unlinkability.

## On-chain state

```
IdentityDatum
└── ownerPkh : PubKeyHash

DelegationDatum
├── identityPolicyId : PolicyId
├── delegatorPkh     : PubKeyHash
├── delegateePkh     : PubKeyHash
├── validFrom        : PosixTime
└── validTo          : PosixTime
```

## Actions

| Action          | Effect                                           |
|-----------------|--------------------------------------------------|
| Mint identity   | Owner spends seed UTxO, mints "i"-prefixed token |
| Mint delegation | Mints "d"-prefixed delegation token              |
| Mint attribute  | Delegatee mints "a"-prefixed attribute token     |
| Revoke          | Burns delegation or attribute tokens             |

## Files

| File                                      | Purpose                          |
|-------------------------------------------|----------------------------------|
| `DecentralizedIdentityValidator.scala`    | On-chain parameterized validator |
| `DecentralizedIdentityContract.scala`     | PlutusV3 compilation             |
| `DecentralizedIdentityTransactions.scala` | Off-chain transaction builders   |
