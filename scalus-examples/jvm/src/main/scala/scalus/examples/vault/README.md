# Vault

Prevents cryptocurrency from being immediately withdrawn by an adversary who has stolen the owner's key. The owner
issues a withdrawal request; after a wait time, the withdrawal can be finalized. During the wait time, the owner can
cancel the request.

## On-chain state

```
State
├── owner                : PubKeyHash
├── status               : Idle | Pending
├── amount               : BigInt
├── waitTime             : BigInt          -- delay in milliseconds
└── finalizationDeadline : PosixTime
```

## Actions

| Action               | When                     | Effect                             |
|----------------------|--------------------------|------------------------------------|
| `Deposit`            | Any time                 | Adds funds, state stays Idle       |
| `InitiateWithdrawal` | Idle                     | Sets Pending, records deadline     |
| `FinalizeWithdrawal` | Pending + after deadline | Releases funds to owner            |
| `Cancel`             | Pending                  | Returns to Idle, funds stay locked |

## Files

| File                      | Purpose                        |
|---------------------------|--------------------------------|
| `VaultValidator.scala`    | On-chain state machine         |
| `VaultContract.scala`     | PlutusV3 compilation           |
| `VaultTransactions.scala` | Off-chain transaction builders |
