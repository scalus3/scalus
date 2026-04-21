# Atomic Transactions

On Cardano every transaction is atomic by ledger rules: all inputs are consumed and all outputs created in one step, or
nothing changes. No smart contract is needed.

This example shows how to batch multiple UTxO spends in a single transaction using the TxBuilder API.

## Files

| File                       | Purpose                     |
|----------------------------|-----------------------------|
| `AtomicTransactions.scala` | Off-chain TxBuilder example |
