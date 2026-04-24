# Atomic Transactions

On Cardano every transaction is atomic by ledger rules: all inputs are consumed and all outputs created in one step, or
nothing changes. No smart contract is needed to guarantee atomicity.

`AtomicTransactions.scala` shows how to batch multiple UTxO spends into a single transaction using the TxBuilder API.
