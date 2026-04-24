# Payment Splitter

Splits cryptocurrency payments among a group of payees in equal shares. The set of payees is fixed at deployment.
Anyone can send ADA to the contract address; a payout transaction distributes the balance equally.

## How it works

This example includes two implementations to illustrate an important Cardano optimization pattern.

**Naive version** (`PaymentSplitterValidator.scala`) — the spending validator runs full validation logic for each UTxO
being spent. When spending N UTxOs in one transaction, this results in O(N^2) cost because each invocation iterates
through all inputs and outputs.

**Optimized version** (`OptimizedPaymentSplitterValidator.scala`) — uses the stake validator pattern. The spending
validator does minimal per-UTxO work (O(1)), while a stake validator runs once per transaction to verify the overall
split is correct. The off-chain code pre-computes the split amounts and passes them in the redeemer.
