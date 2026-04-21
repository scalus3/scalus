# Payment Splitter

Splits cryptocurrency payments among a group of payees in proportion to their shares. The set of shareholders and shares
is fixed at deployment.

Includes a naive O(N^2) implementation and an optimized version using the stake validator pattern for O(1) per-spend
cost.

## Files

| File                                      | Purpose                       |
|-------------------------------------------|-------------------------------|
| `PaymentSplitterValidator.scala`          | Naive on-chain validator      |
| `OptimizedPaymentSplitterValidator.scala` | Optimized via stake validator |
| `PaymentSplitterContract.scala`           | PlutusV3 compilation          |
