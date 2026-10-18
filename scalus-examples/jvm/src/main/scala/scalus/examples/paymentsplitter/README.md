# Payment Splitter

Splits cryptocurrency payments among a group of payees in equal shares. The set of payees is fixed at deployment.
Anyone can send ADA to the contract address; a payout transaction distributes the balance equally.

Only **ADA** is split. Both validators reject contract UTxOs that carry native tokens — otherwise, since the payout
only reconciles lovelace, the fee payer could pocket any tokens locked in the contract for free.

## How it works

This example includes two implementations to illustrate an important Cardano optimization pattern.

**Naive version** (`PaymentSplitterValidator.scala`) — the spending validator runs full validation logic for each UTxO
being spent. When spending N UTxOs in one transaction, this results in O(N^2) cost because each invocation iterates
through all inputs and outputs.

**Optimized version** (`OptimizedPaymentSplitterValidator.scala`) — uses the stake validator pattern. The spending
validator does minimal per-UTxO work (O(1)), while a stake validator runs once per transaction to verify the overall
split is correct. The off-chain code pre-computes the split amounts and passes them in the redeemer.

## Verification

`PaymentSplitterVerificationTest` proves properties of both validators' compiled UPLC with the
`UplcBlaster` tactic of `scalus-verification`: for two payees, any keys and amounts, and whatever
else the transaction has. A payout is accepted only from a payee, pays nobody but the payees, pays
every payee, pays the payees other than its submitter exactly the split (half the locked lovelace,
rounded down), and holds only ADA; an honest payout is accepted. For the optimized version, a
contract output is spent only together with the stake validator's withdrawal and a redeemer that
names it, the stake validator checks the same split, and the contract's stake credential is
registered and deregistered but never delegated. The validators fold over lists, so the
transactions there have a fixed shape: one payee input, one contract input, two outputs at most.
