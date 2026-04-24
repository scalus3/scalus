# Escrow

Trusted intermediary between a buyer and a seller. The seller sets the buyer's address and the required payment amount.
The buyer deposits the payment. Then either the buyer releases the funds to the seller, or the seller refunds the buyer.

## How it works

The contract is parameterized by the seller, buyer, escrow amount, and an initialization amount. On Cardano a contract
UTxO cannot have an empty balance, so the seller provides an initialization amount which is returned during pay or
refund.

- **Deposit** — the buyer deposits the required amount into the contract.
- **Pay** — the buyer releases the full contract balance to the seller.
- **Refund** — the seller returns the full contract balance to the buyer.

`EscrowValidator.scala` is the on-chain spending validator. `EscrowTransactions.scala` builds the off-chain
transactions. `EscrowOffchain.scala` provides off-chain utilities for querying contract state.
