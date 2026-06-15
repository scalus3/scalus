# Vault

Prevents cryptocurrency from being immediately withdrawn by an adversary who has stolen the owner's key. The owner
issues a withdrawal request; after a mandatory wait time, the withdrawal can be finalized. During the wait time, a
separate **recovery key** holder can cancel the request — so even if the owner's key is stolen, the funds can be saved.

## How it works

The contract is a state machine with two states: Idle and Pending. The datum tracks the owner, the recovery key, the
current state, the requested amount, the wait time, and the finalization deadline.

- **Deposit** — anyone can add funds at any time; the state must stay unchanged (a deposit cannot move the machine
  between Idle and Pending).
- **InitiateWithdrawal** — the owner requests a withdrawal. The state transitions to Pending and the finalization
  deadline is set to the request transaction's validity **upper** bound plus the wait time. (Anchoring to the upper
  bound, which the ledger forces to be ≥ now, stops the requester from backdating the deadline to finalize early.)
- **FinalizeWithdrawal** — after the deadline has passed, the withdrawal completes and the funds go to the owner.
- **Cancel** — while Pending, the **recovery key** holder cancels the request. The state returns to Idle and the funds
  stay locked. The owner's key alone cannot cancel — that is the point, since the threat model is a stolen owner key.

`VaultValidator.scala` is the on-chain state machine. `VaultTransactions.scala` builds the off-chain transactions for
all four actions.
