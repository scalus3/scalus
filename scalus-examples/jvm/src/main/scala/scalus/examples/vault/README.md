# Vault

Prevents cryptocurrency from being immediately withdrawn by an adversary who has stolen the owner's key. The owner
issues a withdrawal request; after a mandatory wait time, the withdrawal can be finalized. During the wait time, the
owner (or a recovery key holder) can cancel the request.

## How it works

The contract is a state machine with two states: Idle and Pending. The datum tracks the owner, the current state, the
requested amount, the wait time, and the finalization deadline.

- **Deposit** — anyone can add funds at any time. The state stays Idle.
- **InitiateWithdrawal** — the owner requests a withdrawal. The state transitions to Pending and the finalization
  deadline is set to the current time plus the wait time.
- **FinalizeWithdrawal** — after the deadline has passed, the owner completes the withdrawal and receives the funds.
- **Cancel** — while Pending, the owner cancels the request. The state returns to Idle and the funds stay locked.

`VaultValidator.scala` is the on-chain state machine. `VaultTransactions.scala` builds the off-chain transactions for
all four actions.
