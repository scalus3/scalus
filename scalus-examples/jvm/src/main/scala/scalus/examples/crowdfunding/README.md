# Crowdfunding

Campaign with a funding goal and deadline. Anyone can donate before the deadline. If the goal is met, the recipient
withdraws the funds. If not, donors reclaim their contributions.

## How it works

The contract datum tracks the total donated amount, the goal, the recipient, and the deadline. Each donor receives a
donation token as a receipt.

- **Create** — mints a campaign NFT and initializes the datum.
- **Donate** — before the deadline, a donor sends ADA to the contract. A donation token is minted and sent to the
  donor, and the datum's total is incremented.
- **Withdraw** — after the deadline, if the goal is met, the recipient claims the collected funds.
- **Reclaim** — after the deadline, if the goal is not met, a donor burns their donation token and reclaims the
  corresponding ADA.

`Crowdfunding.scala` contains the on-chain validator and minting policy. `CrowdfundingEndpoints.scala` provides
off-chain API endpoints for building transactions.
