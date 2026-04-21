# Crowdfunding

Campaign with a funding goal and deadline. Anyone can donate before the deadline. If the goal is met, the recipient
withdraws the funds. If not, donors reclaim their contributions by burning their donation tokens.

## On-chain state

```
CampaignDatum
├── totalSum         : BigInt
├── goal             : BigInt
├── recipient        : PubKeyHash
├── deadline         : PosixTime
├── withdrawn        : BigInt
└── donationPolicyId : PolicyId
```

## Actions

| Action     | When                       | Effect                                    |
|------------|----------------------------|-------------------------------------------|
| `Create`   | Minting                    | Creates campaign with NFT                 |
| `Donate`   | Before deadline            | Mints donation token, increments totalSum |
| `Withdraw` | After deadline, goal met   | Recipient claims funds                    |
| `Reclaim`  | After deadline, goal unmet | Donor burns donation token, reclaims ADA  |

## Files

| File                          | Purpose                      |
|-------------------------------|------------------------------|
| `Crowdfunding.scala`          | On-chain validator + minting |
| `CrowdfundingEndpoints.scala` | Off-chain API endpoints      |
