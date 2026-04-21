# Auction

English auction where the seller creates a contract with a starting bid and duration. Anyone can bid before the auction
ends; the highest bidder wins the item when the seller ends the auction. Previous bidders are refunded.

Each auction instance is parameterized by a one-shot UTxO, giving it a unique policy ID. The minted NFT represents the
auctioned item.

## On-chain state

```
Datum
├── seller          : PubKeyHash
├── highestBidder   : Option[PubKeyHash]
├── highestBid      : BigInt
├── auctionEndTime  : PosixTime
└── itemId          : ByteString
```

## Actions

| Action  | When            | Effect                                                          |
|---------|-----------------|-----------------------------------------------------------------|
| `Start` | Minting only    | Spends one-shot UTxO, mints auction NFT, creates initial datum  |
| `Bid`   | Before end time | Updates highest bid, refunds previous bidder via indexed output |
| `End`   | After end time  | Pays NFT to winner + bid to seller (or seller reclaims if none) |

Uses indexed UTxO lookups (O(1)) and delayed redeemer pattern.

## Files

| File                   | Purpose                                      |
|------------------------|----------------------------------------------|
| `Auction.scala`        | On-chain validator + off-chain factory/txs   |
| `UnfixedAuction.scala` | Vulnerable version (double-satisfaction bug) |
