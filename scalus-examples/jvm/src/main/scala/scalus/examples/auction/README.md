# Auction

English auction where the seller creates a contract with a starting bid and duration. Anyone can bid before the auction
ends; the highest bidder wins the item when the seller closes the auction. Previous bidders are refunded.

## How it works

Each auction instance is parameterized by a one-shot UTxO, giving it a unique policy ID. The minted NFT represents the
auctioned item. The datum tracks the seller, highest bidder, current bid, end time, and item ID.

- **Start** — spends the one-shot UTxO, mints the auction NFT, and creates the initial datum.
- **Bid** — before the end time, a new bidder places a higher bid. The previous highest bidder is refunded via an
  indexed output (O(1) lookup).
- **End** — after the end time, the seller closes the auction. The NFT goes to the winner and the bid goes to the
  seller. If nobody bid, the seller reclaims the NFT. The seller's payout output is **tagged with this auction's
  script hash** (an inline datum): because each auction has a unique one-shot hash, the per-input NFT guard can't see
  a sibling auction at a different address, so without the tag two same-seller auctions ended in one transaction
  could share a single seller output and pay the seller once. The tag forces a distinct seller output per auction.

The contract uses indexed UTxO lookups and the delayed redeemer pattern. `Auction.scala` contains both the on-chain
validator and the off-chain transaction builders.

`UnfixedAuction.scala` is a deliberately vulnerable version that demonstrates a double-satisfaction bug.
