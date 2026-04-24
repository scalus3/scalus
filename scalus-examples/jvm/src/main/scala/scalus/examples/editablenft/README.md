# Editable NFT

NFT with mutable metadata that can be permanently sealed. Minting creates a reference NFT (CIP-68 label 100) holding
the data and a user NFT (label 222) for ownership.

## How it works

The reference NFT datum contains a token ID, an arbitrary data field, and a sealed flag. The owner of the user NFT
controls the reference NFT.

- **Mint** — spending a seed UTxO creates both the reference and user NFTs.
- **Edit** — while not sealed, the owner can update the data field in the reference NFT's datum.
- **Seal** — the owner permanently locks the data by setting the sealed flag to true.
- **Burn** — at any time, the owner can burn both tokens.

`EditableNftValidator.scala` is the on-chain validator. `EditableNftTransactions.scala` builds the off-chain
transactions for minting, editing, sealing, and burning.
