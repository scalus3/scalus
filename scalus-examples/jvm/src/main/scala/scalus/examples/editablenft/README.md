# Editable NFT

NFT with mutable data that can be permanently sealed. Minting creates a reference NFT (label 100) holding the data and a
user NFT (label 222) for ownership. The data can be edited until sealed.

## On-chain state

```
ReferenceNftDatum
├── tokenId  : ByteString   -- immutable identifier
├── data     : Data          -- mutable metadata
└── isSealed : Bool          -- once true, permanently locked
```

## Actions

| Action | When       | Effect                            |
|--------|------------|-----------------------------------|
| Mint   | Seed spent | Creates reference + user NFT pair |
| Edit   | Not sealed | Updates data field                |
| Seal   | Not sealed | Sets isSealed to true             |
| Burn   | Any time   | Burns both tokens                 |

## Files

| File                            | Purpose                        |
|---------------------------------|--------------------------------|
| `EditableNftValidator.scala`    | On-chain validator             |
| `EditableNftContract.scala`     | PlutusV3 compilation           |
| `EditableNftTransactions.scala` | Off-chain transaction builders |
