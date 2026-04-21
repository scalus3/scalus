# Linked List

On-chain singly linked list using NFT-chained UTxOs. Each node holds an NFT whose token name encodes the key; the datum
links to the next node.

## On-chain state

```
ListConfig (validator parameter)
├── rootKey   : ByteString
├── prefix    : ByteString
└── prefixLen : BigInt
```

## Actions

| Action   | Effect                                   |
|----------|------------------------------------------|
| `Init`   | Creates root node                        |
| `Deinit` | Destroys root node (list must be empty)  |
| `Insert` | Adds node, updates predecessor's link    |
| `Remove` | Removes node, updates predecessor's link |

## Files

| File                        | Purpose                          |
|-----------------------------|----------------------------------|
| `LinkedListValidator.scala` | On-chain parameterized validator |
| `LinkedListContract.scala`  | PlutusV3 compilation             |
| `LinkedListOffchain.scala`  | Off-chain transaction builders   |
