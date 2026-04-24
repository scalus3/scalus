# Linked List

On-chain singly linked list using NFT-chained UTxOs. Each node holds an NFT whose token name encodes the key; the datum
links to the next node.

## How it works

The list is parameterized by a root key, a prefix, and the prefix length. Each node is a UTxO carrying an NFT under the
list's minting policy. The NFT token name is the prefix concatenated with the node's key. The datum stores the key of
the next node, forming the chain.

- **Init** — creates the root node, which serves as the list's head sentinel.
- **Deinit** — destroys the root node (the list must be empty).
- **Insert** — adds a new node between two existing nodes, updating the predecessor's link to point to the new node and
  the new node's link to point to the successor.
- **Remove** — removes a node and updates the predecessor's link to skip over it.

`LinkedListValidator.scala` is the on-chain parameterized validator that enforces link integrity on every mutation.
`LinkedListOffchain.scala` builds the off-chain transactions for all four operations.
