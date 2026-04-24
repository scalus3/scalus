# Storage

On-chain data storage of uncapped size.

## How it works

Cardano transactions have a size limit (~16 KB), so storing large data requires splitting it across multiple UTxOs.
This implementation uses the Linked List pattern to create a chain of UTxOs, each holding a chunk of the data as its
datum. NFTs link the chunks together in order.

`StorageTransactions.scala` provides the off-chain logic: it splits data into chunks, creates a linked list root with
the first chunk, and appends subsequent chunks as additional nodes. Reading the data back involves traversing the
linked list and concatenating the chunks.

The underlying linked list implementation lives in the `scalus-design-patterns` module.
