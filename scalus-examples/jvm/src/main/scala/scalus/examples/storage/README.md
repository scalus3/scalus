## Storage

This is an implementation of a [smart contract from the Rosetta smart contracts](https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/storage) project.

The idea of the storage is to store data of **uncapped size**.

Since Cardano uses a UTxO model, the main way to put data on the Cardano blockchain is to create a transaction that has
an output with a *datum*, which is Cardanos way to bundle some data with a transaction output.

Transactions, however, have a size limit. It's currently 16KB, but while it may increase in the future, 
there's always going to be a limit.

Thus, to be able to store _unlimited_ data, a more sophisticated approach is required.
This implementation uses a Linked List pattern, which utilizes transaction outputs and NFTs to create a 
**chain of UTxOs**, allowing you to chunk the data, several UTxOs.

Find more information about it in the [Scalus Linked List implementation](https://github.com/scalus3/scalus/blob/master/scalus-design-patterns/src/main/scala/scalus/patterns/LinkedList.scala).
