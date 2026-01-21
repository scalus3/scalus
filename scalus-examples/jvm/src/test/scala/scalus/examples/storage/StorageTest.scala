package scalus.examples.storage

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.hex
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.examples.UnorderedLinkedListContract
import scalus.ledger.api.v1.Credential.ScriptCredential
import scalus.ledger.api.v3.{Address as OnchainAddress, TxId, TxOutRef}
import scalus.patterns.Cons
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.*
import scalus.utils.await

class StorageTest extends AnyFunSuite, ScalusTest:
    given env: CardanoInfo = TestUtil.testEnvironment
    given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val evaluator = PlutusScriptEvaluator.constMaxBudget(env)

    // Init UTxO reference for LinkedList config
    val initRef = TxOutRef(
      TxId(hex"0000000000000000000000000000000000000000000000000000000000000000"),
      BigInt(0)
    )

    val config = scalus.patterns.Config(
      init = initRef,
      deadline = 9999999999999L, // Far future deadline (milliseconds since epoch)
      penalty = OnchainAddress(
        ScriptCredential(hex"0000000000000000000000000000000000000000000000000000000000"),
        scalus.prelude.Option.empty
      )
    )

    val appliedValidator: PlutusV3[Data => Unit] = {
        val parameterizedValidator = UnorderedLinkedListContract.withErrorTraces
        val appliedValidator: PlutusV3[Data => Unit] = parameterizedValidator.apply(config.toData)
        appliedValidator
    }

    val creator = StorageTransactions(
      env = env,
      evaluator = evaluator,
      contract = appliedValidator,
      config = config,
      chunkSize = 1000
    )

    test("store data in single chunk"):
        val data = ByteString.fromString("Hello, Cardano!")

        val initialUtxos = Map(
          Input(TestUtil.genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
          Input(TestUtil.genesisHash, 1) -> Output(Alice.address, Value.ada(5000)),
          // The init reference UTxO that LinkedList needs
          Input(
            TransactionHash.fromByteString(initRef.id.hash),
            initRef.idx.toInt
          ) -> Output(
            Alice.address,
            Value.ada(10)
          )
        )

        val provider = Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )

        val userUtxos = provider.findUtxos(Alice.address).await().toOption.get

        // Build all transactions
        val validTo = java.time.Instant.ofEpochMilli(config.deadline.toLong - 1)
        val txs = creator.storeData(
          data = data,
          userUtxos = userUtxos,
          userPkh = Alice.addrKeyHash,
          userAddress = Alice.address,
          changeAddress = Alice.address,
          userSigner = Alice.signer,
          validTo = validTo
        )

        // Should be 1 transaction: init head with data
        assert(txs.length == 1, s"Expected 1 transaction, got ${txs.length}")

        // Submit transaction
        txs.foreach { tx =>
            provider.submit(tx).await() match
                case Left(error) => fail(s"Failed to submit: $error")
                case Right(_)    => ()
        }

        // Query storage UTxOs (at Alice's address with LinkedList NFTs)
        val storageUtxos = provider
            .findUtxos(Alice.address)
            .await()
            .toOption
            .get
            .filter { case (_, output) =>
                output.value.assets.assets.exists { case (cs, _) =>
                    cs == creator.script.scriptHash
                }
            }

        assert(
          storageUtxos.size == 1,
          s"Expected 1 UTxO (head with data), got ${storageUtxos.size}"
        )

    test("store data across 3 chunks"):
        val data = ByteString.fromArray(Array.fill(250)(0x42.toByte))

        // Create creator with small chunk size for testing
        val smallChunkCreator = StorageTransactions(
          env = env,
          evaluator = evaluator,
          contract = appliedValidator,
          config = config,
          chunkSize = 100 // 250 bytes / 100 = 3 chunks (100, 100, 50)
        )

        val initialUtxos = Map(
          Input(TestUtil.genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
          Input(TestUtil.genesisHash, 1) -> Output(Alice.address, Value.ada(5000)),
          // The init reference UTxO that LinkedList needs
          Input(
            TransactionHash.fromByteString(initRef.id.hash),
            initRef.idx.toInt
          ) -> Output(Alice.address, Value.ada(10))
        )

        val provider = Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )

        val userUtxos = provider.findUtxos(Alice.address).await().toOption.get

        // Build all transactions (should create 3 chunks: 100, 100, 50 bytes)
        val validTo = java.time.Instant.ofEpochMilli(config.deadline.toLong - 1)
        val txs = smallChunkCreator.storeData(
          data = data,
          userUtxos = userUtxos,
          userPkh = Alice.addrKeyHash,
          userAddress = Alice.address,
          changeAddress = Alice.address,
          userSigner = Alice.signer,
          validTo = validTo
        )

        // Should be 3 transactions: init head with chunk0, append chunk1, append chunk2
        assert(txs.length == 3, s"Expected 3 transactions, got ${txs.length}")

        // Submit sequentially - each transaction depends on the previous one being confirmed
        txs.zipWithIndex.foreach { case (tx, idx) =>
            provider.submit(tx).await() match
                case Left(error) => fail(s"Failed to submit transaction ${idx + 1}: $error")
                case Right(_)    => ()
        }

        // Query and verify (LinkedList nodes at Alice's address)
        val storageUtxos = provider
            .findUtxos(Alice.address)
            .await()
            .toOption
            .get
            .filter { case (_, output) =>
                output.value.assets.assets.exists { case (cs, _) =>
                    cs == smallChunkCreator.script.scriptHash
                }
            }

        assert(
          storageUtxos.size == 3,
          s"Expected 3 UTxOs (head + 2 appended nodes), got ${storageUtxos.size}"
        )

        // Reconstruct data by following the ref chain from head
        val reconstructed = reconstructData(storageUtxos, smallChunkCreator.script.scriptHash)
        assert(reconstructed == data, s"Reconstructed data doesn't match original")

    /** Reconstruct data from LinkedList UTxOs by following the ref chain.
      *
      * @param utxos
      *   UTxOs containing LinkedList nodes
      * @param policyId
      *   Script hash (policy ID) of the LinkedList
      * @return
      *   Reconstructed ByteString data
      */
    private def reconstructData(utxos: Utxos, policyId: PolicyId): ByteString =
        // Find head node (key = None)
        val headUtxo = utxos
            .find { case (_, output) =>
                output.inlineDatum.exists { datum =>
                    val cons = datum.to[Cons]
                    cons.key.isEmpty
                }
            }
            .getOrElse(throw new Exception("Head node not found"))

        // Follow the ref chain and collect data
        def followChain(
            currentRef: scalus.prelude.Option[ByteString],
            acc: List[ByteString]
        ): List[ByteString] =
            currentRef match
                case scalus.prelude.Option.None          => acc
                case scalus.prelude.Option.Some(nextKey) =>
                    // Find the node with this key
                    val nodeUtxo = utxos
                        .find { case (_, output) =>
                            output.inlineDatum.exists { datum =>
                                val cons = datum.to[Cons]
                                cons.key.contains(nextKey)
                            }
                        }
                        .getOrElse(throw new Exception(s"Node with key ${nextKey.toHex} not found"))

                    val nodeDatum = nodeUtxo._2.inlineDatum.get.to[Cons]
                    val data = nodeDatum.data match
                        case Data.B(bytes) => bytes
                        case _             => throw new Exception("Expected ByteString data")

                    followChain(nodeDatum.ref, acc :+ data)

        // Start from head's ref and collect all data chunks
        val headDatum = headUtxo._2.inlineDatum.get.to[Cons]
        val headData = headDatum.data match
            case Data.B(bytes) => bytes
            case _             => throw new Exception("Expected ByteString data")

        val chunks = headData :: followChain(headDatum.ref, List.empty)

        // Concatenate all chunks
        chunks.foldLeft(ByteString.empty)(_ ++ _)
