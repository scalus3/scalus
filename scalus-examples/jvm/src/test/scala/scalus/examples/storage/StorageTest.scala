package scalus.examples.storage

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.node.Emulator
import scalus.patterns.{Element, ElementData}
import scalus.testing.budget.{BudgetPinsSuite, BudgetTable}
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.*
import scalus.utils.await

class StorageTest extends AnyFunSuite, ScalusTest, BudgetPinsSuite:
    override protected def budgetTable: BudgetTable = StorageTestBudgets
    given env: CardanoInfo = TestUtil.testEnvironment
    given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    val evaluator = PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)

    val rootKey = hex"deadbeef"
    val prefix = hex"4c414e" // "LAN"

    def makeCreator(chunkSize: Int = 1000) = StorageTransactions(
      env = env,
      evaluator = evaluator,
      rootKey = rootKey,
      prefix = prefix,
      chunkSize = chunkSize
    )

    private def makeProvider(): Emulator =
        Emulator(
          initialUtxos = Map(
            Input(TestUtil.genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
            Input(TestUtil.genesisHash, 1) -> Output(Alice.address, Value.ada(5000))
          ),
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

    test("store data in single chunk"):
        val data = ByteString.fromString("Hello, Cardano!")
        val creator = makeCreator()
        val provider = makeProvider()
        val userUtxos = provider.findUtxos(Alice.address).await().toOption.get

        val txs = creator.storeData(
          data = data,
          userUtxos = userUtxos,
          sponsor = Alice.address,
          signer = Alice.signer
        )

        assert(txs.length == 1, s"Expected 1 transaction, got ${txs.length}")

        txs.foreach(tx => provider.submitOk(tx))

        val storageUtxos = provider.findUtxos(creator.scriptAddress).await().toOption.get
        assert(
          storageUtxos.size == 1,
          s"Expected 1 UTxO (root with data), got ${storageUtxos.size}"
        )

        val root = storageUtxos.values.head.inlineDatum.get.to[Element]
        val rootData = root.data match
            case ElementData.Root(Data.B(bytes)) => bytes
            case _                               => fail("Expected Root with ByteString data")
        assert(rootData == data)

    test("store data across 3 chunks"):
        val data = ByteString.fromArray(Array.fill(250)(0x42.toByte))
        val creator = makeCreator(chunkSize = 100)
        val provider = makeProvider()
        val userUtxos = provider.findUtxos(Alice.address).await().toOption.get

        val txs = creator.storeData(
          data = data,
          userUtxos = userUtxos,
          sponsor = Alice.address,
          signer = Alice.signer
        )

        assert(txs.length == 3, s"Expected 3 transactions, got ${txs.length}")

        txs.zipWithIndex.foreach { case (tx, idx) =>
            pinLabel(s"chunk ${idx + 1}")(provider.submitOk(tx))
        }

        val storageUtxos = provider.findUtxos(creator.scriptAddress).await().toOption.get
        assert(
          storageUtxos.size == 3,
          s"Expected 3 UTxOs (root + 2 nodes), got ${storageUtxos.size}"
        )

        val reconstructed = creator.readData(storageUtxos.map(Utxo.apply))
        assert(reconstructed == data, "Reconstructed data doesn't match original")
