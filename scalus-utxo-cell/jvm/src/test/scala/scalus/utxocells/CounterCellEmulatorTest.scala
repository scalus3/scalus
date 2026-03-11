package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.Alice
import scalus.utxocells.UtxoCellBuilder.*

import scala.concurrent.Await
import scala.concurrent.duration.*

class CounterCellBlockchainReaderTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val counterCell = new UtxoCellDef[CounterState, CounterAction](
      compiled = CounterCellV2Compilation.compiled,
      tokenName = CounterCellV2.beaconName,
      transition = CounterCellV2.transition
    )

    test("UtxoCellDef with BlockchainReader: init -> apply(Increment) -> apply(Reset)") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000))
        )

        val emulator = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // 1. Init via BlockchainReader complete
        val mintTx = Await
            .result(
              TxBuilder(testEnv)
                  .initUtxoCell(counterCell, CounterState(BigInt(0)), Value.ada(10))
                  .complete(emulator, Alice.address),
              5.seconds
            )
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"Init via reader should succeed: $mintResult")

        // 2. Apply Increment (deferred, resolved via BlockchainReader)
        val incrementTx = Await
            .result(
              TxBuilder(testEnv)
                  .spendUtxoCell(counterCell, CounterAction.Increment)
                  .complete(emulator, Alice.address),
              5.seconds
            )
            .sign(Alice.signer)
            .transaction

        val incrementResult = emulator.submitSync(incrementTx)
        assert(incrementResult.isRight, s"Increment via reader should succeed: $incrementResult")

        // 3. Verify state is CounterState(1)
        val cellUtxo = counterCell.findUtxo(emulator.utxos)
        assert(cellUtxo.isDefined, "Should find cell after increment")
        assert(counterCell.currentState(cellUtxo.get) == CounterState(BigInt(1)))

        // 4. Apply Reset (deferred, terminal)
        val resetTx = Await
            .result(
              TxBuilder(testEnv)
                  .spendUtxoCell(counterCell, CounterAction.Reset)
                  .complete(emulator, Alice.address),
              5.seconds
            )
            .sign(Alice.signer)
            .transaction

        val resetResult = emulator.submitSync(resetTx)
        assert(resetResult.isRight, s"Reset via reader should succeed: $resetResult")

        // 5. Verify cell is gone
        val cellUtxo2 = counterCell.findUtxo(emulator.utxos)
        assert(cellUtxo2.isEmpty, "Cell should be consumed after terminal transition")
    }
}
