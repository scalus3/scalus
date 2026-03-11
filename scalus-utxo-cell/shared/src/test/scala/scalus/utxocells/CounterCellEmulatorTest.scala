package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.onchain.plutus.v1 as onchain
import scalus.testing.kit.Party.Alice

class CounterCellEmulatorTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val cellDef: UtxoCellBuilder.CellDef = UtxoCellBuilder.CellDef(
      compiled = CounterCellCompilation.compiled,
      tokenName = CounterCell.beaconName,
      network = scalus.cardano.address.Network.Mainnet
    )

    import UtxoCellBuilder.*

    test("CounterCell lifecycle: mint -> increment -> reset (terminate)") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000))
        )

        val emulator = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        val scriptAddr = cellDef.scriptAddress

        // 1. Mint: create cell with initial state CounterState(0)
        val aliceUtxo = Utxo(emulator.utxos.head)
        val mintTx = TxBuilder(testEnv)
            .mintCell(cellDef, Data.I(0), CounterState(BigInt(0)), Value.ada(10))
            .spend(aliceUtxo)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"Mint transaction should succeed: $mintResult")

        // 2. Find cell UTxO by script address
        val cellEntry = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry.isDefined, "Should find cell UTxO at script address")
        val (cellInput, cellOutput) = cellEntry.get
        val cellUtxo = Utxo(cellInput, cellOutput)

        // Verify initial state datum
        val initialDatum = cellOutput.datumOption.flatMap(_.dataOption)
        assert(
          initialDatum.contains(CounterState(BigInt(0)).toData),
          "Initial datum should be CounterState(0)"
        )

        // 3. Spend: increment counter
        val fundingUtxo1 = Utxo(emulator.utxos.find(_._2.address != scriptAddr).get)
        val incrementTx = TxBuilder(testEnv)
            .spendCellRaw(
              cellDef,
              cellUtxo,
              CounterAction.Increment.toData,
              Some(CounterState(BigInt(1)))
            )
            .spend(fundingUtxo1)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val incrementResult = emulator.submitSync(incrementTx)
        assert(incrementResult.isRight, s"Increment transaction should succeed: $incrementResult")

        // 4. Find updated cell, verify state is CounterState(1)
        val cellEntry2 = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry2.isDefined, "Should find updated cell UTxO")
        val (cellInput2, cellOutput2) = cellEntry2.get
        val cellUtxo2 = Utxo(cellInput2, cellOutput2)
        val updatedDatum = cellOutput2.datumOption.flatMap(_.dataOption)
        assert(
          updatedDatum.contains(CounterState(BigInt(1)).toData),
          "Updated datum should be CounterState(1)"
        )

        // 5. Reset (terminal): spend + burn beacon
        val fundingUtxo2 = Utxo(emulator.utxos.find(_._2.address != scriptAddr).get)
        val resetTx = TxBuilder(testEnv)
            .spendCellRaw[CounterState](cellDef, cellUtxo2, CounterAction.Reset.toData, None)
            .spend(fundingUtxo2)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val resetResult = emulator.submitSync(resetTx)
        assert(resetResult.isRight, s"Reset (terminate) transaction should succeed: $resetResult")

        // 6. Verify cell is gone
        val cellEntry3 = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry3.isEmpty, "Cell UTxO should be consumed after terminal transition")
    }

    // -- CounterCellV2 (CellValidator + CellContext) emulator tests --

    val cellDefV2: UtxoCellBuilder.CellDef = UtxoCellBuilder.CellDef(
      compiled = CounterCellV2Compilation.compiled,
      tokenName = CounterCellV2.beaconName,
      network = scalus.cardano.address.Network.Mainnet
    )

    test("CounterCellV2 lifecycle: mint -> increment -> reset (terminate)") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000))
        )

        val emulator = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        val scriptAddr = cellDefV2.scriptAddress

        // 1. Mint: create cell with initial state CounterState(0)
        val aliceUtxo = Utxo(emulator.utxos.head)
        val mintTx = TxBuilder(testEnv)
            .mintCell(cellDefV2, Data.I(0), CounterState(BigInt(0)), Value.ada(10))
            .spend(aliceUtxo)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"V2 Mint should succeed: $mintResult")

        // 2. Find cell UTxO
        val cellEntry = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry.isDefined, "Should find V2 cell UTxO")
        val cellUtxo = Utxo(cellEntry.get)

        // 3. Spend: increment using OffChainCellContext
        val ctx = new OffChainCellContext(cellDefV2, cellUtxo, CounterAction.Increment.toData, testEnv)
        val currentState = CounterState(BigInt(0))
        val nextState = CounterCellV2.transition(currentState, CounterAction.Increment, ctx)
        assert(nextState == scalus.cardano.onchain.plutus.prelude.Option.Some(CounterState(BigInt(1))))
        assert(ctx.steps.isEmpty, "Counter transition should not accumulate steps")

        val fundingUtxo = Utxo(emulator.utxos.find(_._2.address != scriptAddr).get)
        val incrementTx = TxBuilder(testEnv)
            .spendCellCtx(
              cellDefV2,
              cellUtxo,
              CounterAction.Increment.toData,
              Some(CounterState(BigInt(1))),
              ctx
            )
            .spend(fundingUtxo)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val incrementResult = emulator.submitSync(incrementTx)
        assert(incrementResult.isRight, s"V2 Increment should succeed: $incrementResult")

        // 4. Verify updated state
        val cellEntry2 = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry2.isDefined, "Should find updated V2 cell")
        val cellUtxo2 = Utxo(cellEntry2.get)
        val updatedDatum = cellEntry2.get._2.datumOption.flatMap(_.dataOption)
        assert(updatedDatum.contains(CounterState(BigInt(1)).toData), "Datum should be CounterState(1)")

        // 5. Reset (terminal): spend + burn beacon
        val ctx2 = new OffChainCellContext(cellDefV2, cellUtxo2, CounterAction.Reset.toData, testEnv)
        val nextState2 = CounterCellV2.transition(CounterState(BigInt(1)), CounterAction.Reset, ctx2)
        assert(nextState2 == scalus.cardano.onchain.plutus.prelude.Option.None)

        val fundingUtxo2 = Utxo(emulator.utxos.find(_._2.address != scriptAddr).get)
        val resetTx = TxBuilder(testEnv)
            .spendCellCtx[CounterState](
              cellDefV2,
              cellUtxo2,
              CounterAction.Reset.toData,
              None,
              ctx2
            )
            .spend(fundingUtxo2)
            .build(changeTo = Alice.address)
            .sign(Alice.signer)
            .transaction

        val resetResult = emulator.submitSync(resetTx)
        assert(resetResult.isRight, s"V2 Reset should succeed: $resetResult")

        // 6. Verify cell is gone
        val cellEntry3 = emulator.utxos.find(_._2.address == scriptAddr)
        assert(cellEntry3.isEmpty, "V2 cell should be consumed after terminal transition")
    }

    // -- UtxoCellDef tests --

    val counterCell = new UtxoCellDef[CounterState, CounterAction](
      compiled = CounterCellV2Compilation.compiled,
      tokenName = CounterCellV2.beaconName,
      transition = CounterCellV2.transition
    )

    test("UtxoCellDef lifecycle: init -> apply(Increment) -> apply(Reset)") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000))
        )

        val emulator = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // 1. Init: mint beacon + send initial state
        val mintTx = TxBuilder(testEnv)
            .initUtxoCell(counterCell, CounterState(BigInt(0)), Value.ada(10))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val mintResult = emulator.submitSync(mintTx)
        assert(mintResult.isRight, s"UtxoCellDef init should succeed: $mintResult")

        // 2. Verify cell exists with correct state
        val cellUtxo = counterCell.findUtxo(emulator.utxos)
        assert(cellUtxo.isDefined, "Should find cell by beacon")
        assert(counterCell.currentState(cellUtxo.get) == CounterState(BigInt(0)))

        // 3. Apply Increment (eager variant with utxos)
        val incrementTx = TxBuilder(testEnv)
            .spendUtxoCell(counterCell, CounterAction.Increment, emulator.utxos)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val incrementResult = emulator.submitSync(incrementTx)
        assert(incrementResult.isRight, s"UtxoCellDef increment should succeed: $incrementResult")

        // 4. Verify updated state
        val cellUtxo2 = counterCell.findUtxo(emulator.utxos)
        assert(cellUtxo2.isDefined, "Should find updated cell")
        assert(counterCell.currentState(cellUtxo2.get) == CounterState(BigInt(1)))

        // 5. Apply Reset (deferred variant — resolved during complete)
        val resetTx = TxBuilder(testEnv)
            .spendUtxoCell(counterCell, CounterAction.Reset)
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val resetResult = emulator.submitSync(resetTx)
        assert(resetResult.isRight, s"UtxoCellDef reset should succeed: $resetResult")

        // 6. Verify cell is gone
        val cellUtxo3 = counterCell.findUtxo(emulator.utxos)
        assert(cellUtxo3.isEmpty, "Cell should be consumed after terminal transition")
    }

    test("fromLedgerValue round-trips correctly") {
        val offchainValue = Value.ada(42) + Value.asset(cellDef.policyId, cellDef.assetName, 1)
        val onchainValue = UtxoCellBuilder.fromLedgerValue(offchainValue)
        val roundTripped = onchainValue.toLedgerValue
        assert(roundTripped == offchainValue, s"Round-trip failed: $roundTripped != $offchainValue")
    }

    test("toOffchainAddress converts PubKeyCredential correctly") {
        val hash28 = ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        val onchainAddr = onchain.Address.fromPubKeyHash(onchain.PubKeyHash(hash28))
        val offchainAddr = UtxoCellBuilder.toOffchainAddress(onchainAddr, Network.Mainnet)
        offchainAddr match
            case sa: ShelleyAddress =>
                assert(sa.network == Network.Mainnet)
                assert(sa.payment == ShelleyPaymentPart.Key(AddrKeyHash(hash28)))
                assert(sa.delegation == ShelleyDelegationPart.Null)
            case _ => fail("Expected ShelleyAddress")
    }

    test("toOffchainAddress converts ScriptCredential correctly") {
        val hash28 = ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
        val onchainAddr = onchain.Address.fromScriptHash(hash28)
        val offchainAddr = UtxoCellBuilder.toOffchainAddress(onchainAddr, Network.Testnet)
        offchainAddr match
            case sa: ShelleyAddress =>
                assert(sa.network == Network.Testnet)
                assert(sa.payment == ShelleyPaymentPart.Script(Hash.scriptHash(hash28)))
                assert(sa.delegation == ShelleyDelegationPart.Null)
            case _ => fail("Expected ShelleyAddress")
    }
}
