package scalus.testing

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

class ImmutableEmulatorTest extends AnyFunSuite {

    given CardanoInfo = CardanoInfo.mainnet

    test("fromEmulator carries over evaluatorMode (issue #314)") {
        val emulator = Emulator(
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost)
        )

        val immutable = ImmutableEmulator.fromEmulator(emulator)

        assert(
          immutable.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost,
          "fromEmulator must not silently revert evaluatorMode to Validate"
        )
    }

    test("fromEmulator preserves evaluatorMode after a slot advance (issue #314)") {
        val emulator = Emulator(
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost)
        )
        emulator.setSlot(100L)

        val immutable = ImmutableEmulator.fromEmulator(emulator)

        assert(immutable.currentSlot == 100L)
        assert(immutable.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost)
    }

    test("submit appends to the applied-transaction log; original is unchanged") {
        val genesisHash =
            TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val emu = ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv.testMainnet(),
          validators = Set.empty
        ).setSlot(7L)

        assert(emu.appliedTxLog.isEmpty, "log should start empty")

        val tx = TxBuilder(CardanoInfo.mainnet)
            .payTo(Bob.address, Value.ada(10))
            .complete(emu.asReader, Alice.address)
            .await()
            .transaction

        emu.submit(tx) match
            case Right((hash, next)) =>
                assert(hash == tx.id)
                assert(next.appliedTxLog.size == 1)
                assert(next.appliedTxLog.head.slot == 7L)
                assert(next.appliedTxLog.head.spent == initialUtxos)
                assert(next.getTransaction(tx.id).contains(tx))
                assert(next.appliedTxIndex.keySet == Set(tx.id))
                assert(next.getAppliedTx(tx.id).map(_.slot).contains(7L))
                assert(next.hasTx(tx.id))
                // immutability: the original emulator keeps an empty log
                assert(emu.appliedTxLog.isEmpty, "original emulator must be unchanged")
            case Left(e) => fail(s"submit failed: $e")
    }

    test("fromEmulator and toEmulator preserve certState") {
        val stakeCred = Credential.ScriptHash(PlutusV3.alwaysOk.script.scriptHash)
        val reward = Coin(42_000_000L)
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = Map.empty,
          initialStakeRewards = Map(stakeCred -> reward)
        )
        assert(emulator.certState.dstate.accounts.get(stakeCred).map(_.balance).contains(reward))

        val immutable = ImmutableEmulator.fromEmulator(emulator)
        assert(
          immutable.state.certState.dstate.accounts.get(stakeCred).map(_.balance).contains(reward),
          "fromEmulator must carry over certState"
        )

        val roundTripped = immutable.toEmulator
        assert(
          roundTripped.certState.dstate.accounts.get(stakeCred).map(_.balance).contains(reward),
          "toEmulator must carry over certState"
        )
    }

    test("toEmulator reconstructs datums and index from the applied-tx log") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val provider = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )
        val tx = TxBuilder(CardanoInfo.mainnet)
            .payTo(Bob.address, Value.ada(10), (_: Transaction) => Data.unit)
            .complete(provider, Alice.address)
            .await()
            .transaction
        assert(provider.submit(tx).await().isRight)
        assert(provider.datums.nonEmpty, "sanity: the submitted tx carried an inline datum")

        // ImmutableEmulator has no separate datum store, so datums must be recovered from the
        // applied-tx log when converting back to a mutable Emulator.
        val rebuilt = ImmutableEmulator.fromEmulator(provider).toEmulator
        assert(rebuilt.appliedTxs == provider.appliedTxs)
        assert(rebuilt.datums == provider.datums, "toEmulator must reconstruct datums from the log")
        assert(rebuilt.getTransaction(tx.id).contains(tx))
    }

    test("clearAppliedTxs resets the immutable log/index, keeping ledger state") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val emu = ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv.testMainnet(),
          validators = Set.empty
        )
        val tx = TxBuilder(CardanoInfo.mainnet)
            .payTo(Bob.address, Value.ada(10))
            .complete(emu.asReader, Alice.address)
            .await()
            .transaction
        val next = emu.submit(tx) match
            case Right((_, e)) => e
            case Left(e)       => fail(s"submit failed: $e")
        assert(next.appliedTxLog.nonEmpty && next.appliedTxIndex.nonEmpty)

        val cleared = next.clearAppliedTxs
        assert(cleared.appliedTxLog.isEmpty)
        assert(cleared.appliedTxIndex.isEmpty)
        assert(cleared.getTransaction(tx.id).isEmpty)
        assert(!cleared.hasTx(tx.id))
        assert(cleared.utxos == next.utxos, "ledger state must be untouched")
    }

    test("ImmutableEmulator retains and resolves datums; clearAppliedTxs keeps them") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val emu = ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv.testMainnet(),
          validators = Set.empty
        )
        val tx = TxBuilder(CardanoInfo.mainnet)
            .payTo(Bob.address, Value.ada(10), (_: Transaction) => Data.unit)
            .complete(emu.asReader, Alice.address)
            .await()
            .transaction
        val next = emu.submit(tx) match
            case Right((_, e)) => e
            case Left(e)       => fail(s"submit failed: $e")

        assert(next.datums.nonEmpty, "submit must retain the inline datum")
        val (dh, d) = next.datums.head
        assert(next.getDatum(dh).contains(d), "getDatum resolves the datum")
        assert(
          next.asReader.getDatum(dh).await().contains(d),
          "asReader resolves it (was always None before)"
        )

        // clearAppliedTxs wipes history but keeps the datum resolution cache
        val cleared = next.clearAppliedTxs
        assert(cleared.appliedTxLog.isEmpty)
        assert(cleared.getDatum(dh).contains(d), "datums survive clearAppliedTxs")
    }

    // spec 13.3: the treasury, as of the last epoch boundary, sits in the env
    private val slotConfig = CardanoInfo.mainnet.slotConfig
    private val epochStart = slotConfig.firstSlotOfEpoch(500)

    /** An emulator with a treasury of 1000 lovelace and one accepted 5-lovelace donation. */
    private def emulatorWithDonation: (Emulator, Transaction) = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val context = Context.testMainnet(epochStart + 10)
        val emulator = Emulator(
          initialUtxos = Map(Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))),
          initialContext = context.copy(env = context.env.copy(treasury = Coin(1000)))
        )
        val tx = TxBuilder(CardanoInfo.mainnet)
            .donateToTreasury(Coin(5))
            .setCurrentTreasuryValue(Coin(1000))
            .complete(emulator.utxos, Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(emulator.submitSync(tx) == Right(tx.id))
        (emulator, tx)
    }

    test("fromEmulator and toEmulator keep the fees, the donations and the treasury") {
        // spec [SC-7c]
        val (emulator, tx) = emulatorWithDonation

        val immutable = ImmutableEmulator.fromEmulator(emulator)
        assert(immutable.state.fees == tx.body.value.fee)
        assert(immutable.state.donation == Coin(5))
        assert(immutable.env.treasury == Coin(1000))

        val roundTripped = immutable.toEmulator
        assert(roundTripped.fees == tx.body.value.fee)
        assert(roundTripped.donation == Coin(5))
        assert(roundTripped.treasury == Coin(1000))
    }

    test("setSlot and advanceSlot across an epoch boundary move the donations") {
        // spec [SC-7a], [SC-7f]
        val immutable = ImmutableEmulator.fromEmulator(emulatorWithDonation._1)

        val sameEpoch = immutable.advanceSlot(1)
        assert(sameEpoch.env.treasury == Coin(1000))
        assert(sameEpoch.state.donation == Coin(5))

        for next <- Seq(
              immutable.setSlot(epochStart + slotConfig.epochLength),
              immutable.advanceSlot(slotConfig.epochLength)
            )
        do
            assert(next.env.treasury == Coin(1005))
            assert(next.state.donation == Coin.zero)
    }
}
