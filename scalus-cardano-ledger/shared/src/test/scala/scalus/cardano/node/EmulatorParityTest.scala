package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}

/** The JVM and the JavaScript `Emulator` are two source files claiming to run the same ledger rules
  * over the same state machine. This suite is what makes that claim mechanical: it lives in the
  * shared test source set, so `scalusCardanoLedgerJVM/test` and `scalusCardanoLedgerJS/test` each
  * run it against their own platform's `Emulator`, and a divergence fails one of them.
  *
  * It therefore uses only what both platforms expose:
  *   - no `scalus.utils.await` (JVM-only), so every submission goes through `submitSync` and every
  *     transaction is built with the synchronous `TxBuilder.complete(utxos, sponsor)` overload;
  *   - only the factory arities both companions offer (the JVM one spells them as overloads for
  *     Java, the JavaScript one as default arguments).
  *
  * Every assertion pins a concrete value rather than comparing two emulators to each other, so each
  * platform checks identical facts even though the two runs never meet.
  */
class EmulatorParityTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private def genesisUtxos(values: Value*): Utxos =
        values.zipWithIndex.map { case (value, index) =>
            Input(genesisHash, index) -> Output(alice, value)
        }.toMap

    /** An emulator over `utxos` with the default rule set - the configuration a caller gets from
      * `Emulator(initialUtxos)` on either platform.
      */
    private def emulatorOver(utxos: Utxos): Emulator = Emulator(initialUtxos = utxos)

    private def payment(utxos: Utxos, amount: Value): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, amount)
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction

    /** [[EmulatorBase.getDatum]]'s answer, without `scalus.utils.await` (JVM-only). The emulator
      * answers out of memory with an already-completed `Future`, so its value is there to read.
      */
    private def datumOf(emulator: Emulator, hash: DataHash): Option[Data] =
        emulator
            .getDatum(hash)
            .value
            .getOrElse(fail("the emulator answers synchronously"))
            .get

    private def paymentWithInlineDatum(utxos: Utxos, amount: Value, datum: Data): Transaction =
        TxBuilder(testEnv)
            .payTo(bob, amount, (_: Transaction) => datum)
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction

    test("a valid transaction is applied, and the log, index and hash set agree about it") {
        val initialUtxos = genesisUtxos(Value.ada(100))
        val emulator = emulatorOver(initialUtxos)
        emulator.setSlot(42L)

        assert(emulator.appliedTxLog.isEmpty, "the log starts empty")
        assert(emulator.appliedTxIndex.isEmpty, "the index starts empty")
        assert(emulator.appliedTxs.isEmpty, "the hash set starts empty")

        val tx = payment(initialUtxos, Value.ada(10))
        assert(emulator.submitSync(tx) == Right(tx.id))

        val log = emulator.appliedTxLog
        assert(log.size == 1, s"expected exactly one applied transaction, got ${log.size}")
        val applied = log.head
        assert(applied.tx.id == tx.id)
        assert(applied.slot == 42L, "the record carries the slot the emulator was at")
        assert(applied.spent == initialUtxos, "the record resolves the inputs the tx consumed")

        // The three views of the applied set are derived from one another, so they cannot disagree.
        assert(emulator.appliedTxIndex == Map(tx.id -> applied))
        assert(emulator.appliedTxs == Set(tx.id))
        assert(emulator.hasTx(tx.id))
        assert(emulator.getTransaction(tx.id).contains(tx))
        assert(emulator.getAppliedTx(tx.id).contains(applied))
        assert(!emulator.hasTx(genesisHash), "an unknown hash is not applied")
        assert(emulator.getTransaction(genesisHash).isEmpty)

        // The ledger moved: the genesis input is consumed and the tx's own outputs replace it.
        assert(!emulator.utxos.contains(Input(genesisHash, 0)))
        assert(
          emulator.utxos.keySet.forall(_.transactionId == tx.id),
          "every remaining UTxO comes from the applied transaction"
        )
        assert(
          emulator.utxos.values.exists(o => o.address == bob && o.value == Value.ada(10)),
          "Bob was paid exactly 10 ADA"
        )
    }

    test("a double spend is rejected, names the same rule, and leaves the ledger untouched") {
        val initialUtxos = genesisUtxos(Value.ada(100))
        val emulator = emulatorOver(initialUtxos)

        val first = payment(initialUtxos, Value.ada(10))
        // A different amount, so a different transaction id - but built against the same UTxO set,
        // so it spends the very input `first` just consumed.
        val second = payment(initialUtxos, Value.ada(20))
        assert(first.id != second.id, "the two transactions must be distinct")
        assert(
          first.body.value.inputs == second.body.value.inputs,
          "precondition: both spend the same input"
        )

        assert(emulator.submitSync(first) == Right(first.id))
        val utxosAfterFirst = emulator.utxos

        val rejection = emulator.submitSync(second)
        assert(rejection.isLeft, s"the second spend of the same input must be rejected: $rejection")
        val error = rejection.left.getOrElse(fail("expected a Left"))
        assert(
          error.rule == "UtxoNotAvailable",
          s"a rejection names the condition that produced it, got '${error.rule}'"
        )

        // A rejected transaction leaves nothing behind.
        assert(emulator.utxos == utxosAfterFirst, "the UTxO set is unchanged by a rejection")
        assert(emulator.appliedTxs == Set(first.id))
        assert(emulator.appliedTxLog.map(_.txHash) == Seq(first.id))
        assert(emulator.appliedTxIndex.keySet == Set(first.id))
        assert(!emulator.hasTx(second.id))
    }

    test("applying a transaction captures its datums, and clearAppliedTxs keeps them") {
        val initialUtxos = genesisUtxos(Value.ada(100))
        val emulator = emulatorOver(initialUtxos)

        val datum = Data.I(7)
        val tx = paymentWithInlineDatum(initialUtxos, Value.ada(10), datum)
        assert(emulator.submitSync(tx) == Right(tx.id))

        val datumHash = DataHash.fromByteString(datum.dataHash)
        assert(emulator.datums.get(datumHash).contains(datum), "the inline datum is retained")
        assert(emulator.datums == EmulatorBase.extractDatums(tx))

        val utxosBefore = emulator.utxos
        val datumsBefore = emulator.datums

        emulator.clearAppliedTxs()

        // Bookkeeping is cleared...
        assert(emulator.appliedTxLog.isEmpty)
        assert(emulator.appliedTxIndex.isEmpty)
        assert(emulator.appliedTxs.isEmpty)
        assert(!emulator.hasTx(tx.id))
        assert(emulator.getTransaction(tx.id).isEmpty)
        // ...but the ledger, including the datum store, is not.
        assert(emulator.utxos == utxosBefore, "clearAppliedTxs does not touch the UTxO set")
        assert(emulator.datums == datumsBefore, "clearAppliedTxs does not touch the datum store")
    }

    test("slot control: setSlot and tick move the slot, and stamp the applied record") {
        val initialUtxos = genesisUtxos(Value.ada(100), Value.ada(200))
        val emulator = emulatorOver(initialUtxos)

        assert(emulator.currentSlotSync == 0L, "a fresh emulator starts at the testMainnet slot")
        emulator.setSlot(100L)
        assert(emulator.currentSlotSync == 100L)
        emulator.tick(5L)
        assert(emulator.currentSlotSync == 105L)

        val tx = payment(initialUtxos, Value.ada(10))
        assert(emulator.submitSync(tx) == Right(tx.id))
        assert(emulator.appliedTxLog.head.slot == 105L)

        // Moving the slot preserves the rest of the context, which the emulator reports through
        // `cardanoInfo` - a `Context(...)` rebuild rather than a `copy` would drop it.
        assert(emulator.cardanoInfo.network == Network.Mainnet)
        assert(emulator.evaluatorMode == Context.testMainnet().evaluatorMode)
    }

    test("a snapshot carries the whole state over, and then evolves independently") {
        val initialUtxos = genesisUtxos(Value.ada(100), Value.ada(200))
        val emulator = emulatorOver(initialUtxos)
        emulator.setSlot(11L)
        val first = paymentWithInlineDatum(initialUtxos, Value.ada(10), Data.I(1))
        assert(emulator.submitSync(first) == Right(first.id))

        val copy = emulator.snapshot()

        assert(copy.utxos == emulator.utxos, "the snapshot starts from the same UTxO set")
        assert(copy.certState == emulator.certState)
        assert(copy.datums == emulator.datums)
        assert(copy.appliedTxLog == emulator.appliedTxLog)
        assert(copy.appliedTxIndex == emulator.appliedTxIndex)
        assert(copy.appliedTxs == emulator.appliedTxs)
        assert(copy.currentSlotSync == 11L, "the snapshot keeps the slot")

        // Advancing the original leaves the snapshot where it was.
        emulator.setSlot(99L)
        val second = payment(emulator.utxos, Value.ada(5))
        assert(emulator.submitSync(second) == Right(second.id))

        assert(copy.currentSlotSync == 11L, "the snapshot's slot is its own")
        assert(copy.appliedTxs == Set(first.id), "the snapshot did not see the later transaction")
        assert(emulator.appliedTxs == Set(first.id, second.id))
        assert(copy.utxos != emulator.utxos)
    }

    test("initial state: registered stake credentials show up in the stake distribution") {
        val stakeCredential = Credential.KeyHash(Bob.addrKeyHash)
        val emulator = Emulator.withRegisteredStakeCredentials(
          genesisUtxos(Value.ada(500)),
          Map(stakeCredential -> Coin(1_000_000L))
        )

        val entry = emulator.stakeDistribution
            .find(_.credential == stakeCredential)
            .getOrElse(fail("the pre-registered credential is missing from the distribution"))
        assert(entry.rewards == Coin(1_000_000L))
        assert(entry.pool.isEmpty, "registered, but delegating to no pool")
        assert(emulator.getDelegation(stakeCredential).rewards == Coin(1_000_000L))
        assert(emulator.getDelegation(stakeCredential).poolId.isEmpty)
        assert(
          emulator.certState.dstate.rewards.get(stakeCredential).contains(Coin(1_000_000L)),
          "the reward balance lives in the cert state, not beside it"
        )
    }

    test("withAddresses funds each address with the same value") {
        val emulator = Emulator.withAddresses(Seq(alice, bob), Value.ada(250))
        assert(emulator.utxos.size == 2)
        assert(emulator.utxos.values.map(_.address).toSet == Set(alice, bob))
        assert(emulator.utxos.values.forall(_.value == Value.ada(250)))
        assert(emulator.appliedTxLog.isEmpty, "seeding is not a transaction")
    }

    test("addUtxo and removeUtxo edit the ledger directly on both platforms") {
        val emulator = emulatorOver(genesisUtxos(Value.ada(100)))
        val added = Input(TransactionHash.fromByteString(ByteString.fromHex("1" * 64)), 7)

        emulator.addUtxo(added, Output(bob, Value.ada(42)))
        assert(emulator.utxos.get(added).map(_.value).contains(Value.ada(42)))
        assert(emulator.appliedTxLog.isEmpty, "a direct edit is not a transaction")

        // Overwrites rather than duplicating.
        emulator.addUtxo(added, Output(bob, Value.ada(43)))
        assert(emulator.utxos.get(added).map(_.value).contains(Value.ada(43)))

        emulator.removeUtxo(added)
        assert(!emulator.utxos.contains(added))
        emulator.removeUtxo(added) // a second removal is a no-op, not a failure
        assert(emulator.utxos.size == 1, "the genesis UTxO is untouched")
    }

    test("a datum held inline by a seeded or added UTxO is one getDatum answers for") {
        val datum = Data.I(77)
        val hash = DataHash.fromByteString(datum.dataHash)
        val scriptUtxo =
            Input(genesisHash, 9) -> Output(bob, Value.ada(5), DatumOption.Inline(datum))

        // Seeded at construction: the datum store is not built from the applied-tx log alone.
        val seeded = emulatorOver(genesisUtxos(Value.ada(100)) + scriptUtxo)
        assert(datumOf(seeded, hash).contains(datum), "a seeded inline datum is indexed")

        // And the same for one added after construction.
        val added = emulatorOver(genesisUtxos(Value.ada(100)))
        assert(datumOf(added, hash).isEmpty, "precondition: the emulator has not seen it yet")
        added.addUtxo(scriptUtxo._1, scriptUtxo._2)
        assert(datumOf(added, hash).contains(datum), "an added inline datum is indexed")

        // Spending the output does not un-see the datum, which is how a node behaves.
        added.removeUtxo(scriptUtxo._1)
        assert(datumOf(added, hash).contains(datum), "the datum store keeps what it has seen")
    }

    test("re-applying a transaction appends to the log while the index keeps one entry") {
        val initialUtxos = genesisUtxos(Value.ada(100))
        val emulator = emulatorOver(initialUtxos)
        val tx = payment(initialUtxos, Value.ada(10))
        assert(emulator.submitSync(tx) == Right(tx.id))

        // Put the consumed input back, which is the only way to get the ledger rules to accept the
        // same transaction twice.
        for (input, output) <- initialUtxos do emulator.addUtxo(input, output)
        assert(emulator.submitSync(tx) == Right(tx.id), "the input is available again")

        assert(emulator.appliedTxLog.size == 2, "the log records one entry per application")
        assert(emulator.appliedTxIndex.size == 1, "the index is keyed by hash")
        assert(emulator.appliedTxs == Set(tx.id))
        assert(
          emulator.appliedTxLog.map(_.txHash).toSet == emulator.appliedTxs,
          "the log and the hash set hold the same hashes, however many times each was applied"
        )
    }

    test("an emulator restored from an applied-tx log rebuilds its index, set and datums") {
        val initialUtxos = genesisUtxos(Value.ada(100))
        val emulator = emulatorOver(initialUtxos)
        val tx = paymentWithInlineDatum(initialUtxos, Value.ada(10), Data.I(3))
        assert(emulator.submitSync(tx) == Right(tx.id))

        // The constructor takes only the log; the index, the hash set and the datum store are all
        // derived from it, so a restored emulator cannot disagree with the one it was built from.
        val restored = Emulator(
          initialUtxos = emulator.utxos,
          initialAppliedTxLog = emulator.appliedTxLog.toVector
        )
        assert(restored.appliedTxIndex == emulator.appliedTxIndex)
        assert(restored.appliedTxs == emulator.appliedTxs)
        assert(restored.datums == emulator.datums)
        assert(restored.getAppliedTx(tx.id).map(_.slot).contains(0L))
    }
}
