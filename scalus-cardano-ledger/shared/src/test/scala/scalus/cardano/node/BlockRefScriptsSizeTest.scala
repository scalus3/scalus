package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.emulator.{BlockForge, BlockIssuer, ChainConfig, ForgeError}
import scalus.cardano.node.stream.ChainPoint
import scalus.cardano.txbuilder.{Datum, ScriptSource, ThreeArgumentPlutusScriptWitness, TxBuilder}
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.Term
import scalus.uplc.builtin.{ByteString, Data}

/** The total reference-script size of a block, spec [BB-1] .. [BB-4]: `BodyRefScriptsSizeTooBig`
  * and `totalRefScriptSizeInBlock` of the Conway BBODY rule (C/Rules/Bbody.hs:347-382).
  */
class BlockRefScriptsSizeTest extends AnyFunSuite {

    private given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val alice = Alice.address(Network.Mainnet)
    private val bob = Bob.address(Network.Mainnet)

    private val pv10 = ProtocolVersion(10, 0)
    private val pv11 = ProtocolVersion(11, 0)

    /** A script whose `originalBytesSize` is `size`; it is only referenced, never run. */
    private def script(size: Int): Script =
        Script.PlutusV3(ByteString.fromArray(new Array[Byte](size)))

    private def withScript(size: Int): TransactionOutput =
        TransactionOutput(bob, Value.ada(2), None, Some(ScriptRef(script(size))))

    private def input(n: Int): TransactionInput = Input(genesisHash, n)

    private def tx(
        inputs: Set[TransactionInput] = Set.empty,
        referenceInputs: Set[TransactionInput] = Set.empty,
        outputs: IndexedSeq[TransactionOutput] = IndexedSeq.empty,
        collateralReturn: Option[TransactionOutput] = None,
        isValid: Boolean = true
    ): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(inputs),
            outputs = outputs.map(Sized(_)),
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(referenceInputs),
            collateralReturnOutput = collateralReturn.map(Sized(_))
          )
        ).copy(isValid = isValid)

    test("a later tx that references an output of an earlier valid tx counts it at PV 11") {
        // ledger-rules spec [BB-3]
        val start: Utxos = Map(input(0) -> withScript(1000))
        val first = tx(inputs = Set(input(1)), outputs = IndexedSeq(withScript(500)))
        val second = tx(referenceInputs = Set(Input(first.id, 0), input(0)))
        val txs = IndexedSeq(first, second)
        assert(BlockForge.refScriptsSize(txs, start, pv11) == 1500L)
        // At PV 10 every tx resolves against the UTxO at the block start only.
        assert(BlockForge.refScriptsSize(txs, start, pv10) == 1000L)
    }

    test("an earlier tx with isValid = false adds only its collateral return") {
        // ledger-rules spec [BB-4]
        val first = tx(
          inputs = Set(input(1)),
          outputs = IndexedSeq(withScript(500)),
          collateralReturn = Some(withScript(300)),
          isValid = false
        )
        // the collateral return is at the index after the outputs (mkCollateralTxIn,
        // B/Collateral.hs:52-59)
        val returnOnly = tx(referenceInputs = Set(Input(first.id, 1)))
        assert(BlockForge.refScriptsSize(IndexedSeq(first, returnOnly), Map.empty, pv11) == 300L)
        // the regular output of the invalid tx is not in the lookup
        val outputOnly = tx(referenceInputs = Set(Input(first.id, 0)))
        assert(BlockForge.refScriptsSize(IndexedSeq(first, outputOnly), Map.empty, pv11) == 0L)
    }

    test("an input spent by an earlier tx still counts for a later tx") {
        // ledger-rules spec [BB-3]
        val start: Utxos = Map(input(0) -> withScript(1000))
        val first = tx(inputs = Set(input(0)))
        val second = tx(referenceInputs = Set(input(0)))
        assert(BlockForge.refScriptsSize(IndexedSeq(first, second), start, pv11) == 2000L)
    }

    // A valid tx may reference at most 200 KiB of scripts (TxRefScriptsSizeValidator), so five
    // such txs fit a block and the sixth does not: 5 * 204 800 = 1 024 000 <= 1 048 576 < 6 * 204 800.
    private val refScriptSize = 200 * 1024
    private val txCount = 6

    /** Six txs, each referencing its own script UTxO: five of 200 KiB and one of `last` bytes. */
    private def referencingScripts(last: Int): (IndexedSeq[Transaction], Utxos) = {
        val sizes = IndexedSeq.fill(txCount - 1)(refScriptSize) :+ last
        val utxos: Utxos = sizes.zipWithIndex.map((size, i) => input(i) -> withScript(size)).toMap
        (sizes.indices.map(i => tx(referenceInputs = Set(input(i)))), utxos)
    }

    test("a block of exactly 1 MiB of reference scripts fits, one byte more does not") {
        // ledger-rules spec [BB-1]
        // totalRefScriptSize <= maxRefScriptSizePerBlock (C/Rules/Bbody.hs:347-348), with
        // ppMaxRefScriptSizePerBlockG = 1024 * 1024 (C/PParams.hs:981)
        val params = testEnv.protocolParams
        val rest = 1024 * 1024 - (txCount - 1) * refScriptSize
        val (exact, exactUtxos) = referencingScripts(rest)
        assert(BlockForge.refScriptsSize(exact, exactUtxos, pv11) == 1024L * 1024)
        assert(BlockForge.fitsBlock(exact, params, exactUtxos))
        assert(BlockForge.prefixThatFits(exact, params, exactUtxos) == txCount)
        val (over, overUtxos) = referencingScripts(rest + 1)
        assert(!BlockForge.fitsBlock(over, params, overUtxos))
        assert(BlockForge.prefixThatFits(over, params, overUtxos) == txCount - 1)
    }

    // The Emulator scenarios.

    private val refScriptUtxo =
        Utxo(Input(genesisHash, txCount), withScript(refScriptSize))

    private val initialUtxos: Utxos =
        (0 until txCount).map(i => input(i) -> TransactionOutput(alice, Value.ada(1000))).toMap +
            (refScriptUtxo.input -> refScriptUtxo.output)

    /** A payment from genesis UTxO `index` that references the 200 KiB script. */
    private def referencing(index: Int): Transaction = {
        val utxo = Utxo(input(index), initialUtxos(input(index)))
        TxBuilder(testEnv)
            .spend(utxo)
            .references(refScriptUtxo)
            .payTo(bob, Value.ada(2))
            .complete(
              Map(utxo.input -> utxo.output, refScriptUtxo.input -> refScriptUtxo.output),
              alice
            )
            .sign(Alice.signer)
            .transaction
    }

    private def emulator(): Emulator =
        Emulator.withState(
          EmulatorInitialState(initialUtxos),
          rules.Context.testMainnet(),
          ChainConfig.manual
        )

    test("six txs of 200 KiB of reference scripts forge as two blocks") {
        // ledger-rules spec [BB-1]
        val txs = (0 until txCount).map(referencing)
        assert(BlockForge.refScriptsSize(txs, initialUtxos, pv11) == txCount.toLong * refScriptSize)
        val e = emulator()
        txs.foreach(tx => assert(e.submitSync(tx) == Right(tx.id)))
        e.produceBlock()
        val state = EmulatorBase.stateOf(e)
        assert(state.selectedChain.last.txs.map(_.tx) == txs.take(5))
        assert(e.mempoolTxs == txs.drop(5))
        e.produceBlock()
        assert(EmulatorBase.stateOf(e).selectedChain.last.txs.map(_.tx) == txs.drop(5))
        assert(e.mempoolTxs.isEmpty)
    }

    test("a mempool forged as two blocks counts both for the issuer") {
        // ledger-rules spec [ES-4a]
        // In Instant each tx forges its own block; rollback(6) returns the six txs to the mempool,
        // validated on genesis, and the forge makes two blocks of them without revalidating. The
        // second block's entries still carry the epoch state from before the first block, so the
        // count must come from the ticked base of each forge.
        val pool = PoolKeyHash.fromHex("b7" * 28)
        val e = Emulator.withState(
          EmulatorInitialState(initialUtxos),
          rules.Context.testMainnet(),
          ChainConfig.instant.withBlockIssuer(BlockIssuer.pool(pool))
        )
        (0 until txCount).map(referencing).foreach(tx => assert(e.submitSync(tx) == Right(tx.id)))
        assert(EmulatorBase.stateOf(e).tipSnapshot.epoch.blocksMadeCur == Map(pool -> 6L))
        assert(e.rollback(txCount).isRight)
        val state = EmulatorBase.stateOf(e)
        assert(state.selectedChain.map(_.txs.size) == Vector(5, 1))
        assert(state.tipSnapshot.epoch.blocksMadeCur == Map(pool -> 2L))
    }

    test("a tx with isValid = false over 200 KiB of reference scripts is refused at submit") {
        // ledger-rules spec [BB-1]
        // LEDGER skips the per-tx limit when isValid is false (TxRefScriptsSizeValidator), but the
        // node's mempool measure applies it to every tx it admits (txMeasureRefScripts,
        // ouroboros-consensus Shelley/Ledger/Mempool.hs). Without it the forge, which always takes
        // the first mempool tx, would build a block of 1.2 MiB of reference scripts.
        val failing = Script.PlutusV3(Term.Error().plutusV3.cborByteString)
        val locked = Utxo(
          input(100),
          TransactionOutput(
            Address(Network.Mainnet, Credential.ScriptHash(failing.scriptHash)),
            Value.ada(5),
            Data.unit
          )
        )
        val refs = (0 until txCount).map(i => Utxo(input(200 + i), withScript(refScriptSize)))
        // The tiered fee of 1.2 MiB of reference scripts is about 12 000 ADA, the collateral 150%
        // of it.
        val funds = Utxo(input(300), TransactionOutput(alice, Value.ada(100_000)))
        val utxos = Utxos((locked +: funds +: refs)*)
        val tx = TxBuilder
            .withConstMaxBudgetEvaluator(testEnv)
            .spend(
              locked,
              ThreeArgumentPlutusScriptWitness(
                ScriptSource.PlutusScriptValue(failing),
                Data.unit,
                Datum.DatumInlined
              )
            )
            .references(refs.head, refs.tail*)
            .payTo(bob, Value.ada(2))
            .complete(utxos, alice)
            .sign(Alice.signer)
            .transaction
            .copy(isValid = false)
        val e = Emulator.withState(
          EmulatorInitialState(utxos),
          rules.Context.testMainnet(),
          ChainConfig.manual
        )
        assert(e.submitSync(tx).left.map(_.rule) == Left("TxRefScriptsSizeTooBig"))
        assert(e.mempoolTxs.isEmpty)
    }

    test("produceBlock(on, ...) rejects txs over 1 MiB of reference scripts") {
        // ledger-rules spec [BB-2]
        val txs = (0 until txCount).map(referencing)
        val e = emulator()
        val before = EmulatorBase.stateOf(e)
        assert(e.produceBlock(ChainPoint.origin, txs) == Left(ForgeError.BlockTooBig))
        assert(EmulatorBase.stateOf(e) eq before)
        assert(e.produceBlock(ChainPoint.origin, txs.take(5)).isRight)
    }
}
