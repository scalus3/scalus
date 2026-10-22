package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.serialization.cbor.Cbor
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.{ByteString, Data}

class BlockForgeTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private def payment(index: Int, lovelace: Long): Transaction = {
        val utxos = Map(
          Input(genesisHash, index) -> Output(Alice.address(Network.Mainnet), Value.ada(100))
        )
        TxBuilder(CardanoInfo.mainnet)
            .payTo(Bob.address(Network.Mainnet), Value.lovelace(lovelace))
            .complete(utxos, Alice.address(Network.Mainnet))
            .sign(Alice.signer)
            .transaction
    }

    private val first = payment(0, 2_000_000)
    private val second = payment(1, 3_000_000)
    private val withAux = second.copy(auxiliaryData = Some(KeepRaw(AuxiliaryData.AlonzoFormat())))
    private val parent = BlockHash.fromByteString(ByteString.fromHex("ab" * 32))
    private val version = ProtocolVersion(10, 7)

    private def forge(txs: IndexedSeq[Transaction]): Block =
        BlockForge.forge(Some(parent), 7L, 42L, txs, version)

    test("the body hash is the Alonzo body hash of the block's own segments") {
        // spec [BH-3]
        val block = forge(IndexedSeq(first, withAux))
        val expected = Block.bodyHash(
          block.transactionBodies,
          block.transactionWitnessSets,
          block.auxiliaryDataSet,
          block.invalidTransactions
        )
        assert(block.header.headerBody.blockBodyHash == expected)
    }

    test("the body size is the byte size of the encoded body") {
        // spec [BH-4]: the block is an array of the header and the four body segments
        val block = forge(IndexedSeq(first, withAux))
        val blockBytes = Cbor.encode(block).length
        val headerBytes = Cbor.encode(block.header).length
        val arrayHeaderBytes = 1 // 0x85, an array of 5
        assert(block.header.headerBody.blockBodySize == blockBytes - arrayHeaderBytes - headerBytes)
    }

    test("the header carries the height, the parent, the slot and the protocol version") {
        // spec [BH-5], [BH-6], [BH-7]
        val body = forge(IndexedSeq(first)).header.headerBody
        assert(body.blockNumber == 7L)
        assert(body.prevHash.contains(parent))
        assert(body.slot == 42L)
        assert(body.protocolVersion == version)
        val onGenesis = BlockForge.forge(None, 1L, 0L, IndexedSeq(first), version)
        assert(onGenesis.header.prevHash.isEmpty)
    }

    test("the forged block decodes back to itself") {
        // spec [BH-8]: the consensus fields have the sizes a Conway block decoder expects
        val block = forge(IndexedSeq(first, withAux))
        val decoded = BlockFile.fromCborArray(Cbor.encode(BlockFile(7, block))).block
        assert(decoded.hash == block.hash)
        assert(decoded.header.headerBody.vrfResult.output.size == 64)
    }

    test("txs keep their order, aux data sits at the tx index, invalid txs are listed") {
        // spec [BH-9]
        val block = forge(IndexedSeq(first, withAux, first.copy(isValid = false)))
        assert(
          block.transactionBodies.map(_.raw.toSeq) == Seq(first, withAux, first).map(
            _.body.raw.toSeq
          )
        )
        assert(block.auxiliaryDataSet.keySet == Set(1))
        assert(block.invalidTransactions == IndexedSeq(2))
    }

    test("blocks on two parents with the same txs have different ids") {
        // spec [BH-2], section 4 consequence
        val other = BlockHash.fromByteString(ByteString.fromHex("cd" * 32))
        val a = forge(IndexedSeq(first))
        val b = BlockForge.forge(Some(other), 7L, 42L, IndexedSeq(first), version)
        assert(a.hash != b.hash)
        assert(forge(IndexedSeq(first)).hash == a.hash, "the same inputs forge the same block")
    }

    private val third = payment(2, 4_000_000)
    private val big = payment(3, 5_000_000).copy(auxiliaryData =
        Some(
          KeepRaw(
            AuxiliaryData.Metadata(
              Map(Word64(1L) -> Metadatum.Bytes(ByteString.fromArray(new Array[Byte](2000))))
            )
          )
        )
    )

    private def prefix(txs: IndexedSeq[Transaction], maxBodySize: Long): Int =
        BlockForge.prefixThatFits(txs, maxBodySize, ExUnits(Long.MaxValue, Long.MaxValue))

    test("the prefix stops at the first tx that does not fit: a prefix, not a best fit") {
        // spec [FG-4], section 8.1 example: tx 4 fits after tx 2 but stays behind tx 3
        val txs = IndexedSeq(first, second, big, third)
        val limit = BlockForge.bodySize(IndexedSeq(first, second, third))
        assert(BlockForge.bodySize(IndexedSeq(first, second, big)) > limit)
        assert(prefix(txs, limit) == 2)
    }

    test("the body size limit holds for the encoded body of the whole prefix") {
        // spec [FG-4], [FG-5], [BH-4]
        val txs = IndexedSeq(first, second, third)
        val two = BlockForge.bodySize(txs.take(2))
        assert(two == forge(txs.take(2)).header.headerBody.blockBodySize)
        assert(prefix(txs, two) == 2)
        assert(prefix(txs, two - 1) == 1)
        assert(prefix(txs, Long.MaxValue) == 3)
    }

    private def withExUnits(tx: Transaction, memory: Long, steps: Long): Transaction = {
        val redeemer = Redeemer(RedeemerTag.Spend, 0, Data.I(0), ExUnits(memory, steps))
        tx.withWitness(_.copy(redeemers = Some(KeepRaw(Redeemers.Array(IndexedSeq(redeemer))))))
    }

    test("memory units and CPU steps are each a block limit on their own") {
        // spec [FG-5]
        val limit = ExUnits(10, 10)
        def fit(units: (Long, Long)*): Int = BlockForge.prefixThatFits(
          units.toIndexedSeq.map((m, s) => withExUnits(first, m, s)),
          Long.MaxValue,
          limit
        )
        assert(fit((6, 1), (6, 1)) == 1, "memory")
        assert(fit((1, 6), (1, 6)) == 1, "steps")
        assert(fit((5, 5), (5, 5), (1, 0)) == 2)
    }

    test("the first tx is always taken, so a forge from a non-empty mempool makes progress") {
        // spec [FG-4], [FG-9] implementation status: one tx always fits
        assert(prefix(IndexedSeq(big, first), 1L) == 1)
        assert(prefix(IndexedSeq.empty, 1L) == 0)
    }
}
