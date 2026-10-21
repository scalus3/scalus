package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TxBuilder
import scalus.serialization.cbor.Cbor
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.builtin.ByteString

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
}
