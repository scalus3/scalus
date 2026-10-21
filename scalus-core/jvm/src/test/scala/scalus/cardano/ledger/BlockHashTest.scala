package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

/** Block identity against mainnet blocks. The expected hashes come from Koios
  * (`/blocks?block_height=eq.<n>`).
  */
class BlockHashTest extends AnyFunSuite {

    private val mainnet = Seq(
      11544748L -> "be538dcb039a32eb7f68bfd48943d81dc97b7073e118ce4b8b564fdbc96024c3",
      11618219L -> "184a5fc6faa29f3b6876c8f2ef7df8ff42b24be097b7f8d11c83eaf7a2bd963d"
    )

    private def readBlock(height: Long): Block = {
        val bytes = getClass.getResourceAsStream(s"/blocks/block-$height.cbor").readAllBytes()
        BlockFile.fromCborArray(bytes).block
    }

    test("BlockHeader.hash is the mainnet block hash") { // spec [TE-11], [BH-1]
        for (height, expected) <- mainnet do
            assert(readBlock(height).header.hash.toHex == expected, s"block $height")
    }

    test("Block.hash is the header hash") { // spec [TE-11], [BH-2]
        for (height, expected) <- mainnet do
            assert(readBlock(height).hash.toHex == expected, s"block $height")
    }

    test("Block.bodyHash recomputes the header's blockBodyHash") { // spec [TE-13], [BH-3]
        for (height, _) <- mainnet do
            val block = readBlock(height)
            val computed = Block.bodyHash(
              block.transactionBodies,
              block.transactionWitnessSets,
              block.auxiliaryDataSet,
              block.invalidTransactions
            )
            assert(computed == block.header.headerBody.blockBodyHash, s"block $height")
    }
}
