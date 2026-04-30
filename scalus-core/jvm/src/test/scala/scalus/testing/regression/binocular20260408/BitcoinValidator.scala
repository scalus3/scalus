package scalus.testing.regression.binocular20260408

import scalus.compiler.Compile
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.{FromData, ToData}

type BlockHash = ByteString
type MerkleRoot = ByteString
type CompactBits = ByteString

opaque type BlockHeader = ByteString

object BlockHeader {
    def apply(bytes: ByteString): BlockHeader = bytes

    extension (self: BlockHeader)
        inline def version: BigInt = byteStringToInteger(false, self.slice(0, 4))
        inline def prevBlockHash: BlockHash = self.slice(4, 32)
        inline def bits: CompactBits = self.slice(72, 4)
        inline def merkleRoot: MerkleRoot = self.slice(36, 32)
        inline def timestamp: BigInt = byteStringToInteger(false, self.slice(68, 4))
        inline def bytes: ByteString = self

    @uplcIntrinsic("bData")
    given ToData[BlockHeader] = (a: BlockHeader) => bData(a)

    @uplcIntrinsic("unBData")
    given FromData[BlockHeader] = data => unBData(data)
}

@Compile
object BitcoinValidator {
    def validateBlock(header: BlockHeader): ByteString = {
        header.bits
    }
}
