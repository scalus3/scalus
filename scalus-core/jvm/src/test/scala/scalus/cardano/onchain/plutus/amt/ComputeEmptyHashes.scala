package scalus.cardano.onchain.plutus.amt

import scalus.uplc.builtin.Builtins.{blake2b_256, appendByteString}
import scalus.uplc.builtin.ByteString

object ComputeEmptyHashes {
    def main(args: Array[String]): Unit = {
        val nullHash = ByteString.fromArray(new Array[Byte](32))
        var h = blake2b_256(nullHash)
        for i <- 0 to 20 do
            val sep = if i < 20 then " ++" else ""
            println(s"""        hex"${h.toHex}"$sep // E[$i]""")
            h = blake2b_256(appendByteString(h, h))
    }
}
