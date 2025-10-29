package scalus.cardano.ledger.value

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{AssetName, ScriptHash}
import spire.math.SafeLong

import scala.collection.immutable.TreeMap

class MultiAssetUnit extends AnyFunSuite {
    test("Multiasset: additive semigroup associativity") {
        val combine = MultiAsset.Unbounded.algebra.additive.combine
        val x = combine(combine(Sample1.arg0, Sample1.arg1), Sample1.arg2)
        val y = combine(Sample1.arg0, combine(Sample1.arg1, Sample1.arg2))

        assert(x === y)
    }

    test("Multiasset inner: additive semigroup associativity") {
        val combine = MultiAsset.Inner.Unbounded.algebra.additive.combine
        val x = combine(combine(Sample2.arg0, Sample2.arg1), Sample2.arg2)
        val y = combine(Sample2.arg0, combine(Sample2.arg1, Sample2.arg2))

        assert(x === y)
    }

}

// Multiasset.Unbounded
object Sample1 {
    def arg0: MultiAsset.Unbounded = MultiAsset.Unbounded(
      TreeMap(
        ScriptHash.fromHex(
          "80015d0080735d58de2dc2b07f8d7fe3a0b400f48976807f017e0180"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("1cb237007f7f7f7f7f7f9f7fadff") -> Coin.Unbounded(
              SafeLong(BigInt("-18455117580243881858363790254989867086"))
            ),
            AssetName.fromHex("ffff00547f01ff36417f7f00000180ae03228d0092012675") -> Coin
                .Unbounded(SafeLong(BigInt("-2840902031029747574")))
          )
        ),
        ScriptHash.fromHex(
          "80ff2d156b01e35472ae4055570501c2481c2bff01ff04c428ff003a"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("2fff80ff807f") -> Coin.Unbounded(
              SafeLong(BigInt("16225969679496701499090664111341158154"))
            ),
            AssetName.fromHex("7f") -> Coin.Unbounded(SafeLong(BigInt("9223372036854775807")))
          )
        ),
        ScriptHash.fromHex(
          "b5daed7f9ab580b78090ff80a1b600ff2b0036ff00c3008000800051"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("7f32ff019880ff7faefeffffffd0ad3c011d3a8053ff6f00c4") -> Coin
                .Unbounded(
                  SafeLong(
                    BigInt(
                      "225252559653527453472522159683346328004212064821912648364960272079619809140"
                    )
                  )
                )
          )
        ),
        ScriptHash.fromHex(
          "c31bffcc005dffbcffa3a39f7f80bbffe3802580ad7f7fff7facc500"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("017800ed918cc96b8063fff089732eabff01ff24030001db") -> Coin
                .Unbounded(SafeLong(BigInt("-87841951468961605"))),
            AssetName.fromHex("087f802580") -> Coin.Unbounded(SafeLong(BigInt("0"))),
            AssetName.fromHex("2d00808601017f220080551e78f817918001") -> Coin.Unbounded(
              SafeLong(BigInt("-9143250258543024915"))
            ),
            AssetName.fromHex("7a7fe50180809ea6ff7f69ffd9d237d7") -> Coin.Unbounded(
              SafeLong(BigInt("-6073086581756615311"))
            ),
            AssetName.fromHex("7f011d27adc81e2c677f4e0080b680c0ffa7") -> Coin.Unbounded(
              SafeLong(BigInt("0"))
            ),
            AssetName.fromHex(
              "a916b8fff0771b802eff33622d017f7f00cc5b7f937fff7f1b157500ff57"
            ) -> Coin.Unbounded(SafeLong(BigInt("-2706664316884177434"))),
            AssetName.fromHex("c980a9017fd223e9d2cb807f8017664e0672a78c017aff") -> Coin
                .Unbounded(SafeLong(BigInt("4462242196691072364"))),
            AssetName.fromHex("f601a6ce80") -> Coin.Unbounded(
              SafeLong(BigInt("-5731736646960354338"))
            )
          )
        )
      )
    )

    def arg1: MultiAsset.Unbounded = MultiAsset.Unbounded(
      TreeMap(
        ScriptHash.fromHex(
          "540480ff784d00e8809401d600ff00ffe733ff807f10ff8008012d7f"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("00") -> Coin.Unbounded(SafeLong(BigInt("4115725337727204101"))),
            AssetName.fromHex("00c6e905ff36746b00010aad01a50100ff") -> Coin.Unbounded(
              SafeLong(BigInt("-5180573923122248185"))
            ),
            AssetName.fromHex("7f7f") -> Coin.Unbounded(SafeLong(BigInt("2843427167511201563"))),
            AssetName.fromHex("7f7f01a7") -> Coin.Unbounded(
              SafeLong(
                BigInt(
                  "-245033970711438365570232477742897838636897000226992536478956854014293034243"
                )
              )
            ),
            AssetName.fromHex("a1e27f01007f5080bd3aef0001107f88") -> Coin.Unbounded(
              SafeLong(BigInt("-1"))
            ),
            AssetName.fromHex("eb190101319360a6ffd75e80fc69fa24b46b80f52d83486080") -> Coin
                .Unbounded(
                  SafeLong(BigInt("-284116074579825537910591449976261403282429019397351548384"))
                ),
            AssetName.fromHex("ff871b7fecc82c7367ffff5500428051790100015ba200ff7f") -> Coin
                .Unbounded(SafeLong(BigInt("4614091028871556129"))),
            AssetName.fromHex("ffe6f36d50f9bf234d80548c9101dd010016000047") -> Coin.Unbounded(
              SafeLong(BigInt("0"))
            )
          )
        ),
        ScriptHash.fromHex(
          "80ff7cb33a7f80001e53167b7c92af80803b800550ff84b100010001"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("170001ffb601d7c07f2e805f3beac396807ff3b601") -> Coin.Unbounded(
              SafeLong(BigInt("-373043488302179341"))
            )
          )
        )
      )
    )

    def arg2: MultiAsset.Unbounded = MultiAsset.Unbounded(
      TreeMap(
        ScriptHash.fromHex(
          "9801ce3d398080807f80e4d5107fa780ff01ff7fff9c57f1007f0031"
        ) -> MultiAsset.Inner.Unbounded(
          TreeMap(
            AssetName.fromHex("ff80ff80010000") -> Coin.Unbounded(
              SafeLong(BigInt("9223372036854775807"))
            )
          )
        )
      )
    )
}

// MultiAsset.Inner.Unbounded
object Sample2 {
    def arg0: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(
      TreeMap(
        AssetName.fromHex("00010000cc8098d4fd7f8382040f80f187ff017f807ffffcd37fff7fd5") -> Coin
            .Unbounded(
              SafeLong(BigInt("321190008023549997317069685203906732131662866146384593750"))
            ),
        AssetName.fromHex("01f4ff09") -> Coin.Unbounded(SafeLong(BigInt("9223372036854775807"))),
        AssetName.fromHex("d101") -> Coin.Unbounded(
          SafeLong(BigInt("31698730859183832319326671288136611992"))
        ),
        AssetName.fromHex("e280ff17ffe3807f7fbce4007f0001f281b94c812c8b77") -> Coin.Unbounded(
          SafeLong(BigInt("-154386657429047264532538813100205458682329114585932424154"))
        ),
        AssetName.fromHex("eaf1349d8d8480807fff") -> Coin.Unbounded(
          SafeLong(BigInt("-1225393406793946805"))
        )
      )
    )

    def arg1: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(
      TreeMap(
        AssetName.fromHex("31b82c4c") -> Coin.Unbounded(SafeLong(BigInt("7561029132170992937"))),
        AssetName.fromHex("80c580017ff2119602807fff15697fabff1512ffff") -> Coin.Unbounded(
          SafeLong(BigInt("98979326718604961720210962780132482466134417397919403840"))
        ),
        AssetName.fromHex("8e009c2d7f") -> Coin.Unbounded(SafeLong(BigInt("-7705343657947628033"))),
        AssetName.fromHex("b571016044ff5d80b3ff") -> Coin.Unbounded(
          SafeLong(BigInt("30105993235216400346046495983554747202763564726371121173"))
        )
      )
    )

    def arg2: MultiAsset.Inner.Unbounded = MultiAsset.Inner.Unbounded(
      TreeMap(
        AssetName.fromHex("02306c612cd34a8dca0f010c0124") -> Coin.Unbounded(SafeLong(BigInt("1")))
      )
    )
}
