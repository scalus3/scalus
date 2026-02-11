package scalus.cardano.onchain.plutus.mpf

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Compile
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.prelude.List
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.ProofStep.*
import scalus.cardano.onchain.plutus.mpf.MerklePatriciaForestry.{Neighbor, ProofStep}
import scalus.testing.kit.EvalTestKit

import scala.language.implicitConversions

class MerklePatriciaForestryTest extends AnyFunSuite with EvalTestKit {

    test("verify bitcoin block 845999") {
        assertEval {
            val proofBitcoin845999: List[ProofStep] = List.Cons(
              Branch(
                skip = 0,
                neighbors =
                    hex"bc13df27a19f8caf0bf922c900424025282a892ba8577095fd35256c9d553ca13a589f00f97a417d07903d138b92f25f879f9462994bf0e69b51fa19a67faef996c3f8196278c6ab196979911cc48b2d4a0d2a7aa5ef3f939eb056256d8efdfa0aa456963256af4fcb1ad43ef4e6323d1ca92c6d83ed4327904280228e1ba159"
              ),
              List.Cons(
                Branch(
                  skip = 0,
                  neighbors =
                      hex"eb63f921bd3ac576f979eba32490f8c0988f468d3308c2ed5480aaf6ff27cf9a0e610d8c38c17236104b995eb83aa062181525dccd72a755772004cc2bf4faaf3ac3518525f4b5dec498c8034c566a3539e524c6a2cd5fc8f19c6559a32260513edca31960cd1f5cc6882b820ef57ca65d740734379781db22b479ae0e3bdef3"
                ),
                List.Cons(
                  Branch(
                    skip = 0,
                    neighbors =
                        hex"e7bbc4fc5e5875f6f5469e8a016fa99a872075360e64d623f8b8688e6b63fee5091a7260d2a4fe1ca489c48020772e6d334c63115743e7c390450a139c6bc63b219aff62993846b5522bc1b1fffb5b485fc58d952a8f171bb6a000062fbdcb0eaa5637413d82489f0492c663ad0bac0a2a83b32e1b14e3940017cf830d47378e"
                  ),
                  List.Cons(
                    Branch(
                      skip = 0,
                      neighbors =
                          hex"464f4d2211c7fe6e7e1b298be6cfa6fd35d562d3b37ce8b979df45fac9dbc5e0d4d93d0b14d7061351763cee1d878b8686c658cfca7ef69cfd58d50ffc3a467340c3abc4067220f82f2dffe455038da3138859bffdb3d34fd7e84305de2ddfc61630c97424469f6de887d42ca155069789fa1b843bdf26496d29222f33f8f6ae"
                    ),
                    List.Cons(
                      Branch(
                        skip = 0,
                        neighbors =
                            hex"2170e155c04db534b1f0e27bb7604907d26b046e51dd7ca59f56693e8033b16403f9ff21fe66b6071042d35dcbad83950ffb1e3a2ad6673f96d043f67d58e82040e0c17f6230c44b857ed04dccd8ff1b84819abf26fa9e1e86d61fb08c80b74c0000000000000000000000000000000000000000000000000000000000000000"
                      ),
                      List.Nil
                    )
                  )
                )
              )
            )

            val trie = MerklePatriciaForestry(
              hex"225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9"
            )
            val blockHash = hex"00000000000000000002d79d6d49c114e174c22b8d8432432ce45a05fd6a4d7b"
            val blockBody = hex"f48fcceeac43babbf53a90023be2799a9d7617098b76ff229440ccbd1fd1b4d4"
            trie.has(blockHash, blockBody, proofBitcoin845999)
        }
    }

    test("insert bitcoin block 845602") {
        assertEvalEq(
          {
              val proofBitcoin845602: List[ProofStep] = List.Cons(
                Branch(
                  skip = 0,
                  neighbors =
                      hex"bc13df27a19f8caf0bf922c900424025282a892ba8577095fd35256c9d553ca120b8645121ebc9057f7b28fa4c0032b1f49e616dfb8dbd88e4bffd7c0844d29b011b1af0993ac88158342583053094590c66847acd7890c86f6de0fde0f7ae2479eafca17f9659f252fa13ee353c879373a65ca371093525cf359fae1704cf4a"
                ),
                List.Cons(
                  Branch(
                    skip = 0,
                    neighbors =
                        hex"255753863960985679b4e752d4b133322ff567d210ffbb10ee56e51177db057460b547fe42c6f44dfef8b3ecee35dfd4aa105d28b94778a3f1bb8211cf2679d7434b40848aebdd6565b59efdc781ffb5ca8a9f2b29f95a47d0bf01a09c38fa39359515ddb9d2d37a26bccb022968ef4c8e29a95c7c82edcbe561332ff79a51af"
                  ),
                  List.Cons(
                    Branch(
                      skip = 0,
                      neighbors =
                          hex"9d95e34e6f74b59d4ea69943d2759c01fe9f986ff0c03c9e25ab561b23a413b77792fa78d9fbcb98922a4eed2df0ed70a2852ae8dbac8cff54b9024f229e66629136cfa60a569c464503a8b7779cb4a632ae052521750212848d1cc0ebed406e1ba4876c4fd168988c8fe9e226ed283f4d5f17134e811c3b5322bc9c494a598b"
                    ),
                    List.Cons(
                      Branch(
                        skip = 0,
                        neighbors =
                            hex"b93c3b90e647f90beb9608aecf714e3fbafdb7f852cfebdbd8ff435df84a4116d10ccdbe4ea303efbf0f42f45d8dc4698c3890595be97e4b0f39001bde3f2ad95b8f6f450b1e85d00dacbd732b0c5bc3e8c92fc13d43028777decb669060558821db21a9b01ba5ddf6932708cd96d45d41a1a4211412a46fe41870968389ec96"
                      ),
                      List.Cons(
                        Branch(
                          skip = 0,
                          neighbors =
                              hex"f89f9d06b48ecc0e1ea2e6a43a9047e1ff02ecf9f79b357091ffc0a7104bbb260908746f8e61ecc60dfe26b8d03bcc2f1318a2a95fa895e4d1aadbb917f9f2936b900c75ffe49081c265df9c7c329b9036a0efb46d5bac595a1dcb7c200e7d590000000000000000000000000000000000000000000000000000000000000000"
                        ),
                        List.Nil
                      )
                    )
                  )
                )
              )
              val trie = MerklePatriciaForestry(
                hex"225a4599b804ba53745538c83bfa699ecf8077201b61484c91171f5910a4a8f9"
              )
              val blockHash = hex"0000000000000000000261a131bf48cc5a19658ade8cfede99dc1c3933300d60"
              val blockBody = hex"26f711634eb26999169bb927f629870938bb4b6b4d1a078b44a6b4ec54f9e8df"
              trie.insert(blockHash, blockBody, proofBitcoin845602).root
          },
          hex"507c03bc4a25fd1cac2b03592befa4225c5f3488022affa0ab059ca350de2353"
        )
    }

    import Fruits.*

    // Convert the string literals to ByteStrings.
    // Must be inline as Scalus ByteString.fromString only supports literals.
    private implicit inline def toByteString(inline s: String): ByteString =
        ByteString.fromString(s)

    test("example kumquat membership") {
        assertEval(trie.has(kumquat.name, kumquat.value, kumquat.proof))
    }

    test("has all fruits") {
        assert(trie.has(apple.name, apple.value, apple.proof))
        assert(trie.has(apricot.name, apricot.value, apricot.proof))
        assert(trie.has(banana.name, banana.value, banana.proof))
        assert(trie.has(blueberry.name, blueberry.value, blueberry.proof))
        assert(trie.has(cherry.name, cherry.value, cherry.proof))
        assert(trie.has(coconut.name, coconut.value, coconut.proof))
        assert(trie.has(cranberry.name, cranberry.value, cranberry.proof))
        assert(trie.has(fig.name, fig.value, fig.proof))
        assert(trie.has(grapefruit.name, grapefruit.value, grapefruit.proof))
        assert(trie.has(grapes.name, grapes.value, grapes.proof))
        assert(trie.has(guava.name, guava.value, guava.proof))
        assert(trie.has(kiwi.name, kiwi.value, kiwi.proof))
        assert(trie.has(kumquat.name, kumquat.value, kumquat.proof))
        assert(trie.has(lemon.name, lemon.value, lemon.proof))
        assert(trie.has(lime.name, lime.value, lime.proof))
        assert(trie.has(mango.name, mango.value, mango.proof))
        assert(trie.has(orange.name, orange.value, orange.proof))
        assert(trie.has(papaya.name, papaya.value, papaya.proof))
        assert(trie.has(passion.name, passion.value, passion.proof))
        assert(trie.has(peach.name, peach.value, peach.proof))
        assert(trie.has(pear.name, pear.value, pear.proof))
        assert(trie.has(pineapple.name, pineapple.value, pineapple.proof))
        assert(trie.has(plum.name, plum.value, plum.proof))
        assert(trie.has(pomegranate.name, pomegranate.value, pomegranate.proof))
        assert(trie.has(raspberry.name, raspberry.value, raspberry.proof))
        assert(trie.has(strawberry.name, strawberry.value, strawberry.proof))
        assert(trie.has(tangerine.name, tangerine.value, tangerine.proof))
        assert(trie.has(tomato.name, tomato.value, tomato.proof))
        assert(trie.has(watermelon.name, watermelon.value, watermelon.proof))
        assert(trie.has(yuzu.name, yuzu.value, yuzu.proof))
    }

    test("insert all fruits") {
        assert(apple.withoutRoot.insert(apple.name, apple.value, apple.proof) == trie)
        assert(apricot.withoutRoot.insert(apricot.name, apricot.value, apricot.proof) == trie)
        assert(banana.withoutRoot.insert(banana.name, banana.value, banana.proof) == trie)
        assert(
          blueberry.withoutRoot.insert(blueberry.name, blueberry.value, blueberry.proof) == trie
        )
        assert(cherry.withoutRoot.insert(cherry.name, cherry.value, cherry.proof) == trie)
        assert(coconut.withoutRoot.insert(coconut.name, coconut.value, coconut.proof) == trie)
        assert(
          cranberry.withoutRoot.insert(cranberry.name, cranberry.value, cranberry.proof) == trie
        )
        assert(fig.withoutRoot.insert(fig.name, fig.value, fig.proof) == trie)
        assert(
          grapefruit.withoutRoot.insert(grapefruit.name, grapefruit.value, grapefruit.proof) == trie
        )
        assert(grapes.withoutRoot.insert(grapes.name, grapes.value, grapes.proof) == trie)
        assert(guava.withoutRoot.insert(guava.name, guava.value, guava.proof) == trie)
        assert(kiwi.withoutRoot.insert(kiwi.name, kiwi.value, kiwi.proof) == trie)
        assert(kumquat.withoutRoot.insert(kumquat.name, kumquat.value, kumquat.proof) == trie)
        assert(lemon.withoutRoot.insert(lemon.name, lemon.value, lemon.proof) == trie)
        assert(lime.withoutRoot.insert(lime.name, lime.value, lime.proof) == trie)
        assert(mango.withoutRoot.insert(mango.name, mango.value, mango.proof) == trie)
        assert(orange.withoutRoot.insert(orange.name, orange.value, orange.proof) == trie)
        assert(papaya.withoutRoot.insert(papaya.name, papaya.value, papaya.proof) == trie)
        assert(passion.withoutRoot.insert(passion.name, passion.value, passion.proof) == trie)
        assert(peach.withoutRoot.insert(peach.name, peach.value, peach.proof) == trie)
        assert(pear.withoutRoot.insert(pear.name, pear.value, pear.proof) == trie)
        assert(
          pineapple.withoutRoot.insert(pineapple.name, pineapple.value, pineapple.proof) == trie
        )
        assert(plum.withoutRoot.insert(plum.name, plum.value, plum.proof) == trie)
        assert(
          pomegranate.withoutRoot
              .insert(pomegranate.name, pomegranate.value, pomegranate.proof) == trie
        )
        assert(
          raspberry.withoutRoot.insert(raspberry.name, raspberry.value, raspberry.proof) == trie
        )
        assert(
          strawberry.withoutRoot.insert(strawberry.name, strawberry.value, strawberry.proof) == trie
        )
        assert(
          tangerine.withoutRoot.insert(tangerine.name, tangerine.value, tangerine.proof) == trie
        )
        assert(tomato.withoutRoot.insert(tomato.name, tomato.value, tomato.proof) == trie)
        assert(
          watermelon.withoutRoot.insert(watermelon.name, watermelon.value, watermelon.proof) == trie
        )
        assert(yuzu.withoutRoot.insert(yuzu.name, yuzu.value, yuzu.proof) == trie)
    }

    test("delete all fruits") {
        assert(trie.delete(apple.name, apple.value, apple.proof) == apple.withoutRoot)
        assert(trie.delete(apricot.name, apricot.value, apricot.proof) == apricot.withoutRoot)
        assert(trie.delete(banana.name, banana.value, banana.proof) == banana.withoutRoot)
        assert(
          trie.delete(blueberry.name, blueberry.value, blueberry.proof) == blueberry.withoutRoot
        )
        assert(trie.delete(cherry.name, cherry.value, cherry.proof) == cherry.withoutRoot)
        assert(trie.delete(coconut.name, coconut.value, coconut.proof) == coconut.withoutRoot)
        assert(
          trie.delete(cranberry.name, cranberry.value, cranberry.proof) == cranberry.withoutRoot
        )
        assert(trie.delete(fig.name, fig.value, fig.proof) == fig.withoutRoot)
        assert(
          trie.delete(grapefruit.name, grapefruit.value, grapefruit.proof) == grapefruit.withoutRoot
        )
        assert(trie.delete(grapes.name, grapes.value, grapes.proof) == grapes.withoutRoot)
        assert(trie.delete(guava.name, guava.value, guava.proof) == guava.withoutRoot)
        assert(trie.delete(kiwi.name, kiwi.value, kiwi.proof) == kiwi.withoutRoot)
        assert(trie.delete(kumquat.name, kumquat.value, kumquat.proof) == kumquat.withoutRoot)
        assert(trie.delete(lemon.name, lemon.value, lemon.proof) == lemon.withoutRoot)
        assert(trie.delete(lime.name, lime.value, lime.proof) == lime.withoutRoot)
        assert(trie.delete(mango.name, mango.value, mango.proof) == mango.withoutRoot)
        assert(trie.delete(orange.name, orange.value, orange.proof) == orange.withoutRoot)
        assert(trie.delete(papaya.name, papaya.value, papaya.proof) == papaya.withoutRoot)
        assert(trie.delete(passion.name, passion.value, passion.proof) == passion.withoutRoot)
        assert(trie.delete(peach.name, peach.value, peach.proof) == peach.withoutRoot)
        assert(trie.delete(pear.name, pear.value, pear.proof) == pear.withoutRoot)
        assert(
          trie.delete(pineapple.name, pineapple.value, pineapple.proof) == pineapple.withoutRoot
        )
        assert(trie.delete(plum.name, plum.value, plum.proof) == plum.withoutRoot)
        assert(
          trie.delete(
            pomegranate.name,
            pomegranate.value,
            pomegranate.proof
          ) == pomegranate.withoutRoot
        )
        assert(
          trie.delete(raspberry.name, raspberry.value, raspberry.proof) == raspberry.withoutRoot
        )
        assert(
          trie.delete(strawberry.name, strawberry.value, strawberry.proof) == strawberry.withoutRoot
        )
        assert(
          trie.delete(tangerine.name, tangerine.value, tangerine.proof) == tangerine.withoutRoot
        )
        assert(trie.delete(tomato.name, tomato.value, tomato.proof) == tomato.withoutRoot)
        assert(
          trie.delete(watermelon.name, watermelon.value, watermelon.proof) == watermelon.withoutRoot
        )
        assert(trie.delete(yuzu.name, yuzu.value, yuzu.proof) == yuzu.withoutRoot)
    }

    test("update banana to eggplant") {
        assert(
          trie.update(banana.name, banana.proof, "🍌", "🍆") ==
              MerklePatriciaForestry(updatedBananaRoot)
        )
    }

    test("idempotent update") {
        val updatedTrie = MerklePatriciaForestry(updatedBananaRoot)

        assert(
          trie.update(banana.name, banana.proof, "🍌", "🍌") == trie,
          "Self update should be idempotent"
        )
        assert(
          updatedTrie.update(banana.name, banana.proof, "🍆", "🍆") == updatedTrie,
          "Updated trie self update should be idempotent"
        )
    }

    test("fail fake update") {
        assertThrows[RequirementError] {
            banana.withoutRoot.update(banana.name, banana.proof, "🍌", "🍆")
        }
    }

    test("insert whatever succeeds with different value") {
        assert(kiwi.withoutRoot.insert(kiwi.name, "foo", kiwi.proof) != trie)
    }

    test("fail inserting already present") {
        assertThrows[RequirementError] {
            trie.insert(kiwi.name, "🥝", kiwi.proof)
        }
    }

    test("fail delete with different value") {
        assertThrows[RequirementError] {
            trie.delete(kiwi.name, "🤷", kiwi.proof)
        }
    }

    test("fail insert already present with different value") {
        assertThrows[RequirementError] {
            trie.insert(kiwi.name, "foo", kiwi.proof)
        }
    }

    test("fail insert nearby with wrong proof") {
        assertThrows[RequirementError] {
            kiwi.withoutRoot.insert(guava.name, "🤷", kiwi.proof)
        }
    }

    test("fail insert higher with wrong proof") {
        assertThrows[RequirementError] {
            kiwi.withoutRoot.insert(kumquat.name, "🤷", kiwi.proof)
        }
    }

    test("fail delete nearby with wrong proof") {
        assertThrows[RequirementError] {
            trie.delete(guava.name, "🤷", kiwi.proof)
        }
    }

    test("fail delete higher with wrong proof") {
        assertThrows[RequirementError] {
            trie.delete(kumquat.name, "🤷", kiwi.proof)
        }
    }
}

case class FruitData(
    name: ByteString,
    value: ByteString,
    proof: List[ProofStep],
    withoutRoot: MerklePatriciaForestry
)

@Compile
object FruitData:
    inline def apply(
        inline name: String,
        inline value: String,
        proof: List[ProofStep],
        withoutRoot: ByteString
    ): FruitData =
        new FruitData(
          ByteString.fromString(name),
          ByteString.fromString(value),
          proof,
          MerklePatriciaForestry(withoutRoot)
        )

@Compile
object Fruits {

    /** Main fruit trie from which all tests derive */
    val trie = MerklePatriciaForestry(
      hex"4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078"
    )

    /** All fruit test data */
    val apple = FruitData(
      "apple[uid: 58]",
      "🍎",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d79519b8cdfbd053e5a86cf28a781debae71638cd77f85aad4b88869373d9dcfd"
        ),
        List.Cons(
          Leaf(
            0,
            hex"5cddcd30a0a388cf6feb3fd6e112c96e9daf23e3a9c8a334e7044650471aaa9e",
            hex"f429821ddf89c9df3c7fbb5aa6fadb6c246d75ceede53173ce59d70dde375d14"
          ),
          List.Cons(
            Leaf(
              0,
              hex"5e7ccfedd44c90423b191ecca1eb21dfbac865d561bace8c0f3e94ae7edf4440",
              hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
            ),
            List.Nil
          )
        )
      ),
      hex"cb7812785c2f6b56e3dba69923e37625e94953257915d25f1041f43cefd5cb62"
    )

    val apricot = FruitData(
      "apricot[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f965c07fa815b86794e8703cee7e8f626c88d7da639258d2466aae67d5d041c5a117abf0e19fb78e0535891d82e5ece1310a1cf11674587dbba304c395769a988"
        ),
        List.Nil
      ),
      hex"c08452d768160cd0fcdf5cad3d181cd36055eaf364d0eb7c49a01936bacf7b1f"
    )

    val banana = FruitData(
      "banana[uid: 218]",
      "🍌",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bcac9431d708d20021f1fa3f4f03468b8de194398072a402e7877376d06f747575a"
        ),
        List.Cons(
          Leaf(
            1,
            hex"3ed002d6885ab5d92e1307fccd1d021c32ec429192aea10cb2fd688b92aef3ac",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Nil
        )
      ),
      hex"557990b1257679f2b8e09c507f2582b0566579a2fc26d0d8a6b59a4a88ef16db"
    )

    val blueberry = FruitData(
      "blueberry[uid: 0]",
      "🫐",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50c2f2115774c117f184b58dba7a23d2c93968aa40387ceb0c9a9f53e4f594e881"
        ),
        List.Cons(
          Leaf(
            0,
            hex"b67e71b092e6a54576fa23b0eb48c5e5794a3fb5480983e48b40e453596cc48b",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Nil
        )
      ),
      hex"e2025bb26dae9291d4eeb58817b5c7eb84ab2e47a27c994cc04369fffe8bc842"
    )

    val cherry = FruitData(
      "cherry[uid: 0]",
      "🍒",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d498417520a7f805c5f674e2deca5230b6942bbc71586dc94a783eebe1ed58c9a864e53"
        ),
        List.Cons(
          Branch(
            3,
            hex"2549707d84ecc2fa100fd85bf15f2ec99da70d4b3a39588c1138331eb0e00d3e85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"968b14e351704108f00325985ab0cd81af8617bb131e31607b6bcd3f96d7c4c2"
    )

    val coconut = FruitData(
      "coconut[uid: 0]",
      "🥥",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca0a22a7b4d767ada48673a4a9313a02a35ff47d2f55bcf10ae294127f590a4327c"
        ),
        List.Cons(
          Leaf(
            0,
            hex"df779e7f171b7299c2cede28bb898c1ee3456d98657b95e8082cd375704b678a",
            hex"9e3d695f13a7292b8859d2ba0113e305825a8af8ba886d2ae73e73f2d35c6afe"
          ),
          List.Nil
        )
      ),
      hex"4888f3b72e475510bc0bb78c5f3706c0520a4294a41f8c05b5561776369d9d5d"
    )

    val cranberry = FruitData(
      "cranberry[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca00a747d583e2e3db49524add1eea3063421fc04547e19c4e807810a537a63b379"
        ),
        List.Cons(
          Leaf(
            0,
            hex"c8cac1a325376bbc49936988b4c720d7806e99c878bc645ad90cebb98302c3ca",
            hex"ccfd71674a4dca5f252690588b24bebffa36068206414b1575c0f7f7f8103839"
          ),
          List.Nil
        )
      ),
      hex"c80ac1ba6f8a6437562b25fe4a110f1c0013f26c7209f699df46493ce85e0081"
    )

    val fig = FruitData(
      "fig[uid: 68267]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50da0bdb30bf45c76153418a634f1bcecba8c601ca985fbca14b57582920d82acb"
        ),
        List.Cons(
          Leaf(
            0,
            hex"a4b927e3735c7dbf9f1846844aad53e82362e47e32223d559333f4d154483c69",
            hex"5ee6f548bba6d9da3313a23b395efb48b440063a592d8592e73d87b79d1d887a"
          ),
          List.Cons(
            Leaf(
              2,
              hex"af12ec41241cb0a5cae2a4a1232a64f3ca68f65342cf2a2f98cd6a00cf7971fe",
              hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
            ),
            List.Nil
          )
        )
      ),
      hex"fde975ef370e50b0d93fd3a766d85e502160b9231e6c4887616459f27e786693"
    )

    val grapefruit = FruitData(
      "grapefruit[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50c2f2115774c117f184b58dba7a23d2c93968aa40387ceb0c9a9f53e4f594e881"
        ),
        List.Cons(
          Leaf(
            0,
            hex"b88701c48c6abd03dfc5f4538bb585102ddc2e4640c55c8c3c9bb7e0093d949e",
            hex"6d96ccb103b14005c17b3c17d45e0df0bab5dd1fb2276197a89ed1aeedaad7a0"
          ),
          List.Nil
        )
      ),
      hex"68125b51606cc784d3ed2010a2bc297776ce7442669a5072220f5e6911e5be84"
    )

    val grapes = FruitData(
      "grapes[uid: 0]",
      "🍇",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f965c07fa815b86794e8703cee7e8f626c88d7da639258d2466aae67d5d041c5ada1771d107c86c8e68da458063a47f9cdb63ddb9e922ab6ccb18d9e6d4b7aaf9"
        ),
        List.Cons(
          Leaf(
            0,
            hex"f63c88d1bc9695dfc39eaf90a11248964311383a95345e5b04d6d8f25d5121ca",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Nil
        )
      ),
      hex"a5a405950c2aaf7da30abbfa969fdecccd4ed19077f751b1de641b2bfc2df957"
    )

    val guava = FruitData(
      "guava[uid: 344]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b"
        ),
        List.Cons(
          Leaf(
            0,
            hex"4a522f84bcda4bebb725d5f2b92af615b57cc1777bb0d8b2c6c18c3e3e6520cd",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Cons(
            Leaf(
              1,
              hex"407c58473af4b3e5b24e65481294b0772ed6a7dd793937c6c90179960d154a22",
              hex"05f6a1db018258657194e930ed49e86cbc3622aeae1c13e92110e28d3635fdca"
            ),
            List.Nil
          )
        )
      ),
      hex"aae08af9abdf11e286ae91c430b81cb306d1f43dcec58ab2f59fe024412e54e2"
    )

    val kiwi = FruitData(
      "kiwi[uid: 0]",
      "🥝",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b"
        ),
        List.Cons(
          Leaf(
            0,
            hex"4a522f84bcda4bebb725d5f2b92af615b57cc1777bb0d8b2c6c18c3e3e6520cd",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Cons(
            Leaf(
              1,
              hex"4076d8ab234597ab6a35c03c805381bbc016025b36ff1f7df9c5009e1a8b73ef",
              hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
            ),
            List.Nil
          )
        )
      ),
      hex"621815dfdfca61bca13341df5aa32f4133225b5e3b5d9c030001b2298132a0fa"
    )

    val kumquat = FruitData(
      "kumquat[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d08434fd717aea47d156185d589f44a59fc2e0158eab7ff035083a2a66cd3e15b"
        ),
        List.Cons(
          Fork(
            0,
            Neighbor(
              0,
              hex"07",
              hex"a1ffbc0e72342b41129e2d01d289809079b002e54b123860077d2d66added281"
            )
          ),
          List.Nil
        )
      ),
      hex"4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6"
    )

    val lemon = FruitData(
      "lemon[uid: 0]",
      "🍋",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d"
        ),
        List.Cons(
          Leaf(
            0,
            hex"0389fd2d655e31dac50b00f3113aa327e36680e9df509d48eb255446d4891abc",
            hex"001fb475e73fee4611a4350ae793d7dca387bcc1e199eabf498002a173378cc5"
          ),
          List.Nil
        )
      ),
      hex"6a7c7950e3718263c3f6d0b5cec7d7724c2394d62053692132c2ffebf8b8e4bd"
    )

    val lime = FruitData(
      "lime[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bcac9431d708d20021f1fa3f4f03468b8de194398072a402e7877376d06f747575a"
        ),
        List.Cons(
          Leaf(
            1,
            hex"3ee659e1fddc70f61cc65eb61478cd92a09fd7787ea4f913047469339f26b3b9",
            hex"356a8eb7e12e71400ef0f2e305a89c643ec8cad60506ca9057201a5e36fb01ab"
          ),
          List.Nil
        )
      ),
      hex"cc11203c785e808fc0555562dd9fef4b9c161d2ed64ff16df47080325862f4a7"
    )

    val mango = FruitData(
      "mango[uid: 0]",
      "🥭",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d49841758c5e4a29601399a5bd916e5f3b34c38e13253f4de2a3477114f1b2b8f9f2f4d"
        ),
        List.Cons(
          Leaf(
            0,
            hex"09d23032e6edc0522c00bc9b74edd3af226d1204a079640a367da94c84b69ecc",
            hex"c29c35ad67a5a55558084e634ab0d98f7dd1f60070b9ce2a53f9f305fd9d9795"
          ),
          List.Nil
        )
      ),
      hex"c683f99382df709f322b957c3ff828ab10cb2b6a855458e4b3d23fbea83e7a0e"
    )

    val orange = FruitData(
      "orange[uid: 0]",
      "🍊",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca00a747d583e2e3db49524add1eea3063421fc04547e19c4e807810a537a63b379"
        ),
        List.Cons(
          Leaf(
            0,
            hex"c5dc3c068b45ce9dbf42d07fd86fc2dac165fd1b81ce73267b2aee242afba3f3",
            hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
          ),
          List.Nil
        )
      ),
      hex"59854171e5e36247499d82747754ca56eb7ced82fa27edc95f7c102fbcbee3f0"
    )

    val papaya = FruitData(
      "papaya[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f965c07fa815b86794e8703cee7e8f626c88d7da639258d2466aae67d5d041c5ada1771d107c86c8e68da458063a47f9cdb63ddb9e922ab6ccb18d9e6d4b7aaf9"
        ),
        List.Cons(
          Leaf(
            0,
            hex"fb69c0d60ec9bfb6cafa5cf54675edfbb0017b873ee92a5dbb6bdabcfb352145",
            hex"b5898c51c32083e91b8c18c735d0ba74e08f964a20b1639c189d1e8704b78a09"
          ),
          List.Nil
        )
      ),
      hex"97a6f166b2c5f5a46776e1b471d7855a3e876e95d1eff34c5df3734e70d3fcf5"
    )

    val passion = FruitData(
      "passionfruit[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50da0bdb30bf45c76153418a634f1bcecba8c601ca985fbca14b57582920d82acb"
        ),
        List.Cons(
          Leaf(
            0,
            hex"a4b927e3735c7dbf9f1846844aad53e82362e47e32223d559333f4d154483c69",
            hex"5ee6f548bba6d9da3313a23b395efb48b440063a592d8592e73d87b79d1d887a"
          ),
          List.Cons(
            Leaf(
              2,
              hex"af12a10176ecbb08fc16658069ac132455796c5b62a5ef8985933c76652f50e7",
              hex"7c3715aba2db74d565a6ce6cc72f20d9cb4652ddb29efe6268be15b105e40911"
            ),
            List.Nil
          )
        )
      ),
      hex"2e45678664ccbcad82eb24e7a77cb199593b47f07feafaa205b089a8dc48461e"
    )

    val peach = FruitData(
      "peach[uid: 0]",
      "🍑",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350be527bcfc7febe3c560057d97f4190bd24b537a322315f84daafab3ada562b50da0bdb30bf45c76153418a634f1bcecba8c601ca985fbca14b57582920d82acb"
        ),
        List.Cons(
          Fork(
            0,
            Neighbor(
              15,
              hex"0102",
              hex"2f6b320212dd98c38a7cd074886d942d9577cdad5ef1c72d32a01df1a63ed88f"
            )
          ),
          List.Nil
        )
      ),
      hex"5d4bfc2613624f54751303e605568337f6f8eb7bd63369d9f780c5be839dbdd1"
    )

    val pear = FruitData(
      "pear[uid: 0]",
      "🍐",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbe280ada5ef30d55433934bbc73c89d550ee916f62822c34645e04bb66540c120f323def78732eace937391fc626efcd062552ebcf5e93f00352b86cb0f89daca0a22a7b4d767ada48673a4a9313a02a35ff47d2f55bcf10ae294127f590a4327c"
        ),
        List.Cons(
          Leaf(
            0,
            hex"db30478ecc78451d06c1dfe24a35233a6d448fafa17af644fac693a4ca3f502a",
            hex"1c8ca3866d1b2ab614bf085c95381adbf7be2c6e8fa628034932fe4a4f54e1c3"
          ),
          List.Nil
        )
      ),
      hex"8f2da0fcfeab12df8b53dd6ea4fe6ff8fbd6f954f1ead8d40b298b86c3716510"
    )

    val pineapple = FruitData(
      "pineapple[uid: 12577]",
      "🍍",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bca070a12b8b34948fc52296522a0b1816849392f7c2a73b8a25538ccebed176c9f"
        ),
        List.Cons(
          Branch(
            2,
            hex"4e1fa8ba9cc8e18bbf0194b41e5fac140d0e58758706545f2354f2e42f7b4b0685c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b10eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"dcb9a2b6aae1477da4409ce8a62235d87a006011bf1dc893948fad9c5be955f7"
    )

    val plum = FruitData(
      "plum[uid: 15492]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d498417520a7f805c5f674e2deca5230b6942bbc71586dc94a783eebe1ed58c9a864e53"
        ),
        List.Cons(
          Branch(
            3,
            hex"1955c87798c9f03af2f38429ebdeeefe0a0c84db8e583df37561abf0c64f46ca85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1f34025c0e276d328068b15d428480914fd73946ae94a5f45c3530e3decdeefdf0000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"fc7494dec21f1533d8f97677b890ebf3a7954c0b8110895d663a5c78d15daf92"
    )

    val pomegranate = FruitData(
      "pomegranate[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bca070a12b8b34948fc52296522a0b1816849392f7c2a73b8a25538ccebed176c9f"
        ),
        List.Cons(
          Branch(
            2,
            hex"03c59510714326934442799b6960ed35c11bb26ea47746839d9ab0635887aa13b60d6d31fb16509758290f509e419f55abb79ba2ca63c9b329f97f69f9e5b4f90eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"7267243f71e63165e8a697c1282d205352b9534db9902d97eb90b3bd89431ddb"
    )

    val raspberry = FruitData(
      "raspberry[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039cc9e7ff03faba170e98cd3c24338b95b1ce1b8a621d1016418f1494bbeb9e4a4a0000000000000000000000000000000000000000000000000000000000000000"
        ),
        List.Nil
      ),
      hex"4c9d89603cb1a25361777b8ed7f7c80f71b1dea66603872feea2b34a83d34453"
    )

    val strawberry = FruitData(
      "strawberry[uid: 2532]",
      "🍓",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5fcf22cbaac4ab605dd13dbde57080661b53d8a7e23534c733acf50125cf0e5bca070a12b8b34948fc52296522a0b1816849392f7c2a73b8a25538ccebed176c9f"
        ),
        List.Cons(
          Branch(
            2,
            hex"03c59510714326934442799b6960ed35c11bb26ea47746839d9ab0635887aa13eaf6d47da35b10a0585256dfdb4e5a8456e02d276a6c554842bf3e4148160ba70eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"c5890dfafc0c0b66d3af7775ae9c5bac6974ec0b38b5a9a635299a9eae7c8823"
    )

    val tangerine = FruitData(
      "tangerine[uid: 11]",
      "🍊",
      List.Cons(
        Branch(
          0,
          hex"4be28f4839135e1f8f5372a90b54bb7bfaf997a5d13711bb4d7d93f9d4e04fbefa63eb4576001d8658219f928172eccb5448b4d7d62cd6d95228e13ebcbd5350c1e96bcc431893eef34e03989814375d439faa592edf75c9e5dc10b3c30766700000000000000000000000000000000000000000000000000000000000000000"
        ),
        List.Nil
      ),
      hex"826a0c030ad675740b83a33653fd3fc32b1021233f709759292151abdcd37f8d"
    )

    val tomato = FruitData(
      "tomato[uid: 83468]",
      "🍅",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb45fdf82687b1ab133324cebaf46d99d49f92720c5ded08d5b02f57530f2cc5a5f1508f13471a031a21277db8817615e62a50a7427d5f8be572746aa5f0d498417520a7f805c5f674e2deca5230b6942bbc71586dc94a783eebe1ed58c9a864e53"
        ),
        List.Cons(
          Branch(
            3,
            hex"1955c87798c9f03af2f38429ebdeeefe0a0c84db8e583df37561abf0c64f46ca85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1a93bd84b815df138ab148bfdc2c3ee94f0cdbbb6ab8a38d429a7a8895c470ce70000000000000000000000000000000000000000000000000000000000000000"
          ),
          List.Nil
        )
      ),
      hex"8329dfa8be59c7e677966a62d3de98944f231c02cc8e97ec049fd1a8e5898474"
    )

    val watermelon = FruitData(
      "watermelon[uid: 0]",
      "🍉",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d79519b8cdfbd053e5a86cf28a781debae71638cd77f85aad4b88869373d9dcfd"
        ),
        List.Cons(
          Fork(
            0,
            Neighbor(
              14,
              hex"",
              hex"995afcfa89b7430348dbfb6171b944794119ffc0dd003edc18e2fa8d6d6d48bf"
            )
          ),
          List.Nil
        )
      ),
      hex"e69c446638ad9ae9654e4e6699954996da1f1256d6df711bcb6a740659dfe470"
    )

    val yuzu = FruitData(
      "yuzu[uid: 0]",
      "🤷",
      List.Cons(
        Branch(
          0,
          hex"c7bfa4472f3a98ebe0421e8f3f03adf0f7c4340dec65b4b92b1c9f0bed209eb47238ba5d16031b6bace4aee22156f5028b0ca56dc24f7247d6435292e82c039c3490a825d2e8deddf8679ce2f95f7e3a59d9c3e1af4a49b410266d21c9344d6d79519b8cdfbd053e5a86cf28a781debae71638cd77f85aad4b88869373d9dcfd"
        ),
        List.Cons(
          Leaf(
            0,
            hex"5cddcd30a0a388cf6feb3fd6e112c96e9daf23e3a9c8a334e7044650471aaa9e",
            hex"f429821ddf89c9df3c7fbb5aa6fadb6c246d75ceede53173ce59d70dde375d14"
          ),
          List.Cons(
            Leaf(
              0,
              hex"5ed71f91166242e8477758810ad103aff35313b175b1762b0efe800fa9a126d2",
              hex"09d504e02c4e6fa7b66303a456bc8786da3f51e8bf2834eeb9c95ec479f3681a"
            ),
            List.Nil
          )
        )
      ),
      hex"366a84bbb2274658080acb85026bba389054782b4681f4cf9e29141ac98de253"
    )

    /** Root hash when banana is mapped to eggplant instead */
    val updatedBananaRoot = hex"9057d02799a012a9d47fab6f9f5c43b4b2bf94584b339e3b4d3969fd95d55972"
}
