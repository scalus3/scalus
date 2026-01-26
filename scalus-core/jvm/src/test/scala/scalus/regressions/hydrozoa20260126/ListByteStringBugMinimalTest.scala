package scalus.regressions.hydrozoa20260126

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Builtins.{bls12_381_G2_add, bls12_381_G2_uncompress}
import scalus.builtin.{BLS12_381_G2_Element, ByteString, Data, FromData, ToData}
import scalus.builtin.Data.toData
import scalus.prelude.List
import scalus.prelude.crypto.bls12_381.G2
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.PlutusVM

/** Minimal reproduction of the List[ByteString] bug.
  *
  * The bug: when iterating over List[ByteString] from a case class with derived FromData, using
  * .take().map() pattern, the list elements remain as Data instead of being converted to ByteString
  * before being passed to builtins.
  *
  * Error: "Expected type ByteString, got VCon(Data(B(...)))"
  *
  * Key requirements to trigger the bug:
  *   1. Case class with List[ByteString] field
  *   2. .take(n).map(builtin) pattern on the list
  *   3. Result consumed by another operation (e.g., foldLeft)
  */

// Minimal case class with List[ByteString]
case class Setup(items: List[ByteString])

@Compile
object Setup {
    given FromData[Setup] = FromData.derived
    given ToData[Setup] = ToData.derived

    def process(data: Data): BLS12_381_G2_Element = {
        val setup = data.to[Setup]
        // Bug: items.take(1) returns Data elements instead of ByteString
        val g2Elements = setup.items.take(1).map(bs => bls12_381_G2_uncompress(bs))
        g2Elements.foldLeft(G2.zero)((acc, elem) => bls12_381_G2_add(acc, elem))
    }
}

class ListByteStringBugMinimalTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    // Valid G2 point (96 bytes compressed)
    val g2Point = ByteString.fromHex(
      "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
    )

    test("List[ByteString] bug - take().map() pattern fails with Data instead of ByteString") {
        import Setup.given

        val setup = Setup(items = List(g2Point))
        val data = setup.toData

        val compiled = Compiler.compile(Setup.process)
        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        val applied = uplc $ Term.Const(Constant.Data(data))

        // This fails with: Expected type ByteString, got VCon(Data(...))
        val result = applied.evaluate
    }
}
