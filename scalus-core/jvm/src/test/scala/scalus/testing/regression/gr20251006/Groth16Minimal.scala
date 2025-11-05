package scalus.testing.regression.gr20251006

import scalus.Compile
import scalus.builtin.ByteString
import scalus.builtin.BLS12_381_G1_Element
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.prelude.*
import scalus.prelude.crypto.bls12_381.G1
import scalus.prelude.crypto.bls12_381.G1.*

import scala.annotation.tailrec

/** Minimal reproducer for Groth16 type unification bug
  *
  * The bug occurs when:
  *   1. We have a recursive function with nested pattern matching
  *   2. The outer match is on List[ByteString]
  *   3. The inner match is on List[BigInt] (which becomes List[Int] in SIR)
  *   4. The recursive call is inside the inner match
  *   5. The function is called through Data.to[] conversion
  *
  * Error: "Cannot unify Proxy(X) -> BLS12_381_G1_Element -> BLS12_381_G1_Element and
  * scalus.prelude.List[Int] -> BLS12_381_G1_Element -> BLS12_381_G1_Element"
  */
@Compile
//@ScalusDebug(20)
object Groth16Minimal:
    /** Minimal case class needed for Data conversion */
    case class MinimalData(value: BigInt)

    val x = BigInt(2)

    given FromData[MinimalData] = FromData.derived
    given ToData[MinimalData] = ToData.derived

    /** This is the minimal function that reproduces the bug */
    @tailrec
    def minimalDerive(
        vk_ic: List[ByteString],
        public: List[BigInt],
        result: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = vk_ic match
        case List.Nil => result
        case List.Cons(i, rest) =>
            public match
                case List.Cons(scalar, publicRest) =>
                    minimalDerive(rest, publicRest, result + G1.uncompress(i).scale(scalar))
                case _ => fail("Invalid input")

    /** This wrapper is called with Data conversion - this triggers the bug */
    def minimalWrapper(data: MinimalData): BLS12_381_G1_Element =
        val vk_ic = List.Nil: List[ByteString]
        val public = List.Nil: List[BigInt]
        val result = G1.uncompress(ByteString.empty)
        // NOTE: Bug ONLY occurs with recursive call to minimalDerive
        // Inlining the body without recursion makes the bug disappear!
        minimalDerive(vk_ic, public, result)
