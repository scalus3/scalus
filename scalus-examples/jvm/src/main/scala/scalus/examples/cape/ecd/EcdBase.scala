package scalus.examples.cape.ecd

import scalus.compiler.Compile
import scalus.*

/** CAPE `ecd` scenario: prescribed naive recursive Euclidean algorithm.
  *
  * Direct translation of the spec (fixed mode):
  * {{{ecd a b | b == 0 = abs a | otherwise = ecd b (a `mod` b)}}}
  */
@Compile
object EcdBase {
    def ecd(a: BigInt, b: BigInt): BigInt =
        if b == BigInt(0) then if a < 0 then -a else a
        else ecd(b, a % b)
}
