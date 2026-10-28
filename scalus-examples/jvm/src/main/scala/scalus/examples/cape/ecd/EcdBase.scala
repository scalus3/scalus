package scalus.examples.cape.ecd

import scalus.compiler.Compile
import scalus.*
import scalus.uplc.builtin.Builtins.modInteger

/** CAPE `ecd` scenario: prescribed naive recursive Euclidean algorithm.
  *
  * Direct translation of the spec (fixed mode):
  * {{{ecd a b | b == 0 = abs a | otherwise = ecd b (a `mod` b)}}}
  *
  * Haskell's `mod` takes the sign of the divisor, like the `modInteger` builtin. Scala's `%` is
  * `rem`, which takes the sign of the dividend and changes the recursive arguments for negative
  * inputs: `ecd(-12, 8)` must recurse into `ecd(8, 4)`, not `ecd(8, -4)`.
  */
@Compile
object EcdBase {
    def ecd(a: BigInt, b: BigInt): BigInt =
        if b == BigInt(0) then if a < 0 then -a else a
        else ecd(b, modInteger(a, b))
}
