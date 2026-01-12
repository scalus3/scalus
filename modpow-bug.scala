//> using scala 3.7.4

// BigInt.modPow bug demonstration for Scala.js and Scala Native
//
// Bug: BigInt.modPow returns incorrect results for very large numbers
//
// To reproduce:
//   JVM (correct):      scala-cli run modpow-bug.scala
//   Scala.js (BUG):     scala-cli run --js modpow-bug.scala
//   Scala Native (BUG): scala-cli run --native modpow-bug.scala
//
// Tested on:
//   - Scala 3.3.7, 3.7.0, 3.7.4, 3.8.0-RC5 - all affected
//   - Scala.js 1.20.1
//   - Scala Native 0.5.9

object ModPowBug:
    def main(args: Array[String]): Unit =
        // Values from Plutus conformance test (modulus is 79!)
        val base = BigInt("295783465278346578267348527836475862348589358937497")
        val exp = BigInt("89734578923487957289347527893478952378945268423487234782378423")
        val modulus = BigInt(
          "894618213078297528685144171539831652069808216779571907213868063227837990693501860533361810841010176000000000000000000"
        )

        // Expected result (verified on JVM using Java's BigInteger.modPow)
        val expected = BigInt(
          "280175799933420074585178470510090012806707950340412289432212739835789837904455835552327022379259130346551828535037673"
        )

        val result = base.modPow(exp, modulus)

        println(s"base     = $base")
        println(s"exp      = $exp")
        println(s"modulus  = $modulus (79!)")
        println(s"expected = $expected")
        println(s"result   = $result")
        println(s"match    = ${result == expected}")

        if result != expected then
            println("\n*** BUG: BigInt.modPow returns incorrect result for large numbers! ***")
        else println("\nOK: Result matches expected value")
