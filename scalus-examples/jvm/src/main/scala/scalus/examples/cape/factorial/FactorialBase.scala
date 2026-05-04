package scalus.examples.cape.factorial

import scalus.compiler.Compile

import scalus.*

/** CAPE factorial_naive_recursion scenario.
  *
  * Prescribed naive recursive algorithm for compiler comparison.
  */
@Compile
object FactorialBase {
    def factorial(n: BigInt): BigInt =
        if n <= BigInt(0) then BigInt(1) else n * factorial(n - BigInt(1))
}
