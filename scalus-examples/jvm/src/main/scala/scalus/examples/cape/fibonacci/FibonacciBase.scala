package scalus.examples.cape.fibonacci

import scalus.compiler.Compile

import scalus.*

/** CAPE fibonacci_naive_recursion scenario.
  *
  * Prescribed naive recursive algorithm for compiler comparison. Returns n for n <= 1, so
  * fibonacci(-1) == -1 as CAPE expects.
  */
@Compile
object FibonacciBase {
    def fibonacci(n: BigInt): BigInt =
        if n <= BigInt(1) then n else fibonacci(n - BigInt(1)) + fibonacci(n - BigInt(2))
}
