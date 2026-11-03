package scalus.examples.cape.factorial

import scalus.compiler.Compile

import scalus.*

/** CAPE factorial open mode implementation: the recursion unrolled four times.
  *
  * Every result is computed at run time; the program holds no precomputed values, as the scenario
  * requires. Unrolling makes each recursive call do four multiplications, so `factorial(12)` takes
  * three calls instead of eleven, and a call costs more than a multiplication. The base case covers
  * `n <= 3`: `n * (n - 1)` is `n!` for 2 and 3, and every `n <= 1`, negatives included, gives 1.
  *
  * Measured over the 10 fixture cases (total fee in lovelace, execution plus reference-script fee):
  *
  * | shape                                 |     mem | size | total fee |
  * |:--------------------------------------|--------:|-----:|----------:|
  * | `n * f(n - 1)`                        |  91,195 | 34 B |     7,472 |
  * | unrolled twice                        |  75,999 | 43 B |     6,573 |
  * | unrolled four times (this)            |  62,485 | 74 B |     6,055 |
  * | the same, hand-written UPLC           |  65,185 | 77 B |     6,287 |
  * | accumulator loop `go(n - 1, acc * n)` | 103,595 | 44 B |     8,480 |
  *
  * The compiled version beats the hand-written one: CSE binds the partial application
  * `subtractInteger n` once per call and reuses it for `n - 1` to `n - 4`.
  */
@Compile
object FactorialOpen {
    def factorial(n: BigInt): BigInt =
        if n <= 3 then if n <= 1 then 1 else n * (n - 1)
        else n * (n - 1) * (n - 2) * (n - 3) * factorial(n - 4)
}
